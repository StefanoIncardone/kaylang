use std::{path::PathBuf, io::{BufWriter, Write, ErrorKind}, process::Command, fs::File, borrow::Cow};

use crate::{lexer::*, ast::*, logging::*};


// TODO introduce intermediate representation
#[derive( Debug )]
pub(crate) struct CompilerVariable<'src> {
    name: &'src str,
    typ: Type,
    offset: usize,
}


#[derive( Debug )]
pub(crate) struct CompilerString<'ast> {
    string: &'ast Vec<u8>,
    label: String,
    len_label: String,
}


#[derive( Debug )]
pub(crate) struct Compiler<'src: 'tokens, 'tokens: 'ast, 'ast> {
    src: &'src Src,
    out_path: &'src Option<PathBuf>,

    ast: &'ast Vec<Scope<'tokens, 'src>>,

    rodata: String,
    asm: String,

    variables: Vec<CompilerVariable<'src>>,
    strings: Vec<CompilerString<'ast>>,

    if_depth: usize,
    loop_depth: usize,
    loop_stack: Vec<usize>,
}

// Generation of compilation artifacts (.asm, .o, executable)
impl<'src: 'tokens, 'tokens: 'ast, 'ast> Compiler<'src, 'tokens, 'ast> {
    const STACK_ALIGN: usize = core::mem::size_of::<usize>();

    pub(crate) fn compile( src: &'src Src, out_path: &Option<PathBuf>, ast: &'ast Vec<Scope<'src, 'tokens>>, logger: &mut CompilationLogger ) -> Result<PathBuf, IoError> {
        let mut this = Compiler {
            src,
            out_path,
            ast,
            rodata: String::new(),
            asm: String::new(),
            variables: Vec::new(),
            strings: Vec::new(),
            if_depth: 0,
            loop_depth: 0,
            loop_stack: Vec::new(),
        };

        logger.step( &COMPILING, &this.src.path );


        let (asm_path, obj_path, exe_path) = if let Some( out_path ) = &this.out_path {
            match std::fs::create_dir_all( out_path ) {
                Ok( _ ) => {},
                Err( err ) if err.kind() == ErrorKind::AlreadyExists => {},
                Err( err ) => {
                    logger.substep( &ASM_GENERATION );
                    return Err( IoError {
                        kind: err.kind(),
                        msg: format!( "could not create output directory '{}'", out_path.display() ).into(),
                        cause: err.to_string().into(),
                    } );
                }
            }

            (out_path.join( this.src.path.with_extension( "asm" ).file_name().unwrap() ),
            out_path.join( this.src.path.with_extension( "o" ).file_name().unwrap() ),
            out_path.join( this.src.path.with_extension( "" ).file_name().unwrap() ))
        }
        else {
            (this.src.path.with_extension( "asm" ),
            this.src.path.with_extension( "o" ),
            this.src.path.with_extension( "" ))
        };

        let asm_file = match File::create( &asm_path ) {
            Ok( file ) => file,
            Err( err ) => {
                logger.substep( &ASM_GENERATION );
                return Err( IoError {
                    kind: err.kind(),
                    msg: format!( "could not create file '{}'", asm_path.display() ).into(),
                    cause: err.to_string().into(),
                } );
            },
        };

        let mut asm_writer = BufWriter::new( asm_file );

        this.rodata += &format!(
r#" stdout: equ 1
 stderr: equ 2
 SYS_write: equ 1
 SYS_exit: equ 60
 EXIT_SUCCESS: equ 0
 EXIT_FAILURE: equ 1

 CRASH: db "Crash: "
 CRASH_len: equ $ - CRASH

 _AT: db "at: "
 _AT_len: equ $ - _AT

 attempt_division_by_zero: db "attempt to divide by zero", 10
 attempt_division_by_zero_len: equ $ - attempt_division_by_zero

 attempt_modulo_zero: db "attempt to take the modulo zero of a number", 10
 attempt_modulo_zero_len: equ $ - attempt_modulo_zero

 attempt_exponent_negative: db "attempt to raise a number to a negative power", 10
 attempt_exponent_negative_len: equ $ - attempt_exponent_negative

 file: db "{}:"
 file_len: equ $ - file

 INT_MIN: equ 1 << 63
 INT_MAX: equ ~INT_MIN
 INT_BITS: equ 64

 true: equ 1
 true_str: db "true"
 true_str_len: equ $ - true_str

 false: equ 0
 false_str: db "false"
 false_str_len: equ $ - false_str

 LESS: equ -1
 EQUAL: equ 0
 GREATER: equ 1

 INT_STR_LEN: equ INT_BITS"#,
            src.path.to_str().unwrap()
        );

        let mut stack_size = 0;
        let mut variables: Vec<Vec<&Variable>> = Vec::new();
        for scope in this.ast {
            for variable in &scope.variables {
                let typ = variable.value.typ();

                let mut type_already_encountered = false;
                for vars in &mut variables {
                    if typ == vars[ 0 ].value.typ() {
                        vars.push( variable );
                        type_already_encountered = true;
                        break;
                    }
                }

                if !type_already_encountered {
                    variables.push( vec![variable] );
                }
            }

            while !variables.is_empty() {
                let mut largest_type_bytes = 0;
                let mut current_type = 0;
                for (typ, variables_of_type) in variables.iter().enumerate() {
                    let bytes = variables_of_type[ 0 ].value.typ().size();
                    if bytes > largest_type_bytes {
                        largest_type_bytes = bytes;
                        current_type = typ;
                    }
                }

                for variable in variables.swap_remove( current_type ) {
                    let typ = variable.value.typ();
                    this.variables.push( CompilerVariable { name: variable.name, typ, offset: stack_size } );
                    stack_size += typ.size();
                }
            }
        }

        if stack_size > 0 {
            let misalignment = stack_size % Compiler::STACK_ALIGN;
            let needs_padding = misalignment != 0;
            let padding = needs_padding as usize * (Compiler::STACK_ALIGN - misalignment);
            stack_size += padding;

            this.asm += &format!(
                " push rbp\
                \n sub rsp, {}\
                \n mov rbp, rsp\
                \n\n",
                stack_size
            );
        }

        this.scope( 0 );

        if stack_size > 0 {
            this.asm += &format!(
                " mov rdi, EXIT_SUCCESS\
                \n\
                \nexit:\
                \n add rsp, {}\
                \n pop rbp\n",
                stack_size
            );
        }

        let program = format!(
r"global _start

section .rodata
{}

section .data
 int_str: times INT_STR_LEN db 0

section .text

crash_division_by_zero:
 mov rsi, attempt_division_by_zero
 mov rdx, attempt_division_by_zero_len
 jmp crash

crash_modulo_zero:
 mov rsi, attempt_modulo_zero
 mov rdx, attempt_modulo_zero_len
 jmp crash

crash_exponent_negative:
 mov rsi, attempt_exponent_negative
 mov rdx, attempt_exponent_negative_len
 jmp crash

crash:
 push r8
 push rcx
 push rdx
 push rsi

 mov rdi, stderr
 mov rsi, CRASH
 mov rdx, CRASH_len
 mov rax, SYS_write
 syscall

 ; crash message
 mov rdi, stderr
 pop rsi ; crash message
 pop rdx ; crash message's length
 mov rax, SYS_write
 syscall

 mov rdi, stderr
 mov rsi, _AT
 mov rdx, _AT_len
 mov rax, SYS_write
 syscall

 ; file
 mov rdi, stderr
 mov rsi, file
 mov rdx, file_len
 mov rax, SYS_write
 syscall

 ; line
 pop rdi
 call int_toStr
 mov rdi, stderr
 mov rsi, rax
 mov rax, SYS_write
 syscall

 push ':'
 mov rdi, stderr
 mov rsi, rsp
 mov rdx, 1
 mov rax, SYS_write
 syscall
 pop rsi

 ; column
 pop rdi
 call int_toStr
 mov rdi, stderr
 mov rsi, rax
 mov rax, SYS_write
 syscall

 push 10
 mov rdi, stderr
 mov rsi, rsp
 mov rdx, 1
 mov rax, SYS_write
 syscall
 pop rsi

 mov rdi, EXIT_FAILURE
 jmp exit

int_toStr:
 push rcx

 mov rsi, 10
 mov rcx, int_str + INT_STR_LEN - 1

 mov rax, rdi
 cmp rax, 0
 je .write_zero
 jl .make_number_positive
 jg .next_digit

.write_zero:
 mov byte [rcx], '0'
 jmp .done

.make_number_positive:
 neg rax

.next_digit:
 xor rdx, rdx
 idiv rsi

 add dl, '0'
 mov byte [rcx], dl
 dec rcx

 cmp rax, 0
 jne .next_digit

 cmp rdi, 0
 jl .add_minus_sign
 inc rcx
 jmp .done

.add_minus_sign:
 mov byte [rcx], '-'

.done:
 mov rdx, int_str + INT_STR_LEN
 sub rdx, rcx

 mov rax, rcx
 pop rcx
 ret

 int_pow:
 cmp rsi, 0
 jl crash_exponent_negative
 jg .exponent_positive
 mov rax, 1
 ret

 .exponent_positive:
 cmp rsi, 1
 jne .exponent_not_one
 mov rax, rdi
 ret

.exponent_not_one:
 push rsi

 mov rax, rdi
 mov rdx, 1

.next_power:
 cmp rsi, 1
 jle .done

 test rsi, 1
 jnz .exponent_odd

 imul rax, rax
 shr rsi, 1
 jmp .next_power

.exponent_odd:
 imul rdx, rax
 imul rax, rax

 dec rsi
 shr rsi, 1
 jmp .next_power

.done:
 imul rax, rdx

 pop rsi
 ret

_start:
{}
 mov rax, SYS_exit
 syscall",
            this.rodata, this.asm
        );

        if let Err( err ) = asm_writer.write_all( program.as_bytes() ) {
            logger.substep( &ASM_GENERATION );
            return Err( IoError {
                kind: err.kind(),
                msg: "writing assembly file failed".into(),
                cause: err.to_string().into(),
            } );
        }

        if let Err( err ) = asm_writer.flush() {
            logger.substep( &ASM_GENERATION );
            return Err( IoError {
                kind: err.kind(),
                msg: "writing assembly file failed".into(),
                cause: err.to_string().into(),
            } );
        }

        logger.substep( &ASM_GENERATION );


        let nasm_args = ["-felf64", "-gdwarf", asm_path.to_str().unwrap(), "-o", obj_path.to_str().unwrap()];
        match Command::new( "nasm" ).args( nasm_args ).output() {
            Ok( nasm_out ) =>
                if !nasm_out.status.success() {
                    logger.substep( &ASSEMBLER );
                    return Err( IoError {
                        kind: ErrorKind::InvalidData,
                        msg: "nasm assembler failed".into(),
                        cause: String::from_utf8( nasm_out.stderr ).unwrap().into()
                    } );
                },
            Err( err ) => {
                logger.substep( &ASSEMBLER );
                return Err( IoError {
                    kind: err.kind(),
                    msg: "could not create nasm assembler process".into(),
                    cause: err.to_string().into(),
                } );
            },
        }

        logger.substep( &ASSEMBLER );


        let ld_args = [obj_path.to_str().unwrap(), "-o", exe_path.to_str().unwrap()];
        match Command::new( "ld" ).args( ld_args ).output() {
            Ok( ld_out ) =>
                if !ld_out.status.success() {
                    logger.substep( &LINKER );
                    return Err( IoError {
                        kind: ErrorKind::InvalidData,
                        msg: "ld linker failed".into(),
                        cause: String::from_utf8( ld_out.stderr ).unwrap().into()
                    } );
                },
            Err( err ) => {
                logger.substep( &LINKER );
                return Err( IoError {
                    kind: err.kind(),
                    msg: "could not create ld linker process".into(),
                    cause: err.to_string().into(),
                } );
            },
        };

        logger.substep( &LINKER );
        logger.substep_done();

        return Ok( exe_path );
    }
}

// nodes
impl<'src: 'tokens, 'tokens: 'ast, 'ast> Compiler<'src, 'tokens, 'ast> {
    fn node( &mut self, node: &'ast Node<'tokens, 'src> ) {
        match node {
            Node::Print( argument ) => {
                self.asm += &format!( " ; {}\n", node );
                self.expression( argument );

                match argument.typ() {
                    Type::Int  => self.asm +=
                        " mov rsi, 10\
                        \n call int_toStr\
                        \n\
                        \n mov rdi, stdout\
                        \n mov rsi, rax\
                        \n mov rax, SYS_write\
                        \n syscall\n\n",
                    Type::Char => self.asm +=
                        " push rdi\
                        \n mov rdi, stdout\
                        \n mov rsi, rsp\
                        \n mov rdx, 1\
                        \n mov rax, SYS_write\
                        \n syscall\
                        \n pop rsi\n\n",
                    Type::Bool => self.asm +=
                        " cmp rdi, true\
                        \n mov rsi, true_str\
                        \n mov rdi, false_str\
                        \n cmovne rsi, rdi\
                        \n mov rdx, true_str_len\
                        \n mov rdi, false_str_len\
                        \n cmovne rdx, rdi\
                        \n\
                        \n mov rdi, stdout\
                        \n mov rax, SYS_write\
                        \n syscall\n\n",
                    Type::Str  => self.asm +=
                        " mov rdi, stdout\
                        \n mov rax, SYS_write\
                        \n syscall\n\n",
                }
            },
            Node::Println( argument ) => {
                self.asm += &format!( " ; {}\n", node );
                if let Some( arg ) = argument {
                    self.expression( arg );

                    match arg.typ() {
                        Type::Int  => self.asm +=
                            " mov rsi, 10\
                            \n call int_toStr\
                            \n\
                            \n mov rdi, stdout\
                            \n mov rsi, rax\
                            \n mov rax, SYS_write\
                            \n syscall\n\n",
                        Type::Char => self.asm +=
                            " push rdi\
                            \n mov rdi, stdout\
                            \n mov rsi, rsp\
                            \n mov rdx, 1\
                            \n mov rax, SYS_write\
                            \n syscall\
                            \n pop rsi\n\n",
                        Type::Bool => self.asm +=
                            " cmp rdi, true\
                            \n mov rsi, true_str\
                            \n mov rdi, false_str\
                            \n cmovne rsi, rdi\
                            \n mov rdx, true_str_len\
                            \n mov rdi, false_str_len\
                            \n cmovne rdx, rdi\
                            \n\
                            \n mov rdi, stdout\
                            \n mov rax, SYS_write\
                            \n syscall\n\n",
                        Type::Str  => self.asm +=
                            " mov rdi, stdout\
                            \n mov rax, SYS_write\
                            \n syscall\n\n",
                    }
                }

                self.asm +=
                    " push 10\
                    \n mov rdi, stdout\
                    \n mov rsi, rsp\
                    \n mov rdx, 1\
                    \n mov rax, SYS_write\
                    \n syscall\
                    \n pop rsi\n\n";
            },
            Node::If( if_statement ) => {
                let if_idx = self.if_depth;
                self.if_depth += 1;

                let mut ifs = if_statement.ifs.iter();
                let iff = ifs.next().unwrap();

                // NOTE call ifs.next_back() to get the last else if and match on that instead of
                // checking for the len() of the ifs
                let (has_else_ifs, has_else) = (if_statement.ifs.len() > 1, if_statement.els.is_some());

                // compiling the if branch
                let if_tag = format!( "if_{}", if_idx );
                let (if_false_tag, if_end_tag_idx) = if has_else_ifs {
                    (format!( "if_{}_else_if_0", if_idx ), Some( if_idx ))
                }
                else if has_else {
                    (format!( "if_{}_else", if_idx ), Some( if_idx ))
                }
                else {
                    (format!( "if_{}_end", if_idx ), None)
                };

                self.iff( iff, &if_tag, &if_false_tag );
                if let Some( idx ) = if_end_tag_idx {
                    self.asm += &format!( " jmp if_{}_end\n\n", idx );
                }

                // compiling the else if branches
                if has_else_ifs {
                    let last_else_if = ifs.next_back().unwrap();
                    let else_if_end_tag = format!( " jmp if_{}_end\n\n", if_idx );
                    let mut else_if_tag_idx = 0;

                    for else_if in ifs {
                        let else_if_tag = format!( "if_{}_else_if_{}", if_idx, else_if_tag_idx );
                        let else_if_false_tag = format!( "if_{}_else_if_{}", if_idx, else_if_tag_idx + 1 );

                        self.iff( else_if, &else_if_tag, &else_if_false_tag );
                        self.asm += &else_if_end_tag;
                        else_if_tag_idx += 1;
                    }

                    let else_if_tag = format!( "if_{}_else_if_{}", if_idx, else_if_tag_idx );
                    let else_if_false_tag = if has_else {
                        format!( "if_{}_else", if_idx )
                    }
                    else {
                        format!( "if_{}_end", if_idx )
                    };

                    self.iff( last_else_if, &else_if_tag, &else_if_false_tag );
                    self.asm += &else_if_end_tag;
                }

                // compiling the else branch
                if let Some( els ) = &if_statement.els {
                    self.asm += &format!( "if_{}_else:\n", if_idx );
                    match &**els {
                        Node::Scope( scope ) => self.scope( *scope ),
                        other => self.node( other ),
                    }
                }

                self.asm += &format!( "if_{}_end:\n", if_idx );
            },
            Node::Loop( looop ) => {
                let loop_tag = format!( "loop_{}", self.loop_depth );
                let loop_end_tag = format!( "loop_{}_end", self.loop_depth );

                self.loop_stack.push( self.loop_depth );
                self.loop_depth += 1;
                self.looop( looop, &loop_tag, &loop_end_tag );
                self.loop_stack.pop();

                self.asm += &format!(
                    " jmp {}\
                    \n{}:\n\n",
                    loop_tag,
                    loop_end_tag
                );
            },
            Node::Definition( scope, variable ) => {
                let ast_variable = &self.ast[ *scope ].variables[ *variable ];
                let name = &ast_variable.name;
                let value = &ast_variable.value;
                let variable = self.resolve( ast_variable.name );
                self.assignment( name, value, variable.typ, variable.offset );
            },
            Node::Assignment( scope, variable, new_value ) => {
                let ast_variable = &self.ast[ *scope ].variables[ *variable ];
                let name = &ast_variable.name;
                let variable = self.resolve( ast_variable.name );
                self.assignment( name, new_value, variable.typ, variable.offset );
            },
            Node::Scope( inner )           => self.scope( *inner ),
            Node::Expression( expression ) => self.expression( expression ),
            Node::Break                    => self.asm += &format!( " jmp loop_{}_end\n\n", self.loop_stack[ self.loop_stack.len() - 1 ] ),
            Node::Continue                 => self.asm += &format!( " jmp loop_{}\n\n", self.loop_stack[ self.loop_stack.len() - 1 ] ),
            Node::Empty                    => unreachable!(),
        }
    }

    fn scope( &mut self, scope_idx: usize ) {
        let scope = &self.ast[ scope_idx ];
        for node in &scope.nodes {
            self.node( node );
        }
    }
}

// expressions
impl<'src: 'tokens, 'tokens: 'ast, 'ast> Compiler<'src, 'tokens, 'ast> {
    fn resolve( &self, name: &'src str ) -> &CompilerVariable<'src> {
        for variable in &self.variables {
            if variable.name == name {
                return variable;
            }
        }

        unreachable!();
    }

    fn string_label_idx( &mut self, string: &'ast Vec<u8> ) -> usize {
        let mut string_idx = 0;
        for string_label in &self.strings {
            if std::ptr::eq( string, string_label.string ) {
                return string_idx;
            }
            string_idx += 1;
        }

        let mut string_text = String::with_capacity( string.len() + 2 );
        string_text.push( '`' );
        for ch in string {
            string_text.extend( (*ch as char).escape_default() );
        }
        string_text.push( '`' );

        let label = format!( "str_{}", string_idx );
        let len_label = format!( "str_{}_len", string_idx );

        self.rodata += &format!(
            "\n\n {}: db {}\
            \n {}: equ $ - {}",
            label, string_text,
            len_label, label
        );

        self.strings.push( CompilerString { string, label, len_label } );
        return string_idx;
    }

    fn expression( &mut self, expression: &'ast Expression<'tokens, 'src> ) {
        match expression {
            Expression::Literal( Literal::Int( value ) )  => self.asm += &format!( " mov rdi, {}\n", value ),
            Expression::Literal( Literal::Char( code ) )  => self.asm += &format!( " mov rdi, {}\n", code ),
            Expression::Literal( Literal::Bool( value ) ) => self.asm += &format!( " mov rdi, {}\n", value ),
            Expression::Literal( Literal::Str( string ) ) => {
                let string_label_idx = self.string_label_idx( string );
                let string_label = &self.strings[ string_label_idx ];

                self.asm += &format!(
                    " mov rsi, {}\
                    \n mov rdx, {}\n",
                    string_label.label,
                    string_label.len_label
                );
            },
            Expression::Binary { .. } => self.expression_factor( expression, "rdi" ),
            Expression::Identifier( src_name, _ ) => {
                let src_variable = self.resolve( src_name );
                let src_variable_typ = src_variable.typ;
                let src_variable_offset = src_variable.offset;

                match src_variable_typ {
                    Type::Int =>
                        self.asm += &format!( " mov rdi, [rbp + {}]\n", src_variable_offset ),
                    Type::Char | Type::Bool =>
                        self.asm += &format!( " movzx rdi, byte [rbp + {}]\n", src_variable_offset ),
                    Type::Str =>
                        self.asm += &format!(
                            " mov rsi, [rbp + {}]\
                            \n mov rdx, [rbp + {}]\n",
                            src_variable_offset,
                            src_variable_offset + 8
                        ),
                }
            },
            Expression::Unary { op, operand } => {
                self.expression_factor( operand, "rdi" );
                match op {
                    Op::Not => match operand.typ() {
                        Type::Bool                         => self.asm += " xor dil, 1\n",
                        Type::Int | Type::Char | Type::Str => self.asm += " not rdi\n",
                    },
                    Op::Minus                        => self.asm += " neg rdi\n",
                    _ => unreachable!(),
                }
            },
        }
    }

    fn expression_factor( &mut self, factor: &'ast Expression<'tokens, 'src>, dst: &str ) {
        match factor {
            Expression::Literal( Literal::Int( value ) )  => self.asm += &format!( " mov {}, {}\n", dst, value ),
            Expression::Literal( Literal::Char( code ) )  => self.asm += &format!( " mov {}, {}\n", dst, code ),
            Expression::Literal( Literal::Bool( value ) ) => self.asm += &format!( " mov {}, {}\n", dst, value ),
            Expression::Literal( Literal::Str( string ) ) => {
                let string_label_idx = self.string_label_idx( string );
                let string_label = &self.strings[ string_label_idx ];
                self.asm += &format!( " mov {}, {}\n", dst, string_label.len_label );
            },
            Expression::Binary { lhs, op_col, op, rhs } => {
                let (lhs_reg, rhs_reg, op_asm): (&'static str, &'static str, Cow<'static, str>) = match op {
                    Op::Pow | Op::PowEquals => {
                        let (line, col) = self.src.normalize( **op_col );

                        ("rdi", "rsi",
                        format!(
                            " mov rcx, {line}\
                            \n mov r8, {col}\
                            \n call int_pow\
                            \n mov rdi, rax\n"
                        ).into())
                    },
                    Op::Times | Op::TimesEquals => ("rdi", "rsi",
                        " imul rdi, rsi\n".into()
                    ),
                    Op::Divide | Op::DivideEquals => {
                        let (line, col) = self.src.normalize( **op_col );

                        ("rax", "rdi",
                        format!(
                            " mov rcx, {line}\
                            \n mov r8, {col}\
                            \n test rdi, rdi\
                            \n jz crash_division_by_zero\
                            \n xor rdx, rdx\
                            \n idiv rdi\
                            \n mov rdi, rax\n"
                        ).into())
                    },
                    Op::Remainder | Op::RemainderEquals => {
                        let (line, col) = self.src.normalize( **op_col );

                        ("rax", "rdi",
                        format!(
                            " mov rcx, {line}\
                            \n mov r8, {col}\
                            \n test rdi, rdi\
                            \n jz crash_modulo_zero\
                            \n xor rdx, rdx\
                            \n idiv rdi\
                            \n mov rdi, rdx\n"
                        ).into())
                    },
                    Op::Plus | Op::PlusEquals => ("rdi", "rsi",
                        " add rdi, rsi\n".into()
                    ),
                    Op::Minus | Op::MinusEquals => ("rdi", "rsi",
                        " sub rdi, rsi\n".into()
                    ),
                    Op::EqualsEquals => ("rdi", "rsi",
                        " cmp rdi, rsi\
                        \n mov rdi, false\
                        \n sete dil\n".into()
                    ),
                    Op::NotEquals => ("rdi", "rsi",
                        " cmp rdi, rsi\
                        \n mov rdi, false\
                        \n setne dil\n".into()
                    ),
                    Op::Greater => ("rdi", "rsi",
                        " cmp rdi, rsi\
                        \n mov rdi, false\
                        \n setg dil\n".into()
                    ),
                    Op::GreaterOrEquals => ("rdi", "rsi",
                        " cmp rdi, rsi\
                        \n mov rdi, false\
                        \n setge dil\n".into()
                    ),
                    Op::Less => ("rdi", "rsi",
                        " cmp rdi, rsi\
                        \n mov rdi, false\
                        \n setl dil\n".into()
                    ),
                    Op::LessOrEquals => ("rdi", "rsi",
                        " cmp rdi, rsi\
                        \n mov rdi, false\
                        \n setle dil\n".into()
                    ),
                    Op::Compare => ("rdi", "rsi",
                        " cmp rdi, rsi\
                        \n mov rdi, LESS\
                        \n mov rdx, EQUAL\
                        \n cmove rdi, rdx\
                        \n mov rdx, GREATER\
                        \n cmovg rdi, rdx\n".into()
                    ),
                    // TODO shortcircuit boolean operators
                    Op::And | Op::AndEquals
                    | Op::BitAnd | Op::BitAndEquals => ("rdi", "rsi",
                        " and rdi, rsi\n".into()
                    ),
                    Op::Or | Op::OrEquals
                    | Op::BitOr | Op::BitOrEquals => ("rdi", "rsi",
                        " or rdi, rsi\n".into()
                    ),
                    Op::BitXor | Op::BitXorEquals => ("rdi", "rsi",
                        " xor rdi, rsi\n".into()
                    ),
                    Op::LeftShift | Op::LeftShiftEquals => ("rdi", "rsi",
                        " shl rdi, rsi\n".into()
                    ),
                    Op::RightShift | Op::RightShiftEquals => ("rdi", "rsi",
                        " shr rdi, rsi\n".into()
                    ),
                    Op::Equals | Op::Not => unreachable!(),
                };

                match &**rhs {
                    Expression::Binary { .. } | Expression::Unary { .. } => {
                        self.expression_factor( lhs, lhs_reg );
                        self.asm += " push rdi\n\n";
                        self.expression_factor( rhs, rhs_reg );

                        self.asm += &format!(
                            " mov {}, rdi\
                            \n pop {}\n",
                            rhs_reg,
                            lhs_reg
                        );
                    },
                    _ => {
                        self.expression_factor( lhs, lhs_reg );
                        self.expression_factor( rhs, rhs_reg );
                    }
                }

                self.asm += &format!( "{}\n", op_asm );
            },
            Expression::Identifier( src_name, _ ) => {
                let src_variable = self.resolve( src_name );
                let src_variable_typ = src_variable.typ;
                let src_variable_offset = src_variable.offset;

                match src_variable_typ {
                    Type::Int               => self.asm += &format!( " mov {}, [rbp + {}]\n", dst, src_variable_offset ),
                    Type::Char | Type::Bool => self.asm += &format!( " movzx {}, byte [rbp + {}]\n", dst, src_variable_offset ),
                    Type::Str               => self.asm += &format!( " mov {}, [rbp + {}]\n", dst, src_variable_offset + 8 ),
                }
            },
            Expression::Unary { op, operand } => {
                self.expression_factor( operand, "rdi" );

                match op {
                    Op::Not => match operand.typ() {
                        Type::Bool                         => self.asm += " xor dil, 1\n",
                        Type::Int | Type::Char | Type::Str => self.asm += " not rdi\n",
                    },
                    Op::Minus                        => self.asm += " neg rdi\n",
                    _ => unreachable!(),
                }
            },
        }
    }
}

// ifs and loops
impl<'src: 'tokens, 'tokens: 'ast, 'ast> Compiler<'src, 'tokens, 'ast> {
    fn iff( &mut self, iff: &'ast IfStatement<'tokens, 'src>, tag: &String, false_tag: &String ) {
        self.asm += &format!( "{}:; {}\n", tag, iff.condition );
        self.condition( &iff.condition, false_tag );
        self.node( &iff.statement );
    }

    fn looop( &mut self, looop: &'ast Loop<'tokens, 'src>, tag: &String, false_tag: &String ) {
        self.asm += &format!( "{}:; {}\n", tag, looop );
        match &looop.condition {
            LoopCondition::Pre( condition ) => {
                self.condition( condition, false_tag );
                self.node( &looop.statement );
            },
            LoopCondition::Post( condition ) => {
                /* // NOTE by inverting the jmp instruction and jumping to the start of the loop we can
                avoid compiling an extra jmp instruction:
                    mov rdi, [rbp + 0]
                    mov rsi, 10
                    cmp rdi, rsi
                    jge loop_0_end

                    jmp loop_0
                    loop_0_end:

                what we actually want:
                    mov rdi, [rbp + 0]
                    mov rsi, 10
                    cmp rdi, rsi
                    jl loop_0

                    loop_0_end:
                 */
                self.node( &looop.statement );
                self.condition( condition, false_tag );
            }
        }
    }

    fn condition( &mut self, condition: &'ast Expression<'tokens, 'src>, false_tag: &String ) {
        match condition {
            Expression::Literal( Literal::Bool( value ) ) =>
                self.asm += &format!(
                    " mov dil, {}\
                    \n cmp dil, true\
                    \n jne {}\n\n",
                    *value as usize,
                    false_tag
                ),
            Expression::Literal( Literal::Int( _ ) | Literal::Char( _ ) | Literal::Str( _ ) ) => unreachable!(),
            Expression::Binary { lhs, op, rhs, .. } => {
                match &**rhs {
                    Expression::Binary { .. } | Expression::Unary { .. }=> {
                        self.expression_factor( lhs, "rdi" );
                        self.asm += " push rdi\n\n";
                        self.expression_factor( rhs, "rsi" );
                        self.asm += " mov rsi, rdi\n pop rdi\n";
                    },
                    _ => {
                        self.expression_factor( lhs, "rdi" );
                        self.expression_factor( rhs, "rsi" );
                    }
                }

                match op {
                    Op::EqualsEquals              => self.asm += &format!( " cmp rdi, rsi\n jne {}\n\n", false_tag ),
                    Op::NotEquals                 => self.asm += &format!( " cmp rdi, rsi\n je {}\n\n", false_tag ),
                    Op::Greater                   => self.asm += &format!( " cmp rdi, rsi\n jle {}\n\n", false_tag ),
                    Op::GreaterOrEquals           => self.asm += &format!( " cmp rdi, rsi\n jl {}\n\n", false_tag ),
                    Op::Less                      => self.asm += &format!( " cmp rdi, rsi\n jge {}\n\n", false_tag ),
                    Op::LessOrEquals              => self.asm += &format!( " cmp rdi, rsi\n jg {}\n\n", false_tag ),
                    Op::And | Op::AndEquals => self.asm += &format!( " and rdi, rsi\n jz {}\n\n", false_tag ),
                    Op::Or | Op::OrEquals   => self.asm += &format!( " or rdi, rsi\n jz {}\n\n", false_tag ),
                    // Operator::Xor | Operator::XorEquals => self.asm += &format!( " xor rdi, rsi\n jz {}\n\n", false_tag ),

                    Op::Equals
                    | Op::Pow | Op::PowEquals
                    | Op::Times | Op::TimesEquals
                    | Op::Divide | Op::DivideEquals
                    | Op::Remainder | Op::RemainderEquals
                    | Op::Plus | Op::PlusEquals
                    | Op::Minus | Op::MinusEquals
                    | Op::Compare
                    | Op::Not
                    | Op::BitAnd | Op::BitAndEquals
                    | Op::BitOr | Op::BitOrEquals
                    | Op::BitXor | Op::BitXorEquals
                    | Op::LeftShift | Op::LeftShiftEquals
                    | Op::RightShift | Op::RightShiftEquals => unreachable!(),
                }
            },
            Expression::Identifier( src_name, _ ) => {
                let src_variable = self.resolve( src_name );
                self.asm += &format!(
                    " mov dil, [rbp + {}]\
                    \n cmp dil, true\
                    \n jne {}\n\n",
                    src_variable.offset,
                    false_tag
                )
            },
            Expression::Unary { .. } => {
                self.expression( condition );

                // we can only have boolean expressions at this point, so it's safe to ignore the
                // integer negation case
                self.asm += &format!(
                    " xor dil, 1\
                    \n jz {}\n\n",
                    false_tag
                );
            },
        }
    }
}

// assignments
impl<'src: 'tokens, 'tokens: 'ast, 'ast> Compiler<'src, 'tokens, 'ast> {
    fn assignment( &mut self, name: &'src str, value: &'ast Expression<'tokens, 'src>, typ: Type, offset: usize ) {
        self.asm += &format!( " ; {} = {}\n", name, value );

        match value {
            Expression::Literal( Literal::Int( value ) ) =>
                self.asm += &format!(
                    " mov rdi, {}\
                    \n mov [rbp + {}], rdi\n\n",
                    value, offset
                ),
            Expression::Literal( Literal::Char( code ) ) =>
                self.asm += &format!( " mov byte [rbp + {}], {}\n\n", offset, code ),
            Expression::Literal( Literal::Bool( value ) ) =>
                self.asm += &format!( " mov byte [rbp + {}], {}\n\n", offset, value ),
            Expression::Literal( Literal::Str( string ) ) => {
                let string_label_idx = self.string_label_idx( string );
                let string_label = &self.strings[ string_label_idx ];

                self.asm += &format!(
                    " mov qword [rbp + {}], {}\
                    \n mov qword [rbp + {}], {}\n\n",
                    offset, string_label.label,
                    offset + 8, string_label.len_label
                );
            },
            Expression::Binary { .. } => {
                self.expression( value );

                match typ {
                    Type::Int | Type::Str   => self.asm += &format!( " mov [rbp + {}], rdi\n\n", offset ),
                    Type::Char | Type::Bool => self.asm += &format!( " mov [rbp + {}], dil\n\n", offset ),
                }
            }
            Expression::Identifier( _, _ ) => {
                self.expression( value );

                match typ {
                    Type::Int               => self.asm += &format!( " mov [rbp + {}], rdi\n\n", offset ),
                    Type::Char | Type::Bool => self.asm += &format!( " mov [rbp + {}], dil\n\n", offset ),
                    Type::Str               =>
                        self.asm += &format!(
                            " mov [rbp + {}], rsi\
                            \n mov [rbp + {}], rdx\n\n",
                            offset,
                            offset + 8
                        ),
                }
            },
            Expression::Unary { .. } => {
                self.expression( value );

                match typ {
                    Type::Int | Type::Str   => self.asm += &format!( " mov [rbp + {}], rdi\n\n", offset ),
                    Type::Char | Type::Bool => self.asm += &format!( " mov [rbp + {}], dil\n\n", offset ),
                }
            },
        }
    }
}
