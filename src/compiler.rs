use std::{
    borrow::Cow,
    fs::File,
    io::{BufWriter, ErrorKind, Write},
    path::PathBuf,
    process::Command,
};

use crate::{ast::*, lexer::*, logging::*};

// TODO(stefano): introduce intermediate representation
#[derive(Debug)]
pub(crate) struct Variable<'src: 'tokens, 'tokens: 'ast, 'ast> {
    name: &'src str,
    value: &'ast Expression<'src, 'tokens>,
    offset: usize,
}

#[derive(Debug)]
pub(crate) struct Compiler<'src: 'tokens, 'tokens: 'ast, 'ast> {
    src: &'src SrcFile,
    out_path: &'src Option<PathBuf>,

    ast: &'ast Vec<Scope<'tokens, 'src>>,

    rodata: String,
    asm: String,

    variables: Vec<Variable<'src, 'tokens, 'ast>>,
    strings: Vec<&'ast Vec<u8>>,

    if_counter: usize,
    loop_counter: usize,
    loop_counters: Vec<usize>,
}

// Generation of compilation artifacts (.asm, .o, executable)
impl<'src: 'tokens, 'tokens: 'ast, 'ast> Compiler<'src, 'tokens, 'ast> {
    const STACK_ALIGN: usize = core::mem::size_of::<usize>();

    pub(crate) fn compile(
        src: &'src SrcFile,
        out_path: &Option<PathBuf>,
        ast: &'ast Vec<Scope<'src, 'tokens>>,
        logger: &mut CompilationLogger,
    ) -> Result<PathBuf, IoError> {
        let mut this = Compiler {
            src,
            out_path,
            ast,
            rodata: String::new(),
            asm: String::new(),
            variables: Vec::new(),
            strings: Vec::new(),
            if_counter: 0,
            loop_counter: 0,
            loop_counters: Vec::new(),
        };

        logger.step(&COMPILING, &this.src.path);

        let (asm_path, obj_path, exe_path) = if let Some(out_path) = &this.out_path {
            match std::fs::create_dir_all(out_path) {
                Ok(_) => {}
                Err(err) if err.kind() == ErrorKind::AlreadyExists => {}
                Err(err) => {
                    logger.substep(&ASM_GENERATION);
                    return Err(IoError {
                        kind: err.kind(),
                        msg: format!("could not create output directory '{}'", out_path.display()).into(),
                        cause: err.to_string().into(),
                    });
                }
            }

            (
                out_path.join(this.src.path.with_extension("asm").file_name().unwrap()),
                out_path.join(this.src.path.with_extension("o").file_name().unwrap()),
                out_path.join(this.src.path.with_extension("").file_name().unwrap()),
            )
        } else {
            (this.src.path.with_extension("asm"), this.src.path.with_extension("o"), this.src.path.with_extension(""))
        };

        let asm_file = match File::create(&asm_path) {
            Ok(file) => file,
            Err(err) => {
                logger.substep(&ASM_GENERATION);
                return Err(IoError {
                    kind: err.kind(),
                    msg: format!("could not create file '{}'", asm_path.display()).into(),
                    cause: err.to_string().into(),
                });
            }
        };

        let mut asm_writer = BufWriter::new(asm_file);

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

 attempt_array_index_underflow: db "negative array index", 10
 attempt_array_index_underflow_len: equ $ - attempt_array_index_underflow

 attempt_array_index_overflow: db "array index out of bounds", 10
 attempt_array_index_overflow_len: equ $ - attempt_array_index_overflow

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
 GREATER: equ 1"#,
            src.path.to_str().unwrap()
        );

        if !this.ast.is_empty() {
            // IDEA(stefano): reserve space for the biggest temporary value and reuse as necessary, to allow for stuff like this
            // println 1;
            // println [1, 2, 3];
            // 1; # just a naked value
            // [1, 2, 3]; # just a naked value

            for scope in this.ast {
                for variable in &scope.variables {
                    this.variables.push(Variable {
                        name: variable.name,
                        value: &variable.value,
                        offset: 0, /* placeholder */
                    });
                }
            }

            this.variables.sort_by(|variable_1, variable_2| {
                variable_1.value.typ().size().cmp(&variable_2.value.typ().size()).reverse()
            });

            let mut stack_size = 0;
            for variable in &mut this.variables {
                variable.offset = stack_size;
                stack_size += variable.value.typ().size();
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

            this.scope(0);

            if stack_size > 0 {
                this.asm += &format!(
                    " mov rdi, EXIT_SUCCESS\
                    \n\
                    \nexit:\
                    \n add rsp, {}\
                    \n pop rbp\n",
                    stack_size
                );
            } else {
                this.asm += "\nexit:\
                    \n mov rdi, EXIT_SUCCESS";
            }
        } else {
            this.asm += "exit:\
                \n mov rdi, EXIT_SUCCESS";
        }

        let program = format!(
            r"global _start

section .text
_start:
{}
 mov rax, SYS_exit
 syscall


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

crash_array_index_underflow:
 mov rsi, attempt_array_index_underflow
 mov rdx, attempt_array_index_underflow_len
 jmp crash

crash_array_index_overflow:
 mov rsi, attempt_array_index_overflow
 mov rdx, attempt_array_index_overflow_len
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

memcopy:
 test rdx, rdx
 jz .done
 mov al, [rsi]
 mov [rdi], al
 inc rdi
 inc rsi
 dec rdx
 jmp memcopy

.done:
 ret

int_toStr:
 push rcx

 mov rsi, 10
 mov rcx, int_str + INT_BITS - 1

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
 mov rdx, int_str + INT_BITS
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

char_print:
 mov rsi, rdi
 mov rdi, stdout
 mov rdx, 1
 mov rax, SYS_write
 syscall
 ret

int_print:
 mov rdi, [rdi]
 call int_toStr
 mov rdi, stdout
 mov rsi, rax
 mov rax, SYS_write
 syscall
 ret

bool_print:
 movzx rdi, byte [rdi]
 cmp rdi, true
 mov rsi, true_str
 mov rdi, false_str
 cmovne rsi, rdi
 mov rdx, true_str_len
 mov rdi, false_str_len
 cmovne rdx, rdi
 mov rdi, stdout
 mov rax, SYS_write
 syscall
 ret

str_print:
 mov rsi, [rdi]
 mov rdx, [rdi + 8]
 mov rdi, stdout
 mov rax, SYS_write
 syscall
 ret


array_debug_print:
 push r12
 push r13
 push r14
 push r15

 mov r12, [rdi]
 lea r13, [rdi + 8]
 mov r14, rsi
 mov r15, rdx

 push '['
 mov rdi, stdout
 mov rsi, rsp
 mov rdx, 1
 mov rax, SYS_write
 syscall
 pop rsi

 test r12, r12
 jz .done

 dec r12
 jz .last

.next:
 mov rdi, r13
 call r15

 push ','
 mov rdi, stdout
 mov rsi, rsp
 mov rdx, 1
 mov rax, SYS_write
 syscall
 pop rsi

 push ' '
 mov rdi, stdout
 mov rsi, rsp
 mov rdx, 1
 mov rax, SYS_write
 syscall
 pop rsi

 add r13, r14
 dec r12
 jnz .next

.last:
 mov rdi, r13
 call r15

.done:
 push ']'
 mov rdi, stdout
 mov rsi, rsp
 mov rdx, 1
 mov rax, SYS_write
 syscall
 pop rsi

 pop r15
 pop r14
 pop r13
 pop r12
 ret

section .rodata
{}

section .data
 int_str: times INT_BITS db 0
",
            this.asm, this.rodata
        );

        if let Err(err) = asm_writer.write_all(program.as_bytes()) {
            logger.substep(&ASM_GENERATION);
            return Err(IoError {
                kind: err.kind(),
                msg: "writing assembly file failed".into(),
                cause: err.to_string().into(),
            });
        }

        if let Err(err) = asm_writer.flush() {
            logger.substep(&ASM_GENERATION);
            return Err(IoError {
                kind: err.kind(),
                msg: "writing assembly file failed".into(),
                cause: err.to_string().into(),
            });
        }

        logger.substep(&ASM_GENERATION);

        let nasm_args = ["-felf64", "-gdwarf", asm_path.to_str().unwrap(), "-o", obj_path.to_str().unwrap()];
        match Command::new("nasm").args(nasm_args).output() {
            Ok(nasm_out) => {
                if !nasm_out.status.success() {
                    logger.substep(&ASSEMBLER);
                    return Err(IoError {
                        kind: ErrorKind::InvalidData,
                        msg: "nasm assembler failed".into(),
                        cause: String::from_utf8(nasm_out.stderr).unwrap().into(),
                    });
                }
            }
            Err(err) => {
                logger.substep(&ASSEMBLER);
                return Err(IoError {
                    kind: err.kind(),
                    msg: "could not create nasm assembler process".into(),
                    cause: err.to_string().into(),
                });
            }
        }

        logger.substep(&ASSEMBLER);

        let ld_args = [obj_path.to_str().unwrap(), "-o", exe_path.to_str().unwrap()];
        match Command::new("ld").args(ld_args).output() {
            Ok(ld_out) => {
                if !ld_out.status.success() {
                    logger.substep(&LINKER);
                    return Err(IoError {
                        kind: ErrorKind::InvalidData,
                        msg: "ld linker failed".into(),
                        cause: String::from_utf8(ld_out.stderr).unwrap().into(),
                    });
                }
            }
            Err(err) => {
                logger.substep(&LINKER);
                return Err(IoError {
                    kind: err.kind(),
                    msg: "could not create ld linker process".into(),
                    cause: err.to_string().into(),
                });
            }
        };

        logger.substep(&LINKER);
        logger.substep_done();

        return Ok(exe_path);
    }
}

// nodes
impl<'src: 'tokens, 'tokens: 'ast, 'ast> Compiler<'src, 'tokens, 'ast> {
    fn node(&mut self, node: &'ast Node<'src, 'tokens>) {
        match node {
            Node::Print(argument) => {
                self.asm += &format!(" ; {}\n", node);
                self.print(argument);
            }
            Node::Println(argument) => {
                self.asm += &format!(" ; {}\n", node);
                if let Some(arg) = argument {
                    self.print(arg);
                }

                self.asm += &format!(
                    " push 10\
                    \n mov rdi, stdout\
                    \n mov rsi, rsp\
                    \n mov rdx, {}\
                    \n mov rax, SYS_write\
                    \n syscall\
                    \n pop rsi\n\n",
                    Type::Char.size()
                );
            }
            Node::If(if_statement) => {
                let if_counter = self.if_counter;
                self.if_counter += 1;

                let mut ifs = if_statement.ifs.iter();
                let iff = ifs.next().unwrap();

                // NOTE(stefano): call ifs.next_back() to get the last else if and match on that instead of
                // checking for the len() of the ifs
                let (has_else_ifs, has_else) = (if_statement.ifs.len() > 1, if_statement.els.is_some());

                // compiling the if branch
                let if_tag = format!("if_{}", if_counter);
                let (if_false_tag, if_end_tag_idx) = if has_else_ifs {
                    (format!("if_{}_else_if_0", if_counter), Some(if_counter))
                } else if has_else {
                    (format!("if_{}_else", if_counter), Some(if_counter))
                } else {
                    (format!("if_{}_end", if_counter), None)
                };

                self.iff(iff, &if_tag, &if_false_tag);
                if let Some(idx) = if_end_tag_idx {
                    self.asm += &format!(" jmp if_{}_end\n\n", idx);
                }

                // compiling the else if branches
                if has_else_ifs {
                    let last_else_if = ifs.next_back().unwrap();
                    let else_if_end_tag = format!(" jmp if_{}_end\n\n", if_counter);
                    let mut else_if_tag_idx = 0;

                    for else_if in ifs {
                        let else_if_tag = format!("if_{}_else_if_{}", if_counter, else_if_tag_idx);
                        let else_if_false_tag = format!("if_{}_else_if_{}", if_counter, else_if_tag_idx + 1);

                        self.iff(else_if, &else_if_tag, &else_if_false_tag);
                        self.asm += &else_if_end_tag;
                        else_if_tag_idx += 1;
                    }

                    let else_if_tag = format!("if_{}_else_if_{}", if_counter, else_if_tag_idx);
                    let else_if_false_tag =
                        if has_else { format!("if_{}_else", if_counter) } else { format!("if_{}_end", if_counter) };

                    self.iff(last_else_if, &else_if_tag, &else_if_false_tag);
                    self.asm += &else_if_end_tag;
                }

                // compiling the else branch
                if let Some(els) = &if_statement.els {
                    self.asm += &format!("if_{}_else:\n", if_counter);
                    self.node(els);
                }

                self.asm += &format!("if_{}_end:\n", if_counter);
            }
            Node::Loop(looop) => {
                let loop_tag = format!("loop_{}", self.loop_counter);
                let loop_end_tag = format!("loop_{}_end", self.loop_counter);

                self.loop_counters.push(self.loop_counter);
                self.loop_counter += 1;
                self.looop(looop, &loop_tag, &loop_end_tag);
                self.loop_counters.pop();

                self.asm += &format!(
                    " jmp {}\
                    \n{}:\n\n",
                    loop_tag, loop_end_tag
                );
            }
            Node::Definition(scope, variable) => {
                let ast_variable = &self.ast[*scope].variables[*variable];
                let name = &ast_variable.name;
                let value = &ast_variable.value;

                let variable = self.resolve(ast_variable.name);
                let offset = variable.offset;

                self.asm += &format!(" ; {} = {:?}\n", name, value);
                self.assignment(value, offset);
            }
            Node::Assignment(scope, variable, new_value) => {
                let ast_variable = &self.ast[*scope].variables[*variable];
                let name = &ast_variable.name;

                let variable = self.resolve(ast_variable.name);
                let offset = variable.offset;

                self.asm += &format!(" ; {} = {:?}\n", name, new_value);
                self.assignment(new_value, offset);
            }
            Node::Scope(inner) => self.scope(*inner),
            Node::Expression(expression) => self.expression(expression),
            Node::Break => {
                self.asm += &format!(" jmp loop_{}_end\n\n", self.loop_counters[self.loop_counters.len() - 1])
            }
            Node::Continue => {
                self.asm += &format!(" jmp loop_{}\n\n", self.loop_counters[self.loop_counters.len() - 1])
            }
            Node::Semicolon => unreachable!("should not be present in the ast"),
        }
    }

    fn scope(&mut self, scope_idx: usize) {
        let scope = &self.ast[scope_idx];
        for node in &scope.nodes {
            self.node(node);
        }
    }
}

// expressions
impl<'src: 'tokens, 'tokens: 'ast, 'ast> Compiler<'src, 'tokens, 'ast> {
    fn resolve(&self, name: &'src str) -> &Variable<'src, 'tokens, 'ast> {
        for variable in &self.variables {
            if variable.name == name {
                return variable;
            }
        }

        unreachable!("should always find a variable");
    }

    fn string_label_idx(&mut self, string: &'ast Vec<u8>) -> usize {
        let mut string_idx = 0;
        for string_label in &self.strings {
            if string == *string_label {
                return string_idx;
            }
            string_idx += 1;
        }

        let mut string_text = String::with_capacity(string.len() + 2);
        string_text.push('`');
        for ch in string {
            string_text.extend((*ch as char).escape_default());
        }
        string_text.push('`');

        let label = format!("str_{}", string_idx);
        let len_label = format!("str_{}_len", string_idx);

        self.rodata += &format!(
            "\n\n {}: db {}\
            \n {}: equ $ - {}",
            label, string_text, len_label, label
        );

        self.strings.push(string);
        return string_idx;
    }

    fn expression(&mut self, expression: &'ast Expression<'src, 'tokens>) {
        match expression {
            Expression::Literal(Literal::Int(value)) => self.asm += &format!(" mov rdi, {}\n", value),
            Expression::Literal(Literal::Char(code)) => self.asm += &format!(" mov rdi, {}\n", code),
            Expression::Literal(Literal::Bool(value)) => self.asm += &format!(" mov rdi, {}\n", value),
            Expression::Literal(Literal::Str(string)) => {
                let string_label_idx = self.string_label_idx(string);
                self.asm += &format!(
                    " mov rdi, str_{}\
                    \n mov rsi, str_{}_len\n",
                    string_label_idx, string_label_idx
                );
            }
            Expression::Binary { .. } => self.expression_factor(expression, "rdi"),
            Expression::Identifier(name, typ) => {
                let variable = self.resolve(name);
                match typ {
                    Type::Int => self.asm += &format!(" mov rdi, [rbp + {}]\n", variable.offset),
                    Type::Char | Type::Bool => self.asm += &format!(" movzx rdi, byte [rbp + {}]\n", variable.offset),
                    Type::Str => {
                        self.asm += &format!(
                            " mov rdi, [rbp + {}]\
                            \n mov rsi, [rbp + {}]\n",
                            variable.offset,
                            variable.offset + Type::Int.size()
                        )
                    }
                    Type::Array(_, _) => unreachable!("arrays cannot appear in expressions"),
                    Type::Infer => unreachable!("should have been coerced to a concrete type"),
                }
            }
            Expression::Unary { op, operand } => {
                self.expression_factor(operand, "rdi");
                match op {
                    Op::Not => match operand.typ() {
                        Type::Bool => self.asm += " xor dil, 1\n",
                        Type::Int | Type::Char => self.asm += " not rdi\n",
                        Type::Array(_, _) => unreachable!("cannot invert array values"),
                        Type::Str => unreachable!("cannot invert string values"),
                        Type::Infer => unreachable!("should have been coerced to a concrete type"),
                    },
                    Op::Minus => self.asm += " neg rdi\n",
                    _ => unreachable!("'Not' and 'Minus' are the only unary operators"),
                }
            }
            Expression::Array(array, _) => {
                self.asm += &format!(" mov rdi, {}\n\n", array.len());

                for element in array {
                    self.expression(element);
                }
            }
            Expression::ArrayIndex { .. } => self.expression_factor(expression, "rdi"),
        }
    }

    fn expression_factor(&mut self, factor: &'ast Expression<'src, 'tokens>, dst: &str) {
        match factor {
            Expression::Literal(Literal::Int(value)) => self.asm += &format!(" mov {}, {}\n", dst, value),
            Expression::Literal(Literal::Char(code)) => self.asm += &format!(" mov {}, {}\n", dst, code),
            Expression::Literal(Literal::Bool(value)) => self.asm += &format!(" mov {}, {}\n", dst, value),
            Expression::Literal(Literal::Str(_)) => unreachable!("strings cannot appear in expressions"),
            Expression::Binary { lhs, op_col, op, rhs } => {
                let (lhs_reg, rhs_reg, op_asm): (&'static str, &'static str, Cow<'static, str>) = match op {
                    Op::Pow | Op::PowEquals => {
                        // todo!( "use rax and rdx for line and colum information" );
                        let Position { line, col } = self.src.position(**op_col);

                        (
                            "rdi",
                            "rsi",
                            format!(
                                " mov rcx, {line}\
                            \n mov r8, {col}\
                            \n call int_pow\
                            \n mov rdi, rax\n"
                            )
                            .into(),
                        )
                    }
                    Op::Times | Op::TimesEquals => ("rdi", "rsi", " imul rdi, rsi\n".into()),
                    Op::Divide | Op::DivideEquals => {
                        // todo!( "use rax and rdx for line and colum information" );

                        let Position { line, col } = self.src.position(**op_col);

                        (
                            "rdi",
                            "rsi",
                            format!(
                                " mov rcx, {line}\
                            \n mov r8, {col}\
                            \n test rsi, rsi\
                            \n jz crash_division_by_zero\
                            \n mov rax, rdi\
                            \n xor rdx, rdx\
                            \n idiv rsi\
                            \n mov rdi, rax\n"
                            )
                            .into(),
                        )
                    }
                    Op::Remainder | Op::RemainderEquals => {
                        // todo!( "use rax and rdx for line and colum information" );

                        let Position { line, col } = self.src.position(**op_col);

                        (
                            "rdi",
                            "rsi",
                            format!(
                                " mov rcx, {line}\
                            \n mov r8, {col}\
                            \n test rsi, rsi\
                            \n jz crash_modulo_zero\
                            \n mov rax, rdi\
                            \n xor rdx, rdx\
                            \n idiv rsi\
                            \n mov rdi, rdx\n"
                            )
                            .into(),
                        )
                    }
                    Op::Plus | Op::PlusEquals => ("rdi", "rsi", " add rdi, rsi\n".into()),
                    Op::Minus | Op::MinusEquals => ("rdi", "rsi", " sub rdi, rsi\n".into()),
                    Op::EqualsEquals => (
                        "rdi",
                        "rsi",
                        " cmp rdi, rsi\
                        \n mov rdi, false\
                        \n sete dil\n"
                            .into(),
                    ),
                    Op::NotEquals => (
                        "rdi",
                        "rsi",
                        " cmp rdi, rsi\
                        \n mov rdi, false\
                        \n setne dil\n"
                            .into(),
                    ),
                    Op::Greater => (
                        "rdi",
                        "rsi",
                        " cmp rdi, rsi\
                        \n mov rdi, false\
                        \n setg dil\n"
                            .into(),
                    ),
                    Op::GreaterOrEquals => (
                        "rdi",
                        "rsi",
                        " cmp rdi, rsi\
                        \n mov rdi, false\
                        \n setge dil\n"
                            .into(),
                    ),
                    Op::Less => (
                        "rdi",
                        "rsi",
                        " cmp rdi, rsi\
                        \n mov rdi, false\
                        \n setl dil\n"
                            .into(),
                    ),
                    Op::LessOrEquals => (
                        "rdi",
                        "rsi",
                        " cmp rdi, rsi\
                        \n mov rdi, false\
                        \n setle dil\n"
                            .into(),
                    ),
                    Op::Compare => (
                        "rdi",
                        "rsi",
                        " cmp rdi, rsi\
                        \n mov rdi, LESS\
                        \n mov rdx, EQUAL\
                        \n cmove rdi, rdx\
                        \n mov rdx, GREATER\
                        \n cmovg rdi, rdx\n"
                            .into(),
                    ),
                    // TODO(stefano): shortcircuit boolean operators
                    Op::And | Op::AndEquals | Op::BitAnd | Op::BitAndEquals => ("rdi", "rsi", " and rdi, rsi\n".into()),
                    Op::Or | Op::OrEquals | Op::BitOr | Op::BitOrEquals => ("rdi", "rsi", " or rdi, rsi\n".into()),
                    Op::BitXor | Op::BitXorEquals => ("rdi", "rsi", " xor rdi, rsi\n".into()),
                    Op::LeftShift | Op::LeftShiftEquals => ("rdi", "rsi", " shl rdi, rsi\n".into()),
                    Op::RightShift | Op::RightShiftEquals => ("rdi", "rsi", " shr rdi, rsi\n".into()),
                    Op::Equals => unreachable!("should not be present in the ast"),
                    Op::Not => unreachable!("should only appear in unary expressions"),
                };

                match &**rhs {
                    Expression::Binary { .. } | Expression::Unary { .. } | Expression::ArrayIndex { .. } => {
                        self.expression_factor(lhs, lhs_reg);
                        self.asm += " push rdi\n\n";
                        self.expression_factor(rhs, rhs_reg);

                        self.asm += &format!(
                            " mov {}, rdi\
                            \n pop {}\n",
                            rhs_reg, lhs_reg
                        );
                    }
                    _ => {
                        self.expression_factor(lhs, lhs_reg);
                        self.expression_factor(rhs, rhs_reg);
                    }
                }

                self.asm += &format!("{}\n", op_asm);
            }
            Expression::Identifier(name, typ) => {
                let variable = self.resolve(name);
                match typ {
                    Type::Int => self.asm += &format!(" mov {}, [rbp + {}]\n", dst, variable.offset),
                    Type::Char | Type::Bool => {
                        self.asm += &format!(" movzx {}, byte [rbp + {}]\n", dst, variable.offset)
                    }
                    Type::Array(_, _) => unreachable!("arrays cannot appear in expressions"),
                    Type::Str => unreachable!("strings cannot appear in expressions"),
                    Type::Infer => unreachable!("should have been coerced to a concrete type"),
                }
            }
            Expression::Unary { op, operand } => {
                self.expression_factor(operand, "rdi");
                match op {
                    Op::Not => match operand.typ() {
                        Type::Bool => self.asm += " xor dil, 1\n",
                        Type::Int | Type::Char => self.asm += " not rdi\n",
                        Type::Array(_, _) => unreachable!("cannot invert array values"),
                        Type::Str => unreachable!("cannot invert string values"),
                        Type::Infer => unreachable!("should have been coerced to a concrete type"),
                    },
                    Op::Minus => self.asm += " neg rdi\n",
                    _ => unreachable!("'Not' and 'Minus' are the only unary operators"),
                }
            }
            Expression::Array(_, _) => unreachable!("arrays cannot appear in expressions"),
            Expression::ArrayIndex { array, typ, bracket_col, index } => {
                self.expression(index);

                let variable = self.resolve(array);
                let offset = variable.offset;
                let Position { line, col } = self.src.position(**bracket_col);

                self.asm += &format!(
                    " mov rcx, {line}\
                    \n mov r8, {col}\
                    \n cmp {dst}, 0\
                    \n jl crash_array_index_underflow\
                    \n mov rdx, [rbp + {}]\
                    \n cmp {dst}, rdx\
                    \n jge crash_array_index_overflow\
                    \n imul {dst}, {}\n",
                    offset,
                    typ.size()
                );

                match typ {
                    Type::Int => self.asm += &format!(" mov rdi, [rbp + {} + {} + {dst}]\n", offset, Type::Int.size()),
                    Type::Char | Type::Bool => {
                        self.asm += &format!(" movzx rdi, byte [rbp + {} + {} + {dst}]\n", offset, Type::Int.size())
                    }
                    Type::Str => {
                        self.asm += &format!(
                            " mov rsi, [rbp + {} + {} + {dst} + {}]\
                        \n mov rdi, [rbp + {} + {} + {dst}]\n",
                            offset,
                            Type::Int.size(),
                            Type::Int.size(),
                            offset,
                            Type::Int.size(),
                        )
                    }
                    Type::Array(_, _) => unreachable!("arrays cannot appear in expressions"),
                    Type::Infer => unreachable!("should have been coerced to a concrete type"),
                }
            }
        }
    }
}

// ifs and loops
impl<'src: 'tokens, 'tokens: 'ast, 'ast> Compiler<'src, 'tokens, 'ast> {
    fn iff(&mut self, iff: &'ast IfStatement<'src, 'tokens>, tag: &String, false_tag: &String) {
        self.asm += &format!("{}:; {:?}\n", tag, iff.condition);
        self.condition(&iff.condition, false_tag);
        self.node(&iff.statement);
    }

    fn looop(&mut self, looop: &'ast Loop<'src, 'tokens>, tag: &String, false_tag: &String) {
        self.asm += &format!("{}:; {}\n", tag, looop);
        match &looop.condition {
            LoopCondition::Pre(condition) => {
                self.condition(condition, false_tag);
                self.node(&looop.statement);
            }
            LoopCondition::Post(condition) => {
                // NOTE(stefano): by inverting the jmp instruction and jumping to the start of the loop we can
                // avoid compiling an extra jmp instruction:
                //     mov rdi, [rbp + 0]
                //     mov rsi, 10
                //     cmp rdi, rsi
                //     jge loop_0_end

                //     jmp loop_0
                //     loop_0_end:

                // what we actually want:
                //     mov rdi, [rbp + 0]
                //     mov rsi, 10
                //     cmp rdi, rsi
                //     jl loop_0

                //     loop_0_end:
                //
                self.node(&looop.statement);
                self.condition(condition, false_tag);
            }
        }
    }

    fn condition(&mut self, condition: &'ast Expression<'src, 'tokens>, false_tag: &String) {
        match condition {
            Expression::Literal(Literal::Bool(value)) => {
                self.asm += &format!(
                    " mov dil, {}\
                    \n cmp dil, true\
                    \n jne {}\n\n",
                    *value as usize, false_tag
                )
            }
            Expression::Literal(Literal::Int(_) | Literal::Char(_) | Literal::Str(_)) => {
                unreachable!("non-boolean expressions should not appear here")
            }
            Expression::Binary { lhs, op, rhs, .. } => {
                match &**rhs {
                    Expression::Binary { .. } | Expression::Unary { .. } => {
                        self.expression_factor(lhs, "rdi");
                        self.asm += " push rdi\n\n";
                        self.expression_factor(rhs, "rsi");
                        self.asm += " mov rsi, rdi\n pop rdi\n";
                    }
                    _ => {
                        self.expression_factor(lhs, "rdi");
                        self.expression_factor(rhs, "rsi");
                    }
                }

                match op {
                    Op::EqualsEquals => self.asm += " cmp rdi, rsi\n jne",
                    Op::NotEquals => self.asm += " cmp rdi, rsi\n je",
                    Op::Greater => self.asm += " cmp rdi, rsi\n jle",
                    Op::GreaterOrEquals => self.asm += " cmp rdi, rsi\n jl",
                    Op::Less => self.asm += " cmp rdi, rsi\n jge",
                    Op::LessOrEquals => self.asm += " cmp rdi, rsi\n jg",
                    Op::And | Op::AndEquals => self.asm += " and rdi, rsi\n jz",
                    Op::Or | Op::OrEquals => self.asm += " or rdi, rsi\n jz",

                    Op::Equals
                    | Op::Pow
                    | Op::PowEquals
                    | Op::Times
                    | Op::TimesEquals
                    | Op::Divide
                    | Op::DivideEquals
                    | Op::Remainder
                    | Op::RemainderEquals
                    | Op::Plus
                    | Op::PlusEquals
                    | Op::Minus
                    | Op::MinusEquals
                    | Op::Compare
                    | Op::Not
                    | Op::BitAnd
                    | Op::BitAndEquals
                    | Op::BitOr
                    | Op::BitOrEquals
                    | Op::BitXor
                    | Op::BitXorEquals
                    | Op::LeftShift
                    | Op::LeftShiftEquals
                    | Op::RightShift
                    | Op::RightShiftEquals => unreachable!("non-boolean operators should not appear here"),
                }

                self.asm += &format!(" {}\n\n", false_tag);
            }
            Expression::Identifier(name, _) => {
                let variable = self.resolve(name);
                self.asm += &format!(
                    " mov dil, [rbp + {}]\
                    \n cmp dil, true\
                    \n jne {}\n\n",
                    variable.offset, false_tag
                );
            }
            Expression::Unary { .. } => {
                self.expression(condition);

                // we can only have boolean expressions at this point, so it's safe to ignore the
                // integer negation case
                self.asm += &format!(
                    " xor dil, 1\
                    \n jz {}\n\n",
                    false_tag
                );
            }
            Expression::Array(_, _) => unreachable!("arrays cannot appear in conditions"),
            Expression::ArrayIndex { .. } => {
                self.expression(condition);

                self.asm += &format!(
                    " cmp dil, true\
                    \n jne {}\n\n",
                    false_tag
                );
            }
        }
    }
}

// assignments
impl<'src: 'tokens, 'tokens: 'ast, 'ast> Compiler<'src, 'tokens, 'ast> {
    fn assignment(&mut self, value: &'ast Expression<'src, 'tokens>, offset: usize) {
        match value {
            Expression::Literal(Literal::Int(value)) => {
                self.asm += &format!(
                    " mov rdi, {}\
                    \n mov [rbp + {}], rdi\n\n",
                    value, offset
                )
            }
            Expression::Literal(Literal::Char(code)) => {
                self.asm += &format!(" mov byte [rbp + {}], {}\n\n", offset, code)
            }
            Expression::Literal(Literal::Bool(value)) => {
                self.asm += &format!(" mov byte [rbp + {}], {}\n\n", offset, value)
            }
            Expression::Literal(Literal::Str(string)) => {
                let string_label_idx = self.string_label_idx(string);
                self.asm += &format!(
                    " mov qword [rbp + {}], str_{}\
                    \n mov qword [rbp + {}], str_{}_len\n\n",
                    offset,
                    string_label_idx,
                    offset + Type::Int.size(),
                    string_label_idx
                );
            }
            Expression::Binary { .. } => {
                self.expression(value);
                match value.typ() {
                    Type::Int => self.asm += &format!(" mov [rbp + {}], rdi\n\n", offset),
                    Type::Char | Type::Bool => self.asm += &format!(" mov [rbp + {}], dil\n\n", offset),
                    Type::Array(_, _) => unreachable!("arrays cannot appear in expressions"),
                    Type::Str => unreachable!("strings cannot appear in expressions"),
                    Type::Infer => unreachable!("should have been coerced to a concrete type"),
                }
            }
            Expression::Identifier(name, typ) => match typ {
                Type::Int => {
                    self.expression(value);
                    self.asm += &format!(" mov [rbp + {}], rdi\n\n", offset);
                }
                Type::Char | Type::Bool => {
                    self.expression(value);
                    self.asm += &format!(" mov [rbp + {}], dil\n\n", offset);
                }
                Type::Str => {
                    self.expression(value);
                    self.asm += &format!(
                        " mov [rbp + {}], rdi\
                            \n mov [rbp + {}], rsi\n\n",
                        offset,
                        offset + Type::Int.size()
                    );
                }
                Type::Array(_, typ) => {
                    let array = self.resolve(name);
                    self.asm += &format!(
                        " lea rdi, [rbp + {}]\
                            \n lea rsi, [rbp + {}]\
                            \n mov rdx, [rsi]\
                            \n imul rdx, {}\
                            \n add rdx, {}\
                            \n call memcopy\n\n",
                        offset,
                        array.offset,
                        typ.size(),
                        Type::Int.size(),
                    );
                }
                Type::Infer => unreachable!("should have been coerced to a concrete type"),
            },
            Expression::Unary { .. } => {
                self.expression(value);
                match value.typ() {
                    Type::Int => self.asm += &format!(" mov [rbp + {}], rdi\n\n", offset),
                    Type::Char | Type::Bool => self.asm += &format!(" mov [rbp + {}], dil\n\n", offset),
                    Type::Array(_, _) => unreachable!("cannot invert nor negate array values"),
                    Type::Str => unreachable!("cannot invert nor negate string values"),
                    Type::Infer => unreachable!("should have been coerced to a concrete type"),
                }
            }
            Expression::Array(array, typ) => {
                self.asm += &format!(
                    " mov rdi, {}\
                    \n mov [rbp + {}], rdi\n\n",
                    array.len(),
                    offset
                );

                let len_size = Type::Int.size();
                let typ_size = typ.size();

                for (element_idx, element) in array.iter().enumerate() {
                    let element_offset = len_size + element_idx * typ_size + offset;
                    self.assignment(element, element_offset);
                }
            }
            Expression::ArrayIndex { typ, .. } => {
                self.expression(value);
                match typ {
                    Type::Int => self.asm += &format!("\n mov [rbp + {}], rdi\n\n", offset),
                    Type::Char | Type::Bool => self.asm += &format!("\n mov [rbp + {}], dil\n\n", offset),
                    Type::Str => {
                        self.asm += &format!(
                            " mov [rbp + {}], rdi\
                        \n mov [rbp + {}], rsi\n\n",
                            offset,
                            offset + Type::Int.size()
                        )
                    }
                    Type::Array(_, _) => unreachable!("nested arrays are not supported yet"),
                    Type::Infer => unreachable!("should have been coerced to a concrete type"),
                }
            }
        }
    }
}

// print statements
impl<'src: 'tokens, 'tokens: 'ast, 'ast> Compiler<'src, 'tokens, 'ast> {
    fn print(&mut self, value: &'ast Expression<'src, 'tokens>) {
        match value.typ() {
            Type::Int => {
                self.expression(value);
                self.asm += " call int_toStr\
                    \n mov rdi, stdout\
                    \n mov rsi, rax\
                    \n mov rax, SYS_write\
                    \n syscall\n\n";
            }
            Type::Char => {
                self.expression(value);
                self.asm += " push rdi\
                    \n mov rdi, stdout\
                    \n mov rsi, rsp\
                    \n mov rdx, 1\
                    \n mov rax, SYS_write\
                    \n syscall\
                    \n pop rsi\n\n";
            }
            Type::Bool => {
                self.expression(value);
                self.asm += " cmp dil, true\
                    \n mov rsi, true_str\
                    \n mov rdi, false_str\
                    \n cmovne rsi, rdi\
                    \n mov rdx, true_str_len\
                    \n mov rdi, false_str_len\
                    \n cmovne rdx, rdi\
                    \n mov rdi, stdout\
                    \n mov rax, SYS_write\
                    \n syscall\n\n";
            }
            Type::Str => {
                self.expression(value);
                self.asm += " mov rdx, rsi\
                    \n mov rsi, rdi\
                    \n mov rdi, stdout\
                    \n mov rax, SYS_write\
                    \n syscall\n\n";
            }
            Type::Array(_, typ) => match value {
                Expression::Identifier(src_name, _) => {
                    let variable = self.resolve(src_name);
                    self.asm += &format!(
                        " lea rdi, [rbp + {}]\
                        \n mov rsi, {}\n",
                        variable.offset,
                        typ.size()
                    );

                    match &*typ {
                        Type::Int => self.asm += " mov rdx, int_print\n",
                        Type::Char => self.asm += " mov rdx, char_print\n",
                        Type::Bool => self.asm += " mov rdx, bool_print\n",
                        Type::Str => self.asm += " mov rdx, str_print\n",
                        Type::Array(_, _) => unreachable!("nested arrays are not supported yet"),
                        Type::Infer => unreachable!("should have been coerced to a concrete type"),
                    }

                    self.asm += " call array_debug_print\n\n";
                }
                _ => unreachable!("only array variables can appear here"),
            },
            Type::Infer => unreachable!("should have been coerced to a concrete type"),
        }
    }
}
