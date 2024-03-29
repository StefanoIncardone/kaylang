use crate::{
    ast::{Expression, IfStatement, LoopCondition, Node, Scope, Type, TypeOf},
    error::{BackEndError, BackEndErrorInfo, BackEndErrorKind, ErrorInfo},
    tokenizer::{Literal, Op},
};
use std::{
    borrow::Cow,
    fs::File,
    io::{self, BufWriter, Write},
    path::{Path, PathBuf},
};

// TODO(stefano): introduce intermediate representation
#[derive(Debug)]
struct Variable<'src, 'ast: 'src> {
    name: &'src str,
    value: &'ast Expression<'src>,
    offset: usize,
}

const STACK_ALIGN: usize = core::mem::size_of::<usize>();

#[derive(Debug)]
pub struct Compiler<'src, 'ast: 'src> {
    ast: &'ast [Scope<'src>],

    rodata: String,
    asm: String,

    variables: Vec<Variable<'src, 'ast>>,
    strings: Vec<&'ast Vec<u8>>,

    if_counter: usize,
    loop_counter: usize,
    loop_counters: Vec<usize>,
}

// Generation of compilation artifacts (.asm, .o, executable)
impl<'src, 'ast: 'src> Compiler<'src, 'ast> {
    pub fn compile(
        src_path: &'src Path,
        out_path: Option<&'src Path>,
        ast: &'ast [Scope<'src>],
    ) -> Result<(PathBuf, PathBuf, PathBuf), Error> {
        let (asm_path, obj_path, exe_path) = {
            let mut asm_path = src_path.with_extension("asm");
            let mut obj_path = src_path.with_extension("o");
            let mut exe_path = src_path.with_extension("");

            if let Some(out_path) = out_path {
                match std::fs::create_dir_all(out_path) {
                    Ok(()) => {}
                    Err(err) if err.kind() == io::ErrorKind::AlreadyExists => {}
                    Err(err) => {
                        return Err(BackEndError {
                            kind: ErrorKind::CouldNotCreateOutputDirectory { err, path: out_path.to_path_buf() },
                        });
                    }
                }

                asm_path = out_path.join(asm_path.file_name().unwrap());
                obj_path = out_path.join(obj_path.file_name().unwrap());
                exe_path = out_path.join(exe_path.file_name().unwrap());
            }

            (asm_path, obj_path, exe_path)
        };

        let asm_file = match File::create(&asm_path) {
            Ok(file) => file,
            Err(err) => return Err(BackEndError { kind: ErrorKind::CouldNotCreateFile { err, path: asm_path } }),
        };

        let mut this = Compiler {
            ast,
            rodata: String::new(),
            asm: String::new(),
            variables: Vec::new(),
            strings: Vec::new(),
            if_counter: 0,
            loop_counter: 0,
            loop_counters: Vec::new(),
        };

        this.rodata += &format!(
            r#" stdout: equ 1
 stderr: equ 2
 SYS_write: equ 1
 SYS_exit: equ 60
 EXIT_SUCCESS: equ 0
 EXIT_FAILURE: equ 1

 newline: equ `\n`

 CRASH: db "Crash"
 CRASH_len: equ $ - CRASH

 _AT: db "at"
 _AT_len: equ $ - _AT

 attempt_division_by_zero: db "attempt to divide by zero"
 attempt_division_by_zero_len: equ $ - attempt_division_by_zero

 attempt_modulo_zero: db "attempt to take the modulo zero of a number"
 attempt_modulo_zero_len: equ $ - attempt_modulo_zero

 attempt_exponent_negative: db "attempt to raise a number to a negative power"
 attempt_exponent_negative_len: equ $ - attempt_exponent_negative

 attempt_array_index_underflow: db "negative array index"
 attempt_array_index_underflow_len: equ $ - attempt_array_index_underflow

 attempt_array_index_overflow: db "array index out of bounds"
 attempt_array_index_overflow_len: equ $ - attempt_array_index_overflow

 attempt_int_bit_array_index_underflow: db "negative int bit array index"
 attempt_int_bit_array_index_underflow_len: equ $ - attempt_int_bit_array_index_underflow

 attempt_int_bit_array_index_overflow: db "int bit array index out of bounds"
 attempt_int_bit_array_index_overflow_len: equ $ - attempt_int_bit_array_index_overflow

 file: db "{src_path}:"
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
            src_path = src_path.display()
        );

        if this.ast.is_empty() {
            this.asm += "exit:\
                \n mov rdi, EXIT_SUCCESS";
        } else {
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
                let misalignment = stack_size % STACK_ALIGN;
                let needs_padding = misalignment != 0;
                let padding = usize::from(needs_padding) * (STACK_ALIGN - misalignment);
                stack_size += padding;

                this.asm += &format!(
                    " push rbp\
                    \n sub rsp, {stack_size}\
                    \n mov rbp, rsp\
                    \n\n"
                );
            }

            this.scope(0);

            if stack_size > 0 {
                this.asm += &format!(
                    "\n add rsp, {stack_size}\
                    \n pop rbp\n"
                );
            }

            this.asm += " mov rdi, EXIT_SUCCESS\
                \n\
                \nexit:";
        }

        let program = format!(
            r"global _start

section .text
_start:
{asm}
 mov rax, SYS_exit
 syscall

; fn crash(msg: str @rdi:rsi, line: uint @rdx, col: uint @rcx)
crash:
 push r12
 push r13

 mov r8, rdi; msg_len: uint
 mov r9, rsi; msg_ptr: str*
 mov r12, rdx; line: uint
 mov r13, rcx; col: uint

 mov rdi, CRASH_len
 mov rsi, CRASH
 call str_print

 mov rdi, ':'
 call char_print

 mov rdi, ' '
 call char_print

 ; crash message
 mov rdi, r8
 mov rsi, r9
 call str_print

 mov rdi, newline
 call char_print

 mov rdi, _AT_len
 mov rsi, _AT
 call str_print

 mov rdi, ':'
 call char_print

 mov rdi, ' '
 call char_print

 ; file
 mov rdi, file_len
 mov rsi, file
 call str_print

 ; line
 mov rdi, r12
 call int_to_str
 mov rdi, rdx
 mov rsi, rax
 call str_print

 mov rdi, ':'
 call char_print

 ; column
 mov rdi, r13
 call int_to_str
 mov rdi, rdx
 mov rsi, rax
 call str_print

 mov rdi, newline
 call char_print

 pop r13
 pop r12

 mov rdi, EXIT_FAILURE
 jmp exit

; fn assert_array_idx_in_range(array_len: uint @rdi, idx: int @rsi, line: uint @rdx, col: uint @rcx)
assert_array_idx_in_range:
 cmp rsi, 0
 jl .underflow

 cmp rsi, rdi
 jge .overflow

 ret

.underflow:
 mov rdi, attempt_array_index_underflow_len
 mov rsi, attempt_array_index_underflow
 jmp crash

.overflow:
 mov rdi, attempt_array_index_overflow_len
 mov rsi, attempt_array_index_overflow
 jmp crash

; fn assert_int_bit_array_idx_in_range(array_len: uint @rdi, idx: int @rsi, line: uint @rdx, col: uint @rcx)
assert_int_bit_array_idx_in_range:
 cmp rsi, 0
 jl .underflow

 cmp rsi, rdi
 jge .overflow

 ret

.underflow:
 mov rdi, attempt_int_bit_array_index_underflow_len
 mov rsi, attempt_int_bit_array_index_underflow
 jmp crash

.overflow:
 mov rdi, attempt_int_bit_array_index_overflow_len
 mov rsi, attempt_int_bit_array_index_overflow
 jmp crash

; fn assert_denominator_not_zero(denominator: int @rdi, line: uint @rdx, col: uint @rcx)
assert_denominator_not_zero:
 test rdi, rdi
 jz .denominator_zero

 ret

.denominator_zero:
 mov rdi, attempt_division_by_zero_len
 mov rsi, attempt_division_by_zero
 jmp crash

; fn assert_modulo_not_zero(modulo: int @rdi, line: uint @rdx, col: uint @rcx)
assert_modulo_not_zero:
 test rdi, rdi
 jz .modulo_zero

 ret

.modulo_zero:
 mov rdi, attempt_modulo_zero_len
 mov rsi, attempt_modulo_zero
 jmp crash

; fn assert_exponent_is_positive(exponent: int @rdi, line: uint @rdx, col: uint @rcx)
assert_exponent_is_positive:
 cmp rdi, 0
 jl .exponent_negative

 ret

.exponent_negative:
 mov rdi, attempt_exponent_negative_len
 mov rsi, attempt_exponent_negative
 jmp crash

; fn memcopy(dst: void& @rdi, src: void& @rsi, bytes_to_copy: uint @rdx)
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

; fn int_str: str @rax:rdx = int_to_str(self: int @rdi)
int_to_str:
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
 ret

; fn result: int @rax = int_pow(self: int @rdi, exponent: int @rsi, line: uint @rdx, col: uint @rcx)
int_pow:
 cmp rsi, 0
 jg .exponent_positive
 mov rax, 1
 ret

.exponent_positive:
 cmp rsi, 1
 jne .exponent_not_one
 mov rax, rdi
 ret

.exponent_not_one:
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

 ret

; fn int_print(self: int @rdi)
int_print:
 call int_to_str
 mov rdi, stdout
 mov rsi, rax
 mov rax, SYS_write
 syscall
 ret

; fn int_array_debug_print(self: int[]& @rdi:rsi)
int_array_debug_print:
 mov r8, rdi; len: uint
 mov r9, rsi; array_ptr: int[]*

 mov rdi, '['
 call char_print

 test r8, r8
 jz .done

 dec r8
 jz .last

.next:
 mov rdi, [r9]
 call int_print

 mov rdi, ','
 call char_print

 mov rdi, ' '
 call char_print

 add r9, 8
 dec r8
 jnz .next

.last:
 mov rdi, [r9]
 call int_print

.done:
 mov rdi, ']'
 call char_print

 ret

; fn char_print(self: char @rdi)
char_print:
 push rdi
 mov rsi, rsp
 mov rdi, stdout
 mov rdx, 1
 mov rax, SYS_write
 syscall
 pop rdi
 ret

; fn char_array_debug_print(self: char[]& @rdi:rsi)
char_array_debug_print:
 mov r8, rdi; len: uint
 mov r9, rsi; array_ptr: char[]*

 mov rdi, '['
 call char_print

 test r8, r8
 jz .done

 dec r8
 jz .last

.next:
 movzx rdi, byte [r9]
 call char_print

 mov rdi, ','
 call char_print

 mov rdi, ' '
 call char_print

 inc r9
 dec r8
 jnz .next

.last:
 movzx rdi, byte [r9]
 call char_print

.done:
 mov rdi, ']'
 call char_print

 ret

; fn bool_print(self: bool @rdi)
bool_print:
 cmp dil, true
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

; fn bool_array_debug_print(self: bool[]& @rdi:rsi)
bool_array_debug_print:
 mov r8, rdi; len: uint
 mov r9, rsi; array_ptr: bool[]*

 mov rdi, '['
 call char_print

 test r8, r8
 jz .done

 dec r8
 jz .last

.next:
 movzx rdi, byte [r9]
 call bool_print

 mov rdi, ','
 call char_print

 mov rdi, ' '
 call char_print

 inc r9
 dec r8
 jnz .next

.last:
 movzx rdi, byte [r9]
 call bool_print

.done:
 mov rdi, ']'
 call char_print

 ret

; fn str_print(self: str @rdi:rsi)
str_print:
 mov rdx, rdi
 mov rdi, stdout
 mov rax, SYS_write
 syscall
 ret

; fn str_array_debug_print(self: str[]& @rdi:rsi)
str_array_debug_print:
 mov r8, rdi; len: uint
 mov r9, rsi; array_ptr: str[]*

 mov rdi, '['
 call char_print

 test r8, r8
 jz .done

 dec r8
 jz .last

.next:
 mov rdi, [r9]
 mov rsi, [r9 + 8]
 call str_print

 mov rdi, ','
 call char_print

 mov rdi, ' '
 call char_print

 add r9, 16
 dec r8
 jnz .next

.last:
 mov rdi, [r9]
 mov rsi, [r9 + 8]
 call str_print

.done:
 mov rdi, ']'
 call char_print

 ret


section .rodata
{rodata}

section .data
 int_str: times INT_BITS db 0
",
            asm = this.asm,
            rodata = this.rodata
        );

        let mut asm_writer = BufWriter::new(asm_file);
        if let Err(err) = asm_writer.write_all(program.as_bytes()) {
            return Err(BackEndError { kind: ErrorKind::WritingAssemblyFailed { err } });
        }

        if let Err(err) = asm_writer.flush() {
            return Err(BackEndError { kind: ErrorKind::WritingAssemblyFailed { err } });
        }

        Ok((asm_path, obj_path, exe_path))
    }
}

// nodes
impl<'src, 'ast: 'src> Compiler<'src, 'ast> {
    fn node(&mut self, node: &'ast Node<'src>) {
        match node {
            Node::Print(argument) => {
                self.asm += &format!(" ; {node}\n");
                self.print(argument);
            }
            Node::Println(argument) => {
                self.asm += &format!(" ; {node}\n");
                if let Some(arg) = argument {
                    self.print(arg);
                }

                self.asm += " mov rdi, newline\
                    \n call char_print\n\n";
            }
            Node::If(if_statement) => {
                let if_counter = self.if_counter;
                self.if_counter += 1;

                let mut ifs = if_statement.ifs.iter();
                let first_if = ifs.next().unwrap();

                let has_else_ifs = if_statement.ifs.len() > 1;
                let has_else = if_statement.els.is_some();

                // compiling the if branch
                let if_tag = format!("if_{if_counter}");
                let (if_false_tag, if_end_tag_idx) = if has_else_ifs {
                    (format!("if_{if_counter}_else_if_0"), Some(if_counter))
                } else if has_else {
                    (format!("if_{if_counter}_else"), Some(if_counter))
                } else {
                    (format!("if_{if_counter}_end"), None)
                };

                self.iff(first_if, &if_tag, &if_false_tag);
                if let Some(idx) = if_end_tag_idx {
                    self.asm += &format!(" jmp if_{idx}_end\n\n");
                }

                // compiling the else if branches
                if has_else_ifs {
                    let last_else_if = ifs.next_back().unwrap();
                    let else_if_end_tag = format!(" jmp if_{if_counter}_end\n\n");
                    let mut else_if_tag_idx = 0;

                    for else_if in ifs {
                        let else_if_tag = format!("if_{if_counter}_else_if_{else_if_tag_idx}");
                        let else_if_false_tag = format!(
                            "if_{if_counter}_else_if_{else_if_false_idx}",
                            else_if_false_idx = else_if_tag_idx + 1
                        );

                        self.iff(else_if, &else_if_tag, &else_if_false_tag);
                        self.asm += &else_if_end_tag;
                        else_if_tag_idx += 1;
                    }

                    let else_if_tag = format!("if_{if_counter}_else_if_{else_if_tag_idx}");
                    let else_if_false_tag =
                        if has_else { format!("if_{if_counter}_else") } else { format!("if_{if_counter}_end") };

                    self.iff(last_else_if, &else_if_tag, &else_if_false_tag);
                    self.asm += &else_if_end_tag;
                }

                // compiling the else branch
                if let Some(els) = &if_statement.els {
                    self.asm += &format!("if_{if_counter}_else:\n");
                    self.node(els);
                }

                self.asm += &format!("if_{if_counter}_end:\n");
            }
            Node::Loop(looop) => {
                let loop_tag = format!("loop_{idx}", idx = self.loop_counter);
                let loop_end_tag = format!("loop_{idx}_end", idx = self.loop_counter);

                self.loop_counters.push(self.loop_counter);
                self.loop_counter += 1;

                self.asm += &format!("{loop_tag}:; {looop}\n");
                match &looop.condition {
                    LoopCondition::Loop(condition) => {
                        self.condition(condition, &loop_end_tag);
                        self.node(&looop.statement);

                        self.asm += &format!(
                            " jmp {loop_tag}\
                            \n{loop_end_tag}:\n\n"
                        );
                    }
                    LoopCondition::DoLoop(condition) => {
                        self.node(&looop.statement);
                        self.condition_reversed(condition, &loop_tag);
                    }
                }

                let _ = self.loop_counters.pop();
            }
            Node::Definition(scope, variable) => {
                let ast_variable = &self.ast[*scope].variables[*variable];
                let name = &ast_variable.name;
                let value = &ast_variable.value;

                let variable = self.resolve(ast_variable.name);
                let offset = variable.offset;

                self.asm += &format!(" ; {name} = {value:?}\n");
                self.assignment(value, offset);
            }
            Node::Assignment(scope, variable, new_value) => {
                let ast_variable = &self.ast[*scope].variables[*variable];
                let name = &ast_variable.name;

                let variable = self.resolve(ast_variable.name);
                let offset = variable.offset;

                self.asm += &format!(" ; {name} = {new_value:?}\n");
                self.assignment(new_value, offset);
            }
            Node::Scope(inner) => self.scope(*inner),
            Node::Expression(expression) => self.expression(expression),
            Node::Break => {
                self.asm += &format!(" jmp loop_{idx}_end\n\n", idx = self.loop_counters[self.loop_counters.len() - 1]);
            }
            Node::Continue => {
                self.asm += &format!(" jmp loop_{idx}\n\n", idx = self.loop_counters[self.loop_counters.len() - 1]);
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
impl<'src, 'ast: 'src> Compiler<'src, 'ast> {
    fn resolve(&self, name: &'src str) -> &Variable<'src, 'ast> {
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
            if std::ptr::eq(string, *string_label) {
                return string_idx;
            }
            string_idx += 1;
        }

        self.rodata += &format!(
            "\n\n str_{string_idx}: db `{string_bytes}`\
            \n str_{string_idx}_len: equ $ - str_{string_idx}",
            string_bytes = String::from_utf8(string.clone()).unwrap()
        );

        self.strings.push(string);
        string_idx
    }

    fn expression(&mut self, expression: &'ast Expression<'src>) {
        match expression {
            Expression::Literal(Literal::Int(value)) => self.asm += &format!(" mov rdi, {value}\n"),
            Expression::Literal(Literal::Char(code)) => self.asm += &format!(" mov rdi, {code}\n"),
            Expression::Literal(Literal::Bool(value)) => self.asm += &format!(" mov rdi, {value}\n"),
            Expression::Literal(Literal::Str(string)) => {
                let string_label_idx = self.string_label_idx(string);
                self.asm += &format!(
                    " mov rdi, str_{string_label_idx}_len\
                    \n mov rsi, str_{string_label_idx}\n"
                );
            }
            Expression::Binary { .. } => self.expression_factor(expression, "rdi"),
            Expression::Identifier { name, typ } => {
                let variable = self.resolve(name);
                let offset = variable.offset;
                match typ {
                    Type::Int => self.asm += &format!(" mov rdi, [rbp + {offset}]\n"),
                    Type::Char | Type::Bool => self.asm += &format!(" movzx rdi, byte [rbp + {offset}]\n"),
                    Type::Str => {
                        self.asm += &format!(
                            " mov rdi, [rbp + {offset}]\
                            \n mov rsi, [rbp + {offset} + {ptr_offset}]\n",
                            ptr_offset = Type::Int.size()
                        );
                    }
                    Type::Array { .. } => unreachable!("arrays cannot appear in expressions"),
                    Type::Infer => unreachable!("should have been coerced to a concrete type"),
                }
            }
            Expression::Unary { op, operand } => {
                self.expression_factor(operand, "rdi");
                match op {
                    Op::Not => match operand.typ() {
                        Type::Bool => self.asm += " xor dil, 1\n",
                        Type::Int | Type::Char => self.asm += " not rdi\n",
                        Type::Array { .. } => unreachable!("cannot invert array values"),
                        Type::Str => unreachable!("cannot invert string values"),
                        Type::Infer => unreachable!("should have been coerced to a concrete type"),
                    },
                    Op::Minus => self.asm += " neg rdi\n",
                    _ => unreachable!("'Not' and 'Minus' are the only unary operators"),
                }
            }
            Expression::Array { elements, .. } => {
                for element in elements {
                    self.expression(element);
                }
            }
            Expression::ArrayIndex { .. } => self.expression_factor(expression, "rdi"),
        }
    }

    fn expression_factor(&mut self, factor: &'ast Expression<'src>, dst: &str) {
        match factor {
            Expression::Literal(Literal::Int(value)) => self.asm += &format!(" mov {dst}, {value}\n"),
            Expression::Literal(Literal::Char(code)) => self.asm += &format!(" mov {dst}, {code}\n"),
            Expression::Literal(Literal::Bool(value)) => self.asm += &format!(" mov {dst}, {value}\n"),
            Expression::Literal(Literal::Str(_)) => unreachable!("strings cannot appear in expressions"),
            Expression::Binary { lhs, op_position, op, rhs } => {
                let (lhs_reg, rhs_reg, op_asm): (&str, &str, Cow<'_, str>) = match op {
                    Op::Pow | Op::PowEquals => {
                        // todo!( "use rax and rdx for line and colum information" );
                        (
                            "rdi",
                            "rsi",
                            format!(
                                " push rdi\
                                \n mov rdi, rsi\
                                \n mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call assert_exponent_is_positive\
                                \n pop rdi\
                                \n call int_pow\
                                \n mov rdi, rax\n",
                                line = op_position.line,
                                col = op_position.col
                            )
                            .into(),
                        )
                    }
                    Op::Times | Op::TimesEquals => ("rdi", "rsi", " imul rdi, rsi\n".into()),
                    Op::Divide | Op::DivideEquals => {
                        // todo!( "use rax and rdx for line and colum information" );
                        (
                            "rdi",
                            "rsi",
                            // change the arguments of crash from ptr+len (rsi, rdx) to len+ptr (rdi, rsi)
                            format!(
                                " push rdi\
                                \n mov rdi, rsi\
                                \n mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call assert_denominator_not_zero\
                                \n pop rax\
                                \n xor rdx, rdx\
                                \n idiv rsi\
                                \n mov rdi, rax\n",
                                line = op_position.line,
                                col = op_position.col
                            )
                            .into(),
                        )
                    }
                    Op::Remainder | Op::RemainderEquals => {
                        // todo!( "use rax and rdx for line and colum information" );
                        (
                            "rdi",
                            "rsi",
                            format!(
                                " push rdi\
                                \n mov rdi, rsi\
                                \n mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call assert_modulo_not_zero\
                                \n pop rax\
                                \n xor rdx, rdx\
                                \n idiv rsi\
                                \n mov rdi, rdx\n",
                                line = op_position.line,
                                col = op_position.col,
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
                    Op::LeftShift | Op::LeftShiftEquals => (
                        "rdi",
                        "rsi",
                        " mov cl, sil\
                        \n shl rdi, cl\n"
                            .into(),
                    ),
                    Op::RightShift | Op::RightShiftEquals => (
                        "rdi",
                        "rsi",
                        " mov cl, sil\
                        \n shr rdi, cl\n"
                            .into(),
                    ),
                    Op::Equals => unreachable!("should not be present in the ast"),
                    Op::Not => unreachable!("should only appear in unary expressions"),
                };

                match &**rhs {
                    Expression::Binary { .. } | Expression::Unary { .. } | Expression::ArrayIndex { .. } => {
                        self.expression_factor(lhs, lhs_reg);
                        self.asm += " push rdi\n\n";
                        self.expression_factor(rhs, rhs_reg);

                        self.asm += &format!(
                            " mov {rhs_reg}, rdi\
                            \n pop {lhs_reg}\n"
                        );
                    }
                    _ => {
                        self.expression_factor(lhs, lhs_reg);
                        self.expression_factor(rhs, rhs_reg);
                    }
                }

                self.asm += &format!("{op_asm}\n");
            }
            Expression::Identifier { name, typ } => {
                let variable = self.resolve(name);
                let offset = variable.offset;
                match typ {
                    Type::Int => self.asm += &format!(" mov {dst}, [rbp + {offset}]\n"),
                    Type::Char | Type::Bool => {
                        self.asm += &format!(" movzx {dst}, byte [rbp + {offset}]\n");
                    }
                    Type::Array { .. } => unreachable!("arrays cannot appear in expressions"),
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
                        Type::Array { .. } => unreachable!("cannot invert array values"),
                        Type::Str => unreachable!("cannot invert string values"),
                        Type::Infer => unreachable!("should have been coerced to a concrete type"),
                    },
                    Op::Minus => self.asm += " neg rdi\n",
                    _ => unreachable!("'Not' and 'Minus' are the only unary operators"),
                }
            }
            Expression::Array { .. } => unreachable!("arrays cannot appear in expressions"),
            Expression::ArrayIndex { var_name, element_type, bracket_position, index } => {
                self.expression(index);

                let variable = self.resolve(var_name);
                let offset = variable.offset;
                let variable_value = variable.value;
                let variable_typ = variable_value.typ();

                match variable_typ {
                    Type::Str => {
                        self.asm += &format!(" mov r8, {dst}\n\n");

                        // Note: indexing into an array of strings
                        if let Expression::ArrayIndex { element_type: Type::Str, .. } = variable_value {
                            self.asm += &format!(
                                " mov rdi, [rbp + {offset}]\
                                \n mov rsi, [rbp + {offset} + {ptr_offset}]\n",
                                ptr_offset = Type::Int.size()
                            );
                        } else {
                            self.expression(variable_value);
                        }

                        self.asm += &format!(
                            " push rsi\
                            \n\
                            \n mov rsi, r8\
                            \n mov rdx, {line}\
                            \n mov rcx, {col}\
                            \n call assert_array_idx_in_range\
                            \n pop rdx\
                            \n\
                            \n movzx rdi, byte [rdx + r8]\n",
                            line = bracket_position.line,
                            col = bracket_position.col,
                        );
                    }
                    Type::Array { len, .. } => {
                        self.asm += &format!(
                            " push {dst}\
                            \n\
                            \n mov rsi, {dst}\
                            \n mov rdi, {len}\
                            \n mov rdx, {line}\
                            \n mov rcx, {col}\
                            \n call assert_array_idx_in_range\
                            \n pop rdx\n",
                            line = bracket_position.line,
                            col = bracket_position.col,
                        );

                        match element_type {
                            Type::Int => self.asm += &format!("\n mov rdi, [rbp + {offset} + rdx]\n"),
                            Type::Char | Type::Bool => {
                                self.asm += &format!("\n movzx rdi, byte [rbp + {offset} + rdx]\n");
                            }
                            Type::Str => {
                                self.asm += &format!(
                                    " imul rdx, {typ_size}\
                                    \n\
                                    \n mov rdi, [rbp + {offset} + rdx]\
                                    \n mov rsi, [rbp + {offset} + rdx + {ptr_offset}]\n",
                                    typ_size = element_type.size(),
                                    ptr_offset = Type::Int.size()
                                );
                            }
                            Type::Array { .. } => unreachable!("arrays cannot appear in expressions"),
                            Type::Infer => unreachable!("should have been coerced to a concrete type"),
                        }
                    }
                    Type::Int => match element_type {
                        Type::Int => {
                            self.asm += &format!(
                                " mov rsi, {dst}\
                                \n mov rdi, INT_BITS\
                                \n mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call assert_int_bit_array_idx_in_range\
                                \n\
                                \n mov cl, sil\
                                \n mov rsi, 1\
                                \n shl rsi, cl\
                                \n mov rdi, [rbp + {offset}]\
                                \n and rdi, rsi\
                                \n",
                                line = bracket_position.line,
                                col = bracket_position.col,
                            );
                        }
                        _ => unreachable!("only integers can be treated as bit arrays"),
                    },
                    Type::Bool | Type::Char => {
                        unreachable!("only arrays, strings and integers are allowed in index espressions")
                    }
                    Type::Infer => unreachable!("should have been coerced to a concrete type"),
                }
            }
        }
    }

    fn condition(&mut self, condition: &'ast Expression<'src>, false_tag: &str) {
        match condition {
            Expression::Literal(Literal::Bool(value)) => {
                self.asm += &format!(
                    " mov dil, {bool}\
                    \n cmp dil, true\
                    \n jne {false_tag}\n\n",
                    bool = usize::from(*value)
                );
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
                        self.asm += " mov rsi, rdi\
                                \n pop rdi\n";
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

                self.asm += &format!(" {false_tag}\n\n");
            }
            Expression::Identifier { name, .. } => {
                let variable = self.resolve(name);
                self.asm += &format!(
                    " mov dil, [rbp + {offset}]\
                    \n cmp dil, true\
                    \n jne {false_tag}\n\n",
                    offset = variable.offset
                );
            }
            Expression::Unary { .. } => {
                self.expression(condition);

                // we can only have boolean expressions at this point, so it's safe to ignore the integer negation case
                self.asm += &format!(
                    " xor dil, 1\
                    \n jz {false_tag}\n\n"
                );
            }
            Expression::Array { .. } => unreachable!("arrays cannot appear in conditions"),
            Expression::ArrayIndex { .. } => {
                self.expression(condition);
                self.asm += &format!(
                    " cmp dil, true\
                    \n jne {false_tag}\n\n"
                );
            }
        }
    }

    fn condition_reversed(&mut self, condition: &'ast Expression<'src>, true_tag: &str) {
        match condition {
            Expression::Literal(Literal::Bool(value)) => {
                self.asm += &format!(
                    " mov dil, {bool}\
                    \n cmp dil, true\
                    \n je {true_tag}\n\n",
                    bool = usize::from(*value)
                );
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
                        self.asm += " mov rsi, rdi\
                                \n pop rdi\n";
                    }
                    _ => {
                        self.expression_factor(lhs, "rdi");
                        self.expression_factor(rhs, "rsi");
                    }
                }

                match op {
                    Op::EqualsEquals => self.asm += " cmp rdi, rsi\n je",
                    Op::NotEquals => self.asm += " cmp rdi, rsi\n jne",
                    Op::Greater => self.asm += " cmp rdi, rsi\n jg",
                    Op::GreaterOrEquals => self.asm += " cmp rdi, rsi\n jge",
                    Op::Less => self.asm += " cmp rdi, rsi\n jl",
                    Op::LessOrEquals => self.asm += " cmp rdi, rsi\n jle",
                    Op::And | Op::AndEquals => self.asm += " and rdi, rsi\n jnz",
                    Op::Or | Op::OrEquals => self.asm += " or rdi, rsi\n jnz",

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

                self.asm += &format!(" {true_tag}\n\n");
            }
            Expression::Identifier { name, .. } => {
                let variable = self.resolve(name);
                self.asm += &format!(
                    " mov dil, [rbp + {offset}]\
                    \n cmp dil, true\
                    \n je {true_tag}\n\n",
                    offset = variable.offset
                );
            }
            Expression::Unary { .. } => {
                self.expression(condition);

                // we can only have boolean expressions at this point, so it's safe to ignore the integer negation case
                self.asm += &format!(
                    " xor dil, 1\
                    \n jnz {true_tag}\n\n"
                );
            }
            Expression::Array { .. } => unreachable!("arrays cannot appear in conditions"),
            Expression::ArrayIndex { .. } => {
                self.expression(condition);
                self.asm += &format!(
                    " cmp dil, true\
                    \n je {true_tag}\n\n"
                );
            }
        }
    }
}

// ifs
impl<'src, 'ast: 'src> Compiler<'src, 'ast> {
    fn iff(&mut self, iff: &'ast IfStatement<'src>, tag: &str, false_tag: &str) {
        self.asm += &format!("{tag}:; {condition:?}\n", condition = iff.condition);
        self.condition(&iff.condition, false_tag);
        self.node(&iff.statement);
    }
}

// assignments
impl<'src, 'ast: 'src> Compiler<'src, 'ast> {
    fn assignment(&mut self, value: &'ast Expression<'src>, offset: usize) {
        match value {
            Expression::Literal(Literal::Int(value)) => {
                self.asm += &format!(
                    " mov rdi, {value}\
                    \n mov [rbp + {offset}], rdi\n\n"
                );
            }
            Expression::Literal(Literal::Char(code)) => self.asm += &format!(" mov byte [rbp + {offset}], {code}\n\n"),
            Expression::Literal(Literal::Bool(value)) => {
                self.asm += &format!(" mov byte [rbp + {offset}], {value}\n\n");
            }
            Expression::Literal(Literal::Str(string)) => {
                let string_label_idx = self.string_label_idx(string);
                self.asm += &format!(
                    " mov qword [rbp + {offset}], str_{string_label_idx}_len\
                    \n mov qword [rbp + {offset} + {ptr_offset}], str_{string_label_idx}\n\n",
                    ptr_offset = Type::Int.size()
                );
            }
            Expression::Binary { .. } => {
                self.expression(value);
                match value.typ() {
                    Type::Int => self.asm += &format!(" mov [rbp + {offset}], rdi\n\n"),
                    Type::Char | Type::Bool => self.asm += &format!(" mov [rbp + {offset}], dil\n\n"),
                    Type::Array { .. } => unreachable!("arrays cannot appear in expressions"),
                    Type::Str => unreachable!("strings cannot appear in expressions"),
                    Type::Infer => unreachable!("should have been coerced to a concrete type"),
                }
            }
            Expression::Identifier { name, typ } => match typ {
                Type::Int => {
                    self.expression(value);
                    self.asm += &format!(" mov [rbp + {offset}], rdi\n\n");
                }
                Type::Char | Type::Bool => {
                    self.expression(value);
                    self.asm += &format!(" mov [rbp + {offset}], dil\n\n");
                }
                Type::Str => {
                    self.expression(value);
                    self.asm += &format!(
                        " mov [rbp + {offset}], rdi\
                        \n mov [rbp + {offset} + {ptr_offset}], rsi\n\n",
                        ptr_offset = Type::Int.size()
                    );
                }
                Type::Array { len, elements_type } => {
                    let array = self.resolve(name);
                    self.asm += &format!(
                        " lea rdi, [rbp + {dst_offset}]\
                        \n lea rsi, [rbp + {src_offset}]\
                        \n mov rdx, {len} * {elements_type_size}\
                        \n call memcopy\n\n",
                        dst_offset = offset,
                        src_offset = array.offset,
                        elements_type_size = elements_type.size(),
                    );
                }
                Type::Infer => unreachable!("should have been coerced to a concrete type"),
            },
            Expression::Unary { .. } => {
                self.expression(value);
                match value.typ() {
                    Type::Int => self.asm += &format!(" mov [rbp + {offset}], rdi\n\n"),
                    Type::Char | Type::Bool => self.asm += &format!(" mov [rbp + {offset}], dil\n\n"),
                    Type::Array { .. } => unreachable!("cannot invert nor negate array values"),
                    Type::Str => unreachable!("cannot invert nor negate string values"),
                    Type::Infer => unreachable!("should have been coerced to a concrete type"),
                }
            }
            Expression::Array { elements, elements_type } => {
                let typ_size = elements_type.size();
                for (idx, element) in elements.iter().enumerate() {
                    self.assignment(element, offset + idx * typ_size);
                }
            }
            Expression::ArrayIndex { element_type, .. } => {
                self.expression(value);
                match element_type {
                    Type::Int => self.asm += &format!("\n mov [rbp + {offset}], rdi\n\n"),
                    Type::Char | Type::Bool => self.asm += &format!("\n mov [rbp + {offset}], dil\n\n"),
                    Type::Str => {
                        self.asm += &format!(
                            " mov [rbp + {offset}], rdi\
                            \n mov [rbp + {offset} + {ptr_offset}], rsi\n\n",
                            ptr_offset = Type::Int.size()
                        );
                    }
                    Type::Array { .. } => unreachable!("nested arrays are not supported yet"),
                    Type::Infer => unreachable!("should have been coerced to a concrete type"),
                }
            }
        }
    }
}

// print statements
impl<'src, 'ast: 'src> Compiler<'src, 'ast> {
    fn print(&mut self, value: &'ast Expression<'src>) {
        match value.typ() {
            Type::Int => {
                self.expression(value);
                self.asm += " call int_print\n\n";
            }
            Type::Char => {
                self.expression(value);
                self.asm += " call char_print\n\n";
            }
            Type::Bool => {
                self.expression(value);
                self.asm += " call bool_print\n\n";
            }
            Type::Str => {
                self.expression(value);
                self.asm += " call str_print\n\n";
            }
            Type::Array { len, elements_type } => {
                let variable = match value {
                    Expression::Identifier { name, .. } => self.resolve(name),
                    _ => unreachable!("only array variables can appear here"),
                };

                self.asm += &format!(
                    " mov rdi, {len}\
                    \n lea rsi, [rbp + {offset}]\n",
                    offset = variable.offset
                );

                match &*elements_type {
                    Type::Int => self.asm += " call int_array_debug_print\n\n",
                    Type::Char => self.asm += " call char_array_debug_print\n\n",
                    Type::Bool => self.asm += " call bool_array_debug_print\n\n",
                    Type::Str => self.asm += " call str_array_debug_print\n\n",
                    Type::Array { .. } => unreachable!("nested arrays are not supported yet"),
                    Type::Infer => unreachable!("should have been coerced to a concrete type"),
                }
            }
            Type::Infer => unreachable!("should have been coerced to a concrete type"),
        }
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    NonUtf8Path { path: PathBuf },
    CouldNotCreateOutputDirectory { err: io::Error, path: PathBuf },
    CouldNotCreateFile { err: io::Error, path: PathBuf },
    WritingAssemblyFailed { err: io::Error },
}

impl ErrorInfo for ErrorKind {
    type Info = BackEndErrorInfo;

    fn info(&self) -> Self::Info {
        let (msg, cause) = match self {
            Self::NonUtf8Path { path } => {
                ("invalid path".into(), format!("'{path}' contains non UTF8 characters", path = path.display()).into())
            }
            Self::CouldNotCreateOutputDirectory { err, path } => (
                format!("could not create output directory '{path}'", path = path.display()).into(),
                format!("{err} ({kind})", kind = err.kind()).into(),
            ),
            Self::CouldNotCreateFile { err, path } => (
                format!("could not create file '{path}'", path = path.display()).into(),
                format!("{err} ({kind})", kind = err.kind()).into(),
            ),
            Self::WritingAssemblyFailed { err } => {
                ("writing assembly file failed".into(), format!("{err} ({kind})", kind = err.kind()).into())
            }
        };

        Self::Info { msg, cause }
    }
}

impl BackEndErrorKind for ErrorKind {}

#[deprecated(since = "0.5.3", note = "will be removed to allow for more explicit function signatures")]
pub type Error = BackEndError<ErrorKind>;
