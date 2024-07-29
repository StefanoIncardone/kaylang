// IDEA(stefano): reserve space for the biggest temporary value and reuse as necessary
// IDEA(stefano): have built-in functions return their result in rdi instead of rax

pub mod artifacts;
mod asm;
mod reg;

use self::{artifacts::Artifacts, reg::Reg64};
use crate::{
    src_file::{Position, SrcFile},
    syntax::{
        ast::{
            self, AssignmentOp, Ast, BaseType, BinaryOp, BooleanBinaryOp, ComparisonOp, Expression, IfStatement, Node, ScopeIndex, SizeOf, Type, TypeOf, UnaryOp
        },
        tokenizer::{ascii, uint, RawStr, Str},
    },
    CAUSE, ERROR,
};
use std::{
    borrow::Cow,
    fmt::{Display, Write as _},
    fs::File,
    io::{BufWriter, Write},
    path::PathBuf,
};

#[allow(unused_imports, clippy::enum_glob_use)]
use self::reg::Reg16::*;
#[allow(unused_imports, clippy::enum_glob_use)]
use self::reg::Reg32::*;
#[allow(unused_imports, clippy::enum_glob_use)]
use self::reg::Reg64::*;
#[allow(unused_imports, clippy::enum_glob_use)]
use self::reg::Reg8h::*;
#[allow(unused_imports, clippy::enum_glob_use)]
use self::reg::Reg8l::*;


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Dst {
    Reg(Reg64),
    View { len: Reg64, ptr: Reg64 },
}

impl Dst {
    const fn default(typ: &Type) -> Self {
        return match typ {
            Type::Base(BaseType::Int | BaseType::Ascii | BaseType::Bool) => Self::Reg(Rdi),
            Type::Base(BaseType::Str) | Type::Array { .. } => Self::View { len: Rdi, ptr: Rsi },
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Base {
    Rbp,
    Temp,
}

impl Display for Base {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Rbp => write!(f, "rbp"),
            Self::Temp => write!(f, "temp"),
        }
    }
}

#[derive(Debug)]
struct Variable<'src, 'ast: 'src> {
    inner: &'ast ast::Variable<'src>,
    offset: usize,
}

#[derive(Debug)]
struct TemporaryValue<'src, 'ast: 'src> {
    inner: &'ast Expression<'src>,
    offset: usize,
}

const STACK_ALIGN: usize = std::mem::size_of::<usize>();

#[derive(Debug)]
pub struct Compiler<'src, 'ast: 'src> {
    src: &'src SrcFile,
    ast: &'ast Ast<'src>,

    asm: String,

    variables: Vec<Variable<'src, 'ast>>,
    temporary_values: Vec<TemporaryValue<'src, 'ast>>,

    string_labels: String,
    strings: Vec<&'ast [ascii]>,

    if_counter: usize,

    loop_counter: usize,
    loop_counters: Vec<usize>,
    // and_counter: usize,
    // or_counter: usize,
}

// Generation of compilation artifacts (.asm, .o, executable)
impl<'src, 'ast: 'src> Compiler<'src, 'ast> {
    pub fn compile(
        src: &'src SrcFile,
        ast: &'ast Ast<'src>,
        artifacts: &Artifacts,
    ) -> Result<(), Error> {
        #[allow(clippy::wildcard_imports)]
        use asm::*;

        let asm_file = match File::create(&artifacts.asm_path) {
            Ok(file) => file,
            Err(err) => {
                return Err(Error::CouldNotCreateFile { path: artifacts.asm_path.clone(), err });
            }
        };

        let mut this = Compiler {
            src,
            ast,
            asm: String::new(),
            variables: Vec::with_capacity(ast.variables.len()),
            temporary_values: Vec::with_capacity(ast.temporaries.len()),
            string_labels: String::new(),
            strings: Vec::new(),
            if_counter: 0,
            loop_counter: 0,
            loop_counters: Vec::new(),
            // and_counter: 0,
            // or_counter: 0,
        };

        let mut temporary_values_bytes = 0;

        if this.ast.scopes.is_empty() {
            _ = write!(
                this.asm,
                "exit:\
                \n mov rdi, EXIT_SUCCESS"
            );
        } else {
            for var in this.ast.variables.iter() {
                this.variables.push(Variable { inner: var, offset: 0 /* placeholder */ });
            }

            for var in this.ast.temporaries.iter() {
                this.temporary_values.push(TemporaryValue { inner: var, offset: 0 });

                let var_size = var.typ().size();
                if var_size > temporary_values_bytes {
                    temporary_values_bytes = var_size;
                }
            }

            this.variables.sort_by(|var_1, var_2| {
                return var_1
                    .inner
                    .value
                    .typ()
                    .size()
                    .cmp(&var_2.inner.value.typ().size())
                    .reverse();
            });

            let mut stack_size = 0;
            for var in &mut this.variables {
                var.offset = stack_size;
                stack_size += var.inner.value.typ().size();
            }

            if stack_size > 0 {
                let misalignment = stack_size % STACK_ALIGN;
                let needs_padding = misalignment != 0;
                let padding = usize::from(needs_padding) * (STACK_ALIGN - misalignment);
                stack_size += padding;

                _ = writeln!(
                    this.asm,
                    " push rbp\
                    \n sub rsp, {stack_size}\
                    \n mov rbp, rsp\n"
                );
            }

            this.scope(0);

            if stack_size > 0 {
                _ = writeln!(
                    this.asm,
                    " add rsp, {stack_size}\
                    \n pop rbp\n"
                );
            }

            _ = write!(
                this.asm,
                " mov rdi, EXIT_SUCCESS\
                \n\
                \nexit:"
            );
        }

        let program = format!(
            r#"global _start

section .text
_start:
{asm}
 mov rax, SYS_exit
 syscall

{CRASH_ASM}

{ASSERT_ARRAY_INDEX_IN_RANGE_ASM}

{ASSERT_INT_BIT_INDEX_IN_RANGE_ASM}

{ASSERT_STR_INDEX_IN_RANGE_ASM}

{INT_TO_STR_ASM}

{INT_SAFE_POW_ASM}

{INT_WRAPPING_POW_ASM}

{INT_SATURATING_POW_ASM}

{INT_SAFE_MUL_POW_ASM}

{INT_SAFE_MUL_ASM}

{INT_SATURATING_MUL_ASM}

{INT_SAFE_DIV_ASM}

{INT_WRAPPING_DIV_ASM}

{INT_SATURATING_DIV_ASM}

{INT_SAFE_REMAINDER_ASM}

{INT_SAFE_ADD_ASM}

{INT_SATURATING_ADD_ASM}

{INT_SAFE_ABS_ASM}

{INT_WRAPPING_ABS_ASM}

{INT_SATURATING_ABS_ASM}

{INT_SAFE_SUB_ASM}

{INT_SATURATING_SUB_ASM}

{INT_SAFE_NEGATE_ASM}

{INT_SATURATING_NEGATE_ASM}

{INT_SAFE_LEFT_SHIFT_ASM}

{INT_WRAPPING_LEFT_SHIFT_ASM}

{INT_SATURATING_LEFT_SHIFT_ASM}

{INT_SAFE_RIGHT_SHIFT_ASM}

{INT_SAFE_LEFT_ROTATE_ASM}

{INT_SAFE_RIGHT_ROTATE_ASM}

{INT_PRINT_ASM}

{INT_EPRINT_ASM}

{INT_ARRAY_DEBUG_PRINT_ASM}

{INT_ARRAY_DEBUG_EPRINT_ASM}

{ASCII_PRINT_ASM}

{ASCII_EPRINT_ASM}

{ASCII_ARRAY_DEBUG_PRINT_ASM}

{ASCII_ARRAY_DEBUG_EPRINT_ASM}

{BOOL_PRINT_ASM}

{BOOL_EPRINT_ASM}

{BOOL_ARRAY_DEBUG_PRINT_ASM}

{BOOL_ARRAY_DEBUG_EPRINT_ASM}

{STR_CMP_ASM}

{STR_EQ_ASM}

{STR_ARRAY_CMP_ASM}

{STR_ARRAY_EQ_ASM}

{STR_PRINT_ASM}

{STR_EPRINT_ASM}

{STR_ARRAY_DEBUG_PRINT_ASM}

{STR_ARRAY_DEBUG_EPRINT_ASM}


section .rodata
 %macro str 2
  %1: db %2
  %1_len: equ $ - %1
 %endmacro

 stdout: equ 1
 stderr: equ 2
 SYS_write: equ 1
 SYS_exit: equ 60
 EXIT_SUCCESS: equ 0
 EXIT_FAILURE: equ 1

 newline: equ `\n`

 str CRASH, "Crash"
 str _AT, "at"
 str file, "{src_path}"

 str attempt_division_by_zero, "attempt to divide by zero"
 str attempt_int_min_div_by_minus_one, "attempt to divide the minimum integer value by -1"
 str attempt_remainder_zero, "attempt to take the remainder of a division by zero"
 str attempt_remainder_int_min_div_by_minus_one, "attempt to take the remainder of the minimun integer value divided by -1"
 str attempt_exponent_negative, "attempt to raise an integer to a negative power"
 str attempt_array_index_underflow, "negative array index"
 str attempt_array_index_overflow, "array index out of bounds"
 str attempt_int_bit_index_underflow, "negative int bit array index"
 str attempt_int_bit_index_overflow, "int bit array index out of bounds"
 str attempt_str_index_underflow, "negative string index"
 str attempt_str_index_overflow, "string index out of bounds"
 str attempt_left_shift_negative, "attempting to shift left by a negative quantity"
 str attempt_left_shift_over_6_bits, "attempting to shift left by a quantity over a 6 bit integer"
 str attempt_right_shift_negative, "attempting to shift right by a negative quantity"
 str attempt_right_shift_over_6_bits, "attempting to shift right by a quantity over a 6 bit integer"
 str attempt_left_rotate_negative, "attempting to rotate left by a negative quantity"
 str attempt_left_rotate_over_6_bits, "attempting to rotate left by a quantity over a 6 bit integer"
 str attempt_right_rotate_negative, "attempting to rotate right by a negative quantity"
 str attempt_right_rotate_over_6_bits, "attempting to rotate right by a quantity over a 6 bit integer"
 str pow_overflow, "exponentiation operation resulted in an overflow"
 str mul_overflow, "multiplication operation resulted in an overflow"
 str add_overflow, "add operation resulted in an overflow"
 str abs_overflow, "unary absolute value operation resulted in an overflow"
 str sub_overflow, "subtraction operation resulted in an overflow"
 str negate_overflow, "unary negation operation resulted in an overflow"
 str left_shift_overflow, "left shift operation resulted in an overflow"

 INT_MIN: equ 1 << 63
 INT_MAX: equ ~INT_MIN
 INT_BITS: equ 64

 true: equ 1
 str true_str, "true"

 false: equ 0
 str false_str, "false"

 LESS: equ -1
 EQUAL: equ 0
 GREATER: equ 1

{strings}

section .data
 int_str: times INT_BITS db 0

section .bss
 temp: resb {temporary_values_bytes}
"#,
            asm = this.asm,
            src_path = src.path.display(),
            strings = this.string_labels,
        );

        let mut asm_writer = BufWriter::new(asm_file);
        if let Err(err) = asm_writer.write_all(program.as_bytes()) {
            return Err(Error::WritingAssemblyFailed { err });
        }

        if let Err(err) = asm_writer.flush() {
            return Err(Error::WritingAssemblyFailed { err });
        }

        return Ok(());
    }
}

// nodes
impl<'src, 'ast: 'src> Compiler<'src, 'ast> {
    fn node(&mut self, node: &'ast Node<'src>) {
        match node {
            Node::Print(argument) => {
                _ = writeln!(self.asm, " ; {node}");
                self.print(argument);
            }
            Node::Println(argument) => {
                _ = writeln!(self.asm, " ; {node}");
                if let Some(arg) = argument {
                    self.print(arg);
                }

                _ = writeln!(
                    self.asm,
                    " mov dil, newline\
                    \n call ascii_print\n"
                );
            }
            Node::Eprint(argument) => {
                _ = writeln!(self.asm, " ; {node}");
                self.eprint(argument);
            }
            Node::Eprintln(argument) => {
                _ = writeln!(self.asm, " ; {node}");
                if let Some(arg) = argument {
                    self.eprint(arg);
                }

                _ = writeln!(
                    self.asm,
                    " mov dil, newline\
                    \n call ascii_eprint\n"
                );
            }
            Node::If(if_statement) => {
                let if_counter = self.if_counter;
                self.if_counter += 1;

                let mut ifs = if_statement.ifs.iter();
                let Some(first_if) = ifs.next() else {
                    unreachable!("at least one if block should be present at all times");
                };

                let has_else_ifs = if_statement.ifs.len() > 1;
                let has_else = if_statement.els.is_some();

                // compiling the if branch
                let if_tag = format!("if_{if_counter}");
                let (if_false_tag, if_end_tag_index) = if has_else_ifs {
                    (format!("if_{if_counter}_else_if_0"), Some(if_counter))
                } else if has_else {
                    (format!("if_{if_counter}_else"), Some(if_counter))
                } else {
                    (format!("if_{if_counter}_end"), None)
                };

                self.iff(first_if, &if_tag, &if_false_tag);
                if let Some(index) = if_end_tag_index {
                    _ = writeln!(self.asm, " jmp if_{index}_end\n");
                }

                // compiling the else if branches
                if has_else_ifs {
                    let Some(last_else_if) = ifs.next_back() else {
                        unreachable!("at least one else-if block should be present at all times");
                    };

                    let mut else_if_tag_index = 0;
                    for else_if in ifs {
                        let else_if_tag = format!("if_{if_counter}_else_if_{else_if_tag_index}");
                        let else_if_false_tag =
                            format!("if_{if_counter}_else_if_{}", else_if_tag_index + 1);

                        self.iff(else_if, &else_if_tag, &else_if_false_tag);
                        _ = writeln!(self.asm, " jmp if_{if_counter}_end\n");
                        else_if_tag_index += 1;
                    }

                    let else_if_tag = format!("if_{if_counter}_else_if_{else_if_tag_index}");
                    let else_if_false_tag = if has_else {
                        format!("if_{if_counter}_else")
                    } else {
                        format!("if_{if_counter}_end")
                    };

                    self.iff(last_else_if, &else_if_tag, &else_if_false_tag);
                    _ = writeln!(self.asm, " jmp if_{if_counter}_end\n");
                }

                // compiling the else branch
                if let Some(els) = &if_statement.els {
                    _ = writeln!(self.asm, "if_{if_counter}_else:");
                    self.node(els);
                }

                _ = writeln!(self.asm, "if_{if_counter}_end:");
            }
            Node::Loop(looop) => {
                let loop_tag = format!("loop_{}", self.loop_counter);
                let loop_end_tag = format!("loop_{}_end", self.loop_counter);

                self.loop_counters.push(self.loop_counter);
                self.loop_counter += 1;

                _ = writeln!(self.asm, "{loop_tag}:; {looop}");
                self.condition(&looop.condition, &loop_end_tag);
                self.node(&looop.statement);

                _ = writeln!(
                    self.asm,
                    " jmp {loop_tag}\
                    \n{loop_end_tag}:\n"
                );

                _ = self.loop_counters.pop();
            }
            Node::DoLoop(looop) => {
                let loop_tag = format!("loop_{}", self.loop_counter);

                self.loop_counters.push(self.loop_counter);
                self.loop_counter += 1;

                _ = writeln!(self.asm, "{loop_tag}:; {looop}");
                self.node(&looop.statement);
                self.condition_reversed(&looop.condition, &loop_tag);
                _ = self.loop_counters.pop();
            }
            Node::Definition { var_index } => {
                let ast_var = &self.ast.variables[*var_index];

                let name = ast_var.name;
                let value = &ast_var.value;

                let var = self.resolve(name);
                let dst_offset = var.offset;

                _ = writeln!(self.asm, " ; {name} = {value}");
                self.definition(value, Base::Rbp, dst_offset);
            }
            Node::Assignment { var_index, op, op_col, new_value } => {
                // Note: assignments are only allowed on mutable variables
                let ast_var = &self.ast.variables[*var_index];
                let name = ast_var.name;

                let var = self.resolve(name);
                let dst_offset = var.offset;

                _ = writeln!(self.asm, " ; {name} {op} {new_value}");
                if let AssignmentOp::Equals = op {
                    self.definition(new_value, Base::Rbp, dst_offset);
                } else {
                    self.expression(new_value, Dst::Reg(Rdi));

                    _ = writeln!(
                        self.asm,
                        " mov rsi, rdi\
                        \n mov rdi, [rbp + {dst_offset}]"
                    );

                    // Note: *op*_equals operators can only take integer-like values
                    match op {
                        AssignmentOp::Pow => {
                            let Position { line, col, .. } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_safe_pow\
                                \n mov rdi, rax",
                            );
                        }
                        AssignmentOp::WrappingPow => {
                            let Position { line, col, .. } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_wrapping_pow\
                                \n mov rdi, rax",
                            );
                        }
                        AssignmentOp::SaturatingPow => {
                            let Position { line, col, .. } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_saturating_pow\
                                \n mov rdi, rax",
                            );
                        }
                        AssignmentOp::Times => {
                            let Position { line, col, .. } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_safe_mul",
                            );
                        }
                        AssignmentOp::WrappingTimes => _ = writeln!(self.asm, " imul rdi, rsi"),
                        AssignmentOp::SaturatingTimes => {
                            _ = writeln!(self.asm, " call int_saturating_mul");
                        }
                        AssignmentOp::Divide => {
                            let Position { line, col, .. } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_safe_div",
                            );
                        }
                        AssignmentOp::WrappingDivide => {
                            let Position { line, col, .. } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_wrapping_div",
                            );
                        }
                        AssignmentOp::SaturatingDivide => {
                            let Position { line, col, .. } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_saturating_div",
                            );
                        }
                        AssignmentOp::Remainder => {
                            let Position { line, col, .. } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_safe_remainder",
                            );
                        }
                        AssignmentOp::Plus => {
                            let Position { line, col, .. } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_safe_add",
                            );
                        }
                        AssignmentOp::WrappingPlus => _ = writeln!(self.asm, " add rdi, rsi"),
                        AssignmentOp::SaturatingPlus => {
                            _ = writeln!(self.asm, " call int_saturating_add");
                        }
                        AssignmentOp::Minus => {
                            let Position { line, col, .. } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_safe_sub",
                            );
                        }
                        AssignmentOp::WrappingMinus => _ = writeln!(self.asm, " sub rdi, rsi"),
                        AssignmentOp::SaturatingMinus => {
                            _ = writeln!(self.asm, " call int_saturating_sub");
                        }
                        AssignmentOp::And | AssignmentOp::BitAnd => {
                            _ = writeln!(self.asm, " and rdi, rsi");
                        }
                        AssignmentOp::Or | AssignmentOp::BitOr => {
                            _ = writeln!(self.asm, " or rdi, rsi");
                        }
                        AssignmentOp::BitXor => _ = writeln!(self.asm, " xor rdi, rsi"),
                        AssignmentOp::LeftShift => {
                            let Position { line, col, .. } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_safe_left_shift",
                            );
                        }
                        AssignmentOp::WrappingLeftShift => {
                            let Position { line, col, .. } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_wrapping_left_shift",
                            );
                        }
                        AssignmentOp::SaturatingLeftShift => {
                            let Position { line, col, .. } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_saturating_left_shift",
                            );
                        }
                        AssignmentOp::RightShift => {
                            let Position { line, col, .. } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_safe_right_shift",
                            );
                        }
                        AssignmentOp::LeftRotate => {
                            let Position { line, col, .. } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_safe_left_rotate",
                            );
                        }
                        AssignmentOp::RightRotate => {
                            let Position { line, col, .. } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_safe_right_rotate",
                            );
                        }
                        AssignmentOp::Equals => unreachable!("handled in the previous branch"),
                    };

                    _ = writeln!(self.asm, "\n mov [rbp + {dst_offset}], rdi\n");
                }
            }
            Node::Scope { index } => self.scope(*index),
            Node::Expression(expression) => {
                _ = writeln!(self.asm, " ; {node}");
                self.expression(expression, Dst::default(&expression.typ()));
                _ = writeln!(self.asm);
            }
            Node::Break => {
                _ = writeln!(
                    self.asm,
                    " jmp loop_{}_end\n",
                    self.loop_counters[self.loop_counters.len() - 1]
                );
            }
            Node::Continue => {
                _ = writeln!(
                    self.asm,
                    " jmp loop_{}\n",
                    self.loop_counters[self.loop_counters.len() - 1]
                );
            }
            Node::Semicolon => unreachable!("should not be present in the ast"),
        }
    }

    fn scope(&mut self, scope_index: ScopeIndex) {
        let scope = &self.ast.scopes[scope_index];
        for node in &scope.nodes {
            self.node(node);
        }
    }
}

// expressions
impl<'src, 'ast: 'src> Compiler<'src, 'ast> {
    fn resolve(&self, name: &'src str) -> &Variable<'src, 'ast> {
        for var in &self.variables {
            if var.inner.name == name {
                return var;
            }
        }

        unreachable!("should always find a variable");
    }

    fn resolve_temporary(&self, value: &'ast Expression<'src>) -> &TemporaryValue<'src, 'ast> {
        for temporary in &self.temporary_values {
            if std::ptr::eq(temporary.inner, value) {
                return temporary;
            }
        }

        unreachable!("should always find a temporary value");
    }

    fn str_index(&mut self, string: &'ast Str) -> usize {
        let mut index = 0;
        for string_label in &self.strings {
            if std::ptr::eq(&*string.0, *string_label) {
                return index;
            }
            index += 1;
        }

        // registering the string if it was not encountered before
        self.strings.push(&string.0);

        let string_str = unsafe { std::str::from_utf8_unchecked(&string.0) };
        let string_chars = string_str.escape_debug();
        _ = writeln!(self.string_labels, " str str_{index}, `{string_chars}`");
        return index;
    }

    fn raw_str_index(&mut self, string: &'ast RawStr<'src>) -> usize {
        let mut index = 0;
        for string_label in &self.strings {
            if std::ptr::eq(string.0, *string_label) {
                return index;
            }
            index += 1;
        }

        // registering the string if it was not encountered before
        self.strings.push(string.0);

        let raw_string_str = unsafe { std::str::from_utf8_unchecked(string.0) };
        _ = writeln!(self.string_labels, " str str_{index}, '{raw_string_str}'");
        return index;
    }

    fn lhs_needs_saving(&self, expression: &'ast Expression<'src>) -> bool {
        return match expression {
            Expression::Array { .. } => {
                unreachable!("arrays cannot appear in expressions");
            }

            // these expressions do not need to save the value of the lhs
            Expression::False
            | Expression::True
            | Expression::Int(_)
            | Expression::Ascii(_)
            | Expression::Str(_)
            | Expression::RawStr(_)
            | Expression::Unary { op: UnaryOp::Not | UnaryOp::WrappingMinus, .. }
            | Expression::BooleanUnary { .. }
            | Expression::Identifier { .. } => false,

            // these expressions need to save the value of the lhs
            Expression::Unary { .. }
            | Expression::Binary { .. }
            | Expression::BooleanBinary { .. }
            | Expression::Comparison { .. }
            | Expression::ArrayIndex { .. } => true,

            Expression::Temporary { temporary_value_index, .. } => {
                let temporary = &self.ast.temporaries[*temporary_value_index];
                self.lhs_needs_saving(temporary)
            },
        }
    }

    fn binary_expression(
        &mut self,
        lhs: &'ast Expression<'src>,
        rhs: &'ast Expression<'src>,
        lhs_dst: Dst,
        rhs_dst: Dst,
    ) {
        self.expression(lhs, lhs_dst);

        if !self.lhs_needs_saving(rhs) {
            self.expression(rhs, rhs_dst);
            return;
        }

        match lhs_dst {
            Dst::Reg(reg) => _ = writeln!(self.asm, " push {reg}\n"),
            Dst::View { len, ptr } => {
                _ = writeln!(
                    self.asm,
                    " push {len}\
                    \n push {ptr}\n"
                );
            }
        }

        self.expression(rhs, lhs_dst);
        match (rhs_dst, lhs_dst) {
            (Dst::Reg(rhs_reg), Dst::Reg(lhs_reg)) => {
                _ = writeln!(
                    self.asm,
                    " mov {rhs_reg}, {lhs_reg}\
                    \n pop {lhs_reg}\n"
                );
            }
            (
                Dst::View { len: rhs_len, ptr: rhs_ptr },
                Dst::View { len: lhs_len, ptr: lhs_ptr },
            ) => {
                _ = writeln!(
                    self.asm,
                    " mov {rhs_len}, {lhs_len}\
                    \n mov {rhs_ptr}, {lhs_ptr}\
                    \n pop {lhs_ptr}\
                    \n pop {lhs_len}\n"
                );
            }
            _ => unreachable!(),
        }
    }

    fn identifier(&mut self, typ: Type, dst: Dst, base: Base, offset: usize) {
        match typ {
            Type::Base(BaseType::Int) => match dst {
                Dst::Reg(reg) => _ = writeln!(self.asm, " mov {reg}, [{base} + {offset}]"),
                Dst::View { .. } => unreachable!(),
            },
            Type::Base(BaseType::Ascii | BaseType::Bool) => match dst {
                Dst::Reg(reg) => {
                    _ = writeln!(self.asm, " movzx {reg}, byte [{base} + {offset}]");
                }
                Dst::View { .. } => unreachable!(),
            },
            Type::Base(BaseType::Str) => match dst {
                Dst::View { len, ptr } => {
                    _ = writeln!(
                        self.asm,
                        " mov {len}, [{base} + {offset}]\
                        \n mov {ptr}, [{base} + {offset} + {ptr_offset}]",
                        ptr_offset = std::mem::size_of::<uint>()
                    );
                }
                Dst::Reg(_) => unreachable!(),
            },
            Type::Array { len: array_len, .. } => match dst {
                Dst::View { len, ptr } => {
                    _ = writeln!(
                        self.asm,
                        " mov {len}, {array_len}\
                        \n lea {ptr}, [{base} + {offset}]"
                    );
                }
                Dst::Reg(_) => unreachable!(),
            },
        }
    }

    fn index(&mut self, base_type: BaseType, value: &'ast Expression<'src>, bracket_col: usize, index: &'ast Expression<'src>) {
        let Position { line, col, .. } = self.src.position(bracket_col);

        match value {
            Expression::Int(integer) => {
                self.expression(index, Dst::Reg(Rdi));
                _ = writeln!(
                    self.asm,
                    " mov rsi, INT_BITS\
                    \n mov rdx, {line}\
                    \n mov rcx, {col}\
                    \n call assert_int_bit_index_in_range\
                    \n mov rsi, 1\
                    \n shlx rsi, rsi, rdi\
                    \n mov rdi, {integer}\
                    \n and rdi, rsi\n"
                );
            },
            Expression::Str(string) => {
                self.expression(index, Dst::Reg(Rdi));
                let str_index = self.str_index(string);
                _ = writeln!(
                    self.asm,
                    " mov rsi, str_{str_index}_len\
                    \n mov rdx, {line}\
                    \n mov rcx, {col}\
                    \n call assert_str_index_in_range\
                    \n movzx rdi, byte [str_{str_index} + rdi]\n",
                );
            },
            Expression::RawStr(string) => {
                self.expression(index, Dst::Reg(Rdi));
                let str_index = self.raw_str_index(string);
                _ = writeln!(
                    self.asm,
                    " mov rsi, str_{str_index}_len\
                    \n mov rdx, {line}\
                    \n mov rcx, {col}\
                    \n call assert_str_index_in_range\
                    \n movzx rdi, byte [str_{str_index} + rdi]\n",
                );
            },
            Expression::Unary { ..  }
            | Expression::Binary { .. }
            | Expression::Comparison { op: ComparisonOp::Compare, .. } => {
                self.expression(index, Dst::Reg(Rdi));
                _ = writeln!(
                    self.asm,
                    " mov rsi, INT_BITS\
                    \n mov rdx, {line}\
                    \n mov rcx, {col}\
                    \n call assert_int_bit_index_in_range\
                    \n mov rsi, 1\
                    \n shlx rsi, rsi, rdi\
                    \n push rsi\n"
                );

                self.expression(value, Dst::Reg(Rdi));

                _ = writeln!(
                    self.asm,
                    " pop rsi\
                    \n and rdi, rsi\n"
                );
            },
            Expression::ArrayIndex {
                base_type: nested_base_type,
                value: nested_value,
                bracket_col: nested_bracket_col,
                index: nested_index
            } => {
                self.index(*nested_base_type, nested_value, *nested_bracket_col, nested_index);
                match nested_base_type {
                    BaseType::Str => {
                        _ = writeln!(
                            self.asm,
                            " push rsi\
                            \n push rdi\n"
                        );
                    }
                    BaseType::Int => _ = writeln!(self.asm, " push rdi\n"),
                    BaseType::Ascii | BaseType::Bool => {
                        unreachable!(
                            "only arrays, strings and integers are allowed in index espressions"
                        )
                    }
                }

                self.expression(index, Dst::Reg(Rdi));

                match base_type {
                    BaseType::Ascii => {
                        _ = writeln!(
                            self.asm,
                            " pop rsi\
                            \n mov rdx, {line}\
                            \n mov rcx, {col}\
                            \n call assert_str_index_in_range\
                            \n pop rsi\
                            \n movzx rdi, byte [rsi + rdi]\n",
                        );
                    }
                    BaseType::Int => {
                        _ = writeln!(
                            self.asm,
                            " mov rsi, INT_BITS\
                            \n mov rdx, {line}\
                            \n mov rcx, {col}\
                            \n call assert_int_bit_index_in_range\
                            \n mov rsi, 1\
                            \n shlx rsi, rsi, rdi\
                            \n pop rdi
                            \n and rdi, rsi\n"
                        );
                    }
                    BaseType::Str | BaseType::Bool => {
                        unreachable!(
                            "only integers and ascii are allowed in nested index espressions"
                        )
                    }
                }
            },
            Expression::Temporary { typ, temporary_value_index } => {
                let temporary_expression = &self.ast.temporaries[*temporary_value_index];
                let temporary = self.resolve_temporary(temporary_expression);
                let temporary_offset = temporary.offset;
                self.definition(temporary_expression, Base::Temp, temporary_offset);

                self.expression(index, Dst::Reg(Rdi));

                let Type::Array { len, .. } = typ else {
                    unreachable!("only arrays should appear in temporaries");
                };

                _ = writeln!(
                    self.asm,
                    " mov rsi, {len}\
                    \n mov rdx, {line}\
                    \n mov rcx, {col}\
                    \n call assert_array_index_in_range"
                );

                match base_type {
                    BaseType::Int => {
                        _ = writeln!(
                            self.asm,
                            " mov rdi, [temp + {temporary_offset} + rdi * 8]\n"
                        );
                    }
                    BaseType::Str => {
                        _ = writeln!(
                            self.asm,
                            " imul rdi, {base_type_size}\
                            \n mov rsi, [temp + {temporary_offset} + {ptr_offset} + rdi]\
                            \n mov rdi, [temp + {temporary_offset} + rdi]\n",
                            base_type_size = base_type.size(),
                            ptr_offset = std::mem::size_of::<uint>()
                        );
                    }
                    BaseType::Ascii | BaseType::Bool => {
                        unreachable!(
                            "only arrays, strings and integers are allowed in index espressions"
                        )
                    }
                }
            },
            Expression::Identifier { typ, name } => {
                self.expression(index, Dst::Reg(Rdi));

                let var = self.resolve(name);
                let var_offset = var.offset;

                match typ {
                    Type::Base(BaseType::Str) => {
                        _ = writeln!(
                            self.asm,
                            " mov rsi, [rbp + {var_offset}]\
                            \n mov rdx, {line}\
                            \n mov rcx, {col}\
                            \n call assert_str_index_in_range\
                            \n mov rsi, [rbp + {var_offset} + {ptr_offset}]\
                            \n movzx rdi, byte [rsi + rdi]\n",
                            ptr_offset = std::mem::size_of::<uint>()
                        );
                    }
                    Type::Array { len: array_len, .. } => {
                        _ = writeln!(
                            self.asm,
                            " mov rsi, {array_len}\
                            \n mov rdx, {line}\
                            \n mov rcx, {col}\
                            \n call assert_array_index_in_range"
                        );

                        match base_type {
                            BaseType::Int => {
                                _ = writeln!(
                                    self.asm,
                                    " mov rdi, [rbp + {var_offset} + rdi * 8]\n"
                                );
                            }
                            BaseType::Ascii | BaseType::Bool => {
                                _ = writeln!(
                                    self.asm,
                                    " movzx rdi, byte [rbp + {var_offset} + rdi]\n"
                                );
                            }
                            BaseType::Str => {
                                _ = writeln!(
                                    self.asm,
                                    " imul rdi, {base_type_size}\
                                    \n mov rsi, [rbp + {var_offset} + {ptr_offset} + rdi]\
                                    \n mov rdi, [rbp + {var_offset} + rdi]\n",
                                    base_type_size = base_type.size(),
                                    ptr_offset = std::mem::size_of::<uint>()
                                );
                            }
                        }
                    }
                    Type::Base(BaseType::Int) => {
                        _ = writeln!(
                            self.asm,
                            " mov rsi, INT_BITS\
                            \n mov rdx, {line}\
                            \n mov rcx, {col}\
                            \n call assert_int_bit_index_in_range\
                            \n mov rsi, 1\
                            \n shlx rsi, rsi, rdi\
                            \n mov rdi, [rbp + {var_offset}]\
                            \n and rdi, rsi\n"
                        );
                    }
                    Type::Base(BaseType::Ascii | BaseType::Bool) => {
                        unreachable!(
                            "only arrays, strings and integers are allowed in index espressions"
                        )
                    }
                }
            },

            Expression::False
            | Expression::True
            | Expression::Ascii(_)
            | Expression::BooleanUnary { .. }
            | Expression::BooleanBinary { .. }
            | Expression::Comparison { op:
                ComparisonOp::EqualsEquals
                | ComparisonOp::Greater
                | ComparisonOp::GreaterOrEquals
                | ComparisonOp::Less
                | ComparisonOp::LessOrEquals
                | ComparisonOp::NotEquals,
                ..
            } => {
                unreachable!(
                    "only arrays, strings and integers are allowed in index espressions"
                )
            },
            Expression::Array { .. } => unreachable!("only temporary arrays can appear "),
        }
    }

    fn expression(&mut self, factor: &'ast Expression<'src>, dst: Dst) {
        match factor {
            Expression::Int(integer) => match dst {
                Dst::Reg(reg) => _ = writeln!(self.asm, " mov {reg}, {integer}"),
                Dst::View { .. } => unreachable!(),
            },
            Expression::Ascii(code) => match dst {
                Dst::Reg(reg) => _ = writeln!(self.asm, " mov {reg}, {code}"),
                Dst::View { .. } => unreachable!(),
            },
            Expression::True => match dst {
                Dst::Reg(reg) => _ = writeln!(self.asm, " mov {reg}, true"),
                Dst::View { .. } => unreachable!(),
            },
            Expression::False => match dst {
                Dst::Reg(reg) => _ = writeln!(self.asm, " mov {reg}, false"),
                Dst::View { .. } => unreachable!(),
            },
            Expression::Str(string) => match dst {
                Dst::View { len, ptr } => {
                    let index = self.str_index(string);
                    _ = writeln!(
                        self.asm,
                        " mov {len}, str_{index}_len\
                        \n mov {ptr}, str_{index}"
                    );
                }
                Dst::Reg(_) => unreachable!(),
            },
            Expression::RawStr(string) => match dst {
                Dst::View { len, ptr } => {
                    let index = self.raw_str_index(string);
                    _ = writeln!(
                        self.asm,
                        " mov {len}, str_{index}_len\
                        \n mov {ptr}, str_{index}"
                    );
                }
                Dst::Reg(_) => unreachable!(),
            },
            Expression::Array { .. } => unreachable!("arrays cannot appear in expressions"),
            Expression::Unary { op, op_col, operand } => {
                let Dst::Reg(reg) = dst else {
                    unreachable!();
                };

                match op {
                    UnaryOp::Len => match &**operand {
                        Expression::Str(string) => {
                            let index = self.str_index(string);
                            _ = writeln!(self.asm, " mov {reg}, str_{index}_len");
                        }
                        Expression::RawStr(string) => {
                            let index = self.raw_str_index(string);
                            _ = writeln!(self.asm, " mov {reg}, str_{index}_len");
                        }
                        Expression::Array { items, .. } => {
                            _ = writeln!(self.asm, " mov {reg}, {}", items.len());
                        }
                        Expression::Identifier { typ, name } => {
                            let var = self.resolve(name);
                            let var_offset = var.offset;
                            match typ {
                                Type::Base(BaseType::Str) => {
                                    _ = writeln!(self.asm, " mov {reg}, [rbp + {var_offset}]");
                                }
                                Type::Array { len, .. } => {
                                    _ = writeln!(self.asm, " mov {reg}, {len}");
                                }
                                Type::Base(BaseType::Int | BaseType::Ascii | BaseType::Bool) => {
                                    unreachable!("cannot take the length of numerical types")
                                }
                            }
                        }
                        Expression::ArrayIndex { base_type, value, bracket_col, index } => {
                            self.index(*base_type, value, *bracket_col, index);
                            _ = writeln!(self.asm, "mov {reg}, rdi\n");
                        }
                        Expression::False
                        | Expression::True
                        | Expression::Int(_)
                        | Expression::Ascii(_)
                        | Expression::Unary { .. }
                        | Expression::BooleanUnary { .. }
                        | Expression::Binary { .. }
                        | Expression::BooleanBinary { .. }
                        | Expression::Comparison { .. } => {
                            unreachable!("cannot take the length of numerical types")
                        }
                        Expression::Temporary { ..  } => {
                            unreachable!("should not appear in expressions");
                        }
                    },
                    UnaryOp::Not => {
                        self.expression(operand, dst);
                        let Type::Base(BaseType::Int) = operand.typ() else {
                            unreachable!("can only invert integer and ascii values");
                        };

                        _ = writeln!(self.asm, " not {reg}");
                    }
                    UnaryOp::Plus => {
                        self.expression(operand, dst);
                        match operand.typ() {
                            Type::Base(BaseType::Int) => {
                                let Position { line, col, .. } = self.src.position(*op_col);
                                _ = writeln!(
                                    self.asm,
                                    " mov rdx, {line}\
                                    \n mov rcx, {col}\
                                    \n call int_safe_abs",
                                );
                            }
                            Type::Base(BaseType::Ascii | BaseType::Bool | BaseType::Str)
                            | Type::Array { .. } => {
                                unreachable!("cannot take absolute value of non numerical values");
                            }
                        }
                    }
                    UnaryOp::WrappingPlus => {
                        self.expression(operand, dst);
                        match operand.typ() {
                            Type::Base(BaseType::Int) => {
                                _ = writeln!(self.asm, " call int_wrapping_abs");
                            }
                            Type::Base(BaseType::Ascii | BaseType::Bool | BaseType::Str)
                            | Type::Array { .. } => {
                                unreachable!("cannot take absolute value of non int values");
                            }
                        }
                    }
                    UnaryOp::SaturatingPlus => {
                        self.expression(operand, dst);
                        match operand.typ() {
                            Type::Base(BaseType::Int) => {
                                let Position { line, col, .. } = self.src.position(*op_col);
                                _ = writeln!(
                                    self.asm,
                                    " mov rdx, {line}\
                                    \n mov rcx, {col}\
                                    \n call int_saturating_abs",
                                );
                            }
                            Type::Base(BaseType::Ascii | BaseType::Bool | BaseType::Str)
                            | Type::Array { .. } => {
                                unreachable!("cannot take absolute value of non int values");
                            }
                        }
                    }
                    UnaryOp::Minus => {
                        self.expression(operand, dst);
                        match operand.typ() {
                            Type::Base(BaseType::Int | BaseType::Ascii) => {
                                let Position { line, col, .. } = self.src.position(*op_col);
                                _ = writeln!(
                                    self.asm,
                                    " mov rdx, {line}\
                                    \n mov rcx, {col}\
                                    \n call int_safe_negate",
                                );
                            }
                            Type::Base(BaseType::Bool | BaseType::Str) | Type::Array { .. } => {
                                unreachable!("cannot negate non int/ascii values");
                            }
                        }
                    }
                    UnaryOp::WrappingMinus => {
                        self.expression(operand, dst);
                        match operand.typ() {
                            Type::Base(BaseType::Int | BaseType::Ascii) => {
                                _ = writeln!(self.asm, " neg {reg}");
                            }
                            Type::Base(BaseType::Bool | BaseType::Str) | Type::Array { .. } => {
                                unreachable!("cannot negate non int/ascii values");
                            }
                        }
                    }
                    UnaryOp::SaturatingMinus => {
                        self.expression(operand, dst);
                        match operand.typ() {
                            Type::Base(BaseType::Int | BaseType::Ascii) => {
                                let Position { line, col, .. } = self.src.position(*op_col);
                                _ = writeln!(
                                    self.asm,
                                    " mov rdx, {line}\
                                    \n mov rcx, {col}\
                                    \n call int_saturating_negate",
                                );
                            }
                            Type::Base(BaseType::Bool | BaseType::Str) | Type::Array { .. } => {
                                unreachable!("cannot negate non int/ascii values");
                            }
                        }
                    }
                }
            }
            Expression::BooleanUnary { operand, .. } => {
                let Dst::Reg(reg) = dst else {
                    unreachable!();
                };

                let Type::Base(BaseType::Bool) = operand.typ() else {
                    unreachable!("can only invert boolean values");
                };

                self.expression(operand, dst);

                _ = writeln!(self.asm, " xor {reg}, 1");
            }
            /*
            NOTE(stefano): hard-coding the first and second operand until a better way to manage
            dst and src are developed
            */
            /*
            IDEA(stefano): limit shift/rotation rhs to an 8bit integer, and different strategies to
            deal whit rhs over 6bits:
            - check for an rhs bigger than 8 bits and crash (current)
            - silently discard the missing bits
            - create dedicate operators that implement those strategies
            */
            // Note: strings and arrays cannot appear in expressions
            Expression::Binary { lhs, op, op_col, rhs } => {
                let lhs_dst = Dst::Reg(Rdi);
                let rhs_dst = Dst::Reg(Rsi);
                let op_asm: Cow<'static, str> = match op {
                    BinaryOp::Pow => {
                        let Position { line, col, .. } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {col}\
                            \n call int_safe_pow\
                            \n mov rdi, rax",
                        )
                        .into()
                    }
                    BinaryOp::WrappingPow => {
                        let Position { line, col, .. } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {col}\
                            \n call int_wrapping_pow\
                            \n mov rdi, rax",
                        )
                        .into()
                    }
                    BinaryOp::SaturatingPow => {
                        let Position { line, col, .. } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {col}\
                            \n call int_saturating_pow\
                            \n mov rdi, rax",
                        )
                        .into()
                    }
                    BinaryOp::Times => {
                        let Position { line, col, .. } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {col}\
                            \n call int_safe_mul",
                        )
                        .into()
                    }
                    BinaryOp::WrappingTimes => " imul rdi, rsi".into(),
                    BinaryOp::SaturatingTimes => " call int_saturating_mul".into(),
                    BinaryOp::Divide => {
                        let Position { line, col, .. } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {col}\
                            \n call int_safe_div",
                        )
                        .into()
                    }
                    BinaryOp::WrappingDivide => {
                        let Position { line, col, .. } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {col}\
                            \n call int_wrapping_div",
                        )
                        .into()
                    }
                    BinaryOp::SaturatingDivide => {
                        let Position { line, col, .. } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {col}\
                            \n call int_saturating_div",
                        )
                        .into()
                    }
                    BinaryOp::Remainder => {
                        let Position { line, col, .. } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {col}\
                            \n call int_safe_remainder",
                        )
                        .into()
                    }
                    BinaryOp::Plus => {
                        let Position { line, col, .. } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {col}\
                            \n call int_safe_add",
                        )
                        .into()
                    }
                    BinaryOp::WrappingPlus => " add rdi, rsi".into(),
                    BinaryOp::SaturatingPlus => " call int_saturating_add".into(),
                    BinaryOp::Minus => {
                        let Position { line, col, .. } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {col}\
                            \n call int_safe_sub",
                        )
                        .into()
                    }
                    BinaryOp::WrappingMinus => " sub rdi, rsi".into(),
                    BinaryOp::SaturatingMinus => " call int_saturating_sub".into(),
                    BinaryOp::BitAnd => " and rdi, rsi".into(),
                    BinaryOp::BitOr => " or rdi, rsi".into(),
                    BinaryOp::BitXor => " xor rdi, rsi".into(),
                    BinaryOp::LeftShift => {
                        let Position { line, col, .. } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {col}\
                            \n call int_safe_left_shift",
                        )
                        .into()
                    }
                    BinaryOp::WrappingLeftShift => {
                        let Position { line, col, .. } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {col}\
                            \n call int_wrapping_left_shift",
                        )
                        .into()
                    }
                    BinaryOp::SaturatingLeftShift => {
                        let Position { line, col, .. } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {col}\
                            \n call int_saturating_left_shift",
                        )
                        .into()
                    }
                    BinaryOp::RightShift => {
                        let Position { line, col, .. } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {col}\
                            \n call int_safe_right_shift",
                        )
                        .into()
                    }
                    BinaryOp::LeftRotate => {
                        let Position { line, col, .. } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {col}\
                            \n call int_safe_left_rotate",
                        )
                        .into()
                    }
                    BinaryOp::RightRotate => {
                        let Position { line, col, .. } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {col}\
                            \n call int_safe_right_rotate",
                        )
                        .into()
                    }
                };

                self.binary_expression(lhs, rhs, lhs_dst, rhs_dst);

                _ = writeln!(self.asm, "{op_asm}\n");
            }
            // Note: strings and arrays cannot appear in expressions
            Expression::BooleanBinary { lhs, op, rhs } => {
                let lhs_dst = Dst::Reg(Rdi);
                let rhs_dst = Dst::Reg(Rsi);
                let op_asm = match op {
                    BooleanBinaryOp::And => " and rdi, rsi",
                    BooleanBinaryOp::Or => " or rdi, rsi",
                };

                self.binary_expression(lhs, rhs, lhs_dst, rhs_dst);

                _ = writeln!(self.asm, "{op_asm}\n");
            }
            /*
            IDEA(stefano): string/array comparison operators could also return the index where the
            mismatch occured, since repe CMPcc stops at mismatch_index, i.e:
            @rdx = len @rdx - reverse_mismatch_index @rcx - 1
            so:

            ```nasm
            repe cmpsq
            mov rdi, false
            setz dil
            ```

            would become this:

            ```nasmm
            repe cmpsq
            mov rdi, false
            setz dil
            sub rdx, rcx
            dec rdx
            ```
            */
            Expression::Comparison { lhs, op, rhs } => {
                let (lhs_dst, rhs_dst, op_asm): (Dst, Dst, Cow<'static, str>) =
                    match (lhs.typ(), rhs.typ()) {
                        (Type::Base(BaseType::Str), Type::Base(BaseType::Str)) => (
                            Dst::View { len: Rdi, ptr: Rsi },
                            Dst::View { len: Rdx, ptr: Rcx },
                            match op {
                                ComparisonOp::EqualsEquals => " call str_eq\
                                    \n movzx rdi, al"
                                    .into(),
                                ComparisonOp::NotEquals => " call str_eq\
                                    \n xor rax, 1\
                                    \n movzx rdi, al"
                                    .into(),
                                ComparisonOp::Greater => " call str_cmp\
                                    \n cmp rax, EQUAL\
                                    \n mov rdi, false\
                                    \n setg dil"
                                    .into(),
                                ComparisonOp::GreaterOrEquals => " call str_cmp\
                                    \n cmp rax, EQUAL\
                                    \n mov rdi, false\
                                    \n setge dil"
                                    .into(),
                                ComparisonOp::Less => " call str_cmp\
                                    \n cmp rax, EQUAL\
                                    \n mov rdi, false\
                                    \n setl dil"
                                    .into(),
                                ComparisonOp::LessOrEquals => " call str_cmp\
                                    \n cmp rax, EQUAL\
                                    \n mov rdi, false\
                                    \n setle dil"
                                    .into(),
                                ComparisonOp::Compare => " call str_cmp\
                                    \n mov rdi, rax"
                                    .into(),
                            },
                        ),
                        // Note: we can only compare non-empty arrays of the same type and length, so
                        // its safe to only match on the first array type and not to check for empty arrays
                        (Type::Array { base_type, .. }, Type::Array { .. }) => (
                            Dst::View { len: Rdi, ptr: Rsi },
                            Dst::View { len: Rdx, ptr: Rcx },
                            match op {
                                ComparisonOp::EqualsEquals => match base_type {
                                    BaseType::Int => " mov rdi, rcx\
                                        \n mov rcx, rdx\
                                        \n repe cmpsq\
                                        \n mov rdi, false\
                                        \n sete dil"
                                        .into(),
                                    BaseType::Ascii | BaseType::Bool => " mov rdi, rcx\
                                        \n mov rcx, rdx\
                                        \n repe cmpsb\
                                        \n mov rdi, false\
                                        \n sete dil"
                                        .into(),
                                    BaseType::Str => " call str_array_eq\
                                        \n movzx rdi, al"
                                        .into(),
                                },
                                ComparisonOp::NotEquals => match base_type {
                                    BaseType::Int => " mov rdi, rcx\
                                        \n mov rcx, rdx\
                                        \n repe cmpsq\
                                        \n mov rdi, false\
                                        \n setne dil"
                                        .into(),
                                    BaseType::Ascii | BaseType::Bool => " mov rdi, rcx\
                                        \n mov rcx, rdx\
                                        \n repe cmpsb\
                                        \n mov rdi, false\
                                        \n setne dil"
                                        .into(),
                                    BaseType::Str => " cmp str_array_eq\
                                        \n xor rax, 1\
                                        \n movzx rdi, al"
                                        .into(),
                                },
                                ComparisonOp::Greater => match base_type {
                                    BaseType::Int => " mov rdi, rcx\
                                        \n mov rcx, rdx\
                                        \n repe cmpsq\
                                        \n mov rdi, false\
                                        \n setg dil"
                                        .into(),
                                    BaseType::Ascii | BaseType::Bool => " mov rdi, rcx\
                                        \n mov rcx, rdx\
                                        \n repe cmpsb\
                                        \n mov rdi, false\
                                        \n setg dil"
                                        .into(),
                                    BaseType::Str => " call str_array_cmp\
                                        \n cmp rax, EQUAL\
                                        \n mov rdi, false\
                                        \n setg dil"
                                        .into(),
                                },
                                ComparisonOp::GreaterOrEquals => match base_type {
                                    BaseType::Int => " mov rdi, rcx\
                                        \n mov rcx, rdx\
                                        \n repe cmpsq\
                                        \n mov rdi, false\
                                        \n setge dil"
                                        .into(),
                                    BaseType::Ascii | BaseType::Bool => " mov rdi, rcx\
                                        \n mov rcx, rdx\
                                        \n repe cmpsb\
                                        \n mov rdi, false\
                                        \n setge dil"
                                        .into(),
                                    BaseType::Str => " call str_array_cmp\
                                        \n cmp rax, EQUAL\
                                        \n mov rdi, false\
                                        \n setge dil"
                                        .into(),
                                },
                                ComparisonOp::Less => match base_type {
                                    BaseType::Int => " mov rdi, rcx\
                                        \n mov rcx, rdx\
                                        \n repe cmpsq\
                                        \n mov rdi, false\
                                        \n setl dil"
                                        .into(),
                                    BaseType::Ascii | BaseType::Bool => " mov rdi, rcx\
                                        \n mov rcx, rdx\
                                        \n repe cmpsb\
                                        \n mov rdi, false\
                                        \n setl dil"
                                        .into(),
                                    BaseType::Str => " call str_array_cmp\
                                        \n cmp rax, EQUAL\
                                        \n mov rdi, false\
                                        \n setl dil"
                                        .into(),
                                },
                                ComparisonOp::LessOrEquals => match base_type {
                                    BaseType::Int => " mov rdi, rcx\
                                        \n mov rcx, rdx\
                                        \n repe cmpsq\
                                        \n mov rdi, false\
                                        \n setle dil"
                                        .into(),
                                    BaseType::Ascii | BaseType::Bool => " mov rdi, rcx\
                                        \n mov rcx, rdx\
                                        \n repe cmpsb\
                                        \n mov rdi, false\
                                        \n setle dil"
                                        .into(),
                                    BaseType::Str => " call str_array_cmp\
                                        \n cmp rax, EQUAL\
                                        \n mov rdi, false\
                                        \n setle dil"
                                        .into(),
                                },
                                ComparisonOp::Compare => match base_type {
                                    BaseType::Int => " mov rdi, rcx\
                                        \n mov rcx, rdx\
                                        \n repe cmpsq\
                                        \n mov rdi, LESS\
                                        \n mov rsi, EQUAL\
                                        \n cmove rdi, rsi\
                                        \n mov rsi, GREATER\
                                        \n cmovg rdi, rsi"
                                        .into(),
                                    BaseType::Ascii | BaseType::Bool => " mov rdi, rcx\
                                        \n mov rcx, rdx\
                                        \n repe cmpsb\
                                        \n mov rdi, LESS\
                                        \n mov rsi, EQUAL\
                                        \n cmove rdi, rsi\
                                        \n mov rsi, GREATER\
                                        \n cmovg rdi, rsi"
                                        .into(),
                                    BaseType::Str => " call str_array_cmp\
                                        \n mov rdi, rax"
                                        .into(),
                                },
                            },
                        ),
                        (
                            Type::Base(BaseType::Int | BaseType::Ascii | BaseType::Bool),
                            Type::Base(BaseType::Int | BaseType::Ascii | BaseType::Bool),
                        ) => (
                            Dst::Reg(Rdi),
                            Dst::Reg(Rsi),
                            match op {
                                ComparisonOp::EqualsEquals => " cmp rdi, rsi\
                                    \n mov rdi, false\
                                    \n sete dil"
                                    .into(),
                                ComparisonOp::NotEquals => " cmp rdi, rsi\
                                    \n mov rdi, false\
                                    \n setne dil"
                                    .into(),
                                ComparisonOp::Greater => " cmp rdi, rsi\
                                    \n mov rdi, false\
                                    \n setg dil"
                                    .into(),
                                ComparisonOp::GreaterOrEquals => " cmp rdi, rsi\
                                    \n mov rdi, false\
                                    \n setge dil"
                                    .into(),
                                ComparisonOp::Less => " cmp rdi, rsi\
                                    \n mov rdi, false\
                                    \n setl dil"
                                    .into(),
                                ComparisonOp::LessOrEquals => " cmp rdi, rsi\
                                    \n mov rdi, false\
                                    \n setle dil"
                                    .into(),
                                ComparisonOp::Compare => " cmp rdi, rsi\
                                    \n mov rdi, LESS\
                                    \n mov rsi, EQUAL\
                                    \n cmove rdi, rsi\
                                    \n mov rsi, GREATER\
                                    \n cmovg rdi, rsi"
                                    .into(),
                            },
                        ),
                        (Type::Base(BaseType::Str), _)
                        | (_, Type::Base(BaseType::Str))
                        | (Type::Array { .. }, _)
                        | (_, Type::Array { .. }) => {
                            unreachable!("strings and arrays cannot appear in expressions");
                        }
                    };

                self.binary_expression(lhs, rhs, lhs_dst, rhs_dst);

                _ = writeln!(self.asm, "{op_asm}\n");
            }
            Expression::Temporary { temporary_value_index, .. } => {
                let temporary_value_expression = &self.ast.temporaries[*temporary_value_index];
                let temporary_value = self.resolve_temporary(temporary_value_expression);
                let temporary_value_offset = temporary_value.offset;

                self.definition(temporary_value_expression, Base::Temp, temporary_value_offset);
                self.identifier(temporary_value_expression.typ(), dst, Base::Temp, temporary_value_offset);
            },
            Expression::Identifier { typ, name } => {
                let var = self.resolve(name);
                let var_offset = var.offset;
                self.identifier(*typ, dst, Base::Rbp, var_offset);
            }
            Expression::ArrayIndex { base_type, value, bracket_col, index } => {
                self.index(*base_type, value, *bracket_col, index);
            }
        }
    }

    fn condition(&mut self, condition: &'ast Expression<'src>, false_tag: &str) {
        match condition {
            // IDEA(stefano): remove these checks and do a plain jmp instead
            Expression::True => {
                _ = writeln!(
                    self.asm,
                    " mov dil, true\
                    \n cmp dil, true\
                    \n jne {false_tag}\n",
                );
            }
            Expression::False => {
                _ = writeln!(
                    self.asm,
                    " mov dil, false\
                    \n cmp dil, true\
                    \n jne {false_tag}\n",
                );
            }
            Expression::Array { .. }
            | Expression::Int(_)
            | Expression::Ascii(_)
            | Expression::Str(_)
            | Expression::RawStr(_) => {
                unreachable!("non-boolean expressions not allowed in conditions");
            }
            Expression::Temporary { ..  } => {
                unreachable!("should not appear in conditions");
            },
            Expression::BooleanUnary { operand, .. } => {
                self.expression(operand, Dst::Reg(Rdi));

                // we can only have boolean expressions at this point, so it's safe to ignore the integer negation case
                _ = writeln!(
                    self.asm,
                    " xor dil, 1\
                    \n jz {false_tag}\n"
                );
            }
            Expression::BooleanBinary { lhs, op, rhs, .. } => {
                let lhs_dst = match lhs.typ() {
                    Type::Base(BaseType::Bool) => Dst::Reg(Rdi),
                    Type::Base(BaseType::Int | BaseType::Ascii | BaseType::Str)
                    | Type::Array { .. } => {
                        unreachable!("non-boolean expressions not allowed in conditions");
                    }
                };

                let rhs_dst = match rhs.typ() {
                    Type::Base(BaseType::Bool) => Dst::Reg(Rsi),
                    Type::Base(BaseType::Int | BaseType::Ascii | BaseType::Str)
                    | Type::Array { .. } => {
                        unreachable!("non-boolean expressions not allowed in conditions");
                    }
                };

                self.binary_expression(lhs, rhs, lhs_dst, rhs_dst);

                match op {
                    BooleanBinaryOp::And => {
                        _ = writeln!(
                            self.asm,
                            " and rdi, rsi\
                            \n jz {false_tag}\n"
                        );
                    }
                    BooleanBinaryOp::Or => {
                        _ = writeln!(
                            self.asm,
                            " or rdi, rsi\
                            \n jz {false_tag}\n"
                        );
                    }
                }
            }
            Expression::Comparison { lhs, op, rhs, .. } => {
                let lhs_dst = match lhs.typ() {
                    Type::Base(BaseType::Int | BaseType::Ascii | BaseType::Bool) => Dst::Reg(Rdi),
                    Type::Base(BaseType::Str) | Type::Array { .. } => {
                        Dst::View { len: Rdi, ptr: Rsi }
                    }
                };

                let rhs_dst = match rhs.typ() {
                    Type::Base(BaseType::Int | BaseType::Ascii | BaseType::Bool) => Dst::Reg(Rsi),
                    Type::Base(BaseType::Str) | Type::Array { .. } => {
                        Dst::View { len: Rdx, ptr: Rcx }
                    }
                };

                self.binary_expression(lhs, rhs, lhs_dst, rhs_dst);

                match op {
                    ComparisonOp::EqualsEquals => {
                        _ = writeln!(
                            self.asm,
                            " cmp rdi, rsi\
                            \n jne {false_tag}\n"
                        );
                    }
                    ComparisonOp::NotEquals => {
                        _ = writeln!(
                            self.asm,
                            " cmp rdi, rsi\
                            \n je {false_tag}\n"
                        );
                    }
                    ComparisonOp::Greater => {
                        _ = writeln!(
                            self.asm,
                            " cmp rdi, rsi\
                            \n jle {false_tag}\n"
                        );
                    }
                    ComparisonOp::GreaterOrEquals => {
                        _ = writeln!(
                            self.asm,
                            " cmp rdi, rsi\
                            \n jl {false_tag}\n"
                        );
                    }
                    ComparisonOp::Less => {
                        _ = writeln!(
                            self.asm,
                            " cmp rdi, rsi\
                            \n jge {false_tag}\n"
                        );
                    }
                    ComparisonOp::LessOrEquals => {
                        _ = writeln!(
                            self.asm,
                            " cmp rdi, rsi\
                            \n jg {false_tag}\n"
                        );
                    }
                    ComparisonOp::Compare => {
                        unreachable!("non-boolean operators should not appear here")
                    }
                }
            }
            Expression::Identifier { name, .. } => {
                let var = self.resolve(name);
                let var_offset = var.offset;
                _ = writeln!(
                    self.asm,
                    " mov dil, [rbp + {var_offset}]\
                    \n cmp dil, true\
                    \n jne {false_tag}\n"
                );
            }
            Expression::ArrayIndex { .. } => {
                self.expression(condition, Dst::Reg(Rdi));
                _ = writeln!(
                    self.asm,
                    " cmp dil, true\
                    \n jne {false_tag}\n"
                );
            }
            Expression::Unary { .. } | Expression::Binary { .. } => {
                unreachable!("non-boolean expressions not allowed in conditions")
            }
        }
    }

    fn condition_reversed(&mut self, condition: &'ast Expression<'src>, true_tag: &str) {
        match condition {
            // IDEA(stefano): remove these checks and do a plain jmp instead
            Expression::True => {
                _ = writeln!(
                    self.asm,
                    " mov dil, true\
                    \n cmp dil, true\
                    \n je {true_tag}\n",
                );
            }
            Expression::False => {
                _ = writeln!(
                    self.asm,
                    " mov dil, false\
                    \n cmp dil, true\
                    \n je {true_tag}\n",
                );
            }
            Expression::Array { .. }
            | Expression::Int(_)
            | Expression::Ascii(_)
            | Expression::Str(_)
            | Expression::RawStr(_) => {
                unreachable!("non-boolean expressions not allowed in conditions");
            }
            Expression::Temporary { ..  } => {
                unreachable!("should not appear in conditions");
            },
            Expression::BooleanUnary { operand, .. } => {
                self.expression(operand, Dst::Reg(Rdi));

                // we can only have boolean expressions at this point, so it's safe to ignore the integer negation case
                _ = writeln!(
                    self.asm,
                    " xor dil, 1\
                    \n jnz {true_tag}\n"
                );
            }
            Expression::BooleanBinary { lhs, op, rhs, .. } => {
                let lhs_dst = match lhs.typ() {
                    Type::Base(BaseType::Bool) => Dst::Reg(Rdi),
                    Type::Base(BaseType::Int | BaseType::Ascii | BaseType::Str)
                    | Type::Array { .. } => {
                        unreachable!("non-boolean expressions not allowed in conditions");
                    }
                };

                let rhs_dst = match rhs.typ() {
                    Type::Base(BaseType::Bool) => Dst::Reg(Rsi),
                    Type::Base(BaseType::Int | BaseType::Ascii | BaseType::Str)
                    | Type::Array { .. } => {
                        unreachable!("non-boolean expressions not allowed in conditions");
                    }
                };

                self.binary_expression(lhs, rhs, lhs_dst, rhs_dst);

                match op {
                    BooleanBinaryOp::And => {
                        _ = writeln!(
                            self.asm,
                            " and rdi, rsi\
                            \n jnz {true_tag}\n"
                        );
                    }
                    BooleanBinaryOp::Or => {
                        _ = writeln!(
                            self.asm,
                            " or rdi, rsi\
                            \n jnz {true_tag}\n"
                        );
                    }
                }
            }
            Expression::Comparison { lhs, op, rhs, .. } => {
                let lhs_dst = match lhs.typ() {
                    Type::Base(BaseType::Int | BaseType::Ascii | BaseType::Bool) => Dst::Reg(Rdi),
                    Type::Base(BaseType::Str) | Type::Array { .. } => {
                        Dst::View { len: Rdi, ptr: Rsi }
                    }
                };

                let rhs_dst = match rhs.typ() {
                    Type::Base(BaseType::Int | BaseType::Ascii | BaseType::Bool) => Dst::Reg(Rsi),
                    Type::Base(BaseType::Str) | Type::Array { .. } => {
                        Dst::View { len: Rdx, ptr: Rcx }
                    }
                };

                self.binary_expression(lhs, rhs, lhs_dst, rhs_dst);

                match op {
                    ComparisonOp::EqualsEquals => {
                        _ = writeln!(
                            self.asm,
                            " cmp rdi, rsi\
                            \n je {true_tag}\n"
                        );
                    }
                    ComparisonOp::NotEquals => {
                        _ = writeln!(
                            self.asm,
                            " cmp rdi, rsi\
                            \n jne {true_tag}\n"
                        );
                    }
                    ComparisonOp::Greater => {
                        _ = writeln!(
                            self.asm,
                            " cmp rdi, rsi\
                            \n jg {true_tag}\n"
                        );
                    }
                    ComparisonOp::GreaterOrEquals => {
                        _ = writeln!(
                            self.asm,
                            " cmp rdi, rsi\
                            \n jge {true_tag}\n"
                        );
                    }
                    ComparisonOp::Less => {
                        _ = writeln!(
                            self.asm,
                            " cmp rdi, rsi\
                            \n jl {true_tag}\n"
                        );
                    }
                    ComparisonOp::LessOrEquals => {
                        _ = writeln!(
                            self.asm,
                            " cmp rdi, rsi\
                            \n jle {true_tag}\n"
                        );
                    }
                    ComparisonOp::Compare => {
                        unreachable!("non-boolean operators should not appear here")
                    }
                }
            }
            Expression::Identifier { name, .. } => {
                let var = self.resolve(name);
                let var_offset = var.offset;
                _ = writeln!(
                    self.asm,
                    " mov dil, [rbp + {var_offset}]\
                    \n cmp dil, true\
                    \n je {true_tag}\n"
                );
            }
            Expression::ArrayIndex { .. } => {
                self.expression(condition, Dst::Reg(Rdi));
                _ = writeln!(
                    self.asm,
                    " cmp dil, true\
                    \n je {true_tag}\n"
                );
            }
            Expression::Unary { .. } | Expression::Binary { .. } => {
                unreachable!("non-boolean expressions not allowed in conditions")
            }
        }
    }

    fn definition(&mut self, value: &'ast Expression<'src>, base: Base, dst_offset: usize) {
        match value {
            Expression::Int(integer) => {
                _ = writeln!(
                    self.asm,
                    " mov rdi, {integer}\
                    \n mov [{base} + {dst_offset}], rdi\n"
                );
            }
            Expression::Ascii(code) => {
                _ = writeln!(self.asm, " mov byte [{base} + {dst_offset}], {code}\n");
            }
            Expression::True => {
                _ = writeln!(self.asm, " mov byte [{base} + {dst_offset}], true\n");
            }
            Expression::False => {
                _ = writeln!(self.asm, " mov byte [{base} + {dst_offset}], false\n");
            }
            Expression::Str(string) => {
                let index = self.str_index(string);
                _ = writeln!(
                    self.asm,
                    " mov qword [{base} + {dst_offset}], str_{index}_len\
                    \n mov qword [{base} + {dst_offset} + {ptr_offset}], str_{index}\n",
                    ptr_offset = std::mem::size_of::<uint>()
                );
            }
            Expression::RawStr(string) => {
                let index = self.raw_str_index(string);
                _ = writeln!(
                    self.asm,
                    " mov qword [{base} + {dst_offset}], str_{index}_len\
                    \n mov qword [{base} + {dst_offset} + {ptr_offset}], str_{index}\n",
                    ptr_offset = std::mem::size_of::<uint>()
                );
            }
            Expression::Array { base_type, items } => {
                let typ_size = base_type.size();
                for (index, item) in items.iter().enumerate() {
                    self.definition(item, base, dst_offset + index * typ_size);
                }
            }
            Expression::Unary { op, op_col, operand } => match op {
                UnaryOp::Len => match &**operand {
                    Expression::Str(string) => {
                        let index = self.str_index(string);
                        _ = writeln!(
                            self.asm,
                            " mov qword [{base} + {dst_offset}], str_{index}_len\n"
                        );
                    }
                    Expression::RawStr(string) => {
                        let index = self.raw_str_index(string);
                        _ = writeln!(
                            self.asm,
                            " mov qword [{base} + {dst_offset}], str_{index}_len\n"
                        );
                    }
                    Expression::Array { items, .. } => {
                        _ = writeln!(
                            self.asm,
                            " mov qword [{base} + {dst_offset}], {}\n",
                            items.len()
                        );
                    }
                    Expression::Temporary { .. } => unreachable!("temporaries cannot appear in variables"),
                    Expression::Identifier { typ, name } => {
                        let var = self.resolve(name);
                        let var_offset = var.offset;
                        match typ {
                            Type::Base(BaseType::Str) => {
                                _ = writeln!(
                                    self.asm,
                                    " mov rdi, [{base} + {var_offset}]\
                                    \n mov [{base} + {dst_offset}], rdi\n"
                                );
                            }
                            Type::Array { len, .. } => {
                                _ = writeln!(self.asm, " mov qword [{base} + {dst_offset}], {len}\n");
                            }
                            Type::Base(BaseType::Int | BaseType::Ascii | BaseType::Bool) => {
                                unreachable!("cannot take the length of numerical types")
                            }
                        }
                    }
                    Expression::ArrayIndex { base_type, value: base_array_index_value, bracket_col, index } => {
                        self.index(*base_type, base_array_index_value, *bracket_col, index);
                        _ = writeln!(self.asm, "mov [{base} + {dst_offset}], rdi\n");
                    }
                    Expression::False
                    | Expression::True
                    | Expression::Int(_)
                    | Expression::Ascii(_)
                    | Expression::Unary { .. }
                    | Expression::BooleanUnary { .. }
                    | Expression::Binary { .. }
                    | Expression::BooleanBinary { .. }
                    | Expression::Comparison { .. } => {
                        unreachable!("cannot take the length of numerical types")
                    }
                },
                UnaryOp::Not => {
                    self.expression(operand, Dst::Reg(Rdi));
                    match operand.typ() {
                        Type::Base(BaseType::Int) => {
                            _ = writeln!(
                                self.asm,
                                " not rdi\
                                \n mov [{base} + {dst_offset}], rdi\n"
                            );
                        }
                        Type::Base(BaseType::Ascii) => {
                            _ = writeln!(
                                self.asm,
                                " not rdi\
                                \n mov [{base} + {dst_offset}], dil\n"
                            );
                        }
                        Type::Base(BaseType::Bool | BaseType::Str) | Type::Array { .. } => {
                            unreachable!("cannot invert non numerical values");
                        }
                    }
                }
                UnaryOp::Plus => {
                    self.expression(operand, Dst::Reg(Rdi));
                    match operand.typ() {
                        Type::Base(BaseType::Int) => {
                            let Position { line, col, .. } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_safe_abs\
                                \n mov [{base} + {dst_offset}], rdi\n",
                            );
                        }
                        Type::Base(BaseType::Ascii | BaseType::Bool | BaseType::Str)
                        | Type::Array { .. } => {
                            unreachable!("cannot take absolute value of non numerical values");
                        }
                    }
                }
                UnaryOp::WrappingPlus => {
                    self.expression(operand, Dst::Reg(Rdi));
                    match operand.typ() {
                        Type::Base(BaseType::Int) => {
                            _ = writeln!(
                                self.asm,
                                " call int_wrapping_abs\
                                \n mov [{base} + {dst_offset}], rdi\n"
                            );
                        }
                        Type::Base(BaseType::Ascii | BaseType::Bool | BaseType::Str)
                        | Type::Array { .. } => {
                            unreachable!("cannot take absolute value of non numerical values");
                        }
                    }
                }
                UnaryOp::SaturatingPlus => {
                    self.expression(operand, Dst::Reg(Rdi));
                    match operand.typ() {
                        Type::Base(BaseType::Int) => {
                            let Position { line, col, .. } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_saturating_abs\
                                \n mov [{base} + {dst_offset}], rdi\n",
                            );
                        }
                        Type::Base(BaseType::Ascii | BaseType::Bool | BaseType::Str)
                        | Type::Array { .. } => {
                            unreachable!("cannot take absolute value of non numerical values");
                        }
                    }
                }
                UnaryOp::Minus => {
                    self.expression(operand, Dst::Reg(Rdi));
                    match operand.typ() {
                        Type::Base(BaseType::Int) => {
                            let Position { line, col, .. } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_safe_negate\
                                \n mov [{base} + {dst_offset}], rdi\n",
                            );
                        }
                        Type::Base(BaseType::Ascii) => {
                            let Position { line, col, .. } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_safe_negate\
                                \n mov [{base} + {dst_offset}], dil\n",
                            );
                        }
                        Type::Base(BaseType::Bool | BaseType::Str) | Type::Array { .. } => {
                            unreachable!("cannot negate non int/ascii values");
                        }
                    }
                }
                UnaryOp::WrappingMinus => {
                    self.expression(operand, Dst::Reg(Rdi));
                    match operand.typ() {
                        Type::Base(BaseType::Int) => {
                            _ = writeln!(
                                self.asm,
                                " neg rdi\
                                \n mov [{base} + {dst_offset}], rdi\n"
                            );
                        }
                        Type::Base(BaseType::Ascii) => {
                            _ = writeln!(
                                self.asm,
                                " neg rdi\
                                \n mov [{base} + {dst_offset}], dil\n"
                            );
                        }
                        Type::Base(BaseType::Bool | BaseType::Str) | Type::Array { .. } => {
                            unreachable!("cannot negate non int/ascii values");
                        }
                    }
                }
                UnaryOp::SaturatingMinus => {
                    self.expression(operand, Dst::Reg(Rdi));
                    match operand.typ() {
                        Type::Base(BaseType::Int) => {
                            let Position { line, col, .. } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_saturating_negate\
                                \n mov [{base} + {dst_offset}], rdi\n",
                            );
                        }
                        Type::Base(BaseType::Ascii) => {
                            let Position { line, col, .. } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_saturating_negate\
                                \n mov [{base} + {dst_offset}], dil\n",
                            );
                        }
                        Type::Base(BaseType::Bool | BaseType::Str) | Type::Array { .. } => {
                            unreachable!("cannot negate non int/ascii values");
                        }
                    }
                }
            },
            Expression::BooleanUnary { operand, .. } => {
                self.expression(operand, Dst::Reg(Rdi));
                _ = writeln!(
                    self.asm,
                    " xor rdi, 1\
                    \n mov [{base} + {dst_offset}], dil\n"
                );
            }
            Expression::Binary { .. } => {
                self.expression(value, Dst::Reg(Rdi));

                match value.typ() {
                    Type::Base(BaseType::Int) => {
                        _ = writeln!(self.asm, " mov [{base} + {dst_offset}], rdi\n");
                    }
                    Type::Base(BaseType::Ascii) => {
                        _ = writeln!(self.asm, " mov [{base} + {dst_offset}], dil\n");
                    }
                    Type::Base(BaseType::Bool | BaseType::Str) | Type::Array { .. } => {
                        unreachable!("cannot appear in expressions");
                    }
                }
            }
            Expression::BooleanBinary { .. } => {
                self.expression(value, Dst::Reg(Rdi));

                match value.typ() {
                    Type::Base(BaseType::Bool) => {
                        _ = writeln!(self.asm, " mov [{base} + {dst_offset}], dil\n");
                    }
                    Type::Base(BaseType::Int | BaseType::Ascii | BaseType::Str)
                    | Type::Array { .. } => {
                        unreachable!("cannot appear in boolean expressions");
                    }
                }
            }
            Expression::Comparison { .. } => {
                let Type::Base(BaseType::Bool) = value.typ() else {
                    unreachable!("only booleans can result from comparison expressions");
                };

                self.expression(value, Dst::Reg(Rdi));
                _ = writeln!(self.asm, " mov [{base} + {dst_offset}], dil\n");
            }
            Expression::Temporary { .. } => unreachable!("temporaries cannot appear in variables"),
            Expression::Identifier { typ: identifier_typ, name } => {
                let var = self.resolve(name);
                let src_offset = var.offset;
                match identifier_typ {
                    Type::Base(BaseType::Int) => {
                        _ = writeln!(
                            self.asm,
                            " mov rdi, [{base} + {src_offset}]\
                            \n mov [{base} + {dst_offset}], rdi\n"
                        );
                    }
                    Type::Base(BaseType::Ascii | BaseType::Bool) => {
                        _ = writeln!(
                            self.asm,
                            " mov dil, [{base} + {src_offset}]\
                            \n mov [{base} + {dst_offset}], dil\n"
                        );
                    }
                    Type::Base(BaseType::Str) => {
                        _ = writeln!(
                            self.asm,
                            " mov rdi, [{base} + {src_offset}]\
                            \n mov rsi, [{base} + {src_offset} + {ptr_offset}]\
                            \n mov [{base} + {dst_offset}], rdi\
                            \n mov [{base} + {dst_offset} + {ptr_offset}], rsi\n",
                            ptr_offset = std::mem::size_of::<uint>()
                        );
                    }
                    Type::Array { base_type: array_typ, len } => match *array_typ {
                        BaseType::Int => {
                            _ = writeln!(
                                self.asm,
                                " lea rdi, [{base} + {dst_offset}]\
                                \n lea rsi, [{base} + {src_offset}]\
                                \n mov rcx, {len}\
                                \n rep movsq\n"
                            );
                        }
                        BaseType::Ascii | BaseType::Bool => {
                            _ = writeln!(
                                self.asm,
                                " lea rdi, [{base} + {dst_offset}]\
                                \n lea rsi, [{base} + {src_offset}]\
                                \n mov rcx, {len}\
                                \n rep movsb\n"
                            );
                        }
                        BaseType::Str => {
                            _ = writeln!(
                                self.asm,
                                " lea rdi, [{base} + {dst_offset}]\
                                \n lea rsi, [{base} + {src_offset}]\
                                \n mov rcx, {len} * 2\
                                \n rep movsq\n"
                            );
                        }
                    },
                }
            }
            Expression::ArrayIndex { base_type, .. } => {
                self.expression(value, Dst::default(&Type::Base(*base_type)));

                match base_type {
                    BaseType::Int => _ = writeln!(self.asm, " mov [{base} + {dst_offset}], rdi\n"),
                    BaseType::Ascii | BaseType::Bool => {
                        _ = writeln!(self.asm, " mov [{base} + {dst_offset}], dil\n");
                    }
                    BaseType::Str => {
                        _ = writeln!(
                            self.asm,
                            " mov [{base} + {dst_offset}], rdi\
                            \n mov [{base} + {dst_offset} + {ptr_offset}], rsi\n",
                            ptr_offset = std::mem::size_of::<uint>()
                        );
                    }
                }
            }
        }
    }
}

// ifs
impl<'src, 'ast: 'src> Compiler<'src, 'ast> {
    fn iff(&mut self, iff: &'ast IfStatement<'src>, tag: &str, false_tag: &str) {
        _ = writeln!(self.asm, "{tag}:; {}", iff.condition);
        self.condition(&iff.condition, false_tag);
        self.node(&iff.statement);
    }
}

// print statements
impl<'src, 'ast: 'src> Compiler<'src, 'ast> {
    fn print(&mut self, value: &'ast Expression<'src>) {
        let value_type = value.typ();
        self.expression(value, Dst::default(&value_type));

        match value_type {
            Type::Base(BaseType::Int) => _ = writeln!(self.asm, " call int_print\n"),
            Type::Base(BaseType::Ascii) => _ = writeln!(self.asm, " call ascii_print\n"),
            Type::Base(BaseType::Bool) => _ = writeln!(self.asm, " call bool_print\n"),
            Type::Base(BaseType::Str) => _ = writeln!(self.asm, " call str_print\n"),
            Type::Array { base_type, .. } => match base_type {
                BaseType::Int => _ = writeln!(self.asm, " call int_array_debug_print\n"),
                BaseType::Ascii => _ = writeln!(self.asm, " call ascii_array_debug_print\n"),
                BaseType::Bool => _ = writeln!(self.asm, " call bool_array_debug_print\n"),
                BaseType::Str => _ = writeln!(self.asm, " call str_array_debug_print\n"),
            },
        }
    }

    fn eprint(&mut self, value: &'ast Expression<'src>) {
        let value_type = value.typ();
        self.expression(value, Dst::default(&value_type));

        match value_type {
            Type::Base(BaseType::Int) => _ = writeln!(self.asm, " call int_eprint\n"),
            Type::Base(BaseType::Ascii) => _ = writeln!(self.asm, " call ascii_eprint\n"),
            Type::Base(BaseType::Bool) => _ = writeln!(self.asm, " call bool_eprint\n"),
            Type::Base(BaseType::Str) => _ = writeln!(self.asm, " call str_eprint\n"),
            Type::Array { base_type: typ, .. } => match typ {
                BaseType::Int => _ = writeln!(self.asm, " call int_array_debug_eprint\n"),
                BaseType::Ascii => _ = writeln!(self.asm, " call ascii_array_debug_eprint\n"),
                BaseType::Bool => _ = writeln!(self.asm, " call bool_array_debug_eprint\n"),
                BaseType::Str => _ = writeln!(self.asm, " call str_array_debug_eprint\n"),
            },
        }
    }
}

#[derive(Debug)]
pub enum Error {
    CouldNotCreateFile { path: PathBuf, err: std::io::Error },
    WritingAssemblyFailed { err: std::io::Error },
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut message = String::new();
        let mut cause = String::new();
        match self {
            Self::CouldNotCreateFile { path, err } => {
                _ = write!(message, "could not create file '{}'", path.display());
                _ = write!(cause, "{err} ({})", err.kind());
            }
            Self::WritingAssemblyFailed { err } => {
                _ = write!(message, "writing to assembly file failed");
                _ = write!(cause, "{err} ({})", err.kind());
            }
        };

        return write!(
            f,
            "{ERROR}: {message}\
            \n{CAUSE}: {cause}",
        );
    }
}

impl std::error::Error for Error {}
