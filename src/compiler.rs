// IDEA(stefano): reserve space for the biggest temporary value and reuse as necessary
// IDEA(stefano): have built-in functions return their result in rdi instead of rax

pub mod artifacts;
mod asm;
mod reg;

use self::{artifacts::Artifacts, reg::Reg64};
use crate::{
    src_file::SrcFile,
    syntax::{
        ast::{
            self, AssignmentOp, BaseType, BinaryOp, BooleanBinaryOp, ComparisonOp, Expression,
            IfStatement, Node, Scope, SizeOf, Type, TypeOf, UnaryOp,
        },
        tokenizer::{ascii, uint, Literal, Mutability},
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

#[derive(Debug)]
struct Variable<'src, 'ast: 'src> {
    inner: &'ast ast::Variable<'src>,
    offset: usize,
}

const STACK_ALIGN: usize = std::mem::size_of::<usize>();

#[derive(Debug)]
pub struct Compiler<'src, 'ast: 'src> {
    ast: &'ast [Scope<'src>],

    string_labels: String,
    asm: String,

    variables: Vec<Variable<'src, 'ast>>,
    strings: Vec<&'ast Vec<ascii>>,

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
        artifacts: &Artifacts,
        ast: &'ast [Scope<'src>],
    ) -> Result<(), Error> {
        #[allow(clippy::wildcard_imports)]
        use asm::*;

        let asm_file = match File::create(&artifacts.asm_path) {
            Ok(file) => file,
            Err(err) => {
                return Err(Error::CouldNotCreateFile {
                    path: artifacts.asm_path.clone(),
                    err,
                });
            }
        };

        let mut this = Compiler {
            ast,
            string_labels: String::new(),
            asm: String::new(),
            variables: Vec::new(),
            strings: Vec::new(),
            if_counter: 0,
            loop_counter: 0,
            loop_counters: Vec::new(),
            // and_counter: 0,
            // or_counter: 0,
        };

        if this.ast.is_empty() {
            _ = write!(
                this.asm,
                "exit:\
                \n mov rdi, EXIT_SUCCESS"
            );
        } else {
            for scope in this.ast {
                for var in &scope.let_variables {
                    this.variables.push(Variable { inner: var, offset: 0 /* placeholder */ });
                }

                for var in &scope.var_variables {
                    this.variables.push(Variable { inner: var, offset: 0 /* placeholder */ });
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
 str attempt_exponent_negative, "attempt to raise a number to a negative power"
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
            Node::Definition { mutability, scope_index, var_index } => {
                let ast_var = match mutability {
                    Mutability::Let => &self.ast[*scope_index].let_variables[*var_index],
                    Mutability::Var => &self.ast[*scope_index].var_variables[*var_index],
                };

                let name = ast_var.name;
                let value = &ast_var.value;

                let var = self.resolve(name);
                let dst_offset = var.offset;

                _ = writeln!(self.asm, " ; {name} = {value}");
                self.definition(value, dst_offset);
            }
            Node::Assignment { scope_index, var_index, op, op_position, new_value } => {
                // Note: assignments are only allowed on mutable variables
                let ast_var = &self.ast[*scope_index].var_variables[*var_index];
                let name = ast_var.name;

                let var = self.resolve(name);
                let dst_offset = var.offset;

                _ = writeln!(self.asm, " ; {name} {op} {new_value}");
                if let AssignmentOp::Equals = op {
                    self.definition(new_value, dst_offset);
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
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_safe_pow\
                                \n mov rdi, rax",
                                line = op_position.line,
                                col = op_position.col
                            );
                        }
                        AssignmentOp::WrappingPow => {
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_wrapping_pow\
                                \n mov rdi, rax",
                                line = op_position.line,
                                col = op_position.col
                            );
                        }
                        AssignmentOp::SaturatingPow => {
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_saturating_pow\
                                \n mov rdi, rax",
                                line = op_position.line,
                                col = op_position.col
                            );
                        }
                        AssignmentOp::Times => {
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_safe_mul",
                                line = op_position.line,
                                col = op_position.col
                            );
                        }
                        AssignmentOp::WrappingTimes => _ = writeln!(self.asm, " imul rdi, rsi"),
                        AssignmentOp::SaturatingTimes => {
                            _ = writeln!(self.asm, " call int_saturating_mul");
                        }
                        AssignmentOp::Divide => {
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_safe_div",
                                line = op_position.line,
                                col = op_position.col
                            );
                        }
                        AssignmentOp::WrappingDivide => {
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_wrapping_div",
                                line = op_position.line,
                                col = op_position.col
                            );
                        }
                        AssignmentOp::SaturatingDivide => {
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_saturating_div",
                                line = op_position.line,
                                col = op_position.col
                            );
                        }
                        AssignmentOp::Remainder => {
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_safe_remainder",
                                line = op_position.line,
                                col = op_position.col
                            );
                        }
                        AssignmentOp::Plus => {
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_safe_add",
                                line = op_position.line,
                                col = op_position.col,
                            );
                        }
                        AssignmentOp::WrappingPlus => _ = writeln!(self.asm, " add rdi, rsi"),
                        AssignmentOp::SaturatingPlus => {
                            _ = writeln!(self.asm, " call int_saturating_add");
                        }
                        AssignmentOp::Minus => {
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_safe_sub",
                                line = op_position.line,
                                col = op_position.col,
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
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_safe_left_shift",
                                line = op_position.line,
                                col = op_position.col,
                            );
                        }
                        AssignmentOp::WrappingLeftShift => {
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_wrapping_left_shift",
                                line = op_position.line,
                                col = op_position.col,
                            );
                        }
                        AssignmentOp::SaturatingLeftShift => {
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_saturating_left_shift",
                                line = op_position.line,
                                col = op_position.col,
                            );
                        }
                        AssignmentOp::RightShift => {
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_safe_right_shift",
                                line = op_position.line,
                                col = op_position.col,
                            );
                        }
                        AssignmentOp::LeftRotate => {
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_safe_left_rotate",
                                line = op_position.line,
                                col = op_position.col,
                            );
                        }
                        AssignmentOp::RightRotate => {
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_safe_right_rotate",
                                line = op_position.line,
                                col = op_position.col,
                            );
                        }
                        AssignmentOp::Equals => unreachable!("handled in the previous branch"),
                    };

                    _ = writeln!(self.asm, "\n mov [rbp + {dst_offset}], rdi\n");
                }
            }
            Node::Scope { index } => self.scope(*index),
            Node::Expression(expression) => {
                self.expression(expression, Dst::default(&expression.typ()));
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

    fn scope(&mut self, scope_index: usize) {
        let scope = &self.ast[scope_index];
        for node in &scope.nodes {
            self.node(node);
        }
    }
}

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

    fn string_label_index(&mut self, string: &'ast Vec<ascii>) -> usize {
        let mut string_index = 0;
        for string_label in &self.strings {
            if std::ptr::eq(string, *string_label) {
                return string_index;
            }
            string_index += 1;
        }

        let string_str = unsafe { std::str::from_utf8_unchecked(string) };
        let string_chars = string_str.escape_debug();
        _ = writeln!(self.string_labels, " str str_{string_index}, `{string_chars}`");

        self.strings.push(string);
        return string_index;
    }

    fn binary_expression(
        &mut self,
        lhs: &'ast Expression<'src>,
        rhs: &'ast Expression<'src>,
        lhs_dst: Dst,
        rhs_dst: Dst,
    ) {
        self.expression(lhs, lhs_dst);

        match rhs {
            // these expressions do not need to save the value of the lhs
            Expression::Literal(_)
            | Expression::Unary { op: UnaryOp::Not | UnaryOp::WrappingMinus, .. }
            | Expression::BooleanUnary { .. }
            | Expression::Identifier { .. } => {
                self.expression(rhs, rhs_dst);
                return;
            }
            // these expressions need to save the value of the lhs
            Expression::Unary { .. }
            | Expression::Binary { .. }
            | Expression::BooleanBinary { .. }
            | Expression::Comparison { .. }
            | Expression::ArrayIndex { .. } => {
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
            Expression::Array { .. } => {
                unreachable!("arrays cannot appear in expressions");
            }
        }
    }

    fn expression(&mut self, factor: &'ast Expression<'src>, dst: Dst) {
        match factor {
            Expression::Literal(literal) => match literal {
                Literal::Int(integer) => match dst {
                    Dst::Reg(reg) => _ = writeln!(self.asm, " mov {reg}, {integer}"),
                    Dst::View { .. } => unreachable!(),
                },
                Literal::Ascii(code) => match dst {
                    Dst::Reg(reg) => _ = writeln!(self.asm, " mov {reg}, {code}"),
                    Dst::View { .. } => unreachable!(),
                },
                Literal::Bool(boolean) => match dst {
                    Dst::Reg(reg) => _ = writeln!(self.asm, " mov {reg}, {boolean}"),
                    Dst::View { .. } => unreachable!(),
                },
                Literal::Str(string) => match dst {
                    Dst::View { len, ptr } => {
                        let index = self.string_label_index(string);
                        _ = writeln!(
                            self.asm,
                            " mov {len}, str_{index}_len\
                            \n mov {ptr}, str_{index}"
                        );
                    }
                    Dst::Reg(_) => unreachable!(),
                },
            },
            Expression::Unary { op_position, op, operand } => {
                let Dst::Reg(reg) = dst else {
                    unreachable!();
                };

                match op {
                    UnaryOp::Len => match &**operand {
                        Expression::Literal(Literal::Str(string)) => {
                            let index = self.string_label_index(string);
                            _ = writeln!(self.asm, " mov {reg}, str_{index}_len");
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
                        Expression::Array { items, .. } => {
                            _ = writeln!(self.asm, " mov {reg}, {}", items.len());
                        }
                        Expression::ArrayIndex { base_type: typ, var_name, bracket_position, index } => {
                            self.expression(index, Dst::Reg(Rdi));

                            let var = self.resolve(var_name);
                            let var_offset = var.offset;

                            let Type::Array { len, .. } = var.inner.value.typ() else {
                                unreachable!("only array of strings can appear here");
                            };

                            _ = writeln!(
                                self.asm,
                                " mov rsi, {len}\
                                \n mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call assert_array_index_in_range\
                                \n imul rdi, {typ_size}\
                                \n mov {reg}, [rbp + {var_offset} + rdi]\n",
                                line = bracket_position.line,
                                col = bracket_position.col,
                                typ_size = typ.size(),
                            );
                        }
                        Expression::Literal(_)
                        | Expression::Unary { .. }
                        | Expression::BooleanUnary { .. }
                        | Expression::Binary { .. }
                        | Expression::BooleanBinary { .. }
                        | Expression::Comparison { .. } => {
                            unreachable!("cannot take the length of numerical types")
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
                                _ = writeln!(
                                    self.asm,
                                    " mov rdx, {line}\
                                    \n mov rcx, {col}\
                                    \n call int_safe_abs",
                                    line = op_position.line,
                                    col = op_position.col
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
                                _ = writeln!(
                                    self.asm,
                                    " mov rdx, {line}\
                                    \n mov rcx, {col}\
                                    \n call int_saturating_abs",
                                    line = op_position.line,
                                    col = op_position.col
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
                                _ = writeln!(
                                    self.asm,
                                    " mov rdx, {line}\
                                    \n mov rcx, {col}\
                                    \n call int_safe_negate",
                                    line = op_position.line,
                                    col = op_position.col
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
                                _ = writeln!(
                                    self.asm,
                                    " mov rdx, {line}\
                                    \n mov rcx, {col}\
                                    \n call int_saturating_negate",
                                    line = op_position.line,
                                    col = op_position.col
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
            Expression::Binary { lhs, op_position, op, rhs } => {
                let lhs_dst = Dst::Reg(Rdi);
                let rhs_dst = Dst::Reg(Rsi);
                let op_asm: Cow<'static, str> = match op {
                    BinaryOp::Pow => format!(
                        " mov rdx, {line}\
                        \n mov rcx, {col}\
                        \n call int_safe_pow\
                        \n mov rdi, rax",
                        line = op_position.line,
                        col = op_position.col
                    )
                    .into(),
                    BinaryOp::WrappingPow => format!(
                        " mov rdx, {line}\
                        \n mov rcx, {col}\
                        \n call int_wrapping_pow\
                        \n mov rdi, rax",
                        line = op_position.line,
                        col = op_position.col
                    )
                    .into(),
                    BinaryOp::SaturatingPow => format!(
                        " mov rdx, {line}\
                        \n mov rcx, {col}\
                        \n call int_saturating_pow\
                        \n mov rdi, rax",
                        line = op_position.line,
                        col = op_position.col
                    )
                    .into(),
                    BinaryOp::Times => format!(
                        " mov rdx, {line}\
                        \n mov rcx, {col}\
                        \n call int_safe_mul",
                        line = op_position.line,
                        col = op_position.col
                    )
                    .into(),
                    BinaryOp::WrappingTimes => " imul rdi, rsi".into(),
                    BinaryOp::SaturatingTimes => " call int_saturating_mul".into(),
                    BinaryOp::Divide => format!(
                        " mov rdx, {line}\
                        \n mov rcx, {col}\
                        \n call int_safe_div",
                        line = op_position.line,
                        col = op_position.col
                    )
                    .into(),
                    BinaryOp::WrappingDivide => format!(
                        " mov rdx, {line}\
                        \n mov rcx, {col}\
                        \n call int_wrapping_div",
                        line = op_position.line,
                        col = op_position.col
                    )
                    .into(),
                    BinaryOp::SaturatingDivide => format!(
                        " mov rdx, {line}\
                        \n mov rcx, {col}\
                        \n call int_saturating_div",
                        line = op_position.line,
                        col = op_position.col
                    )
                    .into(),
                    BinaryOp::Remainder => format!(
                        " mov rdx, {line}\
                        \n mov rcx, {col}\
                        \n call int_safe_remainder",
                        line = op_position.line,
                        col = op_position.col
                    )
                    .into(),
                    BinaryOp::Plus => format!(
                        " mov rdx, {line}\
                        \n mov rcx, {col}\
                        \n call int_safe_add",
                        line = op_position.line,
                        col = op_position.col,
                    )
                    .into(),
                    BinaryOp::WrappingPlus => " add rdi, rsi".into(),
                    BinaryOp::SaturatingPlus => " call int_saturating_add".into(),
                    BinaryOp::Minus => format!(
                        " mov rdx, {line}\
                        \n mov rcx, {col}\
                        \n call int_safe_sub",
                        line = op_position.line,
                        col = op_position.col,
                    )
                    .into(),
                    BinaryOp::WrappingMinus => " sub rdi, rsi".into(),
                    BinaryOp::SaturatingMinus => " call int_saturating_sub".into(),
                    BinaryOp::BitAnd => " and rdi, rsi".into(),
                    BinaryOp::BitOr => " or rdi, rsi".into(),
                    BinaryOp::BitXor => " xor rdi, rsi".into(),
                    BinaryOp::LeftShift => format!(
                        " mov rdx, {line}\
                        \n mov rcx, {col}\
                        \n call int_safe_left_shift",
                        line = op_position.line,
                        col = op_position.col,
                    )
                    .into(),
                    BinaryOp::WrappingLeftShift => format!(
                        " mov rdx, {line}\
                        \n mov rcx, {col}\
                        \n call int_wrapping_left_shift",
                        line = op_position.line,
                        col = op_position.col,
                    )
                    .into(),
                    BinaryOp::SaturatingLeftShift => format!(
                        " mov rdx, {line}\
                        \n mov rcx, {col}\
                        \n call int_saturating_left_shift",
                        line = op_position.line,
                        col = op_position.col,
                    )
                    .into(),
                    BinaryOp::RightShift => format!(
                        " mov rdx, {line}\
                        \n mov rcx, {col}\
                        \n call int_safe_right_shift",
                        line = op_position.line,
                        col = op_position.col,
                    )
                    .into(),
                    BinaryOp::LeftRotate => format!(
                        " mov rdx, {line}\
                        \n mov rcx, {col}\
                        \n call int_safe_left_rotate",
                        line = op_position.line,
                        col = op_position.col,
                    )
                    .into(),
                    BinaryOp::RightRotate => format!(
                        " mov rdx, {line}\
                        \n mov rcx, {col}\
                        \n call int_safe_right_rotate",
                        line = op_position.line,
                        col = op_position.col,
                    )
                    .into(),
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
                        (Type::Array { base_type: typ, .. }, Type::Array { .. }) => (
                            Dst::View { len: Rdi, ptr: Rsi },
                            Dst::View { len: Rdx, ptr: Rcx },
                            match op {
                                ComparisonOp::EqualsEquals => match typ {
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
                                ComparisonOp::NotEquals => match typ {
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
                                ComparisonOp::Greater => match typ {
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
                                ComparisonOp::GreaterOrEquals => match typ {
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
                                ComparisonOp::Less => match typ {
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
                                ComparisonOp::LessOrEquals => match typ {
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
                                ComparisonOp::Compare => match typ {
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
            Expression::Identifier { typ, name } => {
                let var = self.resolve(name);
                let var_offset = var.offset;
                match typ {
                    Type::Base(BaseType::Int) => match dst {
                        Dst::Reg(reg) => _ = writeln!(self.asm, " mov {reg}, [rbp + {var_offset}]"),
                        Dst::View { .. } => unreachable!(),
                    },
                    Type::Base(BaseType::Ascii | BaseType::Bool) => match dst {
                        Dst::Reg(reg) => {
                            _ = writeln!(self.asm, " movzx {reg}, byte [rbp + {var_offset}]");
                        }
                        Dst::View { .. } => unreachable!(),
                    },
                    Type::Base(BaseType::Str) => match dst {
                        Dst::View { len, ptr } => {
                            _ = writeln!(
                                self.asm,
                                " mov {len}, [rbp + {var_offset}]\
                                \n mov {ptr}, [rbp + {var_offset} + {ptr_offset}]",
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
                                \n lea {ptr}, [rbp + {var_offset}]"
                            );
                        }
                        Dst::Reg(_) => unreachable!(),
                    },
                }
            }
            Expression::ArrayIndex { base_type: typ, var_name, bracket_position, index } => {
                self.expression(index, Dst::Reg(Rdi));

                let line = bracket_position.line;
                let col = bracket_position.col;
                let var = self.resolve(var_name);
                let var_offset = var.offset;
                let var_typ = var.inner.value.typ();

                match var_typ {
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

                        match typ {
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
                                    " imul rdi, {typ_size}\
                                    \n mov rsi, [rbp + {var_offset} + {ptr_offset} + rdi]\
                                    \n mov rdi, [rbp + {var_offset} + rdi]\n",
                                    typ_size = typ.size(),
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
            }
            Expression::Array { .. } => unreachable!("arrays cannot appear in expressions"),
        }
    }

    fn condition(&mut self, condition: &'ast Expression<'src>, false_tag: &str) {
        match condition {
            Expression::Literal(literal) => match literal {
                Literal::Bool(boolean) => {
                    _ = writeln!(
                        self.asm,
                        " mov dil, {bool}\
                        \n cmp dil, true\
                        \n jne {false_tag}\n",
                        bool = usize::from(*boolean)
                    );
                }
                Literal::Int(_) | Literal::Ascii(_) | Literal::Str(_) => {
                    unreachable!("non-boolean expressions not allowed in conditions");
                }
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
            Expression::Unary { .. } | Expression::Binary { .. } | Expression::Array { .. } => {
                unreachable!("non-boolean expressions not allowed in conditions")
            }
        }
    }

    fn condition_reversed(&mut self, condition: &'ast Expression<'src>, true_tag: &str) {
        match condition {
            Expression::Literal(literal) => match literal {
                Literal::Bool(boolean) => {
                    _ = writeln!(
                        self.asm,
                        " mov dil, {bool}\
                        \n cmp dil, true\
                        \n je {true_tag}\n",
                        bool = usize::from(*boolean)
                    );
                }
                Literal::Int(_) | Literal::Ascii(_) | Literal::Str(_) => {
                    unreachable!("non-boolean expressions not allowed in conditions");
                }
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
            Expression::Unary { .. } | Expression::Binary { .. } | Expression::Array { .. } => {
                unreachable!("non-boolean expressions not allowed in conditions")
            }
        }
    }

    fn definition(&mut self, value: &'ast Expression<'src>, dst_offset: usize) {
        match value {
            Expression::Literal(literal) => match literal {
                Literal::Int(integer) => {
                    _ = writeln!(
                        self.asm,
                        " mov rdi, {integer}\
                        \n mov [rbp + {dst_offset}], rdi\n"
                    );
                }
                Literal::Ascii(code) => {
                    _ = writeln!(self.asm, " mov byte [rbp + {dst_offset}], {code}\n");
                }
                Literal::Bool(boolean) => {
                    _ = writeln!(self.asm, " mov byte [rbp + {dst_offset}], {boolean}\n");
                }
                Literal::Str(string) => {
                    let index = self.string_label_index(string);
                    _ = writeln!(
                        self.asm,
                        " mov qword [rbp + {dst_offset}], str_{index}_len\
                        \n mov qword [rbp + {dst_offset} + {ptr_offset}], str_{index}\n",
                        ptr_offset = std::mem::size_of::<uint>()
                    );
                }
            },
            Expression::Unary { op_position, op, operand } => match op {
                UnaryOp::Len => match &**operand {
                    Expression::Literal(Literal::Str(string)) => {
                        let index = self.string_label_index(string);
                        _ = writeln!(
                            self.asm,
                            " mov qword [rbp + {dst_offset}], str_{index}_len\n"
                        );
                    }
                    Expression::Identifier { typ, name } => {
                        let var = self.resolve(name);
                        let var_offset = var.offset;
                        match typ {
                            Type::Base(BaseType::Str) => {
                                _ = writeln!(
                                    self.asm,
                                    " mov rdi, [rbp + {var_offset}]\
                                    \n mov [rbp + {dst_offset}], rdi\n"
                                );
                            }
                            Type::Array { len, .. } => {
                                _ = writeln!(self.asm, " mov qword [rbp + {dst_offset}], {len}\n");
                            }
                            Type::Base(BaseType::Int | BaseType::Ascii | BaseType::Bool) => {
                                unreachable!("cannot take the length of numerical types")
                            }
                        }
                    }
                    Expression::Array { items, .. } => {
                        _ = writeln!(
                            self.asm,
                            " mov qword [rbp + {dst_offset}], {}\n",
                            items.len()
                        );
                    }
                    Expression::ArrayIndex { base_type: typ, var_name, bracket_position, index } => {
                        self.expression(index, Dst::Reg(Rdi));

                        let var = self.resolve(var_name);
                        let var_offset = var.offset;

                        let Type::Array { len, .. } = var.inner.value.typ() else {
                            unreachable!("only array of strings can appear here");
                        };

                        _ = writeln!(
                            self.asm,
                            " mov rsi, {len}\
                            \n mov rdx, {line}\
                            \n mov rcx, {col}\
                            \n call assert_array_index_in_range\
                            \n imul rdi, {typ_size}\
                            \n mov rdi, [rbp + {var_offset} + rdi]\
                            \n mov [rbp + {dst_offset}], rdi\n",
                            line = bracket_position.line,
                            col = bracket_position.col,
                            typ_size = typ.size(),
                        );
                    }
                    Expression::Literal(_)
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
                                \n mov [rbp + {dst_offset}], rdi\n"
                            );
                        }
                        Type::Base(BaseType::Ascii) => {
                            _ = writeln!(
                                self.asm,
                                " not rdi\
                                \n mov [rbp + {dst_offset}], dil\n"
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
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_safe_abs\
                                \n mov [rbp + {dst_offset}], rdi\n",
                                line = op_position.line,
                                col = op_position.col
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
                                \n mov [rbp + {dst_offset}], rdi\n"
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
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_saturating_abs\
                                \n mov [rbp + {dst_offset}], rdi\n",
                                line = op_position.line,
                                col = op_position.col
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
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_safe_negate\
                                \n mov [rbp + {dst_offset}], rdi\n",
                                line = op_position.line,
                                col = op_position.col
                            );
                        }
                        Type::Base(BaseType::Ascii) => {
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_safe_negate\
                                \n mov [rbp + {dst_offset}], dil\n",
                                line = op_position.line,
                                col = op_position.col
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
                                \n mov [rbp + {dst_offset}], rdi\n"
                            );
                        }
                        Type::Base(BaseType::Ascii) => {
                            _ = writeln!(
                                self.asm,
                                " neg rdi\
                                \n mov [rbp + {dst_offset}], dil\n"
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
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_saturating_negate\
                                \n mov [rbp + {dst_offset}], rdi\n",
                                line = op_position.line,
                                col = op_position.col
                            );
                        }
                        Type::Base(BaseType::Ascii) => {
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_saturating_negate\
                                \n mov [rbp + {dst_offset}], dil\n",
                                line = op_position.line,
                                col = op_position.col
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
                    \n mov [rbp + {dst_offset}], dil\n"
                );
            }
            Expression::Binary { .. } => {
                self.expression(value, Dst::Reg(Rdi));

                match value.typ() {
                    Type::Base(BaseType::Int) => {
                        _ = writeln!(self.asm, " mov [rbp + {dst_offset}], rdi\n");
                    }
                    Type::Base(BaseType::Ascii) => {
                        _ = writeln!(self.asm, " mov [rbp + {dst_offset}], dil\n");
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
                        _ = writeln!(self.asm, " mov [rbp + {dst_offset}], dil\n");
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
                _ = writeln!(self.asm, " mov [rbp + {dst_offset}], dil\n");
            }
            Expression::Identifier { typ: identifier_typ, name } => {
                let var = self.resolve(name);
                let src_offset = var.offset;
                match identifier_typ {
                    Type::Base(BaseType::Int) => {
                        _ = writeln!(
                            self.asm,
                            " mov rdi, [rbp + {src_offset}]\
                            \n mov [rbp + {dst_offset}], rdi\n"
                        );
                    }
                    Type::Base(BaseType::Ascii | BaseType::Bool) => {
                        _ = writeln!(
                            self.asm,
                            " mov dil, [rbp + {src_offset}]\
                            \n mov [rbp + {dst_offset}], dil\n"
                        );
                    }
                    Type::Base(BaseType::Str) => {
                        _ = writeln!(
                            self.asm,
                            " mov rdi, [rbp + {src_offset}]\
                            \n mov rsi, [rbp + {src_offset} + {ptr_offset}]\
                            \n mov [rbp + {dst_offset}], rdi\
                            \n mov [rbp + {dst_offset} + {ptr_offset}], rsi\n",
                            ptr_offset = std::mem::size_of::<uint>()
                        );
                    }
                    Type::Array { base_type: array_typ, len } => match *array_typ {
                        BaseType::Int => {
                            _ = writeln!(
                                self.asm,
                                " lea rdi, [rbp + {dst_offset}]\
                                \n lea rsi, [rbp + {src_offset}]\
                                \n mov rcx, {len}\
                                \n rep movsq\n"
                            );
                        }
                        BaseType::Ascii | BaseType::Bool => {
                            _ = writeln!(
                                self.asm,
                                " lea rdi, [rbp + {dst_offset}]\
                                \n lea rsi, [rbp + {src_offset}]\
                                \n mov rcx, {len}\
                                \n rep movsb\n"
                            );
                        }
                        BaseType::Str => {
                            _ = writeln!(
                                self.asm,
                                " lea rdi, [rbp + {dst_offset}]\
                                \n lea rsi, [rbp + {src_offset}]\
                                \n mov rcx, {len} * 2\
                                \n rep movsq\n"
                            );
                        }
                    },
                }
            }
            Expression::Array { base_type: typ, items } => {
                let typ_size = typ.size();
                for (index, item) in items.iter().enumerate() {
                    self.definition(item, dst_offset + index * typ_size);
                }
            }
            Expression::ArrayIndex { base_type: typ, .. } => {
                self.expression(value, Dst::default(&Type::Base(*typ)));

                match typ {
                    BaseType::Int => _ = writeln!(self.asm, " mov [rbp + {dst_offset}], rdi\n"),
                    BaseType::Ascii | BaseType::Bool => {
                        _ = writeln!(self.asm, " mov [rbp + {dst_offset}], dil\n");
                    }
                    BaseType::Str => {
                        _ = writeln!(
                            self.asm,
                            " mov [rbp + {dst_offset}], rdi\
                            \n mov [rbp + {dst_offset} + {ptr_offset}], rsi\n",
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
        let value_typ = value.typ();
        self.expression(value, Dst::default(&value_typ));

        match value_typ {
            Type::Base(BaseType::Int) => _ = writeln!(self.asm, " call int_print\n"),
            Type::Base(BaseType::Ascii) => _ = writeln!(self.asm, " call ascii_print\n"),
            Type::Base(BaseType::Bool) => _ = writeln!(self.asm, " call bool_print\n"),
            Type::Base(BaseType::Str) => _ = writeln!(self.asm, " call str_print\n"),
            Type::Array { base_type: typ, .. } => match typ {
                BaseType::Int => _ = writeln!(self.asm, " call int_array_debug_print\n"),
                BaseType::Ascii => _ = writeln!(self.asm, " call ascii_array_debug_print\n"),
                BaseType::Bool => _ = writeln!(self.asm, " call bool_array_debug_print\n"),
                BaseType::Str => _ = writeln!(self.asm, " call str_array_debug_print\n"),
            },
        }
    }

    fn eprint(&mut self, value: &'ast Expression<'src>) {
        let value_typ = value.typ();
        self.expression(value, Dst::default(&value_typ));

        match value_typ {
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
            Self::WritingAssemblyFailed { err }=> {
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
