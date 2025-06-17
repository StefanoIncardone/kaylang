/* TODO(stefano):
fix allocation of arrays of 0 items when thet will be allowed by either
- allow for zero size values
- place padding between elements (i.e. disallow zero size values by making them at least 1 byte long)
*/

pub mod artifacts;
mod asm;

use crate::front_end::{
    ast::{
        self, AssignmentOp, Ast, BaseType, BinaryOp, BooleanBinaryOp, ComparisonOp, Expression,
        IfStatement, Node, ScopeIndex, SizeOf as _, Type, TypeOf as _, UnaryOp,
    },
    src_file::{Position, SrcCode},
    tokenizer::ascii,
};
#[expect(clippy::useless_attribute, reason = "false positive")]
#[expect(clippy::pub_use)]
pub use back_to_front::back_end::x86_64::reg::Reg64;
use back_to_front::offset32;
use core::fmt::{Display, Write as _};
use Reg64::{Rcx, Rdi, Rdx, Rsi};
extern crate alloc;
use alloc::borrow::Cow;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Dst {
    Reg(Reg64),
    View { len: Reg64, ptr: Reg64 },
}

impl Dst {
    const fn default(typ: &Type) -> Self {
        return match typ {
            Type::Base(BaseType::I64 | BaseType::Ascii | BaseType::Bool) => Self::Reg(Rdi),
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
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return match self {
            Self::Rbp => write!(f, "rbp"),
            Self::Temp => write!(f, "temp"),
        };
    }
}

#[derive(Debug)]
struct Variable<'ast, 'code: 'ast> {
    inner: &'ast ast::Variable<'code>,
    offset: usize,
}

#[derive(Debug)]
struct TemporaryValue<'ast> {
    inner: &'ast Expression,
    offset: usize,
}
#[derive(Debug)]
pub struct Compiler<'ast, 'src: 'ast, 'path: 'src, 'code: 'src> {
    src: &'src SrcCode<'code, 'path>,
    ast: &'ast Ast<'code>,

    asm: String,

    variables: Vec<Variable<'ast, 'code>>,
    temporary_values: Vec<TemporaryValue<'ast>>,

    if_counter: u32,

    loop_counter: u32,
    loop_counters: Vec<u32>,
    // and_counter: usize,
    // or_counter: usize,
}

// Generation of compilation artifacts (.asm, .o, executable)
impl<'ast, 'src: 'ast, 'path: 'src, 'code: 'src> Compiler<'ast, 'src, 'path, 'code> {
    #[must_use]
    pub fn compile(src: &'src SrcCode<'code, 'path>, ast: &'ast Ast<'code>) -> String {
        use asm::{
            ASCII_ARRAY_DEBUG_EPRINT_ASM, ASCII_ARRAY_DEBUG_PRINT_ASM, ASCII_EPRINT_ASM,
            ASCII_PRINT_ASM, ASSERT_ARRAY_INDEX_IN_RANGE_ASM, ASSERT_STR_INDEX_IN_RANGE_ASM,
            BOOL_ARRAY_DEBUG_EPRINT_ASM, BOOL_ARRAY_DEBUG_PRINT_ASM, BOOL_EPRINT_ASM,
            BOOL_PRINT_ASM, CRASH_ASM, I64_ARRAY_DEBUG_EPRINT_ASM, I64_ARRAY_DEBUG_PRINT_ASM,
            I64_EPRINT_ASM, I64_PRINT_ASM, I64_SAFE_ABS_ASM, I64_SAFE_ADD_ASM, I64_SAFE_DIV_ASM,
            I64_SAFE_LEFT_ROTATE_ASM, I64_SAFE_LEFT_SHIFT_ASM, I64_SAFE_MUL_ASM,
            I64_SAFE_MUL_POW_ASM, I64_SAFE_NEGATE_ASM, I64_SAFE_POW_ASM, I64_SAFE_REMAINDER_ASM,
            I64_SAFE_RIGHT_ROTATE_ASM, I64_SAFE_RIGHT_SHIFT_ASM, I64_SAFE_SUB_ASM,
            I64_SATURATING_ABS_ASM, I64_SATURATING_ADD_ASM, I64_SATURATING_DIV_ASM,
            I64_SATURATING_LEFT_SHIFT_ASM, I64_SATURATING_MUL_ASM, I64_SATURATING_NEGATE_ASM,
            I64_SATURATING_POW_ASM, I64_SATURATING_SUB_ASM, I64_TO_STR_ASM, I64_WRAPPING_ABS_ASM,
            I64_WRAPPING_DIV_ASM, I64_WRAPPING_LEFT_SHIFT_ASM, I64_WRAPPING_POW_ASM,
            STR_ARRAY_CMP_ASM, STR_ARRAY_DEBUG_EPRINT_ASM, STR_ARRAY_DEBUG_PRINT_ASM,
            STR_ARRAY_EQ_ASM, STR_ARRAY_NEQ_ASM, STR_CMP_ASM, STR_EPRINT_ASM, STR_EQ_ASM,
            STR_NEQ_ASM, STR_PRINT_ASM,
        };

        let mut this = Compiler {
            src,
            ast,
            asm: String::new(),
            variables: Vec::with_capacity(ast.variables.len()),
            temporary_values: Vec::with_capacity(ast.temporaries.len()),
            if_counter: 0,
            loop_counter: 0,
            loop_counters: Vec::new(),
            // and_counter: 0,
            // or_counter: 0,
        };

        let mut temporary_values_bytes = 0;
        let mut strings = String::new();

        if !this.ast.nodes.is_empty() {
            // temporary values
            for var in &this.ast.temporaries {
                this.temporary_values.push(TemporaryValue { inner: var, offset: 0 });

                let var_size = var.typ().size();
                if var_size > temporary_values_bytes {
                    temporary_values_bytes = var_size;
                }
            }

            // TODO(stefano): dump raw string/character bytes instead of their "human readable text"
            // form strings
            for (label, string) in &this.ast.string_labels {
                // FIX(stefano): proper empty string handling, consecutive empty strings have the
                // same memory address
                if string.len() == 0 {
                    _ = writeln!(strings, " str str_{label}, ``");
                } else {
                    _ = write!(strings, " str str_{label}, `");
                    let mut chars_index = 0;
                    let chars = string.as_bytes();
                    while chars_index < chars.len() {
                        let ch = chars[chars_index];
                        chars_index += 1;
                        match ch {
                            b'\\' => {
                                _ = write!(strings, "\\");
                                let escape = chars[chars_index];
                                chars_index += 1;
                                _ = write!(strings, "{}", escape as char);
                            }
                            other => {
                                _ = write!(strings, "{}", other as char);
                            }
                        }
                    }
                    _ = writeln!(strings, "`");
                }
            }

            for (label, string) in &this.ast.raw_string_labels {
                // FIX(stefano): proper empty string handling
                if string.len() == 0 {
                    _ = writeln!(strings, " str str_{label}, ``");
                } else {
                    _ = write!(strings, " str str_{label}, `");
                    let mut chars_index = 0;
                    let chars = string.as_bytes();
                    while chars_index < chars.len() {
                        let ch = chars[chars_index];
                        chars_index += 1;
                        match ch {
                            b'\\' => {
                                _ = write!(strings, "\\");
                                let escape = chars[chars_index];
                                if escape == b'"' {
                                    chars_index += 1;
                                    _ = write!(strings, "\"");
                                } else {
                                    _ = write!(strings, "\\");
                                }
                            }
                            other => {
                                _ = write!(strings, "{}", other as char);
                            }
                        }
                    }
                    _ = writeln!(strings, "`");
                }
            }

            // variables
            for var in &this.ast.variables {
                this.variables.push(Variable { inner: var, offset: 0 /* placeholder */ });
            }

            this.variables.sort_by(|var_1, var_2| {
                return var_2.inner.value.typ().size().cmp(&var_1.inner.value.typ().size());
            });

            let mut stack_size = 0;
            for var in &mut this.variables {
                var.offset = stack_size;
                stack_size += var.inner.value.typ().size();
            }

            if stack_size > 0 {
                const STACK_ALIGN: usize = size_of::<usize>();

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
        }

        let program = format!(
            r#"global _start

section .text
_start:
{asm}
 mov rdi, EXIT_SUCCESS
 mov rax, SYS_exit
 syscall

{CRASH_ASM}

{ASSERT_ARRAY_INDEX_IN_RANGE_ASM}

{ASSERT_STR_INDEX_IN_RANGE_ASM}

{I64_TO_STR_ASM}

{I64_SAFE_POW_ASM}

{I64_WRAPPING_POW_ASM}

{I64_SATURATING_POW_ASM}

{I64_SAFE_MUL_POW_ASM}

{I64_SAFE_MUL_ASM}

{I64_SATURATING_MUL_ASM}

{I64_SAFE_DIV_ASM}

{I64_WRAPPING_DIV_ASM}

{I64_SATURATING_DIV_ASM}

{I64_SAFE_REMAINDER_ASM}

{I64_SAFE_ADD_ASM}

{I64_SATURATING_ADD_ASM}

{I64_SAFE_ABS_ASM}

{I64_WRAPPING_ABS_ASM}

{I64_SATURATING_ABS_ASM}

{I64_SAFE_SUB_ASM}

{I64_SATURATING_SUB_ASM}

{I64_SAFE_NEGATE_ASM}

{I64_SATURATING_NEGATE_ASM}

{I64_SAFE_LEFT_SHIFT_ASM}

{I64_WRAPPING_LEFT_SHIFT_ASM}

{I64_SATURATING_LEFT_SHIFT_ASM}

{I64_SAFE_RIGHT_SHIFT_ASM}

{I64_SAFE_LEFT_ROTATE_ASM}

{I64_SAFE_RIGHT_ROTATE_ASM}

{I64_PRINT_ASM}

{I64_EPRINT_ASM}

{I64_ARRAY_DEBUG_PRINT_ASM}

{I64_ARRAY_DEBUG_EPRINT_ASM}

{ASCII_PRINT_ASM}

{ASCII_EPRINT_ASM}

{ASCII_ARRAY_DEBUG_PRINT_ASM}

{ASCII_ARRAY_DEBUG_EPRINT_ASM}

{BOOL_PRINT_ASM}

{BOOL_EPRINT_ASM}

{BOOL_ARRAY_DEBUG_PRINT_ASM}

{BOOL_ARRAY_DEBUG_EPRINT_ASM}

{STR_EQ_ASM}

{STR_NEQ_ASM}

{STR_CMP_ASM}

{STR_ARRAY_EQ_ASM}

{STR_ARRAY_NEQ_ASM}

{STR_ARRAY_CMP_ASM}

{STR_PRINT_ASM}

{STR_EPRINT_ASM}

{STR_ARRAY_DEBUG_PRINT_ASM}

{STR_ARRAY_DEBUG_EPRINT_ASM}

%macro str 2
 %1: db %2
 %1_len: equ $ - %1
%endmacro

section .rodata
 stdout: equ 1
 stderr: equ 2
 SYS_write: equ 1
 SYS_exit: equ 60
 EXIT_SUCCESS: equ 0
 EXIT_FAILURE: equ 1

 I64_MIN: equ 1 << 63
 I64_MAX: equ ~I64_MIN
 I64_BITS: equ 64

 LESS: equ -1
 EQUAL: equ 0
 GREATER: equ 1

 newline: equ `\n`

 str CRASH, "Crash"
 str _AT, "at"
 str file, "{src_path}"

 str attempt_division_by_zero, "attempt to divide by zero"
 str attempt_remainder_zero, "attempt to take the remainder of a division by zero"
 str attempt_exponent_negative, "attempt to raise an integer to a negative power"
 str attempt_array_index_underflow, "negative array index"
 str attempt_array_index_overflow, "array index out of bounds"
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
 str div_overflow, "division operation resulted in an overflow"
 str remainder_overflow, "remainder operation resulted in an overflow"
 str add_overflow, "add operation resulted in an overflow"
 str abs_overflow, "unary absolute value operation resulted in an overflow"
 str sub_overflow, "subtraction operation resulted in an overflow"
 str negate_overflow, "unary negation operation resulted in an overflow"
 str left_shift_overflow, "left shift operation resulted in an overflow"

 true: equ 1
 str true_str, "true"

 false: equ 0
 str false_str, "false"

section .bss
 i64_str: resb I64_BITS
 temp: resb {temporary_values_bytes}

section .data
{strings}"#,
            asm = this.asm,
            src_path = src.path().display(),
        );

        return program;
    }
}

// nodes
impl<'ast> Compiler<'ast, '_, '_, '_> {
    fn node(&mut self, node: &'ast Node) {
        match node {
            Node::Print(argument) => {
                _ = writeln!(self.asm, " ; print {}", argument.display(self.ast));
                self.print(argument);
            }
            Node::Println(argument) => {
                if let Some(arg) = argument {
                    _ = writeln!(self.asm, " ; println {}", arg.display(self.ast));
                    self.print(arg);
                } else {
                    _ = writeln!(self.asm, " ; println");
                }

                _ = writeln!(
                    self.asm,
                    " mov dil, newline\
                    \n call ascii_print\n"
                );
            }
            Node::Eprint(argument) => {
                _ = writeln!(self.asm, " ; eprint {}", argument.display(self.ast));
                self.eprint(argument);
            }
            Node::Eprintln(argument) => {
                if let Some(arg) = argument {
                    _ = writeln!(self.asm, " ; eprintln {}", arg.display(self.ast));
                    self.eprint(arg);
                } else {
                    _ = writeln!(self.asm, " ; eprintln");
                }

                _ = writeln!(
                    self.asm,
                    " mov dil, newline\
                    \n call ascii_eprint\n"
                );
            }
            Node::If(if_index) => {
                let if_counter = self.if_counter;
                self.if_counter += 1;

                let if_statement = &self.ast.ifs[*if_index as usize];
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
            Node::Loop(loop_index) => {
                let loop_tag = format!("loop_{}", self.loop_counter);
                let loop_end_tag = format!("loop_{}_end", self.loop_counter);

                self.loop_counters.push(self.loop_counter);
                self.loop_counter += 1;

                let looop = &self.ast.loops[*loop_index as usize];
                _ = writeln!(self.asm, "{loop_tag}:; loop {}", looop.condition.display(self.ast));
                self.condition(&looop.condition, &loop_end_tag);
                self.node(&looop.statement);

                _ = writeln!(
                    self.asm,
                    " jmp {loop_tag}\
                    \n{loop_end_tag}:\n"
                );

                _ = self.loop_counters.pop();
            }
            Node::DoLoop(do_loop_index) => {
                let loop_tag = format!("loop_{}", self.loop_counter);

                self.loop_counters.push(self.loop_counter);
                self.loop_counter += 1;

                let do_loop = &self.ast.loops[*do_loop_index as usize];
                _ = writeln!(
                    self.asm,
                    "{loop_tag}:; do loop {}",
                    do_loop.condition.display(self.ast)
                );
                self.node(&do_loop.statement);
                self.condition_reversed(&do_loop.condition, &loop_tag);
                _ = self.loop_counters.pop();
            }
            Node::Definition { var_index } => {
                let ast_var = &self.ast.variables[*var_index as usize];

                let ast_variable_name_str = unsafe { core::str::from_utf8_unchecked(ast_var.name) };
                let value = &ast_var.value;

                let var = self.resolve(ast_var.name);
                let dst_offset = var.offset;

                _ = writeln!(self.asm, " ; {ast_variable_name_str} = {}", value.display(self.ast));
                self.definition(value, Base::Rbp, dst_offset);
            }
            Node::Reassignment { target, op, op_col, new_value } => {
                self.reassignment(target, *op, *op_col, new_value);
            }
            Node::Scope { index } => self.scope(*index),
            Node::Expression(expression) => {
                _ = writeln!(self.asm, " ; {}", expression.display(self.ast));
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
            Node::Semicolon | Node::ScopeEnd => unreachable!("should not be present in the ast"),
        }
    }

    fn scope(&mut self, scope_index: ScopeIndex) {
        let scope = &self.ast.nodes[scope_index as usize];
        for node in scope {
            self.node(node);
        }
    }
}

// expressions
impl<'ast, 'code: 'ast> Compiler<'ast, '_, '_, 'code> {
    fn resolve(&self, name: &'code [ascii]) -> &Variable<'ast, 'code> {
        for var in &self.variables {
            if var.inner.name == name {
                return var;
            }
        }

        unreachable!("should always find a variable");
    }

    fn resolve_temporary(&self, value: &'ast Expression) -> &TemporaryValue<'ast> {
        for temporary in &self.temporary_values {
            if core::ptr::eq(temporary.inner, value) {
                return temporary;
            }
        }

        unreachable!("should always find a temporary value");
    }

    fn lhs_needs_saving(&self, expression: &'ast Expression) -> bool {
        return match expression {
            Expression::Array { .. } => {
                unreachable!("arrays cannot appear in expressions");
            }

            // these expressions do not need to save the value of the lhs
            Expression::False
            | Expression::True
            | Expression::I64(_)
            | Expression::Ascii(_)
            | Expression::Str { .. }
            | Expression::Unary { op: UnaryOp::Not | UnaryOp::WrappingMinus, .. }
            | Expression::BooleanUnary { .. }
            | Expression::Variable { .. } => false,

            // these expressions need to save the value of the lhs
            Expression::Unary { .. }
            | Expression::Binary { .. }
            | Expression::BooleanBinary { .. }
            | Expression::Comparison { .. }
            | Expression::ArrayIndex { .. } => true,

            Expression::Parenthesis { expression_index, .. } => {
                let inner = &self.ast.expressions[*expression_index as usize];
                self.lhs_needs_saving(inner)
            }
            Expression::Temporary { temporary_value_index, .. } => {
                let temporary = &self.ast.temporaries[*temporary_value_index as usize];
                self.lhs_needs_saving(temporary)
            }
        };
    }

    fn binary_expression(
        &mut self,
        lhs: &'ast Expression,
        rhs: &'ast Expression,
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
            Type::Base(BaseType::I64) => match dst {
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
                        ptr_offset = size_of::<u64>()
                    );
                }
                Dst::Reg(_) => unreachable!(),
            },
            Type::Array { len: array_len, .. } => {
                debug_assert!(array_len > 0, "arrays of 0 items are not allowed");
                match dst {
                    Dst::View { len, ptr } => {
                        _ = writeln!(
                            self.asm,
                            " mov {len}, {array_len}\
                            \n lea {ptr}, [{base} + {offset}]"
                        );
                    }
                    Dst::Reg(_) => unreachable!(),
                }
            }
        }
    }

    fn index(
        &mut self,
        base_type: BaseType,
        value: &'ast Expression,
        bracket_col: offset32,
        index: &'ast Expression,
    ) {
        let Position { line, column } = self.src.position(bracket_col);

        match value {
            Expression::Parenthesis { .. } => {
                unreachable!("should have been disallowed during parsing")
            }
            Expression::Str { label } => {
                self.expression(index, Dst::Reg(Rdi));
                _ = writeln!(
                    self.asm,
                    " mov rsi, str_{label}_len\
                    \n mov rdx, {line}\
                    \n mov rcx, {column}\
                    \n call assert_str_index_in_range\
                    \n movzx rdi, byte [str_{label} + rdi]\n",
                );
            }
            Expression::ArrayIndex {
                base_type: nested_base_type,
                indexable_index: nested_indexable_index,
                bracket_col: nested_bracket_col,
                index_expression_index: nested_index_expression_index,
            } => {
                let nested_indexable = &self.ast.expressions[*nested_indexable_index as usize];
                let nested_index_expression =
                    &self.ast.expressions[*nested_index_expression_index as usize];
                self.index(
                    *nested_base_type,
                    nested_indexable,
                    *nested_bracket_col,
                    nested_index_expression,
                );
                match nested_base_type {
                    BaseType::Str => {
                        _ = writeln!(
                            self.asm,
                            " push rsi\
                            \n push rdi\n"
                        );
                    }
                    BaseType::I64 | BaseType::Ascii | BaseType::Bool => {
                        unreachable!(
                            "only arrays and strings are allowed in nested index expressions"
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
                            \n mov rcx, {column}\
                            \n call assert_str_index_in_range\
                            \n pop rsi\
                            \n movzx rdi, byte [rsi + rdi]\n",
                        );
                    }
                    BaseType::I64 | BaseType::Str | BaseType::Bool => {
                        unreachable!("only ascii are allowed in nested index expressions")
                    }
                }
            }
            Expression::Variable { typ, variable_index } => {
                self.expression(index, Dst::Reg(Rdi));

                let ast_variable = &self.ast.variables[*variable_index as usize];
                let var = self.resolve(ast_variable.name);
                let var_offset = var.offset;

                match typ {
                    Type::Base(BaseType::Str) => {
                        _ = writeln!(
                            self.asm,
                            " mov rsi, [rbp + {var_offset}]\
                            \n mov rdx, {line}\
                            \n mov rcx, {column}\
                            \n call assert_str_index_in_range\
                            \n mov rsi, [rbp + {var_offset} + {ptr_offset}]\
                            \n movzx rdi, byte [rsi + rdi]\n",
                            ptr_offset = size_of::<u64>()
                        );
                    }
                    Type::Array { len: array_len, .. } => {
                        debug_assert!(*array_len > 0, "arrays of 0 items are not allowed");
                        _ = writeln!(
                            self.asm,
                            " mov rsi, {array_len}\
                            \n mov rdx, {line}\
                            \n mov rcx, {column}\
                            \n call assert_array_index_in_range"
                        );

                        match base_type {
                            BaseType::I64 => {
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
                                    \n mov rsi, [rbp + {var_offset} + rdi + {ptr_offset}]\
                                    \n mov rdi, [rbp + {var_offset} + rdi]\n",
                                    base_type_size = base_type.size(),
                                    ptr_offset = size_of::<u64>()
                                );
                            }
                        }
                    }
                    Type::Base(BaseType::I64 | BaseType::Ascii | BaseType::Bool) => {
                        unreachable!("only arrays and strings are allowed in index expressions")
                    }
                }
            }

            Expression::Array { .. }
            | Expression::Temporary { .. }
            | Expression::I64(_)
            | Expression::False
            | Expression::True
            | Expression::Ascii(_)
            | Expression::BooleanUnary { .. }
            | Expression::BooleanBinary { .. }
            | Expression::Unary { .. }
            | Expression::Binary { .. }
            | Expression::Comparison { .. } => {
                unreachable!("only arrays and strings are allowed in index expressions")
            }
        }
    }

    fn expression(&mut self, factor: &'ast Expression, dst: Dst) {
        match factor {
            Expression::Parenthesis { expression_index, .. } => {
                let inner = &self.ast.expressions[*expression_index as usize];
                self.expression(inner, dst);
            }
            Expression::I64(integer) => match dst {
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
            Expression::Str { label } => match dst {
                Dst::View { len, ptr } => {
                    _ = writeln!(
                        self.asm,
                        " mov {len}, str_{label}_len\
                        \n mov {ptr}, str_{label}"
                    );
                }
                Dst::Reg(_) => unreachable!(),
            },
            Expression::Array { .. } => unreachable!("arrays cannot appear in expressions"),
            Expression::Unary { op, op_col, operand_index } => {
                let operand = &self.ast.expressions[*operand_index as usize];
                let Dst::Reg(reg) = dst else {
                    unreachable!();
                };

                let mut unwrapped_operand = operand;
                while let Expression::Parenthesis { expression_index, .. } = unwrapped_operand {
                    let inner = &self.ast.expressions[*expression_index as usize];
                    unwrapped_operand = inner;
                }

                match op {
                    UnaryOp::Len => match unwrapped_operand {
                        Expression::Parenthesis { .. } => {
                            unreachable!("should have been unwrapped")
                        }
                        Expression::Str { label } => {
                            _ = writeln!(self.asm, " mov {reg}, str_{label}_len");
                        }
                        Expression::Array { items, .. } => {
                            _ = writeln!(self.asm, " mov {reg}, {}", items.len());
                        }
                        Expression::Variable { typ, variable_index } => {
                            let ast_variable = &self.ast.variables[*variable_index as usize];
                            let var = self.resolve(ast_variable.name);
                            let var_offset = var.offset;
                            match typ {
                                Type::Base(BaseType::Str) => {
                                    _ = writeln!(self.asm, " mov {reg}, [rbp + {var_offset}]");
                                }
                                Type::Array { len, .. } => {
                                    debug_assert!(*len > 0, "arrays of 0 items are not allowed");
                                    _ = writeln!(self.asm, " mov {reg}, {len}");
                                }
                                Type::Base(BaseType::I64 | BaseType::Ascii | BaseType::Bool) => {
                                    unreachable!("cannot take the length of numerical types")
                                }
                            }
                        }
                        Expression::ArrayIndex {
                            base_type,
                            indexable_index,
                            bracket_col,
                            index_expression_index,
                        } => {
                            let indexable = &self.ast.expressions[*indexable_index as usize];
                            let index_expression =
                                &self.ast.expressions[*index_expression_index as usize];
                            self.index(*base_type, indexable, *bracket_col, index_expression);
                            _ = writeln!(self.asm, "mov {reg}, rdi\n");
                        }
                        Expression::False
                        | Expression::True
                        | Expression::I64(_)
                        | Expression::Ascii(_)
                        | Expression::Unary { .. }
                        | Expression::BooleanUnary { .. }
                        | Expression::Binary { .. }
                        | Expression::BooleanBinary { .. }
                        | Expression::Comparison { .. } => {
                            unreachable!("cannot take the length of numerical types")
                        }
                        Expression::Temporary { .. } => {
                            unreachable!("should not appear in expressions");
                        }
                    },
                    UnaryOp::Not => {
                        self.expression(operand, dst);
                        let Type::Base(BaseType::I64) = operand.typ() else {
                            unreachable!("can only invert integer and ascii values");
                        };

                        _ = writeln!(self.asm, " not {reg}");
                    }
                    UnaryOp::Plus => {
                        self.expression(operand, dst);
                        match operand.typ() {
                            Type::Base(BaseType::I64) => {
                                let Position { line, column } = self.src.position(*op_col);
                                _ = writeln!(
                                    self.asm,
                                    " mov rdx, {line}\
                                    \n mov rcx, {column}\
                                    \n call i64_safe_abs",
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
                            Type::Base(BaseType::I64) => {
                                _ = writeln!(self.asm, " call i64_wrapping_abs");
                            }
                            Type::Base(BaseType::Ascii | BaseType::Bool | BaseType::Str)
                            | Type::Array { .. } => {
                                unreachable!("cannot take absolute value of non i64 values");
                            }
                        }
                    }
                    UnaryOp::SaturatingPlus => {
                        self.expression(operand, dst);
                        match operand.typ() {
                            Type::Base(BaseType::I64) => {
                                let Position { line, column } = self.src.position(*op_col);
                                _ = writeln!(
                                    self.asm,
                                    " mov rdx, {line}\
                                    \n mov rcx, {column}\
                                    \n call i64_saturating_abs",
                                );
                            }
                            Type::Base(BaseType::Ascii | BaseType::Bool | BaseType::Str)
                            | Type::Array { .. } => {
                                unreachable!("cannot take absolute value of non i64 values");
                            }
                        }
                    }
                    UnaryOp::Minus => {
                        self.expression(operand, dst);
                        match operand.typ() {
                            Type::Base(BaseType::I64 | BaseType::Ascii) => {
                                let Position { line, column } = self.src.position(*op_col);
                                _ = writeln!(
                                    self.asm,
                                    " mov rdx, {line}\
                                    \n mov rcx, {column}\
                                    \n call i64_safe_negate",
                                );
                            }
                            Type::Base(BaseType::Bool | BaseType::Str) | Type::Array { .. } => {
                                unreachable!("cannot negate non i64/ascii values");
                            }
                        }
                    }
                    UnaryOp::WrappingMinus => {
                        self.expression(operand, dst);
                        match operand.typ() {
                            Type::Base(BaseType::I64 | BaseType::Ascii) => {
                                _ = writeln!(self.asm, " neg {reg}");
                            }
                            Type::Base(BaseType::Bool | BaseType::Str) | Type::Array { .. } => {
                                unreachable!("cannot negate non i64/ascii values");
                            }
                        }
                    }
                    UnaryOp::SaturatingMinus => {
                        self.expression(operand, dst);
                        match operand.typ() {
                            Type::Base(BaseType::I64 | BaseType::Ascii) => {
                                let Position { line, column } = self.src.position(*op_col);
                                _ = writeln!(
                                    self.asm,
                                    " mov rdx, {line}\
                                    \n mov rcx, {column}\
                                    \n call i64_saturating_negate",
                                );
                            }
                            Type::Base(BaseType::Bool | BaseType::Str) | Type::Array { .. } => {
                                unreachable!("cannot negate non i64/ascii values");
                            }
                        }
                    }
                }
            }
            Expression::BooleanUnary { operand_index, .. } => {
                let operand = &self.ast.expressions[*operand_index as usize];
                let Dst::Reg(reg) = dst else {
                    unreachable!();
                };

                let Type::Base(BaseType::Bool) = operand.typ() else {
                    unreachable!("can only invert boolean values");
                };

                self.expression(operand, dst);

                _ = writeln!(self.asm, " xor {reg}, 1");
            }
            /* NOTE(stefano):
            hard-coding the first and second operand until a better way to manage
            dst and src are developed
            */
            /* IDEA(stefano):
            limit shift/rotation rhs to an 8bit integer, and different strategies to
            deal whit rhs over 6bits:
            - check for an rhs bigger than 8 bits and crash (current)
            - silently discard the missing bits
            - create dedicate operators that implement those strategies
            */
            // Note: strings and arrays cannot appear in expressions
            Expression::Binary { lhs_index, op, op_col, rhs_index } => {
                let lhs = &self.ast.expressions[*lhs_index as usize];
                let rhs = &self.ast.expressions[*rhs_index as usize];

                let lhs_dst = Dst::Reg(Rdi);
                let rhs_dst = Dst::Reg(Rsi);
                let op_asm: Cow<'static, str> = match op {
                    BinaryOp::Pow => {
                        let Position { line, column } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {column}\
                            \n call i64_safe_pow",
                        )
                        .into()
                    }
                    BinaryOp::WrappingPow => {
                        let Position { line, column } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {column}\
                            \n call i64_wrapping_pow",
                        )
                        .into()
                    }
                    BinaryOp::SaturatingPow => {
                        let Position { line, column } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {column}\
                            \n call i64_saturating_pow",
                        )
                        .into()
                    }
                    BinaryOp::Times => {
                        let Position { line, column } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {column}\
                            \n call i64_safe_mul",
                        )
                        .into()
                    }
                    BinaryOp::WrappingTimes => " imul rdi, rsi".into(),
                    BinaryOp::SaturatingTimes => " call i64_saturating_mul".into(),
                    BinaryOp::Divide => {
                        let Position { line, column } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {column}\
                            \n call i64_safe_div",
                        )
                        .into()
                    }
                    BinaryOp::WrappingDivide => {
                        let Position { line, column } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {column}\
                            \n call i64_wrapping_div",
                        )
                        .into()
                    }
                    BinaryOp::SaturatingDivide => {
                        let Position { line, column } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {column}\
                            \n call i64_saturating_div",
                        )
                        .into()
                    }
                    BinaryOp::Remainder => {
                        let Position { line, column } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {column}\
                            \n call i64_safe_remainder",
                        )
                        .into()
                    }
                    BinaryOp::Plus => {
                        let Position { line, column } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {column}\
                            \n call i64_safe_add",
                        )
                        .into()
                    }
                    BinaryOp::WrappingPlus => " add rdi, rsi".into(),
                    BinaryOp::SaturatingPlus => " call i64_saturating_add".into(),
                    BinaryOp::Minus => {
                        let Position { line, column } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {column}\
                            \n call i64_safe_sub",
                        )
                        .into()
                    }
                    BinaryOp::WrappingMinus => " sub rdi, rsi".into(),
                    BinaryOp::SaturatingMinus => " call i64_saturating_sub".into(),
                    BinaryOp::BitAnd => " and rdi, rsi".into(),
                    BinaryOp::BitOr => " or rdi, rsi".into(),
                    BinaryOp::BitXor => " xor rdi, rsi".into(),
                    BinaryOp::LeftShift => {
                        let Position { line, column } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {column}\
                            \n call i64_safe_left_shift",
                        )
                        .into()
                    }
                    BinaryOp::WrappingLeftShift => {
                        let Position { line, column } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {column}\
                            \n call i64_wrapping_left_shift",
                        )
                        .into()
                    }
                    BinaryOp::SaturatingLeftShift => {
                        let Position { line, column } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {column}\
                            \n call i64_saturating_left_shift",
                        )
                        .into()
                    }
                    BinaryOp::RightShift => {
                        let Position { line, column } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {column}\
                            \n call i64_safe_right_shift",
                        )
                        .into()
                    }
                    BinaryOp::LeftRotate => {
                        let Position { line, column } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {column}\
                            \n call i64_safe_left_rotate",
                        )
                        .into()
                    }
                    BinaryOp::RightRotate => {
                        let Position { line, column } = self.src.position(*op_col);
                        format!(
                            " mov rdx, {line}\
                            \n mov rcx, {column}\
                            \n call i64_safe_right_rotate",
                        )
                        .into()
                    }
                };

                self.binary_expression(lhs, rhs, lhs_dst, rhs_dst);

                _ = writeln!(self.asm, "{op_asm}\n");
            }
            // Note: strings and arrays cannot appear in expressions
            Expression::BooleanBinary { lhs_index, op, rhs_index } => {
                let lhs = &self.ast.expressions[*lhs_index as usize];
                let rhs = &self.ast.expressions[*rhs_index as usize];

                let lhs_dst = Dst::Reg(Rdi);
                let rhs_dst = Dst::Reg(Rsi);
                let op_asm = match op {
                    BooleanBinaryOp::And => " and rdi, rsi",
                    BooleanBinaryOp::Or => " or rdi, rsi",
                };

                self.binary_expression(lhs, rhs, lhs_dst, rhs_dst);

                _ = writeln!(self.asm, "{op_asm}\n");
            }
            /* IDEA(stefano):
            string/array comparison operators could also return the index where the
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
            Expression::Comparison { lhs_index, op, rhs_index } => {
                let lhs = &self.ast.expressions[*lhs_index as usize];
                let rhs = &self.ast.expressions[*rhs_index as usize];

                let (lhs_dst, rhs_dst, op_asm): (Dst, Dst, Cow<'static, str>) =
                    match (lhs.typ(), rhs.typ()) {
                        (Type::Base(BaseType::Str), Type::Base(BaseType::Str)) => (
                            Dst::View { len: Rdi, ptr: Rsi },
                            Dst::View { len: Rdx, ptr: Rcx },
                            match op {
                                ComparisonOp::EqualsEquals => " call str_eq".into(),
                                ComparisonOp::NotEquals => " call str_neq".into(),
                                ComparisonOp::Greater => " call str_cmp\
                                    \n cmp rdi, EQUAL\
                                    \n mov rdi, false\
                                    \n setg dil"
                                    .into(),
                                ComparisonOp::GreaterOrEquals => " call str_cmp\
                                    \n cmp rdi, EQUAL\
                                    \n mov rdi, false\
                                    \n setge dil"
                                    .into(),
                                ComparisonOp::Less => " call str_cmp\
                                    \n cmp rdi, EQUAL\
                                    \n mov rdi, false\
                                    \n setl dil"
                                    .into(),
                                ComparisonOp::LessOrEquals => " call str_cmp\
                                    \n cmp rdi, EQUAL\
                                    \n mov rdi, false\
                                    \n setle dil"
                                    .into(),
                                ComparisonOp::Compare => " call str_cmp".into(),
                            },
                        ),
                        // Note: we can only compare non-empty arrays of the same type and length, so
                        // its safe to only match on the first array type and not to check for empty arrays
                        (Type::Array { base_type, .. }, Type::Array { .. }) => (
                            Dst::View { len: Rdi, ptr: Rsi },
                            Dst::View { len: Rdx, ptr: Rcx },
                            match op {
                                ComparisonOp::EqualsEquals => match base_type {
                                    BaseType::I64 => " mov rdi, rcx\
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
                                    BaseType::Str => " call str_array_eq".into(),
                                },
                                ComparisonOp::NotEquals => match base_type {
                                    BaseType::I64 => " mov rdi, rcx\
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
                                    BaseType::Str => " cmp str_array_neq".into(),
                                },
                                ComparisonOp::Greater => match base_type {
                                    BaseType::I64 => " mov rdi, rcx\
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
                                        \n cmp rdi, EQUAL\
                                        \n mov rdi, false\
                                        \n setg dil"
                                        .into(),
                                },
                                ComparisonOp::GreaterOrEquals => match base_type {
                                    BaseType::I64 => " mov rdi, rcx\
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
                                        \n cmp rdi, EQUAL\
                                        \n mov rdi, false\
                                        \n setge dil"
                                        .into(),
                                },
                                ComparisonOp::Less => match base_type {
                                    BaseType::I64 => " mov rdi, rcx\
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
                                        \n cmp rdi, EQUAL\
                                        \n mov rdi, false\
                                        \n setl dil"
                                        .into(),
                                },
                                ComparisonOp::LessOrEquals => match base_type {
                                    BaseType::I64 => " mov rdi, rcx\
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
                                        \n cmp rdi, EQUAL\
                                        \n mov rdi, false\
                                        \n setle dil"
                                        .into(),
                                },
                                ComparisonOp::Compare => match base_type {
                                    BaseType::I64 => " mov rdi, rcx\
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
                                    BaseType::Str => " call str_array_cmp".into(),
                                },
                            },
                        ),
                        (
                            Type::Base(BaseType::I64 | BaseType::Ascii | BaseType::Bool),
                            Type::Base(BaseType::I64 | BaseType::Ascii | BaseType::Bool),
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
                let temporary_value_expression =
                    &self.ast.temporaries[*temporary_value_index as usize];
                let temporary_value = self.resolve_temporary(temporary_value_expression);
                let temporary_value_offset = temporary_value.offset;

                self.definition(temporary_value_expression, Base::Temp, temporary_value_offset);
                self.identifier(
                    temporary_value_expression.typ(),
                    dst,
                    Base::Temp,
                    temporary_value_offset,
                );
            }
            Expression::Variable { typ, variable_index } => {
                let ast_variable = &self.ast.variables[*variable_index as usize];
                let var = self.resolve(ast_variable.name);
                let var_offset = var.offset;
                self.identifier(*typ, dst, Base::Rbp, var_offset);
            }
            Expression::ArrayIndex {
                base_type,
                indexable_index,
                bracket_col,
                index_expression_index,
            } => {
                let indexable = &self.ast.expressions[*indexable_index as usize];
                let index_expression = &self.ast.expressions[*index_expression_index as usize];

                self.index(*base_type, indexable, *bracket_col, index_expression);
            }
        }
    }

    fn condition(&mut self, condition: &'ast Expression, false_tag: &str) {
        match condition {
            Expression::Parenthesis { expression_index, .. } => {
                let inner = &self.ast.expressions[*expression_index as usize];
                self.condition(inner, false_tag);
            }
            // IDEA(stefano): optimize these checks by doing a plain jmp instead
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
            | Expression::I64(_)
            | Expression::Ascii(_)
            | Expression::Str { .. } => {
                unreachable!("non-boolean expressions not allowed in conditions");
            }
            Expression::Temporary { .. } => {
                unreachable!("should not appear in conditions");
            }
            Expression::BooleanUnary { operand_index, .. } => {
                let operand = &self.ast.expressions[*operand_index as usize];
                self.expression(operand, Dst::Reg(Rdi));

                // we can only have boolean expressions at this point, so it's safe to ignore the integer negation case
                _ = writeln!(
                    self.asm,
                    " xor dil, 1\
                    \n jz {false_tag}\n"
                );
            }
            Expression::BooleanBinary { lhs_index, op, rhs_index, .. } => {
                let lhs = &self.ast.expressions[*lhs_index as usize];
                let rhs = &self.ast.expressions[*rhs_index as usize];

                let lhs_dst = match lhs.typ() {
                    Type::Base(BaseType::Bool) => Dst::Reg(Rdi),
                    Type::Base(BaseType::I64 | BaseType::Ascii | BaseType::Str)
                    | Type::Array { .. } => {
                        unreachable!("non-boolean expressions not allowed in conditions");
                    }
                };

                let rhs_dst = match rhs.typ() {
                    Type::Base(BaseType::Bool) => Dst::Reg(Rsi),
                    Type::Base(BaseType::I64 | BaseType::Ascii | BaseType::Str)
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
            Expression::Comparison { lhs_index, op, rhs_index, .. } => {
                let lhs = &self.ast.expressions[*lhs_index as usize];
                let rhs = &self.ast.expressions[*rhs_index as usize];

                let lhs_dst = match lhs.typ() {
                    Type::Base(BaseType::I64 | BaseType::Ascii | BaseType::Bool) => Dst::Reg(Rdi),
                    Type::Base(BaseType::Str) | Type::Array { .. } => {
                        Dst::View { len: Rdi, ptr: Rsi }
                    }
                };

                let rhs_dst = match rhs.typ() {
                    Type::Base(BaseType::I64 | BaseType::Ascii | BaseType::Bool) => Dst::Reg(Rsi),
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
            Expression::Variable { variable_index, .. } => {
                let ast_variable = &self.ast.variables[*variable_index as usize];
                let var = self.resolve(ast_variable.name);
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

    fn condition_reversed(&mut self, condition: &'ast Expression, true_tag: &str) {
        match condition {
            Expression::Parenthesis { expression_index, .. } => {
                let inner = &self.ast.expressions[*expression_index as usize];
                self.condition_reversed(inner, true_tag);
            }
            // IDEA(stefano): optimize these checks by doing a plain jmp instead
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
            | Expression::I64(_)
            | Expression::Ascii(_)
            | Expression::Str { .. } => {
                unreachable!("non-boolean expressions not allowed in conditions");
            }
            Expression::Temporary { .. } => {
                unreachable!("should not appear in conditions");
            }
            Expression::BooleanUnary { operand_index, .. } => {
                let operand = &self.ast.expressions[*operand_index as usize];
                self.expression(operand, Dst::Reg(Rdi));

                // we can only have boolean expressions at this point, so it's safe to ignore the integer negation case
                _ = writeln!(
                    self.asm,
                    " xor dil, 1\
                    \n jnz {true_tag}\n"
                );
            }
            Expression::BooleanBinary { lhs_index, op, rhs_index, .. } => {
                let lhs = &self.ast.expressions[*lhs_index as usize];
                let rhs = &self.ast.expressions[*rhs_index as usize];

                let lhs_dst = match lhs.typ() {
                    Type::Base(BaseType::Bool) => Dst::Reg(Rdi),
                    Type::Base(BaseType::I64 | BaseType::Ascii | BaseType::Str)
                    | Type::Array { .. } => {
                        unreachable!("non-boolean expressions not allowed in conditions");
                    }
                };

                let rhs_dst = match rhs.typ() {
                    Type::Base(BaseType::Bool) => Dst::Reg(Rsi),
                    Type::Base(BaseType::I64 | BaseType::Ascii | BaseType::Str)
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
            Expression::Comparison { lhs_index, op, rhs_index, .. } => {
                let lhs = &self.ast.expressions[*lhs_index as usize];
                let rhs = &self.ast.expressions[*rhs_index as usize];

                let lhs_dst = match lhs.typ() {
                    Type::Base(BaseType::I64 | BaseType::Ascii | BaseType::Bool) => Dst::Reg(Rdi),
                    Type::Base(BaseType::Str) | Type::Array { .. } => {
                        Dst::View { len: Rdi, ptr: Rsi }
                    }
                };

                let rhs_dst = match rhs.typ() {
                    Type::Base(BaseType::I64 | BaseType::Ascii | BaseType::Bool) => Dst::Reg(Rsi),
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
            Expression::Variable { variable_index, .. } => {
                let ast_variable = &self.ast.variables[*variable_index as usize];
                let var = self.resolve(ast_variable.name);
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
}

// definitions
impl<'ast> Compiler<'ast, '_, '_, '_> {
    fn definition(&mut self, value: &'ast Expression, base: Base, dst_offset: usize) {
        match value {
            Expression::Parenthesis { expression_index, .. } => {
                let inner = &self.ast.expressions[*expression_index as usize];
                self.definition(inner, base, dst_offset);
            }
            Expression::I64(integer) => {
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
            Expression::Str { label } => {
                _ = writeln!(
                    self.asm,
                    " mov qword [{base} + {dst_offset}], str_{label}_len\
                    \n mov qword [{base} + {dst_offset} + {ptr_offset}], str_{label}\n",
                    ptr_offset = size_of::<u64>()
                );
            }
            Expression::Array { base_type, items } => {
                let typ_size = base_type.size();
                for (index, item) in items.iter().enumerate() {
                    self.definition(item, base, dst_offset + index * typ_size);
                }
            }
            Expression::Unary { op, op_col, operand_index } => match op {
                UnaryOp::Len => {
                    let operand = &self.ast.expressions[*operand_index as usize];
                    let mut unwrapped_operand = operand;
                    while let Expression::Parenthesis { expression_index, .. } = unwrapped_operand {
                        let inner = &self.ast.expressions[*expression_index as usize];
                        unwrapped_operand = inner;
                    }

                    match unwrapped_operand {
                        Expression::Parenthesis { .. } => {
                            unreachable!("should have been unwrapped")
                        }
                        Expression::Str { label } => {
                            _ = writeln!(
                                self.asm,
                                " mov qword [{base} + {dst_offset}], str_{label}_len\n"
                            );
                        }
                        Expression::Array { items, .. } => {
                            _ = writeln!(
                                self.asm,
                                " mov qword [{base} + {dst_offset}], {}\n",
                                items.len()
                            );
                        }
                        Expression::Temporary { .. } => {
                            unreachable!("temporaries cannot appear in variables")
                        }
                        Expression::Variable { typ, variable_index } => {
                            let ast_variable = &self.ast.variables[*variable_index as usize];
                            let var = self.resolve(ast_variable.name);
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
                                    debug_assert!(*len > 0, "arrays of 0 items are not allowed");
                                    _ = writeln!(
                                        self.asm,
                                        " mov qword [{base} + {dst_offset}], {len}\n"
                                    );
                                }
                                Type::Base(BaseType::I64 | BaseType::Ascii | BaseType::Bool) => {
                                    unreachable!("cannot take the length of numerical types")
                                }
                            }
                        }
                        Expression::ArrayIndex {
                            base_type,
                            indexable_index: base_array_indexable_index,
                            bracket_col,
                            index_expression_index,
                        } => {
                            let base_array_indexable =
                                &self.ast.expressions[*base_array_indexable_index as usize];
                            let index_expression =
                                &self.ast.expressions[*index_expression_index as usize];

                            self.index(
                                *base_type, base_array_indexable, *bracket_col, index_expression,
                            );
                            _ = writeln!(self.asm, "mov [{base} + {dst_offset}], rdi\n");
                        }
                        Expression::False
                        | Expression::True
                        | Expression::I64(_)
                        | Expression::Ascii(_)
                        | Expression::Unary { .. }
                        | Expression::BooleanUnary { .. }
                        | Expression::Binary { .. }
                        | Expression::BooleanBinary { .. }
                        | Expression::Comparison { .. } => {
                            unreachable!("cannot take the length of numerical types")
                        }
                    }
                }
                UnaryOp::Not => {
                    let operand = &self.ast.expressions[*operand_index as usize];
                    self.expression(operand, Dst::Reg(Rdi));
                    match operand.typ() {
                        Type::Base(BaseType::I64) => {
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
                    let operand = &self.ast.expressions[*operand_index as usize];
                    self.expression(operand, Dst::Reg(Rdi));
                    match operand.typ() {
                        Type::Base(BaseType::I64) => {
                            let Position { line, column } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {column}\
                                \n call i64_safe_abs\
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
                    let operand = &self.ast.expressions[*operand_index as usize];
                    self.expression(operand, Dst::Reg(Rdi));
                    match operand.typ() {
                        Type::Base(BaseType::I64) => {
                            _ = writeln!(
                                self.asm,
                                " call i64_wrapping_abs\
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
                    let operand = &self.ast.expressions[*operand_index as usize];
                    self.expression(operand, Dst::Reg(Rdi));
                    match operand.typ() {
                        Type::Base(BaseType::I64) => {
                            let Position { line, column } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {column}\
                                \n call i64_saturating_abs\
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
                    let operand = &self.ast.expressions[*operand_index as usize];
                    self.expression(operand, Dst::Reg(Rdi));
                    match operand.typ() {
                        Type::Base(BaseType::I64) => {
                            let Position { line, column } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {column}\
                                \n call i64_safe_negate\
                                \n mov [{base} + {dst_offset}], rdi\n",
                            );
                        }
                        Type::Base(BaseType::Ascii) => {
                            let Position { line, column } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {column}\
                                \n call i64_safe_negate\
                                \n mov [{base} + {dst_offset}], dil\n",
                            );
                        }
                        Type::Base(BaseType::Bool | BaseType::Str) | Type::Array { .. } => {
                            unreachable!("cannot negate non i64/ascii values");
                        }
                    }
                }
                UnaryOp::WrappingMinus => {
                    let operand = &self.ast.expressions[*operand_index as usize];
                    self.expression(operand, Dst::Reg(Rdi));
                    match operand.typ() {
                        Type::Base(BaseType::I64) => {
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
                            unreachable!("cannot negate non i64/ascii values");
                        }
                    }
                }
                UnaryOp::SaturatingMinus => {
                    let operand = &self.ast.expressions[*operand_index as usize];
                    self.expression(operand, Dst::Reg(Rdi));
                    match operand.typ() {
                        Type::Base(BaseType::I64) => {
                            let Position { line, column } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {column}\
                                \n call i64_saturating_negate\
                                \n mov [{base} + {dst_offset}], rdi\n",
                            );
                        }
                        Type::Base(BaseType::Ascii) => {
                            let Position { line, column } = self.src.position(*op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {column}\
                                \n call i64_saturating_negate\
                                \n mov [{base} + {dst_offset}], dil\n",
                            );
                        }
                        Type::Base(BaseType::Bool | BaseType::Str) | Type::Array { .. } => {
                            unreachable!("cannot negate non i64/ascii values");
                        }
                    }
                }
            },
            Expression::BooleanUnary { operand_index, .. } => {
                let operand = &self.ast.expressions[*operand_index as usize];
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
                    Type::Base(BaseType::I64) => {
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
                    Type::Base(BaseType::I64 | BaseType::Ascii | BaseType::Str)
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
            Expression::Variable { typ: identifier_typ, variable_index } => {
                let ast_variable = &self.ast.variables[*variable_index as usize];
                let var = self.resolve(ast_variable.name);
                let src_offset = var.offset;
                match identifier_typ {
                    Type::Base(BaseType::I64) => {
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
                            ptr_offset = size_of::<u64>()
                        );
                    }
                    Type::Array { base_type: array_typ, len } => {
                        debug_assert!(*len > 0, "arrays of 0 items are not allowed");
                        match *array_typ {
                            BaseType::I64 => {
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
                        }
                    }
                }
            }
            Expression::ArrayIndex { base_type, .. } => {
                self.expression(value, Dst::default(&Type::Base(*base_type)));

                match base_type {
                    BaseType::I64 => _ = writeln!(self.asm, " mov [{base} + {dst_offset}], rdi\n"),
                    BaseType::Ascii | BaseType::Bool => {
                        _ = writeln!(self.asm, " mov [{base} + {dst_offset}], dil\n");
                    }
                    BaseType::Str => {
                        _ = writeln!(
                            self.asm,
                            " mov [{base} + {dst_offset}], rdi\
                            \n mov [{base} + {dst_offset} + {ptr_offset}], rsi\n",
                            ptr_offset = size_of::<u64>()
                        );
                    }
                }
            }
        }
    }

    fn reassignment(
        &mut self,
        target: &'ast Expression,
        op: AssignmentOp,
        op_col: offset32,
        new_value: &'ast Expression,
    ) {
        match target {
            Expression::ArrayIndex {
                base_type,
                indexable_index,
                bracket_col,
                index_expression_index,
            } => {
                let indexable = &self.ast.expressions[*indexable_index as usize];
                let index_expression = &self.ast.expressions[*index_expression_index as usize];

                match indexable {
                    Expression::Parenthesis { .. } => {
                        unreachable!("should have been disallowed during parsing")
                    }
                    Expression::ArrayIndex {
                        base_type: nested_base_type,
                        indexable_index: nested_indexable_index,
                        bracket_col: nested_bracket_col,
                        index_expression_index: nested_index_expression_index,
                    } => {
                        // Note: can only have nested indexes into str[], i.e.: ["a", "b"][0][0] = 'c'
                        _ = writeln!(
                            self.asm,
                            " ; {} {op} {}",
                            target.display(self.ast),
                            new_value.display(self.ast)
                        );
                        let Position { line, column } = self.src.position(*bracket_col);

                        let nested_indexable =
                            &self.ast.expressions[*nested_indexable_index as usize];
                        let nested_index_expression =
                            &self.ast.expressions[*nested_index_expression_index as usize];

                        self.index(
                            *nested_base_type,
                            nested_indexable,
                            *nested_bracket_col,
                            nested_index_expression,
                        );
                        match nested_base_type {
                            BaseType::Str => {
                                _ = writeln!(
                                    self.asm,
                                    " push rsi\
                                    \n push rdi\n"
                                );
                            }
                            BaseType::I64 | BaseType::Ascii | BaseType::Bool => {
                                unreachable!("only strings are allowed in nested index expressions")
                            }
                        }

                        self.expression(index_expression, Dst::Reg(Rdi));
                        match base_type {
                            BaseType::Ascii => {
                                _ = writeln!(
                                    self.asm,
                                    " pop rsi\
                                    \n mov rdx, {line}\
                                    \n mov rcx, {column}\
                                    \n call assert_str_index_in_range\
                                    \n push rdi\n",
                                );
                            }
                            BaseType::I64 | BaseType::Str | BaseType::Bool => {
                                unreachable!("only ascii are allowed in nested index expressions")
                            }
                        }

                        self.expression(new_value, Dst::Reg(Rdi));
                        _ = writeln!(
                            self.asm,
                            " pop rdx\
                            \n pop rsi\
                            \n mov [rsi + rdx], dil\n"
                        );
                    }
                    Expression::Variable { typ, variable_index } => {
                        _ = writeln!(
                            self.asm,
                            " ; {} {op} {}",
                            target.display(self.ast),
                            new_value.display(self.ast)
                        );

                        let ast_variable = &self.ast.variables[*variable_index as usize];
                        let var = self.resolve(ast_variable.name);
                        let var_offset = var.offset;
                        let Position { line: index_line, column: index_col } =
                            self.src.position(*bracket_col);

                        match typ {
                            Type::Base(BaseType::Str) => {
                                self.expression(index_expression, Dst::Reg(Rdi));
                                _ = writeln!(
                                    self.asm,
                                    " mov rsi, [rbp + {var_offset}]\
                                    \n mov rdx, {index_line}\
                                    \n mov rcx, {index_col}\
                                    \n call assert_str_index_in_range\
                                    \n push rdi\n"
                                );

                                // Note: can only assign ascii values to indexs into strings
                                self.expression(new_value, Dst::Reg(Rdi));
                                _ = writeln!(
                                    self.asm,
                                    "\n mov rsi, [rbp + {var_offset} + {ptr_offset}]\
                                    \n pop rdx\
                                    \n mov [rsi + rdx], dil\n",
                                    ptr_offset = size_of::<u64>()
                                );
                            }
                            Type::Array { len: array_len, .. } => {
                                debug_assert!(*array_len > 0, "arrays of 0 items are not allowed");
                                self.expression(index_expression, Dst::Reg(Rdi));
                                _ = writeln!(
                                    self.asm,
                                    " mov rsi, {array_len}\
                                    \n mov rdx, {index_line}\
                                    \n mov rcx, {index_col}\
                                    \n call assert_array_index_in_range\
                                    \n push rdi\n"
                                );

                                if let AssignmentOp::Equals = op {
                                    match base_type {
                                        BaseType::I64 => {
                                            self.expression(new_value, Dst::Reg(Rdi));
                                            _ = writeln!(
                                                self.asm,
                                                "\n pop rdx\
                                                \n mov [rbp + {var_offset} + rdx * 8], rdi\n"
                                            );
                                        }
                                        BaseType::Ascii | BaseType::Bool => {
                                            self.expression(new_value, Dst::Reg(Rdi));
                                            _ = writeln!(
                                                self.asm,
                                                "\n pop rdx\
                                                \n mov [rbp + {var_offset} + rdx], dil\n"
                                            );
                                        }
                                        BaseType::Str => {
                                            self.expression(
                                                new_value,
                                                Dst::View { len: Rdi, ptr: Rsi },
                                            );
                                            _ = writeln!(
                                                self.asm,
                                                "\n pop rdx\
                                                \n imul rdx, {base_type_size}\
                                                \n mov [rbp + {var_offset} + rdx], rdi\
                                                \n mov [rbp + {var_offset} + rdx + {ptr_offset}], rsi\n",
                                                base_type_size = base_type.size(),
                                                ptr_offset = size_of::<u64>()
                                            );
                                        }
                                    }
                                } else {
                                    self.expression(new_value, Dst::Reg(Rdi));
                                    // Note: *op*= operators can only assign to i64 variables
                                    _ = writeln!(
                                        self.asm,
                                        " mov rsi, rdi\
                                        \n mov rdx, [rsp]\
                                        \n mov rdi, [rbp + {var_offset} + rdx * 8]\n"
                                    );

                                    match op {
                                        AssignmentOp::Pow => {
                                            let Position { line, column } =
                                                self.src.position(op_col);
                                            _ = writeln!(
                                                self.asm,
                                                " mov rdx, {line}\
                                                \n mov rcx, {column}\
                                                \n call i64_safe_pow",
                                            );
                                        }
                                        AssignmentOp::WrappingPow => {
                                            let Position { line, column } =
                                                self.src.position(op_col);
                                            _ = writeln!(
                                                self.asm,
                                                " mov rdx, {line}\
                                                \n mov rcx, {column}\
                                                \n call i64_wrapping_pow",
                                            );
                                        }
                                        AssignmentOp::SaturatingPow => {
                                            let Position { line, column } =
                                                self.src.position(op_col);
                                            _ = writeln!(
                                                self.asm,
                                                " mov rdx, {line}\
                                                \n mov rcx, {column}\
                                                \n call i64_saturating_pow",
                                            );
                                        }
                                        AssignmentOp::Times => {
                                            let Position { line, column } =
                                                self.src.position(op_col);
                                            _ = writeln!(
                                                self.asm,
                                                " mov rdx, {line}\
                                                \n mov rcx, {column}\
                                                \n call i64_safe_mul",
                                            );
                                        }
                                        AssignmentOp::WrappingTimes => {
                                            _ = writeln!(self.asm, " imul rdi, rsi");
                                        }
                                        AssignmentOp::SaturatingTimes => {
                                            _ = writeln!(self.asm, " call i64_saturating_mul");
                                        }
                                        AssignmentOp::Divide => {
                                            let Position { line, column } =
                                                self.src.position(op_col);
                                            _ = writeln!(
                                                self.asm,
                                                " mov rdx, {line}\
                                                \n mov rcx, {column}\
                                                \n call i64_safe_div",
                                            );
                                        }
                                        AssignmentOp::WrappingDivide => {
                                            let Position { line, column } =
                                                self.src.position(op_col);
                                            _ = writeln!(
                                                self.asm,
                                                " mov rdx, {line}\
                                                \n mov rcx, {column}\
                                                \n call i64_wrapping_div",
                                            );
                                        }
                                        AssignmentOp::SaturatingDivide => {
                                            let Position { line, column } =
                                                self.src.position(op_col);
                                            _ = writeln!(
                                                self.asm,
                                                " mov rdx, {line}\
                                                \n mov rcx, {column}\
                                                \n call i64_saturating_div",
                                            );
                                        }
                                        AssignmentOp::Remainder => {
                                            let Position { line, column } =
                                                self.src.position(op_col);
                                            _ = writeln!(
                                                self.asm,
                                                " mov rdx, {line}\
                                                \n mov rcx, {column}\
                                                \n call i64_safe_remainder",
                                            );
                                        }
                                        AssignmentOp::Plus => {
                                            let Position { line, column } =
                                                self.src.position(op_col);
                                            _ = writeln!(
                                                self.asm,
                                                " mov rdx, {line}\
                                                \n mov rcx, {column}\
                                                \n call i64_safe_add",
                                            );
                                        }
                                        AssignmentOp::WrappingPlus => {
                                            _ = writeln!(self.asm, " add rdi, rsi");
                                        }
                                        AssignmentOp::SaturatingPlus => {
                                            _ = writeln!(self.asm, " call i64_saturating_add");
                                        }
                                        AssignmentOp::Minus => {
                                            let Position { line, column } =
                                                self.src.position(op_col);
                                            _ = writeln!(
                                                self.asm,
                                                " mov rdx, {line}\
                                                \n mov rcx, {column}\
                                                \n call i64_safe_sub",
                                            );
                                        }
                                        AssignmentOp::WrappingMinus => {
                                            _ = writeln!(self.asm, " sub rdi, rsi");
                                        }
                                        AssignmentOp::SaturatingMinus => {
                                            _ = writeln!(self.asm, " call i64_saturating_sub");
                                        }
                                        AssignmentOp::And | AssignmentOp::BitAnd => {
                                            _ = writeln!(self.asm, " and rdi, rsi");
                                        }
                                        AssignmentOp::Or | AssignmentOp::BitOr => {
                                            _ = writeln!(self.asm, " or rdi, rsi");
                                        }
                                        AssignmentOp::BitXor => {
                                            _ = writeln!(self.asm, " xor rdi, rsi");
                                        }
                                        AssignmentOp::LeftShift => {
                                            let Position { line, column } =
                                                self.src.position(op_col);
                                            _ = writeln!(
                                                self.asm,
                                                " mov rdx, {line}\
                                                \n mov rcx, {column}\
                                                \n call i64_safe_left_shift",
                                            );
                                        }
                                        AssignmentOp::WrappingLeftShift => {
                                            let Position { line, column } =
                                                self.src.position(op_col);
                                            _ = writeln!(
                                                self.asm,
                                                " mov rdx, {line}\
                                                \n mov rcx, {column}\
                                                \n call i64_wrapping_left_shift",
                                            );
                                        }
                                        AssignmentOp::SaturatingLeftShift => {
                                            let Position { line, column } =
                                                self.src.position(op_col);
                                            _ = writeln!(
                                                self.asm,
                                                " mov rdx, {line}\
                                                \n mov rcx, {column}\
                                                \n call i64_saturating_left_shift",
                                            );
                                        }
                                        AssignmentOp::RightShift => {
                                            let Position { line, column } =
                                                self.src.position(op_col);
                                            _ = writeln!(
                                                self.asm,
                                                " mov rdx, {line}\
                                                \n mov rcx, {column}\
                                                \n call i64_safe_right_shift",
                                            );
                                        }
                                        AssignmentOp::LeftRotate => {
                                            let Position { line, column } =
                                                self.src.position(op_col);
                                            _ = writeln!(
                                                self.asm,
                                                " mov rdx, {line}\
                                                \n mov rcx, {column}\
                                                \n call i64_safe_left_rotate",
                                            );
                                        }
                                        AssignmentOp::RightRotate => {
                                            let Position { line, column } =
                                                self.src.position(op_col);
                                            _ = writeln!(
                                                self.asm,
                                                " mov rdx, {line}\
                                                \n mov rcx, {column}\
                                                \n call i64_safe_right_rotate",
                                            );
                                        }
                                        AssignmentOp::Equals => {
                                            unreachable!("handled in the previous branch")
                                        }
                                    }

                                    _ = writeln!(
                                        self.asm,
                                        "\n pop rdx\
                                        \n mov [rbp + {var_offset} + rdx * 8], rdi\n"
                                    );
                                }
                            }
                            Type::Base(BaseType::I64 | BaseType::Ascii | BaseType::Bool) => {
                                unreachable!(
                                    "only arrays and strings are allowed in index expressions"
                                )
                            }
                        }
                    }

                    Expression::Binary { .. }
                    | Expression::Str { .. }
                    | Expression::Unary { .. }
                    | Expression::Temporary { .. }
                    | Expression::I64(_)
                    | Expression::False
                    | Expression::Array { .. }
                    | Expression::True
                    | Expression::Ascii(_)
                    | Expression::BooleanUnary { .. }
                    | Expression::BooleanBinary { .. }
                    | Expression::Comparison { .. } => {
                        unreachable!("only arrays and strings are allowed in index expressions")
                    }
                }
            }
            Expression::Variable { variable_index, .. } => {
                let ast_variable = &self.ast.variables[*variable_index as usize];
                let var = self.resolve(ast_variable.name);
                let dst_offset = var.offset;

                let ast_variable_name_str =
                    unsafe { core::str::from_utf8_unchecked(ast_variable.name) };
                _ = writeln!(
                    self.asm,
                    " ; {ast_variable_name_str} {op} {}",
                    new_value.display(self.ast)
                );
                if let AssignmentOp::Equals = op {
                    self.definition(new_value, Base::Rbp, dst_offset);
                } else {
                    // Note: *op*= operators can only assign to i64 variables
                    self.expression(new_value, Dst::Reg(Rdi));

                    _ = writeln!(
                        self.asm,
                        " mov rsi, rdi\
                        \n mov rdi, [rbp + {dst_offset}]"
                    );

                    match op {
                        AssignmentOp::Pow => {
                            let Position { line, column } = self.src.position(op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {column}\
                                \n call i64_safe_pow",
                            );
                        }
                        AssignmentOp::WrappingPow => {
                            let Position { line, column } = self.src.position(op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {column}\
                                \n call i64_wrapping_pow",
                            );
                        }
                        AssignmentOp::SaturatingPow => {
                            let Position { line, column } = self.src.position(op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {column}\
                                \n call i64_saturating_pow",
                            );
                        }
                        AssignmentOp::Times => {
                            let Position { line, column } = self.src.position(op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {column}\
                                \n call i64_safe_mul",
                            );
                        }
                        AssignmentOp::WrappingTimes => _ = writeln!(self.asm, " imul rdi, rsi"),
                        AssignmentOp::SaturatingTimes => {
                            _ = writeln!(self.asm, " call i64_saturating_mul");
                        }
                        AssignmentOp::Divide => {
                            let Position { line, column } = self.src.position(op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {column}\
                                \n call i64_safe_div",
                            );
                        }
                        AssignmentOp::WrappingDivide => {
                            let Position { line, column } = self.src.position(op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {column}\
                                \n call i64_wrapping_div",
                            );
                        }
                        AssignmentOp::SaturatingDivide => {
                            let Position { line, column } = self.src.position(op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {column}\
                                \n call i64_saturating_div",
                            );
                        }
                        AssignmentOp::Remainder => {
                            let Position { line, column } = self.src.position(op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {column}\
                                \n call i64_safe_remainder",
                            );
                        }
                        AssignmentOp::Plus => {
                            let Position { line, column } = self.src.position(op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {column}\
                                \n call i64_safe_add",
                            );
                        }
                        AssignmentOp::WrappingPlus => _ = writeln!(self.asm, " add rdi, rsi"),
                        AssignmentOp::SaturatingPlus => {
                            _ = writeln!(self.asm, " call i64_saturating_add");
                        }
                        AssignmentOp::Minus => {
                            let Position { line, column } = self.src.position(op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {column}\
                                \n call i64_safe_sub",
                            );
                        }
                        AssignmentOp::WrappingMinus => _ = writeln!(self.asm, " sub rdi, rsi"),
                        AssignmentOp::SaturatingMinus => {
                            _ = writeln!(self.asm, " call i64_saturating_sub");
                        }
                        AssignmentOp::And | AssignmentOp::BitAnd => {
                            _ = writeln!(self.asm, " and rdi, rsi");
                        }
                        AssignmentOp::Or | AssignmentOp::BitOr => {
                            _ = writeln!(self.asm, " or rdi, rsi");
                        }
                        AssignmentOp::BitXor => _ = writeln!(self.asm, " xor rdi, rsi"),
                        AssignmentOp::LeftShift => {
                            let Position { line, column } = self.src.position(op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {column}\
                                \n call i64_safe_left_shift",
                            );
                        }
                        AssignmentOp::WrappingLeftShift => {
                            let Position { line, column } = self.src.position(op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {column}\
                                \n call i64_wrapping_left_shift",
                            );
                        }
                        AssignmentOp::SaturatingLeftShift => {
                            let Position { line, column } = self.src.position(op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {column}\
                                \n call i64_saturating_left_shift",
                            );
                        }
                        AssignmentOp::RightShift => {
                            let Position { line, column } = self.src.position(op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {column}\
                                \n call i64_safe_right_shift",
                            );
                        }
                        AssignmentOp::LeftRotate => {
                            let Position { line, column } = self.src.position(op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {column}\
                                \n call i64_safe_left_rotate",
                            );
                        }
                        AssignmentOp::RightRotate => {
                            let Position { line, column } = self.src.position(op_col);
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {column}\
                                \n call i64_safe_right_rotate",
                            );
                        }
                        AssignmentOp::Equals => unreachable!("handled in the previous branch"),
                    }

                    _ = writeln!(self.asm, "\n mov [rbp + {dst_offset}], rdi\n");
                }
            }
            Expression::False
            | Expression::True
            | Expression::I64(_)
            | Expression::Ascii(_)
            | Expression::Str { .. }
            | Expression::Array { .. }
            | Expression::Parenthesis { .. }
            | Expression::Unary { .. }
            | Expression::BooleanUnary { .. }
            | Expression::Binary { .. }
            | Expression::BooleanBinary { .. }
            | Expression::Comparison { .. }
            | Expression::Temporary { .. } => {
                unreachable!("cannot assign to expression")
            }
        }
    }
}

// ifs
impl<'ast> Compiler<'ast, '_, '_, '_> {
    fn iff(&mut self, iff: &'ast IfStatement, tag: &str, false_tag: &str) {
        _ = writeln!(self.asm, "{tag}:; {}", iff.condition.display(self.ast));
        self.condition(&iff.condition, false_tag);
        self.node(&iff.statement);
    }
}

// print statements
impl<'ast> Compiler<'ast, '_, '_, '_> {
    fn print(&mut self, value: &'ast Expression) {
        let value_type = value.typ();
        self.expression(value, Dst::default(&value_type));

        match value_type {
            Type::Base(BaseType::I64) => _ = writeln!(self.asm, " call i64_print\n"),
            Type::Base(BaseType::Ascii) => _ = writeln!(self.asm, " call ascii_print\n"),
            Type::Base(BaseType::Bool) => _ = writeln!(self.asm, " call bool_print\n"),
            Type::Base(BaseType::Str) => _ = writeln!(self.asm, " call str_print\n"),
            Type::Array { base_type, .. } => match base_type {
                BaseType::I64 => _ = writeln!(self.asm, " call i64_array_debug_print\n"),
                BaseType::Ascii => _ = writeln!(self.asm, " call ascii_array_debug_print\n"),
                BaseType::Bool => _ = writeln!(self.asm, " call bool_array_debug_print\n"),
                BaseType::Str => _ = writeln!(self.asm, " call str_array_debug_print\n"),
            },
        }
    }

    fn eprint(&mut self, value: &'ast Expression) {
        let value_type = value.typ();
        self.expression(value, Dst::default(&value_type));

        match value_type {
            Type::Base(BaseType::I64) => _ = writeln!(self.asm, " call i64_eprint\n"),
            Type::Base(BaseType::Ascii) => _ = writeln!(self.asm, " call ascii_eprint\n"),
            Type::Base(BaseType::Bool) => _ = writeln!(self.asm, " call bool_eprint\n"),
            Type::Base(BaseType::Str) => _ = writeln!(self.asm, " call str_eprint\n"),
            Type::Array { base_type: typ, .. } => match typ {
                BaseType::I64 => _ = writeln!(self.asm, " call i64_array_debug_eprint\n"),
                BaseType::Ascii => _ = writeln!(self.asm, " call ascii_array_debug_eprint\n"),
                BaseType::Bool => _ = writeln!(self.asm, " call bool_array_debug_eprint\n"),
                BaseType::Str => _ = writeln!(self.asm, " call str_array_debug_eprint\n"),
            },
        }
    }
}
