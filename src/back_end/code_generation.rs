use back_to_front::back_end::x86_64::reg::Reg64;

use crate::{front_end::src_file::SrcCode, ir::ir::{Expression, ExpressionIndex, Ir, Node, Operand, OperandIndex, VariableDefinitionIndex}};

use core::fmt::Write as _;

#[derive(Debug)]
struct StackVariable<'ir, 'code: 'ir> {
    inner: &'ir VariableDefinitionIndex<'code>,
    offset: usize,
}

#[derive(Debug)]
struct RegisterVariable<'ir, 'code: 'ir> {
    inner: &'ir VariableDefinitionIndex<'code>,
    reg: Reg64,
}

// struct Allocator {
//     registers: u16,
// }

// impl Allocator {
//     fn next_i64(&mut self) -> Reg64 {
//         return Reg64::Rdi;
//     }
//     fn next_register(&mut self) -> Option<Reg64> {
//         unimplemented!("need to take into account registers abi");
//         for register_index in 0..Reg64::R15 as u8 {
//             let register_mask = 1 << register_index;
//             if self.registers & register_mask == 0 {
//                 self.registers |= register_mask;
//                 let register: Reg64 = unsafe { core::mem::transmute(register_index) };
//                 return Some(register);
//             }
//         }
//         return None;
//     }

//     fn free_register(&mut self, reg: Reg64) {
//         unimplemented!("need to take into account registers abi");
//         let register_index = reg as u8;
//         let register_mask = !(1 << register_index);
//         self.registers &= register_mask;
//     }
// }

#[derive(Debug)]
pub struct Compiler<'ir, 'tast: 'ir, 'st: 'tast, 'src: 'tast, 'tokens: 'src, 'code: 'src, 'path: 'code> {
    src: &'src SrcCode<'code, 'path>,
    ir: &'ir Ir<'tast, 'st, 'tokens, 'code>,

    asm: String,

    register_variables: Vec<RegisterVariable<'ir, 'code>>,

    // if_counter: u32,

    // loop_counter: u32,
    // loop_counters: Vec<u32>,
    // and_counter: usize,
    // or_counter: usize,
}

// Generation of compilation artifacts (.asm, .o, executable)
impl<'ir, 'tast: 'ir, 'st: 'tast, 'src: 'tast, 'tokens: 'src, 'code: 'src, 'path: 'code>
    Compiler<'ir, 'tast, 'st, 'src, 'tokens, 'code, 'path> {
    #[must_use]
    pub fn compile(src: &'src SrcCode<'code, 'path>, ir: &'ir Ir<'tast, 'st, 'tokens, 'code>) -> String {
        use crate::back_end::asm::{
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
            ir,
            asm: String::new(),
            register_variables: Vec::new(),
            // if_counter: 0,
            // loop_counter: 0,
            // loop_counters: Vec::new(),
            // and_counter: 0,
            // or_counter: 0,
        };

        let mut prologue = String::new();
        let mut epilogue = String::new();

        let strings = String::new();

        let mut node_index = 0;
        if node_index < this.ir.nodes.len() {
            // for var in &this.ir.variables {
            //     this.register_variables.push(RegisterVariable { inner: var, reg: Reg64::Rdi });
            // }

            // this.variables.sort_by(|var_1, var_2| {
            //     return var_2.inner.value.typ().size().cmp(&var_1.inner.value.typ().size());
            // });

            // let mut stack_size = 0;
            // for var in &mut this.variables {
            //     var.offset = stack_size;
            //     stack_size += var.inner.value.typ().size();
            // }

            // if stack_size > 0 {
            //     const STACK_ALIGN: usize = size_of::<usize>();

            //     let misalignment = stack_size % STACK_ALIGN;
            //     let needs_padding = misalignment != 0;
            //     let padding = usize::from(needs_padding) * (STACK_ALIGN - misalignment);
            //     stack_size += padding;

            //     _ = writeln!(
            //         prologue,
            //         " push rbp\
            //         \n sub rsp, {stack_size}\
            //         \n mov rbp, rsp\n"
            //     );
            // }

            while node_index < this.ir.nodes.len() - 1 {
                let node = &this.ir.nodes[node_index];
                node_index += 1;

                this.node(node);
                _ = writeln!(this.asm);
            }
            let last_node = &this.ir.nodes[node_index];
            this.node(last_node);

            // if stack_size > 0 {
            //     _ = writeln!(
            //         epilogue,
            //         " add rsp, {stack_size}\
            //         \n pop rbp\n"
            //     );
            // }
        }

        let program = format!(
            r#"global _start

section .text
_start:
{prologue}
{asm}
{epilogue}
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

 section .data
 {strings}"#,
            asm = this.asm,
            src_path = src.path().display(),
        );

        return program;
    }
}

impl<'ir, 'code: 'ir> Compiler<'ir, '_, '_, '_, '_, 'code, '_> {
    fn node(&mut self, node: &'ir Node<'code>) {
        match node {
            Node::LetVariable { variable } => self.let_variable(*variable),
            Node::VarVariable { variable } => unimplemented!(),
            Node::Print { argument } => {
                self.expression(*argument);
                self.print(*argument);
            },
            // Node::Eprint { argument } => unimplemented!(),
        }
    }

    fn let_variable(&mut self, variable_index: VariableDefinitionIndex<'code>) {
        let variable = &self.ir.variables[variable_index];
        self.expression(variable.value);
    }

    fn operand(&mut self, operand_index: OperandIndex<'code>) {
        let operand = &self.ir.operands[operand_index];
        match operand {
            Operand::False => {
                _ = writeln!(self.asm, " mov rdi, false");
            },
            Operand::True => {
                _ = writeln!(self.asm, " mov rdi, true");
            },
            Operand::I64 { value } => {
                _ = writeln!(self.asm, " mov rdi, {value}");
            },
            Operand::Ascii { character } => {
                _ = writeln!(self.asm, " mov rdi, {character}");
            },
            Operand::Str { literal } => {
                unimplemented!();
            },
            Operand::LetVariable { variable } => self.let_variable(*variable),
            Operand::VarVariable { variable } => unimplemented!(),
        }
    }

    fn expression(&mut self, expression_index: ExpressionIndex<'code>) {
        let expression = &self.ir.expressions[expression_index];
        match expression {
            Expression::Operand { operand } => self.operand(*operand),
            // Expression::Prefix { operator, operator_column, right_operand } => todo!(),
            // Expression::BooleanPrefix { operator, operator_column, right_operand } => todo!(),
            // Expression::Binary { left_operand, operator, operator_column, right_operand } => todo!(),
            // Expression::BooleanBinary { left_operand, operator, operator_column, right_operand } => todo!(),
            // Expression::Comparison { left_operand, operator, operator_column, right_operand } => todo!(),
            // Expression::BooleanComparison { left_operand, operator, operator_column, right_operand } => todo!(),
        }
    }

    fn print(&mut self, argument_index: ExpressionIndex<'code>) {
        let argument = &self.ir.expressions[argument_index];
        match argument {
            Expression::Operand { operand: operand_index } => {
                let operand = &self.ir.operands[*operand_index];
                match operand {
                    Operand::False
                    | Operand::True => _ = writeln!(self.asm, " call bool_print"),
                    Operand::I64 { .. } => _ = writeln!(self.asm, " call i64_print"),
                    Operand::Ascii { .. } => _ = writeln!(self.asm, " call ascii_print"),
                    Operand::Str { .. } => _ = unimplemented!(),
                    Operand::LetVariable { variable: variable_index } => {
                        let variable = &self.ir.variables[*variable_index];
                        self.print(variable.value);
                    },
                    Operand::VarVariable { .. } => unimplemented!(),
                }
            }
        }
    }
}
