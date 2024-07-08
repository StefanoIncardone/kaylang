// TODO(stefano): allow assembly print functions to accept the sink (stdout or stderr)
// IDEA(stefano): reserve space for the biggest temporary value and reuse as necessary
// IDEA(stefano): have built-in functions return their result in rdi instead of rax

use crate::{
    artifacts::Artifacts,
    src_file::{Position, SrcFile},
    syntax::{
        ast::{self, Expression, IfStatement, LoopKind, Node, Scope, Type, TypeOf},
        tokenizer::{ascii, Literal, Op},
    },
    CAUSE, ERROR,
};
use std::{
    borrow::Cow,
    fmt::{Display, Write as _},
    fs::File,
    io::{self, BufWriter, Write},
    path::PathBuf,
};

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Reg64 {
    Rax = 0,
    Rbx = 1,
    Rcx = 2,
    Rdx = 3,

    Rsi = 4,
    Rdi = 5,
    Rbp = 6,
    Rsp = 7,

    R8 = 8,
    R9 = 9,
    R10 = 10,
    R11 = 11,
    R12 = 12,
    R13 = 13,
    R14 = 14,
    R15 = 15,
}

#[rustfmt::skip]
impl Display for Reg64 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Rax => write!(f, "rax"),
            Self::Rbx => write!(f, "rbx"),
            Self::Rcx => write!(f, "rcx"),
            Self::Rdx => write!(f, "rdx"),

            Self::Rsi => write!(f, "rsi"),
            Self::Rdi => write!(f, "rdi"),
            Self::Rbp => write!(f, "rbp"),
            Self::Rsp => write!(f, "rsp"),

            Self::R8  => write!(f, "r8"),
            Self::R9  => write!(f, "r9"),
            Self::R10 => write!(f, "r10"),
            Self::R11 => write!(f, "r11"),
            Self::R12 => write!(f, "r12"),
            Self::R13 => write!(f, "r13"),
            Self::R14 => write!(f, "r14"),
            Self::R15 => write!(f, "r15"),
        };
    }
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Reg32 {
    Eax = 0,
    Ebx = 1,
    Ecx = 2,
    Edx = 3,

    Esi = 4,
    Edi = 5,
    Ebp = 6,
    Esp = 7,

    R8d = 8,
    R9d = 9,
    R10d = 10,
    R11d = 11,
    R12d = 12,
    R13d = 13,
    R14d = 14,
    R15d = 15,
}

#[rustfmt::skip]
impl Display for Reg32 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Eax  => write!(f, "eax"),
            Self::Ebx  => write!(f, "ebx"),
            Self::Ecx  => write!(f, "ecx"),
            Self::Edx  => write!(f, "edx"),

            Self::Esi  => write!(f, "esi"),
            Self::Edi  => write!(f, "edi"),
            Self::Ebp  => write!(f, "ebp"),
            Self::Esp  => write!(f, "esp"),

            Self::R8d  => write!(f, "r8d"),
            Self::R9d  => write!(f, "r9d"),
            Self::R10d => write!(f, "r10d"),
            Self::R11d => write!(f, "r11d"),
            Self::R12d => write!(f, "r12d"),
            Self::R13d => write!(f, "r13d"),
            Self::R14d => write!(f, "r14d"),
            Self::R15d => write!(f, "r15d"),
        };
    }
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Reg16 {
    Ax = 0,
    Bx = 1,
    Cx = 2,
    Dx = 3,

    Si = 4,
    Di = 5,
    Bp = 6,
    Sp = 7,

    R8w = 8,
    R9w = 9,
    R10w = 10,
    R11w = 11,
    R12w = 12,
    R13w = 13,
    R14w = 14,
    R15w = 15,
}

#[rustfmt::skip]
impl Display for Reg16 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Ax   => write!(f, "ax"),
            Self::Bx   => write!(f, "bx"),
            Self::Cx   => write!(f, "cx"),
            Self::Dx   => write!(f, "dx"),

            Self::Si   => write!(f, "si"),
            Self::Di   => write!(f, "di"),
            Self::Bp   => write!(f, "bp"),
            Self::Sp   => write!(f, "sp"),

            Self::R8w  => write!(f, "r8w"),
            Self::R9w  => write!(f, "r9w"),
            Self::R10w => write!(f, "r10w"),
            Self::R11w => write!(f, "r11w"),
            Self::R12w => write!(f, "r12w"),
            Self::R13w => write!(f, "r13w"),
            Self::R14w => write!(f, "r14w"),
            Self::R15w => write!(f, "r15w"),
        };
    }
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Reg8l {
    Al = 0,
    Bl = 1,
    Cl = 2,
    Dl = 3,

    Sil = 4,
    Dil = 5,
    Bpl = 6,
    Spl = 7,

    R8b = 8,
    R9b = 9,
    R10b = 10,
    R11b = 11,
    R12b = 12,
    R13b = 13,
    R14b = 14,
    R15b = 15,
}

#[rustfmt::skip]
impl Display for Reg8l {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Al   => write!(f, "al"),
            Self::Bl   => write!(f, "bl"),
            Self::Cl   => write!(f, "cl"),
            Self::Dl   => write!(f, "dl"),

            Self::Sil  => write!(f, "sil"),
            Self::Dil  => write!(f, "dil"),
            Self::Bpl  => write!(f, "bpl"),
            Self::Spl  => write!(f, "spl"),

            Self::R8b  => write!(f, "r8b"),
            Self::R9b  => write!(f, "r9b"),
            Self::R10b => write!(f, "r10b"),
            Self::R11b => write!(f, "r11b"),
            Self::R12b => write!(f, "r12b"),
            Self::R13b => write!(f, "r13b"),
            Self::R14b => write!(f, "r14b"),
            Self::R15b => write!(f, "r15b"),
        };
    }
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Reg8h {
    Ah = 0,
    Bh = 1,
    Ch = 2,
    Dh = 3,
}

#[rustfmt::skip]
impl Display for Reg8h {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Ah => write!(f, "ah"),
            Self::Bh => write!(f, "bh"),
            Self::Ch => write!(f, "ch"),
            Self::Dh => write!(f, "dh"),
        };
    }
}

impl Into<Reg32> for Reg64 {
    #[inline(always)]
    fn into(self) -> Reg32 {
        return unsafe { std::mem::transmute(self) };
    }
}

impl Into<Reg16> for Reg64 {
    #[inline(always)]
    fn into(self) -> Reg16 {
        return unsafe { std::mem::transmute(self) };
    }
}

impl Into<Reg8l> for Reg64 {
    #[inline(always)]
    fn into(self) -> Reg8l {
        return unsafe { std::mem::transmute(self) };
    }
}

impl Into<Option<Reg8h>> for Reg64 {
    #[inline(always)]
    fn into(self) -> Option<Reg8h> {
        return match self {
            Self::Rax | Self::Rbx | Self::Rcx | Self::Rdx => unsafe {
                Some(std::mem::transmute(self))
            },
            Self::Rsi
            | Self::Rdi
            | Self::Rbp
            | Self::Rsp
            | Self::R8
            | Self::R9
            | Self::R10
            | Self::R11
            | Self::R12
            | Self::R13
            | Self::R14
            | Self::R15 => None,
        };
    }
}

impl Into<Reg64> for Reg32 {
    #[inline(always)]
    fn into(self) -> Reg64 {
        return unsafe { std::mem::transmute(self) };
    }
}

impl Into<Reg16> for Reg32 {
    #[inline(always)]
    fn into(self) -> Reg16 {
        return unsafe { std::mem::transmute(self) };
    }
}

impl Into<Reg8l> for Reg32 {
    #[inline(always)]
    fn into(self) -> Reg8l {
        return unsafe { std::mem::transmute(self) };
    }
}

impl Into<Option<Reg8h>> for Reg32 {
    #[inline(always)]
    fn into(self) -> Option<Reg8h> {
        return match self {
            Self::Eax | Self::Ebx | Self::Ecx | Self::Edx => unsafe {
                Some(std::mem::transmute(self))
            },
            Self::Esi
            | Self::Edi
            | Self::Ebp
            | Self::Esp
            | Self::R8d
            | Self::R9d
            | Self::R10d
            | Self::R11d
            | Self::R12d
            | Self::R13d
            | Self::R14d
            | Self::R15d => None,
        };
    }
}

impl Into<Reg64> for Reg16 {
    #[inline(always)]
    fn into(self) -> Reg64 {
        return unsafe { std::mem::transmute(self) };
    }
}

impl Into<Reg32> for Reg16 {
    #[inline(always)]
    fn into(self) -> Reg32 {
        return unsafe { std::mem::transmute(self) };
    }
}

impl Into<Reg8l> for Reg16 {
    #[inline(always)]
    fn into(self) -> Reg8l {
        return unsafe { std::mem::transmute(self) };
    }
}

impl Into<Option<Reg8h>> for Reg16 {
    #[inline(always)]
    fn into(self) -> Option<Reg8h> {
        return match self {
            Self::Ax | Self::Bx | Self::Cx | Self::Dx => unsafe { Some(std::mem::transmute(self)) },
            Self::Si
            | Self::Di
            | Self::Bp
            | Self::Sp
            | Self::R8w
            | Self::R9w
            | Self::R10w
            | Self::R11w
            | Self::R12w
            | Self::R13w
            | Self::R14w
            | Self::R15w => None,
        };
    }
}

impl Into<Reg64> for Reg8l {
    #[inline(always)]
    fn into(self) -> Reg64 {
        return unsafe { std::mem::transmute(self) };
    }
}

impl Into<Reg32> for Reg8l {
    #[inline(always)]
    fn into(self) -> Reg32 {
        return unsafe { std::mem::transmute(self) };
    }
}

impl Into<Reg16> for Reg8l {
    #[inline(always)]
    fn into(self) -> Reg16 {
        return unsafe { std::mem::transmute(self) };
    }
}

impl Into<Option<Reg8h>> for Reg8l {
    #[inline(always)]
    fn into(self) -> Option<Reg8h> {
        return match self {
            Self::Al | Self::Bl | Self::Cl | Self::Dl => unsafe { Some(std::mem::transmute(self)) },
            Self::Sil
            | Self::Dil
            | Self::Bpl
            | Self::Spl
            | Self::R8b
            | Self::R9b
            | Self::R10b
            | Self::R11b
            | Self::R12b
            | Self::R13b
            | Self::R14b
            | Self::R15b => None,
        };
    }
}

impl Into<Reg64> for Reg8h {
    #[inline(always)]
    fn into(self) -> Reg64 {
        return unsafe { std::mem::transmute(self) };
    }
}

impl Into<Reg32> for Reg8h {
    #[inline(always)]
    fn into(self) -> Reg32 {
        return unsafe { std::mem::transmute(self) };
    }
}

impl Into<Reg16> for Reg8h {
    #[inline(always)]
    fn into(self) -> Reg16 {
        return unsafe { std::mem::transmute(self) };
    }
}

impl Into<Reg8l> for Reg8h {
    #[inline(always)]
    fn into(self) -> Reg8l {
        return unsafe { std::mem::transmute(self) };
    }
}

#[allow(unused_imports, clippy::enum_glob_use)]
use Reg16::*;
#[allow(unused_imports, clippy::enum_glob_use)]
use Reg32::*;
#[allow(unused_imports, clippy::enum_glob_use)]
use Reg64::*;
#[allow(unused_imports, clippy::enum_glob_use)]
use Reg8h::*;
#[allow(unused_imports, clippy::enum_glob_use)]
use Reg8l::*;

static CRASH_ASM: &str = {
    r"; fn crash(msg: str @rdi:rsi, line: uint @rdx, col: uint @rcx)
crash:
 push r12
 push r13

 mov r8, rdi; msg_len: uint
 mov r9, rsi; msg_ptr: ascii*
 mov r12, rdx; line: uint
 mov r13, rcx; col: uint

 mov rdi, CRASH_len
 mov rsi, CRASH
 call str_eprint

 mov dil, ':'
 call ascii_eprint

 mov dil, ' '
 call ascii_eprint

 ; crash message
 mov rdi, r8
 mov rsi, r9
 call str_eprint

 mov dil, newline
 call ascii_eprint

 mov rdi, _AT_len
 mov rsi, _AT
 call str_eprint

 mov dil, ':'
 call ascii_eprint

 mov dil, ' '
 call ascii_eprint

 ; file
 mov rdi, file_len
 mov rsi, file
 call str_eprint

 mov dil, ':'
 call ascii_eprint

 ; line
 mov rdi, r12
 call int_eprint

 mov dil, ':'
 call ascii_eprint

 ; column
 mov rdi, r13
 call int_eprint

 mov dil, newline
 call ascii_eprint

 mov rdi, EXIT_FAILURE
 pop r13
 pop r12
 jmp exit"
};

static ASSERT_ARRAY_INDEX_IN_RANGE_ASM: &str = {
    r"; fn assert_array_index_in_range(index: int @rdi, array_len: uint @rsi, line: uint @rdx, col: uint @rcx)
assert_array_index_in_range:
 cmp rdi, 0
 jl .underflow

 cmp rdi, rsi
 jge .overflow

 ret

.underflow:
 mov rdi, attempt_array_index_underflow_len
 mov rsi, attempt_array_index_underflow
 jmp crash

.overflow:
 mov rdi, attempt_array_index_overflow_len
 mov rsi, attempt_array_index_overflow
 jmp crash"
};

static ASSERT_INT_BIT_INDEX_IN_RANGE_ASM: &str = {
    r"; fn assert_int_bit_index_in_range(index: int @rdi, bits: uint @rsi, line: uint @rdx, col: uint @rcx)
assert_int_bit_index_in_range:
 cmp rdi, 0
 jl .underflow

 cmp rdi, rsi
 jge .overflow

 ret

.underflow:
 mov rdi, attempt_int_bit_index_underflow_len
 mov rsi, attempt_int_bit_index_underflow
 jmp crash

.overflow:
 mov rdi, attempt_int_bit_index_overflow_len
 mov rsi, attempt_int_bit_index_overflow
 jmp crash"
};

static ASSERT_STR_INDEX_IN_RANGE_ASM: &str = {
    r"; fn assert_str_index_in_range(index: int @rdi, str_len: uint @rsi, line: uint @rdx, col: uint @rcx)
assert_str_index_in_range:
 cmp rdi, 0
 jl .underflow

 cmp rdi, rsi
 jge .overflow

 ret

.underflow:
 mov rdi, attempt_str_index_underflow_len
 mov rsi, attempt_str_index_underflow
 jmp crash

.overflow:
 mov rdi, attempt_str_index_overflow_len
 mov rsi, attempt_str_index_overflow
 jmp crash"
};

static INT_TO_STR_ASM: &str = {
    r"; fn int_str: str @rax:rdx = int_to_str(self: int @rdi)
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
 ret"
};

static INT_SAFE_POW_ASM: &str = {
    r"; fn result: int @rax = int_safe_pow(self: int @rdi, exponent: uint @rsi, line: uint @rdx, col: uint @rcx)
int_safe_pow:
 cmp rsi, 0
 jl .exponent_negative
 jg .exponent_positive

 mov rax, 1
 ret

.exponent_negative:
 mov rdi, attempt_exponent_negative_len
 mov rsi, attempt_exponent_negative
 jmp crash

.exponent_positive:
 cmp rsi, 1
 jne .exponent_not_one
 mov rax, rdi
 ret

.exponent_not_one:
 mov rax, rdi
 mov r8, rsi
 mov r10, 1

.next_power:
 cmp r8, 1
 jle .done

 test r8, 1
 jnz .exponent_odd

 mov rdi, rax
 mov rsi, rax
 call int_safe_mul_pow
 mov rax, rdi

 shr r8, 1
 jmp .next_power

.exponent_odd:
 mov rdi, r10
 mov rsi, rax
 call int_safe_mul_pow
 mov r10, rdi

 mov rdi, rax
 mov rsi, rax
 call int_safe_mul_pow
 mov rax, rdi

 dec r8
 shr r8, 1
 jmp .next_power

.done:
 mov rdi, rax
 mov rsi, r10
 call int_safe_mul_pow
 mov rax, rdi

 ret"
};

static INT_WRAPPING_POW_ASM: &str = {
    r"; fn result: int @rax = int_wrapping_pow(self: int @rdi, exponent: uint @rsi, line: uint @rdx, col: uint @rcx)
int_wrapping_pow:
 cmp rsi, 0
 jl .exponent_negative
 jg .exponent_positive

 mov rax, 1
 ret

.exponent_negative:
 mov rdi, attempt_exponent_negative_len
 mov rsi, attempt_exponent_negative
 jmp crash

.exponent_positive:
 cmp rsi, 1
 jne .exponent_not_one
 mov rax, rdi
 ret

.exponent_not_one:
 mov rax, rdi
 mov r8, 1

.next_power:
 cmp rsi, 1
 jle .done

 test rsi, 1
 jnz .exponent_odd

 imul rax, rax
 shr rsi, 1
 jmp .next_power

.exponent_odd:
 imul r8, rax
 imul rax, rax

 dec rsi
 shr rsi, 1
 jmp .next_power

.done:
 imul rax, r8

 ret"
};

static INT_SATURATING_POW_ASM: &str = {
    r"; fn result: int @rax = int_saturating_pow(self: int @rdi, exponent: uint @rsi, line: uint @rdx, col: uint @rcx)
int_saturating_pow:
 cmp rsi, 0
 jl .exponent_negative
 jg .exponent_positive

 mov rax, 1
 ret

.exponent_negative:
 mov rdi, attempt_exponent_negative_len
 mov rsi, attempt_exponent_negative
 jmp crash

.exponent_positive:
 cmp rsi, 1
 jne .exponent_not_one
 mov rax, rdi
 ret

.exponent_not_one:
 mov rax, rdi
 mov r8, rsi
 mov r10, 1

.next_power:
 cmp r8, 1
 jle .done

 test r8, 1
 jnz .exponent_odd

 mov rdi, rax
 mov rsi, rax
 call int_saturating_mul
 mov rax, rdi

 shr r8, 1
 jmp .next_power

.exponent_odd:
 mov rdi, r10
 mov rsi, rax
 call int_saturating_mul
 mov r10, rdi

 mov rdi, rax
 mov rsi, rax
 call int_saturating_mul
 mov rax, rdi

 dec r8
 shr r8, 1
 jmp .next_power

.done:
 mov rdi, rax
 mov rsi, r10
 call int_saturating_mul
 mov rax, rdi

 ret"
};

static INT_SAFE_MUL_POW_ASM: &str = {
    r"; fn int_safe_mul_pow(lhs: int @rdi, rhs: int @rsi, line: uint @rdx, col: uint @rcx)
int_safe_mul_pow:
 imul rdi, rsi
 jno .no_overflow
 mov rdi, pow_overflow_len
 mov rsi, pow_overflow
 jmp crash

.no_overflow:
 ret"
};

static INT_SAFE_MUL_ASM: &str = {
    r"; fn int_safe_mul(lhs: int @rdi, rhs: int @rsi, line: uint @rdx, col: uint @rcx)
int_safe_mul:
 imul rdi, rsi
 jno .no_overflow
 mov rdi, mul_overflow_len
 mov rsi, mul_overflow
 jmp crash

.no_overflow:
 ret"
};

static INT_SATURATING_MUL_ASM: &str = {
    r"; fn int_saturating_mul(lhs: int @rdi, rhs: int @rsi)
int_saturating_mul:
 mov rax, rdi
 xor rdi, rsi
 shr rdi, 63
 mov rdx, INT_MAX
 add rdi, rdx
 imul rsi
 cmovc rax, rdi
 mov rdi, rax
 ret"
};

static INT_SAFE_DIV_ASM: &str = {
    r"; fn int_safe_div(lhs: int @rdi, rhs: int @rsi, line: uint @rdx, col: uint @rcx)
int_safe_div:
 test rsi, rsi
 jnz .check_no_int_min_div_minus_one

 mov rdi, attempt_division_by_zero_len
 mov rsi, attempt_division_by_zero
 jmp crash

.check_no_int_min_div_minus_one:
 mov rax, INT_MIN
 cmp rdi, rax
 jne .no_int_min_div_minus_one
 cmp rsi, -1
 jne .no_int_min_div_minus_one

 mov rdi, attempt_int_min_div_by_minus_one_len
 mov rsi, attempt_int_min_div_by_minus_one
 jmp crash

.no_int_min_div_minus_one:
 mov rax, rdi
 cqo
 idiv rsi
 mov rdi, rax
 ret"
};

static INT_WRAPPING_DIV_ASM: &str = {
    r"; fn int_wrapping_div(lhs: int @rdi, rhs: int @rsi, line: uint @rdx, col: uint @rcx)
int_wrapping_div:
 test rsi, rsi
 jnz .check_no_int_min_div_minus_one

 mov rdi, attempt_division_by_zero_len
 mov rsi, attempt_division_by_zero
 jmp crash

.check_no_int_min_div_minus_one:; INT_MIX @rdi ^ -1 @rsi == INT_MAX @r8
 mov rax, rdi
 xor rax, rsi
 mov r8, INT_MAX
 cmp rax, r8
 jne .no_int_min_div_minus_one

 ret; return INT_MIN @rdi

.no_int_min_div_minus_one:
 mov rax, rdi
 cqo
 idiv rsi
 mov rdi, rax
 ret"
};

static INT_SATURATING_DIV_ASM: &str = {
    r"; fn int_saturating_div(lhs: int @rdi, rhs: int @rsi, line: uint @rdx, col: uint @rcx)
int_saturating_div:
 test rsi, rsi
 jnz .check_no_int_min_div_minus_one

 mov rdi, attempt_division_by_zero_len
 mov rsi, attempt_division_by_zero
 jmp crash

.check_no_int_min_div_minus_one:; INT_MIX @rdi ^ -1 @rsi == INT_MAX @r8
 mov rax, rdi
 xor rax, rsi
 mov r8, INT_MAX
 cmp rax, r8
 jne .no_int_min_div_minus_one

 mov rdi, r8
 ret; return INT_MAX @r8

.no_int_min_div_minus_one:
 mov rax, rdi
 cqo
 idiv rsi
 mov rdi, rax
 ret"
};

static INT_SAFE_REMAINDER_ASM: &str = {
    r"; fn int_safe_remainder(lhs: int @rdi, rhs: int @rsi, line: uint @rdx, col: uint @rcx)
int_safe_remainder:
 test rsi, rsi
 jnz .check_no_int_min_div_minus_one

 mov rdi, attempt_remainder_zero_len
 mov rsi, attempt_remainder_zero
 jmp crash

.check_no_int_min_div_minus_one:; INT_MIX @rdi ^ -1 @rsi == INT_MAX @r8
 mov rax, rdi
 xor rax, rsi
 mov r8, INT_MAX
 cmp rax, r8
 jne .no_int_min_div_minus_one

 mov rdi, attempt_remainder_int_min_div_by_minus_one_len
 mov rsi, attempt_remainder_int_min_div_by_minus_one
 jmp crash

.no_int_min_div_minus_one:
 mov rax, rdi
 cqo
 idiv rsi
 mov rdi, rdx
 ret"
};

static INT_SAFE_ADD_ASM: &str = {
    r"; fn int_safe_add(lhs: int @rdi, rhs: int @rsi, line: uint @rdx, col: uint @rcx)
int_safe_add:
 add rdi, rsi
 jno .no_overflow
 mov rdi, add_overflow_len
 mov rsi, add_overflow
 jmp crash

.no_overflow:
 ret"
};

static INT_SATURATING_ADD_ASM: &str = {
    r"; fn int_saturating_add(lhs: int @rdi, rhs: int @rsi)
int_saturating_add:
 add rdi, rsi
 jno .no_overflow
 mov rdi, INT_MAX
 mov rsi, INT_MIN
 cmovc rdi, rsi

.no_overflow:
 ret"
};

static INT_SAFE_ABS_ASM: &str = {
    r"; fn int_safe_abs(lhs: int @rdi, _dummy: int @rsi, line: uint @rdx, col: uint @rcx)
int_safe_abs:
 mov rsi, rdi
 sar rsi, INT_BITS - 1
 xor rdi, rsi
 sub rdi, rsi
 jno .no_overflow
 mov rdi, abs_overflow_len
 mov rsi, abs_overflow
 jmp crash

.no_overflow:
 ret"
};

static INT_WRAPPING_ABS_ASM: &str = {
    r"; fn int_wrapping_abs(lhs: int @rdi)
int_wrapping_abs:
 mov rsi, rdi
 sar rsi, INT_BITS - 1
 xor rdi, rsi
 sub rdi, rsi
 ret"
};

static INT_SATURATING_ABS_ASM: &str = {
    r"; fn int_saturating_abs(lhs: int @rdi)
int_saturating_abs:
 mov rsi, rdi
 sar rsi, INT_BITS - 1
 xor rdi, rsi
 sub rdi, rsi
 jno .no_overflow
 mov rdi, INT_MAX

.no_overflow:
 ret"
};

static INT_SAFE_SUB_ASM: &str = {
    r"; fn int_safe_sub(lhs: int @rdi, rhs: int @rsi, line: uint @rdx, col: uint @rcx)
int_safe_sub:
 sub rdi, rsi
 jno .no_overflow
 mov rdi, sub_overflow_len
 mov rsi, sub_overflow
 jmp crash

.no_overflow:
 ret"
};

static INT_SATURATING_SUB_ASM: &str = {
    r"; fn int_saturating_sub(lhs: int @rdi, rhs: int @rsi)
int_saturating_sub:
 sub rdi, rsi
 jno .no_overflow
 mov rdi, INT_MIN
 mov rsi, INT_MAX
 cmovc rdi, rsi

.no_overflow:
 ret"
};

static INT_SAFE_NEGATE_ASM: &str = {
    r"; fn int_safe_negate(lhs: int @rdi, _dummy: int @rsi, line: uint @rdx, col: uint @rcx)
int_safe_negate:
 neg rdi
 jno .no_overflow
 mov rdi, negate_overflow_len
 mov rsi, negate_overflow
 jmp crash

.no_overflow:
 ret"
};

static INT_SATURATING_NEGATE_ASM: &str = {
    r"; fn int_saturating_negate(lhs: int @rdi)
int_saturating_negate:
 neg rdi
 jno .no_overflow
 mov rdi, INT_MAX

.no_overflow:
 ret"
};

static INT_SAFE_LEFT_SHIFT_ASM: &str = {
    r"; fn int_safe_left_shift(lhs: int @rdi, rhs: int @rsi, line: uint @rdx, col: uint @rcx)
int_safe_left_shift:
 cmp rsi, 0
 jge .no_negative_shift

 mov rdi, attempt_left_shift_negative_len
 mov rsi, attempt_left_shift_negative
 jmp crash

.no_negative_shift:
 cmp rsi, (1 << 6) - 1
 jle .no_shift_over_6_bits

 mov rdi, attempt_left_shift_over_6_bits_len
 mov rsi, attempt_left_shift_over_6_bits
 jmp crash

.no_shift_over_6_bits:
 shlx rax, rdi, rsi
 sarx rsi, rax, rsi
 cmp rdi, rsi
 je .no_overflow

 mov rdi, left_shift_overflow_len
 mov rsi, left_shift_overflow
 jmp crash

.no_overflow:
 mov rdi, rax
 ret"
};

static INT_WRAPPING_LEFT_SHIFT_ASM: &str = {
    r"; fn int_wrapping_left_shift(lhs: int @rdi, rhs: int @rsi, line: uint @rdx, col: uint @rcx)
int_wrapping_left_shift:
 cmp rsi, 0
 jge .no_negative_shift

 mov rdi, attempt_left_shift_negative_len
 mov rsi, attempt_left_shift_negative
 jmp crash

.no_negative_shift:
 cmp rsi, (1 << 6) - 1
 jle .no_shift_over_6_bits

 mov rdi, attempt_left_shift_over_6_bits_len
 mov rsi, attempt_left_shift_over_6_bits
 jmp crash

.no_shift_over_6_bits:
 shlx rdi, rdi, rsi
 ret"
};

static INT_SATURATING_LEFT_SHIFT_ASM: &str = {
    r"; fn int_saturating_left_shift(lhs: int @rdi, rhs: int @rsi, line: uint @rdx, col: uint @rcx)
int_saturating_left_shift:
 cmp rsi, 0
 jge .no_negative_shift

 mov rdi, attempt_left_shift_negative_len
 mov rsi, attempt_left_shift_negative
 jmp crash

.no_negative_shift:
 cmp rsi, (1 << 6) - 1
 jle .no_shift_over_6_bits

 mov rdi, attempt_left_shift_over_6_bits_len
 mov rsi, attempt_left_shift_over_6_bits
 jmp crash

.no_shift_over_6_bits:
 cmp rdi, 0
 jg .positive_lhs
 jl .negative_lhs

 ret; return 0 @rdi << rhs @rsi -> return 0

.positive_lhs:
 lzcnt rax, rdi
 cmp rax, rsi
 cmovl rsi, rax
 shlx rdi, rdi, rsi
 ret

.negative_lhs:
 mov rax, rdi
 not rax
 lzcnt rax, rax
 sub rax, 1

 cmp rax, rsi
 cmovl rsi, rax
 shlx rdi, rdi, rsi
 ret"
};

static INT_SAFE_RIGHT_SHIFT_ASM: &str = {
    r"; fn int_safe_right_shift(lhs: int @rdi, rhs: int @rsi, line: uint @rdx, col: uint @rcx)
int_safe_right_shift:
 cmp rsi, 0
 jge .no_negative_shift

 mov rdi, attempt_right_shift_negative_len
 mov rsi, attempt_right_shift_negative
 jmp crash

.no_negative_shift:
 cmp rsi, (1 << 6) - 1
 jle .no_shift_over_6_bits

 mov rdi, attempt_right_shift_over_6_bits_len
 mov rsi, attempt_right_shift_over_6_bits
 jmp crash

.no_shift_over_6_bits:
 sarx rdi, rdi, rsi
 ret"
};

static INT_SAFE_LEFT_ROTATE_ASM: &str = {
    r"; fn int_safe_left_rotate(lhs: int @rdi, rhs: int @rsi, line: uint @rdx, col: uint @rcx)
int_safe_left_rotate:
 cmp rsi, 0
 jge .no_negative_shift

 mov rdi, attempt_left_rotate_negative_len
 mov rsi, attempt_left_rotate_negative
 jmp crash

.no_negative_shift:
 cmp rsi, (1 << 6) - 1
 jle .no_shift_over_6_bits

 mov rdi, attempt_left_rotate_over_6_bits_len
 mov rsi, attempt_left_rotate_over_6_bits
 jmp crash

.no_shift_over_6_bits:
 mov cl, sil
 rol rdi, cl
 ret"
};

static INT_SAFE_RIGHT_ROTATE_ASM: &str = {
    r"; fn int_safe_right_rotate(lhs: int @rdi, rhs: int @rsi, line: uint @rdx, col: uint @rcx)
int_safe_right_rotate:
 cmp rsi, 0
 jge .no_negative_shift

 mov rdi, attempt_right_rotate_negative_len
 mov rsi, attempt_right_rotate_negative
 jmp crash

.no_negative_shift:
 cmp rsi, (1 << 6) - 1
 jle .no_shift_over_6_bits

 mov rdi, attempt_right_rotate_over_6_bits_len
 mov rsi, attempt_right_rotate_over_6_bits
 jmp crash

.no_shift_over_6_bits:
 mov cl, sil
 ror rdi, cl
 ret"
};

static INT_PRINT_ASM: &str = {
    r"; fn int_print(self: int @rdi)
int_print:
 call int_to_str
 mov rdi, stdout
 mov rsi, rax
 mov rax, SYS_write
 syscall
 ret"
};

static INT_EPRINT_ASM: &str = {
    r"; fn int_eprint(self: int @rdi)
int_eprint:
 call int_to_str
 mov rdi, stderr
 mov rsi, rax
 mov rax, SYS_write
 syscall
 ret"
};

static INT_ARRAY_DEBUG_PRINT_ASM: &str = {
    r"; fn int_array_debug_print(self: int[]& @rdi:rsi)
int_array_debug_print:
 mov r8, rdi; len: uint
 mov r9, rsi; array_ptr: int[]*

 mov dil, '['
 call ascii_print

 test r8, r8
 jz .done

.next:
 dec r8
 jz .last

 mov rdi, [r9]
 call int_print

 mov dil, ','
 call ascii_print

 mov dil, ' '
 call ascii_print

 add r9, 8
 jmp .next

.last:
 mov rdi, [r9]
 call int_print

.done:
 mov dil, ']'
 call ascii_print

 ret"
};

static INT_ARRAY_DEBUG_EPRINT_ASM: &str = {
    r"; fn int_array_debug_eprint(self: int[]& @rdi:rsi)
int_array_debug_eprint:
 mov r8, rdi; len: uint
 mov r9, rsi; array_ptr: int[]*

 mov dil, '['
 call ascii_eprint

 test r8, r8
 jz .done

.next:
 dec r8
 jz .last

 mov rdi, [r9]
 call int_eprint

 mov dil, ','
 call ascii_eprint

 mov dil, ' '
 call ascii_eprint

 add r9, 8
 jmp .next

.last:
 mov rdi, [r9]
 call int_eprint

.done:
 mov dil, ']'
 call ascii_eprint

 ret"
};

static ASCII_PRINT_ASM: &str = {
    r"; fn ascii_print(self: ascii @dil)
ascii_print:
 mov [rsp - 1], dil
 lea rsi, [rsp - 1]
 mov rdi, stdout
 mov rdx, 1
 mov rax, SYS_write
 syscall
 ret"
};

static ASCII_EPRINT_ASM: &str = {
    r"; fn ascii_eprint(self: ascii @dil)
ascii_eprint:
 mov [rsp - 1], dil
 lea rsi, [rsp - 1]
 mov rdi, stderr
 mov rdx, 1
 mov rax, SYS_write
 syscall
 ret"
};

static ASCII_ARRAY_DEBUG_PRINT_ASM: &str = {
    r"; fn ascii_array_debug_print(self: ascii[]& @rdi:rsi)
ascii_array_debug_print:
 mov r8, rdi; len: uint
 mov r9, rsi; array_ptr: ascii[]*

 mov dil, '['
 call ascii_print

 test r8, r8
 jz .done

.next:
 dec r8
 jz .last

 mov dil, [r9]
 call ascii_print

 mov dil, ','
 call ascii_print

 mov dil, ' '
 call ascii_print

 inc r9
 jmp .next

.last:
 mov dil, [r9]
 call ascii_print

.done:
 mov dil, ']'
 call ascii_print

 ret"
};

static ASCII_ARRAY_DEBUG_EPRINT_ASM: &str = {
    r"; fn ascii_array_debug_eprint(self: ascii[]& @rdi:rsi)
ascii_array_debug_eprint:
 mov r8, rdi; len: uint
 mov r9, rsi; array_ptr: ascii[]*

 mov dil, '['
 call ascii_eprint

 test r8, r8
 jz .done

.next:
 dec r8
 jz .last

 mov dil, [r9]
 call ascii_eprint

 mov dil, ','
 call ascii_eprint

 mov dil, ' '
 call ascii_eprint

 inc r9
 jmp .next

.last:
 mov dil, [r9]
 call ascii_eprint

.done:
 mov dil, ']'
 call ascii_eprint

 ret"
};

static BOOL_PRINT_ASM: &str = {
    r"; fn bool_print(self: bool @dil)
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
 ret"
};

static BOOL_EPRINT_ASM: &str = {
    r"; fn bool_eprint(self: bool @dil)
bool_eprint:
 cmp dil, true
 mov rsi, true_str
 mov rdi, false_str
 cmovne rsi, rdi
 mov rdx, true_str_len
 mov rdi, false_str_len
 cmovne rdx, rdi

 mov rdi, stderr
 mov rax, SYS_write
 syscall
 ret"
};

static BOOL_ARRAY_DEBUG_PRINT_ASM: &str = {
    r"; fn bool_array_debug_print(self: bool[]& @rdi:rsi)
bool_array_debug_print:
 mov r8, rdi; len: uint
 mov r9, rsi; array_ptr: bool[]*

 mov dil, '['
 call ascii_print

 test r8, r8
 jz .done

.next:
 dec r8
 jz .last

 mov dil, [r9]
 call bool_print

 mov dil, ','
 call ascii_print

 mov dil, ' '
 call ascii_print

 inc r9
 jmp .next

.last:
 mov dil, [r9]
 call bool_print

.done:
 mov dil, ']'
 call ascii_print

 ret"
};

static BOOL_ARRAY_DEBUG_EPRINT_ASM: &str = {
    r"; fn bool_array_debug_eprint(self: bool[]& @rdi:rsi)
bool_array_debug_eprint:
 mov r8, rdi; len: uint
 mov r9, rsi; array_ptr: bool[]*

 mov dil, '['
 call ascii_eprint

 test r8, r8
 jz .done

.next:
 dec r8
 jz .last

 mov dil, [r9]
 call bool_eprint

 mov dil, ','
 call ascii_eprint

 mov dil, ' '
 call ascii_eprint

 inc r9
 jmp .next

.last:
 mov dil, [r9]
 call bool_eprint

.done:
 mov dil, ']'
 call ascii_eprint

 ret"
};

static STR_CMP_ASM: &str = {
    r"; fn cmp: int @rax = str_cmp(self: str @rdi:rsi, other: @rdx:rcx)
str_cmp:
 mov rax, rdi
 sub rax, rdx
 cmovb rdx, rdi

 mov rdi, rcx
 mov rcx, rdx
 repe cmpsb
 je .eq
 mov rax, LESS
 mov rsi, GREATER
 cmovg rax, rsi
 ret

.eq:
 cmp rax, 0
 mov rax, LESS
 mov rsi, EQUAL
 cmove rax, rsi
 mov rsi, GREATER
 cmovg rax, rsi
 ret"
};

static STR_EQ_ASM: &str = {
    r"; fn equals: bool @al = str_eq(self: str @rdi:rsi, other: @rdx:rcx)
str_eq:
 cmp rdi, rdx
 jne .done

 mov rdi, rcx
 mov rcx, rdx
 repe cmpsb

.done:
 mov rax, false
 sete al
 ret"
};

static STR_ARRAY_CMP_ASM: &str = {
    r"; fn cmp: int @rax = str_array_cmp[N: uint @rdi](self: str[N]* @rdi:rsi, other: str[N]* @_rdx:rcx)
str_array_cmp:
 mov r8, rsi; self_ptr: str[]*
 mov r9, rcx; other_ptr: str[]*
 mov r10, rdi; N: uint

.next:
 mov rdi, [r8]
 mov rsi, [r8 + 8]
 mov rdx, [r9]
 mov rcx, [r9 + 8]
 call str_cmp
 je .eq
 ret

.eq:
 add r8, 16
 add r9, 16
 dec r10
 jnz .next
 ret"
};

static STR_ARRAY_EQ_ASM: &str = {
    r"; fn equals: bool @al = str_array_eq[N: uint @rdi](self: str[N]* @rdi:rsi, other: str[N]* @_rdx:rcx)
str_array_eq:
 mov r8, rsi; self_ptr: str[]*
 mov r9, rcx; other_ptr: str[]*
 mov r10, rdi; N: uint

.next:
 mov rdi, [r8]
 mov rsi, [r8 + 8]
 mov rdx, [r9]
 mov rcx, [r9 + 8]
 call str_eq
 je .eq
 ret

.eq:
 add r8, 16
 add r9, 16
 dec r10
 jnz .next
 ret"
};

static STR_PRINT_ASM: &str = {
    r"; fn str_print(self: str @rdi:rsi)
str_print:
 mov rdx, rdi
 mov rdi, stdout
 mov rax, SYS_write
 syscall
 ret"
};

static STR_EPRINT_ASM: &str = {
    r"; fn str_eprint(self: str @rdi:rsi)
str_eprint:
 mov rdx, rdi
 mov rdi, stderr
 mov rax, SYS_write
 syscall
 ret"
};

static STR_ARRAY_DEBUG_PRINT_ASM: &str = {
    r"; fn str_array_debug_print(self: str[]& @rdi:rsi)
str_array_debug_print:
 mov r8, rdi; len: uint
 mov r9, rsi; array_ptr: str[]*

 mov dil, '['
 call ascii_print

 test r8, r8
 jz .done

.next:
 dec r8
 jz .last

 mov rdi, [r9]
 mov rsi, [r9 + 8]
 call str_print

 mov dil, ','
 call ascii_print

 mov dil, ' '
 call ascii_print

 add r9, 16
 jmp .next

.last:
 mov rdi, [r9]
 mov rsi, [r9 + 8]
 call str_print

.done:
 mov dil, ']'
 call ascii_print

 ret"
};

static STR_ARRAY_DEBUG_EPRINT_ASM: &str = {
    r"; fn str_array_debug_eprint(self: str[]& @rdi:rsi)
str_array_debug_eprint:
 mov r8, rdi; len: uint
 mov r9, rsi; array_ptr: str[]*

 mov dil, '['
 call ascii_eprint

 test r8, r8
 jz .done

.next:
 dec r8
 jz .last

 mov rdi, [r9]
 mov rsi, [r9 + 8]
 call str_eprint

 mov dil, ','
 call ascii_eprint

 mov dil, ' '
 call ascii_eprint

 add r9, 16
 jmp .next

.last:
 mov rdi, [r9]
 mov rsi, [r9 + 8]
 call str_eprint

.done:
 mov dil, ']'
 call ascii_eprint

 ret"
};

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
}

// Generation of compilation artifacts (.asm, .o, executable)
impl<'src, 'ast: 'src> Compiler<'src, 'ast> {
    pub fn compile(
        src: &'src SrcFile,
        artifacts: &Artifacts,
        ast: &'ast [Scope<'src>],
    ) -> Result<(), Error> {
        let asm_file = match File::create(&artifacts.asm_path) {
            Ok(file) => file,
            Err(err) => {
                return Err(Error {
                    kind: ErrorKind::CouldNotCreateFile { path: artifacts.asm_path.clone() },
                    cause: ErrorCause::IoError(err),
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
        };

        if this.ast.is_empty() {
            this.asm += "exit:\
                \n mov rdi, EXIT_SUCCESS";
        } else {
            for scope in this.ast {
                for var in &scope.variables {
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

            this.asm += " mov rdi, EXIT_SUCCESS\
                \n\
                \nexit:";
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
 stdout: equ 1
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

 attempt_int_min_div_by_minus_one: db "attempt to divide the minimum integer value by -1"
 attempt_int_min_div_by_minus_one_len: equ $ - attempt_int_min_div_by_minus_one

 attempt_remainder_zero: db "attempt to take the remainder of a division by zero"
 attempt_remainder_zero_len: equ $ - attempt_remainder_zero

 attempt_remainder_int_min_div_by_minus_one: db "attempt to take the remainder of the minimun integer value divided by -1"
 attempt_remainder_int_min_div_by_minus_one_len: equ $ - attempt_remainder_int_min_div_by_minus_one

 attempt_exponent_negative: db "attempt to raise a number to a negative power"
 attempt_exponent_negative_len: equ $ - attempt_exponent_negative

 attempt_array_index_underflow: db "negative array index"
 attempt_array_index_underflow_len: equ $ - attempt_array_index_underflow

 attempt_array_index_overflow: db "array index out of bounds"
 attempt_array_index_overflow_len: equ $ - attempt_array_index_overflow

 attempt_int_bit_index_underflow: db "negative int bit array index"
 attempt_int_bit_index_underflow_len: equ $ - attempt_int_bit_index_underflow

 attempt_int_bit_index_overflow: db "int bit array index out of bounds"
 attempt_int_bit_index_overflow_len: equ $ - attempt_int_bit_index_overflow

 attempt_str_index_underflow: db "negative string index"
 attempt_str_index_underflow_len: equ $ - attempt_str_index_underflow

 attempt_str_index_overflow: db "string index out of bounds"
 attempt_str_index_overflow_len: equ $ - attempt_str_index_overflow

 attempt_left_shift_negative: db "attempting to shift left by a negative quantity"
 attempt_left_shift_negative_len: equ $ - attempt_left_shift_negative

 attempt_left_shift_over_6_bits: db "attempting to shift left by a quantity over a 6 bit integer"
 attempt_left_shift_over_6_bits_len: equ $ - attempt_left_shift_over_6_bits

 attempt_right_shift_negative: db "attempting to shift right by a negative quantity"
 attempt_right_shift_negative_len: equ $ - attempt_right_shift_negative

 attempt_right_shift_over_6_bits: db "attempting to shift right by a quantity over a 6 bit integer"
 attempt_right_shift_over_6_bits_len: equ $ - attempt_right_shift_over_6_bits

 attempt_left_rotate_negative: db "attempting to rotate left by a negative quantity"
 attempt_left_rotate_negative_len: equ $ - attempt_left_rotate_negative

 attempt_left_rotate_over_6_bits: db "attempting to rotate left by a quantity over a 6 bit integer"
 attempt_left_rotate_over_6_bits_len: equ $ - attempt_left_rotate_over_6_bits

 attempt_right_rotate_negative: db "attempting to rotate right by a negative quantity"
 attempt_right_rotate_negative_len: equ $ - attempt_right_rotate_negative

 attempt_right_rotate_over_6_bits: db "attempting to rotate right by a quantity over a 6 bit integer"
 attempt_right_rotate_over_6_bits_len: equ $ - attempt_right_rotate_over_6_bits

 pow_overflow: db "exponentiation operation resulted in an overflow"
 pow_overflow_len: equ $ - pow_overflow

 mul_overflow: db "multiplication operation resulted in an overflow"
 mul_overflow_len: equ $ - mul_overflow

 add_overflow: db "add operation resulted in an overflow"
 add_overflow_len: equ $ - add_overflow

 abs_overflow: db "unary absolute value operation resulted in an overflow"
 abs_overflow_len: equ $ - abs_overflow

 sub_overflow: db "subtraction operation resulted in an overflow"
 sub_overflow_len: equ $ - sub_overflow

 negate_overflow: db "unary negation operation resulted in an overflow"
 negate_overflow_len: equ $ - negate_overflow

 left_shift_overflow: db "left shift operation resulted in an overflow"
 left_shift_overflow_len: equ $ - left_shift_overflow

 file: db "{src_path}"
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
            return Err(Error {
                kind: ErrorKind::WritingAssemblyFailed,
                cause: ErrorCause::IoError(err),
            });
        }

        if let Err(err) = asm_writer.flush() {
            return Err(Error {
                kind: ErrorKind::WritingAssemblyFailed,
                cause: ErrorCause::IoError(err),
            });
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

                self.asm += " mov dil, newline\
                    \n call ascii_print\n\n";
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

                self.asm += " mov dil, newline\
                    \n call ascii_eprint\n\n";
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
                match looop.kind {
                    LoopKind::Loop => {
                        self.condition(&looop.condition, &loop_end_tag);
                        self.node(&looop.statement);

                        _ = writeln!(
                            self.asm,
                            " jmp {loop_tag}\
                            \n{loop_end_tag}:\n"
                        );
                    }
                    LoopKind::DoLoop => {
                        self.node(&looop.statement);
                        self.condition_reversed(&looop.condition, &loop_tag);
                    }
                }

                _ = self.loop_counters.pop();
            }
            Node::Definition { scope_index, var_index } => {
                let ast_var = &self.ast[*scope_index].variables[*var_index];
                let name = ast_var.name;
                let value = &ast_var.value;

                let var = self.resolve(name);
                let dst_offset = var.offset;

                _ = writeln!(self.asm, " ; {name} = {value}");
                self.assignment(value, dst_offset);
            }
            Node::Assignment { scope_index, var_index, new_value } => {
                let ast_var = &self.ast[*scope_index].variables[*var_index];
                let name = ast_var.name;

                let var = self.resolve(name);
                let dst_offset = var.offset;

                _ = writeln!(self.asm, " ; {name} = {new_value}");
                self.assignment(new_value, dst_offset);
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
    fn default(typ: &Type) -> Self {
        return match typ {
            Type::Int | Type::Ascii | Type::Bool => Self::Reg(Rdi),
            Type::Str | Type::Array { .. } => Self::View { len: Rdi, ptr: Rsi },
            Type::Infer => unreachable!("should have been inferred"),
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

        let string_bytes = unsafe { std::str::from_utf8_unchecked(string) };
        let string_chars = string_bytes.escape_debug();
        _ = writeln!(
            self.string_labels,
            "\n str_{string_index}: db `{string_chars}`\
            \n str_{string_index}_len: equ $ - str_{string_index}",
        );

        self.strings.push(string);
        return string_index;
    }

    // IDEA(stefano): string comparison operators could also return the index where
    // the mismatch occured, since repe CMPcc stops at
    // mismatch_index @rdx = len @rdx - reverse_mismatch_index @rcx - 1, so:
    //
    // repe cmpsq
    // mov rdi, false
    // setz dil
    //
    // would become this:
    //
    // repe cmpsq
    // mov rdi, false
    // setz dil
    // sub rdx, rcx
    // dec rdx
    #[allow(clippy::single_call_fn)]
    fn binary_str_str_dst_and_asm(op: Op) -> (Dst, Dst, Cow<'static, str>) {
        return match op {
            Op::EqualsEquals => (
                Dst::View { len: Rdi, ptr: Rsi },
                Dst::View { len: Rdx, ptr: Rcx },
                " call str_eq\
                \n movzx rdi, al"
                    .into(),
            ),
            Op::NotEquals => (
                Dst::View { len: Rdi, ptr: Rsi },
                Dst::View { len: Rdx, ptr: Rcx },
                " call str_eq\
                \n xor rax, 1\
                \n movzx rdi, al"
                    .into(),
            ),
            Op::Greater => (
                Dst::View { len: Rdi, ptr: Rsi },
                Dst::View { len: Rdx, ptr: Rcx },
                " call str_cmp\
                \n cmp rax, EQUAL\
                \n mov rdi, false\
                \n setg dil"
                    .into(),
            ),
            Op::GreaterOrEquals => (
                Dst::View { len: Rdi, ptr: Rsi },
                Dst::View { len: Rdx, ptr: Rcx },
                " call str_cmp\
                \n cmp rax, EQUAL\
                \n mov rdi, false\
                \n setge dil"
                    .into(),
            ),
            Op::Less => (
                Dst::View { len: Rdi, ptr: Rsi },
                Dst::View { len: Rdx, ptr: Rcx },
                " call str_cmp\
                \n cmp rax, EQUAL\
                \n mov rdi, false\
                \n setl dil"
                    .into(),
            ),
            Op::LessOrEquals => (
                Dst::View { len: Rdi, ptr: Rsi },
                Dst::View { len: Rdx, ptr: Rcx },
                " call str_cmp\
                \n cmp rax, EQUAL\
                \n mov rdi, false\
                \n setle dil"
                    .into(),
            ),
            Op::Compare => (
                Dst::View { len: Rdi, ptr: Rsi },
                Dst::View { len: Rdx, ptr: Rcx },
                " call str_cmp\
                \n mov rdi, rax"
                    .into(),
            ),
            Op::Pow
            | Op::WrappingPow
            | Op::SaturatingPow
            | Op::PowEquals
            | Op::WrappingPowEquals
            | Op::SaturatingPowEquals
            | Op::Times
            | Op::WrappingTimes
            | Op::SaturatingTimes
            | Op::TimesEquals
            | Op::WrappingTimesEquals
            | Op::SaturatingTimesEquals
            | Op::Divide
            | Op::WrappingDivide
            | Op::SaturatingDivide
            | Op::DivideEquals
            | Op::WrappingDivideEquals
            | Op::SaturatingDivideEquals
            | Op::Remainder
            | Op::RemainderEquals
            | Op::Plus
            | Op::WrappingPlus
            | Op::SaturatingPlus
            | Op::PlusEquals
            | Op::WrappingPlusEquals
            | Op::SaturatingPlusEquals
            | Op::Minus
            | Op::WrappingMinus
            | Op::SaturatingMinus
            | Op::MinusEquals
            | Op::WrappingMinusEquals
            | Op::SaturatingMinusEquals
            | Op::And
            | Op::AndEquals
            | Op::BitAnd
            | Op::BitAndEquals
            | Op::Or
            | Op::OrEquals
            | Op::BitOr
            | Op::BitOrEquals
            | Op::BitXor
            | Op::BitXorEquals
            | Op::LeftShift
            | Op::WrappingLeftShift
            | Op::SaturatingLeftShift
            | Op::LeftShiftEquals
            | Op::WrappingLeftShiftEquals
            | Op::SaturatingLeftShiftEquals
            | Op::RightShift
            | Op::RightShiftEquals
            | Op::LeftRotate
            | Op::LeftRotateEquals
            | Op::RightRotate
            | Op::RightRotateEquals => {
                unreachable!("math operation not allowed on strings")
            }
            Op::Equals => unreachable!("should not be present in the ast"),
            Op::Not | Op::Len => unreachable!("should only appear in unary expressions"),
        }
    }

    // IDEA(stefano): array comparison operators could also return the index where
    // the mismatch occured, since repe CMPcc stops at
    // mismatch_index @rdx = len @rdx - reverse_mismatch_index @rcx - 1, so:
    //
    // repe cmpsq
    // mov rdi, false
    // setz dil
    //
    // would become this:
    //
    // repe cmpsq
    // mov rdi, false
    // setz dil
    // sub rdx, rcx
    // dec rdx
    #[allow(clippy::single_call_fn)]
    fn binary_array_array_dst_and_asm(elements_type: &Type, op: Op) -> (Dst, Dst, Cow<'static, str>) {
        return match op {
            Op::EqualsEquals => {
                let eq_fn = match elements_type {
                    Type::Int => {
                        " mov rdi, rcx\
                        \n mov rcx, rdx\
                        \n repe cmpsq\
                        \n mov rdi, false\
                        \n sete dil"
                    }
                    Type::Ascii | Type::Bool => {
                        " mov rdi, rcx\
                        \n mov rcx, rdx\
                        \n repe cmpsb\
                        \n mov rdi, false\
                        \n sete dil"
                    }
                    Type::Str => {
                        " call str_array_eq\
                        \n movzx rdi, al"
                    }
                    Type::Array { .. } => {
                        unreachable!("nested arrays not supported yet")
                    }
                    Type::Infer => {
                        unreachable!("should have been coerced to a concrete type")
                    }
                };

                (
                    Dst::View { len: Rdi, ptr: Rsi },
                    Dst::View { len: Rdx, ptr: Rcx },
                    eq_fn.into(),
                )
            }
            Op::NotEquals => {
                let eq_fn = match elements_type {
                    Type::Int => {
                        " mov rdi, rcx\
                        \n mov rcx, rdx\
                        \n repe cmpsq\
                        \n mov rdi, false\
                        \n setne dil"
                    }
                    Type::Ascii | Type::Bool => {
                        " mov rdi, rcx\
                        \n mov rcx, rdx\
                        \n repe cmpsb\
                        \n mov rdi, false\
                        \n setne dil"
                    }
                    Type::Str => {
                        " cmp str_array_eq\
                        \n xor rax, 1\
                        \n movzx rdi, al"
                    }
                    Type::Array { .. } => {
                        unreachable!("nested arrays not supported yet")
                    }
                    Type::Infer => {
                        unreachable!("should have been coerced to a concrete type")
                    }
                };

                (
                    Dst::View { len: Rdi, ptr: Rsi },
                    Dst::View { len: Rdx, ptr: Rcx },
                    eq_fn.into(),
                )
            }
            Op::Greater => {
                let eq_fn = match elements_type {
                    Type::Int => {
                        " mov rdi, rcx\
                        \n mov rcx, rdx\
                        \n repe cmpsq\
                        \n mov rdi, false\
                        \n setg dil"
                    }
                    Type::Ascii | Type::Bool => {
                        " mov rdi, rcx\
                        \n mov rcx, rdx\
                        \n repe cmpsb\
                        \n mov rdi, false\
                        \n setg dil"
                    }
                    Type::Str => {
                        " call str_array_cmp\
                        \n cmp rax, EQUAL\
                        \n mov rdi, false\
                        \n setg dil"
                    }
                    Type::Array { .. } => {
                        unreachable!("nested arrays not supported yet")
                    }
                    Type::Infer => {
                        unreachable!("should have been coerced to a concrete type")
                    }
                };

                (
                    Dst::View { len: Rdi, ptr: Rsi },
                    Dst::View { len: Rdx, ptr: Rcx },
                    eq_fn.into(),
                )
            }
            Op::GreaterOrEquals => {
                let eq_fn = match elements_type {
                    Type::Int => {
                        " mov rdi, rcx\
                        \n mov rcx, rdx\
                        \n repe cmpsq\
                        \n mov rdi, false\
                        \n setge dil"
                    }
                    Type::Ascii | Type::Bool => {
                        " mov rdi, rcx\
                        \n mov rcx, rdx\
                        \n repe cmpsb\
                        \n mov rdi, false\
                        \n setge dil"
                    }
                    Type::Str => {
                        " call str_array_cmp\
                        \n cmp rax, EQUAL\
                        \n mov rdi, false\
                        \n setge dil"
                    }
                    Type::Array { .. } => {
                        unreachable!("nested arrays not supported yet")
                    }
                    Type::Infer => {
                        unreachable!("should have been coerced to a concrete type")
                    }
                };

                (
                    Dst::View { len: Rdi, ptr: Rsi },
                    Dst::View { len: Rdx, ptr: Rcx },
                    eq_fn.into(),
                )
            }
            Op::Less => {
                let eq_fn = match elements_type {
                    Type::Int => {
                        " mov rdi, rcx\
                        \n mov rcx, rdx\
                        \n repe cmpsq\
                        \n mov rdi, false\
                        \n setl dil"
                    }
                    Type::Ascii | Type::Bool => {
                        " mov rdi, rcx\
                        \n mov rcx, rdx\
                        \n repe cmpsb\
                        \n mov rdi, false\
                        \n setl dil"
                    }
                    Type::Str => {
                        " call str_array_cmp\
                        \n cmp rax, EQUAL\
                        \n mov rdi, false\
                        \n setl dil"
                    }
                    Type::Array { .. } => {
                        unreachable!("nested arrays not supported yet")
                    }
                    Type::Infer => {
                        unreachable!("should have been coerced to a concrete type")
                    }
                };

                (
                    Dst::View { len: Rdi, ptr: Rsi },
                    Dst::View { len: Rdx, ptr: Rcx },
                    eq_fn.into(),
                )
            }
            Op::LessOrEquals => {
                let eq_fn = match elements_type {
                    Type::Int => {
                        " mov rdi, rcx\
                        \n mov rcx, rdx\
                        \n repe cmpsq\
                        \n mov rdi, false\
                        \n setle dil"
                    }
                    Type::Ascii | Type::Bool => {
                        " mov rdi, rcx\
                        \n mov rcx, rdx\
                        \n repe cmpsb\
                        \n mov rdi, false\
                        \n setle dil"
                    }
                    Type::Str => {
                        " call str_array_cmp\
                        \n cmp rax, EQUAL\
                        \n mov rdi, false\
                        \n setle dil"
                    }
                    Type::Array { .. } => {
                        unreachable!("nested arrays not supported yet")
                    }
                    Type::Infer => {
                        unreachable!("should have been coerced to a concrete type")
                    }
                };

                (
                    Dst::View { len: Rdi, ptr: Rsi },
                    Dst::View { len: Rdx, ptr: Rcx },
                    eq_fn.into(),
                )
            }
            Op::Compare => {
                let eq_fn = match elements_type {
                    Type::Int => {
                        " mov rdi, rcx\
                        \n mov rcx, rdx\
                        \n repe cmpsq\
                        \n mov rdi, LESS\
                        \n mov rsi, EQUAL\
                        \n cmove rdi, rsi\
                        \n mov rsi, GREATER\
                        \n cmovg rdi, rsi"
                    }
                    Type::Ascii | Type::Bool => {
                        " mov rdi, rcx\
                        \n mov rcx, rdx\
                        \n repe cmpsb\
                        \n mov rdi, LESS\
                        \n mov rsi, EQUAL\
                        \n cmove rdi, rsi\
                        \n mov rsi, GREATER\
                        \n cmovg rdi, rsi"
                    }
                    Type::Str => {
                        " call str_array_cmp\
                        \n mov rdi, rax"
                    }
                    Type::Array { .. } => {
                        unreachable!("nested arrays not supported yet")
                    }
                    Type::Infer => {
                        unreachable!("should have been coerced to a concrete type")
                    }
                };

                (
                    Dst::View { len: Rdi, ptr: Rsi },
                    Dst::View { len: Rdx, ptr: Rcx },
                    eq_fn.into(),
                )
            }
            Op::Pow
            | Op::WrappingPow
            | Op::SaturatingPow
            | Op::PowEquals
            | Op::WrappingPowEquals
            | Op::SaturatingPowEquals
            | Op::Times
            | Op::WrappingTimes
            | Op::SaturatingTimes
            | Op::TimesEquals
            | Op::WrappingTimesEquals
            | Op::SaturatingTimesEquals
            | Op::Divide
            | Op::WrappingDivide
            | Op::SaturatingDivide
            | Op::DivideEquals
            | Op::WrappingDivideEquals
            | Op::SaturatingDivideEquals
            | Op::Remainder
            | Op::RemainderEquals
            | Op::Plus
            | Op::WrappingPlus
            | Op::SaturatingPlus
            | Op::PlusEquals
            | Op::WrappingPlusEquals
            | Op::SaturatingPlusEquals
            | Op::Minus
            | Op::WrappingMinus
            | Op::SaturatingMinus
            | Op::MinusEquals
            | Op::WrappingMinusEquals
            | Op::SaturatingMinusEquals
            | Op::And
            | Op::AndEquals
            | Op::BitAnd
            | Op::BitAndEquals
            | Op::Or
            | Op::OrEquals
            | Op::BitOr
            | Op::BitOrEquals
            | Op::BitXor
            | Op::BitXorEquals
            | Op::LeftShift
            | Op::WrappingLeftShift
            | Op::SaturatingLeftShift
            | Op::LeftShiftEquals
            | Op::WrappingLeftShiftEquals
            | Op::SaturatingLeftShiftEquals
            | Op::RightShift
            | Op::RightShiftEquals
            | Op::LeftRotate
            | Op::LeftRotateEquals
            | Op::RightRotate
            | Op::RightRotateEquals => {
                unreachable!("math operation not allowed on arrays")
            }
            Op::Equals => unreachable!("should not be present in the ast"),
            Op::Not | Op::Len => unreachable!("should only appear in unary expressions"),
        }
    }

    // IDEA(stefano): limit shift/rotation rhs to an 8bit integer, and different strategies to deal
    // whit rhs over 6bits:
    // - check for an rhs bigger than 8 bits and crash (current)
    // - silently discard the missing bits
    // - create dedicate operators that implement those strategies
    #[allow(clippy::single_call_fn)]
    fn binary_int_like_int_line_dst_and_asm(op_position: Position, op: Op) -> (Dst, Dst, Cow<'static, str>) {
        return match op {
            Op::Pow | Op::PowEquals => (
                Dst::Reg(Rdi),
                Dst::Reg(Rsi),
                format!(
                    " mov rdx, {line}\
                    \n mov rcx, {col}\
                    \n call int_safe_pow\
                    \n mov rdi, rax",
                    line = op_position.line,
                    col = op_position.col
                )
                .into(),
            ),
            Op::WrappingPow | Op::WrappingPowEquals => (
                Dst::Reg(Rdi),
                Dst::Reg(Rsi),
                format!(
                    " mov rdx, {line}\
                    \n mov rcx, {col}\
                    \n call int_wrapping_pow\
                    \n mov rdi, rax",
                    line = op_position.line,
                    col = op_position.col
                )
                .into(),
            ),
            Op::SaturatingPow | Op::SaturatingPowEquals => (
                Dst::Reg(Rdi),
                Dst::Reg(Rsi),
                format!(
                    " mov rdx, {line}\
                    \n mov rcx, {col}\
                    \n call int_saturating_pow\
                    \n mov rdi, rax",
                    line = op_position.line,
                    col = op_position.col
                )
                .into(),
            ),
            Op::Times | Op::TimesEquals => (
                Dst::Reg(Rdi),
                Dst::Reg(Rsi),
                format!(
                    " mov rdx, {line}\
                    \n mov rcx, {col}\
                    \n call int_safe_mul",
                    line = op_position.line,
                    col = op_position.col
                )
                .into(),
            ),
            Op::WrappingTimes | Op::WrappingTimesEquals => {
                (Dst::Reg(Rdi), Dst::Reg(Rsi), " imul rdi, rsi".into())
            }
            Op::SaturatingTimes | Op::SaturatingTimesEquals => {
                (Dst::Reg(Rdi), Dst::Reg(Rsi), " call int_saturating_mul".into())
            }
            Op::Divide | Op::DivideEquals => (
                Dst::Reg(Rdi),
                Dst::Reg(Rsi),
                format!(
                    " mov rdx, {line}\
                    \n mov rcx, {col}\
                    \n call int_safe_div",
                    line = op_position.line,
                    col = op_position.col
                )
                .into(),
            ),
            Op::WrappingDivide | Op::WrappingDivideEquals => (
                Dst::Reg(Rdi),
                Dst::Reg(Rsi),
                format!(
                    " mov rdx, {line}\
                    \n mov rcx, {col}\
                    \n call int_wrapping_div",
                    line = op_position.line,
                    col = op_position.col
                )
                .into(),
            ),
            Op::SaturatingDivide | Op::SaturatingDivideEquals => (
                Dst::Reg(Rdi),
                Dst::Reg(Rsi),
                format!(
                    " mov rdx, {line}\
                    \n mov rcx, {col}\
                    \n call int_saturating_div",
                    line = op_position.line,
                    col = op_position.col
                )
                .into(),
            ),
            Op::Remainder | Op::RemainderEquals => (
                Dst::Reg(Rdi),
                Dst::Reg(Rsi),
                format!(
                    " mov rdx, {line}\
                    \n mov rcx, {col}\
                    \n call int_safe_remainder",
                    line = op_position.line,
                    col = op_position.col
                )
                .into(),
            ),
            Op::Plus | Op::PlusEquals => (
                Dst::Reg(Rdi),
                Dst::Reg(Rsi),
                format!(
                    " mov rdx, {line}\
                    \n mov rcx, {col}\
                    \n call int_safe_add",
                    line = op_position.line,
                    col = op_position.col,
                )
                .into(),
            ),
            Op::WrappingPlus | Op::WrappingPlusEquals => {
                (Dst::Reg(Rdi), Dst::Reg(Rsi), " add rdi, rsi".into())
            }
            Op::SaturatingPlus | Op::SaturatingPlusEquals => {
                (Dst::Reg(Rdi), Dst::Reg(Rsi), " call int_saturating_add".into())
            }
            Op::Minus | Op::MinusEquals => (
                Dst::Reg(Rdi),
                Dst::Reg(Rsi),
                format!(
                    " mov rdx, {line}\
                    \n mov rcx, {col}\
                    \n call int_safe_sub",
                    line = op_position.line,
                    col = op_position.col,
                )
                .into(),
            ),
            Op::WrappingMinus | Op::WrappingMinusEquals => {
                (Dst::Reg(Rdi), Dst::Reg(Rsi), " sub rdi, rsi".into())
            }
            Op::SaturatingMinus | Op::SaturatingMinusEquals => {
                (Dst::Reg(Rdi), Dst::Reg(Rsi), " call int_saturating_sub".into())
            }
            Op::EqualsEquals => (
                Dst::Reg(Rdi),
                Dst::Reg(Rsi),
                " cmp rdi, rsi\
                \n mov rdi, false\
                \n sete dil"
                    .into(),
            ),
            Op::NotEquals => (
                Dst::Reg(Rdi),
                Dst::Reg(Rsi),
                " cmp rdi, rsi\
                \n mov rdi, false\
                \n setne dil"
                    .into(),
            ),
            Op::Greater => (
                Dst::Reg(Rdi),
                Dst::Reg(Rsi),
                " cmp rdi, rsi\
                \n mov rdi, false\
                \n setg dil"
                    .into(),
            ),
            Op::GreaterOrEquals => (
                Dst::Reg(Rdi),
                Dst::Reg(Rsi),
                " cmp rdi, rsi\
                \n mov rdi, false\
                \n setge dil"
                    .into(),
            ),
            Op::Less => (
                Dst::Reg(Rdi),
                Dst::Reg(Rsi),
                " cmp rdi, rsi\
                \n mov rdi, false\
                \n setl dil"
                    .into(),
            ),
            Op::LessOrEquals => (
                Dst::Reg(Rdi),
                Dst::Reg(Rsi),
                " cmp rdi, rsi\
                \n mov rdi, false\
                \n setle dil"
                    .into(),
            ),
            Op::Compare => (
                Dst::Reg(Rdi),
                Dst::Reg(Rsi),
                " cmp rdi, rsi\
                \n mov rdi, LESS\
                \n mov rsi, EQUAL\
                \n cmove rdi, rsi\
                \n mov rsi, GREATER\
                \n cmovg rdi, rsi"
                    .into(),
            ),
            Op::And | Op::AndEquals | Op::BitAnd | Op::BitAndEquals => {
                (Dst::Reg(Rdi), Dst::Reg(Rsi), " and rdi, rsi".into())
            }
            Op::Or | Op::OrEquals | Op::BitOr | Op::BitOrEquals => {
                (Dst::Reg(Rdi), Dst::Reg(Rsi), " or rdi, rsi".into())
            }
            Op::BitXor | Op::BitXorEquals => {
                (Dst::Reg(Rdi), Dst::Reg(Rsi), " xor rdi, rsi".into())
            }
            Op::LeftShift | Op::LeftShiftEquals => (
                Dst::Reg(Rdi),
                Dst::Reg(Rsi),
                format!(
                    " mov rdx, {line}\
                    \n mov rcx, {col}\
                    \n call int_safe_left_shift",
                    line = op_position.line,
                    col = op_position.col,
                )
                .into(),
            ),
            Op::WrappingLeftShift | Op::WrappingLeftShiftEquals => (
                Dst::Reg(Rdi),
                Dst::Reg(Rsi),
                format!(
                    " mov rdx, {line}\
                    \n mov rcx, {col}\
                    \n call int_wrapping_left_shift",
                    line = op_position.line,
                    col = op_position.col,
                )
                .into(),
            ),
            Op::SaturatingLeftShift | Op::SaturatingLeftShiftEquals => (
                Dst::Reg(Rdi),
                Dst::Reg(Rsi),
                format!(
                    " mov rdx, {line}\
                    \n mov rcx, {col}\
                    \n call int_saturating_left_shift",
                    line = op_position.line,
                    col = op_position.col,
                )
                .into(),
            ),
            Op::RightShift | Op::RightShiftEquals => (
                Dst::Reg(Rdi),
                Dst::Reg(Rsi),
                format!(
                    " mov rdx, {line}\
                    \n mov rcx, {col}\
                    \n call int_safe_right_shift",
                    line = op_position.line,
                    col = op_position.col,
                )
                .into(),
            ),
            Op::LeftRotate | Op::LeftRotateEquals => (
                Dst::Reg(Rdi),
                Dst::Reg(Rsi),
                format!(
                    " mov rdx, {line}\
                    \n mov rcx, {col}\
                    \n call int_safe_left_rotate",
                    line = op_position.line,
                    col = op_position.col,
                )
                .into(),
            ),
            Op::RightRotate | Op::RightRotateEquals => (
                Dst::Reg(Rdi),
                Dst::Reg(Rsi),
                format!(
                    " mov rdx, {line}\
                    \n mov rcx, {col}\
                    \n call int_safe_right_rotate",
                    line = op_position.line,
                    col = op_position.col,
                )
                .into(),
            ),
            Op::Equals => unreachable!("should not be present in the ast"),
            Op::Not | Op::Len => unreachable!("should only appear in unary expressions"),
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

        match rhs {
            // these expressions need to save the value of the lhs
            Expression::Binary { .. }
            | Expression::Unary { op: Op::Len | Op::Minus | Op::Plus | Op::WrappingPlus, .. }
            | Expression::ArrayIndex { .. } => {}

            // these expressions do not need to save the value of the lhs
            Expression::Unary { .. } | Expression::Literal(_) | Expression::Identifier { .. } => {
                self.expression(rhs, rhs_dst);
                return;
            }
            Expression::Array { .. } => unreachable!("arrays cannot appear in expressions"),
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
            // NOTE(stefano): hard-coding the first and second operand until a better way to manage
            // dst and src are developed
            Expression::Binary { lhs, op_position, op, rhs } => {
                let (lhs_dst, rhs_dst, op_asm) = match (lhs.typ(), rhs.typ()) {
                    (Type::Str, Type::Str) => Self::binary_str_str_dst_and_asm(*op),
                    // Note: we can only compare non-empty arrays of the same type and length, so
                    // its safe to only match on the first array type and not to check for empty arrays
                    (Type::Array { typ, .. }, Type::Array { .. }) => Self::binary_array_array_dst_and_asm(&typ, *op),
                    (Type::Int | Type::Bool | Type::Ascii, Type::Int | Type::Bool | Type::Ascii) => {
                        Self::binary_int_like_int_line_dst_and_asm(*op_position, *op)
                    }
                    (Type::Str, _) | (_, Type::Str) => {
                        unreachable!("cannot compare strings and non strings")
                    }
                    (Type::Array { .. }, _) | (_, Type::Array { .. }) => {
                        unreachable!("cannot compare arrays and non arrays")
                    }
                    (Type::Infer, _) | (_, Type::Infer) => {
                        unreachable!("should have been coerced to a concrete type")
                    }
                };

                self.binary_expression(lhs, rhs, lhs_dst, rhs_dst);

                _ = writeln!(self.asm, "{op_asm}\n");
            }
            Expression::Identifier { typ, name } => {
                let var = self.resolve(name);
                let var_offset = var.offset;
                match typ {
                    Type::Int => match dst {
                        Dst::Reg(reg) => _ = writeln!(self.asm, " mov {reg}, [rbp + {var_offset}]"),
                        Dst::View { .. } => unreachable!(),
                    },
                    Type::Ascii | Type::Bool => match dst {
                        Dst::Reg(reg) => {
                            _ = writeln!(self.asm, " movzx {reg}, byte [rbp + {var_offset}]");
                        }
                        Dst::View { .. } => unreachable!(),
                    },
                    Type::Str => match dst {
                        Dst::View { len, ptr } => {
                            _ = writeln!(
                                self.asm,
                                " mov {len}, [rbp + {var_offset}]\
                                \n mov {ptr}, [rbp + {var_offset} + {ptr_offset}]",
                                ptr_offset = Type::Int.size()
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
                    Type::Infer => unreachable!("should have been inferred"),
                }
            }
            Expression::Unary { op_position, op, operand } => {
                let Dst::Reg(reg) = dst else {
                    unreachable!();
                };

                match op {
                    Op::Len => match &**operand {
                        Expression::Literal(Literal::Str(string)) => {
                            let index = self.string_label_index(string);
                            _ = writeln!(self.asm, " mov {reg}, str_{index}_len");
                        }
                        Expression::Identifier { typ, name } => {
                            let var = self.resolve(name);
                            let var_offset = var.offset;
                            match typ {
                                Type::Str => _ = writeln!(self.asm, " mov {reg}, [rbp + {var_offset}]"),
                                Type::Array { len, .. } => _ = writeln!(self.asm, " mov {reg}, {len}"),
                                Type::Int | Type::Bool | Type::Ascii => unreachable!("cannot take the length of numerical types"),
                                Type::Infer => unreachable!("should have been inferred"),
                            }
                        }
                        Expression::Array { items, .. } => _ = writeln!(self.asm, " mov {reg}, {}", items.len()),
                        Expression::ArrayIndex { typ, var_name, bracket_position, index } => {
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
                        | Expression::Binary { .. } => unreachable!("cannot take the length of numerical types"),
                    }
                    Op::Not => {
                        self.expression(operand, dst);
                        match operand.typ() {
                            Type::Bool => _ = writeln!(self.asm, " xor {reg}, 1"),
                            Type::Int | Type::Ascii => _ = writeln!(self.asm, " not {reg}"),
                            Type::Array { .. } => unreachable!("cannot invert array values"),
                            Type::Str => unreachable!("cannot invert string values"),
                            Type::Infer => unreachable!("should have been inferred"),
                        }
                    }
                    Op::Minus => {
                        self.expression(operand, dst);
                        match operand.typ() {
                            Type::Int | Type::Ascii => {
                                _ = writeln!(
                                    self.asm,
                                    " mov rdx, {line}\
                                    \n mov rcx, {col}\
                                    \n call int_safe_negate",
                                    line = op_position.line,
                                    col = op_position.col
                                );
                            }
                            Type::Bool => unreachable!("cannot negate boolean values"),
                            Type::Array { .. } => unreachable!("cannot negate array values"),
                            Type::Str => unreachable!("cannot negate string values"),
                            Type::Infer => unreachable!("should have been inferred"),
                        }
                    }
                    Op::WrappingMinus => {
                        self.expression(operand, dst);
                        match operand.typ() {
                            Type::Int | Type::Ascii => _ = writeln!(self.asm, " neg {reg}"),
                            Type::Bool => unreachable!("cannot negate boolean values"),
                            Type::Array { .. } => unreachable!("cannot negate array values"),
                            Type::Str => unreachable!("cannot negate string values"),
                            Type::Infer => unreachable!("should have been inferred"),
                        }
                    }
                    Op::SaturatingMinus => {
                        self.expression(operand, dst);
                        match operand.typ() {
                            Type::Int | Type::Ascii => {
                                _ = writeln!(
                                    self.asm,
                                    " mov rdx, {line}\
                                    \n mov rcx, {col}\
                                    \n call int_saturating_negate",
                                    line = op_position.line,
                                    col = op_position.col
                                );
                            }
                            Type::Bool => unreachable!("cannot negate boolean values"),
                            Type::Array { .. } => unreachable!("cannot negate array values"),
                            Type::Str => unreachable!("cannot negate string values"),
                            Type::Infer => unreachable!("should have been inferred"),
                        }
                    }
                    Op::Plus => {
                        self.expression(operand, dst);
                        match operand.typ() {
                            Type::Int => {
                                _ = writeln!(
                                    self.asm,
                                    " mov rdx, {line}\
                                    \n mov rcx, {col}\
                                    \n call int_safe_abs",
                                    line = op_position.line,
                                    col = op_position.col
                                );
                            }
                            Type::Ascii => unreachable!("cannot take absolute value of ascii values"),
                            Type::Bool => unreachable!("cannot take absolute value of boolean values"),
                            Type::Array { .. } => {
                                unreachable!("cannot take absolute value of array values")
                            }
                            Type::Str => unreachable!("cannot take absolute value of string values"),
                            Type::Infer => unreachable!("should have been inferred"),
                        }
                    }
                    Op::WrappingPlus => {
                        self.expression(operand, dst);
                        match operand.typ() {
                            Type::Int => _ = writeln!(self.asm, " call int_wrapping_abs"),
                            Type::Ascii => unreachable!("cannot take absolute value of ascii values"),
                            Type::Bool => unreachable!("cannot take absolute value of boolean values"),
                            Type::Array { .. } => {
                                unreachable!("cannot take absolute value of array values")
                            }
                            Type::Str => unreachable!("cannot take absolute value of string values"),
                            Type::Infer => unreachable!("should have been inferred"),
                        }
                    }
                    Op::SaturatingPlus => {
                        self.expression(operand, dst);
                        match operand.typ() {
                            Type::Int => {
                                _ = writeln!(
                                    self.asm,
                                    " mov rdx, {line}\
                                    \n mov rcx, {col}\
                                    \n call int_saturating_abs",
                                    line = op_position.line,
                                    col = op_position.col
                                );
                            }
                            Type::Ascii => unreachable!("cannot take absolute value of ascii values"),
                            Type::Bool => unreachable!("cannot take absolute value of boolean values"),
                            Type::Array { .. } => {
                                unreachable!("cannot take absolute value of array values")
                            }
                            Type::Str => unreachable!("cannot take absolute value of string values"),
                            Type::Infer => unreachable!("should have been inferred"),
                        }
                    }
                    Op::Equals
                    | Op::Pow
                    | Op::WrappingPow
                    | Op::SaturatingPow
                    | Op::PowEquals
                    | Op::WrappingPowEquals
                    | Op::SaturatingPowEquals
                    | Op::Times
                    | Op::WrappingTimes
                    | Op::SaturatingTimes
                    | Op::TimesEquals
                    | Op::WrappingTimesEquals
                    | Op::SaturatingTimesEquals
                    | Op::Divide
                    | Op::WrappingDivide
                    | Op::SaturatingDivide
                    | Op::DivideEquals
                    | Op::WrappingDivideEquals
                    | Op::SaturatingDivideEquals
                    | Op::Remainder
                    | Op::RemainderEquals
                    | Op::PlusEquals
                    | Op::WrappingPlusEquals
                    | Op::SaturatingPlusEquals
                    | Op::MinusEquals
                    | Op::WrappingMinusEquals
                    | Op::SaturatingMinusEquals
                    | Op::LeftShift
                    | Op::WrappingLeftShift
                    | Op::SaturatingLeftShift
                    | Op::LeftShiftEquals
                    | Op::WrappingLeftShiftEquals
                    | Op::SaturatingLeftShiftEquals
                    | Op::RightShift
                    | Op::RightShiftEquals
                    | Op::LeftRotate
                    | Op::LeftRotateEquals
                    | Op::RightRotate
                    | Op::RightRotateEquals
                    | Op::And
                    | Op::AndEquals
                    | Op::BitAnd
                    | Op::BitAndEquals
                    | Op::BitXor
                    | Op::BitXorEquals
                    | Op::Or
                    | Op::OrEquals
                    | Op::BitOr
                    | Op::BitOrEquals
                    | Op::Compare
                    | Op::EqualsEquals
                    | Op::NotEquals
                    | Op::Greater
                    | Op::GreaterOrEquals
                    | Op::Less
                    | Op::LessOrEquals => unreachable!("not a unary operator"),
                }
            }
            Expression::Array { .. } => unreachable!("arrays cannot appear in expressions"),
            Expression::ArrayIndex { typ, var_name, bracket_position, index } => {
                self.expression(index, Dst::Reg(Rdi));

                let line = bracket_position.line;
                let col = bracket_position.col;
                let var = self.resolve(var_name);
                let var_offset = var.offset;
                let var_typ = var.inner.value.typ();

                match var_typ {
                    Type::Str => {
                        _ = writeln!(
                            self.asm,
                            " mov rsi, [rbp + {var_offset}]\
                            \n mov rdx, {line}\
                            \n mov rcx, {col}\
                            \n call assert_str_index_in_range\
                            \n mov rsi, [rbp + {var_offset} + {ptr_offset}]\
                            \n movzx rdi, byte [rsi + rdi]\n",
                            ptr_offset = Type::Int.size()
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
                            Type::Int => {
                                _ = writeln!(
                                    self.asm,
                                    " mov rdi, [rbp + {var_offset} + rdi * 8]\n"
                                );
                            }
                            Type::Ascii | Type::Bool => {
                                _ = writeln!(
                                    self.asm,
                                    " movzx rdi, byte [rbp + {var_offset} + rdi]\n"
                                );
                            }
                            Type::Str => {
                                _ = writeln!(
                                    self.asm,
                                    " imul rdi, {typ_size}\
                                    \n mov rsi, [rbp + {var_offset} + {ptr_offset} + rdi]\
                                    \n mov rdi, [rbp + {var_offset} + rdi]\n",
                                    typ_size = typ.size(),
                                    ptr_offset = Type::Int.size()
                                );
                            }
                            Type::Array { .. } => {
                                unreachable!("arrays cannot appear in expressions")
                            }
                            Type::Infer => {
                                unreachable!("should have been coerced to a concrete type")
                            }
                        }
                    }
                    Type::Int => {
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
                    Type::Bool | Type::Ascii => {
                        unreachable!(
                            "only arrays, strings and integers are allowed in index espressions"
                        )
                    }
                    Type::Infer => unreachable!("should have been inferred"),
                }
            }
        }
    }

    fn condition(&mut self, condition: &'ast Expression<'src>, false_tag: &str) {
        match condition {
            Expression::Literal(Literal::Bool(boolean)) => {
                _ = writeln!(
                    self.asm,
                    " mov dil, {bool}\
                    \n cmp dil, true\
                    \n jne {false_tag}\n",
                    bool = usize::from(*boolean)
                );
            }
            Expression::Literal(Literal::Int(_) | Literal::Ascii(_) | Literal::Str(_)) => {
                unreachable!("non-boolean expressions not allowed in conditions")
            }
            Expression::Binary { lhs, op, rhs, .. } => {
                let lhs_dst = match lhs.typ() {
                    Type::Int | Type::Ascii | Type::Bool => Dst::Reg(Rdi),
                    Type::Str | Type::Array { .. } => Dst::View { len: Rdi, ptr: Rsi },
                    Type::Infer => unreachable!("should have been inferred"),
                };

                let rhs_dst = match rhs.typ() {
                    Type::Int | Type::Ascii | Type::Bool => Dst::Reg(Rsi),
                    Type::Str | Type::Array { .. } => Dst::View { len: Rdx, ptr: Rcx },
                    Type::Infer => unreachable!("should have been inferred"),
                };

                self.binary_expression(lhs, rhs, lhs_dst, rhs_dst);

                match op {
                    Op::EqualsEquals => self.asm += " cmp rdi, rsi\n jne",
                    Op::NotEquals => self.asm += " cmp rdi, rsi\n je",
                    Op::Greater => self.asm += " cmp rdi, rsi\n jle",
                    Op::GreaterOrEquals => self.asm += " cmp rdi, rsi\n jl",
                    Op::Less => self.asm += " cmp rdi, rsi\n jge",
                    Op::LessOrEquals => self.asm += " cmp rdi, rsi\n jg",
                    Op::And | Op::AndEquals => self.asm += " and rdi, rsi\n jz",
                    Op::Or | Op::OrEquals => self.asm += " or rdi, rsi\n jz",

                    Op::Len
                    | Op::Equals
                    | Op::Not
                    | Op::Pow
                    | Op::WrappingPow
                    | Op::SaturatingPow
                    | Op::PowEquals
                    | Op::WrappingPowEquals
                    | Op::SaturatingPowEquals
                    | Op::Times
                    | Op::WrappingTimes
                    | Op::SaturatingTimes
                    | Op::TimesEquals
                    | Op::WrappingTimesEquals
                    | Op::SaturatingTimesEquals
                    | Op::Divide
                    | Op::WrappingDivide
                    | Op::SaturatingDivide
                    | Op::DivideEquals
                    | Op::WrappingDivideEquals
                    | Op::SaturatingDivideEquals
                    | Op::Remainder
                    | Op::RemainderEquals
                    | Op::Plus
                    | Op::WrappingPlus
                    | Op::SaturatingPlus
                    | Op::PlusEquals
                    | Op::WrappingPlusEquals
                    | Op::SaturatingPlusEquals
                    | Op::Minus
                    | Op::WrappingMinus
                    | Op::SaturatingMinus
                    | Op::MinusEquals
                    | Op::WrappingMinusEquals
                    | Op::SaturatingMinusEquals
                    | Op::Compare
                    | Op::BitAnd
                    | Op::BitAndEquals
                    | Op::BitOr
                    | Op::BitOrEquals
                    | Op::BitXor
                    | Op::BitXorEquals
                    | Op::LeftShift
                    | Op::WrappingLeftShift
                    | Op::SaturatingLeftShift
                    | Op::LeftShiftEquals
                    | Op::WrappingLeftShiftEquals
                    | Op::SaturatingLeftShiftEquals
                    | Op::RightShift
                    | Op::RightShiftEquals
                    | Op::LeftRotate
                    | Op::LeftRotateEquals
                    | Op::RightRotate
                    | Op::RightRotateEquals => {
                        unreachable!("non-boolean operators should not appear here")
                    }
                }

                _ = writeln!(self.asm, " {false_tag}\n");
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
            Expression::Unary { operand, .. } => {
                self.expression(operand, Dst::Reg(Rdi));

                // we can only have boolean expressions at this point, so it's safe to ignore the integer negation case
                _ = writeln!(
                    self.asm,
                    " xor dil, 1\
                    \n jz {false_tag}\n"
                );
            }
            Expression::Array { .. } => unreachable!("arrays cannot appear in conditions"),
            Expression::ArrayIndex { .. } => {
                self.expression(condition, Dst::Reg(Rdi));
                _ = writeln!(
                    self.asm,
                    " cmp dil, true\
                    \n jne {false_tag}\n"
                );
            }
        }
    }

    fn condition_reversed(&mut self, condition: &'ast Expression<'src>, true_tag: &str) {
        match condition {
            Expression::Literal(Literal::Bool(boolean)) => {
                _ = writeln!(
                    self.asm,
                    " mov dil, {bool}\
                    \n cmp dil, true\
                    \n je {true_tag}\n",
                    bool = usize::from(*boolean)
                );
            }
            Expression::Literal(Literal::Int(_) | Literal::Ascii(_) | Literal::Str(_)) => {
                unreachable!("non-boolean expressions should not appear here")
            }
            Expression::Binary { lhs, op, rhs, .. } => {
                let lhs_dst = match lhs.typ() {
                    Type::Int | Type::Ascii | Type::Bool => Dst::Reg(Rdi),
                    Type::Str | Type::Array { .. } => Dst::View { len: Rdi, ptr: Rsi },
                    Type::Infer => unreachable!("should have been inferred"),
                };

                let rhs_dst = match rhs.typ() {
                    Type::Int | Type::Ascii | Type::Bool => Dst::Reg(Rsi),
                    Type::Str | Type::Array { .. } => Dst::View { len: Rdx, ptr: Rcx },
                    Type::Infer => unreachable!("should have been inferred"),
                };

                self.binary_expression(lhs, rhs, lhs_dst, rhs_dst);

                match op {
                    Op::EqualsEquals => self.asm += " cmp rdi, rsi\n je",
                    Op::NotEquals => self.asm += " cmp rdi, rsi\n jne",
                    Op::Greater => self.asm += " cmp rdi, rsi\n jg",
                    Op::GreaterOrEquals => self.asm += " cmp rdi, rsi\n jge",
                    Op::Less => self.asm += " cmp rdi, rsi\n jl",
                    Op::LessOrEquals => self.asm += " cmp rdi, rsi\n jle",
                    Op::And | Op::AndEquals => self.asm += " and rdi, rsi\n jnz",
                    Op::Or | Op::OrEquals => self.asm += " or rdi, rsi\n jnz",

                    Op::Len
                    | Op::Equals
                    | Op::Not
                    | Op::Pow
                    | Op::WrappingPow
                    | Op::SaturatingPow
                    | Op::PowEquals
                    | Op::WrappingPowEquals
                    | Op::SaturatingPowEquals
                    | Op::Times
                    | Op::WrappingTimes
                    | Op::SaturatingTimes
                    | Op::TimesEquals
                    | Op::WrappingTimesEquals
                    | Op::SaturatingTimesEquals
                    | Op::Divide
                    | Op::WrappingDivide
                    | Op::SaturatingDivide
                    | Op::DivideEquals
                    | Op::WrappingDivideEquals
                    | Op::SaturatingDivideEquals
                    | Op::Remainder
                    | Op::RemainderEquals
                    | Op::Plus
                    | Op::WrappingPlus
                    | Op::SaturatingPlus
                    | Op::PlusEquals
                    | Op::WrappingPlusEquals
                    | Op::SaturatingPlusEquals
                    | Op::Minus
                    | Op::WrappingMinus
                    | Op::SaturatingMinus
                    | Op::MinusEquals
                    | Op::WrappingMinusEquals
                    | Op::SaturatingMinusEquals
                    | Op::Compare
                    | Op::BitAnd
                    | Op::BitAndEquals
                    | Op::BitOr
                    | Op::BitOrEquals
                    | Op::BitXor
                    | Op::BitXorEquals
                    | Op::LeftShift
                    | Op::WrappingLeftShift
                    | Op::SaturatingLeftShift
                    | Op::LeftShiftEquals
                    | Op::WrappingLeftShiftEquals
                    | Op::SaturatingLeftShiftEquals
                    | Op::RightShift
                    | Op::RightShiftEquals
                    | Op::LeftRotate
                    | Op::LeftRotateEquals
                    | Op::RightRotate
                    | Op::RightRotateEquals => {
                        unreachable!("non-boolean operators should not appear here")
                    }
                }

                _ = writeln!(self.asm, " {true_tag}\n");
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
            Expression::Unary { operand, .. } => {
                self.expression(operand, Dst::Reg(Rdi));

                // we can only have boolean expressions at this point, so it's safe to ignore the integer negation case
                _ = writeln!(
                    self.asm,
                    " xor dil, 1\
                    \n jnz {true_tag}\n"
                );
            }
            Expression::Array { .. } => unreachable!("arrays cannot appear in conditions"),
            Expression::ArrayIndex { .. } => {
                self.expression(condition, Dst::Reg(Rdi));
                _ = writeln!(
                    self.asm,
                    " cmp dil, true\
                    \n je {true_tag}\n"
                );
            }
        }
    }

    fn assignment(&mut self, value: &'ast Expression<'src>, dst_offset: usize) {
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
                        ptr_offset = Type::Int.size()
                    );
                }
            },
            Expression::Binary { .. } => {
                self.expression(value, Dst::Reg(Rdi));

                match value.typ() {
                    Type::Int => _ = writeln!(self.asm, " mov [rbp + {dst_offset}], rdi\n"),
                    Type::Ascii | Type::Bool => {
                        _ = writeln!(self.asm, " mov [rbp + {dst_offset}], dil\n");
                    }
                    Type::Array { .. } => unreachable!("arrays cannot appear in expressions"),
                    Type::Str => unreachable!("strings cannot appear in expressions"),
                    Type::Infer => unreachable!("should have been inferred"),
                }
            }
            Expression::Identifier { typ: identifier_typ, name } => {
                let var = self.resolve(name);
                let src_offset = var.offset;
                match identifier_typ {
                    Type::Int => {
                        _ = writeln!(
                            self.asm,
                            " mov rdi, [rbp + {src_offset}]\
                            \n mov [rbp + {dst_offset}], rdi\n"
                        );
                    }
                    Type::Ascii | Type::Bool => {
                        _ = writeln!(
                            self.asm,
                            " movzx rdi, byte [rbp + {src_offset}]\
                            \n mov [rbp + {dst_offset}], dil\n"
                        );
                    }
                    Type::Str => {
                        _ = writeln!(
                            self.asm,
                            " mov rdi, [rbp + {src_offset}]\
                            \n mov rsi, [rbp + {src_offset} + {ptr_offset}]\
                            \n mov [rbp + {dst_offset}], rdi\
                            \n mov [rbp + {dst_offset} + {ptr_offset}], rsi\n",
                            ptr_offset = Type::Int.size()
                        );
                    }
                    Type::Array { typ: array_typ, len } => match &**array_typ {
                        Type::Int => {
                            _ = writeln!(
                                self.asm,
                                " lea rdi, [rbp + {dst_offset}]\
                                \n lea rsi, [rbp + {src_offset}]\
                                \n mov rcx, {len}\
                                \n rep movsq\n"
                            );
                        }
                        Type::Ascii | Type::Bool => {
                            _ = writeln!(
                                self.asm,
                                " lea rdi, [rbp + {dst_offset}]\
                                \n lea rsi, [rbp + {src_offset}]\
                                \n mov rcx, {len}\
                                \n rep movsb\n"
                            );
                        }
                        Type::Str => {
                            _ = writeln!(
                                self.asm,
                                " lea rdi, [rbp + {dst_offset}]\
                                \n lea rsi, [rbp + {src_offset}]\
                                \n mov rcx, {len} * 2\
                                \n rep movsq\n"
                            );
                        }
                        Type::Array { .. } => unreachable!("nested arrays not supported yet)"),
                        Type::Infer => unreachable!("should have been inferred"),
                    },
                    Type::Infer => unreachable!("should have been inferred"),
                }
            }
            Expression::Unary { op_position, op, operand } => match op {
                Op::Len => match &**operand {
                    Expression::Literal(Literal::Str(string)) => {
                        let index = self.string_label_index(string);
                        _ = writeln!(self.asm, " mov [rbp + {dst_offset}], str_{index}_len\n");
                    }
                    Expression::Identifier { typ, name } => {
                        let var = self.resolve(name);
                        let var_offset = var.offset;
                        match typ {
                            Type::Str => {
                                _ = writeln!(
                                    self.asm,
                                    " mov rdi, [rbp + {var_offset}]\
                                    \n mov [rbp + {dst_offset}], rdi\n"
                                );
                            },
                            Type::Array { len, .. } => _ = writeln!(self.asm, " mov [rbp + {dst_offset}], {len}\n"),
                            Type::Int | Type::Bool | Type::Ascii => unreachable!("cannot take the length of numerical types"),
                            Type::Infer => unreachable!("should have been inferred"),
                        }
                    }
                    Expression::Array { items, .. } => _ = writeln!(self.asm, " mov [rbp + {dst_offset}], {}\n", items.len()),
                    Expression::ArrayIndex { typ, var_name, bracket_position, index } => {
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
                    | Expression::Binary { .. } => unreachable!("cannot take the length of numerical types"),
                }
                Op::Not => {
                    self.expression(operand, Dst::Reg(Rdi));
                    match operand.typ() {
                        Type::Bool => {
                            _ = writeln!(
                                self.asm,
                                " xor rdi, 1\
                                \n mov [rbp + {dst_offset}], dil\n"
                            );
                        }
                        Type::Ascii => {
                            _ = writeln!(
                                self.asm,
                                " not rdi\
                                \n mov [rbp + {dst_offset}], dil\n"
                            );
                        }
                        Type::Int => {
                            _ = writeln!(
                                self.asm,
                                " not rdi\
                                \n mov [rbp + {dst_offset}], rdi\n"
                            );
                        }
                        Type::Array { .. } => unreachable!("cannot invert array values"),
                        Type::Str => unreachable!("cannot invert string values"),
                        Type::Infer => unreachable!("should have been inferred"),
                    }
                }
                Op::Minus => {
                    self.expression(operand, Dst::Reg(Rdi));
                    match operand.typ() {
                        Type::Ascii => {
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
                        Type::Int => {
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
                        Type::Bool => unreachable!("cannot negate boolean values"),
                        Type::Array { .. } => unreachable!("cannot negate array values"),
                        Type::Str => unreachable!("cannot negate string values"),
                        Type::Infer => unreachable!("should have been inferred"),
                    }
                }
                Op::WrappingMinus => {
                    self.expression(operand, Dst::Reg(Rdi));
                    match operand.typ() {
                        Type::Ascii => {
                            _ = writeln!(
                                self.asm,
                                " neg rdi\
                                \n mov [rbp + {dst_offset}], dil\n"
                            );
                        }
                        Type::Int => {
                            _ = writeln!(
                                self.asm,
                                " neg rdi\
                                \n mov [rbp + {dst_offset}], rdi\n"
                            );
                        }
                        Type::Bool => unreachable!("cannot negate boolean values"),
                        Type::Array { .. } => unreachable!("cannot negate array values"),
                        Type::Str => unreachable!("cannot negate string values"),
                        Type::Infer => unreachable!("should have been inferred"),
                    }
                }
                Op::SaturatingMinus => {
                    self.expression(operand, Dst::Reg(Rdi));
                    match operand.typ() {
                        Type::Ascii => {
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
                        Type::Int => {
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
                        Type::Bool => unreachable!("cannot negate boolean values"),
                        Type::Array { .. } => unreachable!("cannot negate array values"),
                        Type::Str => unreachable!("cannot negate string values"),
                        Type::Infer => unreachable!("should have been inferred"),
                    }
                }
                Op::Plus => {
                    self.expression(operand, Dst::Reg(Rdi));
                    match operand.typ() {
                        Type::Ascii => {
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_safe_abs\
                                \n mov [rbp + {dst_offset}], dil\n",
                                line = op_position.line,
                                col = op_position.col
                            );
                        }
                        Type::Int => {
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
                        Type::Bool => unreachable!("cannot take absolute value of boolean values"),
                        Type::Array { .. } => {
                            unreachable!("cannot take absolute value of array values")
                        }
                        Type::Str => unreachable!("cannot take absolute value of string values"),
                        Type::Infer => unreachable!("should have been inferred"),
                    }
                }
                Op::WrappingPlus => {
                    self.expression(operand, Dst::Reg(Rdi));
                    match operand.typ() {
                        Type::Ascii => {
                            _ = writeln!(
                                self.asm,
                                " call int_wrapping_abs\
                                \n mov [rbp + {dst_offset}], dil\n"
                            );
                        }
                        Type::Int => {
                            _ = writeln!(
                                self.asm,
                                " call int_wrapping_abs\
                                \n mov [rbp + {dst_offset}], rdi\n"
                            );
                        }
                        Type::Bool => unreachable!("cannot take absolute value of boolean values"),
                        Type::Array { .. } => {
                            unreachable!("cannot take absolute value of array values")
                        }
                        Type::Str => unreachable!("cannot take absolute value of string values"),
                        Type::Infer => unreachable!("should have been inferred"),
                    }
                }
                Op::SaturatingPlus => {
                    self.expression(operand, Dst::Reg(Rdi));
                    match operand.typ() {
                        Type::Ascii => {
                            _ = writeln!(
                                self.asm,
                                " mov rdx, {line}\
                                \n mov rcx, {col}\
                                \n call int_saturating_abs\
                                \n mov [rbp + {dst_offset}], dil\n",
                                line = op_position.line,
                                col = op_position.col
                            );
                        }
                        Type::Int => {
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
                        Type::Bool => unreachable!("cannot take absolute value of boolean values"),
                        Type::Array { .. } => {
                            unreachable!("cannot take absolute value of array values")
                        }
                        Type::Str => unreachable!("cannot take absolute value of string values"),
                        Type::Infer => unreachable!("should have been inferred"),
                    }
                }
                Op::Equals
                | Op::Pow
                | Op::WrappingPow
                | Op::SaturatingPow
                | Op::PowEquals
                | Op::WrappingPowEquals
                | Op::SaturatingPowEquals
                | Op::Times
                | Op::WrappingTimes
                | Op::SaturatingTimes
                | Op::TimesEquals
                | Op::WrappingTimesEquals
                | Op::SaturatingTimesEquals
                | Op::Divide
                | Op::WrappingDivide
                | Op::SaturatingDivide
                | Op::DivideEquals
                | Op::WrappingDivideEquals
                | Op::SaturatingDivideEquals
                | Op::Remainder
                | Op::RemainderEquals
                | Op::PlusEquals
                | Op::WrappingPlusEquals
                | Op::SaturatingPlusEquals
                | Op::MinusEquals
                | Op::WrappingMinusEquals
                | Op::SaturatingMinusEquals
                | Op::LeftShift
                | Op::WrappingLeftShift
                | Op::SaturatingLeftShift
                | Op::LeftShiftEquals
                | Op::WrappingLeftShiftEquals
                | Op::SaturatingLeftShiftEquals
                | Op::RightShift
                | Op::RightShiftEquals
                | Op::LeftRotate
                | Op::LeftRotateEquals
                | Op::RightRotate
                | Op::RightRotateEquals
                | Op::And
                | Op::AndEquals
                | Op::BitAnd
                | Op::BitAndEquals
                | Op::BitXor
                | Op::BitXorEquals
                | Op::Or
                | Op::OrEquals
                | Op::BitOr
                | Op::BitOrEquals
                | Op::Compare
                | Op::EqualsEquals
                | Op::NotEquals
                | Op::Greater
                | Op::GreaterOrEquals
                | Op::Less
                | Op::LessOrEquals => unreachable!("not a unary operator"),
            }
            Expression::Array { typ, items } => {
                let typ_size = typ.size();
                for (index, item) in items.iter().enumerate() {
                    self.assignment(item, dst_offset + index * typ_size);
                }
            }
            Expression::ArrayIndex { typ, .. } => {
                self.expression(value, Dst::default(typ));

                match typ {
                    Type::Int => _ = writeln!(self.asm, " mov [rbp + {dst_offset}], rdi\n"),
                    Type::Ascii | Type::Bool => {
                        _ = writeln!(self.asm, " mov [rbp + {dst_offset}], dil\n");
                    }
                    Type::Str => {
                        _ = writeln!(
                            self.asm,
                            " mov [rbp + {dst_offset}], rdi\
                            \n mov [rbp + {dst_offset} + {ptr_offset}], rsi\n",
                            ptr_offset = Type::Int.size()
                        );
                    }
                    Type::Array { .. } => unreachable!("nested arrays are not supported yet"),
                    Type::Infer => unreachable!("should have been inferred"),
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
            Type::Int => self.asm += " call int_print\n\n",
            Type::Ascii => self.asm += " call ascii_print\n\n",
            Type::Bool => self.asm += " call bool_print\n\n",
            Type::Str => self.asm += " call str_print\n\n",
            Type::Array { typ, .. } => match &*typ {
                Type::Int => self.asm += " call int_array_debug_print\n\n",
                Type::Ascii => self.asm += " call ascii_array_debug_print\n\n",
                Type::Bool => self.asm += " call bool_array_debug_print\n\n",
                Type::Str => self.asm += " call str_array_debug_print\n\n",
                Type::Array { .. } => unreachable!("nested arrays are not supported yet"),
                Type::Infer => unreachable!("should have been inferred"),
            },
            Type::Infer => unreachable!("should have been inferred"),
        }
    }

    fn eprint(&mut self, value: &'ast Expression<'src>) {
        let value_typ = value.typ();
        self.expression(value, Dst::default(&value_typ));

        match value_typ {
            Type::Int => self.asm += " call int_eprint\n\n",
            Type::Ascii => self.asm += " call ascii_eprint\n\n",
            Type::Bool => self.asm += " call bool_eprint\n\n",
            Type::Str => self.asm += " call str_eprint\n\n",
            Type::Array { typ, .. } => match &*typ {
                Type::Int => self.asm += " call int_array_debug_eprint\n\n",
                Type::Ascii => self.asm += " call ascii_array_debug_eprint\n\n",
                Type::Bool => self.asm += " call bool_array_debug_eprint\n\n",
                Type::Str => self.asm += " call str_array_debug_eprint\n\n",
                Type::Array { .. } => unreachable!("nested arrays are not supported yet"),
                Type::Infer => unreachable!("should have been inferred"),
            },
            Type::Infer => unreachable!("should have been inferred"),
        }
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    CouldNotCreateFile { path: PathBuf },
    WritingAssemblyFailed,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::CouldNotCreateFile { path } => {
                write!(f, "could not create file '{}'", path.display())
            }
            Self::WritingAssemblyFailed => {
                write!(f, "writing to assembly file failed")
            }
        };
    }
}

#[derive(Debug)]
pub enum ErrorCause {
    IoError(io::Error),
}

impl Display for ErrorCause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::IoError(err) => write!(f, "{err} ({})", err.kind()),
        };
    }
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub cause: ErrorCause,
}

impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(
            f,
            "{ERROR}: {msg}\
            \n{CAUSE}: {cause}",
            msg = self.kind,
            cause = self.cause
        );
    }
}
