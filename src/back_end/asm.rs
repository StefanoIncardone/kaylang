// IDEA(stefano): split into nasm files and include them

pub(crate) static CRASH_ASM: &str = {
    r"; fn ! = crash(msg: str @rdi:rsi, line: u64 @rdx, col: u64 @rcx)
crash:
 mov r8, rdi; msg_len: u64
 mov r9, rsi; msg_ptr: ascii*
 mov r12, rdx; line: u64
 mov r13, rcx; col: u64

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
 call i64_eprint

 mov dil, ':'
 call ascii_eprint

 ; column
 mov rdi, r13
 call i64_eprint

 mov dil, newline
 call ascii_eprint

 mov rdi, EXIT_FAILURE
 mov rax, SYS_exit
 syscall"
};

pub(crate) static ASSERT_ARRAY_INDEX_IN_RANGE_ASM: &str = {
    r"; fn !? = assert_array_index_in_range(index: i64 @rdi, array_len: u64 @rsi, line: u64 @rdx, col: u64 @rcx)
assert_array_index_in_range:
 cmp rdi, 0
 jl .underflow

 cmp rdi, rsi
 jge .overflow

 ret

.underflow:
 mov rdi, attempt_array_index_underflow_len
 mov rsi, attempt_array_index_underflow
 call crash

.overflow:
 mov rdi, attempt_array_index_overflow_len
 mov rsi, attempt_array_index_overflow
 call crash"
};

#[expect(
    dead_code,
    reason = "temporarily disabled, will be reanabled when we introduce the `bits` type"
)]
static ASSERT_I64_BIT_INDEX_IN_RANGE_ASM: &str = {
    r"; fn !? = assert_i64_bit_index_in_range(index: i64 @rdi, bits: u64 @rsi, line: u64 @rdx, col: u64 @rcx)
assert_i64_bit_index_in_range:
 cmp rdi, 0
 jl .underflow

 cmp rdi, rsi
 jge .overflow

 ret

.underflow:
 mov rdi, attempt_i64_bit_index_underflow_len
 mov rsi, attempt_i64_bit_index_underflow
 call crash

.overflow:
 mov rdi, attempt_i64_bit_index_overflow_len
 mov rsi, attempt_i64_bit_index_overflow
 call crash"
};

pub(crate) static ASSERT_STR_INDEX_IN_RANGE_ASM: &str = {
    r"; fn !? = assert_str_index_in_range(index: i64 @rdi, str_len: u64 @rsi, line: u64 @rdx, col: u64 @rcx)
assert_str_index_in_range:
 cmp rdi, 0
 jl .underflow

 cmp rdi, rsi
 jge .overflow

 ret

.underflow:
 mov rdi, attempt_str_index_underflow_len
 mov rsi, attempt_str_index_underflow
 call crash

.overflow:
 mov rdi, attempt_str_index_overflow_len
 mov rsi, attempt_str_index_overflow
 call crash"
};

pub(crate) static I64_TO_STR_ASM: &str = {
    r"; fn str @rax:rdx = i64_to_str(self: i64 @rdi)
i64_to_str:
 mov rsi, 10
 mov rcx, i64_str + I64_BITS - 1

 mov rax, rdi
 cmp rax, 0
 je .write_zero
 jl .make_integer_positive
 jg .next_digit

.write_zero:
 mov byte [rcx], '0'
 jmp .done

.make_integer_positive:
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
 mov rdx, i64_str + I64_BITS
 sub rdx, rcx

 mov rax, rcx
 ret"
};

pub(crate) static I64_SAFE_POW_ASM: &str = {
    r"; op i64 @rdi | !? = base: i64 @rdi ** exponent: u64 @rsi [line: u64 @rdx, col: u64 @rcx]
i64_safe_pow:
 cmp rsi, 0
 jl .exponent_negative
 jg .exponent_positive

 mov rdi, 1
 ret

.exponent_negative:
 mov rdi, attempt_exponent_negative_len
 mov rsi, attempt_exponent_negative
 call crash

.exponent_positive:
 cmp rsi, 1
 jne .exponent_not_one
 ret

.exponent_not_one:
 mov r8, rsi
 mov r9, 1

.next_power:
 cmp r8, 1
 jle .done

 test r8, 1
 jnz .exponent_odd

 ; mov rdi, rdi
 mov rsi, rdi
 call i64_safe_mul_pow

 shr r8, 1
 jmp .next_power

.exponent_odd:
 mov r10, rdi

 ; mov rdi, rdi
 mov rsi, r9
 call i64_safe_mul_pow
 mov r9, rdi

 mov rdi, r10
 mov rsi, rdi
 call i64_safe_mul_pow

 dec r8
 shr r8, 1
 jmp .next_power

.done:
 ; mov rdi, rdi
 mov rsi, r9
 call i64_safe_mul_pow

 ret"
};

pub(crate) static I64_WRAPPING_POW_ASM: &str = {
    r"; op i64 @rdi | !? = base: i64 @rdi **\ exponent: u64 @rsi [line: u64 @rdx, col: u64 @rcx]
i64_wrapping_pow:
 cmp rsi, 0
 jl .exponent_negative
 jg .exponent_positive

 mov rdi, 1
 ret

.exponent_negative:
 mov rdi, attempt_exponent_negative_len
 mov rsi, attempt_exponent_negative
 call crash

.exponent_positive:
 cmp rsi, 1
 jne .exponent_not_one
 ret

.exponent_not_one:
 mov r8, rsi
 mov r9, 1

.next_power:
 cmp r8, 1
 jle .done

 test r8, 1
 jnz .exponent_odd

 imul rdi, rdi
 shr r8, 1
 jmp .next_power

.exponent_odd:
 imul r9, rdi
 imul rdi, rdi

 dec r8
 shr r8, 1
 jmp .next_power

.done:
 imul rdi, r9
 ret"
};

pub(crate) static I64_SATURATING_POW_ASM: &str = {
    r"; op i64 @rdi | !? = base: i64 @rdi **| exponent: u64 @rsi [line: u64 @rdx, col: u64 @rcx]
i64_saturating_pow:
 cmp rsi, 0
 jl .exponent_negative
 jg .exponent_positive

 mov rdi, 1
 ret

.exponent_negative:
 mov rdi, attempt_exponent_negative_len
 mov rsi, attempt_exponent_negative
 call crash

.exponent_positive:
 cmp rsi, 1
 jne .exponent_not_one
 ret

.exponent_not_one:
 mov r8, rsi
 mov r9, 1

.next_power:
 cmp r8, 1
 jle .done

 test r8, 1
 jnz .exponent_odd

 ; mov rdi, rdi
 mov rsi, rdi
 call i64_saturating_mul

 shr r8, 1
 jmp .next_power

.exponent_odd:
 mov r10, rdi

 ; mov rdi, rdi
 mov rsi, r9
 call i64_saturating_mul
 mov r9, rdi

 mov rdi, r10
 mov rsi, rdi
 call i64_saturating_mul

 dec r8
 shr r8, 1
 jmp .next_power

.done:
 ; mov rdi, rdi
 mov rsi, r9
 call i64_saturating_mul

 ret"
};

pub(crate) static I64_SAFE_MUL_POW_ASM: &str = {
    r"; op i64 @rdi | !? = lhs: i64 @rdi * rhs: i64 @rsi [line: u64 @rdx, col: u64 @rcx]
i64_safe_mul_pow:
 imul rdi, rsi
 jno .no_overflow
 mov rdi, pow_overflow_len
 mov rsi, pow_overflow
 call crash

.no_overflow:
 ret"
};

pub(crate) static I64_SAFE_MUL_ASM: &str = {
    r"; op i64 @rdi !? = lhs: i64 @rdi * rhs: i64 @rsi [line: u64 @rdx, col: u64 @rcx]
i64_safe_mul:
 imul rdi, rsi
 jno .no_overflow
 mov rdi, mul_overflow_len
 mov rsi, mul_overflow
 call crash

.no_overflow:
 ret"
};

pub(crate) static I64_SATURATING_MUL_ASM: &str = {
    r"; op i64 @rdi = lhs: i64 @rdi *| rhs: i64 @rsi
i64_saturating_mul:
 mov rax, rdi
 xor rdi, rsi
 shr rdi, 63
 mov rdx, I64_MAX
 add rdi, rdx
 imul rsi
 cmovc rax, rdi
 mov rdi, rax
 ret"
};

pub(crate) static I64_SAFE_DIV_ASM: &str = {
    r"; op i64 @rdi | !? = lhs: i64 @rdi / rhs: i64 @rsi [line: u64 @rdx, col: u64 @rcx]
i64_safe_div:
 test rsi, rsi
 jnz .check_no_i64_min_div_minus_one

 mov rdi, attempt_division_by_zero_len
 mov rsi, attempt_division_by_zero
 call crash

.check_no_i64_min_div_minus_one:
 mov rax, I64_MIN
 cmp rdi, rax
 jne .no_i64_min_div_minus_one
 cmp rsi, -1
 jne .no_i64_min_div_minus_one

 mov rdi, div_overflow_len
 mov rsi, div_overflow
 call crash

.no_i64_min_div_minus_one:
 mov rax, rdi
 cqo
 idiv rsi
 mov rdi, rax
 ret"
};

pub(crate) static I64_WRAPPING_DIV_ASM: &str = {
    r"; op i64 @rdi | !? = lhs: i64 @rdi /\ rhs: i64 @rsi [line: u64 @rdx, col: u64 @rcx]
i64_wrapping_div:
 test rsi, rsi
 jnz .check_no_i64_min_div_minus_one

 mov rdi, attempt_division_by_zero_len
 mov rsi, attempt_division_by_zero
 call crash

.check_no_i64_min_div_minus_one:; I64_MIX @rdi ^ -1 @rsi == I64_MAX @r8
 mov rax, rdi
 xor rax, rsi
 mov r8, I64_MAX
 cmp rax, r8
 jne .no_i64_min_div_minus_one

 ret; return I64_MIN @rdi

.no_i64_min_div_minus_one:
 mov rax, rdi
 cqo
 idiv rsi
 mov rdi, rax
 ret"
};

pub(crate) static I64_SATURATING_DIV_ASM: &str = {
    r"; op i64 @rdi | !? = lhs: i64 @rdi /| rhs: i64 @rsi [line: u64 @rdx, col: u64 @rcx]
i64_saturating_div:
 test rsi, rsi
 jnz .check_no_i64_min_div_minus_one

 mov rdi, attempt_division_by_zero_len
 mov rsi, attempt_division_by_zero
 call crash

.check_no_i64_min_div_minus_one:; I64_MIX @rdi ^ -1 @rsi == I64_MAX @r8
 mov rax, rdi
 xor rax, rsi
 mov r8, I64_MAX
 cmp rax, r8
 jne .no_i64_min_div_minus_one

 mov rdi, r8
 ret; return I64_MAX @r8

.no_i64_min_div_minus_one:
 mov rax, rdi
 cqo
 idiv rsi
 mov rdi, rax
 ret"
};

pub(crate) static I64_SAFE_REMAINDER_ASM: &str = {
    r"; op i64 @rdi | !? = lhs: i64 @rdi % rhs: i64 @rsi [line: u64 @rdx, col: u64 @rcx]
i64_safe_remainder:
 test rsi, rsi
 jnz .check_no_i64_min_div_minus_one

 mov rdi, attempt_remainder_zero_len
 mov rsi, attempt_remainder_zero
 call crash

.check_no_i64_min_div_minus_one:; I64_MIX @rdi ^ -1 @rsi == I64_MAX @r8
 mov rax, rdi
 xor rax, rsi
 mov r8, I64_MAX
 cmp rax, r8
 jne .no_i64_min_div_minus_one

 mov rdi, remainder_overflow_len
 mov rsi, remainder_overflow
 call crash

.no_i64_min_div_minus_one:
 mov rax, rdi
 cqo
 idiv rsi
 mov rdi, rdx
 ret"
};

pub(crate) static I64_SAFE_ADD_ASM: &str = {
    r"; op i64 @rdi | !? = lhs: i64 @rdi + rhs: i64 @rsi [line: u64 @rdx, col: u64 @rcx]
i64_safe_add:
 add rdi, rsi
 jno .no_overflow
 mov rdi, add_overflow_len
 mov rsi, add_overflow
 call crash

.no_overflow:
 ret"
};

pub(crate) static I64_SATURATING_ADD_ASM: &str = {
    r"; op i64 @rdi = lhs: i64 @rdi +| rhs: i64 @rsi
i64_saturating_add:
 add rdi, rsi
 jno .no_overflow
 mov rdi, I64_MAX
 mov rsi, I64_MIN
 cmovc rdi, rsi

.no_overflow:
 ret"
};

pub(crate) static I64_SAFE_ABS_ASM: &str = {
    r"; op u64 @rdi | !? = + lhs: i64 @rdi [_dummy: i64 @rsi, line: u64 @rdx, col: u64 @rcx]
i64_safe_abs:
 mov rsi, rdi
 sar rsi, I64_BITS - 1
 xor rdi, rsi
 sub rdi, rsi
 jno .no_overflow
 mov rdi, abs_overflow_len
 mov rsi, abs_overflow
 call crash

.no_overflow:
 ret"
};

pub(crate) static I64_WRAPPING_ABS_ASM: &str = {
    r"; op u64 @rdi = +\ lhs: i64 @rdi
i64_wrapping_abs:
 mov rsi, rdi
 sar rsi, I64_BITS - 1
 xor rdi, rsi
 sub rdi, rsi
 ret"
};

pub(crate) static I64_SATURATING_ABS_ASM: &str = {
    r"; op u64 @rdi = +| lhs: i64 @rdi
i64_saturating_abs:
 mov rsi, rdi
 sar rsi, I64_BITS - 1
 xor rdi, rsi
 sub rdi, rsi
 jno .no_overflow
 mov rdi, I64_MAX

.no_overflow:
 ret"
};

pub(crate) static I64_SAFE_SUB_ASM: &str = {
    r"; op i64 @rdi | !? = lhs: i64 @rdi - rhs: i64 @rsi [line: u64 @rdx, col: u64 @rcx]
i64_safe_sub:
 sub rdi, rsi
 jno .no_overflow
 mov rdi, sub_overflow_len
 mov rsi, sub_overflow
 call crash

.no_overflow:
 ret"
};

pub(crate) static I64_SATURATING_SUB_ASM: &str = {
    r"; op i64 @rdi = lhs: i64 @rdi -| rhs: i64 @rsi
i64_saturating_sub:
 sub rdi, rsi
 jno .no_overflow
 mov rdi, I64_MIN
 mov rsi, I64_MAX
 cmovc rdi, rsi

.no_overflow:
 ret"
};

pub(crate) static I64_SAFE_NEGATE_ASM: &str = {
    r"; op i64 | !? = - lhs: i64 @rdi [_dummy: i64 @rsi, line: u64 @rdx, col: u64 @rcx]
i64_safe_negate:
 neg rdi
 jno .no_overflow
 mov rdi, negate_overflow_len
 mov rsi, negate_overflow
 call crash

.no_overflow:
 ret"
};

pub(crate) static I64_SATURATING_NEGATE_ASM: &str = {
    r"; op i64 @rdi = -| lhs: i64 @rdi
i64_saturating_negate:
 neg rdi
 jno .no_overflow
 mov rdi, I64_MAX

.no_overflow:
 ret"
};

pub(crate) static I64_SAFE_LEFT_SHIFT_ASM: &str = {
    r"; op i64 @rdi | !? = lhs: i64 @rdi << rhs: i64 @rsi [line: u64 @rdx, col: u64 @rcx]
i64_safe_left_shift:
 cmp rsi, 0
 jge .no_negative_shift

 mov rdi, attempt_left_shift_negative_len
 mov rsi, attempt_left_shift_negative
 call crash

.no_negative_shift:
 cmp rsi, (1 << 6) - 1
 jle .no_shift_over_6_bits

 mov rdi, attempt_left_shift_over_6_bits_len
 mov rsi, attempt_left_shift_over_6_bits
 call crash

.no_shift_over_6_bits:
 shlx rax, rdi, rsi
 sarx rsi, rax, rsi
 cmp rdi, rsi
 je .no_overflow

 mov rdi, left_shift_overflow_len
 mov rsi, left_shift_overflow
 call crash

.no_overflow:
 mov rdi, rax
 ret"
};

pub(crate) static I64_WRAPPING_LEFT_SHIFT_ASM: &str = {
    r"; op i64 @ rdi | !? = lhs: i64 @rdi <<\ rhs: i64 @rsi [line: u64 @rdx, col: u64 @rcx]
i64_wrapping_left_shift:
 cmp rsi, 0
 jge .no_negative_shift

 mov rdi, attempt_left_shift_negative_len
 mov rsi, attempt_left_shift_negative
 call crash

.no_negative_shift:
 cmp rsi, (1 << 6) - 1
 jle .no_shift_over_6_bits

 mov rdi, attempt_left_shift_over_6_bits_len
 mov rsi, attempt_left_shift_over_6_bits
 call crash

.no_shift_over_6_bits:
 shlx rdi, rdi, rsi
 ret"
};

pub(crate) static I64_SATURATING_LEFT_SHIFT_ASM: &str = {
    r"; op i64 @rdi | !? = lhs: i64 @rdi <<| rhs: i64 @rsi [line: u64 @rdx, col: u64 @rcx]
i64_saturating_left_shift:
 cmp rsi, 0
 jge .no_negative_shift

 mov rdi, attempt_left_shift_negative_len
 mov rsi, attempt_left_shift_negative
 call crash

.no_negative_shift:
 cmp rsi, (1 << 6) - 1
 jle .no_shift_over_6_bits

 mov rdi, attempt_left_shift_over_6_bits_len
 mov rsi, attempt_left_shift_over_6_bits
 call crash

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

pub(crate) static I64_SAFE_RIGHT_SHIFT_ASM: &str = {
    r"; op i64 @rdi | !? = lhs: i64 @rdi >> rhs: i64 @rsi [line: u64 @rdx, col: u64 @rcx]
i64_safe_right_shift:
 cmp rsi, 0
 jge .no_negative_shift

 mov rdi, attempt_right_shift_negative_len
 mov rsi, attempt_right_shift_negative
 call crash

.no_negative_shift:
 cmp rsi, (1 << 6) - 1
 jle .no_shift_over_6_bits

 mov rdi, attempt_right_shift_over_6_bits_len
 mov rsi, attempt_right_shift_over_6_bits
 call crash

.no_shift_over_6_bits:
 sarx rdi, rdi, rsi
 ret"
};

pub(crate) static I64_SAFE_LEFT_ROTATE_ASM: &str = {
    r"; op i64 @rdi | !? = lhs: i64 @rdi <<< rhs: i64 @rsi [line: u64 @rdx, col: u64 @rcx]
i64_safe_left_rotate:
 cmp rsi, 0
 jge .no_negative_shift

 mov rdi, attempt_left_rotate_negative_len
 mov rsi, attempt_left_rotate_negative
 call crash

.no_negative_shift:
 cmp rsi, (1 << 6) - 1
 jle .no_shift_over_6_bits

 mov rdi, attempt_left_rotate_over_6_bits_len
 mov rsi, attempt_left_rotate_over_6_bits
 call crash

.no_shift_over_6_bits:
 mov cl, sil
 rol rdi, cl
 ret"
};

pub(crate) static I64_SAFE_RIGHT_ROTATE_ASM: &str = {
    r"; op i64 @rdi | ! = lhs: i64 @rdi >>> rhs: i64 @rsi [line: u64 @rdx, col: u64 @rcx]
i64_safe_right_rotate:
 cmp rsi, 0
 jge .no_negative_shift

 mov rdi, attempt_right_rotate_negative_len
 mov rsi, attempt_right_rotate_negative
 call crash

.no_negative_shift:
 cmp rsi, (1 << 6) - 1
 jle .no_shift_over_6_bits

 mov rdi, attempt_right_rotate_over_6_bits_len
 mov rsi, attempt_right_rotate_over_6_bits
 call crash

.no_shift_over_6_bits:
 mov cl, sil
 ror rdi, cl
 ret"
};

pub(crate) static I64_PRINT_ASM: &str = {
    r"; fn i64_print(self: i64 @rdi)
i64_print:
 call i64_to_str
 mov rdi, stdout
 mov rsi, rax
 mov rax, SYS_write
 syscall
 ret"
};

pub(crate) static I64_EPRINT_ASM: &str = {
    r"; fn i64_eprint(self: i64 @rdi)
i64_eprint:
 call i64_to_str
 mov rdi, stderr
 mov rsi, rax
 mov rax, SYS_write
 syscall
 ret"
};

pub(crate) static I64_ARRAY_DEBUG_PRINT_ASM: &str = {
    r"; fn i64_array_debug_print(self: i64[]& @rdi:rsi)
i64_array_debug_print:
 mov r8, rdi; len: u64
 mov r9, rsi; array_ptr: i64[]*

 mov dil, '['
 call ascii_print

 test r8, r8
 jz .done

.next:
 dec r8
 jz .last

 mov rdi, [r9]
 call i64_print

 mov dil, ','
 call ascii_print

 mov dil, ' '
 call ascii_print

 add r9, 8
 jmp .next

.last:
 mov rdi, [r9]
 call i64_print

.done:
 mov dil, ']'
 call ascii_print

 ret"
};

pub(crate) static I64_ARRAY_DEBUG_EPRINT_ASM: &str = {
    r"; fn i64_array_debug_eprint(self: i64[]& @rdi:rsi)
i64_array_debug_eprint:
 mov r8, rdi; len: u64
 mov r9, rsi; array_ptr: i64[]*

 mov dil, '['
 call ascii_eprint

 test r8, r8
 jz .done

.next:
 dec r8
 jz .last

 mov rdi, [r9]
 call i64_eprint

 mov dil, ','
 call ascii_eprint

 mov dil, ' '
 call ascii_eprint

 add r9, 8
 jmp .next

.last:
 mov rdi, [r9]
 call i64_eprint

.done:
 mov dil, ']'
 call ascii_eprint

 ret"
};

pub(crate) static ASCII_PRINT_ASM: &str = {
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

pub(crate) static ASCII_EPRINT_ASM: &str = {
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

pub(crate) static ASCII_ARRAY_DEBUG_PRINT_ASM: &str = {
    r"; fn ascii_array_debug_print(self: ascii[]& @rdi:rsi)
ascii_array_debug_print:
 mov r8, rdi; len: u64
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

pub(crate) static ASCII_ARRAY_DEBUG_EPRINT_ASM: &str = {
    r"; fn ascii_array_debug_eprint(self: ascii[]& @rdi:rsi)
ascii_array_debug_eprint:
 mov r8, rdi; len: u64
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

pub(crate) static BOOL_PRINT_ASM: &str = {
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

pub(crate) static BOOL_EPRINT_ASM: &str = {
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

pub(crate) static BOOL_ARRAY_DEBUG_PRINT_ASM: &str = {
    r"; fn bool_array_debug_print(self: bool[]& @rdi:rsi)
bool_array_debug_print:
 mov r8, rdi; len: u64
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

pub(crate) static BOOL_ARRAY_DEBUG_EPRINT_ASM: &str = {
    r"; fn bool_array_debug_eprint(self: bool[]& @rdi:rsi)
bool_array_debug_eprint:
 mov r8, rdi; len: u64
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

pub(crate) static STR_EQ_ASM: &str = {
    r"; op bool (@dil, @rflags) = lhs: str @rdi:rsi == rhs: @rdx:rcx
str_eq:
 cmp rdi, rdx
 jne .done

 mov rdi, rcx
 mov rcx, rdx
 repe cmpsb

.done:
 mov rdi, false
 sete dil
 ret"
};

pub(crate) static STR_NEQ_ASM: &str = {
    r"; op bool (@dil, @rflags) = lhs: str @rdi:rsi != other: @rdx:rcx
str_neq:
 cmp rdi, rdx
 jne .done

 mov rdi, rcx
 mov rcx, rdx
 repe cmpsb

.done:
 mov rdi, false
 setne dil
 ret"
};

pub(crate) static STR_CMP_ASM: &str = {
    r"; op i64 @rdi = lhs: str @rdi:rsi <=> rhs: @rdx:rcx
str_cmp:
 mov rax, rdi
 sub rax, rdx
 cmovb rdx, rdi

 mov rdi, rcx
 mov rcx, rdx
 repe cmpsb
 je .eq
 mov rdi, LESS
 mov rsi, GREATER
 cmovg rdi, rsi
 ret

.eq:
 cmp rax, 0
 mov rdi, LESS
 mov rsi, EQUAL
 cmove rdi, rsi
 mov rsi, GREATER
 cmovg rdi, rsi
 ret"
};

pub(crate) static STR_ARRAY_EQ_ASM: &str = {
    r"; op[N: u64 @rdi] bool @dil = lhs: str[N] @rdi:rsi == rhs: str[N] @_rdx:rcx
str_array_eq:
 mov r8, rsi; lhs_ptr: str*
 mov r9, rcx; rhs_ptr: str*
 mov r10, rdi; N: u64

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

pub(crate) static STR_ARRAY_NEQ_ASM: &str = {
    r"; op[N: u64 @rdi] bool @dil = lhs: str[N] @rdi:rsi != rhs: str[N] @_rdx:rcx
str_array_neq:
 mov r8, rsi; lhs_ptr: str*
 mov r9, rcx; rhs_ptr: str*
 mov r10, rdi; N: u64

.next:
 mov rdi, [r8]
 mov rsi, [r8 + 8]
 mov rdx, [r9]
 mov rcx, [r9 + 8]
 call str_neq
 je .neq
 ret

.neq:
 add r8, 16
 add r9, 16
 dec r10
 jnz .next
 ret"
};

pub(crate) static STR_ARRAY_CMP_ASM: &str = {
    r"; op[N: u64 @rdi] i64 @rdi = lhs: str[N] @rdi:rsi <=> rhs: str[N] @_rdx:rcx)
str_array_cmp:
 mov r8, rsi; lhs_ptr: str*
 mov r9, rcx; rhs_ptr: str*
 mov r10, rdi; N: u64

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

pub(crate) static STR_PRINT_ASM: &str = {
    r"; fn str_print(self: str @rdi:rsi)
str_print:
 mov rdx, rdi
 mov rdi, stdout
 mov rax, SYS_write
 syscall
 ret"
};

pub(crate) static STR_EPRINT_ASM: &str = {
    r"; fn str_eprint(self: str @rdi:rsi)
str_eprint:
 mov rdx, rdi
 mov rdi, stderr
 mov rax, SYS_write
 syscall
 ret"
};

pub(crate) static STR_ARRAY_DEBUG_PRINT_ASM: &str = {
    r"; fn str_array_debug_print(self: str[] @rdi:rsi)
str_array_debug_print:
 mov r8, rdi; len: u64
 mov r9, rsi; string: str*

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

pub(crate) static STR_ARRAY_DEBUG_EPRINT_ASM: &str = {
    r"; fn str_array_debug_eprint(self: str[] @rdi:rsi)
str_array_debug_eprint:
 mov r8, rdi; len: u64
 mov r9, rsi; string: str*

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
