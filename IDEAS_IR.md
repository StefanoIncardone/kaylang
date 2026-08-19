# Ideas for the kay ir

## Ir

```kbe
## translated from project euler 0001
alias MAX = 1000;
var sum = 0;
var number = 1;
loop_0: if number <=> MAX case >= break loop_0_break;
    let e0 = number % 3; ## no division by zero checking since 3 is known at compile time
    if e0 <=> 0 case == break or_0_true;
    let e1 = number % 5;
    if e1 <=> 0 case == break or_0_true;

    break or_0_end;
    or_0_true:
        sum += number;
    or_0_end:

    number += 1;
    break loop_0;
loop_0_break:

print "expected: "; println 233168;
print "actual:   "; println sum;

let numbers: i64[4] = [0; 0; 0; 0;];

var i = 0
loop_0: if i <=> numbers.len case >= break loop_0_break;
    let e0 = numbers[i].*; ## no bounds checking
    println e0;
    i += 1; ## no bounds checking
loop_0_break:

## kay
let e0 = ...;
var i = 0;
i +/= e0; ## wrapping add
i += e0; ## checked add

## kbe
let e0 = ...;
var i = 0;
i += e0; ## wrapping by default
i += e0 @panic_if_overflow; ## overflow checking as opt-in
## or
@panic_if_overflow i += e0;
## or
@(alias L1 = line +=; alias C1 = col +=) i += e0; ## L1 is the line of the token +=, and C1 the column of the token +=
@(alias L1 = line i; alias C1 = col e0) i += e0; ## L1 is the line of the token i, and C1 the column of the token e0
i += e0 @(alias L1 = line +=; alias C1 = col +=); ## L1 is the line of the token +=, and C1 the column of the token +=
i += e0 @(alias L1 = line i; alias C1 = col e0); ## L1 is the line of the token i, and C1 the column of the token e0
panic_if_overflow(L1, C1);
```

```kbe
## fn str = i64_to_str(self: i64) {
##     let base = 10
##     let ch = i64_str[I64_BITS - 1]
##  mov rsi, 10
##  mov rcx, i64_str + I64_BITS - 1

##     if self <=> 0
##     break == write_zero;
##     break <  make_integer_positive;
##     break >  next_digit;

## write_zero:
##     ch.* = '0';
##     break done;

## make_integer_positive:
##     $3 = -$2;

## next_digit:
##     $4, $5 = $3 /% $0;
##     ch.* = $5 + '0' as u8;
##     ch -= 1;

##     if $2
##  cmp rax, 0
##  jne .next_digit

##  cmp rdi, 0
##  jl .add_minus_sign
##  inc rcx
##  jmp .done

## .add_minus_sign:
##  mov byte [rcx], '-'

## .done:
##  mov rdx, i64_str + I64_BITS
##  sub rdx, rcx

##  mov rax, rcx
##  ret"
## };
```

```kbe
var s: ascii[4][3] = [
    ['1'; '1'; '1'];
    ['2'; '2'; '2'];
    ['3'; '3'; '3'];
    ['4'; '4'; '4'];
];
s[1][2] = '3';
s[1 * ascii[3].size + 2 * ascii.size] = '3';
```
