# Feature Ideas

>[!WARNING]
> no feature is final, modifications can happen at any moment

## Remove do statements

- they are just syntactic sugar for a block with a single statement
- they could cause ambiguities and inconsistencies with their "desugared" form:

    ```kay
    # what would this mean? error? warning?
    if condition do fn foo() do println "foo";

    # would be a shorthand for this
    if condition {
        fn foo() {
            println "foo";
        }
    }
    
    # and would be a 'cleanear' version of this
    if condition { fn foo() { println "foo"; } }
    ```

## Expressions formatting

emit a warning for ambiguos use of unary/binary operators, i.e.:

```kay
1 + 2 -3
    # ^ this means `1 minus 2 minus 3` but it might mean `1 minus 2 *missing* negative 3`
    # Help: to avoid ambiguity consider formatting the code as `1 + 2 - 3`, or if you meant
    # negative 3 you might be missing an operator between `2` and `-3` -> `1 + 2 *op* -3`
```

## Built-in notes

Implement a way to recognize and collect todos, and other tags

```kay
# at: file.kay

(12) # TODO(stefano) implement this features
#                   ^^^^^^^^^^^^^^^^^^^^^^^^ everything after the TODO(stefano) is part of the message
(42) # IDEA(stefano)genious
#                   ^notice the missing space
```

running the `kay notes file.kay` command would output something like:

```text
TODO(stefano): file.kay:12: implement this feature
IDEA(stefano): file.kay:42:genious
```

Could create a config file that specifies what notes to look for, like a notes.toml, or create
dedicated cli commands and flags:

```shell
kay notes -n TODO -n IDEA -n NOTE # would recognize TODO, IDEA and NOTE
kay notes # Error: no specified tags to look for
```

## More output file names flags

currently only the output path (`-o`, `--output`) can be specified and the names of the generated artifacts is
generated from the source file name, i.e:

| source file path | output directory path | assembly file path | object file path | executable file path |
| :--------------- | :-------------------- | :----------------- | :--------------- | :------------------- |
| `test.kay`       |                       | `test.asm`         | `test.o`         | `test`               |
| `test.kay`       | `out`                 | `out/test.asm`     | `out/test.o`     | `out/test`           |

could introduce flags to customize individual file output directories and file names separately:

| file kind   | output directory flag        | output file path flag      |
| :---------- | :--------------------------- | :------------------------- |
| general     | `-o`, `--output`             |                            |
| assembly    | `-oa`, `--output-assembly`   | `-na`, `--name-assembly`   |
| object file | `-oo`, `--output-object`     | `-no`, `--name-object`     |
| executable  | `-oe`, `--output-exectuable` | `-ne`, `--name-exectuable` |

or combined:

- assembly output name and directory: `-oa .. -n ..`, `--output-assembly .. --name ..`
- object output name and directory: `-oo .. -n ..`, `--output-object .. --name ..`
- executable output name and directory: `-oe .. -n ..`, `--output-exectuable .. --name ..`

so using the `compile` command with these extra arguments would work like:

| command                                                    | assembly file path | object file path | executable file path  |
| :--------------------------------------------------------- | :----------------- | :--------------- | :-------------------- |
| `test.kay -oa asm`                                         | `asm/test.asm`     | `test.o`         | `test`                |
| `test.kay -no test_obj`                                    | `test.asm`         | `test_obj.o`     | `test`                |
| `test.kay -o out -oa asm`                                  | `out/asm/test.asm` | `out/test.o`     | `out/test`            |
| `test.kay -o out -oa asm -oo obj -oe exe`                  | `out/asm/test.asm` | `out/obj/test.o` | `out/exe/test`        |
| `test.kay -oa asm -oe exe -ne test_executable -oo out/obj` | `asm/test.asm`     | `out/obj/test.o` | `exe/test_executable` |

## Language version embedded in file extension or in the resulting binary executable

From [Fortran](https://www.cita.utoronto.ca/~merz/intel_f10b/main_for/mergedProjects/bldaps_for/common/bldaps_under_inpext.htm#:~:text=Typical%20Fortran%20source%20files%20have,f.)

## Amount of crash information

To reduce binary size we could allow to specify a flag controlling the amount of information printed
in the case of crashes.

As of right now when crashing due to arithmetic constraints we print the general following message:

```text
Crash: something specific wrong happened
at: file.kay:21:12
```

While this is useful because it cleary shows whan cause the problem and where it happened, it comes
with the drawback that every function now needs to accept extra information, thus leading to bigger
binary sizes due to:

- reason of the crash
- file name, line and column numbers of where the crash happened

Every function is thus bigger and more complex, so we could let a cli flag such as
`--crashinfo` followed by some crash info degree such as:

- `full`: as shown above:
- `none`: no reason, file, line and column information:

    ```text
    Crash: program crashed
    ```

- `reason`: only the reason of the crash:

    ```text
    Crash: something specific wrong happened
    ```

- `location`: only the location of the crash:

    ```text
    Crash: program crashed
    at: file.kay:21:12
    ```

This could also speedup performance since less information would be passed to functions, namely
reason of the crash, file, line and column number

## Loops/ifs

- loops similar to Odin's [for loops](https://odin-lang.org/docs/overview/#for-statement)
- ifs similar to Odin's [if statements](https://odin-lang.org/docs/overview/#if-statement)

## Switch statements

- as little effort required to refactor from a regular if to a switch statement
- introducing as little new keywords as possible (just the `case` keyword, and possibly `fall`)
- avoiding added nesting:

```kay
# regular if:
# - 1 level of indentation
# - two keywords: `if`, `else`
if answer == 19 {
    println "lucky";
} else if answer == 21 {
    println "you stoopid";
} else if answer == 42 {
    println "that's the right answer";
} else {
    println "too bad";
}

# switch statement:
# - 1 level of indentation
# - one keyword plus the two from before: `if`, `else`, `case`
#   - possibly a `fall` keyword to specify a fallthrough case
# - same semantics as a regular if statement
#   - every case is like an `else if` branch
#   - it's basically just syntactic sugar
# - actually less code
# - requires minimal structural changes and refactoring:
#   - replace the first `==` and the rest of the `else if answer ==` with a `case`
#   - keep the `else` keyword for the 'default' case
if answer
case 19 {
    println "lucky";
} case 21 {
    println "you stoopid";
} case 42 {
    println "that's the right answer";
} else {
    println "too bad";
}

# alternative switch statement:
# - 1 level of indentation
# - two keywords plus the two from before: `if`, `else`, `switch`, `case`
#   - possibly a `fall` keyword to specify a fallthrough case
# - same semantics as a regular if statement
#   - every case is like an `else if` branch
#   - it's basically just syntactic sugar
# - actually less code
# - requires minimal structural changes and refactoring:
#   - replace the first `if` keyword with the `switch` keyword
#   - replace the first `==` and the rest of the `else if answer ==` with a `case`
#   - keep the `else` keyword for the 'default' case
switch answer
case 19 {
    println "lucky";
} case 21 {
    println "you stoopid";
} case 42 {
    println "that's the right answer";
} else {
    println "too bad";
}

# pattern matching:
# - only requires to change `==` and `else if answer ==` to `case`
# - values inside pattern matching (i.e.: `ok` and `err`) are only available in the corresponding branch
# - split into multiple lines if preferred
# - short and concise
# - can declare mutability modifiers `let` or `var` on matched values
if answer
case let Ok(ok) do println ok; # ok is available only in the do statement and is immutable
case var Err(err) do println err; # err is available only in the do statement and is mutable
case let Err2(var err1, err2) {
    println err1; # err1 is available only in this block and is mutable
    println err2; # err2 is available only in this block and is immutable
}
case var Err2(let err1, err2) {
    println err1; # err1 is available only in this block and is immutable
    println err2; # err2 is available only in this block and is mutable
}

# could benefit from rust's mutability modifiers
# - would get rid of the initial mutabilty modifiers
if answer
case Ok(ok) do println ok; # ok is available only in the do statement and is immutable by default
case Ok_b(let ok) do println ok; # `let` is redundant
case Err(var err) do println err; # err is available only in the do statement and is mutable
case Err2(var err1, err2) {
    println err1; # err1 is available only in this block and is mutable
    println err2; # err2 is available only in this block and is immutable
}

# rust-inspired let else syntax:
# - `ok` will be available from now on
let Ok(ok) = answer else {
    println "err";
    return; # branch need to diverge
}

# rust-inspired let else syntax, but more consistent with a regular pattern match:
# - `ok` will be available from now on
if Ok(let ok) = answer else {
    println "err";
    return; # branch need to diverge
}

# oh no! i need to access the error value
# - literally just add the pattern corresponding to the err case
# - 'err' will only be available in it's switch branch
# - `case` required to allow for more consistency when adding multiple cases
let Ok(ok) = answer else case let Err(err) {
    println err
    return;
}

# would allow for acces to other values in other patterns if needed
# - just add the other patterns
# - debate wheter the repetition of the `else` kewword should be addressed
let Ok(ok) = answer else
case let Err0(err0) {
    println err0;
    return;
} case var Err1(err1) {
    println err1;
    return;
} else {
    println "err";
    return;
}

# would just be syntactic sugar for
let ok = if answer
case let Ok(ok) do break ok;
case let Err0(err0) {
    println err0;
    return;
} case var Err1(err1) {
    println err1;
    return;
} else {
    println "err";
    return;
}
```

possibly allow for the operator before the first case to propagate, basically sugar for a regular if

```kay
# this would be treated as pattern matching
if answer
case 19 ...;
case 21 ...;
case 42 ...;
else ...;

# this would use the `==` operator to "pattern match" on cases
if answer ==
case 19 ...;
case 21 ...;
case 42 ...;
else ...;

# would be sugar for this
if answer == 19 ...;
else if answer == 21 ...;
else if answer == 42 ...;
else ...;

# this would use the `>` operator to "pattern match" on cases
if answer >
case 19 ...;
case 21 ...;
case 42 ...;
else ...;

# would be sugar for this
if answer > 19 ...;
else if answer > 21 ...;
else if answer > 42 ...;
else ...;

# this would use the `%` operator to "pattern match" on cases
if answer %
case 19 == 0 ...; # same as answer % 19 == 0
case 21 == 3 ...; # same as answer % 21 == 3
case 42 ...; # same as answer > 42 -> error: other branches evaluated to booleans while this evaluated to int
else ...;

# would be sugar for this
if answer % 19 == 0 ...;
else if answer % 21 == 3 ...;
else if answer % 42 ...;
else ...;
```

### compared to C

```c
// regular if:
// - 1 level of indentation
// - two keywords: `if`, `else`
if (answer == 19) {
    printf("lucky");
} else if (answer == 21) {
    printf("you stoopid");
} else if (answer == 42) {
    printf("that's the right answer");
} else {
    printf("too bad");
}

// switch:
// - 2 levels of indentation
// - two/three keywords plus the two from before: `if`, `else`, `switch`, `case`, `break`
// - different semantics than a regular if statement
//  - each case is basically a goto statement
// - more code
// - requires lots of structural change and refactoring
switch (answer) {
    case 19: {
        printf("lucky");
        break;
    }
    case 21: {
        printf("you stoopid");
        break;
    }
    case 42: {
        printf("that's the right answer");
        break;
    }
    default: {
        printf("too bad");
    }
}

// switch:
// - same as above
// - 1 level of indentation
// - ugly
switch answer {
case 19: {
    printf("lucky");
    break;
}
case 21: {
    printf("you stoopid");
    break;
}
case 42: {
    printf("that's the right answer");
    break;
}
default: {
    printf("too bad");
}
}
```

### compared to Rust

```rust
// regular if:
// - 1 level of indentation
// - two keywords: `if`, `else`
if answer == 19 {
    print!("lucky");
} else if answer == 21 {
    print!("you stoopid");
} else if answer == 42 {
    print!("that's the right answer");
} else {
    print!("too bad");
}

// match:
// - 2 levels of indentation
// - one keyword plus the two from before: `if`, `else`, `match`
// - different semantics
//  - allows for pattern matching
//  - requires commas in single statements
// - comparable amount of code, less in many cases
// - requires a lot of structural change and refactoring
match answer {
    19 => {
        print!("lucky");
    },
    21 => {
        print!("you stoopid");
    },
    42 => {
        print!("that's the right answer");
    },
    _ => {
        print!("too bad");
    },
}

// let else:
// - short and concise
let Ok(ok) = answer else {
    // oh no! how do i access the error value? which is exactly what an else case should allow for
    print!("{???}");
    return ???;
};

// i need to completely refactor to this:
// - repetition of `if let`
// - complete change to code structure
//  - need to extract ok to a separate variable
let ok = if let Ok(ok) = answer {
    ok
} else if let Err(err) = answer {
    print!("{err}");
    return err;
};

// or to this:
// - same as above
let ok = match answer {
    Ok(ok) => ok,
    Err(err) => {
        print!("{err}");
        return err;
    },
};
```

## Operators

- boolean operators:
    | operator | shortcircuting | non-shortcircuiting | bitwise |
    | :------- | :------------: | :-----------------: | :-----: |
    | and      |      `&&`      |        `&&&`        |   `&`   |
    | or       |     `\|\|`     |      `\|\|\|`       |  `\|`   |
    | xor      |       NA       |        `^^^`        |   `^`   |
- checked (`++`, `--`, `//`, ..., or `+?`, `-?`, `/?`, ...):
    - overflow/underflow may return both the result and the overflow of the addition
    - division will return either the result or an error value
- divmod:

    ```kay
    let division, remainder = 3 /% 2; # will result in 1, 1
    ```

- boolean flip operator `=!`:
    `boolean = !boolean;` -> `boolean =!;`

### Revised remainder/mod operators

| strategy                    | symbol | math equation                                                          |
| :-------------------------- | :----: | :--------------------------------------------------------------------- |
| **with truncation**         |  `%`   | `remainder = dividend - divisor * trunc(dividend / divisor)`           |
| **with floor**              |  `%-`  | `remainder = dividend - divisor * floor(dividend / divisor)`           |
| **with ceil**               |  `%+`  | `remainder = dividend - divisor * ceil(dividend / divisor)`            |
| **with round**              |  `%*`  | `remainder = dividend - divisor * round(dividend / divisor)`           |
| **with euclidian division** |  `%%`  | `remainder = dividend - abs(divisor) * floor(dividend / abs(divisor))` |

### Revised shift operator

| strategy                     | symbol | x86-64 instruction |
| :--------------------------- | :----: | :----------------- |
| **left logical shift**       |  `<<`  | `shl` / `shlx`     |
| **left arithmetical shift**  |  `<<`  | `sal`              |
| **right logical shift**      |  `>>`  | `shr` / `shrx`     |
| **right arithmetical shift** | `>>-`  | `sar` / `sarx`     |


## Strings

- immutable strings are surrounded by `"`: `"hello world"`
- mutable strings (like string builders) are surrounded by `` ` ``: `` `hello world` ``
    - seamlees way to convert from one string type to another
- multiline strings are prefixed by a `m`, or by multiple quotes like in Java:
    - multiline strings may follow C-style string concatenation
    - lines will have newline characters appended to them unless they end in a `\`, which can be escaped using a `\\`
    - whitespace will be preserved (except before the closing quote) and leading whitespace is calculated based on the
        position of the closing quote, or by the text furthest to the left.  
- formatted strings are prefixed by a `f`: `f"the answer is {40 + 2}"`
- options can appear in any order right before the opening quote, but only once:
    - `frm"`, `fr"`, `rm"` are valid
    - `frrm`, `ff "`, `r "` are not valid
- ascii/utf strings and chars (need to decide on proper names):

    | type name  | type    | type size (bytes) | example  | notes                                 |
    | :--------- | :------ | :---------------- | :------- | :------------------------------------ |
    | ascii char | `ascii` | 1                 | `'h'`    | guaranteed to be valid ascii and utf8 |
    | utf8 char  | `utf8`  | 4                 | `u8'è'`  | guaranteed to be valid utf8           |
    | utf16 char | `utf16` | 4                 | `u16'è'` | guaranteed to be valid utf16          |
    | utf32 char | `utf32` | 4                 | `u32'è'` | guaranteed to be valid utf32          |

    | type name    | type       | pointer type | type size (bytes)              | example      | notes                                 |
    | :----------- | :--------- | :----------- | :----------------------------- | :----------- | :------------------------------------ |
    | ascii string | `str`      | ascii\*      | 1 \* len                       | `"hello"`    | guaranteed to be valid ascii and utf8 |
    | utf8 string  | `utf8str`  | utf8\*       | 1 to 4 \* len (in code points) | `u8"hellò"`  | guaranteed to be valid utf8           |
    | utf16 string | `utf16str` | utf16\*      | 2 or 4 \* len (in code points) | `u16"hellò"` | guaranteed to be valid utf16          |
    | utf32 string | `utf32str` | utf32\*      | 4 \* len                       | `u32"hellò"` | guaranteed to be valid utf32          |

- utf8str/utf16str indexing, since characters might be more than one byte long, indexing doesn't
    work, i.e. `string[12]` might land in the middle of a multibyte character, so we could introduce
    rounding indexing (syntax subject to discussion):
    - ceil indexing: `string[+:12]` or `string.at_or_next(12)`, would mean that if the index lands on a non starting byte, it
        would find the next character and return that
    - floor indexing: `string[-:12]` or `string.at_or_previous(12)`, would mean that if the index lands on a non starting byte, it
        would find the previous character and return that
    - checked indexing: `string[?:12]` or `string.at_or_none(12)`, would mean that if the index lands on a non starting byte, it
        would return a `none` value, else the value of the character
    - unchecked indexing: `string[!:12]` or `string.at_byte(12)`, would just return the byte at index 12
    - regular indexing: `string[12]` or `string.at(12)`, would mean that if the index lands on a non starting byte, it
        would crash

## Arrays

stack-allocated collection of a compile time known fixed amount of items:

```kay
# initial capacity cannot be specified from variables
let capacity = 19;
let code: int[capacity]; # error

# unless we introduce compile-time constants
const capacity = 19;
let code: int[capacity]; # works

# initialized arrays could opt not to specify their lengths, it will get inferred where possible
let codes: int[] = [1, 2, 3]; # will be of length 3

# or (need to decide wether to keep these syntaxes and only allow to specify the type after the colon)
let codes = int[1, 2 ,3];                   # array of three items with indexes 0, 1 and 2 initialized to 1, 2, 3
let codes = int[6: 1, 2, 3];                # array of six items with indexes 0, 1 and 2 initialized to 1, 2, 3
let codes = int[6: 1 = 1, 3 = 2, 0 = 3];    # array of six items with indexes 1, 3 and 0 initialized to 1, 2, 3

# or
let codes = int[6][1 = 1, 3 = 2, 0 = 3];    # array of six items with indexes 1, 3 and 0 initialized to 1, 2, 3
```

will borrow useful features from C like indexed initialization:

```kay
let codes: int[19] = [
    2 = 5, # element at index 2 will contain the value 5
    0 = 9,
    3..18 = 3, # items from index 3 to index 18 will contain the value 3
    42 = 7, # error, out of bounds
];
```

### Arrays of bits

boolean values just need 1 bit to store all possible states (true: 1, false: 0), hence a single
`bool` (8 bits) wastes 7 bits. A `bool[n]` would waste `7 * n` bits, thus a solution maybe of
storing arrays of booleans as a `u8[ceil(n / 8)]` and packing the information of 8 booleans into a
single `u8`, or alternatively with the `bit[n]` type:

- `bit[7]` -> `u8` and only use 7 out 8 bits
- `bit[8]` -> `u8` and use 8 bits
- `bit[9]` -> `u8[2]` and use 8 bits of the first element and only 1 out of 8 of the second
    - `bit[9]` -> `u16` and use only 9 out of 16 bits
- `bit[18]` -> `u16[2]` and use 16 bits of the first element and only 2 out of 16 of the second
- `bit[32]` -> `u32` ...
- `bit[33]` -> `u32[2]` ...
- `bit[64]` -> `u64` ...
- `bit[128]` -> `u64[2]` ...
- `bit[64 * n]` -> `u64[n]`
- `bit[64 * n + e]` -> `u64[n + ceil(e / 8)]`

or just `bit[m = 8 * n + e]` -> `u8[n + ceil(e + 8)]`, so:

- `m = 3` -> `bit[0 * n + 3]` -> `u8`
- `m = 21` -> `bit[2 * n + 5]` -> `u8[2 + ceil(5 / 8)]` -> `u8[3]`

### References to bits

reference to items in arrays of bits, i.e. `let bits: bit[3]; let second = &bits[1]` or
`let i = 3; let second = &i[1]` could be stored as fat pointers, containing the reference to the
corresponding byte that contains that bit and an bit offset, so fat pointer to bit would be
equivalent to the following struct:

```kay
struct BitPointer {
    byte: u8&,
    bit: u8, # storing the index as a u8 since numbers can only be of 64 bits
}
```

and would be accessed like this:

```kay
let bits: bit[18];
# equivalent
let bytes: u8[3];

# so this
let fourteenth = &bits[13];

# would be equivalent to
let fourteenth = BitPointer { byte: bytes[2], bit: 13 % mod 8 };

# so this (reading)
println fourteenth;

# would be equivalent to this
println fourteenth.byte >> fourteenth.bit;
```

## Dynamic array (Lists)

heap-allocated collections of a possibly unknown amount of items:

```kay
# the question mark denotes a dynamic array, or a list
# the initial capacity of the list will be set to some amount (e.g. 4/8/16) for performance
let codes: int[..];

# an optional initial capacity can be specified
let codes: int[19..]; # for consistency with initializing some members
let codes: int[19..: 1 = 0, 3 = 5]; # for consistency with initializing some members in arrays

# initial capacity can be specified from variables
let capacity = 19;
let code: int[capacity..]
```

they can be manipulated in different ways (syntax yet to be dicided):
(need to decide if these will give an error if the specified index is out of bounds,
maybe have unchecked and checked versions)

```kay
codes.append(3); # adding an element to the end
codes.pop();

codes.insert(2, 4) # inserting an element at index 2

codes.remove(3); # removing at index 3
```

## Type unions

ability to create a type with a "tag" discriminating which type is currently active

```kay
type int_or_bool = int | bool;

let x: int_or_bool = 1;

if x is int {
    # x type is now inferred as int
} else {
    # x type is now inferred as bool
}

let y: int | bool = true; # type unions can also be implicit
```

type unions can be used with if-case expressions:

```kay
let s0 = "franco";
let s1 = "giovasanni";
let s2 = "aldo";

let s3 = "franco";
let s4 = "giovanni";
let s5 = "aldo";

let b = ["hello", "from", "kay"];
let a = ["hello", "from", "stefano"];

if array_eq(a, b)
case let mismatch: none do println("equals"); # would not be reached since there was a mismatch
else let mismatch: uint do println(f"mismatch at index {mismatch}"); # mismatch would have the value of 2

if array_eq(a, b)
case let mismatch: none do println("equals"); # would not be reached since there was a mismatch
case let mismatch: uint do println(f"mismatch at index {mismatch}"); # mismatch would have the value of 2
else do ...; # unreachable branch: all variants have been matched


fn mismatch_index: uint? = array_eq[T: type, N: uint](dst: T[N]*, src: T[N]*) {
    loop var i = N; i > 0; i -= 1 {
        if dst* != src* {
            return i;
        }

        # incrementing the pointer based on the pointer size
        # so a pointer to an array would get incremented by the size of a single element
        dst += 1;
        src += 1;
    }

    return none;
}
```

## Structs

structs are just an aggregation of types, basically named heterogeneous arrays:

```kay
struct Rgb {
    r: u8,          # type specific default inizialization, which for u8 is 0
    g: u8 = 255,    # explicit default initialization
    b: u8 = ?,      # intentionally uninitialized member, may contain garbage
            # optional trailing coma
}

let rgb = Rgb { r = 255, g = 255, b = 255 };

# will have r initialized to 0 and b initialized to possibly garbage values
let rgb = Rgb { g = 255 };

# or infering the type of the composite type literal from the type annotation
let rgb: Rgb = { r = 255, g = 255, b = 255 };

# or specifying the arguments in order
let rgb = Rgb { 255, 255, 255 };
```

### alternative syntax

using round brackets instead of curly brackets for ease of use and consistency with function calls

```kay
# struct definition
struct Rgb(r: u8, g: u8, b: u8)
```

### method functions

say we now create a constructor function:

```kay
# rust-like
impl Rgb {
    # associated function or java's "static" method
    # marked as @constructor to allow modifications to fields that can only be set during construction
    @constructor fn Self = new(...) { ... }
}

# new ideas (mainly to avoid having an additional indentation coming from the impl block):

#### impl markers
impl Rgb; # from this point onwards every function is a function related to Rgb

# Rust-like associated function of Rgb
fn function_of_Rgb(...) { ... }

# method function of Rgb
fn method_of_Rgb(self: Self, ...) { ... }

impl; # would reset function defintion to being normal functions

fn regular_function(...) { ... }

impl Foo; # from this point onwards every function is a function related to Foo
...

impl Rgb; # can reopen implementations
...

impl Bar; # now related to Bar
...
####

#### function markers
# <Rgb> means it's a method of Rgb
fn<Rgb> Self = new(...) { ... }

# or
Rgb.fn Self = new(...) { ... }

# or
fn Self = Rgb.new(...) { ... }

# <Rgb> means it's a method of Rgb
# first argument is of type Self, meaning this is a method of a variable of type Rgb
fn<Rgb> do_stuff(self: Self, ...) { ... }

# the name of the first parameter could be anything, unlike Rust
fn<Rgb> do_stuff(rgb: Self, ...) { ... }
####

# no need to convert from curly brackets to round brackets, but could need to use the `struct`
# keyword to avoid colliding with a possible function named `Rgb` 
# "equivalent" constructor function, has no access to fields that can only be set inside the struct constructor
fn Rgb = Rgb(r: u8, g: u8, b: u8) {
    return struct Rgb(r, g, b);
}

let rgb = Rgb(r = 0, g = 0, b = 0);         # this will call a function named `Rgb`
let rgb = struct Rgb(r = 0, g = 0, b = 0);  # this will call the struct constructor for `Rgb` 
let rgb = Rgb { r = 0, g = 0, b = 0 };      # traditional way of calling the struct constructor for `Rgb` 
let rgb = Rgb.new(r = 0, g = 0, b = 0);     # this will call the function `Rgb.new`

rgb.do_stuff();
```

#### comparison to rust

```rust
struct Rgb {
    r: u8,
    g: u8,
    b: u8,
}

impl Rgb {
    fn new(r: u8, g: u8, b: u8) -> Self {
        Self { r, g, b }
    }
}

// with the struct initialization syntax
// - can use named 'arguments'
let rgb = Rgb { r: 255, g: 255, b: 255 }

// with the 'constructor' function
// - complete syntax change
// - had to convert curly brackets to round brackets
// - lost ability to use named arguments
let rgb = Rgb::new(255, 255, 255);
```

### Asymetric fields visibility and read/write privileges

ability to specify who can read and write a struct field

```kay
# struct methods and functions always have read access to every kind of field
# may enforce the usage of `let` and `var`
struct Foo(
    # public read: no
    # public write: no
    # private read: yes
    # private write: no
    #
    # only set during construction, never able to be modified again
    private let x0: int,
    let x0: int, # implies private
    x0: int, # implies private let

    # public read: no
    # public write: no
    # private read: yes
    # private write: yes
    #
    # can be modified inside struct method and functions
    private var x0: int,
    var x0: int, # implies private

    # public read: yes
    # public write: no
    # private read: yes
    # private write: no
    #
    # only set during construction, never able to be modified again
    # can be read from outside
    public let private let x0: int,
    public let x0: int, # implies private let
    public x0: int, # implies public let and private let

    # public read: yes
    # public write: no
    # private read: yes
    # private write: yes
    #
    # can be modified inside struct method and functions, but only read from outside
    public let private var x0: int,
    public let x0: int, # implies private let
    public x0: int, # implies public let and private let

    # public read: yes
    # public write: yes
    # private read: yes
    # private write: no
    #
    # disallowed: public var disagrees with private let, makes no sense being able to be modified
    # outside of the struct methods and functions and not inside
    public var private let x0: int,

    # public read: yes
    # public write: yes
    # private read: yes
    # private write: yes
    #
    # can be accessed and modified from everywhere
    public var private var x0: int,
    public var x0: int, # implies private var
)
```

blanket modifiers:

```kay
# every field follows it's declaration modifiers
var foo = struct Foo(x0 = 0);

# every field is immutable, read access to fields are not changed
let foo = struct Foo(x0 = 0);
```

### Nameless/temporary/Rust-like tuple structs

basically structs whith unnamed fields (referred to by index)

named tuples:

```kay
struct Point { int, int };

let point = Point { 19, 21 };

let x = point.0; # Point { 19, 21 }
                 #         ^^  ^^
                 # index:  0   1

let y = point.1;
```

name-less tuples:

```kay
let stefano: { str, int } = { "stefano", 23 };

# type can be omitted and therefore inferred
let stefano = { "stefano", 23 };

let name = stefano.0;
let age = stefano.1;
```

or with explicit struct keyword and named fields:

```kay
let range: struct { min: int, max: int } = struct(1, 2);
println range.min;
println range.max;
```

### Inheritance

inheritance is just syntactic sugar, this allows for any extended type to be passed as "base" type only carrying
the fields defined in the base type:

```kay
struct Rgba {
    rgb: using Rgb,

    # these fields (of the used Rgb struct are implicitly added)
    # r: u8,
    # g: u8,
    # b: u8,

    a: u8,
}

# the above type is equivalent to:
struct Rgba {
    union {
        rgb: Rgb,
        struct {
            r: u8,
            g: u8,
            b: u8,
        }
    }

    a: u8,
}

# 'using' the same struct multiple times is not allowed
struct Rgba {
    rgb: using Rgb,
    rgb2: using Rgb, # not allowed
    a: u8
}

# but 'using' multiple different struct is
struct Point {
    x: int,
    y: int,
}

struct Pixel {
    rgba: using Rgba,
    position: using Point,
}

# which is equivalent to 
struct Pixel {
    union {
        rgba: Rgba,
        struct {
            union {
                rgb: Rgba,
                struct {
                    r: u8,
                    g: u8,
                    b: u8,
                },
            },
            a: u8,
        }
    },
    union {
        position: Point,
        struct {
            x: int,
            y: int,
        },
    },
}

let rgb = Rgb { r = 255, g = 255, b = 255 };

# this
let rgba: Rgba = rgb;

# is equivalent to:
let rgba = Rgba { r = rgb.r, g = rgb.g, b = rgb.b, a = 0 };

# otherwise to:
let rgba = Rgba { rgb = rgb, a = 0 };

# or to:
let rgba = Rgba {
    rgb, # field with same name shorthand
    a = 0,
};
```

if we have a function defined for the "base" struct only the "base" part of the struct will be passed:

```kay
# so this
function_for_Rgb(rgba);

# is desugared to
function_for_Rgb(rgba.rgb);

# and
function_for_rgb(pixel);

# is desugared to
function_for_Rgb(pixel.rgba.rgb);
```

if we dont explicity extend inside a struct it's going to result in an error

```kay
struct Rgb {
    r: u8,
    g: u8,
    a: u8,
}

struct Rgba {
    rgb: Rgb, # no explicit "using"
    a: u8,
}

function_for_Rgb(rgba.rgb); # works
function_for_Rgb(rgba); # doesn't work
```

## Enum

collection of constant values:

```kay
enum Colors: u32 { # optional data type
    # default value for when converting from u32s that don't match the actual enum value
    # for example converting from 0x00ff00 will result in GREEN being chosen
    # when converting from 0x00beef will result in RED being chosen or the returning of an error
    default RED = 0xff0000,
    GREEN = 0x00ff00,
    BLUE = 0x0000ff,
}
```

## Unions

C-like unions:

```kay
union Rgba {
    struct {
        r: u8,
        g: u8,
        b: u8,
        a: u8,
    }

    rgba: u32,
}
```

## Enum unions

Rust-like collection of variants:

```kay
enum union Statement: u8 { # optional discriminant type
    Empty,
    Single { Node },
    Multiple { Node[] },
}
```

## struct/enum/variable memory layout/info

### Variables

Having a variable such as:

```kay
# at: file.kay:12:0
let name: str = "Stefano";
```

getting the variable layout could be done with the command `kay layout name file.kay:12:0`, which
could output the following valid kay code result:

```kay
# size = 16, align = 8
let name: str = "Stefano";
```

### Structs

Having a struct such as:

```kay
struct Foo {
    x: int,
    y: ascii,
    z: str,
}
```

getting the struct layout could be done with the command `kay layout Foo`, which could output the
following valid kay code result:

```kay
# size = 32, align = 8
struct Foo {
    x: int,   # size = 8,  offset = 0,  align = 8 -> 0:  |#|#|#|#|#|#|#|#|
    y: ascii, # size = 1,  offset = 8,  align = 1 -> 8:  |#| | | | | | | |
    z: str,   # size = 16, offset = 16, align = 8 -> 16: |#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|
}
```

could also emit warnings when wasting space, so a struct such as:

```kay
# at: file.kay:12:0

# size = 40, align = 8
struct Foo {
    a: ascii, # size = 1,  align = 1, offset = 0:  |#| | | | | | | |
    x: int,   # size = 8,  align = 8, offset = 8:  |#|#|#|#|#|#|#|#|
    y: ascii, # size = 1,  align = 1, offset = 16: |#| | | | | | | |
    z: str,   # size = 16, align = 8, offset = 24: |#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|
}
```

would produce the following warnign message

```text
Warning: struct has unoptimal field layout
 at: file.kay:12:0
   |
11 | # size = 40, align = 8
12 | struct Foo {
13 |        a: ascii, # size = 1,  align = 1,offset = 0:  |#| | | | | | | |
   |        ^ this field occupies only 1 byte
   |
14 |        x: int,   # size = 8,  align = 8, offset = 8:  |#|#|#|#|#|#|#|#|
15 |        y: ascii, # size = 1,  align = 1, offset = 16: |#| | | | | | | |
   |        ^ this field also occupies only 1 byte, but is separate from the previous
   |
16 |        z: str,   # size = 16, align = 8, offset = 24: |#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|
17 | }
   |
Help: an optimized layout could look like this
   |
11 | # size = 32, align = 8
12 | struct Foo {
13 |        a: ascii, # size = 1,  align = 1, offset = 0:  |#|_| | | | | | |
14 |        y: ascii, # size = 1,  align = 1, offset = 1:  |_|#| | | | | | |
   |        ^ this field is placed next to the previous one, thus not wasting space
   |
15 |        x: int,   # size = 8,  align = 8, offset = 8:  |#|#|#|#|#|#|#|#|
16 |        z: str,   # size = 16, align = 8, offset = 16: |#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|
17 | }
   |
Note: a packed layout could look like this
   |
11 | # size = 26, align = 1
12 | @packed struct Foo {
13 |        x: int,   # size = 8,  align = 8, offset = 8:  |#|#|#|#|#|#|#|#|
14 |        z: str,   # size = 16, align = 8, offset = 16: |#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|
15 |        a: ascii, # size = 1,  align = 1, offset = 24: |#|_|
16 |        y: ascii, # size = 1,  align = 1, offset = 25: |_|#|
   |        ^ these fields are placed last, thus not wasting space
17 | }
   |
```

a `___` could be a padding member, meaning retaining the usual padding amount:

>[!NOTE]
> this `___` field is equivalent to a `u8[N]`, basically just empty bytes

```kay
# size = 26, align = 1
@packed struct Foo {
    a: ascii, # size = 1,  align = 1, offset = 0:  |#|_|_|_|_|_|_|_|_|
    x: int,   # size = 8,  align = 8, offset = 1:  |_|#|#|#|#|#|#|#|#|
    y: ascii, # size = 1,  align = 1, offset = 9:  |#|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
    z: str,   # size = 16, align = 8, offset = 10: |_|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|
}
```

could be use as:

```kay
# size = 33, align = 1
@packed struct Foo {
    a: ascii, # size = 1,  align = 1, offset = 0:  |#|_|_|_|_|_|_|_|
    ___,      # size = 7,  align = 1, offset = 1:  |_|#|#|#|#|#|#|#|
    x: int,   # size = 8,  align = 8, offset = 8:  |#|#|#|#|#|#|#|#|
    y: ascii, # size = 1,  align = 1, offset = 16: |#|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
    z: str,   # size = 16, align = 8, offset = 17: |_|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|
}
```

## Pointers

pointers are going to come in different flavours (introducing `none` keyword):

```kay
let answer = 42;

let pointer: int*; # owned pointer, pointing to owned memory (will free the memory it owns when going out of scope or something)
let reference: int&; # borrowed pointer, pointing to non-owned memory (will possibly support lifetimes)

# avery pointer type can be created with the same syntax
pointer = &answer;
reference = &answer;

let dereferenced: int;

# checking for none is enforced by the compiler
if reference != none {
    # after this point the compiler knows that "reference" is not none and can safely dereference
    dereferenced = *reference;
}
# after this point the compiler can't guarantee that "reference" is not none, so from now on it's again mandatory to check for null

# or you can forcefully dereference (say for example if you for sure know the pointer is valid), crashing in case of a null pointer
dereferenced = ^reference;
```

## Optional types (nullable pointers)

types that may or may not contain a value (introducing the `none` keyword/value):
they are basically tagged unions in the case of non-pointer variables (like Rust's Options)

```kay
# nullable pointers are just "optional pointers" 
let nullable: int*?;
let nullable: int&?;

let option: int? = 42; # this will create a variable that has a value
let option: int? = none; # this will create a variable that doesn't have a value

let maybe: int?;

# checking for none is enforced by the compiler
if option != none {
    # after this point the compiler knows that "option" is not none and can safely dereference
    maybe = option*; # dereferencing like pointers;
    maybe = *option; # dereferencing like pointers;
}
# after this point the compiler can't guarantee that "option" is not none, so from now on it's again mandatory to check for none

# or you can forcefully dereference, crashing in case of a none
maybe = option^;
maybe = ^option; # or like this
```

### Optionals/Error

maybe this is not useful, could be implemented as a type union to reduce the language complexity:

```kay
# optionals
let optional_int: int?;
let optional_int: int | none;

# or
type Option<T> = T | none;

# or
enum Option<T> {
    Some(T),
    None,
}

# thus
let optional_int_in_rust: Option<int>;

# errors
let int_or_error: int!SomeError; 
let int_or_error: int | SomeError;
let int_or_int_error: int | int; # would need to find a way to express this

# or "force" the user to find better naming (create a distinct int error type or alias)
type int_error = int;
alias int_error = int;
let int_or_int_error: int | int_error;

# or to avoid creating a lot of "new" error types
enum Result<T, E> {
    Ok(T),
    Err(E),
}

# thus
let int_or_int_error: Result<int, int>;

# or create temporary distinct types (syntax subject to discussion)
# this could introduce inline type aliases
# so a function could use them like
fn result: int as ok | int as err = foo(i: int) {
    if i
    case 0 do return 1 as err;
    case 12 do return 21 as err;
    case 21 do return 42 as ok;
}

# so to match on it would look like this
let result = foo(i);
if result
case let integer: ok do {
    # integer is of type `int`
} case let err_code: err do {
    # integer is of type `int` as well
}

# different syntaxes
let int_or_int_error: int | err ! int;
let int_or_int_error: int | int ! err;
let int_or_int_error: int | err: int;
let int_or_int_error: int | int alias err;
let int_or_int_error: int | err alias int;

# or a manual implementation, akin to typescript
type Result<T, E> = {
    success = true,
    data: T,
} | {
    success: false, # or using a value as a type
    err: E,
}

# which would allow for manual optimizations
type c_like_int_return =
    None {
        error := -1, # or with a special `value as type syntax`
    } | Some {
        data := 0..,
    }
```

or a `type enum`

``` kay
type enum int_or_bool {
    integer: int,
    boolean: bool,
}

type enum int_or_error_code {
    integer: int,
    error_code: int,
}

type enum Option<T> {
    Some: T,
    None, # empty value
}

type enum Result<T, E> {
    Ok: T,
    Err: E,
}

# so to match on it would look like this
let result = int_or_error_code.integer(1);
if result
case let int_or_error_code.integer(integer) do {
    # `integer` is of type `int`
} case let int_or_error_code.err_code(code) do {
    # `code` is of type `int` as well
}

let result: Result<int, bool> = Result.Ok(1);
if result
case let Result.Ok(integer) do {
    # `integer` is of type `int`
} case let Result.Err(err) do {
    # `err` is of type `bool`
}

# or
let result: Result<int, bool> = Result.Ok(1);
if result
case let integer: Result.Ok do {
    # `integer` is of type `int`
} case let err: Result.Err do {
    # `err` is of type `bool`
}

# inline type enum
let int_or_bool: type enum { file: File, err: ReadFileError };
# compared to what was discussed above
let int_or_bool: File | ReadFileError;
```

or remove type unions altogether and treat enum as type unions

```kay
# this
type enum Result<T, E> {
    Ok: T,
    Err: E,
}

# would become
enum Result<T, E> {
    Ok(T),
    Err(E),
}

# which would solve type collisions, but would be more verbose
enum integer_or_error_code {
    Integer(int),
    ErrorCode(int),
}

# would solve this
let int_or_int_error: int | err: int;
type int_or_int_error = int | err: int;
```

## Bit-casts

ability to define/overload the casting operator for specific types.
types with explicit conversions can be bit-casted to other types when possible
basically defining different interpretations of the same data

```kay
struct Rgba like u32 {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
}

# or (would be more consistent with regular as conversions, e.g.: true as int)
struct Rgba as u32 {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
}

# or
struct Rgba alias u32 {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
}

# or (would be more consistent with variable type hints)
struct Rgba: u32 {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
}

# basically equivalent to, could also be the default to avoid extra language complexity
union Rgba {
    rgba: u32,
    struct {
        r: u8,
        g: u8,
        b: u8,
        a: u8,
    }
}

# or to
struct union Rgba {
    rgba: u32,
    struct {
        r: u8,
        g: u8,
        b: u8,
        a: u8,
    }
}

# this would result in a type size mismatch, or in some other constrait (need to be defined) being broken
struct Rgba like u8 {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
}
```

when bit casts are used inside expressions they incour in no performance penalty, as the compiler would just
treat the values are of different types:

```kay
let rgba: Rgba;
let rgba_u32: u32 = rgba as u32; # bit-casting should be a nop, in this case just a plain copy or rgba memory

let red = Rgba { r = 255 };
let green = Rgba { g = 255 };

# the compiler would treat this as Rgba + Rgba
let red_plus_green = red + green;

# while this would be treated as u32 + u32 and no conversion code would be run
let red_plus_green = red as u32 + green as u32;

# so it avoids this
var red_plus_green: Rgba;
red_plus_green.r = red.r + green.r;
red_plus_green.g = red.g + green.g;
red_plus_green.b = red.b + green.b;
red_plus_green.a = red.a + green.b;
```

### **BREAKING**: Bit-casting operator for primitive types and removal of implicit conversions

## compile time constants and functions excution

```kay
const answer = 40 + 2; # would just copy paste the value everytime
let i = answer; # equivalent to `let i = 40 + 2`

const fn int = answer() do return 42;
let i = const answer(); # equivalent to `let i = { return 42 }` -> `let i = 42`
```

## experiment with no dynamic dispatch

use unions instead, which have to be checked (kinda like what Casey Muratori explained in
["Clean" Code, Horrible Performance](https://www.youtube.com/watch?v=tD5NrevFtbU)).

dynamically dispatched objects rely on interfaces/traits/concept (whatever name) stipulating that a
type implements a specific function, so the compiler can basically inject the tagged union
representing the polymorfic object and the check for the type of the object by itself.

maybe optionally enable true dynamic dispatch on demand with v-tables and stuff

## MATLAB-inspired [functions](https://www.mathworks.com/help/matlab/ref/function.html) definitions

```kay
# introductory keyword
fn

# return values
result: int, remainder: int

# return values' names are optional
int, int

# equals sign to make it easey to copy paste this function definition in code
=

# name of the function
divmod

# function arguments
(dividend: int, divisor: int)

# body of the function, can also be in the do single-statement form
{
    # we can name our return values
    return result = dividend / divisor, remainder = dividend % divisor;

    # or not, where return values' names just serve as comments
    return dividend / divisor, dividend % divisor;
}
```

putting it all together:

```kay
# no return values
fn answer() do return 42;

# with unnamed return values
fn int, int = divmod(dividend: int, divisor: int) do
    return dividend / divisor, dividend % divisor;

# with named return values (NOTE: naked returns are not going to be allowed)
fn result: int, remainder: int = divmod(dividend: int, divisor: int) do
    return result = dividend / divisor, remainder = dividend % divisor;
```

going from function definition to usage would look like this

```kay
# function definition
fn result: int, remainder: int = divmod(dividend: int, divisor: int) do
    return result = dividend / divisor, remainder = dividend % divisor;

# from here onwards we are pretending that each line is the progression of steps needed to go from function definition to the usage

# copy paste the definition
fn result: int, remainder: int = divmod(dividend: int, divisor: int) do

# change 'fn' to 'let'/'var'
# - explicit mutability qualifiers needed for each variable
let result: int, var remainder: int = divmod(dividend: int, divisor: int) do

# remove everything after the arguments' closing round bracket
let result: int, var remainder: int = divmod(dividend: int, divisor: int)

# add a semicolon at the end
let result: int, var remainder: int = divmod(dividend: int, divisor: int);

# remove the function arguments' type hints and you are done!
let result: int, var remainder: int = divmod(dividend, divisor);
```

going from usage to function definition would look like this

```kay
let dividend = 3;
let divisor = 2;

# usage
let result: int, var remainder: int = dividend / divisor, dividend % divisor;

# from here onwards we are pretending that each line is the progression of steps needed to go from usage to the function definition

# copy paste the usage
let result: int, var remainder: int = dividend / divisor, dividend % divisor;

# remove 'let'/'var' and add the 'fn' keyword at the start of the line
fn result: int, remainder: int = dividend / divisor, dividend % divisor;

# add the function name and arguments
fn result: int, remainder: int = divmod(dividend: int, divisor: int) dividend / divisor, dividend % divisor;

# add the function body, with no named returns
fn result: int, remainder: int = divmod(dividend: int, divisor: int) do
    return dividend / divisor, dividend % divisor;

# optionally remove named returns
fn int, int = divmod(dividend: int, divisor: int) do
    return dividend / divisor, dividend % divisor;

# or add them back
fn result: int, remainder: int = divmod(dividend: int, divisor: int) do
    return result = dividend / divisor, remainder = dividend % divisor;

# and done!
```

## Operator overloading

operator overloading should follow the function's philosofy of resembling the shape of the usage of
the function/operator, so as an example, the definition for the `+` operator might look like this:

- for a more "traditional" style:

    ```kay
    op int = +(lhs: int, rhs: int) {
        return lhs + rhs;
    }
    ```

- for a closer look to it's usage, but still consistent with normal functions declarations:

    ```kay
    op int = (lhs: int) + (rhs: int) {
        return lhs + rhs;
    }
    ```

- for an even closer look to it's usage:

    ```kay
    op int = lhs: int + rhs: int {
        return lhs + rhs;
    }
    ```

might also be able to specify that the function should track the caller's line and column for error
messages:

```kay
@track_caller op int = lhs: int + rhs: int {
    return lhs + rhs;
}
```

so going from usage to function would look like this;

```kay
let lhs = 21;
let rhs = 42;

# 1
let i = lhs + rhs;

# 2
op i = lhs + rhs;

# 3a
op i: int = lhs + rhs;

# 3b
op int = lhs + rhs;

# 4
op int = lhs: int + rhs: int;

# 5
op int = lhs: int + rhs: int {
    return lhs + rhs;
}
```

and back from function to usage would look like this;

```kay
# 1
op int = lhs: int + rhs: int {
    return lhs + rhs;
}

# 2
op int = lhs: int + rhs: int;

# 3
op int = lhs + rhs;

# 4
let int = lhs + rhs;

# 5
let i = lhs + rhs;
```

## "unconventional" variable names

```kay
# variable name is not valid because it contains spaces
let this is not a valid variable name = "some value";

# new syntax, repurposing single quotes
let 'this is not a valid variable name' = c"s"; # character literals would become this, or something else

# or like this, where the i string modifier would mean "identifier"
let i"this is not a valid variable name" = "some value";

# it would allow for the use of keywords as identifiers
let i"let" = "let";

# or for this
let i"2" = 2;
```

debate over usefulness and syntax:

```kay
# this
let i"this is not a valid variable name" = "some value";

# could be just this
let this_is_not_a_valid_variable_name = "some value";

# while this
let i"let" = "let";

# or this (or with some other symbol)
let $let = "let";

# could be just this
let lett = "let";

# or simply this
let _let = "let";


# while this
let i"21" = 9 + 10; # complex calculation might be done here
let answer = i"21" * 2;

# could become this
let $21 = 9 + 10;
let answer = $21 * 2;

# or even this
let _21 = 9 + 10; 
let answer = _21 * 2;

# or allow for stuff like this, where a trailing underscore would indicate that this is an identifier instead of a number
let 21_ = 9 + 10;
```
