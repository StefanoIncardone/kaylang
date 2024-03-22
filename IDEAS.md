# Feature Ideas

**IMPORTANT**: no feature is final, modifications can happen at any moment

## Language version embedded in file extension or in the resulting binary executable

From [Fortran](https://www.cita.utoronto.ca/~merz/intel_f10b/main_for/mergedProjects/bldaps_for/common/bldaps_under_inpext.htm#:~:text=Typical%20Fortran%20source%20files%20have,f.)

## Loops/ifs

- loops similar to Odin's [for loops](https://odin-lang.org/docs/overview/#for-statement)
- ifs similar to Odin's [if statements](https://odin-lang.org/docs/overview/#if-statement)

### switch statements

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
# - short and concise
# - can declare mutability modifiers `let` or `var` on matched values
if answer == let Ok(ok) do println ok; # ok is available only in the do statement

# more cases:
if answer == let Ok(ok) do println ok;
else if answer == let Err(err) do println err;

# refactor to switch:
# - only requires to change `==` and `else if answer ==` to `case`
# - values inside pattern matching (i.e.: `ok` and `err`) are only available in the corresponding branch
# - split into multiple lines if preferred
if answer
case var Ok(ok) do println ok;
case let Err(err) do println err;

# rust-inspired let else syntax:
# - `ok` will be available from now on
let Ok(ok) = answer else do println "err";

# oh no! i need to access the error value
# - literally just add the pattern corresponding to the err case
# - 'err' will only be available in it's switch branch
# - `case` required to allow for more consistency when adding multiple cases
let Ok(ok) = answer else case let Err(err) do println err;

# would allow for acces to other values in other patterns if needed
# - just add the other patterns
# - debate wheter the repetition of the `else` kewword should be addressed
let Ok(ok) = answer else
case let Err0(err0) do println err0;
case var Err1(err1) do println err1;
else do println "err";

# want to refactor to a regular switch?
# - minimal code change
# - all of the matched values will only be available in their switch branch
if answer
case let Ok(ok) do println ok;
case var Err0(err0) do println err0;
case let Err1(err1) do println err1;
else do println "err";
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

#### compared to C

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

#### compared to Rust

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
};

// i need to completely refactor to this:
// - repetition of `if let`
// - complete change to code structure
//  - need to extract ok to a separate variable
//  - need to remember to explicity return in the Err case to match the let-else behaviour
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

- add [Zig inspired arithmetic operators](https://ziglang.org/documentation/master/#Operators)
- unchecked (+, -, /, ...): overflow will wrap, division by zero will crash
- checked (++, --, //, ...):
    - overflow/underflow may return both the result and the overflow of the addition
    - division will return either the result or an error value
- maybe have them as built-in operators or just implement them as functions
- divmod:

    ```kay
    let division, remainder = 3 /% 2; # will result in 1, 1
    ```

## Strings

- immutable strings are surrounded by `"`: `"hello world"`
- mutable strings (like string builders) are surrounded by `` ` ``: `` `hello world` ``
    - seamlees way to convert from one string type to another
- multiline strings are prefixed by a `m`, or by multiple quotes like in Java:
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

## Arrays

stack-allocated collection of a compile time known fixed amount of elements:

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
let codes = int[1, 2 ,3];                   # array of three elements with indexes 0, 1 and 2 initialized to 1, 2, 3
let codes = int[6: 1, 2, 3];                # array of six elements with indexes 0, 1 and 2 initialized to 1, 2, 3
let codes = int[6: 1 = 1, 3 = 2, 0 = 3];    # array of six elements with indexes 1, 3 and 0 initialized to 1, 2, 3

# or
let codes = int[6][1 = 1, 3 = 2, 0 = 3];    # array of six elements with indexes 1, 3 and 0 initialized to 1, 2, 3
```

will borrow useful features from C like indexed initialization:

```kay
let codes: int[19] = [
    2 = 5, # element at index 2 will contain the value 5
    0 = 9,
    3..18 = 3, # elements from index 3 to index 18 will contain the value 3
    42 = 7, # error, out of bounds
];
```

## Dynamic array (Lists)

heap-allocated collections of a possibly unknown amount of elements:

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

## Aliases

ability to create type aliases, which are just alternative names to existing types

```kay
alias byte = u8;
```

## Distinct types

ability to create distinct types, which are considered entirely different types:

```kay
type byte = u8;
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

if array_eq(a, b) is
case let mismatch: none do println("equals"); # would not be reached since there was a mismatch
else let mismatch: uint do println(f"mismatch at index {mismatch}"); # mismatch would have the value of 2

fn mimatch_index: uint? = array_eq[T: type, N: uint](dst: T[N]*, src: T[N]*) {
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
struct RBG {
    r: u8,          # type specific default inizialization, which for u8 is 0
    g: u8 = 255,    # explicit default initialization
    b: u8 = ?,      # intentionally uninitialized member, may contain garbage
            # optional trailing coma
}

let rgb = RGB { r = 255, g = 255, b = 255 };

# will have r initialized to 0 and b initialized to possibly garbage values
let rgb = RGB { g = 255 };

# or infering the type of the composite type literal from the type annotation
let rgb: RGB = { r = 255, g = 255, b = 255 };

# or specifying the arguments in order
let rgb = RGB { 255, 255, 255 };
```

### alternative syntax

using round brackets instead of curly brackets for ease of use and consistency with function calls

```kay
struct RGB (
    r: u8,
    g: u8,
    b: u8,
)

# maybe this can stay as curly brackets
struct RGB {
    r: u8,
    g: u8,
    b: u8,
}

let rgb = RGB(r = 255, g = 255, b = 255);

# say we now create a constructor function 
impl RGB {
    fn Self = new(r: u8, g: u8, b: u8) {
        # the StructName(...) "function" is like an implicit constructor function
        return Self(r, g, b); # round backets
    }
}

# no need to convert from curly brackets to round brackets
let rgb = RGB.new(r = 255, g = 255, b = 255); # using '.' instead of '::'
```

#### comparison to rust

```rust
struct RGB {
    r: u8,
    g: u8,
    b: u8,
}

impl RGB {
    fn new(r: u8, g: u8, b: u8) -> Self {
        Self { r, g, b }
    }
}

// with the struct initialization syntax
// - can use named 'arguments'
let rgb = RGB { r: 255, g: 255, b: 255 }

// with the 'constructor' function
// - complete syntax change
// - had to convert curly brackets to round brackets
// - had to remove named arguments
let rgb = RGB::new(255, 255, 255);
```

### Rust-like tuple structs

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
let stefano: { string, int } = { "stefano", 23 };

# type can be omitted and therefore inferred
let stefano = { "stefano", 23 };

let name = stefano.0;
let age = stefano.1;
```

### Inheritance

inheritance is just syntactic sugar, this allows for any extended type to be passed as "base" type only carrying
the fields defined in the base type:

```kay
struct RGBA {
    rgb: using RGB,

    # these fields (of the used RGB struct are implicitly added)
    # r: u8,
    # g: u8,
    # b: u8,

    a: u8,
}

# the above type is equivalent to:
struct RGBA {
    union {
        rgb: RGB,
        struct {
            r: u8,
            g: u8,
            b: u8,
        }
    }

    a: u8,
}

# 'using' the same struct multiple times is not allowed
struct RGBA {
    rgb: using RGB,
    rgb2: using RGB, # not allowed
    a: u8
}

# but 'using' multiple different struct is
struct Point {
    x: int,
    y: int,
}

struct Pixel {
    rgba: using RGBA,
    position: using Point,
}

# which is equivalent to 
struct Pixel {
    union {
        rgba: RGBA,
        struct {
            union {
                rgb: RGBA,
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

let rgb = RGB { r = 255, g = 255, b = 255 };

# this
let rgba: RGBA = rgb;

# is equivalent to:
let rgba = RGBA { r = rgb.r, g = rgb.g, b = rgb.b, a = 0 };

# otherwise to:
let rgba = RGBA { rgb = rgb, a = 0 };

# or to:
let rgba = RGBA {
    rgb, # field with same name shorthand
    a = 0,
};
```

if we have a function defined for the "base" struct only the "base" part of the struct will be passed:

```kay
# so this
function_for_RGB(rgba);

# is desugared to
function_for_RGB(rgba.rgb);

# and
function_for_rgb(pixel);

# is desugared to
function_for_RGB(pixel.rgba.rgb);
```

if we dont explicity extend inside a struct it's going to result in an error

```kay
struct RGB {
    r: u8,
    g: u8,
    a: u8,
}

struct RGBA {
    rgb: RGB, # no explicit "using"
    a: u8,
}

function_for_RGB(rgba.rgb); # works
function_for_RGB(rgba); # doesn't work
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
union RGBA {
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

## Bit-casts

ability to define/overload the casting operator for specific types.
types with explicit conversions can be bit-casted to other types when possible

```kay
struct RGBA like u32 {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
}

# or
struct RGBA as u32 {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
}

# or
struct RGBA alias u32 {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
}

# basically equivalent to
union RGBA {
    rgba: u32,
    struct {
        r: u8,
        g: u8,
        b: u8,
        a: u8,
    }
}

# this would result in a type size mismatch, or in some other constrait (need to be defined) being broken
struct RGBA like u8 {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
}
```

when bit casts are used inside expressions they incour in no performance penalty, as the compiler would just
treat the values are of different types:

```kay
let rgba: RGBA;
let rgba_u32: u32 = rgba as u32; # bit-casting should be a nop, in this case just a plain copy or rgba memory

let red = RGBA { r = 255 };
let green = RGBA { g = 255 };

# the compiler would treat this as RGBA + RGBA
let red_plus_green = red + green;

# while this would be treated as u32 + u32 and no conversion code would be run
let red_plus_green = red as u32 + green as u32;

# so it avoids this
var red_plus_green: RGBA;
red_plus_green.r = red.r + green.r;
red_plus_green.g = red.g + green.g;
red_plus_green.b = red.b + green.b;
red_plus_green.a = red.a + green.b;
```

## compile time constants and functions excution

```kay
# decide on "compile-time" directives syntax (maybe convert comments to `//` or something else and use `#`)
const answer = 40 + 2;
#static let answer = 40 + 2;
static answer = 40 + 2;
static var answer = 40 + 2;
answer := 40 + 2;
answer: int := 40 + 2;

run some_function();
@run some_function();
const some_function();
static some_function();
```

## pass/none/whatever equivalent to doing nothing (python's pass)

let answer = 0;
if answer == 1 do println "in branch 1";
else if answer > 1 do pass;
else if answer > 1 none; # or like this, explicit no do keyword required (to differentiate from none being a value)

## experiment with no dynamic dispatch

use unions instead, which have to be checked (kinda like what Casey Muratori explained in
["Clean" Code, Horrible Performance](https://www.youtube.com/watch?v=tD5NrevFtbU)).

dynamically dispatched objects rely on interfaces/traits/concept (whatever name) stipulating that a type implements a
specific function, so the compiler can basically inject the tagged union representing the polymorfic object and the
check for the type of the object by itself

maybe optionally enable true dynamic dispatch on demand with v-tables and stuff

## MATLAB-inspired [function](https://www.mathworks.com/help/matlab/ref/function.html) definitions

```kay
# introductory keyword
fn

# return values
result: int, remainder: int

# return values' names are optional
[int, int]

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
