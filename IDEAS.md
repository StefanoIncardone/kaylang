# Feature Ideas

**IMPORTANT**: no feature is final, modifications can happen at any moment

## Language version embedded in file extension

From [Fortran](https://www.cita.utoronto.ca/~merz/intel_f10b/main_for/mergedProjects/bldaps_for/common/bldaps_under_inpext.htm#:~:text=Typical%20Fortran%20source%20files%20have,f.)

## Loops

- loops similar to [Odin's for loops](https://odin-lang.org/docs/overview/#for-statement)
- do-for loops, only for C-style while loops
    - cannot be used with infinite loops or with C-style for loops

## Once keyword

- allow variables to be mutated only once or in special places

## Arbitrary number bases between 1 and 37

| prefix | shorthand | postfix |  base   |         result          |
| :----- | :-------: | :-----: | :-----: | :---------------------: |
| 02_    |   0b/B    |   b2    | base 2  | two's complement binary |
| 08_    |   0o/O    |   b8    | base 8  |          octal          |
| 016_   |   0x/X    |   b16   | base 16 |           hex           |
| 01_    |           |   b1    |         |          error          |
| 04_    |           |   b4    |         |         base 4          |
| 035_   |           |   b35   |         |         base 35         |
| 037_   |           |   b37   |         |          error          |

### shift operators for different number bases (i.e. shifting corresponds to multiplying/dividing by the number base)

```blitz
let number_base_10: int = 123; # base 10
number_base_10 << 1; # 123 << 1 == 1230 (multiplying by 10, desugars to -> * 10)
number_base_10 >> 2; # 123 >> 2 == 1 (dividing by 10 two times, desugars to -> / (10 * 2))

let number_base_2: int = 0b1010; # base 2
number_base_2 << 1; # 1010 << 1 == 10100 (multiplying by 2, uses bit shifting)
number_base_2 >> 1; # 1010 >> 1 == 101 (dividing by 2, uses bit shifting)
```

**or maybe let the user specify the shift base**

#### or maybe let the user specify the shift base

```blitz
let number_base_10: int = 123; # base 10
number_base_10 <<b2 1; # 123 <<b2 1 == 246 (multiplying by 2, uses bit shifting)
number_base_10 >>b2 2; # 123 >>b2 2 == 30 (dividing by 2 two times, uses bit shifting)

let number_base_2: int = 0b1010; # base 2
number_base_2 <<b10 1; # 1010 <<b10 1 == 01100100 (multiplying by 10, desugars to -> * 10)
number_base_2 >>b10 1; # 1010 >>b10 1 == 1 (dividing by 10, desugars to -> / 10)
```

## Unchecked/Checked

- unchecked (+, -, /, ... ): overflow will wrap, division by zero will crash
- checked (++, --, //, ...):
    - overflow/underflow may either crash or return both the result and the overflow of the addition
    - division will return either the result or an error value
- maybe have a compiler flag to use checked/unchecked operators

## Operators

have them as built-in operators or just implement them as functions

- absolute value, enclosed by a `|`:

    ```blitz
    |19| == 19
    |-19| == 19
    ```

- floor, enclosed by `|\` and `/|`, and have to be written as a single token, so `| \` and `/ |` are not valid

    ```blitz
    |\19.3/| == 19
    |\-19.3/| == -20
    ```

- ceil, enclosed by `|/` and `\|`, and have to be written as a single token, so `| /` and `\ |` are not valid

    ```blitz
    |/19.3\| == 20
    |/-19.3\| == -19
    ```

- is multiple of: `%%`

    ```blitz
    let number = 42;
    if number %% 2 { # desugars to number % 2 == 0
        println "even";
    }
    else {
        println "odd";
    }
    ```

- boolean expressions chaining

    ```blitz
    let n = 42;
    # find apropriate syntax
    if n == 19, 21, 42 || 71 < n <= 138 { # desugars to n == 19 || n == 21 || n == 42 || (71 < n && n <= 138)
        println "nice";
    }

    if n %% 2, 6, 14 { # desugars to n % 2 == 0 || n % 6 == 0 || n % 14 == 0
        println "multiple of 2, 6 or 14";
    }
    ```

## Strings

- immutable strings are surrounded by `"`: `"hello world"`
- mutable strings (like string builders) are surrounded by `` ` ``: `` `hello world` ``
    - seamlees way to convert from one string type to another
- raw strings are prefixed by a `r`: `r"\this wo\n't be escaped"`
- multiline strings are prefixed by a `m`, or by multiple quotes like in Java:
    - lines will have newline characters appended to them unless they end in a `\`, which can be escaped using a `\\`
    - whitespace will be preserved (except before the closing quote) and leading whitespace is calculated based on the
        position of the closing quote, or by the text furthest to the left.  
- formatted strings are prefixed by a `f`: `f"the answer is {40 + 2}"`
- options can appear in any order right before the opening quote, but only once:
    - `frm"`, `fr"`, `rm"` are valid
    - `frrm`, `ff "`, `r "` are not valid

## Arrays

stack-allocated collection of a compile time known fixed amount of elements:

```blitz
# uninizialized arrays must specify their lengths and will contain garbage
let codes: int[3];

# initialized arrays could opt not to specify their lengths, it will get inferred where possible
let codes: int[] = [1, 2, 3]; # will be of length 3

# or (need to decide wether to keep these syntaxes and only allow to specify the type after the colon)
let codes = int[1, 2 ,3];
let codes = int[3][1, 2, 3];
```

will borrow useful feature from C like indexed initialization:

```blitz
let codes: int[19] = [
    2 = 5, # element at index 2 will contain the value 5
    0 = 9,
    3..18 = 3, # elements from index 3 to index 18 will contain the value 3
    42 = 7, # error, out of bounds
    ]
```

and expand on them:

```blitz
let codes: int[19] = [..=0] # every element will contain the value 0
```

## Dynamic array (Lists)

heap-allocated collections of a possibly unknown amount of elements:

```blitz
# the question mark denotes a dynamic array, or a list
# the initial capacity of the list will be set to some amount (e.g. 4/8/16) for performance
let codes: int[?];

# an option initial capacity can be specified
let codes: int[?, 19];
```

they can be manipulated in different ways (syntax yet to be dicided):
(need to decide if these will give an error if the specified index is out of bounds,
maybe have unchecked and checked versions)

```blitz
codes.append( 3 ); # adding an element to the end
codes.pop();

codes.insert( 2, 4 ) # inserting an element at index 2

codes.remove( 3 ); # removing at index 3
```

## Aliases

ability to create type aliases, which are just alternative names to existing types

```blitz
alias byte = u8;
```

## Distinct types

ability to create distinct types, which are considered entirely different types:

```blitz
type byte = u8;
```

## Structs

structs are just an aggregation of types:

```blitz
struct RBG {
    r: u8 = default,    # type specific default inizialization, which for u8 is 0
    g: u8 = 255,        # explicit default initialization
    b: u8,              # intentionally uninitialized member, may contain garbage
        # optional trailing coma
}

let rgb = RGB { r = 255, g = 255, b = 255 };

# or infering the type of the composite type literal from the type annotation
let rgb: RGB = { r = 255, g = 255, b = 255 };

# or specifying the arguments in order
let rgb = RGB { 255, 255, 255 };
```

### Inheritance

inheritance is just syntactic sugar, this allows for any extended type to be passed as "base" type only carrying
the fields defined in the base type:

```blitz
struct RGBA_unnamed {
    extends RGB,
    a: u8
}

# the above type is equivalent to
struct RBGA {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
}

# or we can give a name to the "extension" and acces the RGB fields as rgb.r, rgb.g, rgb.b
struct RGBA_named {
    rgb: extends RGB,
    a: u8
}

# Multiple extension are not allowed
struct RGBA {
    rgb: extends RGB,
    rgb2: extends RGB, # not allowed
    a: u8
}

struct RGBA {
    extends RGB,
    rgb2: extends RGB, # not allowed
    a: u8
}

struct RGBA {
    extends RGB,
    extends RGB, # not allowed
    a: u8
}

let rgba: RGBA_unnamed = rgb;

# any extra fields will be default initialized;
let rgba = RGBA_unnamed { r = rgb.r, g = rgb.g, b = rgb.b, a = 0 };

# otherwise
let rgba = RGBA_unnamed { rgb = rgb, a = 255 };

# for named extensions
let rgba: RGBA_named = rgb;

# any extra fields will be default initialized;
let rgba = RGBA_named { rgb = rgb, a = 0 };

# otherwise
let rgba = RGBA_named { rgb, a = 255 };
```

if we have a function defined for the "base" struct only the "base" part of the struct will be passed:

```blitz
# so this
function_for_RGB( rgba );

# is desugared to
function_for_RGB( rgba.rgb );
```

if we dont explicity extend inside a struct it's going to result in an error

```blitz
struct RGB {
    r: u8,
    g: u8,
    a: u8,
}

struct RGBA {
    rgb: RGB, # no explicit "extend"
    a: u8,
}

function_for_RGB( rgb ); # works
function_for_RGB( rgba ); # doesn't work
```

## Enum

collection of constant values:

```blitz
enum Colors: u32 { # optional data type
    RED = 0xff0000,
    GREEN = 0x00ff00,
    BLUE = 0x0000ff,
}
```

## Unions

Rust-like collection of variants:

```blitz
union Statement {
    Empty,
    Single( Node ),
    Multiple( Node[] ),
}
```

## Possible types syntaxes

| old                                                      | new                                                            |
| :------------------------------------------------------- | :------------------------------------------------------------- |
| `alias alias_type = other_type`                          | `type alias_type = other_type`                                 |
| `type distinct_type = other_type`                        | `type distinct_type: other_type`                               |
| `struct struct_name {...}`                               | `type struct_name {...}`                                       |
| `enum enum_name {...}` <br> `enum enum_name: type {...}` | `type enum_name [...]` <br> `type enum_name: type [...]`       |
| `union union_name {...}`                                 | `type union_name \|...\|` <br> or <br> `type union_name (...)` |

## tuples

## multiple return types

## nullable pointers, that are enforced to be checked before being dereferenced

## no or close to no implicit conversions, or just where it makes sense (i.e. u8 -> u16, int -> float)

## compile time constants and functions excution (e.g.: const i = 3;)

## experiment with no dynamic dispatch, use unions instead, which have to be checked (kinda like what Casey Muratori explained in ["Clean" Code, Horrible Performance](https://www.youtube.com/watch?v=tD5NrevFtbU))
