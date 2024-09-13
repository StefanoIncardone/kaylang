# Language Reference

```kay
println "Kay let's go!";
```

## Comments

line comments start with `#` and ignore everything until the end of the line:

```kay
# lines starting with the `#` symbol will be ignored by the compiler
```

## Semicolons

Each valid statement must end in a semicolon:

```kay
"Kay " # Error: missing semicolon
"let's go";
```

## UTF-8 support

The language will only support ASCII characters in the source code, and will eventually allow
UTF-8, UTF-16 and UTF-32 characters only in specific and selected places, such as in comments and in
string and character literals.

As of now UTF-8 characters are only allowed in comments:

```kay
# UTF-8 characters 🤪 are allowed in comments

# will eventually be supported in strings and character literals
"hi 👋";

# Error: UTF-8 characters will not be allowed in any other place
menù;
```

## Integers

Integers, of type `int`, are represented in source code as base 10 numbers, and are represented in
memory as signed 64 bit values:

```kay
21;      # valid number
021;     # leading zeroes are allowed
1_2_3_4; # separating underscores are allowed
21a;     # Error: integer literals cannot contain non-digit characters
```

### Alternative number literals bases

Number literals can be written in other bases other than the default decimal namely, binay, octal
and hexadecimal, with the appropriate prefixes:

| number system     | base | valid digits          | prefix | example        |
| :---------------- | :--- | :-------------------- | :----- | :------------- |
| decimal (default) | 10   | 0..=9                 |        | `21`           |
| binary            | 2    | 0..=1                 | `0b`   | `0b1100`       |
| octal             | 8    | 0..=7                 | `0o`   | `0o14`         |
| hexadecimal       | 16   | 0..=9, A..=F or a..=f | `0x`   | `0xc` or `0xC` |

>[!NOTE]
> empty numbers in bases other than decimal are not allowed, i.e. `0b`, `0o` and `0x`

## Booleans

Boolean values, of type `bool`, are represented in source code as `true` and `false`, and are
represented in memory as unsigned 8 bit values:

## ASCII Characters

Characters, of type `ascii`, are used to represent a single ASCII character, and are are represented
in source code as being surrounded by `'`:

```kay
'f';  # must contain a single ASCII characters
'\n'; # or a valid escape character
'';   # Error: empty characters are not allowed
'f;   # Error: unclosed character
'\f;  # Error: unclosed escape character
```

### Escape sequences

It is also possible to represent special characters by **escaping** them with back slashes:
These are the available escaped characters:

```kay
'\\'; # backslash
'\''; # single quote
'\"'; # double quote
'\n'; # newline
'\r'; # carriage return
'\t'; # tab character
'\0'; # null character

'\f'; # anything not in the previous list is considered an invalid escape sequence
```

## ASCII Strings

Strings, of type `str`, are used to represent strings of ASCII text, and are represented in source
code as being surrounded by `"` and contain any number of regular or escaped characters:

```kay
"Kay\nlet's go";

# Error: unclosed string
"Kay\nlet's go
```

It is also possible to represent special characters by **escaping** them with back slashes, just
like with character literals [escape sequences](#escape-sequences).

Strings can also be configured as **raw strings** with the `r` prefix, where escapes sequences would
not be taken into account:

```kay
"Escaped\nstring";

# taken 'as is' from source code
r"Raw\nstring";
```

Strings can also be indexed with zero-based indexing to gain access to individual character:

```kay
"01234"[3]; # will return the character '3'
```

## Arrays

Arrays are a collection of **at least 2 values** of the same type under a single variable, so an
array of three `int` would be of type `int[3]`.

Arrays of 0 elements are not allowed because they are phantom values not even occupying any memory,
while arrays of 1 element are not allowed becasue they are functionally equivalent to that element
if it was outside of the array.

Arrays are defined as comma-separated lists of items, as follows:

```kay
[];                       # Error: arrays of zero items are not allowed
[19];                     # Error: arrays of one item are not allowed
[12, 21];                 # this declares an array of two items, namely `int[2]`
["Kay", "let's", "go!",]; # trailing commas are allowed, thus the array would be of type `str[3]]
```

Arrays can also be indexed with zero-based indexing, to gain access to individual items:

```kay
[0, 1, 2, 3, 4][3]; # will return the integer 3
["01234", "56789"][0][3]; # will return the string "01234", and access the character '3'
```

## Variables

Variables are **typed** containers, that hold values for later use, and follow these definition
rules:

1. mutability class:
    - `let`: immutable variable, once set cannot be changed later
    - `var`: mutable variable, can be changed at any moment
2. variable name that can be made of (up to a maximum of **63** characters):
    - letters
    - numbers (but not starting with)
    - underscores
3. value:
    - type annotation: `:` followed by the type of the variable
    - variable value: `=` followed by the value we wish the variable to hold
    - both type annotation and variable value

So following the specified rules here are a few examples on how to create variables:

```kay
# 1. mutability class
let kay = "kay"; # immutable variable named `kay` with value "kay"
kay = "let's go!"; # Error: cannot mutate immutable variable

var one = 1; # mutable variable named `one` with value 1
one = 2; # `one` will from now on contain the value 2

# 2. variable names
let kay_lets_go = "kay, let's go!"; # can contain underscores
let two_plus_2 = "two + 2"; # can contain numbers
let 2plus2 = "2 + 2"; # Error: not a valid name, cannot start with a number
let longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong; # Error: over the limit of 63 characters

# 3. optional type annotations
let inferred = 42; # the type will be inferred as `int` by the expression to the right of the `=` sign
let explicit: int = 42; # with the type annotation the type is specified to be `int
let mismatched: int = "42"; # Error: annotated type differs from actual value, expected `int` but got `str`

# 4. optional variable values
# when no initial value is specified a default value will be assigned base on the annotated type
let default_initialized_int: int; # 0 is the default value for `int`
let default_initialized_ascii: ascii; # '\0' is the default value for `ascii`
let default_initialized_bool: bool; # `false` is the default value for `bool`
let default_initialized_str: str; # "" is the default value for `str`

# since all variables must hold a concrete type, there need to be specified either a type annotation
# or an initial value to be able to determine the type of the variable
let cannot_infer_type; # Error: missing either type annotation or initial value to determine the type of the variable
```

## Expressions

>[!NOTE]
> Here are some usefull definitions to keep in mind
>
> ```kay
> let INT_MIN = -9223372036854775808;
> let INT_MAX = 9223372036854775807;
> ```

>[!NOTE]
> Some operators support `*op*=` variations (i.e.: `+=`, `-=`, ...)
>
> ```kay
> var twentyone = 9;
> twentyone += 10; # equivalent to "twentyone = twentyone + 10;"
> ```

>[!NOTE]
> Some operantors support saturating `*op*|` and wrapping `*op*\` variations (i.e.: `+\`, `-|`, ...)

Expressions follow this order of operations (precedence from highest to lowest):

- primary expressions:
    - unary string and array length operator `len`:

        ```kay
        len [12, 21, 42, 19] # -> 4
        len "kay" # -> 3
        ```

    - unary integer negation `-`, `-\`, `-|`:

        ```kay
        -twelve # -> -12
        -INT_MIN # -> crash: overflow, -INT_MIN -> INT_MAX + 1
        
        -\twelve # -> -12
        -\INT_MIN # -> INT_MAX + 1 -> INT_MIN

        -|twelve # -> -12
        -|INT_MIN # -> INT_MAX + 1 -> INT_MAX
        ```

    - unary integer absolute value `+`, `+\`, `+|`:

        ```kay
        +twelve # |12| == +12 -> 12
        +(-twelve) #|-12| == +(-12) -> 12
        +INT_MIN # -> crash: overflow, +INT_MIN -> INT_MAX + 1

        +\twelve # -> -12
        +\(-twelve) #|-12| == +\(-12) -> 12
        +\INT_MIN # -> INT_MAX + 1 -> INT_MAX

        +|twelve # -> 12
        +|(-twelve) #|-12| == +|(-12) -> 12
        +|INT_MIN # -> INT_MAX + 1 -> INT_MIN
        ```

    - unary integer bitwise one's complement and boolean negation `!`:

        ```kay
        !4 # 0100 -> !0100 == 1011
        !true # -> false
        ```

    - round brackets `\(`, `)`
- binary exponentiation `**`, `**\`, `**|`:

    ```kay
    3 ** 2 # -> 9
    INT_MAX ** 2 # -> crash: overflow

    3 **\ 2 # -> 9
    INT_MAX **\ 2 # -> 1

    3 **| 2 # -> 9
    INT_MAX **| 2 # -> INT_MAX
    ```

- binary multiplication `*`, `*\`, `*|`, binary division `/`, `/\`, `/|` and binary remainder `%`:

    ```kay
    3 * 2 # -> 6
    INT_MAX * 2 # -> crash: overlflow
    INT_MIN * -1 # -> equivalent to -INT_MIN
    
    3 *\ 2 # -> 6
    INT_MAX *\ 2 # -> -2
    INT_MIN *\ -1 # equivalent to -\INT_MIN
    
    3 *| 2 # -> 6
    INT_MAX *| 2 # -> -2
    INT_MIN *| -1 # equivalent to -|INT_MIN
    
    6 / 2 # -> 3
    INT_MIN / -1 # -> crash: overflow, equivalent to -INT_MIN

    6 /\ 2 # -> 3
    INT_MIN /\ -1 # -> INT_MIN, equivalent to -\INT_MIN

    6 /| 2 # -> 3
    INT_MIN /| -1 # -> INT_MAX, equivalent to -|INT_MIN
    ```

- binary addition `+` and binary subtraction `-`:

    ```kay
    12 + 21 # -> 33
    42 - 12 # -> 30
    INT_MAX + 1 # -> error
    ```

- binary left shift `<<` and binary right shift `>>`:

    ```kay
    1 << 2 # 0001 << 2 == 0100
    ```

- binary bitwise and `&`
- binary bitwise xor `^`
- binary bitwise or `|`
- comparison operators:
    - binary comparison `<=>`: `-1` if less than, `0` if equals, `1` if greater
    - binary boolean comparisons (cannot be chained, i.e.: `3 > 2 > 1` is not a valid boolean
        expression):
        - equals to `==`
        - not equals to `!=`
        - greater than `>`
        - greater or equals than `>=`
        - less than `<`
        - less or equals than `<=`

        > [!NOTE]
        > arrays and strings can be compared following the
        > [Lexicographic order](https://en.wikipedia.org/wiki/Lexicographic_order)

- binary boolean expressions (opearands must be boolean values):
    - logical and `&&`
    - logical or `||`

>[!NOTE]
> boolean expressions implicit convert to `1` if `true` or `0` if `false` inside math expressions,
>
> ```kay
> 1 + true; # -> 2: 1 + true -> 1 + 1
> 1 + false; # -> 1: 1 + false -> 1 + 0
> ```

## Printing

The only way to print values is using the temporary `print` or `println` keywords:

``` kay
var ten = 5 * 2;
let nine = 3 ** 2;
let lucky = ten + nine;

println lucky;
print 42;
println; # omitting the println argument will just print a newline
```

>[!NOTE]
> `print` and `println` print to `stdout`, printing to `stderr` is done using the `eprint` and
> `eprintln` keywords.

## Scopes

They are enclosed by `{` and `}`, and contain a series of statements, where variables are only
accessible in the scope they were defined in:

```kay
let ten = 10;
print "ten = "; println ten;

{
    let nine = 9;
    print "nine = "; println nine;

    {
        let twentyone = nine + ten;
        print "twentyone = "; println twentyone;

        print "nine in the inner scope = "; println nine;
    }

    print "ten in the inner scope = "; println ten;
}

print "ten in the inner scope = "; println ten;

# Error: "nine" was not defined in this scope
print "nine in the inner scope = "; println nine;
```

## If statements

Executes a block of code based on a condition

- if statement:

    ```kay
    let lucky = 42;
    if lucky == 19 {
        println "well done!";
    }
    ```

- if-else statement:

    ```kay
    let lucky = 42;
    if lucky == 19 {
        println "well done!";
    }
    else {
        println "too bad!";
    }
    ```

- if, else-if, else statement:

    ```kay
    let lucky = 42;
    if lucky == 19 {
        println "well done!";
    }
    else if lucky == 42 {
        println "awesome!";
    }
    else {
        println "too bad!";
    }
    ```

- single statement version:

    ```kay
    let lucky = 42;
    if lucky == 19 do println "well done!";
    else if lucky == 12 do { println "nice"; } # Error: blocks are not allowed in do statements
    else if lucky == 42 do println "awesome!";
    else do println "too bad!";
    ```

## Loops

Executes a block until a condition is not satisfied

```kay
var i = 0;
loop i < 10 {
    println i;
    i += 1;
}
```

can also make use of the do statement syntax:

```kay
var i = 0;
loop i < 10 do i += 1;
println i;
```

### C-style do-while loops

C-style do-while loop can be used to let the body/statement of the loop run at least once:

```kay
var i = 0;
loop false do i += 1;
println i; # will print 0 since the increment inside the loop was never executed

var j = 0;
do loop false do j += 1;
println j; # will print 1 since the increment inside the loop was executed at least once
```

### break and continue statements

They can be used to alter the normal flow of the loop:

```kay
var i = 0;
loop i < 10 {
    # Warning: this will never reach the increment statement, thus creating an infinite loop
    if i == 3 do continue;

    # Correct way of skipping an iteration
    if i == 4 {
        i += 1; # incrementint the loop counter variable to avoid an infinite loop
        continue;
    }

    # numbers 7, 8, 9 will not be printed
    if i == 6 do break;

    println i;
    i += 1;
}
```
