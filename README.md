# Blitz language

Experimenting with programming languages and exploring how to create one.
This language is inspired by many languages, with the most notable being [Rust](https://www.rust-lang.org/).


## Language Features

see the [full language syntax](SYNTAX.ebnf)

### Run modes

- `build`: to compile the program down to a binary executable
- `run`: to compile and run the compiled native assembly code
- `interpret`: to interpret the program without compiling it

### UTF-8 support

only supporting ASCII (`[0, 127]`) characters for now

### Comments

line comment `#...`: ignores everything until the end of the line

### Booleans

Boolean values are `true` and `false`, being functionally equivalent to `1` and `0` when used inside math expressions

- boolean comparisons (evaluates to `1` or `0`):
    - equals to: `==`
    - not equals to: `!=`
    - greater than: `>`
    - greater or equals than: `>=`
    - less than: `<`
    - less or equals than: `<=`
    - compared to: `<=>` (`-1` if less than, `0` if equals, `1` if greater)

### Integers and Math Expressions

Integers are 64 bit base 10 number literals.
Math expressions follow the PEMDAS order of operations:

- round brackets `(`, `)`: changing the order or operation
- power `^`: exponentiation as a binary operator
- times `*`: multiplication as a binary operator
- divide `/`: floor division as a binary operator
- plus `+`: addition as a binary operator
- minus `-`: subtraction as a binary operator

### Characters

Character literals are surrounded by `'`.
They can also be used in math expression, being automatically converted to their ASCII code (temporary implicit conversion)

``` blitz
'\\'; # backslash
'\n'; # newline
'\t'; # tab
'\''; # single quote
'\"'; '"' # double quote
```

### Strings

String literals are surrounded by `"`.

```blitz
let lucky = "nineteen";
println lucky;
```

### Variables

Variable names can be any made of (but not starting with) numbers, underscores and letters

``` blitz
var ten = 5 * 2;
let nine = 3 ^ 2;
const nineteen = ten + nine;
```

- `const`: compile time avaluated immutable variable
- `let`: immutable variable
- `var`: mutable variable

### Printing

The only way to print values is using the temporary intrinsic `print` or `println` keywords

``` blitz
var ten = 5 * 2;
let nine = 3 ^ 2;
const lucky = ten + nine;

println lucky;
print 42;
```

### Scopes

They are enclosed by `{` and `}`, and contain a series of statements
variables are only accessible in the scope they were defined in

```blitz
let ten = 10;
print "ten = ";
println ten;

{
    let nine = 9;
    print "nine = ";
    println nine;

    {
        let twentyone = nine + ten;
        print "twentyone = ";
        println twentyone;

        print "nine in the inner scope = ";
        println nine;
    }

    print "ten in the inner scope = ";
    println ten;
}

print "ten in the inner scope = ";
println ten;

# this will result in an error because "nine" was not defined in this scope
# print "nine in the inner scope = ";
# println nine;

```
