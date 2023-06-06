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

Character literals are surrounded by `'`

``` blitz
'\n'; # newline
'\t'; # tab
'\''; # single quote
'\"'; '"' # double quote
```

### Variables

Variable names can be any made of (but not starting with) numbers, underscores and letters

``` blitz
let number = 19;
let newline = '\n';
```

### Printing

The only way to print values is using the temporary intrinsic `print` keyword

``` blitz
let ten = 10;
let nine = 9;
let lucky = ten + nine;
let newline = '\n';
print 47;
print lucky;
print newline;
```
