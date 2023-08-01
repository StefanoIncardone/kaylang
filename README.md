# Blitz language

Experimenting with programming languages and exploring how to create one.

## Language Features

see the [full language syntax](SYNTAX.ebnf)

### Run modes

- `interpret`: to interpret the program without compiling it
- `compile`: to compile the program down to a binary executable
- `run`: to compile and run the generated binary executable

### UTF-8 support

only supporting ASCII (`[0, 127]`) characters for now

### Comments

line comments start with `#` and ignore everything until the end of the line

### Integers

Integers are 64 bit base 10 number literals.

### Booleans

Boolean values are `true` and `false`, being functionally equivalent to `1` and `0` when used inside math expressions

### Characters

Character literals are surrounded by `'`.
They can also be used in math expression, being automatically converted to their ASCII code (temporary implicit conversion)

These are the available escaped characters:

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

Note: when used in expressions strings get converted to their length (waiting for proper type checking)

```blitz
print "len of \"lucky\" is "; println "lucky" + 0; # easy way to obtain the length of the string
```

### Expressions

Expressions follow this order of operations (from highest to lowest):

- round brackets `(`, `)`
- power `^`: exponentiation as a binary operator
- times `*` and divide `/`: multiplication and floor division as binary operators
- plus `+` and minus `-`: addition and subtraction as binary operators
- boolean comparisons (evaluates to `1` or `0`):
    - equals to: `==`
    - not equals to: `!=`
    - greater than: `>`
    - greater or equals than: `>=`
    - less than: `<`
    - less or equals than: `<=`
    - compared to: `<=>` (`-1` if less than, `0` if equals, `1` if greater)

### Variables

Variable names can be made of (but not starting with) numbers, underscores and letters

- `let`: immutable variable
- `var`: mutable variable

``` blitz
var ten = 5 * 2;
let nine = 3 ^ 2;
```

*op*_assign operators desugar to regular expressions

```blitz
var twentyone = 9;
twentyone += 10; # equivalent to `twentyone = twentyone + 10`
```

### Printing

The only way to print values is using the temporary intrinsic `print` or `println` keywords

``` blitz
var ten = 5 * 2;
let nine = 3 ^ 2;
let lucky = ten + nine;

println lucky;
print 42;
println; # this will just print a newline
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

### If statements

Executes a block of code based on a condition

- if statement:

    ```blitz
    let lucky = 42;
    if lucky == 19 {
        println "well done!";
    }
    ```

- if-else statement:

    ```blitz
    let lucky = 42;
    if lucky == 19 {
        println "well done!";
    }
    else {
        println "too bad!";
    }
    ```

- if, else-if, else statement:

    ```blitz
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
