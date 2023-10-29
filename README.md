# Blitz language

Experimenting with programming languages and exploring how to create one.

**IMPORTANT**: The language could change at any moment for now

## Language Features

see the [full language syntax](SYNTAX.ebnf)

### Run modes

- `compile`: to compile the program down to a binary executable
- `run`: to compile and run the generated binary executable

### UTF-8 support

only supporting ASCII (`[32, 126]`) characters for now, UTF-8 characters are only supported in comments

### Comments

line comments start with `#` and ignore everything until the end of the line

### Integers

Integers are 64 bit base 10 number literals.

### Booleans

Boolean values are `true` and `false`, being functionally equivalent to `1` and `0` when used inside math expressions

### Characters

Character literals are surrounded by `'`: `'B'`
They can also be used in math expression, being automatically converted to their ASCII code (temporary implicit conversion)

These are the available escaped characters:

- backslash: `\\`
- single quote: `\'`
- double quote: `\"` or `"`
- newline: `\n`
- carriage return: `\r`
- tab: `\t`
- null character: `\0`

### Strings

String literals are surrounded by `"` and contain any number of regular or escaped characters: `"Hello\nWorld!"`

### Expressions

Expressions follow this order of operations (precedence from highest to lowest):

- minus `-`, not `!`: integer negation and boolean inversion as unary operators
- round brackets `(`, `)`
- power `**`: exponentiation as a binary operator
- times `*`, divide `/`, remainder `%`: multiplication, floor division and remainder as binary operators
- plus `+` and minus `-`: addition and subtraction as binary operators
- comparisons:
    - comparison operator `<=>`: `-1` if less than, `0` if equals, `1` if greater
    - boolean comparisons (evaluate to `1` or `0` when inside math expressions, otherwise to `true` and `false`):
        - equals to: `==`
        - not equals to: `!=`
        - greater than: `>`
        - greater or equals than: `>=`
        - less than: `<`
        - less or equals than: `<=`
- boolean expressions (evaluates to `1` or `0` when inside match expressions):
    - opearands must be boolean values, i.e.: `1 and 2` is not a valid boolean expression
    - logical and: `&&`
    - logical or: `||`
    - logical xor: `^^`

Note: when using strings in expressions they get converted to their length (waiting for proper type checking)

```blitz
print "length of \"lucky\" is "; println "lucky" + 0; # easy way to obtain the length of the string
```

### Variables

Variable names can be made of (but not starting with) numbers, underscores and letters

- `let`: immutable variable
- `var`: mutable variable

``` blitz
let nine = 3 ** 2;
let ten: int = 5 * 2; # optional type hint
let twenty_one: ten = nine + ten; # twenty one is of the same type as "ten"
```

*op*_assign operators desugar to regular expressions

```blitz
var twentyone = 9;
twentyone += 10; # equivalent to "twentyone = twentyone + 10;"
```

### Printing

The only way to print values is using the temporary intrinsics `print` or `println` keywords

``` blitz
var ten = 5 * 2;
let nine = 3 ** 2;
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

# this will result in an error because "nine" was not defined in this scope
print "nine in the inner scope = "; println nine;
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

- single statement version:

    ```blitz
    let lucky = 42;
    if lucky == 19: println "well done!";
    else if lucky == 42: println "awesome!";
    else: println "too bad!";
    ```

### Loops

Executes a block until a condition is not satisfied

```blitz
var i = 0;
for i < 10 {
    println i;
    i += 1;
}
```

can also make use of the single statement feature:

```blitz
var i = 0;
for i < 10: i += 1;
println i;
```

#### break and continue statements

They can be used to alter the normal flow of the program

```blitz
var i = 0;
for i < 10 {
    if i == 6: break;

    println i;
    i += 1;
}
```

### Semicolons

Semicolons are reguired after statements.

```blitz
print "Hello "; # required semicolon
println "World!";
```

They are optional if they come before a closing curly bracket (i.e. the last statement before the end of a scope),
to allow for cleaner code like this:

```blitz
var i = 0;
for i < 100 {
    if i % 15 == 0 { println "fizzbuz" }
    else if i % 3 == 0 { println "fizz" }
    else if i % 5 == 0 { println "buzz" }
    else { println i }

    i += 1; # this semicolon is also optional
}
```
