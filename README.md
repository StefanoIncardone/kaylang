# Kay language

Experimenting with programming languages and exploring how to create one.

**IMPORTANT**: The language could change at any moment for now

see the [full language syntax](SYNTAX.ebnf)

## Run modes

- `check`: to check the source code for correctness
- `compile`: to compile the program down to a binary executable
- `run`: to compile and run the generated binary executable

## UTF-8 support

only supporting ASCII (`[32, 126]`) characters for now, UTF-8 characters are only supported in comments

## Comments

line comments start with `#` and ignore everything until the end of the line

## Integers

Integers are 64 bit base 10 number literals.

## Booleans

Boolean values are `true` and `false`, being functionally equivalent to `1` and `0` when used inside math expressions

## Characters

Character literals are surrounded by `'`: `'B'`
These are the available escaped characters:

- backslash: `\\`
- single quote: `\'`
- double quote: `\"` or `"`
- newline: `\n`
- carriage return: `\r`
- tab: `\t`
- null character: `\0`

## Strings

String literals are surrounded by `"` and contain any number of regular or escaped characters: `"Hello\nWorld!"`

## Expressions

**Note** boolean expressions implicit convert to `1` if `true` or `0` if `false` inside math expressions,

```kay
println 1 + true; # will print 2 (1 + true -> 1 + 1)
println 1 + false; # will print 1 (1 + false -> 1 + 0)
```

Expressions follow this order of operations (precedence from highest to lowest):

- unary minus `-`, unary not `!`: integer negation and boolean inversion/bitwise not as unary operators
- round brackets `(`, `)`
- binary exponentiation `**`
- binary multiplication `*`, binary division `/` and binary remainder `%`
- binary addition `+` and binary subtraction `-`
- binary left shift `<<` and binary right shift `>>`: `1 << 2 # 0001 << 2 == 0100`
- binary bitwise and `&`
- binary bitwise xor `^`
- binary bitwise or `|`
- binary comparison `<=>`: `-1` if less than, `0` if equals, `1` if greater
- binary boolean comparisons (cannot be chained, i.e.: `3 > 2 > 1` is not a valid boolean expression):
    - equals to `==`
    - not equals to `!=`
    - greater than `>`
    - greater or equals than `>=`
    - less than `<`
    - less or equals than `<=`
- binary boolean expressions (opearands must be boolean values):
    - logical and `&&`
    - logical or `||`
- all *op*= variations (i.e.: `+=`, `-=`, ...)


## Variables

Variable names can be made of (but not starting with) numbers, underscores and letters

- `let`: immutable variable
- `var`: mutable variable

*op*_assign operators desugar to regular expressions:

```kay
var twentyone = 9;
twentyone += 10;    # equivalent to "twentyone = twentyone + 10;"
```

type annotations:

``` kay
let ten: int = 5 * 2;               # explicit type annotation
let nine = 3 ** 2;                  # implicit type annotation lets the compiler infer the type, which will be int in this case
let twenty_one: ten = nine + ten;   # twenty one is of the same type as "ten"
```

default values:

``` kay
let error;      # will result in an error asking for an explicit type annotation
let zero: int;  # will have the default value for a variable of type int, which is 0
```

## Printing

The only way to print values is using the temporary intrinsics `print` or `println` keywords

``` kay
var ten = 5 * 2;
let nine = 3 ** 2;
let lucky = ten + nine;

println lucky;
print 42;
println; # this will just print a newline
```

## Scopes

They are enclosed by `{` and `}`, and contain a series of statements
variables are only accessible in the scope they were defined in

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

# this will result in an error because "nine" was not defined in this scope
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

can also make use of the single statement feature:

```kay
var i = 0;
loop i < 10 do i += 1;
println i;
```

C-style do-while loop can be used to let the body/statement of the loop to run at least once:

```kay
var i = 0;
loop false do i += 1;
println i; # will print 0 since the increment inside the loop was never executed

var j = 0;
do loop false do j += 1;
println j; # will print 1 since the increment inside the loop was executed at least once
```

### break and continue statements

They can be used to alter the normal flow of the program

```kay
var i = 0;
loop i < 10 {
    if i == 6 do break;

    println i;
    i += 1;
}
```

## Semicolons

Semicolons are reguired after statements.

```kay
print "Hello "; # required semicolon
println "World!";
```

## Arrays

Arrays are a collection of values of the same type under a single variable

```kay
let i = [1, 2, 3, 4];
println i[2]; # zero-based indexing

println [1, 2, 3]; # Error: temporary arrays are not supported yet, so they need to be extracted into variables first

let hello = "hello";
println hello[2]; # strings can be indexed to access individual characters
```

### Bit field access

Integers can be treated as bit arrays and can therefore be indexed like regular arrays, where the
value of the indexed type is an other integer with at most a single bit set to 1

```kay
let five = 5; # 0101
println five[0]; # 0001 -> 1
                 # 0101
                 #    ^

println five[1]; # 0000 -> 0
                 # 0101
                 #   ^

println five[2]; # 0100 -> 4
                 # 0101
                 #   ^
```
