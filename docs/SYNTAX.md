# Language Syntax

see the [full language syntax](SYNTAX.ebnf)

## Integers

64 bit unsigned base 10 number literals

## Comments

Line comment ignores everything until the end of the line: `#...`

## Operators

- plus `+`:
    1. addition as a binary operator
- minus `-`:
    1. subtraction as a binary operator
- times `*`:
    1. multiplication as a binary operator
- divide `/`:
    1. floor division as a binary operator
- power `^`:
    1. exponentiation as a binary operator
- round brackets `(`, `)`:
    1. changing the order or operation

## Printing to screen

- integer expressions can be printed using the `print` keyword
- integer numbers can be printed as character using the `print_char` keyword (values over 255 will wrap to 0)
