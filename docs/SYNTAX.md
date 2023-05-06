# Language Syntax

The language is currently only capable of recognizing individual tokens, it doesn't actually recognize semantics errors.
(see the [full language syntax](SYNTAX.ebnf))

## Integers

64 bit positive base 10 number literals

## Comments

Line comment ignore everything until the end of the line: `#...`

## Operators

- plus `+`:
    1. addition as a binary operator
    2. positive number as a unary operator
- minus `-`:
    1. subtraction as a binary operator
    2. negative number as a unary operator
- times `*`:
    1. multiplication as a binary operator
- divide `/`:
    1. division as a binary operator
- power `^`:
    1. exponentiation as a binary operator
