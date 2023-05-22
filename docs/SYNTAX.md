# Language Syntax

see the [full language syntax](SYNTAX.ebnf)

## Comments

Line comment `#...`: ignores everything until the end of the line

## Integers and Math Expressions

- can be printed using the `print` keyword
- 64 bit unsigned base 10 number literals
- Math expressions following PEMDAS:
    - round brackets `(`, `)`: changing the order or operation
    - power `**`: exponentiation as a binary operator
    - times `*`: multiplication as a binary operator
    - divide `/`: floor division as a binary operator
    - plus `+`: addition as a binary operator
    - minus `-`: subtraction as a binary operator

## Characters

- can be printed using the `print_char` keyword
- any Extended ASCII (`[0, 255]`) character surrounded by `'`
    - `'\n'`: newline
