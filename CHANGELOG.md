# Versions

## Feature Ideas

### Arbitrary number bases between 1 and 37

| prefix | shorthand | postfix |  base   |         result          |
| :----- | :-------: | :-----: | :-----: | :---------------------: |
| 02_    |   0b/B    |   b2    | base 2  | two's complement binary |
| 08_    |   0o/O    |   b8    | base 8  |          octal          |
| 016_   |   0x/X    |   b16   | base 16 |           hex           |
| 01_    |           |   b1    |         |          error          |
| 04_    |           |   b4    |         |         base 4          |
| 035_   |           |   b35   |         |         base 35         |
| 037_   |           |   b37   |         |          error          |


## 0.1

- run modes:
    - line comments: `#...`
    - 64 bit unsigned base 10 number literals
    - each expression is separated by semicolons
    - enclosing an expression in round brackets promotes its precedence to the highest level
    - `interpret` to interpret the program without compiling it
        - basic math expression: `+`, `-`, `*`, `/`, `^`
        - printing of math expressions using the `print` keyword
        - printing of math expressions interpreted as ascii characters using the `print_char` keyword
    - `build` to compile the program down to a binary executable
        - single addition expressions
        - printing of single addition expressions using the `print` keyword
        - printing of single addition expressions interpreted as ascii characters using the `print_char` keyword
    - `run` to compile and run the compiled native assembly code
