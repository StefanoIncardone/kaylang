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

### Command to interpret, run or compile the program

- `interpret` to interpret the program without compiling it
- `build` to compile the program down to a binary executable
- `run` to compile and run the compiled native assembly code

## 0.1

- line comments: `#...`
- 64 bit unsigned base 10 number literals
- basic math expression: `+`, `-`, `*`, `/`, `^`
- each expression is separated by semicolons
- printing of math expressions using the `print` keyword
- printing of numbers interpreted as ascii characters using the `print_char` keyword
