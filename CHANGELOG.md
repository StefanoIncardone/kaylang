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

### Unchecked/Checked and signed/unsigned operators

- unchecked (+, -, /, ... ): overflow will wrap, division by zero will crash
- checked (c+, c-, c/, ...): added code to check for overflows/division by zero

| signed | unsigned |
| :----: | :------: |
|   +    |    ++     |
|   -    |    --     |
|   *    |   ???    |
|   /    |    //    |
|   %    |    %%    |
|   /%   |   //%    |
|   **   |   **    |


## 0.1

- `build` to compile the program down to a binary executable
- `run` to compile and run the compiled native assembly code
- `interpret` to interpret the program without compiling it
- line comments: `#...`
- each statement is separated by semicolons
- 64 bit unsigned base 10 number literals
- math expressions (PEMDAS): `+`, `-`, `*`, `/`, `**`
- printing of value of math expressions using the `print` keyword
- character literals:
    - `\n`: newline
    - any Extended ASCII character
- printing of characters using the `print_char` keyword
