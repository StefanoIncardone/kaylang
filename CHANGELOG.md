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

### Unchecked/Checked

- unchecked (+, -, /, ... ): overflow will wrap, division by zero will crash
- checked (c+, c-, c/, ...): added code to check for overflows/division by zero
- maybe have a compiler flag to use checked/unchecked operators

### Absolute value operator

```blitz
|19| == 19
|-19| == 19
```


## 0.1

- only supporting ASCII characters (`[0, 127]`) for now
- `build` to compile the program down to a binary executable
- `run` to compile and run the compiled native assembly code
- `interpret` to interpret the program without compiling it
- each statement is separated by semicolons
- line comments: `#...`
- integers and math operators
- character literals
- variables
- values (integers, characters, variables, ...) can be printed using the `print` keyword
