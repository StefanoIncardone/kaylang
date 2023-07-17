# Versions

## Feature Ideas

### Language version embedded in file extension

From [Fortran](https://www.cita.utoronto.ca/~merz/intel_f10b/main_for/mergedProjects/bldaps_for/common/bldaps_under_inpext.htm#:~:text=Typical%20Fortran%20source%20files%20have,f.)

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

### Operators

- Absolute value:
    - enclosed by a `|`

    ```blitz
    |19| == 19
    |-19| == 19
    ```

- Floor:
    - enclosed by `|\` and `/|`, and have to be written as a single token, so `| \` and `/ |` are not valid

    ```blitz
    |\19.3/| == 19
    |\-19.3/| == -20
    ```

- Ceil:
    - enclosed by `|/` and `\|`, and have to be written as a single token, so `| /` and `\ |` are not valid

    ```blitz
    |/19.3\| == 20
    |/-19.3\| == -19
    ```

### Strings

- immutable strings are surrounded by `"`: `"hello world"`
- mutable strings (like string builders) are surrounded by `` ` ``: `"hello world"`
- unescaped strings are prefixed by a `u`: `u"\this wo\n't be escaped`
- multiline strings are prefixed by a `m`:
    - lines will have newline characters appended to them unless they end in a `\`, which can be escaped using a `\\`
    - whitespace will be preserved (except before the closing quote) and leading whitespace is calculated based on the
        position of the closing quote, or by the text furthest to the left.  
- formatted strings are prefixed by a `f`: `f"the answer is {40 + 2}"`
- options can appear in any order right before the opening quote, but only once:
    - `fum"`, `fu"`, `um"` are valid
    - `fuum`, `ff "`, `u "` are not valid


## 0.1

- only supporting ASCII characters (`[0, 127]`) for now
- `build` to compile the program down to a binary executable
- `run` to compile and run the compiled native assembly code
- `interpret` to interpret the program without compiling it
- line comments: `#...`
- integers and math operators, character literals, string literals, boolean values and comparisons, variables
- values can be printed using the `print` or `println` (printing a newline character afterwards) keywords
- scopes, where variables are only accessible in the scope where they were defined
