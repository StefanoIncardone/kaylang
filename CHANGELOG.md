# Versions

## Feature Ideas

### Language version embedded in file extension

From [Fortran](https://www.cita.utoronto.ca/~merz/intel_f10b/main_for/mergedProjects/bldaps_for/common/bldaps_under_inpext.htm#:~:text=Typical%20Fortran%20source%20files%20have,f.)

### Loops

- loops similar to [Odin's for loops](https://odin-lang.org/docs/overview/#for-statement)
- do-for loops, only for C-style while loops
    - cannot be used with infinite loops
    - cannot be used with C-style for loops

### Once keyword

- allow variables to be mutated only once

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

- shift operators for different number bases (i.e. shifting corresponds to multiplying/dividing by the number base)

    ```blitz
    let number_base_10: int = 123; # base 10
    number_base_10 << 1; # 123 << 1 == 1230 (multiplying by 10, desugars to -> * 10)
    number_base_10 >> 2; # 123 >> 2 == 1 (dividing by 10 two times, desugars to -> / (10 * 2))

    let number_base_2: int = 0b1010; # base 2
    number_base_2 << 1; # 1010 << 1 == 10100 (multiplying by 2, uses bit shifting)
    number_base_2 >> 1; # 1010 >> 1 == 101 (dividing by 2, uses bit shifting)
    ```

- let the user specify the shift base

    ```blitz
    let number_base_10: int = 123; # base 10
    number_base_10 <<b2 1; # 123 <<b2 1 == 246 (multiplying by 2, uses bit shifting)
    number_base_10 >>b2 2; # 123 >>b2 2 == 30 (dividing by 2 two times, uses bit shifting)

    let number_base_2: int = 0b1010; # base 2
    number_base_2 <<b10 1; # 1010 <<b10 1 == 01100100 (multiplying by 10, desugars to -> * 10)
    number_base_2 >>b10 1; # 1010 >>b10 1 == 1 (dividing by 10, desugars to -> / 10)
    ```

### Unchecked/Checked

- unchecked (+, -, /, ... ): overflow will wrap, division by zero will crash
- checked (++, --, //, ...):
    - overflow/underflow may either crash or return both the result and the overflow of the addition
    - division will return either the result or an error value
- maybe have a compiler flag to use checked/unchecked operators

### Operators

- absolute value:
    - enclosed by a `|`

    ```blitz
    |19| == 19
    |-19| == 19
    ```

- floor:
    - enclosed by `|\` and `/|`, and have to be written as a single token, so `| \` and `/ |` are not valid

    ```blitz
    |\19.3/| == 19
    |\-19.3/| == -20
    ```

- ceil:
    - enclosed by `|/` and `\|`, and have to be written as a single token, so `| /` and `\ |` are not valid

    ```blitz
    |/19.3\| == 20
    |/-19.3\| == -19
    ```

- is multiple of: `%%`

    ```blitz
    let number = 42;
    if number %% 2 { # desugars to number % 2 == 0
        println "even";
    }
    else {
        println "odd";
    }
    ```

- checking multiple values for equality

    ```blitz
    let n = 42;
    if n == 19 or 21 or 42 { # desugars to n == 19 or n == 21 or n == 42
        println "nice";
    }
    ```

### Strings

- immutable strings are surrounded by `"`: `"hello world"`
- mutable strings (like string builders) are surrounded by `` ` ``: `` `hello world` ``
    - mutability could be infered by the variable's mutability
- raw strings are prefixed by a `r`: `r"\this wo\n't be escaped"`
- multiline strings are prefixed by a `m`:
    - lines will have newline characters appended to them unless they end in a `\`, which can be escaped using a `\\`
    - whitespace will be preserved (except before the closing quote) and leading whitespace is calculated based on the
        position of the closing quote, or by the text furthest to the left.  
- formatted strings are prefixed by a `f`: `f"the answer is {40 + 2}"`
- options can appear in any order right before the opening quote, but only once:
    - `frm"`, `fr"`, `rm"` are valid
    - `frrm`, `ff "`, `r "` are not valid


## 0.1

- `interpret` to interpret the program without compiling it
- `compile` to compile the program down to a binary executable
- `run` to compile and run the compiled native assembly code
- line comments: `#...`
- integers, character literals, string literals, boolean values and variables
- math expressions, boolean comparisons, boolean expressions
- values can be printed using the `print` or `println` (printing a newline character afterwards) keywords
- scopes, where variables are only accessible in the scope where they were defined
- if, if else, if else-if else statements
- for statements
