# Versions

## 0.2

- removed `interpret` mode
- type annotations
- default values
- `for` kewyord renamed to `loop`
- `do-loop`: C-style `do-while` loop
- single statement ifs and loops must be followed by the `do` keyword instead of a colon

## 0.1

- `interpret` to interpret the program without compiling it
- `compile` to compile the program down to a binary executable
- `run` to compile and run the compiled native assembly code
- UTF-8 line comments: `#...`
- integers, character literals, string literals, boolean values and variables
- math expressions, boolean comparisons, boolean expressions
- values can be printed using the `print` or `println` (printing a newline character afterwards) keywords
- scopes, where variables are only accessible in the scope where they were defined
- if, if else, if else-if else statements
- for statements
