# Versions

## 0.4

- renamed language to `kay`
- revised and added operators

## 0.4.1

- revised compiler API and failure during compilation steps

## 0.3

### 0.3.2

- do-loop loops (i.e.: C-style do-while loops)

### 0.3.1

- type annotations
- default values

### 0.3.0

- removed `interpret` mode
- improved cli
    - added printing of the compile version
    - added compilation output folder
- renamed `for` keyword to `loop`
- introduced the `do` keyword for single statements instead of `:`
    - disallowed blocks in single statement contexts

## 0.2

### 0.2.6

- single statement if statement and for loops: `do` keyword

### 0.2.5

- UTF-8 comments

### 0.2.4

- `check` mode to just check for program correctens

### 0.2.3

- unary operators

### 0.2.2

- while loops: `for` keyword

### 0.2.1

- introduced in-place math assignments (i.e.: `i += 1`)

### 0.2.0

- removed const variables
    - corrected variable reassignment bug

## 0.1

### 0.1.9

- if, else-if, and else statements

### 0.1.8

- scopes

### 0.1.7

- string literals

### 0.1.6

- mutable variables: `var`
- `println` keyword

### 0.1.5

- booleans, `true` and `false` are now keywords
- comparison operators

### 0.1.4

- immutable variables: `let`
- compile time evaluated variables: `const`

### 0.1.3

- character literals

### 0.1.2

- `interpret` mode to interpret the program without compiling it
- `compile` mode to compile the program down to a binary executable
- `run` mode to compile and run the compiled native assembly code

### 0.1.1

- printing of numbers as ascii characters
- order of operations and parenthesis in math expressions

### 0.1.0

- only accepting ASCII characters
- line comments starting with `#`
- basic math expressions parsing (no order of precedence), evaluation and printing
