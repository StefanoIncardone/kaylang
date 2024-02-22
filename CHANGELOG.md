# Changelog

All notable changes to this project will be documented in this file

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html)

## Unreleased

## 0.5.0 - 2024-02-23

### Added

- Static arrays

### Changed

- Unneccessary abstraction over the entire API related to the compilation process
- Assembly code generation

### Removed

- Implicit conversions between strings and other types

### Fixed

- Corrected bugs related to implicit conversions


## 0.4.2 - 2023-12-02

### Added

- Runtime errors for division by zero, taking the modulo 0 and raising to a negative power

## 0.4.1 - 2023-11-29

### Changed

- revised compiler API
- revised and optimized error reporting during compilation steps
- optimized String allocations
- cleaned ast genereation code

## 0.4.0 - 2023-11-03

### Added

- New operators

### Changed

- Old operators
- Renamed language to `kay`


## 0.3.2 - 2023-11-03

### Added

- do-loop loops (i.e.: C-style do-while loops)

## 0.3.1 - 2023-10-31

### Added

- Type annotations
- Default values

## 0.3.0 - 2023-10-29

### Added

- Printing of the compiler version
- Compilation output folder

### Changed

- renamed `for` keyword to `loop`
- `do` keyword for single statements instead of `:`

### Removed

- `interpret` mode
- Blocks in single statement contexts


## 0.2.6 - 2023-10-20

### Added

- single statement if statement and for loops: `do` keyword

## 0.2.5 - 2023-10-14

### Added

- UTF-8 support in comments

## 0.2.4 - 2023-08-28

### Added

- `check` mode to just check for program correctens

## 0.2.3 - 2024-08-28

### Added

- Unary operators

## 0.2.2 - 2024-08-16

### Added

- C-style while loops: `for` keyword

## 0.2.1 - 2024-08-01

### Added

- In-place math assignments (i.e.: `i += 1`)

## 0.2.0 - 2024-08-01

### Fixed

- Variable reassignment bug

### Removed

- `const` variables


## 0.1.9 - 2023-07-30

### Added

- if, else-if, and else statements

## 0.1.8 - 2023-07-17

### Added

- Scopes

## 0.1.7 - 2023-07-08

### Added

- String literals

## 0.1.6 - 2023-07-01

### Added

- Mutable variables: `var`
- `println` keyword

## 0.1.5 - 2023-06-28

### Added

- Booleans, `true` and `false` are now keywords
- Comparison operators

## 0.1.4 - 2023-06-06

### Added

- immutable variables: `let`
- compile time evaluated variables: `const`

## 0.1.3 - 2023-05-22

### Added

- character literals

## 0.1.2 - 2023-05-19

### Added

- `interpret` mode to interpret the program without compiling it
- `compile` mode to compile the program down to a binary executable
- `run` mode to compile and run the compiled native assembly code

## 0.1.1 - 2023-05-14

### Added

- printing of numbers as ascii characters
- order of operations and parenthesis in math expressions

## 0.1.0 - 2023-05-08

### Added

- only accepting ASCII characters
- line comments starting with `#`
- basic math expressions parsing (no order of precedence), evaluation and printing
