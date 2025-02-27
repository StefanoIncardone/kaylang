# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html),
but may switch to [CalVer Versioning](https://calver.org/) in the future.

## Unreleased

- Shortcircuted and/or operators
- Unsigned integers
- Casting operator
- Removal of implicit conversions
- Type aliases, which are just alternative names to existing types:

    ```kay
    alias byte = u8;
    ```

- Distinct types, which are considered entirely different types:

    ```kay
    type byte = u8;
    ```

## 0.6.2 - 2025-02-27

### Language

#### Changed

- Empty binary/octal/hexadecimal numbers literals (`0b`, `0o`, `0x`) are no longer considered syntax
    errors and now mean `0`
- Made block comments delimited by `##` instead of `#{` and `#}`, this also removes the error
    related to uclosed block comments:

    - with opening `#{` and closing `}#` or `#}`:

        ```text
        println #{ symmetric }# 21; # looks symetric with `}#`

        # look asymetric with `}#`
        #{
        asymmetric
        }#

        println #{ asymmetric #} 21; # looks asymetric with `#}`

        # look symetric with `#}` 
        #{
        symmetric
        #}
        ```

    - with `##`:

        ```kay
        # both single line and multiline block comments look symmetric
        println ## symmetric ## 12;

        ##
        symmetric
        ##
        ```

#### Removed

- Removed do-statements in if and loop statements, thus reduced language complexity and
    inconsistencies

### Compiler

#### Added

- Added information about error's absolute source code column
- Added `color::ansi_code` type alias and `color::AnsiCode` enum for text modifiers codes

#### Changed

- Update rust version to [1.81.0](https://releases.rs/docs/1.81.0/)
- Reworked compilation stages:
    - old:
        - loading of source code file and line boundaries precalculations
        - tokenization
        - abstract syntax tree parsing
        - compilation of abstract syntax tree
    - new:
        - loading of source code file
        - tokenization and line boundaries calculations
        - (added) parsing of syntax tree (phantom stage, does not affect other stages for now)
        - abstract syntax tree parsing
        - compilation of abstract syntax tree
        - return the compiled code
- Renamed `syntax` module to `front_end`
- Introduced `offset32`, `line32`, `column32` and `index32` type aliases for `u32`
- Errors related to bracket pairs now contain more descriptive `tokenizer::OpenBracket` and
    `tokenizer::CloseBracket`
- Moved `src_file` module into `front_end`
- `src_file::SrcFile::path` and `src_file::Error::path` are now a `&Path` instead of `PathBuf`
- Split `src_file::SrcFile::position()` into `src_file::SrcFile::position()` and
    `src_file::SrcFile::display_position()`:
    - `src_file::SrcFile::position()` is no longer public due to out-of bounds unsafety and
        inconsistencies between Unix's `\n` and Windows' `\r\n` line terminators
    - `src_file::Position` now only contains information about the sorce code position
    - new `src_file::DisplayPosition` information about the source code position and
        display position
- Split `src_file::SrcFile` into:
    - `src_file::SrcFile`: the path and the source code
    - `src_file::SrcCode`: contains `src_file::SrcFile` and `Vec<Line>`
- `src_file::SrcFile::load()` now only reads the contents of the source code without calculating
    line bounds
- `tokenizer::Tokens::tokenize()` now takes `src_file::SrcFile` and returns the new
    `tokenizer::TokenizedCode` struct
- Renamed `error::MsgWithCauseUnderTextWithLocation::source_code_col` to
    `error::MsgWithCauseUnderTextWithLocation::absolute_column`
- Reordered and changed `error::MsgWithCauseUnderText::pointers_count` and
    `error::MsgWithCauseUnderText::pointers_offset` from `usize` to `column32`
- Renamed `compiler` module to `back_end`
- `back_end::Compiler::compile()` no longer immediately writes the compiled code, it now returns the
    compiled code as a `String`
- `artifacts::Artifacts::new()` and `artifacts::Artifacts::new_with_out_path()` now take a `&Path`
    instead of `&SrcFile`
- Renamed `color::Flags` to `color::flag`
- Struct `color::Flag` was only used as a namespace, so its now an enum named `color::AnsiFlag`
- Improved compilation stages and variables naming consistency
- Renamed `BUILDING_UNTYPED_AST` to `PARSING_SYNTAX_TREE`
- Renamed `BUILDING_AST` to `PARSING_AST`

#### Fixed

- Fixed typo in compilation steps names
- Improved error messages related to utf8 characters
- Optimized colored output

## 0.6.1 - 2024-09-20

### Language

#### Added

- `_` as a digit separator, e.g.: `123_456_678` is now a valid number literal
- Alternative number literals bases:
    - `0b`: binary
    - `0o`: octal
    - `0x`: hexadecimal
- Escape sequences in raw string literals: `r"nested \"quotes\" are now allowed by escaping them"`
- Block comments, enclosed by `#{` and `#}`

#### Fixed

- Added missing `break` and `continue` statements to the [syntax specification](SYNTAX.ebnf)

### Compiler

#### Added

- Created and exposed API for error messages, introduced the `error` module

#### Changed

- Restricted max source file size to 4GB
- Restricted max identifiers length to 63 characters
- `src_file::Error` now contains more detailed error kinds
- Imported from `core` instead of `std` where possible
- `Logger` methods now accept `&dyn Display`

#### Fixed

- Corrected error messages related to:
    - undefined variables and variables already defined
    - integers, identifiers and string, raw_string and character literals tokenization
- Temporary values are now properly displayed in the generated assembly comments
- Bug that allowed indexing into non-variables
- Moved Vscode extension to own [repo](https://github.com/StefanoIncardone/kaylang_vscode)

## 0.6.0 - 2024-08-31

>[!WARNING]
> From version `0.6.0` breaking changes may appear at any time, meaning that features in `0.6.0`
> could not work or could be totally **removed** in `0.6.1`.
> This is to prevent an explosion in version numbers and the stagnation of **breaking** feature
> updates, since breaking and non-breaking changes will be frequent.
> Breaking changes outside of the `Removed` section will be made obvious for easy recognition.
> Changes to the ABI would **not** be considered breaking since no stable ABI is defined for now.
> Changes to error messages, help message and alike would **not** be considered breaking.

### Added

- Arrays indexing assigning
- String and Arrays comparison operators: `<=>`, `==`, `!=`, `<`, `<=`, `>`, `>=`
- String and Arrays `len` operator: `len "hello" # -> 5`
- Raw string literals, i.e.: `r"hello\nworld"` where `\n` would not get escaped
- `+` as the unary absolute value operator
- overflowing operators, will wrap in two's complemente: `...\` (e.g.: `*` -> `*\`)
- saturating operators: `...|` (e.g.: `*` -> `*|`)
- Printing to `stderr`: `eprint` and `eprintln` keywords
<!-- -->
- `help` and `version` commands
- usage examples
- getters for `SrcFile` field: `SrcFile::path()`, `SrcFile::code()` and `SrcFile::lines()`
- [local vscode syntax highlighting extension](https://github.com/StefanoIncardone/kaylang_vscode)

### Changed

- Temporary arrays and indexing into temporary values are now allowed
- Improved generated asm code
- Improved error messages
- Runtime crashes now print to `stderr`
<!--  -->
- help message now prints the name of the executable that run the command
- Allowed non utf-8 paths in cli arguments
- `run` command now returns the exit code of the executed program
- `compile` command now returns the exit code of the assembler and linker in the case of a failure
- Improved logging in case of failures with `run` and `compile` commands
- An error is now returned when no executable name is provided when parsing arguments in the form of
    `Vec<String>`
- Generated assembly file does not get cleared in the case of a compile crash
- Improved documentation
- Renamed `src_file::Line` to `src_file::Span` but kept `src_file::Line` as a type alias

#### Breaking

- `<=>` now has the same precedence as `==`, `!=`, `<`, `<=`, `>`, `>=` operators
- non-wrapping and non-saturating operators will crash on overflow
- `char` type renamed to `ascii`
- Arrays are now mandated to contain at least 2 items, so the `Type::Infer` type is no longer
    needed:
    - arrays of 0 items are meaningless, they don't even occupy any memory
    - arrays of 1 element are literaly just that element with extra steps
- number literal `-0` is no longer allowed, since it is not a valid two's complement number
<!--  -->
- `Ast` renamed to `Parser`, and `Ast::build` to `Parser::parse`
- `Parser::parse` now returns an `Ast` instead of a `Vec<Scope>`
- `Scope` is no longer public
- Pulled `BaseType` enum out of `Type` enum
- `Tokenizer::tokenize` and `Parser::parse` now return `Error`s that can be displayed
    with the `ErrorDisplay` using the `display` method, or alternatively error messages, error cause
    messages, line column and line text information can be obtained separately with the `ErrorInfo`
    struct and `SrcFile::position` method
- `Compiler::compile`
    - requires `SrcFile` to be explicitly provided
    - no longer returns generic `PathBuf`s for assembly, object and executable files
    - now requires an `Artifacts` struct instead of generic `PathBuf`s for source and out directory
        path
- Updated consistency of compilation steps variable names
- Compilation steps are now managed by the `Logger` struct
- verbosity options are now mandated to come after a `check`, `compile` or `run` commands as the
    last option
- `verbosity` field of `Args` struct is now integrated in `Command::Check`, `Command::Compile` and
    `Command::Run` variants, thus removed

### Removed

- indexing into `int` values
- usage of `*op*=` operators on `ascii` and `bool` values
<!--  -->
- `Error`, `ColoredString` and `ColoredStr` type aliases, to allow for more explicit type signatures
- `logging` and `cli` modules inlined inside the root `kaylang` module, thus removed
- `assembler`, `linker` and `run` modules integrated in the new `compiler::artifacts` module.
    `Assembler::assemble`, `Linker::link` and `Run::run` are now substituted by
    `Artifacts::assembler`, `Artifacts::linker` and `Artifacts::runner` member functions and now
    return the relative `std::process::Command` to allow for finer control

### Fixed

- Bug in string literals compilation when escaped characters where present
- Bug in expressions code generations
- Bug in crash error messages printing
- Bug in parsing of do-loop statements
- Bug in result of division/remainder assembly code that would crash when dividing INT_MIN by -1
- Bug when using the `run` command without a `-o` option
- Bug in parsing of negative numbers that would not allow for literal INT_MIN to be represented,
    i.e.: `-9223372036854775808` would not be allowed
- Bug in character literals tokenization that would not emit intermediate errors


## 0.5.3 - 2024-03-02

### Added

- Traits for defining specific `ErrorKind` enums

### Changed

- `ErrorKind` enums have more clear trait bounds, such as Debug and Clone (where possible)
- Internal code structure improvements
- Improvements to error messages when tokenizing UTF-8 characters

### Deprecated

- `Error`, `ColoredString` and `ColoredStr` type aliases: removed in favor of more explicit type
signatures
- Syntax errors will now be returned in a wrapper around a lightweight 'raw' form and methods are
going to be provided to lazily construct full syntax errors

## 0.5.2 - 2024-02-26

### Added

- Accessing of individual bits for integer values

### Changed

- Improved do-loop code generation

### Fixed

- Corrected code generation related to shift operations

## 0.5.1 - 2024-02-25

### Added

- String characters indexing

### Fixed

- Disallowed strings and arrays in math expressions

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

- Runtime errors for division by zero, taking the remainder of a number divided by 0 and raising to
    a negative power

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

## 0.2.3 - 2023-08-28

### Added

- Unary operators

## 0.2.2 - 2023-08-16

### Added

- C-style while loops: `for` keyword

## 0.2.1 - 2023-08-01

### Added

- In-place math assignments (i.e.: `i += 1`)

## 0.2.0 - 2023-08-01

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
