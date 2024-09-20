# Change Log

All notable changes to this project will be documented in this file

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html),
but may switch to [CalVer Versioning](https://calver.org/) in the future.

## Unreleased

- Highlighting of operators and symbols

## Known issues

- Highlighting of emojis as errors
- High CPU load when inspecting tokens which contain complex unicode graphemes
- Highlighting of unclosed multiline comments

## 0.1.1 -

### Added

- Highlighting of identifiers over the limit of 63 characters as errors
- Highlighting of `_` integer literals separator
- Highlighting of alternative integer literal bases
- Highlighting of escaped double quotes `\"` in raw string literals
- Highlighting of multiline comments, enclosed by `#{` and `#}`

### Changed

- Pattern now have more descriptive names

### Fixed

- Highlighting of utf8 characters in variable names now does not split variables
- Highlighting of illegal string and character literals, numbers and indeitifiers mimicking compiler
    behaviour (i.e. multiple errors are correctly taken into account and reported all at once)
- Highlighting of array type annotations

## 0.1.0 - 2024-08-30

### Added

- Initial release
- Highlighting of comments
- Highlighting of keywords
- Highlighting of character literals
- Highlighting of string literals
- Highlighting of raw string literals
- Highlighting of integer literals
- Highlighting of variables
- Highlighting of type hints
