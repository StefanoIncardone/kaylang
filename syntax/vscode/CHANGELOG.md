# Change Log

All notable changes to this project will be documented in this file

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html),
but may switch to [CalVer Versioning](https://calver.org/) in the future.

## Unreleased

- Highlighting of operators

## Known issues

- Highlighting of illegal overflowing/underflowing integer literals
- Highlighting of illegal whole identifiers containing non-ascii characters
- Highlighting of illegal character literals with over two characters

## 0.1.1 -

### Added

- Highlighting as errors of identifiers over the limit of 63 characters

### Fixed

- Highlighting of array type annotations
    - spaces around length are correctly taken into account
    - length of 0 and 1 are reported as errors
    - anything except integer literals are correctly highlighted as errors

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
