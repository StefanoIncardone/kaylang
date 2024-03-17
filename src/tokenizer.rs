use crate::{
    error::{RawSyntaxError, SyntaxErrorCause, SyntaxErrorKind, SyntaxErrors},
    src_file::{Line, SrcFile},
};
use std::{
    fmt::Display,
    num::{IntErrorKind, ParseIntError},
};

pub(crate) trait SrcCodeLen {
    fn src_code_len(&self) -> usize;
}

/// kay's equivalent to pointer sized signed integer
#[allow(non_camel_case_types)]
pub(crate) type int = isize;

#[allow(non_camel_case_types)]
/// kay's equivalent to pointer sized unsigned integer
pub(crate) type uint = usize;

/// kay's ascii character type
#[allow(non_camel_case_types)]
pub(crate) type ascii = u8;

/// kay's utf8 character type
#[allow(non_camel_case_types)]
pub(crate) type utf8 = char;

#[derive(Debug, Clone)]
pub(crate) enum Literal {
    // TODO(stefano): implement unsigned integers
    // IDEA(stefano): have different size integers and default to 32 bits for literals
    Int(int),

    Ascii(ascii), // only supporting ASCII characters for now

    Bool(bool),
    Str(Vec<ascii>), // only supporting ASCII characters for now
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(value) => write!(f, "{value}"),
            Self::Ascii(value) => write!(f, "'{value}'", value = value.escape_ascii()),
            Self::Bool(value) => write!(f, "{value}"),
            Self::Str(string) => {
                write!(f, "\"")?;
                for character in string {
                    write!(f, "{ch}", ch = character.escape_ascii())?;
                }
                write!(f, "\"")
            }
        }
    }
}

impl SrcCodeLen for Literal {
    fn src_code_len(&self) -> usize {
        match self {
            Self::Int(value) => value.to_string().len(),
            Self::Ascii(value) => value.escape_ascii().len() + 2, // + 2 for the quotes
            Self::Bool(value) => value.to_string().len(),
            Self::Str(string) => {
                let mut len = 0;
                for ch in string {
                    len += ch.escape_ascii().len();
                }
                len + 2 // + 2 for the quotes
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    // unary
    Not,
    // Minus can also be a unary operator

    // binary
    Pow,
    Times,
    Divide,
    Remainder,
    Plus,
    Minus,

    LeftShift,
    RightShift,

    BitAnd,
    BitXor,
    BitOr,

    Compare,
    And,
    Or,

    EqualsEquals,
    NotEquals,
    Greater,
    GreaterOrEquals,
    Less,
    LessOrEquals,

    // assignment
    Equals,

    PowEquals,
    TimesEquals,
    DivideEquals,
    RemainderEquals,
    PlusEquals,
    MinusEquals,

    LeftShiftEquals,
    RightShiftEquals,

    BitAndEquals,
    BitXorEquals,
    BitOrEquals,

    AndEquals,
    OrEquals,
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Equals => write!(f, "="),

            Self::Not => write!(f, "!"),

            Self::Pow => write!(f, "**"),
            Self::PowEquals => write!(f, "**="),
            Self::Times => write!(f, "*"),
            Self::TimesEquals => write!(f, "*="),
            Self::Divide => write!(f, "/"),
            Self::DivideEquals => write!(f, "/="),
            Self::Remainder => write!(f, "%"),
            Self::RemainderEquals => write!(f, "%="),
            Self::Plus => write!(f, "+"),
            Self::PlusEquals => write!(f, "+="),
            Self::Minus => write!(f, "-"),
            Self::MinusEquals => write!(f, "-="),

            Self::And => write!(f, "&&"),
            Self::AndEquals => write!(f, "&&="),
            Self::BitAnd => write!(f, "&"),
            Self::BitAndEquals => write!(f, "&="),
            Self::Or => write!(f, "||"),
            Self::OrEquals => write!(f, "||="),
            Self::BitOr => write!(f, "|"),
            Self::BitOrEquals => write!(f, "|="),
            Self::BitXor => write!(f, "^"),
            Self::BitXorEquals => write!(f, "^="),
            Self::LeftShift => write!(f, "<<"),
            Self::LeftShiftEquals => write!(f, "<<="),
            Self::RightShift => write!(f, ">>"),
            Self::RightShiftEquals => write!(f, ">>="),

            Self::EqualsEquals => write!(f, "=="),
            Self::NotEquals => write!(f, "!="),
            Self::Greater => write!(f, ">"),
            Self::GreaterOrEquals => write!(f, ">="),
            Self::Less => write!(f, "<"),
            Self::LessOrEquals => write!(f, "<="),
            Self::Compare => write!(f, "<=>"),
        }
    }
}

impl SrcCodeLen for Op {
    fn src_code_len(&self) -> usize {
        match self {
            Self::Equals => 1,

            Self::Not => 1,

            Self::Pow => 2,
            Self::PowEquals => 3,
            Self::Times => 1,
            Self::TimesEquals => 2,
            Self::Divide => 1,
            Self::DivideEquals => 2,
            Self::Remainder => 1,
            Self::RemainderEquals => 2,
            Self::Plus => 1,
            Self::PlusEquals => 2,
            Self::Minus => 1,
            Self::MinusEquals => 2,

            Self::And => 2,
            Self::AndEquals => 3,
            Self::BitAnd => 1,
            Self::BitAndEquals => 2,
            Self::Or => 2,
            Self::OrEquals => 3,
            Self::BitOr => 1,
            Self::BitOrEquals => 2,
            Self::BitXor => 1,
            Self::BitXorEquals => 2,
            Self::LeftShift => 2,
            Self::LeftShiftEquals => 3,
            Self::RightShift => 2,
            Self::RightShiftEquals => 3,

            Self::EqualsEquals => 2,
            Self::NotEquals => 2,
            Self::Greater => 1,
            Self::GreaterOrEquals => 2,
            Self::Less => 1,
            Self::LessOrEquals => 2,
            Self::Compare => 3,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Mutability {
    Let,
    Var,
}

impl Display for Mutability {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Let => write!(f, "let"),
            Self::Var => write!(f, "var"),
        }
    }
}

impl SrcCodeLen for Mutability {
    fn src_code_len(&self) -> usize {
        match self {
            Self::Let => 3,
            Self::Var => 3,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BracketKind {
    OpenRound,
    CloseRound,
    OpenSquare,
    CloseSquare,
    OpenCurly,
    CloseCurly,
}

impl Display for BracketKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::OpenRound => write!(f, "("),
            Self::CloseRound => write!(f, ")"),
            Self::OpenSquare => write!(f, "["),
            Self::CloseSquare => write!(f, "]"),
            Self::OpenCurly => write!(f, "{{"),
            Self::CloseCurly => write!(f, "}}"),
        }
    }
}

impl SrcCodeLen for BracketKind {
    fn src_code_len(&self) -> usize {
        match self {
            Self::OpenRound => 1,
            Self::CloseRound => 1,
            Self::OpenSquare => 1,
            Self::CloseSquare => 1,
            Self::OpenCurly => 1,
            Self::CloseCurly => 1,
        }
    }
}

#[derive(Debug)]
struct Bracket {
    kind: BracketKind,
    col: usize,
}

#[derive(Debug, Clone)]
pub(crate) enum TokenKind<'src> {
    Comment(&'src str),
    Unexpected(&'src str),

    Bracket(BracketKind),
    Colon,
    SemiColon,
    Comma,
    Op(Op),

    Literal(Literal),
    True,
    False,
    Identifier(&'src str),
    Definition(Mutability),

    // Keywords
    Print,    // temporary way of printing values to stdout
    PrintLn,  // temporary way of printing values followed by a newline to stdout
    Eprint,   // temporary way of printing values to stderr
    EprintLn, // temporary way of printing values followed by a newline to stderr
    Do,
    If,
    Else,
    Loop,
    Break,
    Continue,
}

impl Display for TokenKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Comment(text) => write!(f, "#{text}"),
            Self::Unexpected(text) => write!(f, "{text}"),

            Self::Bracket(bracket) => write!(f, "{bracket}"),
            Self::Colon => write!(f, ":"),
            Self::SemiColon => write!(f, ";"),
            Self::Comma => write!(f, ","),

            Self::Literal(literal) => write!(f, "{literal}"),
            Self::Identifier(name) => write!(f, "{name}"),
            Self::Definition(kind) => write!(f, "{kind}"),

            Self::Op(op) => write!(f, "{op}"),

            Self::Print => write!(f, "print"),
            Self::PrintLn => write!(f, "println"),
            Self::Eprint => write!(f, "eprint"),
            Self::EprintLn => write!(f, "eprintln"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Do => write!(f, "do"),
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::Loop => write!(f, "loop"),
            Self::Break => write!(f, "break"),
            Self::Continue => write!(f, "continue"),
        }
    }
}

impl SrcCodeLen for TokenKind<'_> {
    fn src_code_len(&self) -> usize {
        match self {
            Self::Comment(text) => text.chars().count(),
            Self::Unexpected(text) => text.chars().count(),

            Self::Bracket(bracket) => bracket.src_code_len(),
            Self::Colon => 1,
            Self::SemiColon => 1,
            Self::Comma => 1,
            Self::Op(op) => op.src_code_len(),

            Self::Literal(typ) => typ.src_code_len(),
            Self::True => 4,
            Self::False => 5,
            Self::Identifier(name) => name.chars().count(),
            Self::Definition(kind) => kind.src_code_len(),

            Self::Print => 5,
            Self::PrintLn => 7,
            Self::Eprint => 6,
            Self::EprintLn => 8,
            Self::Do => 2,
            Self::If => 2,
            Self::Else => 4,
            Self::Loop => 4,
            Self::Break => 5,
            Self::Continue => 8,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token<'src> {
    pub(crate) kind: TokenKind<'src>,
    pub(crate) col: usize,
}

#[derive(Debug)]
pub struct Tokenizer<'src> {
    src: &'src SrcFile,
    errors: Vec<RawSyntaxError<ErrorKind, ErrorCause>>,

    col: usize,
    token_start_col: usize,

    line_index: usize,
    line: &'src Line,

    tokens: Vec<Token<'src>>,
    brackets: Vec<Bracket>,
}

impl<'src> Tokenizer<'src> {
    pub fn tokenize(
        src: &'src SrcFile,
    ) -> Result<Vec<Token<'src>>, SyntaxErrors<'src, ErrorKind, ErrorCause>> {
        if src.lines.is_empty() {
            return Ok(Vec::new());
        }

        let mut this = Self {
            src,
            errors: Vec::new(),
            col: 0,
            token_start_col: 0,
            line_index: 0,
            line: &src.lines[0],
            tokens: Vec::new(),
            brackets: Vec::new(),
        };

        'tokenization: loop {
            let token_kind_result = 'skip_whitespace: loop {
                this.token_start_col = this.col;

                let next = match this.next_ascii_char() {
                    Ok(Some(ch)) => ch,
                    Ok(None) => break 'tokenization,
                    Err(err) => break 'skip_whitespace Err(err),
                };

                match next {
                    // ignore whitespace
                    b' ' | b'\t' | b'\r' | b'\x0C' => {}

                    // next line
                    b'\n' => {
                        if this.line_index >= this.src.lines.len() - 1 {
                            break 'tokenization;
                        }

                        this.line_index += 1;
                        this.line = &this.src.lines[this.line_index];
                    }
                    ch => break 'skip_whitespace this.next_token(ch),
                }
            };

            let kind = match token_kind_result {
                Ok(kind) => kind,
                Err(err) => {
                    this.errors.push(err);
                    TokenKind::Unexpected(&this.src.code[this.token_start_col..this.col])
                }
            };

            this.tokens.push(Token { kind, col: this.token_start_col });
        }

        for bracket in &this.brackets {
            // there can only be open brackets at this point
            this.errors.push(RawSyntaxError {
                kind: ErrorKind::UnclosedBracket(bracket.kind),
                cause: ErrorCause::UnclosedBracket,
                col: bracket.col,
                len: 1,
            });
        }

        if this.errors.is_empty() {
            Ok(this.tokens)
        } else {
            Err(SyntaxErrors { src, raw_errors: this.errors })
        }
    }
}

// TODO(stefano): own utf8 parsing
// TODO(stefano): properly handle multi-char utf8 characters (i.e. emojis)
// iteration of characters
impl<'src> Tokenizer<'src> {
    fn token_len(&self) -> usize {
        self.src.code[self.token_start_col..self.col].chars().count()
    }

    fn next_ascii_char(&mut self) -> Result<Option<ascii>, RawSyntaxError<ErrorKind, ErrorCause>> {
        let Some(next) = self.src.code.as_bytes().get(self.col) else {
            return Ok(None);
        };

        match next {
            ascii_ch @ ..=b'\x7F' => {
                self.col += 1;
                Ok(Some(*ascii_ch))
            }
            _utf8_ch => {
                let rest_of_line = &self.src.code[self.col..self.line.end];
                let utf8_ch = rest_of_line.chars().next().unwrap();
                let utf8_ch_col = self.col;
                self.col += utf8_ch.len_utf8();
                Err(RawSyntaxError {
                    kind: ErrorKind::NonAsciiCharacter(utf8_ch),
                    cause: ErrorCause::NonAsciiCharacter,
                    col: utf8_ch_col,
                    len: 1,
                })
            }
        }
    }

    fn next_utf8_char(&mut self) -> Option<utf8> {
        let Some(next) = self.src.code.as_bytes().get(self.col) else {
            return None;
        };

        match next {
            ascii_ch @ ..=b'\x7F' => {
                self.col += 1;
                Some(*ascii_ch as utf8)
            }
            _utf8_ch => {
                let rest_of_line = &self.src.code[self.col..self.line.end];
                let utf8_ch = rest_of_line.chars().next().unwrap();
                self.col += utf8_ch.len_utf8();
                Some(utf8_ch)
            }
        }
    }

    fn peek_next_ascii_char(
        &self,
    ) -> Result<Option<&'src ascii>, RawSyntaxError<ErrorKind, ErrorCause>> {
        let Some(next) = self.src.code.as_bytes().get(self.col) else {
            return Ok(None);
        };

        match next {
            ascii_ch @ ..=b'\x7F' => Ok(Some(ascii_ch)),
            _utf8_ch => {
                let rest_of_line = &self.src.code[self.col..self.line.end];
                let utf8_ch = rest_of_line.chars().next().unwrap();
                Err(RawSyntaxError {
                    kind: ErrorKind::NonAsciiCharacter(utf8_ch),
                    cause: ErrorCause::NonAsciiCharacter,
                    col: self.col,
                    len: 1,
                })
            }
        }
    }

    fn peek_next_utf8_char(&mut self) -> Option<utf8> {
        let Some(next) = self.src.code.as_bytes().get(self.col) else {
            return None;
        };

        match next {
            ascii_ch @ ..=b'\x7F' => Some(*ascii_ch as utf8),
            _utf8_ch => {
                let rest_of_line = &self.src.code[self.col..self.line.end];
                let utf8_ch = rest_of_line.chars().next().unwrap();
                Some(utf8_ch)
            }
        }
    }

    fn next_in_ascii_char_literal(
        &mut self,
    ) -> Result<ascii, RawSyntaxError<ErrorKind, ErrorCause>> {
        match self.src.code.as_bytes().get(self.col) {
            Some(b'\n') | None => Err(RawSyntaxError {
                kind: ErrorKind::InvalidCharacterLiteral,
                cause: ErrorCause::MissingClosingSingleQuote,
                col: self.token_start_col,
                len: self.token_len(),
            }),
            Some(ascii_ch @ ..=b'\x7F') => {
                self.col += 1;
                Ok(*ascii_ch)
            }
            Some(_utf8_ch) => {
                let rest_of_line = &self.src.code[self.col..self.line.end];
                let utf8_ch = rest_of_line.chars().next().unwrap();
                let utf8_ch_col = self.col;
                self.col += utf8_ch.len_utf8();
                Err(RawSyntaxError {
                    kind: ErrorKind::NonAsciiCharacter(utf8_ch),
                    cause: ErrorCause::NonAsciiCharacter,
                    col: utf8_ch_col,
                    len: 1,
                })
            }
        }
    }

    fn next_in_ascii_str_literal(
        &mut self,
    ) -> Result<ascii, RawSyntaxError<ErrorKind, ErrorCause>> {
        match self.src.code.as_bytes().get(self.col) {
            Some(b'\n') | None => Err(RawSyntaxError {
                kind: ErrorKind::InvalidStringLiteral,
                cause: ErrorCause::MissingClosingDoubleQuote,
                col: self.token_start_col,
                len: self.token_len(),
            }),
            Some(ascii_ch @ ..=b'\x7F') => {
                self.col += 1;
                Ok(*ascii_ch)
            }
            Some(_utf8_ch) => {
                let rest_of_line = &self.src.code[self.col..self.line.end];
                let utf8_ch = rest_of_line.chars().next().unwrap();
                let utf8_ch_col = self.col;
                self.col += utf8_ch.len_utf8();
                Err(RawSyntaxError {
                    kind: ErrorKind::NonAsciiCharacter(utf8_ch),
                    cause: ErrorCause::NonAsciiCharacter,
                    col: utf8_ch_col,
                    len: 1,
                })
            }
        }
    }
}

impl<'src> Tokenizer<'src> {
    fn identifier(&mut self) -> Result<TokenKind<'src>, RawSyntaxError<ErrorKind, ErrorCause>> {
        let mut contains_utf8 = false;
        loop {
            match self.peek_next_ascii_char() {
                Ok(Some(b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_')) => {}
                Ok(Some(_) | None) => break,
                Err(_) => contains_utf8 = true,
            }

            let _ = self.next_ascii_char();
        }

        if contains_utf8 {
            return Err(RawSyntaxError {
                kind: ErrorKind::InvalidIdentifier,
                cause: ErrorCause::ContainsNonAsciiCharacters,
                col: self.token_start_col,
                len: self.token_len(),
            });
        }

        let identifier = match &self.src.code[self.token_start_col..self.col] {
            "let" => TokenKind::Definition(Mutability::Let),
            "var" => TokenKind::Definition(Mutability::Var),
            "print" => TokenKind::Print,
            "println" => TokenKind::PrintLn,
            "eprint" => TokenKind::Eprint,
            "eprintln" => TokenKind::EprintLn,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "do" => TokenKind::Do,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "loop" => TokenKind::Loop,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            identifier => TokenKind::Identifier(identifier),
        };

        Ok(identifier)
    }

    fn next_token(
        &mut self,
        next: ascii,
    ) -> Result<TokenKind<'src>, RawSyntaxError<ErrorKind, ErrorCause>> {
        match next {
            b'r' => match self.peek_next_utf8_char() {
                Some('"') => {
                    self.col += 1;

                    let previous_error_count = self.errors.len();
                    let mut raw_string_literal = Vec::<ascii>::new();

                    loop {
                        let next = match self.next_in_ascii_str_literal()? {
                            control @ (b'\x00'..=b'\x1F' | b'\x7F') => {
                                self.errors.push(RawSyntaxError {
                                    kind: ErrorKind::InvalidStringLiteral,
                                    cause: ErrorCause::ControlCharacterNotAllowed(control as utf8),
                                    col: self.col - 1,
                                    len: 1,
                                });
                                control
                            }
                            b'"' => break,
                            ch => ch,
                        };

                        raw_string_literal.push(next);
                    }

                    if self.errors.len() > previous_error_count {
                        // SAFETY: we are now sure that at least one error has occured, so we can safely unwrap
                        let last_error = self.errors.pop().unwrap();
                        Err(last_error)
                    } else {
                        Ok(TokenKind::Literal(Literal::Str(raw_string_literal)))
                    }
                }
                _ => self.identifier(),
            },
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.identifier(),
            b'0'..=b'9' => {
                let mut contains_utf8 = false;
                loop {
                    match self.peek_next_ascii_char() {
                        Ok(Some(b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_')) => {}
                        Ok(Some(_) | None) => break,
                        Err(_) => contains_utf8 = true,
                    }

                    let _ = self.next_ascii_char();
                }

                let token_text = &self.src.code[self.token_start_col..self.col];
                match token_text.parse() {
                    Ok(value) => Ok(TokenKind::Literal(Literal::Int(value))),
                    Err(err) => {
                        let cause = match err.kind() {
                            IntErrorKind::InvalidDigit => {
                                if contains_utf8 {
                                    ErrorCause::ContainsNonAsciiCharacters
                                } else {
                                    ErrorCause::ContainsNonDigitCharacters
                                }
                            }
                            IntErrorKind::PosOverflow => {
                                ErrorCause::IntOverflow { bits: int::BITS, max: int::MAX }
                            }
                            IntErrorKind::NegOverflow => {
                                ErrorCause::IntUnderflow { bits: int::BITS, min: int::MIN }
                            }
                            IntErrorKind::Empty => unreachable!("should never parse empty numbers"),
                            IntErrorKind::Zero => unreachable!("numbers can also be zero"),
                            _ => ErrorCause::InvalidInt(err),
                        };

                        Err(RawSyntaxError {
                            kind: ErrorKind::InvalidNumberLiteral,
                            cause,
                            col: self.token_start_col,
                            len: self.token_len(),
                        })
                    }
                }
            }
            b'#' => {
                // ignoring the hash symbol
                let comment = &self.src.code[self.col..self.line.end];

                // consuming the rest of the characters in the current line
                loop {
                    if let Some('\n') | None = self.peek_next_utf8_char() {
                        break;
                    }
                    let _ = self.next_utf8_char();
                }

                Ok(TokenKind::Comment(comment))
            }
            // FIX(stefano): add proper multiple error handling
            b'"' => {
                let previous_error_count = self.errors.len();
                let mut string_literal = Vec::<ascii>::new();

                loop {
                    let next = match self.next_in_ascii_str_literal()? {
                        b'\\' => match self.next_in_ascii_str_literal()? {
                            b'\\' => b'\\',
                            b'\'' => b'\'',
                            b'"' => b'"',
                            b'n' => b'\n',
                            b'r' => b'\r',
                            b't' => b'\t',
                            b'0' => b'\0',
                            unrecognized => {
                                self.errors.push(RawSyntaxError {
                                    kind: ErrorKind::InvalidStringLiteral,
                                    cause: ErrorCause::UnrecognizedEscapeCharacter(
                                        unrecognized as utf8,
                                    ),
                                    col: self.col - 2,
                                    len: 2,
                                });
                                string_literal.push(b'\\');
                                unrecognized
                            }
                        },
                        control @ (b'\x00'..=b'\x1F' | b'\x7F') => {
                            self.errors.push(RawSyntaxError {
                                kind: ErrorKind::InvalidStringLiteral,
                                cause: ErrorCause::ControlCharacterNotAllowed(control as utf8),
                                col: self.col - 1,
                                len: 1,
                            });
                            control
                        }
                        b'"' => break,
                        ch => ch,
                    };

                    string_literal.push(next);
                }

                if self.errors.len() > previous_error_count {
                    // SAFETY: we are now sure that at least one error has occured, so we can safely unwrap
                    let last_error = self.errors.pop().unwrap();
                    Err(last_error)
                } else {
                    Ok(TokenKind::Literal(Literal::Str(string_literal)))
                }
            }
            b'\'' => {
                let code = match self.next_in_ascii_char_literal()? {
                    b'\\' => match self.next_in_ascii_char_literal()? {
                        b'\\' => Ok(b'\\'),
                        b'\'' => Ok(b'\''),
                        b'"' => Ok(b'"'),
                        b'n' => Ok(b'\n'),
                        b'r' => Ok(b'\r'),
                        b't' => Ok(b'\t'),
                        b'0' => Ok(b'\0'),
                        unrecognized => Err(RawSyntaxError {
                            kind: ErrorKind::InvalidCharacterLiteral,
                            cause: ErrorCause::UnrecognizedEscapeCharacter(unrecognized as utf8),
                            col: self.col - 2,
                            len: 2,
                        }),
                    },
                    control @ (b'\x00'..=b'\x1F' | b'\x7F') => Err(RawSyntaxError {
                        kind: ErrorKind::InvalidCharacterLiteral,
                        cause: ErrorCause::ControlCharacterNotAllowed(control as utf8),
                        col: self.col - 1,
                        len: 1,
                    }),
                    b'\'' => Err(RawSyntaxError {
                        kind: ErrorKind::InvalidCharacterLiteral,
                        cause: ErrorCause::MustNotBeEmpty,
                        col: self.token_start_col,
                        len: 2,
                    }),
                    ch => Ok(ch),
                };

                let Some(b'\'') = self.peek_next_ascii_char()? else {
                    return Err(RawSyntaxError {
                        kind: ErrorKind::InvalidCharacterLiteral,
                        cause: ErrorCause::MissingClosingSingleQuote,
                        col: self.token_start_col,
                        len: self.token_len(),
                    });
                };

                self.col += 1;
                Ok(TokenKind::Literal(Literal::Ascii(code?)))
            }
            b'(' => {
                let kind = BracketKind::OpenRound;
                self.brackets.push(Bracket { kind, col: self.token_start_col });
                Ok(TokenKind::Bracket(kind))
            }
            b')' => match self.brackets.pop() {
                Some(bracket) => match bracket.kind {
                    BracketKind::OpenRound
                    | BracketKind::CloseRound
                    | BracketKind::CloseCurly
                    | BracketKind::CloseSquare => Ok(TokenKind::Bracket(BracketKind::CloseRound)),
                    actual @ (BracketKind::OpenCurly | BracketKind::OpenSquare) => {
                        Err(RawSyntaxError {
                            kind: ErrorKind::MismatchedBracket,
                            cause: ErrorCause::MismatchedBracket {
                                expected: BracketKind::CloseRound,
                                actual,
                            },
                            col: self.token_start_col,
                            len: 1,
                        })
                    }
                },
                None => Err(RawSyntaxError {
                    kind: ErrorKind::UnopenedBracket(BracketKind::CloseRound),
                    cause: ErrorCause::UnopenedBracket,
                    col: self.token_start_col,
                    len: 1,
                }),
            },
            b'[' => {
                let kind = BracketKind::OpenSquare;
                self.brackets.push(Bracket { kind, col: self.token_start_col });
                Ok(TokenKind::Bracket(kind))
            }
            b']' => match self.brackets.pop() {
                Some(bracket) => match bracket.kind {
                    BracketKind::OpenSquare
                    | BracketKind::CloseSquare
                    | BracketKind::CloseCurly
                    | BracketKind::CloseRound => Ok(TokenKind::Bracket(BracketKind::CloseSquare)),
                    actual @ (BracketKind::OpenCurly | BracketKind::OpenRound) => {
                        Err(RawSyntaxError {
                            kind: ErrorKind::MismatchedBracket,
                            cause: ErrorCause::MismatchedBracket {
                                expected: BracketKind::CloseSquare,
                                actual,
                            },
                            col: self.token_start_col,
                            len: 1,
                        })
                    }
                },
                None => Err(RawSyntaxError {
                    kind: ErrorKind::UnopenedBracket(BracketKind::CloseSquare),
                    cause: ErrorCause::UnopenedBracket,
                    col: self.token_start_col,
                    len: 1,
                }),
            },
            b'{' => {
                let kind = BracketKind::OpenCurly;
                self.brackets.push(Bracket { kind, col: self.token_start_col });
                Ok(TokenKind::Bracket(kind))
            }
            b'}' => match self.brackets.pop() {
                Some(bracket) => match bracket.kind {
                    BracketKind::OpenCurly
                    | BracketKind::CloseCurly
                    | BracketKind::CloseRound
                    | BracketKind::CloseSquare => Ok(TokenKind::Bracket(BracketKind::CloseCurly)),
                    actual @ (BracketKind::OpenRound | BracketKind::OpenSquare) => {
                        Err(RawSyntaxError {
                            kind: ErrorKind::MismatchedBracket,
                            cause: ErrorCause::MismatchedBracket {
                                expected: BracketKind::CloseCurly,
                                actual,
                            },
                            col: self.token_start_col,
                            len: 1,
                        })
                    }
                },
                None => Err(RawSyntaxError {
                    kind: ErrorKind::UnopenedBracket(BracketKind::CloseCurly),
                    cause: ErrorCause::UnopenedBracket,
                    col: self.token_start_col,
                    len: 1,
                }),
            },
            b':' => Ok(TokenKind::Colon),
            b';' => Ok(TokenKind::SemiColon),
            b',' => Ok(TokenKind::Comma),
            b'!' => match self.peek_next_utf8_char() {
                Some('=') => {
                    self.col += 1;
                    Ok(TokenKind::Op(Op::NotEquals))
                }
                _ => Ok(TokenKind::Op(Op::Not)),
            },
            b'*' => match self.peek_next_utf8_char() {
                Some('*') => {
                    self.col += 1;
                    match self.peek_next_utf8_char() {
                        Some('=') => {
                            self.col += 1;
                            Ok(TokenKind::Op(Op::PowEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::Pow)),
                    }
                }
                Some('=') => {
                    self.col += 1;
                    Ok(TokenKind::Op(Op::TimesEquals))
                }
                _ => Ok(TokenKind::Op(Op::Times)),
            },
            b'/' => match self.peek_next_utf8_char() {
                Some('=') => {
                    self.col += 1;
                    Ok(TokenKind::Op(Op::DivideEquals))
                }
                _ => Ok(TokenKind::Op(Op::Divide)),
            },
            b'%' => match self.peek_next_utf8_char() {
                Some('=') => {
                    self.col += 1;
                    Ok(TokenKind::Op(Op::RemainderEquals))
                }
                _ => Ok(TokenKind::Op(Op::Remainder)),
            },
            b'+' => match self.peek_next_utf8_char() {
                Some('=') => {
                    self.col += 1;
                    Ok(TokenKind::Op(Op::PlusEquals))
                }
                _ => Ok(TokenKind::Op(Op::Plus)),
            },
            b'-' => match self.peek_next_utf8_char() {
                Some('=') => {
                    self.col += 1;
                    Ok(TokenKind::Op(Op::MinusEquals))
                }
                _ => Ok(TokenKind::Op(Op::Minus)),
            },
            b'&' => match self.peek_next_utf8_char() {
                Some('&') => {
                    self.col += 1;
                    match self.peek_next_utf8_char() {
                        Some('=') => {
                            self.col += 1;
                            Ok(TokenKind::Op(Op::AndEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::And)),
                    }
                }
                Some('=') => {
                    self.col += 1;
                    Ok(TokenKind::Op(Op::BitAndEquals))
                }
                _ => Ok(TokenKind::Op(Op::BitAnd)),
            },
            b'^' => match self.peek_next_utf8_char() {
                Some('=') => {
                    self.col += 1;
                    Ok(TokenKind::Op(Op::BitXorEquals))
                }
                _ => Ok(TokenKind::Op(Op::BitXor)),
            },
            b'|' => match self.peek_next_utf8_char() {
                Some('|') => {
                    self.col += 1;
                    match self.peek_next_utf8_char() {
                        Some('=') => {
                            self.col += 1;
                            Ok(TokenKind::Op(Op::OrEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::Or)),
                    }
                }
                Some('=') => {
                    self.col += 1;
                    Ok(TokenKind::Op(Op::BitOrEquals))
                }
                _ => Ok(TokenKind::Op(Op::BitOr)),
            },
            b'=' => match self.peek_next_utf8_char() {
                Some('=') => {
                    self.col += 1;
                    Ok(TokenKind::Op(Op::EqualsEquals))
                }
                _ => Ok(TokenKind::Op(Op::Equals)),
            },
            b'>' => match self.peek_next_utf8_char() {
                Some('>') => {
                    self.col += 1;
                    match self.peek_next_utf8_char() {
                        Some('=') => {
                            self.col += 1;
                            Ok(TokenKind::Op(Op::RightShiftEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::RightShift)),
                    }
                }
                Some('=') => {
                    self.col += 1;
                    Ok(TokenKind::Op(Op::GreaterOrEquals))
                }
                _ => Ok(TokenKind::Op(Op::Greater)),
            },
            b'<' => match self.peek_next_utf8_char() {
                Some('<') => {
                    self.col += 1;
                    match self.peek_next_utf8_char() {
                        Some('=') => {
                            self.col += 1;
                            Ok(TokenKind::Op(Op::LeftShiftEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::LeftShift)),
                    }
                }
                Some('=') => {
                    self.col += 1;
                    match self.peek_next_utf8_char() {
                        Some('>') => {
                            self.col += 1;
                            Ok(TokenKind::Op(Op::Compare))
                        }
                        _ => Ok(TokenKind::Op(Op::LessOrEquals)),
                    }
                }
                _ => Ok(TokenKind::Op(Op::Less)),
            },
            unrecognized => Err(RawSyntaxError {
                kind: ErrorKind::UnrecognizedCharacter(unrecognized as utf8),
                cause: ErrorCause::UnrecognizedCharacter,
                col: self.token_start_col,
                len: 1,
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    UnclosedBracket(BracketKind),
    UnopenedBracket(BracketKind),
    MismatchedBracket,

    InvalidCharacterLiteral,
    InvalidStringLiteral,
    InvalidIdentifier,
    InvalidNumberLiteral,

    NonAsciiCharacter(utf8),
    UnrecognizedCharacter(utf8),
}

impl SyntaxErrorKind for ErrorKind {}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnclosedBracket(bracket) => write!(f, "unclosed '{bracket}' bracket"),
            Self::UnopenedBracket(bracket) => write!(f, "unopened '{bracket}' bracket"),
            Self::MismatchedBracket => write!(f, "mismatched bracket"),
            Self::InvalidCharacterLiteral => write!(f, "invalid character literal"),
            Self::InvalidStringLiteral => write!(f, "invalid string literal"),
            Self::InvalidIdentifier => write!(f, "invalid identifier"),
            Self::InvalidNumberLiteral => write!(f, "invalid number literal"),
            Self::NonAsciiCharacter(ch) => {
                write!(
                    f,
                    "non-ascii '{ch}' ({escaped_char}) character",
                    escaped_char = ch.escape_unicode()
                )
            }
            Self::UnrecognizedCharacter(ch) => {
                write!(
                    f,
                    "unrecognized '{ch}' ({escaped_char}) character",
                    escaped_char = ch.escape_unicode()
                )
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum ErrorCause {
    UnclosedBracket,
    UnopenedBracket,
    MismatchedBracket { expected: BracketKind, actual: BracketKind },

    MissingClosingSingleQuote,
    MissingClosingDoubleQuote,
    ContainsNonAsciiCharacters,
    ContainsNonDigitCharacters,
    MustNotBeEmpty,
    UnrecognizedEscapeCharacter(utf8),
    ControlCharacterNotAllowed(utf8),

    IntOverflow { bits: u32, max: isize },
    IntUnderflow { bits: u32, min: isize },
    InvalidInt(ParseIntError),

    NonAsciiCharacter,
    UnrecognizedCharacter,
}

impl SyntaxErrorCause for ErrorCause {}

impl Display for ErrorCause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnclosedBracket => write!(f, "was not closed"),
            Self::UnopenedBracket => write!(f, "was not opened"),
            Self::MissingClosingSingleQuote => write!(f, "missing closing single quote"),
            Self::MissingClosingDoubleQuote => write!(f, "missing closing double quote"),
            Self::ContainsNonAsciiCharacters => write!(f, "contains non-ASCII characters"),
            Self::ContainsNonDigitCharacters => write!(f, "contains non-digit characters"),
            Self::MustNotBeEmpty => write!(f, "must not be empty"),
            Self::UnrecognizedEscapeCharacter(ch) => {
                write!(f, "unrecognized '{ch}' escape character")
            }
            Self::ControlCharacterNotAllowed(ch) => {
                write!(f, "`{ch}` control character not allowed")
            }
            Self::MismatchedBracket { expected, actual } => {
                write!(f, "`{actual}` closes the wrong bracket, expected a '{expected}' instead")
            }
            Self::IntOverflow { bits, max } => {
                write!(f, "overflows a {bits} bit signed integer (over {max})")
            }
            Self::IntUnderflow { bits, min } => {
                write!(f, "underflows a {bits} bit signed integer (under {min})")
            }
            Self::InvalidInt(err) => write!(f, "{err}"),
            Self::NonAsciiCharacter => write!(f, "not a valid ASCII character"),
            Self::UnrecognizedCharacter => write!(f, "unrecognized"),
        }
    }
}
