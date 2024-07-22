use crate::src_file::{Line, SrcFile};
use std::{fmt::Display, num::{IntErrorKind, ParseIntError}};
use super::{Error, ErrorInfo, IntoErrorInfo};

pub(super) trait DisplayLen {
    fn display_len(&self) -> usize;
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
        return match self {
            Self::Int(integer) => write!(f, "{integer}"),
            Self::Ascii(code) => write!(f, "'{}'", code.escape_ascii()),
            Self::Bool(boolean) => write!(f, "{boolean}"),
            Self::Str(string) => {
                write!(f, "\"")?;
                for ch in string {
                    write!(f, "{}", ch.escape_ascii())?;
                }
                write!(f, "\"")
            }
        };
    }
}

impl DisplayLen for Literal {
    #[inline(always)]
    fn display_len(&self) -> usize {
        #[inline(always)]
        fn ascii_escaped_len(ascii_char: ascii) -> usize {
            // Note: ascii type guarantees the value to be valid utf8
            let utf8_char = ascii_char as utf8;
            return utf8_char.escape_debug().len();
        }

        return match self {
            Self::Int(integer) => integer.to_string().len(),
            Self::Ascii(ascii_char) => ascii_escaped_len(*ascii_char) + 2, // + 2 to account for the quotes
            Self::Bool(true) => 4,
            Self::Bool(false) => 5,
            Self::Str(text) => {
                let mut len = 2; // starting at 2 to account for the quotes
                for ascii_char in text {
                    len += ascii_escaped_len(*ascii_char);
                }
                return len;
            }
        };
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Mutability {
    Let,
    Var,
}

impl Display for Mutability {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Let => write!(f, "let"),
            Self::Var => write!(f, "var"),
        };
    }
}

impl DisplayLen for Mutability {
    #[inline(always)]
    fn display_len(&self) -> usize {
        return match self {
            Self::Let | Self::Var => 3,
        };
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
        return match self {
            Self::OpenRound => write!(f, "("),
            Self::CloseRound => write!(f, ")"),
            Self::OpenSquare => write!(f, "["),
            Self::CloseSquare => write!(f, "]"),
            Self::OpenCurly => write!(f, "{{"),
            Self::CloseCurly => write!(f, "}}"),
        };
    }
}

impl DisplayLen for BracketKind {
    #[inline(always)]
    fn display_len(&self) -> usize {
        return match self {
            Self::OpenRound
            | Self::CloseRound
            | Self::OpenSquare
            | Self::CloseSquare
            | Self::OpenCurly
            | Self::CloseCurly => 1,
        };
    }
}

#[derive(Debug)]
struct Bracket {
    kind: BracketKind,
    col: usize,
}

/*
IDEA(stefano): introduce "unchecked" operators
skip safety checks, maybe using the '?' or the '!' suffix, i.e:
- <<?, <<<?, >>>?, or <<!, >>!, <<<!, >>>! -> skip the check for a positive 6bit shift amount
- **? -> skip the check for a neagtive index
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Len, // temporary way of getting the length of strings and arrays
    Equals,
    Not,

    Pow,
    WrappingPow,
    SaturatingPow,
    PowEquals,
    WrappingPowEquals,
    SaturatingPowEquals,

    Times,
    WrappingTimes,
    SaturatingTimes,
    TimesEquals,
    WrappingTimesEquals,
    SaturatingTimesEquals,

    Divide,
    WrappingDivide,
    SaturatingDivide,
    DivideEquals,
    WrappingDivideEquals,
    SaturatingDivideEquals,

    Remainder,
    RemainderEquals,

    Plus,
    WrappingPlus,
    SaturatingPlus,
    PlusEquals,
    WrappingPlusEquals,
    SaturatingPlusEquals,

    Minus,
    WrappingMinus,
    SaturatingMinus,
    MinusEquals,
    WrappingMinusEquals,
    SaturatingMinusEquals,

    LeftShift,
    WrappingLeftShift,
    SaturatingLeftShift,
    LeftShiftEquals,
    WrappingLeftShiftEquals,
    SaturatingLeftShiftEquals,

    RightShift,
    RightShiftEquals,

    LeftRotate,
    LeftRotateEquals,
    RightRotate,
    RightRotateEquals,

    BitAnd,
    BitAndEquals,

    BitXor,
    BitXorEquals,

    BitOr,
    BitOrEquals,

    And,
    AndEquals,

    Or,
    OrEquals,

    Compare,
    EqualsEquals,
    NotEquals,
    Greater,
    GreaterOrEquals,
    Less,
    LessOrEquals,
}

impl Display for Op {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Len       => write!(f, "len"),
            Self::Equals    => write!(f, "="),
            Self::Not       => write!(f, "!"),

            Self::Pow                   => write!(f,  "**"),
            Self::WrappingPow           => write!(f, r"**\"),
            Self::SaturatingPow         => write!(f,  "**|"),
            Self::PowEquals             => write!(f,  "**="),
            Self::WrappingPowEquals     => write!(f, r"**\="),
            Self::SaturatingPowEquals   => write!(f,  "**|="),

            Self::Times                 => write!(f,  "*"),
            Self::WrappingTimes         => write!(f, r"*\"),
            Self::SaturatingTimes       => write!(f,  "*|"),
            Self::TimesEquals           => write!(f,  "*="),
            Self::WrappingTimesEquals   => write!(f, r"*\="),
            Self::SaturatingTimesEquals => write!(f,  "*|="),

            Self::Divide                    => write!(f,  "/"),
            Self::WrappingDivide            => write!(f, r"/\"),
            Self::SaturatingDivide          => write!(f,  "/|"),
            Self::DivideEquals              => write!(f,  "/="),
            Self::WrappingDivideEquals      => write!(f, r"/\="),
            Self::SaturatingDivideEquals    => write!(f,  "/|="),

            Self::Remainder         => write!(f, "%"),
            Self::RemainderEquals   => write!(f, "%="),

            Self::Plus                  => write!(f,  "+"), // also unary safe absolute value
            Self::WrappingPlus          => write!(f, r"+\"), // also unary wrapping absolute value
            Self::SaturatingPlus        => write!(f,  "+|"), // also unary saturating absolute value
            Self::PlusEquals            => write!(f,  "+="),
            Self::WrappingPlusEquals    => write!(f, r"+\="),
            Self::SaturatingPlusEquals  => write!(f,  "+|="),

            Self::Minus                 => write!(f,  "-"), // also unary integer negation
            Self::WrappingMinus         => write!(f, r"-\"), // also unary wrapping integer negation
            Self::SaturatingMinus       => write!(f,  "-|"), // also unary saturating integer negation
            Self::MinusEquals           => write!(f,  "-="),
            Self::WrappingMinusEquals   => write!(f, r"-\="),
            Self::SaturatingMinusEquals => write!(f,  "-|="),

            Self::LeftShift                 => write!(f,  "<<"),
            Self::WrappingLeftShift         => write!(f, r"<<\"),
            Self::SaturatingLeftShift       => write!(f,  "<<|"),
            Self::LeftShiftEquals           => write!(f,  "<<="),
            Self::WrappingLeftShiftEquals   => write!(f, r"<<\="),
            Self::SaturatingLeftShiftEquals => write!(f,  "<<|="),

            Self::RightShift        => write!(f,  ">>"),
            Self::RightShiftEquals  => write!(f,  ">>="),

            Self::LeftRotate        => write!(f, "<<<"),
            Self::LeftRotateEquals  => write!(f, "<<<="),
            Self::RightRotate       => write!(f, ">>>"),
            Self::RightRotateEquals => write!(f, ">>>="),

            Self::BitAnd        => write!(f, "&"),
            Self::BitAndEquals  => write!(f, "&="),

            Self::BitOr         => write!(f, "|"),
            Self::BitOrEquals   => write!(f, "|="),

            Self::BitXor        => write!(f, "^"),
            Self::BitXorEquals  => write!(f, "^="),

            Self::And       => write!(f, "&&"),
            Self::AndEquals => write!(f, "&&="),

            Self::Or        => write!(f, "||"),
            Self::OrEquals  => write!(f, "||="),

            Self::Compare           => write!(f, "<=>"),
            Self::EqualsEquals      => write!(f, "=="),
            Self::NotEquals         => write!(f, "!="),
            Self::Greater           => write!(f, ">"),
            Self::GreaterOrEquals   => write!(f, ">="),
            Self::Less              => write!(f, "<"),
            Self::LessOrEquals      => write!(f, "<="),
        };
    }
}

impl DisplayLen for Op {
    #[inline(always)]
    fn display_len(&self) -> usize {
        return match self {
            Self::Len => 3,
            Self::Equals => 1,
            Self::Not => 1,

            Self::Pow => 2,
            Self::WrappingPow => 3,
            Self::SaturatingPow => 3,
            Self::PowEquals => 3,
            Self::WrappingPowEquals => 4,
            Self::SaturatingPowEquals => 4,

            Self::Times => 1,
            Self::WrappingTimes => 2,
            Self::SaturatingTimes => 2,
            Self::TimesEquals => 2,
            Self::WrappingTimesEquals => 3,
            Self::SaturatingTimesEquals => 3,

            Self::Divide => 1,
            Self::WrappingDivide => 2,
            Self::SaturatingDivide => 2,
            Self::DivideEquals => 2,
            Self::WrappingDivideEquals => 3,
            Self::SaturatingDivideEquals => 3,

            Self::Remainder => 1,
            Self::RemainderEquals => 2,

            Self::Plus => 1,
            Self::WrappingPlus => 2,
            Self::SaturatingPlus => 2,
            Self::PlusEquals => 2,
            Self::WrappingPlusEquals => 3,
            Self::SaturatingPlusEquals => 3,

            Self::Minus => 1,
            Self::WrappingMinus => 2,
            Self::SaturatingMinus => 2,
            Self::MinusEquals => 2,
            Self::WrappingMinusEquals => 3,
            Self::SaturatingMinusEquals => 3,

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
            Self::WrappingLeftShift => 3,
            Self::SaturatingLeftShift => 3,
            Self::LeftShiftEquals => 3,
            Self::WrappingLeftShiftEquals => 4,
            Self::SaturatingLeftShiftEquals => 4,

            Self::RightShift => 2,
            Self::RightShiftEquals => 3,

            Self::LeftRotate => 3,
            Self::LeftRotateEquals => 4,
            Self::RightRotate => 3,
            Self::RightRotateEquals => 4,

            Self::EqualsEquals => 2,
            Self::NotEquals => 2,
            Self::Greater => 1,
            Self::GreaterOrEquals => 2,
            Self::Less => 1,
            Self::LessOrEquals => 2,
            Self::Compare => 3,
        };
    }
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
        return match self {
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
        };
    }
}

impl DisplayLen for TokenKind<'_> {
    #[inline(always)]
    fn display_len(&self) -> usize {
        return match self {
            Self::Comment(text) => text.chars().count(),
            Self::Unexpected(text) => text.chars().count(),

            Self::Bracket(bracket) => bracket.display_len(),
            Self::Colon => 1,
            Self::SemiColon => 1,
            Self::Comma => 1,
            Self::Op(op) => op.display_len(),

            Self::Literal(typ) => typ.display_len(),
            Self::True => 4,
            Self::False => 5,
            Self::Identifier(name) => name.chars().count(),
            Self::Definition(kind) => kind.display_len(),

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
        };
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
    errors: Vec<Error<ErrorKind>>,

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
    ) -> Result<Vec<Token<'src>>, Vec<Error<ErrorKind>>> {
        let Some(first_line) = src.lines.first() else {
            return Ok(Vec::new());
        };

        let mut this = Self {
            src,
            errors: Vec::new(),
            col: 0,
            token_start_col: 0,
            line_index: 0,
            line: first_line,
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
            this.errors.push(Error {
                kind: ErrorKind::UnclosedBracket(bracket.kind),
                col: bracket.col,
                pointers_count: 1,
            });
        }

        return if this.errors.is_empty() {
            Ok(this.tokens)
        } else {
            Err(this.errors)
        };
    }
}

// TODO(stefano): own utf8 parsing
// TODO(stefano): properly handle multi-char utf8 characters (i.e. emojis)
// iteration of characters
impl<'src> Tokenizer<'src> {
    fn token_len(&self) -> usize {
        return self.src.code[self.token_start_col..self.col].chars().count();
    }

    fn next_ascii_char(&mut self) -> Result<Option<ascii>, Error<ErrorKind>> {
        let Some(next) = self.src.code.as_bytes().get(self.col) else {
            return Ok(None);
        };

        return match next {
            ascii_ch @ 0..=b'\x7F' => {
                self.col += 1;
                Ok(Some(*ascii_ch))
            }
            _utf8_ch => {
                let rest_of_line = &self.src.code[self.col..self.line.end];

                let Some(utf8_ch) = rest_of_line.chars().next() else {
                    unreachable!("this branch assured we would have a valid utf8 character");
                };

                let utf8_ch_col = self.col;
                self.col += utf8_ch.len_utf8();
                Err(Error {
                    kind: ErrorKind::Utf8Character(utf8_ch),
                    col: utf8_ch_col,
                    pointers_count: 1, // TODO(stefano): proper utf8 len
                })
            }
        };
    }

    fn next_utf8_char(&mut self) -> Option<utf8> {
        let next = self.src.code.as_bytes().get(self.col)?;
        return match next {
            ascii_ch @ 0..=b'\x7F' => {
                self.col += 1;
                Some(*ascii_ch as utf8)
            }
            _utf8_ch => {
                let rest_of_line = &self.src.code[self.col..self.line.end];

                let Some(utf8_ch) = rest_of_line.chars().next() else {
                    unreachable!("this branch assured we would have a valid utf8 character");
                };

                self.col += utf8_ch.len_utf8();
                Some(utf8_ch)
            }
        };
    }

    fn peek_next_ascii_char(&self) -> Result<Option<&'src ascii>, Error<ErrorKind>> {
        let Some(next) = self.src.code.as_bytes().get(self.col) else {
            return Ok(None);
        };

        return match next {
            ascii_ch @ 0..=b'\x7F' => Ok(Some(ascii_ch)),
            _utf8_ch => {
                let rest_of_line = &self.src.code[self.col..self.line.end];

                let Some(utf8_ch) = rest_of_line.chars().next() else {
                    unreachable!("this branch assured we would have a valid utf8 character");
                };

                Err(Error {
                    kind: ErrorKind::Utf8Character(utf8_ch),
                    col: self.col,
                    pointers_count: 1, // TODO(stefano): proper utf8 len
                })
            }
        };
    }

    fn peek_next_utf8_char(&mut self) -> Option<utf8> {
        let next = self.src.code.as_bytes().get(self.col)?;
        return match next {
            ascii_ch @ 0..=b'\x7F' => Some(*ascii_ch as utf8),
            _utf8_ch => {
                let rest_of_line = &self.src.code[self.col..self.line.end];

                let Some(utf8_ch) = rest_of_line.chars().next() else {
                    unreachable!("this branch assured we would have a valid utf8 character");
                };

                Some(utf8_ch)
            }
        };
    }

    fn next_in_ascii_char_literal(&mut self) -> Result<ascii, Error<ErrorKind>> {
        return match self.src.code.as_bytes().get(self.col) {
            Some(b'\n') | None => Err(Error {
                kind: ErrorKind::UnclosedCharacterLiteral,
                col: self.token_start_col,
                pointers_count: self.token_len(),
            }),
            Some(ascii_ch @ 0..=b'\x7F') => {
                self.col += 1;
                Ok(*ascii_ch)
            }
            Some(_utf8_ch) => {
                let rest_of_line = &self.src.code[self.col..self.line.end];

                let Some(utf8_ch) = rest_of_line.chars().next() else {
                    unreachable!("this branch assured we would have a valid utf8 character");
                };

                let utf8_ch_col = self.col;
                self.col += utf8_ch.len_utf8();
                Err(Error {
                    kind: ErrorKind::Utf8Character(utf8_ch),
                    col: utf8_ch_col,
                    pointers_count: 1, // TODO(stefano): proper utf8 len
                })
            }
        };
    }

    fn next_in_ascii_str_literal(&mut self) -> Result<ascii, Error<ErrorKind>> {
        return match self.src.code.as_bytes().get(self.col) {
            Some(b'\n') | None => Err(Error {
                kind: ErrorKind::UnclosedStringLiteral,
                col: self.token_start_col,
                pointers_count: self.token_len(),
            }),
            Some(ascii_ch @ 0..=b'\x7F') => {
                self.col += 1;
                Ok(*ascii_ch)
            }
            Some(_utf8_ch) => {
                let rest_of_line = &self.src.code[self.col..self.line.end];

                let Some(utf8_ch) = rest_of_line.chars().next() else {
                    unreachable!("this branch assured we would have a valid utf8 character");
                };

                let utf8_ch_col = self.col;
                self.col += utf8_ch.len_utf8();
                Err(Error {
                    kind: ErrorKind::Utf8Character(utf8_ch),
                    col: utf8_ch_col,
                    pointers_count: 1, // TODO(stefano): proper utf8 len
                })
            }
        };
    }
}

impl<'src> Tokenizer<'src> {
    fn identifier(&mut self) -> Result<TokenKind<'src>, Error<ErrorKind>> {
        let mut contains_utf8 = false;
        loop {
            match self.peek_next_ascii_char() {
                Ok(Some(b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_')) => {}
                Ok(Some(_) | None) => break,
                Err(_) => contains_utf8 = true,
            }

            _ = self.next_ascii_char();
        }

        if contains_utf8 {
            return Err(Error {
                kind: ErrorKind::Utf8InIdentifier,
                col: self.token_start_col,
                pointers_count: self.token_len(),
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
            "len" => TokenKind::Op(Op::Len),
            identifier => TokenKind::Identifier(identifier),
        };

        return Ok(identifier);
    }

    fn next_token(
        &mut self,
        next: ascii,
    ) -> Result<TokenKind<'src>, Error<ErrorKind>> {
        return match next {
            b'r' => match self.peek_next_utf8_char() {
                Some('"') => {
                    self.col += 1;

                    let previous_error_count = self.errors.len();
                    let mut raw_string = Vec::<ascii>::new();

                    loop {
                        let next_ch = match self.next_in_ascii_str_literal()? {
                            control @ (b'\x00'..=b'\x1F' | b'\x7F') => {
                                self.errors.push(Error {
                                    kind: ErrorKind::ControlCharacterInStringLiteral(control as utf8),
                                    col: self.col - 1,
                                    pointers_count: 1,
                                });
                                control
                            }
                            b'"' => break,
                            ch => ch,
                        };

                        raw_string.push(next_ch);
                    }

                    if self.errors.len() > previous_error_count {
                        let Some(last_error) = self.errors.pop() else {
                            unreachable!("we are now sure that at least one error has occured");
                        };

                        Err(last_error)
                    } else {
                        Ok(TokenKind::Literal(Literal::Str(raw_string)))
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

                    _ = self.next_ascii_char();
                }

                let token_text = &self.src.code[self.token_start_col..self.col];
                match token_text.parse() {
                    Ok(integer) => Ok(TokenKind::Literal(Literal::Int(integer))),
                    Err(err) => {
                        let kind = match err.kind() {
                            IntErrorKind::InvalidDigit => {
                                if contains_utf8 {
                                    ErrorKind::Utf8InNumberLiteral
                                } else {
                                    ErrorKind::NonDigitInNumberLiteral
                                }
                            }
                            IntErrorKind::PosOverflow => {
                                ErrorKind::IntOverflow { bits: int::BITS, max: int::MAX }
                            }
                            IntErrorKind::NegOverflow => {
                                ErrorKind::IntUnderflow { bits: int::BITS, min: int::MIN }
                            }
                            IntErrorKind::Empty => unreachable!("should never parse empty numbers"),
                            IntErrorKind::Zero => unreachable!("numbers can also be zero"),
                            _ => ErrorKind::InvalidNumberLiteral(err),
                        };

                        Err(Error {
                            kind,
                            col: self.token_start_col,
                            pointers_count: self.token_len(),
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
                    _ = self.next_utf8_char();
                }

                Ok(TokenKind::Comment(comment))
            }
            /*
            FIX(stefano): add proper multiple error handling, maybe implement an error pool to
            avoid having to allocate a new vector each time
            */
            b'"' => {
                let previous_error_count = self.errors.len();
                let mut string = Vec::<ascii>::new();

                loop {
                    let next_ch = match self.next_in_ascii_str_literal()? {
                        b'\\' => match self.next_in_ascii_str_literal()? {
                            b'\\' => b'\\',
                            b'\'' => b'\'',
                            b'"' => b'"',
                            b'n' => b'\n',
                            b'r' => b'\r',
                            b't' => b'\t',
                            b'0' => b'\0',
                            unrecognized => {
                                self.errors.push(Error {
                                    kind: ErrorKind::UnrecognizedEscapeCharacterInStringLiteral(
                                        unrecognized as utf8,
                                    ),
                                    col: self.col - 2,
                                    pointers_count: 2,
                                });
                                string.push(b'\\');
                                unrecognized
                            }
                        },
                        control @ (b'\x00'..=b'\x1F' | b'\x7F') => {
                            self.errors.push(Error {
                                kind: ErrorKind::ControlCharacterInStringLiteral(control as utf8),
                                col: self.col - 1,
                                pointers_count: 1,
                            });
                            control
                        }
                        b'"' => break,
                        ch => ch,
                    };

                    string.push(next_ch);
                }

                if self.errors.len() > previous_error_count {
                    let Some(last_error) = self.errors.pop() else {
                        unreachable!("we are now sure that at least one error has occured");
                    };

                    Err(last_error)
                } else {
                    Ok(TokenKind::Literal(Literal::Str(string)))
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
                        unrecognized => Err(Error {
                            kind: ErrorKind::UnrecognizedEscapeCharacterInCharacterLiteral(unrecognized as utf8),
                            col: self.col - 2,
                            pointers_count: 2,
                        }),
                    },
                    control @ (b'\x00'..=b'\x1F' | b'\x7F') => Err(Error {
                        kind: ErrorKind::ControlCharacterInCharacterLiteral(control as utf8),
                        col: self.col - 1,
                        pointers_count: 1,
                    }),
                    b'\'' => Err(Error {
                        kind: ErrorKind::EmptyCharacterLiteral,
                        col: self.token_start_col,
                        pointers_count: 2,
                    }),
                    ch => Ok(ch),
                };

                let Some(b'\'') = self.peek_next_ascii_char()? else {
                    return Err(Error {
                        kind: ErrorKind::UnclosedCharacterLiteral,
                        col: self.token_start_col,
                        pointers_count: self.token_len(),
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
                    actual @ (BracketKind::OpenCurly | BracketKind::OpenSquare) => Err(Error {
                        kind: ErrorKind::MismatchedBracket {
                            expected: BracketKind::CloseRound,
                            actual,
                        },
                        col: self.token_start_col,
                        pointers_count: 1,
                    }),
                },
                None => Err(Error {
                    kind: ErrorKind::UnopenedBracket(BracketKind::CloseRound),
                    col: self.token_start_col,
                    pointers_count: 1,
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
                    actual @ (BracketKind::OpenCurly | BracketKind::OpenRound) => Err(Error {
                        kind: ErrorKind::MismatchedBracket {
                            expected: BracketKind::CloseSquare,
                            actual,
                        },
                        col: self.token_start_col,
                        pointers_count: 1,
                    }),
                },
                None => Err(Error {
                    kind: ErrorKind::UnopenedBracket(BracketKind::CloseSquare),
                    col: self.token_start_col,
                    pointers_count: 1,
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
                    actual @ (BracketKind::OpenRound | BracketKind::OpenSquare) => Err(Error {
                        kind: ErrorKind::MismatchedBracket {
                            expected: BracketKind::CloseCurly,
                            actual,
                        },
                        col: self.token_start_col,
                        pointers_count: 1,
                    }),
                },
                None => Err(Error {
                    kind: ErrorKind::UnopenedBracket(BracketKind::CloseCurly),
                    col: self.token_start_col,
                    pointers_count: 1,
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
                        Some('\\') => {
                            self.col += 1;
                            match self.peek_next_utf8_char() {
                                Some('=') => {
                                    self.col += 1;
                                    Ok(TokenKind::Op(Op::WrappingPowEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::WrappingPow)),
                            }
                        }
                        Some('|') => {
                            self.col += 1;
                            match self.peek_next_utf8_char() {
                                Some('=') => {
                                    self.col += 1;
                                    Ok(TokenKind::Op(Op::SaturatingPowEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::SaturatingPow)),
                            }
                        }
                        _ => Ok(TokenKind::Op(Op::Pow)),
                    }
                }
                Some('=') => {
                    self.col += 1;
                    Ok(TokenKind::Op(Op::TimesEquals))
                }
                Some('\\') => {
                    self.col += 1;
                    match self.peek_next_utf8_char() {
                        Some('=') => {
                            self.col += 1;
                            Ok(TokenKind::Op(Op::WrappingTimesEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::WrappingTimes)),
                    }
                }
                Some('|') => {
                    self.col += 1;
                    match self.peek_next_utf8_char() {
                        Some('=') => {
                            self.col += 1;
                            Ok(TokenKind::Op(Op::SaturatingTimesEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::SaturatingTimes)),
                    }
                }
                _ => Ok(TokenKind::Op(Op::Times)),
            },
            b'/' => match self.peek_next_utf8_char() {
                Some('=') => {
                    self.col += 1;
                    Ok(TokenKind::Op(Op::DivideEquals))
                }
                Some('\\') => {
                    self.col += 1;
                    match self.peek_next_utf8_char() {
                        Some('=') => {
                            self.col += 1;
                            Ok(TokenKind::Op(Op::WrappingDivideEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::WrappingDivide)),
                    }
                }
                Some('|') => {
                    self.col += 1;
                    match self.peek_next_utf8_char() {
                        Some('=') => {
                            self.col += 1;
                            Ok(TokenKind::Op(Op::SaturatingDivideEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::SaturatingDivide)),
                    }
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
                Some('\\') => {
                    self.col += 1;
                    match self.peek_next_utf8_char() {
                        Some('=') => {
                            self.col += 1;
                            Ok(TokenKind::Op(Op::WrappingPlusEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::WrappingPlus)),
                    }
                }
                Some('|') => {
                    self.col += 1;
                    match self.peek_next_utf8_char() {
                        Some('=') => {
                            self.col += 1;
                            Ok(TokenKind::Op(Op::SaturatingPlusEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::SaturatingPlus)),
                    }
                }
                _ => Ok(TokenKind::Op(Op::Plus)),
            },
            b'-' => match self.peek_next_utf8_char() {
                Some('=') => {
                    self.col += 1;
                    Ok(TokenKind::Op(Op::MinusEquals))
                }
                Some('\\') => {
                    self.col += 1;
                    match self.peek_next_utf8_char() {
                        Some('=') => {
                            self.col += 1;
                            Ok(TokenKind::Op(Op::WrappingMinusEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::WrappingMinus)),
                    }
                }
                Some('|') => {
                    self.col += 1;
                    match self.peek_next_utf8_char() {
                        Some('=') => {
                            self.col += 1;
                            Ok(TokenKind::Op(Op::SaturatingMinusEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::SaturatingMinus)),
                    }
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
                        Some('>') => {
                            self.col += 1;
                            match self.peek_next_utf8_char() {
                                Some('=') => {
                                    self.col += 1;
                                    Ok(TokenKind::Op(Op::RightRotateEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::RightRotate)),
                            }
                        }
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
                        Some('<') => {
                            self.col += 1;
                            match self.peek_next_utf8_char() {
                                Some('=') => {
                                    self.col += 1;
                                    Ok(TokenKind::Op(Op::LeftRotateEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::LeftRotate)),
                            }
                        }
                        Some('=') => {
                            self.col += 1;
                            Ok(TokenKind::Op(Op::LeftShiftEquals))
                        }
                        Some('\\') => {
                            self.col += 1;
                            match self.peek_next_utf8_char() {
                                Some('=') => {
                                    self.col += 1;
                                    Ok(TokenKind::Op(Op::WrappingLeftShiftEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::WrappingLeftShift)),
                            }
                        }
                        Some('|') => {
                            self.col += 1;
                            match self.peek_next_utf8_char() {
                                Some('=') => {
                                    self.col += 1;
                                    Ok(TokenKind::Op(Op::SaturatingLeftShiftEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::SaturatingLeftShift)),
                            }
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
            unrecognized => Err(Error {
                kind: ErrorKind::UnrecognizedCharacter(unrecognized as utf8),
                col: self.token_start_col,
                pointers_count: 1,
            }),
        };
    }
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    UnclosedBracket(BracketKind),
    UnopenedBracket(BracketKind),
    MismatchedBracket { actual: BracketKind, expected: BracketKind },

    UnclosedCharacterLiteral,
    ControlCharacterInCharacterLiteral(utf8),
    UnrecognizedEscapeCharacterInCharacterLiteral(utf8),
    EmptyCharacterLiteral,

    UnclosedStringLiteral,
    ControlCharacterInStringLiteral(utf8),
    UnrecognizedEscapeCharacterInStringLiteral(utf8),

    Utf8InNumberLiteral,
    NonDigitInNumberLiteral,
    IntOverflow { bits: u32, max: isize },
    IntUnderflow { bits: u32, min: isize },
    InvalidNumberLiteral(ParseIntError),

    Utf8InIdentifier,

    Utf8Character(utf8),
    UnrecognizedCharacter(utf8),
}

impl IntoErrorInfo for ErrorKind {
    fn info(&self) -> ErrorInfo {
        let (error_message, error_cause_message) = match self {
            Self::UnclosedBracket(bracket) => (
                format!("unclosed '{bracket}' bracket").into(),
                "was not closed".into(),
            ),
            Self::UnopenedBracket(bracket) => (
                format!("unopened '{bracket}' bracket").into(),
                "was not opened before".into(),
            ),
            Self::MismatchedBracket { actual, expected } => (
                "mismatched bracket".into(),
                format!("'{actual}' closes the wrong bracket, expected a '{expected}' instead").into()
            ),

            Self::UnclosedCharacterLiteral => (
                "unclosed character literal".into(),
                "missing closing ' quote".into(),
            ),
            Self::ControlCharacterInCharacterLiteral(control_character) => (
                "invalid character literal".into(),
                format!("'{control_character}' control character not allowed").into(),
            ),
            Self::UnrecognizedEscapeCharacterInCharacterLiteral(unrecognized) => (
                "invalid character literal".into(),
                format!("unrecognized '{unrecognized}' escape character").into(),
            ),

            Self::EmptyCharacterLiteral => (
                "empty character literal".into(),
                "must not be empty".into(),
            ),

            Self::UnclosedStringLiteral => (
                "unclosed string literal".into(),
                "missing closing \" quote".into(),
            ),
            Self::ControlCharacterInStringLiteral(control_character) => (
                "invalid string literal".into(),
                format!("'{control_character}' control character not allowed").into(),
            ),
            Self::UnrecognizedEscapeCharacterInStringLiteral(unrecognized) => (
                "invalid string literal".into(),
                format!("unrecognized '{unrecognized}' escape character").into(),
            ),

            Self::Utf8InNumberLiteral => (
                "invalid number literal".into(),
                "must not contain utf8 characters".into(),
            ),
            Self::NonDigitInNumberLiteral => (
                "invalid number literal".into(),
                "must not contain non-digit characters".into(),
            ),
            Self::IntOverflow { bits, max } => (
                "number literal overflow".into(),
                format!("overflows a {bits} bit signed integer (over {max})").into(),
            ),
            Self::IntUnderflow { bits, min } => (
                "number literal underflow".into(),
                format!("underlows a {bits} bit signed integer (under {min})").into(),
            ),
            Self::InvalidNumberLiteral(err) => (
                "invalid number literal".into(),
                format!("{err}").into(),
            ),

            Self::Utf8InIdentifier => (
                "invalid identifier".into(),
                "must not contain utf8 characters".into(),
            ),

            Self::Utf8Character(character) => (
                "invalid character".into(),
                format!("utf8 character '{character}' ({}) are not allowed", character.escape_unicode()).into(),
            ),
            Self::UnrecognizedCharacter(unrecognized) => (
                "invalid character".into(),
                format!("unrecognized '{unrecognized}' ({}) character", unrecognized.escape_unicode()).into(),
            ),

        };

        return ErrorInfo { error_message, error_cause_message };
    }
}
