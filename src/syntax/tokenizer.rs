use super::{Error, ErrorInfo, IntoErrorInfo};
use crate::src_file::{offset, Line, SrcFile};
use core::fmt::Display;
pub(super) trait DisplayLen {
    fn display_len(&self) -> offset;
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

#[repr(transparent)]
#[derive(Debug, Clone, Copy)]
pub(crate) struct Integer<'src>(pub(crate) &'src [ascii]);

#[repr(u8)]
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum Base {
    #[default]
    Decimal = 10,
    Binary = 0b10,
    Octal = 0o10,
    Hexadecimal = 0x10,
}

impl Base {
    #[must_use]
    pub const fn prefix(self) -> &'static str {
        return match self {
            Self::Decimal => "",
            Self::Binary => "0b",
            Self::Octal => "0o",
            Self::Hexadecimal => "0x",
        };
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MissingDigitsBase {
    Binary = 0b10,
    Octal = 0o10,
    Hexadecimal = 0x10,
}

impl MissingDigitsBase {
    #[must_use]
    pub const fn prefix(self) -> &'static str {
        return match self {
            Self::Binary => "0b",
            Self::Octal => "0o",
            Self::Hexadecimal => "0x",
        };
    }
}

#[repr(transparent)]
#[derive(Debug, Clone)]
pub(crate) struct Str(pub(crate) Box<[ascii]>);

#[repr(transparent)]
#[derive(Debug, Clone, Copy)]
pub(crate) struct RawStr<'src>(pub(crate) &'src [ascii]);

#[derive(Debug, Clone, Copy)]
pub(crate) enum Mutability {
    Let,
    Var,
}

impl Display for Mutability {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return match self {
            Self::Let => write!(f, "let"),
            Self::Var => write!(f, "var"),
        };
    }
}

impl DisplayLen for Mutability {
    #[inline(always)]
    fn display_len(&self) -> offset {
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
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
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
    fn display_len(&self) -> offset {
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
    col: offset,
}

/* IDEA(stefano):
introduce "unchecked" operators
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
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
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
    fn display_len(&self) -> offset {
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

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Quote {
    Single = b'\'',
    Double = b'"',
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QuotedLiteralKind {
    Ascii,
    Str,
    RawStr,
}

impl Display for QuotedLiteralKind {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return match self {
            Self::Ascii => write!(f, "character"),
            Self::Str => write!(f, "string"),
            Self::RawStr => write!(f, "raw string"),
        };
    }
}

impl QuotedLiteralKind {
    #[must_use]
    pub const fn quote(self) -> Quote {
        return match self {
            Self::Ascii => Quote::Single,
            Self::Str | Self::RawStr => Quote::Double,
        };
    }
}

#[derive(Debug, Clone)]
pub(crate) enum TokenKind<'src> {
    Comment(&'src str),
    Unexpected(&'src str),

    // Symbols
    Bracket(BracketKind),
    Colon,
    SemiColon,
    Comma,
    Op(Op),

    // Literal values
    False,
    True,
    /// integer literals are never empty and always contain valid ascii digits
    Integer(Base, Integer<'src>),
    Ascii(ascii),
    /* IDEA(stefano):
    or treat string with no escapes as raw strings
    or split into EscapedStr(Str) and Str(&'src [ascii])
    or remove Str(Str) and only escape the characters during compilation
    */
    /* IDEA(stefano):
    limit string literals to a max amount of logical characters (escapes are considered a single character)
    e.g. 63/127/255/511/1023/2047/4095
    */
    Str(Str),
    RawStr(RawStr<'src>),

    /* IDEA(stefano):
    create:
    struct ShortStr<'src> {
        len: offset,
        ptr: offset,
        _ptr: PhantomData<&'src ascii>,
    }
    */
    Identifier(&'src str),

    // Keywords
    /// temporary way of printing values to stdout
    Print,
    /// temporary way of printing values followed by a newline to stdout
    PrintLn,
    /// temporary way of printing values to stderr
    Eprint,
    /// temporary way of printing values followed by a newline to stderr
    EprintLn,

    Mutability(Mutability),
    Do,
    If,
    Else,
    Loop,
    Break,
    Continue,
}

impl Display for TokenKind<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return match self {
            Self::Comment(text) => write!(f, "#{text}"),
            Self::Unexpected(text) => write!(f, "{text}"),

            Self::Bracket(bracket) => write!(f, "{bracket}"),
            Self::Colon => write!(f, ":"),
            Self::SemiColon => write!(f, ";"),
            Self::Comma => write!(f, ","),
            Self::Op(op) => write!(f, "{op}"),

            Self::False => write!(f, "false"),
            Self::True => write!(f, "true"),
            Self::Integer(base, integer) => {
                let integer_str = unsafe { core::str::from_utf8_unchecked(integer.0) };
                write!(f, "{prefix}{integer_str}", prefix = base.prefix())
            }
            Self::Ascii(code) => write!(f, "'{}'", code.escape_ascii()),
            Self::Str(string) => {
                write!(f, "\"")?;
                for ch in &*string.0 {
                    write!(f, "{}", ch.escape_ascii())?;
                }
                write!(f, "\"")
            }
            Self::RawStr(string) => {
                write!(f, "r\"")?;
                for ch in string.0 {
                    write!(f, "{}", *ch as utf8)?;
                }
                write!(f, "\"")
            }

            Self::Identifier(name) => write!(f, "{name}"),

            Self::Print => write!(f, "print"),
            Self::PrintLn => write!(f, "println"),
            Self::Eprint => write!(f, "eprint"),
            Self::EprintLn => write!(f, "eprintln"),

            Self::Mutability(kind) => write!(f, "{kind}"),
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
    fn display_len(&self) -> offset {
        #[inline(always)]
        fn ascii_escaped_len(ascii_char: ascii) -> offset {
            // Note: ascii type guarantees the value to be valid utf8
            let utf8_char = ascii_char as utf8;
            return utf8_char.escape_debug().len() as offset;
        }

        return match self {
            Self::Comment(text) => text.chars().count() as offset,
            Self::Unexpected(text) => text.chars().count() as offset,

            Self::Bracket(bracket) => bracket.display_len(),
            Self::Colon => 1,
            Self::SemiColon => 1,
            Self::Comma => 1,
            Self::Op(op) => op.display_len(),

            Self::Integer(base, integer) => {
                base.prefix().len() as offset + integer.0.len() as offset
            }
            Self::Ascii(ascii_char) => ascii_escaped_len(*ascii_char) + 2, // + 2 to account for the quotes
            Self::True => 4,
            Self::False => 5,
            Self::Str(text) => {
                let mut len = 2; // starting at 2 to account for the quotes
                for ascii_char in &*text.0 {
                    len += ascii_escaped_len(*ascii_char);
                }
                len
            }
            Self::RawStr(text) => text.0.len() as offset + 3, // + 1 for the `r` prefix, and + 2 for the quotes

            Self::Identifier(name) => name.len() as offset,

            Self::Print => 5,
            Self::PrintLn => 7,
            Self::Eprint => 6,
            Self::EprintLn => 8,

            Self::Mutability(kind) => kind.display_len(),
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
    pub(crate) col: offset,
}

#[derive(Debug)]
pub struct Tokenizer<'src> {
    src: &'src SrcFile,
    errors: Vec<Error<ErrorKind>>,
    token_errors: Vec<Error<ErrorKind>>,

    col: offset,
    token_start_col: offset,

    line_index: offset,
    line: &'src Line,

    tokens: Vec<Token<'src>>,
    brackets: Vec<Bracket>,
}

impl<'src> Tokenizer<'src> {
    pub fn tokenize(src: &'src SrcFile) -> Result<Vec<Token<'src>>, Vec<Error<ErrorKind>>> {
        let Some(first_line) = src.lines.first() else {
            return Ok(Vec::new());
        };

        let mut this = Self {
            src,
            errors: Vec::new(),
            token_errors: Vec::new(),
            col: 0,
            token_start_col: 0,
            line_index: 0,
            line: first_line,
            tokens: Vec::new(),
            brackets: Vec::new(),
        };

        let src_lines_len = this.src.lines.len() as offset;
        'tokenization: loop {
            let token_kind_result = 'skip_whitespace: loop {
                this.token_start_col = this.col;

                let next = match this.next_ascii_char() {
                    Ok(Some(ch)) => ch,
                    Ok(None) => break 'tokenization,
                    Err(err) => {
                        this.token_errors.push(Error {
                            kind: ErrorKind::Utf8Character(err.character),
                            col: err.col,
                            pointers_count: err.len,
                        });
                        break 'skip_whitespace Err(());
                    }
                };

                match next {
                    // ignore whitespace
                    b' ' | b'\t' | b'\r' | b'\x0C' => {}

                    // next line
                    b'\n' => {
                        if this.line_index >= src_lines_len - 1 {
                            break 'tokenization;
                        }

                        this.line_index += 1;
                        this.line = &this.src.lines[this.line_index as usize];
                    }
                    ch => break 'skip_whitespace this.next_token(ch),
                }
            };

            let kind = match token_kind_result {
                Ok(kind) => kind,
                Err(()) => {
                    this.errors.extend_from_slice(&this.token_errors);
                    this.token_errors.clear();
                    TokenKind::Unexpected(
                        &this.src.code[this.token_start_col as usize..this.col as usize],
                    )
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

        return if this.errors.is_empty() { Ok(this.tokens) } else { Err(this.errors) };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Utf8Error {
    character: utf8,
    col: offset,
    len: offset,
}

// TODO(stefano): own utf8 parsing
// TODO(stefano): properly handle multi-char utf8 characters (i.e. emojis)
// iteration of characters
impl<'src> Tokenizer<'src> {
    fn token_len(&self) -> offset {
        return self.src.code[self.token_start_col as usize..self.col as usize].chars().count()
            as offset;
    }

    fn next_ascii_char(&mut self) -> Result<Option<ascii>, Utf8Error> {
        let Some(next) = self.src.code.as_bytes().get(self.col as usize) else {
            return Ok(None);
        };

        return match next {
            ascii_ch @ 0..=b'\x7F' => {
                self.col += 1;
                Ok(Some(*ascii_ch))
            }
            _utf8_ch => {
                let rest_of_line = &self.src.code[self.col as usize..self.line.end as usize];
                let Some(utf8_ch) = rest_of_line.chars().next() else {
                    unreachable!("this branch assured we would have a valid utf8 character");
                };

                let utf8_ch_col = self.col;
                self.col += utf8_ch.len_utf8() as offset;
                Err(Utf8Error {
                    character: utf8_ch,
                    col: utf8_ch_col,
                    len: 1, // TODO(stefano): proper utf8 len
                })
            }
        };
    }

    fn next_utf8_char(&mut self) -> Option<utf8> {
        let next = self.src.code.as_bytes().get(self.col as usize)?;
        return match next {
            ascii_ch @ 0..=b'\x7F' => {
                self.col += 1;
                Some(*ascii_ch as utf8)
            }
            _utf8_ch => {
                let rest_of_line = &self.src.code[self.col as usize..self.line.end as usize];
                let Some(utf8_ch) = rest_of_line.chars().next() else {
                    unreachable!("this branch assured we would have a valid utf8 character");
                };

                self.col += utf8_ch.len_utf8() as offset;
                Some(utf8_ch)
            }
        };
    }

    fn peek_next_ascii_char(&self) -> Result<Option<&'src ascii>, Utf8Error> {
        let Some(next) = self.src.code.as_bytes().get(self.col as usize) else {
            return Ok(None);
        };

        return match next {
            ascii_ch @ 0..=b'\x7F' => Ok(Some(ascii_ch)),
            _utf8_ch => {
                let rest_of_line = &self.src.code[self.col as usize..self.line.end as usize];
                let Some(utf8_ch) = rest_of_line.chars().next() else {
                    unreachable!("this branch assured we would have a valid utf8 character");
                };

                let utf8_ch_col = self.col;
                Err(Utf8Error {
                    character: utf8_ch,
                    col: utf8_ch_col,
                    len: 1, // TODO(stefano): proper utf8 len
                })
            }
        };
    }

    fn peek_next_utf8_char(&self) -> Option<utf8> {
        let next = self.src.code.as_bytes().get(self.col as usize)?;
        return match next {
            ascii_ch @ 0..=b'\x7F' => Some(*ascii_ch as utf8),
            _utf8_ch => {
                let rest_of_line = &self.src.code[self.col as usize..self.line.end as usize];
                let Some(utf8_ch) = rest_of_line.chars().next() else {
                    unreachable!("this branch assured we would have a valid utf8 character");
                };

                Some(utf8_ch)
            }
        };
    }
}

impl<'src> Tokenizer<'src> {
    fn identifier(&mut self) -> Result<TokenKind<'src>, ()> {
        const MAX_IDENTIFIER_LEN: offset = 63;

        loop {
            match self.peek_next_ascii_char() {
                Ok(Some(b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_')) => {}
                Ok(Some(_) | None) => break,
                Err(error) => self.token_errors.push(Error {
                    kind: ErrorKind::Utf8InIdentifier(error.character),
                    col: error.col,
                    pointers_count: error.len,
                }),
            }

            _ = self.next_ascii_char();
        }

        let identifier = match &self.src.code[self.token_start_col as usize..self.col as usize] {
            "let" => TokenKind::Mutability(Mutability::Let),
            "var" => TokenKind::Mutability(Mutability::Var),
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
            identifier => {
                let identifier_len = self.token_len();
                if identifier_len as offset > MAX_IDENTIFIER_LEN {
                    self.token_errors.push(Error {
                        kind: ErrorKind::IdentifierTooLong { max: MAX_IDENTIFIER_LEN },
                        col: self.token_start_col,
                        pointers_count: identifier_len,
                    });
                }
                TokenKind::Identifier(identifier)
            }
        };

        return if self.token_errors.is_empty() { Ok(identifier) } else { Err(()) };
    }

    fn integer_decimal(&mut self) -> Result<&'src [ascii], ()> {
        loop {
            match self.peek_next_ascii_char() {
                Ok(Some(b'0'..=b'9' | b'_')) => {}
                Ok(Some(letter @ (b'a'..=b'z' | b'A'..=b'Z'))) => {
                    self.token_errors.push(Error {
                        kind: ErrorKind::LetterInNumberLiteral(Base::Decimal, *letter),
                        col: self.col,
                        pointers_count: 1,
                    });
                }
                Ok(Some(_) | None) => break,
                Err(error) => {
                    self.token_errors.push(Error {
                        kind: ErrorKind::Utf8InNumberLiteral(error.character),
                        col: error.col,
                        pointers_count: error.len,
                    });
                }
            }

            _ = self.next_ascii_char();
        }

        return if self.token_errors.is_empty() {
            let digits = &self.src.code[self.token_start_col as usize..self.col as usize];
            Ok(digits.as_bytes())
        } else {
            Err(())
        };
    }

    fn integer_binary(&mut self) -> Result<&'src [ascii], ()> {
        loop {
            match self.peek_next_ascii_char() {
                Ok(Some(b'0'..=b'1' | b'_')) => {}
                Ok(Some(out_of_range @ b'2'..=b'9')) => {
                    self.token_errors.push(Error {
                        kind: ErrorKind::DigitOutOfRangeInNumberLiteral(
                            Base::Binary,
                            *out_of_range,
                        ),
                        col: self.col,
                        pointers_count: 1,
                    });
                }
                Ok(Some(letter @ (b'a'..=b'z' | b'A'..=b'Z'))) => {
                    self.token_errors.push(Error {
                        kind: ErrorKind::LetterInNumberLiteral(Base::Binary, *letter),
                        col: self.col,
                        pointers_count: 1,
                    });
                }
                Ok(Some(_) | None) => break,
                Err(error) => {
                    self.token_errors.push(Error {
                        kind: ErrorKind::Utf8InNumberLiteral(error.character),
                        col: error.col,
                        pointers_count: error.len,
                    });
                }
            }

            _ = self.next_ascii_char();
        }

        return if self.token_errors.is_empty() {
            // starting at self.token_start_col + 2 to account for the 0b prefix
            let digits = &self.src.code[self.token_start_col as usize + 2..self.col as usize];
            if digits.is_empty() {
                self.token_errors.push(Error {
                    kind: ErrorKind::MissingDigits(MissingDigitsBase::Binary),
                    col: self.token_start_col,
                    pointers_count: self.token_len(),
                });
                Err(())
            } else {
                Ok(digits.as_bytes())
            }
        } else {
            Err(())
        };
    }

    fn integer_octal(&mut self) -> Result<&'src [ascii], ()> {
        loop {
            match self.peek_next_ascii_char() {
                Ok(Some(b'0'..=b'7' | b'_')) => {}
                Ok(Some(out_of_range @ b'8'..=b'9')) => {
                    self.token_errors.push(Error {
                        kind: ErrorKind::DigitOutOfRangeInNumberLiteral(Base::Octal, *out_of_range),
                        col: self.col,
                        pointers_count: 1,
                    });
                }
                Ok(Some(letter @ (b'a'..=b'z' | b'A'..=b'Z'))) => {
                    self.token_errors.push(Error {
                        kind: ErrorKind::LetterInNumberLiteral(Base::Octal, *letter),
                        col: self.col,
                        pointers_count: 1,
                    });
                }
                Ok(Some(_) | None) => break,
                Err(error) => {
                    self.token_errors.push(Error {
                        kind: ErrorKind::Utf8InNumberLiteral(error.character),
                        col: error.col,
                        pointers_count: error.len,
                    });
                }
            }

            _ = self.next_ascii_char();
        }

        return if self.token_errors.is_empty() {
            // starting at self.token_start_col + 2 to account for the 0o prefix
            let digits = &self.src.code[self.token_start_col as usize + 2..self.col as usize];
            if digits.is_empty() {
                self.token_errors.push(Error {
                    kind: ErrorKind::MissingDigits(MissingDigitsBase::Octal),
                    col: self.token_start_col,
                    pointers_count: self.token_len(),
                });
                Err(())
            } else {
                Ok(digits.as_bytes())
            }
        } else {
            Err(())
        };
    }

    fn integer_hexadecimal(&mut self) -> Result<&'src [ascii], ()> {
        loop {
            match self.peek_next_ascii_char() {
                Ok(Some(b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F' | b'_')) => {}
                Ok(Some(out_of_range @ (b'g'..=b'z' | b'G'..=b'Z'))) => {
                    self.token_errors.push(Error {
                        kind: ErrorKind::DigitOutOfRangeInNumberLiteral(
                            Base::Hexadecimal,
                            *out_of_range,
                        ),
                        col: self.col,
                        pointers_count: 1,
                    });
                }
                Ok(Some(_) | None) => break,
                Err(error) => {
                    self.token_errors.push(Error {
                        kind: ErrorKind::Utf8InNumberLiteral(error.character),
                        col: error.col,
                        pointers_count: error.len,
                    });
                }
            }

            _ = self.next_ascii_char();
        }

        return if self.token_errors.is_empty() {
            // starting at self.token_start_col + 2 to account for the 0x prefix
            let digits = &self.src.code[self.token_start_col as usize + 2..self.col as usize];
            if digits.is_empty() {
                self.token_errors.push(Error {
                    kind: ErrorKind::MissingDigits(MissingDigitsBase::Hexadecimal),
                    col: self.token_start_col,
                    pointers_count: self.token_len(),
                });
                Err(())
            } else {
                Ok(digits.as_bytes())
            }
        } else {
            Err(())
        };
    }

    fn next_token(&mut self, next: ascii) -> Result<TokenKind<'src>, ()> {
        return match next {
            b'r' => match self.peek_next_utf8_char() {
                Some('"') => {
                    self.col += 1;

                    let kind = QuotedLiteralKind::RawStr;
                    let quote = kind.quote();

                    loop {
                        let next_character = match self.next_ascii_char() {
                            Ok(Some(b'\n') | None) => {
                                self.token_errors.push(Error {
                                    kind: ErrorKind::UnclosedQuotedLiteral(kind),
                                    col: self.token_start_col,
                                    pointers_count: self.token_len(),
                                });
                                break;
                            }
                            Ok(Some(next_character)) => next_character,
                            Err(error) => {
                                self.token_errors.push(Error {
                                    kind: ErrorKind::Utf8InIdentifier(error.character),
                                    col: error.col,
                                    pointers_count: error.len,
                                });
                                continue;
                            }
                        };

                        let _character = match next_character {
                            control @ (b'\x00'..=b'\x1F' | b'\x7F') => {
                                self.token_errors.push(Error {
                                    kind: ErrorKind::ControlCharacterInQuotedLiteral(
                                        kind,
                                        control as utf8,
                                    ),
                                    col: self.col - 1,
                                    pointers_count: 1,
                                });
                                control
                            }
                            ch if ch == quote as u8 => break,
                            ch => ch,
                        };
                    }

                    if self.token_errors.is_empty() {
                        // starting at token_start_col + 2 to skip the r prefix, and ending at
                        // col - 1 to skip the closing quote
                        let raw_string = &self.src.code
                            [self.token_start_col as usize + 2..self.col as usize - 1];
                        Ok(TokenKind::RawStr(RawStr(raw_string.as_bytes())))
                    } else {
                        Err(())
                    }
                }
                _ => self.identifier(),
            },
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.identifier(),
            // IDEA(stefano): debate wether to allow trailing underscores or to emit a warning
            // IDEA(stefano): emit warning of inconsistent casing of letters, i.e. 0xFFff_fFfF_ffFF_ffFF
            b'0' => {
                let (base, integer) = match self.peek_next_ascii_char() {
                    Ok(Some(b'b')) => {
                        _ = self.next_ascii_char();
                        (Base::Binary, self.integer_binary())
                    }
                    Ok(Some(b'o')) => {
                        _ = self.next_ascii_char();
                        (Base::Octal, self.integer_octal())
                    }
                    Ok(Some(b'x')) => {
                        _ = self.next_ascii_char();
                        (Base::Hexadecimal, self.integer_hexadecimal())
                    }
                    Ok(Some(_) | None) | Err(_) => (Base::Decimal, self.integer_decimal()),
                };

                match integer {
                    Ok(literal) => Ok(TokenKind::Integer(base, Integer(literal))),
                    Err(()) => Err(()),
                }
            }
            b'1'..=b'9' => match self.integer_decimal() {
                Ok(literal) => Ok(TokenKind::Integer(Base::Decimal, Integer(literal))),
                Err(()) => Err(()),
            },
            b'"' => {
                let kind = QuotedLiteralKind::Str;
                let quote = kind.quote();

                let mut string = Vec::<ascii>::new();

                loop {
                    let next_character = match self.next_ascii_char() {
                        Ok(Some(b'\n') | None) => {
                            self.token_errors.push(Error {
                                kind: ErrorKind::UnclosedQuotedLiteral(kind),
                                col: self.token_start_col,
                                pointers_count: self.token_len(),
                            });
                            break;
                        }
                        Ok(Some(next_character)) => next_character,
                        Err(error) => {
                            self.token_errors.push(Error {
                                kind: ErrorKind::Utf8InQuotedLiteral(kind, error.character),
                                col: error.col,
                                pointers_count: error.len,
                            });
                            continue;
                        }
                    };

                    let character = match next_character {
                        b'\\' => {
                            let escape_character = match self.next_ascii_char() {
                                Ok(Some(b'\n') | None) => {
                                    self.token_errors.push(Error {
                                        kind: ErrorKind::UnclosedQuotedLiteral(kind),
                                        col: self.token_start_col,
                                        pointers_count: self.token_len(),
                                    });
                                    break;
                                }
                                Ok(Some(escape_character)) => escape_character,
                                Err(error) => {
                                    self.token_errors.push(Error {
                                        kind: ErrorKind::Utf8InQuotedLiteral(kind, error.character),
                                        col: error.col,
                                        pointers_count: error.len,
                                    });
                                    continue;
                                }
                            };

                            match escape_character {
                                b'\\' => b'\\',
                                b'\'' => b'\'',
                                b'"' => b'"',
                                b'n' => b'\n',
                                b'r' => b'\r',
                                b't' => b'\t',
                                b'0' => b'\0',
                                unrecognized => {
                                    self.token_errors.push(Error {
                                        kind: ErrorKind::UnrecognizedEscapeCharacterInQuotedLiteral(
                                            kind,
                                            unrecognized as utf8,
                                        ),
                                        col: self.col - 2,
                                        pointers_count: 2,
                                    });
                                    string.push(b'\\');
                                    unrecognized
                                }
                            }
                        }
                        control @ (b'\x00'..=b'\x1F' | b'\x7F') => {
                            self.token_errors.push(Error {
                                kind: ErrorKind::ControlCharacterInQuotedLiteral(
                                    kind,
                                    control as utf8,
                                ),
                                col: self.col - 1,
                                pointers_count: 1,
                            });
                            control
                        }
                        ch if ch == quote as u8 => break,
                        ch => ch,
                    };

                    string.push(character);
                }

                if self.token_errors.is_empty() {
                    Ok(TokenKind::Str(Str(string.into_boxed_slice())))
                } else {
                    Err(())
                }
            }
            b'\'' => {
                let kind = QuotedLiteralKind::Ascii;
                let quote = kind.quote();

                let mut characters = Vec::<ascii>::new();

                loop {
                    let next_character = match self.next_ascii_char() {
                        Ok(Some(b'\n') | None) => {
                            self.token_errors.push(Error {
                                kind: ErrorKind::UnclosedQuotedLiteral(kind),
                                col: self.token_start_col,
                                pointers_count: self.token_len(),
                            });
                            break;
                        }
                        Ok(Some(next_character)) => next_character,
                        Err(error) => {
                            self.token_errors.push(Error {
                                kind: ErrorKind::Utf8InQuotedLiteral(kind, error.character),
                                col: error.col,
                                pointers_count: error.len,
                            });
                            continue;
                        }
                    };

                    let character = match next_character {
                        b'\\' => {
                            let escape_character = match self.next_ascii_char() {
                                Ok(Some(b'\n') | None) => {
                                    self.token_errors.push(Error {
                                        kind: ErrorKind::UnclosedQuotedLiteral(kind),
                                        col: self.token_start_col,
                                        pointers_count: self.token_len(),
                                    });
                                    break;
                                }
                                Ok(Some(escape_character)) => escape_character,
                                Err(error) => {
                                    self.token_errors.push(Error {
                                        kind: ErrorKind::Utf8InQuotedLiteral(kind, error.character),
                                        col: error.col,
                                        pointers_count: error.len,
                                    });
                                    continue;
                                }
                            };

                            match escape_character {
                                b'\\' => b'\\',
                                b'\'' => b'\'',
                                b'"' => b'"',
                                b'n' => b'\n',
                                b'r' => b'\r',
                                b't' => b'\t',
                                b'0' => b'\0',
                                unrecognized => {
                                    self.token_errors.push(Error {
                                        kind: ErrorKind::UnrecognizedEscapeCharacterInQuotedLiteral(
                                            kind,
                                            unrecognized as utf8,
                                        ),
                                        col: self.col - 2,
                                        pointers_count: 2,
                                    });
                                    characters.push(b'\\');
                                    unrecognized
                                }
                            }
                        }
                        control @ (b'\x00'..=b'\x1F' | b'\x7F') => {
                            self.token_errors.push(Error {
                                kind: ErrorKind::ControlCharacterInQuotedLiteral(
                                    kind,
                                    control as utf8,
                                ),
                                col: self.col - 1,
                                pointers_count: 1,
                            });
                            control
                        }
                        ch if ch == quote as u8 => break,
                        ch => ch,
                    };

                    characters.push(character);
                }

                let length_related_error = match characters.as_slice() {
                    [] => Error {
                        kind: ErrorKind::EmptyCharacterLiteral,
                        col: self.token_start_col,
                        pointers_count: 2,
                    },
                    [character] => {
                        return if self.token_errors.is_empty() {
                            Ok(TokenKind::Ascii(*character))
                        } else {
                            Err(())
                        };
                    }
                    [_, ..] => Error {
                        kind: ErrorKind::MultipleCharactersInCharacterLiteral,
                        col: self.token_start_col,
                        pointers_count: self.token_len(),
                    },
                };

                self.token_errors.push(length_related_error);
                Err(())
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
                        self.token_errors.push(Error {
                            kind: ErrorKind::MismatchedBracket {
                                expected: BracketKind::CloseRound,
                                actual,
                            },
                            col: self.token_start_col,
                            pointers_count: 1,
                        });
                        Err(())
                    }
                },
                None => {
                    self.token_errors.push(Error {
                        kind: ErrorKind::UnopenedBracket(BracketKind::CloseRound),
                        col: self.token_start_col,
                        pointers_count: 1,
                    });
                    Err(())
                }
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
                        self.token_errors.push(Error {
                            kind: ErrorKind::MismatchedBracket {
                                expected: BracketKind::CloseSquare,
                                actual,
                            },
                            col: self.token_start_col,
                            pointers_count: 1,
                        });
                        Err(())
                    }
                },
                None => {
                    self.token_errors.push(Error {
                        kind: ErrorKind::UnopenedBracket(BracketKind::CloseSquare),
                        col: self.token_start_col,
                        pointers_count: 1,
                    });
                    Err(())
                }
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
                        self.token_errors.push(Error {
                            kind: ErrorKind::MismatchedBracket {
                                expected: BracketKind::CloseCurly,
                                actual,
                            },
                            col: self.token_start_col,
                            pointers_count: 1,
                        });
                        Err(())
                    }
                },
                None => {
                    self.token_errors.push(Error {
                        kind: ErrorKind::UnopenedBracket(BracketKind::CloseCurly),
                        col: self.token_start_col,
                        pointers_count: 1,
                    });
                    Err(())
                }
            },
            b'#' => {
                // ignoring the hash symbol
                let comment = &self.src.code[self.col as usize..self.line.end as usize];

                // consuming the rest of the characters in the current line
                loop {
                    if let Some('\n') | None = self.peek_next_utf8_char() {
                        break;
                    }
                    _ = self.next_utf8_char();
                }

                Ok(TokenKind::Comment(comment))
            }
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
            unrecognized => {
                self.token_errors.push(Error {
                    kind: ErrorKind::UnrecognizedCharacter(unrecognized as utf8),
                    col: self.token_start_col,
                    pointers_count: 1,
                });
                Err(())
            }
        };
    }
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    UnclosedBracket(BracketKind),
    UnopenedBracket(BracketKind),
    MismatchedBracket { actual: BracketKind, expected: BracketKind },

    Utf8InQuotedLiteral(QuotedLiteralKind, utf8),
    UnclosedQuotedLiteral(QuotedLiteralKind),
    ControlCharacterInQuotedLiteral(QuotedLiteralKind, utf8),
    UnrecognizedEscapeCharacterInQuotedLiteral(QuotedLiteralKind, utf8),
    EmptyCharacterLiteral,
    MultipleCharactersInCharacterLiteral,

    Utf8InNumberLiteral(utf8),
    LetterInNumberLiteral(Base, ascii),
    // IDEA(stefano): display information about the valid digit range
    DigitOutOfRangeInNumberLiteral(Base, ascii),
    MissingDigits(MissingDigitsBase),

    Utf8InIdentifier(utf8),
    IdentifierTooLong { max: offset },

    Utf8Character(utf8),
    UnrecognizedCharacter(utf8),
}

impl IntoErrorInfo for ErrorKind {
    fn info(&self) -> ErrorInfo {
        #[rustfmt::skip]
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

            Self::Utf8InQuotedLiteral(kind, character) => (
                format!("invalid {kind} literal, contains character '{character}' {}", character.escape_unicode()).into(),
                "utf8 characters are not allowed".into(),
            ),
            Self::UnclosedQuotedLiteral(kind) => (
                format!("unclosed {kind} literal").into(),
                format!("missing closing {} quote", kind.quote() as u8 as char).into()
            ),
            Self::ControlCharacterInQuotedLiteral(kind, control_character) => (
                format!("invalid {kind} literal, contains character '{}' {}", control_character.escape_debug(), control_character.escape_unicode()).into(),
                "control characters are not allowed".into(),
            ),
            Self::UnrecognizedEscapeCharacterInQuotedLiteral(kind, unrecognized) => (
                format!("invalid {kind} literal").into(),
                format!("unrecognized '{unrecognized}' escape character").into(),
            ),
            Self::EmptyCharacterLiteral => (
                format!("empty {} literal", QuotedLiteralKind::Ascii).into(),
                "must not be empty".into(),
            ),
            Self::MultipleCharactersInCharacterLiteral => (
                format!("invalid {} literal", QuotedLiteralKind::Ascii).into(),
                format!("must not contain more than one character, if you meant to write a string literal try changing the quotes to {}", Quote::Double as u8 as char).into(),
            ),

            Self::Utf8InNumberLiteral(character) => (
                format!("invalid integer literal, contains character '{character}' {}", character.escape_unicode()).into(),
                "utf8 characters are not allowed".into(),
            ),
            Self::LetterInNumberLiteral(base, letter) => (
                "invalid integer literal".into(),
                format!("letter {} is not allowed in a base {} number", *letter as char, *base as u8).into(),
            ),
            Self::DigitOutOfRangeInNumberLiteral(base, digit) => (
                "invalid integer literal digit".into(),
                format!("digit {} is out of the valid range for a base {} number", *digit as char, *base as u8).into(),
            ),
            Self::MissingDigits(base) => (
                "invalid integer literal".into(),
                format!("at leasts one base {} digt must be present", *base as u8).into(),
            ),

            Self::Utf8InIdentifier(character) => (
                format!("invalid identifier, contains character '{character}' {}", character.escape_unicode()).into(),
                "utf8 characters are not allowed".into(),
            ),
            Self::IdentifierTooLong { max } => (
                "invalid identifier".into(),
                format!("exceeds the length limit of {max}").into(),
            ),

            Self::Utf8Character(character) => (
                format!("invalid character '{character}' {}", character.escape_unicode()).into(),
                "utf8 characters are not allowed".into()
            ),
            Self::UnrecognizedCharacter(unrecognized) => (
                format!("invalid character '{unrecognized}' {}", unrecognized.escape_unicode()).into(),
                "unrecognized".into(),
            ),
        };

        return ErrorInfo { error_message, error_cause_message };
    }
}
