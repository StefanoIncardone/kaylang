// TODO(stefano): more escape characters

use super::{
    src_file::{index32, offset32, Line, SrcCode, SrcFile},
    Error, ErrorInfo, IntoErrorInfo,
};
use crate::error::CharsWidth as _;
use core::{fmt::Display, marker::PhantomData};
use unicode_segmentation::UnicodeSegmentation as _;

#[expect(
    non_camel_case_types,
    reason = "behaves like a primitive type, so it should be named like a primitive type"
)]
/// kay's equivalent to pointer sized signed integer
pub(crate) type int = isize;

#[expect(
    non_camel_case_types,
    reason = "behaves like a primitive type, so it should be named like a primitive type"
)]
/// kay's equivalent to pointer sized unsigned integer
pub(crate) type uint = usize;

#[expect(
    non_camel_case_types,
    reason = "behaves like a primitive type, so it should be named like a primitive type"
)]
/// kay's ascii character type
pub(crate) type ascii = u8;

#[expect(
    non_camel_case_types,
    reason = "behaves like a primitive type, so it should be named like a primitive type"
)]
/// kay's utf8 character type
pub(crate) type utf8 = char;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
#[repr(u8)]
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

    #[must_use]
    pub const fn range(self) -> &'static [core::ops::RangeInclusive<utf8>] {
        return match self {
            Self::Decimal => &['0'..='9'],
            Self::Binary => &['0'..='1'],
            Self::Octal => &['0'..='7'],
            Self::Hexadecimal => &['0'..='9', 'A'..='F', 'a'..='f'],
        };
    }
}

// REMOVE(stefano): too much abstraction
#[derive(Debug, Clone)]
#[repr(transparent)]
pub(crate) struct Str(pub(crate) Box<[ascii]>);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
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

impl Mutability {
    #[expect(dead_code, reason = "kept for consistency")]
    #[inline(always)]
    pub(super) const fn display_len(self) -> offset32 {
        return match self {
            Self::Let | Self::Var => 3,
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Bracket {
    OpenRound,
    CloseRound,
    OpenSquare,
    CloseSquare,
    OpenCurly,
    CloseCurly,
}

impl Display for Bracket {
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

impl Bracket {
    #[expect(clippy::unused_self, reason = "kept for consistency")]
    #[inline(always)]
    pub(super) const fn display_len(self) -> offset32 {
        return 1;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum OpenBracket {
    Round = Bracket::OpenRound as u8,
    Square = Bracket::OpenSquare as u8,
    Curly = Bracket::OpenCurly as u8,
}

impl Into<OpenBracket> for Bracket {
    #[inline(always)]
    fn into(self) -> OpenBracket {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Bracket> for OpenBracket {
    #[inline(always)]
    fn into(self) -> Bracket {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Display for OpenBracket {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let bracket: Bracket = (*self).into();
        return write!(f, "{bracket}");
    }
}

impl OpenBracket {
    #[expect(dead_code, reason = "kept for consistency")]
    #[inline(always)]
    pub(super) fn display_len(self) -> offset32 {
        let bracket: Bracket = self.into();
        return bracket.display_len();
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum CloseBracket {
    Round = Bracket::CloseRound as u8,
    Square = Bracket::CloseSquare as u8,
    Curly = Bracket::CloseCurly as u8,
}

impl Into<CloseBracket> for Bracket {
    #[inline(always)]
    fn into(self) -> CloseBracket {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Bracket> for CloseBracket {
    #[inline(always)]
    fn into(self) -> Bracket {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Display for CloseBracket {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let bracket: Bracket = (*self).into();
        return write!(f, "{bracket}");
    }
}

impl CloseBracket {
    #[expect(dead_code, reason = "kept for consistency")]
    #[inline(always)]
    pub(super) fn display_len(self) -> offset32 {
        let bracket: Bracket = self.into();
        return bracket.display_len();
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Op {
    Equals,

    /// temporary way of getting the length of strings and arrays
    Len,
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

    /// also unary safe absolute value
    Plus,
    /// also unary wrapping absolute value
    WrappingPlus,
    /// also unary saturating absolute value
    SaturatingPlus,
    PlusEquals,
    WrappingPlusEquals,
    SaturatingPlusEquals,

    /// also unary integer negation
    Minus,
    /// also unary wrapping integer negation
    WrappingMinus,
    /// also unary saturating integer negation
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
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        #[rustfmt::skip]
        return match self {
            Self::Len                       => write!(f, "len"),
            Self::Equals                    => write!(f, "="),
            Self::Not                       => write!(f, "!"),

            Self::Pow                       => write!(f,  "**"),
            Self::WrappingPow               => write!(f, r"**\"),
            Self::SaturatingPow             => write!(f,  "**|"),
            Self::PowEquals                 => write!(f,  "**="),
            Self::WrappingPowEquals         => write!(f, r"**\="),
            Self::SaturatingPowEquals       => write!(f,  "**|="),

            Self::Times                     => write!(f,  "*"),
            Self::WrappingTimes             => write!(f, r"*\"),
            Self::SaturatingTimes           => write!(f,  "*|"),
            Self::TimesEquals               => write!(f,  "*="),
            Self::WrappingTimesEquals       => write!(f, r"*\="),
            Self::SaturatingTimesEquals     => write!(f,  "*|="),

            Self::Divide                    => write!(f,  "/"),
            Self::WrappingDivide            => write!(f, r"/\"),
            Self::SaturatingDivide          => write!(f,  "/|"),
            Self::DivideEquals              => write!(f,  "/="),
            Self::WrappingDivideEquals      => write!(f, r"/\="),
            Self::SaturatingDivideEquals    => write!(f,  "/|="),

            Self::Remainder                 => write!(f, "%"),
            Self::RemainderEquals           => write!(f, "%="),

            Self::Plus                      => write!(f,  "+"),
            Self::WrappingPlus              => write!(f, r"+\"),
            Self::SaturatingPlus            => write!(f,  "+|"),
            Self::PlusEquals                => write!(f,  "+="),
            Self::WrappingPlusEquals        => write!(f, r"+\="),
            Self::SaturatingPlusEquals      => write!(f,  "+|="),

            Self::Minus                     => write!(f,  "-"),
            Self::WrappingMinus             => write!(f, r"-\"),
            Self::SaturatingMinus           => write!(f,  "-|"),
            Self::MinusEquals               => write!(f,  "-="),
            Self::WrappingMinusEquals       => write!(f, r"-\="),
            Self::SaturatingMinusEquals     => write!(f,  "-|="),

            Self::LeftShift                 => write!(f,  "<<"),
            Self::WrappingLeftShift         => write!(f, r"<<\"),
            Self::SaturatingLeftShift       => write!(f,  "<<|"),
            Self::LeftShiftEquals           => write!(f,  "<<="),
            Self::WrappingLeftShiftEquals   => write!(f, r"<<\="),
            Self::SaturatingLeftShiftEquals => write!(f,  "<<|="),

            Self::RightShift                => write!(f,  ">>"),
            Self::RightShiftEquals          => write!(f,  ">>="),

            Self::LeftRotate                => write!(f, "<<<"),
            Self::LeftRotateEquals          => write!(f, "<<<="),
            Self::RightRotate               => write!(f, ">>>"),
            Self::RightRotateEquals         => write!(f, ">>>="),

            Self::BitAnd                    => write!(f, "&"),
            Self::BitAndEquals              => write!(f, "&="),

            Self::BitOr                     => write!(f, "|"),
            Self::BitOrEquals               => write!(f, "|="),

            Self::BitXor                    => write!(f, "^"),
            Self::BitXorEquals              => write!(f, "^="),

            Self::And                       => write!(f, "&&"),
            Self::AndEquals                 => write!(f, "&&="),

            Self::Or                        => write!(f, "||"),
            Self::OrEquals                  => write!(f, "||="),

            Self::Compare                   => write!(f, "<=>"),
            Self::EqualsEquals              => write!(f, "=="),
            Self::NotEquals                 => write!(f, "!="),
            Self::Greater                   => write!(f, ">"),
            Self::GreaterOrEquals           => write!(f, ">="),
            Self::Less                      => write!(f, "<"),
            Self::LessOrEquals              => write!(f, "<="),
        };
    }
}

impl Op {
    pub(super) const fn display_len(self) -> offset32 {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Quote {
    Single = b'\'',
    Double = b'"',
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
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

pub(crate) type BracketIndex = index32;
pub(crate) type TextIndex = index32;
pub(crate) type StrIndex = index32;
pub(crate) type TokenIndex = index32;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TokenKind {
    // IDEA(stefano): remove from the returned tokens, to avoid encountering them during the parsing stage
    Comment(TextIndex),
    BlockComment(TextIndex),
    Unexpected(TextIndex),

    // Symbols
    Bracket(Bracket),
    Colon,
    SemiColon,
    Comma,
    Op(Op),

    // Literal values
    False,
    True,
    // IDEA(stefano): expand into the different bases, akin to Str and RawStr
    /// integer literals are never empty and always contain valid ascii digits
    Integer(Base, TextIndex),
    Ascii(ascii),
    Str(StrIndex),
    RawStr(StrIndex),

    Identifier(TextIndex),

    // Keywords
    /// temporary way of printing values to stdout
    Print,
    /// temporary way of printing values followed by a newline to stdout
    PrintLn,
    /// temporary way of printing values to stderr
    Eprint,
    /// temporary way of printing values followed by a newline to stderr
    EprintLn,

    Let,
    Var,
    Do,
    If,
    Else,
    Loop,
    Break,
    Continue,
}

impl TokenKind {
    pub(super) fn display_len(self, tokens: &Tokens<'_, '_>) -> offset32 {
        return match self {
            Self::Comment(comment) => {
                let text = tokens.text[comment as usize];
                text.chars_width() + 1 // + 1 to account for the `#`
            }
            Self::BlockComment(comment) => {
                let text = tokens.text[comment as usize];
                text.chars_width() + 4 // + 4 to account for `##` and `##`
            }
            Self::Unexpected(unexpected) => {
                let text = tokens.text[unexpected as usize];
                text.chars_width()
            }

            Self::Bracket(bracket) => bracket.display_len(),
            Self::Colon => 1,
            Self::SemiColon => 1,
            Self::Comma => 1,
            Self::Op(op) => op.display_len(),

            Self::Integer(base, integer) => {
                let literal = tokens.text[integer as usize];
                base.prefix().len() as offset32 + literal.len() as offset32
            }
            Self::Ascii(ascii_char) => ascii_char.escape_ascii().len() as offset32 + 2, // + 2 to account for the quotes
            Self::True => 4,
            Self::False => 5,
            Self::Str(string) => {
                let text = &tokens.strings[string as usize];
                let mut len = 2; // starting at 2 to account for the quotes
                for ascii_char in &*text.0 {
                    len += ascii_char.escape_ascii().len() as offset32;
                }
                len
            }
            Self::RawStr(string) => {
                let text = &tokens.strings[string as usize];
                text.0.len() as offset32 + 3 // + 1 for the `r` prefix, and + 2 for the quotes
            }

            Self::Identifier(identifier) => {
                let text = tokens.text[identifier as usize];
                text.len() as offset32
            }

            Self::Print => 5,
            Self::PrintLn => 7,
            Self::Eprint => 6,
            Self::EprintLn => 8,

            Self::Let => 3,
            Self::Var => 3,
            Self::Do => 2,
            Self::If => 2,
            Self::Else => 4,
            Self::Loop => 4,
            Self::Break => 5,
            Self::Continue => 8,
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    pub(crate) kind: TokenKind,
    pub(crate) col: offset32,
}

#[derive(Debug, Clone)]
pub struct Tokens<'code, 'path: 'code> {
    pub(crate) tokens: Vec<Token>,

    // IDEA(stefano): store a Range<offset32> instead
    pub(crate) text: Vec<&'code str>,
    pub(crate) strings: Vec<Str>,

    _src: PhantomData<&'code SrcFile<'path>>,
}

#[must_use = "this is similar to a `Result`, which should be handled"]
pub struct TokenizedCode<'code, 'path: 'code> {
    pub result: Result<Tokens<'code, 'path>, Vec<Error<ErrorKind<'code>>>>,
    pub src: SrcCode<'code, 'path>,
}

#[derive(Debug)]
pub struct Tokenizer<'code, 'path: 'code> {
    code: &'code str,
    lines: Vec<Line>,
    line_start: offset32,

    col: offset32,
    token_start_col: offset32,
    tokens: Tokens<'code, 'path>,

    errors: Vec<Error<ErrorKind<'code>>>,
}

impl<'code, 'path: 'code> Tokenizer<'code, 'path> {
    // IDEA(stefano): move into freestanding function
    pub fn tokenize(src_file: &'code SrcFile<'path>) -> TokenizedCode<'code, 'path> {
        let mut tokenizer = Tokenizer {
            code: &src_file.code,
            lines: Vec::new(),
            line_start: 0,

            col: 0,
            token_start_col: 0,
            tokens: Tokens {
                tokens: Vec::new(),
                text: Vec::new(),
                strings: Vec::new(),
                _src: PhantomData,
            },

            errors: Vec::new(),
        };
        let mut brackets_indicies = Vec::<BracketIndex>::new();

        'tokenization: while let Some(next_character) = tokenizer.peek_next_ascii_char() {
            let token_kind_result = 'next_token: {
                let next = match next_character {
                    Ok(next) => match next {
                        // ignore whitespace
                        b' ' | b'\t' | b'\x0C' => {
                            tokenizer.col += 1;
                            continue 'tokenization;
                        }

                        // we reached the end of the line on a LF (\n)
                        b'\n' => {
                            let line = Line { start: tokenizer.line_start, end: tokenizer.col };
                            tokenizer.lines.push(line);
                            tokenizer.col += 1;
                            tokenizer.line_start = tokenizer.col;
                            continue 'tokenization;
                        }
                        b'\r' => {
                            if tokenizer.col as usize + 1 >= tokenizer.code.len() {
                                tokenizer.col += 1;
                                continue 'tokenization;
                            }

                            if let b'\n' = tokenizer.code.as_bytes()[tokenizer.col as usize + 1] {
                                let line = Line { start: tokenizer.line_start, end: tokenizer.col };
                                tokenizer.lines.push(line);
                                tokenizer.col += 2;
                                tokenizer.line_start = tokenizer.col;
                                continue 'tokenization;
                            };

                            tokenizer.col += 1;
                            continue 'tokenization;
                        }
                        other => {
                            tokenizer.token_start_col = tokenizer.col;
                            tokenizer.col += 1;
                            other
                        }
                    },
                    Err(error) => {
                        tokenizer.col += error.grapheme.len() as offset32;
                        tokenizer.errors.push(Error {
                            kind: ErrorKind::Utf8Character { grapheme: error.grapheme },
                            col: error.col,
                            pointers_count: error.pointers_count,
                        });
                        break 'next_token Err(());
                    }
                };

                match next {
                    b'r' => match tokenizer.peek_next_utf8_char() {
                        Some('"') => {
                            tokenizer.col += 1; // skip the `r` prefix

                            match tokenizer.raw_string_literal() {
                                Ok(literal) => {
                                    tokenizer.tokens.strings.push(Str(literal.into_boxed_slice()));
                                    let string_index =
                                        tokenizer.tokens.strings.len() as StrIndex - 1;
                                    Ok(TokenKind::RawStr(string_index))
                                }
                                Err(()) => Err(()),
                            }
                        }
                        _ => tokenizer.identifier(),
                    },
                    b'a'..=b'z' | b'A'..=b'Z' | b'_' => tokenizer.identifier(),
                    // IDEA(stefano): debate wether to allow trailing underscores or to emit a warning
                    // IDEA(stefano): emit warning of inconsistent casing of letters, i.e. 0xFFff_fFfF_ffFF_ffFF
                    b'0' => {
                        let (base, integer) = match tokenizer.peek_next_utf8_char() {
                            Some('b') => {
                                tokenizer.col += 1;
                                (Base::Binary, tokenizer.integer_binary())
                            }
                            Some('o') => {
                                tokenizer.col += 1;
                                (Base::Octal, tokenizer.integer_octal())
                            }
                            Some('x') => {
                                tokenizer.col += 1;
                                (Base::Hexadecimal, tokenizer.integer_hexadecimal())
                            }
                            Some(_) | None => (Base::Decimal, tokenizer.integer_decimal()),
                        };

                        match integer {
                            Ok(literal) => {
                                let literal_str =
                                    unsafe { core::str::from_utf8_unchecked(literal) };
                                tokenizer.tokens.text.push(literal_str);
                                let literal_index = tokenizer.tokens.text.len() as TextIndex - 1;
                                Ok(TokenKind::Integer(base, literal_index))
                            }
                            Err(()) => Err(()),
                        }
                    }
                    b'1'..=b'9' => match tokenizer.integer_decimal() {
                        Ok(literal) => {
                            let literal_str = unsafe { core::str::from_utf8_unchecked(literal) };
                            tokenizer.tokens.text.push(literal_str);
                            let literal_index = tokenizer.tokens.text.len() as TextIndex - 1;
                            Ok(TokenKind::Integer(Base::Decimal, literal_index))
                        }
                        Err(()) => Err(()),
                    },
                    b'"' => match tokenizer.quoted_literal(QuotedLiteralKind::Str) {
                        Ok(literal) => {
                            tokenizer.tokens.strings.push(Str(literal.into_boxed_slice()));
                            let string_index = tokenizer.tokens.strings.len() as StrIndex - 1;
                            Ok(TokenKind::Str(string_index))
                        }
                        Err(()) => Err(()),
                    },
                    b'\'' => match tokenizer.quoted_literal(QuotedLiteralKind::Ascii) {
                        Ok(literal) => match literal.as_slice() {
                            [character] => Ok(TokenKind::Ascii(*character)),
                            [] => {
                                tokenizer.errors.push(Error {
                                    kind: ErrorKind::EmptyCharacterLiteral,
                                    col: tokenizer.token_start_col,
                                    pointers_count: 2,
                                });
                                Err(())
                            }
                            [_, ..] => {
                                tokenizer.errors.push(Error {
                                    kind: ErrorKind::MultipleCharactersInCharacterLiteral,
                                    col: tokenizer.token_start_col,
                                    pointers_count: tokenizer.token_text().chars_width(),
                                });
                                Err(())
                            }
                        },
                        Err(()) => Err(()),
                    },
                    b'#' => match tokenizer.peek_next_utf8_char() {
                        Some('#') => 'comment: {
                            'next_character: loop {
                                match tokenizer.next_utf8_char_multiline() {
                                    Some('#') => match tokenizer.next_utf8_char_multiline() {
                                        Some('#') => break 'next_character,
                                        Some(_) => {}
                                        None => {
                                            tokenizer.errors.push(Error {
                                                kind: ErrorKind::UnclosedBlockComment,
                                                col: tokenizer.token_start_col,
                                                pointers_count: 2,
                                            });
                                            break 'comment Err(());
                                        }
                                    },
                                    Some(_) => {}
                                    None => {
                                        tokenizer.errors.push(Error {
                                            kind: ErrorKind::UnclosedBlockComment,
                                            col: tokenizer.token_start_col,
                                            pointers_count: 2,
                                        });
                                        break 'comment Err(());
                                    }
                                }
                            }

                            // starting at this.token_start_col + 2 to skip the `##`
                            // ending at this.col - 2 to skip the `##`
                            let comment_str = &tokenizer.code[tokenizer.token_start_col as usize + 2
                                ..tokenizer.col as usize + 2];
                            tokenizer.tokens.text.push(comment_str);
                            let comment_index = tokenizer.tokens.text.len() as TextIndex - 1;
                            Ok(TokenKind::BlockComment(comment_index))
                        }
                        Some(_) | None => {
                            loop {
                                match tokenizer.peek_next_utf8_char() {
                                    Some('\n') | None => break,
                                    Some('\r') => {
                                        if tokenizer.col as usize + 1 >= tokenizer.code.len() {
                                            tokenizer.col += 1;
                                            continue;
                                        }

                                        if let b'\n' =
                                            tokenizer.code.as_bytes()[tokenizer.col as usize + 1]
                                        {
                                            break;
                                        };

                                        tokenizer.col += 1;
                                    }
                                    Some(other) => tokenizer.col += other.len_utf8() as offset32,
                                }
                            }

                            let comment_str = &tokenizer.code
                                [tokenizer.token_start_col as usize + 1..tokenizer.col as usize];
                            tokenizer.tokens.text.push(comment_str);
                            let comment_index = tokenizer.tokens.text.len() as TextIndex - 1;
                            Ok(TokenKind::Comment(comment_index))
                        }
                    },
                    b'(' => {
                        brackets_indicies.push(tokenizer.tokens.tokens.len() as TokenIndex);
                        Ok(TokenKind::Bracket(Bracket::OpenRound))
                    }
                    b')' => 'bracket: {
                        let Some(bracket_index) = brackets_indicies.pop() else {
                            tokenizer.errors.push(Error {
                                kind: ErrorKind::UnopenedBracket(CloseBracket::Round),
                                col: tokenizer.token_start_col,
                                pointers_count: 1,
                            });
                            break 'bracket Err(());
                        };

                        let TokenKind::Bracket(bracket) =
                            &tokenizer.tokens.tokens[bracket_index as usize].kind
                        else {
                            unreachable!("incorrect bracket index");
                        };

                        match *bracket {
                            Bracket::OpenRound
                            | Bracket::CloseRound
                            | Bracket::CloseCurly
                            | Bracket::CloseSquare => Ok(TokenKind::Bracket(Bracket::CloseRound)),
                            actual @ (Bracket::OpenCurly | Bracket::OpenSquare) => {
                                tokenizer.errors.push(Error {
                                    kind: ErrorKind::MismatchedBracket {
                                        expected: CloseBracket::Round,
                                        actual: actual.into(),
                                    },
                                    col: tokenizer.token_start_col,
                                    pointers_count: 1,
                                });
                                Err(())
                            }
                        }
                    }
                    b'[' => {
                        brackets_indicies.push(tokenizer.tokens.tokens.len() as TokenIndex);
                        Ok(TokenKind::Bracket(Bracket::OpenSquare))
                    }
                    b']' => 'bracket: {
                        let Some(bracket_index) = brackets_indicies.pop() else {
                            tokenizer.errors.push(Error {
                                kind: ErrorKind::UnopenedBracket(CloseBracket::Square),
                                col: tokenizer.token_start_col,
                                pointers_count: 1,
                            });
                            break 'bracket Err(());
                        };

                        let TokenKind::Bracket(bracket) =
                            &tokenizer.tokens.tokens[bracket_index as usize].kind
                        else {
                            unreachable!("incorrect bracket index");
                        };

                        match *bracket {
                            Bracket::OpenSquare
                            | Bracket::CloseSquare
                            | Bracket::CloseCurly
                            | Bracket::CloseRound => Ok(TokenKind::Bracket(Bracket::CloseSquare)),
                            actual @ (Bracket::OpenCurly | Bracket::OpenRound) => {
                                tokenizer.errors.push(Error {
                                    kind: ErrorKind::MismatchedBracket {
                                        expected: CloseBracket::Square,
                                        actual: actual.into(),
                                    },
                                    col: tokenizer.token_start_col,
                                    pointers_count: 1,
                                });
                                Err(())
                            }
                        }
                    }
                    b'{' => {
                        brackets_indicies.push(tokenizer.tokens.tokens.len() as TokenIndex);
                        Ok(TokenKind::Bracket(Bracket::OpenCurly))
                    }
                    b'}' => 'bracket: {
                        let Some(bracket_index) = brackets_indicies.pop() else {
                            tokenizer.errors.push(Error {
                                kind: ErrorKind::UnopenedBracket(CloseBracket::Curly),
                                col: tokenizer.token_start_col,
                                pointers_count: 1,
                            });
                            break 'bracket Err(());
                        };

                        let TokenKind::Bracket(bracket) =
                            &tokenizer.tokens.tokens[bracket_index as usize].kind
                        else {
                            unreachable!("incorrect bracket index");
                        };

                        match *bracket {
                            Bracket::OpenCurly
                            | Bracket::CloseCurly
                            | Bracket::CloseRound
                            | Bracket::CloseSquare => Ok(TokenKind::Bracket(Bracket::CloseCurly)),
                            actual @ (Bracket::OpenRound | Bracket::OpenSquare) => {
                                tokenizer.errors.push(Error {
                                    kind: ErrorKind::MismatchedBracket {
                                        expected: CloseBracket::Curly,
                                        actual: actual.into(),
                                    },
                                    col: tokenizer.token_start_col,
                                    pointers_count: 1,
                                });
                                Err(())
                            }
                        }
                    }
                    b':' => Ok(TokenKind::Colon),
                    b';' => Ok(TokenKind::SemiColon),
                    b',' => Ok(TokenKind::Comma),
                    b'!' => match tokenizer.peek_next_utf8_char() {
                        Some('=') => {
                            tokenizer.col += 1;
                            Ok(TokenKind::Op(Op::NotEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::Not)),
                    },
                    b'*' => match tokenizer.peek_next_utf8_char() {
                        Some('*') => {
                            tokenizer.col += 1;
                            match tokenizer.peek_next_utf8_char() {
                                Some('=') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::PowEquals))
                                }
                                Some('\\') => {
                                    tokenizer.col += 1;
                                    match tokenizer.peek_next_utf8_char() {
                                        Some('=') => {
                                            tokenizer.col += 1;
                                            Ok(TokenKind::Op(Op::WrappingPowEquals))
                                        }
                                        _ => Ok(TokenKind::Op(Op::WrappingPow)),
                                    }
                                }
                                Some('|') => {
                                    tokenizer.col += 1;
                                    match tokenizer.peek_next_utf8_char() {
                                        Some('=') => {
                                            tokenizer.col += 1;
                                            Ok(TokenKind::Op(Op::SaturatingPowEquals))
                                        }
                                        _ => Ok(TokenKind::Op(Op::SaturatingPow)),
                                    }
                                }
                                _ => Ok(TokenKind::Op(Op::Pow)),
                            }
                        }
                        Some('=') => {
                            tokenizer.col += 1;
                            Ok(TokenKind::Op(Op::TimesEquals))
                        }
                        Some('\\') => {
                            tokenizer.col += 1;
                            match tokenizer.peek_next_utf8_char() {
                                Some('=') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::WrappingTimesEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::WrappingTimes)),
                            }
                        }
                        Some('|') => {
                            tokenizer.col += 1;
                            match tokenizer.peek_next_utf8_char() {
                                Some('=') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::SaturatingTimesEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::SaturatingTimes)),
                            }
                        }
                        _ => Ok(TokenKind::Op(Op::Times)),
                    },
                    b'/' => match tokenizer.peek_next_utf8_char() {
                        Some('=') => {
                            tokenizer.col += 1;
                            Ok(TokenKind::Op(Op::DivideEquals))
                        }
                        Some('\\') => {
                            tokenizer.col += 1;
                            match tokenizer.peek_next_utf8_char() {
                                Some('=') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::WrappingDivideEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::WrappingDivide)),
                            }
                        }
                        Some('|') => {
                            tokenizer.col += 1;
                            match tokenizer.peek_next_utf8_char() {
                                Some('=') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::SaturatingDivideEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::SaturatingDivide)),
                            }
                        }
                        _ => Ok(TokenKind::Op(Op::Divide)),
                    },
                    b'%' => match tokenizer.peek_next_utf8_char() {
                        Some('=') => {
                            tokenizer.col += 1;
                            Ok(TokenKind::Op(Op::RemainderEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::Remainder)),
                    },
                    b'+' => match tokenizer.peek_next_utf8_char() {
                        Some('=') => {
                            tokenizer.col += 1;
                            Ok(TokenKind::Op(Op::PlusEquals))
                        }
                        Some('\\') => {
                            tokenizer.col += 1;
                            match tokenizer.peek_next_utf8_char() {
                                Some('=') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::WrappingPlusEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::WrappingPlus)),
                            }
                        }
                        Some('|') => {
                            tokenizer.col += 1;
                            match tokenizer.peek_next_utf8_char() {
                                Some('=') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::SaturatingPlusEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::SaturatingPlus)),
                            }
                        }
                        _ => Ok(TokenKind::Op(Op::Plus)),
                    },
                    b'-' => match tokenizer.peek_next_utf8_char() {
                        Some('=') => {
                            tokenizer.col += 1;
                            Ok(TokenKind::Op(Op::MinusEquals))
                        }
                        Some('\\') => {
                            tokenizer.col += 1;
                            match tokenizer.peek_next_utf8_char() {
                                Some('=') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::WrappingMinusEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::WrappingMinus)),
                            }
                        }
                        Some('|') => {
                            tokenizer.col += 1;
                            match tokenizer.peek_next_utf8_char() {
                                Some('=') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::SaturatingMinusEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::SaturatingMinus)),
                            }
                        }
                        _ => Ok(TokenKind::Op(Op::Minus)),
                    },
                    b'&' => match tokenizer.peek_next_utf8_char() {
                        Some('&') => {
                            tokenizer.col += 1;
                            match tokenizer.peek_next_utf8_char() {
                                Some('=') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::AndEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::And)),
                            }
                        }
                        Some('=') => {
                            tokenizer.col += 1;
                            Ok(TokenKind::Op(Op::BitAndEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::BitAnd)),
                    },
                    b'^' => match tokenizer.peek_next_utf8_char() {
                        Some('=') => {
                            tokenizer.col += 1;
                            Ok(TokenKind::Op(Op::BitXorEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::BitXor)),
                    },
                    b'|' => match tokenizer.peek_next_utf8_char() {
                        Some('|') => {
                            tokenizer.col += 1;
                            match tokenizer.peek_next_utf8_char() {
                                Some('=') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::OrEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::Or)),
                            }
                        }
                        Some('=') => {
                            tokenizer.col += 1;
                            Ok(TokenKind::Op(Op::BitOrEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::BitOr)),
                    },
                    b'=' => match tokenizer.peek_next_utf8_char() {
                        Some('=') => {
                            tokenizer.col += 1;
                            Ok(TokenKind::Op(Op::EqualsEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::Equals)),
                    },
                    b'>' => match tokenizer.peek_next_utf8_char() {
                        Some('>') => {
                            tokenizer.col += 1;
                            match tokenizer.peek_next_utf8_char() {
                                Some('>') => {
                                    tokenizer.col += 1;
                                    match tokenizer.peek_next_utf8_char() {
                                        Some('=') => {
                                            tokenizer.col += 1;
                                            Ok(TokenKind::Op(Op::RightRotateEquals))
                                        }
                                        _ => Ok(TokenKind::Op(Op::RightRotate)),
                                    }
                                }
                                Some('=') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::RightShiftEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::RightShift)),
                            }
                        }
                        Some('=') => {
                            tokenizer.col += 1;
                            Ok(TokenKind::Op(Op::GreaterOrEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::Greater)),
                    },
                    b'<' => match tokenizer.peek_next_utf8_char() {
                        Some('<') => {
                            tokenizer.col += 1;
                            match tokenizer.peek_next_utf8_char() {
                                Some('<') => {
                                    tokenizer.col += 1;
                                    match tokenizer.peek_next_utf8_char() {
                                        Some('=') => {
                                            tokenizer.col += 1;
                                            Ok(TokenKind::Op(Op::LeftRotateEquals))
                                        }
                                        _ => Ok(TokenKind::Op(Op::LeftRotate)),
                                    }
                                }
                                Some('=') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::LeftShiftEquals))
                                }
                                Some('\\') => {
                                    tokenizer.col += 1;
                                    match tokenizer.peek_next_utf8_char() {
                                        Some('=') => {
                                            tokenizer.col += 1;
                                            Ok(TokenKind::Op(Op::WrappingLeftShiftEquals))
                                        }
                                        _ => Ok(TokenKind::Op(Op::WrappingLeftShift)),
                                    }
                                }
                                Some('|') => {
                                    tokenizer.col += 1;
                                    match tokenizer.peek_next_utf8_char() {
                                        Some('=') => {
                                            tokenizer.col += 1;
                                            Ok(TokenKind::Op(Op::SaturatingLeftShiftEquals))
                                        }
                                        _ => Ok(TokenKind::Op(Op::SaturatingLeftShift)),
                                    }
                                }
                                _ => Ok(TokenKind::Op(Op::LeftShift)),
                            }
                        }
                        Some('=') => {
                            tokenizer.col += 1;
                            match tokenizer.peek_next_utf8_char() {
                                Some('>') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::Compare))
                                }
                                _ => Ok(TokenKind::Op(Op::LessOrEquals)),
                            }
                        }
                        _ => Ok(TokenKind::Op(Op::Less)),
                    },
                    unrecognized => {
                        tokenizer.errors.push(Error {
                            kind: ErrorKind::UnrecognizedCharacter(unrecognized as utf8),
                            col: tokenizer.token_start_col,
                            pointers_count: 1,
                        });
                        Err(())
                    }
                }
            };

            let kind = match token_kind_result {
                Ok(kind) => kind,
                Err(()) => {
                    let unexpected_text =
                        &tokenizer.code[tokenizer.token_start_col as usize..tokenizer.col as usize];
                    tokenizer.tokens.text.push(unexpected_text);
                    let unexpected_index = tokenizer.tokens.text.len() as TextIndex - 1;
                    TokenKind::Unexpected(unexpected_index)
                }
            };

            tokenizer.tokens.tokens.push(Token { kind, col: tokenizer.token_start_col });
        }

        // TODO(stefano): handle \r\n
        if let Some(b'\n') = tokenizer.code.as_bytes().last() {
        } else {
            tokenizer.lines.push(Line { start: tokenizer.line_start, end: tokenizer.col });
        }

        for bracket_index in brackets_indicies {
            // there can only be open brackets at this point
            let bracket_token = &tokenizer.tokens.tokens[bracket_index as usize];
            let TokenKind::Bracket(bracket) = bracket_token.kind else {
                unreachable!("incorrect bracket index");
            };

            tokenizer.errors.push(Error {
                kind: ErrorKind::UnclosedBracket((bracket).into()),
                col: bracket_token.col,
                pointers_count: 1,
            });
        }

        let result =
            if tokenizer.errors.is_empty() { Ok(tokenizer.tokens) } else { Err(tokenizer.errors) };
        return TokenizedCode { result, src: SrcCode { src_file, lines: tokenizer.lines } };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Utf8Error<'code> {
    grapheme: &'code str,
    col: offset32,
    pointers_count: offset32,
}

// iteration of characters
impl<'code> Tokenizer<'code, '_> {
    fn token_text(&self) -> &'code str {
        return &self.code[self.token_start_col as usize..self.col as usize];
    }

    // Note: this function is only called when parsing comments, so no special handling of graphemes is required
    fn next_utf8_char_multiline(&mut self) -> Option<utf8> {
        if self.col as usize >= self.code.len() {
            return None;
        }

        let next = self.code.as_bytes()[self.col as usize];
        return match next {
            b'\n' => {
                self.lines.push(Line { start: self.line_start, end: self.col });
                self.col += 1;
                self.line_start = self.col;
                Some('\n')
            }
            b'\r' => {
                if self.col as usize + 1 >= self.code.len() {
                    self.col += 1;
                    return Some('\r');
                }

                if let b'\n' = self.code.as_bytes()[self.col as usize + 1] {
                    self.lines.push(Line { start: self.line_start, end: self.col });
                    self.col += 2;
                    self.line_start = self.col;
                    return Some('\n');
                }

                self.col += 1;
                Some('\r')
            }
            ascii_ch @ 0..=b'\x7F' => {
                self.col += 1;
                Some(ascii_ch as utf8)
            }
            _utf8_ch => {
                let rest_of_code = &self.code[self.col as usize..];
                let Some(utf8_ch) = rest_of_code.chars().next() else {
                    unreachable!("this branch assured we would have a valid utf8 character");
                };

                self.col += utf8_ch.len_utf8() as offset32;
                Some(utf8_ch)
            }
        };
    }

    fn peek_next_ascii_char(&self) -> Option<Result<ascii, Utf8Error<'code>>> {
        if self.col as usize >= self.code.len() {
            return None;
        }

        let next = self.code.as_bytes()[self.col as usize];
        return match next {
            ascii_ch @ 0..=b'\x7F' => Some(Ok(ascii_ch)),
            _utf8_ch => {
                let rest_of_code = &self.code[self.col as usize..];
                let mut rest_of_line_graphemes = rest_of_code.graphemes(true);
                let Some(grapheme) = rest_of_line_graphemes.next() else {
                    unreachable!("this branch assured we would have a valid grapheme");
                };

                let pointers_count = match grapheme.chars_width() {
                    0 => 1,
                    pointers_count => pointers_count,
                };

                // let Some(utf8_ch) = rest_of_line.chars().next() else {
                //     unreachable!();
                // };
                // let end_of_character = utf8_ch.len_utf8() as offset32;
                // let grapheme = &self.code[self.col as usize..(self.col + end_of_character) as usize];
                // let pointers_count = 1;

                Some(Err(Utf8Error { grapheme, col: self.col, pointers_count }))
            }
        };
    }

    // Note: this function is only called when parsing operators, so no special handling of graphemes is required
    fn peek_next_utf8_char(&self) -> Option<utf8> {
        if self.col as usize >= self.code.len() {
            return None;
        }

        let next = self.code.as_bytes()[self.col as usize];
        return match next {
            ascii_ch @ 0..=b'\x7F' => Some(ascii_ch as utf8),
            _utf8_ch => {
                let rest_of_code = &self.code[self.col as usize..];
                let Some(utf8_ch) = rest_of_code.chars().next() else {
                    unreachable!("this branch assured we would have a valid utf8 character");
                };

                Some(utf8_ch)
            }
        };
    }
}

impl<'code> Tokenizer<'code, '_> {
    fn identifier(&mut self) -> Result<TokenKind, ()> {
        // NOTE(stefano): why 63 and not 64?
        const MAX_IDENTIFIER_LEN: offset32 = 63;
        let previous_errors_len = self.errors.len();

        loop {
            match self.peek_next_ascii_char() {
                Some(Ok(b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_')) => {
                    self.col += 1;
                }
                Some(Err(error)) => {
                    self.errors.push(Error {
                        kind: ErrorKind::Utf8InIdentifier { grapheme: error.grapheme },
                        col: error.col,
                        pointers_count: error.pointers_count,
                    });
                    self.col += error.grapheme.len() as offset32;
                }
                Some(Ok(_)) | None => break,
            }
        }

        if previous_errors_len != self.errors.len() {
            return Err(());
        };

        let identifier = match &self.code[self.token_start_col as usize..self.col as usize] {
            "let" => TokenKind::Let,
            "var" => TokenKind::Var,
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
                let identifier_len = identifier.len() as offset32;
                if identifier_len as offset32 > MAX_IDENTIFIER_LEN {
                    self.errors.push(Error {
                        kind: ErrorKind::IdentifierTooLong { max: MAX_IDENTIFIER_LEN },
                        col: self.token_start_col,
                        pointers_count: identifier_len,
                    });
                    return Err(());
                }

                self.tokens.text.push(identifier);
                let identifier_index = self.tokens.text.len() as TextIndex - 1;
                TokenKind::Identifier(identifier_index)
            }
        };

        return Ok(identifier);
    }

    fn integer_decimal(&mut self) -> Result<&'code [ascii], ()> {
        let previous_errors_len = self.errors.len();

        loop {
            match self.peek_next_ascii_char() {
                Some(Ok(b'0'..=b'9' | b'_')) => {
                    self.col += 1;
                }
                Some(Ok(letter @ (b'a'..=b'z' | b'A'..=b'Z'))) => {
                    self.errors.push(Error {
                        kind: ErrorKind::LetterInNumberLiteral(Base::Decimal, letter),
                        col: self.col,
                        pointers_count: 1,
                    });
                    self.col += 1;
                }
                Some(Err(error)) => {
                    self.errors.push(Error {
                        kind: ErrorKind::Utf8InNumberLiteral { grapheme: error.grapheme },
                        col: error.col,
                        pointers_count: error.pointers_count,
                    });
                    self.col += error.grapheme.len() as offset32;
                }
                Some(Ok(_)) | None => break,
            }
        }

        return if previous_errors_len == self.errors.len() {
            let digits = &self.code[self.token_start_col as usize..self.col as usize];
            Ok(digits.as_bytes())
        } else {
            Err(())
        };
    }

    fn integer_binary(&mut self) -> Result<&'code [ascii], ()> {
        let previous_errors_len = self.errors.len();

        loop {
            match self.peek_next_ascii_char() {
                Some(Ok(b'0'..=b'1' | b'_')) => {
                    self.col += 1;
                }
                Some(Ok(out_of_range @ b'2'..=b'9')) => {
                    self.errors.push(Error {
                        kind: ErrorKind::DigitOutOfRangeInNumberLiteral(Base::Binary, out_of_range),
                        col: self.col,
                        pointers_count: 1,
                    });
                    self.col += 1;
                }
                Some(Ok(letter @ (b'a'..=b'z' | b'A'..=b'Z'))) => {
                    self.errors.push(Error {
                        kind: ErrorKind::LetterInNumberLiteral(Base::Binary, letter),
                        col: self.col,
                        pointers_count: 1,
                    });
                    self.col += 1;
                }
                Some(Err(error)) => {
                    self.errors.push(Error {
                        kind: ErrorKind::Utf8InNumberLiteral { grapheme: error.grapheme },
                        col: error.col,
                        pointers_count: error.pointers_count,
                    });
                    self.col += error.grapheme.len() as offset32;
                }
                Some(Ok(_)) | None => break,
            }
        }

        if previous_errors_len != self.errors.len() {
            return Err(());
        }

        let digits = &self.code[self.token_start_col as usize..self.col as usize];
        return Ok(digits.as_bytes());
    }

    fn integer_octal(&mut self) -> Result<&'code [ascii], ()> {
        let previous_errors_len = self.errors.len();

        loop {
            match self.peek_next_ascii_char() {
                Some(Ok(b'0'..=b'7' | b'_')) => {
                    self.col += 1;
                }
                Some(Ok(out_of_range @ b'8'..=b'9')) => {
                    self.errors.push(Error {
                        kind: ErrorKind::DigitOutOfRangeInNumberLiteral(Base::Octal, out_of_range),
                        col: self.col,
                        pointers_count: 1,
                    });
                    self.col += 1;
                }
                Some(Ok(letter @ (b'a'..=b'z' | b'A'..=b'Z'))) => {
                    self.errors.push(Error {
                        kind: ErrorKind::LetterInNumberLiteral(Base::Octal, letter),
                        col: self.col,
                        pointers_count: 1,
                    });
                    self.col += 1;
                }
                Some(Err(error)) => {
                    self.errors.push(Error {
                        kind: ErrorKind::Utf8InNumberLiteral { grapheme: error.grapheme },
                        col: error.col,
                        pointers_count: error.pointers_count,
                    });
                    self.col += error.grapheme.len() as offset32;
                }
                Some(Ok(_)) | None => break,
            }
        }

        if previous_errors_len != self.errors.len() {
            return Err(());
        }

        let digits = &self.code[self.token_start_col as usize..self.col as usize];
        return Ok(digits.as_bytes());
    }

    fn integer_hexadecimal(&mut self) -> Result<&'code [ascii], ()> {
        let previous_errors_len = self.errors.len();

        loop {
            match self.peek_next_ascii_char() {
                Some(Ok(b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F' | b'_')) => {
                    self.col += 1;
                }
                Some(Ok(out_of_range @ (b'g'..=b'z' | b'G'..=b'Z'))) => {
                    self.errors.push(Error {
                        kind: ErrorKind::DigitOutOfRangeInNumberLiteral(
                            Base::Hexadecimal,
                            out_of_range,
                        ),
                        col: self.col,
                        pointers_count: 1,
                    });
                    self.col += 1;
                }
                Some(Err(error)) => {
                    self.errors.push(Error {
                        kind: ErrorKind::Utf8InNumberLiteral { grapheme: error.grapheme },
                        col: error.col,
                        pointers_count: error.pointers_count,
                    });
                    self.col += error.grapheme.len() as offset32;
                }
                Some(Ok(_)) | None => break,
            }
        }

        if previous_errors_len != self.errors.len() {
            return Err(());
        }

        let digits = &self.code[self.token_start_col as usize..self.col as usize];
        return Ok(digits.as_bytes());
    }

    fn raw_string_literal(&mut self) -> Result<Vec<ascii>, ()> {
        let previous_errors_len = self.errors.len();

        let kind = QuotedLiteralKind::RawStr;
        let quote = kind.quote();
        let mut literal = Vec::<ascii>::new();

        loop {
            let next_character = match self.peek_next_ascii_char() {
                Some(Ok(b'\n')) | None => {
                    self.errors.push(Error {
                        kind: ErrorKind::UnclosedQuotedLiteral(kind),
                        col: self.token_start_col,
                        pointers_count: self.token_text().chars_width(),
                    });
                    break;
                }
                Some(Ok(next_character)) => {
                    self.col += 1;
                    next_character
                }
                Some(Err(error)) => {
                    self.errors.push(Error {
                        kind: ErrorKind::Utf8InIdentifier { grapheme: error.grapheme },
                        col: error.col,
                        pointers_count: error.pointers_count,
                    });
                    self.col += error.grapheme.len() as offset32;
                    continue;
                }
            };

            let character = match next_character {
                b'\\' => {
                    let escape_character = match self.peek_next_ascii_char() {
                        Some(Ok(b'\n')) | None => {
                            self.errors.push(Error {
                                kind: ErrorKind::UnclosedQuotedLiteral(kind),
                                col: self.token_start_col,
                                pointers_count: self.token_text().chars_width(),
                            });
                            break;
                        }
                        Some(Ok(escape_character)) => escape_character,
                        Some(Err(error)) => {
                            self.errors.push(Error {
                                kind: ErrorKind::Utf8InQuotedLiteral {
                                    grapheme: error.grapheme,
                                    quoted_literal_kind: kind,
                                },
                                col: error.col,
                                pointers_count: error.pointers_count,
                            });
                            self.col += error.grapheme.len() as offset32;
                            continue;
                        }
                    };

                    match escape_character {
                        b'"' => {
                            self.col += 1;
                            b'"'
                        }
                        _ => b'\\',
                    }
                }
                control @ (b'\x00'..=b'\x1F' | b'\x7F') => {
                    self.errors.push(Error {
                        kind: ErrorKind::ControlCharacterInQuotedLiteral(kind, control as utf8),
                        col: self.col - 1,
                        pointers_count: 1,
                    });
                    control
                }
                ch if ch == quote as u8 => break,
                ch => ch,
            };

            literal.push(character);
        }

        return if previous_errors_len == self.errors.len() { Ok(literal) } else { Err(()) };
    }

    fn quoted_literal(&mut self, kind: QuotedLiteralKind) -> Result<Vec<ascii>, ()> {
        debug_assert!(kind != QuotedLiteralKind::RawStr, "raw string literals should be tokenized differently than non raw strings and characters");

        let previous_errors_len = self.errors.len();

        let quote = kind.quote();
        let mut literal = Vec::<ascii>::new();

        loop {
            let next_character = match self.peek_next_ascii_char() {
                Some(Ok(b'\n')) | None => {
                    self.errors.push(Error {
                        kind: ErrorKind::UnclosedQuotedLiteral(kind),
                        col: self.token_start_col,
                        pointers_count: self.token_text().chars_width(),
                    });
                    break;
                }
                Some(Ok(next_character)) => {
                    self.col += 1;
                    next_character
                }
                Some(Err(error)) => {
                    self.errors.push(Error {
                        kind: ErrorKind::Utf8InQuotedLiteral {
                            grapheme: error.grapheme,
                            quoted_literal_kind: kind,
                        },
                        col: error.col,
                        pointers_count: error.pointers_count,
                    });
                    self.col += error.grapheme.len() as offset32;
                    continue;
                }
            };

            let character = match next_character {
                b'\\' => {
                    let escape_character = match self.peek_next_ascii_char() {
                        Some(Ok(b'\n')) | None => {
                            self.errors.push(Error {
                                kind: ErrorKind::UnclosedQuotedLiteral(kind),
                                col: self.token_start_col,
                                pointers_count: self.token_text().chars_width(),
                            });
                            break;
                        }
                        Some(Ok(escape_character)) => {
                            self.col += 1;
                            escape_character
                        }
                        Some(Err(error)) => {
                            self.errors.push(Error {
                                kind: ErrorKind::Utf8InQuotedLiteral {
                                    grapheme: error.grapheme,
                                    quoted_literal_kind: kind,
                                },
                                col: error.col,
                                pointers_count: error.pointers_count,
                            });
                            self.col += error.grapheme.len() as offset32;
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
                            self.errors.push(Error {
                                kind: ErrorKind::UnrecognizedEscapeCharacterInQuotedLiteral(
                                    kind, unrecognized as utf8,
                                ),
                                col: self.col - 2,
                                pointers_count: 2,
                            });
                            literal.push(b'\\');
                            unrecognized
                        }
                    }
                }
                control @ (b'\x00'..=b'\x1F' | b'\x7F') => {
                    self.errors.push(Error {
                        kind: ErrorKind::ControlCharacterInQuotedLiteral(kind, control as utf8),
                        col: self.col - 1,
                        pointers_count: 1,
                    });
                    control
                }
                ch if ch == quote as u8 => break,
                ch => ch,
            };

            literal.push(character);
        }

        return if previous_errors_len == self.errors.len() { Ok(literal) } else { Err(()) };
    }
}

#[derive(Debug, Clone)]
pub enum ErrorKind<'code> {
    UnclosedBlockComment,

    UnclosedBracket(OpenBracket),
    UnopenedBracket(CloseBracket),
    MismatchedBracket { actual: OpenBracket, expected: CloseBracket },

    Utf8InQuotedLiteral { grapheme: &'code str, quoted_literal_kind: QuotedLiteralKind },
    UnclosedQuotedLiteral(QuotedLiteralKind),
    ControlCharacterInQuotedLiteral(QuotedLiteralKind, utf8),
    UnrecognizedEscapeCharacterInQuotedLiteral(QuotedLiteralKind, utf8),
    EmptyCharacterLiteral,
    MultipleCharactersInCharacterLiteral,

    Utf8InNumberLiteral { grapheme: &'code str },
    LetterInNumberLiteral(Base, ascii),
    DigitOutOfRangeInNumberLiteral(Base, ascii),

    Utf8InIdentifier { grapheme: &'code str },
    IdentifierTooLong { max: offset32 },

    Utf8Character { grapheme: &'code str },
    UnrecognizedCharacter(utf8),
}

impl IntoErrorInfo for ErrorKind<'_> {
    fn info(&self) -> ErrorInfo {
        let (error_message, error_cause_message) = match self {
            Self::UnclosedBlockComment => (
                "unclosed block comment".into(),
                "missing closing `##`".into(),
            ),

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

            Self::Utf8InQuotedLiteral { grapheme, quoted_literal_kind } => (
                format!("invalid {quoted_literal_kind} literal character '{grapheme}' {}", grapheme.escape_unicode()).into(),
                "utf8 characters are not allowed".into(),
            ),
            Self::UnclosedQuotedLiteral(kind) => (
                format!("unclosed {kind} literal").into(),
                format!("missing closing {} quote", kind.quote() as u8 as char).into()
            ),
            Self::ControlCharacterInQuotedLiteral(kind, control_character) => (
                format!("invalid {kind} literal character '{}' {}", control_character.escape_debug(), control_character.escape_unicode()).into(),
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

            Self::Utf8InNumberLiteral { grapheme } => (
                format!("invalid integer literal character '{grapheme}' {}", grapheme.escape_unicode()).into(),
                "utf8 characters are not allowed".into(),
            ),
            Self::LetterInNumberLiteral(base, letter) => (
                format!("invalid integer literal letter '{}'", *letter as utf8).into(),
                format!("not allowed in a base {} number", *base as u8).into(),
            ),
            Self::DigitOutOfRangeInNumberLiteral(base, digit) => (
                format!("invalid integer literal digit '{}'", *digit as utf8).into(),
                format!("out of the valid range for a base {} number {:?}", *base as u8, base.range()).into(),
            ),

            Self::Utf8InIdentifier { grapheme } => (
                format!("invalid identifier character '{grapheme}' {}", grapheme.escape_unicode()).into(),
                "utf8 characters are not allowed".into(),
            ),
            Self::IdentifierTooLong { max } => (
                "invalid identifier".into(),
                format!("exceeds the length limit of {max}").into(),
            ),

            Self::Utf8Character { grapheme } => (
                format!("invalid character '{grapheme}' {}", grapheme.escape_unicode()).into(),
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
