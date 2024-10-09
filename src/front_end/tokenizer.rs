// TODO(stefano): multiline strings
// TODO(stefano): more escape characters

use super::{
    src_file::{index32, offset32, SrcFile},
    Error, ErrorInfo, IntoErrorInfo,
};
use core::fmt::Display;
use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthChar;

pub(super) trait DisplayLen {
    fn display_len(&self) -> offset32;
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

/// integer literals are never empty and always contain valid ascii digits
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

    #[must_use]
    pub fn range(self) -> Vec<core::ops::RangeInclusive<utf8>> {
        return match self {
            Self::Decimal => vec!['0'..='9'],
            Self::Binary => vec!['0'..='1'],
            Self::Octal => vec!['0'..='7'],
            Self::Hexadecimal => vec!['0'..='9', 'A'..='F', 'a'..='f'],
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
        return unsafe { core::mem::transmute::<Self, Base>(self) }.prefix();
    }

    #[must_use]
    pub fn range(self) -> Vec<core::ops::RangeInclusive<utf8>> {
        return unsafe { core::mem::transmute::<Self, Base>(self) }.range();
    }
}

#[repr(transparent)]
#[derive(Debug, Clone)]
pub(crate) struct Str(pub(crate) Box<[ascii]>);

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
    fn display_len(&self) -> offset32 {
        return match self {
            Self::Let | Self::Var => 3,
        };
    }
}

#[derive(Debug, Clone, Copy)]
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

impl DisplayLen for Bracket {
    #[inline(always)]
    fn display_len(&self) -> offset32 {
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

/* IDEA(stefano):
introduce "unchecked" operators
skip safety checks, maybe using the '?' or the '!' suffix, i.e:
- <<?, <<<?, >>>?, or <<!, >>!, <<<!, >>>! -> skip the check for a positive 6bit shift amount
- **? -> skip the check for a neagtive index
*/
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

impl DisplayLen for Op {
    fn display_len(&self) -> offset32 {
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
    // IDEA(stefano): remove from the returned tokens, to avoid encountering them during the parsing stage
    Comment(&'src str),
    BlockComment(&'src str),
    Unexpected(&'src str),

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
    Integer(Base, Integer<'src>),
    Ascii(ascii),
    Str(Str),
    RawStr(Str),

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

    // IDEA(stefano): expant into TokenKind::Let and TokenKind::Var
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
            Self::BlockComment(text) => write!(f, "##{text}##"),
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
            Self::Str(string) | Self::RawStr(string) => {
                write!(f, "\"")?;
                for ch in &*string.0 {
                    write!(f, "{}", ch.escape_ascii())?;
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
    fn display_len(&self) -> offset32 {
        #[inline]
        fn ascii_escaped_len(ascii_char: ascii) -> offset32 {
            // Note: ascii type guarantees the value to be valid utf8
            let utf8_char = ascii_char as utf8;
            return utf8_char.escape_debug().len() as offset32;
        }

        fn utf8_len(text: &str) -> offset32 {
            let mut len = 0;
            for grapheme in text.graphemes(true) {
                for character in grapheme.chars() {
                    let character_utf8_len = match character.width_cjk() {
                        Some(character_utf8_len) => character_utf8_len,
                        None => 1,
                    };
                    len += character_utf8_len as offset32;
                }
            }

            return len;
        }

        return match self {
            Self::Comment(text) => utf8_len(text) + 1, // + 1 to account for the `#`
            Self::BlockComment(text) => utf8_len(text) + 4, // + 4 to account for `##` and `##`
            Self::Unexpected(text) => utf8_len(text),

            Self::Bracket(bracket) => bracket.display_len(),
            Self::Colon => 1,
            Self::SemiColon => 1,
            Self::Comma => 1,
            Self::Op(op) => op.display_len(),

            Self::Integer(base, integer) => {
                base.prefix().len() as offset32 + integer.0.len() as offset32
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
            Self::RawStr(text) => text.0.len() as offset32 + 3, // + 1 for the `r` prefix, and + 2 for the quotes

            Self::Identifier(name) => name.len() as offset32,

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
    pub(crate) col: offset32,
}

#[derive(Debug)]
pub struct Tokenizer<'src> {
    src: &'src SrcFile,
    errors: Vec<Error<ErrorKind<'src>>>,

    col: offset32,
    token_start_col: offset32,

    line_index: index32,
}

impl<'src> Tokenizer<'src> {
    pub fn tokenize(src: &'src SrcFile) -> Result<Vec<Token<'src>>, Vec<Error<ErrorKind<'src>>>> {
        let mut this = Self {
            src,
            errors: Vec::new(),
            col: 0,
            token_start_col: 0,
            line_index: 0,
        };

        let mut tokens = Vec::<Token<'src>>::new();
        let mut brackets_indicies = Vec::<index32>::new();

        'tokenization: loop {
            let token_kind_result = 'next_token: loop {
                this.token_start_col = this.col;

                let next = match this.peek_next_ascii_char() {
                    Ok(Some(ch)) => {
                        this.col += 1;
                        ch
                    }
                    Ok(None) => break 'tokenization,
                    Err(error) => {
                        this.col += error.grapheme.len() as offset32;
                        this.errors.push(Error {
                            kind: ErrorKind::Utf8Character { grapheme: error.grapheme },
                            col: error.col,
                            pointers_count: error.pointers_count,
                        });
                        break 'next_token Err(());
                    }
                };

                break 'next_token match next {
                    // ignore whitespace
                    b' ' | b'\t' | b'\r' | b'\x0C' => continue 'next_token,

                    // next line
                    b'\n' => {
                        this.line_index += 1;
                        continue 'next_token;
                    }

                    b'r' => match this.peek_next_utf8_char() {
                        Some('"') => {
                            this.col += 1; // skip the `r` prefix

                            match this.raw_string_literal() {
                                Ok(literal) => {
                                    Ok(TokenKind::RawStr(Str(literal.into_boxed_slice())))
                                }
                                Err(()) => Err(()),
                            }
                        }
                        _ => this.identifier(),
                    },
                    b'a'..=b'z' | b'A'..=b'Z' | b'_' => this.identifier(),
                    // IDEA(stefano): debate wether to allow trailing underscores or to emit a warning
                    // IDEA(stefano): emit warning of inconsistent casing of letters, i.e. 0xFFff_fFfF_ffFF_ffFF
                    b'0' => {
                        let (base, integer) = match this.peek_next_utf8_char() {
                            Some('b') => {
                                this.col += 1;
                                (Base::Binary, this.integer_binary())
                            }
                            Some('o') => {
                                this.col += 1;
                                (Base::Octal, this.integer_octal())
                            }
                            Some('x') => {
                                this.col += 1;
                                (Base::Hexadecimal, this.integer_hexadecimal())
                            }
                            Some(_) | None => (Base::Decimal, this.integer_decimal()),
                        };

                        match integer {
                            Ok(literal) => Ok(TokenKind::Integer(base, Integer(literal))),
                            Err(()) => Err(()),
                        }
                    }
                    b'1'..=b'9' => match this.integer_decimal() {
                        Ok(literal) => Ok(TokenKind::Integer(Base::Decimal, Integer(literal))),
                        Err(()) => Err(()),
                    },
                    b'"' => match this.quoted_literal(QuotedLiteralKind::Str) {
                        Ok(literal) => Ok(TokenKind::Str(Str(literal.into_boxed_slice()))),
                        Err(()) => Err(()),
                    },
                    b'\'' => match this.quoted_literal(QuotedLiteralKind::Ascii) {
                        Ok(literal) => match literal.as_slice() {
                            [character] => Ok(TokenKind::Ascii(*character)),
                            [] => {
                                this.errors.push(Error {
                                    kind: ErrorKind::EmptyCharacterLiteral,
                                    col: this.token_start_col,
                                    pointers_count: 2,
                                });
                                Err(())
                            }
                            [_, ..] => {
                                this.errors.push(Error {
                                    kind: ErrorKind::MultipleCharactersInCharacterLiteral,
                                    col: this.token_start_col,
                                    pointers_count: this.token_len(),
                                });
                                Err(())
                            }
                        },
                        Err(()) => Err(()),
                    },
                    b'#' => match this.peek_next_utf8_char() {
                        Some('#') => {
                            'next_character: loop {
                                match this.next_utf8_char_multiline() {
                                    Some('#') => match this.next_utf8_char_multiline() {
                                        Some('#') => break 'next_character,
                                        Some(_) => {}
                                        None => {
                                            this.errors.push(Error {
                                                kind: ErrorKind::UnclosedBlockComment,
                                                col: this.token_start_col,
                                                pointers_count: 2,
                                            });
                                            break 'next_token Err(());
                                        }
                                    },
                                    Some(_) => {}
                                    None => {
                                        this.errors.push(Error {
                                            kind: ErrorKind::UnclosedBlockComment,
                                            col: this.token_start_col,
                                            pointers_count: 2,
                                        });
                                        break 'next_token Err(());
                                    }
                                }
                            }

                            // starting at this.token_start_col + 2 to skip the `##`
                            // ending at this.col - 2 to skip the `##`
                            let comment = &this.src.code
                                [this.token_start_col as usize + 2..this.col as usize - 2];
                            Ok(TokenKind::BlockComment(comment))
                        }
                        Some(_) | None => {
                            let line = &this.src.lines[this.line_index as usize];

                            // starting a this.col to ignore the hash symbol
                            let comment = &this.src.code[this.col as usize..line.end as usize];

                            // consuming the rest of the characters in the current line
                            this.col = line.end;

                            Ok(TokenKind::Comment(comment))
                        }
                    },
                    b'(' => {
                        brackets_indicies.push(tokens.len() as index32);
                        Ok(TokenKind::Bracket(Bracket::OpenRound))
                    }
                    b')' => match brackets_indicies.pop() {
                        Some(bracket_index) => {
                            let TokenKind::Bracket(bracket) = &tokens[bracket_index as usize].kind
                            else {
                                unreachable!("incorrect bracket index");
                            };

                            match *bracket {
                                Bracket::OpenRound
                                | Bracket::CloseRound
                                | Bracket::CloseCurly
                                | Bracket::CloseSquare => {
                                    Ok(TokenKind::Bracket(Bracket::CloseRound))
                                }
                                actual @ (Bracket::OpenCurly | Bracket::OpenSquare) => {
                                    this.errors.push(Error {
                                        kind: ErrorKind::MismatchedBracket {
                                            expected: Bracket::CloseRound,
                                            actual,
                                        },
                                        col: this.token_start_col,
                                        pointers_count: 1,
                                    });
                                    Err(())
                                }
                            }
                        }
                        None => {
                            this.errors.push(Error {
                                kind: ErrorKind::UnopenedBracket(Bracket::CloseRound),
                                col: this.token_start_col,
                                pointers_count: 1,
                            });
                            Err(())
                        }
                    },
                    b'[' => {
                        brackets_indicies.push(tokens.len() as index32);
                        Ok(TokenKind::Bracket(Bracket::OpenSquare))
                    }
                    b']' => match brackets_indicies.pop() {
                        Some(bracket_index) => {
                            let TokenKind::Bracket(bracket) = &tokens[bracket_index as usize].kind
                            else {
                                unreachable!("incorrect bracket index");
                            };

                            match *bracket {
                                Bracket::OpenSquare
                                | Bracket::CloseSquare
                                | Bracket::CloseCurly
                                | Bracket::CloseRound => {
                                    Ok(TokenKind::Bracket(Bracket::CloseSquare))
                                }
                                actual @ (Bracket::OpenCurly | Bracket::OpenRound) => {
                                    this.errors.push(Error {
                                        kind: ErrorKind::MismatchedBracket {
                                            expected: Bracket::CloseSquare,
                                            actual,
                                        },
                                        col: this.token_start_col,
                                        pointers_count: 1,
                                    });
                                    Err(())
                                }
                            }
                        }
                        None => {
                            this.errors.push(Error {
                                kind: ErrorKind::UnopenedBracket(Bracket::CloseSquare),
                                col: this.token_start_col,
                                pointers_count: 1,
                            });
                            Err(())
                        }
                    },
                    b'{' => {
                        brackets_indicies.push(tokens.len() as index32);
                        Ok(TokenKind::Bracket(Bracket::OpenCurly))
                    }
                    b'}' => match brackets_indicies.pop() {
                        Some(bracket_index) => {
                            let TokenKind::Bracket(bracket) = &tokens[bracket_index as usize].kind
                            else {
                                unreachable!("incorrect bracket index");
                            };

                            match *bracket {
                                Bracket::OpenCurly
                                | Bracket::CloseCurly
                                | Bracket::CloseRound
                                | Bracket::CloseSquare => {
                                    Ok(TokenKind::Bracket(Bracket::CloseCurly))
                                }
                                actual @ (Bracket::OpenRound | Bracket::OpenSquare) => {
                                    this.errors.push(Error {
                                        kind: ErrorKind::MismatchedBracket {
                                            expected: Bracket::CloseCurly,
                                            actual,
                                        },
                                        col: this.token_start_col,
                                        pointers_count: 1,
                                    });
                                    Err(())
                                }
                            }
                        }
                        None => {
                            this.errors.push(Error {
                                kind: ErrorKind::UnopenedBracket(Bracket::CloseCurly),
                                col: this.token_start_col,
                                pointers_count: 1,
                            });
                            Err(())
                        }
                    },
                    b':' => Ok(TokenKind::Colon),
                    b';' => Ok(TokenKind::SemiColon),
                    b',' => Ok(TokenKind::Comma),
                    b'!' => match this.peek_next_utf8_char() {
                        Some('=') => {
                            this.col += 1;
                            Ok(TokenKind::Op(Op::NotEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::Not)),
                    },
                    b'*' => match this.peek_next_utf8_char() {
                        Some('*') => {
                            this.col += 1;
                            match this.peek_next_utf8_char() {
                                Some('=') => {
                                    this.col += 1;
                                    Ok(TokenKind::Op(Op::PowEquals))
                                }
                                Some('\\') => {
                                    this.col += 1;
                                    match this.peek_next_utf8_char() {
                                        Some('=') => {
                                            this.col += 1;
                                            Ok(TokenKind::Op(Op::WrappingPowEquals))
                                        }
                                        _ => Ok(TokenKind::Op(Op::WrappingPow)),
                                    }
                                }
                                Some('|') => {
                                    this.col += 1;
                                    match this.peek_next_utf8_char() {
                                        Some('=') => {
                                            this.col += 1;
                                            Ok(TokenKind::Op(Op::SaturatingPowEquals))
                                        }
                                        _ => Ok(TokenKind::Op(Op::SaturatingPow)),
                                    }
                                }
                                _ => Ok(TokenKind::Op(Op::Pow)),
                            }
                        }
                        Some('=') => {
                            this.col += 1;
                            Ok(TokenKind::Op(Op::TimesEquals))
                        }
                        Some('\\') => {
                            this.col += 1;
                            match this.peek_next_utf8_char() {
                                Some('=') => {
                                    this.col += 1;
                                    Ok(TokenKind::Op(Op::WrappingTimesEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::WrappingTimes)),
                            }
                        }
                        Some('|') => {
                            this.col += 1;
                            match this.peek_next_utf8_char() {
                                Some('=') => {
                                    this.col += 1;
                                    Ok(TokenKind::Op(Op::SaturatingTimesEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::SaturatingTimes)),
                            }
                        }
                        _ => Ok(TokenKind::Op(Op::Times)),
                    },
                    b'/' => match this.peek_next_utf8_char() {
                        Some('=') => {
                            this.col += 1;
                            Ok(TokenKind::Op(Op::DivideEquals))
                        }
                        Some('\\') => {
                            this.col += 1;
                            match this.peek_next_utf8_char() {
                                Some('=') => {
                                    this.col += 1;
                                    Ok(TokenKind::Op(Op::WrappingDivideEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::WrappingDivide)),
                            }
                        }
                        Some('|') => {
                            this.col += 1;
                            match this.peek_next_utf8_char() {
                                Some('=') => {
                                    this.col += 1;
                                    Ok(TokenKind::Op(Op::SaturatingDivideEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::SaturatingDivide)),
                            }
                        }
                        _ => Ok(TokenKind::Op(Op::Divide)),
                    },
                    b'%' => match this.peek_next_utf8_char() {
                        Some('=') => {
                            this.col += 1;
                            Ok(TokenKind::Op(Op::RemainderEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::Remainder)),
                    },
                    b'+' => match this.peek_next_utf8_char() {
                        Some('=') => {
                            this.col += 1;
                            Ok(TokenKind::Op(Op::PlusEquals))
                        }
                        Some('\\') => {
                            this.col += 1;
                            match this.peek_next_utf8_char() {
                                Some('=') => {
                                    this.col += 1;
                                    Ok(TokenKind::Op(Op::WrappingPlusEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::WrappingPlus)),
                            }
                        }
                        Some('|') => {
                            this.col += 1;
                            match this.peek_next_utf8_char() {
                                Some('=') => {
                                    this.col += 1;
                                    Ok(TokenKind::Op(Op::SaturatingPlusEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::SaturatingPlus)),
                            }
                        }
                        _ => Ok(TokenKind::Op(Op::Plus)),
                    },
                    b'-' => match this.peek_next_utf8_char() {
                        Some('=') => {
                            this.col += 1;
                            Ok(TokenKind::Op(Op::MinusEquals))
                        }
                        Some('\\') => {
                            this.col += 1;
                            match this.peek_next_utf8_char() {
                                Some('=') => {
                                    this.col += 1;
                                    Ok(TokenKind::Op(Op::WrappingMinusEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::WrappingMinus)),
                            }
                        }
                        Some('|') => {
                            this.col += 1;
                            match this.peek_next_utf8_char() {
                                Some('=') => {
                                    this.col += 1;
                                    Ok(TokenKind::Op(Op::SaturatingMinusEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::SaturatingMinus)),
                            }
                        }
                        _ => Ok(TokenKind::Op(Op::Minus)),
                    },
                    b'&' => match this.peek_next_utf8_char() {
                        Some('&') => {
                            this.col += 1;
                            match this.peek_next_utf8_char() {
                                Some('=') => {
                                    this.col += 1;
                                    Ok(TokenKind::Op(Op::AndEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::And)),
                            }
                        }
                        Some('=') => {
                            this.col += 1;
                            Ok(TokenKind::Op(Op::BitAndEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::BitAnd)),
                    },
                    b'^' => match this.peek_next_utf8_char() {
                        Some('=') => {
                            this.col += 1;
                            Ok(TokenKind::Op(Op::BitXorEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::BitXor)),
                    },
                    b'|' => match this.peek_next_utf8_char() {
                        Some('|') => {
                            this.col += 1;
                            match this.peek_next_utf8_char() {
                                Some('=') => {
                                    this.col += 1;
                                    Ok(TokenKind::Op(Op::OrEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::Or)),
                            }
                        }
                        Some('=') => {
                            this.col += 1;
                            Ok(TokenKind::Op(Op::BitOrEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::BitOr)),
                    },
                    b'=' => match this.peek_next_utf8_char() {
                        Some('=') => {
                            this.col += 1;
                            Ok(TokenKind::Op(Op::EqualsEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::Equals)),
                    },
                    b'>' => match this.peek_next_utf8_char() {
                        Some('>') => {
                            this.col += 1;
                            match this.peek_next_utf8_char() {
                                Some('>') => {
                                    this.col += 1;
                                    match this.peek_next_utf8_char() {
                                        Some('=') => {
                                            this.col += 1;
                                            Ok(TokenKind::Op(Op::RightRotateEquals))
                                        }
                                        _ => Ok(TokenKind::Op(Op::RightRotate)),
                                    }
                                }
                                Some('=') => {
                                    this.col += 1;
                                    Ok(TokenKind::Op(Op::RightShiftEquals))
                                }
                                _ => Ok(TokenKind::Op(Op::RightShift)),
                            }
                        }
                        Some('=') => {
                            this.col += 1;
                            Ok(TokenKind::Op(Op::GreaterOrEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::Greater)),
                    },
                    b'<' => match this.peek_next_utf8_char() {
                        Some('<') => {
                            this.col += 1;
                            match this.peek_next_utf8_char() {
                                Some('<') => {
                                    this.col += 1;
                                    match this.peek_next_utf8_char() {
                                        Some('=') => {
                                            this.col += 1;
                                            Ok(TokenKind::Op(Op::LeftRotateEquals))
                                        }
                                        _ => Ok(TokenKind::Op(Op::LeftRotate)),
                                    }
                                }
                                Some('=') => {
                                    this.col += 1;
                                    Ok(TokenKind::Op(Op::LeftShiftEquals))
                                }
                                Some('\\') => {
                                    this.col += 1;
                                    match this.peek_next_utf8_char() {
                                        Some('=') => {
                                            this.col += 1;
                                            Ok(TokenKind::Op(Op::WrappingLeftShiftEquals))
                                        }
                                        _ => Ok(TokenKind::Op(Op::WrappingLeftShift)),
                                    }
                                }
                                Some('|') => {
                                    this.col += 1;
                                    match this.peek_next_utf8_char() {
                                        Some('=') => {
                                            this.col += 1;
                                            Ok(TokenKind::Op(Op::SaturatingLeftShiftEquals))
                                        }
                                        _ => Ok(TokenKind::Op(Op::SaturatingLeftShift)),
                                    }
                                }
                                _ => Ok(TokenKind::Op(Op::LeftShift)),
                            }
                        }
                        Some('=') => {
                            this.col += 1;
                            match this.peek_next_utf8_char() {
                                Some('>') => {
                                    this.col += 1;
                                    Ok(TokenKind::Op(Op::Compare))
                                }
                                _ => Ok(TokenKind::Op(Op::LessOrEquals)),
                            }
                        }
                        _ => Ok(TokenKind::Op(Op::Less)),
                    },
                    unrecognized => {
                        this.errors.push(Error {
                            kind: ErrorKind::UnrecognizedCharacter(unrecognized as utf8),
                            col: this.token_start_col,
                            pointers_count: 1,
                        });
                        Err(())
                    }
                };
            };

            let kind = match token_kind_result {
                Ok(kind) => kind,
                Err(()) => TokenKind::Unexpected(
                    &this.src.code[this.token_start_col as usize..this.col as usize],
                ),
            };

            tokens.push(Token { kind, col: this.token_start_col });
        }

        for bracket_index in brackets_indicies {
            // there can only be open brackets at this point
            let bracket_token = &tokens[bracket_index as usize];
            let TokenKind::Bracket(bracket) = bracket_token.kind else {
                unreachable!("incorrect bracket index");
            };

            this.errors.push(Error {
                kind: ErrorKind::UnclosedBracket(bracket),
                col: bracket_token.col,
                pointers_count: 1,
            });
        }

        return if this.errors.is_empty() { Ok(tokens) } else { Err(this.errors) };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Utf8Error<'src> {
    grapheme: &'src str,
    col: offset32,
    pointers_count: offset32,
}

// TODO(stefano): own utf8 parsing
// iteration of characters
impl<'src> Tokenizer<'src> {
    fn token_len(&self) -> offset32 {
        return self.src.code[self.token_start_col as usize..self.col as usize].chars().count()
            as offset32;
    }

    // Note: this function is only called when parsing comments, so no special handling of graphemes is required
    fn next_utf8_char_multiline(&mut self) -> Option<utf8> {
        if self.col as usize >= self.src.code.len() {
            return None;
        }

        let next = self.src.code.as_bytes()[self.col as usize];
        return match next {
            b'\n' => {
                self.col += 1;
                self.line_index += 1;
                Some('\n')
            }
            ascii_ch @ 0..=b'\x7F' => {
                self.col += 1;
                Some(ascii_ch as utf8)
            }
            _utf8_ch => {
                let rest_of_code = &self.src.code[self.col as usize..];
                let Some(utf8_ch) = rest_of_code.chars().next() else {
                    unreachable!("this branch assured we would have a valid utf8 character");
                };

                self.col += utf8_ch.len_utf8() as offset32;
                Some(utf8_ch)
            }
        };
    }

    fn peek_next_ascii_char(&self) -> Result<Option<ascii>, Utf8Error<'src>> {
        if self.col as usize >= self.src.code.len() {
            return Ok(None);
        }

        let next = self.src.code.as_bytes()[self.col as usize];
        return match next {
            ascii_ch @ 0..=b'\x7F' => Ok(Some(ascii_ch)),
            _utf8_ch => {
                let rest_of_code = &self.src.code[self.col as usize..];
                let mut rest_of_line_graphemes = rest_of_code.graphemes(true);
                let Some(grapheme) = rest_of_line_graphemes.next() else {
                    unreachable!("this branch assured we would have a valid grapheme");
                };

                let mut pointers_count = 0;
                for character in grapheme.chars() {
                    let character_utf8_len = match character.width_cjk() {
                        Some(character_utf8_len) => character_utf8_len,
                        None => 1,
                    };
                    pointers_count += character_utf8_len as offset32;
                }

                if pointers_count == 0 {
                    pointers_count = 1;
                }

                // let Some(utf8_ch) = rest_of_line.chars().next() else {
                //     unreachable!();
                // };
                // let end_of_character = utf8_ch.len_utf8() as offset32;
                // let grapheme = &self.src.code[self.col as usize..(self.col + end_of_character) as usize];
                // let pointers_count = 1;

                Err(Utf8Error { grapheme, col: self.col, pointers_count })
            }
        };
    }

    // Note: this function is only called when parsing operators, so no special handling of graphemes is required
    fn peek_next_utf8_char(&self) -> Option<utf8> {
        if self.col as usize >= self.src.code.len() {
            return None;
        }

        let next = self.src.code.as_bytes()[self.col as usize];
        return match next {
            ascii_ch @ 0..=b'\x7F' => Some(ascii_ch as utf8),
            _utf8_ch => {
                let rest_of_code = &self.src.code[self.col as usize..];
                let Some(utf8_ch) = rest_of_code.chars().next() else {
                    unreachable!("this branch assured we would have a valid utf8 character");
                };

                Some(utf8_ch)
            }
        };
    }
}

impl<'src> Tokenizer<'src> {
    fn identifier(&mut self) -> Result<TokenKind<'src>, ()> {
        const MAX_IDENTIFIER_LEN: offset32 = 63;
        let previous_errors_len = self.errors.len();

        loop {
            match self.peek_next_ascii_char() {
                Ok(Some(b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_')) => {
                    self.col += 1;
                }
                Ok(Some(_) | None) => break,
                Err(error) => {
                    self.col += error.grapheme.len() as offset32;
                    self.errors.push(Error {
                        kind: ErrorKind::Utf8InIdentifier { grapheme: error.grapheme },
                        col: error.col,
                        pointers_count: error.pointers_count,
                    });
                }
            }
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
                if identifier_len as offset32 > MAX_IDENTIFIER_LEN {
                    self.errors.push(Error {
                        kind: ErrorKind::IdentifierTooLong { max: MAX_IDENTIFIER_LEN },
                        col: self.token_start_col,
                        pointers_count: identifier_len,
                    });
                }
                TokenKind::Identifier(identifier)
            }
        };

        return if previous_errors_len == self.errors.len() { Ok(identifier) } else { Err(()) };
    }

    fn integer_decimal(&mut self) -> Result<&'src [ascii], ()> {
        let previous_errors_len = self.errors.len();

        loop {
            match self.peek_next_ascii_char() {
                Ok(Some(b'0'..=b'9' | b'_')) => {
                    self.col += 1;
                }
                Ok(Some(letter @ (b'a'..=b'z' | b'A'..=b'Z'))) => {
                    self.col += 1;
                    self.errors.push(Error {
                        kind: ErrorKind::LetterInNumberLiteral(Base::Decimal, letter),
                        col: self.col,
                        pointers_count: 1,
                    });
                }
                Ok(Some(_) | None) => break,
                Err(error) => {
                    self.col += error.grapheme.len() as offset32;
                    self.errors.push(Error {
                        kind: ErrorKind::Utf8InNumberLiteral { grapheme: error.grapheme },
                        col: error.col,
                        pointers_count: error.pointers_count,
                    });
                }
            }
        }

        return if previous_errors_len == self.errors.len() {
            let digits = &self.src.code[self.token_start_col as usize..self.col as usize];
            Ok(digits.as_bytes())
        } else {
            Err(())
        };
    }

    fn integer_binary(&mut self) -> Result<&'src [ascii], ()> {
        let previous_errors_len = self.errors.len();

        loop {
            match self.peek_next_ascii_char() {
                Ok(Some(b'0'..=b'1' | b'_')) => {
                    self.col += 1;
                }
                Ok(Some(out_of_range @ b'2'..=b'9')) => {
                    self.col += 1;
                    self.errors.push(Error {
                        kind: ErrorKind::DigitOutOfRangeInNumberLiteral(Base::Binary, out_of_range),
                        col: self.col,
                        pointers_count: 1,
                    });
                }
                Ok(Some(letter @ (b'a'..=b'z' | b'A'..=b'Z'))) => {
                    self.col += 1;
                    self.errors.push(Error {
                        kind: ErrorKind::LetterInNumberLiteral(Base::Binary, letter),
                        col: self.col,
                        pointers_count: 1,
                    });
                }
                Ok(Some(_) | None) => break,
                Err(error) => {
                    self.col += error.grapheme.len() as offset32;
                    self.errors.push(Error {
                        kind: ErrorKind::Utf8InNumberLiteral { grapheme: error.grapheme },
                        col: error.col,
                        pointers_count: error.pointers_count,
                    });
                }
            }
        }

        return if previous_errors_len == self.errors.len() {
            // starting at self.token_start_col + 2 to account for the 0b prefix
            let digits = &self.src.code[self.token_start_col as usize + 2..self.col as usize];
            if digits.is_empty() {
                self.errors.push(Error {
                    kind: ErrorKind::EmptyNumberLiteral(MissingDigitsBase::Binary),
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
        let previous_errors_len = self.errors.len();

        loop {
            match self.peek_next_ascii_char() {
                Ok(Some(b'0'..=b'7' | b'_')) => {
                    self.col += 1;
                }
                Ok(Some(out_of_range @ b'8'..=b'9')) => {
                    self.col += 1;
                    self.errors.push(Error {
                        kind: ErrorKind::DigitOutOfRangeInNumberLiteral(Base::Octal, out_of_range),
                        col: self.col,
                        pointers_count: 1,
                    });
                }
                Ok(Some(letter @ (b'a'..=b'z' | b'A'..=b'Z'))) => {
                    self.col += 1;
                    self.errors.push(Error {
                        kind: ErrorKind::LetterInNumberLiteral(Base::Octal, letter),
                        col: self.col,
                        pointers_count: 1,
                    });
                }
                Ok(Some(_) | None) => break,
                Err(error) => {
                    self.col += error.grapheme.len() as offset32;
                    self.errors.push(Error {
                        kind: ErrorKind::Utf8InNumberLiteral { grapheme: error.grapheme },
                        col: error.col,
                        pointers_count: error.pointers_count,
                    });
                }
            }
        }

        return if previous_errors_len == self.errors.len() {
            // starting at self.token_start_col + 2 to account for the 0o prefix
            let digits = &self.src.code[self.token_start_col as usize + 2..self.col as usize];
            if digits.is_empty() {
                self.errors.push(Error {
                    kind: ErrorKind::EmptyNumberLiteral(MissingDigitsBase::Octal),
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
        let previous_errors_len = self.errors.len();

        loop {
            match self.peek_next_ascii_char() {
                Ok(Some(b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F' | b'_')) => {
                    self.col += 1;
                }
                Ok(Some(out_of_range @ (b'g'..=b'z' | b'G'..=b'Z'))) => {
                    self.col += 1;
                    self.errors.push(Error {
                        kind: ErrorKind::DigitOutOfRangeInNumberLiteral(
                            Base::Hexadecimal,
                            out_of_range,
                        ),
                        col: self.col,
                        pointers_count: 1,
                    });
                }
                Ok(Some(_) | None) => break,
                Err(error) => {
                    self.col += error.grapheme.len() as offset32;
                    self.errors.push(Error {
                        kind: ErrorKind::Utf8InNumberLiteral { grapheme: error.grapheme },
                        col: error.col,
                        pointers_count: error.pointers_count,
                    });
                }
            }
        }

        return if previous_errors_len == self.errors.len() {
            // starting at self.token_start_col + 2 to account for the 0x prefix
            let digits = &self.src.code[self.token_start_col as usize + 2..self.col as usize];
            if digits.is_empty() {
                self.errors.push(Error {
                    kind: ErrorKind::EmptyNumberLiteral(MissingDigitsBase::Hexadecimal),
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

    fn raw_string_literal(&mut self) -> Result<Vec<ascii>, ()> {
        let previous_errors_len = self.errors.len();

        let kind = QuotedLiteralKind::RawStr;
        let quote = kind.quote();
        let mut literal = Vec::<ascii>::new();

        loop {
            let next_character = match self.peek_next_ascii_char() {
                Ok(Some(b'\n') | None) => {
                    self.errors.push(Error {
                        kind: ErrorKind::UnclosedQuotedLiteral(kind),
                        col: self.token_start_col,
                        pointers_count: self.token_len(),
                    });
                    break;
                }
                Ok(Some(next_character)) => {
                    self.col += 1;
                    next_character
                }
                Err(error) => {
                    self.col += error.grapheme.len() as offset32;
                    self.errors.push(Error {
                        kind: ErrorKind::Utf8InIdentifier { grapheme: error.grapheme },
                        col: error.col,
                        pointers_count: error.pointers_count,
                    });
                    continue;
                }
            };

            let character = match next_character {
                b'\\' => {
                    let escape_character = match self.peek_next_ascii_char() {
                        Ok(Some(b'\n') | None) => {
                            self.errors.push(Error {
                                kind: ErrorKind::UnclosedQuotedLiteral(kind),
                                col: self.token_start_col,
                                pointers_count: self.token_len(),
                            });
                            break;
                        }
                        Ok(Some(escape_character)) => escape_character,
                        Err(error) => {
                            self.col += error.grapheme.len() as offset32;
                            self.errors.push(Error {
                                kind: ErrorKind::Utf8InQuotedLiteral {
                                    grapheme: error.grapheme,
                                    quoted_literal_kind: kind,
                                },
                                col: error.col,
                                pointers_count: error.pointers_count,
                            });
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
                Ok(Some(b'\n') | None) => {
                    self.errors.push(Error {
                        kind: ErrorKind::UnclosedQuotedLiteral(kind),
                        col: self.token_start_col,
                        pointers_count: self.token_len(),
                    });
                    break;
                }
                Ok(Some(next_character)) => {
                    self.col += 1;
                    next_character
                }
                Err(error) => {
                    self.col += error.grapheme.len() as offset32;
                    self.errors.push(Error {
                        kind: ErrorKind::Utf8InQuotedLiteral {
                            grapheme: error.grapheme,
                            quoted_literal_kind: kind,
                        },
                        col: error.col,
                        pointers_count: error.pointers_count,
                    });
                    continue;
                }
            };

            let character = match next_character {
                b'\\' => {
                    let escape_character = match self.peek_next_ascii_char() {
                        Ok(Some(b'\n') | None) => {
                            self.errors.push(Error {
                                kind: ErrorKind::UnclosedQuotedLiteral(kind),
                                col: self.token_start_col,
                                pointers_count: self.token_len(),
                            });
                            break;
                        }
                        Ok(Some(escape_character)) => {
                            self.col += 1;
                            escape_character
                        }
                        Err(error) => {
                            self.col += error.grapheme.len() as offset32;
                            self.errors.push(Error {
                                kind: ErrorKind::Utf8InQuotedLiteral {
                                    grapheme: error.grapheme,
                                    quoted_literal_kind: kind,
                                },
                                col: error.col,
                                pointers_count: error.pointers_count,
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
                            self.errors.push(Error {
                                kind: ErrorKind::UnrecognizedEscapeCharacterInQuotedLiteral(
                                    kind,
                                    unrecognized as utf8,
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
pub enum ErrorKind<'src> {
    UnclosedBlockComment,

    UnclosedBracket(Bracket),
    UnopenedBracket(Bracket),
    MismatchedBracket { actual: Bracket, expected: Bracket },

    Utf8InQuotedLiteral { grapheme: &'src str, quoted_literal_kind: QuotedLiteralKind },
    UnclosedQuotedLiteral(QuotedLiteralKind),
    ControlCharacterInQuotedLiteral(QuotedLiteralKind, utf8),
    UnrecognizedEscapeCharacterInQuotedLiteral(QuotedLiteralKind, utf8),
    EmptyCharacterLiteral,
    MultipleCharactersInCharacterLiteral,

    Utf8InNumberLiteral { grapheme: &'src str },
    LetterInNumberLiteral(Base, ascii),
    DigitOutOfRangeInNumberLiteral(Base, ascii),
    EmptyNumberLiteral(MissingDigitsBase),

    Utf8InIdentifier { grapheme: &'src str },
    IdentifierTooLong { max: offset32 },

    Utf8Character { grapheme: &'src str },
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
            Self::EmptyNumberLiteral(base) => (
                "invalid integer literal".into(),
                format!("at leasts one base {} digt must be present {:?}", *base as u8, base.range()).into(),
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
