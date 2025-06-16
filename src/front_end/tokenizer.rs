// TODO(stefano): more escape characters
// TODO(stefano): implement own escaping

use super::{
    src_file::{Line, SrcCode, SrcFile},
    Error, ErrorInfo, IntoErrorInfo,
};
use crate::error::CharsWidth as _;
use core::{fmt::Display, marker::PhantomData};
use back_to_front::offset32;
use unicode_segmentation::UnicodeSegmentation as _;

// TODO(stefano): move to primitives.rs
#[expect(non_camel_case_types, reason = "alias to a primitive type")]
/// kay's ascii character type
pub(crate) type ascii = u8;

// TODO(stefano): move to primitives.rs
#[expect(non_camel_case_types, reason = "alias to a primitive type")]
/// kay's utf32 character type
pub(crate) type utf32 = char;

// TODO(stefano): make pub(crate)
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
    pub const fn range(self) -> &'static [core::ops::RangeInclusive<utf32>] {
        return match self {
            Self::Decimal => &['0'..='9'],
            Self::Binary => &['0'..='1'],
            Self::Octal => &['0'..='7'],
            Self::Hexadecimal => &['0'..='9', 'A'..='F', 'a'..='f'],
        };
    }
}

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

pub(crate) type TextIndex = offset32;
pub(crate) type TokenIndex = offset32;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TokenKind {
    // IDEA(stefano): remove from the returned tokens, to avoid encountering them during the parsing stage
    Comment(TextIndex),
    BlockComment(TextIndex),
    Unexpected(TextIndex),

    // Symbols
    OpenRoundBracket,
    CloseRoundBracket,
    OpenSquareBracket,
    CloseSquareBracket,
    OpenCurlyBracket,
    CloseCurlyBracket,

    Colon,
    SemiColon,
    Comma,
    Op(Op),

    // Literal values
    // IDEA(stefano): represent as 0
    False,
    // IDEA(stefano): represent as non-0 instead of 1
    True,
    /// integer literals are never empty and always contain valid ascii digits
    DecimalInteger(TextIndex),
    BinaryInteger(TextIndex),
    OctalInteger(TextIndex),
    HexadecimalInteger(TextIndex),
    Ascii(ascii), // IDEA(stefano): make into `Ascii(TextIndex)`

    Str(TextIndex),
    RawStr(TextIndex),

    Identifier(TextIndex),
    IdentifierStr(TextIndex),

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
        #[expect(clippy::cast_possible_truncation)]
        return match self {
            Self::Comment(comment) => {
                let text = tokens.text[comment as usize];
                text.chars_width()
            }
            Self::BlockComment(comment) => {
                let text = tokens.text[comment as usize];
                text.chars_width()
            }
            Self::Unexpected(unexpected) => {
                let text = tokens.text[unexpected as usize];
                text.chars_width()
            }

            Self::OpenRoundBracket
            | Self::CloseRoundBracket
            | Self::OpenSquareBracket
            | Self::CloseSquareBracket
            | Self::OpenCurlyBracket
            | Self::CloseCurlyBracket => 1,

            Self::Colon => 1,
            Self::SemiColon => 1,
            Self::Comma => 1,
            Self::Op(op) => op.display_len(),

            Self::DecimalInteger(integer) => {
                let text = tokens.text[integer as usize];
                text.len() as offset32
            }
            Self::BinaryInteger(integer) => {
                let text = tokens.text[integer as usize];
                text.len() as offset32
            }
            Self::OctalInteger(integer) => {
                let text = tokens.text[integer as usize];
                text.len() as offset32
            }
            Self::HexadecimalInteger(integer) => {
                let text = tokens.text[integer as usize];
                text.len() as offset32
            }
            Self::Ascii(ascii_char) => {
                ascii_char.escape_ascii().len() as offset32 + 2 // + 2 for the quotes
            }
            Self::True => 4,
            Self::False => 5,
            Self::Str(string) => {
                let text = tokens.text[string as usize];
                text.len() as offset32
            }
            Self::RawStr(string) => {
                let text = tokens.text[string as usize];
                text.len() as offset32
            }

            Self::Identifier(identifier) => {
                let text = tokens.text[identifier as usize];
                text.len() as offset32
            }
            Self::IdentifierStr(identifier) => {
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

// IDEA(stefano): inline `SrcCode` into here
#[derive(Debug, Clone)]
pub struct Tokens<'code, 'path: 'code> {
    pub(crate) tokens: Vec<Token>,

    // IDEA(stefano): store a Range<offset32> instead
    pub(crate) text: Vec<&'code str>,

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
    // IDEA(stefano): accept a `Text` object (containing a string not longer that 4GB) instead
    pub fn tokenize(src_file: &'code SrcFile<'path>) -> TokenizedCode<'code, 'path> {
        let mut tokenizer = Tokenizer {
            code: &src_file.code,
            lines: Vec::new(),
            line_start: 0,

            col: 0,
            token_start_col: 0,
            tokens: Tokens { tokens: Vec::new(), text: Vec::new(), _src: PhantomData },

            errors: Vec::new(),
        };
        let mut brackets_indicies = Vec::<TokenIndex>::new();

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
                        #[expect(clippy::cast_possible_truncation)]
                        { tokenizer.col += error.grapheme.len() as offset32; }
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
                            tokenizer.raw_string_literal()
                        }
                        _ => tokenizer.identifier(),
                    },
                    b'a'..=b'z' | b'A'..=b'Z' | b'_' => tokenizer.identifier(),
                    b'`' => tokenizer.identifier_str(),
                    // IDEA(stefano): emit warning of inconsistent casing of letters, i.e. 0xFFff_fFfF_ffFF_ffFF
                    b'0' => match tokenizer.peek_next_utf8_char() {
                        Some('b') => {
                            tokenizer.col += 1;
                            match tokenizer.integer_binary() {
                                Ok(literal) => {
                                    tokenizer.tokens.text.push(literal);
                                    #[expect(clippy::cast_possible_truncation)]
                                    let literal_index =
                                        tokenizer.tokens.text.len() as TextIndex - 1;
                                    Ok(TokenKind::BinaryInteger(literal_index))
                                }
                                Err(()) => Err(()),
                            }
                        }
                        Some('o') => {
                            tokenizer.col += 1;
                            match tokenizer.integer_octal() {
                                Ok(literal) => {
                                    tokenizer.tokens.text.push(literal);
                                    #[expect(clippy::cast_possible_truncation)]
                                    let literal_index =
                                        tokenizer.tokens.text.len() as TextIndex - 1;
                                    Ok(TokenKind::OctalInteger(literal_index))
                                }
                                Err(()) => Err(()),
                            }
                        }
                        Some('x') => {
                            tokenizer.col += 1;
                            match tokenizer.integer_hexadecimal() {
                                Ok(literal) => {
                                    tokenizer.tokens.text.push(literal);
                                    #[expect(clippy::cast_possible_truncation)]
                                    let literal_index =
                                        tokenizer.tokens.text.len() as TextIndex - 1;
                                    Ok(TokenKind::HexadecimalInteger(literal_index))
                                }
                                Err(()) => Err(()),
                            }
                        }
                        Some(_) | None => match tokenizer.integer_decimal() {
                            Ok(literal) => {
                                tokenizer.tokens.text.push(literal);
                                #[expect(clippy::cast_possible_truncation)]
                                let literal_index = tokenizer.tokens.text.len() as TextIndex - 1;
                                Ok(TokenKind::DecimalInteger(literal_index))
                            }
                            Err(()) => Err(()),
                        },
                    },
                    b'1'..=b'9' => match tokenizer.integer_decimal() {
                        Ok(literal) => {
                            tokenizer.tokens.text.push(literal);
                            #[expect(clippy::cast_possible_truncation)]
                            let literal_index = tokenizer.tokens.text.len() as TextIndex - 1;
                            Ok(TokenKind::DecimalInteger(literal_index))
                        }
                        Err(()) => Err(()),
                    },
                    b'"' => tokenizer.string_literal(),
                    b'\'' => match tokenizer.ascii_literal() {
                        Ok(literal) => Ok(TokenKind::Ascii(literal)),
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

                            let comment_str = &tokenizer.code
                                [tokenizer.token_start_col as usize..tokenizer.col as usize];
                            tokenizer.tokens.text.push(comment_str);
                            #[expect(clippy::cast_possible_truncation)]
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
                                    #[expect(clippy::cast_possible_truncation)]
                                    Some(other) => tokenizer.col += other.len_utf8() as offset32,
                                }
                            }

                            let comment_str = &tokenizer.code
                                [tokenizer.token_start_col as usize..tokenizer.col as usize];
                            tokenizer.tokens.text.push(comment_str);
                            #[expect(clippy::cast_possible_truncation)]
                            let comment_index = tokenizer.tokens.text.len() as TextIndex - 1;
                            Ok(TokenKind::Comment(comment_index))
                        }
                    },
                    b'(' => {
                        #[expect(clippy::cast_possible_truncation)]
                        brackets_indicies.push(tokenizer.tokens.tokens.len() as TokenIndex);
                        Ok(TokenKind::OpenRoundBracket)
                    }
                    b')' => 'bracket: {
                        let Some(bracket_index) = brackets_indicies.pop() else {
                            tokenizer.errors.push(Error {
                                kind: ErrorKind::UnopenedRoundBracket,
                                col: tokenizer.token_start_col,
                                pointers_count: 1,
                            });
                            break 'bracket Err(());
                        };

                        #[expect(clippy::wildcard_enum_match_arm)]
                        match &tokenizer.tokens.tokens[bracket_index as usize].kind {
                            TokenKind::OpenRoundBracket
                            | TokenKind::CloseRoundBracket
                            | TokenKind::CloseCurlyBracket
                            | TokenKind::CloseSquareBracket => Ok(TokenKind::CloseRoundBracket),
                            TokenKind::OpenCurlyBracket => {
                                tokenizer.errors.push(Error {
                                    kind: ErrorKind::MismatchedCurlyRoundBracket,
                                    col: tokenizer.token_start_col,
                                    pointers_count: 1,
                                });
                                Err(())
                            }
                            TokenKind::OpenSquareBracket => {
                                tokenizer.errors.push(Error {
                                    kind: ErrorKind::MismatchedSquareRoundBracket,
                                    col: tokenizer.token_start_col,
                                    pointers_count: 1,
                                });
                                Err(())
                            }
                            _ => unreachable!("incorrect bracket index"),
                        }
                    }
                    b'[' => {
                        #[expect(clippy::cast_possible_truncation)]
                        brackets_indicies.push(tokenizer.tokens.tokens.len() as TokenIndex);
                        Ok(TokenKind::OpenSquareBracket)
                    }
                    b']' => 'bracket: {
                        let Some(bracket_index) = brackets_indicies.pop() else {
                            tokenizer.errors.push(Error {
                                kind: ErrorKind::UnopenedSquareBracket,
                                col: tokenizer.token_start_col,
                                pointers_count: 1,
                            });
                            break 'bracket Err(());
                        };

                        #[expect(clippy::wildcard_enum_match_arm)]
                        match &tokenizer.tokens.tokens[bracket_index as usize].kind {
                            TokenKind::OpenSquareBracket
                            | TokenKind::CloseSquareBracket
                            | TokenKind::CloseCurlyBracket
                            | TokenKind::CloseRoundBracket => Ok(TokenKind::CloseSquareBracket),
                            TokenKind::OpenCurlyBracket => {
                                tokenizer.errors.push(Error {
                                    kind: ErrorKind::MismatchedCurlySquareBracket,
                                    col: tokenizer.token_start_col,
                                    pointers_count: 1,
                                });
                                Err(())
                            }
                            TokenKind::OpenRoundBracket => {
                                tokenizer.errors.push(Error {
                                    kind: ErrorKind::MismatchedRoundSquareBracket,
                                    col: tokenizer.token_start_col,
                                    pointers_count: 1,
                                });
                                Err(())
                            }
                            _ => unreachable!("incorrect bracket index"),
                        }
                    }
                    b'{' => {
                        #[expect(clippy::cast_possible_truncation)]
                        brackets_indicies.push(tokenizer.tokens.tokens.len() as TokenIndex);
                        Ok(TokenKind::OpenCurlyBracket)
                    }
                    b'}' => 'bracket: {
                        let Some(bracket_index) = brackets_indicies.pop() else {
                            tokenizer.errors.push(Error {
                                kind: ErrorKind::UnopenedCurlyBracket,
                                col: tokenizer.token_start_col,
                                pointers_count: 1,
                            });
                            break 'bracket Err(());
                        };

                        #[expect(clippy::wildcard_enum_match_arm)]
                        match &tokenizer.tokens.tokens[bracket_index as usize].kind {
                            TokenKind::OpenCurlyBracket
                            | TokenKind::CloseCurlyBracket
                            | TokenKind::CloseRoundBracket
                            | TokenKind::CloseSquareBracket => Ok(TokenKind::CloseCurlyBracket),
                            TokenKind::OpenRoundBracket => {
                                tokenizer.errors.push(Error {
                                    kind: ErrorKind::MismatchedRoundCurlyBracket,
                                    col: tokenizer.token_start_col,
                                    pointers_count: 1,
                                });
                                Err(())
                            }
                            TokenKind::OpenSquareBracket => {
                                tokenizer.errors.push(Error {
                                    kind: ErrorKind::MismatchedSquareCurlyBracket,
                                    col: tokenizer.token_start_col,
                                    pointers_count: 1,
                                });
                                Err(())
                            }
                            _ => unreachable!("incorrect bracket index"),
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
                            kind: ErrorKind::UnrecognizedCharacter(unrecognized as utf32),
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
                    #[expect(clippy::cast_possible_truncation)]
                    let unexpected_index = tokenizer.tokens.text.len() as TextIndex - 1;
                    TokenKind::Unexpected(unexpected_index)
                }
            };

            tokenizer.tokens.tokens.push(Token { kind, col: tokenizer.token_start_col });
        }

        if let Some(b'\n') = tokenizer.code.as_bytes().last() {
        } else {
            tokenizer.lines.push(Line { start: tokenizer.line_start, end: tokenizer.col });
        }

        for bracket_index in brackets_indicies {
            // there can only be open brackets at this point
            let bracket_token = &tokenizer.tokens.tokens[bracket_index as usize];

            #[expect(clippy::wildcard_enum_match_arm)]
            let error_kind = match bracket_token.kind {
                TokenKind::OpenRoundBracket => ErrorKind::UnclosedRoundBracket,
                TokenKind::OpenSquareBracket => ErrorKind::UnclosedSquareBracket,
                TokenKind::OpenCurlyBracket => ErrorKind::UnclosedCurlyBracket,
                _ => unreachable!("incorrect bracket index"),
            };

            tokenizer.errors.push(Error {
                kind: error_kind,
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
    fn next_utf8_char_multiline(&mut self) -> Option<utf32> {
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
                Some(ascii_ch as utf32)
            }
            _utf8_ch => {
                let rest_of_code = &self.code[self.col as usize..];
                let Some(utf8_ch) = rest_of_code.chars().next() else {
                    unreachable!("this branch assured we would have a valid utf8 character");
                };
                #[expect(clippy::cast_possible_truncation)]
                { self.col += utf8_ch.len_utf8() as offset32; }
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
    fn peek_next_utf8_char(&self) -> Option<utf32> {
        if self.col as usize >= self.code.len() {
            return None;
        }

        let next = self.code.as_bytes()[self.col as usize];
        return match next {
            ascii_ch @ 0..=b'\x7F' => Some(ascii_ch as utf32),
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

// IDEA(stefano): report unclosed escape character, i.e.: "\
impl<'code> Tokenizer<'code, '_> {
    const MAX_IDENTIFIER_LEN: offset32 = 63;

    fn identifier(&mut self) -> Result<TokenKind, ()> {
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
                    #[expect(clippy::cast_possible_truncation)]
                    { self.col += error.grapheme.len() as offset32; }
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
                #[expect(clippy::cast_possible_truncation)]
                let identifier_len = identifier.len() as offset32;
                if identifier_len as offset32 > Self::MAX_IDENTIFIER_LEN {
                    self.errors.push(Error {
                        kind: ErrorKind::IdentifierTooLong { max: Self::MAX_IDENTIFIER_LEN },
                        col: self.token_start_col,
                        pointers_count: identifier_len,
                    });
                    return Err(());
                }

                self.tokens.text.push(identifier);
                #[expect(clippy::cast_possible_truncation)]
                let identifier_index = self.tokens.text.len() as TextIndex - 1;
                TokenKind::Identifier(identifier_index)
            }
        };

        return Ok(identifier);
    }

    // IDEA(stefano): allow for escaped \`
    fn identifier_str(&mut self) -> Result<TokenKind, ()> {
        let previous_errors_len = self.errors.len();

        loop {
            let next_character = match self.peek_next_ascii_char() {
                Some(Ok(b'\n')) | None => {
                    self.errors.push(Error {
                        kind: ErrorKind::UnclosedIdentifierStr,
                        col: self.token_start_col,
                        pointers_count: self.token_text().chars_width(),
                    });
                    break;
                }
                Some(Ok(next_character)) => next_character,
                Some(Err(error)) => {
                    self.errors.push(Error {
                        kind: ErrorKind::Utf8InIdentifierStr { grapheme: error.grapheme },
                        col: error.col,
                        pointers_count: error.pointers_count,
                    });
                    #[expect(clippy::cast_possible_truncation)]
                    { self.col += error.grapheme.len() as offset32; }
                    continue;
                }
            };
            self.col += 1;

            let _character = match next_character {
                control @ (b'\x00'..=b'\x1F' | b'\x7F') => {
                    self.errors.push(Error {
                        kind: ErrorKind::ControlCharacterInIdentifierStr(control as utf32),
                        col: self.col - 1,
                        pointers_count: 1,
                    });
                    control
                }
                b'`' => break,
                ch => ch,
            };
        }

        if previous_errors_len != self.errors.len() {
            return Err(());
        };

        let identifier_str = &self.code[self.token_start_col as usize..self.col as usize];
        let identifier = {
            #[expect(clippy::cast_possible_truncation)]
            let identifier_len = identifier_str.len() as offset32 - 2; // - 2 for the quotes
            if identifier_len as offset32 > Self::MAX_IDENTIFIER_LEN {
                self.errors.push(Error {
                    kind: ErrorKind::IdentifierStrTooLong { max: Self::MAX_IDENTIFIER_LEN },
                    col: self.token_start_col,
                    pointers_count: identifier_len,
                });
                return Err(());
            }

            self.tokens.text.push(identifier_str);
            #[expect(clippy::cast_possible_truncation)]
            let identifier_index = self.tokens.text.len() as TextIndex - 1;
            TokenKind::IdentifierStr(identifier_index)
        };

        return Ok(identifier);
    }

    fn integer_decimal(&mut self) -> Result<&'code str, ()> {
        let previous_errors_len = self.errors.len();

        loop {
            match self.peek_next_ascii_char() {
                Some(Ok(b'0'..=b'9' | b'_')) => {
                    self.col += 1;
                }
                Some(Ok(letter @ (b'a'..=b'z' | b'A'..=b'Z'))) => {
                    self.errors.push(Error {
                        kind: ErrorKind::LetterInDecimalNumberLiteral(letter),
                        col: self.col,
                        pointers_count: 1,
                    });
                    self.col += 1;
                }
                Some(Err(error)) => {
                    self.errors.push(Error {
                        kind: ErrorKind::Utf8InDecimalNumberLiteral { grapheme: error.grapheme },
                        col: error.col,
                        pointers_count: error.pointers_count,
                    });
                    #[expect(clippy::cast_possible_truncation)]
                    { self.col += error.grapheme.len() as offset32; }
                }
                Some(Ok(_)) | None => break,
            }
        }

        return if previous_errors_len == self.errors.len() {
            let digits = &self.code[self.token_start_col as usize..self.col as usize];
            Ok(digits)
        } else {
            Err(())
        };
    }

    fn integer_binary(&mut self) -> Result<&'code str, ()> {
        let previous_errors_len = self.errors.len();

        loop {
            match self.peek_next_ascii_char() {
                Some(Ok(b'0'..=b'1' | b'_')) => {
                    self.col += 1;
                }
                Some(Ok(out_of_range @ b'2'..=b'9')) => {
                    self.errors.push(Error {
                        kind: ErrorKind::DigitOutOfRangeInBinaryNumberLiteral(out_of_range),
                        col: self.col,
                        pointers_count: 1,
                    });
                    self.col += 1;
                }
                Some(Ok(letter @ (b'a'..=b'z' | b'A'..=b'Z'))) => {
                    self.errors.push(Error {
                        kind: ErrorKind::LetterInBinaryNumberLiteral(letter),
                        col: self.col,
                        pointers_count: 1,
                    });
                    self.col += 1;
                }
                Some(Err(error)) => {
                    self.errors.push(Error {
                        kind: ErrorKind::Utf8InBinaryNumberLiteral { grapheme: error.grapheme },
                        col: error.col,
                        pointers_count: error.pointers_count,
                    });
                    #[expect(clippy::cast_possible_truncation)]
                    { self.col += error.grapheme.len() as offset32; }
                }
                Some(Ok(_)) | None => break,
            }
        }

        return if previous_errors_len == self.errors.len() {
            let digits = &self.code[self.token_start_col as usize..self.col as usize];
            Ok(digits)
        } else {
            Err(())
        };
    }

    fn integer_octal(&mut self) -> Result<&'code str, ()> {
        let previous_errors_len = self.errors.len();

        loop {
            match self.peek_next_ascii_char() {
                Some(Ok(b'0'..=b'7' | b'_')) => {
                    self.col += 1;
                }
                Some(Ok(out_of_range @ b'8'..=b'9')) => {
                    self.errors.push(Error {
                        kind: ErrorKind::DigitOutOfRangeInOctalNumberLiteral(out_of_range),
                        col: self.col,
                        pointers_count: 1,
                    });
                    self.col += 1;
                }
                Some(Ok(letter @ (b'a'..=b'z' | b'A'..=b'Z'))) => {
                    self.errors.push(Error {
                        kind: ErrorKind::LetterInOctalNumberLiteral(letter),
                        col: self.col,
                        pointers_count: 1,
                    });
                    self.col += 1;
                }
                Some(Err(error)) => {
                    self.errors.push(Error {
                        kind: ErrorKind::Utf8InOctalNumberLiteral { grapheme: error.grapheme },
                        col: error.col,
                        pointers_count: error.pointers_count,
                    });
                    #[expect(clippy::cast_possible_truncation)]
                    { self.col += error.grapheme.len() as offset32; }
                }
                Some(Ok(_)) | None => break,
            }
        }

        return if previous_errors_len == self.errors.len() {
            let digits = &self.code[self.token_start_col as usize..self.col as usize];
            Ok(digits)
        } else {
            Err(())
        };
    }

    fn integer_hexadecimal(&mut self) -> Result<&'code str, ()> {
        let previous_errors_len = self.errors.len();

        loop {
            match self.peek_next_ascii_char() {
                Some(Ok(b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F' | b'_')) => {
                    self.col += 1;
                }
                Some(Ok(out_of_range @ (b'g'..=b'z' | b'G'..=b'Z'))) => {
                    self.errors.push(Error {
                        kind: ErrorKind::DigitOutOfRangeInHexadecimalNumberLiteral(out_of_range),
                        col: self.col,
                        pointers_count: 1,
                    });
                    self.col += 1;
                }
                Some(Err(error)) => {
                    self.errors.push(Error {
                        kind: ErrorKind::Utf8InHexadecimalNumberLiteral {
                            grapheme: error.grapheme,
                        },
                        col: error.col,
                        pointers_count: error.pointers_count,
                    });
                    #[expect(clippy::cast_possible_truncation)]
                    { self.col += error.grapheme.len() as offset32; }
                }
                Some(Ok(_)) | None => break,
            }
        }

        return if previous_errors_len == self.errors.len() {
            let digits = &self.code[self.token_start_col as usize..self.col as usize];
            Ok(digits)
        } else {
            Err(())
        };
    }

    fn ascii_literal(&mut self) -> Result<ascii, ()> {
        let previous_errors_len = self.errors.len();

        // REMOVE(stefano): parse similar to strings
        let mut literal = Vec::<ascii>::new();
        loop {
            let next_character = match self.peek_next_ascii_char() {
                Some(Ok(b'\n')) | None => {
                    self.errors.push(Error {
                        kind: ErrorKind::UnclosedCharacterLiteral,
                        col: self.token_start_col,
                        pointers_count: self.token_text().chars_width(),
                    });
                    break;
                }
                Some(Ok(next_character)) => next_character,
                Some(Err(error)) => {
                    self.errors.push(Error {
                        kind: ErrorKind::Utf8InCharacterLiteral { grapheme: error.grapheme },
                        col: error.col,
                        pointers_count: error.pointers_count,
                    });
                    #[expect(clippy::cast_possible_truncation)]
                    { self.col += error.grapheme.len() as offset32; }
                    continue;
                }
            };
            self.col += 1;

            let character = match next_character {
                b'\\' => {
                    let escape_character = match self.peek_next_ascii_char() {
                        Some(Ok(b'\n')) | None => {
                            self.errors.push(Error {
                                kind: ErrorKind::UnclosedCharacterLiteral,
                                col: self.token_start_col,
                                pointers_count: self.token_text().chars_width(),
                            });
                            break;
                        }
                        Some(Ok(escape_character)) => escape_character,
                        Some(Err(error)) => {
                            self.errors.push(Error {
                                kind: ErrorKind::Utf8InCharacterLiteral {
                                    grapheme: error.grapheme,
                                },
                                col: error.col,
                                pointers_count: error.pointers_count,
                            });
                            #[expect(clippy::cast_possible_truncation)]
                            { self.col += error.grapheme.len() as offset32; }
                            continue;
                        }
                    };
                    self.col += 1;

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
                                kind: ErrorKind::UnrecognizedEscapeCharacterInCharacterLiteral(
                                    unrecognized as utf32,
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
                        kind: ErrorKind::ControlCharacterInCharacterLiteral(control as utf32),
                        col: self.col - 1,
                        pointers_count: 1,
                    });
                    control
                }
                b'\'' => break,
                ch => ch,
            };

            literal.push(character);
        }

        return match literal.as_slice() {
            [character] => {
                if previous_errors_len == self.errors.len() {
                    Ok(*character)
                } else {
                    Err(())
                }
            }
            [] => {
                self.errors.push(Error {
                    kind: ErrorKind::EmptyCharacterLiteral,
                    col: self.token_start_col,
                    pointers_count: 2,
                });
                Err(())
            }
            [_, ..] => {
                self.errors.push(Error {
                    kind: ErrorKind::MultipleCharactersInCharacterLiteral,
                    col: self.token_start_col,
                    pointers_count: self.token_text().chars_width(),
                });
                Err(())
            }
        };
    }

    fn string_literal(&mut self) -> Result<TokenKind, ()> {
        let previous_errors_len = self.errors.len();

        loop {
            let next_character = match self.peek_next_ascii_char() {
                Some(Ok(b'\n')) | None => {
                    self.errors.push(Error {
                        kind: ErrorKind::UnclosedStrLiteral,
                        col: self.token_start_col,
                        pointers_count: self.token_text().chars_width(),
                    });
                    break;
                }
                Some(Ok(next_character)) => next_character,
                Some(Err(error)) => {
                    self.errors.push(Error {
                        kind: ErrorKind::Utf8InStrLiteral { grapheme: error.grapheme },
                        col: error.col,
                        pointers_count: error.pointers_count,
                    });
                    #[expect(clippy::cast_possible_truncation)]
                    { self.col += error.grapheme.len() as offset32; }
                    continue;
                }
            };
            self.col += 1;

            let _character = match next_character {
                b'\\' => {
                    let escape_character = match self.peek_next_ascii_char() {
                        Some(Ok(b'\n')) | None => {
                            self.errors.push(Error {
                                kind: ErrorKind::UnclosedStrLiteral,
                                col: self.token_start_col,
                                pointers_count: self.token_text().chars_width(),
                            });
                            break;
                        }
                        Some(Ok(escape_character)) => escape_character,
                        Some(Err(error)) => {
                            self.errors.push(Error {
                                kind: ErrorKind::Utf8InStrLiteral { grapheme: error.grapheme },
                                col: error.col,
                                pointers_count: error.pointers_count,
                            });
                            #[expect(clippy::cast_possible_truncation)]
                            { self.col += error.grapheme.len() as offset32; }
                            continue;
                        }
                    };
                    self.col += 1;

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
                                kind: ErrorKind::UnrecognizedEscapeCharacterInStrLiteral(
                                    unrecognized as utf32,
                                ),
                                col: self.col - 2,
                                pointers_count: 2,
                            });
                            unrecognized
                        }
                    }
                }
                control @ (b'\x00'..=b'\x1F' | b'\x7F') => {
                    self.errors.push(Error {
                        kind: ErrorKind::ControlCharacterInStrLiteral(control as utf32),
                        col: self.col - 1,
                        pointers_count: 1,
                    });
                    control
                }
                b'"' => break,
                ch => ch,
            };
        }

        if previous_errors_len != self.errors.len() {
            return Err(());
        };

        let raw_string_literal_str = &self.code[self.token_start_col as usize..self.col as usize];
        self.tokens.text.push(raw_string_literal_str);
        #[expect(clippy::cast_possible_truncation)]
        let raw_string_literal = self.tokens.text.len() as TextIndex - 1;
        let raw_string = TokenKind::Str(raw_string_literal);
        return Ok(raw_string);
    }

    fn raw_string_literal(&mut self) -> Result<TokenKind, ()> {
        let previous_errors_len = self.errors.len();

        loop {
            let next_character = match self.peek_next_ascii_char() {
                Some(Ok(b'\n')) | None => {
                    self.errors.push(Error {
                        kind: ErrorKind::UnclosedRawStrLiteral,
                        col: self.token_start_col,
                        pointers_count: self.token_text().chars_width(),
                    });
                    break;
                }
                Some(Ok(next_character)) => next_character,
                Some(Err(error)) => {
                    self.errors.push(Error {
                        kind: ErrorKind::Utf8InRawStrLiteral { grapheme: error.grapheme },
                        col: error.col,
                        pointers_count: error.pointers_count,
                    });
                    #[expect(clippy::cast_possible_truncation)]
                    { self.col += error.grapheme.len() as offset32; }
                    continue;
                }
            };
            self.col += 1;

            let _character = match next_character {
                b'\\' => {
                    let escape_character = match self.peek_next_ascii_char() {
                        Some(Ok(b'\n')) | None => {
                            self.errors.push(Error {
                                kind: ErrorKind::UnclosedRawStrLiteral,
                                col: self.token_start_col,
                                pointers_count: self.token_text().chars_width(),
                            });
                            break;
                        }
                        Some(Ok(escape_character)) => escape_character,
                        Some(Err(error)) => {
                            self.errors.push(Error {
                                kind: ErrorKind::Utf8InRawStrLiteral { grapheme: error.grapheme },
                                col: error.col,
                                pointers_count: error.pointers_count,
                            });
                            #[expect(clippy::cast_possible_truncation)]
                            { self.col += error.grapheme.len() as offset32; }
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
                        kind: ErrorKind::ControlCharacterInRawStrLiteral(control as utf32),
                        col: self.col - 1,
                        pointers_count: 1,
                    });
                    control
                }
                b'"' => break,
                ch => ch,
            };
        }

        if previous_errors_len != self.errors.len() {
            return Err(());
        }

        let raw_string_literal_str = &self.code[self.token_start_col as usize..self.col as usize];
        self.tokens.text.push(raw_string_literal_str);
        #[expect(clippy::cast_possible_truncation)]
        let raw_string_literal = self.tokens.text.len() as TextIndex - 1;
        let raw_string = TokenKind::RawStr(raw_string_literal);
        return Ok(raw_string);
    }
}

#[derive(Debug, Clone)]
pub enum ErrorKind<'code> {
    UnclosedBlockComment,

    UnclosedRoundBracket,
    UnclosedSquareBracket,
    UnclosedCurlyBracket,
    UnopenedRoundBracket,
    UnopenedSquareBracket,
    UnopenedCurlyBracket,
    MismatchedRoundCurlyBracket,
    MismatchedRoundSquareBracket,
    MismatchedSquareRoundBracket,
    MismatchedSquareCurlyBracket,
    MismatchedCurlyRoundBracket,
    MismatchedCurlySquareBracket,

    Utf8InCharacterLiteral { grapheme: &'code str },
    Utf8InStrLiteral { grapheme: &'code str },
    Utf8InRawStrLiteral { grapheme: &'code str },
    UnclosedCharacterLiteral,
    UnclosedStrLiteral,
    UnclosedRawStrLiteral,
    ControlCharacterInCharacterLiteral(utf32),
    ControlCharacterInStrLiteral(utf32),
    ControlCharacterInRawStrLiteral(utf32),
    UnrecognizedEscapeCharacterInCharacterLiteral(utf32),
    UnrecognizedEscapeCharacterInStrLiteral(utf32),
    UnrecognizedEscapeCharacterInRawStrLiteral(utf32),
    EmptyCharacterLiteral,
    MultipleCharactersInCharacterLiteral,

    Utf8InDecimalNumberLiteral { grapheme: &'code str },
    Utf8InBinaryNumberLiteral { grapheme: &'code str },
    Utf8InOctalNumberLiteral { grapheme: &'code str },
    Utf8InHexadecimalNumberLiteral { grapheme: &'code str },
    LetterInDecimalNumberLiteral(ascii),
    LetterInBinaryNumberLiteral(ascii),
    LetterInOctalNumberLiteral(ascii),
    DigitOutOfRangeInBinaryNumberLiteral(ascii),
    DigitOutOfRangeInOctalNumberLiteral(ascii),
    DigitOutOfRangeInHexadecimalNumberLiteral(ascii),

    Utf8InIdentifier { grapheme: &'code str },
    Utf8InIdentifierStr { grapheme: &'code str },
    IdentifierTooLong { max: offset32 },
    IdentifierStrTooLong { max: offset32 },
    UnclosedIdentifierStr,
    ControlCharacterInIdentifierStr(utf32),

    Utf8Character { grapheme: &'code str },
    UnrecognizedCharacter(utf32),
}

impl IntoErrorInfo for ErrorKind<'_> {
    fn info(&self) -> ErrorInfo {
        let (error_message, error_cause_message) = match self {
            Self::UnclosedBlockComment => (
                "unclosed block comment".into(),
                "missing closing `##`".into(),
            ),

            Self::UnclosedRoundBracket => (
                "unclosed '(' bracket".into(),
                "was not closed".into(),
            ),
            Self::UnclosedSquareBracket => (
                "unclosed '[' bracket".into(),
                "was not closed".into(),
            ),
            Self::UnclosedCurlyBracket => (
                "unclosed '{' bracket".into(),
                "was not closed".into(),
            ),
            Self::UnopenedRoundBracket => (
                "unopened ')' bracket".into(),
                "was not opened before".into(),
            ),
            Self::UnopenedSquareBracket => (
                "unopened ']' bracket".into(),
                "was not opened before".into(),
            ),
            Self::UnopenedCurlyBracket => (
                "unopened '}' bracket".into(),
                "was not opened before".into(),
            ),
            Self::MismatchedRoundSquareBracket => (
                "mismatched bracket".into(),
                "']' closes the wrong bracket, expected a ')' instead".into()
            ),
            Self::MismatchedRoundCurlyBracket => (
                "mismatched bracket".into(),
                "'}' closes the wrong bracket, expected a ')' instead".into()
            ),
            Self::MismatchedSquareRoundBracket => (
                "mismatched bracket".into(),
                "')' closes the wrong bracket, expected a ']' instead".into()
            ),
            Self::MismatchedSquareCurlyBracket => (
                "mismatched bracket".into(),
                "'}' closes the wrong bracket, expected a ']' instead".into()
            ),
            Self::MismatchedCurlyRoundBracket => (
                "mismatched bracket".into(),
                "')' closes the wrong bracket, expected a '}' instead".into()
            ),
            Self::MismatchedCurlySquareBracket => (
                "mismatched bracket".into(),
                "']' closes the wrong bracket, expected a '}' instead".into()
            ),

            Self::Utf8InCharacterLiteral { grapheme } => (
                format!("invalid character literal character '{grapheme}' {}", grapheme.escape_unicode()).into(),
                "utf8 characters are not allowed".into(),
            ),
            Self::Utf8InStrLiteral { grapheme } => (
                format!("invalid string literal character '{grapheme}' {}", grapheme.escape_unicode()).into(),
                "utf8 characters are not allowed".into(),
            ),
            Self::Utf8InRawStrLiteral { grapheme } => (
                format!("invalid raw string literal character '{grapheme}' {}", grapheme.escape_unicode()).into(),
                "utf8 characters are not allowed".into(),
            ),
            Self::UnclosedCharacterLiteral => (
                "unclosed character literal".into(),
                "missing closing ' quote".into()
            ),
            Self::UnclosedStrLiteral => (
                "unclosed string literal".into(),
                "missing closing \" quote".into()
            ),
            Self::UnclosedRawStrLiteral => (
                "unclosed raw string literal".into(),
                "missing closing \" quote".into()
            ),
            Self::ControlCharacterInCharacterLiteral(control_character) => (
                format!("invalid character literal character '{}' {}", control_character.escape_debug(), control_character.escape_unicode()).into(),
                "control characters are not allowed".into(),
            ),
            Self::ControlCharacterInStrLiteral(control_character) => (
                format!("invalid string literal character '{}' {}", control_character.escape_debug(), control_character.escape_unicode()).into(),
                "control characters are not allowed".into(),
            ),
            Self::ControlCharacterInRawStrLiteral(control_character) => (
                format!("invalid raw string literal character '{}' {}", control_character.escape_debug(), control_character.escape_unicode()).into(),
                "control characters are not allowed".into(),
            ),
            Self::UnrecognizedEscapeCharacterInCharacterLiteral(unrecognized) => (
                "invalid character literal".into(),
                format!("unrecognized '{unrecognized}' escape character").into(),
            ),
            Self::UnrecognizedEscapeCharacterInStrLiteral(unrecognized) => (
                "invalid string literal".into(),
                format!("unrecognized '{unrecognized}' escape character").into(),
            ),
            Self::UnrecognizedEscapeCharacterInRawStrLiteral(unrecognized) => (
                "invalid raw string literal".into(),
                format!("unrecognized '{unrecognized}' escape character").into(),
            ),
            Self::EmptyCharacterLiteral => (
                "empty character literal".into(),
                "must not be empty".into(),
            ),
            Self::MultipleCharactersInCharacterLiteral => (
                "invalid character literal".into(),
                "must not contain more than one character, if you meant to write a string literal try changing the quotes to \"".into(),
            ),

            Self::Utf8InDecimalNumberLiteral { grapheme } => (
                format!("invalid decimal integer literal character '{grapheme}' {}", grapheme.escape_unicode()).into(),
                "utf8 characters are not allowed".into(),
            ),
            Self::Utf8InBinaryNumberLiteral { grapheme } => (
                format!("invalid binary integer literal character '{grapheme}' {}", grapheme.escape_unicode()).into(),
                "utf8 characters are not allowed".into(),
            ),
            Self::Utf8InOctalNumberLiteral { grapheme } => (
                format!("invalid octal integer literal character '{grapheme}' {}", grapheme.escape_unicode()).into(),
                "utf8 characters are not allowed".into(),
            ),
            Self::Utf8InHexadecimalNumberLiteral { grapheme } => (
                format!("invalid hexadecimal integer literal character '{grapheme}' {}", grapheme.escape_unicode()).into(),
                "utf8 characters are not allowed".into(),
            ),
            Self::LetterInDecimalNumberLiteral(letter) => (
                format!("invalid integer literal letter '{}'", *letter as utf32).into(),
                format!("not allowed in a base {} number", Base::Decimal as u8).into(),
            ),
            Self::LetterInBinaryNumberLiteral(letter) => (
                format!("invalid integer literal letter '{}'", *letter as utf32).into(),
                format!("not allowed in a base {} number", Base::Binary as u8).into(),
            ),
            Self::LetterInOctalNumberLiteral(letter) => (
                format!("invalid integer literal letter '{}'", *letter as utf32).into(),
                format!("not allowed in a base {} number", Base::Octal as u8).into(),
            ),
            Self::DigitOutOfRangeInBinaryNumberLiteral(digit) => {
                const BASE: Base = Base::Binary;
                (
                    format!("invalid integer literal digit '{}'", *digit as utf32).into(),
                    format!("out of the valid range for a base {} number {:?}", BASE as u8, BASE.range()).into(),
                )
            }
            Self::DigitOutOfRangeInOctalNumberLiteral(digit) => {
                const BASE: Base = Base::Octal;
                (
                    format!("invalid integer literal digit '{}'", *digit as utf32).into(),
                    format!("out of the valid range for a base {} number {:?}", BASE as u8, BASE.range()).into(),
                )
            }
            Self::DigitOutOfRangeInHexadecimalNumberLiteral(digit) => {
                const BASE: Base = Base::Hexadecimal;
                (
                    format!("invalid integer literal digit '{}'", *digit as utf32).into(),
                    format!("out of the valid range for a base {} number {:?}", BASE as u8, BASE.range()).into(),
                )
            }

            Self::Utf8InIdentifier { grapheme } => (
                format!("invalid identifier character '{grapheme}' {}", grapheme.escape_unicode()).into(),
                "utf8 characters are not allowed".into(),
            ),
            Self::Utf8InIdentifierStr { grapheme } => (
                format!("invalid identifier string character '{grapheme}' {}", grapheme.escape_unicode()).into(),
                "utf8 characters are not allowed".into(),
            ),
            Self::IdentifierTooLong { max } => (
                "invalid identifier".into(),
                format!("exceeds the length limit of {max}").into(),
            ),
            Self::IdentifierStrTooLong { max } => (
                "invalid identifier string".into(),
                format!("exceeds the length limit of {max} characters bewteen quotes").into(),
            ),
            Self::UnclosedIdentifierStr => (
                "unclosed identifier string".into(),
                "missing closing ` quote".into()
            ),
            Self::ControlCharacterInIdentifierStr(control_character) => (
                format!("invalid identifier string character '{}' {}", control_character.escape_debug(), control_character.escape_unicode()).into(),
                "control characters are not allowed".into(),
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
