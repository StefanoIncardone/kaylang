// TODO(stefano): implement own escaping

use super::{
    src_file::{Line, SrcCode, SrcFile},
    IntoMsgInfo, Msg, MsgInfo,
};
use crate::{
    error::DisplayLen as _,
    front_end::{MsgSeverity, SliceIndexPtr},
};
use back_to_front::{digit::{self, AsciiDigit}, offset32};
use core::{fmt::Display, ops::RangeInclusive};
use unicode_segmentation::UnicodeSegmentation as _;

// TODO(stefano): move to primitives.rs
#[expect(non_camel_case_types, reason = "alias to a primitive type")]
/// kay's ascii character type
pub(crate) type ascii = u8;

// TODO(stefano): move to primitives.rs
#[expect(non_camel_case_types, reason = "alias to a primitive type")]
/// kay's utf32 character type
pub(crate) type utf32 = char;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum Op {
    Equals,

    /// temporary way of getting the length of strings and arrays
    Len,
    Not,
    NotEquals,

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
    NotEqualsEquals,
    Greater,
    GreaterOrEquals,
    Less,
    LessOrEquals,
}

impl Display for Op {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        #[rustfmt::skip]
        return match self {
            Self::Equals                    => write!(f, "="),

            Self::Len                       => write!(f, "len"),
            Self::Not                       => write!(f, "!"),
            Self::NotEquals                 => write!(f, "!="),

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
            Self::NotEqualsEquals           => write!(f, "!=="),
            Self::Greater                   => write!(f, ">"),
            Self::GreaterOrEquals           => write!(f, ">="),
            Self::Less                      => write!(f, "<"),
            Self::LessOrEquals              => write!(f, "<="),
        };
    }
}

impl Op {
    pub(crate) const fn display_len(self) -> offset32 {
        return match self {
            Self::Len => 3,
            Self::Equals => 1,
            Self::Not => 1,
            Self::NotEquals => 2,

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
            Self::NotEqualsEquals => 3,
            Self::Greater => 1,
            Self::GreaterOrEquals => 2,
            Self::Less => 1,
            Self::LessOrEquals => 2,
            Self::Compare => 3,
        };
    }
}

pub(crate) type TextIndex<'code> = SliceIndexPtr<&'code str>;
pub(crate) type TokenIndex<'code> = SliceIndexPtr<Token<'code>>;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub(crate) enum TokenKind<'code> {
    LineComment(TextIndex<'code>),
    BlockComment(TextIndex<'code>),
    // IDEA(stefano): remove from the returned tokens, to avoid encountering them during the parsing stage
    Unexpected(TextIndex<'code>),

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
    False,
    True,

    // integer literals are never empty and always contain valid ascii digits
    DecimalInteger(TextIndex<'code>),
    DecimalIntegerPrefix(TextIndex<'code>),
    BinaryInteger(TextIndex<'code>),
    OctalInteger(TextIndex<'code>),
    HexadecimalInteger(TextIndex<'code>),

    Ascii(TextIndex<'code>, ascii),
    Str(TextIndex<'code>),
    RawStr(TextIndex<'code>),
    IdentifierStr(TextIndex<'code>),

    // IDEA(stefano): extract base types from identifiers
    Identifier(TextIndex<'code>),

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

impl<'code> TokenKind<'code> {
    pub(crate) fn display_len(self, tokens: &Tokens<'code>) -> offset32 {
        #[expect(clippy::cast_possible_truncation)]
        return match self {
            Self::LineComment(comment) => {
                let text = tokens.text[comment];
                text.display_len()
            },
            Self::BlockComment(comment) => {
                let text = tokens.text[comment];
                text.display_len()
            },
            Self::Unexpected(unexpected) => {
                let text = tokens.text[unexpected];
                text.display_len()
            },

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

            Self::True => 4,
            Self::False => 5,

            Self::DecimalInteger(integer) | Self::DecimalIntegerPrefix(integer) => {
                let text = tokens.text[integer];
                text.len() as offset32
            },
            Self::BinaryInteger(integer) => {
                let text = tokens.text[integer];
                text.len() as offset32
            },
            Self::OctalInteger(integer) => {
                let text = tokens.text[integer];
                text.len() as offset32
            },
            Self::HexadecimalInteger(integer) => {
                let text = tokens.text[integer];
                text.len() as offset32
            },

            Self::Ascii(ascii_char, _) => {
                let text = tokens.text[ascii_char];
                text.len() as offset32
            },
            Self::Str(string) => {
                let text = tokens.text[string];
                text.len() as offset32
            },
            Self::RawStr(string) => {
                let text = tokens.text[string];
                text.len() as offset32
            },
            Self::IdentifierStr(identifier) => {
                let text = tokens.text[identifier];
                text.len() as offset32
            },

            Self::Identifier(identifier) => {
                let text = tokens.text[identifier];
                text.len() as offset32
            },

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

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub(crate) struct Token<'code> {
    pub(crate) kind: TokenKind<'code>,
    pub(crate) col: offset32,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Tokens<'code> {
    pub(crate) tokens: Vec<Token<'code>>,

    // IDEA(stefano): store a Range<offset32> instead
    pub(crate) text: Vec<&'code str>,
}

#[must_use = "this is similar to a `Result`, which should be handled"]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TokenizedCode<'code, 'path: 'code> {
    pub result: Result<Tokens<'code>, Vec<Msg<ErrorKind<'code>>>>,
    pub src: SrcCode<'code, 'path>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Tokenizer<'code> {
    code: &'code str,
    lines: Vec<Line>,
    line_start: offset32,

    col: offset32,
    token_start_col: offset32,
    tokens: Tokens<'code>,

    errors: Vec<Msg<ErrorKind<'code>>>,
}

impl<'code, 'path: 'code> Tokenizer<'code> {
    // TODO(stefano): move actual tokenization of tokens to own function to use self instead of tokenizer
    pub fn tokenize(src_file: &'code SrcFile<'path>) -> TokenizedCode<'code, 'path> {
        #[repr(C)]
        union BackPatch<'code> {
            token: TokenIndex<'code>,
            column: offset32,
        }

        let tokens = Tokens { tokens: Vec::new(), text: Vec::new() };
        if src_file.code.len() == 0 {
            return TokenizedCode {
                result: Ok(tokens),
                src: SrcCode { src_file, lines: Vec::new() },
            };
        }

        let mut tokenizer = Tokenizer {
            code: &src_file.code,
            lines: Vec::new(),
            line_start: 0,

            col: 0,
            token_start_col: 0,
            tokens,

            errors: Vec::new(),
        };

        let mut back_patches = Vec::<BackPatch<'_>>::new();

        'tokenization: while let Some(next_character) = tokenizer.current_ascii_multiline() {
            let token_kind_result = 'next_token: {
                let next = match next_character {
                    Ok(next) => match next {
                        b' ' | b'\t' | b'\x0B' | b'\x0C' => {
                            // ignore whitespace
                            tokenizer.col += 1;
                            continue 'tokenization;
                        },

                        b'\n' => {
                            tokenizer.new_line(LineEnd::LF);
                            continue 'tokenization;
                        },
                        b'\r' => {
                            if let Some(b'\n') =
                                tokenizer.code.as_bytes().get(tokenizer.col as usize + 1)
                            {
                                tokenizer.new_line(LineEnd::CRLF);
                                continue 'tokenization;
                            }

                            // tokenizer.errors.push(Error {
                            //     severity: MsgKind::NonTerminalError,
                            //     kind: ErrorKind::StrayCarriageReturn,
                            //     col: tokenizer.line_start,
                            //     pointers_count: 0,
                            // });
                            tokenizer.new_line(LineEnd::CR);
                            continue 'tokenization;
                        },
                        other => {
                            tokenizer.token_start_col = tokenizer.col;
                            tokenizer.col += 1;
                            other
                        },
                    },
                    Err(grapheme) => {
                        tokenizer.push_utf8_error(grapheme);
                        #[expect(clippy::cast_possible_truncation)]
                        {
                            tokenizer.col += grapheme.len() as offset32;
                        }
                        break 'next_token Err(());
                    },
                };

                match next {
                    b'r' => match tokenizer.current_byte_multiline() {
                        Some(b'"') => {
                            tokenizer.col += 1;
                            tokenizer.raw_str_literal()
                        },
                        _ => tokenizer.identifier(),
                    },
                    b'a'..=b'z' | b'A'..=b'Z' | b'_' => tokenizer.identifier(),
                    b'0' => match tokenizer.current_byte_singleline() {
                        None => {
                            let literal_text = tokenizer.token_text();
                            let literal_index = tokenizer.new_token_text(literal_text);
                            Ok(TokenKind::DecimalInteger(literal_index))
                        },
                        Some(b'b') => {
                            tokenizer.col += 1;
                            tokenizer.integer_binary()
                        },
                        Some(b'o') => {
                            tokenizer.col += 1;
                            tokenizer.integer_octal()
                        },
                        Some(b'd') => {
                            tokenizer.col += 1;
                            tokenizer.integer_decimal_prefix()
                        },
                        Some(b'x') => {
                            tokenizer.col += 1;
                            tokenizer.integer_hexadecimal()
                        },
                        Some(_) => {
                            tokenizer.integer_decimal()
                        }
                    },
                    b'1'..=b'9' => tokenizer.integer_decimal(),
                    b'\'' => tokenizer.ascii_literal(),
                    b'"' => tokenizer.str_literal(),
                    b'`' => tokenizer.identifier_str(),
                    b'#' => match tokenizer.get_next_byte_singleline() {
                        Some(b'*') => 'comment: {
                            let previous_block_comments_token_start_len = back_patches.len();
                            'next_character: loop {
                                match tokenizer.get_next_byte_multiline() {
                                    Some(b'*') => match tokenizer.get_next_byte_multiline() {
                                        Some(b'#') => {
                                            let comment_text = tokenizer.token_text();
                                            let comment_index = tokenizer.new_token_text(comment_text);
                                            if back_patches.len()
                                                == previous_block_comments_token_start_len
                                            {
                                                break 'comment Ok(TokenKind::BlockComment(
                                                    comment_index,
                                                ));
                                            }
                                            let Some(token_start_col) = back_patches.pop() else {
                                                unreachable!("unclosed block comment");
                                            };

                                            let kind = TokenKind::BlockComment(comment_index);
                                            tokenizer.tokens.tokens.push(Token {
                                                kind,
                                                col: tokenizer.token_start_col,
                                            });
                                            tokenizer.token_start_col =
                                                unsafe { token_start_col.column };
                                        },
                                        Some(_) => {},
                                        None => break 'next_character,
                                    },
                                    Some(b'#') => {
                                        let comment_start_col = tokenizer.col - 1;
                                        match tokenizer.get_next_byte_multiline() {
                                            Some(b'*') => {
                                                let back_patch =
                                                    BackPatch { column: tokenizer.token_start_col };
                                                back_patches.push(back_patch);
                                                tokenizer.token_start_col = comment_start_col;
                                                continue 'next_character;
                                            },
                                            Some(_) => {},
                                            None => break 'next_character,
                                        }
                                    },
                                    Some(_) => {},
                                    None => break 'next_character,
                                }
                            }

                            tokenizer.errors.push(Msg {
                                severity: MsgSeverity::NonTerminalError,
                                kind: ErrorKind::UnclosedBlockComment,
                                col: tokenizer.token_start_col,
                                pointers_count: 2,
                            });
                            while back_patches.len() != previous_block_comments_token_start_len {
                                let Some(block_comment_token_start) = back_patches.pop() else {
                                    break;
                                };
                                tokenizer.errors.push(Msg {
                                    severity: MsgSeverity::NonTerminalError,
                                    kind: ErrorKind::UnclosedBlockComment,
                                    col: unsafe { block_comment_token_start.column },
                                    pointers_count: 2,
                                });
                            }
                            Err(())
                        },
                        Some(_) => {
                            while let Some(_) = tokenizer.get_next_byte_singleline() {
                                // consume next character
                            }
                            let comment_text = tokenizer.token_text();
                            let comment_index = tokenizer.new_token_text(comment_text);
                            Ok(TokenKind::LineComment(comment_index))
                        },
                        None => {
                            let comment_text = tokenizer.token_text();
                            let comment_index = tokenizer.new_token_text(comment_text);
                            Ok(TokenKind::LineComment(comment_index))
                        },
                    },
                    b'(' => {
                        let back_patch =
                            BackPatch { token: TokenIndex::new(tokenizer.tokens.tokens.len()) };
                        back_patches.push(back_patch);
                        Ok(TokenKind::OpenRoundBracket)
                    },
                    b')' => 'bracket: {
                        let Some(bracket_index) = back_patches.pop() else {
                            tokenizer.errors.push(Msg {
                                severity: MsgSeverity::NonTerminalError,
                                kind: ErrorKind::UnopenedRoundBracket,
                                col: tokenizer.token_start_col,
                                pointers_count: 1,
                            });
                            break 'bracket Err(());
                        };

                        let token = unsafe { bracket_index.token };
                        #[expect(clippy::wildcard_enum_match_arm)]
                        match tokenizer.tokens.tokens[token].kind {
                            TokenKind::OpenRoundBracket
                            | TokenKind::CloseRoundBracket
                            | TokenKind::CloseCurlyBracket
                            | TokenKind::CloseSquareBracket => Ok(TokenKind::CloseRoundBracket),
                            TokenKind::OpenCurlyBracket => {
                                tokenizer.errors.push(Msg {
                                    severity: MsgSeverity::NonTerminalError,
                                    kind: ErrorKind::MismatchedCurlyRoundBracket,
                                    col: tokenizer.token_start_col,
                                    pointers_count: 1,
                                });
                                Err(())
                            },
                            TokenKind::OpenSquareBracket => {
                                tokenizer.errors.push(Msg {
                                    severity: MsgSeverity::NonTerminalError,
                                    kind: ErrorKind::MismatchedSquareRoundBracket,
                                    col: tokenizer.token_start_col,
                                    pointers_count: 1,
                                });
                                Err(())
                            },
                            _ => unreachable!("incorrect bracket index"),
                        }
                    },
                    b'[' => {
                        let back_patch =
                            BackPatch { token: TokenIndex::new(tokenizer.tokens.tokens.len()) };
                        back_patches.push(back_patch);
                        Ok(TokenKind::OpenSquareBracket)
                    },
                    b']' => 'bracket: {
                        let Some(bracket_index) = back_patches.pop() else {
                            tokenizer.errors.push(Msg {
                                severity: MsgSeverity::NonTerminalError,
                                kind: ErrorKind::UnopenedSquareBracket,
                                col: tokenizer.token_start_col,
                                pointers_count: 1,
                            });
                            break 'bracket Err(());
                        };

                        let token = unsafe { bracket_index.token };
                        #[expect(clippy::wildcard_enum_match_arm)]
                        match tokenizer.tokens.tokens[token].kind {
                            TokenKind::OpenSquareBracket
                            | TokenKind::CloseSquareBracket
                            | TokenKind::CloseCurlyBracket
                            | TokenKind::CloseRoundBracket => Ok(TokenKind::CloseSquareBracket),
                            TokenKind::OpenCurlyBracket => {
                                tokenizer.errors.push(Msg {
                                    severity: MsgSeverity::NonTerminalError,
                                    kind: ErrorKind::MismatchedCurlySquareBracket,
                                    col: tokenizer.token_start_col,
                                    pointers_count: 1,
                                });
                                Err(())
                            },
                            TokenKind::OpenRoundBracket => {
                                tokenizer.errors.push(Msg {
                                    severity: MsgSeverity::NonTerminalError,
                                    kind: ErrorKind::MismatchedRoundSquareBracket,
                                    col: tokenizer.token_start_col,
                                    pointers_count: 1,
                                });
                                Err(())
                            },
                            _ => unreachable!("incorrect bracket index"),
                        }
                    },
                    b'{' => {
                        let back_patch =
                            BackPatch { token: TokenIndex::new(tokenizer.tokens.tokens.len()) };
                        back_patches.push(back_patch);
                        Ok(TokenKind::OpenCurlyBracket)
                    },
                    b'}' => 'bracket: {
                        let Some(bracket_index) = back_patches.pop() else {
                            tokenizer.errors.push(Msg {
                                severity: MsgSeverity::NonTerminalError,
                                kind: ErrorKind::UnopenedCurlyBracket,
                                col: tokenizer.token_start_col,
                                pointers_count: 1,
                            });
                            break 'bracket Err(());
                        };

                        let token = unsafe { bracket_index.token };
                        #[expect(clippy::wildcard_enum_match_arm)]
                        match tokenizer.tokens.tokens[token].kind {
                            TokenKind::OpenCurlyBracket
                            | TokenKind::CloseCurlyBracket
                            | TokenKind::CloseRoundBracket
                            | TokenKind::CloseSquareBracket => Ok(TokenKind::CloseCurlyBracket),
                            TokenKind::OpenRoundBracket => {
                                tokenizer.errors.push(Msg {
                                    severity: MsgSeverity::NonTerminalError,
                                    kind: ErrorKind::MismatchedRoundCurlyBracket,
                                    col: tokenizer.token_start_col,
                                    pointers_count: 1,
                                });
                                Err(())
                            },
                            TokenKind::OpenSquareBracket => {
                                tokenizer.errors.push(Msg {
                                    severity: MsgSeverity::NonTerminalError,
                                    kind: ErrorKind::MismatchedSquareCurlyBracket,
                                    col: tokenizer.token_start_col,
                                    pointers_count: 1,
                                });
                                Err(())
                            },
                            _ => unreachable!("incorrect bracket index"),
                        }
                    },
                    b':' => Ok(TokenKind::Colon),
                    b';' => Ok(TokenKind::SemiColon),
                    b',' => Ok(TokenKind::Comma),
                    b'!' => match tokenizer.current_byte_multiline() {
                        Some(b'=') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte_multiline() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::NotEqualsEquals))
                                },
                                _ => Ok(TokenKind::Op(Op::NotEquals)),
                            }
                        },
                        _ => Ok(TokenKind::Op(Op::Not)),
                    },
                    b'*' => match tokenizer.current_byte_multiline() {
                        Some(b'*') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte_multiline() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::PowEquals))
                                },
                                Some(b'\\') => {
                                    tokenizer.col += 1;
                                    match tokenizer.current_byte_multiline() {
                                        Some(b'=') => {
                                            tokenizer.col += 1;
                                            Ok(TokenKind::Op(Op::WrappingPowEquals))
                                        },
                                        _ => Ok(TokenKind::Op(Op::WrappingPow)),
                                    }
                                },
                                Some(b'|') => {
                                    tokenizer.col += 1;
                                    match tokenizer.current_byte_multiline() {
                                        Some(b'=') => {
                                            tokenizer.col += 1;
                                            Ok(TokenKind::Op(Op::SaturatingPowEquals))
                                        },
                                        _ => Ok(TokenKind::Op(Op::SaturatingPow)),
                                    }
                                },
                                _ => Ok(TokenKind::Op(Op::Pow)),
                            }
                        },
                        Some(b'=') => {
                            tokenizer.col += 1;
                            Ok(TokenKind::Op(Op::TimesEquals))
                        },
                        Some(b'\\') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte_multiline() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::WrappingTimesEquals))
                                },
                                _ => Ok(TokenKind::Op(Op::WrappingTimes)),
                            }
                        },
                        Some(b'|') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte_multiline() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::SaturatingTimesEquals))
                                },
                                _ => Ok(TokenKind::Op(Op::SaturatingTimes)),
                            }
                        },
                        _ => Ok(TokenKind::Op(Op::Times)),
                    },
                    b'/' => match tokenizer.current_byte_multiline() {
                        Some(b'=') => {
                            tokenizer.col += 1;
                            Ok(TokenKind::Op(Op::DivideEquals))
                        },
                        Some(b'\\') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte_multiline() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::WrappingDivideEquals))
                                },
                                _ => Ok(TokenKind::Op(Op::WrappingDivide)),
                            }
                        },
                        Some(b'|') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte_multiline() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::SaturatingDivideEquals))
                                },
                                _ => Ok(TokenKind::Op(Op::SaturatingDivide)),
                            }
                        },
                        _ => Ok(TokenKind::Op(Op::Divide)),
                    },
                    b'%' => match tokenizer.current_byte_multiline() {
                        Some(b'=') => {
                            tokenizer.col += 1;
                            Ok(TokenKind::Op(Op::RemainderEquals))
                        },
                        _ => Ok(TokenKind::Op(Op::Remainder)),
                    },
                    b'+' => match tokenizer.current_byte_multiline() {
                        Some(b'=') => {
                            tokenizer.col += 1;
                            Ok(TokenKind::Op(Op::PlusEquals))
                        },
                        Some(b'\\') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte_multiline() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::WrappingPlusEquals))
                                },
                                _ => Ok(TokenKind::Op(Op::WrappingPlus)),
                            }
                        },
                        Some(b'|') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte_multiline() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::SaturatingPlusEquals))
                                },
                                _ => Ok(TokenKind::Op(Op::SaturatingPlus)),
                            }
                        },
                        _ => Ok(TokenKind::Op(Op::Plus)),
                    },
                    b'-' => match tokenizer.current_byte_multiline() {
                        Some(b'=') => {
                            tokenizer.col += 1;
                            Ok(TokenKind::Op(Op::MinusEquals))
                        },
                        Some(b'\\') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte_multiline() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::WrappingMinusEquals))
                                },
                                _ => Ok(TokenKind::Op(Op::WrappingMinus)),
                            }
                        },
                        Some(b'|') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte_multiline() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::SaturatingMinusEquals))
                                },
                                _ => Ok(TokenKind::Op(Op::SaturatingMinus)),
                            }
                        },
                        _ => Ok(TokenKind::Op(Op::Minus)),
                    },
                    b'&' => match tokenizer.current_byte_multiline() {
                        Some(b'&') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte_multiline() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::AndEquals))
                                },
                                _ => Ok(TokenKind::Op(Op::And)),
                            }
                        },
                        Some(b'=') => {
                            tokenizer.col += 1;
                            Ok(TokenKind::Op(Op::BitAndEquals))
                        },
                        _ => Ok(TokenKind::Op(Op::BitAnd)),
                    },
                    b'^' => match tokenizer.current_byte_multiline() {
                        Some(b'=') => {
                            tokenizer.col += 1;
                            Ok(TokenKind::Op(Op::BitXorEquals))
                        },
                        _ => Ok(TokenKind::Op(Op::BitXor)),
                    },
                    b'|' => match tokenizer.current_byte_multiline() {
                        Some(b'|') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte_multiline() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::OrEquals))
                                },
                                _ => Ok(TokenKind::Op(Op::Or)),
                            }
                        },
                        Some(b'=') => {
                            tokenizer.col += 1;
                            Ok(TokenKind::Op(Op::BitOrEquals))
                        },
                        _ => Ok(TokenKind::Op(Op::BitOr)),
                    },
                    b'=' => match tokenizer.current_byte_multiline() {
                        Some(b'=') => {
                            tokenizer.col += 1;
                            Ok(TokenKind::Op(Op::EqualsEquals))
                        },
                        _ => Ok(TokenKind::Op(Op::Equals)),
                    },
                    b'>' => match tokenizer.current_byte_multiline() {
                        Some(b'>') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte_multiline() {
                                Some(b'>') => {
                                    tokenizer.col += 1;
                                    match tokenizer.current_byte_multiline() {
                                        Some(b'=') => {
                                            tokenizer.col += 1;
                                            Ok(TokenKind::Op(Op::RightRotateEquals))
                                        },
                                        _ => Ok(TokenKind::Op(Op::RightRotate)),
                                    }
                                },
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::RightShiftEquals))
                                },
                                _ => Ok(TokenKind::Op(Op::RightShift)),
                            }
                        },
                        Some(b'=') => {
                            tokenizer.col += 1;
                            Ok(TokenKind::Op(Op::GreaterOrEquals))
                        },
                        _ => Ok(TokenKind::Op(Op::Greater)),
                    },
                    b'<' => match tokenizer.current_byte_multiline() {
                        Some(b'<') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte_multiline() {
                                Some(b'<') => {
                                    tokenizer.col += 1;
                                    match tokenizer.current_byte_multiline() {
                                        Some(b'=') => {
                                            tokenizer.col += 1;
                                            Ok(TokenKind::Op(Op::LeftRotateEquals))
                                        },
                                        _ => Ok(TokenKind::Op(Op::LeftRotate)),
                                    }
                                },
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::LeftShiftEquals))
                                },
                                Some(b'\\') => {
                                    tokenizer.col += 1;
                                    match tokenizer.current_byte_multiline() {
                                        Some(b'=') => {
                                            tokenizer.col += 1;
                                            Ok(TokenKind::Op(Op::WrappingLeftShiftEquals))
                                        },
                                        _ => Ok(TokenKind::Op(Op::WrappingLeftShift)),
                                    }
                                },
                                Some(b'|') => {
                                    tokenizer.col += 1;
                                    match tokenizer.current_byte_multiline() {
                                        Some(b'=') => {
                                            tokenizer.col += 1;
                                            Ok(TokenKind::Op(Op::SaturatingLeftShiftEquals))
                                        },
                                        _ => Ok(TokenKind::Op(Op::SaturatingLeftShift)),
                                    }
                                },
                                _ => Ok(TokenKind::Op(Op::LeftShift)),
                            }
                        },
                        Some(b'=') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte_multiline() {
                                Some(b'>') => {
                                    tokenizer.col += 1;
                                    Ok(TokenKind::Op(Op::Compare))
                                },
                                _ => Ok(TokenKind::Op(Op::LessOrEquals)),
                            }
                        },
                        _ => Ok(TokenKind::Op(Op::Less)),
                    },
                    control @ (b'\x00'..=b'\x1F' | b'\x7F') => {
                        tokenizer.errors.push(Msg {
                            severity: MsgSeverity::NonTerminalError,
                            kind: ErrorKind::ControlCharacter(control),
                            col: tokenizer.token_start_col,
                            pointers_count: 1,
                        });
                        Err(())
                    },
                    unrecognized => {
                        tokenizer.errors.push(Msg {
                            severity: MsgSeverity::NonTerminalError,
                            kind: ErrorKind::UnrecognizedCharacter(unrecognized),
                            col: tokenizer.token_start_col,
                            pointers_count: 1,
                        });
                        Err(())
                    },
                }
            };

            let kind = match token_kind_result {
                Ok(kind) => kind,
                Err(()) => {
                    let unexpected_text = tokenizer.token_text();
                    let unexpected_index = tokenizer.new_token_text(unexpected_text);
                    TokenKind::Unexpected(unexpected_index)
                },
            };

            tokenizer.tokens.tokens.push(Token { kind, col: tokenizer.token_start_col });
        }

        if tokenizer.line_start != tokenizer.col {
            // file ended without a newline
            let line = Line { start: tokenizer.line_start, end: tokenizer.col };
            tokenizer.lines.push(line);
        }

        while let Some(bracket_index) = back_patches.pop() {
            let token = unsafe { bracket_index.token };
            // there can only be open brackets at this point
            let bracket_token = tokenizer.tokens.tokens[token];

            #[expect(clippy::wildcard_enum_match_arm)]
            let error_kind = match bracket_token.kind {
                TokenKind::OpenRoundBracket => ErrorKind::UnclosedRoundBracket,
                TokenKind::OpenSquareBracket => ErrorKind::UnclosedSquareBracket,
                TokenKind::OpenCurlyBracket => ErrorKind::UnclosedCurlyBracket,
                _ => unreachable!("incorrect bracket index"),
            };

            tokenizer.errors.push(Msg {
                severity: MsgSeverity::NonTerminalError,
                kind: error_kind,
                col: bracket_token.col,
                pointers_count: 1,
            });
        }

        let result =
            if tokenizer.errors.len() == 0 { Ok(tokenizer.tokens) } else { Err(tokenizer.errors) };
        return TokenizedCode { result, src: SrcCode { src_file, lines: tokenizer.lines } };
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
enum LineEnd {
    LF = 1,
    #[expect(clippy::upper_case_acronyms)]
    CRLF = 2,
}

impl LineEnd {
    const CR: Self = Self::LF;
}

// iteration of characters
impl<'code> Tokenizer<'code> {
    #[inline]
    fn new_line(&mut self, line_end: LineEnd) {
        let line = Line { start: self.line_start, end: self.col };
        self.col += line_end as offset32;
        self.line_start = self.col;
        self.lines.push(line);
    }

    #[must_use]
    #[inline]
    fn new_token_text(&mut self, text: &'code str) -> TextIndex<'code> {
        let index = TextIndex::new(self.tokens.text.len());
        self.tokens.text.push(text);
        return index;
    }

    #[must_use]
    #[inline(always)]
    fn token_text(&self) -> &'code str {
        return &self.code[self.token_start_col as usize..self.col as usize];
    }

    #[must_use]
    #[inline]
    const fn current_byte_multiline(&self) -> Option<u8> {
        if self.col as usize >= self.code.as_bytes().len() {
            return None;
        }
        return Some(self.code.as_bytes()[self.col as usize]);
    }

    #[must_use]
    const fn current_byte_singleline(&self) -> Option<u8> {
        let Some(next) = self.current_byte_multiline() else {
            return None;
        };
        return match next {
            b'\r' | b'\n' => None,
            other => Some(other),
        };
    }

    #[expect(clippy::question_mark)]
    #[must_use]
    fn get_next_byte_multiline(&mut self) -> Option<u8> {
        let Some(next) = self.current_byte_multiline() else {
            return None;
        };
        return match next {
            b'\n' => {
                self.new_line(LineEnd::LF);
                Some(b'\n')
            },
            b'\r' => {
                if let Some(b'\n') = self.code.as_bytes().get(self.col as usize + 1) {
                    self.new_line(LineEnd::CRLF);
                    return Some(b'\n');
                }

                // self.errors.push(Error {
                //     severity: MsgSeverity::NonTerminalError,
                //     kind: ErrorKind::StrayCarriageReturn,
                //     col: self.line_start,
                //     pointers_count: 0,
                // });
                self.new_line(LineEnd::CR);
                Some(b'\n')
            },
            other => {
                self.col += 1;
                Some(other)
            },
        };
    }

    #[expect(clippy::question_mark)]
    #[must_use]
    fn get_next_byte_singleline(&mut self) -> Option<u8> {
        let Some(next) = self.current_byte_multiline() else {
            return None;
        };
        return match next {
            b'\r' | b'\n' => None,
            other => {
                self.col += 1;
                Some(other)
            },
        };
    }

    #[expect(clippy::question_mark)]
    #[must_use]
    fn current_ascii_multiline(&self) -> Option<Result<ascii, &'code str>> {
        let Some(next) = self.current_byte_multiline() else {
            return None;
        };
        return match next {
            ascii_ch @ 0..=b'\x7F' => Some(Ok(ascii_ch)),
            _utf8_ch => {
                let rest_of_code = &self.code[self.col as usize..];
                let mut rest_of_line_graphemes = rest_of_code.graphemes(true);
                let Some(grapheme) = rest_of_line_graphemes.next() else {
                    unreachable!("this branch assured we would have a valid grapheme");
                };

                Some(Err(grapheme))
            },
        };
    }

    #[expect(clippy::question_mark)]
    #[must_use]
    fn current_ascii_singleline(&self) -> Option<Result<ascii, &'code str>> {
        let Some(next) = self.current_byte_singleline() else {
            return None;
        };
        return match next {
            ascii_ch @ 0..=b'\x7F' => Some(Ok(ascii_ch)),
            _utf8_ch => {
                let rest_of_code = &self.code[self.col as usize..];
                let mut rest_of_line_graphemes = rest_of_code.graphemes(true);
                let Some(grapheme) = rest_of_line_graphemes.next() else {
                    unreachable!("this branch assured we would have a valid grapheme");
                };

                Some(Err(grapheme))
            },
        };
    }

    #[expect(clippy::question_mark)]
    #[must_use]
    fn current_or_until_next_ascii_singleline(&mut self) -> Option<ascii> {
        loop {
            let Some(next) = self.current_byte_singleline() else {
                return None;
            };
            match next {
                ascii_ch @ 0..=b'\x7F' => return Some(ascii_ch),
                _utf8_ch => {
                    let rest_of_code = &self.code[self.col as usize..];
                    let mut rest_of_line_graphemes = rest_of_code.graphemes(true);
                    let Some(grapheme) = rest_of_line_graphemes.next() else {
                        unreachable!("this branch assured we would have a valid grapheme");
                    };
                    self.push_utf8_error(grapheme);
                    #[expect(clippy::cast_possible_truncation)]
                    {
                        self.col += grapheme.len() as offset32;
                    }
                }
            }
        }
    }

    fn push_utf8_error(&mut self, grapheme: &'code str) {
        self.errors.push(Msg {
            severity: MsgSeverity::NonTerminalError,
            kind: ErrorKind::Utf8Character { grapheme },
            col: self.col,
            pointers_count: grapheme.display_len(),
        });
    }
}

// tokenization of numbers
impl<'code> Tokenizer<'code> {
    fn decimal_digits(&mut self) -> Result<&'code str, ()> {
        let previous_errors_len = self.errors.len();

        while let Some(digit) = self.current_or_until_next_ascii_singleline() {
            match digit::check_decimal(digit) {
                AsciiDigit::Ok | AsciiDigit::Underscore => {},
                AsciiDigit::Other | AsciiDigit::Dot => break,
                AsciiDigit::OutOfRange => {
                    self.errors.push(Msg {
                        severity: MsgSeverity::NonTerminalError,
                        kind: ErrorKind::DigitOutOfRange(digit, Base(digit::Base::Decimal)),
                        col: self.col,
                        pointers_count: 1,
                    });
                },
            }
            self.col += 1;
        }

        if previous_errors_len != self.errors.len() {
            return Err(());
        }
        let literal_text = self.token_text();
        return Ok(literal_text)
    }

    fn integer_decimal(&mut self) -> Result<TokenKind<'code>, ()> {
        let literal_text = self.decimal_digits()?;
        let literal_index = self.new_token_text(literal_text);
        return Ok(TokenKind::DecimalInteger(literal_index));
    }

    fn integer_decimal_prefix(&mut self) -> Result<TokenKind<'code>, ()> {
        let literal_text = self.decimal_digits()?;
        let literal_index = self.new_token_text(literal_text);
        return Ok(TokenKind::DecimalIntegerPrefix(literal_index));
    }

    fn binary_digits(&mut self) -> Result<&'code str, ()> {
        let previous_errors_len = self.errors.len();

        while let Some(digit) = self.current_or_until_next_ascii_singleline() {
            match digit::check_binary(digit) {
                AsciiDigit::Ok | AsciiDigit::Underscore => {},
                AsciiDigit::Other | AsciiDigit::Dot => break,
                AsciiDigit::OutOfRange => {
                    self.errors.push(Msg {
                        severity: MsgSeverity::NonTerminalError,
                        kind: ErrorKind::DigitOutOfRange(digit, Base(digit::Base::Binary)),
                        col: self.col,
                        pointers_count: 1,
                    });
                },
            }
            self.col += 1;
        }

        if previous_errors_len != self.errors.len() {
            return Err(());
        }
        let literal_text = self.token_text();
        return Ok(literal_text)
    }

    fn integer_binary(&mut self) -> Result<TokenKind<'code>, ()> {
        let literal_text = self.binary_digits()?;
        let literal_index = self.new_token_text(literal_text);
        return Ok(TokenKind::BinaryInteger(literal_index));
    }

    fn octal_digits(&mut self) -> Result<&'code str, ()> {
        let previous_errors_len = self.errors.len();

        while let Some(digit) = self.current_or_until_next_ascii_singleline() {
            match digit::check_octal(digit) {
                AsciiDigit::Ok | AsciiDigit::Underscore => {},
                AsciiDigit::Other | AsciiDigit::Dot => break,
                AsciiDigit::OutOfRange => {
                    self.errors.push(Msg {
                        severity: MsgSeverity::NonTerminalError,
                        kind: ErrorKind::DigitOutOfRange(digit, Base(digit::Base::Octal)),
                        col: self.col,
                        pointers_count: 1,
                    });
                },
            }
            self.col += 1;
        }

        if previous_errors_len != self.errors.len() {
            return Err(());
        }
        let literal_text = self.token_text();
        return Ok(literal_text)
    }

    fn integer_octal(&mut self) -> Result<TokenKind<'code>, ()> {
        let literal_text = self.octal_digits()?;
        let literal_index = self.new_token_text(literal_text);
        return Ok(TokenKind::OctalInteger(literal_index));
    }

    fn hexadecimal_digits(&mut self) -> Result<&'code str, ()> {
        let previous_errors_len = self.errors.len();

        while let Some(digit) = self.current_or_until_next_ascii_singleline() {
            match digit::check_hexadecimal(digit) {
                AsciiDigit::Ok | AsciiDigit::Underscore => {},
                AsciiDigit::Other | AsciiDigit::Dot => break,
                AsciiDigit::OutOfRange => {
                    self.errors.push(Msg {
                        severity: MsgSeverity::NonTerminalError,
                        kind: ErrorKind::DigitOutOfRange(digit, Base(digit::Base::Hexadecimal)),
                        col: self.col,
                        pointers_count: 1,
                    });
                },
            }
            self.col += 1;
        }

        if previous_errors_len != self.errors.len() {
            return Err(());
        }
        let literal_text = self.token_text();
        return Ok(literal_text)
    }

    fn integer_hexadecimal(&mut self) -> Result<TokenKind<'code>, ()> {
        let literal_text = self.hexadecimal_digits()?;
        let literal_index = self.new_token_text(literal_text);
        return Ok(TokenKind::HexadecimalInteger(literal_index));
    }
}

#[derive(Clone, Copy)]
enum Character {
    OkEscaped(ascii),
    ErrBreak,
    ErrContinue,
}

// tokenization of strings and character literals
impl<'code> Tokenizer<'code> {
    fn decimal_escape_sequence(&mut self) -> Character {
        unimplemented!()
    }

    fn binary_escape_sequence(&mut self) -> Character {
        //     const ASCII_MAX_BINARY_DIGITS: u32 = 0;

            //     self.col += 1;
            //     let start_of_digits_col = self.col;
            //     loop {
            //         let digit = match self.peek_ascii_singleline() {
            //             Some(Ok(digit)) => digit,
            //             Some(Err(grapheme)) => {
            //                 self.push_utf8_error(grapheme);
            //                 #[expect(clippy::cast_possible_truncation)]
            //                 {
            //                     self.col += grapheme.len() as offset32;
            //                 }
            //                 return EscapedCharacter::ErrNonTerminal;
            //             }
            //             None => {
            //                 self.errors.push(Msg {
            //                     severity: MsgSeverity::NonTerminalError,
            //                     kind: ErrorKind::UnterminatedEscapeCharacter,
            //                     col: start_of_ch,
            //                     pointers_count: self.col - start_of_ch,
            //                 });
            //                 return EscapedCharacter::Err;
            //             }
            //         };
            //         let Some(digit_result) = Self::integer_binary_digit(digit) else {
            //             break;
            //         };

            //         self.col += 1;
            //         if let Err(out_of_range) = digit_result {
            //             self.errors.push(Msg {
            //                 severity: MsgSeverity::NonTerminalError,
            //                 kind: ErrorKind::DigitOutOfRange(out_of_range, Base::Binary),
            //                 col: self.col,
            //                 pointers_count: 1,
            //             });
            //         }
            //     }
            //     let Some(terminator) = self.next_ascii_singleline() else {
            //         self.errors.push(Msg {
            //             severity: MsgSeverity::NonTerminalError,
            //             kind: ErrorKind::UnterminatedEscapeCharacter,
            //             col: start_of_ch,
            //             pointers_count: self.col - start_of_ch,
            //         });
            //         return EscapedCharacter::Err;
            //     };
            //     let b'\\' = terminator else {
            //         self.errors.push(Msg {
            //             severity: MsgSeverity::NonTerminalError,
            //             kind: ErrorKind::UnterminatedEscapeCharacter,
            //             col: start_of_ch,
            //             pointers_count: self.col - start_of_ch,
            //         });
            //         return EscapedCharacter::ErrNonTerminal;
            //     };

            //     let digits_count = self.col - start_of_digits_col;
            //     if digits_count > ASCII_MAX_BINARY_DIGITS {
            //         self.errors.push(Msg {
            //             severity: MsgSeverity::NonTerminalError,
            //             kind: ErrorKind::AsciiBinaryEscapeOverflow,
            //             col: start_of_ch,
            //             pointers_count: self.col - start_of_ch,
            //         });
            //         return EscapedCharacter::ErrNonTerminal;
            //     }
            //     unimplemented!("parsing of the ascii value (expose the implementation from the parsing in the typed abstract syntax tree");
        unimplemented!()
    }

    fn octal_escape_sequence(&mut self) -> Character {
        unimplemented!()
    }

    fn hexadecimal_escape_sequence(&mut self) -> Character {
        unimplemented!()
    }

    fn escape_sequence(&mut self, start_of_character: offset32) -> Character {
        // TODO: factor out this peeking of the next character in quoted literal
        let next_character = match self.current_ascii_singleline() {
            Some(Ok(escape_character)) => escape_character,
            Some(Err(grapheme)) => {
                self.push_utf8_error(grapheme);
                #[expect(clippy::cast_possible_truncation)]
                {
                    self.col += grapheme.len() as offset32;
                }
                return Character::ErrContinue;
            },
            None => {
                self.errors.push(Msg {
                    severity: MsgSeverity::NonTerminalError,
                    kind: ErrorKind::UnterminatedEscapeCharacter,
                    col: start_of_character,
                    pointers_count: self.col - start_of_character,
                });
                return Character::ErrBreak;
            },
        };
        self.col += 1;

        let escaped_character = match next_character {
            b'\\' => b'\\',
            b'\'' => b'\'',
            b'"'  => b'\"',
            b'e'  => b'\x1b',
            b'n'  => b'\n',
            b'r'  => b'\r',
            b't'  => b'\t',
            b'0'  => b'\0',
            // b'0'  => {
            //     let digit = match self.current_ascii_singleline() {
            //         Some(Ok(digit)) => digit,
            //         Some(Err(grapheme)) => {
            //             self.push_utf8_error(grapheme);
            //             #[expect(clippy::cast_possible_truncation)]
            //             {
            //                 self.col += grapheme.len() as offset32;
            //             }
            //             return Character::ErrContinue;
            //         },
            //         None => {
            //             self.errors.push(Msg {
            //                 severity: MsgSeverity::NonTerminalError,
            //                 kind: ErrorKind::UnterminatedEscapeCharacter,
            //                 col: start_of_character,
            //                 pointers_count: self.col - start_of_character,
            //             });
            //             return Character::ErrBreak;
            //         },
            //     };
            //     self.col += 1;

            //     let escaped_character = match digit {
            //         b'b'  => {
            //             self.col += 1;
            //             self.binary_escape_sequence()
            //         },
            //         b'o'  => {
            //             self.col += 1;
            //             self.octal_escape_sequence()
            //         },
            //         b'd'  => {
            //             self.col += 1;
            //             self.decimal_escape_sequence()
            //         },
            //         b'x'  => {
            //             self.col += 1;
            //             self.hexadecimal_escape_sequence()
            //         },
            //         _ => {
            //             self.decimal_escape_sequence()
            //         }
            //     };

            //     unimplemented!("parsing of ascii value");
            // },
            // b'c'  => {
            //     unimplemented!("ascii control mnemonics");
            // },
            b'^' => {
                let caret_character = match self.current_ascii_singleline() {
                    Some(Ok(escape_character)) => escape_character,
                    Some(Err(grapheme)) => {
                        self.push_utf8_error(grapheme);
                        #[expect(clippy::cast_possible_truncation)]
                        {
                            self.col += grapheme.len() as offset32;
                        }
                        return Character::ErrContinue;
                    },
                    None => {
                        self.errors.push(Msg {
                            severity: MsgSeverity::NonTerminalError,
                            kind: ErrorKind::UnterminatedEscapeCharacter,
                            col: start_of_character,
                            pointers_count: self.col - start_of_character,
                        });
                        return Character::ErrBreak;
                    },
                };
                self.col += 1;

                match caret_character {
                    b'@'..=b'Z' => caret_character - b'@',
                    b'['..=b'_' => caret_character - b'[' + b'Z' - b'@' + 1,
                    b'?' => b'\x7f',
                    unrecognized => {
                        self.errors.push(Msg {
                            severity: MsgSeverity::NonTerminalError,
                            kind: ErrorKind::UnrecognizedEscapeCharacter(unrecognized),
                            col: start_of_character,
                            pointers_count: 3,
                        });
                        return Character::ErrContinue;
                    },
                }
            },
            control @ (b'\x00'..=b'\x1F' | b'\x7F') => {
                self.errors.push(Msg {
                    severity: MsgSeverity::NonTerminalError,
                    kind: ErrorKind::ControlCharacter(control),
                    col: start_of_character + 1,
                    pointers_count: 1,
                });
                return Character::ErrContinue;
            },
            unrecognized => {
                self.errors.push(Msg {
                    severity: MsgSeverity::NonTerminalError,
                    kind: ErrorKind::UnrecognizedEscapeCharacter(unrecognized),
                    col: start_of_character,
                    pointers_count: 2,
                });
                return Character::ErrContinue;
            },
        };

        return Character::OkEscaped(escaped_character);
    }

    fn ascii_literal_characters(&mut self) -> Result<(&'code str, ascii), ()> {
        let previous_errors_len = self.errors.len();

        let mut logical_character = b'\0';
        let mut logical_characters_count = 0;
        loop {
            let next_character = match self.current_ascii_singleline() {
                Some(Ok(next_character)) => next_character,
                Some(Err(grapheme)) => {
                    self.push_utf8_error(grapheme);
                    #[expect(clippy::cast_possible_truncation)]
                    {
                        self.col += grapheme.len() as offset32;
                    }
                    continue;
                },
                None => {
                    self.errors.push(Msg {
                        severity: MsgSeverity::NonTerminalError,
                        kind: ErrorKind::UnclosedCharacterLiteral,
                        col: self.token_start_col,
                        pointers_count: self.token_text().display_len(),
                    });
                    break;
                },
            };
            let start_of_character = self.col;
            self.col += 1;

            match next_character {
                b'\\' => {
                    logical_character = match self.escape_sequence(start_of_character) {
                        Character::OkEscaped(ch) => ch,
                        Character::ErrBreak => break,
                        Character::ErrContinue => continue,
                    };
                },
                control @ (b'\x00'..=b'\x1F' | b'\x7F') => {
                    self.errors.push(Msg {
                        severity: MsgSeverity::NonTerminalError,
                        kind: ErrorKind::ControlCharacter(control),
                        col: start_of_character,
                        pointers_count: 1,
                    });
                },
                b'\'' => break,
                _ => {},
            }

            logical_characters_count += 1;
        }

        if previous_errors_len != self.errors.len() {
            return Err(());
        }

        if logical_characters_count == 0 {
            self.errors.push(Msg {
                severity: MsgSeverity::NonTerminalError,
                kind: ErrorKind::EmptyCharacterLiteral,
                col: self.token_start_col,
                pointers_count: 2,
            });
            return Err(());
        }
        if logical_characters_count > 1 {
            self.errors.push(Msg {
                severity: MsgSeverity::NonTerminalError,
                kind: ErrorKind::MultipleCharactersInCharacterLiteral,
                col: self.token_start_col,
                pointers_count: self.token_text().display_len(),
            });
            return Err(());
        }
        let literal_text = self.token_text();
        return Ok((literal_text, logical_character));
    }

    fn ascii_literal(&mut self) -> Result<TokenKind<'code>, ()> {
        let (literal_text, ascii_value) = self.ascii_literal_characters()?;
        let literal_index = self.new_token_text(literal_text);
        return Ok(TokenKind::Ascii(literal_index, ascii_value));
    }

    fn str_literal_characters(&mut self) -> Result<&'code str, ()> {
        let previous_errors_len = self.errors.len();

        loop {
            let next_character = match self.current_ascii_singleline() {
                Some(Ok(next_character)) => next_character,
                Some(Err(grapheme)) => {
                    self.push_utf8_error(grapheme);
                    #[expect(clippy::cast_possible_truncation)]
                    {
                        self.col += grapheme.len() as offset32;
                    }
                    continue;
                },
                None => {
                    self.errors.push(Msg {
                        severity: MsgSeverity::NonTerminalError,
                        kind: ErrorKind::UnclosedStrLiteral,
                        col: self.token_start_col,
                        pointers_count: self.token_text().display_len(),
                    });
                    break;
                },
            };
            let start_of_character = self.col;
            self.col += 1;

            match next_character {
                b'\\' => {
                    let _logical_character = match self.escape_sequence(start_of_character) {
                        Character::OkEscaped(ch) => ch,
                        Character::ErrBreak => break,
                        Character::ErrContinue => continue,
                    };
                },
                control @ (b'\x00'..=b'\x1F' | b'\x7F') => {
                    self.errors.push(Msg {
                        severity: MsgSeverity::NonTerminalError,
                        kind: ErrorKind::ControlCharacter(control),
                        col: start_of_character,
                        pointers_count: 1,
                    });
                },
                b'"' => break,
                _ => {},
            }
        }

        if previous_errors_len != self.errors.len() {
            return Err(());
        };
        let literal_text = self.token_text();
        return Ok(literal_text)
    }

    fn str_literal(&mut self) -> Result<TokenKind<'code>, ()> {
        let literal_text = self.str_literal_characters()?;
        let literal_index = self.new_token_text(literal_text);
        return Ok(TokenKind::Str(literal_index));
    }

    fn raw_str_literal_characters(&mut self) -> Result<&'code str, ()> {
        let previous_errors_len = self.errors.len();

        loop {
            let next_character = match self.current_ascii_singleline() {
                Some(Ok(next_character)) => next_character,
                Some(Err(grapheme)) => {
                    self.push_utf8_error(grapheme);
                    #[expect(clippy::cast_possible_truncation)]
                    {
                        self.col += grapheme.len() as offset32;
                    }
                    continue;
                },
                None => {
                    self.errors.push(Msg {
                        severity: MsgSeverity::NonTerminalError,
                        kind: ErrorKind::UnclosedRawStrLiteral,
                        col: self.token_start_col,
                        pointers_count: self.token_text().display_len(),
                    });
                    break;
                },
            };
            let start_of_next_character = self.col;
            self.col += 1;

            match next_character {
                b'\\' => {
                    let escape_character = match self.current_ascii_singleline() {
                        Some(Ok(escape_character)) => escape_character,
                        Some(Err(grapheme)) => {
                            self.push_utf8_error(grapheme);
                            #[expect(clippy::cast_possible_truncation)]
                            {
                                self.col += grapheme.len() as offset32;
                            }
                            continue;
                        },
                        None => {
                            self.errors.push(Msg {
                                severity: MsgSeverity::NonTerminalError,
                                kind: ErrorKind::UnterminatedEscapeCharacter,
                                col: start_of_next_character,
                                pointers_count: self.col - start_of_next_character,
                            });
                            break;
                        },
                    };

                    if escape_character == b'"' {
                        self.col += 1;
                    }
                },
                control @ (b'\x00'..=b'\x1F' | b'\x7F') => {
                    self.errors.push(Msg {
                        severity: MsgSeverity::NonTerminalError,
                        kind: ErrorKind::ControlCharacter(control),
                        col: start_of_next_character,
                        pointers_count: 1,
                    });
                },
                b'"' => break,
                _ => {},
            }
        }

        if previous_errors_len != self.errors.len() {
            return Err(());
        }
        let literal_text = self.token_text();
        return Ok(literal_text)
    }

    fn raw_str_literal(&mut self) -> Result<TokenKind<'code>, ()> {
        let literal_text = self.raw_str_literal_characters()?;
        let literal_index = self.new_token_text(literal_text);
        return Ok(TokenKind::RawStr(literal_index));
    }
}

// tokenization of identifiers
impl<'code> Tokenizer<'code> {
    const MAX_IDENTIFIER_LEN: offset32 = 63;

    fn identifier_str_characters(&mut self) -> Result<&'code str, ()> {
        let previous_errors_len = self.errors.len();

        loop {
            let next_character = match self.current_ascii_singleline() {
                Some(Ok(next_character)) => next_character,
                Some(Err(grapheme)) => {
                    self.push_utf8_error(grapheme);
                    #[expect(clippy::cast_possible_truncation)]
                    {
                        self.col += grapheme.len() as offset32;
                    }
                    continue;
                },
                None => {
                    self.errors.push(Msg {
                        severity: MsgSeverity::NonTerminalError,
                        kind: ErrorKind::UnclosedIdentifierStr,
                        col: self.token_start_col,
                        pointers_count: self.token_text().display_len(),
                    });
                    break;
                },
            };
            let start_of_next_character = self.col;
            self.col += 1;

            match next_character {
                control @ (b'\x00'..=b'\x1F' | b'\x7F') => {
                    self.errors.push(Msg {
                        severity: MsgSeverity::NonTerminalError,
                        kind: ErrorKind::ControlCharacter(control),
                        col: start_of_next_character,
                        pointers_count: 1,
                    });
                },
                b'`' => break,
                _ => {},
            }
        }

        if previous_errors_len != self.errors.len() {
            return Err(());
        }

        let identifier = self.token_text();
        #[expect(clippy::cast_possible_truncation)]
        let identifier_len = identifier.len() as offset32 - 2; // - 2 for the quotes
        if identifier_len > Self::MAX_IDENTIFIER_LEN {
            self.errors.push(Msg {
                severity: MsgSeverity::NonTerminalError,
                kind: ErrorKind::IdentifierTooLong { max: Self::MAX_IDENTIFIER_LEN },
                col: self.token_start_col,
                pointers_count: identifier_len,
            });
            return Err(());
        }

        return Ok(identifier);
    }

    fn identifier_str(&mut self) -> Result<TokenKind<'code>, ()> {
        let literal_text = self.identifier_str_characters()?;
        let literal_index = self.new_token_text(literal_text);
        return Ok(TokenKind::IdentifierStr(literal_index));
    }

    fn identifier(&mut self) -> Result<TokenKind<'code>, ()> {
        let previous_errors_len = self.errors.len();

        while let Some(letter) = self.current_or_until_next_ascii_singleline() {
            match letter {
                b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                    self.col += 1;
                },
                _ => break,
            }
        }

        if previous_errors_len != self.errors.len() {
            return Err(());
        }

        let identifier = match self.token_text() {
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
                if identifier_len > Self::MAX_IDENTIFIER_LEN {
                    self.errors.push(Msg {
                        severity: MsgSeverity::NonTerminalError,
                        kind: ErrorKind::IdentifierTooLong { max: Self::MAX_IDENTIFIER_LEN },
                        col: self.token_start_col,
                        pointers_count: identifier_len,
                    });
                    return Err(());
                }

                let identifier_index = self.new_token_text(identifier);
                TokenKind::Identifier(identifier_index)
            },
        };

        return Ok(identifier);
    }
}

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
#[repr(transparent)]
pub struct Base(pub digit::Base);

#[rustfmt::skip]
impl Base {
    pub const BINARY_LETTER:           &str = "b";
    pub const BINARY_PREFIX:           &str = "0b";

    pub const OCTAL_LETTER:            &str = "o";
    pub const OCTAL_PREFIX:            &str = "0o";

    pub const DECIMAL_LETTER:          &str = "";
    pub const DECIMAL_PREFIX:          &str = "";
    pub const DECIMAL_LETTER_EXTENDED: &str = "d";
    pub const DECIMAL_PREFIX_EXTENDED: &str = "0d";

    pub const HEXADECIMAL_LETTER:      &str = "x";
    pub const HEXADECIMAL_PREFIX:      &str = "0x";

    #[must_use]
    #[inline]
    pub const fn letter(self) -> &'static str {
        return match self.0 {
            digit::Base::Binary      => Self::BINARY_LETTER,
            digit::Base::Octal       => Self::OCTAL_LETTER,
            digit::Base::Decimal     => Self::DECIMAL_LETTER,
            digit::Base::Hexadecimal => Self::HEXADECIMAL_LETTER,
        };
    }

    #[must_use]
    #[inline]
    pub const fn letter_extended(self) -> &'static str {
        return match self.0 {
            digit::Base::Binary      => Self::BINARY_LETTER,
            digit::Base::Octal       => Self::OCTAL_LETTER,
            digit::Base::Decimal     => Self::DECIMAL_LETTER_EXTENDED,
            digit::Base::Hexadecimal => Self::HEXADECIMAL_LETTER,
        };
    }

    #[must_use]
    #[inline]
    pub const fn prefix(self) -> &'static str {
        return match self.0 {
            digit::Base::Binary      => Self::BINARY_PREFIX,
            digit::Base::Octal       => Self::OCTAL_PREFIX,
            digit::Base::Decimal     => Self::DECIMAL_PREFIX,
            digit::Base::Hexadecimal => Self::HEXADECIMAL_PREFIX,
        };
    }

    #[must_use]
    #[inline]
    pub const fn prefix_extended(self) -> &'static str {
        return match self.0 {
            digit::Base::Binary      => Self::BINARY_PREFIX,
            digit::Base::Octal       => Self::OCTAL_PREFIX,
            digit::Base::Decimal     => Self::DECIMAL_PREFIX_EXTENDED,
            digit::Base::Hexadecimal => Self::HEXADECIMAL_PREFIX,
        };
    }

    #[must_use]
    #[inline]
    pub const fn range(self) -> &'static [RangeInclusive<utf32>] {
        return self.0.range_ops();
    }

    #[must_use]
    #[inline]
    pub const fn range_ascii(self) -> &'static [RangeInclusive<ascii>] {
        return self.0.range_ascii_ops();
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
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

    DigitOutOfRange(ascii, Base),

    UnrecognizedEscapeCharacter(ascii),
    UnterminatedEscapeCharacter,
    AsciiBinaryEscapeOverflow,

    UnclosedCharacterLiteral,
    UnclosedStrLiteral,
    UnclosedRawStrLiteral,
    UnclosedIdentifierStr,

    EmptyCharacterLiteral,
    MultipleCharactersInCharacterLiteral,

    IdentifierTooLong { max: offset32 },

    ControlCharacter(ascii),
    Utf8Character { grapheme: &'code str },
    UnrecognizedCharacter(ascii),
    // IDEA(stefano): report this as a warning instead of an error
    // FIX(stefano): change `col` in lines ending in `\r` from `.line_start` to `.col` when pointers
    // in errors are allowed to go past the end of the line, and make `pointers_count: 1`
    // StrayCarriageReturn,
}

impl IntoMsgInfo for ErrorKind<'_> {
    fn info(&self) -> MsgInfo {
        let (error_message, error_cause_message) = match self {
            Self::UnclosedBlockComment => (
                "unclosed block comment".into(),
                "missing closing `*#`".into(),
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

            Self::DigitOutOfRange(digit, base) => (
                "invalid integer literal".into(),
                format!(
                    "digit '{escaped}' ({raw}) is out of the valid range for a base {} number {:?}",
                    base.0 as u8,
                    base.range(),
                    escaped = *digit as utf32,
                    raw = digit,
                ).into(),
            ),

            Self::UnrecognizedEscapeCharacter(unrecognized) => (
                "invalid escape character".into(),
                format!(
                    "unrecognized escape character '{letter}' ({codepoint})",
                    letter = *unrecognized as utf32,
                    codepoint = unrecognized,
                ).into(),
            ),
            Self::UnterminatedEscapeCharacter => (
                "invalid escape character".into(),
                "unterminated escape character".into(),
            ),
            Self::AsciiBinaryEscapeOverflow => (
                "invalid escape character".into(),
                format!("must not be over \\b{:b} (ASCII 127)", b'\x7f').into(),
            ),

            Self::UnclosedCharacterLiteral => (
                "unclosed character literal".into(),
                "missing closing ' quote".into()
            ),
            Self::EmptyCharacterLiteral => (
                "empty character literal".into(),
                "must not be empty".into(),
            ),
            Self::MultipleCharactersInCharacterLiteral => (
                "invalid character literal".into(),
                "must not contain more than one character, if you meant to write a string literal try changing the quotes to \"".into(),
            ),

            Self::UnclosedStrLiteral => (
                "unclosed string literal".into(),
                "missing closing \" quote".into()
            ),

            Self::UnclosedRawStrLiteral => (
                "unclosed raw string literal".into(),
                "missing closing \" quote".into()
            ),

            Self::UnclosedIdentifierStr => (
                "unclosed identifier string".into(),
                "missing closing ` quote".into()
            ),

            Self::IdentifierTooLong { max } => (
                "invalid identifier".into(),
                format!("exceeds the length limit of {max}").into(),
            ),

            Self::ControlCharacter(control_character) => (
                "control characters are not allowed".into(),
                format!(
                    "invalid character '{escaped}' ({raw})",
                    escaped = control_character.escape_ascii(),
                    raw = control_character,
                ).into(),
            ),
            Self::Utf8Character { grapheme } => (
                "utf8 characters are not allowed".into(),
                format!(
                    "invalid character '{letter}' ({codepoint})",
                    letter = grapheme,
                    codepoint = grapheme.escape_unicode(),
                ).into(),
            ),
            Self::UnrecognizedCharacter(unrecognized) => (
                "unrecognized".into(),
                format!(
                    "invalid character '{letter}' ({codepoint})",
                    letter = *unrecognized as utf32,
                    codepoint = unrecognized,
                ).into(),
            ),
            // Self::StrayCarriageReturn => (
            //     "invalid line ending".into(),
            //     "line ends in '\\r' without the following '\\n'".into()
            // )
        };

        return MsgInfo { message: error_message, cause: error_cause_message };
    }
}
