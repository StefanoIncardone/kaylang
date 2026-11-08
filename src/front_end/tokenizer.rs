use super::{IntoMsgInfo, Msg, MsgInfo, src_file::{SrcCode, SrcFile}};
use crate::{error::PointersCount as _, front_end::{Index32, MsgSeverity}};
use back_to_front::{
    ascii, digit, src_code::LineEnd, text, usize32, utf32
};
use core::fmt::Write as _;
use unicode_segmentation::UnicodeSegmentation as _;

macro_rules! ascii_whitespace_common  { () => { b' ' | b'\t' } }
macro_rules! ascii_whitespace_uncommon{ () => { b'\x0B' | b'\x0C' } }
macro_rules! ascii_newlines           { () => { b'\n' | b'\r' } }
macro_rules! ascii_controls           { () => { b'\x00'..=b'\x1F' | b'\x7F' } }
macro_rules! ascii_range              { () => { 0..=b'\x7F' } }
#[expect(unused_macros)] macro_rules! ascii_out_of_range       { () => { b'\x80'.. } }

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[must_use] #[repr(u8)]
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

impl Op {
    #[must_use]
    pub(crate) const fn display_len(self) -> usize32 {
        return match self {
            Self::Len                       => 3,
            Self::Equals                    => 1,
            Self::Not                       => 1,
            Self::NotEquals                 => 2,

            Self::Pow                       => 2,
            Self::WrappingPow               => 3,
            Self::SaturatingPow             => 3,
            Self::PowEquals                 => 3,
            Self::WrappingPowEquals         => 4,
            Self::SaturatingPowEquals       => 4,

            Self::Times                     => 1,
            Self::WrappingTimes             => 2,
            Self::SaturatingTimes           => 2,
            Self::TimesEquals               => 2,
            Self::WrappingTimesEquals       => 3,
            Self::SaturatingTimesEquals     => 3,

            Self::Divide                    => 1,
            Self::WrappingDivide            => 2,
            Self::SaturatingDivide          => 2,
            Self::DivideEquals              => 2,
            Self::WrappingDivideEquals      => 3,
            Self::SaturatingDivideEquals    => 3,

            Self::Remainder                 => 1,
            Self::RemainderEquals           => 2,

            Self::Plus                      => 1,
            Self::WrappingPlus              => 2,
            Self::SaturatingPlus            => 2,
            Self::PlusEquals                => 2,
            Self::WrappingPlusEquals        => 3,
            Self::SaturatingPlusEquals      => 3,

            Self::Minus                     => 1,
            Self::WrappingMinus             => 2,
            Self::SaturatingMinus           => 2,
            Self::MinusEquals               => 2,
            Self::WrappingMinusEquals       => 3,
            Self::SaturatingMinusEquals     => 3,

            Self::And                       => 2,
            Self::AndEquals                 => 3,

            Self::BitAnd                    => 1,
            Self::BitAndEquals              => 2,

            Self::Or                        => 2,
            Self::OrEquals                  => 3,

            Self::BitOr                     => 1,
            Self::BitOrEquals               => 2,

            Self::BitXor                    => 1,
            Self::BitXorEquals              => 2,

            Self::LeftShift                 => 2,
            Self::WrappingLeftShift         => 3,
            Self::SaturatingLeftShift       => 3,
            Self::LeftShiftEquals           => 3,
            Self::WrappingLeftShiftEquals   => 4,
            Self::SaturatingLeftShiftEquals => 4,

            Self::RightShift                => 2,
            Self::RightShiftEquals          => 3,

            Self::LeftRotate                => 3,
            Self::LeftRotateEquals          => 4,
            Self::RightRotate               => 3,
            Self::RightRotateEquals         => 4,

            Self::EqualsEquals              => 2,
            Self::NotEqualsEquals           => 3,
            Self::Greater                   => 1,
            Self::GreaterOrEquals           => 2,
            Self::Less                      => 1,
            Self::LessOrEquals              => 2,
            Self::Compare                   => 3,
        };
    }

    #[must_use]
    pub(crate) const fn to_str(self) -> &'static str {
        #[rustfmt::skip]
        return match self {
            Self::Equals                    => "=",

            Self::Len                       => "len",
            Self::Not                       => "!",
            Self::NotEquals                 => "!=",

            Self::Pow                       =>  "**",
            Self::WrappingPow               => r"**\",
            Self::SaturatingPow             =>  "**|",
            Self::PowEquals                 =>  "**=",
            Self::WrappingPowEquals         => r"**\=",
            Self::SaturatingPowEquals       =>  "**|=",

            Self::Times                     =>  "*",
            Self::WrappingTimes             => r"*\",
            Self::SaturatingTimes           =>  "*|",
            Self::TimesEquals               =>  "*=",
            Self::WrappingTimesEquals       => r"*\=",
            Self::SaturatingTimesEquals     =>  "*|=",

            Self::Divide                    =>  "/",
            Self::WrappingDivide            => r"/\",
            Self::SaturatingDivide          =>  "/|",
            Self::DivideEquals              =>  "/=",
            Self::WrappingDivideEquals      => r"/\=",
            Self::SaturatingDivideEquals    =>  "/|=",

            Self::Remainder                 => "%",
            Self::RemainderEquals           => "%=",

            Self::Plus                      =>  "+",
            Self::WrappingPlus              => r"+\",
            Self::SaturatingPlus            =>  "+|",
            Self::PlusEquals                =>  "+=",
            Self::WrappingPlusEquals        => r"+\=",
            Self::SaturatingPlusEquals      =>  "+|=",

            Self::Minus                     =>  "-",
            Self::WrappingMinus             => r"-\",
            Self::SaturatingMinus           =>  "-|",
            Self::MinusEquals               =>  "-=",
            Self::WrappingMinusEquals       => r"-\=",
            Self::SaturatingMinusEquals     =>  "-|=",

            Self::LeftShift                 =>  "<<",
            Self::WrappingLeftShift         => r"<<\",
            Self::SaturatingLeftShift       =>  "<<|",
            Self::LeftShiftEquals           =>  "<<=",
            Self::WrappingLeftShiftEquals   => r"<<\=",
            Self::SaturatingLeftShiftEquals =>  "<<|=",

            Self::RightShift                =>  ">>",
            Self::RightShiftEquals          =>  ">>=",

            Self::LeftRotate                => "<<<",
            Self::LeftRotateEquals          => "<<<=",
            Self::RightRotate               => ">>>",
            Self::RightRotateEquals         => ">>>=",

            Self::BitAnd                    => "&",
            Self::BitAndEquals              => "&=",

            Self::BitOr                     => "|",
            Self::BitOrEquals               => "|=",

            Self::BitXor                    => "^",
            Self::BitXorEquals              => "^=",

            Self::And                       => "&&",
            Self::AndEquals                 => "&&=",

            Self::Or                        => "||",
            Self::OrEquals                  => "||=",

            Self::Compare                   => "<=>",
            Self::EqualsEquals              => "==",
            Self::NotEqualsEquals           => "!==",
            Self::Greater                   => ">",
            Self::GreaterOrEquals           => ">=",
            Self::Less                      => "<",
            Self::LessOrEquals              => "<=",
        };
    }
}

impl core::fmt::Display for Op {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return f.write_str(self.to_str());
    }
}

#[derive(Clone, Copy)]
#[must_use]
enum EscapeSequence {
    Ok(ascii),
    // REMOVE(stefano): only happens with unterminated escape sequences
    ErrBreak,
    ErrContinue,
}

#[derive(Clone)]
#[must_use]
pub(crate) struct EscapedAscii {
    pub(crate) ch: u8,
}

impl core::fmt::Display for EscapedAscii {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return match self.ch {
            control @ b'\x00'..=b'\x1A' => {
                let caret = (b'@' + control) as utf32;
                write!(f, "\\^{caret}")
            },
            control @ b'\x1B'..=b'\x1F' => {
                let offset = control - b'\x1B';
                let caret = (b'[' + offset) as utf32;
                write!(f, "\\^{caret}")
            },
            b'\x7F' => write!(f, "\\^?"),
            other => f.write_char(other as utf32),
        };
    }
}

pub type Text<'code> = &'code str;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub enum TokenKind {
    LineComment  { text_end: usize32 },
    BlockComment { text_end: usize32 },
    Unexpected   { text_end: usize32 },

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
    DecimalInteger       { text_end: usize32 },
    DecimalIntegerPrefix { text_end: usize32 },
    BinaryInteger        { text_end: usize32 },
    OctalInteger         { text_end: usize32 },
    HexadecimalInteger   { text_end: usize32 },

    Ascii         { text_end: usize32, value: ascii },
    Str           { text_end: usize32 },
    RawStr        { text_end: usize32 },
    IdentifierStr { text_end: usize32 },

    // IDEA(stefano): extract base types from identifiers
    Identifier { text_end: usize32 },

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

// TODO(stefano): implement TokenDebug that resolves texts
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub struct Token {
    pub kind: TokenKind,
    pub col: usize32,
}

impl Token {
    #[must_use]
    pub fn display_len(self, tokens: &Tokens<'_, '_>) -> usize32 {
        return match self.kind {
            TokenKind::OpenRoundBracket
            | TokenKind::CloseRoundBracket
            | TokenKind::OpenSquareBracket
            | TokenKind::CloseSquareBracket
            | TokenKind::OpenCurlyBracket
            | TokenKind::CloseCurlyBracket => 1,

            TokenKind::Colon     => 1,
            TokenKind::SemiColon => 1,
            TokenKind::Comma     => 1,
            TokenKind::Op(op)    => op.display_len(),

            TokenKind::True  => 4,
            TokenKind::False => 5,

            TokenKind::LineComment { text_end }
            | TokenKind::BlockComment { text_end }
            | TokenKind::Unexpected { text_end } => {
                let text = tokens.src.src_file.text(self.col, text_end);
                text.pointers_count()
            },

            TokenKind::DecimalInteger { text_end }
            | TokenKind::DecimalIntegerPrefix { text_end }
            | TokenKind::BinaryInteger { text_end }
            | TokenKind::OctalInteger { text_end }
            | TokenKind::HexadecimalInteger { text_end }

            | TokenKind::Ascii { text_end, .. }
            | TokenKind::Str { text_end }
            | TokenKind::RawStr { text_end }
            | TokenKind::IdentifierStr { text_end }
            | TokenKind::Identifier { text_end } => {
                let text = tokens.src.src_file.text(self.col, text_end);
                text.len() as usize32
            },

            TokenKind::Print    => 5,
            TokenKind::PrintLn  => 7,
            TokenKind::Eprint   => 6,
            TokenKind::EprintLn => 8,

            TokenKind::Let      => 3,
            TokenKind::Var      => 3,
            TokenKind::Do       => 2,
            TokenKind::If       => 2,
            TokenKind::Else     => 4,
            TokenKind::Loop     => 4,
            TokenKind::Break    => 5,
            TokenKind::Continue => 8,
        };
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub struct TokenizationResult<'code, 'path> {
    pub src: SrcCode<'code, 'path>,
    pub tokens: Vec<Token>,
    pub errors: Vec<Msg<ErrorKind>>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub struct Tokens<'code, 'path> {
    pub src: SrcCode<'code, 'path>,
    pub tokens: Vec<Token>,
}

#[derive(Clone, Copy)]
#[must_use] #[repr(C)]
union BackPatch {
    token: Index32<Token>,
    column: usize32,
}

impl core::fmt::Debug for BackPatch {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return f.debug_struct("BackPatch")
            .field("token", unsafe { &self.token })
            .field("column", unsafe { &self.column })
            .finish();
    }
}

#[derive(Clone, Debug)]
#[must_use]
pub struct Tokenizer<'code, 'path> {
    tokens: TokenizationResult<'code, 'path>,
    back_patches: Vec<BackPatch>,

    col: usize32,
    token_start_col: usize32,

    logical_characters_count: usize32,
}

// getters/setters
#[rustfmt::skip]
impl<'code> Tokenizer<'code, '_> {
    #[must_use] #[inline(always)] pub(crate) const fn code(&self) -> &'code str { return self.tokens.src.code(); }
}

impl<'code, 'path> Tokenizer<'code, 'path> {
    // IDEA(stefano): make into an iterator
    pub fn tokenize(src_file: &'code SrcFile<'path>) -> TokenizationResult<'code, 'path> {
        let tokens = TokenizationResult {
            src: SrcCode { src_file, line_ends: Vec::new() },
            tokens: Vec::new(),
            errors: Vec::new()
        };

        if src_file.code.len() == 0 { return tokens; }

        let mut tokenizer = Tokenizer {
            tokens,
            back_patches: Vec::new(),

            col: 0,
            token_start_col: 0,
            logical_characters_count: 0,
        };

        'tokenization: while let Some(next_character) = tokenizer.current_byte() {
            tokenizer.token_start_col = tokenizer.col;

            let kind = match next_character {
                ascii_whitespace_common!() => { tokenizer.col += 1; continue 'tokenization; },
                b'\n' => { tokenizer.new_line_lf(); continue 'tokenization; },
                b'\r' => { tokenizer.new_line_cr(); continue 'tokenization; },
                b'r' => {
                    tokenizer.col += 1;
                    match tokenizer.current_byte() {
                        Some(b'"') => {
                            tokenizer.col += 1;
                            tokenizer.raw_str_literal()
                        },
                        _ => tokenizer.identifier(),
                    }
                }
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                    tokenizer.col += 1;
                    tokenizer.identifier()
                },
                b'0' => {
                    tokenizer.col += 1;
                    match tokenizer.current_byte() {
                        Some(b'b') => {
                            tokenizer.col += 1;
                            match tokenizer.digits_text(digit::Base::Binary) {
                                Ok(()) => TokenKind::BinaryInteger { text_end: tokenizer.col },
                                Err(()) => TokenKind::Unexpected { text_end: tokenizer.col },
                            }
                        },
                        Some(b'o') => {
                            tokenizer.col += 1;
                            match tokenizer.digits_text(digit::Base::Octal) {
                                Ok(()) => TokenKind::OctalInteger { text_end: tokenizer.col },
                                Err(()) => TokenKind::Unexpected { text_end: tokenizer.col },
                            }
                        },
                        Some(b'd') => {
                            tokenizer.col += 1;
                            match tokenizer.digits_text(digit::Base::Decimal) {
                                Ok(()) => TokenKind::DecimalIntegerPrefix { text_end: tokenizer.col },
                                Err(()) => TokenKind::Unexpected { text_end: tokenizer.col },
                            }
                        },
                        Some(b'x') => {
                            tokenizer.col += 1;
                            match tokenizer.digits_text(digit::Base::Hexadecimal) {
                                Ok(()) => TokenKind::HexadecimalInteger { text_end: tokenizer.col },
                                Err(()) => TokenKind::Unexpected { text_end: tokenizer.col },
                            }
                        },
                        Some(_) => match tokenizer.digits_text(digit::Base::Decimal) {
                            Ok(()) => TokenKind::DecimalInteger { text_end: tokenizer.col },
                            Err(()) => TokenKind::Unexpected { text_end: tokenizer.col },
                        }
                        None => TokenKind::DecimalInteger { text_end: tokenizer.col },
                    }
                },
                b'1'..=b'9' => {
                    tokenizer.col += 1;
                    match tokenizer.digits_text(digit::Base::Decimal) {
                        Ok(()) => TokenKind::DecimalInteger { text_end: tokenizer.col },
                        Err(()) => TokenKind::Unexpected { text_end: tokenizer.col },
                    }
                },
                b'\'' => {
                    tokenizer.col += 1;
                    tokenizer.ascii_literal()
                },
                b'"' => {
                    tokenizer.col += 1;
                    tokenizer.str_literal()
                },
                b'`' => {
                    tokenizer.col += 1;
                    tokenizer.identifier_str()
                },
                b'#' => {
                    tokenizer.col += 1;
                    match tokenizer.current_byte() {
                        Some(b'*') => 'comment: {
                            tokenizer.col += 1;
                            let previous_block_comments_token_start_len = tokenizer.back_patches.len();
                            'next_character: loop {
                                let Some(first_ch) = tokenizer.current_byte() else { break 'next_character; };
                                match first_ch {
                                    b'*' => {
                                        tokenizer.col += 1;

                                        let Some(ch) = tokenizer.current_byte() else { break 'next_character; };
                                        match ch {
                                            b'#' => {
                                                tokenizer.col += 1;
                                                let kind = TokenKind::BlockComment { text_end: tokenizer.col };

                                                if tokenizer.back_patches.len() == previous_block_comments_token_start_len {
                                                    break 'comment kind;
                                                }
                                                let Some(token_start_col) = tokenizer.back_patches.pop() else {
                                                    unreachable!("unclosed block comment");
                                                };

                                                // REMOVE(stefano): why keep track of nested comments?
                                                tokenizer.tokens.tokens.push(Token { kind, col: tokenizer.token_start_col });
                                                tokenizer.token_start_col = unsafe { token_start_col.column };
                                            },
                                            b'\n' => tokenizer.new_line_lf(),
                                            b'\r' => tokenizer.new_line_cr(),
                                            _ => tokenizer.col += 1,
                                        }
                                    },
                                    b'#' => {
                                        let comment_start_col = tokenizer.col;
                                        tokenizer.col += 1;

                                        let Some(ch) = tokenizer.current_byte() else { break 'next_character; };
                                        match ch {
                                            b'*' => {
                                                tokenizer.col += 1;
                                                let back_patch = BackPatch { column: tokenizer.token_start_col };
                                                tokenizer.back_patches.push(back_patch);
                                                tokenizer.token_start_col = comment_start_col;
                                            },
                                            b'\n' => tokenizer.new_line_lf(),
                                            b'\r' => tokenizer.new_line_cr(),
                                            _ => tokenizer.col += 1,
                                        }
                                    },
                                    b'\n' => tokenizer.new_line_lf(),
                                    b'\r' => tokenizer.new_line_cr(),
                                    _ => tokenizer.col += 1,
                                }
                            }

                            tokenizer.tokens.errors.push(Msg {
                                severity: MsgSeverity::NonTerminalError,
                                kind: ErrorKind::UnclosedBlockComment,
                                col: tokenizer.token_start_col,
                                pointers_count: 2,
                            });
                            while tokenizer.back_patches.len() != previous_block_comments_token_start_len {
                                let Some(block_comment_token_start) = tokenizer.back_patches.pop() else { break; };

                                tokenizer.tokens.errors.push(Msg {
                                    severity: MsgSeverity::NonTerminalError,
                                    kind: ErrorKind::UnclosedBlockComment,
                                    col: unsafe { block_comment_token_start.column },
                                    pointers_count: 2,
                                });
                            }
                            TokenKind::Unexpected { text_end: tokenizer.col }
                        },
                        Some(b'#') => {
                            tokenizer.col += 1;
                            while !matches!(tokenizer.current_byte(), Some(ascii_newlines!()) | None) {
                                tokenizer.col += 1;
                            }
                            TokenKind::LineComment { text_end: tokenizer.col }
                        },
                        Some(_) | None => {
                            tokenizer.tokens.errors.push(Msg {
                                severity: MsgSeverity::NonTerminalError,
                                kind: ErrorKind::OldStyleLineComment,
                                col: tokenizer.token_start_col,
                                pointers_count: 1,
                            });

                            // continue tokenizing the old style
                            while !matches!(tokenizer.current_byte(), Some(ascii_newlines!()) | None) {
                                tokenizer.col += 1;
                            }
                            TokenKind::Unexpected { text_end: tokenizer.col }
                        },
                    }
                },
                b'(' => {
                    tokenizer.col += 1;
                    let back_patch = BackPatch { token: Index32::new(tokenizer.tokens.tokens.len() as usize32) };
                    tokenizer.back_patches.push(back_patch);
                    TokenKind::OpenRoundBracket
                },
                b')' => 'bracket: {
                    tokenizer.col += 1;

                    let Some(bracket_index) = tokenizer.back_patches.pop() else {
                        tokenizer.unopened_bracket_error(ErrorKind::UnopenedRoundBracket);
                        break 'bracket TokenKind::Unexpected { text_end: tokenizer.col };
                    };

                    let token = unsafe { bracket_index.token };
                    #[expect(clippy::wildcard_enum_match_arm)]
                    match tokenizer.tokens.tokens[token].kind {
                        TokenKind::OpenRoundBracket
                        | TokenKind::CloseRoundBracket
                        | TokenKind::CloseCurlyBracket
                        | TokenKind::CloseSquareBracket => TokenKind::CloseRoundBracket,
                        TokenKind::OpenCurlyBracket => {
                            tokenizer.mismatched_bracket_error(ErrorKind::MismatchedCurlyRoundBracket);
                            TokenKind::Unexpected { text_end: tokenizer.col }
                        },
                        TokenKind::OpenSquareBracket => {
                            tokenizer.mismatched_bracket_error(ErrorKind::MismatchedSquareRoundBracket);
                            TokenKind::Unexpected { text_end: tokenizer.col }
                        },
                        _ => unreachable!("incorrect bracket index"),
                    }
                },
                b'[' => {
                    tokenizer.col += 1;
                    let back_patch = BackPatch { token: Index32::new(tokenizer.tokens.tokens.len() as usize32) };
                    tokenizer.back_patches.push(back_patch);
                    TokenKind::OpenSquareBracket
                },
                b']' => 'bracket: {
                    tokenizer.col += 1;

                    let Some(bracket_index) = tokenizer.back_patches.pop() else {
                        tokenizer.unopened_bracket_error(ErrorKind::UnopenedSquareBracket);
                        break 'bracket TokenKind::Unexpected { text_end: tokenizer.col };
                    };

                    let token = unsafe { bracket_index.token };
                    #[expect(clippy::wildcard_enum_match_arm)]
                    match tokenizer.tokens.tokens[token].kind {
                        TokenKind::OpenSquareBracket
                        | TokenKind::CloseSquareBracket
                        | TokenKind::CloseCurlyBracket
                        | TokenKind::CloseRoundBracket => TokenKind::CloseSquareBracket,
                        TokenKind::OpenCurlyBracket => {
                            tokenizer.mismatched_bracket_error(ErrorKind::MismatchedCurlySquareBracket);
                            TokenKind::Unexpected { text_end: tokenizer.col }
                        },
                        TokenKind::OpenRoundBracket => {
                            tokenizer.mismatched_bracket_error(ErrorKind::MismatchedRoundSquareBracket);
                            TokenKind::Unexpected { text_end: tokenizer.col }
                        },
                        _ => unreachable!("incorrect bracket index"),
                    }
                },
                b'{' => {
                    tokenizer.col += 1;
                    let back_patch = BackPatch { token: Index32::new(tokenizer.tokens.tokens.len() as usize32) };
                    tokenizer.back_patches.push(back_patch);
                    TokenKind::OpenCurlyBracket
                },
                b'}' => 'bracket: {
                    tokenizer.col += 1;

                    let Some(bracket_index) = tokenizer.back_patches.pop() else {
                        tokenizer.unopened_bracket_error(ErrorKind::UnopenedCurlyBracket);
                        break 'bracket TokenKind::Unexpected { text_end: tokenizer.col };
                    };

                    let token = unsafe { bracket_index.token };
                    #[expect(clippy::wildcard_enum_match_arm)]
                    match tokenizer.tokens.tokens[token].kind {
                        TokenKind::OpenCurlyBracket
                        | TokenKind::CloseCurlyBracket
                        | TokenKind::CloseRoundBracket
                        | TokenKind::CloseSquareBracket => TokenKind::CloseCurlyBracket,
                        TokenKind::OpenRoundBracket => {
                            tokenizer.mismatched_bracket_error(ErrorKind::MismatchedRoundCurlyBracket);
                            TokenKind::Unexpected { text_end: tokenizer.col }
                        },
                        TokenKind::OpenSquareBracket => {
                            tokenizer.mismatched_bracket_error(ErrorKind::MismatchedSquareCurlyBracket);
                            TokenKind::Unexpected { text_end: tokenizer.col }
                        },
                        _ => unreachable!("incorrect bracket index"),
                    }
                },
                b':' => {
                    tokenizer.col += 1;
                    TokenKind::Colon
                },
                b';' => {
                    tokenizer.col += 1;
                    TokenKind::SemiColon
                },
                b',' => {
                    tokenizer.col += 1;
                    TokenKind::Comma
                },
                b'!' => {
                    tokenizer.col += 1;
                    match tokenizer.current_byte() {
                        Some(b'=') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    TokenKind::Op(Op::NotEqualsEquals)
                                },
                                _ => TokenKind::Op(Op::NotEquals),
                            }
                        },
                        _ => TokenKind::Op(Op::Not),
                    }
                },
                b'*' => {
                    tokenizer.col += 1;
                    match tokenizer.current_byte() {
                        Some(b'*') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    TokenKind::Op(Op::PowEquals)
                                },
                                Some(b'\\') => {
                                    tokenizer.col += 1;
                                    match tokenizer.current_byte() {
                                        Some(b'=') => {
                                            tokenizer.col += 1;
                                            TokenKind::Op(Op::WrappingPowEquals)
                                        },
                                        _ => TokenKind::Op(Op::WrappingPow),
                                    }
                                },
                                Some(b'|') => {
                                    tokenizer.col += 1;
                                    match tokenizer.current_byte() {
                                        Some(b'=') => {
                                            tokenizer.col += 1;
                                            TokenKind::Op(Op::SaturatingPowEquals)
                                        },
                                        _ => TokenKind::Op(Op::SaturatingPow),
                                    }
                                },
                                _ => TokenKind::Op(Op::Pow),
                            }
                        },
                        Some(b'=') => {
                            tokenizer.col += 1;
                            TokenKind::Op(Op::TimesEquals)
                        },
                        Some(b'\\') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    TokenKind::Op(Op::WrappingTimesEquals)
                                },
                                _ => TokenKind::Op(Op::WrappingTimes),
                            }
                        },
                        Some(b'|') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    TokenKind::Op(Op::SaturatingTimesEquals)
                                },
                                _ => TokenKind::Op(Op::SaturatingTimes),
                            }
                        },
                        Some(b'#') => {
                            tokenizer.col += 1;
                            tokenizer.tokens.errors.push(Msg {
                                severity: MsgSeverity::NonTerminalError,
                                kind: ErrorKind::UnopenedBlockComment,
                                col: tokenizer.token_start_col,
                                pointers_count: 2,
                            });
                            TokenKind::Unexpected { text_end: tokenizer.col }
                        }
                        _ => TokenKind::Op(Op::Times),
                    }
                },
                b'/' => {
                    tokenizer.col += 1;
                    match tokenizer.current_byte() {
                        Some(b'=') => {
                            tokenizer.col += 1;
                            TokenKind::Op(Op::DivideEquals)
                        },
                        Some(b'\\') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    TokenKind::Op(Op::WrappingDivideEquals)
                                },
                                _ => TokenKind::Op(Op::WrappingDivide),
                            }
                        },
                        Some(b'|') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    TokenKind::Op(Op::SaturatingDivideEquals)
                                },
                                _ => TokenKind::Op(Op::SaturatingDivide),
                            }
                        },
                        _ => TokenKind::Op(Op::Divide),
                    }
                }
                b'%' => {
                    tokenizer.col += 1;
                    match tokenizer.current_byte() {
                        Some(b'=') => {
                            tokenizer.col += 1;
                            TokenKind::Op(Op::RemainderEquals)
                        },
                        _ => TokenKind::Op(Op::Remainder),
                    }
                },
                b'+' => {
                    tokenizer.col += 1;
                    match tokenizer.current_byte() {
                        Some(b'=') => {
                            tokenizer.col += 1;
                            TokenKind::Op(Op::PlusEquals)
                        },
                        Some(b'\\') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    TokenKind::Op(Op::WrappingPlusEquals)
                                },
                                _ => TokenKind::Op(Op::WrappingPlus),
                            }
                        },
                        Some(b'|') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    TokenKind::Op(Op::SaturatingPlusEquals)
                                },
                                _ => TokenKind::Op(Op::SaturatingPlus),
                            }
                        },
                        _ => TokenKind::Op(Op::Plus),
                    }
                },
                b'-' => {
                    tokenizer.col += 1;
                    match tokenizer.current_byte() {
                        Some(b'=') => {
                            tokenizer.col += 1;
                            TokenKind::Op(Op::MinusEquals)
                        },
                        Some(b'\\') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    TokenKind::Op(Op::WrappingMinusEquals)
                                },
                                _ => TokenKind::Op(Op::WrappingMinus),
                            }
                        },
                        Some(b'|') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    TokenKind::Op(Op::SaturatingMinusEquals)
                                },
                                _ => TokenKind::Op(Op::SaturatingMinus),
                            }
                        },
                        _ => TokenKind::Op(Op::Minus),
                    }
                },
                b'&' => {
                    tokenizer.col += 1;
                    match tokenizer.current_byte() {
                        Some(b'&') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    TokenKind::Op(Op::AndEquals)
                                },
                                _ => TokenKind::Op(Op::And),
                            }
                        },
                        Some(b'=') => {
                            tokenizer.col += 1;
                            TokenKind::Op(Op::BitAndEquals)
                        },
                        _ => TokenKind::Op(Op::BitAnd),
                    }
                },
                b'^' => {
                    tokenizer.col += 1;
                    match tokenizer.current_byte() {
                        Some(b'=') => {
                            tokenizer.col += 1;
                            TokenKind::Op(Op::BitXorEquals)
                        },
                        _ => TokenKind::Op(Op::BitXor),
                    }
                },
                b'|' => {
                    tokenizer.col += 1;
                    match tokenizer.current_byte() {
                        Some(b'|') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    TokenKind::Op(Op::OrEquals)
                                },
                                _ => TokenKind::Op(Op::Or),
                            }
                        },
                        Some(b'=') => {
                            tokenizer.col += 1;
                            TokenKind::Op(Op::BitOrEquals)
                        },
                        _ => TokenKind::Op(Op::BitOr),
                    }
                },
                b'=' => {
                    tokenizer.col += 1;
                    match tokenizer.current_byte() {
                        Some(b'=') => {
                            tokenizer.col += 1;
                            TokenKind::Op(Op::EqualsEquals)
                        },
                        _ => TokenKind::Op(Op::Equals),
                    }
                },
                b'>' => {
                    tokenizer.col += 1;
                    match tokenizer.current_byte() {
                        Some(b'>') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte() {
                                Some(b'>') => {
                                    tokenizer.col += 1;
                                    match tokenizer.current_byte() {
                                        Some(b'=') => {
                                            tokenizer.col += 1;
                                            TokenKind::Op(Op::RightRotateEquals)
                                        },
                                        _ => TokenKind::Op(Op::RightRotate),
                                    }
                                },
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    TokenKind::Op(Op::RightShiftEquals)
                                },
                                _ => TokenKind::Op(Op::RightShift),
                            }
                        },
                        Some(b'=') => {
                            tokenizer.col += 1;
                            TokenKind::Op(Op::GreaterOrEquals)
                        },
                        _ => TokenKind::Op(Op::Greater),
                    }
                },
                b'<' => {
                    tokenizer.col += 1;
                    match tokenizer.current_byte() {
                        Some(b'<') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte() {
                                Some(b'<') => {
                                    tokenizer.col += 1;
                                    match tokenizer.current_byte() {
                                        Some(b'=') => {
                                            tokenizer.col += 1;
                                            TokenKind::Op(Op::LeftRotateEquals)
                                        },
                                        _ => TokenKind::Op(Op::LeftRotate),
                                    }
                                },
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    TokenKind::Op(Op::LeftShiftEquals)
                                },
                                Some(b'\\') => {
                                    tokenizer.col += 1;
                                    match tokenizer.current_byte() {
                                        Some(b'=') => {
                                            tokenizer.col += 1;
                                            TokenKind::Op(Op::WrappingLeftShiftEquals)
                                        },
                                        _ => TokenKind::Op(Op::WrappingLeftShift),
                                    }
                                },
                                Some(b'|') => {
                                    tokenizer.col += 1;
                                    match tokenizer.current_byte() {
                                        Some(b'=') => {
                                            tokenizer.col += 1;
                                            TokenKind::Op(Op::SaturatingLeftShiftEquals)
                                        },
                                        _ => TokenKind::Op(Op::SaturatingLeftShift),
                                    }
                                },
                                _ => TokenKind::Op(Op::LeftShift),
                            }
                        },
                        Some(b'=') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte() {
                                Some(b'>') => {
                                    tokenizer.col += 1;
                                    TokenKind::Op(Op::Compare)
                                },
                                _ => TokenKind::Op(Op::LessOrEquals),
                            }
                        },
                        _ => TokenKind::Op(Op::Less),
                    }
                },
                ascii_whitespace_uncommon!() => { tokenizer.col += 1; continue 'tokenization; },
                _control @ ascii_controls!() => {
                    tokenizer.tokens.errors.push(Msg {
                        severity: MsgSeverity::NonTerminalError,
                        kind: ErrorKind::ControlCharacter,
                        col: tokenizer.col,
                        pointers_count: 1,
                    });

                    tokenizer.col += 1;
                    TokenKind::Unexpected { text_end: tokenizer.col }
                },
                __unrecognized @ ascii_range!() => {
                    tokenizer.tokens.errors.push(Msg {
                        severity: MsgSeverity::NonTerminalError,
                        kind: ErrorKind::UnrecognizedCharacter,
                        col: tokenizer.col,
                        pointers_count: 1,
                    });

                    tokenizer.col += 1;
                    TokenKind::Unexpected { text_end: tokenizer.col }
                },
                _utf8_ch => {
                    tokenizer.utf8_error();
                    TokenKind::Unexpected { text_end: tokenizer.col }
                },
            };

            tokenizer.tokens.tokens.push(Token { kind, col: tokenizer.token_start_col });
        }

        match tokenizer.code().as_bytes().last() {
            Some(ascii_newlines!()) => {
                // file ended on a newline, a line entry has already been added
            }
            None => {
                // file is empty, no need to add a line entry
            }
            Some(_) => {
                // file ended on a character other than a newline
                tokenizer.tokens.src.line_ends.push(tokenizer.col);
            }
        }

        while let Some(bracket_index) = tokenizer.back_patches.pop() {
            let token = unsafe { bracket_index.token };
            let bracket_token = tokenizer.tokens.tokens[token];

            #[expect(clippy::wildcard_enum_match_arm)]
            let error_kind = match bracket_token.kind {
                TokenKind::OpenRoundBracket => ErrorKind::UnclosedRoundBracket,
                TokenKind::OpenSquareBracket => ErrorKind::UnclosedSquareBracket,
                TokenKind::OpenCurlyBracket => ErrorKind::UnclosedCurlyBracket,
                _ => unreachable!("incorrect bracket index"),
            };

            tokenizer.tokens.errors.push(Msg {
                severity: MsgSeverity::NonTerminalError,
                kind: error_kind,
                col: bracket_token.col,
                pointers_count: 1,
            });
        }

        return tokenizer.tokens;
    }
}

// iteration of characters
impl<'code> Tokenizer<'code, '_> {
    #[inline(always)]
    fn new_line(&mut self, line_end: LineEnd) {
        self.tokens.src.line_ends.push(self.col);
        self.col += line_end as usize32;
    }

    #[inline(always)]
    fn new_line_lf(&mut self) {
        self.new_line(LineEnd::LF);
    }

    #[inline]
    fn new_line_cr(&mut self) {
        match text::byte(self.code(), self.col as usize + 1) {
            Some(b'\n')    => self.new_line(LineEnd::CRLF),
            Some(_) | None => self.new_line(LineEnd::CR),
        }
    }

    #[must_use] #[inline(always)] #[track_caller]
    const fn token_text(&self) -> Text<'code> {
        return text::slice(self.code(), self.token_start_col as usize, self.col as usize);
    }

    #[must_use] #[inline(always)] #[track_caller]
    const fn current_byte(&self) -> Option<u8> {
        return text::byte(self.code(), self.col as usize);
    }
}

// tokenization of numbers, strings and character literals
impl Tokenizer<'_, '_> {
    fn digits_text(&mut self, base: digit::Base) -> Result<(), ()> {
        let previous_errors_len = self.tokens.errors.len();

        let check_fn = match base {
            digit::Base::Binary      => digit::binary::check,
            digit::Base::Octal       => digit::octal::check,
            digit::Base::Decimal     => digit::decimal::check,
            digit::Base::Hexadecimal => digit::hexadecimal::check,
        };

        while let Some(digit) = self.current_byte() {
            match check_fn(digit) {
                digit::NOT_A_DIGIT => match digit {
                    _ascii_ch @ ascii_range!() => break,
                    _utf8_ch => self.utf8_error(),
                }
                digit::OUT_OF_RANGE => {
                    self.tokens.errors.push(Msg {
                        severity: MsgSeverity::NonTerminalError,
                        kind: ErrorKind::DigitOutOfRange(Base(base)),
                        col: self.col,
                        pointers_count: 1,
                    });
                    self.col += 1;
                },
                _ => self.col += 1,
            }
        }

        if previous_errors_len != self.tokens.errors.len() { return Err(()); }
        return Ok(());
    }

    fn ascii_or_str_escape_sequence(&mut self) -> EscapeSequence {
        let start_of_character = self.col;
        self.col += 1; // skipping the backslash

        let Some(current_character) = self.current_byte() else {
            self.tokens.errors.push(Msg {
                severity: MsgSeverity::NonTerminalError,
                kind: ErrorKind::UnterminatedEscapeCharacter,
                col: start_of_character,
                pointers_count: self.col - start_of_character,
            });
            return EscapeSequence::ErrBreak;
        };

        let escaped_character = match current_character {
            b'\\' => {
                self.col += 1;
                self.logical_characters_count += 1;
                b'\\'
            },
            b'\'' => {
                self.col += 1;
                self.logical_characters_count += 1;
                b'\''
            },
            b'"'  => {
                self.col += 1;
                self.logical_characters_count += 1;
                b'\"'
            },
            b'e'  => {
                self.col += 1;
                self.logical_characters_count += 1;
                b'\x1b'
            },
            b'n'  => {
                self.col += 1;
                self.logical_characters_count += 1;
                b'\n'
            },
            b'r'  => {
                self.col += 1;
                self.logical_characters_count += 1;
                b'\r'
            },
            b't'  => {
                self.col += 1;
                self.logical_characters_count += 1;
                b'\t'
            },
            b'0'  => {
                self.col += 1;
                self.logical_characters_count += 1;
                b'\0'
            },
            b'^' => {
                let Some(caret_character) = self.current_byte() else {
                    self.tokens.errors.push(Msg {
                        severity: MsgSeverity::NonTerminalError,
                        kind: ErrorKind::UnterminatedEscapeCharacter,
                        col: start_of_character,
                        pointers_count: self.col - start_of_character,
                    });
                    return EscapeSequence::ErrBreak;
                };
                self.col += 1;

                match caret_character {
                    b'@'..=b'Z' => {
                        self.col += 1;
                        self.logical_characters_count += 1;
                        caret_character - b'@'
                    },
                    b'['..=b'_' => {
                        self.col += 1;
                        self.logical_characters_count += 1;
                        caret_character - b'[' + b'Z' - b'@' + 1
                    },
                    b'?' => {
                        self.col += 1;
                        self.logical_characters_count += 1;
                        b'\x7f'
                    },
                    ascii_newlines!() => {
                        self.tokens.errors.push(Msg {
                            severity: MsgSeverity::NonTerminalError,
                            kind: ErrorKind::UnterminatedEscapeCharacter,
                            col: start_of_character,
                            pointers_count: self.col - start_of_character,
                        });
                        return EscapeSequence::ErrBreak;
                    },
                    _control @ ascii_controls!() => {
                        self.tokens.errors.push(Msg {
                            severity: MsgSeverity::NonTerminalError,
                            kind: ErrorKind::ControlCharacter,
                            col: start_of_character + 2,
                            pointers_count: 1,
                        });
                        self.col += 1;
                        self.logical_characters_count += 1;
                        return EscapeSequence::ErrContinue;
                    },
                    _unrecognized @ ascii_range!() => {
                        self.tokens.errors.push(Msg {
                            severity: MsgSeverity::NonTerminalError,
                            kind: ErrorKind::UnrecognizedEscapeCharacter,
                            col: start_of_character,
                            pointers_count: self.col - start_of_character,
                        });
                        self.col += 1;
                        self.logical_characters_count += 1;
                        return EscapeSequence::ErrContinue;
                    },
                    _utf8_ch => {
                        self.utf8_error();
                        self.logical_characters_count += 1;
                        return EscapeSequence::ErrContinue;
                    },
                }
            },
            ascii_newlines!() => {
                self.tokens.errors.push(Msg {
                    severity: MsgSeverity::NonTerminalError,
                    kind: ErrorKind::UnterminatedEscapeCharacter,
                    col: start_of_character,
                    pointers_count: self.col - start_of_character,
                });
                return EscapeSequence::ErrBreak;
            },
            _control @ ascii_controls!() => {
                self.tokens.errors.push(Msg {
                    severity: MsgSeverity::NonTerminalError,
                    kind: ErrorKind::ControlCharacter,
                    col: start_of_character + 1,
                    pointers_count: 1,
                });
                self.col += 1;
                self.logical_characters_count += 1;
                return EscapeSequence::ErrContinue;
            },
            _unrecognized @ ascii_range!() => {
                self.tokens.errors.push(Msg {
                    severity: MsgSeverity::NonTerminalError,
                    kind: ErrorKind::UnrecognizedEscapeCharacter,
                    col: start_of_character,
                    pointers_count: self.col - start_of_character,
                });
                self.col += 1;
                self.logical_characters_count += 1;
                return EscapeSequence::ErrContinue;
            },
            _utf8_ch => {
                self.utf8_error();
                self.logical_characters_count += 1;
                return EscapeSequence::ErrContinue;
            },
        };

        return EscapeSequence::Ok(escaped_character);
    }

    fn raw_str_escape_sequence(&mut self) -> EscapeSequence {
        let start_of_character = self.col;
        self.col += 1; // skipping the backslash

        let Some(current_character) = self.current_byte() else {
            self.tokens.errors.push(Msg {
                severity: MsgSeverity::NonTerminalError,
                kind: ErrorKind::UnterminatedEscapeCharacter,
                col: start_of_character,
                pointers_count: self.col - start_of_character,
            });
            return EscapeSequence::ErrBreak;
        };

        let escaped_character = match current_character {
            b'"'  => {
                self.col += 1;
                self.logical_characters_count += 1;
                b'\"'
            },
            _ => return EscapeSequence::ErrContinue,
        };

        return EscapeSequence::Ok(escaped_character);
    }

    fn ascii_literal(&mut self) -> TokenKind {
        let previous_errors_len = self.tokens.errors.len();
        self.logical_characters_count = 0;

        let mut logical_character = b'\0';
        loop {
            let Some(next_character) = self.current_byte() else {
                self.tokens.errors.push(Msg {
                    severity: MsgSeverity::NonTerminalError,
                    kind: ErrorKind::UnclosedCharacterLiteral,
                    col: self.token_start_col,
                    pointers_count: self.token_text().pointers_count(),
                });
                break;
            };

            match next_character {
                b'\\' => match self.ascii_or_str_escape_sequence() {
                    EscapeSequence::Ok(ch) => logical_character = ch,
                    EscapeSequence::ErrBreak => break,
                    EscapeSequence::ErrContinue => {},
                },
                b'\'' => {
                    if self.logical_characters_count == 0 {
                        self.tokens.errors.push(Msg {
                            severity: MsgSeverity::NonTerminalError,
                            kind: ErrorKind::EmptyCharacterLiteral,
                            col: self.token_start_col,
                            pointers_count: 2,
                        });
                    }
                    if self.logical_characters_count > 1 {
                        self.tokens.errors.push(Msg {
                            severity: MsgSeverity::NonTerminalError,
                            kind: ErrorKind::MultipleCharactersInCharacterLiteral,
                            col: self.token_start_col,
                            pointers_count: self.token_text().pointers_count(),
                        });
                    }
                    self.col += 1;
                    break;
                },
                ascii_newlines!() => {
                    self.tokens.errors.push(Msg {
                        severity: MsgSeverity::NonTerminalError,
                        kind: ErrorKind::UnclosedCharacterLiteral,
                        col: self.token_start_col,
                        pointers_count: self.token_text().pointers_count(),
                    });
                    self.col += 1;
                    break;
                },
                _control @ ascii_controls!() => {
                    self.tokens.errors.push(Msg {
                        severity: MsgSeverity::NonTerminalError,
                        kind: ErrorKind::ControlCharacter,
                        col: self.col,
                        pointers_count: 1,
                    });
                    self.col += 1;
                    self.logical_characters_count += 1;
                },
                _unrecognized @ ascii_range!() => {
                    self.col += 1;
                    self.logical_characters_count += 1;
                },
                _utf8_ch => {
                    self.utf8_error();
                    self.logical_characters_count += 1;
                },
            }
        }

        if previous_errors_len != self.tokens.errors.len() { return TokenKind::Unexpected { text_end: self.col }; }
        return TokenKind::Ascii { text_end: self.col, value: logical_character };
    }

    fn str_literal(&mut self) -> TokenKind {
        let previous_errors_len = self.tokens.errors.len();

        loop {
            let Some(next_character) = self.current_byte() else {
                self.tokens.errors.push(Msg {
                    severity: MsgSeverity::NonTerminalError,
                    kind: ErrorKind::UnclosedStrLiteral,
                    col: self.token_start_col,
                    pointers_count: self.token_text().pointers_count(),
                });
                break;
            };

            match next_character {
                b'\\' => match self.ascii_or_str_escape_sequence() {
                    EscapeSequence::Ok(_ch) => {},
                    EscapeSequence::ErrBreak => break,
                    EscapeSequence::ErrContinue => {},
                },
                b'"' => {
                    self.col += 1;
                    break;
                },
                ascii_newlines!() => {
                    self.tokens.errors.push(Msg {
                        severity: MsgSeverity::NonTerminalError,
                        kind: ErrorKind::UnclosedStrLiteral,
                        col: self.token_start_col,
                        pointers_count: self.token_text().pointers_count(),
                    });
                    self.col += 1;
                    break;
                }
                _control @ ascii_controls!() => {
                    self.tokens.errors.push(Msg {
                        severity: MsgSeverity::NonTerminalError,
                        kind: ErrorKind::ControlCharacter,
                        col: self.col,
                        pointers_count: 1,
                    });
                    self.col += 1;
                },
                _unrecognized @ ascii_range!() => self.col += 1,
                _utf8_ch => self.utf8_error(),
            }
        }

        if previous_errors_len != self.tokens.errors.len() { return TokenKind::Unexpected { text_end: self.col }; }
        return TokenKind::Str { text_end: self.col };
    }

    fn raw_str_literal(&mut self) -> TokenKind {
        let previous_errors_len = self.tokens.errors.len();

        loop {
            let Some(next_character) = self.current_byte() else {
                self.tokens.errors.push(Msg {
                    severity: MsgSeverity::NonTerminalError,
                    kind: ErrorKind::UnclosedRawStrLiteral,
                    col: self.token_start_col,
                    pointers_count: self.token_text().pointers_count(),
                });
                break;
            };

            match next_character {
                b'\\' => match self.raw_str_escape_sequence() {
                    EscapeSequence::Ok(_ch) => {},
                    EscapeSequence::ErrBreak => break,
                    EscapeSequence::ErrContinue => {},
                },
                b'"' => {
                    self.col += 1;
                    break;
                },
                ascii_newlines!() => {
                    self.tokens.errors.push(Msg {
                        severity: MsgSeverity::NonTerminalError,
                        kind: ErrorKind::UnclosedRawStrLiteral,
                        col: self.token_start_col,
                        pointers_count: self.token_text().pointers_count(),
                    });
                    break;
                }
                _control @ ascii_controls!() => {
                    self.tokens.errors.push(Msg {
                        severity: MsgSeverity::NonTerminalError,
                        kind: ErrorKind::ControlCharacter,
                        col: self.col,
                        pointers_count: 1,
                    });
                    self.col += 1;
                },
                _unrecognized @ ascii_range!() => self.col += 1,
                _utf8_ch => self.utf8_error(),
            }
        }

        if previous_errors_len != self.tokens.errors.len() { return TokenKind::Unexpected { text_end: self.col }; }
        return TokenKind::RawStr { text_end: self.col };
    }
}

// tokenization of identifiers
impl Tokenizer<'_, '_> {
    const MAX_IDENTIFIER_LEN: usize32 = 63;

    fn identifier_str(&mut self) -> TokenKind {
        let previous_errors_len = self.tokens.errors.len();
        self.logical_characters_count = 0;

        loop {
            let Some(next_character) = self.current_byte() else {
                self.tokens.errors.push(Msg {
                    severity: MsgSeverity::NonTerminalError,
                    kind: ErrorKind::UnclosedIdentifierStr,
                    col: self.token_start_col,
                    pointers_count: self.token_text().pointers_count(),
                });
                break;
            };

            match next_character {
                b'`' => {
                    self.col += 1;

                    if self.logical_characters_count > Self::MAX_IDENTIFIER_LEN {
                        self.tokens.errors.push(Msg {
                            severity: MsgSeverity::NonTerminalError,
                            kind: ErrorKind::IdentifierTooLong { max: Self::MAX_IDENTIFIER_LEN },
                            col: self.token_start_col,
                            pointers_count: self.token_text().pointers_count(),
                        });
                    }

                    break;
                },
                ascii_newlines!() => {
                    self.tokens.errors.push(Msg {
                        severity: MsgSeverity::NonTerminalError,
                        kind: ErrorKind::UnclosedIdentifierStr,
                        col: self.token_start_col,
                        pointers_count: self.token_text().pointers_count(),
                    });
                    break;
                }
                _control @ ascii_controls!() => {
                    self.tokens.errors.push(Msg {
                        severity: MsgSeverity::NonTerminalError,
                        kind: ErrorKind::ControlCharacter,
                        col: self.col,
                        pointers_count: 1,
                    });
                    self.logical_characters_count += 1;
                    self.col += 1;
                },
                _unrecognized @ ascii_range!() => {
                    self.logical_characters_count += 1;
                    self.col += 1;
                },
                _utf8_ch => {
                    self.logical_characters_count += 1;
                    self.utf8_error();
                },
            }
        }

        if previous_errors_len != self.tokens.errors.len() { return TokenKind::Unexpected { text_end: self.col }; }
        return TokenKind::IdentifierStr { text_end: self.col };
    }

    fn identifier(&mut self) -> TokenKind {
        let previous_errors_len = self.tokens.errors.len();

        while let Some(letter) = self.current_byte() {
            match letter {
                b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.col += 1,
                _other @ ascii_range!() => break,
                _utf8_ch => self.utf8_error(),
            }
        }

        if previous_errors_len != self.tokens.errors.len() { return TokenKind::Unexpected { text_end: self.col }; }

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
                if identifier.len() > Self::MAX_IDENTIFIER_LEN as usize {
                    self.tokens.errors.push(Msg {
                        severity: MsgSeverity::NonTerminalError,
                        kind: ErrorKind::IdentifierTooLong { max: Self::MAX_IDENTIFIER_LEN },
                        col: self.token_start_col,
                        pointers_count: identifier.len() as usize32,
                    });
                    return TokenKind::Unexpected { text_end: self.col };
                }

                TokenKind::Identifier { text_end: self.col }
            },
        };

        return identifier;
    }
}

// errors
impl Tokenizer<'_, '_> {
    fn utf8_error(&mut self) {
        let rest_of_code = &self.code()[self.col as usize..];
        let mut rest_of_line_graphemes = rest_of_code.graphemes(true);
        let Some(grapheme) = rest_of_line_graphemes.next() else {
            // IDEA(stefano): find next good utf8 character and move column to there
            self.tokens.errors.push(Msg {
                severity: MsgSeverity::NonTerminalError,
                kind: ErrorKind::InvalidUtf8,
                col: self.col,
                pointers_count: 1,
            });
            self.col += 1;
            return;
        };

        let text_end = self.col + grapheme.len() as usize32;
        self.tokens.errors.push(Msg {
            severity: MsgSeverity::NonTerminalError,
            kind: ErrorKind::Utf8Character { text_end },
            col: self.col,
            pointers_count: grapheme.pointers_count(),
        });
        self.col = text_end;
    }

    #[inline] #[track_caller]
    fn unopened_bracket_error(&mut self, kind: ErrorKind) {
        debug_assert!(matches!(
            kind,
            ErrorKind::UnopenedRoundBracket | ErrorKind::UnopenedSquareBracket | ErrorKind::UnopenedCurlyBracket
        ), "invalid token kind");

        self.tokens.errors.push(Msg {
            severity: MsgSeverity::NonTerminalError,
            kind,
            col: self.token_start_col,
            pointers_count: 1,
        });
    }

    #[inline] #[track_caller]
    fn mismatched_bracket_error(&mut self, kind: ErrorKind) {
        debug_assert!(matches!(
            kind,
            ErrorKind::MismatchedRoundCurlyBracket
            | ErrorKind::MismatchedRoundSquareBracket
            | ErrorKind::MismatchedSquareRoundBracket
            | ErrorKind::MismatchedSquareCurlyBracket
            | ErrorKind::MismatchedCurlyRoundBracket
            | ErrorKind::MismatchedCurlySquareBracket
        ), "invalid token kind");

        self.tokens.errors.push(Msg {
            severity: MsgSeverity::NonTerminalError,
            kind,
            col: self.token_start_col,
            pointers_count: 1,
        });
    }
}

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
#[must_use] #[repr(transparent)]
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
}

impl Base {
    #[must_use] #[inline]
    pub const fn letter(self) -> &'static str {
        #[rustfmt::skip]
        return match self.0 {
            digit::Base::Binary      => Self::BINARY_LETTER,
            digit::Base::Octal       => Self::OCTAL_LETTER,
            digit::Base::Decimal     => Self::DECIMAL_LETTER,
            digit::Base::Hexadecimal => Self::HEXADECIMAL_LETTER,
        };
    }

    #[must_use] #[inline]
    pub const fn letter_extended(self) -> &'static str {
        #[rustfmt::skip]
        return match self.0 {
            digit::Base::Binary      => Self::BINARY_LETTER,
            digit::Base::Octal       => Self::OCTAL_LETTER,
            digit::Base::Decimal     => Self::DECIMAL_LETTER_EXTENDED,
            digit::Base::Hexadecimal => Self::HEXADECIMAL_LETTER,
        };
    }

    #[must_use] #[inline]
    pub const fn prefix(self) -> &'static str {
        #[rustfmt::skip]
        return match self.0 {
            digit::Base::Binary      => Self::BINARY_PREFIX,
            digit::Base::Octal       => Self::OCTAL_PREFIX,
            digit::Base::Decimal     => Self::DECIMAL_PREFIX,
            digit::Base::Hexadecimal => Self::HEXADECIMAL_PREFIX,
        };
    }

    #[must_use] #[inline]
    pub const fn prefix_extended(self) -> &'static str {
        #[rustfmt::skip]
        return match self.0 {
            digit::Base::Binary      => Self::BINARY_PREFIX,
            digit::Base::Octal       => Self::OCTAL_PREFIX,
            digit::Base::Decimal     => Self::DECIMAL_PREFIX_EXTENDED,
            digit::Base::Hexadecimal => Self::HEXADECIMAL_PREFIX,
        };
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use] #[repr(u8)]
pub enum OpenBracket {
    Round  = b'(',
    Square = b'[',
    Curly  = b'{',
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use] #[repr(u8)]
pub enum CloseBracket {
    Round  = b')',
    Square = b']',
    Curly  = b'}',
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub enum ErrorKind {
    OldStyleLineComment,
    UnclosedBlockComment,
    UnopenedBlockComment,

    // UnclosedBracket(OpenBracket),
    // UnopenedBracket(CloseBracket),
    // MismatchedBraacket { expected: CloseBracket, actual: CloseBracket },

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

    DigitOutOfRange(Base),

    UnrecognizedEscapeCharacter,
    UnterminatedEscapeCharacter,
    AsciiBinaryEscapeOverflow,

    UnclosedCharacterLiteral,
    UnclosedStrLiteral,
    UnclosedRawStrLiteral,
    UnclosedIdentifierStr,

    EmptyCharacterLiteral,
    MultipleCharactersInCharacterLiteral,

    IdentifierTooLong { max: usize32 },

    ControlCharacter,
    Utf8Character { text_end: usize32 },
    InvalidUtf8,
    UnrecognizedCharacter,
}

impl IntoMsgInfo for ErrorKind {
    fn info(&self, col: usize32, src: &SrcFile<'_>) -> MsgInfo {
        let (error_message, error_cause_message) = match self {
            Self::OldStyleLineComment => (
                "old style line comment".into(),
                "comments now start with '##' instead of '#'".into(),
            ),
            Self::UnclosedBlockComment => (
                "unclosed block comment".into(),
                "missing closing `*#`".into(),
            ),
            Self::UnopenedBlockComment => (
                "unopened block comment".into(),
                "missing opening `#*`".into(),
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

            // IDEA(stefano): group consecutive digit out of range errors together
            Self::DigitOutOfRange(base) => {
                let mut bases_ranges = String::new();
                bases_ranges += "(";

                let mut ranges = base.0.range_utf32().iter();
                let first_range = unsafe { ranges.next().unwrap_unchecked() };
                let mut _ignored = write!(bases_ranges, "{first_range}");

                for range in ranges {
                    _ignored = write!(bases_ranges, ", {range}");
                }
                bases_ranges += ")";

                let digit = src.code().as_bytes()[col as usize];

                (
                    "invalid integer literal".into(),
                    format!(
                        "digit '{escaped}' is out of the valid range for a base {} number {}",
                        base.0 as u8,
                        bases_ranges,
                        escaped = digit as utf32,
                    ).into()
                )
            },

            Self::UnrecognizedEscapeCharacter => {
                let unrecognized = src.code().as_bytes()[col as usize];
                (
                    "invalid escape character".into(),
                    format!(
                        "unrecognized escape character '{letter}' ({codepoint})",
                        letter = unrecognized as utf32,
                        codepoint = unrecognized,
                    ).into(),
                )
            },
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

            Self::ControlCharacter => {
                let ch = src.code().as_bytes()[col as usize];
                (
                    "control characters are not allowed".into(),
                    format!("invalid character '{escaped}' ({ch})", escaped = ch.escape_ascii()).into(),
                )
            },
            Self::Utf8Character { text_end } => {
                let grapheme = src.text(col, *text_end);
                (
                    "utf8 characters are not allowed".into(),
                    format!("invalid character '{grapheme}' ({codepoint})", codepoint = grapheme.escape_unicode()).into(),
                )
            },
            Self::InvalidUtf8 => (
                "invalid utf8 characters".into(),
                "invalid character".into(),
            ),
            Self::UnrecognizedCharacter => {
                let unrecognized = src.code().as_bytes()[col as usize];
                (
                    "unrecognized".into(),
                    format!("invalid character '{letter}' ({unrecognized})", letter = unrecognized as utf32).into(),
                )
            },
        };

        return MsgInfo { message: error_message, cause: error_cause_message };
    }
}
