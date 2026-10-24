use super::{IntoMsgInfo, Msg, MsgInfo, src_file::{SrcCode, SrcFile}};
use crate::{error::DisplayLen as _, front_end::{Index32, MsgSeverity}};
use back_to_front::{
    ascii, digit, src_code::{Line, LineEnd}, text, usize32, utf32
};
use core::fmt::Write as _;
use std::path::Path;
use core::marker::PhantomData;
use unicode_segmentation::UnicodeSegmentation as _;

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

pub(crate) type Text<'code> = &'code str;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub(crate) enum TokenKind {
    LineComment { text_end: usize32 },
    BlockComment { text_end: usize32 },
    Unexpected { text_end: usize32 },

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
    DecimalInteger { text_end: usize32 },
    DecimalIntegerPrefix { text_end: usize32 },
    BinaryInteger { text_end: usize32 },
    OctalInteger { text_end: usize32 },
    HexadecimalInteger { text_end: usize32 },

    Ascii { text_end: usize32, value: ascii },
    Str { text_end: usize32 },
    RawStr { text_end: usize32 },
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
pub(crate) struct Token {
    pub(crate) kind: TokenKind,
    pub(crate) col: usize32,
}

impl Token {
    #[must_use]
    pub(crate) fn display_len(self, src: &SrcFile<'_>) -> usize32 {
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
                let text = src.text(self.col, text_end);
                text.display_len()
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
                let text = src.text(self.col, text_end);
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
#[must_use] #[repr(transparent)]
pub struct Tokens<'code> {
    pub(crate) tokens: Vec<Token>,
    src: PhantomData<Text<'code>>
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub struct TokenizationResult<'code, 'path> {
    pub src: SrcCode<'code, 'path>,
    pub result: Result<Tokens<'code>, Vec<Msg<ErrorKind>>>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub struct TokenizedCode<'code, 'path> {
    pub src: SrcCode<'code, 'path>,
    pub tokens: Tokens<'code>,
}

// getters
#[rustfmt::skip]
impl<'code, 'path> TokenizedCode<'code, 'path> {
    #[must_use] #[inline(always)] pub(crate) const fn path(&self)   -> &'path Path { return self.src.path(); }
    #[must_use] #[inline(always)] pub(crate) const fn code(&self)   -> &'code str  { return self.src.code(); }
                #[inline(always)] pub(crate) const fn lines(&self)  -> &[Line]     { return self.src.lines(); }
                #[inline(always)] pub(crate) const fn tokens(&self) -> &[Token]    { return self.tokens.tokens.as_slice(); }
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
    errors: Vec<Msg<ErrorKind>>,
    tokens: TokenizedCode<'code, 'path>,
    back_patches: Vec<BackPatch>,

    col: usize32,
    line_start_col: usize32,
    token_start_col: usize32,
}

// getters/setters
#[rustfmt::skip]
impl<'code> Tokenizer<'code, '_> {
    #[must_use] #[inline(always)] pub(crate) const fn code(&self) -> &'code str { return self.tokens.src.code(); }
}

impl<'code, 'path> Tokenizer<'code, 'path> {
    // IDEA(stefano): make into an iterator
    pub fn tokenize(src_file: &'code SrcFile<'path>) -> TokenizationResult<'code, 'path> {
        let src = SrcCode { src_file, lines: Vec::new() };
        let tokens = Tokens { tokens: Vec::new(), src: PhantomData };

        if src_file.code.len() == 0 { return TokenizationResult { src, result: Ok(tokens) }; }

        let mut tokenizer = Tokenizer {
            errors: Vec::new(),
            tokens: TokenizedCode { src, tokens },
            back_patches: Vec::new(),

            col: 0,
            line_start_col: 0,
            token_start_col: 0,
        };

        'tokenization: while let Some(next_character) = tokenizer.current_ascii_multiline() {
            let kind = 'next_token: {
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
                            if let Some(b'\n') = text::byte(tokenizer.code(), tokenizer.col as usize + 1) {
                                tokenizer.new_line(LineEnd::CRLF);
                                continue 'tokenization;
                            }

                            tokenizer.new_line(LineEnd::CR);
                            continue 'tokenization;
                        },
                        control @ (b'\x00'..=b'\x1F' | b'\x7F') => {
                            tokenizer.errors.push(Msg {
                                severity: MsgSeverity::NonTerminalError,
                                kind: ErrorKind::ControlCharacter(control),
                                col: tokenizer.col,
                                pointers_count: 1,
                            });

                            tokenizer.col += 1;
                            break 'next_token TokenKind::Unexpected { text_end: tokenizer.col };
                        },
                        other => {
                            tokenizer.token_start_col = tokenizer.col;
                            tokenizer.col += 1;
                            other
                        },
                    },
                    Err(grapheme) => {
                        tokenizer.errors.push(tokenizer.utf8_error(grapheme));
                        tokenizer.col += grapheme.len() as usize32;
                        break 'next_token TokenKind::Unexpected { text_end: tokenizer.col };
                    },
                };

                match next {
                    b'r' => match tokenizer.current_byte_multiline() {
                        Some(b'"') => {
                            tokenizer.col += 1;
                            match tokenizer.raw_str_literal() {
                                Ok(text_end) => TokenKind::RawStr { text_end },
                                Err(text_end) => TokenKind::Unexpected { text_end },
                            }
                        },
                        _ => tokenizer.identifier(),
                    },
                    b'a'..=b'z' | b'A'..=b'Z' | b'_' => tokenizer.identifier(),

                    b'0' => match tokenizer.current_byte_singleline() {
                        None => TokenKind::DecimalInteger { text_end: tokenizer.col },
                        Some(b'b') => {
                            tokenizer.col += 1;
                            match tokenizer.digits_text(digit::Base::Binary) {
                                Ok(text_end) => TokenKind::BinaryInteger { text_end },
                                Err(text_end) => TokenKind::Unexpected { text_end },
                            }
                        },
                        Some(b'o') => {
                            tokenizer.col += 1;
                            match tokenizer.digits_text(digit::Base::Octal) {
                                Ok(text_end) => TokenKind::OctalInteger { text_end },
                                Err(text_end) => TokenKind::Unexpected { text_end },
                            }
                        },
                        Some(b'd') => {
                            tokenizer.col += 1;
                            match tokenizer.digits_text(digit::Base::Decimal) {
                                Ok(text_end) => TokenKind::DecimalIntegerPrefix { text_end },
                                Err(text_end) => TokenKind::Unexpected { text_end },
                            }
                        },
                        Some(b'x') => {
                            tokenizer.col += 1;
                            match tokenizer.digits_text(digit::Base::Hexadecimal) {
                                Ok(text_end) => TokenKind::HexadecimalInteger { text_end },
                                Err(text_end) => TokenKind::Unexpected { text_end },
                            }
                        },
                        Some(_) => match tokenizer.digits_text(digit::Base::Decimal) {
                            Ok(text_end) => TokenKind::DecimalInteger { text_end },
                            Err(text_end) => TokenKind::Unexpected { text_end },
                        }
                    },
                    b'1'..=b'9' => match tokenizer.digits_text(digit::Base::Decimal) {
                        Ok(text_end) => TokenKind::DecimalInteger { text_end },
                        Err(text_end) => TokenKind::Unexpected { text_end },
                    },

                    b'\'' => match tokenizer.ascii_literal() {
                        Ok((text_end, value)) => TokenKind::Ascii { text_end, value },
                        Err(text_end) => TokenKind::Unexpected { text_end },
                    },
                    b'"' => match tokenizer.str_literal() {
                        Ok(text_end) => TokenKind::Str { text_end },
                        Err(text_end) => TokenKind::Unexpected { text_end },
                    },
                    b'`' => match tokenizer.identifier_str() {
                        Ok(text_end) => TokenKind::IdentifierStr { text_end },
                        Err(text_end) => TokenKind::Unexpected { text_end },
                    },

                    b'#' => match tokenizer.get_next_byte_singleline() {
                        Some(b'*') => 'comment: {
                            let previous_block_comments_token_start_len = tokenizer.back_patches.len();
                            'next_character: loop {
                                match tokenizer.get_next_byte_multiline() {
                                    Some(b'*') => match tokenizer.get_next_byte_multiline() {
                                        Some(b'#') => {
                                            let kind = TokenKind::BlockComment { text_end: tokenizer.col };

                                            if tokenizer.back_patches.len() == previous_block_comments_token_start_len {
                                                break 'comment kind;
                                            }
                                            let Some(token_start_col) = tokenizer.back_patches.pop() else {
                                                unreachable!("unclosed block comment");
                                            };

                                            tokenizer.tokens.tokens.tokens.push(Token { kind, col: tokenizer.token_start_col });
                                            tokenizer.token_start_col = unsafe { token_start_col.column };
                                        },
                                        Some(_) => {},
                                        None => break 'next_character,
                                    },
                                    Some(b'#') => {
                                        let comment_start_col = tokenizer.col - 1;
                                        match tokenizer.get_next_byte_multiline() {
                                            Some(b'*') => {
                                                let back_patch = BackPatch { column: tokenizer.token_start_col };
                                                tokenizer.back_patches.push(back_patch);
                                                tokenizer.token_start_col = comment_start_col;
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
                            while tokenizer.back_patches.len() != previous_block_comments_token_start_len {
                                let Some(block_comment_token_start) = tokenizer.back_patches.pop() else { break; };

                                tokenizer.errors.push(Msg {
                                    severity: MsgSeverity::NonTerminalError,
                                    kind: ErrorKind::UnclosedBlockComment,
                                    col: unsafe { block_comment_token_start.column },
                                    pointers_count: 2,
                                });
                            }
                            TokenKind::Unexpected { text_end: tokenizer.col }
                        },
                        Some(b'#') => {
                            while let Some(_) = tokenizer.get_next_byte_singleline() {
                                // consume next character
                            }
                            TokenKind::LineComment { text_end: tokenizer.col }
                        },
                        Some(_) | None => {
                            tokenizer.errors.push(Msg {
                                severity: MsgSeverity::NonTerminalError,
                                kind: ErrorKind::OldStyleLineComment,
                                col: tokenizer.token_start_col,
                                pointers_count: 1,
                            });
                            // continue tokenizing the old style
                            while let Some(_) = tokenizer.get_next_byte_singleline() {
                                // consume next character
                            }
                            TokenKind::Unexpected { text_end: tokenizer.col }
                        },
                    },

                    b'(' => {
                        let back_patch = BackPatch { token: Index32::new(tokenizer.tokens.tokens().len()) };
                        tokenizer.back_patches.push(back_patch);
                        TokenKind::OpenRoundBracket
                    },
                    b')' => 'bracket: {
                        let Some(bracket_index) = tokenizer.back_patches.pop() else {
                            tokenizer.errors.push(Msg {
                                severity: MsgSeverity::NonTerminalError,
                                kind: ErrorKind::UnopenedRoundBracket,
                                col: tokenizer.token_start_col,
                                pointers_count: 1,
                            });
                            break 'bracket TokenKind::Unexpected { text_end: tokenizer.col };
                        };

                        let token = unsafe { bracket_index.token };
                        #[expect(clippy::wildcard_enum_match_arm)]
                        match tokenizer.tokens.tokens()[token].kind {
                            TokenKind::OpenRoundBracket
                            | TokenKind::CloseRoundBracket
                            | TokenKind::CloseCurlyBracket
                            | TokenKind::CloseSquareBracket => TokenKind::CloseRoundBracket,
                            TokenKind::OpenCurlyBracket => {
                                tokenizer.errors.push(Msg {
                                    severity: MsgSeverity::NonTerminalError,
                                    kind: ErrorKind::MismatchedCurlyRoundBracket,
                                    col: tokenizer.token_start_col,
                                    pointers_count: 1,
                                });
                                TokenKind::Unexpected { text_end: tokenizer.col }
                            },
                            TokenKind::OpenSquareBracket => {
                                tokenizer.errors.push(Msg {
                                    severity: MsgSeverity::NonTerminalError,
                                    kind: ErrorKind::MismatchedSquareRoundBracket,
                                    col: tokenizer.token_start_col,
                                    pointers_count: 1,
                                });
                                TokenKind::Unexpected { text_end: tokenizer.col }
                            },
                            _ => unreachable!("incorrect bracket index"),
                        }
                    },
                    b'[' => {
                        let back_patch = BackPatch { token: Index32::new(tokenizer.tokens.tokens().len()) };
                        tokenizer.back_patches.push(back_patch);
                        TokenKind::OpenSquareBracket
                    },
                    b']' => 'bracket: {
                        let Some(bracket_index) = tokenizer.back_patches.pop() else {
                            tokenizer.errors.push(Msg {
                                severity: MsgSeverity::NonTerminalError,
                                kind: ErrorKind::UnopenedSquareBracket,
                                col: tokenizer.token_start_col,
                                pointers_count: 1,
                            });
                            break 'bracket TokenKind::Unexpected { text_end: tokenizer.col };
                        };

                        let token = unsafe { bracket_index.token };
                        #[expect(clippy::wildcard_enum_match_arm)]
                        match tokenizer.tokens.tokens()[token].kind {
                            TokenKind::OpenSquareBracket
                            | TokenKind::CloseSquareBracket
                            | TokenKind::CloseCurlyBracket
                            | TokenKind::CloseRoundBracket => TokenKind::CloseSquareBracket,
                            TokenKind::OpenCurlyBracket => {
                                tokenizer.errors.push(Msg {
                                    severity: MsgSeverity::NonTerminalError,
                                    kind: ErrorKind::MismatchedCurlySquareBracket,
                                    col: tokenizer.token_start_col,
                                    pointers_count: 1,
                                });
                                TokenKind::Unexpected { text_end: tokenizer.col }
                            },
                            TokenKind::OpenRoundBracket => {
                                tokenizer.errors.push(Msg {
                                    severity: MsgSeverity::NonTerminalError,
                                    kind: ErrorKind::MismatchedRoundSquareBracket,
                                    col: tokenizer.token_start_col,
                                    pointers_count: 1,
                                });
                                TokenKind::Unexpected { text_end: tokenizer.col }
                            },
                            _ => unreachable!("incorrect bracket index"),
                        }
                    },
                    b'{' => {
                        let back_patch = BackPatch { token: Index32::new(tokenizer.tokens.tokens().len()) };
                        tokenizer.back_patches.push(back_patch);
                        TokenKind::OpenCurlyBracket
                    },
                    b'}' => 'bracket: {
                        let Some(bracket_index) = tokenizer.back_patches.pop() else {
                            tokenizer.errors.push(Msg {
                                severity: MsgSeverity::NonTerminalError,
                                kind: ErrorKind::UnopenedCurlyBracket,
                                col: tokenizer.token_start_col,
                                pointers_count: 1,
                            });
                            break 'bracket TokenKind::Unexpected { text_end: tokenizer.col };
                        };

                        let token = unsafe { bracket_index.token };
                        #[expect(clippy::wildcard_enum_match_arm)]
                        match tokenizer.tokens.tokens()[token].kind {
                            TokenKind::OpenCurlyBracket
                            | TokenKind::CloseCurlyBracket
                            | TokenKind::CloseRoundBracket
                            | TokenKind::CloseSquareBracket => TokenKind::CloseCurlyBracket,
                            TokenKind::OpenRoundBracket => {
                                tokenizer.errors.push(Msg {
                                    severity: MsgSeverity::NonTerminalError,
                                    kind: ErrorKind::MismatchedRoundCurlyBracket,
                                    col: tokenizer.token_start_col,
                                    pointers_count: 1,
                                });
                                TokenKind::Unexpected { text_end: tokenizer.col }
                            },
                            TokenKind::OpenSquareBracket => {
                                tokenizer.errors.push(Msg {
                                    severity: MsgSeverity::NonTerminalError,
                                    kind: ErrorKind::MismatchedSquareCurlyBracket,
                                    col: tokenizer.token_start_col,
                                    pointers_count: 1,
                                });
                                TokenKind::Unexpected { text_end: tokenizer.col }
                            },
                            _ => unreachable!("incorrect bracket index"),
                        }
                    },

                    b':' => TokenKind::Colon,
                    b';' => TokenKind::SemiColon,
                    b',' => TokenKind::Comma,

                    b'!' => match tokenizer.current_byte_multiline() {
                        Some(b'=') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte_multiline() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    TokenKind::Op(Op::NotEqualsEquals)
                                },
                                _ => TokenKind::Op(Op::NotEquals),
                            }
                        },
                        _ => TokenKind::Op(Op::Not),
                    },
                    b'*' => match tokenizer.current_byte_multiline() {
                        Some(b'*') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte_multiline() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    TokenKind::Op(Op::PowEquals)
                                },
                                Some(b'\\') => {
                                    tokenizer.col += 1;
                                    match tokenizer.current_byte_multiline() {
                                        Some(b'=') => {
                                            tokenizer.col += 1;
                                            TokenKind::Op(Op::WrappingPowEquals)
                                        },
                                        _ => TokenKind::Op(Op::WrappingPow),
                                    }
                                },
                                Some(b'|') => {
                                    tokenizer.col += 1;
                                    match tokenizer.current_byte_multiline() {
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
                            match tokenizer.current_byte_multiline() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    TokenKind::Op(Op::WrappingTimesEquals)
                                },
                                _ => TokenKind::Op(Op::WrappingTimes),
                            }
                        },
                        Some(b'|') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte_multiline() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    TokenKind::Op(Op::SaturatingTimesEquals)
                                },
                                _ => TokenKind::Op(Op::SaturatingTimes),
                            }
                        },
                        Some(b'#') => {
                            tokenizer.col += 1;
                            tokenizer.errors.push(Msg {
                                severity: MsgSeverity::NonTerminalError,
                                kind: ErrorKind::UnopenedBlockComment,
                                col: tokenizer.token_start_col,
                                pointers_count: 2,
                            });
                            TokenKind::Unexpected { text_end: tokenizer.col }
                        }
                        _ => TokenKind::Op(Op::Times),
                    },
                    b'/' => match tokenizer.current_byte_multiline() {
                        Some(b'=') => {
                            tokenizer.col += 1;
                            TokenKind::Op(Op::DivideEquals)
                        },
                        Some(b'\\') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte_multiline() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    TokenKind::Op(Op::WrappingDivideEquals)
                                },
                                _ => TokenKind::Op(Op::WrappingDivide),
                            }
                        },
                        Some(b'|') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte_multiline() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    TokenKind::Op(Op::SaturatingDivideEquals)
                                },
                                _ => TokenKind::Op(Op::SaturatingDivide),
                            }
                        },
                        _ => TokenKind::Op(Op::Divide),
                    },
                    b'%' => match tokenizer.current_byte_multiline() {
                        Some(b'=') => {
                            tokenizer.col += 1;
                            TokenKind::Op(Op::RemainderEquals)
                        },
                        _ => TokenKind::Op(Op::Remainder),
                    },
                    b'+' => match tokenizer.current_byte_multiline() {
                        Some(b'=') => {
                            tokenizer.col += 1;
                            TokenKind::Op(Op::PlusEquals)
                        },
                        Some(b'\\') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte_multiline() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    TokenKind::Op(Op::WrappingPlusEquals)
                                },
                                _ => TokenKind::Op(Op::WrappingPlus),
                            }
                        },
                        Some(b'|') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte_multiline() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    TokenKind::Op(Op::SaturatingPlusEquals)
                                },
                                _ => TokenKind::Op(Op::SaturatingPlus),
                            }
                        },
                        _ => TokenKind::Op(Op::Plus),
                    },
                    b'-' => match tokenizer.current_byte_multiline() {
                        Some(b'=') => {
                            tokenizer.col += 1;
                            TokenKind::Op(Op::MinusEquals)
                        },
                        Some(b'\\') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte_multiline() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    TokenKind::Op(Op::WrappingMinusEquals)
                                },
                                _ => TokenKind::Op(Op::WrappingMinus),
                            }
                        },
                        Some(b'|') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte_multiline() {
                                Some(b'=') => {
                                    tokenizer.col += 1;
                                    TokenKind::Op(Op::SaturatingMinusEquals)
                                },
                                _ => TokenKind::Op(Op::SaturatingMinus),
                            }
                        },
                        _ => TokenKind::Op(Op::Minus),
                    },
                    b'&' => match tokenizer.current_byte_multiline() {
                        Some(b'&') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte_multiline() {
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
                    },
                    b'^' => match tokenizer.current_byte_multiline() {
                        Some(b'=') => {
                            tokenizer.col += 1;
                            TokenKind::Op(Op::BitXorEquals)
                        },
                        _ => TokenKind::Op(Op::BitXor),
                    },
                    b'|' => match tokenizer.current_byte_multiline() {
                        Some(b'|') => {
                            tokenizer.col += 1;
                            match tokenizer.current_byte_multiline() {
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
                    },
                    b'=' => match tokenizer.current_byte_multiline() {
                        Some(b'=') => {
                            tokenizer.col += 1;
                            TokenKind::Op(Op::EqualsEquals)
                        },
                        _ => TokenKind::Op(Op::Equals),
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
                                    match tokenizer.current_byte_multiline() {
                                        Some(b'=') => {
                                            tokenizer.col += 1;
                                            TokenKind::Op(Op::WrappingLeftShiftEquals)
                                        },
                                        _ => TokenKind::Op(Op::WrappingLeftShift),
                                    }
                                },
                                Some(b'|') => {
                                    tokenizer.col += 1;
                                    match tokenizer.current_byte_multiline() {
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
                            match tokenizer.current_byte_multiline() {
                                Some(b'>') => {
                                    tokenizer.col += 1;
                                    TokenKind::Op(Op::Compare)
                                },
                                _ => TokenKind::Op(Op::LessOrEquals),
                            }
                        },
                        _ => TokenKind::Op(Op::Less),
                    },
                    unrecognized => {
                        tokenizer.errors.push(Msg {
                            severity: MsgSeverity::NonTerminalError,
                            kind: ErrorKind::UnrecognizedCharacter(unrecognized),
                            col: tokenizer.token_start_col,
                            pointers_count: 1,
                        });
                        TokenKind::Unexpected { text_end: tokenizer.col }
                    },
                }
            };

            tokenizer.tokens.tokens.tokens.push(Token { kind, col: tokenizer.token_start_col });
        }

        if tokenizer.line_start_col != tokenizer.col {
            // file ended without a newline
            let line = unsafe { Line::new_unchecked(tokenizer.line_start_col, tokenizer.col) };
            tokenizer.tokens.src.lines.push(line);
        }

        while let Some(bracket_index) = tokenizer.back_patches.pop() {
            let token = unsafe { bracket_index.token };
            // there can only be open brackets at this point
            let bracket_token = tokenizer.tokens.tokens()[token];

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

        let result = if tokenizer.errors.len() == 0 {
            Ok(tokenizer.tokens.tokens)
        } else {
            Err(tokenizer.errors)
        };
        return TokenizationResult { src: tokenizer.tokens.src, result };
    }
}

// iteration of characters
impl<'code> Tokenizer<'code, '_> {
    #[inline]
    fn new_line(&mut self, line_end: LineEnd) {
        let line = unsafe { Line::new_unchecked(self.line_start_col, self.col) };
        self.tokens.src.lines.push(line);

        self.col += line_end as usize32;
        self.line_start_col = self.col;
    }

    #[must_use] #[inline(always)] #[track_caller]
    const fn token_text(&self) -> Text<'code> {
        return text::slice(self.code(), self.token_start_col as usize, self.col as usize);
    }

    #[must_use] #[inline]
    const fn current_byte_multiline(&self) -> Option<u8> {
        return text::byte(self.code(), self.col as usize);
    }

    #[must_use]
    const fn current_byte_singleline(&self) -> Option<u8> {
        let Some(next) = self.current_byte_multiline() else { return None; };
        return match next {
            b'\r' | b'\n' => None,
            other => Some(other),
        };
    }

    #[must_use]
    fn get_next_byte_multiline(&mut self) -> Option<u8> {
        let Some(next) = self.current_byte_multiline() else { return None; };
        return match next {
            b'\n' => {
                self.new_line(LineEnd::LF);
                Some(b'\n')
            },
            b'\r' => {
                if let Some(b'\n') = text::byte(self.code(), self.col as usize + 1) {
                    self.new_line(LineEnd::CRLF);
                    return Some(b'\n');
                }

                self.new_line(LineEnd::CR);
                Some(b'\n')
            },
            other => {
                self.col += 1;
                Some(other)
            },
        };
    }

    #[must_use]
    fn current_grapheme(&self) -> &'code str {
        let rest_of_code = &self.code()[self.col as usize..];
        let mut rest_of_line_graphemes = rest_of_code.graphemes(true);
        // TODO(stefano): report invalid utf8 errors
        let Some(grapheme) = rest_of_line_graphemes.next() else { unreachable!("not a valid grapheme"); };
        return grapheme;
    }

    #[must_use]
    const fn get_next_byte_singleline(&mut self) -> Option<u8> {
        let Some(next) = self.current_byte_multiline() else { return None; };
        return match next {
            b'\r' | b'\n' => None,
            other => {
                self.col += 1;
                Some(other)
            },
        };
    }

    #[must_use]
    fn current_ascii_multiline(&self) -> Option<Result<ascii, &'code str>> {
        let Some(next) = self.current_byte_multiline() else { return None; };
        return match next {
            ascii_ch @ 0..=b'\x7F' => Some(Ok(ascii_ch)),
            _utf8_ch => Some(Err(self.current_grapheme())),
        };
    }

    #[must_use]
    fn current_ascii_singleline(&self) -> Option<Result<ascii, &'code str>> {
        let Some(next) = self.current_byte_singleline() else { return None; };
        return match next {
            ascii_ch @ 0..=b'\x7F' => Some(Ok(ascii_ch)),
            _utf8_ch => Some(Err(self.current_grapheme())),
        };
    }

    #[must_use]
    fn current_or_until_next_ascii_singleline(&mut self) -> Option<ascii> {
        loop {
            let Some(next) = self.current_byte_singleline() else { return None; };
            match next {
                ascii_ch @ 0..=b'\x7F' => return Some(ascii_ch),
                _utf8_ch => {
                    let grapheme = self.current_grapheme();
                    self.errors.push(self.utf8_error(grapheme));
                    self.col += grapheme.len() as usize32;
                },
            }
        }
    }

    #[inline(always)]
    fn utf8_error(&self, grapheme: &'code str) -> Msg<ErrorKind> {
        let text_end = self.col + grapheme.len() as usize32;
        return Msg {
            severity: MsgSeverity::NonTerminalError,
            kind: ErrorKind::Utf8Character { text_end },
            col: self.col,
            pointers_count: grapheme.display_len(),
        };
    }
}

// tokenization of numbers, strings and character literals
impl Tokenizer<'_, '_> {
    fn digits_text(&mut self, base: digit::Base) -> Result<usize32, usize32> {
        let previous_errors_len = self.errors.len();

        let check_fn = match base {
            digit::Base::Binary => digit::binary::check,
            digit::Base::Octal => digit::octal::check,
            digit::Base::Decimal => digit::decimal::check,
            digit::Base::Hexadecimal => digit::hexadecimal::check,
        };

        while let Some(digit) = self.current_or_until_next_ascii_singleline() {
            match check_fn(digit) {
                digit::NOT_A_DIGIT => break,
                digit::OUT_OF_RANGE => self.errors.push(Msg {
                    severity: MsgSeverity::NonTerminalError,
                    kind: ErrorKind::DigitOutOfRange(digit, Base(base)),
                    col: self.col,
                    pointers_count: 1,
                }),
                _ => {},
            }
            self.col += 1;
        }

        if previous_errors_len != self.errors.len() { return Err(self.col); }
        return Ok(self.col);
    }

    fn escape_sequence(&mut self, start_of_character: usize32) -> EscapeSequence {
        let current_character = match self.current_ascii_singleline() {
            Some(Ok(current_character)) => current_character,
            Some(Err(grapheme)) => {
                self.errors.push(self.utf8_error(grapheme));
                self.col += grapheme.len() as usize32;
                return EscapeSequence::ErrContinue;
            },
            None => {
                self.errors.push(Msg {
                    severity: MsgSeverity::NonTerminalError,
                    kind: ErrorKind::UnterminatedEscapeCharacter,
                    col: start_of_character,
                    pointers_count: self.col - start_of_character,
                });
                return EscapeSequence::ErrBreak;
            },
        };
        self.col += 1;

        let escaped_character = match current_character {
            b'\\' => b'\\',
            b'\'' => b'\'',
            b'"' => b'\"',
            b'e' => b'\x1b',
            b'n' => b'\n',
            b'r' => b'\r',
            b't' => b'\t',
            b'0' => b'\0',
            b'^' => {
                let caret_character = match self.current_ascii_singleline() {
                    Some(Ok(escape_character)) => escape_character,
                    Some(Err(grapheme)) => {
                        self.errors.push(self.utf8_error(grapheme));
                        self.col += grapheme.len() as usize32;
                        return EscapeSequence::ErrContinue;
                    },
                    None => {
                        self.errors.push(Msg {
                            severity: MsgSeverity::NonTerminalError,
                            kind: ErrorKind::UnterminatedEscapeCharacter,
                            col: start_of_character,
                            pointers_count: self.col - start_of_character,
                        });
                        return EscapeSequence::ErrBreak;
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
                        return EscapeSequence::ErrContinue;
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
                return EscapeSequence::ErrContinue;
            },
            unrecognized => {
                self.errors.push(Msg {
                    severity: MsgSeverity::NonTerminalError,
                    kind: ErrorKind::UnrecognizedEscapeCharacter(unrecognized),
                    col: start_of_character,
                    pointers_count: 2,
                });
                return EscapeSequence::ErrContinue;
            },
        };

        return EscapeSequence::Ok(escaped_character);
    }

    fn ascii_literal(&mut self) -> Result<(usize32, ascii), usize32> {
        let previous_errors_len = self.errors.len();

        let mut logical_character = b'\0';
        let mut logical_characters_count = 0;
        loop {
            let next_character = match self.current_ascii_singleline() {
                Some(Ok(next_character)) => next_character,
                Some(Err(grapheme)) => {
                    self.errors.push(self.utf8_error(grapheme));
                    self.col += grapheme.len() as usize32;
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
                        EscapeSequence::Ok(ch) => ch,
                        EscapeSequence::ErrBreak => break,
                        EscapeSequence::ErrContinue => continue,
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
            return Err(self.col);
        }

        if logical_characters_count == 0 {
            self.errors.push(Msg {
                severity: MsgSeverity::NonTerminalError,
                kind: ErrorKind::EmptyCharacterLiteral,
                col: self.token_start_col,
                pointers_count: 2,
            });
            return Err(self.col);
        }
        if logical_characters_count > 1 {
            self.errors.push(Msg {
                severity: MsgSeverity::NonTerminalError,
                kind: ErrorKind::MultipleCharactersInCharacterLiteral,
                col: self.token_start_col,
                pointers_count: self.token_text().display_len(),
            });
            return Err(self.col);
        }
        return Ok((self.col, logical_character));
    }

    fn str_literal(&mut self) -> Result<usize32, usize32> {
        let previous_errors_len = self.errors.len();

        loop {
            let next_character = match self.current_ascii_singleline() {
                Some(Ok(next_character)) => next_character,
                Some(Err(grapheme)) => {
                    self.errors.push(self.utf8_error(grapheme));
                    self.col += grapheme.len() as usize32;
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
                        EscapeSequence::Ok(ch) => ch,
                        EscapeSequence::ErrBreak => break,
                        EscapeSequence::ErrContinue => continue,
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

        if previous_errors_len != self.errors.len() { return Err(self.col); }
        return Ok(self.col);
    }

    fn raw_str_literal(&mut self) -> Result<usize32, usize32> {
        let previous_errors_len = self.errors.len();

        loop {
            let next_character = match self.current_ascii_singleline() {
                Some(Ok(next_character)) => next_character,
                Some(Err(grapheme)) => {
                    self.errors.push(self.utf8_error(grapheme));
                    self.col += grapheme.len() as usize32;
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
                            self.errors.push(self.utf8_error(grapheme));
                            self.col += grapheme.len() as usize32;
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

        if previous_errors_len != self.errors.len() { return Err(self.col); }
        return Ok(self.col);
    }
}

// tokenization of identifiers
impl Tokenizer<'_, '_> {
    const MAX_IDENTIFIER_LEN: usize32 = 63;

    fn identifier_str(&mut self) -> Result<usize32, usize32> {
        let previous_errors_len = self.errors.len();

        loop {
            let next_character = match self.current_ascii_singleline() {
                Some(Ok(next_character)) => next_character,
                Some(Err(grapheme)) => {
                    self.errors.push(self.utf8_error(grapheme));
                    self.col += grapheme.len() as usize32;
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

        if previous_errors_len != self.errors.len() { return Err(self.col); }

        let identifier_len = self.col - self.token_start_col - 2; // - 2 for the quotes
        if identifier_len > Self::MAX_IDENTIFIER_LEN {
            self.errors.push(Msg {
                severity: MsgSeverity::NonTerminalError,
                kind: ErrorKind::IdentifierTooLong { max: Self::MAX_IDENTIFIER_LEN },
                col: self.token_start_col,
                pointers_count: identifier_len,
            });
            return Err(self.col);
        }

        return Ok(self.col);
    }

    fn identifier(&mut self) -> TokenKind {
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
            return TokenKind::Unexpected { text_end: self.col };
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
                let identifier_len = identifier.len() as usize32;
                if identifier_len > Self::MAX_IDENTIFIER_LEN {
                    self.errors.push(Msg {
                        severity: MsgSeverity::NonTerminalError,
                        kind: ErrorKind::IdentifierTooLong { max: Self::MAX_IDENTIFIER_LEN },
                        col: self.token_start_col,
                        pointers_count: identifier_len,
                    });
                    return TokenKind::Unexpected { text_end: self.col };
                }

                TokenKind::Identifier { text_end: self.col }
            },
        };

        return identifier;
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
#[must_use]
pub enum ErrorKind {
    OldStyleLineComment,
    UnclosedBlockComment,
    UnopenedBlockComment,

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

    IdentifierTooLong { max: usize32 },

    ControlCharacter(ascii),
    Utf8Character { text_end: usize32 },
    UnrecognizedCharacter(ascii),
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

            Self::DigitOutOfRange(digit, base) => {
                let mut bases_ranges = String::new();
                bases_ranges += "(";

                let mut ranges = base.0.range_utf32().iter();
                let first_range = unsafe { ranges.next().unwrap_unchecked() };
                let mut _ignored = write!(bases_ranges, "{first_range}");

                for range in ranges {
                    _ignored = write!(bases_ranges, ", {range}");
                }
                bases_ranges += ")";

                (
                    "invalid integer literal".into(),
                    format!(
                        "digit '{escaped}' is out of the valid range for a base {} number {}",
                        base.0 as u8,
                        bases_ranges,
                        escaped = *digit as utf32,
                    ).into()
                )
            },

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

            Self::ControlCharacter(ch) => (
                "control characters are not allowed".into(),
                format!("invalid character '{escaped}' ({ch})", escaped = ch.escape_ascii()).into(),
            ),
            Self::Utf8Character { text_end } => {
                let grapheme = src.text(col, *text_end);
                (
                    "utf8 characters are not allowed".into(),
                    format!("invalid character '{grapheme}' ({codepoint})", codepoint = grapheme.escape_unicode()).into(),
                )
            },
            Self::UnrecognizedCharacter(unrecognized) => (
                "unrecognized".into(),
                format!("invalid character '{letter}' ({unrecognized})", letter = *unrecognized as utf32).into(),
            ),
        };

        return MsgInfo { message: error_message, cause: error_cause_message };
    }
}
