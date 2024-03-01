use crate::{
    error::{ErrorInfo, SyntaxError, SyntaxErrorInfo},
    src_file::{Line, SrcFile},
};
use std::{
    fmt::Display,
    num::{IntErrorKind, ParseIntError},
};

// TODO(stefano): inline this trait inside the TokenKind
pub(crate) trait SrcCodeLen {
    fn src_code_len(&self) -> usize;
}

#[derive(Debug, Clone)]
pub(crate) enum Literal {
    // TODO(stefano): implement unsigned integers
    // IDEA(stefano): have different size integers and default to 32 bits for literals
    Int(isize),
    Char(u8), // only supporting ASCII characters for now
    Bool(bool),
    Str(Vec<u8>), // only supporting ASCII characters for now
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(value) => write!(f, "{value}"),
            Self::Char(code) => write!(f, "'{code}'", code = code.escape_ascii()),
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
            Self::Char(value) => value.escape_ascii().len() + 2, // + 2 for the quotes
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
    line_idx: usize,
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
    Print,   // temporary way of printing values
    PrintLn, // temporary way of printing values followed by a newline
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

    col: usize,
    token_start_col: usize,

    line_idx: usize,
    line: &'src Line,

    tokens: Vec<Token<'src>>,
    brackets: Vec<Bracket>,
    errors: Vec<Error<'src>>,
}

impl<'src> Tokenizer<'src> {
    pub fn tokenize(src: &'src SrcFile) -> Result<Vec<Token<'src>>, Vec<Error<'src>>> {
        if src.lines.is_empty() {
            return Ok(Vec::new());
        }

        let mut this = Self {
            src,
            col: 0,
            token_start_col: 0,
            line_idx: 0,
            line: &src.lines[0],
            tokens: Vec::new(),
            brackets: Vec::new(),
            errors: Vec::new(),
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
                        if this.line_idx >= this.src.lines.len() - 1 {
                            break 'tokenization;
                        }

                        this.line_idx += 1;
                        this.line = &this.src.lines[this.line_idx];
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
            this.errors.push(Error::new_with_line_idx(
                this.src,
                bracket.line_idx,
                bracket.col,
                1,
                ErrorKind::UnclosedBracket(bracket.kind),
            ));
        }

        if this.errors.is_empty() {
            Ok(this.tokens)
        } else {
            Err(this.errors)
        }
    }
}

// TODO(stefano): own utf8 parsing
// TODO(stefano): properly handle multi-char utf8 characters (i.e. emojis)
// TODO(stefano): allow utf-8 characters in strings, characters
// iteration of characters
impl<'src> Tokenizer<'src> {
    fn token_len(&self) -> usize {
        self.src.code[self.token_start_col..self.col].chars().count()
    }

    fn next_ascii_char(&mut self) -> Result<Option<u8>, Error<'src>> {
        let Some(next) = self.src.code.as_bytes().get(self.col) else {
            return Ok(None);
        };

        match next {
            ascii @ ..=b'\x7F' => {
                self.col += 1;
                Ok(Some(*ascii))
            }
            _non_ascii => {
                let rest_of_line = &self.src.code[self.col..self.line.end];
                let non_ascii_char = rest_of_line.chars().next().unwrap();
                let non_ascii_col = self.col;
                self.col += non_ascii_char.len_utf8();
                Err(Error::new_with_line_idx(
                    self.src,
                    self.line_idx,
                    non_ascii_col,
                    1,
                    ErrorKind::NonAsciiCharacter(non_ascii_char),
                ))
            }
        }
    }

    fn next_utf8_char(&mut self) -> Option<char> {
        let Some(next) = self.src.code.as_bytes().get(self.col) else {
            return None;
        };

        match next {
            ascii @ ..=b'\x7F' => {
                self.col += 1;
                Some(*ascii as char)
            }
            _non_ascii => {
                let rest_of_line = &self.src.code[self.col..self.line.end];
                let non_ascii_char = rest_of_line.chars().next().unwrap();
                self.col += non_ascii_char.len_utf8();
                Some(non_ascii_char)
            }
        }
    }

    fn peek_next_ascii_char(&self) -> Result<Option<&'src u8>, Error<'src>> {
        let Some(next) = self.src.code.as_bytes().get(self.col) else {
            return Ok(None);
        };

        match next {
            ascii @ ..=b'\x7F' => Ok(Some(ascii)),
            _non_ascii => {
                let rest_of_line = &self.src.code[self.col..self.line.end];
                let non_ascii_char = rest_of_line.chars().next().unwrap();
                Err(Error::new_with_line_idx(
                    self.src,
                    self.line_idx,
                    self.col,
                    1,
                    ErrorKind::NonAsciiCharacter(non_ascii_char),
                ))
            }
        }
    }

    fn peek_next_utf8_char(&mut self) -> Option<char> {
        let Some(next) = self.src.code.as_bytes().get(self.col) else {
            return None;
        };

        match next {
            ascii @ ..=b'\x7F' => Some(*ascii as char),
            _non_ascii => {
                let rest_of_line = &self.src.code[self.col..self.line.end];
                let non_ascii_char = rest_of_line.chars().next().unwrap();
                Some(non_ascii_char)
            }
        }
    }

    fn next_in_char_literal(&mut self) -> Result<u8, Error<'src>> {
        match self.src.code.as_bytes().get(self.col) {
            Some(b'\n') | None => Err(Error::new_with_line_idx(
                self.src,
                self.line_idx,
                self.token_start_col,
                self.token_len(),
                ErrorKind::UnclosedCharacterLiteral,
            )),
            Some(ascii @ ..=b'\x7F') => {
                self.col += 1;
                Ok(*ascii)
            }
            Some(_non_ascii) => {
                let rest_of_line = &self.src.code[self.col..self.line.end];
                let non_ascii_char = rest_of_line.chars().next().unwrap();
                let non_ascii_col = self.col;
                self.col += non_ascii_char.len_utf8();
                Err(Error::new_with_line_idx(
                    self.src,
                    self.line_idx,
                    non_ascii_col,
                    1,
                    ErrorKind::NonAsciiCharacter(non_ascii_char),
                ))
            }
        }
    }

    fn next_in_str_literal(&mut self) -> Result<u8, Error<'src>> {
        match self.src.code.as_bytes().get(self.col) {
            Some(b'\n') | None => Err(Error::new_with_line_idx(
                self.src,
                self.line_idx,
                self.token_start_col,
                self.token_len(),
                ErrorKind::UnclosedStringLiteral,
            )),
            Some(ascii @ ..=b'\x7F') => {
                self.col += 1;
                Ok(*ascii)
            }
            Some(_non_ascii) => {
                let rest_of_line = &self.src.code[self.col..self.line.end];
                let non_ascii_char = rest_of_line.chars().next().unwrap();
                let non_ascii_col = self.col;
                self.col += non_ascii_char.len_utf8();
                Err(Error::new_with_line_idx(
                    self.src,
                    self.line_idx,
                    non_ascii_col,
                    1,
                    ErrorKind::NonAsciiCharacter(non_ascii_char),
                ))
            }
        }
    }
}

impl<'src> Tokenizer<'src> {
    fn next_token(&mut self, next: u8) -> Result<TokenKind<'src>, Error<'src>> {
        match next {
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let mut contains_non_ascii = false;
                loop {
                    match self.peek_next_ascii_char() {
                        Ok(Some(b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_')) => {}
                        Ok(Some(_) | None) => break,
                        Err(_) => contains_non_ascii = true,
                    }

                    let _ = self.next_ascii_char();
                }

                if contains_non_ascii {
                    return Err(Error::new_with_line_idx(
                        self.src,
                        self.line_idx,
                        self.token_start_col,
                        self.token_len(),
                        ErrorKind::NonAsciiIdentifier,
                    ));
                }

                let identifier = match &self.src.code[self.token_start_col..self.col] {
                    "let" => TokenKind::Definition(Mutability::Let),
                    "var" => TokenKind::Definition(Mutability::Var),
                    "print" => TokenKind::Print,
                    "println" => TokenKind::PrintLn,
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
            b'0'..=b'9' => {
                let mut contains_non_ascii = false;
                loop {
                    match self.peek_next_ascii_char() {
                        Ok(Some(b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_')) => {}
                        Ok(Some(_) | None) => break,
                        Err(_) => contains_non_ascii = true,
                    }

                    let _ = self.next_ascii_char();
                }

                let token_text = &self.src.code[self.token_start_col..self.col];
                match token_text.parse() {
                    Ok(value) => Ok(TokenKind::Literal(Literal::Int(value))),
                    Err(err) => {
                        let kind = match err.kind() {
                            IntErrorKind::InvalidDigit if contains_non_ascii => ErrorKind::NonAsciiNumberLiteral,
                            IntErrorKind::InvalidDigit => ErrorKind::NonDigitNumberLiteral,
                            IntErrorKind::PosOverflow => ErrorKind::NumberLiteralOverflow,
                            IntErrorKind::NegOverflow => ErrorKind::NumberLiteralUnderflow,
                            IntErrorKind::Empty => unreachable!("should never parse empty numbers"),
                            IntErrorKind::Zero => unreachable!("numbers can also be zero"),
                            _ => ErrorKind::GenericInvalidNumberLiteral(err),
                        };

                        Err(Error::new_with_line_idx(
                            self.src,
                            self.line_idx,
                            self.token_start_col,
                            self.token_len(),
                            kind,
                        ))
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
                let mut string_literal = Vec::<u8>::new();
                loop {
                    let next = match self.next_in_str_literal()? {
                        b'\\' => match self.next_in_str_literal()? {
                            b'\\' => b'\\',
                            b'\'' => b'\'',
                            b'"' => b'"',
                            b'n' => b'\n',
                            b'r' => b'\r',
                            b't' => b'\t',
                            b'0' => b'\0',
                            unrecognized => {
                                self.errors.push(Error::new_with_line_idx(
                                    self.src,
                                    self.line_idx,
                                    self.col - 2,
                                    2,
                                    ErrorKind::UnrecognizedStringEscapeCharacter(unrecognized as char),
                                ));
                                string_literal.push(b'\\');
                                unrecognized
                            }
                        },
                        control @ (b'\x00'..=b'\x1F' | b'\x7F') => {
                            self.errors.push(Error::new_with_line_idx(
                                self.src,
                                self.line_idx,
                                self.col - 1,
                                1,
                                ErrorKind::ControlCharacterInStringLiteral,
                            ));
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
                let code = match self.next_in_char_literal()? {
                    b'\\' => match self.next_in_char_literal()? {
                        b'\\' => Ok(b'\\'),
                        b'\'' => Ok(b'\''),
                        b'"' => Ok(b'"'),
                        b'n' => Ok(b'\n'),
                        b'r' => Ok(b'\r'),
                        b't' => Ok(b'\t'),
                        b'0' => Ok(b'\0'),
                        unrecognized => Err(Error::new_with_line_idx(
                            self.src,
                            self.line_idx,
                            self.col - 2,
                            2,
                            ErrorKind::UnrecognizedCharacterEscapeCharacter(unrecognized as char),
                        )),
                    },
                    b'\x00'..=b'\x1F' | b'\x7F' => Err(Error::new_with_line_idx(
                        self.src,
                        self.line_idx,
                        self.col - 1,
                        1,
                        ErrorKind::ControlCharacterInCharacterLiteral,
                    )),
                    b'\'' => Err(Error::new_with_line_idx(
                        self.src,
                        self.line_idx,
                        self.token_start_col,
                        2,
                        ErrorKind::EmptyCharacterLiteral,
                    )),
                    ch => Ok(ch),
                };

                let Some(b'\'') = self.peek_next_ascii_char()? else {
                    return Err(Error::new_with_line_idx(
                        self.src,
                        self.line_idx,
                        self.token_start_col,
                        self.token_len(),
                        ErrorKind::UnclosedCharacterLiteral,
                    ));
                };

                self.col += 1;
                Ok(TokenKind::Literal(Literal::Char(code?)))
            }
            b'(' => {
                let kind = BracketKind::OpenRound;
                self.brackets.push(Bracket { kind, col: self.token_start_col, line_idx: self.line_idx });
                Ok(TokenKind::Bracket(kind))
            }
            b')' => match self.brackets.pop() {
                Some(bracket) => match bracket.kind {
                    BracketKind::OpenRound
                    | BracketKind::CloseRound
                    | BracketKind::CloseCurly
                    | BracketKind::CloseSquare => Ok(TokenKind::Bracket(BracketKind::CloseRound)),
                    actual @ (BracketKind::OpenCurly | BracketKind::OpenSquare) => Err(Error::new_with_line_idx(
                        self.src,
                        self.line_idx,
                        self.token_start_col,
                        1,
                        ErrorKind::MismatchedBracket { expected: BracketKind::CloseRound, actual },
                    )),
                },
                None => Err(Error::new_with_line_idx(
                    self.src,
                    self.line_idx,
                    self.token_start_col,
                    1,
                    ErrorKind::UnopenedBracket(BracketKind::CloseRound),
                )),
            },
            b'[' => {
                let kind = BracketKind::OpenSquare;
                self.brackets.push(Bracket { kind, col: self.token_start_col, line_idx: self.line_idx });
                Ok(TokenKind::Bracket(kind))
            }
            b']' => match self.brackets.pop() {
                Some(bracket) => match bracket.kind {
                    BracketKind::OpenSquare
                    | BracketKind::CloseSquare
                    | BracketKind::CloseCurly
                    | BracketKind::CloseRound => Ok(TokenKind::Bracket(BracketKind::CloseSquare)),
                    actual @ (BracketKind::OpenCurly | BracketKind::OpenRound) => Err(Error::new_with_line_idx(
                        self.src,
                        self.line_idx,
                        self.token_start_col,
                        1,
                        ErrorKind::MismatchedBracket { expected: BracketKind::CloseSquare, actual },
                    )),
                },
                None => Err(Error::new_with_line_idx(
                    self.src,
                    self.line_idx,
                    self.token_start_col,
                    1,
                    ErrorKind::UnopenedBracket(BracketKind::CloseSquare),
                )),
            },
            b'{' => {
                let kind = BracketKind::OpenCurly;
                self.brackets.push(Bracket { kind, col: self.token_start_col, line_idx: self.line_idx });
                Ok(TokenKind::Bracket(kind))
            }
            b'}' => match self.brackets.pop() {
                Some(bracket) => match bracket.kind {
                    BracketKind::OpenCurly
                    | BracketKind::CloseCurly
                    | BracketKind::CloseRound
                    | BracketKind::CloseSquare => Ok(TokenKind::Bracket(BracketKind::CloseCurly)),
                    actual @ (BracketKind::OpenRound | BracketKind::OpenSquare) => Err(Error::new_with_line_idx(
                        self.src,
                        self.line_idx,
                        self.token_start_col,
                        1,
                        ErrorKind::MismatchedBracket { expected: BracketKind::CloseCurly, actual },
                    )),
                },
                None => Err(Error::new_with_line_idx(
                    self.src,
                    self.line_idx,
                    self.token_start_col,
                    1,
                    ErrorKind::UnopenedBracket(BracketKind::CloseCurly),
                )),
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
            unrecognized => Err(Error::new_with_line_idx(
                self.src,
                self.line_idx,
                self.token_start_col,
                1,
                ErrorKind::UnrecognizedCharacter(unrecognized as char),
            )),
        }
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    UnclosedBracket(BracketKind),
    NonAsciiCharacter(char),
    UnclosedCharacterLiteral,
    UnclosedStringLiteral,
    NonAsciiIdentifier,
    NonAsciiNumberLiteral,
    NonDigitNumberLiteral,
    NumberLiteralOverflow,
    NumberLiteralUnderflow,
    GenericInvalidNumberLiteral(ParseIntError),
    UnrecognizedStringEscapeCharacter(char),
    ControlCharacterInStringLiteral,
    UnrecognizedCharacterEscapeCharacter(char),
    ControlCharacterInCharacterLiteral,
    EmptyCharacterLiteral,
    MismatchedBracket { expected: BracketKind, actual: BracketKind },
    UnopenedBracket(BracketKind),
    UnrecognizedCharacter(char),
}

impl ErrorInfo for ErrorKind {
    type Info = SyntaxErrorInfo;

    fn info(&self) -> Self::Info {
        let (msg, help_msg) = match &self {
            Self::UnclosedBracket(bracket) => (format!("unclosed '{bracket}' bracket").into(), "was not closed".into()),
            Self::NonAsciiCharacter(ch) => (
                format!("unrecognized '{ch}' ({escaped_char}) character", escaped_char = ch.escape_unicode()).into(),
                "not a valid ASCII character".into(),
            ),
            Self::UnclosedCharacterLiteral => {
                ("invalid character literal".into(), "missing closing single quote".into())
            }
            Self::UnclosedStringLiteral => ("invalid string literal".into(), "missing closing double quote".into()),
            Self::NonAsciiIdentifier => ("invalid identifier".into(), "contains non-ASCII characters".into()),
            Self::NonAsciiNumberLiteral => ("invalid number literal".into(), "contains non-ASCII characters".into()),
            Self::NonDigitNumberLiteral => ("invalid number literal".into(), "contains non-digit characters".into()),
            Self::NumberLiteralOverflow => (
                "invalid number literal".into(),
                format!("overflows a {bits} bit signed integer (over {max})", bits = isize::BITS, max = isize::MAX)
                    .into(),
            ),
            Self::NumberLiteralUnderflow => (
                "invalid number literal".into(),
                format!("underflows a {bits} bit signed integer (under {min})", bits = isize::BITS, min = isize::MIN)
                    .into(),
            ),
            Self::GenericInvalidNumberLiteral(err) => ("invalid number literal".into(), format!("{err}").into()),
            Self::UnrecognizedStringEscapeCharacter(ch) => {
                ("invalid string literal".into(), format!("unrecognized '{ch}' escape character").into())
            }
            Self::ControlCharacterInStringLiteral => {
                ("invalid string literal".into(), "cannot be a control character".into())
            }
            Self::UnrecognizedCharacterEscapeCharacter(ch) => {
                ("invalid character literal".into(), format!("unrecognized '{ch}' escape character").into())
            }
            Self::ControlCharacterInCharacterLiteral => {
                ("invalid character literal".into(), "cannot be a control character".into())
            }
            Self::EmptyCharacterLiteral => ("invalid character literal".into(), "must not be empty".into()),
            Self::MismatchedBracket { expected, actual } => (
                format!("mismatched '{actual}' bracket").into(),
                format!("closes the wrong bracket, expected a '{expected}' instead").into(),
            ),
            Self::UnopenedBracket(bracket) => {
                (format!("unopened '{bracket}' bracket").into(), "was not opened before".into())
            }
            Self::UnrecognizedCharacter(ch) => (
                format!("unrecognized '{ch}' ({escaped_char}) character", escaped_char = ch.escape_unicode()).into(),
                "unrecognized".into(),
            ),
        };

        Self::Info { msg, help_msg }
    }
}

pub type Error<'src> = SyntaxError<'src, ErrorKind>;
