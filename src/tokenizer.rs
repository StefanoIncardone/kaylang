use crate::{
    error::{SyntaxError, SyntaxErrorInfo, SyntaxErrorKindInfo},
    src_file::{Line, SrcFile},
};
use std::{
    fmt::Display,
    num::{IntErrorKind, ParseIntError},
};

pub(crate) trait Len {
    fn len(&self) -> usize;
}

#[derive(Debug, Clone)]
pub(crate) enum Literal {
    // TODO(stefano): implement unsigned integers
    // IDEA(stefano): have different size integers and default to 32 bits for literals
    Int(isize),
    Char(u8), // only supporting ASCII characters for now
    Bool(bool),
    Str(Vec<u8>),
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

impl Len for Literal {
    fn len(&self) -> usize {
        match self {
            Self::Int(value) => value.to_string().len(),
            Self::Char(value) => value.escape_ascii().len() + 2, // + 2 for the quotes
            Self::Bool(value) => value.to_string().len(),
            Self::Str(string) => string.len() + 2, // + 2 for the quotes
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

impl Len for Op {
    fn len(&self) -> usize {
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

impl Len for Mutability {
    fn len(&self) -> usize {
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

impl Len for BracketKind {
    fn len(&self) -> usize {
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
    col: usize,
    kind: BracketKind,
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

impl Len for TokenKind<'_> {
    fn len(&self) -> usize {
        match self {
            Self::Comment(text) => text.len(),
            Self::Unexpected(text) => text.len(),

            Self::Bracket(bracket) => bracket.len(),
            Self::Colon => 1,
            Self::SemiColon => 1,
            Self::Comma => 1,
            Self::Op(op) => op.len(),

            Self::Literal(typ) => typ.len(),
            Self::True => 4,
            Self::False => 5,
            Self::Identifier(name) => name.len(),
            Self::Definition(kind) => kind.len(),

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
    // pub(crate) len: usize,
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
            let token_kind_result = 'token_kind: loop {
                this.token_start_col = this.col;
                let next = match this.next_ascii_character() {
                    Ok(Some(ch)) => ch,
                    Ok(None) => break 'tokenization,
                    Err(err) => break 'token_kind Err(err),
                };

                match next {
                    // ignore whitespace
                    b'\t' | b'\r' | b'\x0C' | b' ' => {}

                    // next line
                    b'\n' => {
                        if this.line_idx >= this.src.lines.len() - 1 {
                            break 'tokenization;
                        }

                        this.line_idx += 1;
                        this.line = &this.src.lines[this.line_idx];
                    }
                    ch => break 'token_kind this.next_token(ch),
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
            this.errors.push(Error::new(
                this.src,
                bracket.col,
                bracket.kind.len(),
                ErrorKind::UnclosedBracket(bracket.kind),
            ));
        }

        if this.errors.is_empty() {
            Ok(this.tokens)
        } else {
            Err(this.errors)
        }
    }

    fn next_token(&mut self, next: u8) -> Result<TokenKind<'src>, Error<'src>> {
        match next {
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let mut contains_non_ascii = false;
                loop {
                    match self.peek_next_ascii_character() {
                        Ok(Some(b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_')) => self.col += 1,
                        Ok(Some(_) | None) => break,
                        Err(_) => {
                            contains_non_ascii = true;
                            self.col += 1;
                        }
                    }
                }

                if contains_non_ascii {
                    Err(Error::new(
                        self.src,
                        self.token_start_col,
                        self.col - self.token_start_col + 1,
                        ErrorKind::NonAsciiIdentifier,
                    ))
                } else {
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
            }
            b'0'..=b'9' => {
                let mut contains_non_ascii = false;
                loop {
                    match self.peek_next_ascii_character() {
                        Ok(Some(b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_')) => self.col += 1,
                        Ok(Some(_) | None) => break,
                        Err(_) => {
                            contains_non_ascii = true;
                            self.col += 1;
                        }
                    }
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

                        Err(Error::new(self.src, self.token_start_col, token_text.len(), kind))
                    }
                }
            }
            b'#' => {
                // ignoring the hash symbol
                let comment = &self.src.code[self.col..self.line.end];

                // consuming the rest of the characters in the current line
                self.col = self.line.end;

                Ok(TokenKind::Comment(comment))
            }
            b'"' => {
                let mut errors = Vec::<Error<'src>>::new();
                let mut string_literal = Vec::<u8>::new();

                loop {
                    let next = match self.next_in_str_literal()? {
                        b'\\' => match self.next_in_str_literal()? {
                            b'\\' => Ok(b'\\'),
                            b'\'' => Ok(b'\''),
                            b'"' => Ok(b'"'),
                            b'n' => Ok(b'\n'),
                            b'r' => Ok(b'\r'),
                            b't' => Ok(b'\t'),
                            b'0' => Ok(b'\0'),
                            unrecognized => Err(Error::new(
                                self.src,
                                self.col,
                                1,
                                ErrorKind::UnrecognizedStringEscapeCharacter(unrecognized as char),
                            )),
                        },
                        b'\x00'..=b'\x1F' | b'\x7F' => {
                            Err(Error::new(self.src, self.col, 1, ErrorKind::ControlCharacterInStringLiteral))
                        }
                        b'"' => break,
                        other => Ok(other),
                    };

                    match next {
                        Ok(next_char) => string_literal.push(next_char),
                        Err(err) => errors.push(err),
                    }
                }

                // after here there cannot be unclosed strings
                if errors.is_empty() {
                    Ok(TokenKind::Literal(Literal::Str(string_literal)))
                } else {
                    // FIX(stefano): add proper multiple error handling
                    let last_error = errors.pop().unwrap(); // we have already made sure there are errors
                    for error in errors {
                        self.errors.push(error);
                    }
                    Err(last_error)
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
                        unrecognized => Err(Error::new(
                            self.src,
                            self.col,
                            1,
                            ErrorKind::UnrecognizedCharacterEscapeCharacter(unrecognized as char),
                        )),
                    },
                    b'\x00'..=b'\x1F' | b'\x7F' => {
                        Err(Error::new(self.src, self.col, 1, ErrorKind::ControlCharacterInCharacterLiteral))
                    }
                    b'\'' => Err(Error::new(self.src, self.token_start_col, 2, ErrorKind::EmptyCharacterLiteral)),
                    ch => Ok(ch),
                };

                match self.peek_next_ascii_character()? {
                    Some(b'\'') => {
                        self.col += 1;
                        Ok(TokenKind::Literal(Literal::Char(code?)))
                    }
                    Some(_) | None => Err(Error::new(
                        self.src,
                        self.token_start_col,
                        self.col - self.token_start_col + 1,
                        ErrorKind::UnclosedCharacterLiteral,
                    )),
                }
            }
            b'(' => {
                let kind = BracketKind::OpenRound;
                self.brackets.push(Bracket { col: self.token_start_col, kind });
                Ok(TokenKind::Bracket(kind))
            }
            b')' => match self.brackets.pop() {
                Some(bracket) => match bracket.kind {
                    BracketKind::OpenRound
                    | BracketKind::CloseRound
                    | BracketKind::CloseCurly
                    | BracketKind::CloseSquare => Ok(TokenKind::Bracket(BracketKind::CloseRound)),
                    actual @ (BracketKind::OpenCurly | BracketKind::OpenSquare) => Err(Error::new(
                        self.src,
                        self.token_start_col,
                        1,
                        ErrorKind::MismatchedBracket { expected: BracketKind::CloseRound, actual },
                    )),
                },
                None => Err(Error::new(
                    self.src,
                    self.token_start_col,
                    1,
                    ErrorKind::UnopenedBracket(BracketKind::CloseRound),
                )),
            },
            b'[' => {
                let kind = BracketKind::OpenSquare;
                self.brackets.push(Bracket { col: self.token_start_col, kind });
                Ok(TokenKind::Bracket(kind))
            }
            b']' => match self.brackets.pop() {
                Some(bracket) => match bracket.kind {
                    BracketKind::OpenSquare
                    | BracketKind::CloseSquare
                    | BracketKind::CloseCurly
                    | BracketKind::CloseRound => Ok(TokenKind::Bracket(BracketKind::CloseSquare)),
                    actual @ (BracketKind::OpenCurly | BracketKind::OpenRound) => Err(Error::new(
                        self.src,
                        self.token_start_col,
                        1,
                        ErrorKind::MismatchedBracket { expected: BracketKind::CloseSquare, actual },
                    )),
                },
                None => Err(Error::new(
                    self.src,
                    self.token_start_col,
                    1,
                    ErrorKind::UnopenedBracket(BracketKind::CloseSquare),
                )),
            },
            b'{' => {
                let kind = BracketKind::OpenCurly;
                self.brackets.push(Bracket { col: self.token_start_col, kind });
                Ok(TokenKind::Bracket(kind))
            }
            b'}' => match self.brackets.pop() {
                Some(bracket) => match bracket.kind {
                    BracketKind::OpenCurly
                    | BracketKind::CloseCurly
                    | BracketKind::CloseRound
                    | BracketKind::CloseSquare => Ok(TokenKind::Bracket(BracketKind::CloseCurly)),
                    actual @ (BracketKind::OpenRound | BracketKind::OpenSquare) => Err(Error::new(
                        self.src,
                        self.token_start_col,
                        1,
                        ErrorKind::MismatchedBracket { expected: BracketKind::CloseCurly, actual },
                    )),
                },
                None => Err(Error::new(
                    self.src,
                    self.token_start_col,
                    1,
                    ErrorKind::UnopenedBracket(BracketKind::CloseCurly),
                )),
            },
            b':' => Ok(TokenKind::Colon),
            b';' => Ok(TokenKind::SemiColon),
            b',' => Ok(TokenKind::Comma),
            b'!' => match self.peek_next_ascii_character()? {
                Some(b'=') => {
                    self.col += 1;
                    Ok(TokenKind::Op(Op::NotEquals))
                }
                _ => Ok(TokenKind::Op(Op::Not)),
            },
            b'*' => match self.peek_next_ascii_character()? {
                Some(b'*') => {
                    self.col += 1;
                    match self.peek_next_ascii_character()? {
                        Some(b'=') => {
                            self.col += 1;
                            Ok(TokenKind::Op(Op::PowEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::Pow)),
                    }
                }
                Some(b'=') => {
                    self.col += 1;
                    Ok(TokenKind::Op(Op::TimesEquals))
                }
                _ => Ok(TokenKind::Op(Op::Times)),
            },
            b'/' => match self.peek_next_ascii_character()? {
                Some(b'=') => {
                    self.col += 1;
                    Ok(TokenKind::Op(Op::DivideEquals))
                }
                _ => Ok(TokenKind::Op(Op::Divide)),
            },
            b'%' => match self.peek_next_ascii_character()? {
                Some(b'=') => {
                    self.col += 1;
                    Ok(TokenKind::Op(Op::RemainderEquals))
                }
                _ => Ok(TokenKind::Op(Op::Remainder)),
            },
            b'+' => match self.peek_next_ascii_character()? {
                Some(b'=') => {
                    self.col += 1;
                    Ok(TokenKind::Op(Op::PlusEquals))
                }
                _ => Ok(TokenKind::Op(Op::Plus)),
            },
            b'-' => match self.peek_next_ascii_character()? {
                Some(b'=') => {
                    self.col += 1;
                    Ok(TokenKind::Op(Op::MinusEquals))
                }
                _ => Ok(TokenKind::Op(Op::Minus)),
            },
            b'&' => match self.peek_next_ascii_character()? {
                Some(b'&') => {
                    self.col += 1;
                    match self.peek_next_ascii_character()? {
                        Some(b'=') => {
                            self.col += 1;
                            Ok(TokenKind::Op(Op::AndEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::And)),
                    }
                }
                Some(b'=') => {
                    self.col += 1;
                    Ok(TokenKind::Op(Op::BitAndEquals))
                }
                _ => Ok(TokenKind::Op(Op::BitAnd)),
            },
            b'^' => match self.peek_next_ascii_character()? {
                Some(b'=') => {
                    self.col += 1;
                    Ok(TokenKind::Op(Op::BitXorEquals))
                }
                _ => Ok(TokenKind::Op(Op::BitXor)),
            },
            b'|' => match self.peek_next_ascii_character()? {
                Some(b'|') => {
                    self.col += 1;
                    match self.peek_next_ascii_character()? {
                        Some(b'=') => {
                            self.col += 1;
                            Ok(TokenKind::Op(Op::OrEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::Or)),
                    }
                }
                Some(b'=') => {
                    self.col += 1;
                    Ok(TokenKind::Op(Op::BitOrEquals))
                }
                _ => Ok(TokenKind::Op(Op::BitOr)),
            },
            b'=' => match self.peek_next_ascii_character()? {
                Some(b'=') => {
                    self.col += 1;
                    Ok(TokenKind::Op(Op::EqualsEquals))
                }
                _ => Ok(TokenKind::Op(Op::Equals)),
            },
            b'>' => match self.peek_next_ascii_character()? {
                Some(b'>') => {
                    self.col += 1;
                    match self.peek_next_ascii_character()? {
                        Some(b'=') => {
                            self.col += 1;
                            Ok(TokenKind::Op(Op::RightShiftEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::RightShift)),
                    }
                }
                Some(b'=') => {
                    self.col += 1;
                    Ok(TokenKind::Op(Op::GreaterOrEquals))
                }
                _ => Ok(TokenKind::Op(Op::Greater)),
            },
            b'<' => match self.peek_next_ascii_character()? {
                Some(b'<') => {
                    self.col += 1;
                    match self.peek_next_ascii_character()? {
                        Some(b'=') => {
                            self.col += 1;
                            Ok(TokenKind::Op(Op::LeftShiftEquals))
                        }
                        _ => Ok(TokenKind::Op(Op::LeftShift)),
                    }
                }
                Some(b'=') => {
                    self.col += 1;
                    match self.peek_next_ascii_character()? {
                        Some(b'>') => {
                            self.col += 1;
                            Ok(TokenKind::Op(Op::Compare))
                        }
                        _ => Ok(TokenKind::Op(Op::LessOrEquals)),
                    }
                }
                _ => Ok(TokenKind::Op(Op::Less)),
            },
            unrecognized => Err(Error::new(
                self.src,
                self.token_start_col,
                1,
                ErrorKind::UnrecognizedCharacter(unrecognized as char),
            )),
        }
    }
}

// iteration of characters and lines
impl<'src> Tokenizer<'src> {
    // FIX(stefano): properly handle non ASCII characters related errors and column advancing
    // IDEA(stefano): allow utf-8 characters in strings, characters
    fn next_ascii_character(&mut self) -> Result<Option<u8>, Error<'src>> {
        if self.col >= self.src.code.len() {
            return Ok(None);
        }

        match self.src.code.as_bytes()[self.col] {
            ascii @ ..=b'\x7F' => {
                self.col += 1;
                Ok(Some(ascii))
            }
            non_ascii => {
                let non_ascii_col = self.col;
                self.col += 1;

                Err(Error::new(self.src, non_ascii_col, 1, ErrorKind::NonAsciiCharacter(non_ascii as char)))
            }
        }
    }

    fn peek_next_ascii_character(&self) -> Result<Option<&'src u8>, Error<'src>> {
        if self.col >= self.src.code.len() {
            return Ok(None);
        }

        match &self.src.code.as_bytes()[self.col] {
            ascii @ ..=b'\x7F' => Ok(Some(ascii)),
            non_ascii => Err(Error::new(self.src, self.col, 1, ErrorKind::NonAsciiCharacter(*non_ascii as char))),
        }
    }
}

// character literals
impl<'src> Tokenizer<'src> {
    fn next_in_char_literal(&mut self) -> Result<u8, Error<'src>> {
        match self.next_ascii_character()? {
            Some(next) => Ok(next),
            None => Err(Error::new(
                self.src,
                self.token_start_col,
                self.col - self.token_start_col + 1,
                ErrorKind::UnclosedCharacterLiteral,
            )),
        }
    }
}

// string literals
impl<'src> Tokenizer<'src> {
    fn next_in_str_literal(&mut self) -> Result<u8, Error<'src>> {
        match self.next_ascii_character()? {
            Some(next) => Ok(next),
            None => Err(Error::new(
                self.src,
                self.token_start_col,
                self.col - self.token_start_col + 1,
                ErrorKind::UnclosedStringLiteral,
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

impl SyntaxErrorKindInfo for ErrorKind {
    fn info(&self) -> SyntaxErrorInfo {
        let (msg, help_msg) = match &self {
            Self::UnclosedBracket(bracket) => (format!("unclosed '{bracket}' bracket").into(), "was not closed".into()),
            Self::NonAsciiCharacter(ch) => {
                (format!("unrecognized '{ch}' character").into(), "not a valid ASCII character".into())
            }
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
            Self::UnrecognizedCharacter(ch) => (format!("unrecognized character '{ch}'").into(), "unrecognized".into()),
        };

        SyntaxErrorInfo { msg, help_msg }
    }
}

pub type Error<'src> = SyntaxError<'src, ErrorKind>;
