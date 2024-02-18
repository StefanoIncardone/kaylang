use crate::{
    color::{Bg, Colored, Fg, Flag},
    logging::{AT, BAR, ERROR},
    src_file::{Line, Position, SrcFile},
};
use std::{
    borrow::Cow,
    fmt::Display,
    num::{IntErrorKind, ParseIntError},
    path::Path,
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

    // TODO(stefano): replace these two fields with an iterator
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

        loop {
            let token = match this.next_token() {
                Ok(None) => break,
                Ok(Some(kind)) => Token { kind, col: this.token_start_col },
                Err(err) => {
                    this.errors.push(err);
                    let unexpected = &this.src.code[this.token_start_col..this.col];
                    Token { kind: TokenKind::Unexpected(unexpected), col: this.token_start_col }
                }
            };

            this.tokens.push(token);
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
            this.errors.sort_by(|e1, e2| e1.position.line.cmp(&e2.position.line));
            Err(this.errors)
        }
    }
}

// tokenization of src code
impl<'src> Tokenizer<'src> {
    fn next_token(&mut self) -> Result<Option<TokenKind<'src>>, Error<'src>> {
        let next = loop {
            self.token_start_col = self.col;
            match self.next_character()? {
                // ignore whitespace
                Some(b'\t' | b'\r' | b'\x0C' | b' ') => {}

                // next line
                Some(b'\n') => {
                    self.line_idx += 1;
                    if self.line_idx >= self.src.lines.len() {
                        return Ok(None);
                    }

                    self.line = &self.src.lines[self.line_idx];
                }
                Some(next) => break next,
                None => return Ok(None),
            }
        };

        match next {
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let mut contains_non_ascii = false;
                loop {
                    match self.peek_next_character() {
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

                    Ok(Some(identifier))
                }
            }
            // TODO(stefano): implement negative numbers
            b'0'..=b'9' => {
                let mut contains_non_ascii = false;
                loop {
                    match self.peek_next_character() {
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
                    Ok(value) => Ok(Some(TokenKind::Literal(Literal::Int(value)))),
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
                // consuming the rest of the characters in the current line
                self.col = self.line.end;

                // starting at token_start_col + 1 to ignore the hash symbol
                let comment = &self.src.code[self.token_start_col + 1..self.col];

                Ok(Some(TokenKind::Comment(comment)))
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
                                ErrorKind::UnrecognizedStringEscapeCharacter(unrecognized),
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
                    Ok(Some(TokenKind::Literal(Literal::Str(string_literal))))
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
                            ErrorKind::UnrecognizedCharacterEscapeCharacter(unrecognized),
                        )),
                    },
                    b'\x00'..=b'\x1F' | b'\x7F' => {
                        Err(Error::new(self.src, self.col, 1, ErrorKind::ControlCharacterInCharacterLiteral))
                    }
                    b'\'' => Err(Error::new(self.src, self.token_start_col, 2, ErrorKind::EmptyCharacterLiteral)),
                    ch => Ok(ch),
                };

                match self.peek_next_character()? {
                    Some(b'\'') => {
                        self.col += 1;
                        Ok(Some(TokenKind::Literal(Literal::Char(code?))))
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
                Ok(Some(TokenKind::Bracket(kind)))
            }
            b')' => match self.brackets.pop() {
                Some(bracket) => match bracket.kind {
                    BracketKind::OpenRound
                    | BracketKind::CloseRound
                    | BracketKind::CloseCurly
                    | BracketKind::CloseSquare => Ok(Some(TokenKind::Bracket(BracketKind::CloseRound))),
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
                Ok(Some(TokenKind::Bracket(kind)))
            }
            b']' => match self.brackets.pop() {
                Some(bracket) => match bracket.kind {
                    BracketKind::OpenSquare
                    | BracketKind::CloseSquare
                    | BracketKind::CloseCurly
                    | BracketKind::CloseRound => Ok(Some(TokenKind::Bracket(BracketKind::CloseSquare))),
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
                Ok(Some(TokenKind::Bracket(kind)))
            }
            b'}' => match self.brackets.pop() {
                Some(bracket) => match bracket.kind {
                    BracketKind::OpenCurly
                    | BracketKind::CloseCurly
                    | BracketKind::CloseRound
                    | BracketKind::CloseSquare => Ok(Some(TokenKind::Bracket(BracketKind::CloseCurly))),
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
            b':' => Ok(Some(TokenKind::Colon)),
            b';' => Ok(Some(TokenKind::SemiColon)),
            b',' => Ok(Some(TokenKind::Comma)),
            b'!' => match self.peek_next_character()? {
                Some(b'=') => {
                    self.col += 1;
                    Ok(Some(TokenKind::Op(Op::NotEquals)))
                }
                _ => Ok(Some(TokenKind::Op(Op::Not))),
            },
            b'*' => match self.peek_next_character()? {
                Some(b'*') => {
                    self.col += 1;
                    match self.peek_next_character()? {
                        Some(b'=') => {
                            self.col += 1;
                            Ok(Some(TokenKind::Op(Op::PowEquals)))
                        }
                        _ => Ok(Some(TokenKind::Op(Op::Pow))),
                    }
                }
                Some(b'=') => {
                    self.col += 1;
                    Ok(Some(TokenKind::Op(Op::TimesEquals)))
                }
                _ => Ok(Some(TokenKind::Op(Op::Times))),
            },
            b'/' => match self.peek_next_character()? {
                Some(b'=') => {
                    self.col += 1;
                    Ok(Some(TokenKind::Op(Op::DivideEquals)))
                }
                _ => Ok(Some(TokenKind::Op(Op::Divide))),
            },
            b'%' => match self.peek_next_character()? {
                Some(b'=') => {
                    self.col += 1;
                    Ok(Some(TokenKind::Op(Op::RemainderEquals)))
                }
                _ => Ok(Some(TokenKind::Op(Op::Remainder))),
            },
            b'+' => match self.peek_next_character()? {
                Some(b'=') => {
                    self.col += 1;
                    Ok(Some(TokenKind::Op(Op::PlusEquals)))
                }
                _ => Ok(Some(TokenKind::Op(Op::Plus))),
            },
            b'-' => match self.peek_next_character()? {
                Some(b'=') => {
                    self.col += 1;
                    Ok(Some(TokenKind::Op(Op::MinusEquals)))
                }
                _ => Ok(Some(TokenKind::Op(Op::Minus))),
            },
            b'&' => match self.peek_next_character()? {
                Some(b'&') => {
                    self.col += 1;
                    match self.peek_next_character()? {
                        Some(b'=') => {
                            self.col += 1;
                            Ok(Some(TokenKind::Op(Op::AndEquals)))
                        }
                        _ => Ok(Some(TokenKind::Op(Op::And))),
                    }
                }
                Some(b'=') => {
                    self.col += 1;
                    Ok(Some(TokenKind::Op(Op::BitAndEquals)))
                }
                _ => Ok(Some(TokenKind::Op(Op::BitAnd))),
            },
            b'^' => match self.peek_next_character()? {
                Some(b'=') => {
                    self.col += 1;
                    Ok(Some(TokenKind::Op(Op::BitXorEquals)))
                }
                _ => Ok(Some(TokenKind::Op(Op::BitXor))),
            },
            b'|' => match self.peek_next_character()? {
                Some(b'|') => {
                    self.col += 1;
                    match self.peek_next_character()? {
                        Some(b'=') => {
                            self.col += 1;
                            Ok(Some(TokenKind::Op(Op::OrEquals)))
                        }
                        _ => Ok(Some(TokenKind::Op(Op::Or))),
                    }
                }
                Some(b'=') => {
                    self.col += 1;
                    Ok(Some(TokenKind::Op(Op::BitOrEquals)))
                }
                _ => Ok(Some(TokenKind::Op(Op::BitOr))),
            },
            b'=' => match self.peek_next_character()? {
                Some(b'=') => {
                    self.col += 1;
                    Ok(Some(TokenKind::Op(Op::EqualsEquals)))
                }
                _ => Ok(Some(TokenKind::Op(Op::Equals))),
            },
            b'>' => match self.peek_next_character()? {
                Some(b'>') => {
                    self.col += 1;
                    match self.peek_next_character()? {
                        Some(b'=') => {
                            self.col += 1;
                            Ok(Some(TokenKind::Op(Op::RightShiftEquals)))
                        }
                        _ => Ok(Some(TokenKind::Op(Op::RightShift))),
                    }
                }
                Some(b'=') => {
                    self.col += 1;
                    Ok(Some(TokenKind::Op(Op::GreaterOrEquals)))
                }
                _ => Ok(Some(TokenKind::Op(Op::Greater))),
            },
            b'<' => match self.peek_next_character()? {
                Some(b'<') => {
                    self.col += 1;
                    match self.peek_next_character()? {
                        Some(b'=') => {
                            self.col += 1;
                            Ok(Some(TokenKind::Op(Op::LeftShiftEquals)))
                        }
                        _ => Ok(Some(TokenKind::Op(Op::LeftShift))),
                    }
                }
                Some(b'=') => {
                    self.col += 1;
                    match self.peek_next_character()? {
                        Some(b'>') => {
                            self.col += 1;
                            Ok(Some(TokenKind::Op(Op::Compare)))
                        }
                        _ => Ok(Some(TokenKind::Op(Op::LessOrEquals))),
                    }
                }
                _ => Ok(Some(TokenKind::Op(Op::Less))),
            },
            unrecognized => {
                Err(Error::new(self.src, self.token_start_col, 1, ErrorKind::UnrecognizedCharacter(unrecognized)))
            }
        }
    }
}

// iteration of characters and lines
impl<'src> Tokenizer<'src> {
    // FIX(stefano): properly handle non ASCII characters related errors and column advancing
    // IDEA(stefano): allow utf-8 characters in strings, characters
    fn next_character(&mut self) -> Result<Option<u8>, Error<'src>> {
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

                Err(Error::new(self.src, non_ascii_col, 1, ErrorKind::NonAsciiCharacter(non_ascii)))
            }
        }
    }

    fn peek_next_character(&self) -> Result<Option<&'src u8>, Error<'src>> {
        if self.col >= self.src.code.len() {
            return Ok(None);
        }

        match &self.src.code.as_bytes()[self.col] {
            ascii @ ..=b'\x7F' => Ok(Some(ascii)),
            non_ascii => Err(Error::new(self.src, self.col, 1, ErrorKind::NonAsciiCharacter(*non_ascii))),
        }
    }
}

// character literals
impl<'src> Tokenizer<'src> {
    fn next_in_char_literal(&mut self) -> Result<u8, Error<'src>> {
        match self.next_character()? {
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
        match self.next_character()? {
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

#[derive(Debug)]
pub enum ErrorKind {
    UnclosedBracket(BracketKind),
    NonAsciiCharacter(u8),
    UnclosedCharacterLiteral,
    UnclosedStringLiteral,
    NonAsciiIdentifier,
    NonAsciiNumberLiteral,
    NonDigitNumberLiteral,
    NumberLiteralOverflow,
    NumberLiteralUnderflow,
    GenericInvalidNumberLiteral(ParseIntError),
    UnrecognizedStringEscapeCharacter(u8),
    ControlCharacterInStringLiteral,
    UnrecognizedCharacterEscapeCharacter(u8),
    ControlCharacterInCharacterLiteral,
    EmptyCharacterLiteral,
    MismatchedBracket { expected: BracketKind, actual: BracketKind },
    UnopenedBracket(BracketKind),
    UnrecognizedCharacter(u8),
}

#[derive(Debug)]
pub struct Error<'src> {
    pub path: &'src Path,
    pub position: Position,
    pub len: usize,
    pub line_text: &'src str,
    pub kind: ErrorKind,
}

impl<'src> Error<'src> {
    fn new(src: &'src SrcFile, col: usize, len: usize, kind: ErrorKind) -> Self {
        let position = src.position(col);
        let line_text = src.line_text(position);
        Self { path: &src.path, position, len, line_text, kind }
    }
}

impl Display for Error<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (msg, help_msg): (Cow<'_, str>, Cow<'_, str>) = match &self.kind {
            ErrorKind::UnclosedBracket(bracket) => {
                (format!("unclosed '{bracket}' bracket").into(), "was not closed".into())
            }
            ErrorKind::NonAsciiCharacter(ch) => {
                (format!("unrecognized '{ch}' character").into(), "not a valid ASCII character".into())
            }
            ErrorKind::UnclosedCharacterLiteral => {
                ("invalid character literal".into(), "missing closing single quote".into())
            }
            ErrorKind::UnclosedStringLiteral => {
                ("invalid string literal".into(), "missing closing double quote".into())
            }
            ErrorKind::NonAsciiIdentifier => ("invalid identifier".into(), "contains non-ASCII characters".into()),
            ErrorKind::NonAsciiNumberLiteral => {
                ("invalid number literal".into(), "contains non-ASCII characters".into())
            }
            ErrorKind::NonDigitNumberLiteral => {
                ("invalid number literal".into(), "contains non-digit characters".into())
            }
            ErrorKind::NumberLiteralOverflow => (
                "invalid number literal".into(),
                format!("overflows a {bits} bit signed integer (over {max})", bits = isize::BITS, max = isize::MAX)
                    .into(),
            ),
            ErrorKind::NumberLiteralUnderflow => (
                "invalid number literal".into(),
                format!("underflows a {bits} bit signed integer (under {min})", bits = isize::BITS, min = isize::MIN)
                    .into(),
            ),
            ErrorKind::GenericInvalidNumberLiteral(err) => ("invalid number literal".into(), format!("{err}").into()),
            ErrorKind::UnrecognizedStringEscapeCharacter(ch) => {
                ("invalid string literal".into(), format!("unrecognized '{ch}' escape character").into())
            }
            ErrorKind::ControlCharacterInStringLiteral => {
                ("invalid string literal".into(), "cannot be a control character".into())
            }
            ErrorKind::UnrecognizedCharacterEscapeCharacter(ch) => {
                ("invalid character literal".into(), format!("unrecognized '{ch}' escape character").into())
            }
            ErrorKind::ControlCharacterInCharacterLiteral => {
                ("invalid character literal".into(), "cannot be a control character".into())
            }
            ErrorKind::EmptyCharacterLiteral => ("invalid character literal".into(), "must not be empty".into()),
            ErrorKind::MismatchedBracket { expected, actual } => (
                format!("mismatched '{actual}' bracket").into(),
                format!("closes the wrong bracket, expected a '{expected}' instead").into(),
            ),
            ErrorKind::UnopenedBracket(bracket) => {
                (format!("unopened '{bracket}' bracket").into(), "was not opened before".into())
            }
            ErrorKind::UnrecognizedCharacter(ch) => {
                (format!("unexpected character {ch}").into(), "unrecognized".into())
            }
        };

        let error_msg = Colored { text: msg.to_string(), fg: Fg::White, bg: Bg::Default, flags: Flag::Bold };

        let line_number_text =
            Colored { text: self.position.line.to_string(), fg: Fg::LightBlue, bg: Bg::Default, flags: Flag::Bold };

        let visualization_padding = line_number_text.text.len() + 1 + BAR.text.len();
        let at_padding = visualization_padding - 1;

        let pointers_col = self.position.col - 1;
        let pointers_len = self.len;

        let pointers_and_help_msg = Colored {
            text: format!("{:>pointers_col$}{:^>pointers_len$} {help_msg}", "", ""),
            fg: Fg::LightRed,
            bg: Bg::Default,
            flags: Flag::Bold,
        };

        write!(
            f,
            "{ERROR}: {error_msg}\
            \n{AT:>at_padding$}: {path}:{line}:{col}\
            \n{BAR:>visualization_padding$}\
            \n{line_number_text} {BAR} {line_text}\
            \n{BAR:>visualization_padding$} {pointers_and_help_msg}",
            path = self.path.display(),
            line = self.position.line,
            col = self.position.col,
            line_text = self.line_text,
        )
    }
}

impl std::error::Error for Error<'_> {}
