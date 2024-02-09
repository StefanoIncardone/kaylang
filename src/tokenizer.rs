use std::{
    fmt::Display,
    fs::File,
    io::{BufRead, BufReader, ErrorKind},
    num::IntErrorKind,
    path::{Path, PathBuf},
};

use crate::error::{AddError, IoError, RawSyntaxError, SyntaxError, SyntaxErrors};

#[derive(Debug, Clone, Copy)]
pub(crate) struct Line {
    pub(crate) start: usize, // inclusive
    pub(crate) end: usize,   // not inclusive
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Position {
    pub(crate) line: usize,
    pub(crate) col: usize,
}

#[derive(Debug)]
pub struct SrcFile {
    pub(crate) path: PathBuf,
    pub(crate) code: String,
    pub(crate) lines: Vec<Line>,
}

impl SrcFile {
    pub fn load<P: AsRef<Path>>(path: P) -> Result<Self, IoError> {
        let path = path.as_ref();

        let file = match File::open(path) {
            Ok(f) => f,
            Err(err) => {
                return Err(IoError {
                    kind: err.kind(),
                    msg: format!("could not open '{}'", path.display()).into(),
                    cause: err.to_string().into(),
                })
            }
        };

        let file_len = match file.metadata() {
            Ok(metadata) if metadata.is_file() => metadata.len() as usize,
            Ok(_) => {
                return Err(IoError {
                    kind: ErrorKind::InvalidInput,
                    msg: "invalid path".into(),
                    cause: format!("expected a file but got directory '{}'", path.display()).into(),
                });
            }
            Err(err) => {
                return Err(IoError {
                    kind: err.kind(),
                    msg: format!("could not read metadata of '{}'", path.display()).into(),
                    cause: err.to_string().into(),
                });
            }
        };

        // plus one to account for a possible phantom newline at the end
        let mut code = String::with_capacity(file_len + 1);
        let mut lines = Vec::<Line>::new();
        let mut start = 0;
        let mut src = BufReader::new(file);

        loop {
            let mut chars_read = match src.read_line(&mut code) {
                Ok(0) => break,
                Ok(read) => read,
                Err(err) => {
                    return Err(IoError {
                        kind: err.kind(),
                        msg: format!("could not read contents of '{}'", path.display()).into(),
                        cause: err.to_string().into(),
                    })
                }
            };

            let mut end = code.len() - 1;
            if end > start {
                if let cr @ b'\r' = &mut unsafe { code.as_bytes_mut() }[end - 1] {
                    *cr = b'\n';
                    unsafe { code.as_mut_vec().set_len(end) };
                    end -= 1;
                    chars_read -= 1;
                }
            }

            lines.push(Line { start, end });
            start += chars_read;
        }

        // it will make lexing simpler
        if !code.is_empty() {
            let last_char = code.len() - 1;
            if code.as_bytes()[last_char] != b'\n' {
                code.push('\n');
                let last_line = lines.len() - 1;
                lines[last_line].end += 1;
            }
        }

        return Ok(Self { path: path.to_path_buf(), code, lines });
    }

    pub(crate) fn position(&self, col: usize) -> Position {
        let mut left = 0;
        let mut right = self.lines.len();
        while left < right {
            let middle = left + (right - left) / 2;
            if col < self.lines[middle].end {
                right = middle;
            } else {
                left = middle + 1;
            }
        }

        return Position { line: left + 1, col: col + 1 - self.lines[left].start };
    }
}

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
        return match self {
            Self::Int(value) => write!(f, "{}", value),
            Self::Char(code) => write!(f, "'{}'", code.escape_ascii()),
            Self::Bool(value) => write!(f, "{}", value),
            Self::Str(string) => {
                write!(f, "\"")?;
                for character in string {
                    write!(f, "{}", character.escape_ascii())?;
                }
                write!(f, "\"")
            }
        };
    }
}

impl Len for Literal {
    fn len(&self) -> usize {
        return match self {
            Self::Int(value) => value.to_string().len(),
            Self::Char(value) => value.escape_ascii().len() + 2, // + 2 for the quotes
            Self::Bool(value) => value.to_string().len(),
            Self::Str(string) => string.len() + 2, // + 2 for the quotes
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum Op {
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
        return match self {
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
        };
    }
}

impl Len for Op {
    fn len(&self) -> usize {
        return match self {
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
        };
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Mutability {
    Let,
    Var,
}

impl Display for Mutability {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Let => write!(f, "let"),
            Self::Var => write!(f, "var"),
        };
    }
}

impl Len for Mutability {
    fn len(&self) -> usize {
        return match self {
            Self::Let => 3,
            Self::Var => 3,
        };
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum BracketKind {
    OpenRound,
    CloseRound,
    OpenSquare,
    CloseSquare,
    OpenCurly,
    CloseCurly,
}

impl Display for BracketKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

impl Len for BracketKind {
    fn len(&self) -> usize {
        return match self {
            Self::OpenRound => 1,
            Self::CloseRound => 1,
            Self::OpenSquare => 1,
            Self::CloseSquare => 1,
            Self::OpenCurly => 1,
            Self::CloseCurly => 1,
        };
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
        return match self {
            Self::Comment(text) => write!(f, "#{}", text),
            Self::Unexpected(text) => write!(f, "{}", text),

            Self::Bracket(bracket) => write!(f, "{}", bracket),
            Self::Colon => write!(f, ":"),
            Self::SemiColon => write!(f, ";"),
            Self::Comma => write!(f, ","),

            Self::Literal(literal) => write!(f, "{}", literal),
            Self::Identifier(name) => write!(f, "{}", name),
            Self::Definition(kind) => write!(f, "{}", kind),

            Self::Op(op) => write!(f, "{}", op),

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
        };
    }
}

impl Len for TokenKind<'_> {
    fn len(&self) -> usize {
        return match self {
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
        };
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
    errors: Vec<SyntaxError>,
}

impl<'src> Tokenizer<'src> {
    pub fn tokenize(src: &'src SrcFile) -> Result<Vec<Token<'src>>, SyntaxErrors<'src>> {
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
            #[cfg(debug_assertions)]
            #[allow(unused_variables)]
            let line_text = &this.src.code[this.line.start..this.line.end];

            let token = match this.next_token() {
                Ok(None) => break,
                Ok(Some(kind)) => Token { kind, col: this.token_start_col },
                Err(err) => {
                    this.errors.add(this.src, err);
                    let unexpected = &this.src.code[this.token_start_col..this.col];
                    Token { kind: TokenKind::Unexpected(unexpected), col: this.token_start_col }
                }
            };

            this.tokens.push(token);
        }

        for bracket in &this.brackets {
            // there can only be open brackets at this point
            this.errors.add(
                this.src,
                RawSyntaxError {
                    col: bracket.col,
                    len: bracket.kind.len(),
                    msg: "stray bracket".into(),
                    help_msg: "was not closed".into(),
                },
            );
        }

        return if this.errors.is_empty() {
            Ok(this.tokens)
        } else {
            this.errors.sort_by(|e1, e2| e1.line.cmp(&e2.line));
            Err(SyntaxErrors { src: this.src, errors: this.errors })
        };
    }

    // this function exists just to be able to use the ? operator
    fn next_token(&mut self) -> Result<Option<TokenKind<'src>>, RawSyntaxError> {
        // this loop exists just to be able to use continues and breaks
        loop {
            self.token_start_col = self.col;
            let Some(next) = self.next()? else { return Ok(None) };

            return match next {
                // ignore whitespace
                b'\t' | b'\r' | b'\x0C' | b' ' => continue,
                b'\n' => match self.next_line() {
                    Some(line) => {
                        self.line = line;
                        continue;
                    }
                    None => return Ok(None),
                },
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                    let mut contains_non_ascii = false;
                    loop {
                        match self.peek_next() {
                            Ok(Some(b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_')) => self.col += 1,
                            Ok(Some(_) | None) => break,
                            Err(_) => {
                                contains_non_ascii = true;
                                self.col += 1;
                            }
                        }
                    }

                    if contains_non_ascii {
                        Err(RawSyntaxError {
                            col: self.token_start_col,
                            len: self.col - self.token_start_col + 1,
                            msg: "invalid identifier".into(),
                            help_msg: "contains non-ASCII characters".into(),
                        })
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
                // implement negative numbers
                b'0'..=b'9' => {
                    let mut contains_non_ascii = false;
                    loop {
                        match self.peek_next() {
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
                        Err(err) => match err.kind() {
                            IntErrorKind::InvalidDigit => {
                                if contains_non_ascii {
                                    Err(RawSyntaxError {
                                        col: self.token_start_col,
                                        len: token_text.len(),
                                        msg: "invalid number literal".into(),
                                        help_msg: "contains non-ASCII characters".into(),
                                    })
                                } else {
                                    Err(RawSyntaxError {
                                        col: self.token_start_col,
                                        len: token_text.len(),
                                        msg: "invalid number literal".into(),
                                        help_msg: "contains non-digit characters".into(),
                                    })
                                }
                            }
                            IntErrorKind::PosOverflow => Err(RawSyntaxError {
                                col: self.token_start_col,
                                len: token_text.len(),
                                msg: "invalid number literal".into(),
                                help_msg: format!(
                                    "overflows a {} bit signed integer (over {})",
                                    isize::BITS,
                                    isize::MAX
                                )
                                .into(),
                            }),
                            IntErrorKind::NegOverflow => Err(RawSyntaxError {
                                col: self.token_start_col,
                                len: token_text.len(),
                                msg: "invalid number literal".into(),
                                help_msg: format!(
                                    "underflows a {} bit signed integer (under {})",
                                    isize::BITS,
                                    isize::MIN
                                )
                                .into(),
                            }),
                            IntErrorKind::Empty => unreachable!("should never parse empty numbers"),
                            IntErrorKind::Zero => unreachable!("numbers can also be zero"),
                            _ => Err(RawSyntaxError {
                                col: self.token_start_col,
                                len: token_text.len(),
                                msg: "invalid number literal".into(),
                                help_msg: err.to_string().into(),
                            }),
                        },
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
                    let mut errors = Vec::<RawSyntaxError>::new();
                    let mut text = Vec::<u8>::new();

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
                                _ => Err(RawSyntaxError {
                                    col: self.col,
                                    len: 1,
                                    msg: "invalid string character".into(),
                                    help_msg: "unrecognized escape character".into(),
                                }),
                            },
                            b'\x00'..=b'\x1F' | b'\x7F' => Err(RawSyntaxError {
                                col: self.col,
                                len: 1,
                                msg: "invalid string literal".into(),
                                help_msg: "cannot be a control character".into(),
                            }),
                            b'"' => break,
                            other => Ok(other),
                        };

                        match next {
                            Ok(next_char) => text.push(next_char),
                            Err(err) => errors.push(err),
                        }
                    }

                    // after here there cannot be unclosed strings
                    if errors.is_empty() {
                        Ok(Some(TokenKind::Literal(Literal::Str(text))))
                    } else {
                        // FIX(stefano): add proper multiple error handling
                        let last_error = errors.pop().unwrap();
                        for error in errors {
                            self.errors.add(self.src, error);
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
                            _ => Err(RawSyntaxError {
                                col: self.col,
                                len: 1,
                                msg: "invalid character literal".into(),
                                help_msg: "unrecognized escape character".into(),
                            }),
                        },
                        b'\x00'..=b'\x1F' | b'\x7F' => Err(RawSyntaxError {
                            col: self.col,
                            len: 1,
                            msg: "invalid character literal".into(),
                            help_msg: "cannot be a control character".into(),
                        }),
                        b'\'' => {
                            return Err(RawSyntaxError {
                                col: self.token_start_col,
                                len: 2,
                                msg: "invalid character literal".into(),
                                help_msg: "must not be empty".into(),
                            })
                        }
                        ch => Ok(ch),
                    };

                    match self.peek_next()? {
                        Some(b'\'') => {
                            self.col += 1;
                            Ok(Some(TokenKind::Literal(Literal::Char(code?))))
                        }
                        Some(_) | None => Err(RawSyntaxError {
                            col: self.token_start_col,
                            len: self.col - self.token_start_col + 1,
                            msg: "invalid character literal".into(),
                            help_msg: "missing closing single quote".into(),
                        }),
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
                        BracketKind::OpenCurly | BracketKind::OpenSquare => Err(RawSyntaxError {
                            col: self.token_start_col,
                            len: 1,
                            msg: "stray bracket".into(),
                            help_msg: "closes the wrong bracket".into(),
                        }),
                    },
                    None => Err(RawSyntaxError {
                        col: self.token_start_col,
                        len: 1,
                        msg: "stray bracket".into(),
                        help_msg: "was not opened before".into(),
                    }),
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
                        BracketKind::OpenCurly | BracketKind::OpenRound => Err(RawSyntaxError {
                            col: self.token_start_col,
                            len: 1,
                            msg: "stray bracket".into(),
                            help_msg: "closes the wrong bracket".into(),
                        }),
                    },
                    None => Err(RawSyntaxError {
                        col: self.token_start_col,
                        len: 1,
                        msg: "stray bracket".into(),
                        help_msg: "was not opened before".into(),
                    }),
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
                        BracketKind::OpenRound | BracketKind::OpenSquare => Err(RawSyntaxError {
                            col: self.token_start_col,
                            len: 1,
                            msg: "stray bracket".into(),
                            help_msg: "closes the wrong bracket".into(),
                        }),
                    },
                    None => Err(RawSyntaxError {
                        col: self.token_start_col,
                        len: 1,
                        msg: "stray bracket".into(),
                        help_msg: "was not opened before".into(),
                    }),
                },
                b':' => Ok(Some(TokenKind::Colon)),
                b';' => Ok(Some(TokenKind::SemiColon)),
                b',' => Ok(Some(TokenKind::Comma)),
                b'!' => match self.peek_next()? {
                    Some(b'=') => {
                        self.col += 1;
                        Ok(Some(TokenKind::Op(Op::NotEquals)))
                    }
                    _ => Ok(Some(TokenKind::Op(Op::Not))),
                },
                b'*' => match self.peek_next()? {
                    Some(b'*') => {
                        self.col += 1;
                        match self.peek_next()? {
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
                b'/' => match self.peek_next()? {
                    Some(b'=') => {
                        self.col += 1;
                        Ok(Some(TokenKind::Op(Op::DivideEquals)))
                    }
                    _ => Ok(Some(TokenKind::Op(Op::Divide))),
                },
                b'%' => match self.peek_next()? {
                    Some(b'=') => {
                        self.col += 1;
                        Ok(Some(TokenKind::Op(Op::RemainderEquals)))
                    }
                    _ => Ok(Some(TokenKind::Op(Op::Remainder))),
                },
                b'+' => match self.peek_next()? {
                    Some(b'=') => {
                        self.col += 1;
                        Ok(Some(TokenKind::Op(Op::PlusEquals)))
                    }
                    _ => Ok(Some(TokenKind::Op(Op::Plus))),
                },
                b'-' => match self.peek_next()? {
                    Some(b'=') => {
                        self.col += 1;
                        Ok(Some(TokenKind::Op(Op::MinusEquals)))
                    }
                    _ => Ok(Some(TokenKind::Op(Op::Minus))),
                },
                b'&' => match self.peek_next()? {
                    Some(b'&') => {
                        self.col += 1;
                        match self.peek_next()? {
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
                b'^' => match self.peek_next()? {
                    Some(b'=') => {
                        self.col += 1;
                        Ok(Some(TokenKind::Op(Op::BitXorEquals)))
                    }
                    _ => Ok(Some(TokenKind::Op(Op::BitXor))),
                },
                b'|' => match self.peek_next()? {
                    Some(b'|') => {
                        self.col += 1;
                        match self.peek_next()? {
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
                b'=' => match self.peek_next()? {
                    Some(b'=') => {
                        self.col += 1;
                        Ok(Some(TokenKind::Op(Op::EqualsEquals)))
                    }
                    _ => Ok(Some(TokenKind::Op(Op::Equals))),
                },
                b'>' => match self.peek_next()? {
                    Some(b'>') => {
                        self.col += 1;
                        match self.peek_next()? {
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
                b'<' => match self.peek_next()? {
                    Some(b'<') => {
                        self.col += 1;
                        match self.peek_next()? {
                            Some(b'=') => {
                                self.col += 1;
                                Ok(Some(TokenKind::Op(Op::LeftShiftEquals)))
                            }
                            _ => Ok(Some(TokenKind::Op(Op::LeftShift))),
                        }
                    }
                    Some(b'=') => {
                        self.col += 1;
                        match self.peek_next()? {
                            Some(b'>') => {
                                self.col += 1;
                                Ok(Some(TokenKind::Op(Op::Compare)))
                            }
                            _ => Ok(Some(TokenKind::Op(Op::LessOrEquals))),
                        }
                    }
                    _ => Ok(Some(TokenKind::Op(Op::Less))),
                },
                _ => Err(RawSyntaxError {
                    col: self.token_start_col,
                    len: 1,
                    msg: "unexpected character".into(),
                    help_msg: "unrecognized".into(),
                }),
            };
        }
    }
}

// iteration of characters and lines
impl<'src> Tokenizer<'src> {
    fn next_line(&mut self) -> Option<&'src Line> {
        self.line_idx += 1;
        if self.line_idx >= self.src.lines.len() {
            return None;
        }

        return Some(&self.src.lines[self.line_idx]);
    }

    // FIX(stefano): properly handle non ASCII characters related errors and column advancing
    // IDEA(stefano): allow utf-8 characters in strings, characters
    fn next(&mut self) -> Result<Option<u8>, RawSyntaxError> {
        if self.col >= self.src.code.len() {
            return Ok(None);
        }

        let next = self.src.code.as_bytes()[self.col];
        self.col += 1;
        return match next {
            ..=b'\x7F' => Ok(Some(next)),
            _ => Err(RawSyntaxError {
                col: self.col,
                len: 1,
                msg: "unrecognized character".into(),
                help_msg: "not a valid ASCII character".into(),
            }),
        };
    }

    fn peek_next(&self) -> Result<Option<&'src u8>, RawSyntaxError> {
        if self.col >= self.src.code.len() {
            return Ok(None);
        }

        let next = &self.src.code.as_bytes()[self.col];
        return match next {
            ..=b'\x7F' => Ok(Some(next)),
            _ => Err(RawSyntaxError {
                col: self.col,
                len: 1,
                msg: "unrecognized character".into(),
                help_msg: "not a valid ASCII character".into(),
            }),
        };
    }
}

// character literals
impl<'src> Tokenizer<'src> {
    fn next_in_char_literal(&mut self) -> Result<u8, RawSyntaxError> {
        return match self.next()? {
            Some(next) => Ok(next),
            None => Err(RawSyntaxError {
                col: self.token_start_col,
                len: self.col - self.token_start_col + 1,
                msg: "invalid character literal".into(),
                help_msg: "missing closing single quote".into(),
            }),
        };
    }
}

// string literals
impl<'src> Tokenizer<'src> {
    fn next_in_str_literal(&mut self) -> Result<u8, RawSyntaxError> {
        return match self.next()? {
            Some(next) => Ok(next),
            None => Err(RawSyntaxError {
                col: self.token_start_col,
                len: self.col - self.token_start_col + 1,
                msg: "invalid string literal".into(),
                help_msg: "missing closing double quote".into(),
            }),
        };
    }
}