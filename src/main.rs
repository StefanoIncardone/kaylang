use std::{io::{BufReader, BufRead, ErrorKind, BufWriter, Write, Seek, SeekFrom}, fs::File, env::{self, Args}, process::{ExitCode, Command}, fmt::Display, path::{Path, PathBuf}, borrow::Cow, cmp::Ordering, num::IntErrorKind};

mod color;
use crate::color::*;


trait Len {
    fn len( &self ) -> usize;
}

trait TypeOf {
    fn typ( &self ) -> Type;
}

trait Precedence {
    fn precedence( &self ) -> usize;
    fn precedence_over( &self, other: &Self ) -> Ordering;
}


#[derive( Debug, Clone )]
struct Str {
    text: Vec<u8>,
}


#[derive( Debug, Clone )]
enum Literal {
    // IDEA have different size integers and default to 32 bits for literals
    Int { value: isize },
    Char { value: u8 }, // only supporting ASCII characters for now
    Bool { value: bool },
    Str( Str ),
}

impl Display for Literal {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Int { value } => write!( f, "{}", value ),
            Self::Char { value } => write!( f, "'{}'", value.escape_ascii() ), // TODO create own escaping function
            Self::Bool { value } => write!( f, "{}", value ),
            Self::Str( string ) => {
                write!( f, "\"" )?;
                for character in &string.text {
                    write!( f, "{}", character.escape_ascii() )?;
                }
                write!( f, "\"" )
            },
        }
    }
}

impl Into<isize> for Literal {
    fn into( self ) -> isize {
        return match self {
            Self::Int { value } => value.into(),
            Self::Char { value } => value.into(),
            Self::Bool { value } => value.into(),
            Self::Str( string ) => string.text.len() as isize,
        }
    }
}

impl Len for Literal {
    fn len( &self ) -> usize {
        return match self {
            Self::Int { value } => value.to_string().len(),
            Self::Char { .. } => 1,
            Self::Bool { value } => value.to_string().len(),
            Self::Str( string ) => string.text.len() + 2,
        }
    }
}

impl TypeOf for Literal {
    fn typ( &self ) -> Type {
        return match self {
            Self::Int { .. } => Type::Int,
            Self::Char { .. } => Type::Char,
            Self::Bool { .. } => Type::Bool,
            Self::Str( _ ) => Type::Str,
        }
    }
}

impl Literal {
    fn display( &self ) {
        match self {
            Self::Int { value } => print!( "{}", value ),
            Self::Char { value } => print!( "{}", *value as char ),
            Self::Bool { value } => print!( "{}", value ),
            Self::Str( string ) =>
                for character in &string.text {
                    print!( "{}", *character as char );
                },
        }
    }
}


#[derive( Debug, PartialEq, Clone, Copy )]
enum Type {
    Int,
    Char,
    Bool,
    Str,
}

impl Display for Type {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Int => write!( f, "int" ),
            Self::Char => write!( f, "char" ),
            Self::Bool => write!( f, "bool" ),
            Self::Str => write!( f, "str" ),
        }
    }
}

impl Len for Type {
    fn len( &self ) -> usize {
        match self {
            Self::Int => core::mem::size_of::<isize>(),
            Self::Char => core::mem::size_of::<u8>(),
            Self::Bool => core::mem::size_of::<bool>(),
            Self::Str => core::mem::size_of::<*const u8>() + core::mem::size_of::<usize>(),
        }
    }
}


#[derive( Debug, Clone, Copy, PartialEq )]
enum Operator {
    Pow,
    PowEquals,

    Times,
    TimesEquals,
    Divide,
    DivideEquals,
    Remainder,
    RemainderEquals,

    Plus,
    PlusEquals,
    Minus,
    Negate,
    MinusEquals,

    EqualsEquals,
    NotEquals,
    Greater,
    GreaterOrEquals,
    Less,
    LessOrEquals,
    Compare,

    And,
    Or,
    Not,
    Xor
}

impl Display for Operator {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Pow => write!( f, "**" ),
            Self::PowEquals => write!( f, "**=" ),
            Self::Times => write!( f, "*" ),
            Self::TimesEquals => write!( f, "*=" ),
            Self::Divide => write!( f, "/" ),
            Self::DivideEquals => write!( f, "/=" ),
            Self::Remainder => write!( f, "%" ),
            Self::RemainderEquals => write!( f, "%=" ),

            Self::Plus => write!( f, "+" ),
            Self::PlusEquals => write!( f, "+=" ),
            Self::Minus | Self::Negate => write!( f, "-" ),
            Self::MinusEquals => write!( f, "-=" ),

            Self::EqualsEquals => write!( f, "==" ),
            Self::NotEquals => write!( f, "!=" ),
            Self::Greater => write!( f, ">" ),
            Self::GreaterOrEquals => write!( f, ">=" ),
            Self::Less => write!( f, "<" ),
            Self::LessOrEquals => write!( f, "<=" ),
            Self::Compare => write!( f, "<=>" ),

            Self::And => write!( f, "&&" ),
            Self::Or => write!( f, "||" ),
            Self::Not => write!( f, "!" ),
            Self::Xor => write!( f, "^^" ),
        }
    }
}

impl Len for Operator {
    fn len( &self ) -> usize {
        return match self {
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
            Self::Minus | Self::Negate => 1,
            Self::MinusEquals => 2,

            Self::EqualsEquals => 2,
            Self::NotEquals => 2,
            Self::Greater => 1,
            Self::GreaterOrEquals => 2,
            Self::Less => 1,
            Self::LessOrEquals => 2,
            Self::Compare => 3,

            Self::And => 2,
            Self::Or => 2,
            Self::Not => 1,
            Self::Xor => 2,
        }
    }
}

impl TypeOf for Operator {
    fn typ( &self ) -> Type {
        return match self {
            Self::Pow | Self::PowEquals
            | Self::Times | Self::TimesEquals
            | Self::Divide | Self::DivideEquals
            | Self::Remainder | Self::RemainderEquals
            | Self::Plus | Self::PlusEquals
            | Self::Minus | Self::MinusEquals | Self::Negate
            | Self::Compare => Type::Int,

            Self::EqualsEquals | Self::NotEquals
            | Self::Greater | Self::GreaterOrEquals
            | Self::Less | Self::LessOrEquals
            | Self::And | Self::Or | Self::Not | Self::Xor => Type::Bool,
        }
    }
}

impl Precedence for Operator {
    fn precedence( &self ) -> usize {
        return match self {
            Self::Negate | Self::And | Self::Or | Self::Not | Self::Xor => 0,

            Self::EqualsEquals | Self::NotEquals
            | Self::Greater | Self::GreaterOrEquals
            | Self::Less | Self::LessOrEquals
            | Self::Compare => 1,

            Self::Plus | Self::PlusEquals
            | Self::Minus | Self::MinusEquals => 2,

            Self::Times | Self::TimesEquals
            | Self::Divide | Self::DivideEquals
            | Self::Remainder | Self::RemainderEquals => 3,

            Self::Pow | Self::PowEquals => 4,
        }
    }

    fn precedence_over( &self, other: &Self ) -> Ordering {
        return self.precedence().cmp( &other.precedence() );
    }
}


#[derive( Debug, Clone, Copy )]
enum Mutability {
    Let,
    Var,
}

impl Display for Mutability {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Let => write!( f, "let" ),
            Self::Var => write!( f, "var" ),
        }
    }
}

impl Len for Mutability {
    fn len( &self ) -> usize {
        return match self {
            Self::Let => 3,
            Self::Var => 3,
        }
    }
}


#[derive( Debug, Clone, Copy )]
enum BracketKind {
    OpenRound,
    CloseRound,
    OpenCurly,
    CloseCurly,

}

impl Display for BracketKind {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::OpenRound => write!( f, "(" ),
            Self::CloseRound => write!( f, ")" ),
            Self::OpenCurly => write!( f, "{{" ),
            Self::CloseCurly => write!( f, "}}" ),
        }
    }
}

impl Len for BracketKind {
    fn len( &self ) -> usize {
        return match self {
            Self::OpenRound => 1,
            Self::CloseRound => 1,
            Self::OpenCurly => 1,
            Self::CloseCurly => 1,
        }
    }
}


#[derive( Debug, Clone )]
enum TokenKind {
    Comment( String ),
    Unexpected( String ),

    Bracket( BracketKind ),
    SemiColon,
    Equals,
    Op( Operator ),

    Literal( Literal ),
    Identifier( String ),
    Definition( Mutability ),

    // Keywords
    Print, // temporary way of printing values
    PrintLn, // temporary way of printing values followed by a newline
    True,
    False,
    If,
    Else,
    For,
    Break,
    Continue,

    // Special
    Empty,
    // TODO remove these two tokens
    SOF, // start of file
    EOF, // end of file
}

impl Display for TokenKind {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Comment( text ) => write!( f, "{}", text ),
            Self::Unexpected( text ) => write!( f, "{}", text ),

            Self::Bracket( bracket ) => write!( f, "{}", bracket ),
            Self::Equals => write!( f, "=" ),
            Self::SemiColon => write!( f, ";" ),

            Self::Literal( literal ) => write!( f, "{}", literal ),
            Self::Identifier( name ) => write!( f, "{}", name ),
            Self::Definition( kind ) => write!( f, "{}", kind ),

            Self::Op( op ) => write!( f, "{}", op ),

            Self::Print => write!( f, "print" ),
            Self::PrintLn => write!( f, "println" ),
            Self::True => write!( f, "true" ),
            Self::False => write!( f, "false" ),
            Self::If => write!( f, "if" ),
            Self::Else => write!( f, "else" ),
            Self::For => write!( f, "for" ),
            Self::Break => write!( f, "break" ),
            Self::Continue => write!( f, "continue" ),

            Self::Empty | Self::SOF | Self::EOF => write!( f, "" ),
        }
    }
}

impl Len for TokenKind {
    fn len( &self ) -> usize {
        return match self {
            Self::Comment( text ) => text.len(),
            Self::Unexpected( text ) => text.len(),

            Self::Bracket( bracket ) => bracket.len(),
            Self::Equals => 1,
            Self::SemiColon => 1,

            Self::Literal( typ ) => typ.len(),
            Self::Identifier( name ) => name.len(),
            Self::Definition( kind ) => kind.len(),

            Self::Op( op ) => op.len(),

            Self::Print => 5,
            Self::PrintLn => 7,
            Self::True => 4,
            Self::False => 5,
            Self::If => 2,
            Self::Else => 4,
            Self::For => 3,
            Self::Break => 5,
            Self::Continue => 8,

            Self::Empty => 1,
            Self::SOF => 1,
            Self::EOF => 1,
        }
    }
}


#[derive( Debug, Clone )]
struct Token {
    col: usize,
    kind: TokenKind,
}

#[derive( Debug, Clone, Copy )]
struct Line {
    byte_start: usize,
    number: usize,
    token_start_idx: usize,
    token_end_idx: usize,
}


// TODO create different kinds of errors for different stages of compilation
// TODO implement NOTE, HINT, HELP in error messages
#[derive( Debug )]
struct SyntaxError {
    line_byte_start: usize,
    line_number: usize,
    col: usize,
    len: usize,
    msg: Cow<'static, str>,
    help_msg: Cow<'static, str>,
}

#[derive( Debug )]
struct SyntaxErrors {
    src: BlitzSrc,
    errors: Vec<SyntaxError>,
}

impl SyntaxErrors {
    const ERROR: &'static str = colored!{
        text: "Error",
        foreground: Foreground::LightRed,
        bold: true,
    };


    // TODO make this not mut
    fn display( &mut self ) {
        let mut line_start = self.errors[ 0 ].line_byte_start;
        let mut line_text = String::new();
        let _ = self.src.src.seek( SeekFrom::Start( line_start as u64 ) );
        let _ = self.src.src.read_line( &mut line_text );

        for error in &self.errors {
            if line_start != error.line_byte_start {
                line_start = error.line_byte_start;
                line_text.clear();
                let _ = self.src.src.seek( SeekFrom::Start( line_start as u64 ) );
                let _ = self.src.src.read_line( &mut line_text );
            }

            let error_msg = Colored {
                text: error.msg.to_string(),
                foreground: Foreground::White,
                bold: true,
                ..Default::default()
            };

            let line_number_and_bar = Colored {
                text: format!( "{} |", error.line_number ),
                foreground: Foreground::LightBlue,
                ..Default::default()
            };

            let visualization_padding = line_number_and_bar.text.len();
            let at_padding = visualization_padding - 1;

            let at = Colored {
                text: format!( "{:>at_padding$}", "at" ),
                foreground: Foreground::LightRed,
                bold: true,
                ..Default::default()
            };

            let bar = Colored {
                text: format!( "{:>visualization_padding$}", "|" ),
                foreground: Foreground::LightBlue,
                ..Default::default()
            };

            let pointers_col = error.col - 1;
            let pointers_len = error.len;

            let pointers_and_help_msg = Colored {
                text: format!( "{:^>pointers_len$} {}", "", error.help_msg ),
                foreground: Foreground::LightRed,
                ..Default::default()
            };

            eprintln!(
                "{}: {}\
                \n{}: {}:{}:{}\
                \n{}\
                \n{} {}\
                \n{} {:pointers_col$}{}\n",
                SyntaxErrors::ERROR, error_msg,
                at, self.src.path.display(), error.line_number, error.col,
                bar,
                line_number_and_bar, line_text.trim_end(),
                bar, "", pointers_and_help_msg
            );
        }
    }
}


#[derive( Debug )]
struct Bracket {
    line_byte_start: usize,
    line_number: usize,
    col: usize,
    kind: BracketKind,
}


#[derive( Debug )]
struct Lexer {
    src: BlitzSrc,

    line: usize,
    line_byte_start: usize,
    line_token_start_idx: usize,
    col: usize,
    token_start_col: usize,

    lines: Vec<Line>,
    tokens: Vec<Token>,

    line_bytes: Vec<u8>,
    token_text: String,

    brackets: Vec<Bracket>,
    errors: Vec<SyntaxError>,
}

impl TryFrom<BlitzSrc> for Lexer {
    type Error = SyntaxErrors;


    fn try_from( src: BlitzSrc ) -> Result<Self, Self::Error> {
        let mut this = Self {
            src,
            line: 0,
            line_byte_start: 0,
            line_token_start_idx: 0,
            col: 0,
            token_start_col: 1,
            lines: Vec::new(),
            tokens: vec![ Token { col: 1, kind: TokenKind::SOF } ],
            line_bytes: Vec::new(),
            token_text: String::new(),
            brackets: Vec::new(),
            errors: Vec::new(),
        };

        loop {
            this.line_bytes.clear();
            match this.src.src.read_until( b'\n', &mut this.line_bytes ) {
                Ok( chars_read ) => {
                    this.line += 1;
                    this.col = 0;
                    this.token_start_col = 1;

                    let mut line_len = this.line_bytes.len();
                    if line_len == 0 {
                        this.tokens.push( Token { col: 1, kind: TokenKind::Empty } );
                    }
                    else {
                        while line_len > 0 && this.line_bytes[ line_len - 1 ].is_ascii_whitespace() {
                            line_len -= 1;
                        }

                        this.line_bytes.truncate( line_len );
                        loop {
                            let token = match this.tokeninze_next() {
                                Ok( None ) => break,
                                Ok( Some( kind ) ) => Token { col: this.token_start_col, kind },
                                Err( err ) => {
                                    this.errors.push( err );
                                    Token { col: this.token_start_col, kind: TokenKind::Unexpected( this.token_text.clone() ) }
                                }
                            };

                            this.tokens.push( token );
                        }
                    }

                    let reached_eof = chars_read == line_len;
                    if reached_eof {
                        this.tokens.push( Token { col: this.token_start_col, kind: TokenKind::EOF } );
                    }

                    let line = Line {
                        byte_start: this.line_byte_start,
                        number: this.line,
                        token_start_idx: this.line_token_start_idx,
                        token_end_idx: this.tokens.len()
                    };

                    this.line_token_start_idx = this.tokens.len();
                    this.line_byte_start += chars_read;
                    this.lines.push( line );

                    if reached_eof {
                        break;
                    }
                },
                Err( err ) => panic!( "Error: {}", err ),
            }
        }

        // FIX insert bracket related errors in the correct place, right now they appear at the end, out of order from the rest of the errors
        for bracket in &this.brackets {
            // there can only be open brackets at this point
            this.errors.push( SyntaxError {
                line_byte_start: bracket.line_byte_start,
                line_number: bracket.line_number,
                col: bracket.col,
                len: bracket.kind.len(),
                msg: "stray bracket".into(),
                help_msg: "was not closed".into()
            } );
        }

        return match this.errors.is_empty() {
            true => Ok( this ),
            false => Err( SyntaxErrors { src: this.src, errors: this.errors } ),
        }
    }
}

impl Lexer {
    fn iter<'lexer>( &'lexer self ) -> TokenCursor<'lexer> {
        return TokenCursor { line: 0, lines: &self.lines, token: 0, tokens: &self.tokens };
    }

    // FIX properly handle non ASCII characters
        // IDEA only allow utf-8 characters in strings, characters and comments
    fn next( &mut self ) -> Result<Option<u8>, SyntaxError> {
        if self.col >= self.line_bytes.len() {
            return Ok( None );
        }

        let next = self.line_bytes[ self.col ];
        self.col += 1;
        self.token_text.push( next as char );
        return match next {
            ..=b'\x7F' => Ok( Some( next ) ),
            _ => Err( SyntaxError {
                line_byte_start: self.line_byte_start,
                line_number: self.line,
                col: self.col,
                len: 1,
                msg: "unrecognized character".into(),
                help_msg: "not a valid ASCII character".into()
            }),
        }
    }

    fn peek_next<'src>( &'src self ) -> Result<Option<&'src u8>, SyntaxError> {
        if self.col >= self.line_bytes.len() {
            return Ok( None );
        }

        let next = &self.line_bytes[ self.col ];
        return match next {
            ..=b'\x7F' => Ok( Some( next ) ),
            _ => Err( SyntaxError {
                line_byte_start: self.line_byte_start,
                line_number: self.line,
                col: self.col,
                len: 1,
                msg: "unrecognized character".into(),
                help_msg: "not a valid ASCII character".into()
            }),
        }
    }

    fn next_char( &mut self ) -> Result<u8, SyntaxError> {
        return match self.next()? {
            Some( b'\n' ) | None => Err( SyntaxError {
                line_byte_start: self.line_byte_start,
                line_number: self.line,
                col: self.token_start_col,
                len: self.token_text.len(),
                msg: "invalid character literal".into(),
                help_msg: "missing closing single quote".into()
            }),
            Some( next ) => Ok( next ),
        }
    }

    fn next_str_char( &mut self ) -> Result<u8, SyntaxError> {
        return match self.next()? {
            Some( b'\n' ) | None => Err( SyntaxError {
                line_byte_start: self.line_byte_start,
                line_number: self.line,
                col: self.token_start_col,
                len: self.token_text.len(),
                msg: "invalid string literal".into(),
                help_msg: "missing closing double quote".into()
            }),
            Some( next ) => Ok( next ),
        }
    }

    fn tokeninze_next( &mut self ) -> Result<Option<TokenKind>, SyntaxError> {
        // NOTE this loop is ever going to run just once, it's here just so we can use continue and break
        loop {
            self.token_text.clear();
            let next = match self.next()? {
                None => return Ok( None ),
                Some( ch ) => ch,
            };

            self.token_start_col = self.col;
            return match next {
                // ignore whitespace
                b'\t' | b'\x0C' | b'\r' | b' ' => continue,
                b'a'..=b'z' | b'A'..=b'Z' | b'_'  => {
                    let mut contains_non_ascii = false;
                    loop {
                        match self.peek_next() {
                            Ok( Some( next @ (b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_') ) ) => {
                                self.token_text.push( *next as char );
                                self.col += 1;
                            },
                            Ok( Some( _ ) | None ) => break,
                            Err( _ ) => {
                                let next = self.line_bytes[ self.col ];
                                self.token_text.push( next as char );
                                contains_non_ascii = true;
                            }
                        }
                    }

                    if contains_non_ascii {
                        Err( SyntaxError {
                            line_byte_start: self.line_byte_start,
                            line_number: self.line,
                            col: self.token_start_col,
                            len: self.token_text.len(),
                            msg: "invalid identifier".into(),
                            help_msg: "contains non-ASCII characters".into()
                        })
                    }
                    else {
                        let identifier = match self.token_text.as_str() {
                            "let" => TokenKind::Definition( Mutability::Let ),
                            "var" => TokenKind::Definition( Mutability::Var ),
                            "print" => TokenKind::Print,
                            "println" => TokenKind::PrintLn,
                            "true" => TokenKind::True,
                            "false" => TokenKind::False,
                            "if" => TokenKind::If,
                            "else" => TokenKind::Else,
                            "for" => TokenKind::For,
                            "break" => TokenKind::Break,
                            "continue" => TokenKind::Continue,
                            _ => TokenKind::Identifier( self.token_text.clone() ),
                        };

                        Ok( Some( identifier ) )
                    }
                },
                b'0'..=b'9' => {
                    let mut contains_non_ascii = false;
                    loop {
                        match self.peek_next() {
                            Ok( Some( next @ (b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_') ) ) => {
                                self.token_text.push( *next as char );
                                self.col += 1;
                            },
                            Ok( Some( _ ) | None ) => break,
                            Err( _ ) => {
                                let next = self.line_bytes[ self.col ];
                                self.token_text.push( next as char );
                                contains_non_ascii = true;
                            }
                        }
                    }

                    match self.token_text.parse() { // TODO create own number parsing function
                        Ok( value ) => Ok( Some( TokenKind::Literal( Literal::Int { value } ) ) ),
                        Err( err ) => match err.kind() {
                            IntErrorKind::InvalidDigit =>
                                if !contains_non_ascii {
                                    Err( SyntaxError {
                                        line_byte_start: self.line_byte_start,
                                        line_number: self.line,
                                        col: self.token_start_col,
                                        len: self.token_text.len(),
                                        msg: "invalid number literal".into(),
                                        help_msg: "contains non-digit characters".into()
                                    })
                                }
                                else {
                                    Err( SyntaxError {
                                        line_byte_start: self.line_byte_start,
                                        line_number: self.line,
                                        col: self.token_start_col,
                                        len: self.token_text.len(),
                                        msg: "invalid number literal".into(),
                                        help_msg: "contains non-ASCII characters".into()
                                    })
                                },
                            IntErrorKind::PosOverflow => Err( SyntaxError {
                                line_byte_start: self.line_byte_start,
                                line_number: self.line,
                                col: self.token_start_col,
                                len: self.token_text.len(),
                                msg: "invalid number literal".into(),
                                help_msg: format!( "overflows a {} bit signed integer (over {})", isize::BITS, isize::MAX ).into()
                            }),
                            IntErrorKind::NegOverflow => Err( SyntaxError {
                                line_byte_start: self.line_byte_start,
                                line_number: self.line,
                                col: self.token_start_col,
                                len: self.token_text.len(),
                                msg: "invalid number literal".into(),
                                help_msg: format!( "underflows a {} bit signed integer (under {})", isize::BITS, isize::MIN ).into()
                            }),
                            IntErrorKind::Empty | std::num::IntErrorKind::Zero => unreachable!(),
                            _ => Err( SyntaxError {
                                line_byte_start: self.line_byte_start,
                                line_number: self.line,
                                col: self.token_start_col,
                                len: self.token_text.len(),
                                msg: "invalid number literal".into(),
                                help_msg: format!( "{}", err ).into()
                            }),
                        },
                    }
                },
                b'#' => {
                    self.col = self.line_bytes.len(); // consume the rest of the tokens in the current line
                    let comment = String::from_utf8( self.line_bytes[ self.token_start_col - 1..self.line_bytes.len() ].to_vec() ).unwrap();
                    Ok( Some( TokenKind::Comment( comment ) ) )
                },
                b'"' =>
                    loop { // FIX proper string tokenization: return multiple errors
                        match self.next_str_char()? {
                            b'\\' => match self.next_str_char()? {
                                b'\\'| b'n'| b't'| b'\''| b'"' => (),
                                _ => break Err( SyntaxError {
                                    line_byte_start: self.line_byte_start,
                                    line_number: self.line,
                                    col: self.token_start_col,
                                    len: self.token_text.len(),
                                    msg: "invalid string character".into(),
                                    help_msg: "contains invalid escape characters, check the documentation for a list of valid escape characters".into()
                                }),
                            },
                            b'\x00'..=b'\x1F' | b'\x7F' => break Err( SyntaxError {
                                line_byte_start: self.line_byte_start,
                                line_number: self.line,
                                col: self.token_start_col,
                                len: self.token_text.len(),
                                msg: "invalid string literal".into(),
                                help_msg: "contains a control character".into()
                            }),
                            b'"' => break Ok( Some(
                                TokenKind::Literal(
                                    Literal::Str(
                                        Str { text: self.token_text.as_bytes()[ 1..self.token_text.len() - 1 ].to_vec() }
                                    )
                                )
                            )),
                            _ => (),
                        }
                    },
                b'\'' => { // FIX proper character tokenization: return muliple errors
                    let character = match self.next_char()? {
                        b'\\' => match self.next_char()? {
                            b'\\' => Ok( b'\\' ),
                            b'n' => Ok( b'\n' ),
                            b't' => Ok( b'\t' ),
                            b'\'' => Ok( b'\'' ),
                            b'"' => Ok( b'"' ),
                            _ => Err( SyntaxError {
                                line_byte_start: self.line_byte_start,
                                line_number: self.line,
                                col: self.token_start_col,
                                len: self.token_text.len(),
                                msg: "invalid character literal".into(),
                                help_msg: "check the documentation for a list of valid escape characters".into()
                            }),
                        },
                        b'\x00'..=b'\x1F' | b'\x7F' => Err( SyntaxError {
                            line_byte_start: self.line_byte_start,
                            line_number: self.line,
                            col: self.token_start_col,
                            len: self.token_text.len(),
                            msg: "invalid character literal".into(),
                            help_msg: "cannot be a control character".into()
                        }),
                        ch => Ok( ch ),
                    };

                    match character {
                        Ok( b'\'' ) => Err( SyntaxError {
                            line_byte_start: self.line_byte_start,
                            line_number: self.line,
                            col: self.token_start_col,
                            len: self.token_text.len(),
                            msg: "invalid character literal".into(),
                            help_msg: "must not be empty".into()
                        }),
                        Ok( ch ) => match self.next()? {
                            Some( b'\'' ) => Ok( Some( TokenKind::Literal( Literal::Char { value: ch } ) ) ),
                            Some( _ ) | None => Err( SyntaxError {
                                line_byte_start: self.line_byte_start,
                                line_number: self.line,
                                col: self.token_start_col,
                                len: self.token_text.len(),
                                msg: "invalid character literal".into(),
                                help_msg: "missing closing single quote".into()
                            })
                        },
                        Err( err ) => Err( err )
                    }
                },
                b'(' => {
                    let kind = BracketKind::OpenRound;
                    self.brackets.push( Bracket {
                        line_byte_start: self.line_byte_start,
                        line_number: self.line,
                        col: self.token_start_col,
                        kind
                    } );
                    Ok( Some( TokenKind::Bracket( kind ) ) )
                },
                b')' => match self.brackets.pop() {
                    Some( Bracket { kind: BracketKind::OpenRound, .. } ) =>
                        Ok( Some( TokenKind::Bracket( BracketKind::CloseRound ) ) ),
                    Some( Bracket { kind: BracketKind::CloseCurly | BracketKind::CloseRound, .. } ) =>
                        Ok( Some( TokenKind::Bracket( BracketKind::CloseRound ) ) ),
                    Some( Bracket { kind: BracketKind::OpenCurly, .. } ) => Err( SyntaxError {
                        line_byte_start: self.line_byte_start,
                        line_number: self.line,
                        col: self.token_start_col,
                        len: 1,
                        msg: "stray bracket".into(),
                        help_msg: "closes the wrong bracket".into()
                    }),
                    None => Err( SyntaxError {
                        line_byte_start: self.line_byte_start,
                        line_number: self.line,
                        col: self.token_start_col,
                        len: 1,
                        msg: "stray bracket".into(),
                        help_msg: "was not opened before".into()
                    }),
                },
                b'{' => {
                    let kind = BracketKind::OpenCurly;
                    self.brackets.push( Bracket {
                        line_byte_start: self.line_byte_start,
                        line_number: self.line,
                        col: self.token_start_col,
                        kind
                    } );
                    Ok( Some( TokenKind::Bracket( kind ) ) )
                },
                b'}' => match self.brackets.pop() {
                    Some( Bracket { kind: BracketKind::OpenCurly, .. } ) =>
                        Ok( Some( TokenKind::Bracket( BracketKind::CloseCurly ) ) ),
                    Some( Bracket { kind: BracketKind::CloseCurly | BracketKind::CloseRound, .. } ) =>
                        Ok( Some( TokenKind::Bracket( BracketKind::CloseCurly ) ) ),
                    Some( Bracket { kind: BracketKind::OpenRound, .. } ) => Err( SyntaxError {
                        line_byte_start: self.line_byte_start,
                        line_number: self.line,
                        col: self.token_start_col,
                        len: 1,
                        msg: "stray bracket".into(),
                        help_msg: "closes the wrong bracket".into()
                    }),
                    None => Err( SyntaxError {
                        line_byte_start: self.line_byte_start,
                        line_number: self.line,
                        col: self.token_start_col,
                        len: 1,
                        msg: "stray bracket".into(),
                        help_msg: "was not opened before".into()
                    }),
                },
                b';' => Ok( Some( TokenKind::SemiColon ) ),
                b'*' => match self.peek_next()? {
                    Some( b'*' ) => {
                        let _ = self.next();

                        match self.peek_next()? {
                            Some( b'=' ) => {
                                let _ = self.next();
                                Ok( Some( TokenKind::Op( Operator::PowEquals ) ) )
                            }
                            _ => Ok( Some( TokenKind::Op( Operator::Pow ) ) ),
                        }
                    },
                    Some( b'=' ) => {
                        let _ = self.next();
                        Ok( Some( TokenKind::Op( Operator::TimesEquals ) ) )
                    },
                    _ => Ok( Some( TokenKind::Op( Operator::Times ) ) ),
                },
                b'/' => match self.peek_next()? {
                    Some( b'=' ) => {
                        let _ = self.next();
                        Ok( Some( TokenKind::Op( Operator::DivideEquals ) ) )
                    },
                    _ => Ok( Some( TokenKind::Op( Operator::Divide ) ) ),
                },
                b'%' => match self.peek_next()? {
                    Some( b'=' ) => {
                        let _ = self.next();
                        Ok( Some( TokenKind::Op( Operator::RemainderEquals ) ) )
                    },
                    _ => Ok( Some( TokenKind::Op( Operator::Remainder ) ) ),
                },
                b'+' => match self.peek_next()? {
                    Some( b'=' ) => {
                        let _ = self.next();
                        Ok( Some( TokenKind::Op( Operator::PlusEquals ) ) )
                    },
                    _ => Ok( Some( TokenKind::Op( Operator::Plus ) ) )
                },
                b'-' => match self.peek_next()? {
                    Some( b'=' ) => {
                        let _ = self.next();
                        Ok( Some( TokenKind::Op( Operator::MinusEquals ) ) )
                    },
                    _ => Ok( Some( TokenKind::Op( Operator::Minus ) ) ),
                },
                b'=' => match self.peek_next()? {
                    Some( b'=' ) => {
                        let _ = self.next();
                        Ok( Some( TokenKind::Op( Operator::EqualsEquals ) ) )
                    },
                    _ => Ok( Some( TokenKind::Equals ) ),
                },
                b'!' => match self.peek_next()? {
                    Some( b'=' ) => {
                        let _ = self.next();
                        Ok( Some( TokenKind::Op( Operator::NotEquals ) ) )
                    },
                    _ => Ok( Some( TokenKind::Op( Operator::Not ) ) ),
                },
                b'>' => match self.peek_next()? {
                    Some( b'=' ) => {
                        let _ = self.next();
                        Ok( Some( TokenKind::Op( Operator::GreaterOrEquals ) ) )
                    },
                    _ => Ok( Some( TokenKind::Op( Operator::Greater ) ) ),
                },
                b'<' => match self.peek_next()? {
                    Some( b'=' ) => {
                        let _ = self.next();

                        match self.peek_next()? {
                            Some( b'>' ) => {
                                let _ = self.next();
                                Ok( Some( TokenKind::Op( Operator::Compare ) ) )
                            },
                            _ => Ok( Some( TokenKind::Op( Operator::LessOrEquals ) ) ),
                        }
                    },
                    _ => Ok( Some( TokenKind::Op( Operator::Less ) ) ),
                },
                b'^' => match self.peek_next()? {
                    Some( b'^' ) => {
                        let _ = self.next();
                        Ok( Some( TokenKind::Op( Operator::Xor ) ) )
                    },
                    _ => Err( SyntaxError {
                        line_byte_start: self.line_byte_start,
                        line_number: self.line,
                        col: self.token_start_col,
                        len: 1,
                        msg: "unexpected character".into(),
                        help_msg: "did you mean '^^'?".into()
                    }),
                },
                b'&' => match self.peek_next()? {
                    Some( b'&' ) => {
                        let _ = self.next();
                        Ok( Some( TokenKind::Op( Operator::And ) ) )
                    },
                    _ => Err( SyntaxError {
                        line_byte_start: self.line_byte_start,
                        line_number: self.line,
                        col: self.token_start_col,
                        len: 1,
                        msg: "unexpected character".into(),
                        help_msg: "did you mean '&&'?".into()
                    }),
                },
                b'|' => match self.peek_next()? {
                    Some( b'|' ) => {
                        let _ = self.next();
                        Ok( Some( TokenKind::Op( Operator::Or ) ) )
                    },
                    _ => Err( SyntaxError {
                        line_byte_start: self.line_byte_start,
                        line_number: self.line,
                        col: self.token_start_col,
                        len: 1,
                        msg: "unexpected character".into(),
                        help_msg: "did you mean '||'?".into()
                    }),
                },
                b'\n' => unreachable!( "line text should have been trimmed already" ),
                _ => Err( SyntaxError {
                    line_byte_start: self.line_byte_start,
                    line_number: self.line,
                    col: self.token_start_col,
                    len: 1,
                    msg: "unexpected character".into(),
                    help_msg: "unrecognized".into()
                }),
            }
        }
    }
}


#[derive( Debug, Clone, Copy )]
struct TokenPosition<'lexer> {
    line: &'lexer Line,
    token: &'lexer Token,
}

#[derive( Debug )]
struct TokenCursor<'lexer> {
    line: usize,
    lines: &'lexer [Line],

    token: usize,
    tokens: &'lexer [Token],
}

// TODO implement methods that return only tokens or lines since lines are only really needed when reporting errors
impl<'lexer> TokenCursor<'lexer> {
    fn current( &self ) -> TokenPosition<'lexer> {
        return TokenPosition{ line: &self.lines[ self.line ], token: &self.tokens[ self.token ] };
    }

    fn current_or_next( &mut self ) -> Option<TokenPosition<'lexer>> {
        return Some( self.current() ).or_next( self );
    }

    fn next( &mut self ) -> Option<TokenPosition<'lexer>> {
        if self.token == self.tokens.len() - 1 {
            return None;
        }

        self.token += 1;
        let token = &self.tokens[ self.token ];

        let mut line = &self.lines[ self.line ];
        if self.token > line.token_end_idx {
            self.line += 1;
            line = &self.lines[ self.line ];
        }

        return Some( TokenPosition { line, token } ).or_next( self );
    }

    fn previous( &mut self ) -> Option<TokenPosition<'lexer>> {
        if self.token == 0 {
            return None;
        }

        self.token -= 1;
        let token = &self.tokens[ self.token ];

        let mut line = &self.lines[ self.line ];
        if self.token < line.token_start_idx {
            self.line -= 1;
            line = &self.lines[ self.line ];
        }

        return Some( TokenPosition { line, token } ).or_previous( self );
    }

    fn peek_next( &mut self ) -> Option<TokenPosition<'lexer>> {
        let (starting_line, starting_token) = (self.line, self.token);
        let next = self.next();
        (self.line, self.token) = (starting_line, starting_token);
        return next;
    }

    fn peek_previous( &mut self ) -> Option<TokenPosition<'lexer>> {
        let (starting_line, starting_token) = (self.line, self.token);
        let previous = self.previous();
        (self.line, self.token) = (starting_line, starting_token);
        return previous;
    }
}


trait BoundedPosition<'lexer> {
    type Error;


    fn bounded( self, tokens: &mut TokenCursor<'lexer>, err_msg: impl Into<String> ) -> Result<Self, Self::Error> where Self: Sized;
    fn or_next( self, tokens: &mut TokenCursor<'lexer> ) -> Self where Self: Sized;
    fn or_previous( self, tokens: &mut TokenCursor<'lexer> ) -> Self where Self: Sized;
}

impl<'lexer> BoundedPosition<'lexer> for Option<TokenPosition<'lexer>> {
    type Error = SyntaxError;


    fn bounded( self, tokens: &mut TokenCursor<'lexer>, err_msg: impl Into<String> ) -> Result<Self, Self::Error> {
        return match self {
            Some( current ) => match current.token.kind {
                TokenKind::EOF => {
                    // we are always sure that there is at least the SOF token before the EOF token, so we can safely unwrap
                    let previous = tokens.peek_previous().unwrap();
                    Err( SyntaxError {
                        line_byte_start: previous.line.byte_start,
                        line_number: previous.line.number,
                        col: previous.token.col,
                        len: previous.token.kind.len(),
                        msg: err_msg.into().into(),
                        help_msg: "file ended after here instead".into()
                    } )
                },
                TokenKind::SOF => {
                    // we are always sure that there is at least the EOF token after the SOF token, so we can safely unwrap
                    let next = tokens.peek_previous().unwrap();
                    Err( SyntaxError {
                        line_byte_start: next.line.byte_start,
                        line_number: next.line.number,
                        col: next.token.col,
                        len: next.token.kind.len(),
                        msg: err_msg.into().into(),
                        help_msg: "file started before here instead".into()
                    } )
                },
                _ => Ok( self ),
            }
            None => Ok( None ),
        }
    }

    fn or_next( self, tokens: &mut TokenCursor<'lexer> ) -> Self {
        return match self?.token.kind {
            TokenKind::Comment( _ ) | TokenKind::Empty => tokens.next(),
            _ => self,
        }
    }

    fn or_previous( self, tokens: &mut TokenCursor<'lexer> ) -> Self {
        return match self?.token.kind {
            TokenKind::Comment( _ ) | TokenKind::Empty => tokens.previous(),
            _ => self,
        }
    }
}


#[derive( Debug, Clone )]
enum Expression {
    Literal( Literal ),
    Unary { op: Operator, operand: Box<Expression> },
    Binary { lhs: Box<Expression>, op: Operator, rhs: Box<Expression> },
    Identifier( String, Type ),
}

impl Display for Expression {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Literal( literal ) => write!( f, "{}", literal ),
            Self::Unary { op, operand } => match op {
                Operator::Negate => write!( f, "-{}", operand ),
                Operator::Not => write!( f, "not {}", operand ),
                _ => unreachable!(),
            },
            Self::Binary { lhs, op, rhs } => write!( f, "({} {} {})", lhs, op, rhs ),
            Self::Identifier( name, _ ) => write!( f, "{}", name ),
        }
    }
}

impl TypeOf for Expression {
    fn typ( &self ) -> Type {
        return match self {
            Self::Literal( literal ) => literal.typ(),
            Self::Unary { operand, .. } => operand.typ(),
            Self::Binary { op, .. } => op.typ(),
            Self::Identifier( _, typ ) => *typ,
        }
    }
}

impl Precedence for Expression {
    fn precedence( &self ) -> usize {
        return match self {
            Self::Unary { .. } => 0,
            Self::Literal( _ ) | Self::Identifier( _, _) => 1,
            Self::Binary { .. } => 2,
        }
    }

    fn precedence_over( &self, other: &Self ) -> Ordering {
        return self.precedence().cmp( &other.precedence() );
    }
}


#[derive( Debug )]
struct Definition {
    mutability: Mutability,
    name: String,
    typ: Type,
}

#[derive( Debug )]
struct Scope {
    parent: Option<ScopeIdx>,
    definitions: Vec<Definition>,
    nodes: Vec<Node>,
}

type ScopeIdx = usize;

#[derive( Debug )]
struct Scopes {
    scopes: Vec<Scope>,
    current: ScopeIdx,
}

// Creation of scopes during AST creation
impl<'this> Scopes {
    fn create_and_add_to_current( &mut self ) -> ScopeIdx {
        let new_scope = self.scopes.len();
        self.scopes.push( Scope { parent: Some( self.current ), definitions: Vec::new(), nodes: Vec::new() } );
        self.scopes[ self.current ].nodes.push( Node::Scope( new_scope ) );
        self.current = new_scope;
        return self.current;
    }

    fn create( &mut self ) -> ScopeIdx {
        self.scopes.push( Scope { parent: Some( self.current ), definitions: Vec::new(), nodes: Vec::new() } );
        self.current = self.scopes.len() - 1;
        return self.current;
    }

    fn current( &'this self ) -> &'this Scope {
        return &self.scopes[ self.current ];
    }
}

// Resolution of identifiers
impl<'this> Scopes {
    fn resolve( &'this self, name: &str ) -> Option<&'this Definition> {
        let mut current_scope = self.current();

        loop {
            for definition in &current_scope.definitions {
                if definition.name == name {
                    return Some( definition );
                }
            }

            current_scope = &self.scopes[ current_scope.parent? ];
        }
    }

    fn resolve_mut( &'this mut self, name: &str ) -> Option<&'this mut Definition> {
        let mut current_scope = self.current;

        loop {
            let scope = &self.scopes[ current_scope ];
            for (i, definition) in scope.definitions.iter().enumerate() {
                if definition.name == name {
                    return Some( &mut self.scopes[ current_scope ].definitions[ i ] );
                }
            }

            current_scope = scope.parent?;
        }
    }
}


#[derive( Debug, Clone )]
struct If {
    condition: Expression,
    scope: ScopeIdx,
}

impl Display for If {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return write!( f, "if {}", self.condition );
    }
}

#[derive( Debug, Clone )]
struct IfStatement {
    ifs: Vec<If>,
    els: Option<ScopeIdx>,
}


#[allow( dead_code )]
#[derive( Debug, Clone )]
struct ForStatement {
    pre_statement: Option<Box<Node>>,
    condition: Option<Expression>,
    post_statement: Option<Box<Node>>,
    scope: ScopeIdx,
}


enum Statement {
    // TODO remove Empty and Stop variants, and make methods return Option<Statement>
    Empty, // NOTE this is going to be gone completely as it's just to signal to do nothing
    Stop, // NOTE this is going to become the None variant
    Single( Node ),
    Multiple( Vec<Node> ),
}

#[derive( Debug, Clone )]
enum Node {
    Expression( Expression ),
    Print( Expression ),
    If( IfStatement ),
    For( ForStatement ),
    Break,
    Continue,

    Definition( String, Expression ),
    Assignment( String, Expression ),
    Scope( ScopeIdx ),
}

impl Display for Node {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Expression( expression ) => write!( f, "{}", expression ),
            Self::Print( argument ) => write!( f, "print {}", argument ),
            Self::If( iff ) => write!( f, "{}", iff.ifs[ 0 ] ),
            Self::For( forr ) => match &forr.condition {
                None => write!( f, "for" ),
                Some( condition ) => write!( f, "for {}", condition ),
            },

            Self::Break | Self::Continue
            | Self::Definition( _, _ ) | Self::Assignment( _, _ )
            | Self::Scope( _ ) => unreachable!(),
        }
    }
}

// TODO process entire statement for syntactical correctness and then report all the errors
    // IDEA create Parser class that builds the AST, and then validate the AST afterwards
#[derive( Debug )]
struct AST {
    scopes: Scopes,
    for_depth: usize,
}

// IDEA reintroduce "semicolon" function, taking out the semicolon checking out of the expression parsing
impl TryFrom<Lexer> for AST {
    type Error = SyntaxErrors;

    fn try_from( lexer: Lexer ) -> Result<Self, Self::Error> {
        let mut this = Self {
            scopes: Scopes { scopes: Vec::new(), current: 0 },
            for_depth: 0,
        };

        let mut tokens = lexer.iter();
        let mut errors: Vec<SyntaxError> = Vec::new();

        this.parse_scope( &mut tokens, &mut errors );

        return match errors.is_empty() {
            true => Ok( this ),
            false => Err( SyntaxErrors { src: lexer.src, errors } ),
        }
    }
}

// Parsing of scopes
impl<'lexer> AST {
    fn parse( &mut self, tokens: &mut TokenCursor<'lexer>, errors: &mut Vec<SyntaxError>, current: TokenPosition<'lexer> ) -> Result<Statement, SyntaxError> {
        return match current.token.kind {
            TokenKind::Literal( _ )
            | TokenKind::True | TokenKind::False
            | TokenKind::Bracket( BracketKind::OpenRound )
            | TokenKind::Op( Operator::Minus | Operator::Not ) => match self.expression( tokens ) {
                Ok( expression ) => Ok( Statement::Single( Node::Expression( expression ) ) ),
                Err( err ) => Err( err ),
            },
            TokenKind::Definition( _ ) => self.variable_definition( tokens ),
            TokenKind::Print | TokenKind::PrintLn => self.print( tokens ),
            TokenKind::Identifier( _ ) => match tokens.peek_next() {
                Some( next ) => match next.token.kind {
                    TokenKind::Equals
                    | TokenKind::Op( Operator::PowEquals )
                    | TokenKind::Op( Operator::TimesEquals )
                    | TokenKind::Op( Operator::DivideEquals )
                    | TokenKind::Op( Operator::PlusEquals )
                    | TokenKind::Op( Operator::MinusEquals ) => self.variable_reassignment( tokens ),
                    _ => match self.expression( tokens ) {
                        Ok( expression ) => Ok( Statement::Single( Node::Expression( expression ) ) ),
                        Err( err ) => Err( err ),
                    },
                },
                None => Ok( Statement::Stop ),
            },
            TokenKind::If => self.if_statement( tokens, errors ),
            TokenKind::Else => {
                tokens.next();
                Err( SyntaxError {
                    line_byte_start: current.line.byte_start,
                    line_number: current.line.number,
                    col: current.token.col,
                    len: current.token.kind.len(),
                    msg: "invalid if statement".into(),
                    help_msg: "stray else block".into()
                })
            },
            TokenKind::For => self.for_statement( tokens, errors ),
            TokenKind::Break => {
                tokens.next();
                match self.for_depth {
                    0 => Err( SyntaxError {
                        line_byte_start: current.line.byte_start,
                        line_number: current.line.number,
                        col: current.token.col,
                        len: current.token.kind.len(),
                        msg: "invalid break statement".into(),
                        help_msg: "cannot be used outside of loops".into()
                    }),
                    _ => Ok( Statement::Single( Node::Break ) ),
                }
            },
            TokenKind::Continue => {
                tokens.next();
                match self.for_depth {
                    0 => Err( SyntaxError {
                        line_byte_start: current.line.byte_start,
                        line_number: current.line.number,
                        col: current.token.col,
                        len: current.token.kind.len(),
                        msg: "invalid continue statement".into(),
                        help_msg: "cannot be used outside of loops".into()
                    }),
                    _ => Ok( Statement::Single( Node::Continue ) ),
                }
            },
            TokenKind::Bracket( BracketKind::OpenCurly ) => {
                self.scopes.create_and_add_to_current();
                tokens.next();
                Ok( Statement::Empty )
            },
            TokenKind::Bracket( BracketKind::CloseCurly ) => {
                self.scopes.current = self.scopes.current().parent.unwrap_or( 0 );
                tokens.next();
                Ok( Statement::Stop )
            },
            TokenKind::Bracket( BracketKind::CloseRound ) => {
                tokens.next();
                Err( SyntaxError {
                    line_byte_start: current.line.byte_start,
                    line_number: current.line.number,
                    col: current.token.col,
                    len: current.token.kind.len(),
                    msg: "invalid expression".into(),
                    help_msg: "stray closed parenthesis".into()
                })
            },
            TokenKind::Equals => {
                tokens.next();
                Err( SyntaxError {
                    line_byte_start: current.line.byte_start,
                    line_number: current.line.number,
                    col: current.token.col,
                    len: current.token.kind.len(),
                    msg: "invalid assignment".into(),
                    help_msg: "stray assignment".into()
                })
            },
            TokenKind::Op( _ ) => {
                tokens.next();
                Err( SyntaxError {
                    line_byte_start: current.line.byte_start,
                    line_number: current.line.number,
                    col: current.token.col,
                    len: current.token.kind.len(),
                    msg: "invalid expression".into(),
                    help_msg: "stray binary operator".into()
                })
            },
            TokenKind::SemiColon => {
                tokens.next();
                Ok( Statement::Empty )
            },
            TokenKind::EOF => {
                tokens.next();
                Ok( Statement::Stop )
            },
            TokenKind::SOF => {
                let global_scope = Scope { parent: None, definitions: Vec::new(), nodes: Vec::new() };
                self.scopes.scopes.push( global_scope );

                tokens.next();
                Ok( Statement::Empty )
            },
            TokenKind::Comment( _ ) | TokenKind::Empty | TokenKind::Unexpected( _ ) => unreachable!(),
        }
    }

    fn parse_scope( &mut self, tokens: &mut TokenCursor<'lexer>, errors: &mut Vec<SyntaxError> ) {
        while let Some( current ) = tokens.current_or_next() {
            let statement_result = self.parse( tokens, errors, current );

            match statement_result {
                Ok( Statement::Single( node ) ) => self.scopes.scopes[ self.scopes.current ].nodes.push( node ),
                Ok( Statement::Multiple( multiple_nodes ) ) =>
                    for node in multiple_nodes {
                        self.scopes.scopes[ self.scopes.current ].nodes.push( node );
                    },
                Ok( Statement::Empty ) => continue,
                Ok( Statement::Stop ) => break,
                Err( err ) => {
                    errors.push( err );

                    // NOTE only parsing until the first error until a fault tolerant parser is developed
                    // this is because the first truly relevant error is the first one,
                    // which in turn causes a ripple effect that propagates to the rest of the parsing, causing extra errors
                    tokens.line = tokens.lines.len(); // as if we reached the end of the file
                    break;
                },
            }
        }
    }
}

// Parsing of Expressions
impl<'lexer> AST {
    // TODO disallow implicit conversions (str + i64, char + i64, str + char or str + str (maybe treat this as concatenation))
        // IDEA introduce casting operators
    fn factor( &mut self, tokens: &mut TokenCursor<'lexer> ) -> Result<Expression, SyntaxError> {
        let current = tokens.current_or_next().bounded( tokens, "expected expression" )?.unwrap();
        let factor = match &current.token.kind {
            TokenKind::Literal( literal ) => Ok( Expression::Literal( literal.clone() ) ),
            TokenKind::True => Ok( Expression::Literal( Literal::Bool { value: true } ) ),
            TokenKind::False => Ok( Expression::Literal( Literal::Bool { value: false } ) ),
            TokenKind::Identifier( name ) => match self.scopes.resolve( name ) {
                Some( definition ) => Ok( Expression::Identifier( name.clone(), definition.typ ) ),
                None => Err( SyntaxError {
                    line_byte_start: current.line.byte_start,
                    line_number: current.line.number,
                    col: current.token.col,
                    len: current.token.kind.len(),
                    msg: "variable not defined".into(),
                    help_msg: "was not previously defined in this scope".into()
                }),
            },
            TokenKind::Bracket( BracketKind::OpenRound ) => {
                let expression_start_pos = tokens.next().bounded( tokens, "expected expression" )?.unwrap();
                match expression_start_pos.token.kind {
                    TokenKind::Bracket( BracketKind::CloseRound ) => Err( SyntaxError {
                        line_byte_start: expression_start_pos.line.byte_start,
                        line_number: expression_start_pos.line.number,
                        col: expression_start_pos.token.col,
                        len: expression_start_pos.token.kind.len(),
                        msg: "invalid expression".into(),
                        help_msg: "empty expressions are not allowed".into()
                    }),
                    _ => {
                        let expression = self.expression_no_semicolon( tokens )?;
                        let close_bracket_pos = tokens.current_or_next().bounded( tokens, "expected closed parenthesis" )?.unwrap();
                        match close_bracket_pos.token.kind {
                            TokenKind::Bracket( BracketKind::CloseRound ) => Ok( expression ),
                            _ => Err( SyntaxError {
                                line_byte_start: current.line.byte_start,
                                line_number: current.line.number,
                                col: current.token.col,
                                len: current.token.kind.len(),
                                msg: "invalid expression".into(),
                                help_msg: "unclosed parenthesis".into()
                            }),
                        }
                    }
                }
            },
            TokenKind::Op( Operator::Minus ) => {
                let mut should_be_negated = true;
                while let Some( TokenPosition { token: Token { kind: TokenKind::Op( Operator::Minus ), .. }, .. } ) = tokens.next() {
                    should_be_negated = !should_be_negated;
                }

                let mut operand = self.factor( tokens )?;
                return match operand.typ() {
                    Type::Int | Type::Char | Type::Str => match should_be_negated {
                        true => match operand {
                            Expression::Literal( ref mut literal ) => match literal {
                                Literal::Int { ref mut value } => {
                                    *value = -*value;
                                    Ok( operand )
                                },
                                Literal::Char { value } => {
                                    *literal = Literal::Int { value: -(*value as isize) };
                                    Ok( operand )
                                },
                                Literal::Str( Str { text } ) => {
                                    *literal = Literal::Int { value: -(text.len() as isize) };
                                    Ok( operand )
                                },
                                Literal::Bool { .. } => unreachable!(),
                            },
                            _ => Ok( Expression::Unary { op: Operator::Negate, operand: Box::new( operand ) } )
                        }
                        false => Ok( operand ),
                    },
                    Type::Bool => Err( SyntaxError {
                        line_byte_start: current.line.byte_start,
                        line_number: current.line.number,
                        col: current.token.col,
                        len: current.token.kind.len(),
                        msg: "invalid expression".into(),
                        help_msg: "cannot negate boolean value, use the 'not' operator instead".into()
                    }),
                }
            },
            TokenKind::Op( Operator::Not ) => {
                let mut should_be_inverted = true;
                while let Some( TokenPosition { token: Token { kind: TokenKind::Op( Operator::Not ), .. }, .. } ) = tokens.next() {
                    should_be_inverted = !should_be_inverted;
                }

                let mut operand = self.factor( tokens )?;
                return match operand.typ() {
                    Type::Bool => match should_be_inverted {
                        true => match operand {
                            Expression::Literal( Literal::Bool { ref mut value } ) => {
                                *value = !*value;
                                Ok( operand )
                            },
                            _ => Ok( Expression::Unary { op: Operator::Not, operand: Box::new( operand ) } ),
                        },
                        false => Ok( operand ),
                    },
                    Type::Int | Type::Char | Type::Str => Err( SyntaxError {
                        line_byte_start: current.line.byte_start,
                        line_number: current.line.number,
                        col: current.token.col,
                        len: current.token.kind.len(),
                        msg: "invalid expression".into(),
                        help_msg: "cannot invert non boolean value, use the '-' operator instead".into()
                    }),
                };
            },
            _ => Err( SyntaxError {
                line_byte_start: current.line.byte_start,
                line_number: current.line.number,
                col: current.token.col,
                len: current.token.kind.len(),
                msg: "invalid expression".into(),
                help_msg: "expected expression operand".into()
            }),
        };

        tokens.next();
        return factor;
    }

    fn operator( &mut self, tokens: &mut TokenCursor<'lexer>, ops: &[Operator] ) -> Result<Option<Operator>, SyntaxError> {
        let current_pos = tokens.current_or_next().bounded( tokens, "expected operator or semicolon" )?.unwrap();
        let operator = match current_pos.token.kind {
            TokenKind::Op( op ) => match ops.contains( &op ) {
                true => {
                    tokens.next();
                    Some( op )
                },
                false => None,
            },
            _ => None,
        };

        return Ok( operator );
    }

    // IDEA optimize expression building by implementing associativity rules (ie. remove parenthesis around additions and subtractions)
    // FIX division by zero, raising to a negative power
    // TODO implement unsigned integers and force powers to only accept those as exponents
    // IDEA print crash error message
        // TODO implement a way to print file, line and column information in source code
    fn exponentiation( &mut self, tokens: &mut TokenCursor<'lexer> ) -> Result<Expression, SyntaxError> {
        let mut lhs = self.factor( tokens )?;

        while let Some( op ) = self.operator( tokens, &[Operator::Pow] )? {
            let rhs = self.factor( tokens )?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn multiplicative_expression( &mut self, tokens: &mut TokenCursor<'lexer> ) -> Result<Expression, SyntaxError> {
        let mut lhs = self.exponentiation( tokens )?;

        while let Some( op ) = self.operator( tokens, &[Operator::Times, Operator::Divide, Operator::Remainder] )? {
            let rhs = self.exponentiation( tokens )?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn additive_expression( &mut self, tokens: &mut TokenCursor<'lexer> ) -> Result<Expression, SyntaxError> {
        let mut lhs = self.multiplicative_expression( tokens )?;

        while let Some( op ) = self.operator( tokens, &[Operator::Plus, Operator::Minus] )? {
            let rhs = self.multiplicative_expression( tokens )?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn comparative_expression( &mut self, tokens: &mut TokenCursor<'lexer> ) -> Result<Expression, SyntaxError> {
        let mut lhs = self.additive_expression( tokens )?;

        let ops = [
            Operator::EqualsEquals, Operator::NotEquals,
            Operator::Greater, Operator::GreaterOrEquals,
            Operator::Less, Operator::LessOrEquals,
            Operator::Compare
        ];

        while let Some( op ) = self.operator( tokens, &ops )? {
            let rhs = self.additive_expression( tokens )?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    // FIX shortcircuit boolean operators
    // TODO implement boolean operators for strings
    fn boolean_expression( &mut self, tokens: &mut TokenCursor<'lexer> ) -> Result<Expression, SyntaxError> {
        let mut lhs = self.comparative_expression( tokens )?;

        while let Some( op ) = self.operator( tokens, &[Operator::And, Operator::Or, Operator::Xor] )? {
            let op_pos = tokens.peek_previous().unwrap();

            if lhs.typ() != Type::Bool {
                return Err( SyntaxError {
                    line_byte_start: op_pos.line.byte_start,
                    line_number: op_pos.line.number,
                    col: op_pos.token.col,
                    len: op_pos.token.kind.len(),
                    msg: "invalid boolean expression".into(),
                    help_msg: "must be preceded by a boolean expression".into()
                });
            }

            let rhs = self.comparative_expression( tokens )?;
            if rhs.typ() != Type::Bool {
                return Err( SyntaxError {
                    line_byte_start: op_pos.line.byte_start,
                    line_number: op_pos.line.number,
                    col: op_pos.token.col,
                    len: op_pos.token.kind.len(),
                    msg: "invalid boolean expression".into(),
                    help_msg: "must be followed by a boolean expression".into()
                });
            }

            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn expression_no_semicolon( &mut self, tokens: &mut TokenCursor<'lexer> ) -> Result<Expression, SyntaxError> {
        return self.boolean_expression( tokens );
    }

    fn expression( &mut self, tokens: &mut TokenCursor<'lexer> ) -> Result<Expression, SyntaxError> {
        let expression = self.boolean_expression( tokens )?;
        let current = tokens.current_or_next().bounded( tokens, "expected semicolon" )?.unwrap();
        return match current.token.kind {
            // semicolons are optional if found before a closing curly bracket
            // this allows the last statement before the end of a block not to have a semicolon
            TokenKind::Bracket( BracketKind::CloseCurly ) => Ok( expression ),
            // IDEA allow optional semicolons in chains of ifs-else ifs.else with single statements
            TokenKind::SemiColon => {
                tokens.next();
                Ok( expression )
            },
            _ => {
                let previous = tokens.peek_previous().bounded( tokens, "expected semicolon" )?.unwrap();
                tokens.next();

                Err( SyntaxError {
                    line_byte_start: previous.line.byte_start,
                    line_number: previous.line.number,
                    col: previous.token.col,
                    len: previous.token.kind.len(),
                    msg: "invalid expression".into(),
                    help_msg: "expected an operator after this token to complete the expression, or a ';' to end the statement".into()
                })
            },
        }
    }
}

// Parsing of variable definitions and assignments
impl<'lexer> AST {
    fn variable_definition( &mut self, tokens: &mut TokenCursor<'lexer> ) -> Result<Statement, SyntaxError> {
        let definition_pos = tokens.current();
        let kind = match definition_pos.token.kind {
            TokenKind::Definition( kind ) => kind,
            _ => unreachable!(),
        };

        let name_pos = tokens.next().bounded( tokens, "expected identifier" )?.unwrap();
        let name = match &name_pos.token.kind {
            TokenKind::Identifier( name ) => Ok( name.clone() ),
            _ => Err( SyntaxError {
                line_byte_start: name_pos.line.byte_start,
                line_number: name_pos.line.number,
                col: name_pos.token.col,
                len: name_pos.token.kind.len(),
                msg: "invalid assignment".into(),
                help_msg: "expected variable name".into()
            }),
        };

        let equals_pos = tokens.next().bounded( tokens, "expected equals" )?.unwrap();
        let equals = match equals_pos.token.kind {
            TokenKind::Equals => Ok( () ),
            _ => Err( SyntaxError {
                line_byte_start: name_pos.line.byte_start,
                line_number: name_pos.line.number,
                col: name_pos.token.col,
                len: name_pos.token.kind.len(),
                msg: "invalid assignment".into(),
                help_msg: "expected '=' after variable name".into()
            }),
        };

        let value_pos = tokens.next().bounded( tokens, "expected expression" )?.unwrap();
        let value = match value_pos.token.kind {
            TokenKind::Literal( _ ) | TokenKind::Identifier( _ )
            | TokenKind::True | TokenKind::False
            | TokenKind::Bracket( BracketKind::OpenRound )
            | TokenKind::Op( Operator::Minus | Operator::Not ) => self.expression( tokens ),
            _ => Err( SyntaxError {
                line_byte_start: equals_pos.line.byte_start,
                line_number: equals_pos.line.number,
                col: equals_pos.token.col,
                len: equals_pos.token.kind.len(),
                msg: "invalid assignment".into(),
                help_msg: "expected expression after '='".into()
            }),
        };

        let name = name?;
        let _ = equals?;
        let value = value?;

        return match self.scopes.resolve( &name ) {
            None => {
                let definition = Definition { mutability: kind, name: name.clone(), typ: value.typ() };
                self.scopes.scopes[ self.scopes.current ].definitions.push( definition );
                Ok( Statement::Single( Node::Definition( name, value ) ) )
            },
            Some( _ ) => Err( SyntaxError {
                line_byte_start: name_pos.line.byte_start,
                line_number: name_pos.line.number,
                col: name_pos.token.col,
                len: name_pos.token.kind.len(),
                msg: "variable redefinition".into(),
                help_msg: "was previously defined".into()
            }),
        }
    }

    fn variable_reassignment( &mut self, tokens: &mut TokenCursor<'lexer> ) -> Result<Statement, SyntaxError> {
        let name_pos = tokens.current();
        let name = name_pos.token.kind.to_string();
        let assignment_pos = tokens.next().unwrap();


        let value_pos = tokens.next().bounded( tokens, "expected expression" )?.unwrap();
        let new_value = match value_pos.token.kind {
            TokenKind::Literal( _ ) | TokenKind::Identifier( _ )
            | TokenKind::True | TokenKind::False
            | TokenKind::Bracket( BracketKind::OpenRound )
            | TokenKind::Op( Operator::Minus | Operator::Not ) => match self.expression( tokens ) {
                Ok( rhs ) => match &assignment_pos.token.kind {
                    TokenKind::Equals => Ok( rhs ),
                    operator => {
                        let op = match operator {
                            TokenKind::Op( op ) => *op,
                            _ => unreachable!(),
                        };

                        let lhs = Expression::Identifier( name, op.typ() );
                        Ok( Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) } )
                    },
                },
                Err( err ) => Err( err ),
            },
            _ => Err( SyntaxError {
                line_byte_start: assignment_pos.line.byte_start,
                line_number: assignment_pos.line.number,
                col: assignment_pos.token.col,
                len: assignment_pos.token.kind.len(),
                msg: "invalid assignment".into(),
                help_msg: format!( "expected expression after '{}'", assignment_pos.token.kind ).into()
            })
        };

        let variable = match &name_pos.token.kind {
            TokenKind::Identifier( name ) => match self.scopes.resolve_mut( &name ) {
                Some( definition ) => Ok( definition ),
                None => Err( SyntaxError {
                    line_byte_start: name_pos.line.byte_start,
                    line_number: name_pos.line.number,
                    col: name_pos.token.col,
                    len: name_pos.token.kind.len(),
                    msg: "variable redefinition".into(),
                    help_msg: "was not previously defined in this scope".into()
                }),
            },
            _ => unreachable!(),
        };

        let variable = variable?;

        let value = match variable.mutability {
            Mutability::Let => Err( SyntaxError {
                line_byte_start: name_pos.line.byte_start,
                line_number: name_pos.line.number,
                col: name_pos.token.col,
                len: name_pos.token.kind.len(),
                msg: "invalid assignment".into(),
                help_msg: "was defined as immutable".into()
            }),
            Mutability::Var => {
                let new_value = new_value?;
                let new_value_typ = new_value.typ();

                if variable.typ != new_value_typ {
                    Err( SyntaxError {
                        line_byte_start: name_pos.line.byte_start,
                        line_number: name_pos.line.number,
                        col: name_pos.token.col,
                        len: name_pos.token.kind.len(),
                        msg: "mismatched types".into(),
                        help_msg: format!(
                            "trying to assign an expression of type '{}' to a variable of type '{}'",
                            new_value_typ,
                            variable.typ
                         ).into()
                    })
                }
                else {
                    Ok( new_value )
                }
            },
        };

        return Ok( Statement::Single( Node::Assignment( name_pos.token.kind.to_string(), value? ) ) );
    }
}

// Parsing of print statements
impl<'lexer> AST {
    fn print( &mut self, tokens: &mut TokenCursor<'lexer> ) -> Result<Statement, SyntaxError> {
        let print_pos = tokens.current();
        match print_pos.token.kind {
            TokenKind::PrintLn => if let Some( TokenPosition { token: &Token { kind: TokenKind::SemiColon, .. }, .. } ) = tokens.peek_next() {
                tokens.next();
                let new_line = Literal::Char { value: b'\n' };
                let argument = Expression::Literal( new_line );
                return Ok( Statement::Single( Node::Print( argument ) ) );
            },
            TokenKind::Print => (),
            _ => unreachable!(),
        }

        let argument_pos = tokens.next().bounded( tokens, "expected print argument or semicolon" )?.unwrap();
        let argument = match &argument_pos.token.kind {
            TokenKind::Literal( _ ) | TokenKind::Identifier( _ )
            | TokenKind::True | TokenKind::False
            | TokenKind::Bracket( BracketKind::OpenRound )
            | TokenKind::Op( Operator::Minus | Operator::Not ) => self.expression( tokens ),
            _ => Err( SyntaxError {
                line_byte_start: argument_pos.line.byte_start,
                line_number: argument_pos.line.number,
                col: argument_pos.token.col,
                len: argument_pos.token.kind.len(),
                msg: "invalid print argument".into(),
                help_msg: "expected an expression".into()
            }),
        };

        let argument = argument?;

        return match print_pos.token.kind {
            TokenKind::Print => Ok( Statement::Single( Node::Print( argument ) ) ),
            TokenKind::PrintLn => {
                let new_line = Literal::Char { value: b'\n' };
                let println_argument = Expression::Literal( new_line );
                Ok( Statement::Multiple( vec![
                    Node::Print( argument ),
                    Node::Print( println_argument )
                ] ) )
            },
            _ => unreachable!(),
        };
    }
}

// Parsing of if statements
impl<'lexer> AST {
    fn if_statement( &mut self, tokens: &mut TokenCursor<'lexer>, errors: &mut Vec<SyntaxError> ) -> Result<Statement, SyntaxError> {
        let iff = self.iff( tokens, errors )?;
        let mut if_statement = IfStatement { ifs: vec![ iff ], els: None };
        let else_pos = tokens.current_or_next();
        self.els( &mut if_statement, else_pos, tokens, errors )?;

        return Ok( Statement::Single( Node::If( if_statement ) ) );
    }

    fn iff( &mut self, tokens: &mut TokenCursor<'lexer>, errors: &mut Vec<SyntaxError> ) -> Result<If, SyntaxError> {
        let if_pos = tokens.current();
        tokens.next().bounded( tokens, "expected boolean expression" )?.unwrap();

        let expression = self.expression_no_semicolon( tokens )?;
        let condition = match &expression.typ() {
            Type::Bool => Ok( expression ),
            Type::Char | Type::Int | Type::Str => Err( SyntaxError {
                line_byte_start: if_pos.line.byte_start,
                line_number: if_pos.line.number,
                col: if_pos.token.col,
                len: if_pos.token.kind.len(),
                msg: "expected boolean expression".into(),
                help_msg: "must be followed by a boolean expression".into()
            }),
        };

        // TODO allow for single statement ifs using colons, (i.e. if x == 19: println 19;)
        let open_curly_pos = tokens.current_or_next().bounded( tokens, "expected curly bracket" )?.unwrap();
        let open_curly = match open_curly_pos.token.kind {
            TokenKind::Bracket( BracketKind::OpenCurly ) => {
                let scope = self.scopes.create();
                tokens.next();
                Ok( scope )
            },
            _ => {
                let before_curly_bracket_pos = tokens.peek_previous().unwrap();
                Err( SyntaxError {
                    line_byte_start: before_curly_bracket_pos.line.byte_start,
                    line_number: before_curly_bracket_pos.line.number,
                    col: before_curly_bracket_pos.token.col,
                    len: before_curly_bracket_pos.token.kind.len(),
                    msg: "expected block scope".into(),
                    help_msg: "must be followed by an opened curly bracket".into()
                })
            },
        };

        let condition = condition?;
        let scope = open_curly?;

        self.parse_scope( tokens, errors );
        return Ok( If { condition, scope } );
    }

    fn els( &mut self, if_statement: &mut IfStatement, else_pos: Option<TokenPosition<'lexer>>, tokens: &mut TokenCursor<'lexer>, errors: &mut Vec<SyntaxError> ) -> Result<(), SyntaxError> {
        match else_pos {
            None => (),
            Some( pos ) => if let TokenKind::Else = pos.token.kind {
                let if_or_block_pos = tokens.next().bounded( tokens, "expected block or if statement after this token" )?.unwrap();
                match if_or_block_pos.token.kind {
                    TokenKind::Bracket( BracketKind::OpenCurly ) => {
                        let els = self.scopes.create();
                        tokens.next();

                        self.parse_scope( tokens, errors );
                        if_statement.els = Some( els );
                    },
                    TokenKind::If => {
                        let else_if = self.iff( tokens, errors )?;
                        if_statement.ifs.push( else_if );

                        let next_else_pos = tokens.current_or_next();
                        self.els( if_statement, next_else_pos, tokens, errors )?;
                    },
                    _ => return Err( SyntaxError {
                        line_byte_start: pos.line.byte_start,
                        line_number: pos.line.number,
                        col: pos.token.col,
                        len: pos.token.kind.len(),
                        msg: "invalid else statement".into(),
                        help_msg: "expected block of if statement after this token".into()
                    }),
                }
            }
        }

        return Ok( () );
    }
}

// Parsing of for statements
impl<'lexer> AST {
    fn for_statement( &mut self, tokens: &mut TokenCursor<'lexer>, errors: &mut Vec<SyntaxError> ) -> Result<Statement, SyntaxError> {
        self.for_depth += 1;
        // let for_pos = self.tokens.current().unwrap();

        // let mut pre_statement: Option<Box<Node>> = None;
        // let mut condition: Option<Expression> = None;
        // let mut post_statement: Option<Box<Node>> = None;

        // let mut next_pos = self.tokens.next().bounded( &mut self.tokens, "expected for loop statments" )?.unwrap();
        // let scope = match next_pos.token.kind {
        //     TokenKind::Bracket( BracketKind::OpenCurly ) => {
        //         let scope = self.scopes.create();
        //         self.tokens.next();
        //         Ok( scope )
        //     },
        //     _ =>
        //     TokenKind::SemiColon => {
        //         next_pos = self.tokens.next().bounded( &mut self.tokens, "expected for loop condition or semicolon" )?.unwrap();
        //         match next_pos.token.kind {
        //             TokenKind::SemiColon => {
        //                 next_pos = self.tokens.next().bounded( &mut self.tokens, "expected for loop post statement or block" )?.unwrap();
        //                 match next_pos.token.kind {
        //                     TokenKind::Bracket( BracketKind::OpenCurly ) => {
        //                         let scope = self.scopes.create();
        //                         self.tokens.next();
        //                         Ok( scope )
        //                     },
        //                     other => {

        //                     }
        //                 }
        //             },
        //         }
        //     }
        // };

        let iff = self.iff( tokens, errors );
        self.for_depth -= 1;

        let iff = iff?;
        return Ok( Statement::Single( Node::For( ForStatement {
            pre_statement: None,
            condition: Some( iff.condition ),
            post_statement: None,
            scope: iff.scope
        } ) ) );
    }
}


struct Checker;

impl Checker {
    const CHECKING: &'static str = colored!{
        text: "Checking",
        foreground: Foreground::LightGreen,
        bold: true,
    };


    fn check( file_path: &str ) {
        println!( "{}: {}", Self::CHECKING, file_path );
    }
}


#[derive( Debug )]
enum ControlFlowStatement {
    None,
    Break,
    Continue,
}

#[derive( Debug )]
struct InterpreterVariable {
    name: String,
    value: Expression
}

#[derive( Debug )]
struct Interpreter<'ast> {
    ast: &'ast AST,
    variables: Vec<InterpreterVariable>,
}

// Resolution of identifiers and evaluation of nodes
impl<'ast> Interpreter<'ast> {
    fn resolve( &'ast self, name: &str ) -> &'ast InterpreterVariable {
        for variable in &self.variables {
            if variable.name == name {
                return variable;
            }
        }

        unreachable!();
    }


    fn evaluate( &self, expression: &Expression ) -> Literal {
        return match expression {
            Expression::Literal( literal ) => literal.clone(),
            Expression::Binary { lhs, op, rhs } => {
                let lhs: isize = self.evaluate( lhs ).into();
                let rhs: isize = self.evaluate( rhs ).into();

                match op {
                    Operator::Pow | Operator::PowEquals => match rhs {
                        2 => Literal::Int { value: lhs * lhs },
                        _ => Literal::Int { value: lhs.pow( rhs as u32 ) },
                    },
                    Operator::Times | Operator::TimesEquals => Literal::Int { value: lhs * rhs },
                    Operator::Divide | Operator::DivideEquals => Literal::Int { value: lhs / rhs },
                    Operator::Remainder | Operator::RemainderEquals => Literal::Int { value: lhs % rhs },

                    Operator::Plus | Operator::PlusEquals => Literal::Int { value: lhs + rhs },
                    Operator::Minus | Operator::MinusEquals => Literal::Int { value: lhs - rhs },

                    Operator::EqualsEquals => Literal::Bool { value: lhs == rhs },
                    Operator::NotEquals => Literal::Bool { value: lhs != rhs },
                    Operator::Greater => Literal::Bool { value: lhs > rhs },
                    Operator::GreaterOrEquals => Literal::Bool { value: lhs >= rhs },
                    Operator::Less => Literal::Bool { value: lhs < rhs },
                    Operator::LessOrEquals => Literal::Bool { value: lhs <= rhs },
                    Operator::Compare => Literal::Int { value: lhs.cmp( &rhs ) as isize },

                    Operator::And => Literal::Bool { value: lhs == 1 && rhs == 1 },
                    Operator::Or => Literal::Bool { value: lhs == 1 || rhs == 1 },
                    Operator::Xor => Literal::Bool { value: (lhs != 1 && rhs == 1) || (lhs == 1 && rhs != 1) },

                    Operator::Not | Operator::Negate => unreachable!(),
                }
            },
            Expression::Identifier( name, _ ) => self.evaluate( &self.resolve( name ).value ),
            Expression::Unary { op, operand } => match op {
                Operator::Negate => {
                    let value: isize = self.evaluate( operand ).into();
                    Literal::Int { value: -value }
                },
                Operator::Not => {
                    let value: isize = self.evaluate( operand ).into();
                    Literal::Bool { value: value == 0 }
                },
                _ => unreachable!(),
            },
        }
    }

    fn evaluate_expression( &self, expression: &Expression ) -> Expression {
        return Expression::Literal( self.evaluate( expression ) );
    }
}

// Interpretation of nodes
impl Interpreter<'_> {
    const INTERPRETING: &'static str = colored!{
        text: "Interpreting",
        foreground: Foreground::LightGreen,
        bold: true,
    };


    fn interpret_scope( &mut self, scope: ScopeIdx ) {
        let current_scope = &self.ast.scopes.scopes[ scope ];
        let mut defined_variables = 0;

        for node in  &current_scope.nodes {
            match node {
                Node::Print( argument ) => self.evaluate( argument ).display(),
                Node::If( if_statement ) => 'iff: {
                    for iff in &if_statement.ifs {
                        if let Literal::Bool { value: true } = self.evaluate( &iff.condition ) {
                            self.interpret_scope( iff.scope );
                            break 'iff;
                        }
                    }

                    if let Some( els ) = &if_statement.els {
                        self.interpret_scope( *els );
                    }
                },
                Node::For( forr ) =>
                    if let Some( condition ) = &forr.condition {
                        while let Literal::Bool { value: true } = self.evaluate( condition ) {
                            if let ControlFlowStatement::Break = self.interpret_loop( forr.scope ) {
                                break;
                            }
                        }
                    }
                    else {
                        loop {
                            if let ControlFlowStatement::Break = self.interpret_loop( forr.scope ) {
                                break;
                            }
                        }
                    },
                Node::Definition( name, value ) => {
                    defined_variables += 1;
                    self.variables.push( InterpreterVariable {
                        name: name.clone(),
                        value: self.evaluate_expression( value )
                    } );
                }
                Node::Assignment( name, new_value ) => {
                    let value = self.evaluate_expression( new_value );
                    for variable in &mut self.variables {
                        if variable.name == *name {
                            variable.value = value;
                            break;
                        }
                    }
                },
                Node::Scope( inner ) => self.interpret_scope( *inner ),
                Node::Expression( expression ) => { self.evaluate( expression ); },
                Node::Break | Node::Continue => unreachable!(),
            }
        }

        for _ in 0..defined_variables {
            self.variables.pop();
        }
    }

    fn interpret_loop( &mut self, scope: ScopeIdx ) -> ControlFlowStatement {
        let current_scope = &self.ast.scopes.scopes[ scope ];
        let mut defined_variables = 0;

        let control_flow = 'control_flow: {
            for node in  &current_scope.nodes {
                let cf = match node {
                    Node::Print( argument ) => {
                        self.evaluate( argument ).display();
                        ControlFlowStatement::None
                    },
                    Node::If( if_statement ) => 'iff: {
                        for iff in &if_statement.ifs {
                            if let Literal::Bool { value: true } = self.evaluate( &iff.condition ) {
                                break 'iff self.interpret_loop( iff.scope );
                            }
                        }

                        if let Some( els ) = &if_statement.els {
                            break 'iff self.interpret_loop( *els );
                        }

                        ControlFlowStatement::None
                    },
                    Node::For( forr ) => {
                        if let Some( condition ) = &forr.condition {
                            while let Literal::Bool { value: true } = self.evaluate( condition ) {
                                if let ControlFlowStatement::Break = self.interpret_loop( forr.scope ) {
                                    break;
                                }
                            }
                        }
                        else {
                            loop {
                                if let ControlFlowStatement::Break = self.interpret_loop( forr.scope ) {
                                    break;
                                }
                            }
                        }

                        ControlFlowStatement::None
                    },
                    Node::Definition( name, value ) => {
                        defined_variables += 1;
                        self.variables.push( InterpreterVariable {
                            name: name.clone(),
                            value: self.evaluate_expression( value )
                        } );

                        ControlFlowStatement::None
                    },
                    Node::Assignment( name, new_value ) => {
                        let value = self.evaluate_expression( new_value );
                        for variable in &mut self.variables {
                            if variable.name == *name {
                                variable.value = value;
                                break;
                            }
                        }

                        ControlFlowStatement::None
                    },
                    Node::Scope( inner ) => self.interpret_loop( *inner ),
                    Node::Expression( expression ) => {
                        self.evaluate( expression );
                        ControlFlowStatement::None
                    },
                    Node::Break => break 'control_flow ControlFlowStatement::Break,
                    Node::Continue => break 'control_flow ControlFlowStatement::Continue,
                };

                if let ControlFlowStatement::None = cf {} else { break 'control_flow cf; }
            }

            ControlFlowStatement::None
        };

        for _ in 0..defined_variables {
            self.variables.pop();
        }

        return control_flow;
    }

    fn interpret( &mut self, file_path: &str ) {
        println!( "{}: {}", Self::INTERPRETING, file_path );

        self.interpret_scope( 0 );
    }
}


#[allow( dead_code )]
#[derive( Debug, PartialEq, Clone, Copy )]
enum Register {
    RAX,
    RBX,
    RCX,
    RDX,
    RSI,
    RDI,
    RBP,
    RSP,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15
}

impl Display for Register {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::RAX => write!( f, "rax" ),
            Self::RBX => write!( f, "rbx" ),
            Self::RCX => write!( f, "rcx" ),
            Self::RDX => write!( f, "rdx" ),
            Self::RSI => write!( f, "rsi" ),
            Self::RDI => write!( f, "rdi" ),
            Self::RBP => write!( f, "rbp" ),
            Self::RSP => write!( f, "rsp" ),
            Self::R8 => write!( f, "r8" ),
            Self::R9 => write!( f, "r9" ),
            Self::R10 => write!( f, "r10" ),
            Self::R11 => write!( f, "r11" ),
            Self::R12 => write!( f, "r12" ),
            Self::R13 => write!( f, "r13" ),
            Self::R14 => write!( f, "r14" ),
            Self::R15 => write!( f, "r15" ),
        }
    }
}

#[allow( dead_code )]
enum MemorySize {
    Byte,
    Word,
    DWord,
    QWord,
}

impl Display for MemorySize {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Byte => write!( f, "byte" ),
            Self::Word => write!( f, "word" ),
            Self::DWord => write!( f, "dword" ),
            Self::QWord => write!( f, "qword" ),
        }
    }
}

#[derive( Debug )]
struct Memory {
    reg: Register,
    offset: usize,
}

impl Display for Memory {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return write!( f, "[{} + {}]", self.reg, self.offset );
    }
}

#[allow( dead_code )]
enum MovDst {
    Register( Register ),
    Memory( MemorySize, Memory ),
}

impl Display for MovDst {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Register( reg ) => write!( f, "{}", reg ),
            Self::Memory( size, mem ) => write!( f, "{} {}", size, mem ),
        }
    }
}

#[allow( dead_code )]
enum MovSrc {
    Immediate( Literal ),
    Register( Register ),
    Memory( MemorySize, Memory ),
}

impl Display for MovSrc {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Immediate( literal ) => write!( f, "{}", literal ),
            Self::Register( reg ) => write!( f, "{}", reg ),
            Self::Memory( size, mem ) => write!( f, "{} {}", size, mem ),
        }
    }
}

#[allow( dead_code )]
struct Mov {
    // instruction: Instruction, // TODO introduce instruction
    dst: MovDst,
    src: MovSrc,
}

#[allow( dead_code )]
enum MovKind {
    Single( Mov ),
    Multiple( Vec<Mov> ),
}


#[derive( Debug )]
struct CompilerVariable {
    name: String,
    typ: Type,
    offset: usize,
}

#[derive( Debug )]
struct StringLabel<'ast> {
    string: &'ast Str,
    label: String,
    len_label: String,
}

#[derive( Debug )]
struct Compiler<'ast> {
    ast: &'ast AST,

    rodata: String,
    asm: String,

    variables: Vec<CompilerVariable>,
    strings: Vec<StringLabel<'ast>>,

    if_idx: usize,
    for_idx: usize,
    for_idx_stack: Vec<usize>,
}

impl Compiler<'_> {
    const BUILDING: &'static str = colored!{
        text: "Building",
        foreground: Foreground::LightGreen,
        bold: true,
    };

    const RUNNING: &'static str = colored!{
        text: "Running",
        foreground: Foreground::LightGreen,
        bold: true,
    };

    const STACK_ALIGN: usize = core::mem::size_of::<usize>();


    fn compile( &mut self, file_path: &str ) -> Result<PathBuf, ()> {
        let src_file_path = Path::new( file_path );
        println!( "{}: {}", Compiler::BUILDING, src_file_path.display() );

        let asm_file_path = src_file_path.with_extension( "asm" );
        let mut asm_file = BufWriter::new( File::create( &asm_file_path ).unwrap() );

        self.rodata +=
r#" stdout: equ 1
 SYS_write: equ 1
 SYS_exit: equ 60
 EXIT_SUCCESS: equ 0

 I64_MIN: equ 1 << 63
 I64_MAX: equ ~I64_MIN
 INT_MAX_DIGITS: equ 64

 true: equ 1
 true_str: db "true"
 true_str_len: equ $ - true_str

 false: equ 0
 false_str: db "false"
 false_str_len: equ $ - false_str

 LESS: equ -1
 EQUAL: equ 0
 GREATER: equ 1"#;

        let mut stack_size = 0;
        for scope in &self.ast.scopes.scopes {
            if scope.definitions.is_empty() {
                continue;
            }

            let mut variables: Vec<(Type, Vec<&Definition>)> = Vec::new();
            for definition in &scope.definitions {
                let typ = definition.typ;

                let mut type_already_encountered = false;
                for variable in &mut variables {
                    if typ == variable.0 {
                        variable.1.push( definition );
                        type_already_encountered = true;
                        break;
                    }
                }

                if !type_already_encountered {
                    variables.push( (typ, vec![ definition ]) );
                }
            }

            while !variables.is_empty() {
                let mut largest_type_bytes = 0;
                let mut current_type = 0;
                for (variable_idx, variable) in variables.iter().enumerate() {
                    let bytes = variable.0.len();
                    if bytes > largest_type_bytes {
                        largest_type_bytes = bytes;
                        current_type = variable_idx;
                    }
                }

                for variable in variables.swap_remove( current_type ).1 {
                    let name = variable.name.clone();
                    let typ = variable.typ;

                    self.variables.push( CompilerVariable { name, typ, offset: stack_size } );
                    stack_size += typ.len();
                }
            }
        }

        if stack_size > 0 {
            let misalignment = stack_size % Compiler::STACK_ALIGN;
            let needs_padding = misalignment != 0;
            let padding = needs_padding as usize * (Compiler::STACK_ALIGN - misalignment);
            stack_size += padding;

            self.asm += &format!(
                " push rbp\
                \n sub rsp, {}\
                \n mov rbp, rsp\
                \n\n",
                stack_size
            );
        }

        self.compile_scope( 0 );

        if stack_size > 0 {
            self.asm += &format!(
                " add rsp, {}\
                \n pop rbp\n",
                stack_size
            );
        }

        let program = format!(
r"global _start

section .rodata
{}

section .data
 int_str: times INT_MAX_DIGITS + 1 db 0
 int_str_bufsize: equ $ - int_str
 int_str_len: equ int_str_bufsize - 1

section .text
int_toStr:
 push rcx

 mov rsi, 10
 mov rcx, (int_str + int_str_len) - 1

 mov rax, rdi
 cmp rax, 0
 je .writeZero
 jl .makeNumberPositive
 jg .extractNextDigit

.writeZero:
 mov byte [rcx], '0'
 jmp .done

.makeNumberPositive:
 neg rax

.extractNextDigit:
 xor rdx, rdx
 idiv rsi

 add dl, '0'
 mov byte [rcx], dl
 dec rcx

 cmp rax, 0
 jne .extractNextDigit

 cmp rdi, 0
 jl .addMinusSign
 inc rcx
 jmp .done

.addMinusSign:
 mov byte [rcx], '-'

.done:
 mov rdx, int_str + int_str_len
 sub rdx, rcx

 mov rax, rcx
 pop rcx
 ret

int_pow:
 cmp rsi, 1
 jne .exponent_is_not_one
 mov rax, rdi
 ret

.exponent_is_not_one:
 cmp rsi, 0
 jne .exponent_is_not_zero
 mov rax, 1
 ret

.exponent_is_not_zero:
 push rsi

 mov rax, rdi
 mov rdx, 1

.next_power:
 cmp rsi, 1
 jle .done

 test rsi, 1
 jnz .exponent_is_odd

 imul rax, rax
 shr rsi, 1
 jmp .next_power

.exponent_is_odd:
 imul rdx, rax
 imul rax, rax

 dec rsi
 shr rsi, 1
 jmp .next_power

.done:
 imul rax, rdx

 pop rsi
 ret

_start:
{}

 mov rdi, EXIT_SUCCESS
 mov rax, SYS_exit
 syscall",
 self.rodata, self.asm
);

        asm_file.write_all( program.as_bytes() ).unwrap();
        asm_file.flush().unwrap();

        let nasm = Command::new( "nasm" )
            .args( ["-felf64", "-gdwarf", asm_file_path.to_str().unwrap()] )
            .output()
            .expect( "failed to run nasm assembler" );

        print!( "{}", String::from_utf8_lossy( &nasm.stdout ) );
        print!( "{}", String::from_utf8_lossy( &nasm.stderr ) );

        let obj_file_path = src_file_path.with_extension( "o" );
        let executable_file_path = src_file_path.with_extension( "" );
        let ld = Command::new( "ld" )
            .args( [obj_file_path.to_str().unwrap(), "-o", executable_file_path.to_str().unwrap()] )
            .output()
            .expect( "failed to link" );

        print!( "{}", String::from_utf8_lossy( &ld.stdout ) );
        print!( "{}", String::from_utf8_lossy( &ld.stderr ) );

        return Ok( executable_file_path );
    }

    fn run( &mut self, file_path: &str ) -> Result<(), ()> {
        let executable_file_path = self.compile( file_path )?;
        println!( "{}: {}", Compiler::RUNNING, executable_file_path.display() );

        let output = Command::new( format!( "{}", executable_file_path.display() ) ).output().unwrap();
        print!( "{}", String::from_utf8_lossy( &output.stdout ) );

        return Ok( () );
    }
}

// Resolution of identifiers and string tags
impl<'ast> Compiler<'ast> {
    fn resolve( &'ast self, name: &str ) -> &'ast CompilerVariable {
        for variable in &self.variables {
            if variable.name == name {
                return variable;
            }
        }

        unreachable!();
    }

    fn string_label_idx( &mut self, string: &'ast Str ) -> usize {
        let mut string_idx = 0;
        for string_label in &self.strings {
            if std::ptr::eq( string, string_label.string ) {
                return string_idx;
            }
            string_idx += 1;
        }

        let mut string_text = String::with_capacity( string.text.len() + 2 );
        string_text.push( '`' );
        for ch in &string.text {
            string_text.extend( (*ch as char).escape_default() );
        }
        string_text.push( '`' );

        let label = format!( "str_{}", string_idx );
        let len_label = format!( "str_{}_len", string_idx );

        self.rodata += &format!(
            "\n\n {}: db {}\
            \n {}: equ $ - {}",
            label, string_text,
            len_label, label
        );

        self.strings.push( StringLabel { string, label, len_label } );
        return string_idx;
    }
}

// Compilation of nodes
impl<'ast> Compiler<'ast> {
    fn compile_scope( &mut self, scope: usize ) {
        for node in &self.ast.scopes.scopes[ scope ].nodes {
            match node {
                Node::Print( argument ) => {
                    self.asm += &format!( " ; {}\n", node );
                    self.compile_expression( argument );

                    match argument.typ() {
                        Type::Int => self.asm +=
                            " mov rsi, 10\
                            \n call int_toStr\
                            \n\
                            \n mov rdi, stdout\
                            \n mov rsi, rax\
                            \n mov rax, SYS_write\
                            \n syscall\n\n",
                        Type::Char => self.asm +=
                            " push rdi\
                            \n mov rdi, stdout\
                            \n mov rsi, rsp\
                            \n mov rdx, 1\
                            \n mov rax, SYS_write\
                            \n syscall\
                            \n pop rsi\n\n",
                        Type::Bool => self.asm +=
                            " cmp rdi, true\
                            \n mov rsi, true_str\
                            \n mov rdi, false_str\
                            \n cmovne rsi, rdi\
                            \n mov rdx, true_str_len\
                            \n mov rdi, false_str_len\
                            \n cmovne rdx, rdi\
                            \n\
                            \n mov rdi, stdout\
                            \n mov rax, SYS_write\
                            \n syscall\n\n",
                        Type::Str => self.asm +=
                            " mov rdi, stdout\
                            \n mov rax, SYS_write\
                            \n syscall\n\n",
                    }
                },
                // TODO simplify: avoid checking for the presence of the else/else-ifs branches too many times
                Node::If( if_statement ) => {
                    let if_idx = self.if_idx;
                    self.if_idx += 1;

                    let mut ifs = if_statement.ifs.iter();
                    let iff = ifs.next().unwrap();

                    let (has_else_ifs, has_else) = (if_statement.ifs.len() > 1, if_statement.els.is_some());

                    // compiling the if branch
                    let if_tag = format!( "if_{}", if_idx );
                    let (if_false_tag, if_end_tag_idx) = if has_else_ifs {
                        (format!( "if_{}_else_if_0", if_idx ), Some( if_idx ))
                    }
                    else if has_else {
                        (format!( "if_{}_else", if_idx ), Some( if_idx ))
                    }
                    else {
                        (format!( "if_{}_end", if_idx ), None)
                    };

                    self.compile_if( iff, &if_tag, &if_false_tag );
                    if let Some( idx ) = if_end_tag_idx {
                        self.asm += &format!( " jmp if_{}_end\n\n", idx );
                    }

                    // compiling the else if branches
                    if has_else_ifs {
                        let last_else_if = ifs.next_back().unwrap();
                        let else_if_end_tag = format!( " jmp if_{}_end\n\n", if_idx );
                        let mut else_if_tag_idx = 0;

                        for else_if in ifs {
                            let else_if_tag = format!( "if_{}_else_if_{}", if_idx, else_if_tag_idx );
                            let else_if_false_tag = format!( "if_{}_else_if_{}", if_idx, else_if_tag_idx + 1 );

                            self.compile_if( else_if, &else_if_tag, &else_if_false_tag );
                            self.asm += &else_if_end_tag;
                            else_if_tag_idx += 1;
                        }

                        let else_if_tag = format!( "if_{}_else_if_{}", if_idx, else_if_tag_idx );
                        let else_if_false_tag = if has_else {
                            format!( "if_{}_else", if_idx )
                        }
                        else {
                            format!( "if_{}_end", if_idx )
                        };

                        self.compile_if( last_else_if, &else_if_tag, &else_if_false_tag );
                        self.asm += &else_if_end_tag;
                    }

                    // compiling the else branch
                    if let Some( els ) = if_statement.els {
                        self.asm += &format!( "if_{}_else:\n", if_idx );
                        self.compile_scope( els );
                    }

                    self.asm += &format!( "if_{}_end:\n", if_idx );
                },
                Node::For( forr ) => {
                    let for_tag = format!( "for_{}", self.for_idx );
                    let for_end_tag = format!( "for_{}_end", self.for_idx );

                    self.for_idx_stack.push( self.for_idx );
                    self.for_idx += 1;
                    self.compile_for( forr, &for_tag, &for_end_tag );
                    self.for_idx_stack.pop();

                    self.asm += &format!(
                        " jmp {}\
                        \n{}:\n\n",
                        for_tag,
                        for_end_tag
                    );
                },
                Node::Definition( name, value ) => self.compile_assignment( name, value ),
                Node::Assignment( name, value ) => self.compile_assignment( name, value ),
                Node::Scope( inner ) => self.compile_scope( *inner ),
                Node::Expression( expression ) => self.compile_expression( expression ),
                Node::Break => self.asm += &format!( " jmp for_{}_end\n\n", self.for_idx_stack.last().unwrap() ),
                Node::Continue => self.asm += &format!( " jmp for_{}\n\n", self.for_idx_stack.last().unwrap() ),
            }
        }
    }


    fn compile_expression( &mut self, expression: &'ast Expression ) {
        match expression {
            Expression::Literal( Literal::Int { value } ) => self.asm += &format!( " mov rdi, {}\n", value ),
            Expression::Literal( Literal::Char { value } ) => self.asm += &format!( " mov rdi, {}\n", value ),
            Expression::Literal( Literal::Bool { value } ) => self.asm += &format!( " mov rdi, {}\n", value ),
            Expression::Literal( Literal::Str( string ) ) => {
                let string_label_idx = self.string_label_idx( string );
                let string_label = &self.strings[ string_label_idx ];

                self.asm += &format!(
                    " mov rsi, {}\
                    \n mov rdx, {}\n",
                    string_label.label,
                    string_label.len_label
                );
            },
            Expression::Binary { .. } => self.compile_expression_factor( expression, Register::RDI ),
            Expression::Identifier( src_name, _ ) => {
                let src_variable = self.resolve( src_name );
                let src_variable_typ = src_variable.typ;
                let src_variable_offset = src_variable.offset;

                match src_variable_typ {
                    Type::Int => self.asm += &format!( " mov rdi, [rbp + {}]\n", src_variable_offset ),
                    Type::Char | Type::Bool => self.asm += &format!( " movzx rdi, byte [rbp + {}]\n", src_variable_offset ),
                    Type::Str =>
                        self.asm += &format!(
                            " mov rsi, [rbp + {}]\
                            \n mov rdx, [rbp + {}]\n",
                            src_variable_offset,
                            src_variable_offset + 8
                        ),
                }
            },
            Expression::Unary { op, operand } => {
                self.compile_expression( operand );
                match op {
                    Operator::Negate => self.asm += " neg rdi\n",
                    Operator::Not => self.asm += " xor dil, 1\n",
                    _ => unreachable!()
                }
            },
        }
    }

    // IDEA make this return the place of where to find the result of the operation
    fn compile_expression_factor( &mut self, factor: &'ast Expression, dst: Register ) {
        match factor {
            Expression::Literal( Literal::Int { value } ) => self.asm += &format!( " mov {}, {}\n", dst, value ),
            Expression::Literal( Literal::Char { value } ) => self.asm += &format!( " mov {}, {}\n", dst, value ),
            Expression::Literal( Literal::Bool { value } ) => self.asm += &format!( " mov {}, {}\n", dst, value ),
            Expression::Literal( Literal::Str( string ) ) => {
                let string_label_idx = self.string_label_idx( string );
                let string_label = &self.strings[ string_label_idx ];

                self.asm += &format!( " mov {}, {}\n", dst, string_label.len_label );
            },
            // TODO find way to avoiding compiling the move to a support register if the rhs operand is a literal
                // IDEA optimize increments
                // IDEA optimize checking for even values by testing the least significant bit (e.g. test rax, 1)
            // IDEA optimize check for divisibility by 2 (i.e. even or odd)
            Expression::Binary { lhs, op, rhs } => {
                let (lhs_reg, rhs_reg, op_asm) = match op {
                    Operator::Pow | Operator::PowEquals => match &**rhs {
                        Expression::Literal( Literal::Int { value: 2 } ) => (Register::RDI, Register::RSI,
                            " imul rdi, rdi\n"
                        ),
                        _ => (Register::RDI, Register::RSI,
                            " call int_pow\
                            \n mov rdi, rax\n"
                        ),
                    },
                    Operator::Times | Operator::TimesEquals => (Register::RDI, Register::RSI, " imul rdi, rsi\n"),
                    Operator::Divide | Operator::DivideEquals =>
                        (Register::RDI, Register::RSI,
                            " mov rax, rdi\
                            \n xor rdx, rdx\
                            \n idiv rsi\
                            \n mov rdi, rax\n"
                        ),
                    Operator::Remainder | Operator::RemainderEquals =>
                        (Register::RDI, Register::RSI,
                            " mov rax, rdi\
                            \n xor rdx, rdx\
                            \n idiv rsi\
                            \n mov rdi, rdx\n"
                        ),
                    Operator::Plus | Operator::PlusEquals => (Register::RDI, Register::RSI, " add rdi, rsi\n"),
                    Operator::Minus | Operator::MinusEquals => (Register::RDI, Register::RSI, " sub rdi, rsi\n"),
                    Operator::EqualsEquals =>
                        (Register::RDI, Register::RSI,
                            " cmp rdi, rsi\
                            \n mov rdi, false\
                            \n sete dil\n"
                        ),
                    Operator::NotEquals =>
                        (Register::RDI, Register::RSI,
                            " cmp rdi, rsi\
                            \n mov rdi, false\
                            \n setne dil\n"
                        ),
                    Operator::Greater =>
                        (Register::RDI, Register::RSI,
                            " cmp rdi, rsi\
                            \n mov rdi, false\
                            \n setg dil\n"
                        ),
                    Operator::GreaterOrEquals =>
                        (Register::RDI, Register::RSI,
                            " cmp rdi, rsi\
                            \n mov rdi, false\
                            \n setge dil\n"
                        ),
                    Operator::Less =>
                        (Register::RDI, Register::RSI,
                            " cmp rdi, rsi\
                            \n mov rdi, false\
                            \n setl dil\n"
                        ),
                    Operator::LessOrEquals =>
                        (Register::RDI, Register::RSI,
                            " cmp rdi, rsi\
                            \n mov rdi, false\
                            \n setle dil\n"
                        ),

                    Operator::Compare =>
                        (Register::RDI, Register::RSI,
                            " cmp rdi, rsi\
                            \n mov rdi, LESS\
                            \n mov rdx, EQUAL\
                            \n cmove rdi, rdx\
                            \n mov rdx, GREATER\
                            \n cmovg rdi, rdx\n"
                        ),

                    Operator::And => (Register::RDI, Register::RSI, " and rdi, rsi\n"),
                    Operator::Or => (Register::RDI, Register::RSI, " or rdi, rsi\n" ),
                    Operator::Xor => (Register::RDI, Register::RSI, " xor rdi, rsi\n" ),

                    Operator::Not | Operator::Negate => unreachable!(),
                };

                match &**rhs {
                    Expression::Binary { .. } | Expression::Unary { .. } => {
                        self.compile_expression_factor( lhs, lhs_reg );
                        self.asm += " push rdi\n\n";
                        self.compile_expression_factor( rhs, rhs_reg );

                        self.asm += &format!(
                            " mov {}, rdi\
                            \n pop {}\n",
                            rhs_reg,
                            lhs_reg
                        );
                    },
                    _ => {
                        self.compile_expression_factor( lhs, lhs_reg );
                        self.compile_expression_factor( rhs, rhs_reg );
                    }
                }

                self.asm += &format!( "{}\n", op_asm );
            },
            Expression::Identifier( src_name, _ ) => {
                let src_variable = self.resolve( src_name );
                let src_variable_typ = src_variable.typ;
                let src_variable_offset = src_variable.offset;

                match src_variable_typ {
                    Type::Int => self.asm += &format!( " mov {}, [rbp + {}]\n", dst, src_variable_offset ),
                    Type::Char | Type::Bool => self.asm += &format!( " movzx {}, byte [rbp + {}]\n", dst, src_variable_offset ),
                    Type::Str => self.asm += &format!( " mov {}, [rbp + {}]\n", dst, src_variable_offset + 8 ),
                }
            },
            Expression::Unary { op, operand } => {
                self.compile_expression( operand );
                match op {
                    Operator::Negate => self.asm += " neg rdi\n",
                    Operator::Not => self.asm += " xor dil, 1\n",
                    _ => unreachable!()
                }
            },
        }
    }


    fn compile_if( &mut self, iff: &'ast If, if_tag: &String, if_false_tag: &String ) {
        self.asm += &format!( "{}:; {}\n", if_tag, iff.condition );

        match &iff.condition {
            Expression::Literal( Literal::Bool { value } ) =>
                self.asm += &format!(
                    " mov dil, {}\
                    \n cmp dil, true\
                    \n jne {}\n\n",
                    *value as usize,
                    if_false_tag
                ),
            Expression::Binary { lhs, op, rhs } => {
                match &**rhs {
                    Expression::Binary { .. } | Expression::Unary { .. }=> {
                        self.compile_expression_factor( lhs, Register::RDI );
                        self.asm += " push rdi\n\n";
                        self.compile_expression_factor( rhs, Register::RSI );

                        self.asm += " mov rsi, rdi\n pop rdi\n";
                    },
                    _ => {
                        self.compile_expression_factor( lhs, Register::RDI );
                        self.compile_expression_factor( rhs, Register::RSI );
                    }
                }

                match op {
                    Operator::EqualsEquals => self.asm += &format!( " cmp rdi, rsi\n jne {}\n\n", if_false_tag ),
                    Operator::NotEquals => self.asm += &format!( " cmp rdi, rsi\n je {}\n\n", if_false_tag ),
                    Operator::Greater => self.asm += &format!( " cmp rdi, rsi\n jle {}\n\n", if_false_tag ),
                    Operator::GreaterOrEquals => self.asm += &format!( " cmp rdi, rsi\n jl {}\n\n", if_false_tag ),
                    Operator::Less => self.asm += &format!( " cmp rdi, rsi\n jge {}\n\n", if_false_tag ),
                    Operator::LessOrEquals => self.asm += &format!( " cmp rdi, rsi\n jg {}\n\n", if_false_tag ),
                    Operator::And => self.asm += &format!( " and rdi, rsi\n jz {}\n\n", if_false_tag ),
                    Operator::Or => self.asm += &format!( " or rdi, rsi\n jz {}\n\n", if_false_tag ),
                    Operator::Xor => self.asm += &format!( " xor rdi, rsi\n jz {}\n\n", if_false_tag ),

                    Operator::Pow | Operator::PowEquals
                    | Operator::Times | Operator::TimesEquals
                    | Operator::Divide | Operator::DivideEquals
                    | Operator::Remainder | Operator::RemainderEquals
                    | Operator::Plus | Operator::PlusEquals
                    | Operator::Minus | Operator::MinusEquals
                    | Operator::Compare
                    | Operator::Not | Operator::Negate => unreachable!(),
                };

            },
            Expression::Identifier( src_name, _ ) => {
                let src_variable = self.resolve( src_name );
                self.asm += &format!(
                    " mov dil, [rbp + {}]\
                    \n cmp dil, true\
                    \n jne {}\n\n",
                    src_variable.offset,
                    if_false_tag
                )
            },
            Expression::Unary { operand, .. } => {
                self.compile_expression( operand );

                // we can only have boolean expressions at this point, so it's safe to ignore the integer negation case
                self.asm += &format!(
                    " xor dil, 1\
                    \n jz {}\n\n",
                    if_false_tag
                );
            },
            Expression::Literal( _ ) => unreachable!(),
        }

        self.compile_scope( iff.scope );
    }

    fn compile_for( &mut self, forr: &'ast ForStatement, for_tag: &String, for_false_tag: &String ) {
        if let Some( condition ) = &forr.condition {
            self.asm += &format!( "{}:; {}\n", for_tag, condition );

            match condition {
                Expression::Literal( Literal::Bool { value } ) =>
                    self.asm += &format!(
                        " mov dil, {}\
                        \n cmp dil, true\
                        \n jne {}\n\n",
                        *value as usize,
                        for_false_tag
                    ),
                Expression::Binary { lhs, op, rhs } => {
                    match &**rhs {
                        Expression::Binary { .. } | Expression::Unary { .. } => {
                            self.compile_expression_factor( lhs, Register::RDI );
                            self.asm += " push rdi\n\n";
                            self.compile_expression_factor( rhs, Register::RSI );

                            self.asm += " mov rsi, rdi\n pop rdi\n";
                        },
                        _ => {
                            self.compile_expression_factor( lhs, Register::RDI );
                            self.compile_expression_factor( rhs, Register::RSI );
                        }
                    }

                    match op {
                        Operator::EqualsEquals => self.asm += &format!( " cmp rdi, rsi\n jne {}\n\n", for_false_tag ),
                        Operator::NotEquals => self.asm += &format!( " cmp rdi, rsi\n je {}\n\n", for_false_tag ),
                        Operator::Greater => self.asm += &format!( " cmp rdi, rsi\n jle {}\n\n", for_false_tag ),
                        Operator::GreaterOrEquals => self.asm += &format!( " cmp rdi, rsi\n jl {}\n\n", for_false_tag ),
                        Operator::Less => self.asm += &format!( " cmp rdi, rsi\n jge {}\n\n", for_false_tag ),
                        Operator::LessOrEquals => self.asm += &format!( " cmp rdi, rsi\n jg {}\n\n", for_false_tag ),
                        Operator::And => self.asm += &format!( " and rdi, rsi\n jz {}\n\n", for_false_tag ),
                        Operator::Or => self.asm += &format!( " or rdi, rsi\n jz {}\n\n", for_false_tag ),
                        Operator::Xor => self.asm += &format!( " xor rdi, rsi\n jz {}\n\n", for_false_tag ),

                        Operator::Pow | Operator::PowEquals
                        | Operator::Times | Operator::TimesEquals
                        | Operator::Divide | Operator::DivideEquals
                        | Operator::Remainder | Operator::RemainderEquals
                        | Operator::Plus | Operator::PlusEquals
                        | Operator::Minus | Operator::MinusEquals
                        | Operator::Compare
                        | Operator::Not | Operator::Negate => unreachable!(),
                    };

                },
                Expression::Identifier( src_name, _ ) => {
                    let src_variable = self.resolve( src_name );
                    self.asm += &format!(
                        " mov dil, [rbp + {}]\
                        \n cmp dil, true\
                        \n jne {}\n\n",
                        src_variable.offset,
                        for_false_tag
                    )
                },
                Expression::Unary { operand, .. } => {
                    self.compile_expression( operand );

                    // we can only have boolean expressions at this point, so it's safe to ignore the integer negation case
                    self.asm += &format!(
                        " xor dil, 1\
                        \n jz {}\n\n",
                        for_false_tag
                    );
                },
                Expression::Literal( _ ) => unreachable!(),
            }
        }

        self.compile_scope( forr.scope );
    }


    fn compile_assignment( &mut self, name: &String, new_value: &'ast Expression ) {
        let variable = self.resolve( name );
        let variable_typ = variable.typ;
        let variable_offset = variable.offset;
        self.asm += &format!( " ; {} = {}\n", name, new_value );

        match new_value {
            Expression::Literal( Literal::Int { value } ) =>
                self.asm += &format!(
                    " mov rdi, {}\
                    \n mov [rbp + {}], rdi\n\n",
                    value, variable_offset
                ),
            Expression::Literal( Literal::Char { value } ) =>
                self.asm += &format!( " mov byte [rbp + {}], {}\n\n", variable_offset, value ),
            Expression::Literal( Literal::Bool { value } ) =>
                self.asm += &format!( " mov byte [rbp + {}], {}\n\n", variable_offset, value ),
            Expression::Literal( Literal::Str( string @ Str { .. } ) ) => {
                let string_label_idx = self.string_label_idx( string );
                let string_label = &self.strings[ string_label_idx ];

                self.asm += &format!(
                    " mov qword [rbp + {}], {}\
                    \n mov qword [rbp + {}], {}\n\n",
                    variable_offset, string_label.label,
                    variable_offset + 8, string_label.len_label
                );
            },
            Expression::Binary { .. } => {
                self.compile_expression( new_value );

                match variable_typ {
                    Type::Int | Type::Str => self.asm += &format!( " mov [rbp + {}], rdi\n\n", variable_offset ),
                    Type::Char | Type::Bool => self.asm += &format!( " mov [rbp + {}], dil\n\n", variable_offset ),
                }
            }
            Expression::Identifier( src_name, _ ) => {
                let src_variable = self.resolve( src_name );
                let src_variable_typ = src_variable.typ;
                let src_variable_offset = src_variable.offset;

                match src_variable_typ {
                    Type::Int =>
                        self.asm += &format!(
                            " mov rdi, [rbp + {}]\
                            \n mov [rbp + {}], rdi\n\n",
                            src_variable_offset, variable_offset
                        ),
                    Type::Char | Type::Bool =>
                        self.asm += &format!(
                            " movzx rdi, byte [rbp + {}]\
                            \n mov [rbp + {}], rdi\n\n",
                            src_variable_offset, variable_offset
                        ),
                    Type::Str =>
                        self.asm += &format!(
                            " mov rsi, [rbp + {}]\
                            \n mov rdx, [rbp + {}]\
                            \n mov [rbp + {}], rsi\
                            \n mov [rbp + {}], rdx\n\n",
                            src_variable_offset, variable_offset,
                            src_variable_offset + 8, variable_offset + 8
                        ),
                }
            },
            Expression::Unary { op, operand } => {
                self.compile_expression( operand );
                match op {
                    Operator::Negate => self.asm += " neg rdi\n",
                    Operator::Not => self.asm += " xor dil, 1\n",
                    _ => unreachable!()
                }

                match variable_typ {
                    Type::Int | Type::Str => self.asm += &format!( " mov [rbp + {}], rdi\n\n", variable_offset ),
                    Type::Char | Type::Bool => self.asm += &format!( " mov [rbp + {}], dil\n\n", variable_offset ),
                }
            },
        }
    }
}


fn print_usage() {
    println!( r"
Blitzlang compiler, version {}

Usage: blitz [Options] [Run mode] file.blz

Options:
    -h, --help              Display this message

Run mode:
    check     <file.blz>    Check the source code for correctness
    interpret <file.blz>    Interpret the source code (default if no run mode command is provided)
    compile   <file.blz>    Compile the source code down to a binary executable
    run       <file.blz>    Compile and run the generated binary executable
", env!( "CARGO_PKG_VERSION" ) );
}


// IDEA make the source generic, eg: to be able to compile from strings instead of just files
#[derive( Debug )]
struct BlitzSrc {
    path: PathBuf,
    src: BufReader<File>,
}

impl BlitzSrc {
    const CAUSE: &'static str = colored!{
        text: "Cause",
        foreground: Foreground::LightRed,
        bold: true,
    };


    fn try_from( path: &Path ) -> Result<Self, std::io::Error> {
        return match File::open( path ) {
            Ok( file ) => match file.metadata().unwrap().is_file() {
                true => Ok( Self { path: PathBuf::from( path ), src: BufReader::with_capacity( 1, file ) } ),
                false => Err( std::io::Error::new(
                    ErrorKind::InvalidInput,
                    format!( "{}: invalid path '{}'\n{}: is a directory", SyntaxErrors::ERROR, path.display(), BlitzSrc::CAUSE )
                ) ),
            },
            Err( err ) => {
                let kind = err.kind();
                Err( std::io::Error::new(
                    kind,
                    format!( "{}: could not open file '{}'\n{}: {}", SyntaxErrors::ERROR, path.display(), BlitzSrc::CAUSE, kind )
                ) )
            },
        }
    }
}

struct Blitz;

impl Blitz {
    fn from( args: Args ) -> ExitCode {
        // to quickly debug
        // args.push( "compile".to_string() );
        // args.push( "run".to_string() );
        // args.push( "examples/main.blz".to_string() );

        if args.len() < 2 {
            print_usage();
            return ExitCode::SUCCESS;
        }

        let mut check_flag = false;
        let mut interpret_flag = false;
        let mut build_flag = false;
        let mut run_flag = false;

        let mut source_file_path: Option<String> = None;
        let mut args_iter = args.into_iter();
        args_iter.next(); // skipping the name of this executable
        for arg in args_iter {
            match arg.as_str() {
                "-h" | "--help" => {
                    print_usage();
                    return ExitCode::SUCCESS;
                },
                "check" => check_flag = true,
                "interpret" => interpret_flag = true,
                "compile" => build_flag = true,
                "run" => run_flag = true,
                _ => match source_file_path {
                    None => source_file_path = Some( arg ),
                    Some( _ ) => {
                        eprintln!( "{}: too many source file paths provided", SyntaxErrors::ERROR );
                        return ExitCode::FAILURE;
                    },
                },
            }
        }


        if !(check_flag || interpret_flag || build_flag || run_flag) {
            interpret_flag = true;
        }
        else if check_flag {
            // ignore all other run mode flags
        }
        else if interpret_flag && (build_flag || run_flag) {
            eprintln!( "{}: cannot interpret and build/run at the same time", SyntaxErrors::ERROR );
            return ExitCode::FAILURE;
        }
        else if build_flag && run_flag {
            eprintln!( "{}: build and run commands cannot be used together", SyntaxErrors::ERROR );
            return ExitCode::FAILURE;
        }

        let source_file_path = match source_file_path {
            Some( path ) => path,
            None => {
                eprintln!( "{}: no source file path provided", SyntaxErrors::ERROR );
                return ExitCode::FAILURE;
            },
        };

        let source_file = match BlitzSrc::try_from( Path::new( &source_file_path ) ) {
            Ok( src ) => src,
            Err( err ) => {
                eprintln!( "{}", err );
                return ExitCode::FAILURE;
            },
        };

        if check_flag {
            Checker::check( &source_file_path );
        }

        let lexer = match Lexer::try_from( source_file ) {
            Ok( lexer ) => {
                lexer
            },
            Err( mut errors ) => {
                errors.display();
                return ExitCode::FAILURE;
            },
        };

        let ast = match AST::try_from( lexer ) {
            Ok( ast ) => {
                // println!( "{:#?}", ast );
                ast
            },
            Err( mut errors ) => {
                errors.display();
                return ExitCode::FAILURE;
            },
        };

        if check_flag {
            // do nothing
        }
        else if interpret_flag {
            let mut interpreter = Interpreter { ast: &ast, variables: Vec::new() };
            interpreter.interpret( &source_file_path );
        }
        else {
            let mut compiler = Compiler {
                ast: &ast,
                rodata: String::new(),
                asm: String::new(),
                variables: Vec::new(),
                strings: Vec::new(),
                if_idx: 0,
                for_idx: 0,
                for_idx_stack: Vec::new(),
            };

            if build_flag {
                let _ = compiler.compile( &source_file_path );
            }
            else if run_flag {
                let _ = compiler.run( &source_file_path );
            }
        }

        return ExitCode::SUCCESS;
    }
}


// IDEA adapt SyntaxErrors to report cli mistakes
fn main() -> ExitCode {
    return Blitz::from( env::args() );
}
