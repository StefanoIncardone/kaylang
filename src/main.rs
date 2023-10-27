// TODO detect if running in tty or being piped and disable coloring
use std::{io::{BufReader, BufRead, ErrorKind, BufWriter, Write, Seek, SeekFrom}, fs::File, env::{self, Args}, process::{ExitCode, Command}, fmt::Display, path::{Path, PathBuf}, borrow::Cow, cmp::Ordering, num::IntErrorKind, time::Instant};

mod color;
use crate::color::*;


static CHECKING:    ColoredStr = ColoredStr { text: " Checking", foreground: Foreground::LightGreen, background: Background::Default, flags: Flags::Bold };
static COMPILING:   ColoredStr = ColoredStr { text: "Compiling", foreground: Foreground::LightGreen, background: Background::Default, flags: Flags::Bold };
static RUNNING:     ColoredStr = ColoredStr { text: "  Running", foreground: Foreground::LightGreen, background: Background::Default, flags: Flags::Bold };
static DONE:        ColoredStr = ColoredStr { text: "     Done", foreground: Foreground::LightGreen, background: Background::Default, flags: Flags::Bold };

static ERROR:       ColoredStr = ColoredStr { text: "Error", foreground: Foreground::LightRed, background: Background::Default, flags: Flags::Bold };
static CAUSE:       ColoredStr = ColoredStr { text: "Cause", foreground: Foreground::LightRed, background: Background::Default, flags: Flags::Bold };

static VERSION:     ColoredStr = ColoredStr { text: env!( "CARGO_PKG_VERSION" ), foreground: Foreground::LightGray, background: Background::Default, flags: Flags::Bold };
static OPTIONS:     ColoredStr = ColoredStr { text: "Options",  foreground: Foreground::LightGray, background: Background::Default, flags: Flags::Bold };
static RUN_MODE:    ColoredStr = ColoredStr { text: "Run mode", foreground: Foreground::LightGray, background: Background::Default, flags: Flags::Bold };
#[allow(unused)]
static OUTPUT:      ColoredStr = ColoredStr { text: "Output",   foreground: Foreground::LightGray, background: Background::Default, flags: Flags::Bold };
static MODE:        ColoredStr = ColoredStr { text: "mode",     foreground: Foreground::LightGray, background: Background::Default, flags: Flags::Bold };


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
    Int( isize ),
    Char( u8 ), // only supporting ASCII characters for now
    Bool( bool ),
    Str( Str ),
}

impl Display for Literal {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Int( value ) => write!( f, "{}", value ),
            Self::Char( code ) => write!( f, "'{}'", code.escape_ascii() ), // TODO create own escaping function
            Self::Bool( value ) => write!( f, "{}", value ),
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
            Self::Int( value ) => value.into(),
            Self::Char( code ) => code.into(),
            Self::Bool( value ) => value.into(),
            Self::Str( string ) => string.text.len() as isize,
        }
    }
}

impl Len for Literal {
    fn len( &self ) -> usize {
        return match self {
            Self::Int( value ) => value.to_string().len(),
            Self::Char( _ ) => 1,
            Self::Bool( value ) => value.to_string().len(),
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
    Colon,
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
}

impl Display for TokenKind {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Comment( text ) => write!( f, "{}", text ),
            Self::Unexpected( text ) => write!( f, "{}", text ),

            Self::Bracket( bracket ) => write!( f, "{}", bracket ),
            Self::Equals => write!( f, "=" ),
            Self::Colon => write!( f, ":" ),
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

            Self::Empty => write!( f, "" ),
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
            Self::Colon => 1,
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
    number: usize,
    byte_start: usize,
    token_start_idx: usize,
    token_end_idx: usize,
}


// TODO create different kinds of errors for different stages of compilation
// TODO implement NOTE, HINT, HELP in error messages
#[derive( Debug )]
struct SyntaxError {
    line_byte_start: usize,
    line: usize,
    col: usize,
    len: usize,
    msg: Cow<'static, str>,
    help_msg: Cow<'static, str>,
}

// TODO find a way to implement display for this
    // NOTE reading from file requires &mut self, while the Display trait requires a &self,
    // so the implementation of the "Display" trait is inlined for now
#[derive( Debug )]
struct SyntaxErrors {
    src: BlitzSrc,
    errors: Vec<SyntaxError>,
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
            col: 0,
            token_start_col: 1,
            lines: Vec::new(),
            tokens: Vec::new(),
            line_bytes: Vec::new(),
            token_text: String::new(),
            brackets: Vec::new(),
            errors: Vec::new(),
        };

        let mut line = Line {
            number: 1,
            byte_start: 0,
            token_start_idx: 0,
            token_end_idx: 0
        };

        loop {
            this.line_bytes.clear();
            match this.src.src.read_until( b'\n', &mut this.line_bytes ) {
                Ok( chars_read ) => {
                    this.line += 1;
                    this.col = 0;
                    this.token_start_col = 1;

                    let mut line_len = this.line_bytes.len();
                    while line_len > 0 && this.line_bytes[ line_len - 1 ].is_ascii_whitespace() {
                        line_len -= 1;
                    }

                    if line_len == 0 {
                        this.tokens.push( Token { col: 1, kind: TokenKind::Empty } );
                    }
                    else {
                        this.line_bytes.truncate( line_len );
                        loop {
                            let token = match this.tokeninze_next() {
                                Ok( None ) => break,
                                Ok( Some( kind ) ) => Token { col: this.token_start_col, kind },
                                Err( err ) => {
                                    this.errors.push( err );
                                    let unrecognized = this.token_text();
                                    Token { col: this.token_start_col, kind: TokenKind::Unexpected( unrecognized ) }
                                }
                            };

                            this.tokens.push( token );
                        }
                    }


                    line.token_end_idx = this.tokens.len() - 1;
                    this.lines.push( line );

                    this.line_byte_start += chars_read;
                    line.byte_start += chars_read;
                    line.number += 1;
                    line.token_start_idx = this.tokens.len();

                    let reached_eof = chars_read == line_len;
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
                line: bracket.line_number,
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

    fn gather_token_text( &mut self ) {
        self.token_text = self.token_text();
    }

    // TODO move String related function to the Str struct
    // TODO create own from_utf8 function
    fn token_text( &self ) -> String {
        return String::from_utf8_lossy( &self.line_bytes[ self.token_start_col - 1..self.col ] ).into();
    }

    // FIX properly handle non ASCII characters
        // TODO add an absolute column for the bytes in the line
        // IDEA only allow utf-8 characters in strings, characters and comments
    fn next( &mut self ) -> Result<Option<u8>, SyntaxError> {
        if self.col >= self.line_bytes.len() {
            return Ok( None );
        }

        let next = self.line_bytes[ self.col ];
        self.col += 1;
        return match next {
            ..=b'\x7F' => Ok( Some( next ) ),
            _ => Err( SyntaxError {
                line_byte_start: self.line_byte_start,
                line: self.line,
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
                line: self.line,
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
                line: self.line,
                col: self.token_start_col,
                len: self.col - self.token_start_col + 1,
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
                line: self.line,
                col: self.token_start_col,
                len: self.col - self.token_start_col + 1,
                msg: "invalid string literal".into(),
                help_msg: "missing closing double quote".into()
            }),
            Some( next ) => Ok( next ),
        }
    }

    fn tokeninze_next( &mut self ) -> Result<Option<TokenKind>, SyntaxError> {
        // this loop is ever going to run just once, it's here just so we can use continue and break
        loop {
            self.token_text.clear();
            let next = match self.next()? {
                Some( ch ) => ch,
                None => return Ok( None ),
            };

            // registering the token start column after getting the next character to maintain 1 indexing token columns
            self.token_start_col = self.col;
            return match next {
                // ignore whitespace
                b'\t' | b'\x0C' | b'\r' | b' ' => continue,
                b'a'..=b'z' | b'A'..=b'Z' | b'_'  => {
                    let mut contains_non_ascii = false;
                    loop {
                        match self.next() {
                            Ok( Some( b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_' ) ) => {},
                            Ok( Some( _ ) ) => {
                                self.col -= 1;
                                break;
                            },
                            Ok( None ) => break,
                            Err( _ ) => contains_non_ascii = true,
                        }
                    }

                    if contains_non_ascii {
                        Err( SyntaxError {
                            line_byte_start: self.line_byte_start,
                            line: self.line,
                            col: self.token_start_col,
                            len: self.col - self.token_start_col + 1,
                            msg: "invalid identifier".into(),
                            help_msg: "contains non-ASCII characters".into()
                        })
                    }
                    else {
                        self.gather_token_text();
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
                        match self.next() {
                            Ok( Some( b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_') ) => {},
                            Ok( Some( _ ) ) => {
                                self.col -= 1;
                                break;
                            },
                            Ok( None ) => break,
                            Err( _ ) => contains_non_ascii = true,
                        }
                    }

                    self.gather_token_text();
                    match self.token_text.parse() { // TODO create own number parsing function
                        Ok( value ) => Ok( Some( TokenKind::Literal( Literal::Int( value ) ) ) ),
                        Err( err ) => match err.kind() {
                            IntErrorKind::InvalidDigit =>
                                if !contains_non_ascii {
                                    Err( SyntaxError {
                                        line_byte_start: self.line_byte_start,
                                        line: self.line,
                                        col: self.token_start_col,
                                        len: self.token_text.len(),
                                        msg: "invalid number literal".into(),
                                        help_msg: "contains non-digit characters".into()
                                    })
                                }
                                else {
                                    Err( SyntaxError {
                                        line_byte_start: self.line_byte_start,
                                        line: self.line,
                                        col: self.token_start_col,
                                        len: self.token_text.len(),
                                        msg: "invalid number literal".into(),
                                        help_msg: "contains non-ASCII characters".into()
                                    })
                                },
                            IntErrorKind::PosOverflow => Err( SyntaxError {
                                line_byte_start: self.line_byte_start,
                                line: self.line,
                                col: self.token_start_col,
                                len: self.token_text.len(),
                                msg: "invalid number literal".into(),
                                help_msg: format!( "overflows a {} bit signed integer (over {})", isize::BITS, isize::MAX ).into()
                            }),
                            IntErrorKind::NegOverflow => Err( SyntaxError {
                                line_byte_start: self.line_byte_start,
                                line: self.line,
                                col: self.token_start_col,
                                len: self.token_text.len(),
                                msg: "invalid number literal".into(),
                                help_msg: format!( "underflows a {} bit signed integer (under {})", isize::BITS, isize::MIN ).into()
                            }),
                            IntErrorKind::Empty | std::num::IntErrorKind::Zero => unreachable!(),
                            _ => Err( SyntaxError {
                                line_byte_start: self.line_byte_start,
                                line: self.line,
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
                    let comment = self.token_text();
                    Ok( Some( TokenKind::Comment( comment ) ) )
                },
                b'"' => {
                    let mut errors: Vec<SyntaxError> = Vec::new();

                    loop {
                        let next = match self.next_str_char()? {
                            b'\\' => match self.next_str_char()? {
                                b'\\' => Ok( '\\' ),
                                b'\'' => Ok( '\'' ),
                                b'"' => Ok( '"' ),
                                b'n' => Ok( '\n' ),
                                b'r' => Ok( '\r' ),
                                b't' => Ok( '\t' ),
                                b'0' => Ok( '\0' ),
                                _ => Err( SyntaxError {
                                    line_byte_start: self.line_byte_start,
                                    line: self.line,
                                    col: self.col,
                                    len: 1,
                                    msg: "invalid string character".into(),
                                    help_msg: "unrecognized escape character".into()
                                }),
                            },
                            b'\x00'..=b'\x1F' | b'\x7F' => Err( SyntaxError {
                                line_byte_start: self.line_byte_start,
                                line: self.line,
                                col: self.col,
                                len: 1,
                                msg: "invalid string literal".into(),
                                help_msg: "cannot be a control character".into()
                            }),
                            b'"' => break,
                            other => Ok( other as char ),
                        };

                        match next {
                            Ok( next_char ) => self.token_text.push( next_char ),
                            Err( err ) => errors.push( err ),
                        }
                    }

                    // after here there cannot be unclosed strings
                    if errors.is_empty() {
                        Ok( Some( TokenKind::Literal( Literal::Str( Str { text: self.token_text.clone().into_bytes() } ))))
                    }
                    else {
                        // FIX add proper multiple error handling
                        let last_error = errors.pop().unwrap();
                        self.errors.extend( errors );
                        Err( last_error )
                    }
                },
                b'\'' => {
                    let code = match self.next_char()? {
                        b'\\' => match self.next_char()? {
                            b'\\' => Ok( b'\\' ),
                            b'\'' => Ok( b'\'' ),
                            b'"' => Ok( b'"' ),
                            b'n' => Ok( b'\n' ),
                            b'r' => Ok( b'\r' ),
                            b't' => Ok( b'\t' ),
                            b'0' => Ok( b'\0' ),
                            _ => Err( SyntaxError {
                                line_byte_start: self.line_byte_start,
                                line: self.line,
                                col: self.col,
                                len: 1,
                                msg: "invalid character literal".into(),
                                help_msg: "unrecognized escape character".into()
                            }),
                        },
                        b'\x00'..=b'\x1F' | b'\x7F' => Err( SyntaxError {
                            line_byte_start: self.line_byte_start,
                            line: self.line,
                            col: self.col,
                            len: 1,
                            msg: "invalid character literal".into(),
                            help_msg: "cannot be a control character".into()
                        }),
                        b'\'' => break Err( SyntaxError {
                            line_byte_start: self.line_byte_start,
                            line: self.line,
                            col: self.token_start_col,
                            len: 2,
                            msg: "invalid character literal".into(),
                            help_msg: "must not be empty".into()
                        }),
                        ch => Ok( ch ),
                    };

                    match self.peek_next()? {
                        Some( b'\'' ) => {
                            self.col += 1;
                            Ok( Some( TokenKind::Literal( Literal::Char( code? ) ) ) )
                        },
                        Some( _ ) | None => Err( SyntaxError {
                            line_byte_start: self.line_byte_start,
                            line: self.line,
                            col: self.token_start_col,
                            len: self.col - self.token_start_col + 1,
                            msg: "invalid character literal".into(),
                            help_msg: "missing closing single quote".into()
                        })
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
                    Some( Bracket { kind: BracketKind::OpenRound | BracketKind::CloseCurly | BracketKind::CloseRound, .. } ) =>
                        Ok( Some( TokenKind::Bracket( BracketKind::CloseRound ) ) ),
                    Some( Bracket { kind: BracketKind::OpenCurly, .. } ) => Err( SyntaxError {
                        line_byte_start: self.line_byte_start,
                        line: self.line,
                        col: self.token_start_col,
                        len: 1,
                        msg: "stray bracket".into(),
                        help_msg: "closes the wrong bracket".into()
                    }),
                    None => Err( SyntaxError {
                        line_byte_start: self.line_byte_start,
                        line: self.line,
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
                    Some( Bracket { kind: BracketKind::OpenCurly | BracketKind::CloseCurly | BracketKind::CloseRound, .. } ) =>
                        Ok( Some( TokenKind::Bracket( BracketKind::CloseCurly ) ) ),
                    Some( Bracket { kind: BracketKind::OpenRound, .. } ) => Err( SyntaxError {
                        line_byte_start: self.line_byte_start,
                        line: self.line,
                        col: self.token_start_col,
                        len: 1,
                        msg: "stray bracket".into(),
                        help_msg: "closes the wrong bracket".into()
                    }),
                    None => Err( SyntaxError {
                        line_byte_start: self.line_byte_start,
                        line: self.line,
                        col: self.token_start_col,
                        len: 1,
                        msg: "stray bracket".into(),
                        help_msg: "was not opened before".into()
                    }),
                },
                b':' => Ok( Some( TokenKind::Colon ) ),
                b';' => Ok( Some( TokenKind::SemiColon ) ),
                b'*' => match self.peek_next()? {
                    Some( b'*' ) => {
                        self.col += 1;
                        match self.peek_next()? {
                            Some( b'=' ) => {
                                self.col += 1;
                                Ok( Some( TokenKind::Op( Operator::PowEquals ) ) )
                            }
                            _ => Ok( Some( TokenKind::Op( Operator::Pow ) ) ),
                        }
                    },
                    Some( b'=' ) => {
                        self.col += 1;
                        Ok( Some( TokenKind::Op( Operator::TimesEquals ) ) )
                    },
                    _ => Ok( Some( TokenKind::Op( Operator::Times ) ) ),
                },
                b'/' => match self.peek_next()? {
                    Some( b'=' ) => {
                        self.col += 1;
                        Ok( Some( TokenKind::Op( Operator::DivideEquals ) ) )
                    },
                    _ => Ok( Some( TokenKind::Op( Operator::Divide ) ) ),
                },
                b'%' => match self.peek_next()? {
                    Some( b'=' ) => {
                        self.col += 1;
                        Ok( Some( TokenKind::Op( Operator::RemainderEquals ) ) )
                    },
                    _ => Ok( Some( TokenKind::Op( Operator::Remainder ) ) ),
                },
                b'+' => match self.peek_next()? {
                    Some( b'=' ) => {
                        self.col += 1;
                        Ok( Some( TokenKind::Op( Operator::PlusEquals ) ) )
                    },
                    _ => Ok( Some( TokenKind::Op( Operator::Plus ) ) )
                },
                b'-' => match self.peek_next()? {
                    Some( b'=' ) => {
                        self.col += 1;
                        Ok( Some( TokenKind::Op( Operator::MinusEquals ) ) )
                    },
                    _ => Ok( Some( TokenKind::Op( Operator::Minus ) ) ),
                },
                b'=' => match self.peek_next()? {
                    Some( b'=' ) => {
                        self.col += 1;
                        Ok( Some( TokenKind::Op( Operator::EqualsEquals ) ) )
                    },
                    _ => Ok( Some( TokenKind::Equals ) ),
                },
                b'!' => match self.peek_next()? {
                    Some( b'=' ) => {
                        self.col += 1;
                        Ok( Some( TokenKind::Op( Operator::NotEquals ) ) )
                    },
                    _ => Ok( Some( TokenKind::Op( Operator::Not ) ) ),
                },
                b'>' => match self.peek_next()? {
                    Some( b'=' ) => {
                        self.col += 1;
                        Ok( Some( TokenKind::Op( Operator::GreaterOrEquals ) ) )
                    },
                    _ => Ok( Some( TokenKind::Op( Operator::Greater ) ) ),
                },
                b'<' => match self.peek_next()? {
                    Some( b'=' ) => {
                        self.col += 1;
                        match self.peek_next()? {
                            Some( b'>' ) => {
                                self.col += 1;
                                Ok( Some( TokenKind::Op( Operator::Compare ) ) )
                            },
                            _ => Ok( Some( TokenKind::Op( Operator::LessOrEquals ) ) ),
                        }
                    },
                    _ => Ok( Some( TokenKind::Op( Operator::Less ) ) ),
                },
                b'^' => match self.peek_next()? {
                    Some( b'^' ) => {
                        self.col += 1;
                        Ok( Some( TokenKind::Op( Operator::Xor ) ) )
                    },
                    _ => Err( SyntaxError {
                        line_byte_start: self.line_byte_start,
                        line: self.line,
                        col: self.token_start_col,
                        len: 1,
                        msg: "unexpected character".into(),
                        help_msg: "did you mean '^^'?".into()
                    }),
                },
                b'&' => match self.peek_next()? {
                    Some( b'&' ) => {
                        self.col += 1;
                        Ok( Some( TokenKind::Op( Operator::And ) ) )
                    },
                    _ => Err( SyntaxError {
                        line_byte_start: self.line_byte_start,
                        line: self.line,
                        col: self.token_start_col,
                        len: 1,
                        msg: "unexpected character".into(),
                        help_msg: "did you mean '&&'?".into()
                    }),
                },
                b'|' => match self.peek_next()? {
                    Some( b'|' ) => {
                        self.col += 1;
                        Ok( Some( TokenKind::Op( Operator::Or ) ) )
                    },
                    _ => Err( SyntaxError {
                        line_byte_start: self.line_byte_start,
                        line: self.line,
                        col: self.token_start_col,
                        len: 1,
                        msg: "unexpected character".into(),
                        help_msg: "did you mean '||'?".into()
                    }),
                },
                b'\n' => unreachable!( "line text should have been trimmed already" ),
                _ => Err( SyntaxError {
                    line_byte_start: self.line_byte_start,
                    line: self.line,
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
    fn current( &mut self ) -> Option<TokenPosition<'lexer>> {
        if self.token >= self.tokens.len() || self.line >= self.lines.len() {
            return None
        }
        return Some( TokenPosition{ line: &self.lines[ self.line ], token: &self.tokens[ self.token ] } ).or_next( self );
    }

    fn next( &mut self ) -> Option<TokenPosition<'lexer>> {
        if self.token >= self.tokens.len() - 1 {
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


    fn bounded( self, tokens: &mut TokenCursor<'lexer>, err_msg: impl Into<String> ) -> Result<TokenPosition<'lexer>, Self::Error> where Self: Sized;
    // NOTE no implementation for "bounded_previous" is needed since it is never actually needed during parsin
    fn or_next( self, tokens: &mut TokenCursor<'lexer> ) -> Self where Self: Sized;
    fn or_previous( self, tokens: &mut TokenCursor<'lexer> ) -> Self where Self: Sized;
}

impl<'lexer> BoundedPosition<'lexer> for Option<TokenPosition<'lexer>> {
    type Error = SyntaxError;


    fn bounded( self, tokens: &mut TokenCursor<'lexer>, err_msg: impl Into<String> ) -> Result<TokenPosition<'lexer>, Self::Error> {
        return match self {
            Some( position ) => Ok( position ),
            None => {
                // we are always sure that this function is never called without a previous token existing, so we can safely unwrap
                let previous = unsafe{ tokens.peek_previous().unwrap_unchecked() };
                Err( SyntaxError {
                    line_byte_start: previous.line.byte_start,
                    line: previous.line.number,
                    col: previous.token.col,
                    len: previous.token.kind.len(),
                    msg: err_msg.into().into(),
                    help_msg: "file ended after here instead".into()
                } )
            },
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


#[derive( Debug, Clone )]
struct Variable {
    mutability: Mutability,
    name: String,
    typ: Type,
}


#[derive( Debug, Clone )]
struct If {
    condition: Expression,
    statement: Node,
}

impl Display for If {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return write!( f, "if {}", self.condition );
    }
}

#[derive( Debug, Clone )]
struct IfStatement {
    ifs: Vec<If>,
    els: Option<Box<Node>>,
}


#[allow( dead_code )]
#[derive( Debug, Clone )]
struct ForStatement {
    pre: Option<Box<Node>>,
    condition: Option<Expression>,
    post: Option<Box<Node>>,
    statement: Box<Node>,
}


#[derive( Debug, Clone )]
enum Node {
    Expression( Expression ),
    Print( Expression ),
    Println( Option<Expression> ),
    If( IfStatement ),
    For( ForStatement ),
    Break,
    Continue,

    Definition( String, Expression ),
    Assignment( String, Expression ),
    Scope( usize ),
}

impl Display for Node {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Expression( expression ) => write!( f, "{}", expression ),
            Self::Print( argument ) => write!( f, "print {}", argument ),
            Self::Println( argument ) => match argument {
                Some( arg ) => write!( f, "println {}", arg ),
                None => write!( f, "println" ),
            },
            Self::If( iff ) => write!( f, "{}", iff.ifs[ 0 ] ),
            Self::For( forr ) => match &forr.condition {
                Some( condition ) => write!( f, "for {}", condition ),
                None => write!( f, "for" ),
            },

            Self::Break | Self::Continue
            | Self::Definition( _, _ ) | Self::Assignment( _, _ )
            | Self::Scope( _ ) => unreachable!(),
        }
    }
}


#[derive( Debug, Clone )]
struct Scope {
    parent: usize,
    #[allow(dead_code)]
    types: Vec<String>,
    variables: Vec<Variable>,
    nodes: Vec<Node>,
}

// TODO process entire statement for syntactical correctness and then report all the errors
    // IDEA create Parser class that builds the AST, and then validate the AST afterwards
#[derive( Debug )]
struct AST {
    scopes: Vec<Scope>,
    current_scope: usize,
    for_depth: usize,

    errors: Vec<SyntaxError>,
}

impl TryFrom<Lexer> for AST {
    type Error = SyntaxErrors;

    fn try_from( lexer: Lexer ) -> Result<Self, Self::Error> {
        let mut this = Self {
            scopes: vec![Scope {
                parent: 0,
                types: vec![
                    Type::Int.to_string(),
                    Type::Char.to_string(),
                    Type::Bool.to_string(),
                    Type::Str.to_string()
                ],
                variables: Vec::new(),
                nodes: Vec::new(),
            }],
            current_scope: 0,
            for_depth: 0,
            errors: Vec::new(),
        };

        let mut tokens = lexer.iter();
        this.parse_scope( &mut tokens );

        return match this.errors.is_empty() {
            true => Ok( this ),
            false => Err( SyntaxErrors { src: lexer.src, errors: this.errors } ),
        }
    }
}

// scopes
impl AST {
    fn parse_scope( &mut self, tokens: &mut TokenCursor ) {
        loop {
            match self.parse_single( tokens ) {
                Ok( Some( node ) ) => self.scopes[ self.current_scope ].nodes.push( node ),
                Ok( None ) => break,
                Err( err ) => {
                    self.errors.push( err );

                    // consuming all remaining tokens until the end of the file
                    tokens.line = tokens.lines.len();
                    tokens.token = tokens.tokens.len();

                    // NOTE only parsing until the first error until a fault tolerant parser is developed, this is
                    // because the first truly relevant error is the first one, which in turn causes a ripple effect
                    // that propagates to the rest of the parsing, causing subsequent errors to be wrong
                    break;
                },
            }
        }
    }

    fn parse_single( &mut self, tokens: &mut TokenCursor ) -> Result<Option<Node>, SyntaxError> {
        loop {
            let current = match tokens.current() {
                Some( position ) => position,
                None => return Ok( None ),
            };

            return match current.token.kind {
                TokenKind::Literal( _ )
                | TokenKind::True | TokenKind::False
                | TokenKind::Bracket( BracketKind::OpenRound )
                | TokenKind::Op( Operator::Minus | Operator::Not ) => Ok( Some( Node::Expression( self.expression( tokens )? ) ) ),
                TokenKind::Definition( _ ) => Ok( Some( self.variable_definition( tokens )? ) ),
                TokenKind::Print | TokenKind::PrintLn => Ok( Some( self.print( tokens )? ) ),
                TokenKind::Identifier( _ ) => Ok( Some( self.variable_reassignment_or_expression( tokens )? ) ),
                TokenKind::If => Ok( Some( self.if_statement( tokens )? ) ),
                TokenKind::Else => {
                    tokens.next();
                    Err( SyntaxError {
                        line_byte_start: current.line.byte_start,
                        line: current.line.number,
                        col: current.token.col,
                        len: current.token.kind.len(),
                        msg: "invalid if statement".into(),
                        help_msg: "stray else block".into()
                    })
                },
                TokenKind::For => Ok( Some( self.for_statement( tokens )? ) ),
                TokenKind::Break => {
                    tokens.next();
                    match self.for_depth {
                        0 => Err( SyntaxError {
                            line_byte_start: current.line.byte_start,
                            line: current.line.number,
                            col: current.token.col,
                            len: current.token.kind.len(),
                            msg: "invalid break statement".into(),
                            help_msg: "cannot be used outside of loops".into()
                        }),
                        _ => {
                            self.semicolon( tokens )?;
                            Ok( Some( Node::Break ) )
                        },
                    }
                },
                TokenKind::Continue => {
                    tokens.next();
                    match self.for_depth {
                        0 => Err( SyntaxError {
                            line_byte_start: current.line.byte_start,
                            line: current.line.number,
                            col: current.token.col,
                            len: current.token.kind.len(),
                            msg: "invalid continue statement".into(),
                            help_msg: "cannot be used outside of loops".into()
                        }),
                        _ => {
                            self.semicolon( tokens )?;
                            Ok( Some( Node::Continue ) )
                        },
                    }
                },
                TokenKind::Bracket( BracketKind::OpenCurly ) => {
                    let new_scope = self.scopes.len();
                    self.scopes.push( Scope {
                        parent: self.current_scope,
                        types: Vec::new(),
                        variables: Vec::new(),
                        nodes: Vec::new()
                    } );
                    self.current_scope = new_scope;

                    tokens.next();
                    self.parse_scope( tokens );
                    Ok( Some( Node::Scope( new_scope ) ) )
                },
                TokenKind::Bracket( BracketKind::CloseCurly ) => {
                    self.current_scope = self.scopes[ self.current_scope ].parent;
                    tokens.next();
                    Ok( None )
                },
                TokenKind::Bracket( BracketKind::CloseRound ) => {
                    tokens.next();
                    Err( SyntaxError {
                        line_byte_start: current.line.byte_start,
                        line: current.line.number,
                        col: current.token.col,
                        len: current.token.kind.len(),
                        msg: "invalid expression".into(),
                        help_msg: "stray closed parenthesis".into()
                    })
                },
                TokenKind::Colon => {
                    tokens.next();
                    Err( SyntaxError {
                        line_byte_start: current.line.byte_start,
                        line: current.line.number,
                        col: current.token.col,
                        len: current.token.kind.len(),
                        msg: "invalid statement".into(),
                        help_msg: "stray colon".into()
                    })
                },
                TokenKind::Equals => {
                    tokens.next();
                    Err( SyntaxError {
                        line_byte_start: current.line.byte_start,
                        line: current.line.number,
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
                        line: current.line.number,
                        col: current.token.col,
                        len: current.token.kind.len(),
                        msg: "invalid expression".into(),
                        help_msg: "stray binary operator".into()
                    })
                },
                TokenKind::SemiColon => {
                    tokens.next();
                    continue;
                },
                TokenKind::Comment( _ ) | TokenKind::Empty | TokenKind::Unexpected( _ ) => unreachable!(),
            }
        }
    }
}

// semicolons
impl AST {
    fn semicolon( &mut self, tokens: &mut TokenCursor ) -> Result<(), SyntaxError> {
        let semicolon = tokens.current().bounded( tokens, "expected semicolon" )?;
        return match &semicolon.token.kind {
            // semicolons are optional if found before a closing curly bracket
            TokenKind::Bracket( BracketKind::CloseCurly ) => Ok( () ),
            TokenKind::SemiColon => {
                tokens.next();
                Ok( () )
            },
            _ => {
                tokens.next();
                Err( SyntaxError {
                    line_byte_start: semicolon.line.byte_start,
                    line: semicolon.line.number,
                    col: semicolon.token.col,
                    len: semicolon.token.kind.len(),
                    msg: "invalid statement".into(),
                    help_msg: "expected semicolon after this token".into(),
                } )
            }
        }
    }
}

// expressions
impl AST {
    // TODO disallow implicit conversions (str + i64, char + i64, str + char or str + str (maybe treat this as concatenation))
        // IDEA introduce casting operators
    fn factor( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
        let current = tokens.current().bounded( tokens, "expected expression" )?;
        let factor = match &current.token.kind {
            TokenKind::Literal( literal ) => Ok( Expression::Literal( literal.clone() ) ),
            TokenKind::True => Ok( Expression::Literal( Literal::Bool( true ) ) ),
            TokenKind::False => Ok( Expression::Literal( Literal::Bool( false ) ) ),
            TokenKind::Identifier( name ) => match self.resolve( name ) {
                Some( definition ) => Ok( Expression::Identifier( name.clone(), definition.typ ) ),
                None => Err( SyntaxError {
                    line_byte_start: current.line.byte_start,
                    line: current.line.number,
                    col: current.token.col,
                    len: current.token.kind.len(),
                    msg: "variable not defined".into(),
                    help_msg: "was not previously defined in this scope".into()
                }),
            },
            TokenKind::Bracket( BracketKind::OpenRound ) => {
                let expression_start_pos = tokens.next().bounded( tokens, "expected expression" )?;
                match expression_start_pos.token.kind {
                    TokenKind::Bracket( BracketKind::CloseRound ) => Err( SyntaxError {
                        line_byte_start: expression_start_pos.line.byte_start,
                        line: expression_start_pos.line.number,
                        col: expression_start_pos.token.col,
                        len: expression_start_pos.token.kind.len(),
                        msg: "invalid expression".into(),
                        help_msg: "empty expressions are not allowed".into()
                    }),
                    _ => {
                        let expression = self.expression( tokens )?;
                        let close_bracket_pos = tokens.current().bounded( tokens, "expected closed parenthesis" )?;
                        match close_bracket_pos.token.kind {
                            TokenKind::Bracket( BracketKind::CloseRound ) => Ok( expression ),
                            _ => Err( SyntaxError {
                                line_byte_start: current.line.byte_start,
                                line: current.line.number,
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
                                Literal::Int( ref mut value ) => {
                                    *value = -*value;
                                    Ok( operand )
                                },
                                Literal::Char( code ) => {
                                    *literal = Literal::Int( -(*code as isize) );
                                    Ok( operand )
                                },
                                Literal::Str( Str { text } ) => {
                                    *literal = Literal::Int( -(text.len() as isize) );
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
                        line: current.line.number,
                        col: current.token.col,
                        len: current.token.kind.len(),
                        msg: "invalid expression".into(),
                        help_msg: "cannot negate boolean value, use the '!' operator instead".into()
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
                            Expression::Literal( Literal::Bool( ref mut value ) ) => {
                                *value = !*value;
                                Ok( operand )
                            },
                            _ => Ok( Expression::Unary { op: Operator::Not, operand: Box::new( operand ) } ),
                        },
                        false => Ok( operand ),
                    },
                    Type::Int | Type::Char | Type::Str => Err( SyntaxError {
                        line_byte_start: current.line.byte_start,
                        line: current.line.number,
                        col: current.token.col,
                        len: current.token.kind.len(),
                        msg: "invalid expression".into(),
                        help_msg: "cannot invert non boolean value, use the '-' operator instead".into()
                    }),
                };
            },
            _ => Err( SyntaxError {
                line_byte_start: current.line.byte_start,
                line: current.line.number,
                col: current.token.col,
                len: current.token.kind.len(),
                msg: "invalid expression".into(),
                help_msg: "expected expression operand after this token".into()
            }),
        };

        tokens.next();
        return factor;
    }

    fn operator( &mut self, tokens: &mut TokenCursor, ops: &[Operator] ) -> Result<Option<Operator>, SyntaxError> {
        let current_pos = tokens.current().bounded( tokens, "expected operator or semicolon" )?;
        return if let TokenKind::Op( op ) = current_pos.token.kind {
            if ops.contains( &op ) {
                tokens.next();
                Ok( Some( op ) )
            }
            else {
                Ok( None )
            }
        }
        else {
            Ok( None )
        }
    }

    // IDEA optimize expression building by implementing associativity rules (ie. remove parenthesis around additions and subtractions)
    // FIX division by zero, raising to a negative power
    // TODO implement unsigned integers and force powers to only accept those as exponents
    // IDEA print crash error message
        // TODO implement a way to print file, line and column information in source code
    fn exponentiation( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
        let mut lhs = self.factor( tokens )?;

        while let Some( op ) = self.operator( tokens, &[Operator::Pow] )? {
            let rhs = self.factor( tokens )?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn multiplicative_expression( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
        let mut lhs = self.exponentiation( tokens )?;

        while let Some( op ) = self.operator( tokens, &[Operator::Times, Operator::Divide, Operator::Remainder] )? {
            let rhs = self.exponentiation( tokens )?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn additive_expression( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
        let mut lhs = self.multiplicative_expression( tokens )?;

        while let Some( op ) = self.operator( tokens, &[Operator::Plus, Operator::Minus] )? {
            let rhs = self.multiplicative_expression( tokens )?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn comparative_expression( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
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
    fn boolean_expression( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
        let mut lhs = self.comparative_expression( tokens )?;

        while let Some( op ) = self.operator( tokens, &[Operator::And, Operator::Or, Operator::Xor] )? {
            let op_pos = tokens.peek_previous().unwrap();

            if lhs.typ() != Type::Bool {
                return Err( SyntaxError {
                    line_byte_start: op_pos.line.byte_start,
                    line: op_pos.line.number,
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
                    line: op_pos.line.number,
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

    fn expression( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
        return self.boolean_expression( tokens );
    }
}

// variable definitions and assignments
impl<'lexer> AST {
    fn resolve( &'lexer self, name: &str ) -> Option<&'lexer Variable> {
        let mut current_scope = self.current_scope;
        loop {
            let scope = &self.scopes[ current_scope ];

            for variable in &scope.variables {
                if variable.name == name {
                    return Some( variable );
                }
            }

            if current_scope == 0 && scope.parent == 0 {
                return None;
            }

            current_scope = scope.parent;
        }
    }

    fn resolve_mut( &'lexer mut self, name: &str ) -> Option<&'lexer mut Variable> {
        let mut current_scope = self.current_scope;
        loop {
            let scope = &self.scopes[ current_scope ];

            let mut current_variable = 0;
            for variable in &scope.variables {
                if variable.name == name {
                    return Some( &mut self.scopes[ current_scope ].variables[ current_variable ] );
                }
                current_variable += 1;
            }

            if current_scope == 0 && scope.parent == 0 {
                return None;
            }

            current_scope = scope.parent;
        }
    }

    fn variable_definition( &mut self, tokens: &mut TokenCursor ) -> Result<Node, SyntaxError> {
        let definition_pos = tokens.current().unwrap();
        let kind = match definition_pos.token.kind {
            TokenKind::Definition( kind ) => kind,
            _ => unreachable!(),
        };

        let name_pos = tokens.next().bounded( tokens, "expected identifier" )?;
        let name = match &name_pos.token.kind {
            TokenKind::Identifier( name ) => Ok( name.clone() ),
            _ => Err( SyntaxError {
                line_byte_start: name_pos.line.byte_start,
                line: name_pos.line.number,
                col: name_pos.token.col,
                len: name_pos.token.kind.len(),
                msg: "invalid assignment".into(),
                help_msg: "expected variable name".into()
            }),
        };

        let equals_pos = tokens.next().bounded( tokens, "expected equals" )?;
        let equals = match equals_pos.token.kind {
            TokenKind::Equals => Ok( () ),
            _ => Err( SyntaxError {
                line_byte_start: name_pos.line.byte_start,
                line: name_pos.line.number,
                col: name_pos.token.col,
                len: name_pos.token.kind.len(),
                msg: "invalid assignment".into(),
                help_msg: "expected '=' after variable name".into()
            }),
        };

        tokens.next().bounded( tokens, "expected expression" )?;
        let value = self.expression( tokens );

        let name = name?;
        let _ = equals?;
        let value = value?;
        self.semicolon( tokens )?;

        return match self.resolve( &name ) {
            None => {
                self.scopes[ self.current_scope ].variables.push( Variable { mutability: kind, name: name.clone(), typ: value.typ() } );
                Ok( Node::Definition( name, value ) )
            },
            Some( _ ) => Err( SyntaxError {
                line_byte_start: name_pos.line.byte_start,
                line: name_pos.line.number,
                col: name_pos.token.col,
                len: name_pos.token.kind.len(),
                msg: "variable redefinition".into(),
                help_msg: "was previously defined".into()
            }),
        }
    }

    fn variable_reassignment_or_expression( &mut self, tokens: &mut TokenCursor ) -> Result<Node, SyntaxError> {
        let name_pos = tokens.current().unwrap();
        let operator_pos = tokens.peek_next();
        return match operator_pos {
            Some( op_pos ) => match op_pos.token.kind {
                TokenKind::Equals
                | TokenKind::Op( Operator::PowEquals )
                | TokenKind::Op( Operator::TimesEquals )
                | TokenKind::Op( Operator::DivideEquals )
                | TokenKind::Op( Operator::PlusEquals )
                | TokenKind::Op( Operator::MinusEquals ) => {
                    let name = name_pos.token.kind.to_string();
                    tokens.next();
                    tokens.next().bounded( tokens, "expected expression" )?;
                    let rhs = self.expression( tokens )?;

                    let new_value = match &op_pos.token.kind {
                        TokenKind::Equals => Ok( rhs ),
                        TokenKind::Op( op ) => {
                            let lhs = Expression::Identifier( name.clone(), op.typ() );
                            Ok( Expression::Binary { lhs: Box::new( lhs ), op: *op, rhs: Box::new( rhs ) } )
                        },
                        _ => unreachable!(),
                    };

                    self.semicolon( tokens )?;

                    return match self.resolve_mut( &name ) {
                        Some( var ) => match var.mutability {
                            Mutability::Let => Err( SyntaxError {
                                line_byte_start: name_pos.line.byte_start,
                                line: name_pos.line.number,
                                col: name_pos.token.col,
                                len: name_pos.token.kind.len(),
                                msg: "invalid assignment".into(),
                                help_msg: "was defined as immutable".into()
                            }),
                            Mutability::Var => {
                                let new_value = new_value?;
                                let new_value_typ = new_value.typ();

                                if var.typ != new_value_typ {
                                    Err( SyntaxError {
                                        line_byte_start: name_pos.line.byte_start,
                                        line: name_pos.line.number,
                                        col: name_pos.token.col,
                                        len: name_pos.token.kind.len(),
                                        msg: "mismatched types".into(),
                                        help_msg: format!(
                                            "trying to assign an expression of type '{}' to a variable of type '{}'",
                                            new_value_typ,
                                            var.typ
                                        ).into()
                                    })
                                }
                                else {
                                    Ok( Node::Assignment( name_pos.token.kind.to_string(), new_value ) )
                                }
                            },
                        },
                        None => Err( SyntaxError {
                            line_byte_start: name_pos.line.byte_start,
                            line: name_pos.line.number,
                            col: name_pos.token.col,
                            len: name_pos.token.kind.len(),
                            msg: "variable redefinition".into(),
                            help_msg: "was not previously defined in this scope".into()
                        })
                    }
                },
                _ => Ok( Node::Expression( self.expression( tokens )? ) ),
            },
            None => Ok( Node::Expression( self.expression( tokens )? ) ),
        }
    }
}

// print statements
impl AST {
    fn print( &mut self, tokens: &mut TokenCursor ) -> Result<Node, SyntaxError> {
        let print_pos = tokens.current().unwrap();
        match print_pos.token.kind {
            TokenKind::PrintLn => if let Some( TokenPosition { token: &Token { kind: TokenKind::SemiColon, .. }, .. } ) = tokens.peek_next() {
                tokens.next();
                return Ok( Node::Println( None ) );
            },
            TokenKind::Print => (),
            _ => unreachable!(),
        }

        tokens.next().bounded( tokens, "expected expression" )?;
        let argument = self.expression( tokens )?;
        self.semicolon( tokens )?;

        return match print_pos.token.kind {
            TokenKind::Print => Ok( Node::Print( argument ) ),
            TokenKind::PrintLn => Ok( Node::Println( Some( argument ) ) ),
            _ => unreachable!(),
        }
    }
}

// if statements
impl AST {
    fn if_statement( &mut self, tokens: &mut TokenCursor ) -> Result<Node, SyntaxError> {
        let iff = self.iff( tokens )?;
        let mut if_statement = IfStatement { ifs: vec![iff], els: None };
        self.els( &mut if_statement, tokens )?;

        return Ok( Node::If( if_statement ) );
    }

    fn iff( &mut self, tokens: &mut TokenCursor ) -> Result<If, SyntaxError> {
        let if_pos = tokens.current().unwrap();
        tokens.next().bounded( tokens, "expected boolean expression" )?;

        let expression = self.expression( tokens )?;
        let condition = match &expression.typ() {
            Type::Bool => Ok( expression ),
            Type::Char | Type::Int | Type::Str => Err( SyntaxError {
                line_byte_start: if_pos.line.byte_start,
                line: if_pos.line.number,
                col: if_pos.token.col,
                len: if_pos.token.kind.len(),
                msg: "expected boolean expression".into(),
                help_msg: "must be followed by a boolean expression".into()
            }),
        };

        let block_or_statement_pos = tokens.current().bounded( tokens, "expected colon or curly bracket" )?;
        return match block_or_statement_pos.token.kind {
            TokenKind::Bracket( BracketKind::OpenCurly ) => {
                let condition = condition?;
                let scope = self.parse_single( tokens )?.unwrap();
                Ok( If { condition, statement: scope } )
            },
            TokenKind::Colon => {
                let condition = condition?;
                tokens.next().bounded( tokens, "expected statement" )?;
                match self.parse_single( tokens )? {
                    Some( statement ) => Ok( If { condition, statement } ),
                    None => Err( SyntaxError {
                        line_byte_start: block_or_statement_pos.line.byte_start,
                        line: block_or_statement_pos.line.number,
                        col: block_or_statement_pos.token.col,
                        len: block_or_statement_pos.token.kind.len(),
                        msg: "invalid else statement".into(),
                        help_msg: "must be followed by a statement".into()
                    })
                }
                // since we checked for the presence of a token after this point we can safely unwrap
            },
            _ => {
                let before_curly_bracket_pos = tokens.peek_previous().unwrap();
                Err( SyntaxError {
                    line_byte_start: before_curly_bracket_pos.line.byte_start,
                    line: before_curly_bracket_pos.line.number,
                    col: before_curly_bracket_pos.token.col,
                    len: before_curly_bracket_pos.token.kind.len(),
                    msg: "expected block scope".into(),
                    help_msg: "must be followed by an opened curly bracket or a colon".into()
                })
            },
        }
    }

    fn els( &mut self, if_statement: &mut IfStatement, tokens: &mut TokenCursor ) -> Result<(), SyntaxError> {
        let els_pos = match tokens.current() {
            Some( pos ) => pos,
            None => return Ok( () )
        };

        return if let TokenKind::Else = els_pos.token.kind {
            let if_or_block_pos = tokens.next().bounded( tokens, "expected colon, block or if statement" )?;
            match if_or_block_pos.token.kind {
                TokenKind::Bracket( BracketKind::OpenCurly ) => {
                    let scope = self.parse_single( tokens )?.unwrap();
                    if_statement.els = Some( Box::new( scope ) );
                    Ok( () )
                },
                TokenKind::Colon => {
                    tokens.next().bounded( tokens, "expected statement" )?;
                    match self.parse_single( tokens )? {
                        Some( statement ) => {
                            if_statement.els = Some( Box::new( statement ) );
                            Ok( () )
                        },
                        None => Err( SyntaxError {
                            line_byte_start: if_or_block_pos.line.byte_start,
                            line: if_or_block_pos.line.number,
                            col: if_or_block_pos.token.col,
                            len: if_or_block_pos.token.kind.len(),
                            msg: "invalid else statement".into(),
                            help_msg: "must be followed by a statement".into()
                        })
                    }
                },
                TokenKind::If => {
                    let else_if = self.iff( tokens )?;
                    if_statement.ifs.push( else_if );

                    self.els( if_statement, tokens )
                },
                _ => Err( SyntaxError {
                    line_byte_start: els_pos.line.byte_start,
                    line: els_pos.line.number,
                    col: els_pos.token.col,
                    len: els_pos.token.kind.len(),
                    msg: "invalid else statement".into(),
                    help_msg: "expected block of if statement after this token".into()
                }),
            }
        }
        else {
            Ok( () )
        }
    }
}

// for statements
impl AST {
    fn for_statement( &mut self, tokens: &mut TokenCursor ) -> Result<Node, SyntaxError> {
        self.for_depth += 1;
        let iff = self.iff( tokens );
        self.for_depth -= 1;

        let iff = iff?;
        return Ok( Node::For( ForStatement {
            pre: None,
            condition: Some( iff.condition ),
            post: None,
            statement: Box::new( iff.statement )
        } ) );
    }
}


struct Checker;

impl Checker {
    fn check( src: BlitzSrc, start_time: &Instant ) -> Result<AST, SyntaxErrors> {
        print!( "{}: {}", CHECKING, src.path.display() );
        let mut time_info = String::new();

        let lexer = Lexer::try_from( src );
        let lexing_time = start_time.elapsed();
        let elapsed_lexing = Colored {
            text: format!( "{}s", lexing_time.as_secs_f64() ),
            foreground: Foreground::LightGray,
            ..Default::default()
        };
        time_info.push_str( &format!( "lexing: {}", elapsed_lexing ) );

        let lexer = match lexer {
            Ok( lexer ) => lexer,
            Err( err ) => {
                println!( " ... in [{}]", time_info );
                return Err( err );
            },
        };

        let ast = AST::try_from( lexer );
        let ast_time = start_time.elapsed() - lexing_time;
        let elapsed_ast = Colored {
            text: format!( "{}s", ast_time.as_secs_f64() ),
            foreground: Foreground::LightGray,
            ..Default::default()
        };
        time_info.push_str( &format!( ", parsing: {}", elapsed_ast ) );

        let elapsed_total = Colored {
            text: format!( "{}s", start_time.elapsed().as_secs_f64() ),
            foreground: Foreground::LightGray,
            ..Default::default()
        };

        println!( " ... in {} [{}]", elapsed_total, time_info );
        return ast;
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
    source_file_path: PathBuf,
    #[allow(dead_code)]
    output_folder_path: PathBuf,

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
    const STACK_ALIGN: usize = core::mem::size_of::<usize>();


    fn compile( &mut self, start_time: &Instant ) -> std::io::Result<PathBuf> {
        print!( "{}: {}", COMPILING, self.source_file_path.display() );
        let mut time_info = String::new();

        let asm_file_path = self.source_file_path.with_extension( "asm" );
        let mut asm_file = BufWriter::new( File::create( &asm_file_path ).unwrap() );

        self.rodata +=
r#" stdout: equ 1
 SYS_write: equ 1
 SYS_exit: equ 60
 EXIT_SUCCESS: equ 0

 INT_MIN: equ 1 << 63
 INT_MAX: equ ~INT_MIN
 INT_BITS: equ 64

 true: equ 1
 true_str: db "true"
 true_str_len: equ $ - true_str

 false: equ 0
 false_str: db "false"
 false_str_len: equ $ - false_str

 LESS: equ -1
 EQUAL: equ 0
 GREATER: equ 1

 INT_STR_LEN: equ INT_BITS"#;

        let mut stack_size = 0;
        let mut variables: Vec<(Type, Vec<&Variable>)> = Vec::new();
        for scope in &self.ast.scopes {
            for definition in &scope.variables {
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
                    variables.push( (typ, vec![definition]) );
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
 int_str: times INT_STR_LEN db 0

section .text
int_toStr:
 push rcx

 mov rsi, 10
 mov rcx, int_str + INT_STR_LEN - 1

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
 mov rdx, int_str + INT_STR_LEN
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

        let asm_generation_time = start_time.elapsed();
        let elapsed_asm_generation = Colored {
            text: format!( "{}s", asm_generation_time.as_secs_f64() ),
            foreground: Foreground::LightGray,
            ..Default::default()
        };
        time_info.push_str( &format!( "asm generation: {}", elapsed_asm_generation ) );

        match Command::new( "nasm" ).args( ["-felf64", "-gdwarf", asm_file_path.to_str().unwrap()] ).output() {
            Ok( nasm_out ) => if !nasm_out.status.success() {
                println!( " ... in [{}]", time_info );
                return Err( std::io::Error::new(
                    ErrorKind::InvalidData,
                    format!( "{}: nasm assembler failed\n{}: {}", ERROR, CAUSE, String::from_utf8_lossy( &nasm_out.stderr ) )
                ) );
            },
            Err( err ) => {
                println!( " ... in [{}]", time_info );
                return Err( std::io::Error::new(
                    err.kind(),
                    format!( "{}: could not create nasm assembler process\n{}: {}", ERROR, CAUSE, err )
                ) );
            },
        }

        let nasm_time = start_time.elapsed() - asm_generation_time;
        let elapsed_nasm = Colored {
            text: format!( "{}s", nasm_time.as_secs_f64() ),
            foreground: Foreground::LightGray,
            ..Default::default()
        };
        time_info.push_str( &format!( ", assembler: {}", elapsed_nasm ) );

        let obj_file_path = self.source_file_path.with_extension( "o" );
        let executable_file_path = self.source_file_path.with_extension( "" );

        match Command::new( "ld" ).args( [obj_file_path.to_str().unwrap(), "-o", executable_file_path.to_str().unwrap()] ).output() {
            Ok( ld_out ) => if !ld_out.status.success() {
                println!( " ... in [{}]", time_info );
                return Err( std::io::Error::new(
                    ErrorKind::InvalidData,
                    format!( "{}: ld linker failed\n{}: {}", ERROR, CAUSE, String::from_utf8_lossy( &ld_out.stderr ) )
                ) );
            },
            Err( err ) => {
                println!( " ... in [{}]", time_info );
                return Err( std::io::Error::new(
                    err.kind(),
                    format!( "{}: could not create ld linker process\n{}: {}", ERROR, CAUSE, err )
                ) );
            },
        };

        let ld_time = start_time.elapsed() - nasm_time;
        let elapsed_ld = Colored {
            text: format!( "{}s", ld_time.as_secs_f64() ),
            foreground: Foreground::LightGray,
            ..Default::default()
        };
        time_info.push_str( &format!( ", linker: {}", elapsed_ld ) );

        let elapsed_total = Colored {
            text: format!( "{}s", start_time.elapsed().as_secs_f64() ),
            foreground: Foreground::LightGray,
            ..Default::default()
        };

        println!( " ... in {} [{}]", elapsed_total, time_info );

        return Ok( executable_file_path );
    }

    fn run( &mut self, executable_file_path: &Path ) -> std::io::Result<()> {
        println!( "{}: {}", RUNNING, executable_file_path.display() );

        return match Command::new( executable_file_path.display().to_string() ).spawn() {
            Ok( mut blitz ) => match blitz.wait() {
                Ok( _status ) => Ok( () ),
                Err( err ) => Err( std::io::Error::new(
                    err.kind(),
                    format!( "{}: could not run blitz executable", ERROR )
                ) ),
            },
            Err( err ) => Err( std::io::Error::new(
                err.kind(),
                format!( "{}: could not create blitz executable process\n{}: {}", ERROR, CAUSE, err )
            ) ),
        }
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
    fn compile_node( &mut self, node: &'ast Node ) {
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
            Node::Println( argument ) => {
                self.asm += &format!( " ; {}\n", node );
                if let Some( arg ) = argument {
                    self.compile_expression( arg );

                    match arg.typ() {
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
                }

                self.asm +=
                    " push 10\
                    \n mov rdi, stdout\
                    \n mov rsi, rsp\
                    \n mov rdx, 1\
                    \n mov rax, SYS_write\
                    \n syscall\
                    \n pop rsi\n\n"
            },
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
                if let Some( els ) = &if_statement.els {
                    self.asm += &format!( "if_{}_else:\n", if_idx );
                    match &**els {
                        Node::Scope( scope ) => self.compile_scope( *scope ),
                        other => self.compile_node( other ),
                    }
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

    fn compile_scope( &mut self, scope_idx: usize ) {
        let scope = &self.ast.scopes[ scope_idx ];
        for node in &scope.nodes {
            self.compile_node( node );
        }
    }


    fn compile_expression( &mut self, expression: &'ast Expression ) {
        match expression {
            Expression::Literal( Literal::Int( value ) ) => self.asm += &format!( " mov rdi, {}\n", value ),
            Expression::Literal( Literal::Char( code ) ) => self.asm += &format!( " mov rdi, {}\n", code ),
            Expression::Literal( Literal::Bool( value ) ) => self.asm += &format!( " mov rdi, {}\n", value ),
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
            Expression::Literal( Literal::Int( value ) ) => self.asm += &format!( " mov {}, {}\n", dst, value ),
            Expression::Literal( Literal::Char( code ) ) => self.asm += &format!( " mov {}, {}\n", dst, code ),
            Expression::Literal( Literal::Bool( value ) ) => self.asm += &format!( " mov {}, {}\n", dst, value ),
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
                        Expression::Literal( Literal::Int( 2 ) ) => (Register::RDI, Register::RSI,
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
            Expression::Literal( Literal::Bool( value ) ) =>
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

        self.compile_node( &iff.statement );
    }

    fn compile_for( &mut self, forr: &'ast ForStatement, for_tag: &String, for_false_tag: &String ) {
        if let Some( condition ) = &forr.condition {
            self.asm += &format!( "{}:; {}\n", for_tag, condition );

            match condition {
                Expression::Literal( Literal::Bool( value ) ) =>
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

        self.compile_node( &forr.statement );
    }


    fn compile_assignment( &mut self, name: &String, new_value: &'ast Expression ) {
        let variable = self.resolve( name );
        let variable_typ = variable.typ;
        let variable_offset = variable.offset;
        self.asm += &format!( " ; {} = {}\n", name, new_value );

        match new_value {
            Expression::Literal( Literal::Int( value ) ) =>
                self.asm += &format!(
                    " mov rdi, {}\
                    \n mov [rbp + {}], rdi\n\n",
                    value, variable_offset
                ),
            Expression::Literal( Literal::Char( code ) ) =>
                self.asm += &format!( " mov byte [rbp + {}], {}\n\n", variable_offset, code ),
            Expression::Literal( Literal::Bool( value ) ) =>
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


// IDEA make the source generic, eg: to be able to compile from strings instead of just files
#[derive( Debug )]
struct BlitzSrc {
    path: PathBuf,
    src: BufReader<File>,
}

impl BlitzSrc {
    fn try_from( path: &Path ) -> std::io::Result<Self> {
        return match File::open( path ) {
            Ok( file ) => match file.metadata() {
                Ok( metadata ) => match metadata.is_file() {
                    true => Ok( Self { path: PathBuf::from( path ), src: BufReader::with_capacity( 1, file ) } ),
                    false => Err( std::io::Error::new(
                        ErrorKind::InvalidInput,
                        format!( "{}: invalid path '{}'\n{}: expected a file but go a directory", ERROR, path.display(), CAUSE )
                    ) ),
                },
                Err( err ) => Err( std::io::Error::new(
                    err.kind(),
                    format!( "{}: could not read metadata of '{}'\n{}: {}", ERROR, path.display(), CAUSE, err )
                ) ),
            },
            Err( err ) => Err( std::io::Error::new(
                err.kind(),
                format!( "{}: could not open '{}'\n{}: {}", ERROR, path.display(), CAUSE, err )
            ) ),
        }
    }
}


enum RunModeKind {
    Check,
    Compile { output_path: PathBuf, run: bool },
}

struct RunMode {
    src_path: PathBuf,
    kind: RunModeKind,
}

struct Blitz;

impl Blitz {
    fn print_usage() {
        Self::print_version();

        println!( r"
Usage: blitz [{OPTIONS}] [{RUN_MODE}]

{OPTIONS}:
    -h, --help            Display this message
    -v, --version         Display the compile version
    -c, --color <{MODE}>    Wether to display colored output ({MODE}: auto (default), never, always)

{RUN_MODE}:
    check   <file.blz>    Check the source code for correctness
    compile <file.blz>    Compile the source code down to a binary executable
    run     <file.blz>    Compile and run the generated binary executable
"
        );
    }

    fn print_version() {
        println!( "\nBlitzlang compiler, version {}", VERSION );
    }

    fn from( args: Args ) -> ExitCode {
        color::auto();

        #[allow(unused_mut)]
        let mut args = args.collect::<Vec<String>>();
        // to quickly debug
        // args.push( "-o".to_string() );
        // args.push( "examples/out".to_string() );

        if args.len() < 2 {
            Self::print_usage();
            return ExitCode::SUCCESS;
        }

        let mut run_mode: Option<RunMode> = None;

        let mut args_iter = args.into_iter().peekable();
        args_iter.next(); // skipping the name of this executable

        while let Some( arg ) = args_iter.next() {
            match arg.as_str() {
                "-h" | "--help" => {
                    Self::print_usage();
                    return ExitCode::SUCCESS;
                },
                "-v" | "--version" => {
                    Self::print_version();
                    return ExitCode::SUCCESS;
                },
                "-c" | "--color" => match args_iter.next() {
                    Some( mode ) => match mode.as_str() {
                        "auto" => color::auto(),
                        "always" => color::always(),
                        "never" => color::never(),
                        _ => {
                            eprintln!( "{}: unrecognized color mode", ERROR );
                            return ExitCode::FAILURE;
                        },
                    },
                    None => {
                        eprintln!( "{}: expected color mode", ERROR );
                        return ExitCode::FAILURE;
                    },
                },
                "check" => match args_iter.next() {
                    Some( src_path ) => match run_mode {
                        None => run_mode = Some( RunMode { src_path: src_path.into(), kind: RunModeKind::Check } ),
                        Some( _ ) => {
                            eprintln!( "{}: run mode already selected", ERROR );
                            return ExitCode::FAILURE;
                        },
                    },
                    None => {
                        eprintln!( "{}: missing source file path for 'check' mode", ERROR );
                        return ExitCode::FAILURE;
                    },
                },
                "compile" | "run" => match args_iter.next() {
                    Some( src_path ) => match run_mode {
                        None => {
                            let src_path: PathBuf = src_path.into();
                            let output_path: PathBuf = ".".into();

                            run_mode = match arg.as_str() {
                                "compile" => Some( RunMode { src_path, kind: RunModeKind::Compile { output_path, run: false } } ),
                                "run" => Some( RunMode { src_path, kind: RunModeKind::Compile { output_path, run: true } } ),
                                _ => unreachable!(),
                            };
                        },
                        Some( _ ) => {
                            eprintln!( "{}: run mode already selected", ERROR );
                            return ExitCode::FAILURE;
                        },
                    },
                    None => {
                        eprintln!( "{}: missing source file path for '{}' mode", ERROR, arg );
                        return ExitCode::FAILURE;
                    },
                },
                _ => {
                    eprintln!( "{}: unrecognized option", ERROR );
                    return ExitCode::FAILURE;
                }
            }
        }

        let run_mode = match run_mode {
            Some( mode ) => mode,
            // None => {
            //     eprintln!( "{}: at least one run mode needs to be specified", ERROR );
            //     return ExitCode::FAILURE;
            // },
            None => {
                Self::print_usage();
                return ExitCode::SUCCESS;
            },
        };

        let source_file = match BlitzSrc::try_from( Path::new( &run_mode.src_path ) ) {
            Ok( src ) => src,
            Err( err ) => {
                eprintln!( "{}", err );
                return ExitCode::FAILURE;
            },
        };

        let start_time = Instant::now();
        let ast = match Checker::check( source_file, &start_time ) {
            Ok( ast ) => {
                ast
            }
            Err( mut errors ) => {
                let mut line_byte_start = errors.errors[ 0 ].line_byte_start;
                let mut line_text = String::new();
                let _ = errors.src.src.seek( SeekFrom::Start( line_byte_start as u64 ) );
                let _ = errors.src.src.read_line( &mut line_text );

                for error in &errors.errors {
                    if line_byte_start != error.line_byte_start {
                        line_byte_start = error.line_byte_start;
                        line_text.clear();
                        let _ = errors.src.src.seek( SeekFrom::Start( line_byte_start as u64 ) );
                        let _ = errors.src.src.read_line( &mut line_text );
                    }

                    let error_msg = Colored {
                        text: error.msg.to_string(),
                        foreground: Foreground::White,
                        flags: Flags::Bold,
                        ..Default::default()
                    };

                    let line_number_and_bar = Colored {
                        text: format!( "{} |", error.line ),
                        foreground: Foreground::LightBlue,
                        ..Default::default()
                    };

                    let visualization_padding = line_number_and_bar.text.len();
                    let at_padding = visualization_padding - 1;

                    let at = Colored {
                        text: format!( "{:>at_padding$}", "at" ),
                        foreground: Foreground::LightRed,
                        flags: Flags::Bold,
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
                        ERROR, error_msg,
                        at, errors.src.path.display(), error.line, error.col,
                        bar,
                        line_number_and_bar, line_text.trim_end(),
                        bar, "", pointers_and_help_msg
                    );
                }

                return ExitCode::FAILURE;
            }
        };

        match &run_mode.kind {
            RunModeKind::Check => (/* pass */),
            RunModeKind::Compile { output_path, run } => {
                let mut compiler = Compiler {
                    source_file_path: run_mode.src_path.into(),
                    output_folder_path: output_path.into(),
                    ast: &ast,
                    rodata: String::new(),
                    asm: String::new(),
                    variables: Vec::new(),
                    strings: Vec::new(),
                    if_idx: 0,
                    for_idx: 0,
                    for_idx_stack: Vec::new(),
                };

                let executable_path = match compiler.compile( &start_time ) {
                    Ok( path ) => path,
                    Err( err ) => {
                        eprintln!( "{}", err );
                        return ExitCode::FAILURE;
                    },
                };

                let done_time = start_time.elapsed();
                let elapsed_done = Colored {
                    text: format!( "{}s", done_time.as_secs_f64() ),
                    foreground: Foreground::LightGray,
                    ..Default::default()
                };
                println!( "{}: ... in {}", DONE, elapsed_done );

                if *run {
                    match compiler.run( &executable_path ) {
                        Ok( _ ) => (),
                        Err( err ) => {
                            eprintln!( "{}", err );
                            return ExitCode::FAILURE;
                        },
                    }
                }
            }
        }

        return ExitCode::SUCCESS;
    }
}

// IDEA adapt SyntaxErrors to report cli mistakes
// TODO add option to specify output path
fn main() -> ExitCode {
    return Blitz::from( env::args() );
}
