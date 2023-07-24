// TODO process entire statement for syntactical correctness and then report all the errors
    // IDEA read the src file line when printing errors instead of composing it from the tokens
        // IDEA have Token contain the absolute column in the source file, and when encountering errors read the corresponding line
// TODO fix division by zero
// TODO implement boolean operators for strings
use std::{io::{BufReader, BufRead, ErrorKind, BufWriter, Write}, fs::File, env, process::{ExitCode, Command}, fmt::Display, path::{Path, PathBuf}, iter::Peekable, str::Chars, borrow::Cow};

mod color;
use crate::color::*;


trait Len {
    fn len( &self ) -> usize;
}


#[derive( Debug, Clone )]
enum Type {
    I64 { value: i64 },
    Char { value: u8 }, // only supporting ASCII characters for now
    Bool { value: bool },
    Str { text: Vec<u8> },
}

impl Display for Type {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::I64 { value } => write!( f, "{}", value ),
            Self::Char { value } => write!( f, "'{}'", value.escape_ascii() ), // TODO create own escaping function
            Self::Bool { value } => write!( f, "{}", value ),
            Self::Str { text } => {
                write!( f, "\"" )?;
                for character in text {
                    write!( f, "{}", character.escape_ascii() )?;
                }
                write!( f, "\"" )?;

                Ok( () )
            },
        }
    }
}

impl Into<i64> for Type {
    fn into( self ) -> i64 {
        return match self {
            Self::I64 { value } => value.into(),
            Self::Char { value } => value.into(),
            Self::Bool { value } => value.into(),
            Self::Str { text } => text.len() as i64,
        }
    }
}

impl Len for Type {
    fn len( &self ) -> usize {
        return match self {
            Self::I64 { value } => value.to_string().len(),
            Self::Char { value: _ } => 1 as usize,
            Self::Bool { value } => value.to_string().len(),
            Self::Str { text } => text.len(),
        }
    }
}

impl Type {
    fn display( &self ) {
        match self {
            Self::I64 { value } => print!( "{}", value ),
            Self::Char { value } => print!( "{}", *value as char ),
            Self::Bool { value } => print!( "{}", value ),
            Self::Str { text } => for character in text {
                print!( "{}", *character as char );
            },
        }
    }
}


#[derive( Debug, Clone, Copy, PartialEq )]
enum OpKind {
    Pow,
    Times,
    Divide,
    Plus,
    Minus,

    Equals,
    NotEquals,
    Greater,
    GreaterOrEquals,
    Less,
    LessOrEquals,

    Compare,
}

impl Display for OpKind {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Pow => write!( f, "^" ),
            Self::Times => write!( f, "*" ),
            Self::Divide => write!( f, "/" ),
            Self::Plus => write!( f, "+" ),
            Self::Minus => write!( f, "-" ),

            Self::Equals => write!( f, "==" ),
            Self::NotEquals => write!( f, "!=" ),
            Self::Greater => write!( f, ">" ),
            Self::GreaterOrEquals => write!( f, ">=" ),
            Self::Less => write!( f, "<" ),
            Self::LessOrEquals => write!( f, "<=" ),

            Self::Compare => write!( f, "<=>" ),
        }
    }
}

impl Len for OpKind {
    fn len( &self ) -> usize {
        return match self {
            Self::Pow => 1,
            Self::Times => 1,
            Self::Divide => 1,
            Self::Plus => 1,
            Self::Minus => 1,
            Self::Equals => 2,
            Self::NotEquals => 2,
            Self::Greater => 1,
            Self::GreaterOrEquals => 2,
            Self::Less => 1,
            Self::LessOrEquals => 2,
            Self::Compare => 3,
        }
    }
}


#[derive( Debug, Clone, Copy )]
enum DefinitionKind {
    // Fn,
    Const,
    Let,
    Var,
}

impl Display for DefinitionKind {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Let => write!( f, "let" ),
            Self::Const => write!( f, "const" ),
            Self::Var => write!( f, "var" ),
        }
    }
}

impl Len for DefinitionKind {
    fn len( &self ) -> usize {
        return match self {
            Self::Const => 5,
            Self::Let => 3,
            Self::Var => 3,
        }
    }
}


#[derive( Debug, Clone, Copy )]
enum BracketKind {
    OpenRound,
    CloseRound,
    // OpenSquare,
    // CloseSquare,
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
    // Whitespace
    Comment( String ), // is used as a placeholder when encountering errors during lexing

    // Symbols
    Bracket( BracketKind ),
    Equals,
    // Colon,
    SemiColon,

    // Identifiers
    Literal( Type ),
    Identifier( String ),
    Definition( DefinitionKind ),

    // Operators
    Op( OpKind ),

    // Keywords
    Print, // temporary way of printing values
    PrintLn, // temporary way of printing values followed by a newline
    True,
    False,
    If,
    // Entry,
    // Return,

    // Special
    Empty,
    SOF, // start of file
    EOF, // end of file
}

impl Display for TokenKind {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Comment( text ) => write!( f, "{}", text ),

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

            Self::Empty | Self::SOF | Self::EOF => write!( f, "" ),
        }
    }
}

impl Len for TokenKind {
    fn len( &self ) -> usize {
        return match self {
            Self::Comment( text ) => text.len(),

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

            Self::Empty => 1,
            Self::SOF => 1,
            Self::EOF => 1,
        };
    }
}


#[derive( Debug, Clone )]
struct Token {
    // line: usize,
    col: usize,
    // src_col: usize,
    kind: TokenKind,
}

#[derive( Debug, Clone )]
struct Line {
    number: usize,
    tokens: Vec<Token>,
}

impl Display for Line {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        if self.tokens.is_empty() {
            return writeln!( f );
        }

        let mut tokens = self.tokens.iter().peekable();
        let leading_whitespace = match self.tokens[ 0 ].kind {
            TokenKind::SOF => {
                tokens.next(); // skipping the SOF
                self.tokens[ 1 ].col - 1
            },
            _ => self.tokens[ 0 ].col - 1
        };

        write!( f, "{:leading_whitespace$}", "" )?;

        while let Some( token ) = tokens.next() {
            let spaces_before_next_token = match tokens.peek() {
                Some( next_token ) => next_token.col - (token.col + token.kind.len()),
                None => 0,
            };

            write!( f, "{}{:spaces_before_next_token$}", token.kind, "" )?;
        }

        return Ok( () );
    }
}


// TODO implement NOTE, HINT, HELP in error messages
#[derive( Debug, Clone )]
struct SyntaxError {
    err_line: usize,
    col: usize,
    text: String,
    msg: &'static str,
    help_msg: &'static str,
}

#[derive( Debug )]
struct SyntaxErrors<'this> {
    file_path: String,
    lines: Vec<Cow<'this, Line>>,
    errors: Vec<SyntaxError>,
}

impl Display for SyntaxErrors<'_> {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        let mut line_number = self.errors[ 0 ].err_line;
        let mut line = &self.lines[ line_number ];
        let mut line_text = line.to_string();

        for error in &self.errors {
            if error.err_line != line_number {
                line_number = error.err_line;
                line = &self.lines[ line_number ];
                line_text = line.to_string();
            }

            let error_msg = Colored {
                text: error.msg.to_string(),
                foreground: Foreground::White,
                bold: true,
                ..Default::default()
            };

            let mut line_number_and_bar = Colored {
                text: format!( "{} |", line.number ),
                foreground: Foreground::LightBlue,
                ..Default::default()
            };

            let visualization_padding = line_number_and_bar.text.len();
            let location_padding = visualization_padding - 1;

            line_number_and_bar.text = format!( "{:>visualization_padding$}", line_number_and_bar );
            line_number_and_bar.foreground = Foreground::LightBlue;

            let in_ = Colored {
                text: format!( "{:>location_padding$}", "in" ),
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
            let pointers_len = error.text.len();

            let help_msg = Colored {
                text: format!( "{:^>pointers_len$} {}", "", error.help_msg ),
                foreground: Foreground::LightRed,
                ..Default::default()
            };

            writeln!( f,
                "{}: {}\
                \n{}: {}:{}:{}\
                \n{}\
                \n{} {}\
                \n{} {:pointers_col$}{}\n",
                SyntaxError::ERROR, error_msg,
                in_, self.file_path, line.number, error.col,
                bar,
                line_number_and_bar, line_text,
                bar, "", help_msg
            )?;
        }

        return Ok( () );
    }
}

impl SyntaxError {
    const ERROR: &'static str = colored!{
        text: "Error",
        foreground: Foreground::LightRed,
        bold: true,
    };
}


#[derive( Debug )]
struct Lexer {
    file_path: String,
    lines: Vec<Line>,
}

impl<'program> TryFrom<(&'program str, File)> for Lexer {
    type Error = SyntaxErrors<'program>;

    // IDEA make the input character stream generic, eg: to be able to compile from strings instead of just files
    // TODO make an character iterator similar to LexerIter
    fn try_from( src: (&str, File) ) -> Result<Self, Self::Error> {
        let (file_path, src_file) = (src.0, src.1);

        let mut lines: Vec<Line> = Vec::new();
        let mut tokens: Vec<Token> = vec![ Token { col: 1, kind: TokenKind::SOF } ];

        let mut err_lines: Vec<Cow<Line>> = Vec::new();
        let mut errors: Vec<SyntaxError> = Vec::new();

        let mut src_lines = BufReader::new( src_file );
        let mut src_line_text = String::new();
        let mut token_text = String::new();

        while let Ok( chars_read ) = src_lines.read_line( &mut src_line_text ) {
            src_line_text = src_line_text.trim_end().to_string();
            let trimmed_line_len = src_line_text.len();

            let mut line_contains_errors = false;
            let mut col = 1;

            let mut src = src_line_text.chars().peekable();
            // IDEA try extracting this to a separate function
            loop {
                let token_info: Result<TokenKind, SyntaxError> = match Self::next( &mut src ) {
                    Ok( None ) => break,
                    Err( err ) => Err( err ),
                    Ok( Some( ch ) ) => match ch {
                        // TODO consume until not whitespace
                        // ignore whitespace
                        _ if ch.is_ascii_whitespace() => {
                            col += 1;
                            continue;
                        },
                        '(' => Ok( TokenKind::Bracket( BracketKind::OpenRound ) ),
                        ')' => Ok( TokenKind::Bracket( BracketKind::CloseRound ) ),
                        '{' => Ok( TokenKind::Bracket( BracketKind::OpenCurly ) ),
                        '}' => Ok( TokenKind::Bracket( BracketKind::CloseCurly ) ),
                        '=' => match Self::peek_next( &mut src ) {
                            Ok( Some( '=' ) ) => {
                                let _ = Self::next( &mut src );
                                Ok( TokenKind::Op( OpKind::Equals ) )
                            },
                            _ => Ok( TokenKind::Equals ),
                        },
                        '!' => match Self::peek_next( &mut src ) {
                            Ok( Some( '=' ) ) => {
                                let _ = Self::next( &mut src );

                                Ok( TokenKind::Op( OpKind::NotEquals ) )
                            },
                            _ => Err( SyntaxError {
                                err_line: err_lines.len(),
                                col,
                                text: ch.to_string(),
                                msg: "unexpected character",
                                help_msg: "unrecognized"
                            } ),
                        },
                        '>' => match Self::peek_next( &mut src ) {
                            Ok( Some( '=' ) ) => {
                                let _ = Self::next( &mut src );

                                Ok( TokenKind::Op( OpKind::GreaterOrEquals ) )
                            },
                            _ => Ok( TokenKind::Op( OpKind::Greater ) ),
                        },
                        '<' => match Self::peek_next( &mut src ) {
                            Ok( Some( '=' ) ) => {
                                let _ = Self::next( &mut src );

                                match Self::peek_next( &mut src ) {
                                    Ok( Some( '>' ) ) => {
                                        let _ = Self::next( &mut src );

                                        Ok( TokenKind::Op( OpKind::Compare ) )
                                    },
                                    _ => {
                                        Ok( TokenKind::Op( OpKind::LessOrEquals ) )
                                    }
                                }
                            },
                            _ => Ok( TokenKind::Op( OpKind::Less ) ),
                        },
                        '^' => Ok( TokenKind::Op( OpKind::Pow ) ),
                        '*' => Ok( TokenKind::Op( OpKind::Times ) ),
                        '/' => Ok( TokenKind::Op( OpKind::Divide ) ),
                        '+' => Ok( TokenKind::Op( OpKind::Plus ) ),
                        '-' => Ok( TokenKind::Op( OpKind::Minus ) ),
                        ';' => Ok( TokenKind::SemiColon ),
                        '\'' => {
                            token_text.push( ch );

                            match Self::parse_char( &mut src, &mut token_text ) {
                                Ok( b'\'' ) if token_text.len() == 2 => Err( SyntaxError {
                                    err_line: err_lines.len(),
                                    col,
                                    text: token_text.clone(),
                                    msg: "empty character literal",
                                    help_msg: "must not be empty"
                                } ),
                                Ok( value ) => match Self::next_char( &mut src, &mut token_text ) {
                                    Ok( next ) => match next {
                                        b'\'' => Ok( TokenKind::Literal( Type::Char { value } ) ),
                                        _ => Err( SyntaxError {
                                            err_line: err_lines.len(),
                                            col,
                                            text: token_text.clone(),
                                            msg: "unclosed character literal",
                                            help_msg: "missing closing single quote"
                                        } ),
                                    },
                                    Err( err ) => Err( err ),
                                },
                                Err( err ) => Err( err ),
                            }
                        },
                        '"' => match Self::parse_str( &mut src ) {
                            Ok( text ) => Ok( TokenKind::Literal( Type::Str { text } ) ),
                            Err( err ) => Err( err ),
                        },
                        '#' => {
                            token_text.push( ch );

                            // consume the rest of the tokens in the current line
                            while let Some( next ) = src.next_if( |c| *c != '\n' ) {
                                token_text.push( next );
                            }

                            Ok( TokenKind::Comment( token_text.clone() ) )
                        },
                        '0'..='9' => { // TODO handle negative numbers
                            token_text.push( ch );

                            let mut is_digit = true;
                            while let Some( next ) = src.next_if( |c| matches!( c, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' ) ) {
                                if !next.is_ascii_digit() {
                                    is_digit = false;
                                }

                                token_text.push( next );
                            }

                            if is_digit {
                                match token_text.parse() { // TODO create own number parsing function
                                    Ok( value ) => Ok( TokenKind::Literal( Type::I64 { value } ) ),
                                    Err( _ ) => Err( SyntaxError {
                                        err_line: err_lines.len(),
                                        col,
                                        text: token_text.clone(),
                                        msg: "expected number literal",
                                        help_msg: "overflows a 64 bit integer [-9223372036854775808, 9223372036854775807]"
                                    } ),
                                }
                            }
                            else {
                                Err( SyntaxError {
                                    err_line: err_lines.len(),
                                    col,
                                    text: token_text.clone(),
                                    msg: "expected number literal",
                                    help_msg: "not a number literal"
                                } )
                            }
                        },
                        'a'..='z' | 'A'..='Z' | '_'  => {
                            token_text.push( ch );

                            while let Some( next ) = src.next_if( |c| matches!( c, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' ) ) {
                                token_text.push( next );
                            }

                            let kind = match token_text.as_str() {
                                // "entry" => TokenKind::Entry,
                                // "fn" => TokenKind::Fn,
                                "const" => TokenKind::Definition( DefinitionKind::Const ),
                                "let" => TokenKind::Definition( DefinitionKind::Let ),
                                "var" => TokenKind::Definition( DefinitionKind::Var ),
                                "print" => TokenKind::Print,
                                "println" => TokenKind::PrintLn,
                                "true" => TokenKind::True,
                                "false" => TokenKind::False,
                                "if" => TokenKind::If,
                                // "return" => TokenKind::Return,
                                _ => TokenKind::Identifier( token_text.clone() )
                            };

                            Ok( kind )
                        },
                        _ => Err( SyntaxError {
                            err_line: err_lines.len(),
                            col,
                            text: ch.to_string(),
                            msg: "unexpected character",
                            help_msg: "unrecognized"
                        } ),
                    },
                };

                let len = match token_info {
                    Ok( kind ) => {
                        let len = kind.len();
                        tokens.push( Token { col, kind } );
                        len
                    },
                    Err( mut err ) => {
                        line_contains_errors = true;

                        err.err_line = err_lines.len();
                        err.col = col;
                        let len = err.text.len();

                        let place_holder = TokenKind::Comment( err.text.clone() );
                        tokens.push( Token { col, kind: place_holder } );

                        errors.push( err );

                        len
                    }
                };

                token_text.clear();
                col += len;
            }

            if trimmed_line_len == 0 {
                tokens.push( Token { col, kind: TokenKind::Empty } );
            }

            let reached_eof = trimmed_line_len == chars_read;
            if reached_eof {
                tokens.push( Token { col, kind: TokenKind::EOF } );
            }

            let line = Line { number: lines.len() + 1, tokens };
            if line_contains_errors {
                err_lines.push( Cow::Owned( line ) );
            }
            else {
                lines.push( line );
            }

            if reached_eof {
                break;
            }

            src_line_text.clear();
            tokens = Vec::new();
        }

        return if errors.is_empty() {
            Ok( Self { file_path: file_path.to_string(), lines } )
        }
        else {
            Err( SyntaxErrors { file_path: file_path.to_string(), lines: err_lines, errors } )
        }
    }
}

impl Lexer {
    fn iter<'lexer>( &'lexer self ) -> LexerIter<'lexer> {
        LexerIter{ lexer: self, line: 0, token: 0 }
    }

    // FIX properly handle non ASCII codes in error messages, or simply implement UTF-8 support
    fn next( src: &mut Peekable<Chars> ) -> Result<Option<char>, SyntaxError> {
        return match src.next() {
            Some( next @ ..='\x7F' ) => Ok( Some( next ) ),
            Some( next ) => Err( SyntaxError {
                err_line: 0,
                col: 0,
                text: next.to_string(),
                msg: "unrecognized character",
                help_msg: "not a valid ASCII character"
            } ),
            None => Ok( None ),
        }
    }

    fn peek_next<'src>( src: &'src mut Peekable<Chars> ) -> Result<Option<&'src char>, SyntaxError> {
        return match src.peek() {
            Some( next @ ..='\x7F' ) => Ok( Some( next ) ),
            Some( next ) => Err( SyntaxError {
                err_line: 0,
                col: 0,
                text: next.to_string(),
                msg: "unrecognized character",
                help_msg: "not a valid ASCII character"
            } ),
            None => Ok( None ),
        }
    }

    fn next_char( src: &mut Peekable<Chars>, token_text: &mut String ) -> Result<u8, SyntaxError> {
        return match Self::next( src )? {
            Some( '\n' ) | None => Err( SyntaxError {
                err_line: 0,
                col: 0,
                text: token_text.clone(),
                msg: "invalid character literal",
                help_msg: "missing closing single quote"
            } ),
            Some( next ) => {
                token_text.push( next );
                Ok( next as u8 )
            },
        }
    }

    fn parse_char( src: &mut Peekable<Chars>, token_text: &mut String ) -> Result<u8, SyntaxError> {
        // IDEA treat character literals as just strings of lenght 1, reporting errors if over 1
        return match Self::next_char( src, token_text )? {
            b'\\' => match Self::next_char( src, token_text )? {
                b'\\' => Ok( b'\\' ),
                b'n' => Ok( b'\n' ),
                b't' => Ok( b'\t' ),
                b'\'' => Ok( b'\'' ),
                b'"' => Ok( b'"' ),
                _ => Err( SyntaxError {
                    err_line: 0,
                    col: 0,
                    text: token_text.clone(),
                    msg: "invalid escape character literal",
                    help_msg: "check the documentation for a list of valid escape characters"
                } ),
            },
            b'\x00'..=b'\x1F' | b'\x7F' => Err( SyntaxError {
                err_line: 0,
                col: 0,
                text: token_text.clone(),
                msg: "invalid character literal",
                help_msg: "cannot be a control character"
            } ),
            next => Ok( next ),
        }
    }

    fn next_str_char( src: &mut Peekable<Chars>, text: &mut Vec<u8> ) -> Result<u8, SyntaxError> {
        return match Self::next( src )? {
            Some( ch ) => Ok( ch as u8 ),
            None => Err( SyntaxError {
                err_line: 0,
                col: 0,
                text: format!( "\"{}", String::from_utf8( text.to_owned() ).unwrap() ),
                msg: "invalid string literal",
                help_msg: "missing closing double quote"
            } ),
        }
    }

    fn parse_str( src: &mut Peekable<Chars> ) -> Result<Vec<u8>, SyntaxError> {
        let mut text: Vec<u8> = Vec::new();

        loop {
            match Self::next_str_char( src, &mut text )? {
                b'"' => return Ok( text ),
                b'\\' => match Self::next_str_char( src, &mut text )? {
                    next @ (b'\\' | b'n' | b't' | b'\'' | b'"') => match next {
                        b'\\' => text.push( b'\\' ),
                        b'n' => text.push( b'\n' ),
                        b't' => text.push( b'\t' ),
                        b'\'' => text.push( b'\'' ),
                        b'"' => text.push( b'"' ),
                        _ => unreachable!(),
                    }
                    next => {
                        text.push( b'\\' );
                        text.push( next );
                        return Err( SyntaxError {
                            err_line: 0,
                            col: 0,
                            text: format!( "\"{}", String::from_utf8( text.to_owned() ).unwrap() ),
                            msg: "invalid escape character",
                            help_msg: "check the documentation for a list of valid escape characters"
                        } );
                    },
                },
                b'\x00'..=b'\x1F' | b'\x7F' => return Err( SyntaxError {
                    err_line: 0,
                    col: 0,
                    text: format!( "\"{}", String::from_utf8( text.to_owned() ).unwrap() ),
                    msg: "invalid string literal",
                    help_msg: "contains a control character"
                } ),
                other => text.push( other ),
            }
        }
    }
}


#[derive( Debug, Clone, Copy )]
struct Position<'lexer> {
    line: &'lexer Line,
    token: &'lexer Token,
}

#[derive( Debug )]
struct LexerIter<'lexer> {
    lexer: &'lexer Lexer,
    line: usize,
    token: usize,
}

impl<'lexer> LexerIter<'lexer> {
    // fn sof( &self ) -> Position<'lexer> {
    //     let line = &self.lexer.lines[ 0 ];
    //     let token = &line.tokens[ 0 ];

    //     return Position { line, token };
    // }

    // fn eof( &self ) -> Position<'lexer> {
    //     let line = self.lexer.lines.last().unwrap();
    //     let token = line.tokens.last().unwrap();

    //     return Position { line, token };
    // }

    fn current( &self ) -> Option<Position<'lexer>> {
        let line = self.lexer.lines.get( self.line )?;
        let token = line.tokens.get( self.token )?;

        return Some( Position { line, token } );
    }

    fn next( &mut self ) -> Option<Position<'lexer>> {
        let line = self.lexer.lines.get( self.line )?;
        let (line, token) = if self.token + 1 < line.tokens.len() {
            self.token += 1;
            let token = line.tokens.get( self.token )?;

            (line, token)
        }
        else {
            self.line += 1;
            let line = self.lexer.lines.get( self.line )?;

            self.token = 0;
            let token = line.tokens.get( self.token )?;

            (line, token)
        };

        return Some( Position { line, token } ).or_next( self );
    }

    fn previous( &mut self ) -> Option<Position<'lexer>> {
        let line = self.lexer.lines.get( self.line )?;
        let (line, token) = if self.token > 0 {
            self.token -= 1;
            let token = line.tokens.get( self.token )?;

            (line, token)
        }
        else {
            self.line -= 1;
            let line = self.lexer.lines.get( self.line )?;

            self.token = line.tokens.len() - 1;
            let token = line.tokens.get( self.token )?;

            (line, token)
        };

        return Some( Position { line, token } ).or_previous( self );
    }

    fn peek_next( &mut self ) -> Option<Position<'lexer>> {
        let (starting_line, starting_token) = (self.line, self.token);
        let position = self.next();
        (self.line, self.token) = (starting_line, starting_token);
        return position;
    }

    fn peek_previous( &mut self ) -> Option<Position<'lexer>> {
        let (starting_line, starting_token) = (self.line, self.token);
        let position = self.previous();
        (self.line, self.token) = (starting_line, starting_token);
        return position;
    }
}

trait BoundedPosition<'lexer> {
    type Error;

    fn bounded( self, tokens: &mut LexerIter<'lexer>, err_msg: &'static str ) -> Result<Self, Self::Error>
    where Self: Sized;

    fn or_next( self, tokens: &mut LexerIter<'lexer> ) -> Self
    where Self: Sized;

    fn or_previous( self, tokens: &mut LexerIter<'lexer> ) -> Self
    where Self: Sized;
}

impl<'lexer> BoundedPosition<'lexer> for Option<Position<'lexer>> {
    type Error = (&'lexer Line, SyntaxError);

    fn bounded( self, tokens: &mut LexerIter<'lexer>, err_msg: &'static str ) -> Result<Self, Self::Error> {
        return match self {
            Some( current ) => match current.token.kind {
                TokenKind::EOF => {
                    let previous = tokens.peek_previous().unwrap();
                    Err( (previous.line, SyntaxError {
                        // we are always sure that there is at least the SOF token before the EOF token, so we can safely unwrap
                        err_line: 0,
                        col: previous.token.col,
                        text: previous.token.kind.to_string(),
                        msg: err_msg,
                        help_msg: "file ended after here instead"
                    } ) )
                },
                TokenKind::SOF => {
                    let next = tokens.peek_previous().unwrap();
                    Err( (next.line, SyntaxError {
                        // we are always sure that there is at least the EOF token after the SOF token, so we can safely unwrap
                        err_line: 0,
                        col: next.token.col,
                        text: next.token.kind.to_string(),
                        msg: err_msg,
                        help_msg: "file ended after here instead"
                    } ) )
                },
                _ => Ok( Some( current ) ),
            }
            None => Ok( None ),
        }
    }

    fn or_next( self, tokens: &mut LexerIter<'lexer> ) -> Self {
        return match self?.token.kind {
            TokenKind::Comment( _ ) | TokenKind::Empty => tokens.next(),
            _ => self,
        }
    }

    fn or_previous( self, tokens: &mut LexerIter<'lexer> ) -> Self {
        return match self?.token.kind {
            TokenKind::Comment( _ ) | TokenKind::Empty => tokens.previous(),
            _ => self,
        }
    }
}


#[derive( Debug, Clone )]
enum ExpressionKind {
    Literal( Type ),
    Binary { lhs: Box<ExpressionKind>, op: OpKind, rhs: Box<ExpressionKind> },
    Identifier( String ),
}

impl Display for ExpressionKind {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        match self {
            Self::Literal( literal ) => write!( f, "{}", literal ),
            Self::Binary { lhs, op, rhs } => write!( f, "({} {} {})", lhs, op, rhs ),
            Self::Identifier( name ) => write!( f, "{}", name ),
        }
    }
}


#[derive( Debug, Clone )]
struct If {
    condition: ExpressionKind,
    nodes: Vec<Node>,
}

impl Display for If {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return write!( f, "{}", self.condition );
    }
}


#[derive( Debug, Clone )]
enum Node {
    Expression( ExpressionKind ),
    Print( ExpressionKind ),
    If( If ),
}

impl Display for Node {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Expression( expression ) => write!( f, "{}", expression ),
            Self::Print( node ) => write!( f, "print {}", node ),
            Self::If( iff ) => write!( f, "{}", iff ),
        }
    }
}


enum Statement {
    Empty,
    Stop,
    Single( Node ),
    Multiple( Vec<Node> ),
}

#[derive( Debug, Clone, Copy )]
enum IdentifierExpansion {
    Expand,
    Keep,
}

#[derive( Debug, Clone )]
struct Definition {
    kind: DefinitionKind,
    name: String,
    value: ExpressionKind,
}


#[derive( Debug )]
struct Scope {
    parent: Option<usize>,
    definitions: Vec<Definition>,
}

#[derive( Debug )]
struct Bracket<'lexer> {
    position: Position<'lexer>,
    kind: BracketKind,
}

#[derive( Debug )]
struct AST<'lexer> {
    tokens: LexerIter<'lexer>, // NOTE possibly remove this field

    scopes: Vec<Scope>,
    current_scope: usize,

    nodes: Vec<Node>,
    brackets: Vec<Bracket<'lexer>>,
}

impl<'lexer> TryFrom<&'lexer Lexer> for AST<'lexer> {
    type Error = SyntaxErrors<'lexer>;

    fn try_from( lexer: &'lexer Lexer ) -> Result<Self, Self::Error> {
        let mut this = Self {
            tokens: lexer.iter(),
            scopes: vec![ Scope { parent: None, definitions: Vec::new() } ],
            current_scope: 0,
            nodes: Vec::new(),
            brackets: Vec::new(),
        };

        let mut errors = SyntaxErrors {
            file_path: lexer.file_path.clone(),
            lines: Vec::new(),
            errors: Vec::new(),
        };

        this.tokens.next(); // skipping the SOF token

        let nodes = this.parse( &mut errors, None );
        this.nodes.extend( nodes );

        if !this.brackets.is_empty() {
            for bracket in &this.brackets {
                let mut found = false;
                for line in &errors.lines {
                    if bracket.position.line.number == line.number {
                        found = true;
                    }
                }

                if !found {
                    errors.lines.push( Cow::Borrowed( bracket.position.line ) );
                }

                // there can only be open brackets at this point
                errors.errors.push( SyntaxError {
                    err_line: errors.lines.len() - 1,
                    col: bracket.position.token.col,
                    text: bracket.kind.to_string(),
                    msg: "stray bracket",
                    help_msg: "was not closed"
                } );
            }
        }

        if errors.errors.is_empty() {
            return Ok( this );
        }
        else {
            return Err( errors );
        }
    }
}

impl<'lexer, 'definition> AST<'lexer> {
    fn parse( &mut self, errors: &mut SyntaxErrors<'lexer>, initial_bracket_stack_len: Option<usize> ) -> Vec<Node> {
        let mut nodes: Vec<Node> = Vec::new();

        while let Some( current ) = self.tokens.current().or_next( &mut self.tokens ) {
            let statement_result = match current.token.kind {
                TokenKind::Definition( _ ) => self.variable_definition(),
                TokenKind::Identifier( _ ) => match self.tokens.peek_next() {
                    Some( next ) => match next.token.kind {
                        TokenKind::Equals => self.variable_assignment(),
                        _ => match self.expression( IdentifierExpansion::Keep, true ) {
                            Ok( expression ) => Ok( Statement::Single( Node::Expression( expression ) ) ),
                            Err( err ) => Err( err ),
                        },
                    },
                    None => Ok( Statement::Stop ),
                },
                TokenKind::Print | TokenKind::PrintLn => self.print(),
                TokenKind::If => self.iff( errors ),
                TokenKind::True | TokenKind::False
                | TokenKind::Literal( _ )
                | TokenKind::Bracket( BracketKind::OpenRound ) => match self.expression( IdentifierExpansion::Keep, true ) {
                    Ok( expression ) => Ok( Statement::Single( Node::Expression( expression ) ) ),
                    Err( err ) => Err( err ),
                },
                TokenKind::Bracket( BracketKind::CloseRound ) => {
                    self.tokens.next();
                    Err( (current.line, SyntaxError {
                        err_line: 0,
                        col: current.token.col,
                        text: current.token.kind.to_string(),
                        msg: "invalid expression",
                        help_msg: "stray closed parenthesis"
                    } ) )
                },
                TokenKind::Bracket( kind @ BracketKind::OpenCurly ) => {
                    self.brackets.push( Bracket { position: current, kind } );
                    self.scopes.push( Scope { parent: Some( self.current_scope ), definitions: Vec::new() } );

                    self.current_scope = self.scopes.len() - 1;
                    self.tokens.next();
                    Ok( Statement::Empty )
                },
                TokenKind::Bracket( BracketKind::CloseCurly ) => {
                    self.tokens.next();
                    match self.brackets.pop() {
                        Some( _ ) => {
                            self.current_scope = match self.scopes[ self.current_scope ].parent {
                                None => 0,
                                Some( parent ) => parent,
                            };

                            match initial_bracket_stack_len {
                                Some( len ) => match self.brackets.len() == len {
                                    true => Ok( Statement::Stop ),
                                    false => Ok( Statement::Empty ),
                                },
                                None => Ok( Statement::Empty ),
                            }
                        },
                        None => Err( (current.line, SyntaxError {
                            err_line: 0,
                            col: current.token.col,
                            text: current.token.kind.to_string(),
                            msg: "stray bracket",
                            help_msg: "was not opened before"
                        }) ),
                    }
                },
                TokenKind::Op( _ ) => {
                    self.tokens.next();
                    Err( (current.line, SyntaxError {
                        err_line: 0,
                        col: current.token.col,
                        text: current.token.kind.to_string(),
                        msg: "invalid expression",
                        help_msg: "stray binary operator"
                    } ) )
                },
                TokenKind::Equals => {
                    self.tokens.next();
                    Err( (current.line, SyntaxError {
                        err_line: 0,
                        col: current.token.col,
                        text: current.token.kind.to_string(),
                        msg: "invalid assignment",
                        help_msg: "stray assignment"
                    } ) )
                },
                TokenKind::Comment( _ ) | TokenKind::SemiColon | TokenKind::Empty | TokenKind::EOF => {
                    self.tokens.next();
                    Ok( Statement::Empty )
                },
                TokenKind::SOF => unreachable!(),
            };

            match statement_result {
                Ok( Statement::Empty ) => continue,
                Ok( Statement::Stop ) => break,
                Ok( Statement::Single( node ) ) => nodes.push( node ),
                Ok( Statement::Multiple( multiple_nodes ) ) => for node in multiple_nodes {
                    nodes.push( node );
                },
                Err( (err_line, mut err) ) => {
                    let mut found = false;
                    for line in &errors.lines {
                        if err_line.number == line.number {
                            found = true;
                            break;
                        }
                    }

                    if !found {
                        errors.lines.push( Cow::Borrowed( err_line ) );
                    }

                    err.err_line = errors.lines.len() - 1;
                    errors.errors.push( err );
                },
            }
        }

        return nodes;
    }


    fn resolve( &self, name: &str ) -> &Definition {
        for scope in &self.scopes {
            for definition in &scope.definitions {
                if definition.name == name {
                    return definition;
                }
            }
        }

        unreachable!();
    }

    fn resolve_scoped( &'definition self, name: &str ) -> Option<&'definition Definition> {
        let mut current_scope = self.current_scope;

        loop {
            let scope = &self.scopes[ current_scope ];
            for definition in &scope.definitions {
                if definition.name == name {
                    return Some( definition );
                }
            }

            current_scope = scope.parent?;
        }
    }

    fn resolve_scoped_mut( &'definition mut self, name: &str ) -> Option<&'definition mut Definition> {
        let mut current_scope = self.current_scope;

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


    fn variable_definition( &mut self ) -> Result<Statement, (&'lexer Line, SyntaxError)> {
        let definition_pos = self.tokens.current().unwrap();
        let kind = match definition_pos.token.kind {
            TokenKind::Definition( kind ) => kind.clone(),
            _ => unreachable!(),
        };

        let name_pos = self.tokens.next().bounded( &mut self.tokens, "expected identifier" )?.unwrap();
        let name = match &name_pos.token.kind {
            TokenKind::Identifier( name ) => Ok( name.clone() ),
            _ => Err( (name_pos.line, SyntaxError {
                err_line: 0,
                col: name_pos.token.col,
                text: name_pos.token.kind.to_string(),
                msg: "invalid assignment",
                help_msg: "expected variable name"
            } ) ),
        };

        let equals_pos = self.tokens.next().bounded( &mut self.tokens, "expected equals" )?.unwrap();
        let equals = match equals_pos.token.kind {
            TokenKind::Equals => Ok( () ),
            _ => Err( (name_pos.line, SyntaxError {
                err_line: 0,
                col: name_pos.token.col,
                text: name_pos.token.kind.to_string(),
                msg: "invalid assignment",
                help_msg: "expected '=' after variable name"
            } ) ),
        };

        let value_pos = self.tokens.next().bounded( &mut self.tokens, "expected expression" )?.unwrap();
        let value = match value_pos.token.kind {
            TokenKind::True | TokenKind::False
            | TokenKind::Literal( _ ) | TokenKind::Identifier( _ )
            | TokenKind::Bracket( BracketKind::OpenRound ) => self.expression( IdentifierExpansion::Keep, true ),
            _ => Err( (equals_pos.line, SyntaxError {
                err_line: 0,
                col: equals_pos.token.col,
                text: equals_pos.token.kind.to_string(),
                msg: "invalid assignment",
                help_msg: "expected expression after '='"
            } ) ),
        };

        let name = name?;
        let _ = equals?;
        let value = value?;

        return match self.resolve_scoped( &name ) {
            None => {
                let value = match kind {
                    DefinitionKind::Const => ExpressionKind::Literal( self.evaluate_expression( &value ) ),
                    DefinitionKind::Let | DefinitionKind::Var => value,
                };

                self.scopes[ self.current_scope ].definitions.push( Definition { kind, name, value } );
                Ok( Statement::Empty )
            },
            Some( _ ) => Err( (name_pos.line, SyntaxError {
                err_line: 0,
                col: name_pos.token.col,
                text: name_pos.token.kind.to_string(),
                msg: "variable redefinition",
                help_msg: "was previously defined"
            } ) ),
        }
    }

    fn variable_assignment( &mut self ) -> Result<Statement, (&'lexer Line, SyntaxError)> {
        let name_pos = self.tokens.current().unwrap();
        let equals_pos = self.tokens.next().unwrap();

        let value_pos = self.tokens.next().bounded( &mut self.tokens, "expected expression" )?.unwrap();
        let value = match value_pos.token.kind {
            TokenKind::True | TokenKind::False
            | TokenKind::Literal( _ ) | TokenKind::Identifier( _ )
            | TokenKind::Bracket( BracketKind::OpenRound ) => self.expression( IdentifierExpansion::Expand, true ),
            _ => Err( (equals_pos.line, SyntaxError {
                err_line: 0,
                col: equals_pos.token.col,
                text: equals_pos.token.kind.to_string(),
                msg: "invalid assignment",
                help_msg: "expected expression after '='"
            } ) ),
        };

        let variable = match &name_pos.token.kind {
            TokenKind::Identifier( name ) => match self.resolve_scoped_mut( &name ) {
                Some( identifier ) => Ok( identifier ),
                None => Err( (name_pos.line, SyntaxError {
                    err_line: 0,
                    col: name_pos.token.col,
                    text: name_pos.token.kind.to_string(),
                    msg: "variable redefinition",
                    help_msg: "was not previously defined in this scope"
                } ) ),
            },
            _ => unreachable!()
        };

        let variable = variable?;

        let assignment = match variable.kind {
            DefinitionKind::Let | DefinitionKind::Const => Err( (name_pos.line, SyntaxError {
                err_line: 0,
                col: name_pos.token.col,
                text: name_pos.token.kind.to_string(),
                msg: "invalid assignment",
                help_msg: "was defined as immutable"
            } ) ),
            DefinitionKind::Var => value,
        };

        variable.value = assignment?;
        return Ok( Statement::Empty );
    }


    fn print( &mut self ) -> Result<Statement, (&'lexer Line, SyntaxError)> {
        let print_pos = self.tokens.current().unwrap();
        match print_pos.token.kind {
            TokenKind::PrintLn => match self.tokens.peek_next() {
                Some( Position { token: &Token { kind: TokenKind::SemiColon, .. }, .. } ) => {
                    self.tokens.next();
                    return Ok( Statement::Single( Node::Print( ExpressionKind::Literal( Type::Char { value: '\n' as u8 } ) ) ) );
                },
                _ => (),
            },
            TokenKind::Print => (),
            _ => unreachable!(),
        }

        let argument_pos = self.tokens.next().bounded( &mut self.tokens, "expected print argument or semicolon" )?.unwrap();
        let argument = match &argument_pos.token.kind {
            TokenKind::True | TokenKind::False
            | TokenKind::Literal( _ ) | TokenKind::Identifier( _ )
            | TokenKind::Bracket( BracketKind::OpenRound ) => self.expression( IdentifierExpansion::Keep, true ),
            _ => Err( (argument_pos.line, SyntaxError {
                err_line: 0,
                col: argument_pos.token.col,
                text: argument_pos.token.kind.to_string(),
                msg: "invalid print argument",
                help_msg: "expected an expression"
            } ) ),
        };

        let argument = argument?;

        return match print_pos.token.kind {
            TokenKind::Print => Ok( Statement::Single( Node::Print( argument ) ) ),
            TokenKind::PrintLn => Ok( Statement::Multiple( vec![
                Node::Print( argument ),
                Node::Print( ExpressionKind::Literal( Type::Char { value: '\n' as u8 } ) )
            ] ) ),
            _ => unreachable!(),
        };
    }


    fn iff( &mut self, errors: &mut SyntaxErrors<'lexer> ) -> Result<Statement, (&'lexer Line, SyntaxError)> {
        let if_pos = self.tokens.current().unwrap();

        let _condition_pos = self.tokens.next().bounded( &mut self.tokens, "expected boolean expression" )?.unwrap();
        let condition_value = match self.expression( IdentifierExpansion::Keep, false )? {
            ExpressionKind::Identifier( name ) => self.resolve_scoped( &name ).unwrap().value.clone(),
            condition => condition,
        };

        let condition = match &condition_value {
            ExpressionKind::Literal( literal ) => match literal {
                Type::Bool { .. } => Ok( condition_value ),
                Type::Char { .. }
                | Type::I64 { .. }
                | Type::Str { .. } => Err( (if_pos.line, SyntaxError {
                    err_line: 0,
                    col: if_pos.token.col,
                    text: if_pos.token.kind.to_string(),
                    msg: "expected boolean expression",
                    help_msg: "must be followed by a boolean expression",
                } )),
            },
            ExpressionKind::Binary { op, .. } => match op {
                OpKind::Equals | OpKind::NotEquals
                | OpKind::Greater | OpKind::GreaterOrEquals
                | OpKind::Less | OpKind::LessOrEquals => Ok( condition_value ),
                OpKind::Pow | OpKind::Times | OpKind::Divide
                | OpKind::Plus | OpKind::Minus | OpKind::Compare => Err( (if_pos.line, SyntaxError {
                    err_line: 0,
                    col: if_pos.token.col,
                    text: if_pos.token.kind.to_string(),
                    msg: "expected boolean expression",
                    help_msg: "must be followed by a boolean expression",
                } )),
            },
            ExpressionKind::Identifier( _ ) => unreachable!(),
        };

        let condition = condition?;

        let initial_bracket_stack_len = self.brackets.len();
        let open_curly_pos = self.tokens.current().or_next( &mut self.tokens ).bounded( &mut self.tokens, "expected curly bracket" )?.unwrap();
        let open_curly = match open_curly_pos.token.kind {
            TokenKind::Bracket( kind @ BracketKind::OpenCurly ) => {
                self.brackets.push( Bracket { position: open_curly_pos, kind } );
                self.scopes.push( Scope { parent: Some( self.current_scope ), definitions: Vec::new() } );

                self.current_scope = self.scopes.len() - 1;
                self.tokens.next();
                Ok( () )
            },
            _ => Err( (open_curly_pos.line, SyntaxError {
                err_line: 0,
                col: open_curly_pos.token.col,
                text: open_curly_pos.token.kind.to_string(),
                msg: "expected block scope",
                help_msg: "must be an opened curly bracket",
            }) ),
        };

        let _ = open_curly?;

        let iff = If { condition, nodes: self.parse( errors, Some( initial_bracket_stack_len ) ) };
        return Ok( Statement::Single( Node::If( iff ) ) );
    }


    // TODO disallow implicit conversions (str + i64, char + i64, str + char or str + str (maybe treat this as concatenation))
        // IDEA introduce casting operators
    // TODO implement negative numbers
    fn factor( &mut self, expansion: IdentifierExpansion ) -> Result<ExpressionKind, (&'lexer Line, SyntaxError)> {
        let current = self.tokens.current().or_next( &mut self.tokens ).bounded( &mut self.tokens, "expected expression" )?.unwrap();
        let factor = match &current.token.kind {
            TokenKind::True => Ok( ExpressionKind::Literal( Type::Bool { value: true } ) ),
            TokenKind::False => Ok( ExpressionKind::Literal( Type::Bool { value: false } ) ),
            TokenKind::Literal( literal ) => Ok( ExpressionKind::Literal( literal.clone() ) ),
            TokenKind::Identifier( name ) => match self.resolve_scoped( name ) {
                Some( definition ) => match expansion {
                    IdentifierExpansion::Expand => Ok( definition.value.clone() ),
                    IdentifierExpansion::Keep => Ok( ExpressionKind::Identifier( name.clone() ) ),
                },
                None => Err( (current.line, SyntaxError {
                    err_line: 0,
                    col: current.token.col,
                    text: current.token.kind.to_string(),
                    msg: "variable not defined",
                    help_msg: "was not previously defined in this scope"
                } ) ),
            },
            TokenKind::Bracket( BracketKind::OpenRound ) => {
                let expression_start_pos = self.tokens.next().bounded( &mut self.tokens, "expected expression" )?.unwrap();
                match expression_start_pos.token.kind {
                    TokenKind::Bracket( BracketKind::CloseRound ) => Err( (expression_start_pos.line, SyntaxError {
                        err_line: 0,
                        col: expression_start_pos.token.col,
                        text: expression_start_pos.token.kind.to_string(),
                        msg: "invalid expression",
                        help_msg: "empty expressions are not allowed"
                    } ) ),
                    _ => {
                        let expression = self.expression( expansion, false )?;
                        let close_bracket_pos = self.tokens.current().or_next( &mut self.tokens ).bounded( &mut self.tokens, "expected closed parenthesis" )?.unwrap();
                        match close_bracket_pos.token.kind {
                            TokenKind::Bracket( BracketKind::CloseRound ) => Ok( expression ),
                            _ => Err( (current.line, SyntaxError {
                                err_line: 0,
                                col: current.token.col,
                                text: current.token.kind.to_string(),
                                msg: "invalid expression",
                                help_msg: "unclosed parenthesis"
                            } ) ),
                        }
                    }
                }
            },
            _ => Err( (current.line, SyntaxError {
                err_line: 0,
                col: current.token.col,
                text: current.token.kind.to_string(),
                msg: "invalid expression",
                help_msg: "expected number literal"
            } ) ),
        };

        self.tokens.next();
        return factor;
    }

    // FIX dealing with missing semicolons or missing operators
        // TODO move it outside the expression
        // IDEA create two versions of this function, one that checks for the closing semicolon, and one that doesn't
    fn operator( &mut self, ops: &[OpKind] ) -> Result<Option<OpKind>, (&'lexer Line, SyntaxError)> {
        let current_pos = self.tokens.current().or_next( &mut self.tokens ).bounded( &mut self.tokens, "expected expression or semicolon" )?.unwrap();
        let operator = match current_pos.token.kind {
            TokenKind::Op( op ) => match ops.contains( &op ) {
                true => Some( op ),
                false => None,
            },
            _ => None,
        };

        if let Some( _ ) = operator {
            self.tokens.next();
        }

        return Ok( operator );
    }

    fn exponentiation( &mut self, expansion: IdentifierExpansion ) -> Result<ExpressionKind, (&'lexer Line, SyntaxError)> {
        let mut lhs = self.factor( expansion )?;

        while let Some( op ) = self.operator( &[OpKind::Pow] )? {
            let rhs = self.factor( expansion )?;
            lhs = ExpressionKind::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn multiplication_or_division( &mut self, expansion: IdentifierExpansion ) -> Result<ExpressionKind, (&'lexer Line, SyntaxError)> {
        let mut lhs = self.exponentiation( expansion )?;

        while let Some( op ) = self.operator( &[OpKind::Times, OpKind::Divide] )? {
            let rhs = self.exponentiation( expansion )?;
            lhs = ExpressionKind::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn math( &mut self, expansion: IdentifierExpansion ) -> Result<ExpressionKind, (&'lexer Line, SyntaxError)> {
        let mut lhs = self.multiplication_or_division( expansion )?;

        while let Some( op ) = self.operator( &[OpKind::Plus, OpKind::Minus] )? {
            let rhs = self.multiplication_or_division( expansion )?;
            lhs = ExpressionKind::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn expression( &mut self, expansion: IdentifierExpansion, expected_semicolon: bool ) -> Result<ExpressionKind, (&'lexer Line, SyntaxError)> {
        let mut lhs = self.math( expansion )?;

        let ops = [
            OpKind::Equals, OpKind::NotEquals,
            OpKind::Greater, OpKind::GreaterOrEquals,
            OpKind::Less, OpKind::LessOrEquals,
            OpKind::Compare
        ];

        while let Some( op ) = self.operator( &ops )? {
            let rhs = self.math( expansion )?;
            lhs = ExpressionKind::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        let expression = match expected_semicolon {
            true => {
                let current = self.tokens.current().or_next( &mut self.tokens ).bounded( &mut self.tokens, "expected semicolon" )?.unwrap();
                let expr = match current.token.kind {
                    TokenKind::SemiColon => Ok( lhs ),
                    _ => {
                        let previous = self.tokens.peek_previous().bounded( &mut self.tokens, "expected semicolon" )?.unwrap();
                        Err( (previous.line, SyntaxError {
                            err_line: 0,
                            col: previous.token.col,
                            text: previous.token.kind.to_string(),
                            msg: "invalid expression",
                            help_msg: "expected an operator after this token to complete the expression, or a ';' to end the statement"
                        } ) )
                    },
                };

                self.tokens.next();
                expr
            },
            false => Ok( lhs ),
        };

        return expression;
    }

    fn evaluate_expression( &self, expression: &ExpressionKind ) -> Type {
        return match expression {
            ExpressionKind::Literal( literal ) => literal.clone(),
            ExpressionKind::Binary { lhs, op, rhs } => {
                let lhs: i64 = self.evaluate_expression( lhs ).into();
                let rhs: i64 = self.evaluate_expression( rhs ).into();

                match op {
                    OpKind::Plus => Type::I64 { value: lhs + rhs },
                    OpKind::Minus => Type::I64 { value: lhs - rhs },
                    OpKind::Times => Type::I64 { value: lhs * rhs },
                    OpKind::Divide => Type::I64 { value: lhs / rhs },
                    OpKind::Pow => Type::I64 { value: lhs.pow( rhs as u32 ) },

                    OpKind::Equals => Type::Bool { value: lhs == rhs },
                    OpKind::NotEquals => Type::Bool { value: lhs != rhs },
                    OpKind::Greater => Type::Bool { value: lhs > rhs },
                    OpKind::GreaterOrEquals => Type::Bool { value: lhs >= rhs },
                    OpKind::Less => Type::Bool { value: lhs < rhs },
                    OpKind::LessOrEquals => Type::Bool { value: lhs <= rhs },

                    OpKind::Compare => Type::I64 { value: lhs.cmp( &rhs ) as i64 },
                }
            },
            ExpressionKind::Identifier( name ) => self.evaluate_expression( &self.resolve( name ).value ),
        };
    }
}


// NOTE only epxlicitly processing nodes that print values for now
// TODO move identifier resolution to here
    // NOTE have identifiers contain their definition line and token

#[derive( Debug )]
struct Interpreter<'ast> {
    ast: &'ast AST<'ast>,
}

// Interpreting
impl<'ast> Interpreter<'ast> {
    const INTERPRETING: &'static str = colored!{
        text: "Interpreting",
        foreground: Foreground::LightGreen,
        bold: true,
    };

    fn evaluate_expression( &self, expression: &ExpressionKind ) -> Type {
        return match expression {
            ExpressionKind::Literal( literal ) => literal.clone(),
            ExpressionKind::Binary { lhs, op, rhs } => {
                let lhs: i64 = self.evaluate_expression( lhs ).into();
                let rhs: i64 = self.evaluate_expression( rhs ).into();

                match op {
                    OpKind::Plus => Type::I64 { value: lhs + rhs },
                    OpKind::Minus => Type::I64 { value: lhs - rhs },
                    OpKind::Times => Type::I64 { value: lhs * rhs },
                    OpKind::Divide => Type::I64 { value: lhs / rhs },
                    OpKind::Pow => Type::I64 { value: lhs.pow( rhs as u32 ) },

                    OpKind::Equals => Type::Bool { value: lhs == rhs },
                    OpKind::NotEquals => Type::Bool { value: lhs != rhs },
                    OpKind::Greater => Type::Bool { value: lhs > rhs },
                    OpKind::GreaterOrEquals => Type::Bool { value: lhs >= rhs },
                    OpKind::Less => Type::Bool { value: lhs < rhs },
                    OpKind::LessOrEquals => Type::Bool { value: lhs <= rhs },

                    OpKind::Compare => Type::I64 { value: lhs.cmp( &rhs ) as i64 },
                }
            },
            ExpressionKind::Identifier( name ) => self.evaluate_expression( &self.ast.resolve( name ).value ),
        };
    }

    fn interpret_nodes( &self, nodes: &Vec<Node> ) {
        for node in nodes {
            match node {
                Node::Print( argument ) => self.evaluate_expression( argument ).display(),
                Node::Expression { .. } => continue,
                Node::If( iff ) => match self.evaluate_expression( &iff.condition ) {
                    Type::Bool { value } => if value {
                        self.interpret_nodes( &iff.nodes );
                    },
                    Type::Char { .. } | Type::I64 { .. } | Type::Str { .. } => unreachable!(),
                },
            }
        }
    }

    fn interpret( &self, file_path: &str ) {
        println!( "{}: {}", Interpreter::INTERPRETING, file_path );

        self.interpret_nodes( &self.ast.nodes );
    }
}


#[derive( Debug )]
struct StringTag<'ast> {
    string: &'ast Type,
    tag: String,
    len_tag: String,
}

#[derive( Debug )]
struct IfTag<'ast> {
    iff: &'ast Node,
    tag: String,
    false_tag: String,
}

#[derive( Debug )]
struct Compiler<'ast> {
    ast: &'ast AST<'ast>,

    strings: Vec<StringTag<'ast>>,
    ifs: Vec<IfTag<'ast>>,
}

// Compiling
impl<'ast> Compiler<'ast> {
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

    fn gather_tags( &mut self, rodata: &mut String, nodes: &'ast Vec<Node> ) {
        for node in nodes {
            let value = match node {
                Node::Expression( expression ) => expression,
                Node::Print( argument ) => match argument {
                    ExpressionKind::Identifier( name ) => &self.ast.resolve( name ).value,
                    _ => argument,
                },
                Node::If( iff ) => {
                    let if_idx = self.ifs.len();
                    self.ifs.push( IfTag {
                        iff: &node,
                        tag: format!( "if_{}", if_idx ),
                        false_tag: format!( "if_{}_false", if_idx )
                    } );

                    self.gather_tags( rodata, &iff.nodes );
                    continue;
                },
            };

            self.gather_strings( rodata, value );
        }
    }

    fn gather_strings( &mut self, rodata: &mut String, expression: &'ast ExpressionKind ) {
        match expression {
            ExpressionKind::Literal( string @ Type::Str { text } ) => {
                let string_idx = self.strings.len();
                let string_tag = StringTag {
                    string: &string,
                    tag: format!( "str_{}", string_idx ),
                    len_tag: format!( "str_{}_len", string_idx )
                };

                let mut string_asm = String::with_capacity( string.len() + 2 );
                string_asm.push( '`' );
                for ch in text {
                    string_asm.push( *ch as char );
                }
                string_asm.push( '`' );

                rodata.push_str( &format!(
                    "\n {}: db {}\
                    \n {}: equ $ - {}\n",
                    string_tag.tag, string_asm,
                    string_tag.len_tag, string_tag.tag
                ) );

                self.strings.push( string_tag );
            },
            ExpressionKind::Literal( _ ) => (),
            ExpressionKind::Binary { lhs, op: _, rhs } => {
                self.gather_strings( rodata, lhs );
                self.gather_strings( rodata, rhs );
            },
            ExpressionKind::Identifier( name ) => self.gather_strings( rodata, &self.ast.resolve( &name ).value ),
        }
    }


    fn compile_nodes( &self, nodes: &Vec<Node>, asm: &mut String, only_string_len: bool ) {
        for node in nodes {
            match node {
                Node::Print( argument ) => {
                    let value = match argument {
                        ExpressionKind::Literal( _ ) | ExpressionKind::Binary { .. } => argument,
                        ExpressionKind::Identifier( name ) => &self.ast.resolve( name ).value,
                    };

                    let value_type = match value {
                        ExpressionKind::Literal( literal ) => literal,
                        ExpressionKind::Binary { op, .. } => match op {
                            OpKind::Pow | OpKind::Times | OpKind::Divide
                            | OpKind::Plus | OpKind::Minus | OpKind::Compare => &Type::I64 { value: 0 },
                            OpKind::Equals | OpKind::NotEquals
                            | OpKind::Greater | OpKind::GreaterOrEquals
                            | OpKind::Less | OpKind::LessOrEquals => &Type::Bool { value: false }
                        }
                        ExpressionKind::Identifier( _ ) => unreachable!(),
                    };

                    let print_asm = match value_type {
                        Type::I64 { .. } =>
                            "\n pop rdi\
                            \n mov rsi, 10\
                            \n call int_toStr\
                            \n\
                            \n mov rdi, stdout\
                            \n mov rsi, rax\
                            \n mov rax, SYS_write\
                            \n syscall",
                        Type::Char { .. } =>
                            "\n mov rdi, stdout\
                            \n mov rsi, rsp\
                            \n mov rdx, 1\
                            \n mov rax, SYS_write\
                            \n syscall\
                            \n pop rsi",
                        Type::Bool { .. } =>
                            "\n pop rsi\
                            \n pop rdx\
                            \n mov rdi, stdout\
                            \n mov rax, SYS_write\
                            \n syscall",
                        Type::Str { text: _ } =>
                            "\n pop rdx\
                            \n pop rsi\
                            \n mov rdi, stdout\
                            \n mov rax, SYS_write\
                            \n syscall",
                    };

                    asm.push_str( &format!( " ; {}\n", node ) );
                    self.compile_expression( value, asm, only_string_len, true );
                    asm.push_str( &format!( "{}\n\n", print_asm ) );
                },
                Node::If( iff ) => for if_tag in &self.ifs {
                    if node as *const _ == if_tag.iff as *const _ {
                        asm.push_str( &format!( "{}:\n", if_tag.tag ) );

                        self.compile_expression( &iff.condition, asm, only_string_len, false );

                        asm.push_str( &format!(
                            "\n pop rax\
                            \n pop rdx\
                            \n cmp rax, 1\
                            \n jne {}\n",
                            if_tag.false_tag
                        ) );

                        self.compile_nodes( &iff.nodes, asm, only_string_len );

                        asm.push_str( &format!( "{}:\n", if_tag.false_tag ) );
                    }
                }
                Node::Expression( _ ) => continue,
            }
        }
    }

    fn compile_expression( &self, expression: &ExpressionKind, asm: &mut String, only_string_len: bool, bool_to_str: bool ) {
        // NOTE it is literally possible to copy paste the entire expression in the generated asm file (only if it doesn't include basic math expressions)
            // IDEA check if the expression contains non basic math expressions, if not just copy paste the literal expression
        match expression {
            ExpressionKind::Literal( literal ) => match literal {
                Type::I64 { value } => asm.push_str( &format!( " push {}\n", value ) ),
                Type::Char { value } => asm.push_str( &format!( " push {}\n", value ) ),
                Type::Bool { value } => asm.push_str( &format!( " push {}\n", value ) ),
                Type::Str { .. } => for string_tag in &self.strings {
                    if literal as *const _ == string_tag.string as *const _  {
                        if !only_string_len {
                            asm.push_str( &format!( " push {}\n", string_tag.tag ) );
                        }

                        asm.push_str( &format!( " push {}\n", string_tag.len_tag ) );
                        break;
                    }
                },
            },
            ExpressionKind::Binary { lhs, op, rhs } => {
                self.compile_expression( lhs, asm, true, bool_to_str );
                self.compile_expression( rhs, asm, true, bool_to_str );
                let op_asm = match op {
                    OpKind::Pow =>
                        "\n pop rsi\
                        \n pop rdi\
                        \n call int_pow\
                        \n push rax".to_string(),
                    OpKind::Times =>
                        "\n pop rax\
                        \n pop rbx\
                        \n imul rax, rbx\
                        \n push rax".to_string(),
                    OpKind::Divide =>
                        "\n pop rbx\
                        \n pop rax\
                        \n xor rdx, rdx\
                        \n idiv rbx\
                        \n push rax".to_string(),
                    OpKind::Plus =>
                        "\n pop rax\
                        \n pop rbx\
                        \n add rax, rbx\
                        \n push rax".to_string(),
                    OpKind::Minus =>
                        "\n pop rbx\
                        \n pop rax\
                        \n sub rax, rbx\
                        \n push rax".to_string(),

                    OpKind::Equals | OpKind::NotEquals
                    | OpKind::Greater | OpKind::GreaterOrEquals
                    | OpKind::Less | OpKind::LessOrEquals => {
                        let cmov_kind = match op {
                            OpKind::Equals => "e",
                            OpKind::NotEquals => "ne",
                            OpKind::Greater => "g",
                            OpKind::GreaterOrEquals => "ge",
                            OpKind::Less => "l",
                            OpKind::LessOrEquals => "le",
                            _ => unreachable!(),
                        };

                        if bool_to_str {
                            format!(
                                "\n pop rsi\
                                \n pop rdi\
                                \n mov rbx, true_str\
                                \n mov rax, false_str\
                                \n cmp rdi, rsi\
                                \n cmov{} rax, rbx\
                                \n mov rbx, true_str_len\
                                \n mov rdx, false_str_len\
                                \n cmov{} rdx, rbx\
                                \n push rdx\
                                \n push rax",
                                cmov_kind,
                                cmov_kind
                            )
                        }
                        else {
                            format!(
                                "\n pop rsi\
                                \n pop rdi\
                                \n mov rbx, 1\
                                \n mov rax, 0\
                                \n cmp rdi, rsi\
                                \n cmov{} rax, rbx\
                                \n push rax",
                                cmov_kind
                            )
                        }
                    },

                    OpKind::Compare =>
                        "\n pop rsi\
                        \n pop rdi\
                        \n mov rax, LESS\
                        \n cmp rdi, rsi\
                        \n mov rbx, EQUAL\
                        \n cmove rax, rbx\
                        \n mov rbx, GREATER\
                        \n cmovg rax, rbx\
                        \n push rax".to_string(),
                };

                asm.push_str( &format!( " ; {}", expression ) );
                asm.push_str( &format!( "{}\n\n", op_asm ) );
            },
            ExpressionKind::Identifier( name ) => self.compile_expression( &self.ast.resolve( name ).value, asm, only_string_len, bool_to_str ),
        }
    }


    fn compile( &mut self, file_path: &str ) -> Result<PathBuf, ()> {
        let src_file_path = Path::new( file_path );
        println!( "{}: {}", Compiler::BUILDING, src_file_path.display() );

        let asm_file_path = src_file_path.with_extension( "asm" );
        let mut asm_file = BufWriter::new( File::create( &asm_file_path ).unwrap() );

        let mut rodata =
r#" stdout: equ 1
 SYS_write: equ 1
 SYS_exit: equ 60
 EXIT_SUCCESS: equ 0
 newline: db 10

 I64_MIN: equ 1 << 63
 I64_MAX: equ ~I64_MIN
 INT_MAX_DIGITS: equ 64

 true_str: db "true"
 true_str_len: equ $ - true_str

 false_str: db "false"
 false_str_len: equ $ - false_str

 LESS: equ -1
 EQUAL: equ 0
 GREATER: equ 1
"#.to_string();

        self.gather_tags( &mut rodata, &self.ast.nodes );

        let data =
r" int_str: times INT_MAX_DIGITS + 1 db 0
 int_str_bufsize: equ $ - int_str
 int_str_len: equ int_str_bufsize - 1";

        let int_to_str =
r"int_toStr:
 mov rsi, 10

 push rcx
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
 ret";

        let int_pow =
r"int_pow:
 cmp rsi, 1
 je .exponent_is_one

 cmp rsi, 0
 je .exponent_is_zero

 push rdi
 push rsi

 mov rax, rdi
 mov rdi, 1

.next_power:
 cmp rsi, 1
 jle .done

 test rsi, 1
 jnz .exponent_is_odd

 imul rax, rax
 shr rsi, 1
 jmp .next_power

.exponent_is_odd:
 imul rdi, rax
 imul rax, rax

 dec rsi
 shr rsi, 1
 jmp .next_power

.done:
 imul rax, rdi

 pop rsi
 pop rdi
 ret

.exponent_is_one:
 mov rax, rdi
 ret

.exponent_is_zero:
 mov rax, 1
 ret";

        let sys_exit =
r" mov rdi, EXIT_SUCCESS
 mov rax, SYS_exit
 syscall";

        let mut program_asm = String::new();
        self.compile_nodes( &self.ast.nodes, &mut program_asm, false );

        let program = format!(
r"global _start

section .rodata
{}
section .data
{}

section .text
{}

{}

_start:
{}
{}", rodata, data, int_to_str, int_pow, program_asm, sys_exit );

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
        let executable_file_path = self.compile( &file_path )?;
        println!( "{}: {}", Compiler::RUNNING, executable_file_path.display() );

        let output = Command::new( format!( "{}", executable_file_path.display() ) ).output().unwrap();
        print!( "{}", String::from_utf8_lossy( &output.stdout ) );

        return Ok( () );
    }
}


fn print_usage() {
    println!( r"
Blitzlang compiler, version {}

Usage: blitz [Options] [Run mode] file.blz

Options:
    -h, --help              Display this message

Run mode:
    interpret <file.blz>    Interpret the source code (default if no run mode command is provided)
    build     <file.blz>    Compile the source code down to a binary executable
    run       <file.blz>    Compile and run the generated binary executable
", env!( "CARGO_PKG_VERSION" ) );
}

// TODO implement SyntaxErrors to report cli mistakes
fn main() -> ExitCode {
    let mut args: Vec<String> = env::args().collect();

    // to quickly debug
    // args.push( "build".to_string() );
    args.push( "run".to_string() );
    args.push( "examples/main.blz".to_string() );

    if args.len() < 2 {
        print_usage();
        return ExitCode::SUCCESS;
    }

    let mut interpret_flag = false;
    let mut build_flag = false;
    let mut run_flag = false;

    let mut source_file_path: Option<String> = None;
    for arg in args.into_iter().skip( 1 ) { // skipping the name of this executable
        match arg.as_str() {
            "-h" | "--help" => {
                print_usage();
                return ExitCode::SUCCESS;
            },
            "interpret" => interpret_flag = true,
            "build" => build_flag = true,
            "run" => run_flag = true,
            _ => match source_file_path {
                None => source_file_path = Some( arg ),
                Some( _ ) => {
                    eprintln!( "{}: too many source file paths provided", SyntaxError::ERROR );
                    return ExitCode::FAILURE;
                }
            },
        }
    }

    if !interpret_flag && !build_flag && !run_flag {
        interpret_flag = true;
    }
    else if interpret_flag && (build_flag || run_flag) {
        eprintln!( "{}: cannot interpret and build/run at the same time", SyntaxError::ERROR );
        return ExitCode::FAILURE;
    }
    else if build_flag && run_flag {
        eprintln!( "{}: build and run commands cannot be used together", SyntaxError::ERROR );
        return ExitCode::FAILURE;
    }

    let source_file_path = match source_file_path {
        Some( path ) => path,
        None => {
            eprintln!( "{}: no source file path provided", SyntaxError::ERROR );
            return ExitCode::FAILURE;
        }
    };

    let source_file = match File::open( &source_file_path ) {
        Ok( file ) => {
            if file.metadata().unwrap().is_dir() {
                eprintln!( "{}: provided file was a directory", SyntaxError::ERROR );
                return ExitCode::FAILURE;
            }
            else {
                file
            }
        },
        Err( err ) => {
            let cause = match err.kind() {
                ErrorKind::NotFound => "file not found, or wrong file name",
                _ => "unknown error",
            };

            eprintln!( "{}: could not open file '{}'!\nCause: {}", SyntaxError::ERROR, &source_file_path, cause );
            return ExitCode::FAILURE;
        },
    };

    let lexer: Lexer = match (source_file_path.as_str(), source_file).try_into() {
        Ok( lexer ) => {
            // println!( "{:#?}\n", lexer );
            lexer
        },
        Err( errors ) => {
            eprint!( "{}", errors );
            return ExitCode::FAILURE;
        },
    };

    let ast: AST = match (&lexer).try_into() {
        Ok( ast ) => {
            // println!( "{:#?}", ast );
            ast
        },
        Err( errors ) => {
            eprint!( "{}", errors );
            return ExitCode::FAILURE;
        },
    };

    if interpret_flag {
        let interpreter = Interpreter { ast: &ast };
        interpreter.interpret( &source_file_path );
    }
    else {
        let mut compiler = Compiler { ast: &ast, strings: Vec::new(), ifs: Vec::new() };

        if build_flag {
            let _ = compiler.compile( &source_file_path );
        }
        else if run_flag {
            let _ = compiler.run( &source_file_path );
        }
    }

    return ExitCode::SUCCESS;
}
