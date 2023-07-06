// TODO create standardized module for outputting colored text
// TODO implement negative numbers
use std::{io::{BufReader, BufRead, ErrorKind, BufWriter, Write}, fs::File, env, process::{ExitCode, Command}, fmt::Display, path::{Path, PathBuf}, iter::Peekable, str::Chars};


#[derive( Debug, Clone )]
enum Type {
    I64 { value: i64 },
    Char { value: u8 }, // only supporting ASCII characters for now
    Bool { value: bool },
}

impl Display for Type {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::I64 { value } => write!( f, "{}", value ),
            Self::Char { value } => write!( f, "'{}'", value.escape_ascii().to_string() ), // TODO create own escaping function
            Self::Bool { value } => write!( f, "{}", value ),
        }
    }
}

impl Into<i64> for Type {
    fn into( self ) -> i64 {
        return match self {
            Self::I64 { value } => value.into(),
            Self::Char { value } => value.into(),
            Self::Bool { value } => value.into(),
        }
    }
}

impl Type {
    fn display( &self ) {
        match self {
            Self::I64 { value } => print!( "{}", value ),
            Self::Char { value } => print!( "{}", *value as char ),
            Self::Bool { value } => print!( "{}", value ),
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


#[derive( Debug, Clone, Copy )]
enum DefinitionKind {
    // Fn,
    Let,
    Const,
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


#[derive( Debug, Clone )]
enum TokenKind {
    Unexpected{ text: String, err_msg: &'static str, help_msg: &'static str },

    // Whitespace
    Comment( String ),

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
            Self::Unexpected { text, .. } => write!( f, "{}", text ),

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

            Self::Empty | Self::SOF | Self::EOF => write!( f, "" ),
        }
    }
}


#[derive( Debug, Clone )]
struct Token {
    col: usize,
    len: usize,
    kind: TokenKind,
}

#[derive( Debug, Clone )]
struct Line {
    number: usize,
    tokens: Vec<Token>,
}

impl Display for Line {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
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
                Some( next_token ) => next_token.col - (token.col + token.len),
                None => 0,
            };

            write!( f, "{}{:spaces_before_next_token$}", token.kind, "" )?;
        }

        return Ok( () );
    }
}

// IDEA consider removing Line struct and have each token remember its line, or dont store the line number and calculate it somehow
#[derive( Debug )]
struct Lexer {
    file_path: String,
    lines: Vec<Line>,
}

impl Display for Lexer {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        for line in &self.lines {
            for token in &line.tokens {
                if let TokenKind::Unexpected { err_msg, help_msg, .. } = &token.kind {
                    let error = SyntaxError { pos: Position{ line, token }, err_msg, help_msg };
                    error.display( f, &self.file_path )?;
                }
            }
        }

        return Ok( () );
    }
}

impl TryFrom<(&str, File)> for Lexer {
    type Error = Self;

    // IDEA make the input character stream generic, eg: to be able to compile from strings instead of just files
    // TODO make an character iterator similar to LexerIter
    fn try_from( src: (&str, File) ) -> Result<Self, Self::Error> {
        let (file_path, src_file) = (src.0, src.1);
        let mut errors: Vec<Line> = Vec::new();
        let mut lines: Vec<Line> = Vec::new();
        let mut line = Line { number: 1, tokens: vec![ Token { col: 1, len: 1, kind: TokenKind::SOF } ] };

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
                let (len, kind): (usize, TokenKind) = match Self::next( &mut src ) {
                    Ok( None ) => break,
                    Err( err ) => (token_text.len(), err.clone()),
                    Ok( Some( ch ) ) => match ch {
                        // TODO consume until not whitespace
                        // ignore whitespace
                        _ if ch.is_ascii_whitespace() => {
                            col += 1;
                            continue;
                        },
                        '(' => (1, TokenKind::Bracket( BracketKind::OpenRound )),
                        ')' => (1, TokenKind::Bracket( BracketKind::CloseRound )),
                        '{' => (1, TokenKind::Bracket( BracketKind::OpenCurly )),
                        '}' => (1, TokenKind::Bracket( BracketKind::CloseCurly )),
                        '=' => match Self::peek_next( &mut src ) {
                            Ok( Some( '=' ) ) => {
                                let _ = Self::next( &mut src );
                                (2, TokenKind::Op( OpKind::Equals ))
                            },
                            _ => (1, TokenKind::Equals),
                        },
                        '!' => match Self::peek_next( &mut src ) {
                            Ok( Some( '=' ) ) => {
                                let _ = Self::next( &mut src );

                                (2, TokenKind::Op( OpKind::NotEquals ) )
                            },
                            _ => (1, TokenKind::Unexpected {
                                text: ch.to_string(),
                                err_msg: "unexpected character",
                                help_msg: "unrecognized"
                            }),
                        },
                        '>' => match Self::peek_next( &mut src ) {
                            Ok( Some( '=' ) ) => {
                                let _ = Self::next( &mut src );

                                (2, TokenKind::Op( OpKind::GreaterOrEquals ) )
                            },
                            _ => (1, TokenKind::Op( OpKind::Greater )),
                        },
                        '<' => match Self::peek_next( &mut src ) {
                            Ok( Some( '=' ) ) => {
                                let _ = Self::next( &mut src );

                                match Self::peek_next( &mut src ) {
                                    Ok( Some( '>' ) ) => {
                                        let _ = Self::next( &mut src );

                                        (3, TokenKind::Op( OpKind::Compare ) )
                                    },
                                    _ => {
                                        (2, TokenKind::Op( OpKind::LessOrEquals ) )
                                    }
                                }
                            },
                            _ => (1, TokenKind::Op( OpKind::Less )),
                        },
                        '^' => (1, TokenKind::Op( OpKind::Pow )),
                        '*' => (1, TokenKind::Op( OpKind::Times )),
                        '/' => (1, TokenKind::Op( OpKind::Divide )),
                        '+' => (1, TokenKind::Op( OpKind::Plus )),
                        '-' => (1, TokenKind::Op( OpKind::Minus )),
                        ';' => (1, TokenKind::SemiColon),
                        '\'' => {
                            token_text.clear();
                            token_text.push( ch );

                            let kind = match Self::parse_char( &mut src, &mut token_text ) {
                                Ok( b'\'' ) if token_text.len() == 2 => TokenKind::Unexpected {
                                text: token_text.clone(),
                                err_msg: "empty character literal",
                                help_msg: "must not be empty"
                                },
                                Ok( value ) => match Self::next_char( &mut src, &mut token_text ) {
                                    Ok( next ) => match next {
                                        b'\'' => TokenKind::Literal( Type::Char { value } ),
                                        _ => TokenKind::Unexpected {
                                            text: token_text.clone(),
                                            err_msg: "unclosed character literal",
                                            help_msg: "missing closing single quote"
                                        },
                                    },
                                    Err( err ) => err,
                                },
                                Err( err ) => err,
                            };

                            (token_text.len(), kind)
                        },
                        '#' => {
                            token_text.clear();
                            token_text.push( ch );

                            // consume the rest of the tokens in the current line
                            while let Some( next ) = src.next_if( |c| *c != '\n' ) {
                                token_text.push( next );
                            }

                            (token_text.len(), TokenKind::Comment( token_text.clone() ))
                        },
                        '0'..='9' => { // TODO handle negative numbers
                            token_text.clear();
                            token_text.push( ch );

                            let mut is_digit = true;
                            while let Some( next ) = src.next_if( |c| matches!( c, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' ) ) {
                                if !next.is_ascii_digit() {
                                    is_digit = false;
                                }

                                token_text.push( next );
                            }

                            let kind = if is_digit {
                                match token_text.parse() { // TODO create own number parsing function
                                    Ok( value ) => TokenKind::Literal( Type::I64 { value } ),
                                    Err( _ ) => TokenKind::Unexpected {
                                        text: token_text.clone(),
                                        err_msg: "expected number literal",
                                        help_msg: "overflows a 64 bit integer [-9223372036854775808, 9223372036854775807]"
                                    },
                                }
                            }
                            else {
                                TokenKind::Unexpected {
                                    text: token_text.clone(),
                                    err_msg: "expected number literal",
                                    help_msg: "not a number literal"
                                }
                            };

                            (token_text.len(), kind)
                        },
                        'a'..='z' | 'A'..='Z' | '_'  => {
                            token_text.clear();
                            token_text.push( ch );

                            while let Some( next ) = src.next_if( |c| matches!( c, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' ) ) {
                                token_text.push( next );
                            }

                            let kind = match token_text.as_str() {
                                // "entry" => TokenKind::Entry,
                                // "fn" => TokenKind::Fn,
                                "let" => TokenKind::Definition( DefinitionKind::Let ),
                                "const" => TokenKind::Definition( DefinitionKind::Const ),
                                "var" => TokenKind::Definition( DefinitionKind::Var ),
                                "true" => TokenKind::True,
                                "false" => TokenKind::False,
                                "print" => TokenKind::Print,
                                "println" => TokenKind::PrintLn,
                                // "return" => TokenKind::Return,
                                _ => TokenKind::Identifier( token_text.clone() )
                            };

                            (token_text.len(), kind)
                        },
                        _ => (1, TokenKind::Unexpected {
                            text: ch.to_string(),
                            err_msg: "unexpected character",
                            help_msg: "unrecognized"
                        })
                    },
                };

                if let TokenKind::Unexpected { .. } = kind {
                    line_contains_errors = true;
                }

                line.tokens.push( Token { col, len, kind } );
                col += len;
            }

            if trimmed_line_len == 0 {
                line.tokens.push( Token { col, len: 1, kind: TokenKind::Empty } );
            }

            let reached_eof = trimmed_line_len == chars_read;
            if reached_eof {
                line.tokens.push( Token { col, len: 1, kind: TokenKind::EOF } );
            }

            if line_contains_errors {
                errors.push( line.clone() );
            }
            else {
                lines.push( line.clone() );
            }

            if reached_eof {
                break;
            }

            src_line_text.clear();
            line.tokens.clear();
            line.number = lines.len() + 1;
        }

        return if errors.is_empty() {
            Ok( Self { file_path: file_path.to_string(), lines } )
        }
        else {
            Err( Self { file_path: file_path.to_string(), lines: errors } )
        }
    }
}

impl Lexer {
    fn iter<'lexer>( &'lexer self ) -> LexerIter<'lexer> {
        LexerIter{ lexer: self, line: 0, token: 0 }
    }

    // FIX properly handle non ASCII codes in error messages
    fn next( src: &mut Peekable<Chars> ) -> Result<Option<char>, TokenKind> {
        return match src.next() {
            Some( next @ ..='\x7F' ) => Ok( Some( next ) ),
            Some( next ) => Err( TokenKind::Unexpected {
                text: next.to_string(),
                err_msg: "unrecognized character",
                help_msg: "not a valid ASCII character"
            } ),
            None => Ok( None ),
        }
    }

    fn peek_next<'src>( src: &'src mut Peekable<Chars> ) -> Result<Option<&'src char>, TokenKind> {
        return match src.peek() {
            Some( next @ ..='\x7F' ) => Ok( Some( next ) ),
            Some( next ) => Err( TokenKind::Unexpected {
                text: next.to_string(),
                err_msg: "unrecognized character",
                help_msg: "not a valid ASCII character"
            } ),
            None => Ok( None ),
        }
    }

    fn next_char( src: &mut Peekable<Chars>, token_text: &mut String ) -> Result<u8, TokenKind> {
        return match Self::next( src )? {
            Some( '\n' ) | None => Err( TokenKind::Unexpected {
                text: token_text.clone(),
                err_msg: "invalid character literal",
                help_msg: "missing closing single quote"
            } ),
            Some( next ) => {
                token_text.push( next );
                Ok( next as u8 )
            },
        }
    }

    fn parse_char( src: &mut Peekable<Chars>, token_text: &mut String ) -> Result<u8, TokenKind> {
        // IDEA treat character literals as just strings of lenght 1, reporting errors if over 1
        return match Self::next_char( src, token_text )? {
            b'\\' => match Self::next_char( src, token_text )? {
                b'n' => Ok( b'\n' as u8 ),
                b't' => Ok( b'\t' as u8 ),
                b'\'' => Ok( b'\'' as u8 ),
                b'"' => Ok( b'"' as u8 ),
                _ => Err( TokenKind::Unexpected {
                    text: token_text.clone(),
                    err_msg: "invalid escape character literal",
                    help_msg: "check the documentation for a list of valid escape characters"
                } ),
            },
            b'\x00'..=b'\x1F' | b'\x7F' => Err( TokenKind::Unexpected {
                text: token_text.clone(),
                err_msg: "invalid character literal",
                help_msg: "cannot be a control character"
            } ),
            next => Ok( next as u8 ),
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
    fn bounded( self, tokens: &mut LexerIter<'lexer>, err_msg: &'static str ) -> Result<Self, SyntaxError<'lexer>>
    where Self: Sized;

    fn or_next( self, tokens: &mut LexerIter<'lexer> ) -> Self
    where Self: Sized;

    fn or_previous( self, tokens: &mut LexerIter<'lexer> ) -> Self
    where Self: Sized;
}

impl<'lexer> BoundedPosition<'lexer> for Option<Position<'lexer>> {
    fn bounded( self, tokens: &mut LexerIter<'lexer>, err_msg: &'static str ) -> Result<Self, SyntaxError<'lexer>> {
        return match self {
            Some( current ) => match current.token.kind {
                TokenKind::EOF => Err( SyntaxError {
                    // we are always sure that there is at least the SOF token before the EOF token, so we can safely unwrap
                    pos: tokens.peek_previous().unwrap(),
                    err_msg,
                    help_msg: "file ended after here instead"
                } ),
                TokenKind::SOF => Err( SyntaxError {
                    // we are always sure that there is at least the EOF token after the SOF token, so we can safely unwrap
                    pos: tokens.peek_next().unwrap(),
                    err_msg,
                    help_msg: "file ended after here instead"
                } ),
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


// TODO introduce Unexpected Node to let the parsing of the expression continue untill finished, and the reporting errors
#[derive( Debug, Clone )]
enum Node {
    Literal( Type ),
    Expression{ lhs: Box<Node>, op: OpKind, rhs: Box<Node> },
    Identifier( String ),
    Print( Box<Node> ),
    PrintLn( Box<Node> ),
}

impl Display for Node {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Literal( literal ) => write!( f, "{}", literal ),
            Self::Expression { lhs, op, rhs } => write!( f, "({} {} {})", lhs, op, rhs ),
            Self::Identifier( name ) => write!( f, "{}", name ),
            Self::Print( node ) => write!( f, "print {}", node ),
            Self::PrintLn( node ) => write!( f, "println {}", node ),
        }
    }
}


// TODO create standardized error class for both lexing and parsing, with error codes and corresponding error messages
// TODO implement NOTE, HINT, HELP in error messages
#[derive( Debug )]
struct SyntaxError<'lexer> {
    pos: Position<'lexer>,
    err_msg: &'static str,
    help_msg: &'static str,
}

impl<'lexer> SyntaxError<'lexer> {
    fn display( &self, f: &mut std::fmt::Formatter<'_>, file_path: &str ) -> std::fmt::Result {
        let mut line_number_and_bar = format!( "{} |", self.pos.line.number );
        let visualization_padding = line_number_and_bar.len();
        let location_padding = visualization_padding - 1;

        let bar = format!( "\x1b[94m{:>visualization_padding$}\x1b[0m", "|" );
        line_number_and_bar = format!( "\x1b[94m{:>visualization_padding$}\x1b[0m", line_number_and_bar );

        let pointers_col = self.pos.token.col - 1;
        let pointers_len = self.pos.token.len;

        return write!( f,
            "\x1b[91;1m{}\x1b[0m: \x1b[97;1m{}\x1b[0m\
            \n\x1b[91m{:>location_padding$}\x1b[0m: {}:{}:{}\
            \n{}\
            \n{} {}\
            \n{} {:pointers_col$}\x1b[91m{:^>pointers_len$} {}\x1b[0m\n\n",
            "Error", self.err_msg,
            "in", file_path, self.pos.line.number, self.pos.token.col,
            bar,
            line_number_and_bar, self.pos.line,
            bar, "", "", self.help_msg
        );
    }
}

#[derive( Debug )]
struct SyntaxErrors<'program> {
    file_path: String,
    errors: Vec<SyntaxError<'program>>,
}

impl<'program> Display for SyntaxErrors<'program> {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        for error in &self.errors {
            error.display( f, &self.file_path )?;
        }

        return Ok( () );
    }
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
    value: Box<Node>,
}

// TODO introduce the notion of context:
    // parenthesis stack
    // blocks and scopes
        // variables, functions, etc.
#[derive( Debug )]
struct AST<'lexer> {
    tokens: LexerIter<'lexer>, // NOTE possibly remove this field
    nodes: Vec<Node>,
    definitions: Vec<Definition>,
}

impl<'lexer> TryFrom<&'lexer Lexer> for AST<'lexer> {
    type Error = SyntaxErrors<'lexer>;

    fn try_from( lexer: &'lexer Lexer ) -> Result<Self, Self::Error> {
        let mut this = Self {
            tokens: lexer.iter(),
            nodes: Vec::new(),
            definitions: Vec::new(),
        };

        let mut errors: Vec<SyntaxError> = Vec::new();
        this.tokens.next(); // skipping the SOF token

        while let Some( current ) = this.tokens.current().or_next( &mut this.tokens ) {
            let statement_result = match current.token.kind {
                // NOTE definitions are planned to be reworked, so this is just temporary
                TokenKind::Definition( _ ) => match this.variable_definition() {
                    Ok( _ ) => continue, // skipping this node since it is already in the definitions
                    Err( err ) => Err( err ),
                },
                TokenKind::Print | TokenKind::PrintLn => this.print(),
                TokenKind::True | TokenKind::False |
                TokenKind::Literal( _ ) |
                TokenKind::Bracket( BracketKind::OpenRound ) => this.expression( IdentifierExpansion::Keep ),
                TokenKind::Identifier( _ ) => match this.tokens.peek_next().bounded( &mut this.tokens, "" ) {
                    Ok( Some( next ) ) => match next.token.kind {
                        TokenKind::Equals => match this.variable_assignment() {
                            Ok( _ ) => continue,
                            Err( err ) => Err( err ),
                        },
                        _ => this.expression( IdentifierExpansion::Keep ),
                    },
                    Ok( None ) => continue,
                    Err( _ ) => this.expression( IdentifierExpansion::Keep ),
                },
                TokenKind::Bracket( BracketKind::CloseRound ) => {
                    this.tokens.next();
                    Err( SyntaxError {
                        pos: current,
                        err_msg: "invalid expression",
                        help_msg: "stray closed parenthesis"
                    } )
                },
                TokenKind::Bracket( BracketKind::OpenCurly ) => {
                    this.tokens.next();
                    continue;
                },
                TokenKind::Bracket( BracketKind::CloseCurly ) => {
                    this.tokens.next();
                    continue;
                },
                TokenKind::Op( _ ) => {
                    this.tokens.next();
                    Err( SyntaxError {
                        pos: current,
                        err_msg: "invalid expression",
                        help_msg: "stray binary operator"
                    } )
                },
                TokenKind::Equals => {
                    this.tokens.next();
                    Err( SyntaxError {
                        pos: current,
                        err_msg: "invalid assignment",
                        help_msg: "stray assignment"
                    } )
                },
                TokenKind::Comment( _ ) | TokenKind::SemiColon | TokenKind::Empty | TokenKind::EOF => {
                    this.tokens.next();
                    continue;
                },
                TokenKind::SOF | TokenKind::Unexpected { .. } => unreachable!(),
            };

            match statement_result {
                Ok( statement ) => this.nodes.push( statement ),
                Err( err ) => errors.push( err )
            }
        }

        if errors.is_empty() {
            return Ok( this );
        }
        else {
            return Err( SyntaxErrors { file_path: lexer.file_path.clone(), errors } );
        }
    }
}

impl<'lexer> AST<'lexer> {
    fn resolve<'definition>( &'definition self, name: &str ) -> Option<&'definition Definition> {
        for definition in &self.definitions {
            if definition.name == name {
                return Some( definition );
            }
        }

        return None;
    }

    fn resolve_mut<'definition>( &'definition mut self, name: &str ) -> Option<&'definition mut Definition> {
        for definition in &mut self.definitions {
            if definition.name == name {
                return Some( definition );
            }
        }

        return None;
    }


    fn semicolon( &mut self ) -> Result<(), SyntaxError<'lexer>> {
        let current = self.tokens.current().or_next( &mut self.tokens ).bounded( &mut self.tokens, "expected semicolon" )?.unwrap();
        let result = match current.token.kind {
            TokenKind::SemiColon => Ok( () ),
            _ => {
                let previous = self.tokens.peek_previous().bounded( &mut self.tokens, "expected semicolon" )?.unwrap();
                Err( SyntaxError {
                    pos: previous,
                    err_msg: "invalid expression",
                    help_msg: "expected semicolon after this token"
                } )
            },
        };

        self.tokens.next();
        return result;
    }


    fn variable_definition( &mut self ) -> Result<(), SyntaxError<'lexer>> {
        let definition_pos = self.tokens.current().unwrap();
        let kind = match definition_pos.token.kind {
            TokenKind::Definition( kind ) => kind.clone(),
            _ => unreachable!(),
        };

        let name_pos = self.tokens.next().bounded( &mut self.tokens, "expected identifier" )?.unwrap();
        let name = match &name_pos.token.kind {
            TokenKind::Identifier( name ) => Ok( name.clone() ),
            _ => Err( SyntaxError {
                pos: name_pos,
                err_msg: "invalid assignment",
                help_msg: "expected variable name"
            } ),
        };

        let equals_pos = self.tokens.next().bounded( &mut self.tokens, "expected equals" )?.unwrap();
        let equals = match equals_pos.token.kind {
            TokenKind::Equals => Ok( () ),
            _ => Err( SyntaxError {
                pos: name_pos,
                err_msg: "invalid assignment",
                help_msg: "expected '=' after variable name"
            } ),
        };

        let value_pos = self.tokens.next().bounded( &mut self.tokens, "expected expression" )?.unwrap();
        let value = match value_pos.token.kind {
            TokenKind::True | TokenKind::False |
            TokenKind::Literal( _ ) | TokenKind::Identifier( _ ) |
            TokenKind::Bracket( BracketKind::OpenRound ) => self.expression( IdentifierExpansion::Keep ),
            _ => Err( SyntaxError {
                pos: equals_pos,
                err_msg: "invalid assignment",
                help_msg: "expected expression after '='"
            } ),
        };

        let name = name?;
        let _equals = equals?;
        let value = value?;
        self.semicolon()?;

        return match self.resolve( &name ) {
            None => {
                let value = match kind {
                    DefinitionKind::Let | DefinitionKind::Var => value,
                    DefinitionKind::Const => Node::Literal( self.evaluate_node( &value ) ),
                };

                self.definitions.push( Definition { kind, name, value: Box::new( value ) } );
                Ok( () )
            },
            Some( _ ) => Err( SyntaxError {
                pos: name_pos,
                err_msg: "variable redefinition",
                help_msg: "was previously defined"
            } ),
        }
    }

    fn variable_assignment( &mut self ) -> Result<(), SyntaxError<'lexer>> {
        let name_pos = self.tokens.current().unwrap();
        let equals_pos = self.tokens.next().unwrap();

        let value_pos = self.tokens.next().bounded( &mut self.tokens, "expected expression" )?.unwrap();
        let value = match value_pos.token.kind {
            TokenKind::True | TokenKind::False |
            TokenKind::Literal( _ ) | TokenKind::Identifier( _ ) |
            TokenKind::Bracket( BracketKind::OpenRound ) => self.expression( IdentifierExpansion::Expand ),
            _ => Err( SyntaxError {
                pos: equals_pos,
                err_msg: "invalid assignment",
                help_msg: "expected expression after '='"
            } ),
        };

        let variable = match &name_pos.token.kind {
            TokenKind::Identifier( name ) => match self.resolve_mut( &name ) {
                Some( identifier ) => identifier,
                None => return Err( SyntaxError {
                    pos: name_pos,
                    err_msg: "variable redefinition",
                    help_msg: "was not previously defined"
                } )
            },
            _ => unreachable!()
        };

        let assignment = match variable.kind {
            DefinitionKind::Let | DefinitionKind::Const => return Err( SyntaxError {
                pos: name_pos,
                err_msg: "invalid assignment",
                help_msg: "was defined as immutable"
            } ),
            DefinitionKind::Var => value?,
        };

        variable.value = Box::new( assignment );
        return Ok( () );
    }


    fn print( &mut self ) -> Result<Node, SyntaxError<'lexer>> {
        let print_pos = self.tokens.current().unwrap();

        let argument_pos = self.tokens.next().bounded( &mut self.tokens, "expected print argument" )?.unwrap();
        let argument = match &argument_pos.token.kind {
            TokenKind::True | TokenKind::False |
            TokenKind::Literal( _ ) | TokenKind::Identifier( _ ) |
            TokenKind::Bracket( BracketKind::OpenRound ) => self.expression( IdentifierExpansion::Keep ),
            _ => Err( SyntaxError {
                pos: argument_pos,
                err_msg: "invalid print argument",
                help_msg: "expected an expression"
            } )
        };

        let argument = argument?;
        self.semicolon()?;

        let print = match print_pos.token.kind {
            TokenKind::Print => Node::Print( Box::new( argument ) ),
            TokenKind::PrintLn => Node::PrintLn( Box::new( argument ) ),
            _ => unreachable!(),
        };

        return Ok( print );
    }


    fn factor( &mut self, expansion: IdentifierExpansion ) -> Result<Node, SyntaxError<'lexer>> {
        let pos = self.tokens.current().or_next( &mut self.tokens ).bounded( &mut self.tokens, "expected expression" )?.unwrap();
        let result = match &pos.token.kind {
            // FIX forbid implicit conversions
            TokenKind::True => Ok( Node::Literal( Type::Bool { value: true } ) ),
            TokenKind::False => Ok( Node::Literal( Type::Bool { value: false } ) ),
            TokenKind::Literal( literal ) => Ok( Node::Literal( literal.clone() ) ),
            TokenKind::Identifier( name ) => match self.resolve( name ) {
                Some( definition ) => match expansion {
                    IdentifierExpansion::Expand => Ok( *definition.value.clone() ),
                    IdentifierExpansion::Keep => Ok( Node::Identifier( name.clone() ) ),
                },
                None => Err( SyntaxError {
                    pos,
                    err_msg: "variable not defined",
                    help_msg: "was not previously defined"
                } )
            },
            TokenKind::Bracket( BracketKind::OpenRound ) => {
                let expression_start_pos = self.tokens.next().bounded( &mut self.tokens, "expected expression" )?.unwrap();
                match expression_start_pos.token.kind {
                    TokenKind::Bracket( BracketKind::CloseRound ) => {
                        Err( SyntaxError {
                            pos: expression_start_pos,
                            err_msg: "invalid expression",
                            help_msg: "empty expressions are not allowed"
                        } )
                    },
                    _ => {
                        let expression = self.expression( expansion )?;
                        let close_bracket_pos = self.tokens.current().or_next( &mut self.tokens ).bounded( &mut self.tokens, "expected closed parenthesis" )?.unwrap();
                        match close_bracket_pos.token.kind {
                            TokenKind::Bracket( BracketKind::CloseRound ) => {
                                Ok( expression )
                            },
                            _ => Err( SyntaxError {
                                pos,
                                err_msg: "invalid expression",
                                help_msg: "unclosed parenthesis"
                            } )
                        }
                    }
                }
            },
            _ => Err( SyntaxError {
                pos,
                err_msg: "invalid expression",
                help_msg: "expected number literal"
            } ),
        };

        self.tokens.next();
        return result;
    }

    // FIX dealing with missing semicolons or missing operators
        // TODO move it outside the expression
        // IDEA create two versions of this function, one that checks for the closing semicolon, and one that doesn't
    fn operator( &mut self, ops: &[OpKind] ) -> Result<Option<OpKind>, SyntaxError<'lexer>> {
        let current_pos = self.tokens.current().or_next( &mut self.tokens ).bounded( &mut self.tokens, "expected expression or semicolon" )?.unwrap();
        let result = match current_pos.token.kind {
            TokenKind::Op( op ) => match ops.contains( &op ) {
                true => Ok( Some( op.clone() ) ),
                false => Ok( None ),
            },
            TokenKind::Bracket( BracketKind::CloseRound ) | TokenKind::SemiColon => Ok( None ),
             _ => Err( SyntaxError {
                pos: self.tokens.peek_previous().bounded( &mut self.tokens, "expected expression" )?.unwrap(),
                err_msg: "invalid expression or missing semicolon",
                help_msg: "expected an operator after this token to complete the expression, or a ';' to end the statement"
            } ),
        };

        if let Ok( Some( _ ) ) = result {
            self.tokens.next();
        }
        return result;
    }

    fn exponentiation( &mut self, expansion: IdentifierExpansion ) -> Result<Node, SyntaxError<'lexer>> {
        let mut lhs = self.factor( expansion )?;

        while let Some( op ) = self.operator( &[OpKind::Pow] )? {
            let rhs = self.factor( expansion )?;
            lhs = Node::Expression { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn multiplication_or_division( &mut self, expansion: IdentifierExpansion ) -> Result<Node, SyntaxError<'lexer>> {
        let mut lhs = self.exponentiation( expansion )?;

        while let Some( op ) = self.operator( &[OpKind::Times, OpKind::Divide] )? {
            let rhs = self.exponentiation( expansion )?;
            lhs = Node::Expression { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn math( &mut self, expansion: IdentifierExpansion ) -> Result<Node, SyntaxError<'lexer>> {
        let mut lhs = self.multiplication_or_division( expansion )?;

        while let Some( op ) = self.operator( &[OpKind::Plus, OpKind::Minus] )? {
            let rhs = self.multiplication_or_division( expansion )?;
            lhs = Node::Expression { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn expression( &mut self, expansion: IdentifierExpansion ) -> Result<Node, SyntaxError<'lexer>> {
        let mut lhs = self.math( expansion )?;

        let ops = [
            OpKind::Equals, OpKind::NotEquals,
            OpKind::Greater, OpKind::GreaterOrEquals,
            OpKind::Less, OpKind::LessOrEquals,
            OpKind::Compare
        ];

        while let Some( op ) = self.operator( &ops )? {
            let rhs = self.math( expansion )?;
            lhs = Node::Expression { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }
}

// After construction
impl<'lexer> AST<'lexer> {
    fn evaluate_node( &self, node: &Node ) -> Type {
        return match node {
            Node::Literal( value ) => value.clone(),
            Node::Expression{ lhs, op, rhs } => {
                let lhs: i64 = self.evaluate_node( lhs ).into();
                let rhs: i64 = self.evaluate_node( rhs ).into();

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
            Node::Identifier( name ) => self.evaluate_node( &*self.resolve( name ).unwrap().value ),
            Node::Print( _ ) | Node::PrintLn( _ ) => unreachable!(),
        }
    }

    fn compile_node( &self, node: &Node, asm: &mut String ) {
        /* NOTE
            it is literally possible to copy paste the entire literal expression (won't work with variables)
            in the generated asm file (only if it doesn't include exponentiations)

            // IDEA check if the expression contains non inlineable expression, if not just copy paste the literal expression
        */
        match node {
            Node::Print( argument ) => {
                let value = match &**argument {
                    Node::Literal( _ ) | Node::Expression{ .. } => &**argument,
                    Node::Identifier( name ) => &*self.resolve( name ).unwrap().value,
                    Node::Print( _ ) | Node::PrintLn( _ ) => unreachable!(),
                };

                let value_type = match value {
                    Node::Literal( literal ) => literal,
                    Node::Expression { op, .. } => match op {
                        OpKind::Pow | OpKind::Times | OpKind::Divide |
                        OpKind::Plus | OpKind::Minus | OpKind::Compare => &Type::I64 { value: 0 },
                        OpKind::Equals | OpKind::NotEquals |
                        OpKind::Greater | OpKind::GreaterOrEquals |
                        OpKind::Less | OpKind::LessOrEquals => &Type::Bool { value: false }
                    }
                    Node::Print( _ ) | Node::PrintLn( _ ) | Node::Identifier( _ ) => unreachable!(),
                };

                let print_asm = match value_type {
                    Type::I64 { .. } =>
                        "\n pop rax\
                        \n mov rdi, rax\
                        \n mov rsi, 10\
                        \n call int_toStr\
                        \n\
                        \n mov rdi, stdout\
                        \n mov rsi, rax\
                        \n mov rdx, rdx\
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
                        "\n pop rax\
                        \n pop rdx\
                        \n mov rdi, stdout\
                        \n mov rsi, rax\
                        \n mov rdx, rdx\
                        \n mov rax, SYS_write\
                        \n syscall",
                };

                asm.push_str( &format!( " ; {}\n", node ) );
                self.compile_node( value, asm );
                asm.push_str( &format!( "{}\n\n", print_asm ) );
            },
            Node::Literal( literal ) => match &literal {
                Type::I64 { value } => asm.push_str( &format!( " push {}\n", value ) ),
                Type::Char { value } => asm.push_str( &format!( " push {}\n", value ) ),
                Type::Bool { value } => asm.push_str( &format!( " push {}\n", value ) ),
            },
            Node::Expression { lhs, op, rhs } => {
                self.compile_node( lhs, asm );
                self.compile_node( rhs, asm );
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

                    OpKind::Equals | OpKind::NotEquals |
                    OpKind::Greater | OpKind::GreaterOrEquals |
                    OpKind::Less | OpKind::LessOrEquals => {
                        let asm_op = match op {
                            OpKind::Equals => "cmove",
                            OpKind::NotEquals => "cmovne",
                            OpKind::Greater => "cmovg",
                            OpKind::GreaterOrEquals => "cmovge",
                            OpKind::Less => "cmovl",
                            OpKind::LessOrEquals => "cmovle",
                            _ => unreachable!(),
                        };

                        format!(
                            "\n pop rsi\
                            \n pop rdi\
                            \n mov rbx, true\
                            \n mov rax, false\
                            \n cmp rdi, rsi\
                            \n {} rax, rbx\
                            \n mov rbx, true_str_len\
                            \n mov rdx, false_str_len\
                            \n {} rdx, rbx\
                            \n push rdx\
                            \n push rax",
                            asm_op,
                            asm_op
                        )
                    },

                    OpKind::Compare =>
                        "\n pop rsi
                        \n pop rdi\
                        \n mov rax, LESS\
                        \n mov rbx, EQUAL\
                        \n mov rdx, GREATER\
                        \n cmp rdi, rsi\
                        \n cmove rax, rbx\
                        \n cmovg rax, rdx".to_string(),
                };

                asm.push_str( &format!( " ; {}", node ) );
                asm.push_str( &format!( "{}\n\n", op_asm ) );
            },
            Node::Identifier( name ) => self.compile_node( &*self.resolve( name ).unwrap().value, asm ),
            Node::PrintLn( _ ) => unreachable!(),
        }
    }

    fn interpret( &self, file_path: &str ) {
        println!( "\x1b[92;1mIntepreting\x1b[0m: {}", file_path );

        for node in &self.nodes {
            match node {
                Node::Print( argument ) => self.evaluate_node( argument ).display(),
                Node::PrintLn( argument ) => {
                    self.evaluate_node( argument ).display();

                    let newline = Node::Literal( Type::Char { value: '\n' as u8 } );
                    self.evaluate_node( &newline ).display();
                }
                _ => continue,
            }
        }
    }

    fn compile( &self, file_path: &str ) -> Result<PathBuf, ()> {
        let src_file_path = Path::new( file_path );
        println!( "\x1b[92;1mBuilding\x1b[0m: {}", src_file_path.display() );

        let asm_file_path = src_file_path.with_extension( "asm" );
        let mut asm_file = BufWriter::new( File::create( &asm_file_path ).unwrap() );

        let preamble =
r#"global _start

section .rodata
 stdout: equ 1
 SYS_write: equ 1
 SYS_exit: equ 60
 EXIT_SUCCESS: equ 0
 newline: db 10

 I64_MIN: equ 1 << 63
 I64_MAX: equ ~I64_MIN
 INT_MAX_DIGITS: equ 64

 true: db "true", 0
 true_str_len: equ $ - true

 false: db "false", 0
 false_str_len: equ $ - false

 LESS: equ -1
 EQUAL: equ 0
 GREATER: equ 1

section .data
 int_str: times INT_MAX_DIGITS + 1 db 0
 int_str_bufsize: equ $ - int_str
 int_str_len: equ int_str_bufsize - 1"#;

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

        let mut user_program = String::new();

        for node in &self.nodes {
            match node {
                Node::Print( _ ) => self.compile_node( node, &mut user_program ),
                Node::PrintLn( argument ) => {
                    let print = Node::Print( argument.clone() );
                    self.compile_node( &print, &mut user_program );

                    let newline = Node::Print( Box::new( Node::Literal( Type::Char { value: '\n' as u8 } ) ) );
                    self.compile_node( &newline, &mut user_program );
                }
                _ => continue,
            }
        }

        let program = format!(
r"{}

section .text
{}

{}


_start:
{}
{}
", preamble, int_to_str, int_pow, user_program, sys_exit );

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

    fn run( &self, file_path: &str ) -> Result<(), ()> {
        let executable_file_path = self.compile( &file_path )?;
        println!( "\x1b[92;1mRunning\x1b[0m: {}", executable_file_path.display() );

        let output = Command::new( format!( "{}", executable_file_path.display() ) ).output().unwrap();
        print!( "{}", String::from_utf8_lossy( &output.stdout ) );

        return Ok( () );
    }
}


// IDEA add man page
fn print_usage() {
    println!( r"
Blitzlang compiler, version {}

Usage: blitz [Options] [Run mode] file.blz

Options:
    -h, --help              Display this message

Run mode:
    build     <file.blz>    Compile the program down to a binary executable
    run       <file.blz>    Compile and run the generated binary executable
    interpret <file.blz>    Run the program in interpret mode
",
        env!( "CARGO_PKG_VERSION" )
    );
}

fn main() -> ExitCode {
    let mut args: Vec<String> = env::args().collect();

    // to quickly debug
    args.push( "interpret".to_string() );
    // args.push( "build".to_string() );
    // args.push( "run".to_string() );
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
            _ => if source_file_path.is_none() {
                    source_file_path = Some( arg );
                }
                else {
                    eprintln!( "\x1b[91;1mError\x1b[0m: too many source file paths provided" );
                    return ExitCode::FAILURE;
                },
        }
    }

    if !interpret_flag && !build_flag && !run_flag {
        eprintln!( "\x1b[91;1mError\x1b[0m: no run mode command provided" );
        return ExitCode::FAILURE;
    }
    else if interpret_flag && (build_flag || run_flag) {
        eprintln!( "\x1b[91;1mError\x1b[0m: canno interpret and build/run at the same time" );
        return ExitCode::FAILURE;
    }
    else if build_flag && run_flag {
        eprintln!( "\x1b[91;1mError\x1b[0m: build and run commands cannot be used together" );
        return ExitCode::FAILURE;
    }

    let source_file_path = match source_file_path {
        Some( path ) => path,
        None => {
            eprintln!( "\x1b[91;1mError\x1b[0m: no source file path provided" );
            return ExitCode::FAILURE;
        }
    };

    let source_file = match File::open( &source_file_path ) {
        Ok( file ) => {
            if file.metadata().unwrap().is_dir() {
                eprintln!( "\x1b[91;1mError\x1b[0m: provided file was a directory" );
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

            eprintln!( "\x1b[91;1mError\x1b[0m: could not open file '{}'!\nCause: {}", &source_file_path, cause );
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
        ast.interpret( &source_file_path );
    }
    else if build_flag {
        let _ = ast.compile( &source_file_path );
    }
    else if run_flag {
        let _ = ast.run( &source_file_path );
    }

    return ExitCode::SUCCESS;
}
