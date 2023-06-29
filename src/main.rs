// TODO create standardized module for outputting colored text
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

impl Into<i64> for &Type {
    fn into( self ) -> i64 {
        return match self {
            Type::I64 { value } => value.to_owned().into(),
            Type::Char { value } => value.to_owned().into(),
            Type::Bool { value } => value.to_owned().into(),
        }
    }
}

impl Type {
    fn actual( &self ) {
        match self {
            Self::I64 { value } => print!( "{}", value ),
            Self::Char { value } => print!( "{}", *value as char ),
            Self::Bool { value } => print!( "{}", value ),
        }
    }
}


#[derive( Debug, Clone, PartialEq )]
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


#[derive( Debug, Clone )]
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


#[derive( Debug, Clone )]
enum TokenKind {
    Unexpected{ text: String, err_msg: &'static str, help_msg: &'static str },

    // Whitespace
    Comment( String ),

    // Symbols
    OpenRoundBracket,
    CloseRoundBracket,
    // OpenSquareBracket,
    // CloseSquareBracket,
    // OpenCurlyBracket,
    // CloseCurlyBracket,

    Equals,
    // Colon,
    SemiColon,

    // Identifiers
    Literal( Type ),
    Identifier( String ),
    Definition( DefinitionKind ),

    // Keywords
    Print, // temporary way of printing values
    True,
    False,
    // Entry,
    // Return,

    // Operators
    Op( OpKind ),

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

            Self::OpenRoundBracket => write!( f, "(" ),
            Self::CloseRoundBracket => write!( f, ")" ),

            Self::Equals => write!( f, "=" ),
            Self::SemiColon => write!( f, ";" ),

            Self::Literal( literal ) => write!( f, "{}", literal ),
            Self::Identifier( name ) => write!( f, "{}", name ),
            Self::Definition( kind ) => write!( f, "{}", kind ),

            Self::Print => write!( f, "print" ),
            Self::True => write!( f, "true" ),
            Self::False => write!( f, "false" ),

            Self::Op( op ) => write!( f, "{}", op ),

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


#[derive( Debug )]
struct Line {
    number: usize,
    tokens: Vec<Token>,
}

impl Display for Line {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        if self.tokens.is_empty() {
            return write!( f, "" );
        }

        // leading whitespaces
        write!( f, "{}", " ".repeat( self.tokens[ 0 ].col - 1 ) )?;

        let mut tokens = self.tokens.iter().peekable();
        if self.number == 1 {
            tokens.next(); // skipping the SOF
        }

        while let Some( token ) = tokens.next() {
            let spaces_before_next_token = match token.kind {
                TokenKind::Comment( _ ) | TokenKind::EOF => 0,
                _ => match tokens.peek() {
                    Some( next_token ) => next_token.col - (token.col + token.len),
                    None => 0,
                }
            };

            write!( f, "{}{}", token.kind, " ".repeat( spaces_before_next_token ) )?;
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

fn display_error( f: &mut std::fmt::Formatter<'_>, file_path: &str, line_text: &str, line_number: usize, token_col: usize, token_len: usize, err_msg: &str, help_msg: &str ) -> std::fmt::Result {
    let gutter_padding_amount = line_number.ilog10() as usize + 1;
    let gutter_padding = " ".repeat( gutter_padding_amount );
    let pointers_padding = " ".repeat( token_col - 1 );
    let pointers = "^".repeat( token_len );
    let bar = "\x1b[94m|\x1b[0m";

    let error_visualization = &format!(
        " {} {}\
        \n \x1b[94m{: >gutter_padding_amount$}\x1b[0m {} {}\
        \n {} {} {}\x1b[91m{} {}\x1b[0m",
        gutter_padding, bar,
        line_number, bar, line_text,
        gutter_padding, bar, pointers_padding, pointers, help_msg
    );

    return writeln!( f,
        "\x1b[91;1mError\x1b[0m [L]: \x1b[1m{}\x1b[0m\
        \n {} \x1b[91min\x1b[0m: {}:{}:{}\n{}\n",
        err_msg,
        gutter_padding, file_path, line_number, token_col, error_visualization
    );
}

impl Display for Lexer {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        for line in &self.lines {
            let line_text = format!( "{}", line );

            for token in &line.tokens {
                if let TokenKind::Unexpected { err_msg, help_msg, .. } = &token.kind {
                    display_error( f, &self.file_path, &line_text, line.number, token.col, token.len, err_msg, help_msg )?;
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
        let (file_path, source_file) = (src.0, src.1);
        let mut errors: Vec<Line> = Vec::new();
        let mut lines: Vec<Line> = Vec::new();
        let mut number: usize = 1;

        let mut src_lines = BufReader::new( source_file );
        let mut src_line = String::new();

        let mut tokens: Vec<Token> = vec![ Token { col: 1, len: 1, kind: TokenKind::SOF } ];
        let mut token_text = String::new();

        while let Ok( chars_read ) = src_lines.read_line( &mut src_line ) {
            // reached EOF on an empty line
            if chars_read == 0 {
                tokens.push( Token { col: 1, len: 1, kind: TokenKind::Empty  } );
                tokens.push( Token { col: 1, len: 1, kind: TokenKind::EOF } );

                lines.push( Line { number, tokens } );
                break;
            }

            let mut line_contains_errors = false;
            let mut col = 1;

            let mut src = src_line.chars().peekable();
            // IDEA try extracting this to a separate function
            loop {
                let token: Token = match Self::next( &mut src ) {
                    Ok( None ) => break,
                    Err( err ) => match &err {
                        TokenKind::Unexpected { text, err_msg: _, help_msg: _ } => {
                            let len = text.len();
                            let token = Token { col, len, kind: err };
                            col += len - 1;
                            token
                        },
                        _ => unreachable!(),
                    },
                    Ok( Some( ch ) ) => match ch {
                        // TODO consume until not whitespace
                        // ignore whitespace
                        _ if ch.is_ascii_whitespace() => {
                            col += 1;
                            continue;
                        },
                        '(' => Token { col, len: 1, kind: TokenKind::OpenRoundBracket },
                        ')' => Token { col, len: 1, kind: TokenKind::CloseRoundBracket },
                        // '[' => Token { kind: TokenKind::OpenSquareBracket, col },
                        // ']' => Token { kind: TokenKind::CloseSquareBracket, col },
                        // '{' => Token { kind: TokenKind::OpenCurlyBracket, col },
                        // '}' => Token { kind: TokenKind::CloseCurlyBracket, col },
                        '=' => match Self::peek_next( &mut src ) {
                            Ok( Some( '=' ) ) => {
                                let _ = Self::next( &mut src );

                                let token = Token { col, len: 2, kind: TokenKind::Op( OpKind::Equals ) };
                                col += 1;
                                token
                            },
                            _ => Token { col, len: 1, kind: TokenKind::Equals },
                        },
                        '!' => match Self::peek_next( &mut src ) {
                            Ok( Some( '=' ) ) => {
                                let _ = Self::next( &mut src );

                                let token = Token { col, len: 2, kind: TokenKind::Op( OpKind::NotEquals ) };
                                col += 1;
                                token
                            },
                            _ => Token{ col, len: 1, kind: TokenKind::Unexpected {
                                text: ch.to_string(),
                                err_msg: "unexpected character",
                                help_msg: "unrecognized"
                            } },
                        },
                        '>' => match Self::peek_next( &mut src ) {
                            Ok( Some( '=' ) ) => {
                                let _ = Self::next( &mut src );

                                let token = Token { col, len: 2, kind: TokenKind::Op( OpKind::GreaterOrEquals ) };
                                col += 1;
                                token
                            },
                            _ => Token { col, len: 1, kind: TokenKind::Op( OpKind::Greater ) },
                        },
                        '<' => match Self::peek_next( &mut src ) {
                            Ok( Some( '=' ) ) => {
                                let _ = Self::next( &mut src );

                                match Self::peek_next( &mut src ) {
                                    Ok( Some( '>' ) ) => {
                                        let _ = Self::next( &mut src );

                                        let token = Token { col, len: 3, kind: TokenKind::Op( OpKind::Compare ) };
                                        col += 2;
                                        token
                                    },
                                    _ => {
                                        let token = Token { col, len: 2, kind: TokenKind::Op( OpKind::LessOrEquals ) };
                                        col += 1;
                                        token
                                    }
                                }
                            },
                            _ => Token { col, len: 1, kind: TokenKind::Op( OpKind::Less ) },
                        },
                        '^' => Token { col, len: 1, kind: TokenKind::Op( OpKind::Pow ) },
                        '*' => Token { col, len: 1, kind: TokenKind::Op( OpKind::Times ) },
                        '/' => Token { col, len: 1, kind: TokenKind::Op( OpKind::Divide ) },
                        '+' => Token { col, len: 1, kind: TokenKind::Op( OpKind::Plus ) },
                        '-' => Token { col, len: 1, kind: TokenKind::Op( OpKind::Minus ) },
                        // ':' => Token { kind: TokenKind::Colon, col },
                        ';' => Token { col, len: 1, kind: TokenKind::SemiColon },
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

                            let len = token_text.len();
                            let token = Token { col, len, kind };
                            col += len - 1;
                            token
                        },
                        '#' => {
                            token_text.clear();
                            token_text.push( ch );

                            // consume the rest of the tokens in the current line
                            while let Some( next ) = src.next_if( |c| *c != '\n' ) {
                                token_text.push( next );
                            }

                            let len = token_text.len();
                            let token = Token { col, len, kind: TokenKind::Comment( token_text.clone() ) };
                            col += len - 1;
                            token
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
                                    Ok( value ) => TokenKind::Literal( Type::I64{ value } ),
                                    Err( _ ) => TokenKind::Unexpected {
                                        text: token_text.clone(),
                                        err_msg: "expected number literal",
                                        help_msg: "overflows a 64 bit integer [-9223372036854775808, 9223372036854775807]"
                                    },
                                }
                            }
                            else {
                                TokenKind::Unexpected{
                                    text: token_text.clone(),
                                    err_msg: "expected number literal",
                                    help_msg: "not a number literal"
                                }
                            };

                            let len = token_text.len();
                            let token = Token { col, len, kind };
                            col += len - 1;
                            token
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
                                // "return" => TokenKind::Return,
                                _ => TokenKind::Identifier( token_text.clone() )
                            };

                            let len = token_text.len();
                            let token = Token { col, len, kind };
                            col += len - 1;
                            token
                        },
                        _ => Token{ col, len: 1, kind: TokenKind::Unexpected {
                            text: ch.to_string(),
                            err_msg: "unexpected character",
                            help_msg: "unrecognized"
                        } }
                    },
                };

                if let TokenKind::Unexpected { .. } = token.kind {
                    line_contains_errors = true;
                }

                tokens.push( token );
                col += 1;
            }

            if tokens.is_empty() {
                tokens.push( Token { col: 1, len: 1, kind: TokenKind::Empty  } );
            }

            let file_ended_at_line_end = !src_line.ends_with( "\n" );
            if file_ended_at_line_end {
                tokens.push( Token { col, len: 1, kind: TokenKind::EOF } );
            }

            let line = Line { number, tokens: tokens.clone() };
            if line_contains_errors {
                errors.push( line );
            }
            else {
                lines.push( line );
            }

            if file_ended_at_line_end {
                break;
            }

            src_line.clear();
            tokens.clear();
            number += 1;
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


// IDEA introduce the current movement direction, to allow for better chaining of methods (ie. non_eof, or_next/or_previous versions of methods)
#[derive( Debug )]
struct LexerIter<'lexer> {
    lexer: &'lexer Lexer,
    line: usize,
    token: usize,
}

type LexerIterItem<'lexer> = (&'lexer Line, &'lexer Token);

// IDEA create methods for iterating over all tokens, even whitespace
impl<'lexer> LexerIter<'lexer> {
    fn current( &self ) -> Option<LexerIterItem<'lexer>> {
        let line = self.lexer.lines.get( self.line )?;
        let token = line.tokens.get( self.token )?;

        return Some( (line, token) );
    }

    fn current_or_next( &mut self ) -> Option<LexerIterItem<'lexer>> {
        let (line, token) = self.current()?;
        return match token.kind {
            TokenKind::Comment( _ ) | TokenKind::Empty => self.next(),
            _ => Some( (line, token) ),
        }
    }

    fn current_or_previous( &mut self ) -> Option<LexerIterItem<'lexer>> {
        let (line, token) = self.current()?;
        return match token.kind {
            TokenKind::Comment( _ ) | TokenKind::Empty => self.previous(),
            _ => Some( (line, token) ),
        }
    }


    fn next( &mut self ) -> Option<LexerIterItem<'lexer>> {
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

        return match token.kind {
            TokenKind::Comment( _ ) | TokenKind::Empty => self.next(),
            _ => Some( (line, token) ),
        }
    }

    fn previous( &mut self ) -> Option<LexerIterItem<'lexer>> {
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

        return match token.kind {
            TokenKind::Comment( _ ) | TokenKind::Empty => self.previous(),
            _ => Some( (line, token) ),
        }
    }


    fn peek_next( &mut self ) -> Option<LexerIterItem<'lexer>> {
        let (starting_line, starting_token) = (self.line, self.token);
        let item = self.next();
        (self.line, self.token) = (starting_line, starting_token);
        return item;
    }

    fn peek_previous( &mut self ) -> Option<LexerIterItem<'lexer>> {
        let (starting_line, starting_token) = (self.line, self.token);
        let item = self.previous();
        (self.line, self.token) = (starting_line, starting_token);
        return item;
    }
}


trait BoundedLexerItem<'lexer> {
    fn bounded( &self, tokens: &mut LexerIter<'lexer>, err_msg: &'static str ) -> Result<Option<LexerIterItem<'lexer>>, SyntaxError<'lexer>>;
}

impl<'lexer> BoundedLexerItem<'lexer> for Option<LexerIterItem<'lexer>> {
    fn bounded( &self, tokens: &mut LexerIter<'lexer>, err_msg: &'static str ) -> Result<Option<LexerIterItem<'lexer>>, SyntaxError<'lexer>> {
        return match self {
            Some( (line, token) ) => match token.kind {
                TokenKind::EOF => {
                    // we are always sure that there is at least the SOF token before the EOF token, so we can safely unwrap
                    let (previous_line, previous_token) = tokens.peek_previous().unwrap();
                    Err( SyntaxError {
                        line: previous_line,
                        token: previous_token,
                        msg: err_msg,
                        help_msg: "file ended after here instead"
                    } )
                },
                TokenKind::SOF => {
                    // we are always sure that there is at least the EOF token after the SOF token, so we can safely unwrap
                    let (next_line, next_token) = tokens.peek_next().unwrap();
                    Err( SyntaxError {
                        line: next_line,
                        token: next_token,
                        msg: err_msg,
                        help_msg: "file ended after here instead"
                    } )
                },
                _ => Ok( Some( (line, token) ) ),
            }
            None => Ok( None ),
        }
    }
}


#[derive( Debug, Clone )]
struct Definition {
    kind: DefinitionKind,
    name: String,
    value: Box<Node>,
}

type Definitions = Vec<Definition>;

trait Resolve {
    fn resolve<'ast>( &'ast self, name: &str ) -> Option<&'ast Definition>;
}

impl Resolve for Definitions {
    fn resolve<'ast>( &'ast self, name: &str ) -> Option<&'ast Definition> {
        for definition in self {
            if definition.name == name {
                return Some( definition );
            }
        }

        return None;
    }
}


// TODO introduce Unexpected Node to let the parsing of the expression continue untill finished, and the reporting errors
#[derive( Debug, Clone )]
enum Node {
    Literal( Type ),
    Expression{ lhs: Box<Node>, op: OpKind, rhs: Box<Node> },
    Identifier( String ),
    Print( Box<Node> ),
}

impl Display for Node {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Literal( literal ) => write!( f, "{}", literal ),
            Self::Expression { lhs, op, rhs } => write!( f, "({} {} {})", lhs, op, rhs ),
            Self::Identifier( name ) => write!( f, "{}", name ),
            Self::Print( node ) => write!( f, "print {}", node ),
        }
    }
}


// TODO create standardized error class for both lexing and parsing, with error codes and corresponding error messages
// TODO implement NOTE, HINT, HELP in error messages
#[derive( Debug )]
struct SyntaxError<'line> {
    line: &'line Line,
    token: &'line Token,
    msg: &'static str,
    help_msg: &'static str,
}

#[derive( Debug )]
struct SyntaxErrors<'program> {
    file_path: String,
    errors: Vec<SyntaxError<'program>>,
}

impl<'program> Display for SyntaxErrors<'program> {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        for error in &self.errors {
            let line_text = format!( "{}", error.line );
            display_error( f, &self.file_path, &line_text, error.line.number, error.token.col, error.token.len, error.msg, error.help_msg )?;
        }

        return Ok( () );
    }
}


// TODO introduce the notion of context:
    // parenthesis stack
    // blocks and scopes
        // variables, functions, etc.
#[derive( Debug )]
struct AST<'lexer> {
    tokens: LexerIter<'lexer>,
    nodes: Vec<Node>,
    definitions: Definitions,
}

impl<'lexer> TryFrom<&'lexer Lexer> for AST<'lexer> {
    type Error = SyntaxErrors<'lexer>;

    fn try_from( lexer: &'lexer Lexer ) -> Result<Self, Self::Error> {
        let mut ast = Self { tokens: lexer.iter(), nodes: Vec::new(), definitions: Vec::new() };
        if lexer.lines.is_empty() {
            return Ok( ast );
        }

        let mut errors: Vec<SyntaxError> = Vec::new();
        ast.tokens.next(); // skipping the SOF token
        while let Some( (line, token) ) = ast.tokens.current_or_next() {
            let statement_result = match &token.kind {
                // NOTE definitions are planned to be reworked, so this is just temporary
                TokenKind::Definition( _ ) => match ast.variable_definition() {
                    Ok( _ ) => continue, // skipping this node since it is already in the definitions
                    Err( err ) => Err( err ),
                },
                TokenKind::Print => ast.print(),
                TokenKind::True | TokenKind::False | TokenKind::Literal( _ ) |
                TokenKind::Identifier( _ ) |
                TokenKind::OpenRoundBracket => ast.expression(),
                TokenKind::CloseRoundBracket => {
                    ast.tokens.next();
                    Err( SyntaxError {
                        line,
                        token,
                        msg: "invalid expression",
                        help_msg: "stray closed parenthesis"
                    } )
                },
                TokenKind::Op( _ ) => {
                    ast.tokens.next();
                    Err( SyntaxError {
                        line,
                        token,
                        msg: "invalid expression",
                        help_msg: "stray binary operator"
                    } )
                },
                TokenKind::Equals => {
                    ast.tokens.next();
                    Err( SyntaxError {
                        line,
                        token,
                        msg: "invalid assignment",
                        help_msg: "stray assignment"
                    } )
                },
                TokenKind::Comment( _ ) | TokenKind::SemiColon | TokenKind::Empty | TokenKind::EOF => {
                    ast.tokens.next();
                    continue;
                },
                TokenKind::SOF | TokenKind::Unexpected { .. } => unreachable!(),
            };

            match statement_result {
                Ok( statement ) => ast.nodes.push( statement ),
                Err( err ) => errors.push( err )
            }
        }

        if errors.is_empty() {
            return Ok( ast );
        }
        else {
            return Err( SyntaxErrors { file_path: lexer.file_path.clone(), errors } );
        }
    }
}

impl<'lexer> AST<'lexer> {
    fn semicolon( &mut self ) -> Result<(), SyntaxError<'lexer>> {
        let (_line, token) = self.tokens.current_or_next().bounded( &mut self.tokens, "expected semicolon" )?.unwrap();
        let result = match token.kind {
            TokenKind::SemiColon => Ok( () ),
            _ => {
                let (previous_line, previous_token) = self.tokens.peek_previous().bounded( &mut self.tokens, "expected semicolon" )?.unwrap();
                Err( SyntaxError {
                    line: previous_line,
                    token: previous_token,
                    msg: "invalid expression",
                    help_msg: "expected semicolon after this token"
                } )
            },
        };

        self.tokens.next();
        return result;
    }

    fn variable_definition( &mut self ) -> Result<(), SyntaxError<'lexer>> {
        let (_definition_kind_line, definition_kind_token) = self.tokens.current().unwrap();
        let kind = match &definition_kind_token.kind {
            TokenKind::Definition( kind ) => kind.clone(),
            _ => unreachable!(),
        };

        let (identifier_line, identifier_token) = self.tokens.next().bounded( &mut self.tokens, "expected identifier" )?.unwrap();
        let name = match &identifier_token.kind {
            TokenKind::Identifier( name ) => Ok( name.clone() ),
            _ => Err( SyntaxError {
                line: identifier_line,
                token: identifier_token,
                msg: "invalid assignment",
                help_msg: "expected variable name"
            } ),
        };

        let (equals_line, equals_token) = self.tokens.next().bounded( &mut self.tokens, "expected equals" )?.unwrap();
        let equals = match equals_token.kind {
            TokenKind::Equals => Ok( () ),
            _ => Err( SyntaxError {
                line: identifier_line,
                token: identifier_token,
                msg: "invalid assignment",
                help_msg: "expected '=' after variable name"
            } ),
        };

        let (_value_line, value_token) = self.tokens.next().bounded( &mut self.tokens, "expected expression" )?.unwrap();
        let value = match value_token.kind {
            TokenKind::True | TokenKind::False | TokenKind::Literal( _ ) |
            TokenKind::Identifier( _ ) | TokenKind::OpenRoundBracket => self.expression(),
            _ => Err( SyntaxError {
                line: equals_line,
                token: equals_token,
                msg: "invalid assignment",
                help_msg: "expected expression after '='"
            } ),
        };

        let name = name?;
        let _equals = equals?;
        let value = value?;
        self.semicolon()?;

        return match self.definitions.resolve( &name ) {
            None => {
                self.definitions.push( Definition { kind, name, value: Box::new( value ) } );
                Ok( () )
                // return Ok( Node::Definition( self.definitions.last().unwrap().clone() ) );
            },
            Some( _ ) => Err( SyntaxError {
                line: identifier_line,
                token: identifier_token,
                msg: "variable redefinition",
                help_msg: "was previously defined"
            } ),
        }
    }

    fn print( &mut self ) -> Result<Node, SyntaxError<'lexer>> {
        let (argument_line, argument_token) = self.tokens.next().bounded( &mut self.tokens, "expected print argument" )?.unwrap();
        let argument = match &argument_token.kind {
            TokenKind::True | TokenKind::False | TokenKind::Literal( _ ) |
            TokenKind::Identifier( _ ) | TokenKind::OpenRoundBracket => self.expression(),
            _ => Err( SyntaxError {
                line: argument_line,
                token: argument_token,
                msg: "invalid print argument",
                help_msg: "expected an expression"
            } )
        };

        let argument = argument?;
        self.semicolon()?;

        return Ok( Node::Print( Box::new( argument ) ) );
    }

    // TODO implement negative numbers
    fn factor( &mut self ) -> Result<Node, SyntaxError<'lexer>> {
        let (line, token) = self.tokens.current_or_next().bounded( &mut self.tokens, "expected expression" )?.unwrap();
        let result = match &token.kind {
            // FIX forbid implicit conversions
            TokenKind::Literal( literal ) => Ok( Node::Literal( literal.clone() ) ),
            TokenKind::True => Ok( Node::Literal( Type::Bool { value: true } ) ),
            TokenKind::False => Ok( Node::Literal( Type::Bool { value: false } ) ),
            TokenKind::Identifier( name ) => match self.definitions.resolve( name ) {
                Some( _ ) => Ok( Node::Identifier( name.clone() ) ),
                None => Err( SyntaxError {
                    line,
                    token,
                    msg: "variable not defined",
                    help_msg: "not defined previously"
                } )
            },
            TokenKind::OpenRoundBracket => {
                let (empty_expression_line, empty_expression_token) = self.tokens.next().bounded( &mut self.tokens, "expected expression" )?.unwrap();
                match empty_expression_token.kind {
                    TokenKind::CloseRoundBracket => Err( SyntaxError {
                        line: empty_expression_line,
                        token: empty_expression_token,
                        msg: "invalid expression",
                        help_msg: "empty expressions are not allowed"
                    } ),
                    _ => {
                        let expression = self.expression()?;
                        let (_close_bracket_line, close_bracket_token) = self.tokens.current_or_next().bounded( &mut self.tokens, "expected closed parenthesis" )?.unwrap();
                        match close_bracket_token.kind {
                            TokenKind::CloseRoundBracket => Ok( expression ),
                            _ => Err( SyntaxError {
                                line,
                                token,
                                msg: "invalid expression",
                                help_msg: "unclosed parenthesis"
                            } )
                        }
                    }
                }
            },
            _ => Err( SyntaxError {
                line,
                token,
                msg: "invalid expression",
                help_msg: "expected number literal"
            } ),
        };

        self.tokens.next();
        return result;
    }

    // FIX dealing with missing semicolons or missing operators
        // TODO move it outside the expression
    fn operator( &mut self, ops: &[OpKind] ) -> Result<Option<OpKind>, SyntaxError<'lexer>> {
        let (_line, token) = self.tokens.current_or_next().bounded( &mut self.tokens, "expected expression or semicolon" )?.unwrap();
        let result = match &token.kind {
            TokenKind::Op( op ) => match ops.contains( op ) {
                true => Ok( Some( op.clone() ) ),
                false => Ok( None ),
            },
            TokenKind::CloseRoundBracket | TokenKind::SemiColon => Ok( None ),
             _ => {
                let (previous_line, previous_token) = self.tokens.peek_previous().bounded( &mut self.tokens, "expected expression" )?.unwrap();
                Err( SyntaxError {
                    line: previous_line,
                    token: previous_token,
                    msg: "invalid expression or missing semicolon",
                    help_msg: "expected an operator after this token to complete the expression, or a ';' to end the statement"
                } )
            },
        };

        if let Ok( Some( _ ) ) = result {
            self.tokens.next();
        }
        return result;
    }

    fn exponentiation( &mut self ) -> Result<Node, SyntaxError<'lexer>> {
        let mut lhs = self.factor()?;

        while let Some( op ) = self.operator( &[OpKind::Pow] )? {
            let rhs = self.factor()?;
            lhs = Node::Expression { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn multiplication_or_division( &mut self ) -> Result<Node, SyntaxError<'lexer>> {
        let mut lhs = self.exponentiation()?;

        while let Some( op ) = self.operator( &[OpKind::Times, OpKind::Divide] )? {
            let rhs = self.exponentiation()?;
            lhs = Node::Expression { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn math( &mut self ) -> Result<Node, SyntaxError<'lexer>> {
        let mut lhs = self.multiplication_or_division()?;

        while let Some( op ) = self.operator( &[OpKind::Plus, OpKind::Minus] )? {
            let rhs = self.multiplication_or_division()?;
            lhs = Node::Expression { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn expression( &mut self ) -> Result<Node, SyntaxError<'lexer>> {
        let mut lhs = self.math()?;

        let ops = [
            OpKind::Equals, OpKind::NotEquals,
            OpKind::Greater, OpKind::GreaterOrEquals,
            OpKind::Less, OpKind::LessOrEquals,
            OpKind::Compare
        ];

        while let Some( op ) = self.operator( &ops )? {
            let rhs = self.math()?;
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
            Node::Identifier( name ) => self.evaluate_node( &*self.definitions.resolve( name ).unwrap().value ),
            Node::Print( argument ) => self.evaluate_node( &**argument ),
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
                    Node::Identifier( name ) => &*self.definitions.resolve( name ).unwrap().value,
                    Node::Print( _ ) => unreachable!(),
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
                    Node::Print( _ ) | Node::Identifier( _ ) => unreachable!(),
                };

                let print_asm = match value_type {
                    Type::I64 { .. } =>
                        "\n mov rdi, rax\
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
                        "\n mov rdi, stdout\
                        \n mov rsi, rax\
                        \n mov rdx, rdx\
                        \n mov rax, SYS_write\
                        \n syscall",
                };

                asm.push_str( &format!( " ; {}\n", node ) );
                self.compile_node( value, asm );
                asm.push_str( print_asm );
            },
            Node::Literal( literal ) => match &literal {
                Type::I64 { value, .. } => asm.push_str( &format!( " push {}\n", value ) ),
                Type::Char { value } => asm.push_str( &format!( " push {}\n", value ) ),
                Type::Bool { value } => asm.push_str( &format!( " push {}\n", value ) ),
                // _ => panic!( "Bug: fix case when not printing values, as they just get pushed on to the stack and never get popped" ),
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
                            \n cmp rsi, rdi\
                            \n {} rax, rbx\
                            \n mov rbx, true_str_len\
                            \n mov rdx, false_str_len\
                            \n cmp rsi, rdi\
                            \n {} rdx, rbx",
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

                asm.push_str( &format!(
                    " ; {}\
                    {}\n\n",
                    node,
                    op_asm
                ) );
            },
            Node::Identifier( name ) => self.compile_node( &*self.definitions.resolve( name ).unwrap().value, asm ),
        }
    }

    fn interpret( &self, file_path: &str ) {
        println!( "\x1b[92;1mIntepreting\x1b[0m: {}", file_path );

        for node in &self.nodes {
            self.evaluate_node( node ).actual();
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
            self.compile_node( node, &mut user_program );
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
