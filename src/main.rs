// TODO create standardized module for outputting colored text
use std::{io::{BufReader, BufRead, ErrorKind, BufWriter, Write}, fs::File, env, process::{ExitCode, Command}, fmt::Display, path::{Path, PathBuf}, iter::Peekable, str::Chars};


#[derive( Debug, Clone )]
enum Type {
    I64 { value: i64 },

    // TODO convert to using char
    Char { value: u8 }, // only supporting ASCII characters for now
    Bool { value: bool },
}

impl Display for Type {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        match self {
            Self::I64 { value } => write!( f, "{}", value ),
            Self::Char { value } => write!( f, "'{}'", value.escape_ascii().to_string() ), // TODO create own escaping function
            Self::Bool { value } => write!( f, "{}", value ),
        }
    }
}


#[derive( Debug, Clone )]
enum OpKind {
    Plus,
    Minus,
    Times,
    Divide,
    Pow,
    DoubleEquals,
}

impl Display for OpKind {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        match self {
            Self::Plus => write!( f, "+" ),
            Self::Minus => write!( f, "-" ),
            Self::Times => write!( f, "*" ),
            Self::Divide => write!( f, "/" ),
            Self::Pow => write!( f, "^" ),
            Self::DoubleEquals => write!( f, "==" ),
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
        match self {
            Self::Let => write!( f, "let" ),
            Self::Const => write!( f, "const" ),
            Self::Var => write!( f, "var" ),
        }
    }
}


#[derive( Debug )]
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

    // Keywords
    Print, // temporary way of printing values
    // Entry,
    Definition( DefinitionKind ),
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
        match self {
            Self::Unexpected { text, .. } => write!( f, "{}", text ),
            Self::Comment( text ) => write!( f, "{}", text ),

            Self::OpenRoundBracket => write!( f, "(" ),
            Self::CloseRoundBracket => write!( f, ")" ),
            Self::Equals => write!( f, "=" ),
            Self::SemiColon => write!( f, ";" ),

            Self::Literal( literal ) => write!( f, "{}", literal ),
            Self::Identifier( name ) => write!( f, "{}", name ),

            Self::Print => write!( f, "print" ),
            Self::Definition( kind ) => write!( f, "{}", kind ),

            Self::Op( op ) => write!( f, "{}", op ),

            Self::Empty | Self::EOF | Self::SOF => write!( f, "" ),
        }
    }
}


#[derive( Debug )]
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
            write!( f, "" )?;
            return Ok( () );
        }

        // leading whitespaces
        write!( f, "{}", " ".repeat( self.tokens[ 0 ].col - 1 ) )?;

        let mut tokens = self.tokens.iter().peekable();

        if self.number == 1 {
            tokens.next(); // skipping the SOF
        }

        while let Some( token ) = tokens.next() {
            let spaces = if let TokenKind::Comment( _ ) = token.kind {
                0
            }
            else if let Some( &next_token ) = tokens.peek() {
                next_token.col - (token.col + token.len)
            }
            else {
                0
            };

            write!( f, "{}{}", token.kind, " ".repeat( spaces ) )?;
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
            let line_text = format!( "{}", line );

            for token in &line.tokens {
                if let TokenKind::Unexpected { err_msg, help_msg, .. } = &token.kind {
                    let gutter_padding_amount = line.number.ilog10() as usize + 1;
                    let gutter_padding = " ".repeat( gutter_padding_amount );
                    let pointers_padding = " ".repeat( token.col - 1 );
                    let pointers = "^".repeat( token.len );
                    let bar = "\x1b[94m|\x1b[0m";

                    let error_visualization = &format!(
                        " {} {}\
                        \n \x1b[94m{: >gutter_padding_amount$}\x1b[0m {} {}\
                        \n {} {} {}\x1b[91m{} {}\x1b[0m",
                        gutter_padding, bar,
                        line.number, bar, line_text,
                        gutter_padding, bar, pointers_padding, pointers, help_msg
                    );

                    writeln!( f,
                        "\x1b[91;1mError\x1b[0m [L]: \x1b[1m{}\x1b[0m\
                        \n {} \x1b[91min\x1b[0m: {}:{}:{}\n{}\n",
                        err_msg,
                        gutter_padding, self.file_path, line.number, token.col, error_visualization
                    )?;
                }
            }
        }

        return Ok( () );
    }
}

impl TryFrom<(&str, File)> for Lexer {
    type Error = Self;

    // IDEA make the input character stream generic, eg: to be able to compile from strings instead of just files
    fn try_from( src: (&str, File) ) -> Result<Self, Self::Error> {
        let (file_path, source_file) = (src.0, src.1);
        let mut errors: Vec<Line> = Vec::new();
        let mut lines: Vec<Line> = Vec::new();
        let mut number: usize = 1;

        let mut src_lines = BufReader::new( source_file );
        let mut src_line = String::new();
        let mut token_text = String::new();
        while let Ok( chars_read ) = src_lines.read_line( &mut src_line ) {
            // reached EOF on an empty line
            if chars_read == 0 {
                lines.push( Line { number, tokens: vec![Token { col: 1, len: 1, kind: TokenKind::EOF }] } );
                break;
            }

            let mut line_contains_errors = false;
            let mut tokens: Vec<Token> = Vec::new();
            let mut col = 1;

            let mut src = src_line.chars().peekable();
            // IDEA try extracting this to a separate function
            loop {
                let token: Token = match Self::next_ascii( &mut src ) {
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
                        '=' => {
                            match Self::next_char( &mut src, &mut token_text ) {
                                Ok( b'=' ) => {
                                    let token = Token { col, len: 2, kind: TokenKind::Op( OpKind::DoubleEquals ) };
                                    col += 1;
                                    token
                                },
                                _ => Token { col, len: 1, kind: TokenKind::Equals },
                            }
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
                                "true" => TokenKind::Literal( Type::Bool { value: true } ),
                                "false" => TokenKind::Literal( Type::Bool { value: false } ),
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

            let file_ended_at_line_end = !src_line.ends_with( "\n" );
            if file_ended_at_line_end {
                tokens.push( Token { col, len: 1, kind: TokenKind::EOF } );
            }

            if tokens.is_empty() {
                tokens.push( Token { col: 1, len: 1, kind: TokenKind::Empty  } );
            }
            let line = Line { number, tokens };

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
            number += 1;
        }

        if !errors.is_empty() {
            errors[ 0 ].tokens.insert( 0, Token { col: 1, len: 1, kind: TokenKind::SOF } );
            return Err( Self { file_path: file_path.to_string(), lines: errors } );
        }
        else {
            lines[ 0 ].tokens.insert( 0, Token { col: 1, len: 1, kind: TokenKind::SOF } );
            return Ok( Self { file_path: file_path.to_string(), lines } );
        }
    }
}

impl Lexer {
    fn iter<'lexer>( &'lexer self ) -> LexerIter<'lexer> {
        return LexerIter{ lexer: self, line: 0, token: 0 };
    }

    // FIX properly handle non ASCII codes in error messages
    fn next_ascii( src: &mut Peekable<Chars> ) -> Result<Option<char>, TokenKind> {
        match src.next() {
            Some( next @ ..='\x7F' ) => return Ok( Some( next ) ),
            Some( next ) => return Err( TokenKind::Unexpected {
                text: next.to_string(),
                err_msg: "unrecognized character",
                help_msg: "not a valid ASCII character"
            } ),
            None => return Ok( None ),
        }
    }

    fn next_char( src: &mut Peekable<Chars>, token_text: &mut String ) -> Result<u8, TokenKind> {
        match Self::next_ascii( src )? {
            Some( '\n' ) | None => return Err( TokenKind::Unexpected {
                text: token_text.clone(),
                err_msg: "invalid character literal",
                help_msg: "missing closing single quote"
            } ),
            Some( next ) => {
                token_text.push( next );
                return Ok( next as u8 );
            },
        }
    }

    fn parse_char( src: &mut Peekable<Chars>, token_text: &mut String ) -> Result<u8, TokenKind> {
        // IDEA treat character literals as just strings of lenght 1, reporting errors if over 1
        match Self::next_char( src, token_text )? {
            b'\\' => match Self::next_char( src, token_text )? {
                b'n' => return Ok( b'\n' as u8 ),
                b't' => return Ok( b'\t' as u8 ),
                b'\'' => return Ok( b'\'' as u8 ),
                b'"' => return Ok( b'"' as u8 ),
                _ => return Err( TokenKind::Unexpected {
                    text: token_text.clone(),
                    err_msg: "invalid escape character literal",
                    help_msg: "check the documentation for a list of valid escape characters"
                } ),
            },
            b'\x00'..=b'\x1F' | b'\x7F' => return Err( TokenKind::Unexpected {
                text: token_text.clone(),
                err_msg: "invalid character literal",
                help_msg: "cannot be a control character"
            } ),
            next => return Ok( next as u8 ),
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
        match token.kind {
            TokenKind::Comment( _ ) | TokenKind::Empty => return self.next(),
            _ => return Some( (line, token) ),
        }
    }

    fn current_or_previous( &mut self ) -> Option<LexerIterItem<'lexer>> {
        let (line, token) = self.current()?;
        match token.kind {
            TokenKind::Comment( _ ) | TokenKind::Empty => return self.previous(),
            _ => return Some( (line, token) ),
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

        match token.kind {
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

        match token.kind {
            TokenKind::Comment( _ ) | TokenKind::Empty => return self.previous(),
            _ => return Some( (line, token) ),
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
        match self {
            Some( (line, token) ) => match token.kind {
                TokenKind::EOF => {
                    // we are always sure that there is at least the SOF token before the EOF token, so we can safely unwrap
                    let (previous_line, previous_token) = tokens.peek_previous().unwrap();
                    return Err( SyntaxError {
                        line: previous_line,
                        token: previous_token,
                        msg: err_msg,
                        help_msg: "file ended after here instead"
                    } )
                },
                TokenKind::SOF => {
                    // we are always sure that there is at least the EOF token after the SOF token, so we can safely unwrap
                    let (next_line, next_token) = tokens.peek_next().unwrap();
                    return Err( SyntaxError {
                        line: next_line,
                        token: next_token,
                        msg: err_msg,
                        help_msg: "file ended after here instead"
                    } )
                },
                _ => return Ok( Some( (line, token) ) ),
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



#[derive( Debug, Clone )]
enum Node {
    Literal( Type ),
    Expression{ lhs: Box<Node>, op: OpKind, rhs: Box<Node> },
    Identifier( String ),
    Print( Box<Node> ),
    Definition( Definition ),
}

impl Display for Node {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        match self {
            Self::Literal( literal ) => write!( f, "{}", literal ),
            Self::Expression { lhs, op, rhs } => write!( f, "({} {} {})", lhs, op, rhs ),
            Self::Identifier( name ) => write!( f, "{}", name ),
            Self::Print( node ) => write!( f, "print {}", node ),
            Self::Definition( assignment ) => write!( f, "{} {} = {}", assignment.kind, assignment.name, assignment.value ),
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

            let gutter_padding_amount = error.line.number.ilog10() as usize + 1;
            let gutter_padding = " ".repeat( gutter_padding_amount );
            let pointers_padding = " ".repeat( error.token.col - 1 );
            let pointers = "^".repeat( error.token.len );
            let bar = "\x1b[94m|\x1b[0m";

            let error_visualization = &format!(
                " {} {}\
                \n \x1b[94m{: >gutter_padding_amount$}\x1b[0m {} {}\
                \n {} {} {}\x1b[91m{} {}\x1b[0m",
                gutter_padding, bar,
                error.line.number, bar, line_text,
                gutter_padding, bar, pointers_padding, pointers, error.help_msg
            );

            writeln!( f,
                "\x1b[91;1mError\x1b[0m [P]: \x1b[1m{}\x1b[0m\
                \n {} \x1b[91min\x1b[0m: {}:{}:{}\n{}\n",
                error.msg,
                gutter_padding, self.file_path, error.line.number, error.token.col, error_visualization
            )?;
        }

        return Ok( () );
    }
}


// TODO introduce the notion of context:
    // parenthesis stack
    // blocks and scopes
        // variables, functions, etc.
#[derive( Debug )]
struct AST {
    nodes: Vec<Node>,
    definitions: Definitions,
}

impl<'lexer> TryFrom<&'lexer Lexer> for AST {
    type Error = SyntaxErrors<'lexer>;

    fn try_from( lexer: &'lexer Lexer ) -> Result<Self, Self::Error> {
        let mut definitions: Definitions = Vec::new();
        let mut nodes: Vec<Node> = Vec::new();

        if lexer.lines.is_empty() {
            return Ok( Self { nodes, definitions } );
        }

        let mut errors: Vec<SyntaxError> = Vec::new();
        let mut tokens = lexer.iter();
        tokens.next(); // skipping the SOF token
        while let Some( (line, token) ) = tokens.current_or_next() {
            let statement_result = match &token.kind {
                // NOTE definitions are planned to be reworked, so this is just temporary
                TokenKind::Definition( _ ) => match Self::variable_definition( &mut tokens, &mut definitions ) {
                    Ok( _ ) => continue, // skipping this node since it is already in the definitions
                    err @ Err( _ ) => err,
                },
                TokenKind::Print => Self::print( &mut tokens, &definitions ),
                TokenKind::Literal( _ ) |
                TokenKind::Identifier( _ ) |
                TokenKind::OpenRoundBracket => Self::math( &mut tokens, &definitions ),
                TokenKind::CloseRoundBracket => {
                    tokens.next();
                    Err( SyntaxError {
                        line,
                        token,
                        msg: "invalid expression",
                        help_msg: "stray closed parenthesis"
                    } )
                },
                TokenKind::Op( _ ) => {
                    tokens.next();
                    Err( SyntaxError {
                        line,
                        token,
                        msg: "invalid expression",
                        help_msg: "stray binary operator"
                    } )
                },
                TokenKind::Equals => {
                    tokens.next();
                    Err( SyntaxError {
                        line,
                        token,
                        msg: "invalid assignment",
                        help_msg: "stray assignment"
                    } )
                },
                TokenKind::Comment( _ ) | TokenKind::SemiColon | TokenKind::Empty | TokenKind::EOF => {
                    tokens.next();
                    continue;
                },
                TokenKind::SOF | TokenKind::Unexpected { .. } => unreachable!(),
            };

            match statement_result {
                Ok( statement ) => nodes.push( statement ),
                Err( err ) => errors.push( err )
            }
        }

        if !errors.is_empty() {
            return Err( SyntaxErrors { file_path: lexer.file_path.clone(), errors } );
        }
        else {
            return Ok( Self { nodes, definitions } );
        }
    }
}

impl<'lexer, 'ast> AST {
    fn semicolon( tokens: &mut LexerIter<'lexer> ) -> Result<(), SyntaxError<'lexer>> {
        let (_line, token) = tokens.current_or_next().bounded( tokens, "expected semicolon" )?.unwrap();
        let result = match token.kind {
            TokenKind::SemiColon => Ok( () ),
            _ => {
                let (previous_line, previous_token) = tokens.peek_previous().bounded( tokens, "expected semicolon" )?.unwrap();
                Err( SyntaxError {
                    line: previous_line,
                    token: previous_token,
                    msg: "invalid expression",
                    help_msg: "expected semicolon after this token"
                } )
            },
        };

        tokens.next();
        return result;
    }

    fn variable_definition( tokens: &mut LexerIter<'lexer>, definitions: &mut Definitions ) -> Result<Node, SyntaxError<'lexer>> {
        let (definition_kind_line, definition_kind_token) = tokens.current().unwrap();
        let kind = match &definition_kind_token.kind {
            TokenKind::Definition( kind ) => kind.clone(),
            _ => unreachable!(),
        };

        let (identifier_line, identifier_token) = tokens.next().bounded( tokens, "expected identifier" )?.unwrap();
        let name = match &identifier_token.kind {
            TokenKind::Identifier( name ) => Ok( name.clone() ),
            _ => Err( SyntaxError {
                line: definition_kind_line,
                token: definition_kind_token,
                msg: "invalid assignment",
                help_msg: "expected variable name after definition kind specifier"
            } ),
        };

        let (equals_line, equals_token) = tokens.next().bounded( tokens, "expected equals" )?.unwrap();
        let equals = match equals_token.kind {
            TokenKind::Equals => Ok( () ),
            _ => Err( SyntaxError {
                line: identifier_line,
                token: identifier_token,
                msg: "invalid assignment",
                help_msg: "expected '=' after variable name"
            } ),
        };

        let (_value_line, value_token) = tokens.next().bounded( tokens, "expected expression" )?.unwrap();
        let value = match value_token.kind {
            TokenKind::Literal( _ ) | TokenKind::Identifier( _ ) | TokenKind::OpenRoundBracket => Self::math( tokens, definitions ),
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
        Self::semicolon( tokens )?;

        match definitions.resolve( &name ) {
            None => {
                definitions.push( Definition { kind, name, value: Box::new( value ) } );
                return Ok( Node::Definition( definitions.last().unwrap().clone() ) );
            },
            Some( _ ) => return Err( SyntaxError {
                line: identifier_line,
                token: identifier_token,
                msg: "variable redefinition",
                help_msg: "was previously defined"
            } ),
        }
    }

    fn print( tokens: &mut LexerIter<'lexer>, definitions: &Definitions ) -> Result<Node, SyntaxError<'lexer>> {
        let (argument_line, argument_token) = tokens.next().bounded( tokens, "expected print argument" )?.unwrap();
        let argument = match &argument_token.kind {
            TokenKind::Literal( _ ) | TokenKind::Identifier( _ ) | TokenKind::OpenRoundBracket => Self::math( tokens, definitions ),
            _ => Err( SyntaxError {
                line: argument_line,
                token: argument_token,
                msg: "invalid print argument",
                help_msg: "expected an expression"
            } )
        };

        let argument = argument?;
        Self::semicolon( tokens )?;

        return Ok( Node::Print( Box::new( argument ) ) );
    }

    fn factor( tokens: &mut LexerIter<'lexer>, definitions: &Definitions ) -> Result<Node, SyntaxError<'lexer>> {
        let (line, token) = tokens.current_or_next().bounded( tokens, "expected expression" )?.unwrap();
        let result = match &token.kind {
            // FIX forbid implicit conversions
            TokenKind::Literal( literal ) => match literal {
                Type::Bool { value } => Ok( Node::Literal( Type::I64 { value: *value as i64 } ) ),
                _ => Ok( Node::Literal( literal.clone() ) ),
            },
            TokenKind::Identifier( name ) => match definitions.resolve( name ) {
                Some( _ ) => Ok( Node::Identifier( name.clone() ) ),
                None => Err( SyntaxError {
                    line,
                    token,
                    msg: "variable not defined",
                    help_msg: "not defined previously"
                } )
            },
            TokenKind::OpenRoundBracket => {
                let (empty_expression_line, empty_expression_token) = tokens.next().bounded( tokens, "expected expression" )?.unwrap();
                match empty_expression_token.kind {
                    TokenKind::CloseRoundBracket => Err( SyntaxError {
                        line: empty_expression_line,
                        token: empty_expression_token,
                        msg: "invalid expression",
                        help_msg: "empty expressions are not allowed"
                    } ),
                    _ => {
                        let expression = Self::expression( tokens, definitions )?;
                        let (_close_bracket_line, close_bracket_token) = tokens.current_or_next().bounded( tokens, "expected closed parenthesis" )?.unwrap();
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

        tokens.next();
        return result;
    }

    fn power( tokens: &mut LexerIter<'lexer> ) -> Result<Option<OpKind>, SyntaxError<'lexer>> {
        let (_line, token) = tokens.current_or_next().bounded( tokens, "expected expression or semicolon" )?.unwrap();
        let result = match &token.kind {
            TokenKind::Op( op @ OpKind::Pow ) => Ok( Some( op.clone() ) ),
            _ => Ok( None ),
        };

        if let Ok( Some( _ ) ) = result {
            tokens.next();
        }
        return result;
    }

    fn exponentiation( tokens: &mut LexerIter<'lexer>, definitions: &Definitions ) -> Result<Node, SyntaxError<'lexer>> {
        let mut lhs = Self::factor( tokens, definitions )?;

        while let Some( op ) = Self::power( tokens )? {
            let rhs = Self::factor( tokens, definitions )?;
            lhs = Node::Expression { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn times_or_divide( tokens: &mut LexerIter<'lexer> ) -> Result<Option<OpKind>, SyntaxError<'lexer>> {
        let (_line, token) = tokens.current_or_next().bounded( tokens, "expected expression or semicolon" )?.unwrap();
        let result = match &token.kind {
            TokenKind::Op( op @ (OpKind::Times | OpKind::Divide) ) => Ok( Some( op.clone() ) ),
            _ => Ok( None ),
        };

        if let Ok( Some( _ ) ) = result {
            tokens.next();
        }
        return result;
    }

    fn multiplication_or_division( tokens: &mut LexerIter<'lexer>, definitions: &Definitions ) -> Result<Node, SyntaxError<'lexer>> {
        let mut lhs = Self::exponentiation( tokens, definitions )?;

        while let Some( op ) = Self::times_or_divide( tokens )? {
            let rhs = Self::exponentiation( tokens, definitions )?;
            lhs = Node::Expression { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    // FIX dealing with missing semicolons or missing operators
        // TODO move it outside the expression
    fn plus_or_minus( tokens: &mut LexerIter<'lexer> ) -> Result<Option<OpKind>, SyntaxError<'lexer>> {
        let (_line, token) = tokens.current_or_next().bounded( tokens, "expected expression or semicolon" )?.unwrap();
        let result = match &token.kind {
            TokenKind::Op( op @ (OpKind::Plus | OpKind::Minus) ) => Ok( Some( op.clone() ) ),
            _ => Ok( None ),
        };

        if let Ok( Some( _ ) ) = result {
            tokens.next();
        }
        return result;
    }

    fn expression( tokens: &mut LexerIter<'lexer>, definitions: &Definitions ) -> Result<Node, SyntaxError<'lexer>> {
        let mut lhs = Self::multiplication_or_division( tokens, definitions )?;

        while let Some( op ) = Self::plus_or_minus( tokens )? {
            let rhs = Self::multiplication_or_division( tokens, definitions )?;
            lhs = Node::Expression { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn double_equals( tokens: &mut LexerIter<'lexer> ) -> Result<Option<OpKind>, SyntaxError<'lexer>> {
        let (_line, token) = tokens.current_or_next().bounded( tokens, "expected expression or semicolon" )?.unwrap();
        let result = match &token.kind {
            TokenKind::Op( op @ OpKind::DoubleEquals ) => Ok( Some( op.clone() ) ),
            TokenKind::CloseRoundBracket | TokenKind::SemiColon => Ok( None ),
            _ => {
                let (previous_line, previous_token) = tokens.peek_previous().bounded( tokens, "expected expression" )?.unwrap();
                Err( SyntaxError {
                    line: previous_line,
                    token: previous_token,
                    msg: "invalid expression or missing semicolon",
                    help_msg: "expected an operator after this token to complete the expression, or a ';' to end the statement"
                } )
            },
        };

        if let Ok( Some( _ ) ) = result {
            tokens.next();
        }
        return result;
    }

    fn math( tokens: &mut LexerIter<'lexer>, definitions: &Definitions ) -> Result<Node, SyntaxError<'lexer>> {
        let mut lhs = Self::expression( tokens, definitions )?;

        while let Some( op ) = Self::double_equals( tokens )? {
            let rhs = Self::expression( tokens, definitions )?;
            lhs = Node::Expression { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

}

// After construction
impl<'lexer, 'ast> AST {
    fn evaluate_node( &self, node: &Node ) -> i64 {
        match node {
            Node::Literal( value ) => match *value {
                Type::I64 { value } => return value,
                Type::Char { value } => return value as i64,
                Type::Bool { value } => return value as i64,
            },
            Node::Expression{ lhs, op, rhs } => match op {
                OpKind::Plus => return self.evaluate_node( lhs ) + self.evaluate_node( rhs ),
                OpKind::Minus => return self.evaluate_node( lhs ) - self.evaluate_node( rhs ),
                OpKind::Times => return self.evaluate_node( lhs ) * self.evaluate_node( rhs ),
                OpKind::Divide => return self.evaluate_node( lhs ) / self.evaluate_node( rhs ),
                OpKind::Pow => return self.evaluate_node( lhs ).pow( self.evaluate_node( rhs ) as u32 ),
                OpKind::DoubleEquals => return (self.evaluate_node( lhs ) == self.evaluate_node( rhs )) as i64,
            },
            Node::Identifier( name ) => self.evaluate_node( &*self.definitions.resolve( name ).unwrap().value ),
            Node::Print( _ ) | Node::Definition( _ ) => unreachable!(),
        }
    }

    fn interpret_node( &self, node: &Node ) {
        match node {
            Node::Literal( _ ) | Node::Expression { .. } | Node::Identifier( _ ) => return, // ignored
            Node::Print( argument ) => {
                let arg = match &**argument {
                    Node::Print( _ ) | Node::Definition( _ ) => unreachable!(),
                    Node::Literal( _ ) | Node::Expression{ .. } => &**argument,
                    Node::Identifier( name ) => &*self.definitions.resolve( name ).unwrap().value,
                };

                match arg {
                    Node::Literal( value ) => match *value {
                        Type::I64 { value } => print!( "{}", value ),
                        Type::Char { value } => print!( "{}", value as char ),
                        Type::Bool { value } => print!( "{}", value as bool ),
                    },
                    Node::Expression{ .. } => print!( "{}", self.evaluate_node( argument ) ),
                    Node::Print( _ ) | Node::Identifier( _ ) | Node::Definition( _ ) => unreachable!()
                }
            }
            Node::Definition( _ ) => unreachable!(),
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
                asm.push_str( &format!( " ; {}", node ) );

                let arg = match &**argument {
                    Node::Print( _ ) | Node::Definition( _ ) => unreachable!(),
                    Node::Literal( _ ) | Node::Expression{ .. } => &**argument,
                    Node::Identifier( name ) => &*self.definitions.resolve( name ).unwrap().value,
                };

                match arg {
                    Node::Literal( literal ) => match literal {
                        Type::I64 { value } => asm.push_str( &format!(
                            "\n mov rdi, {}\
                            \n mov rsi, 10\
                            \n call int_toStr\
                            \n\
                            \n mov rdi, stdout\
                            \n mov rsi, rax\
                            \n mov rdx, rdx\
                            \n mov rax, SYS_write\
                            \n syscall\n\n",
                            value
                        ) ),
                        Type::Char { value } => asm.push_str( &format!(
                            "\n push {}\
                            \n mov rdi, stdout\
                            \n mov rsi, rsp\
                            \n mov rdx, 1\
                            \n mov rax, SYS_write\
                            \n syscall\
                            \n pop rsi\n\n",
                            value
                        ) ),
                        Type::Bool { value } => asm.push_str( &format!(
                            "\n mov rdi, stdout\
                            \n mov rsi, {}\
                            \n mov rdx, {}_str_len\
                            \n mov rax, SYS_write\
                            \n syscall\n\n",
                            value,
                            value
                        ) ),
                    },
                    Node::Expression { .. } => {
                        asm.push_str( &format!( " ; {}\n", argument ) );
                        self.compile_node( argument, asm );
                        asm.push_str(
                            "\n pop rdi\
                            \n mov rsi, 10\
                            \n call int_toStr\
                            \n\
                            \n mov rdi, stdout\
                            \n mov rsi, rax\
                            \n mov rdx, rdx\
                            \n mov rax, SYS_write\
                            \n syscall\n\n"
                        );
                    },
                    Node::Print( _ ) | Node::Identifier( _ ) | Node::Definition( _ ) => unreachable!(),
                }
            },
            Node::Literal( literal ) => match &literal {
                // Type::I64 { value, .. } => asm.push_str( &format!( " push {}\n", value ) ),
                // Type::Char { value } => asm.push_str( &format!( " push {}\n", value ) ),
                // Type::Bool { value } => asm.push_str( &format!( " push {}\n", value ) ),
                _ => panic!( "Bug: fix case when not printing values, as they just get pushed on to the stack and never get popped" ),
            },
            Node::Expression { lhs, op, rhs } => {
                self.compile_node( lhs, asm );
                self.compile_node( rhs, asm );
                let op_asm = match op {
                    OpKind::Plus => format!(
                        " ; {}\
                        \n pop rax\
                        \n pop rbx\
                        \n add rax, rbx\
                        \n push rax\n\n",
                        node
                    ),
                    OpKind::Minus => format!(
                        " ; {}\
                        \n pop rbx\
                        \n pop rax\
                        \n sub rax, rbx\
                        \n push rax\n\n",
                        node
                    ),
                    OpKind::Times => format!(
                        " ; {}\
                        \n pop rax\
                        \n pop rbx\
                        \n imul rax, rbx\
                        \n push rax\n\n",
                        node
                    ),
                    OpKind::Divide => format!(
                        " ; {}\
                        \n pop rbx\
                        \n pop rax\
                        \n xor rdx, rdx\
                        \n idiv rbx\
                        \n push rax\n\n",
                        node
                    ),
                    OpKind::Pow => format!(
                        " ; {}\
                        \n pop rsi\
                        \n pop rdi\
                        \n call int_pow\
                        \n push rax\n\n",
                        node
                    ),
                    OpKind::DoubleEquals => format!(
                        " ; {}\
                        \n pop rsi\
                        \n pop rdi\
                        \n mov rdx, 1\
                        \n xor rax, rax\
                        \n cmp rsi, rdi\
                        \n cmove rax, rdx\n\n",
                        node
                    ),
                };

                asm.push_str( &op_asm );
            },
            Node::Identifier( name ) => self.compile_node( &*self.definitions.resolve( name ).unwrap().value, asm ),
            Node::Definition( _ ) => unreachable!(),
        }
    }

    fn interpret( &self, file_path: &str ) {
        println!( "\x1b[92;1mIntepreting\x1b[0m: {}", file_path );

        for node in &self.nodes {
            self.interpret_node( node );
        }
    }

    fn compile( &self, file_path: &str ) -> Result<PathBuf, ()> {
        // println!( "\x1b[91;1mError\x1b[0m: compilation mode under development" );
        // return Err( () );

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
            println!( "{:#?}", ast );
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
