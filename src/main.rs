// TODO implement NOTE, HINT, HELP in error messages
use std::{io::{BufReader, BufRead, ErrorKind, BufWriter, Write}, fs::File, env, process::{ExitCode, Command}, fmt::Display, path::{Path, PathBuf}};


#[derive( Debug, PartialEq, Clone, Copy )]
enum LiteralKind {
    I64{ base: u8, value: i64 }
}

impl Display for LiteralKind {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        match self {
            Self::I64 { value, .. } => write!( f, "{}", value ),
        }
    }
}


#[derive( Debug, PartialEq, Clone, Copy )]
enum OpKind {
    Plus,
    Minus,
    Times,
    Divide,
    Pow,
}

impl Display for OpKind {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        match self {
            Self::Plus => write!( f, "+" ),
            Self::Minus => write!( f, "-" ),
            Self::Times => write!( f, "*" ),
            Self::Divide => write!( f, "/" ),
            Self::Pow => write!( f, "^" ),
        }
    }
}


#[derive( Debug, PartialEq )]
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
    // Equals,
    // Colon,
    SemiColon,

    // Identifiers
    Literal( LiteralKind ),
    // Identifier( String ),

    // Keywords
    Print, // temporary way of printing numbers
    PrintChar, // temporary way of printing numbers interpreted as ascii characters
    // Entry,
    // Fn,
    // Let,
    // Const,
    // Var,
    // Return,

    // Operators
    Op( OpKind ),

    // Special
    // Newline,
    EOF,
}

impl Display for TokenKind {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        match self {
            Self::Unexpected { text, .. } => write!( f, "{}", text ),
            Self::Comment( text ) => write!( f, "{}", text ),

            Self::OpenRoundBracket => write!( f, "(" ),
            Self::CloseRoundBracket => write!( f, ")" ),
            Self::SemiColon => write!( f, ";" ),

            Self::Literal( literal ) => write!( f, "{}", literal ),

            Self::Print => write!( f, "print" ),
            Self::PrintChar => write!( f, "print_char" ),

            Self::Op( op ) => write!( f, "{}", op ),
            // Self::Equals => write!( f, "=" ),

            /* Self::Newline | */ Self::EOF => write!( f, "" ),
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


#[derive( Debug )]
struct Lexer {
    file_path: String,
    lines: Vec<Line>,
}

impl Lexer {
    // TODO make the input character stream generic
    fn parse( file_path: &str, source_file: File ) -> Result<Self, Self> {
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
            let mut line = Line { number, tokens: Vec::new() };
            let mut col = 1;

            let mut src = src_line.chars().peekable();
            while let Some( ch ) = src.next() {
                let token: Token = match ch {
                    // '\n' => Token { col, len: 1, kind: TokenKind::Newline },
                    // ignore whitespace
                    c if c.is_ascii_whitespace() => {
                        col += 1;
                        continue;
                    },
                    '(' => Token { col, len: 1, kind: TokenKind::OpenRoundBracket },
                    ')' => Token { col, len: 1, kind: TokenKind::CloseRoundBracket },
                    // '[' => Token { kind: TokenKind::OpenSquareBracket, col },
                    // ']' => Token { kind: TokenKind::CloseSquareBracket, col },
                    // '{' => Token { kind: TokenKind::OpenCurlyBracket, col },
                    // '}' => Token { kind: TokenKind::CloseCurlyBracket, col },
                    '+' => Token { col, len: 1, kind: TokenKind::Op( OpKind::Plus ) },
                    '-' => Token { col, len: 1, kind: TokenKind::Op( OpKind::Minus ) },
                    '*' => Token { col, len: 1, kind: TokenKind::Op( OpKind::Times ) },
                    '/' => Token { col, len: 1, kind: TokenKind::Op( OpKind::Divide ) },
                    '^' => Token { col, len: 1, kind: TokenKind::Op( OpKind::Pow ) },
                    // '=' => Token { kind: TokenKind::Equals, col },
                    // ':' => Token { kind: TokenKind::Colon, col },
                    ';' => Token { col, len: 1, kind: TokenKind::SemiColon },
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
                            match token_text.parse() {
                                Ok( value ) => TokenKind::Literal( LiteralKind::I64{ base: 10, value } ),
                                Err( _ ) => TokenKind::Unexpected {
                                    text: token_text.clone(),
                                    err_msg: "expected number literal",
                                    help_msg: "overflows a 64 bit unsigned integer [-9223372036854775808, 9223372036854775807]"
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
                    'a'..='z' | 'A'..='Z' | '_' => {
                        token_text.clear();
                        token_text.push( ch );

                        while let Some( next ) = src.next_if( |c| matches!( c, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' ) ) {
                            token_text.push( next );
                        }

                        let kind = match token_text.as_str() {
                            "print" => TokenKind::Print,
                            "print_char" => TokenKind::PrintChar,
                            // "entry" => TokenKind::Entry,
                            // "fn" => TokenKind::Fn,
                            // "const" => TokenKind::Const,
                            // "let" => TokenKind::Let,
                            // "var" => TokenKind::Var,
                            // "return" => TokenKind::Return,
                            // _ => TokenKind::Identifier( current_token_text.clone() )
                            _ => TokenKind::Unexpected {
                                text: token_text.clone(),
                                err_msg: "unexpected token",
                                help_msg: "here"
                            },
                        };

                        let len = token_text.len();
                        let token = Token { col, len, kind };
                        col += len - 1;
                        token
                    },
                    _ => {
                        token_text.clear();
                        token_text.push( ch );

                        while let Some( next ) = src.next_if( |c| matches!( c, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' ) ) {
                            token_text.push( next );
                        }

                        let kind = TokenKind::Unexpected {
                            text: token_text.clone(),
                            err_msg: "unexpected token",
                            help_msg: "here"
                        };

                        let len = token_text.len();
                        let token = Token { col, len, kind };
                        col += len - 1;
                        token
                    },
                };

                if let TokenKind::Unexpected { .. } = token.kind {
                    line_contains_errors = true;
                }

                line.tokens.push( token );
                col += 1;
            }

            let file_ended_at_line_end = !src_line.ends_with( "\n" );
            if file_ended_at_line_end {
                line.tokens.push( Token { col, len: 1, kind: TokenKind::EOF } );
            }

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
            return Err( Self { file_path: file_path.to_string(), lines: errors } );
        }
        else {
            return Ok( Self { file_path: file_path.to_string(), lines } );
        }
    }
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
                        " {} {}\n \
                        \x1b[94m{: >gutter_padding_amount$}\x1b[0m {} {}\n \
                        {} {} {}\x1b[91m{} {}\x1b[0m",
                        gutter_padding, bar,
                        line.number, bar, line_text,
                        gutter_padding, bar, pointers_padding, pointers, help_msg
                    );

                    writeln!( f,
                        "\x1b[91;1mError\x1b[0m [L]: \x1b[1m{}\x1b[0m\n \
                        {} \x1b[91min\x1b[0m: {}:{}:{}\n{}\n",
                        err_msg,
                        gutter_padding, self.file_path, line.number, token.col, error_visualization
                    )?;
                }
            }
        }

        return Ok( () );
    }
}

impl<'lexer> IntoIterator for &'lexer Lexer {
    type Item = (&'lexer Line, &'lexer Token);
    type IntoIter = LexerTokenIter<'lexer>;


    fn into_iter( self ) -> Self::IntoIter {
        return LexerTokenIter{ lexer: self, line: 0, token: 0 };
    }
}


#[derive( Debug )]
struct LexerTokenIter<'lexer> {
    lexer: &'lexer Lexer,
    line: usize,
    token: usize,
}

impl<'lexer> LexerTokenIter<'lexer> {
    fn current( &self ) -> (&'lexer Line, &'lexer Token) {
        let line = &self.lexer.lines[ self.line ];
        return (line, &line.tokens[ self.token ]);
    }

    fn next_back_non_whitespace( &mut self, err_msg: &'static str ) -> Result<(&'lexer Line, &'lexer Token), SyntaxError<'lexer>> {
        match self.next_back() {
            Some( (line, token) ) => match token.kind {
                TokenKind::EOF => return Err( SyntaxError {
                    line,
                    token,
                    msg: err_msg,
                    help_msg: "file ended here instead"
                } ),
                /* TokenKind::Newline | */ TokenKind::Comment( _ ) => return self.next_back_non_whitespace( err_msg ),
                _ => return Ok( (line, token) ),
            },
            None => unreachable!(),
        }
    }

    fn next_non_whitespace( &mut self, err_msg: &'static str ) -> Result<(&'lexer Line, &'lexer Token), SyntaxError<'lexer>> {
        match self.next() {
            Some( (line, token) ) => match token.kind {
                TokenKind::EOF => return Err( SyntaxError {
                    line,
                    token,
                    msg: err_msg,
                    help_msg: "file ended here instead"
                } ),
                /* TokenKind::Newline | */ TokenKind::Comment( _ ) => return self.next_non_whitespace( err_msg ),
                _ => return Ok( (line, token) ),
            },
            None => unreachable!(),
        }
    }

    // // TODO make so that it does not modifiy the state of the iterator
    // fn peek_back_non_whitespace( &mut self, err_msg: &'static str ) -> Result<(&'lexer Line, &'lexer Token), SyntaxError<'lexer>> {
    //     match self.peek_back() {
    //         Some( (line, token) ) => match token.kind {
    //             TokenKind::EOF => return Err( SyntaxError {
    //                 line,
    //                 token,
    //                 msg: err_msg,
    //                 help_msg: "file ended here instead"
    //             } ),
    //             /* TokenKind::Newline | */ TokenKind::Comment( _ ) => {
    //                 self.next_back();
    //                 return self.peek_back_non_whitespace( err_msg );
    //             },
    //             _ => return Ok( (line, token) ),
    //         },
    //         None => unreachable!(),
    //     }
    // }

    // TODO make so that it does not modifiy the state of the iterator
    fn peek_non_whitespace( &mut self, err_msg: &'static str ) -> Result<(&'lexer Line, &'lexer Token), SyntaxError<'lexer>> {
        match self.peek() {
            Some( (line, token) ) => match token.kind {
                TokenKind::EOF => return Err( SyntaxError {
                    line,
                    token,
                    msg: err_msg,
                    help_msg: "file ended here instead"
                } ),
                /* TokenKind::Newline | */ TokenKind::Comment( _ ) => {
                    self.next();
                    return self.peek_non_whitespace( err_msg );
                },
                _ => return Ok( (line, token) ),
            },
            None => unreachable!(),
        }
    }

    // TODO make this take an immutable reference to self by properly calculating the previous value
    fn peek( &mut self ) -> Option<(&'lexer Line, &'lexer Token)> {
        // if (self.line as usize) >= self.lexer.lines.len() {
        //     return None;
        // }

        // let line = &self.lexer.lines[ self.line as usize ];
        // let tokens = &line.tokens;
        // if (self.token as usize) < tokens.len() {
        //     let token = &tokens[ self.token as usize ];
        //     return Some( (line, token) );
        // }
        // else {
        //     if ((self.line + 1) as usize) >= self.lexer.lines.len() {
        //         return None;
        //     }

        //     let token = &self.lexer.lines[ (self.line + 1) as usize ].tokens[ 0 ];
        //     return Some( (line, token) );
        // }
        let next = self.next();
        self.next_back();
        return next;
    }

    // // TODO make this take an immutable reference to self by properly calculating the previous value
    // fn peek_back( &mut self ) -> Option<(&'lexer Line, &'lexer Token)> {
    //     let previous = self.next_back();
    //     self.next();
    //     return previous;
    // }
}

impl<'lexer> Iterator for LexerTokenIter<'lexer> {
    type Item = (&'lexer Line, &'lexer Token);

    fn next( &mut self ) -> Option<Self::Item> {
        if (self.line) >= self.lexer.lines.len() {
            return None;
        }

        let line = &self.lexer.lines[ self.line ];
        let tokens = &line.tokens;
        if (self.token) < tokens.len() {
            let token = &tokens[ self.token ];
            self.token += 1;
            return Some( (line, token) );
        }
        else {
            self.line += 1;
            self.token = 0;

            return self.next();
        }
    }
}

impl<'lexer> DoubleEndedIterator for LexerTokenIter<'lexer> {
    fn next_back( &mut self ) -> Option<Self::Item> {
        if self.token == 0 {
            if self.line == 0 {
                return None;
            }

            self.line -= 1;
            let line = &self.lexer.lines[ self.line ];
            let tokens = &line.tokens;

            self.token = tokens.len() - 1;
            let token = &tokens[ self.token ];
            return Some( (line, token) );
        }
        else {
            let line = &self.lexer.lines[ self.line ];
            let tokens = &line.tokens;

            self.token -= 1;
            let token = &tokens[ self.token ];
            return Some( (line, token) );
        }
    }
}


#[derive( Debug )]
enum Node {
    Literal( LiteralKind ),
    Expression{ lhs: Box<Node>, op: OpKind, rhs: Box<Node> },
    Print( Box<Node> ),
    PrintChar( Box<Node> ),
}

impl Display for Node {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        match self {
            Self::Literal( literal ) => write!( f, "{}", literal ),
            Self::Expression{ lhs, op, rhs } => write!( f, "({} {} {})", lhs, op, rhs ),
            Self::Print( node ) => write!( f, "print {}", node ),
            Self::PrintChar( ascii ) => write!( f, "print {}", ascii ),
        }
    }
}


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
                " {} {}\n \
                \x1b[94m{: >gutter_padding_amount$}\x1b[0m {} {}\n \
                {} {} {}\x1b[91m{} {}\x1b[0m",
                gutter_padding, bar,
                error.line.number, bar, line_text,
                gutter_padding, bar, pointers_padding, pointers, error.help_msg
            );

            writeln!( f,
                "\x1b[91;1mError\x1b[0m [P]: \x1b[1m{}\x1b[0m\n \
                {} \x1b[91min\x1b[0m: {}:{}:{}\n{}\n",
                error.msg,
                gutter_padding, self.file_path, error.line.number, error.token.col, error_visualization
            )?;
        }

        return Ok( () );
    }
}


#[derive( Debug )]
struct AST {
    nodes: Vec<Node>,
}

impl AST {
    fn parse<'program>( lexer: &'program Lexer ) -> Result<Self, SyntaxErrors<'program>> {
        let mut syntax_errors = SyntaxErrors { file_path: lexer.file_path.clone(), errors: Vec::new() };
        let mut ast = AST{ nodes: Vec::new() };

        let mut tokens = lexer.into_iter();
        while let Some( (line, token) ) = tokens.peek() {
            let statement_result = match &token.kind {
                TokenKind::Comment( _ ) | TokenKind::EOF | TokenKind::SemiColon /* | TokenKind::Newline */ => {
                    tokens.next();
                    continue;
                },
                TokenKind::Print => {
                    tokens.next();
                    match AST::expression( &mut tokens ) {
                        Ok( factor ) => Ok( Node::Print( Box::new( factor ) ) ),
                        err @ Err( _ ) => err,
                    }
                },
                TokenKind::PrintChar => {
                    tokens.next();
                    match AST::expression( &mut tokens ) {
                        Ok( factor ) => Ok( Node::PrintChar( Box::new( factor ) ) ),
                        err @ Err( _ ) => err,
                    }
                },
                TokenKind::Literal( _ ) | TokenKind::OpenRoundBracket |
                TokenKind::Op( _ ) => AST::expression( &mut tokens ),
                TokenKind::CloseRoundBracket => {
                    tokens.next();
                    Err( SyntaxError {
                        line,
                        token,
                        msg: "invalid expression",
                        help_msg: "stray closed parenthesis, may be missing an opening round bracket"
                    } )
                },
                TokenKind::Unexpected { .. } => unreachable!(),
            };

            match statement_result {
                Ok( statement ) => ast.nodes.push( statement ),
                Err( err ) => syntax_errors.errors.push( err )
            }
        }

        if !syntax_errors.errors.is_empty() {
            return Err( syntax_errors );
        }
        else {
            return Ok( ast );
        }
    }

    fn factor<'program>( tokens: &mut LexerTokenIter<'program> ) -> Result<Node, SyntaxError<'program>> {
        let (line, token) = tokens.peek_non_whitespace( "expected expression" )?;

        let number = match token.kind {
            TokenKind::Literal( literal ) => Ok( Node::Literal( literal ) ),
            TokenKind::OpenRoundBracket => {
                tokens.next();
                // FIXME handling of case when the expression is just an empty bracket pair
                // FIXME handling of case when a bracket is used as a factor for an expression (eg: "3 + )")
                let expression = Self::expression( tokens );
                let (_current_line, current_token) = tokens.current();

                if let Token { kind: TokenKind::CloseRoundBracket, .. } = current_token {
                    expression
                }
                else {
                    Err( SyntaxError {
                        line,
                        token,
                        msg: "invalid expression",
                        help_msg: "unclosed parenthesis"
                    } )
                }
            },
            TokenKind::Op( _ ) => {
                tokens.next();
                Err( SyntaxError {
                    line,
                    token,
                    msg: "invalid expression",
                    help_msg: "stray binary operator, consider putting a number literal before this token"
                } )
            },
            _ => Err( SyntaxError {
                line,
                token,
                msg: "invalid expression",
                help_msg: "expected number literal"
            } ),
        };

        if let Ok( _ ) = number {
            tokens.next();
        }

        return number;
    }

    fn power<'program>( tokens: &mut LexerTokenIter<'program> ) -> Result<Option<OpKind>, SyntaxError<'program>> {
        let (_line, token) = tokens.peek_non_whitespace( "expected expression or semicolon" )?;

        let op = match token.kind {
            TokenKind::Op( op @ OpKind::Pow ) => Ok( Some( op ) ),
            TokenKind::SemiColon | _ => Ok( None ),
        };

        if let Ok( Some( _ ) ) = op {
            tokens.next();
        }

        return op;
    }

    fn exponentiation<'program>( tokens: &mut LexerTokenIter<'program> ) -> Result<Node, SyntaxError<'program>> {
        let mut lhs = Self::factor( tokens )?;

        while let Some( op ) = Self::power( tokens )? {
            let rhs = Self::factor( tokens )?;
            lhs = Node::Expression { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn times_or_divide<'program>( tokens: &mut LexerTokenIter<'program> ) -> Result<Option<OpKind>, SyntaxError<'program>> {
        let (_line, token) = tokens.peek_non_whitespace( "expected expression or semicolon" )?;

        let op = match token.kind {
            TokenKind::Op( op @ OpKind::Times | op @ OpKind::Divide ) => Ok( Some( op ) ),
            TokenKind::SemiColon | _ => Ok( None ),
        };

        if let Ok( Some( _ ) ) = op {
            tokens.next();
        }

        return op;
    }

    fn term<'program>( tokens: &mut LexerTokenIter<'program> ) -> Result<Node, SyntaxError<'program>> {
        let mut lhs = Self::exponentiation( tokens )?;

        while let Some( op ) = Self::times_or_divide( tokens )? {
            let rhs = Self::exponentiation( tokens )?;
            lhs = Node::Expression { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn plus_or_minus<'program>( tokens: &mut LexerTokenIter<'program> ) -> Result<Option<OpKind>, SyntaxError<'program>> {
        let (_line, token) = tokens.peek_non_whitespace( "expected expression or semicolon" )?;

        let op = match token.kind {
            TokenKind::Op( op @ OpKind::Plus | op @ OpKind::Minus ) => Ok( Some( op ) ),
            TokenKind::CloseRoundBracket | TokenKind::SemiColon => Ok( None ),
            _ => {
                let (previous_line, previous_token) = tokens.next_back_non_whitespace( "" ).unwrap();

                let err = match token.kind {
                    TokenKind::Literal( _ ) => Err( SyntaxError {
                        line: previous_line,
                        token: previous_token,
                        msg: "invalid expression",
                        help_msg: "expected '+', '-', '*', '/' or '^' after this token to complete the expression"
                    } ),
                    _ => Err( SyntaxError {
                        line: previous_line,
                        token: previous_token,
                        msg: "invalid expression or missing semicolon",
                        help_msg: "expected '+', '-', '*', '/' or '^' after this token to complete the expression, or a ';' after this token to end the previous statement"
                    } ),
                };

                tokens.next_non_whitespace( "" ).unwrap();
                err
            },
        };

        if let Ok( Some( _ ) ) = op {
            tokens.next();
        }

        return op;
    }

    fn expression<'program>( tokens: &mut LexerTokenIter<'program> ) -> Result<Node, SyntaxError<'program>> {
        let mut lhs = Self::term( tokens )?;

        while let Some( op ) = Self::plus_or_minus( tokens )? {
            let rhs = Self::term( tokens )?;
            lhs = Node::Expression { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }
}


#[derive( Debug )]
struct Program;

impl Program {
    fn evaluate_expression<'ast>( expression: &'ast Node ) -> i64 {
        return match expression {
            Node::Literal( value ) => match *value {
                LiteralKind::I64 { value, .. } => value,
            },
            Node::Expression{ lhs, op, rhs } => match op {
                OpKind::Plus => Self::evaluate_expression( lhs ) + Self::evaluate_expression( rhs ),
                OpKind::Minus => Self::evaluate_expression( lhs ) - Self::evaluate_expression( rhs ),
                OpKind::Times => Self::evaluate_expression( lhs ) * Self::evaluate_expression( rhs ),
                OpKind::Divide => Self::evaluate_expression( lhs ) / Self::evaluate_expression( rhs ),
                OpKind::Pow => Self::evaluate_expression( lhs ).pow( Self::evaluate_expression( rhs ) as u32 ),
            },
            _ => unreachable!(),
        };
    }

    fn interpret( ast: &AST, file_path: &str ) {
        println!( "\x1b[92;1mIntepreting\x1b[0m: {}", file_path );

        for node in &ast.nodes {
            match &node {
                Node::PrintChar( number ) => match **number {
                    Node::Literal( _ ) |
                    Node::Expression { .. } => print!( "{}", Self::evaluate_expression( &*number ) as u8 as char ),
                    _ => unreachable!(),
                },
                Node::Print( number ) => match **number {
                    Node::Literal( _ ) |
                    Node::Expression { .. } => print!( "{}", Self::evaluate_expression( &*number ) ),
                    _ => unreachable!(),
                }
                _ => (),
            }
        }
    }

    fn compile( ast: &AST ) -> String {
        let mut src_code = String::new();

        for node in &ast.nodes {
            let expression_asm = match &node {
                Node::PrintChar( character_node ) => {
                    let character = match **character_node {
                        Node::Literal( literal ) => match literal {
                            LiteralKind::I64 { value, .. } => format!( "{}", value ),
                        },
                        // Node::Expression { .. } => src_code.push_str( &format!( "{}", Self::evaluate_expression( &*number ) ) ),
                        _ => unreachable!()
                    };

                    format!( " push {character}\
                        \n mov rdi, stdout\
                        \n mov rsi, rsp\
                        \n mov rdx, 1\
                        \n mov rax, SYS_write\
                        \n syscall\
                        \n pop rsi\
                        \n\n"
                    )
                },
                Node::Print( number_node ) => {
                    let number = match **number_node {
                        Node::Literal( literal ) => match literal {
                            LiteralKind::I64 { value, .. } => format!( "mov rdi, {}", value ),
                        },
                        Node::Expression { .. } => Self::compile_expression( &number_node ),
                        _ => unreachable!()
                    };

                    format!( "{number}\
                        \n mov rsi, 10\
                        \n call int_toStr\
                        \n\
                        \n mov rdi, stdout\
                        \n mov rsi, rax\
                        \n mov rdx, rdx\
                        \n mov rax, SYS_write\
                        \n syscall\
                        \n\n"
                    )
                },
                Node::Literal( literal ) => match literal {
                    LiteralKind::I64 { value, .. } => format!( " mov rdi, {}\n", value ),
                },
                Node::Expression { .. } => Self::compile_expression( node ),
            };

            src_code.push_str( &expression_asm );
        }

        return src_code;
    }

    fn compile_expression( node: &Node ) -> String {
        match &node {
            Node::Literal( literal ) => match literal {
                LiteralKind::I64 { value, .. } => return format!( "{}", value ),
            },
            Node::Expression { lhs, op, rhs } => match op {
                OpKind::Plus => {
                    let lhs_asm = format!( "{}", &Self::compile_expression( lhs ) );
                    let rhs_asm = format!( "{}", &Self::compile_expression( rhs ) );

                    return format!( " mov rdi, {}\
                        \n add rdi, {}",
                        lhs_asm, rhs_asm
                    );
                }
                // OpKind::Minus => Self::evaluate_expression( lhs ) - Self::evaluate_expression( rhs ),
                // OpKind::Times => Self::evaluate_expression( lhs ) * Self::evaluate_expression( rhs ),
                // OpKind::Divide => Self::evaluate_expression( lhs ) / Self::evaluate_expression( rhs ),
                // OpKind::Pow => Self::evaluate_expression( lhs ).pow( Self::evaluate_expression( rhs ) as u32 ),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn build( ast: &AST, file_path: &str ) -> Result<PathBuf, ()> {
        let src_file_path = Path::new( file_path );
        println!( "\x1b[92;1mBuilding\x1b[0m: {}", src_file_path.display() );

        let asm_file_path = src_file_path.with_extension( "asm" );
        let mut asm_file = BufWriter::new( File::create( &asm_file_path ).unwrap() );

        let preamble =
r"global _start

section .rodata
 stdout: equ 1
 SYS_write: equ 1
 SYS_exit: equ 60
 EXIT_SUCCESS: equ 0
 newline: db 10

 I64_MIN: equ 1 << 63
 I64_MAX: equ ~I64_MIN

 INT_MAX_DIGITS: equ 64

section .data
 int_str: times INT_MAX_DIGITS + 1 db 0
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
r"
int_pow:
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

        let user_program = Self::compile( &ast );

        let program = format!(
r"{}

section .text
{}

{}


_start:
{}{}
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

    fn run( ast: &AST, file_path: &str ) -> Result<(), ()> {
        let executable_file_path = Self::build( &ast, &file_path )?;

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
    interpret <file.blz>    Run the program in interpret mode
    build     <file.blz>    Compile the program down to a binary executable
    run       <file.blz>    Compile and run the generated binary executable
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

    let lexer = match Lexer::parse( &source_file_path, source_file ) {
        Ok( lexer ) => {
            // println!( "{:?}\n", lexer );
            lexer
        },
        Err( errors ) => {
            eprint!( "{}", errors );
            return ExitCode::FAILURE;
        },
    };

    let ast = match AST::parse( &lexer ) {
        Ok( ast ) => {
            // println!( "{}", parser );
            ast
        },
        Err( errors ) => {
            eprint!( "{}", errors );
            return ExitCode::FAILURE;
        },
    };

    if interpret_flag {
        Program::interpret( &ast, &source_file_path );
    }
    else if build_flag {
        Program::build( &ast, &source_file_path ).unwrap();
    }
    else if run_flag {
        Program::run( &ast, &source_file_path ).unwrap();
    }

    return ExitCode::SUCCESS;
}
