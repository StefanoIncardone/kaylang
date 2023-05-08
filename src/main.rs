// TODO implement NOTE, HINT, HELP in error messages
// TODO try to unify the errors reporting and struct handling
use std::{io::{BufReader, BufRead, ErrorKind}, fs::File, env, process::ExitCode, fmt::Display, iter::Peekable};


#[derive( Debug, PartialEq, Clone, Copy )]
enum LiteralKind {
    U64{ base: u8, value: u64 }
}

impl Display for LiteralKind {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        match self {
            Self::U64 { base: _, value } => write!( f, "{}", value ),
        }
    }
}


#[derive( Debug, PartialEq )]
enum TokenKind {
    Unexpected{ text: String, err_msg: &'static str, help_msg: &'static str },

    // OpenRoundBracket,
    // CloseRoundBracket,
    // OpenSquareBracket,
    // CloseSquareBracket,
    // OpenCurlyBracket,
    // CloseCurlyBracket,

    Literal( LiteralKind ),
    // Identifier( String ),
    Comment( String ),

    // Keywords
    // Entry,
    // Fn,
    // Let,
    // Const,
    // Var,
    // Return,

    // Symbols and operators
    Plus,
    Minus,
    Times,
    Divide,
    Pow,
    // Equals,
    // Colon,
    SemiColon,

    // Special
    // Newline,
    EOF,
}

impl Display for TokenKind {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        match self {
            Self::Unexpected { text, err_msg: _, help_msg: _ } => write!( f, "{}", text ),
            // Self::OpenRoundBracket => write!( f, "(" ),
            // Self::CloseRoundBracket => write!( f, ")" ),
            Self::Literal( literal ) => write!( f, "{}", literal ),
            Self::Comment( text ) => write!( f, "{}", text ),
            Self::Plus => write!( f, "+" ),
            Self::Minus => write!( f, "-" ),
            Self::Times => write!( f, "*" ),
            Self::Divide => write!( f, "/" ),
            Self::SemiColon => write!( f, ";" ),
            Self::Pow => write!( f, "^" ),
            /* Self::Newline | */ Self::EOF => write!( f, "" ),
            // Self::Equals => write!( f, "=" ),
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
    fn parse( file_path: String, source_file: File ) -> Result<Self, Self> {
        let mut found_errors = false;
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
                    // '(' => Token { kind: TokenKind::OpenRoundBracket, col },
                    // ')' => Token { kind: TokenKind::CloseRoundBracket, col },
                    // '[' => Token { kind: TokenKind::OpenSquareBracket, col },
                    // ']' => Token { kind: TokenKind::CloseSquareBracket, col },
                    // '{' => Token { kind: TokenKind::OpenCurlyBracket, col },
                    // '}' => Token { kind: TokenKind::CloseCurlyBracket, col },
                    '+' => Token { col, len: 1, kind: TokenKind::Plus },
                    '-' => Token { col, len: 1, kind: TokenKind::Minus },
                    '*' => Token { col, len: 1, kind: TokenKind::Times },
                    '/' => Token { col, len: 1, kind: TokenKind::Divide },
                    '^' => Token { col, len: 1, kind: TokenKind::Pow },
                    // '=' => Token { kind: TokenKind::Equals, col },
                    // ':' => Token { kind: TokenKind::Colon, col },
                    ';' => Token { col, len: 1, kind: TokenKind::SemiColon },
                    '#' => {
                        token_text.clear();
                        token_text.push( ch );
                        
                        // consume the rest of the tokens in the current line
                        while let Some( next ) = src.next_if( |c| !matches!( c, '\r' | '\n' ) ) {
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
                                Ok( value ) => TokenKind::Literal( LiteralKind::U64{ base: 10, value } ),
                                Err( _ ) => TokenKind::Unexpected {
                                    text: token_text.clone(),
                                    err_msg: "expected number literal",
                                    help_msg: "overflows a 64 bit unsigned integer [0, 18446744073709551615]"
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
                    // 'a'..='z' | 'A'..='Z' | '_' => {
                    //     token_text.clear();
                    //     token_text.push( ch );

                    //     while let Some( next ) = src.next_if( |c| matches!( c, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' ) ) {
                    //         token_text.push( next );
                    //     }

                    //     let kind = match token_text.as_str() {
                    //         // "entry" => TokenKind::Entry,
                    //         // "fn" => TokenKind::Fn,
                    //         // "const" => TokenKind::Const,
                    //         // "let" => TokenKind::Let,
                    //         // "var" => TokenKind::Var,
                    //         // "return" => TokenKind::Return,
                    //         // _ => TokenKind::Identifier( current_token_text.clone() )
                    //         _ => TokenKind::Unexpected {
                    //             token_text: token_text.clone(),
                    //             err_msg: "unexpected token",
                    //             help_msg: "here"
                    //         },
                    //     };

                    //     let token = Token { kind, col };
                    //     col += token_text.len() - 1;
                    //     token
                    // },
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

                if let TokenKind::Unexpected { text: _, err_msg: _, help_msg: _ } = token.kind {
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
                found_errors = true;
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

        if found_errors {
            return Err( Self { file_path, lines: errors } );
        }
        else {
            return Ok( Self { file_path, lines } );
        }
    }
}

impl Display for Lexer {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        for line in &self.lines {
            let line_text = format!( "{}", line );

            for token in &line.tokens {
                if let TokenKind::Unexpected { text: _, err_msg, help_msg } = &token.kind {
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
    type Item = (usize, &'lexer Token);
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

impl<'lexer> Iterator for LexerTokenIter<'lexer> {
    type Item = (usize, &'lexer Token);

    fn next( &mut self ) -> Option<Self::Item> {
        if self.line < self.lexer.lines.len() {
            let tokens = &self.lexer.lines[ self.line ].tokens;
            if self.token < tokens.len() {
                let token = &tokens[ self.token ];
                self.token += 1;
                return Some( (self.line, token) );
            }
            else {
                self.line += 1;
                self.token = 0;

                return self.next();
            }
        }
        else {
            return None;
        }
    }
}


// #[derive( Debug )]
// enum UnaryOpKind {
//     Plus,
//     Minus,
// }

// impl Display for UnaryOpKind {
//     fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
//         match self {
//             Self::Plus => write!( f, "+" ),
//             Self::Minus => write!( f, "-" ),
//         }
//     }
// }


// #[derive( Debug )]
// enum UnaryOpKind {
//     Plus,
//     Minus,
// }

// impl Display for UnaryOpKind {
//     fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
//         match self {
//             Self::Plus => write!( f, "+" ),
//             Self::Minus => write!( f, "-" ),
//         }
//     }
// }


#[derive( Debug )]
enum BinaryOpKind {
    Plus,
    Minus,
    Times,
    Divide,
    Pow,
}

impl Display for BinaryOpKind {
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


// #[derive( Debug )]
// enum OpKind {
//     Unary{ lhs: LiteralKind, op: UnaryOpKind },
//     Binary{ lhs: LiteralKind, op: BinaryOpKind, rhs: LiteralKind },
// }

// impl Display for OpKind {
//     fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
//         match self {
//             Self::Unary{ lhs, op } => write!( f, "{} {}", lhs,op ),
//             Self::Binary{ lhs, op, rhs } => write!( f, "{} {} {}", lhs, op, rhs ),
//         }
//     }
// }


#[derive( Debug )]
enum Node<'program> {
    Unexpected{ token: &'program Token, err_msg: &'static str, help_msg: &'static str },

    Literal( LiteralKind ),
    // UnaryOp{ op: UnaryOpKind, rhs: LiteralKind },
    BinaryOp{ lhs: Box<Node<'program>>, op: BinaryOpKind, rhs: Box<Node<'program>> },
}

impl<'program> Display for Node<'program> {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        match self {
            Self::Literal( literal ) => write!( f, "{}", literal ),
            Self::BinaryOp{ lhs, op, rhs } => match op {
                BinaryOpKind::Divide | BinaryOpKind::Times | BinaryOpKind::Pow => write!( f, "({} {} {})", lhs, op, rhs ),
                _ => write!( f, "{} {} {}", lhs, op, rhs ),
            },
            _ => write!( f, "{:?}", self ),
        }
    }
}


#[derive( Debug )]
struct Statement<'program> {
    line: &'program Line,
    node: Node<'program>,
}

#[derive( Debug )]
struct Parser<'program> {
    lexer: &'program Lexer,
    statements: Vec<Statement<'program>>,
}

impl<'program> Parser<'program> {
    fn parse( lexer: &'program Lexer ) -> Result<Self, Self> {
        let mut found_errors = false;
        let mut errors: Vec<Statement> = Vec::new();
        let mut statements: Vec<Statement> = Vec::new();
        let mut tokens = lexer.into_iter().peekable();

        // TODO fix errors occurring while parsing expressions that span multiple lines
        while let Some( (line, token) ) = tokens.peek() {
            let line = &lexer.lines[ *line ];
            let node = match &token.kind {
                TokenKind::Comment( _ ) | TokenKind::EOF | TokenKind::SemiColon /* | TokenKind::Newline */ => {
                    tokens.next();
                    continue;
                },
                TokenKind::Plus | TokenKind::Minus | TokenKind::Times | TokenKind::Divide | TokenKind::Pow => {
                    let node = Node::Unexpected {
                        token,
                        err_msg: "expected number literal",
                        help_msg: "stray binary operation"
                    };
                    tokens.next();
                    node
                }
                TokenKind::Literal( _ ) => Parser::factor( &mut tokens ),
                _ => {
                    let node = Node::Unexpected {
                        token,
                        err_msg: "unexpected token",
                        help_msg: "here"
                    };
                    tokens.next();
                    node
                },
            };

            let statement = Statement { line, node };
            match statement.node {
                Node::Unexpected { token: _, err_msg: _, help_msg: _ } => {
                    found_errors = true;
                    errors.push( statement )
                },
                _ => statements.push( statement ),
            }

            let next = tokens.peek();
            match next {
                Some( (_, Token{ col: _, len: _, kind: TokenKind::SemiColon }) ) => (),
                Some( (_, next_token) ) => {
                    found_errors = true;
                    let help_msg = match next_token.kind {
                        TokenKind::EOF => "put a semicolon here to end the previous statement",
                        _ => "put a semicolon before this token to end the previous statement",
                    };

                    errors.push( Statement { line, node: Node::Unexpected {
                        token: next_token,
                        err_msg: "expected semicolon",
                        help_msg
                    } } );
                },
                None => unreachable!(),
            }
        }

        if found_errors {
            return Err( Self { lexer, statements: errors } );
        }
        else {
            return Ok( Self { lexer, statements } );
        }
    }


    fn skip_comments_and_newlines( tokens: &mut Peekable<LexerTokenIter<'program>>, err_msg: &'static str ) -> Result<&'program Token, Node<'program>> {
        match tokens.peek() {
            Some( (_, token) ) => match token.kind {
                TokenKind::EOF => Err( Node::Unexpected {
                    token,
                    err_msg,
                    help_msg: "file ended here instead"
                } ),
                /* TokenKind::Newline | */ TokenKind::Comment( _ ) => {
                    tokens.next();
                    return Self::skip_comments_and_newlines( tokens, err_msg );
                },
                _ => return Ok( token ),
            },
            None => unreachable!(),
        }
    }

    fn number( tokens: &mut Peekable<LexerTokenIter<'program>> ) -> Node<'program> {
        let token = match Self::skip_comments_and_newlines( tokens, "expected number literal" ) {
            Ok( t ) => t,
            Err( err ) => return err,
        };

        let number = match token.kind {
            TokenKind::Literal( literal ) => match literal {
                LiteralKind::U64 { base: _, value: _ } => Node::Literal( literal ),
            },
            _ => Node::Unexpected {
                token,
                err_msg: "unexpected token",
                help_msg: "expected number literal"
            },
        };
        
        if let Node::Unexpected { token: _, err_msg: _, help_msg: _ } = number {} else {
            tokens.next();
        }

        number
    }

    fn term_op( tokens: &mut Peekable<LexerTokenIter<'program>> ) -> Option<Result<BinaryOpKind, Node<'program>>> {
        let token = match Self::skip_comments_and_newlines( tokens, "expected binary operator" ) {
            Ok( t ) => t,
            Err( _ ) => return None,
        };
        
        let op = match token.kind {
            TokenKind::SemiColon => return None,
            TokenKind::Times => Some( Ok( BinaryOpKind::Times ) ),
            TokenKind::Divide => Some( Ok( BinaryOpKind::Divide ) ),
            TokenKind::Pow => Some( Ok( BinaryOpKind::Pow ) ),
            // _ => Some( Err( Node::Unexpected {
            //     token,
            //     err_msg: "expected binary operator",
            //     help_msg: "expected '*' or '/' operator before this token" }
            // ) ),
            _ => return None,
        };
        
        if let Some( Err( Node::Unexpected { token: _, err_msg: _, help_msg: _ } ) ) = op {} else {
            tokens.next();
        }

        op
    }

    fn term( tokens: &mut Peekable<LexerTokenIter<'program>> ) -> Node<'program> {
        let mut lhs = Self::number( tokens );
        if let Node::Unexpected { token: _, err_msg: _, help_msg: _ } = lhs {
            return lhs;
        }

        while let Some( op_result ) = Self::term_op( tokens ) {
            let op = match op_result {
                Ok( o ) => o,
                Err( err ) => return err,
            };

            let rhs = Self::number( tokens );
            if let Node::Unexpected { token: _, err_msg: _, help_msg: _ } = rhs {
                return rhs;
            }

            lhs = Node::BinaryOp { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return lhs;
    }

    fn factor_op( tokens: &mut Peekable<LexerTokenIter<'program>> ) -> Option<Result<BinaryOpKind, Node<'program>>> {
        let token = match Self::skip_comments_and_newlines( tokens, "expected binary operator" ) {
            Ok( t ) => t,
            Err( _ ) => return None,
        };

        let op = match token.kind {
            TokenKind::SemiColon => return None,
            TokenKind::Plus => Some( Ok( BinaryOpKind::Plus ) ),
            TokenKind::Minus => Some( Ok( BinaryOpKind::Minus ) ),
            // _ => Some( Err( Node::Unexpected {
            //     token,
            //     err_msg: "expected binary operator",
            //     help_msg: "expected '+' or '-' operator before this token" }
            // ) ),
            _ => return None,
        };

        if let Some( Err( Node::Unexpected { token: _, err_msg: _, help_msg: _ } ) ) = op {} else {
            tokens.next();
        }
        
        op
    }

    fn factor( tokens: &mut Peekable<LexerTokenIter<'program>> ) -> Node<'program> {
        let mut lhs = Self::term( tokens );
        if let Node::Unexpected { token, err_msg: _, help_msg: _ } = lhs {
            match token.kind {
                TokenKind::Plus | TokenKind::Minus => (),
                _ => return lhs,
            }
        }

        while let Some( op_result ) = Self::factor_op( tokens ) {
            let op = match op_result {
                Ok( o ) => o,
                Err( err ) => return err,
            };

            let rhs = Self::term( tokens );
            if let Node::Unexpected { token: _, err_msg: _, help_msg: _ } = rhs {
                return rhs;
            }

            lhs = Node::BinaryOp { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return lhs;
    }
}

impl<'program> Display for Parser<'program> {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        for statement in &self.statements {
            let line_text = format!( "{}", statement.line );

            match &statement.node {
                Node::Unexpected { token, err_msg, help_msg } => {
                    let gutter_padding_amount = statement.line.number.ilog10() as usize + 1;
                    let gutter_padding = " ".repeat( gutter_padding_amount );
                    let pointers_padding = " ".repeat( token.col - 1 );
                    let pointers = "^".repeat( token.len );
                    let bar = "\x1b[94m|\x1b[0m";

                    let error_visualization = &format!(
                        " {} {}\n \
                        \x1b[94m{: >gutter_padding_amount$}\x1b[0m {} {}\n \
                        {} {} {}\x1b[91m{} {}\x1b[0m",
                        gutter_padding, bar,
                        statement.line.number, bar, line_text,
                        gutter_padding, bar, pointers_padding, pointers, help_msg
                    );

                    writeln!( f,
                        "\x1b[91;1mError\x1b[0m [P]: \x1b[1m{}\x1b[0m\n \
                        {} \x1b[91min\x1b[0m: {}:{}:{}\n{}\n",
                        err_msg,
                        gutter_padding, self.lexer.file_path, statement.line.number, token.col, error_visualization
                    )?;
                },
                _ => writeln!( f, "{}", statement.node )?,
            }
        }

        return Ok( () );
    }
}


// IDEA add man page
fn print_usage() {
    println!( r"
Blitzlang compiler, version {}

Usage: blitz [Options] file.blz

Options:
    -h, --help Display this message
",
        env!( "CARGO_PKG_VERSION" )
    );
}

fn main() -> ExitCode {
    let mut args: Vec<String> = env::args().collect();

    // to quickly debug
    // args.push( "src/main.rs".to_string() );
    // args.push( "-h".to_string() );
    // args.push( "-v".to_string() );
    args.push( "examples/main.blz".to_string() );
    // args.push( "-s".to_string() );

    if args.len() < 2 {
        print_usage();
        return ExitCode::SUCCESS;
    }

    let mut source_file_path: Option<String> = None;
    for arg in args.into_iter().skip( 1 ) {
        if arg.starts_with( '-' ) {
            match arg.as_str() {
                "-h" | "--help" => {
                    print_usage();
                    return ExitCode::SUCCESS;
                },
                _ => {
                    eprintln!( "Error: unrecognized option flag!" );
                    return ExitCode::FAILURE;
                }
            }
        }
        else {
            match arg.as_str() {
                _ => if source_file_path.is_none() {
                        source_file_path = Some( arg );
                    }
                    else {
                        eprintln!( "Error: too many source file paths provided!" );
                        return ExitCode::FAILURE;
                    }
            }
        }
    }

    let source_file_path = match source_file_path {
        Some( path ) => path,
        None => {
            eprintln!( "Error: no source file path provided!" );
            return ExitCode::FAILURE;
        }
    };

    let source_file = match File::open( &source_file_path ) {
        Ok( file ) => file,
        Err( err ) => {
            let cause = match err.kind() {
                ErrorKind::NotFound => "file not found, or wrong file name",
                _ => "unknown error",
            };

            eprintln!( "Error: could not open file '{}'!\nCause: {}!", &source_file_path, cause );
            return ExitCode::FAILURE;
        },
    };

    let lexer = match Lexer::parse( source_file_path, source_file ) {
        Ok( lexer ) => {
            // println!( "{:?}\n", lexer );
            lexer
        },
        Err( errors ) => {
            eprint!( "{}", errors );
            return ExitCode::FAILURE;
        },
    };

    let _parser = match Parser::parse( &lexer ) {
        Ok( parser ) => {
            println!( "{}", parser );
            parser
        },
        Err( parser ) => {
            eprint!( "{}", parser );
            return ExitCode::FAILURE;
        },
    };

    return ExitCode::SUCCESS;
}
