// TODO implement NOTE, HINT, HELP in error messages
use std::{io::{BufReader, BufRead, ErrorKind}, fs::File, env, process::ExitCode, fmt::Display, iter::Peekable};


#[derive( Debug, PartialEq, Clone, Copy )]
enum LiteralKind {
    U64{ base: u8, value: u64 }
}

impl Display for LiteralKind {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        match self {
            Self::U64 { value, .. } => write!( f, "{}", value ),
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
    Print, // temporary way of printing numbers
    PrintChar, // temporary way of printing numbers interpreted as ascii characters
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
            Self::Unexpected { text, .. } => write!( f, "{}", text ),
            // Self::OpenRoundBracket => write!( f, "(" ),
            // Self::CloseRoundBracket => write!( f, ")" ),

            Self::Literal( literal ) => write!( f, "{}", literal ),
            Self::Comment( text ) => write!( f, "{}", text ),

            Self::Print => write!( f, "print" ),
            Self::PrintChar => write!( f, "print_char" ),

            Self::Plus => write!( f, "+" ),
            Self::Minus => write!( f, "-" ),
            Self::Times => write!( f, "*" ),
            Self::Divide => write!( f, "/" ),
            Self::Pow => write!( f, "^" ),
            // Self::Equals => write!( f, "=" ),
            Self::SemiColon => write!( f, ";" ),

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
    fn parse( file_path: String, source_file: File ) -> Result<Self, Self> {
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


// TODO try fixing the implementation of next/peek or implement my own
// TODO create method current()
#[derive( Debug )]
struct LexerTokenIter<'lexer> {
    lexer: &'lexer Lexer,
    line: isize,
    token: isize,
}

impl<'lexer> Iterator for LexerTokenIter<'lexer> {
    type Item = (&'lexer Line, &'lexer Token);

    fn next( &mut self ) -> Option<Self::Item> {
        if (self.line as usize) >= self.lexer.lines.len() {
            return None;
        }

        let line = &self.lexer.lines[ self.line as usize ];
        let tokens = &line.tokens;
        if (self.token as usize) < tokens.len() {
            let token = &tokens[ self.token as usize ];
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
enum Node {
    Literal( LiteralKind ),
    // UnaryOp{ op: UnaryOpKind, rhs: LiteralKind },
    Expression{ lhs: Box<Node>, op: BinaryOpKind, rhs: Box<Node> },
    Print( Box<Node> ),
    PrintChar( Box<Node> ),
}

impl Display for Node {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        match self {
            Self::Literal( literal ) => write!( f, "{}", literal ),
            Self::Expression{ lhs, op, rhs } => match op {
                BinaryOpKind::Divide | BinaryOpKind::Times | BinaryOpKind::Pow => write!( f, "({} {} {})", lhs, op, rhs ),
                _ => write!( f, "{} {} {}", lhs, op, rhs ),
            },
            Self::Print( node ) => write!( f, "print {}", node ),
            Self::PrintChar( ascii ) => write!( f, "print {}", ascii ),
        }
    }
}


// TODO extract line to separate struct => SyntaxErrors { line: Line, errors: Vec<SyntaxError> }
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
struct Parser {
    ast: Vec<Node>,
}

impl Parser {
    fn parse<'program>( lexer: &'program Lexer ) -> Result<Self, SyntaxErrors<'program>> {
        let mut errors: Vec<SyntaxError> = Vec::new();
        let mut statements: Vec<Node> = Vec::new();
        let mut tokens = lexer.into_iter().peekable();

        while let Some( (line, token) ) = tokens.peek() {
            let line = *line;
            let statement_result = match &token.kind {
                TokenKind::Comment( _ ) | TokenKind::EOF | TokenKind::SemiColon /* | TokenKind::Newline */ => {
                    tokens.next();
                    continue;
                },
                TokenKind::Print => {
                    tokens.next();
                    let factor_result = Parser::factor( &mut tokens );
                    match factor_result {
                        Ok( factor ) => Ok( Node::Print( Box::new( factor ) ) ),
                        Err( _ ) => factor_result,
                    }
                },
                TokenKind::PrintChar => {
                    tokens.next();
                    let factor_result = Parser::factor( &mut tokens );
                    match factor_result {
                        Ok( factor ) => Ok( Node::PrintChar( Box::new( factor ) ) ),
                        Err( _ ) => factor_result,
                    }
                },
                TokenKind::Plus | TokenKind::Minus | TokenKind::Times | TokenKind::Divide | TokenKind::Pow => {
                    let node = Err( SyntaxError {
                        line,
                        token,
                        msg: "expected number literal",
                        help_msg: "stray binary operation"
                    } );
                    tokens.next();
                    node
                },
                TokenKind::Literal( _ ) => Parser::factor( &mut tokens ),
                TokenKind::Unexpected { .. } => unreachable!(),
            };

            match statement_result {
                Ok( statement ) => statements.push( statement ),
                Err( error ) => errors.push( error ),
            };

            let next = tokens.peek();
            match next {
                Some( (_, Token{ kind: TokenKind::SemiColon, .. }) ) => (),
                Some( (line, token) ) => {
                    let help_msg = match token.kind {
                        TokenKind::EOF => "put a semicolon here to end the previous statement",
                        _ => "put a semicolon before this token to end the previous statement",
                    };

                    errors.push( SyntaxError {
                        line,
                        token,
                        msg: "missing semicolon",
                        help_msg
                    } );

                },
                None => unreachable!(),
            }
        }

        if !errors.is_empty() {
            return Err( SyntaxErrors { file_path: lexer.file_path.clone(), errors } );
        }
        else {
            return Ok( Self { ast: statements } );
        }
    }

    // TODO check for semicolon at end of statement
    fn skip_to_next_token<'program>(
        tokens: &mut Peekable<LexerTokenIter<'program>>, err_msg: &'static str
    ) -> Result<(&'program Line, &'program Token), SyntaxError<'program>>
    {
        match tokens.peek() {
            Some( (line, token) ) => match token.kind {
                TokenKind::EOF => return Err( SyntaxError {
                    line,
                    token,
                    msg: err_msg,
                    help_msg: "file ended here instead"
                } ),
                /* TokenKind::Newline | */ TokenKind::Comment( _ ) => {
                    tokens.next();
                    return Self::skip_to_next_token( tokens, err_msg );
                },
                _ => return Ok( (line, token) ),
            },
            None => unreachable!(),
        }
    }

    fn number<'program>( tokens: &mut Peekable<LexerTokenIter<'program>> ) -> Result<Node, SyntaxError<'program>> {
        let (line, token) = Self::skip_to_next_token( tokens, "expected number literal" )?;

        let number = match token.kind {
            TokenKind::Literal( literal ) => match literal {
                LiteralKind::U64 { .. } => Ok( Node::Literal( literal ) ),
            },
            _ => Err( SyntaxError {
                line,
                token,
                msg: "unexpected token",
                help_msg: "expected number literal"
            } ),
        };

        if let Ok( _ ) = number {
            tokens.next();
        }

        return number;
    }

    fn term_op<'program>( tokens: &mut Peekable<LexerTokenIter<'program>> ) -> Result<Option<BinaryOpKind>, SyntaxError<'program>> {
        let (line, token) = Self::skip_to_next_token( tokens, "expected binary operator" )?;

        let op = match token.kind {
            TokenKind::Times => Ok( Some( BinaryOpKind::Times ) ),
            TokenKind::Divide => Ok( Some( BinaryOpKind::Divide ) ),
            TokenKind::Pow => Ok( Some( BinaryOpKind::Pow ) ),
            TokenKind::Literal( _ ) => Err( SyntaxError {
                line,
                token,
                msg: "expected binary operator",
                help_msg: "expected '+', '-', '*', '/' or '^' before this token, or a ';' to end the previous statement"
            } ),
            _ => return Ok( None ),
        };

        if let Ok( _ ) = op {
            tokens.next();
        }

        return op;
    }

    fn term<'program>( tokens: &mut Peekable<LexerTokenIter<'program>> ) -> Result<Node, SyntaxError<'program>> {
        let mut lhs = Self::number( tokens )?;

        while let Some( op ) = Self::term_op( tokens )? {
            let rhs = Self::number( tokens )?;
            lhs = Node::Expression { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn factor_op<'program>( tokens: &mut Peekable<LexerTokenIter<'program>> ) -> Result<Option<BinaryOpKind>, SyntaxError<'program>> {
        let (_, token) = Self::skip_to_next_token( tokens, "expected binary operator" )?;

        let op = match token.kind {
            TokenKind::Plus => Ok( Some( BinaryOpKind::Plus ) ),
            TokenKind::Minus => Ok( Some( BinaryOpKind::Minus ) ),
            _ => return Ok( None ),
        };

        if let Ok( _ ) = op {
            tokens.next();
        }

        return op;
    }

    fn factor<'program>( tokens: &mut Peekable<LexerTokenIter<'program>> ) -> Result<Node, SyntaxError<'program>> {
        let mut lhs = Self::term( tokens )?;

        while let Some( op ) = Self::factor_op( tokens )? {
            let rhs = Self::term( tokens )?;
            lhs = Node::Expression { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }
}


#[derive( Debug )]
struct Program;

impl Program {
    fn interpret( parser: &Parser ) {
        for node in &parser.ast {
            match &node {
                Node::PrintChar( number ) => match **number {
                    Node::Literal( value ) => match value {
                        LiteralKind::U64 { value, .. } => print!( "{}", value as u8 as char ),
                    },
                    Node::Expression { .. } => print!( "{}", Self::evaluate_expression( &*number ) as u8 as char ),
                    _ => unreachable!(),
                }
                Node::Print( number ) => match **number {
                    Node::Literal( value ) => match value {
                        LiteralKind::U64 { value, .. } => print!( "{}", value ),
                    },
                    Node::Expression { .. } => print!( "{}", Self::evaluate_expression( &*number ) ),
                    _ => unreachable!(),
                }
                _ => (),
            }
        }
    }

    // TODO make non recursive
    fn _evaluate_expression<'a>( expression: &'a Node, result: u64 ) -> u64 {
        let value = match expression {
            Node::Expression{ lhs, op, rhs } => match op {
                BinaryOpKind::Plus => Self::evaluate_expression( lhs ) + Self::evaluate_expression( rhs ),
                BinaryOpKind::Minus => Self::evaluate_expression( lhs ) - Self::evaluate_expression( rhs ),
                BinaryOpKind::Times => Self::evaluate_expression( lhs ) * Self::evaluate_expression( rhs ),
                BinaryOpKind::Divide => Self::evaluate_expression( lhs ) / Self::evaluate_expression( rhs ),
                BinaryOpKind::Pow => Self::evaluate_expression( lhs ).pow( Self::evaluate_expression( rhs ) as u32 ),
            },
            Node::Literal( value ) => match *value {
                LiteralKind::U64 { value, .. } => value,
            },
            _ => unreachable!(),
        };

        return result + value;
    }

    fn evaluate_expression<'a>( expression: &'a Node ) -> u64 {
        return Self::_evaluate_expression( expression, 0 );
    }

}


// IDEA add man page
fn print_usage() {
    println!( r"
Blitzlang compiler, version {}

Usage: blitz [Options] [Commands] file.blz

Options:
    -h, --help              Display this message

Commands:
    TODO: interpret <file.blz>    Run the program in interpret mode
    TODO: build <file.blz>        Compile the program down to a binary executable
    TODO: run <file.blz>          Compile and run the generated binary executable
",
        env!( "CARGO_PKG_VERSION" )
    );
}

fn main() -> ExitCode {
    let mut args: Vec<String> = env::args().collect();

    // to quickly debug
    args.push( "interpret".to_string() );
    args.push( "examples/main.blz".to_string() );

    if args.len() < 2 {
        print_usage();
        return ExitCode::SUCCESS;
    }

    let mut source_file_path: Option<String> = None;
    for arg in args.into_iter().skip( 1 ) { // skipping the name of this executable
        match arg.as_str() {
            "-h" | "--help" => {
                print_usage();
                return ExitCode::SUCCESS;
            },
            "interpret" => (),
            "build" => (),
            "run" => (),
            _ => if source_file_path.is_none() {
                    source_file_path = Some( arg );
                }
                else {
                    eprintln!( "Error: too many source file paths provided!" );
                    return ExitCode::FAILURE;
                },
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

    let parser = match Parser::parse( &lexer ) {
        Ok( parser ) => {
            // println!( "{}", parser );
            parser
        },
        Err( errors ) => {
            eprint!( "{}", errors );
            return ExitCode::FAILURE;
        },
    };

    Program::interpret( &parser );
    return ExitCode::SUCCESS;
}
