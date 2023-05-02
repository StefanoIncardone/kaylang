// TODO create IDEAS.md
use std::{io::{BufReader, BufRead, ErrorKind}, fs::File, env, process::ExitCode, fmt::Display};


#[derive( Debug, PartialEq )]
enum LiteralKind {
    /*
        IDEA handle arbitrary bases: has to be bigger than 1 and smaller than 36 => {
            ...b1 = error,
            ...b2 = base 2,
            ...b4 = base 4,
            ...b35 = base 35,
            ...b37 = error,
        }
    */
    // IDEA let the user define new bases with custom symbols
    Int{ base: i8, value: i64 }
}

#[derive( Debug, PartialEq )]
enum TokenKind {
    Unexpected{ token_text: String, err_msg: &'static str, help_msg: &'static str },

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
    // SemiColon,
}

impl Display for TokenKind {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        match self {
            Self::Unexpected{ token_text, err_msg: _, help_msg: _ } => write!( f, "{}", token_text ),
            // Self::OpenRoundBracket => write!( f, "(" ),
            // Self::CloseRoundBracket => write!( f, ")" ),
            Self::Literal( kind ) => match kind {
                LiteralKind::Int{ base: _, value } => write!( f, "{}", value ),
            },
            Self::Comment( text ) => write!( f, "{}", text ),
            Self::Plus => write!( f, "+" ),
            Self::Minus => write!( f, "-" ),
            Self::Times => write!( f, "*" ),
            Self::Divide => write!( f, "/" ),
            Self::Pow => write!( f, "^" ),
            // Self::Equals => write!( f, "=" ),
        }
    }
}

#[derive( Debug )]
struct Token {
    col: usize,
    kind: TokenKind,
}

#[derive( Debug )]
struct Line {
    number: usize,
    tokens: Vec<Token>,
}

impl Display for Line {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        if self.tokens.len() == 0 {
            write!( f, "" )?;
            return Ok( () );
        }

        // leading whitespaces
        write!( f, "{}", " ".repeat( self.tokens[ 0 ].col - 1 ) )?;

        let mut tokens = self.tokens.iter().peekable();
        while let Some( token ) = tokens.next() {
            let spaces = if let Some( next_token ) = tokens.peek() {
                ((*next_token).col - token.col) - token.kind.to_string().len()
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

// TODO implement iterator, yielding a line at a time
impl Lexer {
    fn parse( file_path: String, source_file: File ) -> Result<Self, Self> {
        let mut found_errors = false;
        let mut errors: Vec<Line> = Vec::new();

        let mut lines: Vec<Line> = Vec::new();
        let mut token_text = String::new();

        let mut row: usize = 0;
        for src_line in BufReader::new( source_file ).lines() {
            if let Ok( current_line ) = src_line {
                let mut line_contains_errors = false;
                let mut col: usize = 1;
                row += 1;

                let mut line = Line{ number: row, tokens: Vec::new() };
                let mut src = current_line.chars().peekable();
                while let Some( ch ) = src.next() {
                    let token: Token = match ch {
                        // ignore whitespace
                        c if c.is_ascii_whitespace() => {
                            col += 1;
                            continue;
                        },
                        // '(' => Token{ kind: TokenKind::OpenRoundBracket, col },
                        // ')' => Token{ kind: TokenKind::CloseRoundBracket, col },
                        // '[' => Token{ kind: TokenKind::OpenSquareBracket, col },
                        // ']' => Token{ kind: TokenKind::CloseSquareBracket, col },
                        // '{' => Token{ kind: TokenKind::OpenCurlyBracket, col },
                        // '}' => Token{ kind: TokenKind::CloseCurlyBracket, col },
                        '+' => Token{ kind: TokenKind::Plus, col },
                        '-' => Token{ kind: TokenKind::Minus, col },
                        '*' => Token{ kind: TokenKind::Times, col },
                        '/' => Token{ kind: TokenKind::Divide, col },
                        '^' => Token{ kind: TokenKind::Pow, col },
                        // '=' => Token{ kind: TokenKind::Equals, col },
                        // ':' => Token{ kind: TokenKind::Colon, col },
                        // ';' => Token{ kind: TokenKind::SemiColon, col },
                        '#' => {
                            token_text.clear();
                            token_text.push( ch );

                            // consume the rest of the tokens in the current line
                            while let Some( next ) = src.next() {
                                token_text.push( next );
                            }

                            let token = Token{ kind: TokenKind::Comment( token_text.clone() ), col };
                            col += token_text.len() - 1;
                            token
                        },
                        '0'..='9' => {
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
                                    Ok( number ) => TokenKind::Literal( LiteralKind::Int{ base: 10, value: number } ),
                                    Err( _ ) => TokenKind::Unexpected{
                                        token_text: token_text.clone(),
                                        err_msg: "expected number literal",
                                        help_msg: "this overflows a 64 bit number [-9223372036854775808, 9223372036854775807]"
                                    },
                                }
                            }
                            else {
                                TokenKind::Unexpected{
                                    token_text: token_text.clone(),
                                    err_msg: "unexpected token",
                                    help_msg: "expected number literal"
                                }
                            };

                            let token = Token{ kind, col };
                            col += token_text.len() - 1;
                            token
                        },
                        'a'..='z' | 'A'..='Z' | '_' => {
                            token_text.clear();
                            token_text.push( ch );

                            while let Some( next ) = src.next_if( |c| matches!( c, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' ) ) {
                                token_text.push( next );
                            }

                            let kind = match token_text.as_str() {
                                // "entry" => TokenKind::Entry,
                                // "fn" => TokenKind::Fn,
                                // "const" => TokenKind::Const,
                                // "let" => TokenKind::Let,
                                // "var" => TokenKind::Var,
                                // "return" => TokenKind::Return,
                                // _ => TokenKind::Identifier( current_token_text.clone() )
                                _ => TokenKind::Unexpected{
                                    token_text: token_text.clone(),
                                    err_msg: "unexpected token",
                                    help_msg: "only numbers are allowed for now"
                                },
                            };

                            let token = Token{ kind, col };
                            col += token_text.len() - 1;
                            token
                        },
                        _ => Token{ col, kind: TokenKind::Unexpected{
                                token_text: ch.to_string(),
                                err_msg: "unexpected token",
                                help_msg: "only numbers are allowed for now"
                        } },
                    };

                    if let TokenKind::Unexpected{ token_text: _, err_msg: _, help_msg: _ } = token.kind {
                        line_contains_errors = true;
                    }

                    line.tokens.push( token );
                    col += 1;
                }

                // skip empty lines
                if line.tokens.len() > 0 {
                    if line_contains_errors {
                        found_errors = true;
                        errors.push( line );
                    }
                    else {
                        lines.push( line );
                    }
                }
            }
        }

        return if found_errors {
            Err( Self{ file_path, lines: errors } )
        }
        else {
            Ok( Self{ file_path, lines } )
        }
    }
}

impl Display for Lexer{
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        for line in &self.lines {
            let line_text = format!( "{}", line );
            for token in &line.tokens {
                if let TokenKind::Unexpected{ token_text, err_msg, help_msg } = &token.kind {
                    let gutter_padding_amount = line.number.ilog10() as usize + 1;
                    let gutter_padding = " ".repeat( gutter_padding_amount );
                    let pointers_padding = " ".repeat( token.col - 1 );
                    let pointers = "^".repeat( token_text.len() );
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
                        "\x1b[91;1mError\x1b[0m: \x1b[1m{}\x1b[0m\n \
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

#[derive( Debug )]
enum Value {
    I64{ base: i8, value: i64 },
}

#[derive( Debug )]
enum BinaryOpKind {
    Plus,
    Minus,
    Times,
    Divide,
    Pow,
}

#[derive( Debug )]
enum Node<'program> {
    Unexpected{ token: &'program Token, err_msg: &'static str, help_msg: &'static str },

    Literal( Value ),
    BinaryOp{ lhs: Value, op: BinaryOpKind, rhs: Value },
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

        for line in &lexer.lines {
            let mut tokens = line.tokens.iter().peekable();

            while let Some( token ) = tokens.next() {
                let node = match &token.kind {
                    TokenKind::Comment( _ ) => continue,
                    TokenKind::Literal( literal ) => match literal {
                        LiteralKind::Int{ base, value } => {
                            let lhs = Value::I64{ base: *base, value: *value };
                            
                            let op_token = tokens.next();
                            let op_node = match op_token {
                                Some( next ) => match next.kind {
                                    TokenKind::Plus => Some( Ok( BinaryOpKind::Plus ) ),
                                    TokenKind::Minus => Some( Ok( BinaryOpKind::Minus ) ),
                                    TokenKind::Times => Some( Ok( BinaryOpKind::Times ) ),
                                    TokenKind::Divide => Some( Ok( BinaryOpKind::Divide ) ),
                                    TokenKind::Pow => Some( Ok( BinaryOpKind::Pow ) ),
                                    _ => Some( Err( Node::Unexpected{
                                        token: next,
                                        err_msg: "expected binary operator",
                                        help_msg: "needs to be either one of '+', '-', '*', '/' or '^'"
                                    } ) ),
                                },
                                None => None,
                            };

                            let rhs_token = tokens.next();
                            let rhs_node = match rhs_token {
                                Some( next ) => match &next.kind {
                                    TokenKind::Literal( literal ) => match literal {
                                        LiteralKind::Int{ base, value } => Some( Ok( Value::I64{ base: *base, value: *value } ) ),
                                    },
                                    _ => Some( Err( Node::Unexpected{
                                        token: next,
                                        err_msg: "expected number literal",
                                        help_msg: "not a number literal"
                                    } ) ),
                                },
                                None => None,
                            };

                            match op_node {
                                Some( op_result ) => match op_result {
                                    Ok( op ) => match rhs_node {
                                        Some( rhs_result ) => match rhs_result {
                                            Ok( rhs ) => Node::BinaryOp{ lhs, op, rhs },
                                            Err( rhs_err ) => rhs_err,
                                        },
                                        None => {
                                            Node::Unexpected{
                                                token: op_token.unwrap(),
                                                err_msg: "unexpected line end",
                                                help_msg: "expected number literal after the operator"
                                            }
                                        },
                                    },
                                    Err( op_err ) => op_err,
                                },
                                None => Node::Literal( lhs ),
                            }
                        }
                    },
                    TokenKind::Unexpected{ token_text: _, err_msg: _, help_msg: _ } => Node::Unexpected{
                        token,
                        err_msg: "unexpected token",
                        help_msg: "this may be a bug in the compiler"
                    },
                    _ => Node::Unexpected{
                        token,
                        err_msg: "unexpected token",
                        help_msg: ""
                    },
                };

                if let Node::Unexpected{ token: _, err_msg: _, help_msg: _ } = node {
                    found_errors = true;
                    errors.push( Statement{ line: &line, node } );
                }
                else {
                    statements.push( Statement{ line: &line, node } );
                }
            }
        }

        return if found_errors {
            Err( Self{ lexer, statements: errors } )
        }
        else {
            Ok( Self{ lexer, statements } )
        }
    }
}

impl<'program> Display for Parser<'program> {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        for statement in &self.statements {
            let line_text = format!( "{}", statement.line );

            match &statement.node {
                Node::Unexpected{ token, err_msg, help_msg } => {
                    let gutter_padding_amount = statement.line.number.ilog10() as usize + 1;
                    let gutter_padding = " ".repeat( gutter_padding_amount );
                    let pointers_padding = " ".repeat( token.col - 1 );
                    let pointers = "^".repeat( token.kind.to_string().len() );
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
                        "\x1b[91;1mError\x1b[0m: \x1b[1m{}\x1b[0m\n \
                            {} \x1b[91min\x1b[0m: {}:{}:{}\n{}\n",
                        err_msg,
                        gutter_padding, self.lexer.file_path, statement.line.number, token.col, error_visualization
                    )?;
                },
                Node::Literal( literal ) => writeln!( f, "literal: {:?}", literal )?,
                Node::BinaryOp{ lhs, op, rhs } => writeln!( f, "binary op: {:?} - {:?} - {:?}", lhs, op, rhs )?,
            }
        }

        return Ok( () );
    }
}

// IDEA add man page
fn print_usage() {
    let usage = format!( r"
Blitzlang compiler, version {}

Usage: blitz [Options] file.blz

Options:
    -h, --help Display this message
",
env!( "CARGO_PKG_VERSION" ) );

    println!( "{}", usage );
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
                _ => {
                    if let None = source_file_path {
                        source_file_path = Some( arg );
                    }
                    else {
                        eprintln!( "Error: too many source file paths provided!" );
                        return ExitCode::FAILURE;
                    }
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
        Err( lexer ) => {
            eprint!( "{}", lexer );
            return ExitCode::FAILURE;
        },
    };

    let parser = match Parser::parse( &lexer ) {
        Ok( parser ) => {
            eprint!( "{}", parser );
            parser
        },
        Err( parser ) => {
            eprint!( "{}", parser );
            return ExitCode::FAILURE;
        },
    };

    return ExitCode::SUCCESS;
}
