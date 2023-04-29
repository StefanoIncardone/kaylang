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
    Int{ base: i8, value: String }
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
                                TokenKind::Literal( LiteralKind::Int{ base: 10, value: token_text.clone() } )
                            }
                            else {
                                found_errors = true;
                                TokenKind::Unexpected{ token_text: token_text.clone(), err_msg: "unexpected token", help_msg: "only numbers are allowed for now" }
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
                                _ => {
                                    found_errors = true;
                                    TokenKind::Unexpected{ token_text: token_text.clone(), err_msg: "unexpected token", help_msg: "only numbers are allowed for now" }
                                },
                            };

                            let token = Token{ kind, col };
                            col += token_text.len() - 1;
                            token
                        },
                        _ => {
                            found_errors = true;
                            Token{ col, kind: TokenKind::Unexpected{ token_text: token_text.clone(), err_msg: "unexpected token", help_msg: "only numbers are allowed for now" } }
                        },
                    };

                    line.tokens.push( token );
                    col += 1;
                }

                // skip empty lines
                if line.tokens.len() > 0 {
                    if found_errors {
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

    fn print_errors( &self ) {
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
    
                    eprintln!(
                        "\x1b[91;1mError\x1b[0m: \x1b[1m{}\x1b[0m\n \
                         {} \x1b[91min\x1b[0m: {}:{}:{}\n{}\n",
                        err_msg,
                        gutter_padding, self.file_path, line.number, token.col, error_visualization
                    );
                }
            }
        }
    }
}

// #[derive( Debug )]
// struct Variable {
//     kind: VariableDefinitionKind,
//     name: String,
//     value: VariableValue
// }

// #[derive( Debug )]
// enum Node {
//     Unrecognized,

//     Literal( Value ),
//     Node( Box<Node> )

    // Variable{
    //     mutability: Mutability,
    //     name: Option<String>,
    //     typ: String,
    //     value: Option<Value>,
    // },
// }

// #[derive( Debug )]
// struct AST {
//     statements: Vec<Node>,
// }

// impl AST {
//     fn parse( lexer: &Lexer ) -> Self {
//         let mut statements: Vec<Node> = Vec::new();
        // for line in &lexer.lines {
        //     let mut tokens = line.tokens.iter().peekable();
        //     while let Some( current_token ) = tokens.next() {
        //         match current_token.kind {
        //             TokenKind::Comment( _ ) /* | TokenKind::NewLine */ | TokenKind::EOF | TokenKind::SemiColon => continue,
        //             TokenKind::Let | TokenKind::Var | TokenKind::Const => {
        //                 let mutability = match current_token.kind {
        //                     TokenKind::Let => Mutability::Let,
        //                     TokenKind::Var => Mutability::Var,
        //                     TokenKind::Const => Mutability::Const,
        //                     _ => unreachable!(),
        //                 };

        //                 let name = match tokens.next() {
        //                     Some( token ) => match &token.kind {
        //                         TokenKind::Identifier( var_name ) => Some( var_name.clone() ),
        //                         _ => {
        //                             eprintln!( "Error: expected identifier in variable definition!" );
        //                             None
        //                         }
        //                     },
        //                     None => {
        //                         eprintln!( "Error: Run out of tokens!" );
        //                         None
        //                     },
        //                 };

        //                 let mut typ = match tokens.next() {
        //                     Some( token ) => match &token.kind {
        //                         TokenKind::Colon => match tokens.next() {
        //                             Some( var_type ) => match &var_type.kind {
        //                                 TokenKind::Identifier( type_name ) => type_name.clone(),
        //                                 _ => {
        //                                     eprintln!( "Error: expected type identifier!" );
        //                                     String::new()
        //                                 }
        //                             }
        //                             None => {
        //                                 eprintln!( "Error: run out of tokens!" );
        //                                 String::new()
        //                             }
        //                         }
        //                         _ => {
        //                             eprintln!( "Error: expected type qualification!" );
        //                             String::new()
        //                         }
        //                     },
        //                     None => {
        //                         eprintln!( "Error: run out of tokens!" );
        //                         String::new()
        //                     }
        //                 };

        //                 let value = match tokens.next() {
        //                     Some( token ) => match &token.kind {
        //                         TokenKind::Equals => match tokens.next() {
        //                             Some( initializer ) => match &initializer.kind {
        //                                 TokenKind::Literal{ kind } => match kind {
        //                                     LiteralKind::Int{ base, value } => {
        //                                         typ = "int".to_string();
        //                                         Some( Value::Integer( value.clone(), base.clone() ) )
        //                                     }
        //                                 },
        //                                 _ => {
        //                                     eprintln!( "Error: expected literal or expression!" );
        //                                     None
        //                                 }
        //                             }
        //                             None => {
        //                                 eprintln!( "Error: run out of tokens!" );
        //                                 None
        //                             },
        //                         },
        //                         _ => {
        //                             eprintln!( "Error: expected equals" );
        //                             None
        //                         },
        //                     },
        //                     None => {
        //                         eprintln!( "Error: run out of tokens!" );
        //                         None
        //                     },
        //                 };

        //                 statements.push( Node::Variable{ mutability, typ, name, value } );
        //             }
        //             _ => unreachable!(),
        //         }
        //     }
        // }

//         return Self{ statements };
//     }
// }

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
        Err( lexer ) => {
            lexer.print_errors();
            return ExitCode::FAILURE;
        }
        Ok( lexer ) => {
            println!( "{:?}\n", lexer );
            lexer
        },
    };

    // let ast = AST::parse( &lexer );

    println!( "{:?}", lexer );
    // println!( "{:?}", ast );
    return ExitCode::SUCCESS;
}
