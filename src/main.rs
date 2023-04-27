// TODO create IDEAS.md
use std::{io::{BufReader, BufRead, ErrorKind}, fs::File, env, process::ExitCode, fmt::Display};


#[derive( Debug, PartialEq )]
enum LiteralKind {
    Int{ base: i8, value: String }
}

#[derive( Debug, PartialEq )]
enum TokenKind {
    Unexpected( String, &'static str ),

    // Brackets
    OpenRoundBracket,
    CloseRoundBracket,
    // OpenSquareBracket,
    // CloseSquareBracket,
    // OpenCurlyBracket,
    // CloseCurlyBracket,
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

    Literal{ kind: LiteralKind },
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

    // Special characters
    // NewLine,
    EOF,
}

// impl Display for TokenKind {
//     fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
//         match self {
//             Self::Unexpected( text, _ ) => write!( f, "{}", text ),
//             Self::OpenRoundBracket => write!( f, "(" ),
//             Self::CloseRoundBracket => write!( f, ")" ),
//             Self::Literal{ kind } => match kind {
//                 LiteralKind::Int{ base: _, value } => write!( f, "{}", value ),
//             },
//             Self::Comment( text ) => write!( f, "{}", text ),
//             Self::Plus => write!( f, "+" ),
//             Self::Minus => write!( f, "-" ),
//             Self::Times => write!( f, "*" ),
//             Self::Divide => write!( f, "/" ),
//             Self::Pow => write!( f, "^" ),
//             // Self::Equals => write!( f, "=" ),
//             Self::EOF => write!( f, "" ),
//         }
//     }
// }

#[derive( Debug )]
struct Token {
    col: usize,
    kind: TokenKind,
}

#[derive( Debug )]
struct Line {
    row: usize,
    tokens: Vec<Token>,
}

// impl Display for Line {
//     fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
//         if self.tokens.len() == 0 {
//             write!( f, "" )?;
//             return Ok( () );
//         }

//         // leading whitespaces
//         write!( f, "{}", " ".repeat( self.tokens[ 0 ].col - 1 ) )?;

//         let mut tokens = self.tokens.iter().peekable();
//         while let Some( token ) = tokens.next() {
//             let spaces = if let Some( next_token ) = tokens.peek() {
//                 ((*next_token).col - token.col) - token.kind.to_string().len()
//             }
//             else {
//                 0
//             };

//             write!( f, "{}{}", token.kind, " ".repeat( spaces ) )?;
//         }

//         return Ok( () );
//     }
// }

#[derive( Debug )]
struct Lexer {
    file_path: String,
    lines: Vec<Line>,
}

impl Lexer {
    fn parse( file_path: String, source_file: File ) -> Self {
        let mut lines: Vec<Line> = Vec::new();
        let mut token_text = String::new();
        let mut row: usize = 1;

        for source_line in BufReader::new( source_file ).lines() {
            if let Ok( current_line ) = source_line {
                let mut col: usize = 1;
                let mut line = Line{ row, tokens: Vec::new() };

                let mut src = current_line.chars().peekable();
                while let Some( ch ) = src.next() {
                    let token: Token = match ch {
                        // TODO handle other whitespace characters
                        // ignore whitespace
                        ' ' | '\t' => {
                            col += 1;
                            continue;
                        },
                        '(' => Token{ kind: TokenKind::OpenRoundBracket, col },
                        ')' => Token{ kind: TokenKind::CloseRoundBracket, col },
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
                            // token_text.clear();
                            // token_text.push( ch );

                            while let Some( _ ) = src.next() {
                                // FIXME find better way to drain the line iterators
                                // consume the rest of the tokens in the current line
                                // token_text.push( next );
                            }

                            continue;
                            // let token = Token{ kind: TokenKind::Comment( token_text.clone() ), col };
                            // col += token_text.len() - 1;
                            // token
                        }
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
                                TokenKind::Literal{ kind: LiteralKind::Int{ base: 10, value: token_text.clone() } }
                            }
                            else {
                                TokenKind::Unexpected( token_text.clone(), "invalid number literal" )
                            };

                            let token = Token{ kind, col };
                            col += token_text.len() - 1;
                            token
                        }
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
                                _ => TokenKind::Unexpected( token_text.clone(), "unrecognized token" )
                            };

                            let token = Token{ kind, col };
                            col += token_text.len() - 1;
                            token
                        },
                        _ => Token{ kind: TokenKind::Unexpected( ch.into(), "unrecognized token" ), col },
                    };

                    // TODO push to a struct containing all errors and print that instead
                    Self::print_error( &file_path, row, &current_line, &token );
                    line.tokens.push( token );
                    col += 1;
                }

                // skip empty lines
                if line.tokens.len() > 0 {
                    lines.push( line );
                }
                row += 1;
            }
        }

        lines.push( Line{ row, tokens: vec![ Token{ kind: TokenKind::EOF, col: 1 } ] } );

        return Self{ file_path, lines };
    }

    fn print_error( file_path: &str, row: usize, line: &str, token: &Token ) {
        if let TokenKind::Unexpected( token_text, err ) = &token.kind  {
            let padding_count = row.ilog10() as usize + 1;
            let padding = " ".repeat( padding_count );
            let bar = "\x1b[94m|\x1b[0m";
            let error_visualization = &format!(
                " {padding} {bar}\n \x1b[94m{: >padding_count$}\x1b[0m {bar} {}\n {padding} {bar} {}\x1b[91m{} <- {}\x1b[0m",
                row, line, " ".repeat( token.col - 1 ), "^".repeat( token_text.len() ), err
            );

            eprintln!(
                "\x1b[91mError\x1b[0m:\n {padding} \x1b[94m>\x1b[0m {}:{}:{}\n{}\n",
                file_path, row, token.col, error_visualization
            );
        }
    }
}

// #[derive( Debug )]
// enum Mutability {
//     Let,
//     Var,
//     Const,
// }

// #[derive( Debug )]
// enum Value {
//     Integer( String, i8 ),
// }


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

    let lexer = Lexer::parse( source_file_path, source_file );
    // let ast = AST::parse( &tokens );

    println!( "{:?}", lexer );
    // println!( "{:?}", ast );
    return ExitCode::SUCCESS;
}
