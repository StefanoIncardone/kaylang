use std::{io::{BufReader, BufRead, ErrorKind}, fs::File, env, process::ExitCode};


#[derive( Debug, PartialEq )]
pub enum TokenKind {
    Unexpected( String ),

    // Brackets
    OpenRoundBracket,
    CloseRoundBracket,
    OpenSquareBracket,
    CloseSquareBracket,
    OpenCurlyBracket,
    CloseCurlyBracket,

    // TODO handle different bit sizes and bases
    /*
        IDEA handle arbitrary bases: has to be bigger than 1 and smaller than 36 => {
            ...b1 = error,
            ...b2 = base 2,
            ...b4 = base 4,
            ...b35 = base 35,
            ...b37 = error,
        }

        IDEA let the user define new bases with custom symbols => { TODO }
    */
    // Identifiers
    Digit( String, i8 ),
    Identifier( String ),
    Comment( String ),

    // Keywords
    Entry,
    Fn,
    Let,
    Const,
    Var,
    Return,

    // Symbols and operators
    Plus,
    Minus,
    Times,
    Divide,
    Pow,
    Equals,
    Colon,
    SemiColon,

    // Special characters
    NewLine,
    EOF,
}

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

#[derive( Debug )]
struct Lexer {
    lines: Vec<Line>,
}

impl Lexer {
    fn parse( source_file: File ) -> Self {
        let mut lines: Vec<Line> = Vec::new();
        let mut current_token_text = String::new();
        let mut row: usize = 1;

        for source_line in BufReader::new( source_file ).lines() {
            if let Ok( l ) = source_line {
                let mut col: usize = 1;
                let mut line = Line{ row, tokens: Vec::new() };

                let mut src = l.chars().peekable();
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
                        '[' => Token{ kind: TokenKind::OpenSquareBracket, col },
                        ']' => Token{ kind: TokenKind::CloseSquareBracket, col },
                        '{' => Token{ kind: TokenKind::OpenCurlyBracket, col },
                        '}' => Token{ kind: TokenKind::CloseCurlyBracket, col },
                        '+' => Token{ kind: TokenKind::Plus, col },
                        '-' => Token{ kind: TokenKind::Minus, col },
                        '*' => Token{ kind: TokenKind::Times, col },
                        '/' => Token{ kind: TokenKind::Divide, col },
                        '^' => Token{ kind: TokenKind::Pow, col },
                        '=' => Token{ kind: TokenKind::Equals, col },
                        ':' => Token{ kind: TokenKind::Colon, col },
                        ';' => Token{ kind: TokenKind::SemiColon, col },
                        '#' => {
                            current_token_text.clear();
                            current_token_text.push( ch );

                            while let Some( next ) = src.next() {
                                current_token_text.push( next );
                            }

                            let token = Token{ kind: TokenKind::Comment( current_token_text.clone() ), col };
                            col += current_token_text.len() - 1;
                            token
                        }
                        '0'..='9' => {
                            current_token_text.clear();
                            current_token_text.push( ch );

                            let mut is_digit = true;
                            while let Some( next ) = src.next_if( |c| matches!( c, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' ) ) {
                                if !next.is_ascii_digit() {
                                    is_digit = false;
                                }

                                current_token_text.push( next );
                            }

                            let kind = if is_digit {
                                TokenKind::Digit( current_token_text.clone(), 10 )
                            }
                            else {
                                TokenKind::Unexpected( current_token_text.clone() )
                            };

                            let token = Token{ kind, col };
                            col += current_token_text.len() - 1;
                            token
                        }
                        'a'..='z' | 'A'..='Z' | '_' => {
                            current_token_text.clear();
                            current_token_text.push( ch );

                            while let Some( next ) = src.next_if( |c| matches!( c, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' ) ) {
                                current_token_text.push( next );
                            }

                            let kind = match current_token_text.as_str() {
                                "entry" => TokenKind::Entry,
                                "fn" => TokenKind::Fn,
                                "const" => TokenKind::Const,
                                "let" => TokenKind::Let,
                                "var" => TokenKind::Var,
                                "return" => TokenKind::Return,
                                _ => TokenKind::Identifier( current_token_text.clone() )
                            };

                            let token = Token{ kind, col };
                            col += current_token_text.len() - 1;
                            token
                        },
                        _ => Token{ kind: TokenKind::Unexpected( ch.into() ), col },
                    };

                    line.tokens.push( token );
                    col += 1;
                }

                lines.push( line );
                row += 1;
            }
        }

        lines.push( Line{ row, tokens: vec![ Token{ kind: TokenKind::EOF, col: 1 } ] } );

        return Self{ lines };
    }
}

// TODO add man page
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
    args.push( "./examples/main.blz".to_string() );
    // args.push( "-h".to_string() );
    // args.push( "-v".to_string() );
    // args.push( "./examples/main.blz".to_string() );
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

    let tokens = Lexer::parse( source_file );

    println!( "{:?}", tokens );
    return ExitCode::SUCCESS;
}
