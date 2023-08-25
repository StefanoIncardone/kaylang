// TODO process entire statement for syntactical correctness and then report all the errors
    // IDEA read the src file line when printing errors instead of composing it from the tokens
        // IDEA have Token contain the absolute column in the source file, and when encountering errors read the corresponding line
// TODO fix division by zero, rasing to a negative power
    // IDEA print crash error message
        // TODO implement a way to print file, line and column information in source code
// TODO implement boolean operators for strings
// IDEA optimize expression building by implementing associativity rules (ie. remove parenthesis around additions and subtractions)
// TODO shortcircuit boolean operators
// IDEA reintroduce "semicolon" function, taking out the semicolon checking out of the expression parsing
// IDEA create Parser class that builds the AST, and then validate the AST afterwards
use std::{io::{BufReader, BufRead, ErrorKind, BufWriter, Write}, fs::File, env, process::{ExitCode, Command}, fmt::Display, path::{Path, PathBuf}, iter::Peekable, str::Chars, borrow::Cow, cmp::Ordering};

mod color;
use crate::color::*;


trait Len {
    fn len( &self ) -> usize;
}

trait TypeOf {
    fn typ( &self ) -> Type;
}

trait Precedence {
    fn precedence( &self ) -> usize;
    fn precedence_over( &self, other: &Self ) -> Ordering;
}


#[derive( Debug, Clone )]
struct Str {
    text: Vec<u8>,
}


#[derive( Debug, Clone )]
enum Literal {
    // TODO have different size integers and default to 32 bits for literals
    Int { value: isize },
    Char { value: u8 }, // only supporting ASCII characters for now
    Bool { value: bool },
    Str( Str ),
}

impl Display for Literal {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Int { value } => write!( f, "{}", value ),
            Self::Char { value } => write!( f, "'{}'", value.escape_ascii() ), // TODO create own escaping function
            Self::Bool { value } => write!( f, "{}", value ),
            Self::Str( string ) => {
                write!( f, "\"" )?;
                for character in &string.text {
                    write!( f, "{}", character.escape_ascii() )?;
                }
                write!( f, "\"" )
            },
        }
    }
}

impl Into<isize> for Literal {
    fn into( self ) -> isize {
        return match self {
            Self::Int { value } => value.into(),
            Self::Char { value } => value.into(),
            Self::Bool { value } => value.into(),
            Self::Str( string ) => string.text.len() as isize,
        }
    }
}

impl Len for Literal {
    fn len( &self ) -> usize {
        return match self {
            Self::Int { value } => value.to_string().len(),
            Self::Char { .. } => 1 as usize,
            Self::Bool { value } => value.to_string().len(),
            Self::Str( string ) => string.text.len() + 2,
        }
    }
}

impl TypeOf for Literal {
    fn typ( &self ) -> Type {
        return match self {
            Self::Int { .. } => Type::Int,
            Self::Char { .. } => Type::Char,
            Self::Bool { .. } => Type::Bool,
            Self::Str { .. } => Type::Str,
        }
    }
}

impl Literal {
    fn display( &self ) {
        match self {
            Self::Int { value } => print!( "{}", value ),
            Self::Char { value } => print!( "{}", *value as char ),
            Self::Bool { value } => print!( "{}", value ),
            Self::Str( string ) =>
                for character in &string.text {
                    print!( "{}", *character as char );
                },
        }
    }
}


#[derive( Debug, PartialEq, Clone, Copy)]
enum Type {
    Int,
    Char,
    Bool,
    Str,
}

impl Display for Type {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Type::Int => write!( f, "int" ),
            Type::Char => write!( f, "char" ),
            Type::Bool => write!( f, "bool" ),
            Type::Str => write!( f, "str" ),
        }
    }
}

impl Len for Type {
    fn len( &self ) -> usize {
        match self {
            Self::Int => core::mem::size_of::<usize>(),
            Self::Char => core::mem::size_of::<u8>(),
            Self::Bool => core::mem::size_of::<bool>(),
            Self::Str => core::mem::size_of::<*const u8>() + core::mem::size_of::<usize>(),
        }
    }
}

// impl Type {
//     const fn align( &self ) -> usize {
//         return match self {
//             Self::Int => core::mem::size_of::<usize>(),
//             Self::Char => core::mem::size_of::<u8>(),
//             Self::Bool => core::mem::size_of::<bool>(),
//             Self::Str => core::mem::size_of::<usize>(),
//         }
//     }
// }


#[derive( Debug, Clone, Copy, PartialEq )]
enum Operator {
    Pow,
    PowEquals,

    Times,
    TimesEquals,
    Divide,
    DivideEquals,
    Remainder,
    RemainderEquals,

    Plus,
    PlusEquals,
    Minus,
    MinusEquals,

    EqualsEquals,
    NotEquals,
    Greater,
    GreaterOrEquals,
    Less,
    LessOrEquals,
    Compare,

    And,
    Or,
    Xor
}

impl Display for Operator {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Pow => write!( f, "^" ),
            Self::PowEquals => write!( f, "^=" ),
            Self::Times => write!( f, "*" ),
            Self::TimesEquals => write!( f, "*=" ),
            Self::Divide => write!( f, "/" ),
            Self::DivideEquals => write!( f, "/=" ),
            Self::Remainder => write!( f, "%" ),
            Self::RemainderEquals => write!( f, "%=" ),

            Self::Plus => write!( f, "+" ),
            Self::PlusEquals => write!( f, "+=" ),
            Self::Minus => write!( f, "-" ),
            Self::MinusEquals => write!( f, "-=" ),

            Self::EqualsEquals => write!( f, "==" ),
            Self::NotEquals => write!( f, "!=" ),
            Self::Greater => write!( f, ">" ),
            Self::GreaterOrEquals => write!( f, ">=" ),
            Self::Less => write!( f, "<" ),
            Self::LessOrEquals => write!( f, "<=" ),
            Self::Compare => write!( f, "<=>" ),

            Self::And => write!( f, "and" ),
            Self::Or => write!( f, "or" ),
            Self::Xor => write!( f, "xor" ),
        }
    }
}

impl Len for Operator {
    fn len( &self ) -> usize {
        return match self {
            Self::Pow
            | Self::Times
            | Self::Divide
            | Self::Remainder
            | Self::Plus
            | Self::Minus => 1,

            Self::PowEquals
            | Self::TimesEquals
            | Self::DivideEquals
            | Self::RemainderEquals
            | Self::PlusEquals
            | Self::MinusEquals => 2,

            Self::EqualsEquals => 2,
            Self::NotEquals => 2,
            Self::Greater => 1,
            Self::GreaterOrEquals => 2,
            Self::Less => 1,
            Self::LessOrEquals => 2,

            Self::Compare => 3,

            Self::And => 3,
            Self::Or => 2,
            Self::Xor => 3,
        }
    }
}

impl TypeOf for Operator {
    fn typ( &self ) -> Type {
        return match self {
            Self::Pow | Self::PowEquals
            | Self::Times | Self::TimesEquals
            | Self::Divide | Self::DivideEquals
            | Self::Remainder | Self::RemainderEquals
            | Self::Plus | Self::PlusEquals
            | Self::Minus | Self::MinusEquals
            | Self::Compare => Type::Int,

            Self::EqualsEquals | Self::NotEquals
            | Self::Greater | Self::GreaterOrEquals
            | Self::Less | Self::LessOrEquals
            | Self::And | Self::Or | Self::Xor => Type::Bool,
        }
    }
}

impl Precedence for Operator {
    fn precedence( &self ) -> usize {
        return match self {
            Self::And | Self::Or | Self::Xor => 0,

            Self::EqualsEquals | Self::NotEquals
            | Self::Greater | Self::GreaterOrEquals
            | Self::Less | Self::LessOrEquals
            | Self::Compare => 1,

            Self::Plus | Self::PlusEquals
            | Self::Minus | Self::MinusEquals => 2,

            Self::Times | Self::TimesEquals
            | Self::Divide | Self::DivideEquals
            | Self::Remainder | Self::RemainderEquals => 3,

            Self::Pow | Self::PowEquals => 4,
        }
    }

    fn precedence_over( &self, other: &Self ) -> Ordering {
        return self.precedence().cmp( &other.precedence() );
    }
}


#[derive( Debug, Clone, Copy )]
enum Mutability {
    Let,
    Var,
}

impl Display for Mutability {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Let => write!( f, "let" ),
            Self::Var => write!( f, "var" ),
        }
    }
}

impl Len for Mutability {
    fn len( &self ) -> usize {
        return match self {
            Self::Let => 3,
            Self::Var => 3,
        }
    }
}


#[derive( Debug, Clone, Copy )]
enum BracketKind {
    OpenRound,
    CloseRound,
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

impl Len for BracketKind {
    fn len( &self ) -> usize {
        return match self {
            Self::OpenRound => 1,
            Self::CloseRound => 1,
            Self::OpenCurly => 1,
            Self::CloseCurly => 1,
        }
    }
}


#[derive( Debug, Clone )]
enum TokenKind {
    Comment( String ),
    Unexpected( String ),

    Bracket( BracketKind ),
    SemiColon,
    Equals,
    Op( Operator ),

    Literal( Literal ),
    Identifier( String ),
    Definition( Mutability ),

    // Keywords
    Print, // temporary way of printing values
    PrintLn, // temporary way of printing values followed by a newline
    True,
    False,
    If,
    Else,
    For,
    Break,
    Continue,

    // Special
    Empty,
    SOF, // start of file
    EOF, // end of file
}

impl Display for TokenKind {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Comment( text ) => write!( f, "{}", text ),
            Self::Unexpected( text ) => write!( f, "{}", text ),

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
            Self::If => write!( f, "if" ),
            Self::Else => write!( f, "else" ),
            Self::For => write!( f, "for" ),
            Self::Break => write!( f, "break" ),
            Self::Continue => write!( f, "continue" ),

            Self::Empty | Self::SOF | Self::EOF => write!( f, "" ),
        }
    }
}

impl Len for TokenKind {
    fn len( &self ) -> usize {
        return match self {
            Self::Comment( text ) => text.len(),
            Self::Unexpected( text ) => text.len(),

            Self::Bracket( bracket ) => bracket.len(),
            Self::Equals => 1,
            Self::SemiColon => 1,

            Self::Literal( typ ) => typ.len(),
            Self::Identifier( name ) => name.len(),
            Self::Definition( kind ) => kind.len(),

            Self::Op( op ) => op.len(),

            Self::Print => 5,
            Self::PrintLn => 7,
            Self::True => 4,
            Self::False => 5,
            Self::If => 2,
            Self::Else => 4,
            Self::For => 3,
            Self::Break => 5,
            Self::Continue => 8,

            Self::Empty => 1,
            Self::SOF => 1,
            Self::EOF => 1,
        }
    }
}


#[derive( Debug, Clone )]
struct Token {
    col: usize,
    kind: TokenKind,
}

#[derive( Debug, Clone )]
struct Line {
    number: usize,
    tokens: Vec<Token>,
}

impl Display for Line {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        if self.tokens.is_empty() {
            return writeln!( f );
        }

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
                Some( next_token ) => next_token.col - (token.col + token.kind.len()),
                None => 0,
            };

            write!( f, "{}{:spaces_before_next_token$}", token.kind, "" )?;
        }

        return Ok( () );
    }
}


// TODO implement NOTE, HINT, HELP in error messages
#[derive( Debug )]
struct SyntaxError {
    line_idx: usize,
    col: usize,
    len: usize,
    msg: Cow<'static, str>,
    help_msg: Cow<'static, str>,
}

impl SyntaxError {
    const ERROR: &'static str = colored!{
        text: "Error",
        foreground: Foreground::LightRed,
        bold: true,
    };
}

type LexerError = (String, SyntaxError);
type ASTError<'lexer> = (&'lexer Line, SyntaxError);

#[derive( Debug )]
struct SyntaxErrors<'this> {
    file_path: String,
    lines: Vec<Cow<'this, Line>>,
    errors: Vec<SyntaxError>,
}

impl Display for SyntaxErrors<'_> {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        let mut line_number = self.errors[ 0 ].line_idx;
        let mut line = &self.lines[ line_number ];
        let mut line_text = line.to_string();

        for error in &self.errors {
            if error.line_idx != line_number {
                line_number = error.line_idx;
                line = &self.lines[ line_number ];
                line_text = line.to_string();
            }

            let error_msg = Colored {
                text: error.msg.to_string(),
                foreground: Foreground::White,
                bold: true,
                ..Default::default()
            };

            let mut line_number_and_bar = Colored {
                text: format!( "{} |", line.number ),
                foreground: Foreground::LightBlue,
                ..Default::default()
            };

            let visualization_padding = line_number_and_bar.text.len();
            let location_padding = visualization_padding - 1;

            line_number_and_bar.text = format!( "{:>visualization_padding$}", line_number_and_bar );
            line_number_and_bar.foreground = Foreground::LightBlue;

            let in_ = Colored {
                text: format!( "{:>location_padding$}", "in" ),
                foreground: Foreground::LightRed,
                bold: true,
                ..Default::default()
            };

            let bar = Colored {
                text: format!( "{:>visualization_padding$}", "|" ),
                foreground: Foreground::LightBlue,
                ..Default::default()
            };

            let pointers_col = error.col - 1;
            let pointers_len = error.len;

            let pointers_and_help_msg = Colored {
                text: format!( "{:^>pointers_len$} {}", "", error.help_msg ),
                foreground: Foreground::LightRed,
                ..Default::default()
            };

            writeln!( f,
                "{}: {}\
                \n{}: {}:{}:{}\
                \n{}\
                \n{} {}\
                \n{} {:pointers_col$}{}\n",
                SyntaxError::ERROR, error_msg,
                in_, self.file_path, line.number, error.col,
                bar,
                line_number_and_bar, line_text,
                bar, "", pointers_and_help_msg
            )?;
        }

        return Ok( () );
    }
}


type LineIdx = usize;
type TokenIdx = usize;

#[derive( Debug )]
struct Bracket {
    line_idx: LineIdx,
    token_idx: TokenIdx,
    kind: BracketKind,
}

#[derive( Debug )]
struct Lexer {
    file_path: String,
    lines: Vec<Line>,
}

impl<'program> TryFrom<(&'program str, File)> for Lexer {
    type Error = SyntaxErrors<'program>;


    // IDEA make the input character stream generic, eg: to be able to compile from strings instead of just files
    // TODO make an character iterator similar to LexerIter
    fn try_from( src: (&'program str, File) ) -> Result<Self, Self::Error> {
        let (file_path, src_file) = (src.0, src.1);

        let mut lines: Vec<Line> = Vec::new();
        let mut tokens: Vec<Token> = vec![ Token { col: 1, kind: TokenKind::SOF } ];
        let mut brackets: Vec<Bracket> = Vec::new();

        let mut err_lines: Vec<Cow<Line>> = Vec::new();
        let mut errors: Vec<SyntaxError> = Vec::new();

        let mut src_lines = BufReader::new( src_file );
        let mut src_line_text = String::new();
        let mut token_text = String::new();

        while let Ok( chars_read ) = src_lines.read_line( &mut src_line_text ) {
            src_line_text = src_line_text.trim_end().to_string();
            let trimmed_line_len = src_line_text.len();

            let mut line_contains_errors = false;
            let mut col = 1;

            let mut src = src_line_text.chars().peekable();
            loop {
                let token_info = match Self::next( &mut src ) {
                    Ok( None ) => break,
                    Err( err ) => Err( err ),
                    Ok( Some( ch ) ) => match ch {
                        // ignore whitespace
                        _ if ch.is_ascii_whitespace() => {
                            col += 1;
                            continue;
                        },
                        '#' => {
                            token_text.push( ch );

                            // consume the rest of the tokens in the current line
                            while let Some( next ) = src.next_if( |c| *c != '\n' ) {
                                token_text.push( next );
                            }

                            Ok( TokenKind::Comment( token_text.clone() ) )
                        },
                        '(' => {
                            let kind = BracketKind::OpenRound;
                            brackets.push( Bracket { line_idx: lines.len(), token_idx: tokens.len(), kind } );
                            Ok( TokenKind::Bracket( kind ) )
                        },
                        ')' => match brackets.pop() {
                            Some( bracket ) => match bracket.kind {
                                BracketKind::OpenRound => Ok( TokenKind::Bracket( BracketKind::CloseRound ) ),
                                BracketKind::CloseCurly | BracketKind::CloseRound | BracketKind::OpenCurly =>
                                    Err( (ch.to_string(), SyntaxError {
                                        line_idx: lines.len(),
                                        col,
                                        len: ch.len_utf8(),
                                        msg: Cow::Borrowed( "mismatched bracket" ),
                                        help_msg: Cow::Borrowed( "closes the wrong bracket" )
                                    }) )
                            },
                            None => Err( (ch.to_string(), SyntaxError {
                                line_idx: lines.len(),
                                col,
                                len: ch.len_utf8(),
                                msg: Cow::Borrowed( "stray bracket" ),
                                help_msg: Cow::Borrowed( "was not opened before" )
                            }) )
                        },
                        '{' => {
                            let kind = BracketKind::OpenCurly;
                            brackets.push( Bracket { line_idx: lines.len(), token_idx: tokens.len(), kind } );
                            Ok( TokenKind::Bracket( kind ) )
                        },
                        '}' => match brackets.pop() {
                            Some( bracket ) => match bracket.kind {
                                BracketKind::OpenCurly => Ok( TokenKind::Bracket( BracketKind::CloseCurly ) ),
                                BracketKind::CloseCurly | BracketKind::CloseRound | BracketKind::OpenRound =>
                                    Err( (ch.to_string(), SyntaxError {
                                        line_idx: 0,
                                        col,
                                        len: ch.len_utf8(),
                                        msg: Cow::Borrowed( "mismatched bracket" ),
                                        help_msg: Cow::Borrowed( "closes the wrong bracket" )
                                    }) )
                            },
                            None => Err( (ch.to_string(), SyntaxError {
                                line_idx: 0,
                                col,
                                len: ch.len_utf8(),
                                msg: Cow::Borrowed( "stray bracket" ),
                                help_msg: Cow::Borrowed( "was not opened before" )
                            }) )
                        },
                        ';' => Ok( TokenKind::SemiColon ),
                        '^' => match Self::peek_next( &mut src ) {
                            Ok( Some( '=' ) ) => {
                                let _ = Self::next( &mut src );
                                Ok( TokenKind::Op( Operator::PowEquals ) )
                            },
                            _ => Ok( TokenKind::Op( Operator::Pow ) ),
                        },
                        '*' => match Self::peek_next( &mut src ) {
                            Ok( Some( '=' ) ) => {
                                let _ = Self::next( &mut src );
                                Ok( TokenKind::Op( Operator::TimesEquals ) )
                            },
                            _ => Ok( TokenKind::Op( Operator::Times ) ),
                        },
                        '/' => match Self::peek_next( &mut src ) {
                            Ok( Some( '=' ) ) => {
                                let _ = Self::next( &mut src );
                                Ok( TokenKind::Op( Operator::DivideEquals ) )
                            },
                            _ => Ok( TokenKind::Op( Operator::Divide ) ),
                        },
                        '%' => match Self::peek_next( &mut src ) {
                            Ok( Some( '=' ) ) => {
                                let _ = Self::next( &mut src );
                                Ok( TokenKind::Op( Operator::RemainderEquals ) )
                            },
                            _ => Ok( TokenKind::Op( Operator::Remainder ) ),
                        },
                        '+' => match Self::peek_next( &mut src ) {
                            Ok( Some( '=' ) ) => {
                                let _ = Self::next( &mut src );
                                Ok( TokenKind::Op( Operator::PlusEquals ) )
                            },
                            _ => Ok( TokenKind::Op( Operator::Plus ) ),
                        },
                        '-' => match Self::peek_next( &mut src ) {
                            Ok( Some( '=' ) ) => {
                                let _ = Self::next( &mut src );
                                Ok( TokenKind::Op( Operator::MinusEquals ) )
                            },
                            _ => Ok( TokenKind::Op( Operator::Minus ) ),
                        },
                        '=' => match Self::peek_next( &mut src ) {
                            Ok( Some( '=' ) ) => {
                                let _ = Self::next( &mut src );
                                Ok( TokenKind::Op( Operator::EqualsEquals ) )
                            },
                            _ => Ok( TokenKind::Equals ),
                        },
                        '!' => match Self::peek_next( &mut src ) {
                            Ok( Some( '=' ) ) => {
                                let _ = Self::next( &mut src );
                                Ok( TokenKind::Op( Operator::NotEquals ) )
                            },
                            _ => Err( (ch.to_string(), SyntaxError {
                                line_idx: 0,
                                col,
                                len: ch.len_utf8(),
                                msg: Cow::Borrowed( "unexpected character" ),
                                help_msg: Cow::Borrowed( "unrecognized" )
                            }) ),
                        },
                        '>' => match Self::peek_next( &mut src ) {
                            Ok( Some( '=' ) ) => {
                                let _ = Self::next( &mut src );
                                Ok( TokenKind::Op( Operator::GreaterOrEquals ) )
                            },
                            _ => Ok( TokenKind::Op( Operator::Greater ) ),
                        },
                        '<' => match Self::peek_next( &mut src ) {
                            Ok( Some( '=' ) ) => {
                                let _ = Self::next( &mut src );

                                match Self::peek_next( &mut src ) {
                                    Ok( Some( '>' ) ) => {
                                        let _ = Self::next( &mut src );
                                        Ok( TokenKind::Op( Operator::Compare ) )
                                    },
                                    _ => {
                                        Ok( TokenKind::Op( Operator::LessOrEquals ) )
                                    },
                                }
                            },
                            _ => Ok( TokenKind::Op( Operator::Less ) ),
                        },
                        '\'' => {
                            token_text.push( ch );

                            match Self::parse_char( &mut src, &mut token_text ) {
                                Ok( b'\'' ) if token_text.len() == 2 => Err( (token_text.clone(), SyntaxError {
                                    line_idx: 0,
                                    col,
                                    len: token_text.len(),
                                    msg: Cow::Borrowed( "empty character literal" ),
                                    help_msg: Cow::Borrowed( "must not be empty" )
                                }) ),
                                Ok( value ) => match Self::next_char( &mut src, &mut token_text ) {
                                    Ok( next ) => match next {
                                        b'\'' => Ok( TokenKind::Literal( Literal::Char { value } ) ),
                                        _ => Err( (token_text.clone(), SyntaxError {
                                            line_idx: 0,
                                            col,
                                            len: token_text.len(),
                                            msg: Cow::Borrowed( "unclosed character literal" ),
                                            help_msg: Cow::Borrowed( "missing closing single quote" )
                                        }) ),
                                    },
                                    Err( err ) => Err( err ),
                                },
                                Err( err ) => Err( err ),
                            }
                        },
                        '"' => match Self::parse_str( &mut src ) {
                            Ok( text ) => Ok( TokenKind::Literal( Literal::Str( Str { text } ) ) ),
                            Err( err ) => Err( err ),
                        },
                        '0'..='9' => {
                            token_text.push( ch );

                            let mut is_digit = true;
                            while let Some( next ) = src.next_if( |c| matches!( c, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' ) ) {
                                if !next.is_ascii_digit() {
                                    is_digit = false;
                                }

                                token_text.push( next );
                            }

                            match is_digit {
                                true => match token_text.parse() { // TODO create own number parsing function
                                    Ok( value ) => Ok( TokenKind::Literal( Literal::Int { value } ) ),
                                    Err( _ ) => Err( (token_text.clone(), SyntaxError {
                                        line_idx: 0,
                                        col,
                                        len: token_text.len(),
                                        msg: Cow::Borrowed( "expected number literal" ),
                                        help_msg: Cow::Owned( format!( "overflows a 64 bit integer [{}, {}]", usize::MIN, usize::MAX ) )
                                    }) ),
                                },
                                false => Err( (token_text.clone(), SyntaxError {
                                    line_idx: 0,
                                    col,
                                    len: token_text.len(),
                                    msg: Cow::Borrowed( "expected number literal" ),
                                    help_msg: Cow::Borrowed( "not a number literal" )
                                }) ),
                            }
                        },
                        'a'..='z' | 'A'..='Z' | '_'  => {
                            token_text.push( ch );

                            while let Some( next ) = src.next_if( |c| matches!( c, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' ) ) {
                                token_text.push( next );
                            }

                            let kind = match token_text.as_str() {
                                "let" => TokenKind::Definition( Mutability::Let ),
                                "var" => TokenKind::Definition( Mutability::Var ),
                                "print" => TokenKind::Print,
                                "println" => TokenKind::PrintLn,
                                "true" => TokenKind::True,
                                "false" => TokenKind::False,
                                "and" => TokenKind::Op( Operator::And ),
                                "or" => TokenKind::Op( Operator::Or ),
                                "xor" => TokenKind::Op( Operator::Xor ),
                                "if" => TokenKind::If,
                                "else" => TokenKind::Else,
                                "for" => TokenKind::For,
                                "break" => TokenKind::Break,
                                "continue" => TokenKind::Continue,
                                _ => TokenKind::Identifier( token_text.clone() ),
                            };

                            Ok( kind )
                        },
                        _ => Err( (ch.to_string(), SyntaxError {
                            line_idx: 0,
                            col,
                            len: ch.len_utf8(),
                            msg: Cow::Borrowed( "unexpected character" ),
                            help_msg: Cow::Borrowed( "unrecognized" )
                        }) ),
                    },
                };

                let len = match token_info {
                    Ok( kind ) => {
                        let len = kind.len();
                        tokens.push( Token { col, kind } );
                        len
                    },
                    Err( (err_text, mut err) ) => {
                        line_contains_errors = true;

                        err.line_idx = err_lines.len();
                        err.col = col;

                        let len = err.len;
                        tokens.push( Token { col, kind: TokenKind::Unexpected( err_text ) } );
                        errors.push( err );

                        len
                    },
                };

                token_text.clear();
                col += len;
            }

            if trimmed_line_len == 0 {
                tokens.push( Token { col, kind: TokenKind::Empty } );
            }

            let reached_eof = trimmed_line_len == chars_read;
            if reached_eof {
                tokens.push( Token { col, kind: TokenKind::EOF } );
            }

            let line = Line { number: lines.len() + 1, tokens };
            if line_contains_errors {
                err_lines.push( Cow::Owned( line ) );
            }
            else {
                lines.push( line );
            }

            if reached_eof {
                break;
            }

            src_line_text.clear();
            tokens = Vec::new();
        }

        if !brackets.is_empty() {
            for bracket in brackets {
                let line = &lines[ bracket.line_idx ];

                let mut found = false;
                for err_line in &err_lines {
                    if line.number == err_line.number {
                        found = true;
                        break;
                    }
                }

                if !found {
                    err_lines.push( Cow::Owned( line.clone() ) );
                }

                // there can only be open brackets at this point
                errors.push( SyntaxError {
                    line_idx: err_lines.len() - 1,
                    col: line.tokens[ bracket.token_idx ].col,
                    len: bracket.kind.len(),
                    msg: Cow::Borrowed( "stray bracket" ),
                    help_msg: Cow::Borrowed( "was not closed" )
                } );
            }
        }

        return match errors.is_empty() {
            true => Ok( Self { file_path: file_path.to_string(), lines } ),
            false => Err( SyntaxErrors { file_path: file_path.to_string(), lines: err_lines, errors } ),
        }
    }
}

impl Lexer {
    fn iter<'lexer>( &'lexer self ) -> LexerIter<'lexer> {
        LexerIter{ lexer: self, line: 0, token: 0 }
    }


    // FIX properly handle non ASCII codes in error messages
        // IDEA only handle utf-8 characters in strings
    fn next( src: &mut Peekable<Chars> ) -> Result<Option<char>, LexerError> {
        return match src.next() {
            Some( next @ ..='\x7F' ) => Ok( Some( next ) ),
            Some( next ) => Err( (next.to_string(), SyntaxError {
                line_idx: 0,
                col: 0,
                len: next.len_utf8(),
                msg: Cow::Borrowed( "unrecognized character" ),
                help_msg: Cow::Borrowed( "not a valid ASCII character" )
            }) ),
            None => Ok( None ),
        }
    }

    fn peek_next<'src>( src: &'src mut Peekable<Chars> ) -> Result<Option<&'src char>, LexerError> {
        return match src.peek() {
            Some( next @ ..='\x7F' ) => Ok( Some( next ) ),
            Some( next ) => Err( (next.to_string(), SyntaxError {
                line_idx: 0,
                col: 0,
                len: next.len_utf8(),
                msg: Cow::Borrowed( "unrecognized character" ),
                help_msg: Cow::Borrowed( "not a valid ASCII character" )
            }) ),
            None => Ok( None ),
        }
    }


    fn next_char( src: &mut Peekable<Chars>, token_text: &mut String ) -> Result<u8, LexerError> {
        return match Self::next( src )? {
            Some( '\n' ) | None => Err( (token_text.clone(), SyntaxError {
                line_idx: 0,
                col: 0,
                len: token_text.len(),
                msg: Cow::Borrowed( "invalid character literal" ),
                help_msg: Cow::Borrowed( "missing closing single quote" )
            }) ),
            Some( next ) => {
                token_text.push( next );
                Ok( next as u8 )
            },
        }
    }

    fn parse_char( src: &mut Peekable<Chars>, token_text: &mut String ) -> Result<u8, LexerError> {
        // IDEA treat character literals as just strings of lenght 1, reporting errors if over 1
        return match Self::next_char( src, token_text )? {
            b'\\' => match Self::next_char( src, token_text )? {
                b'\\' => Ok( b'\\' ),
                b'n' => Ok( b'\n' ),
                b't' => Ok( b'\t' ),
                b'\'' => Ok( b'\'' ),
                b'"' => Ok( b'"' ),
                _ => Err( (token_text.clone(), SyntaxError {
                    line_idx: 0,
                    col: 0,
                    len: token_text.len(),
                    msg: Cow::Borrowed( "invalid escape character literal" ),
                    help_msg: Cow::Borrowed( "check the documentation for a list of valid escape characters" )
                }) ),
            },
            b'\x00'..=b'\x1F' | b'\x7F' => Err( (token_text.clone(), SyntaxError {
                line_idx: 0,
                col: 0,
                len: token_text.len(),
                msg: Cow::Borrowed( "invalid character literal" ),
                help_msg: Cow::Borrowed( "cannot be a control character" )
            }) ),
            next => Ok( next ),
        }
    }


    fn next_str_char( src: &mut Peekable<Chars>, text: &mut Vec<u8> ) -> Result<u8, LexerError> {
        return match Self::next( src )? {
            Some( ch ) => Ok( ch as u8 ),
            None => {
                // TODO avoid cloning of text
                let unclosed_string = format!( "\"{}", String::from_utf8( text.to_owned() ).unwrap() );
                let unclosed_string_len = unclosed_string.len();

                Err( (unclosed_string, SyntaxError {
                    line_idx: 0,
                    col: 0,
                    len: unclosed_string_len,
                    msg: Cow::Borrowed( "invalid string literal" ),
                    help_msg: Cow::Borrowed( "missing closing double quote" )
                }) )
            },
        }
    }

    fn parse_str( src: &mut Peekable<Chars> ) -> Result<Vec<u8>, LexerError> {
        let mut text: Vec<u8> = Vec::new();

        loop {
            match Self::next_str_char( src, &mut text )? {
                b'"' => return Ok( text ),
                b'\\' => match Self::next_str_char( src, &mut text )? {
                    b'\\' => text.push( b'\\' ),
                    b'n' => text.push( b'\n' ),
                    b't' => text.push( b'\t' ),
                    b'\'' => text.push( b'\'' ),
                    b'"' => text.push( b'"' ),
                    next => {
                        text.push( b'\\' );
                        text.push( next );

                        let unclosed_string = format!( "\"{}", String::from_utf8( text ).unwrap() );
                        let unclosed_string_len = unclosed_string.len();

                        return Err( (unclosed_string, SyntaxError {
                            line_idx: 0,
                            col: 0,
                            len: unclosed_string_len,
                            msg: Cow::Borrowed( "invalid escape character" ),
                            help_msg: Cow::Borrowed( "check the documentation for a list of valid escape characters" )
                        }) );
                    },
                },
                b'\x00'..=b'\x1F' | b'\x7F' => {
                    let unclosed_string = format!( "\"{}", String::from_utf8( text ).unwrap() );
                    let unclosed_string_len = unclosed_string.len();

                    return Err( (unclosed_string, SyntaxError {
                        line_idx: 0,
                        col: 0,
                        len: unclosed_string_len,
                        msg: Cow::Borrowed( "invalid string literal" ),
                        help_msg: Cow::Borrowed( "contains a control character" )
                    }) );
                },
                other => text.push( other ),
            }
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

    fn current_or_next( &mut self ) -> Option<Position<'lexer>> {
        return self.current().or_next( self );
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
    type Error;


    fn bounded( self, tokens: &mut LexerIter<'lexer>, err_msg: impl Into<String> ) -> Result<Self, Self::Error>
    where Self: Sized;

    fn or_next( self, tokens: &mut LexerIter<'lexer> ) -> Self
    where Self: Sized;

    fn or_previous( self, tokens: &mut LexerIter<'lexer> ) -> Self
    where Self: Sized;
}

impl<'lexer> BoundedPosition<'lexer> for Option<Position<'lexer>> {
    type Error = ASTError<'lexer>;


    fn bounded( self, tokens: &mut LexerIter<'lexer>, err_msg: impl Into<String> ) -> Result<Self, Self::Error> {
        return match self {
            Some( current ) => match current.token.kind {
                TokenKind::EOF => {
                    // we are always sure that there is at least the SOF token before the EOF token, so we can safely unwrap
                    let previous = tokens.peek_previous().unwrap();
                    Err( (previous.line, SyntaxError {
                        line_idx: 0,
                        col: previous.token.col,
                        len: previous.token.kind.len(),
                        msg: Cow::Owned( err_msg.into() ),
                        help_msg: Cow::Borrowed( "file ended after here instead" )
                    } ) )
                },
                TokenKind::SOF => {
                    // we are always sure that there is at least the EOF token after the SOF token, so we can safely unwrap
                    let next = tokens.peek_previous().unwrap();
                    Err( (next.line, SyntaxError {
                        line_idx: 0,
                        col: next.token.col,
                        len: next.token.kind.len(),
                        msg: Cow::Owned( err_msg.into() ),
                        help_msg: Cow::Borrowed( "file started before here instead" )
                    } ) )
                },
                _ => Ok( self ),
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


#[derive( Debug, Clone )]
enum Expression {
    Literal( Literal ),
    Binary { lhs: Box<Expression>, op: Operator, rhs: Box<Expression> },
    Identifier( String, Type ),
}

impl Display for Expression {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Literal( literal ) => write!( f, "{}", literal ),
            Self::Binary { lhs, op, rhs } => write!( f, "({} {} {})", lhs, op, rhs ),
            Self::Identifier( name, _ ) => write!( f, "{}", name ),
        }
    }
}

impl TypeOf for Expression {
    fn typ( &self ) -> Type {
        return match self {
            Self::Literal( literal ) => literal.typ(),
            Self::Binary { op, .. } => op.typ(),
            Self::Identifier( _, typ ) => *typ,
        }
    }
}

impl Precedence for Expression {
    fn precedence( &self ) -> usize {
        return match self {
            Self::Literal( _ ) | Self::Identifier( _, _) => 0,
            Self::Binary { .. } => 1,
        }
    }

    fn precedence_over( &self, other: &Self ) -> Ordering {
        return self.precedence().cmp( &other.precedence() );
    }
}


#[derive( Debug )]
struct Definition {
    mutability: Mutability,
    name: String,
    typ: Type,
}

#[derive( Debug )]
struct Scope {
    parent: Option<ScopeIdx>,
    definitions: Vec<Definition>,
    nodes: Vec<Node>,
}

type ScopeIdx = usize;

#[derive( Debug )]
struct Scopes {
    scopes: Vec<Scope>,
    current: ScopeIdx,
}

// Creation of scopes during AST creation
impl<'this> Scopes {
    fn create_and_add_to_current( &mut self ) -> ScopeIdx {
        self.scopes.push( Scope { parent: Some( self.current ), definitions: Vec::new(), nodes: Vec::new() } );
        let new_scope = self.scopes.len() - 1;
        self.scopes[ self.current ].nodes.push( Node::Scope( new_scope ) );
        self.current = new_scope;
        return self.current;
    }

    fn create( &mut self ) -> ScopeIdx {
        self.scopes.push( Scope { parent: Some( self.current ), definitions: Vec::new(), nodes: Vec::new() } );
        self.current = self.scopes.len() - 1;
        return self.current;
    }

    fn current( &'this self ) -> &'this Scope {
        return &self.scopes[ self.current ];
    }
}

// Resolution of identifiers
impl<'this> Scopes {
    fn resolve( &'this self, name: &str ) -> Option<&'this Definition> {
        let mut current_scope = self.current();

        loop {
            for definition in &current_scope.definitions {
                if definition.name == name {
                    return Some( definition );
                }
            }

            current_scope = &self.scopes[ current_scope.parent? ];
        }
    }

    fn resolve_mut( &'this mut self, name: &str ) -> Option<&'this mut Definition> {
        let mut current_scope = self.current;

        loop {
            let scope = &self.scopes[ current_scope ];
            for (i, definition) in scope.definitions.iter().enumerate() {
                if definition.name == name {
                    return Some( &mut self.scopes[ current_scope ].definitions[ i ] );
                }
            }

            current_scope = scope.parent?;
        }
    }
}


#[derive( Debug, Clone )]
struct If {
    condition: Expression,
    scope: ScopeIdx,
}

impl Display for If {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return write!( f, "if {}", self.condition );
    }
}

#[derive( Debug, Clone )]
struct IfStatement {
    ifs: Vec<If>,
    els: Option<ScopeIdx>,
}


#[allow( dead_code )]
#[derive( Debug, Clone )]
struct ForStatement {
    pre_statement: Option<Box<Node>>,
    condition: Option<Expression>,
    post_statement: Option<Box<Node>>,
    scope: ScopeIdx,
}


enum Statement {
    Empty,
    Stop,
    Single( Node ),
    Multiple( Vec<Node> ),
}

#[derive( Debug, Clone )]
enum Node {
    Expression( Expression ),
    Print( Expression ),
    If( IfStatement ),
    For( ForStatement ),
    Break,
    Continue,

    Definition( String, Expression ),
    Assignment( String, Expression ),
    Scope( ScopeIdx ),
}

impl Display for Node {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Expression( expression ) => write!( f, "{}", expression ),
            Self::Print( node ) => write!( f, "print {}", node ),
            Self::If( iff ) => write!( f, "{}", iff.ifs[ 0 ] ),
            Self::For( forr ) => match &forr.condition {
                None => write!( f, "for" ),
                Some( condition ) => write!( f, "for {}", condition ),
            },

            Self::Break | Self::Continue
            | Self::Definition( _, _ ) | Self::Assignment( _, _ )
            | Self::Scope( _ ) => unreachable!(),
        }
    }
}


#[derive( Debug )]
struct AST<'lexer> {
    tokens: LexerIter<'lexer>, // NOTE possibly remove this field
    scopes: Scopes,
    for_depth: usize,
}

impl<'lexer> TryFrom<&'lexer Lexer> for AST<'lexer> {
    type Error = SyntaxErrors<'lexer>;

    fn try_from( lexer: &'lexer Lexer ) -> Result<Self, Self::Error> {
        let mut this = Self {
            tokens: lexer.iter(),
            scopes: Scopes { scopes: Vec::new(), current: 0 },
            for_depth: 0,
        };

        let mut errors = SyntaxErrors {
            file_path: lexer.file_path.clone(),
            lines: Vec::new(),
            errors: Vec::new(),
        };

        this.parse_scope( &mut errors );

        return match errors.errors.is_empty() {
            true => Ok( this ),
            false => Err( errors ),
        }
    }
}

// Parsing of tokens
impl<'lexer> AST<'lexer> {
    fn parse_scope( &mut self, errors: &mut SyntaxErrors<'lexer> ) {
        while let Some( current ) = self.tokens.current_or_next() {
            let statement_result = match current.token.kind {
                TokenKind::Literal( _ )
                | TokenKind::Bracket( BracketKind::OpenRound )
                | TokenKind::True | TokenKind::False => match self.expression() {
                    Ok( expression ) => Ok( Statement::Single( Node::Expression( expression ) ) ),
                    Err( err ) => Err( err ),
                },
                TokenKind::Print | TokenKind::PrintLn => self.print(),
                TokenKind::If => self.if_statement( errors ),
                TokenKind::Else => {
                    self.tokens.next();
                    Err( (current.line, SyntaxError {
                        line_idx: 0,
                        col: current.token.col,
                        len: current.token.kind.len(),
                        msg: Cow::Borrowed( "invalid if statement" ),
                        help_msg: Cow::Borrowed( "stray else block" )
                    }) )
                },
                TokenKind::For => self.for_statement( errors ),
                TokenKind::Break => {
                    self.tokens.next();
                    match self.for_depth {
                        0 => Err( (current.line, SyntaxError {
                            line_idx: 0,
                            col: current.token.col,
                            len: current.token.kind.len(),
                            msg: Cow::Borrowed( "invalid break statement" ),
                            help_msg: Cow::Borrowed( "cannot be used outside of loops" )
                        }) ),
                        _ => Ok( Statement::Single( Node::Break ) ),
                    }
                },
                TokenKind::Continue => {
                    self.tokens.next();
                    match self.for_depth {
                        0 => Err( (current.line, SyntaxError {
                            line_idx: 0,
                            col: current.token.col,
                            len: current.token.kind.len(),
                            msg: Cow::Borrowed( "invalid continue statement" ),
                            help_msg: Cow::Borrowed( "cannot be used outside of loops" )
                        }) ),
                        _ => Ok( Statement::Single( Node::Continue ) ),
                    }
                },
                TokenKind::Definition( _ ) => self.variable_definition(),
                TokenKind::Identifier( _ ) => match self.tokens.peek_next() {
                    Some( next ) => match next.token.kind {
                        TokenKind::Equals
                        | TokenKind::Op( Operator::PowEquals )
                        | TokenKind::Op( Operator::TimesEquals )
                        | TokenKind::Op( Operator::DivideEquals )
                        | TokenKind::Op( Operator::PlusEquals )
                        | TokenKind::Op( Operator::MinusEquals ) => self.variable_reassignment(),
                        _ => match self.expression() {
                            Ok( expression ) => Ok( Statement::Single( Node::Expression( expression ) ) ),
                            Err( err ) => Err( err ),
                        },
                    },
                    None => Ok( Statement::Stop ),
                },
                TokenKind::Bracket( BracketKind::OpenCurly ) => {
                    self.scopes.create_and_add_to_current();

                    self.tokens.next();
                    Ok( Statement::Empty )
                },
                TokenKind::Bracket( BracketKind::CloseCurly ) => {
                    self.tokens.next();
                    self.scopes.current = match self.scopes.current().parent {
                        None => 0,
                        Some( parent ) => parent,
                    };

                    Ok( Statement::Stop )
                },
                TokenKind::Bracket( BracketKind::CloseRound ) => {
                    self.tokens.next();
                    Err( (current.line, SyntaxError {
                        line_idx: 0,
                        col: current.token.col,
                        len: current.token.kind.len(),
                        msg: Cow::Borrowed( "invalid expression" ),
                        help_msg: Cow::Borrowed( "stray closed parenthesis" )
                    }) )
                },
                TokenKind::Equals => {
                    self.tokens.next();
                    Err( (current.line, SyntaxError {
                        line_idx: 0,
                        col: current.token.col,
                        len: current.token.kind.len(),
                        msg: Cow::Borrowed( "invalid assignment" ),
                        help_msg: Cow::Borrowed( "stray assignment" )
                    }) )
                },
                TokenKind::Op( _ ) => {
                    self.tokens.next();
                    Err( (current.line, SyntaxError {
                        line_idx: 0,
                        col: current.token.col,
                        len: current.token.kind.len(),
                        msg: Cow::Borrowed( "invalid expression" ),
                        help_msg: Cow::Borrowed( "stray binary operator" )
                    }) )
                },
                TokenKind::Comment( _ ) | TokenKind::SemiColon | TokenKind::Empty => {
                    self.tokens.next();
                    Ok( Statement::Empty )
                },
                TokenKind::EOF => {
                    self.tokens.next();
                    Ok( Statement::Stop )
                },
                TokenKind::SOF => {
                    let global_scope = Scope { parent: None, definitions: Vec::new(), nodes: Vec::new() };
                    self.scopes.scopes.push( global_scope );

                    self.tokens.next();
                    Ok( Statement::Empty )
                },
                TokenKind::Unexpected( _ ) => unreachable!(),
            };

            match statement_result {
                Ok( Statement::Empty ) => continue,
                Ok( Statement::Stop ) => break,
                Ok( Statement::Single( node ) ) => self.scopes.scopes[ self.scopes.current ].nodes.push( node ),
                Ok( Statement::Multiple( multiple_nodes ) ) =>
                    for node in multiple_nodes {
                        self.scopes.scopes[ self.scopes.current ].nodes.push( node );
                    },
                Err( (err_line, mut err) ) => {
                    let mut found = false;
                    for line in &errors.lines {
                        if err_line.number == line.number {
                            found = true;
                            break;
                        }
                    }

                    if !found {
                        errors.lines.push( Cow::Borrowed( err_line ) );
                    }

                    err.line_idx = errors.lines.len() - 1;
                    errors.errors.push( err );
                },
            }
        }
    }
}

// Parsing of Expressions
impl<'lexer> AST<'lexer> {
    // TODO disallow implicit conversions (str + i64, char + i64, str + char or str + str (maybe treat this as concatenation))
        // IDEA introduce casting operators
    // TODO implement negative numbers
    fn factor( &mut self ) -> Result<Expression, ASTError<'lexer>> {
        let current = self.tokens.current_or_next().bounded( &mut self.tokens, "expected expression" )?.unwrap();
        let factor = match &current.token.kind {
            TokenKind::True => Ok( Expression::Literal( Literal::Bool { value: true } ) ),
            TokenKind::False => Ok( Expression::Literal( Literal::Bool { value: false } ) ),
            TokenKind::Literal( literal ) => Ok( Expression::Literal( literal.clone() ) ),
            TokenKind::Identifier( name ) => match self.scopes.resolve( name ) {
                Some( definition ) => Ok( Expression::Identifier( name.clone(), definition.typ ) ),
                None => Err( (current.line, SyntaxError {
                    line_idx: 0,
                    col: current.token.col,
                    len: current.token.kind.len(),
                    msg: Cow::Borrowed( "variable not defined" ),
                    help_msg: Cow::Borrowed( "was not previously defined in this scope" )
                }) ),
            },
            TokenKind::Bracket( BracketKind::OpenRound ) => {
                let expression_start_pos = self.tokens.next().bounded( &mut self.tokens, "expected expression" )?.unwrap();
                match expression_start_pos.token.kind {
                    TokenKind::Bracket( BracketKind::CloseRound ) => Err( (expression_start_pos.line, SyntaxError {
                        line_idx: 0,
                        col: expression_start_pos.token.col,
                        len: expression_start_pos.token.kind.len(),
                        msg: Cow::Borrowed( "invalid expression" ),
                        help_msg: Cow::Borrowed( "empty expressions are not allowed" )
                    }) ),
                    _ => {
                        // TODO check if it is okay to remove the closed parenthesis check
                        let expression = self.expression_no_semicolon()?;
                        let close_bracket_pos = self.tokens.current_or_next().bounded( &mut self.tokens, "expected closed parenthesis" )?.unwrap();
                        match close_bracket_pos.token.kind {
                            TokenKind::Bracket( BracketKind::CloseRound ) => Ok( expression ),
                            _ => Err( (current.line, SyntaxError {
                                line_idx: 0,
                                col: current.token.col,
                                len: current.token.kind.len(),
                                msg: Cow::Borrowed( "invalid expression" ),
                                help_msg: Cow::Borrowed( "unclosed parenthesis" )
                            }) ),
                        }
                    }
                }
            },
            _ => Err( (current.line, SyntaxError {
                line_idx: 0,
                col: current.token.col,
                len: current.token.kind.len(),
                msg: Cow::Borrowed( "invalid expression" ),
                help_msg: Cow::Borrowed( "expected number literal" )
            }) ),
        };

        self.tokens.next();
        return factor;
    }

    fn operator( &mut self, ops: &[Operator] ) -> Result<Option<Operator>, ASTError<'lexer>> {
        let current_pos = self.tokens.current_or_next().bounded( &mut self.tokens, "invalid expression" )?.unwrap();
        let operator = match current_pos.token.kind {
            TokenKind::Op( op ) => match ops.contains( &op ) {
                true => Some( op ),
                false => None,
            },
            _ => None,
        };

        if let Some( _ ) = operator {
            self.tokens.next();
        }

        return Ok( operator );
    }

    fn exponentiation( &mut self ) -> Result<Expression, ASTError<'lexer>> {
        let mut lhs = self.factor()?;

        while let Some( op ) = self.operator( &[Operator::Pow] )? {
            let rhs = self.factor()?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn multiplicative_expression( &mut self ) -> Result<Expression, ASTError<'lexer>> {
        let mut lhs = self.exponentiation()?;

        while let Some( op ) = self.operator( &[Operator::Times, Operator::Divide, Operator::Remainder] )? {
            let rhs = self.exponentiation()?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn additive_expression( &mut self ) -> Result<Expression, ASTError<'lexer>> {
        let mut lhs = self.multiplicative_expression()?;

        while let Some( op ) = self.operator( &[Operator::Plus, Operator::Minus] )? {
            let rhs = self.multiplicative_expression()?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn comparative_expression( &mut self ) -> Result<Expression, ASTError<'lexer>> {
        let mut lhs = self.additive_expression()?;

        let ops = [
            Operator::EqualsEquals, Operator::NotEquals,
            Operator::Greater, Operator::GreaterOrEquals,
            Operator::Less, Operator::LessOrEquals,
            Operator::Compare
        ];

        while let Some( op ) = self.operator( &ops )? {
            let rhs = self.additive_expression()?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn boolean_expression( &mut self ) -> Result<Expression, ASTError<'lexer>> {
        let mut lhs = self.comparative_expression()?;

        while let Some( op ) = self.operator( &[Operator::And, Operator::Or, Operator::Xor] )? {
            let op_pos = self.tokens.peek_previous().unwrap();

            if lhs.typ() != Type::Bool {
                return Err( (op_pos.line, SyntaxError {
                    line_idx: 0,
                    col: op_pos.token.col,
                    len: op_pos.token.kind.len(),
                    msg: Cow::Borrowed( "invalid boolean expression" ),
                    help_msg: Cow::Borrowed( "must be preceded by a boolean expression" )
                }) );
            }

            let rhs = self.comparative_expression()?;
            if rhs.typ() != Type::Bool {
                return Err( (op_pos.line, SyntaxError {
                    line_idx: 0,
                    col: op_pos.token.col,
                    len: op_pos.token.kind.len(),
                    msg: Cow::Borrowed( "invalid boolean expression" ),
                    help_msg: Cow::Borrowed( "must be followed by a boolean expression" )
                }) );
            }

            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn expression_no_semicolon( &mut self ) -> Result<Expression, ASTError<'lexer>> {
        return self.boolean_expression();
    }

    fn expression( &mut self ) -> Result<Expression, ASTError<'lexer>> {
        let expression = self.boolean_expression()?;
        let current = self.tokens.current_or_next().bounded( &mut self.tokens, "expected semicolon" )?.unwrap();
        return match current.token.kind {
            // semicolons are optional if found before a closing curly bracket
            // this allows the last statement before the end of a block not to have a semicolon
            TokenKind::Bracket( BracketKind::CloseCurly ) => Ok( expression ),
            TokenKind::SemiColon => {
                self.tokens.next();
                Ok( expression )
            },
            _ => {
                let previous = self.tokens.peek_previous().bounded( &mut self.tokens, "expected semicolon" )?.unwrap();
                self.tokens.next();

                Err( (previous.line, SyntaxError {
                    line_idx: 0,
                    col: previous.token.col,
                    len: previous.token.kind.len(),
                    msg: Cow::Borrowed( "invalid expression" ),
                    help_msg: Cow::Borrowed( "expected an operator after this token to complete the expression, or a ';' to end the statement" )
                }) )
            },
        }
    }
}

// Parsing of variable definitions and assignments
impl<'lexer> AST<'lexer> {
    fn variable_definition( &mut self ) -> Result<Statement, ASTError<'lexer>> {
        let definition_pos = self.tokens.current().unwrap();
        let kind = match definition_pos.token.kind {
            TokenKind::Definition( kind ) => kind,
            _ => unreachable!(),
        };

        let name_pos = self.tokens.next().bounded( &mut self.tokens, "expected identifier" )?.unwrap();
        let name = match &name_pos.token.kind {
            TokenKind::Identifier( name ) => Ok( name.clone() ),
            _ => Err( (name_pos.line, SyntaxError {
                line_idx: 0,
                col: name_pos.token.col,
                len: name_pos.token.kind.len(),
                msg: Cow::Borrowed( "invalid assignment" ),
                help_msg: Cow::Borrowed( "expected variable name" )
            }) ),
        };

        let equals_pos = self.tokens.next().bounded( &mut self.tokens, "expected equals" )?.unwrap();
        let equals = match equals_pos.token.kind {
            TokenKind::Equals => Ok( () ),
            _ => Err( (name_pos.line, SyntaxError {
                line_idx: 0,
                col: name_pos.token.col,
                len: name_pos.token.kind.len(),
                msg: Cow::Borrowed( "invalid assignment" ),
                help_msg: Cow::Borrowed( "expected '=' after variable name" )
            }) ),
        };

        let value_pos = self.tokens.next().bounded( &mut self.tokens, "expected expression" )?.unwrap();
        let value = match value_pos.token.kind {
            TokenKind::True | TokenKind::False
            | TokenKind::Literal( _ ) | TokenKind::Identifier( _ )
            | TokenKind::Bracket( BracketKind::OpenRound ) => self.expression(),
            _ => Err( (equals_pos.line, SyntaxError {
                line_idx: 0,
                col: equals_pos.token.col,
                len: equals_pos.token.kind.len(),
                msg: Cow::Borrowed( "invalid assignment" ),
                help_msg: Cow::Borrowed( "expected expression after '='" )
            }) ),
        };

        let name = name?;
        let _ = equals?;
        let value = value?;

        return match self.scopes.resolve( &name ) {
            None => {
                let definition = Definition { mutability: kind, name: name.clone(), typ: value.typ() };
                self.scopes.scopes[ self.scopes.current ].definitions.push( definition );
                Ok( Statement::Single( Node::Definition( name, value ) ) )
            },
            Some( _ ) => Err( (name_pos.line, SyntaxError {
                line_idx: 0,
                col: name_pos.token.col,
                len: name_pos.token.kind.len(),
                msg: Cow::Borrowed( "variable redefinition" ),
                help_msg: Cow::Borrowed( "was previously defined" )
            }) ),
        }
    }

    fn variable_reassignment( &mut self ) -> Result<Statement, ASTError<'lexer>> {
        let name_pos = self.tokens.current().unwrap();
        let name = name_pos.token.kind.to_string();
        let assignment_pos = self.tokens.next().unwrap();


        let value_pos = self.tokens.next().bounded( &mut self.tokens, "expected expression" )?.unwrap();
        let new_value = match value_pos.token.kind {
            TokenKind::True | TokenKind::False
            | TokenKind::Literal( _ ) | TokenKind::Identifier( _ )
            | TokenKind::Bracket( BracketKind::OpenRound ) => match self.expression() {
                Ok( rhs ) => match &assignment_pos.token.kind {
                    TokenKind::Equals => Ok( rhs ),
                    operator => {
                        let op = match operator {
                            TokenKind::Op( op ) => *op,
                            _ => unreachable!(),
                        };

                        let lhs = Expression::Identifier( name, op.typ() );
                        Ok( Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) } )
                    },
                },
                Err( err ) => Err( err ),
            },
            _ => Err( (assignment_pos.line, SyntaxError {
                line_idx: 0,
                col: assignment_pos.token.col,
                len: assignment_pos.token.kind.len(),
                msg: Cow::Borrowed( "invalid assignment" ),
                help_msg: Cow::Owned( format!( "expected expression after '{}'", assignment_pos.token.kind) )
            }) )
        };

        let variable = match &name_pos.token.kind {
            TokenKind::Identifier( name ) => match self.scopes.resolve_mut( &name ) {
                Some( definition ) => Ok( definition ),
                None => Err( (name_pos.line, SyntaxError {
                    line_idx: 0,
                    col: name_pos.token.col,
                    len: name_pos.token.kind.len(),
                    msg: Cow::Borrowed( "variable redefinition" ),
                    help_msg: Cow::Borrowed( "was not previously defined in this scope" )
                }) ),
            },
            _ => unreachable!(),
        };

        let variable = variable?;

        let value = match variable.mutability {
            Mutability::Let => Err( (name_pos.line, SyntaxError {
                line_idx: 0,
                col: name_pos.token.col,
                len: name_pos.token.kind.len(),
                msg: Cow::Borrowed( "invalid assignment" ),
                help_msg: Cow::Borrowed( "was defined as immutable" )
            }) ),
            Mutability::Var => {
                let new_value = new_value?;
                let new_value_typ = new_value.typ();

                if variable.typ != new_value_typ {
                    Err( (name_pos.line, SyntaxError {
                        line_idx: 0,
                        col: name_pos.token.col,
                        len: name_pos.token.kind.len(),
                        msg: Cow::Borrowed( "mismatched types" ),
                        help_msg: Cow::Owned( format!(
                            "trying to assign an expression of type '{}' to a variable of type '{}'",
                            new_value_typ,
                            variable.typ
                        ) )
                    }) )
                }
                else {
                    Ok( new_value )
                }
            },
        };

        return Ok( Statement::Single( Node::Assignment( name_pos.token.kind.to_string(), value? ) ) );
    }
}

// Parsing of print statements
impl<'lexer> AST<'lexer> {
    fn print( &mut self ) -> Result<Statement, ASTError<'lexer>> {
        let print_pos = self.tokens.current().unwrap();
        match print_pos.token.kind {
            TokenKind::PrintLn => match self.tokens.peek_next() {
                Some( Position { token: &Token { kind: TokenKind::SemiColon, .. }, .. } ) => {
                    self.tokens.next();
                    let new_line = Literal::Char { value: '\n' as u8 };
                    let argument = Expression::Literal( new_line );
                    return Ok( Statement::Single( Node::Print( argument ) ) );
                },
                _ => (),
            },
            TokenKind::Print => (),
            _ => unreachable!(),
        }

        let argument_pos = self.tokens.next().bounded( &mut self.tokens, "expected print argument or semicolon" )?.unwrap();
        let argument = match &argument_pos.token.kind {
            TokenKind::True | TokenKind::False
            | TokenKind::Literal( _ ) | TokenKind::Identifier( _ )
            | TokenKind::Bracket( BracketKind::OpenRound ) => self.expression(),
            _ => Err( (argument_pos.line, SyntaxError {
                line_idx: 0,
                col: argument_pos.token.col,
                len: argument_pos.token.kind.len(),
                msg: Cow::Borrowed( "invalid print argument" ),
                help_msg: Cow::Borrowed( "expected an expression" )
            }) ),
        };

        let argument = argument?;

        return match print_pos.token.kind {
            TokenKind::Print => Ok( Statement::Single( Node::Print( argument ) ) ),
            TokenKind::PrintLn => {
                let new_line = Literal::Char { value: '\n' as u8 };
                let println_argument = Expression::Literal( new_line );
                Ok( Statement::Multiple( vec![
                    Node::Print( argument ),
                    Node::Print( println_argument )
                ] ) )
            },
            _ => unreachable!(),
        };
    }
}

// Parsing of if statements
impl<'lexer> AST<'lexer> {
    fn if_statement( &mut self, errors: &mut SyntaxErrors<'lexer> ) -> Result<Statement, ASTError<'lexer>> {
        let iff = self.iff( errors )?;
        let mut if_statement = IfStatement { ifs: vec![ iff ], els: None };
        let else_pos = self.tokens.current_or_next();
        self.els( &mut if_statement, else_pos, errors )?;

        return Ok( Statement::Single( Node::If( if_statement ) ) );
    }

    fn iff( &mut self, errors: &mut SyntaxErrors<'lexer> ) -> Result<If, ASTError<'lexer>> {
        let if_pos = self.tokens.current().unwrap();

        self.tokens.next().bounded( &mut self.tokens, "expected boolean expression" )?.unwrap();
        let expression = self.expression_no_semicolon()?;
        let condition = match &expression.typ() {
            Type::Bool => Ok( expression ),
            Type::Char | Type::Int | Type::Str => Err( (if_pos.line, SyntaxError {
                line_idx: 0,
                col: if_pos.token.col,
                len: if_pos.token.kind.len(),
                msg: Cow::Borrowed( "expected boolean expression" ),
                help_msg: Cow::Borrowed( "must be followed by a boolean expression", )
            }) ),
        };

        let condition = condition?;

        let open_curly_pos = self.tokens.current_or_next().bounded( &mut self.tokens, "expected curly bracket" )?.unwrap();
        let open_curly = match open_curly_pos.token.kind {
            TokenKind::Bracket( BracketKind::OpenCurly ) => {
                let scope = self.scopes.create();
                self.tokens.next();
                Ok( scope )
            },
            _ => {
                let before_curly_bracket_pos = self.tokens.peek_previous().unwrap();
                Err( (before_curly_bracket_pos.line, SyntaxError {
                    line_idx: 0,
                    col: before_curly_bracket_pos.token.col,
                    len: before_curly_bracket_pos.token.kind.len(),
                    msg: Cow::Borrowed( "expected block scope" ),
                    help_msg: Cow::Borrowed( "must be followed by an opened curly bracket", )
                }) )
            },
        };

        let scope = open_curly?;

        self.parse_scope( errors );
        return Ok( If { condition, scope } );
    }

    fn els( &mut self, if_statement: &mut IfStatement, else_pos: Option<Position<'lexer>>, errors: &mut SyntaxErrors<'lexer> ) -> Result<(), ASTError<'lexer>> {
        match else_pos {
            None => (),
            Some( pos ) => if let TokenKind::Else = pos.token.kind {
                let if_or_block_pos = self.tokens.next().bounded( &mut self.tokens, "expected block or if statement after this token" )?.unwrap();
                match if_or_block_pos.token.kind {
                    TokenKind::Bracket( BracketKind::OpenCurly ) => {
                        let els = self.scopes.create();
                        self.tokens.next();

                        self.parse_scope( errors );
                        if_statement.els = Some( els );
                    },
                    TokenKind::If => {
                        let else_if = self.iff( errors )?;
                        if_statement.ifs.push( else_if );

                        let next_else_pos = self.tokens.current_or_next();
                        self.els( if_statement, next_else_pos, errors )?;
                    },
                    _ => return Err( (pos.line, SyntaxError {
                        line_idx: 0,
                        col: pos.token.col,
                        len: pos.token.kind.len(),
                        msg: Cow::Borrowed( "invalid else statement" ),
                        help_msg: Cow::Borrowed( "expected block of if statement after this token" )
                    }) ),
                }
            }
        }

        return Ok( () );
    }
}

// Parsing of for statements
impl<'lexer> AST<'lexer> {
    fn for_statement( &mut self, errors: &mut SyntaxErrors<'lexer> ) -> Result<Statement, ASTError<'lexer>> {
        self.for_depth += 1;
        let iff = self.iff( errors );
        self.for_depth -= 1;

        let iff = iff?;
        return Ok( Statement::Single( Node::For( ForStatement {
            pre_statement: None,
            condition: Some( iff.condition ),
            post_statement: None,
            scope: iff.scope
        } ) ) );
    }
}


#[derive( Debug )]
enum ControlFlowStatement {
    None,
    Break,
    Continue,
}

#[derive( Debug )]
struct InterpreterVariable {
    name: String,
    value: Expression
}

#[derive( Debug )]
struct Interpreter<'ast> {
    ast: &'ast AST<'ast>,
    variables: Vec<InterpreterVariable>,
}

// Resolution and evaluation from identifiers
impl<'ast> Interpreter<'ast> {
    fn resolve( &'ast self, name: &str ) -> &'ast InterpreterVariable {
        for variable in &self.variables {
            if variable.name == name {
                return variable;
            }
        }

        unreachable!();
    }


    fn evaluate( &self, expression: &Expression ) -> Literal {
        return match expression {
            Expression::Literal( literal ) => literal.clone(),
            Expression::Binary { lhs, op, rhs } => {
                let lhs: isize = self.evaluate( &lhs ).into();
                let rhs: isize = self.evaluate( &rhs ).into();

                match op {
                    Operator::Pow | Operator::PowEquals => match rhs {
                        2 => Literal::Int { value: lhs * lhs },
                        _ => Literal::Int { value: lhs.pow( rhs as u32 ) },
                    },
                    Operator::Times | Operator::TimesEquals => Literal::Int { value: lhs * rhs },
                    Operator::Divide | Operator::DivideEquals => Literal::Int { value: lhs / rhs },
                    Operator::Remainder | Operator::RemainderEquals => Literal::Int { value: lhs % rhs },

                    Operator::Plus | Operator::PlusEquals => Literal::Int { value: lhs + rhs },
                    Operator::Minus | Operator::MinusEquals => Literal::Int { value: lhs - rhs },

                    Operator::EqualsEquals => Literal::Bool { value: lhs == rhs },
                    Operator::NotEquals => Literal::Bool { value: lhs != rhs },
                    Operator::Greater => Literal::Bool { value: lhs > rhs },
                    Operator::GreaterOrEquals => Literal::Bool { value: lhs >= rhs },
                    Operator::Less => Literal::Bool { value: lhs < rhs },
                    Operator::LessOrEquals => Literal::Bool { value: lhs <= rhs },
                    Operator::Compare => Literal::Int { value: lhs.cmp( &rhs ) as isize },

                    Operator::And => Literal::Bool { value: lhs == 1 && rhs == 1 },
                    Operator::Or => Literal::Bool { value: lhs == 1 || rhs == 1 },
                    Operator::Xor => Literal::Bool { value: (lhs != 1 && rhs == 1) || (lhs == 1 && rhs != 1) },
                }
            },
            Expression::Identifier( name, _ ) => self.evaluate( &self.resolve( &name ).value ),
        }
    }

    fn evaluate_expression( &self, expression: &Expression ) -> Expression {
        return Expression::Literal( self.evaluate( expression ) );
    }
}

// Interpretation of nodes
impl<'ast> Interpreter<'ast> {
    const INTERPRETING: &'static str = colored!{
        text: "Interpreting",
        foreground: Foreground::LightGreen,
        bold: true,
    };


    fn interpret_scope( &mut self, scope: ScopeIdx ) {
        let current_scope = &self.ast.scopes.scopes[ scope ];
        let mut defined_variables = 0;

        for node in  &current_scope.nodes {
            match node {
                Node::Print( argument ) => self.evaluate( &argument ).display(),
                Node::If( if_statement ) => 'iff: {
                    for iff in &if_statement.ifs {
                        if let Literal::Bool { value: true } = self.evaluate( &iff.condition ) {
                            self.interpret_scope( iff.scope );
                            break 'iff;
                        }
                    }

                    if let Some( els ) = &if_statement.els {
                        self.interpret_scope( *els );
                    }
                },
                Node::For( forr ) =>
                    if let Some( condition ) = &forr.condition {
                        while let Literal::Bool { value: true } = self.evaluate( condition ) {
                            if let ControlFlowStatement::Break = self.interpret_loop( forr.scope ) {
                                break;
                            }
                        }
                    }
                    else {
                        loop {
                            if let ControlFlowStatement::Break = self.interpret_loop( forr.scope ) {
                                break;
                            }
                        }
                    },
                Node::Break | Node::Continue => unreachable!(),
                Node::Definition( name, value ) => {
                    defined_variables += 1;
                    self.variables.push( InterpreterVariable {
                        name: name.clone(),
                        value: self.evaluate_expression( value )
                    } );
                }
                Node::Assignment( name, new_value ) => {
                    let value = self.evaluate_expression( new_value );
                    for variable in &mut self.variables {
                        if variable.name == *name {
                            variable.value = value;
                            break;
                        }
                    }
                },
                Node::Scope( inner ) => self.interpret_scope( *inner ),
                Node::Expression( expression ) => { self.evaluate( expression ); },
            }
        }

        for _ in 0..defined_variables {
            self.variables.pop();
        }
    }

    fn interpret_loop( &mut self, scope: ScopeIdx ) -> ControlFlowStatement {
        let current_scope = &self.ast.scopes.scopes[ scope ];
        let mut defined_variables = 0;

        let control_flow = 'control_flow: {
            for node in  &current_scope.nodes {
                let cf = match node {
                    Node::Print( argument ) => {
                        self.evaluate( &argument ).display();
                        ControlFlowStatement::None
                    },
                    Node::If( if_statement ) => 'iff: {
                        for iff in &if_statement.ifs {
                            if let Literal::Bool { value: true } = self.evaluate( &iff.condition ) {
                                break 'iff self.interpret_loop( iff.scope );
                            }
                        }

                        if let Some( els ) = &if_statement.els {
                            break 'iff self.interpret_loop( *els );
                        }

                        ControlFlowStatement::None
                    },
                    Node::For( forr ) => {
                        if let Some( condition ) = &forr.condition {
                            while let Literal::Bool { value: true } = self.evaluate( condition ) {
                                if let ControlFlowStatement::Break = self.interpret_loop( forr.scope ) {
                                    break;
                                }
                            }
                        }
                        else {
                            loop {
                                if let ControlFlowStatement::Break = self.interpret_loop( forr.scope ) {
                                    break;
                                }
                            }
                        }

                        ControlFlowStatement::None
                    },
                    Node::Break => break 'control_flow ControlFlowStatement::Break,
                    Node::Continue => break 'control_flow ControlFlowStatement::Continue,
                    Node::Definition( name, value ) => {
                        defined_variables += 1;
                        self.variables.push( InterpreterVariable {
                            name: name.clone(),
                            value: self.evaluate_expression( value )
                        } );

                        ControlFlowStatement::None
                    },
                    Node::Assignment( name, new_value ) => {
                        let value = self.evaluate_expression( new_value );
                        for variable in &mut self.variables {
                            if variable.name == *name {
                                variable.value = value;
                                break;
                            }
                        }

                        ControlFlowStatement::None
                    },
                    Node::Scope( inner ) => self.interpret_loop( *inner ),
                    Node::Expression( expression ) => {
                        self.evaluate( expression );
                        ControlFlowStatement::None
                    },
                };

                if let ControlFlowStatement::None = cf {} else { break 'control_flow cf; }
            }

            ControlFlowStatement::None
        };

        for _ in 0..defined_variables {
            self.variables.pop();
        }

        return control_flow;
    }

    fn interpret( &mut self, file_path: &str ) {
        println!( "{}: {}", Self::INTERPRETING, file_path );

        self.interpret_scope( 0 );
    }
}


#[allow( dead_code )]
#[derive( Debug, PartialEq, Clone, Copy )]
enum Register {
    RAX,
    RBX,
    RCX,
    RDX,
    RSI,
    RDI,
    RBP,
    RSP,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15
}

impl Display for Register {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::RAX => write!( f, "rax" ),
            Self::RBX => write!( f, "rbx" ),
            Self::RCX => write!( f, "rcx" ),
            Self::RDX => write!( f, "rdx" ),
            Self::RSI => write!( f, "rsi" ),
            Self::RDI => write!( f, "rdi" ),
            Self::RBP => write!( f, "rbp" ),
            Self::RSP => write!( f, "rsp" ),
            Self::R8 => write!( f, "r8" ),
            Self::R9 => write!( f, "r9" ),
            Self::R10 => write!( f, "r10" ),
            Self::R11 => write!( f, "r11" ),
            Self::R12 => write!( f, "r12" ),
            Self::R13 => write!( f, "r13" ),
            Self::R14 => write!( f, "r14" ),
            Self::R15 => write!( f, "r15" ),
        }
    }
}

#[allow( dead_code )]
enum MemorySize {
    Byte,
    Word,
    DWord,
    QWord,
}

impl Display for MemorySize {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Byte => write!( f, "byte" ),
            Self::Word => write!( f, "word" ),
            Self::DWord => write!( f, "dword" ),
            Self::QWord => write!( f, "qword" ),
        }
    }
}

#[derive( Debug )]
struct Memory {
    reg: Register,
    offset: usize,
}

impl Display for Memory {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return write!( f, "[{} + {}]", self.reg, self.offset );
    }
}

#[allow( dead_code )]
enum MovDst {
    Register( Register ),
    Memory( MemorySize, Memory ),
}

impl Display for MovDst {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Register( reg ) => write!( f, "{}", reg ),
            Self::Memory( size, mem ) => write!( f, "{} {}", size, mem ),
        }
    }
}

#[allow( dead_code )]
enum MovSrc {
    Immediate( Literal ),
    Register( Register ),
    Memory( MemorySize, Memory ),
}

impl Display for MovSrc {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Immediate( literal ) => write!( f, "{}", literal ),
            Self::Register( reg ) => write!( f, "{}", reg ),
            Self::Memory( size, mem ) => write!( f, "{} {}", size, mem ),
        }
    }
}

#[allow( dead_code )]
struct Mov {
    // instruction: Instruction, // TODO introduce instruction
    dst: MovDst,
    src: MovSrc,
}

#[allow( dead_code )]
enum MovKind {
    Single( Mov ),
    Multiple( Vec<Mov> ),
}


#[derive( Debug )]
struct CompilerVariable {
    name: String,
    typ: Type,
    offset: usize,
}

#[derive( Debug )]
struct StringLabel<'ast> {
    string: &'ast Str,
    label: String,
    len_label: String,
}

#[derive( Debug )]
struct Compiler<'ast> {
    ast: &'ast AST<'ast>,

    rodata: String,
    asm: String,

    variables: Vec<CompilerVariable>,
    strings: Vec<StringLabel<'ast>>,

    if_idx: usize,
    for_idx: usize,
    for_idx_stack: Vec<usize>,
}

impl<'ast> Compiler<'ast> {
    const BUILDING: &'static str = colored!{
        text: "Building",
        foreground: Foreground::LightGreen,
        bold: true,
    };

    const RUNNING: &'static str = colored!{
        text: "Running",
        foreground: Foreground::LightGreen,
        bold: true,
    };

    const STACK_ALIGN: usize = core::mem::size_of::<usize>();


    fn compile( &mut self, file_path: &str ) -> Result<PathBuf, ()> {
        let src_file_path = Path::new( file_path );
        println!( "{}: {}", Compiler::BUILDING, src_file_path.display() );

        let asm_file_path = src_file_path.with_extension( "asm" );
        let mut asm_file = BufWriter::new( File::create( &asm_file_path ).unwrap() );

        self.rodata +=
r#" stdout: equ 1
 SYS_write: equ 1
 SYS_exit: equ 60
 EXIT_SUCCESS: equ 0

 I64_MIN: equ 1 << 63
 I64_MAX: equ ~I64_MIN
 INT_MAX_DIGITS: equ 64

 true: equ 1
 true_str: db "true"
 true_str_len: equ $ - true_str

 false: equ 0
 false_str: db "false"
 false_str_len: equ $ - false_str

 LESS: equ -1
 EQUAL: equ 0
 GREATER: equ 1"#;

        let mut stack_size = 0;
        for scope in &self.ast.scopes.scopes {
            if scope.definitions.is_empty() {
                continue;
            }

            let mut variables: Vec<(Type, Vec<&'ast Definition>)> = Vec::new();
            for definition in &scope.definitions {
                let typ = definition.typ;

                let mut type_already_encountered = false;
                for variable in &mut variables {
                    if typ == variable.0 {
                        variable.1.push( definition );
                        type_already_encountered = true;
                        break;
                    }
                }

                if !type_already_encountered {
                    variables.push( (typ, vec![ definition ]) );
                }
            }

            while !variables.is_empty() {
                let mut largest_type_bytes = 0;
                let mut current_type = 0;
                for (variable_idx, variable) in variables.iter().enumerate() {
                    let bytes = variable.0.len();
                    if bytes > largest_type_bytes {
                        largest_type_bytes = bytes;
                        current_type = variable_idx;
                    }
                }

                for variable in variables.swap_remove( current_type ).1 {
                    let name = variable.name.clone();
                    let typ = variable.typ;

                    self.variables.push( CompilerVariable { name, typ, offset: stack_size } );
                    stack_size += typ.len();
                }
            }
        }

        if stack_size > 0 {
            let misalignment = stack_size % Compiler::STACK_ALIGN;
            let needs_padding = misalignment != 0;
            let padding = needs_padding as usize * (Compiler::STACK_ALIGN - misalignment);
            stack_size += padding;

            self.asm += &format!(
                " push rbp\
                \n sub rsp, {}\
                \n mov rbp, rsp\
                \n\n",
                stack_size
            );
        }

        self.compile_scope( 0 );

        if stack_size > 0 {
            self.asm += &format!(
                " add rsp, {}\
                \n pop rbp\n",
                stack_size
            );
        }

        let program = format!(
r"global _start

section .rodata
{}

section .data
 int_str: times INT_MAX_DIGITS + 1 db 0
 int_str_bufsize: equ $ - int_str
 int_str_len: equ int_str_bufsize - 1

section .text
int_toStr:
 push rcx

 mov rsi, 10
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
 ret

int_pow:
 cmp rsi, 1
 jne .exponent_is_not_one
 mov rax, rdi
 ret

.exponent_is_not_one:
 cmp rsi, 0
 jne .exponent_is_not_zero
 mov rax, 1
 ret

.exponent_is_not_zero:
 push rsi

 mov rax, rdi
 mov rdx, 1

.next_power:
 cmp rsi, 1
 jle .done

 test rsi, 1
 jnz .exponent_is_odd

 imul rax, rax
 shr rsi, 1
 jmp .next_power

.exponent_is_odd:
 imul rdx, rax
 imul rax, rax

 dec rsi
 shr rsi, 1
 jmp .next_power

.done:
 imul rax, rdx

 pop rsi
 ret

_start:
{}

 mov rdi, EXIT_SUCCESS
 mov rax, SYS_exit
 syscall",
 self.rodata, self.asm
);

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

    fn run( &mut self, file_path: &str ) -> Result<(), ()> {
        let executable_file_path = self.compile( &file_path )?;
        println!( "{}: {}", Compiler::RUNNING, executable_file_path.display() );

        let output = Command::new( format!( "{}", executable_file_path.display() ) ).output().unwrap();
        print!( "{}", String::from_utf8_lossy( &output.stdout ) );

        return Ok( () );
    }
}

// Resolution of identifiers and string tags
impl<'ast> Compiler<'ast> {
    fn resolve( &'ast self, name: &str ) -> &'ast CompilerVariable {
        for variable in &self.variables {
            if variable.name == name {
                return variable;
            }
        }

        unreachable!();
    }

    fn string_label_idx( &mut self, string: &'ast Str ) -> usize {
        let mut string_idx = 0;
        for string_label in &self.strings {
            if string as *const _ == string_label.string as *const _ {
                return string_idx;
            }
            string_idx += 1;
        }

        let mut string_text = String::with_capacity( string.text.len() + 2 );
        string_text.push( '`' );
        for ch in &string.text {
            string_text.extend( (*ch as char).escape_default() );
        }
        string_text.push( '`' );

        let label = format!( "str_{}", string_idx );
        let len_label = format!( "str_{}_len", string_idx );

        self.rodata += &format!(
            "\n\n {}: db {}\
            \n {}: equ $ - {}",
            label, string_text,
            len_label, label
        );

        self.strings.push( StringLabel { string, label, len_label } );
        return string_idx;
    }
}

// Compilation of nodes
impl<'ast> Compiler<'ast> {
    fn compile_scope( &mut self, scope: usize ) {
        for node in &self.ast.scopes.scopes[ scope ].nodes {
            match node {
                Node::Print( argument ) => {
                    self.asm += &format!( " ; {}\n", node );
                    self.compile_expression( argument );

                    match argument.typ() {
                        Type::Int => self.asm +=
                            " mov rsi, 10\
                            \n call int_toStr\
                            \n\
                            \n mov rdi, stdout\
                            \n mov rsi, rax\
                            \n mov rax, SYS_write\
                            \n syscall\n\n",
                        Type::Char => self.asm +=
                            " push rdi\
                            \n mov rdi, stdout\
                            \n mov rsi, rsp\
                            \n mov rdx, 1\
                            \n mov rax, SYS_write\
                            \n syscall\
                            \n pop rsi\n\n",
                        Type::Bool => self.asm +=
                            " cmp rdi, true\
                            \n mov rsi, true_str\
                            \n mov rdi, false_str\
                            \n cmovne rsi, rdi\
                            \n mov rdx, true_str_len\
                            \n mov rdi, false_str_len\
                            \n cmovne rdx, rdi\
                            \n\
                            \n mov rdi, stdout\
                            \n mov rax, SYS_write\
                            \n syscall\n\n",
                        Type::Str => self.asm +=
                            " mov rdi, stdout\
                            \n mov rax, SYS_write\
                            \n syscall\n\n",
                    }
                },
                // TODO simplify: avoid checking for the presence of the else/else-ifs branches too many times
                Node::If( if_statement ) => {
                    let if_idx = self.if_idx;
                    self.if_idx += 1;

                    let mut ifs = if_statement.ifs.iter();
                    let iff = ifs.next().unwrap();

                    let (has_else_ifs, has_else) = (if_statement.ifs.len() > 1, if_statement.els.is_some());

                    // compiling the if branch
                    let if_tag = format!( "if_{}", if_idx );
                    let (if_false_tag, if_end_tag_idx) = if has_else_ifs {
                        (format!( "if_{}_else_if_0", if_idx ), Some( if_idx ))
                    }
                    else if has_else {
                        (format!( "if_{}_else", if_idx ), Some( if_idx ))
                    }
                    else {
                        (format!( "if_{}_end", if_idx ), None)
                    };

                    self.compile_if( iff, &if_tag, &if_false_tag );
                    if let Some( idx ) = if_end_tag_idx {
                        self.asm += &format!( " jmp if_{}_end\n\n", idx );
                    }

                    // compiling the else if branches
                    if has_else_ifs {
                        let last_else_if = ifs.next_back().unwrap();
                        let else_if_end_tag = format!( " jmp if_{}_end\n\n", if_idx );
                        let mut else_if_tag_idx = 0;

                        for else_if in ifs {
                            let else_if_tag = format!( "if_{}_else_if_{}", if_idx, else_if_tag_idx );
                            let else_if_false_tag = format!( "if_{}_else_if_{}", if_idx, else_if_tag_idx + 1 );

                            self.compile_if( &else_if, &else_if_tag, &else_if_false_tag );
                            self.asm += &else_if_end_tag;
                            else_if_tag_idx += 1;
                        }

                        let else_if_tag = format!( "if_{}_else_if_{}", if_idx, else_if_tag_idx );
                        let else_if_false_tag = if has_else {
                            format!( "if_{}_else", if_idx )
                        }
                        else {
                            format!( "if_{}_end", if_idx )
                        };

                        self.compile_if( last_else_if, &else_if_tag, &else_if_false_tag );
                        self.asm += &else_if_end_tag;
                    }

                    // compiling the else branch
                    if let Some( els ) = if_statement.els {
                        self.asm += &format!( "if_{}_else:\n", if_idx );
                        self.compile_scope( els );
                    }

                    self.asm += &format!( "if_{}_end:\n", if_idx );
                },
                Node::For( forr ) => {
                    let for_tag = format!( "for_{}", self.for_idx );
                    let for_end_tag = format!( "for_{}_end", self.for_idx );

                    self.for_idx_stack.push( self.for_idx );
                    self.for_idx += 1;
                    self.compile_for( forr, &for_tag, &for_end_tag );
                    self.for_idx_stack.pop();

                    self.asm += &format!(
                        " jmp {}\
                        \n{}:\n\n",
                        for_tag,
                        for_end_tag
                    );
                },
                Node::Break => self.asm += &format!( " jmp for_{}_end\n\n", self.for_idx_stack.last().unwrap() ),
                Node::Continue => self.asm += &format!( " jmp for_{}\n\n", self.for_idx_stack.last().unwrap() ),
                Node::Definition( name, value ) => self.compile_assignment( name, value ),
                Node::Assignment( name, value ) => self.compile_assignment( name, value ),
                Node::Scope( inner ) => self.compile_scope( *inner ),
                Node::Expression( expression ) => self.compile_expression( expression ),
            }
        }
    }


    fn compile_expression( &mut self, expression: &'ast Expression ) {
        match expression {
            Expression::Literal( Literal::Int { value } ) => self.asm += &format!( " mov rdi, {}\n", value ),
            Expression::Literal( Literal::Char { value } ) => self.asm += &format!( " mov rdi, {}\n", value ),
            Expression::Literal( Literal::Bool { value } ) => self.asm += &format!( " mov rdi, {}\n", value ),
            Expression::Literal( Literal::Str( string ) ) => {
                let string_label_idx = self.string_label_idx( string );
                let string_label = &self.strings[ string_label_idx ];

                self.asm += &format!(
                    " mov rsi, {}\
                    \n mov rdx, {}\n",
                    string_label.label,
                    string_label.len_label
                );
            },
            Expression::Binary { .. } => self.compile_expression_factor( expression, Register::RDI ),
            Expression::Identifier( src_name, _ ) => {
                let src_variable = self.resolve( src_name );
                let src_variable_typ = src_variable.typ;
                let src_variable_offset = src_variable.offset;

                match src_variable_typ {
                    Type::Int => self.asm += &format!( " mov rdi, [rbp + {}]\n", src_variable_offset ),
                    Type::Char | Type::Bool => self.asm += &format!( " movzx rdi, byte [rbp + {}]\n", src_variable_offset ),
                    Type::Str =>
                        self.asm += &format!(
                            " mov rsi, [rbp + {}]\
                            \n mov rdx, [rbp + {}]\n",
                            src_variable_offset,
                            src_variable_offset + 8
                        ),
                }
            },
        }
    }

    // IDEA make this return the place of where to find the result of the operation
    fn compile_expression_factor( &mut self, factor: &'ast Expression, dst: Register ) {
        match factor {
            Expression::Literal( Literal::Int { value } ) => self.asm += &format!( " mov {}, {}\n", dst, value ),
            Expression::Literal( Literal::Char { value } ) => self.asm += &format!( " mov {}, {}\n", dst, value ),
            Expression::Literal( Literal::Bool { value } ) => self.asm += &format!( " mov {}, {}\n", dst, value ),
            Expression::Literal( Literal::Str( string ) ) => {
                let string_label_idx = self.string_label_idx( string );
                let string_label = &self.strings[ string_label_idx ];

                self.asm += &format!( " mov {}, {}\n", dst, string_label.len_label );
            },
            // TODO find way to avoiding compiling the move to a support register if the rhs operand is a literal
                // IDEA optimize increments
                // IDEA optimize checking for even values by testing the least significant bit (e.g. test rax, 1)
            Expression::Binary { lhs, op, rhs } => {
                let (lhs_reg, rhs_reg, op_asm) = match op {
                    Operator::Pow | Operator::PowEquals => match &**rhs {
                        Expression::Literal( Literal::Int { value: 2 } ) => (Register::RDI, Register::RSI,
                            " imul rdi, rdi\n"
                        ),
                        _ => (Register::RDI, Register::RSI,
                            " call int_pow\
                            \n mov rdi, rax\n"
                        ),
                    },
                    Operator::Times | Operator::TimesEquals => (Register::RDI, Register::RSI, " imul rdi, rsi\n"),
                    Operator::Divide | Operator::DivideEquals =>
                        (Register::RDI, Register::RSI,
                            " mov rax, rdi\
                            \n xor rdx, rdx\
                            \n idiv rsi\
                            \n mov rdi, rax\n"
                        ),
                    Operator::Remainder | Operator::RemainderEquals =>
                        (Register::RDI, Register::RSI,
                            " mov rax, rdi\
                            \n xor rdx, rdx\
                            \n idiv rsi\
                            \n mov rdi, rdx\n"
                        ),
                    Operator::Plus | Operator::PlusEquals => (Register::RDI, Register::RSI, " add rdi, rsi\n"),
                    Operator::Minus | Operator::MinusEquals => (Register::RDI, Register::RSI, " sub rdi, rsi\n"),
                    Operator::EqualsEquals => // IDEA ottimizzare controllo divisibilita per 2 o qui o durante l'operazione di resto
                        (Register::RDI, Register::RSI,
                            " cmp rdi, rsi\
                            \n mov rdi, false\
                            \n sete dil\n"
                        ),
                    Operator::NotEquals =>
                        (Register::RDI, Register::RSI,
                            " cmp rdi, rsi\
                            \n mov rdi, false\
                            \n setne dil\n"
                        ),
                    Operator::Greater =>
                        (Register::RDI, Register::RSI,
                            " cmp rdi, rsi\
                            \n mov rdi, false\
                            \n setg dil\n"
                        ),
                    Operator::GreaterOrEquals =>
                        (Register::RDI, Register::RSI,
                            " cmp rdi, rsi\
                            \n mov rdi, false\
                            \n setge dil\n"
                        ),
                    Operator::Less =>
                        (Register::RDI, Register::RSI,
                            " cmp rdi, rsi\
                            \n mov rdi, false\
                            \n setl dil\n"
                        ),
                    Operator::LessOrEquals =>
                        (Register::RDI, Register::RSI,
                            " cmp rdi, rsi\
                            \n mov rdi, false\
                            \n setle dil\n"
                        ),

                    Operator::Compare =>
                        (Register::RDI, Register::RSI,
                            " cmp rdi, rsi\
                            \n mov rdi, LESS\
                            \n mov rdx, EQUAL\
                            \n cmove rdi, rdx\
                            \n mov rdx, GREATER\
                            \n cmovg rdi, rdx\n"
                        ),

                    Operator::And => (Register::RDI, Register::RSI, " and rdi, rsi\n"),
                    Operator::Or => (Register::RDI, Register::RSI, " or rdi, rsi\n" ),
                    Operator::Xor => (Register::RDI, Register::RSI, " xor rdi, rsi\n" ),
                };

                match &**rhs {
                    Expression::Binary { .. } => {
                        self.compile_expression_factor( lhs, lhs_reg );
                        self.asm += " push rdi\n\n";
                        self.compile_expression_factor( rhs, rhs_reg );

                        self.asm += &format!(
                            " mov {}, rdi\
                            \n pop {}\n",
                            rhs_reg,
                            lhs_reg
                        );
                    },
                    _ => {
                        self.compile_expression_factor( lhs, lhs_reg );
                        self.compile_expression_factor( rhs, rhs_reg );
                    }
                }

                self.asm += &format!( "{}\n", op_asm );
            },
            Expression::Identifier( src_name, _ ) => {
                let src_variable = self.resolve( src_name );
                let src_variable_typ = src_variable.typ;
                let src_variable_offset = src_variable.offset;

                match src_variable_typ {
                    Type::Int => self.asm += &format!( " mov {}, [rbp + {}]\n", dst, src_variable_offset ),
                    Type::Char | Type::Bool => self.asm += &format!( " movzx {}, byte [rbp + {}]\n", dst, src_variable_offset ),
                    Type::Str => self.asm += &format!( " mov {}, [rbp + {}]\n", dst, src_variable_offset + 8 ),
                }
            },
        }
    }


    fn compile_if( &mut self, iff: &'ast If, if_tag: &String, if_false_tag: &String ) {
        self.asm += &format!( "{}:; {}\n", if_tag, iff.condition );

        match &iff.condition {
            Expression::Literal( Literal::Bool { value } ) =>
                self.asm += &format!(
                    " mov dil, {}\
                    \n cmp dil, true\
                    \n jne {}\n\n",
                    *value as usize,
                    if_false_tag
                ),
            Expression::Literal( _ ) => unreachable!(),
            Expression::Binary { lhs, op, rhs } => {
                match &**rhs {
                    Expression::Binary { .. } => {
                        self.compile_expression_factor( lhs, Register::RDI );
                        self.asm += " push rdi\n\n";
                        self.compile_expression_factor( rhs, Register::RSI );

                        self.asm += " mov rsi, rdi\n pop rdi\n";
                    },
                    _ => {
                        self.compile_expression_factor( lhs, Register::RDI );
                        self.compile_expression_factor( rhs, Register::RSI );
                    }
                }

                match op {
                    Operator::EqualsEquals => self.asm += &format!( " cmp rdi, rsi\n jne {}\n\n", if_false_tag ),
                    Operator::NotEquals => self.asm += &format!( " cmp rdi, rsi\n je {}\n\n", if_false_tag ),
                    Operator::Greater => self.asm += &format!( " cmp rdi, rsi\n jle {}\n\n", if_false_tag ),
                    Operator::GreaterOrEquals => self.asm += &format!( " cmp rdi, rsi\n jl {}\n\n", if_false_tag ),
                    Operator::Less => self.asm += &format!( " cmp rdi, rsi\n jge {}\n\n", if_false_tag ),
                    Operator::LessOrEquals => self.asm += &format!( " cmp rdi, rsi\n jg {}\n\n", if_false_tag ),
                    Operator::And => self.asm += &format!( " and rdi, rsi\n jz {}\n\n", if_false_tag ),
                    Operator::Or => self.asm += &format!( " or rdi, rsi\n jz {}\n\n", if_false_tag ),
                    Operator::Xor => self.asm += &format!( " xor rdi, rsi\n jz {}\n\n", if_false_tag ),

                    Operator::Pow | Operator::PowEquals
                    | Operator::Times | Operator::TimesEquals
                    | Operator::Divide | Operator::DivideEquals
                    | Operator::Remainder | Operator::RemainderEquals
                    | Operator::Plus | Operator::PlusEquals
                    | Operator::Minus | Operator::MinusEquals
                    | Operator::Compare => unreachable!(),
                };

            },
            Expression::Identifier( src_name, _ ) => {
                let src_variable = self.resolve( src_name );
                self.asm += &format!(
                    " mov dil, [rbp + {}]\
                    \n cmp dil, true\
                    \n jne {}\n\n",
                    src_variable.offset,
                    if_false_tag
                )
            },
        }

        self.compile_scope( iff.scope );
    }

    fn compile_for( &mut self, forr: &'ast ForStatement, for_tag: &String, for_false_tag: &String ) {
        if let Some( condition ) = &forr.condition {
            self.asm += &format!( "{}:; {}\n", for_tag, condition );

            match condition {
                Expression::Literal( Literal::Bool { value } ) =>
                    self.asm += &format!(
                        " mov dil, {}\
                        \n cmp dil, true\
                        \n jne {}\n\n",
                        *value as usize,
                        for_false_tag
                    ),
                Expression::Literal( _ ) => unreachable!(),
                Expression::Binary { lhs, op, rhs } => {
                    match &**rhs {
                        Expression::Binary { .. } => {
                            self.compile_expression_factor( lhs, Register::RDI );
                            self.asm += " push rdi\n\n";
                            self.compile_expression_factor( rhs, Register::RSI );

                            self.asm += " mov rsi, rdi\n pop rdi\n";
                        },
                        _ => {
                            self.compile_expression_factor( lhs, Register::RDI );
                            self.compile_expression_factor( rhs, Register::RSI );
                        }
                    }

                    match op {
                        Operator::EqualsEquals => self.asm += &format!( " cmp rdi, rsi\n jne {}\n\n", for_false_tag ),
                        Operator::NotEquals => self.asm += &format!( " cmp rdi, rsi\n je {}\n\n", for_false_tag ),
                        Operator::Greater => self.asm += &format!( " cmp rdi, rsi\n jle {}\n\n", for_false_tag ),
                        Operator::GreaterOrEquals => self.asm += &format!( " cmp rdi, rsi\n jl {}\n\n", for_false_tag ),
                        Operator::Less => self.asm += &format!( " cmp rdi, rsi\n jge {}\n\n", for_false_tag ),
                        Operator::LessOrEquals => self.asm += &format!( " cmp rdi, rsi\n jg {}\n\n", for_false_tag ),
                        Operator::And => self.asm += &format!( " and rdi, rsi\n jz {}\n\n", for_false_tag ),
                        Operator::Or => self.asm += &format!( " or rdi, rsi\n jz {}\n\n", for_false_tag ),
                        Operator::Xor => self.asm += &format!( " xor rdi, rsi\n jz {}\n\n", for_false_tag ),

                        Operator::Pow | Operator::PowEquals
                        | Operator::Times | Operator::TimesEquals
                        | Operator::Divide | Operator::DivideEquals
                        | Operator::Remainder | Operator::RemainderEquals
                        | Operator::Plus | Operator::PlusEquals
                        | Operator::Minus | Operator::MinusEquals
                        | Operator::Compare => unreachable!(),
                    };

                },
                Expression::Identifier( src_name, _ ) => {
                    let src_variable = self.resolve( src_name );
                    self.asm += &format!(
                        " mov dil, [rbp + {}]\
                        \n cmp dil, true\
                        \n jne {}\n\n",
                        src_variable.offset,
                        for_false_tag
                    )
                },
            }
        }

        self.compile_scope( forr.scope );
    }


    fn compile_assignment( &mut self, name: &String, value: &'ast Expression ) {
        let variable = self.resolve( name );
        let variable_typ = variable.typ;
        let variable_offset = variable.offset;
        self.asm += &format!( " ; {} = {}\n", name, value );

        match value {
            Expression::Literal( Literal::Int { value } ) =>
                self.asm += &format!(
                    " mov rdi, {}\
                    \n mov [rbp + {}], rdi\n\n",
                    value, variable_offset
                ),
            Expression::Literal( Literal::Char { value } ) =>
                self.asm += &format!( " mov byte [rbp + {}], {}\n\n", variable_offset, value ),
            Expression::Literal( Literal::Bool { value } ) =>
                self.asm += &format!( " mov byte [rbp + {}], {}\n\n", variable_offset, value ),
            Expression::Literal( Literal::Str( string @ Str { .. } ) ) => {
                let string_label_idx = self.string_label_idx( string );
                let string_label = &self.strings[ string_label_idx ];

                self.asm += &format!(
                    " mov qword [rbp + {}], {}\
                    \n mov qword [rbp + {}], {}\n\n",
                    variable_offset, string_label.label,
                    variable_offset + 8, string_label.len_label
                );
            },
            Expression::Binary { .. } => {
                self.compile_expression( value );

                match variable_typ {
                    Type::Int | Type::Str => self.asm += &format!( " mov [rbp + {}], rdi\n\n", variable_offset ),
                    Type::Char | Type::Bool => self.asm += &format!( " mov [rbp + {}], dil\n\n", variable_offset ),
                }
            }
            Expression::Identifier( src_name, _ ) => {
                let src_variable = self.resolve( src_name );
                let src_variable_typ = src_variable.typ;
                let src_variable_offset = src_variable.offset;

                match src_variable_typ {
                    Type::Int =>
                        self.asm += &format!(
                            " mov rdi, [rbp + {}]\
                            \n mov [rbp + {}], rdi\n\n",
                            src_variable_offset, variable_offset
                        ),
                    Type::Char | Type::Bool =>
                        self.asm += &format!(
                            " movzx rdi, byte [rbp + {}]\
                            \n mov [rbp + {}], rdi\n\n",
                            src_variable_offset, variable_offset
                        ),
                    Type::Str =>
                        self.asm += &format!(
                            " mov rsi, [rbp + {}]\
                            \n mov rdx, [rbp + {}]\
                            \n mov [rbp + {}], rsi\
                            \n mov [rbp + {}], rdx\n\n",
                            src_variable_offset, variable_offset,
                            src_variable_offset + 8, variable_offset + 8
                        ),
                }
            },
        }
    }
}


fn print_usage() {
    println!( r"
Blitzlang compiler, version {}

Usage: blitz [Options] [Run mode] file.blz

Options:
    -h, --help              Display this message

Run mode:
    interpret <file.blz>    Interpret the source code (default if no run mode command is provided)
    compile   <file.blz>    Compile the source code down to a binary executable
    run       <file.blz>    Compile and run the generated binary executable
", env!( "CARGO_PKG_VERSION" ) );
}

// TODO implement SyntaxErrors to report cli mistakes
fn main() -> ExitCode {
    #[allow(unused_mut)]
    let mut args: Vec<String> = env::args().collect();

    // to quickly debug
    // args.push( "compile".to_string() );
    // args.push( "run".to_string() );
    // args.push( "examples/main.blz".to_string() );

    if args.len() < 2 {
        print_usage();
        return ExitCode::SUCCESS;
    }

    let mut interpret_flag = false;
    let mut build_flag = false;
    let mut run_flag = false;

    let mut source_file_path: Option<String> = None;
    let mut args_iter = args.into_iter();
    args_iter.next();
    for arg in args_iter { // skipping the name of this executable
        match arg.as_str() {
            "-h" | "--help" => {
                print_usage();
                return ExitCode::SUCCESS;
            },
            "interpret" => interpret_flag = true,
            "compile" => build_flag = true,
            "run" => run_flag = true,
            _ => match source_file_path {
                None => source_file_path = Some( arg ),
                Some( _ ) => {
                    eprintln!( "{}: too many source file paths provided", SyntaxError::ERROR );
                    return ExitCode::FAILURE;
                },
            },
        }
    }

    if !interpret_flag && !build_flag && !run_flag {
        interpret_flag = true;
    }
    else if interpret_flag && (build_flag || run_flag) {
        eprintln!( "{}: cannot interpret and build/run at the same time", SyntaxError::ERROR );
        return ExitCode::FAILURE;
    }
    else if build_flag && run_flag {
        eprintln!( "{}: build and run commands cannot be used together", SyntaxError::ERROR );
        return ExitCode::FAILURE;
    }

    let source_file_path = match source_file_path {
        Some( path ) => path,
        None => {
            eprintln!( "{}: no source file path provided", SyntaxError::ERROR );
            return ExitCode::FAILURE;
        },
    };

    let source_file = match File::open( &source_file_path ) {
        Ok( file ) => {
            if file.metadata().unwrap().is_dir() {
                eprintln!( "{}: provided file was a directory", SyntaxError::ERROR );
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

            eprintln!( "{}: could not open file '{}'!\nCause: {}", SyntaxError::ERROR, &source_file_path, cause );
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

    println!( "after lexing" );
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
        let mut interpreter = Interpreter { ast: &ast, variables: Vec::new() };
        interpreter.interpret( &source_file_path );
    }
    else {
        let mut compiler = Compiler {
            ast: &ast,
            rodata: String::new(),
            asm: String::new(),
            variables: Vec::new(),
            strings: Vec::new(),
            if_idx: 0,
            for_idx: 0,
            for_idx_stack: Vec::new(),
        };

        if build_flag {
            let _ = compiler.compile( &source_file_path );
        }
        else if run_flag {
            let _ = compiler.run( &source_file_path );
        }
    }
    return ExitCode::SUCCESS;
}
