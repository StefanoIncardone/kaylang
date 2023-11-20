use std::{fmt::Display, path::{PathBuf, Path}, io::{BufReader, ErrorKind, BufRead}, fs::File, num::IntErrorKind};

use crate::errors::*;


// IDEA make the source generic, eg: to be able to compile from strings instead of just files
#[derive( Debug )]
pub(crate) struct Src {
    pub(crate) path: PathBuf,
    pub(crate) src: BufReader<File>,
}

impl Src {
    pub(crate) fn try_from( path: &Path ) -> Result<Self, IoError> {
        return match File::open( path ) {
            Ok( file ) => match file.metadata() {
                Ok( metadata ) => match metadata.is_file() {
                    true => Ok( Self { path: PathBuf::from( path ), src: BufReader::with_capacity( 1, file ) } ),
                    false => Err( IoError {
                        kind: ErrorKind::InvalidInput,
                        msg: "invalid path".into(),
                        cause: "expected a file but got a directory".into(),
                    } ),
                },
                Err( err ) => Err( IoError {
                    kind: err.kind(),
                    msg: format!( "could not read metadata of '{}'", path.display() ).into(),
                    cause: err.to_string().into(),
                } ),
            },
            Err( err ) => Err( IoError {
                kind: err.kind(),
                msg: format!( "could not open '{}'", path.display() ).into(),
                cause: err.to_string().into(),
            } ),
        }
    }
}

pub(crate) trait Len {
    fn len( &self ) -> usize;
}

pub(crate) trait TypeOf {
    fn typ( &self ) -> Type;
}


#[derive( Debug, Clone )]
pub(crate) struct Str {
    pub(crate) text: Vec<u8>,
}

#[derive( Debug, Clone )]
pub(crate) enum Literal {
    Int( isize ), // IDEA have different size integers and default to 32 bits for literals
    Char( u8 ), // only supporting ASCII characters for now
    Bool( bool ),
    Str( Str ),
}

impl Display for Literal {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Int( value )       => write!( f, "{}", value ),
            // TODO create own escaping function
            Self::Char( code )       => write!( f, "'{}'", code.escape_ascii() ),
            Self::Bool( value )      => write!( f, "{}", value ),
            Self::Str( string )      => {
                write!( f, "\"" )?;
                for character in &string.text {
                    write!( f, "{}", character.escape_ascii() )?;
                }
                write!( f, "\"" )
            },
        }
    }
}

impl From<Literal> for isize {
    fn from( literal: Literal) -> Self {
        return match literal {
            Literal::Int( value )  => value,
            Literal::Char( code )  => code.into(),
            Literal::Bool( value ) => value.into(),
            Literal::Str( string ) => string.text.len() as isize,
        }
    }
}

impl Len for Literal {
    fn len( &self ) -> usize {
        return match self {
            Self::Int( value )         => value.to_string().len(),
            Self::Char( _ )            => 1,
            Self::Bool( value )        => value.to_string().len(),
            Self::Str( string )        => string.text.len() + 2,
        }
    }
}

impl TypeOf for Literal {
    fn typ( &self ) -> Type {
        return match self {
            Self::Int { .. }           => Type::Int,
            Self::Char { .. }          => Type::Char,
            Self::Bool { .. }          => Type::Bool,
            Self::Str( _ )             => Type::Str,
        }
    }
}

// TODO implement unsigned integers
#[derive( Debug, PartialEq, Clone, Copy )]
pub(crate) enum Type {
    Int,
    Char,
    Bool,
    Str,
}

impl Display for Type {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Int  => write!( f, "int" ),
            Self::Char => write!( f, "char" ),
            Self::Bool => write!( f, "bool" ),
            Self::Str  => write!( f, "str" ),
        }
    }
}

impl Len for Type {
    fn len( &self ) -> usize {
        match self {
            Self::Int  => core::mem::size_of::<isize>(),
            Self::Char => core::mem::size_of::<u8>(),
            Self::Bool => core::mem::size_of::<bool>(),
            Self::Str  => core::mem::size_of::<*const u8>() + core::mem::size_of::<usize>(),
        }
    }
}

impl Type {
    pub(crate) fn default( &self ) -> Literal {
        match self {
            Self::Bool => Literal::Bool( false ),
            Self::Char => Literal::Char( 0 ),
            Self::Int  => Literal::Int( 0 ),
            Self::Str  => Literal::Str( Str { text: Vec::new() } ),
        }
    }
}


#[derive( Debug, Clone, Copy, PartialEq )]
pub(crate) enum Operator {
    // unary operators
    Not,

    // binary operators
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

    LeftShift,
    LeftShiftEquals,
    RightShift,
    RightShiftEquals,

    BitAnd,
    BitAndEquals,
    BitXor,
    BitXorEquals,
    BitOr,
    BitOrEquals,

    Compare,

    EqualsEquals,
    NotEquals,
    Greater,
    GreaterOrEquals,
    Less,
    LessOrEquals,

    And,
    AndEquals,
    // NOTE temporarily disabling xor operators until we have a flat/chained expression type
    // such that    [operand, op, operand, op, operand, op, operand]
    // instead of   [[[operand, op, operand] op, operand], op, operand]
    // Xor,
    // XorEquals,
    Or,
    OrEquals,
}

impl Display for Operator {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Not              => write!( f, "!" ),

            Self::Pow              => write!( f, "**" ),
            Self::PowEquals        => write!( f, "**=" ),
            Self::Times            => write!( f, "*" ),
            Self::TimesEquals      => write!( f, "*=" ),
            Self::Divide           => write!( f, "/" ),
            Self::DivideEquals     => write!( f, "/=" ),
            Self::Remainder        => write!( f, "%" ),
            Self::RemainderEquals  => write!( f, "%=" ),
            Self::Plus             => write!( f, "+" ),
            Self::PlusEquals       => write!( f, "+=" ),
            Self::Minus            => write!( f, "-" ),
            Self::MinusEquals      => write!( f, "-=" ),

            Self::And              => write!( f, "&&" ),
            Self::AndEquals        => write!( f, "&&=" ),
            Self::BitAnd           => write!( f, "&" ),
            Self::BitAndEquals     => write!( f, "&=" ),
            Self::Or               => write!( f, "||" ),
            Self::OrEquals         => write!( f, "||=" ),
            Self::BitOr            => write!( f, "|" ),
            Self::BitOrEquals      => write!( f, "|=" ),
            // Self::Xor              => write!( f, "^^" ),
            // Self::XorEquals        => write!( f, "^^=" ),
            Self::BitXor           => write!( f, "^" ),
            Self::BitXorEquals     => write!( f, "^=" ),
            Self::LeftShift        => write!( f, "<<" ),
            Self::LeftShiftEquals  => write!( f, "<<=" ),
            Self::RightShift       => write!( f, ">>" ),
            Self::RightShiftEquals => write!( f, ">>=" ),

            Self::EqualsEquals     => write!( f, "==" ),
            Self::NotEquals        => write!( f, "!=" ),
            Self::Greater          => write!( f, ">" ),
            Self::GreaterOrEquals  => write!( f, ">=" ),
            Self::Less             => write!( f, "<" ),
            Self::LessOrEquals     => write!( f, "<=" ),
            Self::Compare          => write!( f, "<=>" ),
        }
    }
}

impl Len for Operator {
    fn len( &self ) -> usize {
        return match self {
            Self::Not              => 1,

            Self::Pow              => 2,
            Self::PowEquals        => 3,
            Self::Times            => 1,
            Self::TimesEquals      => 2,
            Self::Divide           => 1,
            Self::DivideEquals     => 2,
            Self::Remainder        => 1,
            Self::RemainderEquals  => 2,
            Self::Plus             => 1,
            Self::PlusEquals       => 2,
            Self::Minus            => 1,
            Self::MinusEquals      => 2,

            Self::And              => 2,
            Self::AndEquals        => 3,
            Self::BitAnd           => 1,
            Self::BitAndEquals     => 2,
            Self::Or               => 2,
            Self::OrEquals         => 3,
            Self::BitOr            => 1,
            Self::BitOrEquals      => 2,
            // Self::Xor              => 2,
            // Self::XorEquals        => 3,
            Self::BitXor           => 1,
            Self::BitXorEquals     => 2,
            Self::LeftShift        => 2,
            Self::LeftShiftEquals  => 3,
            Self::RightShift       => 2,
            Self::RightShiftEquals => 3,

            Self::EqualsEquals     => 2,
            Self::NotEquals        => 2,
            Self::Greater          => 1,
            Self::GreaterOrEquals  => 2,
            Self::Less             => 1,
            Self::LessOrEquals     => 2,
            Self::Compare          => 3,
        }
    }
}

impl TypeOf for Operator {
    fn typ( &self ) -> Type {
        return match self {
            Self::Compare
            | Self::Pow | Self::PowEquals
            | Self::Times | Self::TimesEquals
            | Self::Divide | Self::DivideEquals
            | Self::Remainder | Self::RemainderEquals
            | Self::Plus | Self::PlusEquals
            | Self::Minus | Self::MinusEquals
            | Self::BitAnd | Self::BitAndEquals
            | Self::BitOr | Self::BitOrEquals
            | Self::BitXor | Self::BitXorEquals
            | Self::LeftShift | Self::LeftShiftEquals
            | Self::RightShift | Self::RightShiftEquals => Type::Int,

            Self::EqualsEquals | Self::NotEquals
            | Self::Greater | Self::GreaterOrEquals
            | Self::Less | Self::LessOrEquals
            | Self::Not
            | Self::And | Self::AndEquals
            | Self::Or | Self::OrEquals
            /* | Self::Xor | Self::XorEquals */ => Type::Bool,
        }
    }
}

#[derive( Debug, Clone, Copy )]
pub(crate) enum Mutability {
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
pub(crate) enum BracketKind {
    OpenRound,
    CloseRound,
    OpenCurly,
    CloseCurly,
}

impl Display for BracketKind {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::OpenRound  => write!( f, "(" ),
            Self::CloseRound => write!( f, ")" ),
            Self::OpenCurly  => write!( f, "{{" ),
            Self::CloseCurly => write!( f, "}}" ),
        }
    }
}

impl Len for BracketKind {
    fn len( &self ) -> usize {
        return match self {
            Self::OpenRound  => 1,
            Self::CloseRound => 1,
            Self::OpenCurly  => 1,
            Self::CloseCurly => 1,
        }
    }
}

#[derive( Debug )]
struct Bracket {
    line_byte_start: usize,
    line_number: usize,
    col: usize,
    kind: BracketKind,
}

#[derive( Debug, Clone )]
pub(crate) enum TokenKind {
    Comment( String ),
    Unexpected( String ),

    Bracket( BracketKind ),
    Colon,
    SemiColon,
    Equals,
    Op( Operator ),

    Literal( Literal ),
    True,
    False,
    Identifier( String ),
    Definition( Mutability ),

    // Keywords
    Print, // temporary way of printing values
    PrintLn, // temporary way of printing values followed by a newline
    Do,
    If,
    Else,
    Loop,
    Break,
    Continue,

    // Special
    Empty,
}

impl Display for TokenKind {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Comment( text )    => write!( f, "{}", text ),
            Self::Unexpected( text ) => write!( f, "{}", text ),

            Self::Bracket( bracket ) => write!( f, "{}", bracket ),
            Self::Equals             => write!( f, "=" ),
            Self::Colon              => write!( f, ":" ),
            Self::SemiColon          => write!( f, ";" ),

            Self::Literal( literal ) => write!( f, "{}", literal ),
            Self::Identifier( name ) => write!( f, "{}", name ),
            Self::Definition( kind ) => write!( f, "{}", kind ),

            Self::Op( op )           => write!( f, "{}", op ),

            Self::Print              => write!( f, "print" ),
            Self::PrintLn            => write!( f, "println" ),
            Self::True               => write!( f, "true" ),
            Self::False              => write!( f, "false" ),
            Self::Do                 => write!( f, "do" ),
            Self::If                 => write!( f, "if" ),
            Self::Else               => write!( f, "else" ),
            Self::Loop               => write!( f, "loop" ),
            Self::Break              => write!( f, "break" ),
            Self::Continue           => write!( f, "continue" ),

            Self::Empty              => write!( f, "" ),
        }
    }
}

impl Len for TokenKind {
    fn len( &self ) -> usize {
        return match self {
            Self::Comment( text )    => text.len(),
            Self::Unexpected( text ) => text.len(),

            Self::Bracket( bracket ) => bracket.len(),
            Self::Colon              => 1,
            Self::SemiColon          => 1,
            Self::Equals             => 1,
            Self::Op( op )           => op.len(),

            Self::Literal( typ )     => typ.len(),
            Self::True               => 4,
            Self::False              => 5,
            Self::Identifier( name ) => name.len(),
            Self::Definition( kind ) => kind.len(),

            Self::Print              => 5,
            Self::PrintLn            => 7,
            Self::Do                 => 2,
            Self::If                 => 2,
            Self::Else               => 4,
            Self::Loop               => 4,
            Self::Break              => 5,
            Self::Continue           => 8,

            Self::Empty              => 0,
        }
    }
}

#[derive( Debug, Clone )]
pub(crate) struct Token {
    pub(crate) col: usize,
    pub(crate) kind: TokenKind,
}

#[derive( Debug, Clone, Copy )]
pub(crate) struct Line {
    pub(crate) number: usize,
    pub(crate) byte_start: usize,
    pub(crate) token_start_idx: usize,
    pub(crate) token_end_idx: usize,
}

#[derive( Debug )]
pub(crate) struct Lexer {
    pub(crate) src: Src,

    line: usize,
    line_byte_start: usize,
    col: usize,
    token_start_col: usize,

    pub(crate) lines: Vec<Line>,
    pub(crate) tokens: Vec<Token>,

    line_bytes: Vec<u8>,
    token_text: String,

    brackets: Vec<Bracket>,
    errors: Vec<SyntaxError>,
}

impl TryFrom<Src> for Lexer {
    type Error = SyntaxErrors;


    fn try_from( src: Src ) -> Result<Self, Self::Error> {
        let mut this = Self {
            src,
            line: 0,
            line_byte_start: 0,
            col: 0,
            token_start_col: 1,
            lines: Vec::new(),
            tokens: Vec::new(),
            line_bytes: Vec::new(),
            token_text: String::new(),
            brackets: Vec::new(),
            errors: Vec::new(),
        };

        let mut line = Line {
            number: 1,
            byte_start: 0,
            token_start_idx: 0,
            token_end_idx: 0,
        };

        loop {
            this.line_bytes.clear();
            match this.src.src.read_until( b'\n', &mut this.line_bytes ) {
                Ok( chars_read ) => {
                    this.line += 1;
                    this.col = 0;
                    this.token_start_col = 1;

                    let mut line_len = this.line_bytes.len();
                    while line_len > 0 && this.line_bytes[ line_len - 1 ].is_ascii_whitespace() {
                        line_len -= 1;
                    }

                    if line_len == 0 {
                        this.tokens.push( Token { col: 1, kind: TokenKind::Empty } );
                    }
                    else {
                        this.line_bytes.truncate( line_len );
                        loop {
                            let token = match this.tokeninze_next() {
                                Ok( None ) => break,
                                Ok( Some( kind ) ) => match kind {
                                    TokenKind::Empty => continue,
                                    _ => Token { col: this.token_start_col, kind },
                                },
                                Err( err ) => {
                                    this.errors.push( err );
                                    Token {
                                        col: this.token_start_col,
                                        kind: TokenKind::Unexpected( this.token_text() )
                                    }
                                }
                            };

                            this.tokens.push( token );
                        }
                    }

                    line.token_end_idx = this.tokens.len() - 1;
                    this.lines.push( line );

                    this.line_byte_start += chars_read;
                    line.byte_start += chars_read;
                    line.number += 1;
                    line.token_start_idx = this.tokens.len();

                    let reached_eof = chars_read == line_len;
                    if reached_eof {
                        break;
                    }
                },
                Err( err ) => panic!( "Error: {}", err ),
            }
        }

        for bracket in &this.brackets {
            // there can only be open brackets at this point
            this.errors.push( SyntaxError {
                line_byte_start: bracket.line_byte_start,
                line: bracket.line_number,
                col: bracket.col,
                len: bracket.kind.len(),
                msg: "stray bracket".into(),
                help_msg: "was not closed".into(),
            } );
        }

        return match this.errors.is_empty() {
            true => Ok( this ),
            false => Err( SyntaxErrors::new( this.errors, &mut this.src ) ),
        }
    }
}

impl Lexer {
    fn gather_token_text( &mut self ) {
        self.token_text = self.token_text();
    }

    // TODO move String related function to the Str struct
    // TODO create own from_utf8 function
    fn token_text( &self ) -> String {
        return String::from_utf8_lossy( &self.line_bytes[ self.token_start_col - 1..self.col ] ).into();
    }

    // FIX properly handle non ASCII characters
        // TODO add an absolute column for the bytes in the line
        // IDEA only allow utf-8 characters in strings, characters and comments
    fn next( &mut self ) -> Result<Option<u8>, SyntaxError> {
        if self.col >= self.line_bytes.len() {
            return Ok( None );
        }

        let next = self.line_bytes[ self.col ];
        self.col += 1;
        return match next {
            ..=b'\x7F' => Ok( Some( next ) ),
            _ => Err( SyntaxError {
                line_byte_start: self.line_byte_start,
                line: self.line,
                col: self.col,
                len: 1,
                msg: "unrecognized character".into(),
                help_msg: "not a valid ASCII character".into(),
            } ),
        }
    }

    fn peek_next( &self ) -> Result<Option<&u8>, SyntaxError> {
        if self.col >= self.line_bytes.len() {
            return Ok( None );
        }

        let next = &self.line_bytes[ self.col ];
        return match next {
            ..=b'\x7F' => Ok( Some( next ) ),
            _ => Err( SyntaxError {
                line_byte_start: self.line_byte_start,
                line: self.line,
                col: self.col,
                len: 1,
                msg: "unrecognized character".into(),
                help_msg: "not a valid ASCII character".into(),
            } ),
        }
    }

    fn next_char( &mut self ) -> Result<u8, SyntaxError> {
        return match self.next()? {
            Some( b'\n' ) | None => Err( SyntaxError {
                line_byte_start: self.line_byte_start,
                line: self.line,
                col: self.token_start_col,
                len: self.col - self.token_start_col + 1,
                msg: "invalid character literal".into(),
                help_msg: "missing closing single quote".into(),
            } ),
            Some( next ) => Ok( next ),
        }
    }

    fn next_str_char( &mut self ) -> Result<u8, SyntaxError> {
        return match self.next()? {
            Some( b'\n' ) | None => Err( SyntaxError {
                line_byte_start: self.line_byte_start,
                line: self.line,
                col: self.token_start_col,
                len: self.col - self.token_start_col + 1,
                msg: "invalid string literal".into(),
                help_msg: "missing closing double quote".into(),
            } ),
            Some( next ) => Ok( next ),
        }
    }

    fn tokeninze_next( &mut self ) -> Result<Option<TokenKind>, SyntaxError> {
        self.token_text.clear();
        let next = match self.next()? {
            Some( ch ) => ch,
            None => return Ok( None ),
        };

        // registering the token start column after getting the next character to maintain 1 indexing token columns
        self.token_start_col = self.col;
        return match next {
            // ignore whitespace
            b'\t' | b'\x0C' | b'\r' | b' ' => Ok( Some( TokenKind::Empty ) ),
            b'a'..=b'z' | b'A'..=b'Z' | b'_'  => {
                let mut contains_non_ascii = false;
                loop {
                    match self.next() {
                        Ok( Some( b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_' ) ) => {},
                        Ok( Some( _ ) ) => {
                            self.col -= 1;
                            break;
                        },
                        Ok( None ) => break,
                        Err( _ ) => contains_non_ascii = true,
                    }
                }

                if contains_non_ascii {
                    Err( SyntaxError {
                        line_byte_start: self.line_byte_start,
                        line: self.line,
                        col: self.token_start_col,
                        len: self.col - self.token_start_col + 1,
                        msg: "invalid identifier".into(),
                        help_msg: "contains non-ASCII characters".into(),
                    } )
                }
                else {
                    self.gather_token_text();
                    let identifier = match self.token_text.as_str() {
                        "let"      => TokenKind::Definition( Mutability::Let ),
                        "var"      => TokenKind::Definition( Mutability::Var ),
                        "print"    => TokenKind::Print,
                        "println"  => TokenKind::PrintLn,
                        "true"     => TokenKind::True,
                        "false"    => TokenKind::False,
                        "do"       => TokenKind::Do,
                        "if"       => TokenKind::If,
                        "else"     => TokenKind::Else,
                        "loop"     => TokenKind::Loop,
                        "break"    => TokenKind::Break,
                        "continue" => TokenKind::Continue,
                        _          => TokenKind::Identifier( self.token_text.clone() ),
                    };

                    Ok( Some( identifier ) )
                }
            },
            b'0'..=b'9' => {
                let mut contains_non_ascii = false;
                loop {
                    match self.next() {
                        Ok( Some( b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_') ) => {},
                        Ok( Some( _ ) ) => {
                            self.col -= 1;
                            break;
                        },
                        Ok( None ) => break,
                        Err( _ ) => contains_non_ascii = true,
                    }
                }

                self.gather_token_text();
                // TODO create own number parsing function
                match self.token_text.parse() {
                    Ok( value ) => Ok( Some( TokenKind::Literal( Literal::Int( value ) ) ) ),
                    Err( err ) => match err.kind() {
                        IntErrorKind::InvalidDigit =>
                            if contains_non_ascii {
                                Err( SyntaxError {
                                    line_byte_start: self.line_byte_start,
                                    line: self.line,
                                    col: self.token_start_col,
                                    len: self.token_text.len(),
                                    msg: "invalid number literal".into(),
                                    help_msg: "contains non-ASCII characters".into(),
                                } )
                            }
                            else {
                                Err( SyntaxError {
                                    line_byte_start: self.line_byte_start,
                                    line: self.line,
                                    col: self.token_start_col,
                                    len: self.token_text.len(),
                                    msg: "invalid number literal".into(),
                                    help_msg: "contains non-digit characters".into(),
                                } )
                            },
                        IntErrorKind::PosOverflow => Err( SyntaxError {
                            line_byte_start: self.line_byte_start,
                            line: self.line,
                            col: self.token_start_col,
                            len: self.token_text.len(),
                            msg: "invalid number literal".into(),
                            help_msg: format!( "overflows a {} bit signed integer (over {})", isize::BITS, isize::MAX ).into(),
                        } ),
                        IntErrorKind::NegOverflow => Err( SyntaxError {
                            line_byte_start: self.line_byte_start,
                            line: self.line,
                            col: self.token_start_col,
                            len: self.token_text.len(),
                            msg: "invalid number literal".into(),
                            help_msg: format!( "underflows a {} bit signed integer (under {})", isize::BITS, isize::MIN ).into(),
                        } ),
                        IntErrorKind::Empty | std::num::IntErrorKind::Zero => unreachable!(),
                        _ => Err( SyntaxError {
                            line_byte_start: self.line_byte_start,
                            line: self.line,
                            col: self.token_start_col,
                            len: self.token_text.len(),
                            msg: "invalid number literal".into(),
                            help_msg: err.to_string().into(),
                        } ),
                    },
                }
            },
            b'#' => {
                // consume the rest of the tokens in the current line
                self.col = self.line_bytes.len();
                let comment = self.token_text();
                Ok( Some( TokenKind::Comment( comment ) ) )
            },
            b'"' => {
                let mut errors: Vec<SyntaxError> = Vec::new();

                loop {
                    let next = match self.next_str_char()? {
                        b'\\' => match self.next_str_char()? {
                            b'\\' => Ok( '\\' ),
                            b'\'' => Ok( '\'' ),
                            b'"' => Ok( '"' ),
                            b'n' => Ok( '\n' ),
                            b'r' => Ok( '\r' ),
                            b't' => Ok( '\t' ),
                            b'0' => Ok( '\0' ),
                            _ => Err( SyntaxError {
                                line_byte_start: self.line_byte_start,
                                line: self.line,
                                col: self.col,
                                len: 1,
                                msg: "invalid string character".into(),
                                help_msg: "unrecognized escape character".into(),
                            } ),
                        },
                        b'\x00'..=b'\x1F' | b'\x7F' => Err( SyntaxError {
                            line_byte_start: self.line_byte_start,
                            line: self.line,
                            col: self.col,
                            len: 1,
                            msg: "invalid string literal".into(),
                            help_msg: "cannot be a control character".into(),
                        } ),
                        b'"' => break,
                        other => Ok( other as char ),
                    };

                    match next {
                        Ok( next_char ) => self.token_text.push( next_char ),
                        Err( err ) => errors.push( err ),
                    }
                }

                // after here there cannot be unclosed strings
                if errors.is_empty() {
                    Ok( Some( TokenKind::Literal( Literal::Str( Str { text: self.token_text.clone().into_bytes() } ) ) ) )
                }
                else {
                    // FIX add proper multiple error handling
                    let last_error = errors.pop().unwrap();
                    self.errors.extend( errors );
                    Err( last_error )
                }
            },
            b'\'' => {
                let code = match self.next_char()? {
                    b'\\' => match self.next_char()? {
                        b'\\' => Ok( b'\\' ),
                        b'\'' => Ok( b'\'' ),
                        b'"' => Ok( b'"' ),
                        b'n' => Ok( b'\n' ),
                        b'r' => Ok( b'\r' ),
                        b't' => Ok( b'\t' ),
                        b'0' => Ok( b'\0' ),
                        _ => Err( SyntaxError {
                            line_byte_start: self.line_byte_start,
                            line: self.line,
                            col: self.col,
                            len: 1,
                            msg: "invalid character literal".into(),
                            help_msg: "unrecognized escape character".into(),
                        } ),
                    },
                    b'\x00'..=b'\x1F' | b'\x7F' => Err( SyntaxError {
                        line_byte_start: self.line_byte_start,
                        line: self.line,
                        col: self.col,
                        len: 1,
                        msg: "invalid character literal".into(),
                        help_msg: "cannot be a control character".into(),
                    } ),
                    b'\'' => return Err( SyntaxError {
                        line_byte_start: self.line_byte_start,
                        line: self.line,
                        col: self.token_start_col,
                        len: 2,
                        msg: "invalid character literal".into(),
                        help_msg: "must not be empty".into(),
                    } ),
                    ch => Ok( ch ),
                };

                match self.peek_next()? {
                    Some( b'\'' ) => {
                        self.col += 1;
                        Ok( Some( TokenKind::Literal( Literal::Char( code? ) ) ) )
                    },
                    Some( _ ) | None => Err( SyntaxError {
                        line_byte_start: self.line_byte_start,
                        line: self.line,
                        col: self.token_start_col,
                        len: self.col - self.token_start_col + 1,
                        msg: "invalid character literal".into(),
                        help_msg: "missing closing single quote".into(),
                    } )
                }
            },
            b'(' => {
                let kind = BracketKind::OpenRound;
                self.brackets.push( Bracket {
                    line_byte_start: self.line_byte_start,
                    line_number: self.line,
                    col: self.token_start_col,
                    kind,
                } );
                Ok( Some( TokenKind::Bracket( kind ) ) )
            },
            b')' => match self.brackets.pop() {
                Some( bracket ) => match bracket.kind {
                    BracketKind::OpenRound | BracketKind::CloseCurly | BracketKind::CloseRound =>
                        Ok( Some( TokenKind::Bracket( BracketKind::CloseRound ) ) ),
                    BracketKind::OpenCurly => Err( SyntaxError {
                        line_byte_start: self.line_byte_start,
                        line: self.line,
                        col: self.token_start_col,
                        len: 1,
                        msg: "stray bracket".into(),
                        help_msg: "closes the wrong bracket".into(),
                    } ),
                },
                None => Err( SyntaxError {
                    line_byte_start: self.line_byte_start,
                    line: self.line,
                    col: self.token_start_col,
                    len: 1,
                    msg: "stray bracket".into(),
                    help_msg: "was not opened before".into(),
                } ),
            },
            b'{' => {
                let kind = BracketKind::OpenCurly;
                self.brackets.push( Bracket {
                    line_byte_start: self.line_byte_start,
                    line_number: self.line,
                    col: self.token_start_col,
                    kind,
                } );
                Ok( Some( TokenKind::Bracket( kind ) ) )
            },
            b'}' => match self.brackets.pop() {
                Some( bracket ) => match bracket.kind {
                    BracketKind::OpenCurly | BracketKind::CloseCurly | BracketKind::CloseRound =>
                        Ok( Some( TokenKind::Bracket( BracketKind::CloseCurly ) ) ),
                    BracketKind::OpenRound => Err( SyntaxError {
                        line_byte_start: self.line_byte_start,
                        line: self.line,
                        col: self.token_start_col,
                        len: 1,
                        msg: "stray bracket".into(),
                        help_msg: "closes the wrong bracket".into(),
                    } ),
                },
                None => Err( SyntaxError {
                    line_byte_start: self.line_byte_start,
                    line: self.line,
                    col: self.token_start_col,
                    len: 1,
                    msg: "stray bracket".into(),
                    help_msg: "was not opened before".into(),
                } ),
            },
            b':' => Ok( Some( TokenKind::Colon ) ),
            b';' => Ok( Some( TokenKind::SemiColon ) ),
            b'!' => match self.peek_next()? {
                Some( b'=' ) => {
                    self.col += 1;
                    Ok( Some( TokenKind::Op( Operator::NotEquals ) ) )
                },
                _ => Ok( Some( TokenKind::Op( Operator::Not ) ) ),
            },
            b'*' => match self.peek_next()? {
                Some( b'*' ) => {
                    self.col += 1;
                    match self.peek_next()? {
                        Some( b'=' ) => {
                            self.col += 1;
                            Ok( Some( TokenKind::Op( Operator::PowEquals ) ) )
                        }
                        _ => Ok( Some( TokenKind::Op( Operator::Pow ) ) ),
                    }
                },
                Some( b'=' ) => {
                    self.col += 1;
                    Ok( Some( TokenKind::Op( Operator::TimesEquals ) ) )
                },
                _ => Ok( Some( TokenKind::Op( Operator::Times ) ) ),
            },
            b'/' => match self.peek_next()? {
                Some( b'=' ) => {
                    self.col += 1;
                    Ok( Some( TokenKind::Op( Operator::DivideEquals ) ) )
                },
                _ => Ok( Some( TokenKind::Op( Operator::Divide ) ) ),
            },
            b'%' => match self.peek_next()? {
                Some( b'=' ) => {
                    self.col += 1;
                    Ok( Some( TokenKind::Op( Operator::RemainderEquals ) ) )
                },
                _ => Ok( Some( TokenKind::Op( Operator::Remainder ) ) ),
            },
            b'+' => match self.peek_next()? {
                Some( b'=' ) => {
                    self.col += 1;
                    Ok( Some( TokenKind::Op( Operator::PlusEquals ) ) )
                },
                _ => Ok( Some( TokenKind::Op( Operator::Plus ) ) )
            },
            b'-' => match self.peek_next()? {
                Some( b'=' ) => {
                    self.col += 1;
                    Ok( Some( TokenKind::Op( Operator::MinusEquals ) ) )
                },
                _ => Ok( Some( TokenKind::Op( Operator::Minus ) ) ),
            },
            b'&' => match self.peek_next()? {
                Some( b'&' ) => {
                    self.col += 1;
                    match self.peek_next()? {
                        Some( b'=' ) => {
                            self.col += 1;
                            Ok( Some( TokenKind::Op( Operator::AndEquals ) ) )
                        },
                        _ => Ok( Some( TokenKind::Op( Operator::And ) ) ),
                    }
                },
                Some( b'=' ) => {
                    self.col += 1;
                    Ok( Some( TokenKind::Op( Operator::BitAndEquals ) ) )
                },
                _ => Ok( Some( TokenKind::Op( Operator::BitAnd ) ) ),
            },
            b'^' => match self.peek_next()? {
                // Some( b'^' ) => {
                //     self.col += 1;
                //     match self.peek_next()? {
                //         Some( b'=' ) => {
                //             self.col += 1;
                //             Ok( Some( TokenKind::Op( Operator::XorEquals ) ) )
                //         },
                //         _ => Ok( Some( TokenKind::Op( Operator::Xor ) ) ),
                //     }
                // },
                Some( b'=' ) => {
                    self.col += 1;
                    Ok( Some( TokenKind::Op( Operator::BitXorEquals ) ) )
                },
                _ => Ok( Some( TokenKind::Op( Operator::BitXor ) ) ),
            },
            b'|' => match self.peek_next()? {
                Some( b'|' ) => {
                    self.col += 1;
                    match self.peek_next()? {
                        Some( b'=' ) => {
                            self.col += 1;
                            Ok( Some( TokenKind::Op( Operator::OrEquals ) ) )
                        },
                        _ => Ok( Some( TokenKind::Op( Operator::Or ) ) ),
                    }
                },
                Some( b'=' ) => {
                    self.col += 1;
                    Ok( Some( TokenKind::Op( Operator::BitOrEquals ) ) )
                },
                _ => Ok( Some( TokenKind::Op( Operator::BitOr ) ) ),
            },
            b'=' => match self.peek_next()? {
                Some( b'=' ) => {
                    self.col += 1;
                    Ok( Some( TokenKind::Op( Operator::EqualsEquals ) ) )
                },
                _ => Ok( Some( TokenKind::Equals ) ),
            },
            b'>' => match self.peek_next()? {
                Some( b'>' ) => {
                    self.col += 1;
                    match self.peek_next()? {
                        Some( b'=' ) => {
                            self.col += 1;
                            Ok( Some( TokenKind::Op( Operator::RightShiftEquals ) ) )
                        },
                        _ => Ok( Some( TokenKind::Op( Operator::RightShift ) ) ),
                    }
                },
                Some( b'=' ) => {
                    self.col += 1;
                    Ok( Some( TokenKind::Op( Operator::GreaterOrEquals ) ) )
                },
                _ => Ok( Some( TokenKind::Op( Operator::Greater ) ) ),
            },
            b'<' => match self.peek_next()? {
                Some( b'<' ) => {
                    self.col += 1;
                    match self.peek_next()? {
                        Some( b'=' ) => {
                            self.col += 1;
                            Ok( Some( TokenKind::Op( Operator::LeftShiftEquals ) ) )
                        },
                        _ => Ok( Some( TokenKind::Op( Operator::LeftShift ) ) ),
                    }
                },
                Some( b'=' ) => {
                    self.col += 1;
                    match self.peek_next()? {
                        Some( b'>' ) => {
                            self.col += 1;
                            Ok( Some( TokenKind::Op( Operator::Compare ) ) )
                        },
                        _ => Ok( Some( TokenKind::Op( Operator::LessOrEquals ) ) ),
                    }
                },
                _ => Ok( Some( TokenKind::Op( Operator::Less ) ) ),
            },
            b'\n' => unreachable!( "line text should have been trimmed already" ),
            _ => Err( SyntaxError {
                line_byte_start: self.line_byte_start,
                line: self.line,
                col: self.token_start_col,
                len: 1,
                msg: "unexpected character".into(),
                help_msg: "unrecognized".into(),
            } ),
        }
    }
}


#[derive( Debug, Clone, Copy )]
pub(crate) struct TokenPosition<'lexer> {
    pub(crate) line: &'lexer Line,
    pub(crate) token: &'lexer Token,
}

#[derive( Debug )]
pub(crate) struct TokenCursor<'lexer> {
    pub(crate) line: usize,
    pub(crate) lines: &'lexer [Line],

    pub(crate) token: usize,
    pub(crate) tokens: &'lexer [Token],
}

// TODO implement methods that return only tokens or lines since lines are only
// really needed when reporting errors
impl<'lexer> TokenCursor<'lexer> {
    // TODO remove this method and make the next/previous methods return their current position and
    // then move (i.e. like a normal iterator)
        // NOTE its goint to be required to pass a position object around instead of calling current
        // when needed
    pub(crate) fn current( &mut self ) -> Option<TokenPosition<'lexer>> {
        if self.token >= self.tokens.len() || self.line >= self.lines.len() {
            return None
        }

        return Some( TokenPosition {
            line: &self.lines[ self.line ],
            token: &self.tokens[ self.token ]
        } ).or_next( self );
    }

    pub(crate) fn next( &mut self ) -> Option<TokenPosition<'lexer>> {
        if self.token >= self.tokens.len() - 1 {
            self.token += 1;
            return None;
        }

        self.token += 1;
        let token = &self.tokens[ self.token ];

        let mut line = &self.lines[ self.line ];
        if self.token > line.token_end_idx {
            self.line += 1;
            line = &self.lines[ self.line ];
        }

        return Some( TokenPosition { line, token } ).or_next( self );
    }

    pub(crate) fn previous( &mut self ) -> Option<TokenPosition<'lexer>> {
        if self.token == 0 {
            return None;
        }

        self.token -= 1;
        let token = &self.tokens[ self.token ];

        let mut line = &self.lines[ self.line ];
        if self.token < line.token_start_idx {
            self.line -= 1;
            line = &self.lines[ self.line ];
        }

        return Some( TokenPosition { line, token } ).or_previous( self );
    }

    pub(crate) fn peek_next( &mut self ) -> Option<TokenPosition<'lexer>> {
        let (starting_line, starting_token) = (self.line, self.token);
        let next = self.next();
        (self.line, self.token) = (starting_line, starting_token);
        return next;
    }

    pub(crate) fn peek_previous( &mut self ) -> Option<TokenPosition<'lexer>> {
        let (starting_line, starting_token) = (self.line, self.token);
        let previous = self.previous();
        (self.line, self.token) = (starting_line, starting_token);
        return previous;
    }
}

pub(crate) trait BoundedPosition<'lexer> {
    type Error;

    fn bounded(
        self,
        tokens: &mut TokenCursor<'lexer>,
        err_msg: impl Into<String>
    ) -> Result<TokenPosition<'lexer>, Self::Error> where Self: Sized;
    fn or_next( self, tokens: &mut TokenCursor<'lexer> ) -> Self where Self: Sized;
    fn or_previous( self, tokens: &mut TokenCursor<'lexer> ) -> Self where Self: Sized;
}

impl<'lexer> BoundedPosition<'lexer> for Option<TokenPosition<'lexer>> {
    type Error = SyntaxError;

    fn bounded(
        self,
        tokens: &mut TokenCursor<'lexer>,
        err_msg: impl Into<String>
    ) -> Result<TokenPosition<'lexer>, Self::Error> {
        return match self {
            Some( position ) => Ok( position ),
            None => {
                // this function is never called without a previous token, so we can safely unwrap
                let previous = unsafe{ tokens.peek_previous().unwrap_unchecked() };
                Err( SyntaxError {
                    line_byte_start: previous.line.byte_start,
                    line: previous.line.number,
                    col: previous.token.col,
                    len: previous.token.kind.len(),
                    msg: err_msg.into().into(),
                    help_msg: "file ended after here instead".into(),
                } )
            },
        }
    }

    fn or_next( self, tokens: &mut TokenCursor<'lexer> ) -> Self {
        return match self?.token.kind {
            TokenKind::Comment( _ ) | TokenKind::Empty => tokens.next(),
            _ => self,
        }
    }

    fn or_previous( self, tokens: &mut TokenCursor<'lexer> ) -> Self {
        return match self?.token.kind {
            TokenKind::Comment( _ ) | TokenKind::Empty => tokens.previous(),
            _ => self,
        }
    }
}

impl Lexer {
    pub(crate) fn iter( &self ) -> TokenCursor<'_> {
        return TokenCursor { line: 0, lines: &self.lines, token: 0, tokens: &self.tokens };
    }
}
