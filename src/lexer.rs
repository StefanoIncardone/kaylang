use std::{fmt::Display, path::{PathBuf, Path}, io::{ErrorKind, BufReader, BufRead}, fs::File, num::IntErrorKind};

use crate::errors::*;


#[derive( Debug, Clone, Copy )]
pub(crate) struct Line {
    pub(crate) start: usize,
    pub(crate) end: usize,
}

#[derive( Debug )]
pub struct Src {
    pub(crate) path: PathBuf,
    pub(crate) code: String,
    pub(crate) lines: Vec<Line>,
}

impl Src {
    pub(crate) fn try_from( path: &Path ) -> Result<Self, IoError> {
        let file = match File::open( path ) {
            Ok( f ) => f,
            Err( err ) => return Err( IoError {
                kind: err.kind(),
                msg: format!( "could not open '{}'", path.display() ).into(),
                cause: err.to_string().into(),
            } ),
        };

        let file_len = match file.metadata() {
            Ok( metadata ) => match metadata.is_file() {
                true => metadata.len() as usize,
                false => return Err( IoError {
                    kind: ErrorKind::InvalidInput,
                    msg: "invalid path".into(),
                    cause: format!( "expected a file but got directory '{}'", path.display() ).into(),
                } )
            },
            Err( err ) => return Err( IoError {
                kind: err.kind(),
                msg: format!( "could not read metadata of '{}'", path.display() ).into(),
                cause: err.to_string().into(),
            } ),
        };

        // plus one to account for a phantom newline at the end
        let mut data = String::with_capacity( file_len + 1 );
        let mut lines: Vec<Line> = Vec::new();
        let mut start = 0;
        let mut src = BufReader::new( file );

        loop {
            let chars_read = match src.read_line( &mut data ) {
                Ok( 0 ) => {
                    // it will make lexing simpler
                    if !data.is_empty() {
                        let last_char = data.len() - 1;
                        if data.as_bytes()[ last_char ] != b'\n' {
                            data.push( '\n' );
                            let last_line = lines.len() - 1;
                            lines[ last_line ].end += 1;
                        }
                    }
                    break;
                },
                Ok( read ) => read,
                Err( err ) => return Err( IoError {
                    kind: err.kind(),
                    msg: format!( "could not read contents of '{}'", path.display() ).into(),
                    cause: err.to_string().into(),
                } )
            };

            let mut end = data.len() - 1;
            if end > start && data.as_bytes()[ end - 1 ] == b'\r' {
                end -= 1;
            }

            lines.push( Line { start, end } );
            start += chars_read;
        }

        return Ok( Self { path: path.into(), code: data, lines } );
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
    col: usize,
    kind: BracketKind,
}

// TODO replace String with &'src str
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
}

impl Display for TokenKind {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Comment( text )    => write!( f, "#{}", text ),
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
        }
    }
}

#[derive( Debug, Clone )]
pub(crate) struct Token {
    pub(crate) col: usize,
    pub(crate) kind: TokenKind,
}

#[derive( Debug )]
pub(crate) struct Lexer<'src> {
    src: &'src Src,

    col: usize,
    token_start_col: usize,
    line_end: usize,

    tokens: Vec<Token>,

    brackets: Vec<Bracket>,
    errors: Vec<SyntaxError>,
}

impl<'src> Lexer<'src> {
    pub(crate) fn tokenize( src: &'src Src ) -> Result<Vec<Token>, Vec<SyntaxError>> {
        if src.lines.is_empty() {
            return Ok( Vec::new() );
        }

        let mut this = Self {
            src,
            col: 0,
            token_start_col: 0,
            line_end: 0,
            tokens: Vec::new(),
            brackets: Vec::new(),
            errors: Vec::new(),
        };

        for line in &src.lines {
            if line.start == line.end {
                continue;
            }

            this.col = line.start;
            this.line_end = line.end;
            // just to check if we are getting the correct line text
            let _line_text = &src.code[ line.start..line.end ];
            loop {
                let token = match this.tokeninze_next() {
                    Ok( None ) => break,
                    Ok( Some( kind ) ) => Token { col: this.token_start_col, kind },
                    Err( err ) => {
                        this.errors.push( err );
                        Token {
                            col: this.token_start_col,
                            kind: TokenKind::Unexpected( this.token_text().to_string() )
                        }
                    }
                };

                this.tokens.push( token );
            }
        }

        for bracket in &this.brackets {
            // there can only be open brackets at this point
            this.errors.push( SyntaxError {
                col: bracket.col,
                len: bracket.kind.len(),
                msg: "stray bracket".into(),
                help_msg: "was not closed".into(),
            } );
        }

        return match this.errors.is_empty() {
            true => Ok( this.tokens ),
            false => Err( this.errors ),
        }
    }
}

// TODO make next/peek methods return Option<Result> instead of Result<Option>
impl<'src> Lexer<'src> {
    fn token_text( &self ) -> &'src str {
        return &self.src.code[ self.token_start_col..self.col ];
    }

    // FIX properly handle non ASCII characters
        // TODO add an absolute column for the bytes in the line
        // IDEA only allow utf-8 characters in strings, characters and comments
    fn next( &mut self ) -> Result<Option<u8>, SyntaxError> {
        if self.col >= self.line_end {
            return Ok( None );
        }

        let next = self.src.code.as_bytes()[ self.col ];
        self.col += 1;
        return match next {
            ..=b'\x7F' => Ok( Some( next ) ),
            _ => Err( SyntaxError {
                col: self.col,
                len: 1,
                msg: "unrecognized character".into(),
                help_msg: "not a valid ASCII character".into(),
            } ),
        }
    }

    fn peek_next( &self ) -> Result<Option<&'src u8>, SyntaxError> {
        if self.col >= self.line_end {
            return Ok( None );
        }

        let next = &self.src.code.as_bytes()[ self.col ];
        return match next {
            ..=b'\x7F' => Ok( Some( next ) ),
            _ => Err( SyntaxError {
                col: self.col,
                len: 1,
                msg: "unrecognized character".into(),
                help_msg: "not a valid ASCII character".into(),
            } ),
        }
    }

    fn next_char( &mut self ) -> Result<u8, SyntaxError> {
        return match self.next()? {
            Some( next ) => Ok( next ),
            None => Err( SyntaxError {
                col: self.token_start_col,
                len: self.col - self.token_start_col + 1,
                msg: "invalid character literal".into(),
                help_msg: "missing closing single quote".into(),
            } ),
        }
    }

    fn next_str_char( &mut self ) -> Result<u8, SyntaxError> {
        return match self.next()? {
            Some( next ) => Ok( next ),
            None => Err( SyntaxError {
                col: self.token_start_col,
                len: self.col - self.token_start_col + 1,
                msg: "invalid string literal".into(),
                help_msg: "missing closing double quote".into(),
            } ),
        }
    }

    fn tokeninze_next( &mut self ) -> Result<Option<TokenKind>, SyntaxError> {
        loop {
            self.token_start_col = self.col;
            let next = match self.next()? {
                Some( ch ) => ch,
                None => return Ok( None ),
            };

            return match next {
                // ignore whitespace
                b'\t' | b'\r' | b'\n' | b'\x0C' | b' ' => continue,
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
                            col: self.token_start_col,
                            len: self.col - self.token_start_col + 1,
                            msg: "invalid identifier".into(),
                            help_msg: "contains non-ASCII characters".into(),
                        } )
                    }
                    else {
                        let identifier = match self.token_text() {
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
                            identifier => TokenKind::Identifier( identifier.to_string() ),
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

                    let token_text = self.token_text();
                    match token_text.parse() {
                        Ok( value ) => Ok( Some( TokenKind::Literal( Literal::Int( value ) ) ) ),
                        Err( err ) => match err.kind() {
                            IntErrorKind::InvalidDigit =>
                                if contains_non_ascii {
                                    Err( SyntaxError {
                                        col: self.token_start_col,
                                        len: token_text.len(),
                                        msg: "invalid number literal".into(),
                                        help_msg: "contains non-ASCII characters".into(),
                                    } )
                                }
                                else {
                                    Err( SyntaxError {
                                        col: self.token_start_col,
                                        len: token_text.len(),
                                        msg: "invalid number literal".into(),
                                        help_msg: "contains non-digit characters".into(),
                                    } )
                                },
                            IntErrorKind::PosOverflow => Err( SyntaxError {
                                col: self.token_start_col,
                                len: token_text.len(),
                                msg: "invalid number literal".into(),
                                help_msg: format!( "overflows a {} bit signed integer (over {})", isize::BITS, isize::MAX ).into(),
                            } ),
                            IntErrorKind::NegOverflow => Err( SyntaxError {
                                col: self.token_start_col,
                                len: token_text.len(),
                                msg: "invalid number literal".into(),
                                help_msg: format!( "underflows a {} bit signed integer (under {})", isize::BITS, isize::MIN ).into(),
                            } ),
                            IntErrorKind::Empty | std::num::IntErrorKind::Zero => unreachable!(),
                            _ => Err( SyntaxError {
                                col: self.token_start_col,
                                len: token_text.len(),
                                msg: "invalid number literal".into(),
                                help_msg: err.to_string().into(),
                            } ),
                        },
                    }
                },
                b'#' => match self.peek_next()? {
                    Some( b'"' ) => todo!( "block comments" ),
                    _ => {
                        // consume the rest of the tokens in the current line
                        self.col = self.line_end;
                        self.token_start_col += 1;
                        let comment = self.token_text();
                        Ok( Some( TokenKind::Comment( comment.to_string() ) ) )
                    },
                },
                b'"' => {
                    let mut errors: Vec<SyntaxError> = Vec::new();
                    let mut text = String::new();

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
                                    col: self.col,
                                    len: 1,
                                    msg: "invalid string character".into(),
                                    help_msg: "unrecognized escape character".into(),
                                } ),
                            },
                            b'\x00'..=b'\x1F' | b'\x7F' => Err( SyntaxError {
                                col: self.col,
                                len: 1,
                                msg: "invalid string literal".into(),
                                help_msg: "cannot be a control character".into(),
                            } ),
                            b'"' => break,
                            other => Ok( other as char ),
                        };

                        match next {
                            Ok( next_char ) => text.push( next_char ),
                            Err( err ) => errors.push( err ),
                        }
                    }

                    // after here there cannot be unclosed strings
                    if errors.is_empty() {
                        Ok( Some( TokenKind::Literal( Literal::Str( Str { text: text.into_bytes() } ) ) ) )
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
                                col: self.col,
                                len: 1,
                                msg: "invalid character literal".into(),
                                help_msg: "unrecognized escape character".into(),
                            } ),
                        },
                        b'\x00'..=b'\x1F' | b'\x7F' => Err( SyntaxError {
                            col: self.col,
                            len: 1,
                            msg: "invalid character literal".into(),
                            help_msg: "cannot be a control character".into(),
                        } ),
                        b'\'' => return Err( SyntaxError {
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
                            col: self.token_start_col,
                            len: self.col - self.token_start_col + 1,
                            msg: "invalid character literal".into(),
                            help_msg: "missing closing single quote".into(),
                        } )
                    }
                },
                b'(' => {
                    let kind = BracketKind::OpenRound;
                    self.brackets.push( Bracket { col: self.token_start_col, kind } );
                    Ok( Some( TokenKind::Bracket( kind ) ) )
                },
                b')' => match self.brackets.pop() {
                    Some( bracket ) => match bracket.kind {
                        BracketKind::OpenRound | BracketKind::CloseCurly | BracketKind::CloseRound =>
                            Ok( Some( TokenKind::Bracket( BracketKind::CloseRound ) ) ),
                        BracketKind::OpenCurly => Err( SyntaxError {
                            col: self.token_start_col,
                            len: 1,
                            msg: "stray bracket".into(),
                            help_msg: "closes the wrong bracket".into(),
                        } ),
                    },
                    None => Err( SyntaxError {
                        col: self.token_start_col,
                        len: 1,
                        msg: "stray bracket".into(),
                        help_msg: "was not opened before".into(),
                    } ),
                },
                b'{' => {
                    let kind = BracketKind::OpenCurly;
                    self.brackets.push( Bracket { col: self.token_start_col, kind } );
                    Ok( Some( TokenKind::Bracket( kind ) ) )
                },
                b'}' => match self.brackets.pop() {
                    Some( bracket ) => match bracket.kind {
                        BracketKind::OpenCurly | BracketKind::CloseCurly | BracketKind::CloseRound =>
                            Ok( Some( TokenKind::Bracket( BracketKind::CloseCurly ) ) ),
                        BracketKind::OpenRound => Err( SyntaxError {
                            col: self.token_start_col,
                            len: 1,
                            msg: "stray bracket".into(),
                            help_msg: "closes the wrong bracket".into(),
                        } ),
                    },
                    None => Err( SyntaxError {
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
                // b'\r' | b'\n' => unreachable!( "line text should have been trimmed already" ),
                _ => Err( SyntaxError {
                    col: self.token_start_col,
                    len: 1,
                    msg: "unexpected character".into(),
                    help_msg: "unrecognized".into(),
                } ),
            }
        }
    }
}