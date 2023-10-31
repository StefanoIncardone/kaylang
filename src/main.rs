use std::{io::{BufReader, BufRead, ErrorKind, BufWriter, Write, Seek, SeekFrom, IsTerminal}, fs::File, env::{self, Args}, process::{ExitCode, Command}, fmt::Display, path::{Path, PathBuf}, borrow::Cow, cmp::Ordering, num::IntErrorKind, time::Instant};


static CHECKING:    Colored = Colored { text: Cow::Borrowed( " Checking" ), foreground: Foreground::LightGreen, background: Background::Default, flags: Flags::Bold };
static COMPILING:   Colored = Colored { text: Cow::Borrowed( "Compiling" ), foreground: Foreground::LightGreen, background: Background::Default, flags: Flags::Bold };
static RUNNING:     Colored = Colored { text: Cow::Borrowed( "  Running" ), foreground: Foreground::LightGreen, background: Background::Default, flags: Flags::Bold };
static DONE:        Colored = Colored { text: Cow::Borrowed( "     Done" ), foreground: Foreground::LightGreen, background: Background::Default, flags: Flags::Bold };

static ERROR:       Colored = Colored { text: Cow::Borrowed( "Error" ), foreground: Foreground::LightRed, background: Background::Default, flags: Flags::Bold };
static AT:          Colored = Colored { text: Cow::Borrowed( "at" ), foreground: Foreground::LightRed, background: Background::Default, flags: Flags::Bold };
static BAR:         Colored = Colored { text: Cow::Borrowed( "|" ), foreground: Foreground::LightBlue, background: Background::Default, flags: Flags::Bold };
static CAUSE:       Colored = Colored { text: Cow::Borrowed( "Cause" ), foreground: Foreground::LightRed, background: Background::Default, flags: Flags::Bold };

static VERSION:     Colored = Colored { text: Cow::Borrowed( env!( "CARGO_PKG_VERSION" ) ), foreground: Foreground::LightGray, background: Background::Default, flags: Flags::Bold };
static OPTIONS:     Colored = Colored { text: Cow::Borrowed( "Options" ),  foreground: Foreground::LightGray, background: Background::Default, flags: Flags::Bold };
static RUN_MODE:    Colored = Colored { text: Cow::Borrowed( "Run mode" ), foreground: Foreground::LightGray, background: Background::Default, flags: Flags::Bold };
static MODE:        Colored = Colored { text: Cow::Borrowed( "mode" ), foreground: Foreground::LightGray, background: Background::Default, flags: Flags::Bold };
static FILE:        Colored = Colored { text: Cow::Borrowed( "file" ), foreground: Foreground::LightGray, background: Background::Default, flags: Flags::Bold };
static PATH:        Colored = Colored { text: Cow::Borrowed( "path" ), foreground: Foreground::LightGray, background: Background::Default, flags: Flags::Bold };
static OUTPUT:      Colored = Colored { text: Cow::Borrowed( "Output" ),  foreground: Foreground::LightGray, background: Background::Default, flags: Flags::Bold };


#[allow(dead_code)]
#[derive( Debug, Default, Clone, Copy, PartialEq )]
#[repr(u8)]
pub enum Foreground {
    #[default] Default = 0,
    Black = 30,
    Red = 31,
    Green = 32,
    Yellow = 33,
    Blue = 34,
    Magenta = 35,
    Cyan = 36,
    LightGray = 37,
    DarkGray = 90,
    LightRed = 91,
    LightGreen = 92,
    LightYellow = 93,
    LightBlue = 94,
    LightMagenta = 95,
    LightCyan = 96,
    White = 97,
}

#[allow(dead_code)]
#[derive( Debug, Default, Clone, Copy, PartialEq )]
#[repr(u8)]
pub enum Background {
    #[default] Default = 0,
    Black = 40,
    DarkRed = 41,
    DarkGreen = 42,
    DarkYellow = 43,
    DarkBlue = 44,
    DarkMagenta = 45,
    DarkCyan = 46,
    DarkWhite = 47,
    BrightBlack = 100,
    BrightRed = 101,
    BrightGreen = 102,
    BrightYellow = 103,
    BrightBlue = 104,
    BrightMagenta = 105,
    BrightCyan = 106,
    White = 107,
}

#[derive( Debug, Default, Clone, Copy, PartialEq )]
#[repr(u8)]
pub enum Flags {
    #[default]
    Default         = 0b0000_0000,
    Bold            = 0b0000_0001,
    Underline       = 0b0000_0010,
    NoUnderline     = 0b0000_0100,
    ReverseText     = 0b0000_1000,
    PositiveText    = 0b0001_0000,
}

impl Flags {
    fn is( &self, flag: Flags ) -> bool { return *self as u8 & flag as u8 != 0; }

    fn bold( &self )            -> bool { return self.is( Flags::Bold ); }
    fn underline( &self )       -> bool { return self.is( Flags::Underline ); }
    fn no_underline( &self )    -> bool { return self.is( Flags::NoUnderline ); }
    fn reverse_text( &self )    -> bool { return self.is( Flags::ReverseText ); }
    fn positive_text( &self )   -> bool { return self.is( Flags::PositiveText ); }
}

#[derive( Debug, Default )]
pub struct Colored {
    pub text: Cow<'static, str>,
    pub foreground: Foreground,
    pub background: Background,
    pub flags: Flags,
}

impl Display for Colored {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return unsafe { display( self, f ) };
    }
}

impl Colored {
    fn no_color( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return self.text.fmt( f );
    }

    fn color( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        let mut codes = String::with_capacity( 15 );

        if self.foreground != Foreground::Default {
            codes += &format!( "{};", self.foreground as u8 );
        }
        if self.background != Background::Default {
            codes += &format!( "{};", self.background as u8 );
        }
        if self.flags.bold() {
            codes += "1;";
        }
        if self.flags.underline() {
            codes += "4;";
        }
        if self.flags.no_underline() {
            codes += "24;";
        }
        if self.flags.reverse_text() {
            codes += "7;";
        }
        if self.flags.positive_text() {
            codes += "27;";
        }

        return if codes.is_empty() {
            self.text.fmt( f )
        }
        else {
            codes.pop(); //remove the last ";"

            write!( f, "\x1b[{}m", codes )?;
            self.text.fmt( f )?;
            write!( f, "\x1b[0m" )
        }
    }
}

#[allow(non_upper_case_globals)]
static mut display: fn(&Colored, &mut std::fmt::Formatter<'_>) -> std::fmt::Result = Colored::color;

#[derive( Debug, Default, Clone, Copy, PartialEq )]
#[repr(u8)]
pub enum Color {
    #[default] Auto,
    Always,
    Never,
}

impl Color {
    pub fn set( &self ) {
        unsafe { display = match self {
            Self::Auto => if !std::io::stderr().is_terminal() { Colored::no_color } else { Colored::color },
            Self::Always => Colored::color,
            Self::Never => Colored::no_color,
        } }
    }

    // since printing version and help message are the only places where printing to stdoud is performed we are
    // manually checking if stdout (overring stderr coloring modes) is in terminal mode until a way to separately print
    // colored/non-colored output to stdout/stderr is found
    pub fn set_stdout( &self ) {
        unsafe { display = match self {
            Self::Auto => if !std::io::stdout().is_terminal() { Colored::no_color } else { Colored::color },
            Self::Always => Colored::color,
            Self::Never => Colored::no_color,
        } }
    }
}


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
    // IDEA have different size integers and default to 32 bits for literals
    Int( isize ),
    Char( u8 ), // only supporting ASCII characters for now
    Bool( bool ),
    Str( Str ),
    Uninitialized( Type ),
}

impl Display for Literal {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Int( value ) => write!( f, "{}", value ),
            Self::Char( code ) => write!( f, "'{}'", code.escape_ascii() ), // TODO create own escaping function
            Self::Bool( value ) => write!( f, "{}", value ),
            Self::Str( string ) => {
                write!( f, "\"" )?;
                for character in &string.text {
                    write!( f, "{}", character.escape_ascii() )?;
                }
                write!( f, "\"" )
            },
            Self::Uninitialized( _ ) => write!( f, "?" ),
        }
    }
}

impl Into<isize> for Literal {
    fn into( self ) -> isize {
        return match self {
            Self::Int( value ) => value.into(),
            Self::Char( code ) => code.into(),
            Self::Bool( value ) => value.into(),
            Self::Str( string ) => string.text.len() as isize,
            Self::Uninitialized( _ ) => panic!( "cannot get the int value of an uninitialized value")
        }
    }
}

impl Len for Literal {
    fn len( &self ) -> usize {
        return match self {
            Self::Int( value ) => value.to_string().len(),
            Self::Char( _ ) => 1,
            Self::Bool( value ) => value.to_string().len(),
            Self::Str( string ) => string.text.len() + 2,
            Self::Uninitialized( typ ) => typ.len(),
        }
    }
}

impl TypeOf for Literal {
    fn typ( &self ) -> Type {
        return match self {
            Self::Int { .. } => Type::Int,
            Self::Char { .. } => Type::Char,
            Self::Bool { .. } => Type::Bool,
            Self::Str( _ ) => Type::Str,
            Self::Uninitialized( typ ) => *typ,
        }
    }
}

// TODO implement unsigned integers
#[derive( Debug, PartialEq, Clone, Copy )]
enum Type {
    Int,
    Char,
    Bool,
    Str,
}

impl Display for Type {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Int => write!( f, "int" ),
            Self::Char => write!( f, "char" ),
            Self::Bool => write!( f, "bool" ),
            Self::Str => write!( f, "str" ),
        }
    }
}

impl Len for Type {
    fn len( &self ) -> usize {
        match self {
            Self::Int => core::mem::size_of::<isize>(),
            Self::Char => core::mem::size_of::<u8>(),
            Self::Bool => core::mem::size_of::<bool>(),
            Self::Str => core::mem::size_of::<*const u8>() + core::mem::size_of::<usize>(),
        }
    }
}

impl Type {
    fn default( &self ) -> Literal {
        match self {
            Self::Bool => Literal::Bool( false ),
            Self::Char => Literal::Char( 0 ),
            Self::Int => Literal::Int( 0 ),
            Self::Str => Literal::Str( Str { text: Vec::new() } ),
        }
    }
}

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
    Negate,
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
    Not,
    Xor
}

impl Display for Operator {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Pow => write!( f, "**" ),
            Self::PowEquals => write!( f, "**=" ),
            Self::Times => write!( f, "*" ),
            Self::TimesEquals => write!( f, "*=" ),
            Self::Divide => write!( f, "/" ),
            Self::DivideEquals => write!( f, "/=" ),
            Self::Remainder => write!( f, "%" ),
            Self::RemainderEquals => write!( f, "%=" ),

            Self::Plus => write!( f, "+" ),
            Self::PlusEquals => write!( f, "+=" ),
            Self::Minus | Self::Negate => write!( f, "-" ),
            Self::MinusEquals => write!( f, "-=" ),

            Self::EqualsEquals => write!( f, "==" ),
            Self::NotEquals => write!( f, "!=" ),
            Self::Greater => write!( f, ">" ),
            Self::GreaterOrEquals => write!( f, ">=" ),
            Self::Less => write!( f, "<" ),
            Self::LessOrEquals => write!( f, "<=" ),
            Self::Compare => write!( f, "<=>" ),

            Self::And => write!( f, "&&" ),
            Self::Or => write!( f, "||" ),
            Self::Not => write!( f, "!" ),
            Self::Xor => write!( f, "^^" ),
        }
    }
}

impl Len for Operator {
    fn len( &self ) -> usize {
        return match self {
            Self::Pow => 2,
            Self::PowEquals => 3,
            Self::Times => 1,
            Self::TimesEquals => 2,
            Self::Divide => 1,
            Self::DivideEquals => 2,
            Self::Remainder => 1,
            Self::RemainderEquals => 2,

            Self::Plus => 1,
            Self::PlusEquals => 2,
            Self::Minus | Self::Negate => 1,
            Self::MinusEquals => 2,

            Self::EqualsEquals => 2,
            Self::NotEquals => 2,
            Self::Greater => 1,
            Self::GreaterOrEquals => 2,
            Self::Less => 1,
            Self::LessOrEquals => 2,
            Self::Compare => 3,

            Self::And => 2,
            Self::Or => 2,
            Self::Not => 1,
            Self::Xor => 2,
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
            | Self::Minus | Self::MinusEquals | Self::Negate
            | Self::Compare => Type::Int,

            Self::EqualsEquals | Self::NotEquals
            | Self::Greater | Self::GreaterOrEquals
            | Self::Less | Self::LessOrEquals
            | Self::And | Self::Or | Self::Not | Self::Xor => Type::Bool,
        }
    }
}

impl Precedence for Operator {
    fn precedence( &self ) -> usize {
        return match self {
            Self::Negate | Self::And | Self::Or | Self::Not | Self::Xor => 0,

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
    Colon,
    SemiColon,
    Equals,
    #[allow(dead_code)]
    QuestionMark,
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
    Loop,
    Break,
    Continue,

    // Special
    Empty,
}

impl Display for TokenKind {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Comment( text ) => write!( f, "{}", text ),
            Self::Unexpected( text ) => write!( f, "{}", text ),

            Self::Bracket( bracket ) => write!( f, "{}", bracket ),
            Self::Equals => write!( f, "=" ),
            Self::Colon => write!( f, ":" ),
            Self::SemiColon => write!( f, ";" ),
            Self::QuestionMark => write!( f, "?" ),

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
            Self::Loop => write!( f, "loop" ),
            Self::Break => write!( f, "break" ),
            Self::Continue => write!( f, "continue" ),

            Self::Empty => write!( f, "" ),
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
            Self::Colon => 1,
            Self::SemiColon => 1,
            Self::QuestionMark => 1,

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
            Self::Loop => 4,
            Self::Break => 5,
            Self::Continue => 8,

            Self::Empty => 1,
        }
    }
}

#[derive( Debug, Clone )]
struct Token {
    col: usize,
    kind: TokenKind,
}

#[derive( Debug, Clone, Copy )]
struct Line {
    number: usize,
    byte_start: usize,
    token_start_idx: usize,
    token_end_idx: usize,
}

// TODO create different kinds of errors for different stages of compilation
// TODO implement NOTE, HINT, HELP in error messages
#[derive( Debug )]
struct SyntaxError {
    line_byte_start: usize,
    line: usize,
    col: usize,
    len: usize,
    msg: Cow<'static, str>,
    help_msg: Cow<'static, str>,
}

// TODO find a way to implement display for this
    // NOTE reading from file requires &mut self, while the Display trait requires a &self,
    // so the implementation of the "Display" trait is inlined for now
#[derive( Debug )]
struct SyntaxErrors {
    src: BlitzSrc,
    errors: Vec<SyntaxError>,
}

#[derive( Debug )]
struct Bracket {
    line_byte_start: usize,
    line_number: usize,
    col: usize,
    kind: BracketKind,
}

#[derive( Debug )]
struct Lexer {
    src: BlitzSrc,

    line: usize,
    line_byte_start: usize,
    col: usize,
    token_start_col: usize,

    lines: Vec<Line>,
    tokens: Vec<Token>,

    line_bytes: Vec<u8>,
    token_text: String,

    brackets: Vec<Bracket>,
    errors: Vec<SyntaxError>,
}

impl TryFrom<BlitzSrc> for Lexer {
    type Error = SyntaxErrors;


    fn try_from( src: BlitzSrc ) -> Result<Self, Self::Error> {
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
            token_end_idx: 0
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
                                Ok( Some( kind ) ) => Token { col: this.token_start_col, kind },
                                Err( err ) => {
                                    this.errors.push( err );
                                    let unrecognized = this.token_text();
                                    Token { col: this.token_start_col, kind: TokenKind::Unexpected( unrecognized ) }
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

        // FIX insert bracket related errors in the correct place, right now they appear at the end, out of order from the rest of the errors
        for bracket in &this.brackets {
            // there can only be open brackets at this point
            this.errors.push( SyntaxError {
                line_byte_start: bracket.line_byte_start,
                line: bracket.line_number,
                col: bracket.col,
                len: bracket.kind.len(),
                msg: "stray bracket".into(),
                help_msg: "was not closed".into()
            } );
        }

        return match this.errors.is_empty() {
            true => Ok( this ),
            false => Err( SyntaxErrors { src: this.src, errors: this.errors } ),
        }
    }
}

impl Lexer {
    fn iter<'lexer>( &'lexer self ) -> TokenCursor<'lexer> {
        return TokenCursor { line: 0, lines: &self.lines, token: 0, tokens: &self.tokens };
    }

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
                help_msg: "not a valid ASCII character".into()
            }),
        }
    }

    fn peek_next<'src>( &'src self ) -> Result<Option<&'src u8>, SyntaxError> {
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
                help_msg: "not a valid ASCII character".into()
            }),
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
                help_msg: "missing closing single quote".into()
            }),
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
                help_msg: "missing closing double quote".into()
            }),
            Some( next ) => Ok( next ),
        }
    }

    fn tokeninze_next( &mut self ) -> Result<Option<TokenKind>, SyntaxError> {
        // this loop is ever going to run just once, it's here just so we can use continue and break
        loop {
            self.token_text.clear();
            let next = match self.next()? {
                Some( ch ) => ch,
                None => return Ok( None ),
            };

            // registering the token start column after getting the next character to maintain 1 indexing token columns
            self.token_start_col = self.col;
            return match next {
                // ignore whitespace
                b'\t' | b'\x0C' | b'\r' | b' ' => continue,
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
                            help_msg: "contains non-ASCII characters".into()
                        })
                    }
                    else {
                        self.gather_token_text();
                        let identifier = match self.token_text.as_str() {
                            "let" => TokenKind::Definition( Mutability::Let ),
                            "var" => TokenKind::Definition( Mutability::Var ),
                            "print" => TokenKind::Print,
                            "println" => TokenKind::PrintLn,
                            "true" => TokenKind::True,
                            "false" => TokenKind::False,
                            "if" => TokenKind::If,
                            "else" => TokenKind::Else,
                            "loop" => TokenKind::Loop,
                            "break" => TokenKind::Break,
                            "continue" => TokenKind::Continue,
                            _ => TokenKind::Identifier( self.token_text.clone() ),
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
                    match self.token_text.parse() { // TODO create own number parsing function
                        Ok( value ) => Ok( Some( TokenKind::Literal( Literal::Int( value ) ) ) ),
                        Err( err ) => match err.kind() {
                            IntErrorKind::InvalidDigit =>
                                if !contains_non_ascii {
                                    Err( SyntaxError {
                                        line_byte_start: self.line_byte_start,
                                        line: self.line,
                                        col: self.token_start_col,
                                        len: self.token_text.len(),
                                        msg: "invalid number literal".into(),
                                        help_msg: "contains non-digit characters".into()
                                    })
                                }
                                else {
                                    Err( SyntaxError {
                                        line_byte_start: self.line_byte_start,
                                        line: self.line,
                                        col: self.token_start_col,
                                        len: self.token_text.len(),
                                        msg: "invalid number literal".into(),
                                        help_msg: "contains non-ASCII characters".into()
                                    })
                                },
                            IntErrorKind::PosOverflow => Err( SyntaxError {
                                line_byte_start: self.line_byte_start,
                                line: self.line,
                                col: self.token_start_col,
                                len: self.token_text.len(),
                                msg: "invalid number literal".into(),
                                help_msg: format!( "overflows a {} bit signed integer (over {})", isize::BITS, isize::MAX ).into()
                            }),
                            IntErrorKind::NegOverflow => Err( SyntaxError {
                                line_byte_start: self.line_byte_start,
                                line: self.line,
                                col: self.token_start_col,
                                len: self.token_text.len(),
                                msg: "invalid number literal".into(),
                                help_msg: format!( "underflows a {} bit signed integer (under {})", isize::BITS, isize::MIN ).into()
                            }),
                            IntErrorKind::Empty | std::num::IntErrorKind::Zero => unreachable!(),
                            _ => Err( SyntaxError {
                                line_byte_start: self.line_byte_start,
                                line: self.line,
                                col: self.token_start_col,
                                len: self.token_text.len(),
                                msg: "invalid number literal".into(),
                                help_msg: err.to_string().into()
                            }),
                        },
                    }
                },
                b'#' => {
                    self.col = self.line_bytes.len(); // consume the rest of the tokens in the current line
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
                                    help_msg: "unrecognized escape character".into()
                                }),
                            },
                            b'\x00'..=b'\x1F' | b'\x7F' => Err( SyntaxError {
                                line_byte_start: self.line_byte_start,
                                line: self.line,
                                col: self.col,
                                len: 1,
                                msg: "invalid string literal".into(),
                                help_msg: "cannot be a control character".into()
                            }),
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
                        Ok( Some( TokenKind::Literal( Literal::Str( Str { text: self.token_text.clone().into_bytes() } ))))
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
                                help_msg: "unrecognized escape character".into()
                            }),
                        },
                        b'\x00'..=b'\x1F' | b'\x7F' => Err( SyntaxError {
                            line_byte_start: self.line_byte_start,
                            line: self.line,
                            col: self.col,
                            len: 1,
                            msg: "invalid character literal".into(),
                            help_msg: "cannot be a control character".into()
                        }),
                        b'\'' => break Err( SyntaxError {
                            line_byte_start: self.line_byte_start,
                            line: self.line,
                            col: self.token_start_col,
                            len: 2,
                            msg: "invalid character literal".into(),
                            help_msg: "must not be empty".into()
                        }),
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
                            help_msg: "missing closing single quote".into()
                        })
                    }
                },
                b'(' => {
                    let kind = BracketKind::OpenRound;
                    self.brackets.push( Bracket {
                        line_byte_start: self.line_byte_start,
                        line_number: self.line,
                        col: self.token_start_col,
                        kind
                    } );
                    Ok( Some( TokenKind::Bracket( kind ) ) )
                },
                b')' => match self.brackets.pop() {
                    Some( Bracket { kind: BracketKind::OpenRound | BracketKind::CloseCurly | BracketKind::CloseRound, .. } ) =>
                        Ok( Some( TokenKind::Bracket( BracketKind::CloseRound ) ) ),
                    Some( Bracket { kind: BracketKind::OpenCurly, .. } ) => Err( SyntaxError {
                        line_byte_start: self.line_byte_start,
                        line: self.line,
                        col: self.token_start_col,
                        len: 1,
                        msg: "stray bracket".into(),
                        help_msg: "closes the wrong bracket".into()
                    }),
                    None => Err( SyntaxError {
                        line_byte_start: self.line_byte_start,
                        line: self.line,
                        col: self.token_start_col,
                        len: 1,
                        msg: "stray bracket".into(),
                        help_msg: "was not opened before".into()
                    }),
                },
                b'{' => {
                    let kind = BracketKind::OpenCurly;
                    self.brackets.push( Bracket {
                        line_byte_start: self.line_byte_start,
                        line_number: self.line,
                        col: self.token_start_col,
                        kind
                    } );
                    Ok( Some( TokenKind::Bracket( kind ) ) )
                },
                b'}' => match self.brackets.pop() {
                    Some( Bracket { kind: BracketKind::OpenCurly | BracketKind::CloseCurly | BracketKind::CloseRound, .. } ) =>
                        Ok( Some( TokenKind::Bracket( BracketKind::CloseCurly ) ) ),
                    Some( Bracket { kind: BracketKind::OpenRound, .. } ) => Err( SyntaxError {
                        line_byte_start: self.line_byte_start,
                        line: self.line,
                        col: self.token_start_col,
                        len: 1,
                        msg: "stray bracket".into(),
                        help_msg: "closes the wrong bracket".into()
                    }),
                    None => Err( SyntaxError {
                        line_byte_start: self.line_byte_start,
                        line: self.line,
                        col: self.token_start_col,
                        len: 1,
                        msg: "stray bracket".into(),
                        help_msg: "was not opened before".into()
                    }),
                },
                b':' => Ok( Some( TokenKind::Colon ) ),
                b';' => Ok( Some( TokenKind::SemiColon ) ),
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
                b'=' => match self.peek_next()? {
                    Some( b'=' ) => {
                        self.col += 1;
                        Ok( Some( TokenKind::Op( Operator::EqualsEquals ) ) )
                    },
                    _ => Ok( Some( TokenKind::Equals ) ),
                },
                b'!' => match self.peek_next()? {
                    Some( b'=' ) => {
                        self.col += 1;
                        Ok( Some( TokenKind::Op( Operator::NotEquals ) ) )
                    },
                    _ => Ok( Some( TokenKind::Op( Operator::Not ) ) ),
                },
                b'>' => match self.peek_next()? {
                    Some( b'=' ) => {
                        self.col += 1;
                        Ok( Some( TokenKind::Op( Operator::GreaterOrEquals ) ) )
                    },
                    _ => Ok( Some( TokenKind::Op( Operator::Greater ) ) ),
                },
                b'<' => match self.peek_next()? {
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
                b'^' => match self.peek_next()? {
                    Some( b'^' ) => {
                        self.col += 1;
                        Ok( Some( TokenKind::Op( Operator::Xor ) ) )
                    },
                    _ => Err( SyntaxError {
                        line_byte_start: self.line_byte_start,
                        line: self.line,
                        col: self.token_start_col,
                        len: 1,
                        msg: "unexpected character".into(),
                        help_msg: "did you mean '^^'?".into()
                    }),
                },
                b'&' => match self.peek_next()? {
                    Some( b'&' ) => {
                        self.col += 1;
                        Ok( Some( TokenKind::Op( Operator::And ) ) )
                    },
                    _ => Err( SyntaxError {
                        line_byte_start: self.line_byte_start,
                        line: self.line,
                        col: self.token_start_col,
                        len: 1,
                        msg: "unexpected character".into(),
                        help_msg: "did you mean '&&'?".into()
                    }),
                },
                b'|' => match self.peek_next()? {
                    Some( b'|' ) => {
                        self.col += 1;
                        Ok( Some( TokenKind::Op( Operator::Or ) ) )
                    },
                    _ => Err( SyntaxError {
                        line_byte_start: self.line_byte_start,
                        line: self.line,
                        col: self.token_start_col,
                        len: 1,
                        msg: "unexpected character".into(),
                        help_msg: "did you mean '||'?".into()
                    }),
                },
                b'\n' => unreachable!( "line text should have been trimmed already" ),
                // disabling explicit uninitialization until we have proper uninitialization checking
                b'?' | _ => Err( SyntaxError {
                    line_byte_start: self.line_byte_start,
                    line: self.line,
                    col: self.token_start_col,
                    len: 1,
                    msg: "unexpected character".into(),
                    help_msg: "unrecognized".into()
                }),
            }
        }
    }
}


#[derive( Debug, Clone, Copy )]
struct TokenPosition<'lexer> {
    line: &'lexer Line,
    token: &'lexer Token,
}

#[derive( Debug )]
struct TokenCursor<'lexer> {
    line: usize,
    lines: &'lexer [Line],

    token: usize,
    tokens: &'lexer [Token],
}

// TODO implement methods that return only tokens or lines since lines are only really needed when reporting errors
impl<'lexer> TokenCursor<'lexer> {
    fn current( &mut self ) -> Option<TokenPosition<'lexer>> {
        if self.token >= self.tokens.len() || self.line >= self.lines.len() {
            return None
        }
        return Some( TokenPosition{ line: &self.lines[ self.line ], token: &self.tokens[ self.token ] } ).or_next( self );
    }

    fn next( &mut self ) -> Option<TokenPosition<'lexer>> {
        if self.token >= self.tokens.len() - 1 {
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

    fn previous( &mut self ) -> Option<TokenPosition<'lexer>> {
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

    fn peek_next( &mut self ) -> Option<TokenPosition<'lexer>> {
        let (starting_line, starting_token) = (self.line, self.token);
        let next = self.next();
        (self.line, self.token) = (starting_line, starting_token);
        return next;
    }

    fn peek_previous( &mut self ) -> Option<TokenPosition<'lexer>> {
        let (starting_line, starting_token) = (self.line, self.token);
        let previous = self.previous();
        (self.line, self.token) = (starting_line, starting_token);
        return previous;
    }
}

trait BoundedPosition<'lexer> {
    type Error;


    fn bounded( self, tokens: &mut TokenCursor<'lexer>, err_msg: impl Into<String> ) -> Result<TokenPosition<'lexer>, Self::Error> where Self: Sized;
    // no implementation for "bounded_previous" is provided since it is never actually needed during parsing
    fn or_next( self, tokens: &mut TokenCursor<'lexer> ) -> Self where Self: Sized;
    fn or_previous( self, tokens: &mut TokenCursor<'lexer> ) -> Self where Self: Sized;
}

impl<'lexer> BoundedPosition<'lexer> for Option<TokenPosition<'lexer>> {
    type Error = SyntaxError;


    fn bounded( self, tokens: &mut TokenCursor<'lexer>, err_msg: impl Into<String> ) -> Result<TokenPosition<'lexer>, Self::Error> {
        return match self {
            Some( position ) => Ok( position ),
            None => {
                // we are always sure that this function is never called without a previous token existing, so we can safely unwrap
                let previous = unsafe{ tokens.peek_previous().unwrap_unchecked() };
                Err( SyntaxError {
                    line_byte_start: previous.line.byte_start,
                    line: previous.line.number,
                    col: previous.token.col,
                    len: previous.token.kind.len(),
                    msg: err_msg.into().into(),
                    help_msg: "file ended after here instead".into()
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

#[derive( Debug, Clone )]
enum Expression {
    Literal( Literal ),
    Unary { op: Operator, operand: Box<Expression> },
    Binary { lhs: Box<Expression>, op: Operator, rhs: Box<Expression> },
    Identifier( String, Type ),
}

impl Display for Expression {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Literal( literal ) => write!( f, "{}", literal ),
            Self::Unary { op, operand } => match op {
                Operator::Negate => write!( f, "-{}", operand ),
                Operator::Not => write!( f, "not {}", operand ),
                _ => unreachable!(),
            },
            Self::Binary { lhs, op, rhs } => write!( f, "({} {} {})", lhs, op, rhs ),
            Self::Identifier( name, _ ) => write!( f, "{}", name ),
        }
    }
}

impl TypeOf for Expression {
    fn typ( &self ) -> Type {
        return match self {
            Self::Literal( literal ) => literal.typ(),
            Self::Unary { operand, .. } => operand.typ(),
            Self::Binary { op, .. } => op.typ(),
            Self::Identifier( _, typ ) => *typ,
        }
    }
}

impl Precedence for Expression {
    fn precedence( &self ) -> usize {
        return match self {
            Self::Unary { .. } => 0,
            Self::Literal( _ ) | Self::Identifier( _, _) => 1,
            Self::Binary { .. } => 2,
        }
    }

    fn precedence_over( &self, other: &Self ) -> Ordering {
        return self.precedence().cmp( &other.precedence() );
    }
}

#[derive( Debug, Clone )]
struct Variable {
    mutability: Mutability,
    name: String,
    typ: Type,
    initialized: bool,
}

#[derive( Debug, Clone )]
struct IfStatement {
    condition: Expression,
    statement: Node,
}

impl Display for IfStatement {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return write!( f, "if {}", self.condition );
    }
}

#[derive( Debug, Clone )]
struct If {
    ifs: Vec<IfStatement>,
    els: Option<Box<Node>>,
}

#[allow( dead_code )]
#[derive( Debug, Clone )]
struct Loop {
    pre: Option<Box<Node>>,
    condition: Option<Expression>,
    post: Option<Box<Node>>,
    statement: Box<Node>,
}

#[derive( Debug, Clone )]
enum Node {
    Empty,

    Expression( Expression ),
    Print( Expression ),
    Println( Option<Expression> ),
    If( If ),
    Loop( Loop ),
    Break,
    Continue,

    Definition( String, Expression ),
    Assignment( String, Expression ),
    Scope( usize ),
}

impl Display for Node {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Expression( expression ) => write!( f, "{}", expression ),
            Self::Print( argument ) => write!( f, "print {}", argument ),
            Self::Println( argument ) => match argument {
                Some( arg ) => write!( f, "println {}", arg ),
                None => write!( f, "println" ),
            },
            Self::If( iff ) => write!( f, "{}", iff.ifs[ 0 ] ),
            Self::Loop( looop ) => match &looop.condition {
                Some( condition ) => write!( f, "loop {}", condition ),
                None => write!( f, "loop" ),
            },

            Self::Empty
            | Self::Break | Self::Continue
            | Self::Definition( _, _ ) | Self::Assignment( _, _ )
            | Self::Scope( _ ) => unreachable!(),
        }
    }
}

#[derive( Debug, Clone )]
struct Scope {
    parent: usize,
    types: Vec<Type>,
    variables: Vec<Variable>,
    nodes: Vec<Node>,
}

// TODO process entire statement for syntactical correctness and then report all the errors
    // IDEA create Parser class that builds the AST, and then validate the AST afterwards
#[derive( Debug )]
struct AST {
    scopes: Vec<Scope>,
    current_scope: usize,
    loop_depth: usize,

    errors: Vec<SyntaxError>,
}

impl TryFrom<Lexer> for AST {
    type Error = SyntaxErrors;

    fn try_from( lexer: Lexer ) -> Result<Self, Self::Error> {
        let mut this = Self {
            scopes: vec![Scope {
                parent: 0,
                types: vec![Type::Int, Type::Char, Type::Bool, Type::Str],
                variables: Vec::new(),
                nodes: Vec::new(),
            }],
            current_scope: 0,
            loop_depth: 0,
            errors: Vec::new(),
        };

        let mut tokens = lexer.iter();
        this.parse_scope( &mut tokens );

        return match this.errors.is_empty() {
            true => Ok( this ),
            false => Err( SyntaxErrors { src: lexer.src, errors: this.errors } ),
        }
    }
}

// scopes
impl AST {
    fn parse_scope( &mut self, tokens: &mut TokenCursor ) {
        loop {
            match self.parse_single( tokens ) {
                Ok( Some( Node::Empty ) ) => continue,
                Ok( Some( node ) ) => self.scopes[ self.current_scope ].nodes.push( node ),
                Ok( None ) => break,
                // only parsing until the first error until a fault tolerant parser is developed, this is
                // because the first truly relevant error is the first one, which in turn causes a ripple effect
                // that propagates to the rest of the parsing, causing subsequent errors to be wrong
                Err( err ) => {
                    self.errors.push( err );

                    // consuming all remaining tokens until the end of the file
                    tokens.line = tokens.lines.len();
                    tokens.token = tokens.tokens.len();
                    break;
                },
            }
        }
    }

    fn parse_single( &mut self, tokens: &mut TokenCursor ) -> Result<Option<Node>, SyntaxError> {
        let current = match tokens.current() {
            Some( position ) => position,
            None => return Ok( None ),
        };

        return match current.token.kind {
            TokenKind::Literal( _ )
            | TokenKind::True | TokenKind::False
            | TokenKind::Bracket( BracketKind::OpenRound )
            | TokenKind::Op( Operator::Minus | Operator::Not ) => Ok( Some( Node::Expression( self.expression( tokens )? ) ) ),
            TokenKind::Definition( _ ) => Ok( Some( self.variable_definition( tokens )? ) ),
            TokenKind::Print | TokenKind::PrintLn => Ok( Some( self.print( tokens )? ) ),
            TokenKind::Identifier( _ ) => Ok( Some( self.variable_reassignment_or_expression( tokens )? ) ),
            TokenKind::If => Ok( Some( self.if_statement( tokens )? ) ),
            TokenKind::Else => {
                tokens.next();
                Err( SyntaxError {
                    line_byte_start: current.line.byte_start,
                    line: current.line.number,
                    col: current.token.col,
                    len: current.token.kind.len(),
                    msg: "invalid if statement".into(),
                    help_msg: "stray else block".into()
                })
            },
            TokenKind::Loop => Ok( Some( self.loop_statement( tokens )? ) ),
            TokenKind::Break => {
                tokens.next();
                match self.loop_depth {
                    0 => Err( SyntaxError {
                        line_byte_start: current.line.byte_start,
                        line: current.line.number,
                        col: current.token.col,
                        len: current.token.kind.len(),
                        msg: "invalid break statement".into(),
                        help_msg: "cannot be used outside of loops".into()
                    }),
                    _ => {
                        self.semicolon( tokens )?;
                        Ok( Some( Node::Break ) )
                    },
                }
            },
            TokenKind::Continue => {
                tokens.next();
                match self.loop_depth {
                    0 => Err( SyntaxError {
                        line_byte_start: current.line.byte_start,
                        line: current.line.number,
                        col: current.token.col,
                        len: current.token.kind.len(),
                        msg: "invalid continue statement".into(),
                        help_msg: "cannot be used outside of loops".into()
                    }),
                    _ => {
                        self.semicolon( tokens )?;
                        Ok( Some( Node::Continue ) )
                    },
                }
            },
            TokenKind::Bracket( BracketKind::OpenCurly ) => {
                let new_scope = self.scopes.len();
                self.scopes.push( Scope {
                    parent: self.current_scope,
                    types: Vec::new(),
                    variables: Vec::new(),
                    nodes: Vec::new()
                } );
                self.current_scope = new_scope;

                tokens.next();
                self.parse_scope( tokens );
                Ok( Some( Node::Scope( new_scope ) ) )
            },
            TokenKind::Bracket( BracketKind::CloseCurly ) => {
                self.current_scope = self.scopes[ self.current_scope ].parent;
                tokens.next();
                Ok( None )
            },
            TokenKind::Bracket( BracketKind::CloseRound ) => {
                tokens.next();
                Err( SyntaxError {
                    line_byte_start: current.line.byte_start,
                    line: current.line.number,
                    col: current.token.col,
                    len: current.token.kind.len(),
                    msg: "invalid expression".into(),
                    help_msg: "stray closed parenthesis".into()
                })
            },
            TokenKind::Colon => {
                tokens.next();
                Err( SyntaxError {
                    line_byte_start: current.line.byte_start,
                    line: current.line.number,
                    col: current.token.col,
                    len: current.token.kind.len(),
                    msg: "invalid statement".into(),
                    help_msg: "stray colon".into()
                })
            },
            TokenKind::Equals => {
                tokens.next();
                Err( SyntaxError {
                    line_byte_start: current.line.byte_start,
                    line: current.line.number,
                    col: current.token.col,
                    len: current.token.kind.len(),
                    msg: "invalid assignment".into(),
                    help_msg: "stray assignment".into()
                })
            },
            TokenKind::QuestionMark => {
                tokens.next();
                Err( SyntaxError {
                    line_byte_start: current.line.byte_start,
                    line: current.line.number,
                    col: current.token.col,
                    len: current.token.kind.len(),
                    msg: "invalid explicit uninitialization".into(),
                    help_msg: "stray uninitialized value".into()
                })
            },
            TokenKind::Op( _ ) => {
                tokens.next();
                Err( SyntaxError {
                    line_byte_start: current.line.byte_start,
                    line: current.line.number,
                    col: current.token.col,
                    len: current.token.kind.len(),
                    msg: "invalid expression".into(),
                    help_msg: "stray binary operator".into()
                })
            },
            TokenKind::SemiColon => {
                tokens.next();
                Ok( Some( Node::Empty ) )
            },
            TokenKind::Comment( _ ) | TokenKind::Empty | TokenKind::Unexpected( _ ) => unreachable!(),
        }
    }
}

// semicolons
impl AST {
    fn semicolon( &mut self, tokens: &mut TokenCursor ) -> Result<(), SyntaxError> {
        let semicolon = tokens.current().bounded( tokens, "expected semicolon" )?;
        return match &semicolon.token.kind {
            // semicolons are optional if found before a closing curly bracket
            TokenKind::Bracket( BracketKind::CloseCurly ) => Ok( () ),
            TokenKind::SemiColon => {
                tokens.next();
                Ok( () )
            },
            _ => {
                tokens.next();
                Err( SyntaxError {
                    line_byte_start: semicolon.line.byte_start,
                    line: semicolon.line.number,
                    col: semicolon.token.col,
                    len: semicolon.token.kind.len(),
                    msg: "invalid statement".into(),
                    help_msg: "expected semicolon after this token".into(),
                } )
            }
        }
    }
}

// expressions
impl AST {
    // TODO disallow implicit conversions (str + i64, char + i64, str + char or str + str (maybe treat this as concatenation))
        // IDEA introduce casting operators
    fn factor( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
        let previous = tokens.peek_previous().unwrap();
        let current = tokens.current().bounded( tokens, "expected expression" )?;
        let factor = match &current.token.kind {
            TokenKind::Literal( literal ) => Ok( Expression::Literal( literal.clone() ) ),
            TokenKind::True => Ok( Expression::Literal( Literal::Bool( true ) ) ),
            TokenKind::False => Ok( Expression::Literal( Literal::Bool( false ) ) ),
            TokenKind::Identifier( name ) => match self.resolve_type( name ) {
                None => match self.resolve_variable( name ) {
                    Some( variable ) => match variable.initialized {
                        true => Ok( Expression::Identifier( name.clone(), variable.typ ) ),
                        false => Err( SyntaxError {
                            line_byte_start: current.line.byte_start,
                            line: current.line.number,
                            col: current.token.col,
                            len: current.token.kind.len(),
                            msg: "variable not initialized".into(),
                            help_msg: "was not previously initialized".into()
                        }),
                    },
                    None => Err( SyntaxError {
                        line_byte_start: current.line.byte_start,
                        line: current.line.number,
                        col: current.token.col,
                        len: current.token.kind.len(),
                        msg: "variable not defined".into(),
                        help_msg: "was not previously defined in this scope".into()
                    }),
                },
                Some( _ ) => Err( SyntaxError {
                    line_byte_start: current.line.byte_start,
                    line: current.line.number,
                    col: current.token.col,
                    len: current.token.kind.len(),
                    msg: "invalid expression".into(),
                    help_msg: "cannot be a type name".into()
                } ),
            },
            TokenKind::Bracket( BracketKind::OpenRound ) => {
                let expression_start_pos = tokens.next().bounded( tokens, "expected expression" )?;
                match expression_start_pos.token.kind {
                    TokenKind::Bracket( BracketKind::CloseRound ) => Err( SyntaxError {
                        line_byte_start: expression_start_pos.line.byte_start,
                        line: expression_start_pos.line.number,
                        col: expression_start_pos.token.col,
                        len: expression_start_pos.token.kind.len(),
                        msg: "invalid expression".into(),
                        help_msg: "empty expressions are not allowed".into()
                    }),
                    _ => {
                        let expression = self.expression( tokens )?;
                        let close_bracket_pos = tokens.current().bounded( tokens, "expected closed parenthesis" )?;
                        match close_bracket_pos.token.kind {
                            TokenKind::Bracket( BracketKind::CloseRound ) => Ok( expression ),
                            _ => Err( SyntaxError {
                                line_byte_start: current.line.byte_start,
                                line: current.line.number,
                                col: current.token.col,
                                len: current.token.kind.len(),
                                msg: "invalid expression".into(),
                                help_msg: "unclosed parenthesis".into()
                            }),
                        }
                    }
                }
            },
            TokenKind::Op( Operator::Minus ) => {
                let mut sign: isize = -1;
                while let Some( TokenPosition { token: Token { kind: TokenKind::Op( Operator::Minus ), .. }, .. } ) = tokens.next() {
                    sign *= -1;
                }

                let mut operand = self.factor( tokens )?;
                return match operand {
                    Expression::Literal( ref mut literal ) => match literal {
                        Literal::Int( ref mut value ) => {
                            *value = *value * sign;
                            Ok( operand )
                        },
                        Literal::Char( code ) => {
                            *literal = Literal::Int( (*code as isize) * sign );
                            Ok( operand )
                        },
                        Literal::Str( Str { text } ) => {
                            *literal = Literal::Int( (text.len() as isize) * sign );
                            Ok( operand )
                        },
                        Literal::Bool( _ ) => Err( SyntaxError {
                            line_byte_start: current.line.byte_start,
                            line: current.line.number,
                            col: current.token.col,
                            len: current.token.kind.len(),
                            msg: "invalid expression".into(),
                            help_msg: "cannot negate boolean value, use the '!' operator instead".into()
                        } ),
                        Literal::Uninitialized( _ ) => Err( SyntaxError {
                            line_byte_start: current.line.byte_start,
                            line: current.line.number,
                            col: current.token.col,
                            len: current.token.kind.len(),
                            msg: "variable not initialized".into(),
                            help_msg: "was not previously initialized".into()
                        } ),
                    }
                    _ => Ok( Expression::Unary { op: Operator::Negate, operand: Box::new( operand ) } ),
                }
            },
            TokenKind::Op( Operator::Not ) => {
                let mut should_be_inverted = true;
                while let Some( TokenPosition { token: Token { kind: TokenKind::Op( Operator::Not ), .. }, .. } ) = tokens.next() {
                    should_be_inverted = !should_be_inverted;
                }

                let mut operand = self.factor( tokens )?;
                return match operand {
                    Expression::Literal( ref mut literal ) => match literal {
                        Literal::Bool( ref mut value ) => {
                            if should_be_inverted {
                                *value = !*value;
                            }
                            Ok( operand )
                        },
                        Literal::Int( _ ) | Literal::Char( _ ) | Literal::Str( Str { .. } ) => Err( SyntaxError {
                            line_byte_start: current.line.byte_start,
                            line: current.line.number,
                            col: current.token.col,
                            len: current.token.kind.len(),
                            msg: "invalid expression".into(),
                            help_msg: "cannot invert non boolean value, use the '-' operator instead".into()
                        } ),
                        Literal::Uninitialized( _ ) => Err( SyntaxError {
                            line_byte_start: current.line.byte_start,
                            line: current.line.number,
                            col: current.token.col,
                            len: current.token.kind.len(),
                            msg: "variable not initialized".into(),
                            help_msg: "was not previously initialized".into()
                        } ),
                    }
                    _ => Ok( Expression::Unary { op: Operator::Not, operand: Box::new( operand ) } ),
                }
            },
            TokenKind::Definition( _ )
            | TokenKind::Print | TokenKind::PrintLn
            | TokenKind::If | TokenKind::Else
            | TokenKind::Loop | TokenKind::Break | TokenKind::Continue => Err( SyntaxError {
                line_byte_start: previous.line.byte_start,
                line: previous.line.number,
                col: previous.token.col,
                len: previous.token.kind.len(),
                msg: "invalid expression".into(),
                help_msg: "cannot be followed by a keyword".into()
            }),
            TokenKind::QuestionMark => Err( SyntaxError {
                line_byte_start: previous.line.byte_start,
                line: previous.line.number,
                col: previous.token.col,
                len: previous.token.kind.len(),
                msg: "invalid expression".into(),
                help_msg: "uninitialized values cannot be used inside expressions".into()
            }),
            _ => Err( SyntaxError {
                line_byte_start: previous.line.byte_start,
                line: previous.line.number,
                col: previous.token.col,
                len: previous.token.kind.len(),
                msg: "invalid expression".into(),
                help_msg: "expected expression operand after this token".into()
            }),
        };

        tokens.next();
        return factor;
    }

    fn operator( &mut self, tokens: &mut TokenCursor, ops: &[Operator] ) -> Result<Option<Operator>, SyntaxError> {
        let current_pos = tokens.current().bounded( tokens, "expected operator or semicolon" )?;
        return match current_pos.token.kind {
            TokenKind::Op( op ) => match ops.contains( &op ) {
                true => {
                    tokens.next();
                    Ok( Some( op ) )
                },
                false => Ok( None ),
            },
            _ => Ok( None ),
        }
    }

    // IDEA optimize expression building by implementing associativity rules (ie. remove parenthesis around additions and subtractions)
    // FIX division by zero, raising to a negative power
    // IDEA print crash error message
        // TODO implement a way to print file, line and column information in source code
    fn exponentiation( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
        let mut lhs = self.factor( tokens )?;

        while let Some( op ) = self.operator( tokens, &[Operator::Pow] )? {
            let rhs = self.factor( tokens )?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn multiplicative_expression( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
        let mut lhs = self.exponentiation( tokens )?;

        while let Some( op ) = self.operator( tokens, &[Operator::Times, Operator::Divide, Operator::Remainder] )? {
            let rhs = self.exponentiation( tokens )?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn additive_expression( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
        let mut lhs = self.multiplicative_expression( tokens )?;

        while let Some( op ) = self.operator( tokens, &[Operator::Plus, Operator::Minus] )? {
            let rhs = self.multiplicative_expression( tokens )?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn comparative_expression( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
        let mut lhs = self.additive_expression( tokens )?;

        let ops = [
            Operator::EqualsEquals, Operator::NotEquals,
            Operator::Greater, Operator::GreaterOrEquals,
            Operator::Less, Operator::LessOrEquals,
            Operator::Compare
        ];

        while let Some( op ) = self.operator( tokens, &ops )? {
            let rhs = self.additive_expression( tokens )?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    // TODO implement boolean operators for strings
    fn boolean_expression( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
        let mut lhs = self.comparative_expression( tokens )?;

        while let Some( op ) = self.operator( tokens, &[Operator::And, Operator::Or, Operator::Xor] )? {
            let op_pos = tokens.peek_previous().unwrap();

            if lhs.typ() != Type::Bool {
                return Err( SyntaxError {
                    line_byte_start: op_pos.line.byte_start,
                    line: op_pos.line.number,
                    col: op_pos.token.col,
                    len: op_pos.token.kind.len(),
                    msg: "invalid boolean expression".into(),
                    help_msg: "must be preceded by a boolean expression".into()
                });
            }

            let rhs = self.comparative_expression( tokens )?;
            if rhs.typ() != Type::Bool {
                return Err( SyntaxError {
                    line_byte_start: op_pos.line.byte_start,
                    line: op_pos.line.number,
                    col: op_pos.token.col,
                    len: op_pos.token.kind.len(),
                    msg: "invalid boolean expression".into(),
                    help_msg: "must be followed by a boolean expression".into()
                });
            }

            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn expression( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
        return self.boolean_expression( tokens );
    }
}

// variable definitions and assignments
impl<'lexer> AST {
    fn resolve_variable( &'lexer self, name: &str ) -> Option<&'lexer Variable> {
        let mut current_scope = self.current_scope;
        loop {
            let scope = &self.scopes[ current_scope ];

            for variable in &scope.variables {
                if variable.name == name {
                    return Some( variable );
                }
            }

            if current_scope == 0 && scope.parent == 0 {
                return None;
            }

            current_scope = scope.parent;
        }
    }

    fn resolve_variable_mut( &'lexer mut self, name: &str ) -> Option<&'lexer mut Variable> {
        let mut current_scope = self.current_scope;
        loop {
            let scope = &self.scopes[ current_scope ];

            let mut current_variable = 0;
            for variable in &scope.variables {
                if variable.name == name {
                    return Some( &mut self.scopes[ current_scope ].variables[ current_variable ] );
                }
                current_variable += 1;
            }

            if current_scope == 0 && scope.parent == 0 {
                return None;
            }

            current_scope = scope.parent;
        }
    }

    fn resolve_type( &'lexer self, name: &str ) -> Option<&'lexer Type> {
        let mut current_scope = self.current_scope;
        loop {
            let scope = &self.scopes[ current_scope ];

            for typ in &scope.types {
                if typ.to_string() == name {
                    return Some( typ );
                }
            }

            if current_scope == 0 && scope.parent == 0 {
                return None;
            }

            current_scope = scope.parent;
        }
    }


    fn variable_definition( &mut self, tokens: &mut TokenCursor ) -> Result<Node, SyntaxError> {
        let definition_pos = tokens.current().unwrap();
        let mutability = match definition_pos.token.kind {
            TokenKind::Definition( kind ) => kind,
            _ => unreachable!(),
        };

        let name_pos = tokens.next().bounded( tokens, "expected identifier" )?;
        let name = match &name_pos.token.kind {
            TokenKind::Identifier( name ) => match self.resolve_type( name ) {
                None => Ok( name.clone() ),
                Some( _ ) => Err( SyntaxError {
                    line_byte_start: name_pos.line.byte_start,
                    line: name_pos.line.number,
                    col: name_pos.token.col,
                    len: name_pos.token.kind.len(),
                    msg: "invalid variable name".into(),
                    help_msg: "cannot be a type name".into()
                } ),
            },
            _ => Err( SyntaxError {
                line_byte_start: name_pos.line.byte_start,
                line: name_pos.line.number,
                col: name_pos.token.col,
                len: name_pos.token.kind.len(),
                msg: "invalid assignment".into(),
                help_msg: "expected variable name".into()
            }),
        };

        let colon_pos = tokens.peek_next().bounded( tokens, "expected type annotation or variable definition" )?;
        let annotation = match &colon_pos.token.kind {
            TokenKind::Colon => {
                tokens.next();
                let annotation_pos = tokens.next().bounded( tokens, "expected type" )?;
                match &annotation_pos.token.kind {
                    TokenKind::Identifier( type_name ) => match self.resolve_type( type_name ) {
                        Some( typ ) => Ok( Some( (*typ, annotation_pos) ) ),
                        None => match self.resolve_variable( type_name ) {
                            Some( var ) => Ok( Some( (var.typ, annotation_pos) ) ),
                            None => Err( SyntaxError {
                                line_byte_start: annotation_pos.line.byte_start,
                                line: annotation_pos.line.number,
                                col: annotation_pos.token.col,
                                len: annotation_pos.token.kind.len(),
                                msg: "invalid type annotation".into(),
                                help_msg: "was not previously defined".into()
                            } )
                        },
                    },
                    // TokenKind::Equals => Ok( None ), // NOTE debate wether to allow for declarations like "var i := 0;"
                    _ => Err( SyntaxError {
                        line_byte_start: colon_pos.line.byte_start,
                        line: colon_pos.line.number,
                        col: colon_pos.token.col,
                        len: colon_pos.token.kind.len(),
                        msg: "invalid type annotation".into(),
                        help_msg: "expected type name after here".into()
                    } )
                }
            },
            _ => Ok( None ),
        };

        let equals_or_semicolon_pos = tokens.next().bounded( tokens, "expected equals" )?;
        let mut initialized = true;
        let expression = match equals_or_semicolon_pos.token.kind {
            TokenKind::Equals => {
                let expression_start = tokens.next().bounded( tokens, "expected expression" )?;
                match expression_start.token.kind {
                    TokenKind::QuestionMark => {
                        initialized = false;
                        tokens.next();
                        Ok( Some( Expression::Literal( Literal::Uninitialized( Type::Int /* placeholder */ ) ) ) )
                    },
                    _ => match self.expression( tokens ) {
                        Ok( expr ) => Ok( Some( expr ) ),
                        Err( err ) => Err( err ),
                    }
                }
            },
            TokenKind::SemiColon => Ok( None ),
            _ => Err( SyntaxError {
                line_byte_start: name_pos.line.byte_start,
                line: name_pos.line.number,
                col: name_pos.token.col,
                len: name_pos.token.kind.len(),
                msg: "invalid assignment".into(),
                help_msg: "expected '=' or ';' after the variable name".into()
            }),
        };

        let name = name?;
        let annotation = annotation?;
        let expression = expression?;
        self.semicolon( tokens )?;

        if let Some( _ ) = self.resolve_variable( &name ) {
            return Err( SyntaxError {
                line_byte_start: name_pos.line.byte_start,
                line: name_pos.line.number,
                col: name_pos.token.col,
                len: name_pos.token.kind.len(),
                msg: "variable redefinition".into(),
                help_msg: "was previously defined".into()
            } );
        }

        return match expression {
            Some( mut value ) => match annotation {
                Some( (typ, pos) ) => {
                    if typ != value.typ() {
                        return Err( SyntaxError {
                            line_byte_start: pos.line.byte_start,
                            line: pos.line.number,
                            col: pos.token.col,
                            len: pos.token.kind.len(),
                            msg: "invalid definition".into(),
                            help_msg: format!( "declared type of '{}' doesn't match expression of type '{}'", typ, value.typ() ).into()
                        })
                    }

                    if let Expression::Literal( Literal::Uninitialized( ref mut uninitialized_type ) ) = value {
                        *uninitialized_type = typ;
                    }

                    self.scopes[ self.current_scope ].variables.push( Variable { mutability, name: name.clone(), typ, initialized } );
                    Ok( Node::Definition( name, value ) )
                },
                None => match value {
                    Expression::Literal( Literal::Uninitialized( _ ) ) => Err( SyntaxError {
                        line_byte_start: name_pos.line.byte_start,
                        line: name_pos.line.number,
                        col: name_pos.token.col,
                        len: name_pos.token.kind.len(),
                        msg: "invalid definition".into(),
                        help_msg: "expected type annotation after here to infer the type of the variable".into()
                    }),
                    _ => {
                        self.scopes[ self.current_scope ].variables.push( Variable { mutability, name: name.clone(), typ: value.typ(), initialized } );
                        Ok( Node::Definition( name, value ) )
                    }
                },
            },
            None => match annotation {
                Some( (typ, _) ) => {
                    self.scopes[ self.current_scope ].variables.push( Variable { mutability, name: name.clone(), typ, initialized } );
                    Ok( Node::Definition( name, Expression::Literal( typ.default() ) ) )
                },
                None => Err( SyntaxError {
                    line_byte_start: name_pos.line.byte_start,
                    line: name_pos.line.number,
                    col: name_pos.token.col,
                    len: name_pos.token.kind.len(),
                    msg: "invalid definition".into(),
                    help_msg: "expected type annotation or value after here to infer the type of the variable".into()
                }),
            },
        }
    }

    fn variable_reassignment_or_expression( &mut self, tokens: &mut TokenCursor ) -> Result<Node, SyntaxError> {
        let name_pos = tokens.current().unwrap();
        let op_pos = match tokens.peek_next() {
            Some( pos ) => match pos.token.kind {
                TokenKind::Equals
                | TokenKind::Op( Operator::PowEquals )
                | TokenKind::Op( Operator::TimesEquals )
                | TokenKind::Op( Operator::DivideEquals )
                | TokenKind::Op( Operator::PlusEquals )
                | TokenKind::Op( Operator::MinusEquals ) => pos,
                _ => return Ok( Node::Expression( self.expression( tokens )? ) ),
            },
            None => return Ok( Node::Expression( self.expression( tokens )? ) ),
        };

        tokens.next();
        tokens.next().bounded( tokens, "expected expression" )?;

        let name = name_pos.token.kind.to_string();
        let rhs = self.expression( tokens )?;
        let value = match &op_pos.token.kind {
            TokenKind::Equals => Ok( rhs ),
            TokenKind::Op( op ) => {
                let lhs = Expression::Identifier( name.clone(), op.typ() );
                Ok( Expression::Binary { lhs: Box::new( lhs ), op: *op, rhs: Box::new( rhs ) } )
            },
            _ => unreachable!(),
        };

        self.semicolon( tokens )?;

        if let Some( _ ) = self.resolve_type( &name ) {
            return Err( SyntaxError {
                line_byte_start: name_pos.line.byte_start,
                line: name_pos.line.number,
                col: name_pos.token.col,
                len: name_pos.token.kind.len(),
                msg: "invalid assignment".into(),
                help_msg: "cannot be a type name".into()
            } )
        }

        match self.resolve_variable_mut( &name ) {
            Some( Variable { mutability: Mutability::Let, .. } ) => Err( SyntaxError {
                line_byte_start: name_pos.line.byte_start,
                line: name_pos.line.number,
                col: name_pos.token.col,
                len: name_pos.token.kind.len(),
                msg: "invalid assignment".into(),
                help_msg: "was defined as immutable".into()
            }),
            Some( var ) => {
                let value = value?;
                let value_typ = value.typ();

                if var.typ != value_typ {
                    return Err( SyntaxError {
                        line_byte_start: name_pos.line.byte_start,
                        line: name_pos.line.number,
                        col: name_pos.token.col,
                        len: name_pos.token.kind.len(),
                        msg: "mismatched types".into(),
                        help_msg: format!(
                            "trying to assign an expression of type '{}' to a variable of type '{}'",
                            value_typ,
                            var.typ,
                        ).into()
                    });
                }

                var.initialized = true;
                Ok( Node::Assignment( name_pos.token.kind.to_string(), value ) )
            },
            None => Err( SyntaxError {
                line_byte_start: name_pos.line.byte_start,
                line: name_pos.line.number,
                col: name_pos.token.col,
                len: name_pos.token.kind.len(),
                msg: "variable redefinition".into(),
                help_msg: "was not previously defined in this scope".into()
            }),
        }
    }
}

// print statements
impl AST {
    fn print( &mut self, tokens: &mut TokenCursor ) -> Result<Node, SyntaxError> {
        let print_pos = tokens.current().unwrap();
        match print_pos.token.kind {
            TokenKind::PrintLn => if let Some( TokenPosition { token: &Token { kind: TokenKind::SemiColon, .. }, .. } ) = tokens.peek_next() {
                tokens.next();
                return Ok( Node::Println( None ) );
            },
            TokenKind::Print => (),
            _ => unreachable!(),
        }

        tokens.next().bounded( tokens, "expected expression" )?;
        let argument = self.expression( tokens )?;
        self.semicolon( tokens )?;

        return match print_pos.token.kind {
            TokenKind::Print => Ok( Node::Print( argument ) ),
            TokenKind::PrintLn => Ok( Node::Println( Some( argument ) ) ),
            _ => unreachable!(),
        }
    }
}

// if statements
impl AST {
    fn if_statement( &mut self, tokens: &mut TokenCursor ) -> Result<Node, SyntaxError> {
        let iff = self.iff( tokens )?;
        let mut if_statement = If { ifs: vec![iff], els: None };
        self.els( &mut if_statement, tokens )?;

        return Ok( Node::If( if_statement ) );
    }

    fn iff( &mut self, tokens: &mut TokenCursor ) -> Result<IfStatement, SyntaxError> {
        let if_pos = tokens.current().unwrap();
        tokens.next().bounded( tokens, "expected boolean expression" )?;

        let expression = self.expression( tokens )?;
        let condition = match &expression.typ() {
            Type::Bool => Ok( expression ),
            Type::Char | Type::Int | Type::Str => Err( SyntaxError {
                line_byte_start: if_pos.line.byte_start,
                line: if_pos.line.number,
                col: if_pos.token.col,
                len: if_pos.token.kind.len(),
                msg: "expected boolean expression".into(),
                help_msg: "must be followed by a boolean expression".into()
            }),
        };

        let block_or_statement_pos = tokens.current().bounded( tokens, "expected colon or curly bracket" )?;
        return match block_or_statement_pos.token.kind {
            TokenKind::Bracket( BracketKind::OpenCurly ) => {
                let condition = condition?;
                let scope = self.parse_single( tokens )?.unwrap();
                Ok( IfStatement { condition, statement: scope } )
            },
            TokenKind::Colon => {
                let condition = condition?;
                tokens.next().bounded( tokens, "expected statement" )?;
                match self.parse_single( tokens )? {
                    Some( statement ) => Ok( IfStatement { condition, statement } ),
                    None => Err( SyntaxError {
                        line_byte_start: block_or_statement_pos.line.byte_start,
                        line: block_or_statement_pos.line.number,
                        col: block_or_statement_pos.token.col,
                        len: block_or_statement_pos.token.kind.len(),
                        msg: "invalid else statement".into(),
                        help_msg: "must be followed by a statement".into()
                    })
                }
                // since we checked for the presence of a token after this point we can safely unwrap
            },
            _ => {
                let before_curly_bracket_pos = tokens.peek_previous().unwrap();
                Err( SyntaxError {
                    line_byte_start: before_curly_bracket_pos.line.byte_start,
                    line: before_curly_bracket_pos.line.number,
                    col: before_curly_bracket_pos.token.col,
                    len: before_curly_bracket_pos.token.kind.len(),
                    msg: "expected block scope".into(),
                    help_msg: "must be followed by an opened curly bracket or a colon".into()
                })
            },
        }
    }

    fn els( &mut self, if_statement: &mut If, tokens: &mut TokenCursor ) -> Result<(), SyntaxError> {
        let els_pos = match tokens.current() {
            Some( pos ) => match pos.token.kind {
                TokenKind::Else => pos,
                _ => return Ok( () ),
            },
            None => return Ok( () )
        };

        let if_or_block_pos = tokens.next().bounded( tokens, "expected colon, block or if statement" )?;
        return match if_or_block_pos.token.kind {
            TokenKind::Bracket( BracketKind::OpenCurly ) => {
                let scope = self.parse_single( tokens )?.unwrap();
                if_statement.els = Some( Box::new( scope ) );
                Ok( () )
            },
            TokenKind::Colon => {
                tokens.next().bounded( tokens, "expected statement" )?;
                match self.parse_single( tokens )? {
                    Some( statement ) => {
                        if_statement.els = Some( Box::new( statement ) );
                        Ok( () )
                    },
                    None => Err( SyntaxError {
                        line_byte_start: if_or_block_pos.line.byte_start,
                        line: if_or_block_pos.line.number,
                        col: if_or_block_pos.token.col,
                        len: if_or_block_pos.token.kind.len(),
                        msg: "invalid else statement".into(),
                        help_msg: "must be followed by a statement".into()
                    })
                }
            },
            TokenKind::If => {
                let else_if = self.iff( tokens )?;
                if_statement.ifs.push( else_if );

                self.els( if_statement, tokens )
            },
            _ => Err( SyntaxError {
                line_byte_start: els_pos.line.byte_start,
                line: els_pos.line.number,
                col: els_pos.token.col,
                len: els_pos.token.kind.len(),
                msg: "invalid else statement".into(),
                help_msg: "expected block of if statement after this token".into()
            }),
        }
    }
}

// for statements
impl AST {
    fn loop_statement( &mut self, tokens: &mut TokenCursor ) -> Result<Node, SyntaxError> {
        self.loop_depth += 1;
        let iff = self.iff( tokens );
        self.loop_depth -= 1;

        let iff = iff?;
        return Ok( Node::Loop( Loop {
            pre: None,
            condition: Some( iff.condition ),
            post: None,
            statement: Box::new( iff.statement )
        } ) );
    }
}


struct Checker;

impl Checker {
    fn check( src: BlitzSrc, start_time: &Instant ) -> Result<AST, SyntaxErrors> {
        eprint!( "{}: {}", CHECKING, src.path.display() );
        let mut time_info = String::new();

        let lexer = Lexer::try_from( src );
        let lexing_time = start_time.elapsed();
        let elapsed_lexing = Colored {
            text: format!( "{}s", lexing_time.as_secs_f64() ).into(),
            foreground: Foreground::LightGray,
            ..Default::default()
        };
        time_info.push_str( &format!( "lexing: {}", elapsed_lexing ) );

        let lexer = match lexer {
            Ok( lexer ) => lexer,
            Err( err ) => {
                eprintln!( " ... in [{}]", time_info );
                return Err( err );
            },
        };

        let ast = AST::try_from( lexer );
        let ast_time = start_time.elapsed() - lexing_time;
        let elapsed_ast = Colored {
            text: format!( "{}s", ast_time.as_secs_f64() ).into(),
            foreground: Foreground::LightGray,
            ..Default::default()
        };
        time_info.push_str( &format!( ", parsing: {}", elapsed_ast ) );

        let elapsed_total = Colored {
            text: format!( "{}s", start_time.elapsed().as_secs_f64() ).into(),
            foreground: Foreground::LightGray,
            ..Default::default()
        };

        eprintln!( " ... in {} [{}]", elapsed_total, time_info );
        return ast;
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
    src_path: PathBuf,
    out_path: Option<PathBuf>,

    ast: &'ast AST,

    rodata: String,
    asm: String,

    variables: Vec<CompilerVariable>,
    strings: Vec<StringLabel<'ast>>,

    if_idx: usize,
    loop_idx: usize,
    loop_idx_stack: Vec<usize>,
}

// generation of compilation artifacts (.asm, .o, executable)
impl Compiler<'_> {
    const STACK_ALIGN: usize = core::mem::size_of::<usize>();

    fn compile( &mut self, start_time: &Instant ) -> std::io::Result<PathBuf> {
        eprint!( "{}: {}", COMPILING, self.src_path.display() );
        let mut time_info = String::new();

        let (asm_path, obj_path, exe_path) = if let Some( out_path ) = &self.out_path {
            match std::fs::create_dir_all( &out_path ) {
                Ok( _ ) => (),
                Err( err ) if err.kind() == ErrorKind::AlreadyExists => (),
                Err( err ) => {
                    let out_creation_time = start_time.elapsed();
                    let elapsed_out_creation = Colored {
                        text: format!( "{}s", out_creation_time.as_secs_f64() ).into(),
                        foreground: Foreground::LightGray,
                        ..Default::default()
                    };
                    time_info.push_str( &format!( "asm generation: {}", elapsed_out_creation ) );

                    eprintln!( " ... in [{}]", time_info );
                    return Err( std::io::Error::new(
                        err.kind(),
                        format!( "{}: could not create output path '{}'\n{}: {}", ERROR, out_path.display(), CAUSE, err )
                    ) );
                }
            }

            (out_path.join( self.src_path.with_extension( "asm" ).file_name().unwrap() ),
            out_path.join( self.src_path.with_extension( "o" ).file_name().unwrap() ),
            out_path.join( self.src_path.with_extension( "" ).file_name().unwrap() ))
        }
        else {
            (self.src_path.with_extension( "asm" ),
            self.src_path.with_extension( "o" ),
            self.src_path.with_extension( "" ))
        };

        let asm_file = match File::create( &asm_path ) {
            Ok( file ) => file,
            Err( err ) => {
                let asm_file_opening_time = start_time.elapsed();
                let elapsed_asm_file_opening = Colored {
                    text: format!( "{}s", asm_file_opening_time.as_secs_f64() ).into(),
                    foreground: Foreground::LightGray,
                    ..Default::default()
                };
                time_info.push_str( &format!( "asm generation: {}", elapsed_asm_file_opening ) );

                eprintln!( " ... in [{}]", time_info );
                return Err( std::io::Error::new(
                    err.kind(),
                    format!( "{}: could not create file '{}'\n{}: {}", ERROR, asm_path.display(), CAUSE, err )
                ) );
            },
        };

        let mut asm_writer = BufWriter::new( asm_file );

        self.rodata +=
r#" stdout: equ 1
 SYS_write: equ 1
 SYS_exit: equ 60
 EXIT_SUCCESS: equ 0

 INT_MIN: equ 1 << 63
 INT_MAX: equ ~INT_MIN
 INT_BITS: equ 64

 true: equ 1
 true_str: db "true"
 true_str_len: equ $ - true_str

 false: equ 0
 false_str: db "false"
 false_str_len: equ $ - false_str

 LESS: equ -1
 EQUAL: equ 0
 GREATER: equ 1

 INT_STR_LEN: equ INT_BITS"#;

        let mut stack_size = 0;
        let mut variables: Vec<(Type, Vec<&Variable>)> = Vec::new();
        for scope in &self.ast.scopes {
            for variable in &scope.variables {
                let typ = &variable.typ;

                let mut type_already_encountered = false;
                for var_info in &mut variables {
                    if *typ == var_info.0 {
                        var_info.1.push( variable );
                        type_already_encountered = true;
                        break;
                    }
                }

                if !type_already_encountered {
                    variables.push( (variable.typ, vec![variable]) );
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
 int_str: times INT_STR_LEN db 0

section .text
int_toStr:
 push rcx

 mov rsi, 10
 mov rcx, int_str + INT_STR_LEN - 1

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
 mov rdx, int_str + INT_STR_LEN
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

        if let Err( err ) = asm_writer.write_all( program.as_bytes() ) {
            let asm_writing_time = start_time.elapsed();
            let elapsed_asm_writing = Colored {
                text: format!( "{}s", asm_writing_time.as_secs_f64() ).into(),
                foreground: Foreground::LightGray,
                ..Default::default()
            };
            time_info.push_str( &format!( "asm generation: {}", elapsed_asm_writing ) );

            eprintln!( " ... in [{}]", time_info );
            return Err( std::io::Error::new(
                err.kind(),
                format!( "{}: writing assembly file failed\n{}: {}", ERROR, CAUSE, err )
            ) );
        }

        if let Err( err ) = asm_writer.flush() {
            let asm_flushin_time = start_time.elapsed();
            let elapsed_asm_flushin = Colored {
                text: format!( "{}s", asm_flushin_time.as_secs_f64() ).into(),
                foreground: Foreground::LightGray,
                ..Default::default()
            };
            time_info.push_str( &format!( "asm generation: {}", elapsed_asm_flushin ) );

            eprintln!( " ... in [{}]", time_info );
            return Err( std::io::Error::new(
                err.kind(),
                format!( "{}: writing assembly file failed\n{}: {}", ERROR, CAUSE, err )
            ) );
        }

        let asm_generation_time = start_time.elapsed();
        let elapsed_asm_generation = Colored {
            text: format!( "{}s", asm_generation_time.as_secs_f64() ).into(),
            foreground: Foreground::LightGray,
            ..Default::default()
        };
        time_info.push_str( &format!( "asm generation: {}", elapsed_asm_generation ) );


        let nasm_args = ["-felf64", "-gdwarf", asm_path.to_str().unwrap(), "-o", obj_path.to_str().unwrap()];
        match Command::new( "nasm" ).args( nasm_args ).output() {
            Ok( nasm_out ) => if !nasm_out.status.success() {
                eprintln!( " ... in [{}]", time_info );
                return Err( std::io::Error::new(
                    ErrorKind::InvalidData,
                    format!( "{}: nasm assembler failed\n{}: {}", ERROR, CAUSE, String::from_utf8_lossy( &nasm_out.stderr ) )
                ) );
            },
            Err( err ) => {
                eprintln!( " ... in [{}]", time_info );
                return Err( std::io::Error::new(
                    err.kind(),
                    format!( "{}: could not create nasm assembler process\n{}: {}", ERROR, CAUSE, err )
                ) );
            },
        }

        let nasm_time = start_time.elapsed() - asm_generation_time;
        let elapsed_nasm = Colored {
            text: format!( "{}s", nasm_time.as_secs_f64() ).into(),
            foreground: Foreground::LightGray,
            ..Default::default()
        };
        time_info.push_str( &format!( ", assembler: {}", elapsed_nasm ) );


        let ld_args = [obj_path.to_str().unwrap(), "-o", exe_path.to_str().unwrap()];
        match Command::new( "ld" ).args( ld_args ).output() {
            Ok( ld_out ) => if !ld_out.status.success() {
                eprintln!( " ... in [{}]", time_info );
                return Err( std::io::Error::new(
                    ErrorKind::InvalidData,
                    format!( "{}: ld linker failed\n{}: {}", ERROR, CAUSE, String::from_utf8_lossy( &ld_out.stderr ) )
                ) );
            },
            Err( err ) => {
                eprintln!( " ... in [{}]", time_info );
                return Err( std::io::Error::new(
                    err.kind(),
                    format!( "{}: could not create ld linker process\n{}: {}", ERROR, CAUSE, err )
                ) );
            },
        };

        let ld_time = start_time.elapsed() - nasm_time;
        let elapsed_ld = Colored {
            text: format!( "{}s", ld_time.as_secs_f64() ).into(),
            foreground: Foreground::LightGray,
            ..Default::default()
        };
        time_info.push_str( &format!( ", linker: {}", elapsed_ld ) );


        let elapsed_total = Colored {
            text: format!( "{}s", start_time.elapsed().as_secs_f64() ).into(),
            foreground: Foreground::LightGray,
            ..Default::default()
        };

        eprintln!( " ... in {} [{}]", elapsed_total, time_info );
        return Ok( exe_path );
    }

    fn run( &mut self, exe_path: &Path ) -> std::io::Result<()> {
        eprintln!( "{}: {}", RUNNING, exe_path.display() );

        return match Command::new( Path::new( "." ).join( exe_path ).display().to_string() ).spawn() {
            Ok( mut blitz ) => match blitz.wait() {
                Ok( _status ) => Ok( () ),
                Err( err ) => Err( std::io::Error::new(
                    err.kind(),
                    format!( "{}: could not run blitz executable\n{}: {}", ERROR, CAUSE, err )
                ) ),
            },
            Err( err ) => Err( std::io::Error::new(
                err.kind(),
                format!( "{}: could not create blitz executable process\n{}: {}", ERROR, CAUSE, err )
            ) ),
        }
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
            if std::ptr::eq( string, string_label.string ) {
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
    fn compile_node( &mut self, node: &'ast Node ) {
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
            Node::Println( argument ) => {
                self.asm += &format!( " ; {}\n", node );
                if let Some( arg ) = argument {
                    self.compile_expression( arg );

                    match arg.typ() {
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
                }

                self.asm +=
                    " push 10\
                    \n mov rdi, stdout\
                    \n mov rsi, rsp\
                    \n mov rdx, 1\
                    \n mov rax, SYS_write\
                    \n syscall\
                    \n pop rsi\n\n"
            },
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

                        self.compile_if( else_if, &else_if_tag, &else_if_false_tag );
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
                if let Some( els ) = &if_statement.els {
                    self.asm += &format!( "if_{}_else:\n", if_idx );
                    match &**els {
                        Node::Scope( scope ) => self.compile_scope( *scope ),
                        other => self.compile_node( other ),
                    }
                }

                self.asm += &format!( "if_{}_end:\n", if_idx );
            },
            Node::Loop( looop ) => {
                let loop_tag = format!( "loop_{}", self.loop_idx );
                let loop_end_tag = format!( "loop_{}_end", self.loop_idx );

                self.loop_idx_stack.push( self.loop_idx );
                self.loop_idx += 1;
                self.compile_loop( looop, &loop_tag, &loop_end_tag );
                self.loop_idx_stack.pop();

                self.asm += &format!(
                    " jmp {}\
                    \n{}:\n\n",
                    loop_tag,
                    loop_end_tag
                );
            },
            Node::Definition( name, value ) => self.compile_assignment( name, value ),
            Node::Assignment( name, value ) => self.compile_assignment( name, value ),
            Node::Scope( inner ) => self.compile_scope( *inner ),
            Node::Expression( expression ) => self.compile_expression( expression ),
            Node::Break => self.asm += &format!( " jmp loop_{}_end\n\n", self.loop_idx_stack.last().unwrap() ),
            Node::Continue => self.asm += &format!( " jmp loop_{}\n\n", self.loop_idx_stack.last().unwrap() ),
            Node::Empty => (/* do nothing */),
        }
    }

    fn compile_scope( &mut self, scope_idx: usize ) {
        let scope = &self.ast.scopes[ scope_idx ];
        for node in &scope.nodes {
            self.compile_node( node );
        }
    }


    fn compile_expression( &mut self, expression: &'ast Expression ) {
        match expression {
            Expression::Literal( Literal::Int( value ) ) => self.asm += &format!( " mov rdi, {}\n", value ),
            Expression::Literal( Literal::Char( code ) ) => self.asm += &format!( " mov rdi, {}\n", code ),
            Expression::Literal( Literal::Bool( value ) ) => self.asm += &format!( " mov rdi, {}\n", value ),
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
            Expression::Literal( Literal::Uninitialized( _ ) ) => unreachable!(),
            Expression::Binary { .. } => self.compile_expression_factor( expression, Register::RDI ),
            Expression::Identifier( src_name, _ ) => {
                let src_variable = self.resolve( src_name );
                let src_variable_typ = &src_variable.typ;
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
            Expression::Unary { op, operand } => {
                self.compile_expression( operand );
                match op {
                    Operator::Negate => self.asm += " neg rdi\n",
                    Operator::Not => self.asm += " xor dil, 1\n",
                    _ => unreachable!()
                }
            },
        }
    }

    // IDEA make this return the place of where to find the result of the operation
    fn compile_expression_factor( &mut self, factor: &'ast Expression, dst: Register ) {
        match factor {
            Expression::Literal( Literal::Int( value ) ) => self.asm += &format!( " mov {}, {}\n", dst, value ),
            Expression::Literal( Literal::Char( code ) ) => self.asm += &format!( " mov {}, {}\n", dst, code ),
            Expression::Literal( Literal::Bool( value ) ) => self.asm += &format!( " mov {}, {}\n", dst, value ),
            Expression::Literal( Literal::Str( string ) ) => {
                let string_label_idx = self.string_label_idx( string );
                let string_label = &self.strings[ string_label_idx ];

                self.asm += &format!( " mov {}, {}\n", dst, string_label.len_label );
            },
            Expression::Literal( Literal::Uninitialized( _ ) ) => unreachable!(),
            // TODO find way to avoiding compiling the move to a support register if the rhs operand is a literal
                // IDEA optimize increments
                // IDEA optimize checking for even values by testing the least significant bit (e.g. test rax, 1)
            // IDEA optimize check for divisibility by 2 (i.e. even or odd)
            Expression::Binary { lhs, op, rhs } => {
                let (lhs_reg, rhs_reg, op_asm) = match op {
                    Operator::Pow | Operator::PowEquals => match &**rhs {
                        Expression::Literal( Literal::Int( 2 ) ) => (Register::RDI, Register::RSI,
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
                    Operator::EqualsEquals =>
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

                    // FIX shortcircuit boolean operators
                    Operator::And => (Register::RDI, Register::RSI, " and rdi, rsi\n"),
                    Operator::Or => (Register::RDI, Register::RSI, " or rdi, rsi\n" ),
                    Operator::Xor => (Register::RDI, Register::RSI, " xor rdi, rsi\n" ),

                    Operator::Not | Operator::Negate => unreachable!(),
                };

                match &**rhs {
                    Expression::Binary { .. } | Expression::Unary { .. } => {
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
            Expression::Unary { op, operand } => {
                self.compile_expression( operand );
                match op {
                    Operator::Negate => self.asm += " neg rdi\n",
                    Operator::Not => self.asm += " xor dil, 1\n",
                    _ => unreachable!()
                }
            },
        }
    }


    fn compile_if( &mut self, iff: &'ast IfStatement, if_tag: &String, if_false_tag: &String ) {
        self.asm += &format!( "{}:; {}\n", if_tag, iff.condition );

        match &iff.condition {
            Expression::Literal( Literal::Bool( value ) ) =>
                self.asm += &format!(
                    " mov dil, {}\
                    \n cmp dil, true\
                    \n jne {}\n\n",
                    *value as usize,
                    if_false_tag
                ),
            Expression::Binary { lhs, op, rhs } => {
                match &**rhs {
                    Expression::Binary { .. } | Expression::Unary { .. }=> {
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
                    | Operator::Compare
                    | Operator::Not | Operator::Negate => unreachable!(),
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
            Expression::Unary { operand, .. } => {
                self.compile_expression( operand );

                // we can only have boolean expressions at this point, so it's safe to ignore the integer negation case
                self.asm += &format!(
                    " xor dil, 1\
                    \n jz {}\n\n",
                    if_false_tag
                );
            },
            Expression::Literal( _ ) => unreachable!(),
        }

        self.compile_node( &iff.statement );
    }

    fn compile_loop( &mut self, looop: &'ast Loop, loop_tag: &String, loop_false_tag: &String ) {
        if let Some( condition ) = &looop.condition {
            self.asm += &format!( "{}:; {}\n", loop_tag, condition );

            match condition {
                Expression::Literal( Literal::Bool( value ) ) =>
                    self.asm += &format!(
                        " mov dil, {}\
                        \n cmp dil, true\
                        \n jne {}\n\n",
                        *value as usize,
                        loop_false_tag
                    ),
                Expression::Binary { lhs, op, rhs } => {
                    match &**rhs {
                        Expression::Binary { .. } | Expression::Unary { .. } => {
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
                        Operator::EqualsEquals => self.asm += &format!( " cmp rdi, rsi\n jne {}\n\n", loop_false_tag ),
                        Operator::NotEquals => self.asm += &format!( " cmp rdi, rsi\n je {}\n\n", loop_false_tag ),
                        Operator::Greater => self.asm += &format!( " cmp rdi, rsi\n jle {}\n\n", loop_false_tag ),
                        Operator::GreaterOrEquals => self.asm += &format!( " cmp rdi, rsi\n jl {}\n\n", loop_false_tag ),
                        Operator::Less => self.asm += &format!( " cmp rdi, rsi\n jge {}\n\n", loop_false_tag ),
                        Operator::LessOrEquals => self.asm += &format!( " cmp rdi, rsi\n jg {}\n\n", loop_false_tag ),
                        Operator::And => self.asm += &format!( " and rdi, rsi\n jz {}\n\n", loop_false_tag ),
                        Operator::Or => self.asm += &format!( " or rdi, rsi\n jz {}\n\n", loop_false_tag ),
                        Operator::Xor => self.asm += &format!( " xor rdi, rsi\n jz {}\n\n", loop_false_tag ),

                        Operator::Pow | Operator::PowEquals
                        | Operator::Times | Operator::TimesEquals
                        | Operator::Divide | Operator::DivideEquals
                        | Operator::Remainder | Operator::RemainderEquals
                        | Operator::Plus | Operator::PlusEquals
                        | Operator::Minus | Operator::MinusEquals
                        | Operator::Compare
                        | Operator::Not | Operator::Negate => unreachable!(),
                    };

                },
                Expression::Identifier( src_name, _ ) => {
                    let src_variable = self.resolve( src_name );
                    self.asm += &format!(
                        " mov dil, [rbp + {}]\
                        \n cmp dil, true\
                        \n jne {}\n\n",
                        src_variable.offset,
                        loop_false_tag
                    )
                },
                Expression::Unary { operand, .. } => {
                    self.compile_expression( operand );

                    // we can only have boolean expressions at this point, so it's safe to ignore the integer negation case
                    self.asm += &format!(
                        " xor dil, 1\
                        \n jz {}\n\n",
                        loop_false_tag
                    );
                },
                Expression::Literal( _ ) => unreachable!(),
            }
        }

        self.compile_node( &looop.statement );
    }


    fn compile_assignment( &mut self, name: &String, new_value: &'ast Expression ) {
        let variable = self.resolve( name );
        let variable_typ = variable.typ;
        let variable_offset = variable.offset;
        self.asm += &format!( " ; {} = {}\n", name, new_value );

        match new_value {
            Expression::Literal( Literal::Int( value ) ) =>
                self.asm += &format!(
                    " mov rdi, {}\
                    \n mov [rbp + {}], rdi\n\n",
                    value, variable_offset
                ),
            Expression::Literal( Literal::Char( code ) ) =>
                self.asm += &format!( " mov byte [rbp + {}], {}\n\n", variable_offset, code ),
            Expression::Literal( Literal::Bool( value ) ) =>
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
            /* leave blank, possibility to get garbage values */
            Expression::Literal( Literal::Uninitialized( _ ) ) => self.asm += "\n",
            Expression::Binary { .. } => {
                self.compile_expression( new_value );

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
            Expression::Unary { op, operand } => {
                self.compile_expression( operand );
                match op {
                    Operator::Negate => self.asm += " neg rdi\n",
                    Operator::Not => self.asm += " xor dil, 1\n",
                    _ => unreachable!()
                }

                match variable_typ {
                    Type::Int | Type::Str => self.asm += &format!( " mov [rbp + {}], rdi\n\n", variable_offset ),
                    Type::Char | Type::Bool => self.asm += &format!( " mov [rbp + {}], dil\n\n", variable_offset ),
                }
            },
        }
    }
}


// IDEA make the source generic, eg: to be able to compile from strings instead of just files
#[derive( Debug )]
struct BlitzSrc {
    path: PathBuf,
    src: BufReader<File>,
}

impl BlitzSrc {
    fn try_from( path: &Path ) -> std::io::Result<Self> {
        return match File::open( path ) {
            Ok( file ) => match file.metadata() {
                Ok( metadata ) => match metadata.is_file() {
                    true => Ok( Self { path: PathBuf::from( path ), src: BufReader::with_capacity( 1, file ) } ),
                    false => Err( std::io::Error::new(
                        ErrorKind::InvalidInput,
                        format!( "{}: invalid path '{}'\n{}: expected a file but got a directory", ERROR, path.display(), CAUSE )
                    ) ),
                },
                Err( err ) => Err( std::io::Error::new(
                    err.kind(),
                    format!( "{}: could not read metadata of '{}'\n{}: {}", ERROR, path.display(), CAUSE, err )
                ) ),
            },
            Err( err ) => Err( std::io::Error::new(
                err.kind(),
                format!( "{}: could not open '{}'\n{}: {}", ERROR, path.display(), CAUSE, err )
            ) ),
        }
    }
}

enum RunModeKind {
    Check,
    Compile { out_path: Option<PathBuf>, run: bool },
}

struct RunMode {
    src_path: PathBuf,
    kind: RunModeKind,
}

struct Blitz;

impl Blitz {
    fn print_usage( color: Color ) {
        Self::print_version( color );

        println!( r"
Usage: blitz [{OPTIONS}] [{RUN_MODE}]

{OPTIONS}:
    -h, --help            Display this message
    -v, --version         Display the compiler version
    -c, --color <{MODE}>    Wether to display colored output ({MODE}: auto (default), never, always)

{RUN_MODE}:
    check   <{FILE}>              Check the source code for correctness
    compile <{FILE}> [{OUTPUT}]     Compile the source code down to an executable
    run     <{FILE}> [{OUTPUT}]     Compile and run the generated executable

{OUTPUT}:
    -o, --output <{PATH}>       Folder to populate with compilation artifacts (.asm, .o, executable) (default: '.')"
        );
    }

    fn print_version( color: Color ) {
        color.set_stdout();
        println!( "Blitzlang compiler, version {}", VERSION );
    }

    fn from( args: Args ) -> ExitCode {
        #[allow(unused_mut)]
        let mut args = args.collect::<Vec<String>>();
        let mut current_arg = 1; // starting at 1 to skip the name of this executable
        // to quickly debug
        // args.push( "-c".to_string() );
        // args.push( "never".to_string() );
        // args.push( "-v".to_string() );
        // args.push( "-h".to_string() );
        // args.push( "run".to_string() );
        // args.push( "examples/features_test.blz".to_string() );
        // args.push( "-o".to_string() );
        // args.push( "examples/out".to_string() );
        // args.push( "compile".to_string() );
        // args.push( "examples/features_test.blz".to_string() );


        // looking for color modes flags
        let mut color_mode: Option<Color> = None;

        while current_arg < args.len() {
            let arg = &args[ current_arg ];
            if arg == "-c" || arg == "--color" {
                if let Some( _ ) = color_mode {
                    eprintln!( "{}: color mode already selected", ERROR );
                    return ExitCode::FAILURE;
                }

                current_arg += 1;
                if current_arg >= args.len() {
                    eprintln!( "{}: expected color mode", ERROR );
                    return ExitCode::FAILURE;
                }

                color_mode = match args[ current_arg ].as_str() {
                    "auto" => Some( Color::Auto ),
                    "always" => Some( Color::Always ),
                    "never" => Some( Color::Never ),
                    _ => {
                        eprintln!( "{}: unrecognized color mode", ERROR );
                        return ExitCode::FAILURE;
                    },
                };
            }

            current_arg += 1;
        }

        let color_mode = match color_mode {
            Some( mode ) => mode,
            None => Color::Auto,
        };
        color_mode.set();


        // looking for help commands
        // printing help message when no arguments were provided
        if args.len() < 2 {
            Self::print_usage( color_mode );
            return ExitCode::SUCCESS;
        }

        current_arg = 1;
        while current_arg < args.len() {
            let arg = &args[ current_arg ];
            if arg == "-h" || arg == "--help" {
                Self::print_usage( color_mode );
                return ExitCode::SUCCESS;
            }

            current_arg += 1;
        }


        // looking for version commands
        current_arg = 1;
        while current_arg < args.len() {
            let arg = &args[ current_arg ];
            if arg == "-v" || arg == "--version" {
                Self::print_version( color_mode );
                return ExitCode::SUCCESS;
            }

            current_arg += 1;
        }


        // looking for other commands
        let mut run_mode: Option<RunMode> = None;

        current_arg = 1;
        while current_arg < args.len() {
            let arg = &args[ current_arg ];
            match arg.as_str() {
                "check" | "compile" | "run" => {
                    if let Some( _ ) = run_mode {
                        eprintln!( "{}: run mode already selected", ERROR );
                        return ExitCode::FAILURE;
                    }

                    current_arg += 1;
                    if current_arg >= args.len() {
                        eprintln!( "{}: missing source file path for '{}' mode", ERROR, arg );
                        return ExitCode::FAILURE;
                    }

                    let src_path: PathBuf = args[ current_arg ].to_owned().into();

                    run_mode = match arg.as_str() {
                        "check" => Some( RunMode { src_path, kind: RunModeKind::Check } ),
                        "compile" | "run" => {
                            let mut output_path: Option<PathBuf> = None;

                            current_arg += 1;
                            if current_arg < args.len() {
                                let out_arg = &args[ current_arg ];
                                if out_arg == "-o" || out_arg == "--output" {
                                    current_arg += 1;
                                    if current_arg >= args.len() {
                                        eprintln!( "{}: missing output folder path", ERROR );
                                        return ExitCode::FAILURE;
                                    }

                                    output_path = Some( args[ current_arg ].to_owned().into() );
                                }
                            }

                            match arg.as_str() {
                                "compile" => Some( RunMode { src_path, kind: RunModeKind::Compile { out_path: output_path, run: false } } ),
                                "run" => Some( RunMode { src_path, kind: RunModeKind::Compile { out_path: output_path, run: true } } ),
                                _ => unreachable!(),
                            }
                        },
                        _ => unreachable!(),
                    };
                },
                "-h" | "--help" | "-v" | "--version" => (),
                "-c" | "--color" => current_arg += 1,
                "-o" | "--output" => {
                    current_arg += 1;
                    if current_arg < args.len() {
                        let out_arg = &args[ current_arg ];
                        if out_arg == "-o" || out_arg == "--output" {
                            current_arg += 1;
                            if current_arg >= args.len() {
                                eprintln!( "{}: missing output folder path", ERROR );
                                return ExitCode::FAILURE;
                            }
                        }
                    }

                    eprintln!( "{}: output folder option can only be used after a 'compile' or 'run' command", ERROR );
                    return ExitCode::FAILURE;
                }
                _ => {
                    eprintln!( "{}: unrecognized option '{}'", ERROR, arg );
                    return ExitCode::FAILURE;
                }
            }

            current_arg += 1;
        }


        let run_mode = match run_mode {
            Some( mode ) => mode,
            None => {
                Self::print_usage( color_mode );
                return ExitCode::SUCCESS;
            },
        };

        let source_file = match BlitzSrc::try_from( Path::new( &run_mode.src_path ) ) {
            Ok( src ) => src,
            Err( err ) => {
                eprintln!( "{}", err );
                return ExitCode::FAILURE;
            },
        };

        let start_time = Instant::now();
        let ast = match Checker::check( source_file, &start_time ) {
            Ok( ast ) => {
                ast
            }
            Err( mut errors ) => {
                let mut line_byte_start = errors.errors[ 0 ].line_byte_start;
                let mut line_text = String::new();
                let _ = errors.src.src.seek( SeekFrom::Start( line_byte_start as u64 ) );
                let _ = errors.src.src.read_line( &mut line_text );

                for error in &errors.errors {
                    if line_byte_start != error.line_byte_start {
                        line_byte_start = error.line_byte_start;
                        line_text.clear();
                        let _ = errors.src.src.seek( SeekFrom::Start( line_byte_start as u64 ) );
                        let _ = errors.src.src.read_line( &mut line_text );
                    }

                    let error_msg = Colored {
                        text: error.msg.to_string().into(),
                        foreground: Foreground::White,
                        flags: Flags::Bold,
                        ..Default::default()
                    };

                    let line_number_and_bar = Colored {
                        text: format!( "{} |", error.line ).into(),
                        foreground: Foreground::LightBlue,
                        ..Default::default()
                    };

                    let visualization_padding = line_number_and_bar.text.len();
                    let at_padding = visualization_padding - 1;

                    let pointers_col = error.col - 1;
                    let pointers_len = error.len;

                    let pointers_and_help_msg = Colored {
                        text: format!( "{:^>pointers_len$} {}", "", error.help_msg ).into(),
                        foreground: Foreground::LightRed,
                        ..Default::default()
                    };

                    eprintln!(
                        "{}: {}\
                        \n{:>#at_padding$}: {}:{}:{}\
                        \n{:>#visualization_padding$}\
                        \n{} {}\
                        \n{:>#visualization_padding$} {:>pointers_col$}{}\n",
                        ERROR, error_msg,
                        AT, errors.src.path.display(), error.line, error.col,
                        BAR,
                        line_number_and_bar, line_text.trim_end(),
                        BAR, "", pointers_and_help_msg
                    );
                }

                return ExitCode::FAILURE;
            }
        };

        match &run_mode.kind {
            RunModeKind::Check => (/* do nothing */),
            RunModeKind::Compile { out_path, run } => {
                let mut compiler = Compiler {
                    src_path: run_mode.src_path.into(),
                    out_path: out_path.clone(),
                    ast: &ast,
                    rodata: String::new(),
                    asm: String::new(),
                    variables: Vec::new(),
                    strings: Vec::new(),
                    if_idx: 0,
                    loop_idx: 0,
                    loop_idx_stack: Vec::new(),
                };

                let executable_path = match compiler.compile( &start_time ) {
                    Ok( path ) => path,
                    Err( err ) => {
                        eprintln!( "{}", err );
                        return ExitCode::FAILURE;
                    },
                };

                let done_time = start_time.elapsed();
                let elapsed_done = Colored {
                    text: format!( "{}s", done_time.as_secs_f64() ).into(),
                    foreground: Foreground::LightGray,
                    ..Default::default()
                };
                eprintln!( "{}: {} ... in {}", DONE, executable_path.display(), elapsed_done );

                if *run {
                    match compiler.run( &executable_path ) {
                        Ok( _ ) => (),
                        Err( err ) => {
                            eprintln!( "{}", err );
                            return ExitCode::FAILURE;
                        },
                    }
                }
            }
        }

        return ExitCode::SUCCESS;
    }
}


// IDEA adapt SyntaxErrors to report cli mistakes
fn main() -> ExitCode {
    return Blitz::from( env::args() );
}
