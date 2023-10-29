use std::{fmt::Display, io::IsTerminal, borrow::Cow};


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

    // NOTE since printing version and help message are the only places where printing to stdoud is performed we are
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
