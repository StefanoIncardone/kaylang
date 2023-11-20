use std::{fmt::Display, io::IsTerminal};

use crate::Color;


#[allow( dead_code )]
#[derive( Debug, Default, Clone, Copy, PartialEq )]
#[repr( u8 )]
pub(crate) enum Fg {
    #[default] Default = 0,
    Black              = 30,
    Red                = 31,
    Green              = 32,
    Yellow             = 33,
    Blue               = 34,
    Magenta            = 35,
    Cyan               = 36,
    LightGray          = 37,
    DarkGray           = 90,
    LightRed           = 91,
    LightGreen         = 92,
    LightYellow        = 93,
    LightBlue          = 94,
    LightMagenta       = 95,
    LightCyan          = 96,
    White              = 97,
}

#[allow( dead_code )]
#[derive( Debug, Default, Clone, Copy, PartialEq )]
#[repr( u8 )]
pub(crate) enum Bg {
    #[default] Default = 0,
    Black              = 40,
    DarkRed            = 41,
    DarkGreen          = 42,
    DarkYellow         = 43,
    DarkBlue           = 44,
    DarkMagenta        = 45,
    DarkCyan           = 46,
    DarkWhite          = 47,
    BrightBlack        = 100,
    BrightRed          = 101,
    BrightGreen        = 102,
    BrightYellow       = 103,
    BrightBlue         = 104,
    BrightMagenta      = 105,
    BrightCyan         = 106,
    White              = 107,
}

pub(crate) type Flags = u8;
pub(crate) struct Flag;

#[allow( non_upper_case_globals )]
#[allow( dead_code )]
impl Flag {
    pub(crate) const Default:      Flags = 0b0000_0000;
    pub(crate) const Bold:         Flags = 0b0000_0001;
    pub(crate) const Underline:    Flags = 0b0000_0010;
    pub(crate) const NoUnderline:  Flags = 0b0000_0100;
    pub(crate) const ReverseText:  Flags = 0b0000_1000;
    pub(crate) const PositiveText: Flags = 0b0001_0000;
}

#[allow( non_upper_case_globals )]
static mut display: fn( &str, Fg, Bg, Flags, &mut std::fmt::Formatter<'_> ) -> std::fmt::Result = Color::color;

impl Color {
    pub(crate) fn set( &self ) {
        unsafe { display = match self {
            Self::Auto   => if !std::io::stderr().is_terminal() { Self::no_color } else { Self::color },
            Self::Always => Self::color,
            Self::Never  => Self::no_color,
        } }
    }

    // since printing version and help message are the only places where printing to stdoud is
    // performed we are manually checking if stdout (overring stderr coloring modes) is in terminal
    // mode until a way to separately print colored/non-colored output to stdout/stderr is found
    pub(crate) fn set_stdout( &self ) {
        unsafe { display = match self {
            Self::Auto   => if !std::io::stdout().is_terminal() { Self::no_color } else { Self::color },
            Self::Always => Self::color,
            Self::Never  => Self::no_color,
        } }
    }


    fn no_color( text: &str, _: Fg, _: Bg, _: Flags, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return text.fmt( f );
    }

    fn color( text: &str, fg: Fg, bg: Bg, flags: Flags, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        let mut codes = String::with_capacity( 15 );

        if fg != Fg::Default {
            codes += &format!( "{};", fg as u8 );
        }
        if bg != Bg::Default {
            codes += &format!( "{};", bg as u8 );
        }
        if flags & Flag::Bold != 0 {
            codes += "1;";
        }
        if flags & Flag::Underline != 0 {
            codes += "4;";
        }
        if flags & Flag::NoUnderline != 0 {
            codes += "24;";
        }
        if flags & Flag::ReverseText != 0 {
            codes += "7;";
        }
        if flags & Flag::PositiveText != 0 {
            codes += "27;";
        }

        return if codes.is_empty() {
            text.fmt( f )
        }
        else {
            codes.pop(); //remove the last ";"

            write!( f, "\x1b[{}m", codes )?;
            text.fmt( f )?;
            write!( f, "\x1b[0m" )
        }
    }
}

// TODO creare frozen colored struct, where codes are all baked in instead of having to build
// color codes each time
#[derive( Debug, Default )]
pub(crate) struct ColoredStr {
    pub(crate) text: &'static str,
    pub(crate) fg: Fg,
    pub(crate) bg: Bg,
    pub(crate) flags: Flags,
}

impl Display for ColoredStr {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return unsafe { display( self.text, self.fg, self.bg, self.flags, f ) };
    }
}

#[derive( Debug, Default )]
pub(crate) struct Colored {
    pub(crate) text: String,
    pub(crate) fg: Fg,
    pub(crate) bg: Bg,
    pub(crate) flags: Flags,
}

impl Display for Colored {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return unsafe { display( &self.text, self.fg, self.bg, self.flags, f ) };
    }
}
