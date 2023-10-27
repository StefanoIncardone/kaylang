use std::{fmt::Display, io::IsTerminal};


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
    Default         = 0b00000,
    Bold            = 0b00001,
    Underline       = 0b00010,
    NoUnderline     = 0b00100,
    ReverseText     = 0b01000,
    PositiveText    = 0b10000,
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
    pub text: String,
    pub foreground: Foreground,
    pub background: Background,
    pub flags: Flags,
}

impl Display for Colored {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return unsafe { display( &self.text, self.foreground, self.background, self.flags, f ) };
    }
}


#[allow(non_upper_case_globals)]
static mut display: fn(&str, Foreground, Background, Flags, &mut std::fmt::Formatter<'_>) -> std::fmt::Result = color;

fn no_color( text: &str, _foreground: Foreground, _background: Background, _flags: Flags, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
    return write!( f, "{}", text );
}

// TODO make it so that formatting options are applied to the string only, and not on the escape codes
fn color( text: &str, foreground: Foreground, background: Background, flags: Flags, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
    let mut codes = String::with_capacity( 15 );

    if foreground != Foreground::Default {
        codes += &format!( "{};", foreground as u8 );
    }
    if background != Background::Default {
        codes += &format!( "{};", background as u8 );
    }
    if flags.bold() {
        codes += "1;";
    }
    if flags.underline() {
        codes += "4;";
    }
    if flags.no_underline() {
        codes += "24;";
    }
    if flags.reverse_text() {
        codes += "7;";
    }
    if flags.positive_text() {
        codes += "27;";
    }

    return if codes.is_empty() {
        write!( f, "{}", text )
    }
    else {
        codes.pop(); //remove the last ";"
        write!( f, "\x1b[{}m{}\x1b[0m", codes, text )
    }
}

pub fn auto() {
    unsafe {
        display = if !std::io::stdout().is_terminal() || !std::io::stderr().is_terminal() {
            no_color
        }
        else {
            color
        };
    }
}

pub fn always() {
    unsafe { display = color; }
}

pub fn never() {
    unsafe { display = no_color; }
}


#[derive( Debug, Default )]
pub struct ColoredStr {
    pub text: &'static str,
    pub foreground: Foreground,
    pub background: Background,
    pub flags: Flags,
}

impl Display for ColoredStr {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return unsafe { display( self.text, self.foreground, self.background, self.flags, f ) };
    }
}


#[macro_export]
macro_rules! foreground {
    (Default) => {""};
    (Black) => {"30"};
    (Red) => {"31"};
    (Green) => {"32"};
    (Yellow) => {"33"};
    (Blue) => {"34"};
    (Magenta) => {"35"};
    (Cyan) => {"36"};
    (LightGray) => {"37"};
    (DarkGray) => {"90"};
    (LightRed) => {"91"};
    (LightGreen) => {"92"};
    (LightYellow) => {"93"};
    (LightBlue) => {"94"};
    (LightMagenta) => {"95"};
    (LightCyan) => {"96"};
    (White) => {"97"};
}

#[macro_export]
macro_rules! background {
    (Default) => {""};
    (Black) => {";40"};
    (DarkRed) => {";41"};
    (DarkGreen) => {";42"};
    (DarkYellow) => {";43"};
    (DarkBlue) => {";44"};
    (DarkMagenta) => {";45"};
    (DarkCyan) => {";46"};
    (DarkWhite) => {";47"};
    (BrightBlack) => {";100"};
    (BrightRed) => {";101"};
    (BrightGreen) => {";102"};
    (BrightYellow) => {";103"};
    (BrightBlue) => {";104"};
    (BrightMagenta) => {";105"};
    (BrightCyan) => {";106"};
    (White) => {";107"};
}

#[macro_export]
macro_rules! bold {
    (false) => {""};
    (true) => {";1"};
}

#[macro_export]
macro_rules! underline {
    (false) => {""};
    (true) => {";4"};
}

#[macro_export]
macro_rules! no_underline {
    (false) => {""};
    (true) => {";24"};
}

#[macro_export]
macro_rules! reverse_text {
    (false) => {""};
    (true) => {";7"};
}

#[macro_export]
macro_rules! positive_text {
    (false) => {""};
    (true) => {";27"};
}

#[macro_export]
macro_rules! colored {
    () => {""};
    ($(text:)?$text: literal$(,)?) => {$text};
    (
        $(text:)?$text: literal
        $(, foreground: Foreground::$fg: tt)?
        $(, background: Background::$bg: tt)?
        $(, bold: $b: ident)?
        $(, underline: $u: ident)?
        $(, no_underline: $nu: ident)?
        $(, reverse_text: $rt: ident)?
        $(, positive_text: $pt: ident)?
        $(,)?
    ) => {
        concat!(
            "\x1b[",
            $(foreground!($fg), )?
            $(background!($bg), )?
            $(bold!($b), )?
            $(underline!($u), )?
            $(no_underline!($u), )?
            $(reverse_text!($rt), )?
            $(positive_text!($pt), )?
            "m",
            $text,
            "\x1b[0m"
        )
    };
}

#[macro_export]
macro_rules! colored_raw {
    () => {""};
    ($(text:)?$text: literal$(,)?) => {$text};
    (
        $(text:)?$text: literal
        $(, foreground: Foreground::$fg: tt)?
        $(, background: Background::$bg: tt)?
        $(, bold: $b: ident)?
        $(, underline: $u: ident)?
        $(, no_underline: $nu: ident)?
        $(, reverse_text: $rt: ident)?
        $(, positive_text: $pt: ident)?
        $(,)?
    ) => {
        concat!(
            r"\x1b[",
            $(foreground!($fg), )?
            $(background!($bg), )?
            $(bold!($b), )?
            $(underline!($u), )?
            $(no_underline!($u), )?
            $(reverse_text!($rt), )?
            $(positive_text!($pt), )?
            "m",
            $text,
            r"\x1b[0m"
        )
    };
}
