use std::fmt::Display;


#[derive( Debug, Default, Clone, Copy, PartialEq )]
#[allow(dead_code)]
pub enum Foreground {
    #[default]
    Default = 0,
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

#[derive( Debug, Default, Clone, Copy, PartialEq )]
#[allow(dead_code)]
pub enum Background {
    #[default]
    Default = 0,
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


#[derive( Debug, Default )]
pub struct Colored {
    pub text: String,
    pub foreground: Foreground,
    pub background: Background,
    pub bold: bool,
    pub underline: bool,
    pub no_underline: bool,
    pub reverse_text: bool,
    pub positive_text: bool,
}

impl Display for Colored {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        let mut codes = String::new();

        if self.foreground != Foreground::Default {
            codes.push_str( &format!( "{};", self.foreground as u8 ) )
        }
        if self.background != Background::Default {
            codes.push_str( &format!( "{};", self.background as u8 ) )
        }
        if self.bold {
            codes.push_str( "1;" );
        }
        if self.underline {
            codes.push_str( "4;" );
        }
        if self.no_underline {
            codes.push_str( "24;" );
        }
        if self.reverse_text {
            codes.push_str( "7;" );
        }
        if self.positive_text {
            codes.push_str( "27;" );
        }

        return if codes.is_empty() {
            write!( f, "{}", self.text )
        }
        else {
            codes.pop(); //remove the last ";"
            return write!( f, "\x1b[{}m{}\x1b[0m", codes, self.text )
        }
    }
}


#[macro_export]
macro_rules! foreground {
    (Foreground::Default) => {""};
    (Foreground::Black) => {"30"};
    (Foreground::Red) => {"31"};
    (Foreground::Green) => {"32"};
    (Foreground::Yellow) => {"33"};
    (Foreground::Blue) => {"34"};
    (Foreground::Magenta) => {"35"};
    (Foreground::Cyan) => {"36"};
    (Foreground::LightGray) => {"37"};
    (Foreground::DarkGray) => {"90"};
    (Foreground::LightRed) => {"91"};
    (Foreground::LightGreen) => {"92"};
    (Foreground::LightYellow) => {"93"};
    (Foreground::LightBlue) => {"94"};
    (Foreground::LightMagenta) => {"95"};
    (Foreground::LightCyan) => {"96"};
    (Foreground::White) => {"97"};
}

#[macro_export]
macro_rules! background {
    (Background::Default) => {""};
    (Background::Black) => {";40"};
    (Background::DarkRed) => {";41"};
    (Background::DarkGreen) => {";42"};
    (Background::DarkYellow) => {";43"};
    (Background::DarkBlue) => {";44"};
    (Background::DarkMagenta) => {";45"};
    (Background::DarkCyan) => {";46"};
    (Background::DarkWhite) => {";47"};
    (Background::BrightBlack) => {";100"};
    (Background::BrightRed) => {";101"};
    (Background::BrightGreen) => {";102"};
    (Background::BrightYellow) => {";103"};
    (Background::BrightBlue) => {";104"};
    (Background::BrightMagenta) => {";105"};
    (Background::BrightCyan) => {";106"};
    (Background::White) => {";107"};
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

// TODO allow for arbitrary argument order
#[macro_export]
macro_rules! colored {
    () => {""};
    ($(text:)?$text: literal$(,)?) => {$text};
    (
        $(text:)?$text: literal,
        $(foreground: Foreground::$fg: tt,)?
        $(background: Background::$bg: tt,)?
        $(bold: $b: ident,)?
        $(underline: $u: ident,)?
        $(no_underline: $nu: ident,)?
        $(reverse_text: $rt: ident,)?
        $(positive_text: $pt: ident,)?
        $(,)?
    ) => {
        concat!(
            "\x1b[",
            $(foreground!(Foreground::$fg), )?
            $(background!(Background::$bg), )?
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
