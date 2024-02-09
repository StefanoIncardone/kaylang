use std::fmt::Display;

use crate::logging::log;

#[allow(dead_code)]
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub enum Fg {
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

#[allow(dead_code)]
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub enum Bg {
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

pub type Flags = u8;
pub struct Flag;

#[allow(non_upper_case_globals)]
#[allow(dead_code)]
impl Flag {
    pub const Default: Flags = 0b0000_0000;
    pub const Bold: Flags = 0b0000_0001;
    pub const Underline: Flags = 0b0000_0010;
    pub const NoUnderline: Flags = 0b0000_0100;
    pub const ReverseText: Flags = 0b0000_1000;
    pub const PositiveText: Flags = 0b0001_0000;
}

#[derive(Debug, Default)]
pub struct ColoredStr {
    pub text: &'static str,
    pub fg: Fg,
    pub bg: Bg,
    pub flags: Flags,
}

impl Display for ColoredStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return unsafe { log(self.text, self.fg, self.bg, self.flags, f) };
    }
}

#[derive(Debug, Default)]
pub struct Colored {
    pub text: String,
    pub fg: Fg,
    pub bg: Bg,
    pub flags: Flags,
}

impl Display for Colored {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return unsafe { log(&self.text, self.fg, self.bg, self.flags, f) };
    }
}
