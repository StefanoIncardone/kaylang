use std::fmt::Display;

use crate::cli::display;

#[allow(dead_code)]
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub(crate) enum Fg {
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
pub(crate) enum Bg {
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

pub(crate) type Flags = u8;
pub(crate) struct Flag;

#[allow(non_upper_case_globals)]
#[allow(dead_code)]
impl Flag {
    pub(crate) const Default: Flags = 0b0000_0000;
    pub(crate) const Bold: Flags = 0b0000_0001;
    pub(crate) const Underline: Flags = 0b0000_0010;
    pub(crate) const NoUnderline: Flags = 0b0000_0100;
    pub(crate) const ReverseText: Flags = 0b0000_1000;
    pub(crate) const PositiveText: Flags = 0b0001_0000;
}

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct Options {
    pub(crate) fg: Fg,
    pub(crate) bg: Bg,
    pub(crate) flags: Flags,
}

#[derive(Debug, Default)]
pub(crate) struct ColoredStr {
    pub(crate) text: &'static str,
    pub(crate) opt: Options,
}

impl Display for ColoredStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return unsafe { display(self.text, self.opt, f) };
    }
}

#[derive(Debug, Default)]
pub(crate) struct Colored {
    pub(crate) text: String,
    pub(crate) opt: Options,
}

impl Display for Colored {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return unsafe { display(&self.text, self.opt, f) };
    }
}
