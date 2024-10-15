use core::fmt::{Display, Write as _};
use std::io::IsTerminal;

use crate::Color;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
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

#[expect(non_upper_case_globals, reason = "used as type safe constants")]
impl Flag {
    pub const Default: Flags = 0b0000_0000;
    pub const Bold: Flags = 0b0000_0001;
    pub const Underline: Flags = 0b0000_0010;
    pub const NoUnderline: Flags = 0b0000_0100;
    pub const ReverseText: Flags = 0b0000_1000;
    pub const PositiveText: Flags = 0b0001_0000;
}

#[expect(non_upper_case_globals, reason = "it's a function, so it should be named like a function")]
pub(crate) static mut print: fn(
    &str,
    Fg,
    Bg,
    Flags,
    &mut core::fmt::Formatter<'_>,
) -> core::fmt::Result = print_color;

impl Color {
    pub fn set<I: IsTerminal>(self, sink: &I) {
        unsafe {
            print = match self {
                Self::Auto if sink.is_terminal() => print_color,
                Self::Auto => print_no_color,
                Self::Always => print_color,
                Self::Never => print_no_color,
            }
        }
    }
}

fn print_no_color(
    text: &str,
    _: Fg,
    _: Bg,
    _: Flags,
    f: &mut core::fmt::Formatter<'_>,
) -> core::fmt::Result {
    return text.fmt(f);
}

fn print_color(
    text: &str,
    fg: Fg,
    bg: Bg,
    flags: Flags,
    f: &mut core::fmt::Formatter<'_>,
) -> core::fmt::Result {
    // TODO(stefano): fill a preallocated static buffer instead of creating a new one each time
    let mut codes = String::with_capacity(24);

    if fg != Fg::Default {
        _ = write!(codes, "{};", fg as u8);
    }
    if bg != Bg::Default {
        _ = write!(codes, "{};", bg as u8);
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
        text.fmt(f)
    } else {
        let _last_semicolon = codes.pop();

        write!(f, "\x1b[{codes}m")?;
        text.fmt(f)?;
        write!(f, "\x1b[0m")
    };
}

#[derive(Debug, Default)]
pub struct Colored<Text: AsRef<str>> {
    pub text: Text,
    pub fg: Fg,
    pub bg: Bg,
    pub flags: Flags,
}

impl<Text: AsRef<str>> Display for Colored<Text> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return unsafe { print(self.text.as_ref(), self.fg, self.bg, self.flags, f) };
    }
}
