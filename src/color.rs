use crate::Color;
use std::{fmt::Display, io::IsTerminal};

#[allow(dead_code)]
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

#[allow(dead_code)]
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

#[allow(non_upper_case_globals)]
pub(crate) static mut log: fn(&str, Fg, Bg, Flags, &mut std::fmt::Formatter<'_>) -> std::fmt::Result = log_color;

impl Color {
    pub fn set(self, sink: &impl IsTerminal) {
        unsafe {
            log = match self {
                Self::Auto => {
                    if sink.is_terminal() {
                        log_color
                    } else {
                        log_no_color
                    }
                }
                Self::Always => log_color,
                Self::Never => log_no_color,
            }
        }
    }
}

fn log_no_color(text: &str, _: Fg, _: Bg, _: Flags, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    text.fmt(f)
}

fn log_color(text: &str, fg: Fg, bg: Bg, flags: Flags, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut codes = String::with_capacity(15);

    if fg != Fg::Default {
        codes += &format!("{fg};", fg = fg as u8);
    }
    if bg != Bg::Default {
        codes += &format!("{bg};", bg = bg as u8);
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

    if codes.is_empty() {
        text.fmt(f)
    } else {
        let _last_semicolon = codes.pop();

        write!(f, "\x1b[{codes}m")?;
        text.fmt(f)?;
        write!(f, "\x1b[0m")
    }
}

#[derive(Debug, Default)]
pub struct Colored<Str: AsRef<str>> {
    pub text: Str,
    pub fg: Fg,
    pub bg: Bg,
    pub flags: Flags,
}

impl<Str: AsRef<str>> Display for Colored<Str> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe { log(self.text.as_ref(), self.fg, self.bg, self.flags, f) }
    }
}

#[deprecated(since = "0.5.3", note = "will be removed to allow for more explicit type signatures")]
pub type ColoredString = Colored<String>;
#[deprecated(since = "0.5.3", note = "will be removed to allow for more explicit type signatures")]
pub type ColoredStr = Colored<&'static str>;
