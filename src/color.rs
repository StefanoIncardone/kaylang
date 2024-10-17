use core::fmt::Display;
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

#[expect(
    non_camel_case_types,
    reason = "behaves like a primitive type, so it should be named like a primitive type"
)]
pub type flag = u8;

#[expect(non_upper_case_globals, reason = "used as type safe constants")]
pub mod flags {
    use super::flag;

    pub const Default: flag = 0b0000_0000;
    pub const Bold: flag = 0b0000_0001;
    pub const Underline: flag = 0b0000_0010;
    pub const NoUnderline: flag = 0b0000_0100;
    pub const ReverseText: flag = 0b0000_1000;
    pub const PositiveText: flag = 0b0001_0000;
}

#[expect(
    non_camel_case_types,
    reason = "behaves like a primitive type, so it should be named like a primitive type"
)]
pub type ansi_code = u8;

#[expect(non_upper_case_globals, reason = "used as type safe constants")]
pub mod ansi_codes {
    use super::ansi_code;

    pub const Default: ansi_code = 0;
    pub const Bold: ansi_code = 1;
    pub const Underline: ansi_code = 4;
    pub const NoUnderline: ansi_code = 24;
    pub const ReverseText: ansi_code = 7;
    pub const PositiveText: ansi_code = 27;
}

#[expect(non_upper_case_globals, reason = "it's a function, so it should be named like a function")]
pub(crate) static mut print: fn(
    &str,
    Fg,
    Bg,
    flag,
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
    _: flag,
    f: &mut core::fmt::Formatter<'_>,
) -> core::fmt::Result {
    return text.fmt(f);
}

fn print_color(
    text: &str,
    fg: Fg,
    bg: Bg,
    flags: flag,
    f: &mut core::fmt::Formatter<'_>,
) -> core::fmt::Result {
    const CODES_LEN: usize = 24;

    let mut codes_bytes = 0_u64;

    if fg != Fg::Default {
        codes_bytes |= fg as ansi_code as u64;
        codes_bytes <<= u8::BITS as u64;
    }
    if bg != Bg::Default {
        codes_bytes |= bg as ansi_code as u64;
        codes_bytes <<= u8::BITS as u64;
    }
    if flags & flags::Bold != 0 {
        codes_bytes |= ansi_codes::Bold as u64;
        codes_bytes <<= u8::BITS as u64;
    }
    if flags & flags::Underline != 0 {
        codes_bytes |= ansi_codes::Underline as u64;
        codes_bytes <<= u8::BITS as u64;
    }
    if flags & flags::NoUnderline != 0 {
        codes_bytes |= ansi_codes::NoUnderline as u64;
        codes_bytes <<= u8::BITS as u64;
    }
    if flags & flags::ReverseText != 0 {
        codes_bytes |= ansi_codes::ReverseText as u64;
        codes_bytes <<= u8::BITS as u64;
    }
    if flags & flags::PositiveText != 0 {
        codes_bytes |= ansi_codes::PositiveText as u64;
        codes_bytes <<= u8::BITS as u64;
    }

    codes_bytes >>= u8::BITS as u64;
    if codes_bytes == 0 {
        return text.fmt(f);
    }

    let mut codes: [u8; CODES_LEN] = [b';'; CODES_LEN];
    let mut codes_end_pointer = codes.as_mut_ptr().wrapping_add(codes.len());

    loop {
        let mut code = codes_bytes as u8;
        loop {
            codes_end_pointer = codes_end_pointer.wrapping_sub(1);
            unsafe { *codes_end_pointer = (code % 10) + b'0'; }
            code /= 10;
            if code == 0 {
                break;
            }
        }
        codes_end_pointer = codes_end_pointer.wrapping_sub(1);

        codes_bytes >>= u8::BITS as u64;
        if codes_bytes == 0 {
            break;
        }
    }

    // skipping the last semicolon
    codes_end_pointer = codes_end_pointer.wrapping_add(1);
    let codes_len = CODES_LEN - (codes_end_pointer as usize - codes.as_ptr() as usize);

    let codes_slice = unsafe { core::slice::from_raw_parts(codes_end_pointer, codes_len) };
    let codes_str = unsafe { core::str::from_utf8_unchecked(codes_slice) };

    write!(f, "\x1b[{codes_str}m")?;
    text.fmt(f)?;
    return write!(f, "\x1b[0m");
}

#[derive(Debug, Default)]
pub struct Colored<Text: AsRef<str>> {
    pub text: Text,
    pub fg: Fg,
    pub bg: Bg,
    pub flags: flag,
}

impl<Text: AsRef<str>> Display for Colored<Text> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return unsafe { print(self.text.as_ref(), self.fg, self.bg, self.flags, f) };
    }
}
