use core::fmt::Display;

#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum Fg {
    #[default]
    Default      = 0,
    Black        = 30,
    Red          = 31,
    Green        = 32,
    Yellow       = 33,
    Blue         = 34,
    Magenta      = 35,
    Cyan         = 36,
    LightGray    = 37,
    DarkGray     = 90,
    LightRed     = 91,
    LightGreen   = 92,
    LightYellow  = 93,
    LightBlue    = 94,
    LightMagenta = 95,
    LightCyan    = 96,
    White        = 97,
}

#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum Bg {
    #[default]
    Default       = 0,
    Black         = 40,
    DarkRed       = 41,
    DarkGreen     = 42,
    DarkYellow    = 43,
    DarkBlue      = 44,
    DarkMagenta   = 45,
    DarkCyan      = 46,
    DarkWhite     = 47,
    BrightBlack   = 100,
    BrightRed     = 101,
    BrightGreen   = 102,
    BrightYellow  = 103,
    BrightBlue    = 104,
    BrightMagenta = 105,
    BrightCyan    = 106,
    White         = 107,
}

#[expect(non_camel_case_types, reason = "alias to a primitive type")]
pub type ansi_flag = u8;

#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum AnsiFlag {
    #[default]
    Default      = 0b000_00000,
    Bold         = 0b000_00001,
    Underline    = 0b000_00010,
    NoUnderline  = 0b000_00100,
    ReverseText  = 0b000_01000,
    PositiveText = 0b000_10000,
}

#[expect(non_camel_case_types, reason = "alias to a primitive type")]
pub type ansi_code = u8;

#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum AnsiCode {
    #[default]
    Default      = 0,
    Bold         = 1,
    Underline    = 4,
    NoUnderline  = 24,
    ReverseText  = 7,
    PositiveText = 27,
}

// IDEA(stefano): make more "pure" by selecting the printing mode each time
#[expect(non_upper_case_globals, reason = "alias to a function")]
pub(super) static mut print: fn(
    &dyn Display,
    Fg,
    Bg,
    ansi_flag,
    &mut core::fmt::Formatter<'_>,
) -> core::fmt::Result = print_color;

pub(super) fn print_no_color(
    text: &dyn Display,
    _: Fg,
    _: Bg,
    _: ansi_flag,
    f: &mut core::fmt::Formatter<'_>,
) -> core::fmt::Result {
    return text.fmt(f);
}

pub(super) fn print_color(
    text: &dyn Display,
    fg: Fg,
    bg: Bg,
    flags: ansi_flag,
    f: &mut core::fmt::Formatter<'_>,
) -> core::fmt::Result {
    const CODES_LEN: usize = 24;
    const CODES_START_INDEX: usize = CODES_LEN - 2; // skipping the last m
    type Codes = [u8; CODES_LEN];

    fn u8_1_digits_to_str(value: u8, codes: &mut Codes, digit_index: &mut usize) {
        let digit = value % 10;
        codes[*digit_index] = digit.wrapping_add(b'0');
        *digit_index = digit_index.wrapping_sub(1 + 1); // skipping the semicolon
    }

    fn u8_2_digits_to_str(mut value: u8, codes: &mut Codes, digit_index: &mut usize) {
        let mut digit = value % 10;
        value /= 10;
        codes[*digit_index] = digit.wrapping_add(b'0');
        *digit_index = digit_index.wrapping_sub(1);

        digit = value % 10;
        codes[*digit_index] = digit.wrapping_add(b'0');
        *digit_index = digit_index.wrapping_sub(1 + 1); // skipping the semicolon
    }

    #[expect(clippy::single_call_fn)]
    fn u8_3_digits_to_str(mut value: u8, codes: &mut Codes, digit_index: &mut usize) {
        let mut digit = value % 10;
        value /= 10;
        codes[*digit_index] = digit.wrapping_add(b'0');
        *digit_index = digit_index.wrapping_sub(1);

        digit = value % 10;
        value /= 10;
        codes[*digit_index] = digit.wrapping_add(b'0');
        *digit_index = digit_index.wrapping_sub(1);

        digit = value % 10;
        codes[*digit_index] = digit.wrapping_add(b'0');
        *digit_index = digit_index.wrapping_sub(1 + 1); // skipping the semicolon
    }

    // precalculated from the max amount of digits that can appear in fg, bg and flags
    #[rustfmt::skip]
    let mut codes: Codes = [
        b';', b';', b';', b';', b';', b';', b';', b';',
        b';', b';', b';', b';', b';', b';', b';', b';',
        b';', b';', b';', b';', b';', b';', b';', b'm',
    ];
    let mut digit_index = CODES_START_INDEX;

    if fg != Fg::Default {
        u8_2_digits_to_str(fg as u8, &mut codes, &mut digit_index);
    }
    if bg as u8 >= Bg::BrightBlack as u8 {
        u8_3_digits_to_str(bg as u8, &mut codes, &mut digit_index);
    } else if bg as u8 >= Bg::Black as u8 {
        u8_2_digits_to_str(bg as u8, &mut codes, &mut digit_index);
    } else {
        // nothing to convert
    }
    if flags & AnsiFlag::Bold as ansi_flag != 0 {
        u8_1_digits_to_str(AnsiCode::Bold as u8, &mut codes, &mut digit_index);
    }
    if flags & AnsiFlag::Underline as ansi_flag != 0 {
        u8_1_digits_to_str(AnsiCode::Underline as u8, &mut codes, &mut digit_index);
    }
    if flags & AnsiFlag::NoUnderline as ansi_flag != 0 {
        u8_2_digits_to_str(AnsiCode::NoUnderline as u8, &mut codes, &mut digit_index);
    }
    if flags & AnsiFlag::ReverseText as ansi_flag != 0 {
        u8_1_digits_to_str(AnsiCode::ReverseText as u8, &mut codes, &mut digit_index);
    }
    if flags & AnsiFlag::PositiveText as ansi_flag != 0 {
        u8_2_digits_to_str(AnsiCode::PositiveText as u8, &mut codes, &mut digit_index);
    }

    if digit_index == CODES_START_INDEX {
        return text.fmt(f);
    }

    // digit index now points two characters past the last ansi code
    codes[digit_index] = b'\x1b';
    codes[digit_index.wrapping_add(1)] = b'[';

    let codes_str = unsafe { core::str::from_utf8_unchecked(&codes[digit_index..]) };

    f.write_str(codes_str)?;
    text.fmt(f)?;
    return f.write_str("\x1b[0m");
}

#[derive(Clone)]
pub struct Colored<'text, Text: Display + ?Sized> {
    pub text: &'text Text,
    pub fg: Fg,
    pub bg: Bg,
    pub flags: ansi_flag,
}

impl<Text: Display + ?Sized> Display for Colored<'_, Text> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return unsafe { print(&self.text, self.fg, self.bg, self.flags, f) };
    }
}
