use core::fmt::Display;

use back_to_front::cli::{AnsiBg, AnsiCode, AnsiFg, AnsiFlag};

pub type ansi_flag = u8;
pub type ansi_code = u8;

// IDEA(stefano): make more "pure" by selecting the printing mode each time
pub(super) static mut print: fn(
    f: &mut core::fmt::Formatter<'_>,
    text: &dyn Display,
    fg: AnsiFg,
    bg: AnsiBg,
    flags: ansi_flag,
) -> core::fmt::Result = print_color;

pub(super) fn print_no_color(
    f: &mut core::fmt::Formatter<'_>,
    text: &dyn Display,
    _: AnsiFg,
    _: AnsiBg,
    _: ansi_flag,
) -> core::fmt::Result {
    return text.fmt(f);
}

pub(super) fn print_color(
    f: &mut core::fmt::Formatter<'_>,
    text: &dyn Display,
    fg: AnsiFg,
    bg: AnsiBg,
    flags: ansi_flag,
) -> core::fmt::Result {
    const CODES_LEN: usize = 24;
    const CODES_START_INDEX: usize = CODES_LEN - 2; // skipping the last m
    type Codes = [u8; CODES_LEN];

    const fn u8_1_digits_to_str(value: u8, codes: &mut Codes, digit_index: &mut usize) {
        let digit = value % 10;
        codes[*digit_index] = digit.wrapping_add(b'0');
        *digit_index = digit_index.wrapping_sub(1 + 1); // skipping the semicolon
    }

    const fn u8_2_digits_to_str(mut value: u8, codes: &mut Codes, digit_index: &mut usize) {
        let mut digit = value % 10;
        value /= 10;
        codes[*digit_index] = digit.wrapping_add(b'0');
        *digit_index = digit_index.wrapping_sub(1);

        digit = value % 10;
        codes[*digit_index] = digit.wrapping_add(b'0');
        *digit_index = digit_index.wrapping_sub(1 + 1); // skipping the semicolon
    }

    #[expect(clippy::single_call_fn)]
    const fn u8_3_digits_to_str(mut value: u8, codes: &mut Codes, digit_index: &mut usize) {
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
    let mut codes: Codes = *b";;;;;;;;;;;;;;;;;;;;;;;m";
    let mut digit_index = CODES_START_INDEX;

    if fg != AnsiFg::Default {
        u8_2_digits_to_str(fg as u8, &mut codes, &mut digit_index);
    }
    if bg as u8 >= AnsiBg::BrightBlack as u8 {
        u8_3_digits_to_str(bg as u8, &mut codes, &mut digit_index);
    } else if bg as u8 >= AnsiBg::Black as u8 {
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
#[must_use]
pub struct Colored<'text, Text: Display + ?Sized> {
    pub text: &'text Text,
    pub fg: AnsiFg,
    pub bg: AnsiBg,
    pub flags: ansi_flag,
}

impl<Text: Display + ?Sized> Display for Colored<'_, Text> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return unsafe { print(f, &self.text, self.fg, self.bg, self.flags) };
    }
}
