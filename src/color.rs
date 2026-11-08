use back_to_front::{cli::{AnsiBg, AnsiCode, AnsiFg, AnsiFlag}, text};

pub type ansi_flag = u8;
pub type ansi_code = u8;

// IDEA(stefano): make more "pure" by selecting the printing mode each time
pub(super) static mut print: fn(
    f: &mut core::fmt::Formatter<'_>,
    text: &dyn core::fmt::Display,
    fg: AnsiFg,
    bg: AnsiBg,
    flags: ansi_flag,
) -> core::fmt::Result = print_color;

pub(super) fn print_no_color(
    f: &mut core::fmt::Formatter<'_>,
    text: &dyn core::fmt::Display,
    _: AnsiFg,
    _: AnsiBg,
    _: ansi_flag,
) -> core::fmt::Result {
    return text.fmt(f);
}

pub(super) fn print_color(
    f: &mut core::fmt::Formatter<'_>,
    text: &dyn core::fmt::Display,
    fg: AnsiFg,
    bg: AnsiBg,
    flags: ansi_flag,
) -> core::fmt::Result {
    let codes_str = unsafe { AnsiColor { fg, bg, flags }.codes() };

    f.write_str(codes_str)?;
    text.fmt(f)?;
    return f.write_str(AnsiColor::CLOSE);
}

type Codes = [u8; CODES_LEN];
const CODES_LEN: usize = 24;
const CODES_START_INDEX: usize = CODES_LEN - 2; // skipping the last m
static mut CODES: Codes = [0; 24];

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
#[must_use]
pub struct AnsiColor {
    pub fg: AnsiFg,
    pub bg: AnsiBg,
    pub flags: ansi_flag,
}

impl AnsiColor {
    pub const CLOSE: &str = "\x1b[0m";

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

    /// # Safety
    /// returns a reference to static memory, contents may get overwritten between calls
    #[must_use] #[expect(static_mut_refs)]
    pub const unsafe fn codes(self) -> &'static str {
        // precalculated from the max amount of digits that can appear in fg, bg and flags
        unsafe { CODES = *b";;;;;;;;;;;;;;;;;;;;;;;m"; }
        let codes = unsafe { &mut CODES };
        let mut digit_index = CODES_START_INDEX;

        if self.fg as u8 != AnsiFg::Default as u8 {
            Self::u8_2_digits_to_str(self.fg as u8, codes, &mut digit_index);
        }

        if self.bg as u8 >= AnsiBg::BrightBlack as u8 {
            Self::u8_3_digits_to_str(self.bg as u8, codes, &mut digit_index);
        }
        else if self.bg as u8 >= AnsiBg::Black as u8 {
            Self::u8_2_digits_to_str(self.bg as u8, codes, &mut digit_index);
        }
        else {
            // nothing to convert
        }

        if self.flags & AnsiFlag::Bold as ansi_flag != 0 {
            Self::u8_1_digits_to_str(AnsiCode::Bold as u8, codes, &mut digit_index);
        }
        if self.flags & AnsiFlag::Underline as ansi_flag != 0 {
            Self::u8_1_digits_to_str(AnsiCode::Underline as u8, codes, &mut digit_index);
        }
        if self.flags & AnsiFlag::NoUnderline as ansi_flag != 0 {
            Self::u8_2_digits_to_str(AnsiCode::NoUnderline as u8, codes, &mut digit_index);
        }
        if self.flags & AnsiFlag::ReverseText as ansi_flag != 0 {
            Self::u8_1_digits_to_str(AnsiCode::ReverseText as u8, codes, &mut digit_index);
        }
        if self.flags & AnsiFlag::PositiveText as ansi_flag != 0 {
            Self::u8_2_digits_to_str(AnsiCode::PositiveText as u8, codes, &mut digit_index);
        }

        if digit_index == CODES_START_INDEX { return ""; }

        // digit index now points two characters past the last ansi code
        codes[digit_index.wrapping_add(1)] = b'[';
        codes[digit_index] = b'\x1b';

        let codes_str = unsafe { text::slice(str::from_utf8_unchecked(codes), digit_index, codes.len()) };
        return codes_str;
    }
}

#[derive(Clone)]
#[must_use]
pub struct Colored<'text, Text: core::fmt::Display + ?Sized> {
    pub text: &'text Text,
    pub color: AnsiColor,
}

impl<Text: core::fmt::Display + ?Sized> core::fmt::Display for Colored<'_, Text> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return unsafe { print(f, &self.text, self.color.fg, self.color.bg, self.color.flags) };
    }
}
