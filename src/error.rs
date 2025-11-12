use crate::{
    color::{ansi_flag, AnsiFlag, Bg, Colored, Fg},
    front_end::tokenizer::{ascii, utf32},
    AT, BAR, CAUSE,
};
use core::fmt::Display;
use std::path::Path;

use back_to_front::offset32;
use unicode_width::UnicodeWidthChar as _;

// IDEA(stefano): move to back-to-front
// IDEA(stefano): rename to PointersLen or something that refers to the fact that it's not
// calculating the actual visible length of characters, but rather the length of the pointers in
// error messages
pub(crate) trait DisplayLen {
    fn display_len(&self) -> offset32;
}

impl DisplayLen for str {
    fn display_len(&self) -> offset32 {
        let mut len = 0;
        for character in self.chars() {
            len += character.width_cjk().unwrap_or_default();
        }
        if len == 0 {
            len = 1;
        }
        #[expect(clippy::cast_possible_truncation)]
        return len as offset32;
    }
}

impl DisplayLen for utf32 {
    #[inline]
    fn display_len(&self) -> offset32 {
        let mut len = self.width_cjk().unwrap_or_default();
        if len == 0 {
            len = 1;
        }
        #[expect(clippy::cast_possible_truncation)]
        return len as offset32;
    }
}

impl DisplayLen for ascii {
    #[inline]
    fn display_len(&self) -> offset32 {
        return (*self as char).display_len();
    }
}

#[derive(Clone)]
pub struct MsgSimple<'kind, 'message> {
    pub kind: &'kind dyn Display,
    pub message: &'message dyn Display,
}

impl Display for MsgSimple<'_, '_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return write!(f, "{kind}: {message}", kind = self.kind, message = self.message);
    }
}

#[derive(Clone)]
pub struct MsgWithCause<'kind, 'message, 'cause> {
    pub kind: &'kind dyn Display,
    pub message: &'message dyn Display,
    pub cause: &'cause dyn Display,
}

impl Display for MsgWithCause<'_, '_, '_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return write!(
            f,
            "{kind}: {message}\
            \n{CAUSE}: {cause}",
            kind = self.kind,
            message = self.message,
            cause = self.cause,
        );
    }
}

#[derive(Clone)]
pub struct MsgWithCauseUnderText<'kind, 'message, 'cause, 'src> {
    pub kind: &'kind dyn Display,
    pub message: &'message dyn Display,
    pub cause: &'cause dyn Display,
    pub line_text: &'src dyn Display,
    pub pointers_count: offset32,
    pub pointers_offset: offset32,
}

impl Display for MsgWithCauseUnderText<'_, '_, '_, '_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let error_message = Colored {
            text: self.message,
            fg: Fg::White,
            bg: Bg::Default,
            flags: AnsiFlag::Bold as ansi_flag,
        };

        let pointers_and_cause = Colored {
            text: &format!(
                "{spaces:^>pointers_count$} {cause}",
                spaces = "",
                pointers_count = self.pointers_count as usize,
                cause = self.cause
            ),
            fg: Fg::LightRed,
            bg: Bg::Default,
            flags: AnsiFlag::Bold as ansi_flag,
        };

        return write!(
            f,
            "{kind}: {error_message}\
            \n{BAR}\
            \n{BAR} {line_text}\
            \n{BAR} {spaces:>pointers_offset$}{pointers_and_cause}",
            kind = self.kind,
            line_text = self.line_text,
            pointers_offset = self.pointers_offset as usize,
            spaces = ""
        );
    }
}

#[derive(Clone)]
pub struct MsgWithCauseUnderTextWithLocation<'kind, 'message, 'cause, 'src> {
    pub kind: &'kind dyn Display,
    pub message: &'message dyn Display,
    pub cause: &'cause dyn Display,
    pub file: &'src Path,
    pub line: offset32,
    pub column: offset32,
    pub absolute_column: offset32,
    pub line_text: &'src dyn Display,
    pub pointers_count: offset32,
    pub pointers_offset: offset32,
}

// IDEA(stefano, ?.?.?): add cli flag to control the amount of spaces (default 4) to display when printing tabs
impl Display for MsgWithCauseUnderTextWithLocation<'_, '_, '_, '_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let error_message = Colored {
            text: &self.message,
            fg: Fg::White,
            bg: Bg::Default,
            flags: AnsiFlag::Bold as ansi_flag,
        };

        let line_number_text = self.line.to_string();
        let line_number = Colored {
            text: &line_number_text,
            fg: Fg::LightBlue,
            bg: Bg::Default,
            flags: AnsiFlag::Bold as ansi_flag,
        };

        let line_number_padding = line_number_text.len() + 1 + BAR.text.len();

        let pointers_and_cause = Colored {
            text: &format!(
                "{spaces:^>pointers_count$} {cause}",
                spaces = "",
                pointers_count = self.pointers_count as usize,
                cause = self.cause
            ),
            fg: Fg::LightRed,
            bg: Bg::Default,
            flags: AnsiFlag::Bold as ansi_flag,
        };

        return write!(
            f,
            "{kind}: {error_message}\
            \n{AT:>at_padding$}: {path}:{line}:{column}:{absolute_column}\
            \n{BAR:>line_number_padding$}\
            \n{line_number} {BAR} {line_text}\
            \n{BAR:>line_number_padding$}{spaces:>pointers_offset$}{pointers_and_cause}",
            kind = self.kind,
            at_padding = line_number_padding - 1,
            path = self.file.display(),
            line = self.line,
            column = self.column as usize,
            absolute_column = self.absolute_column as usize,
            line_text = self.line_text,
            pointers_offset = self.pointers_offset as usize,
            spaces = "",
        );
    }
}
