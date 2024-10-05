use crate::{
    color::{Bg, Colored, Fg, Flag},
    src_file::offset,
    AT, BAR, CAUSE,
};
use core::fmt::Display;
use std::path::Path;

#[derive(Clone)]
pub struct Msg<'kind, 'message> {
    pub kind: &'kind dyn Display,
    pub message: &'message dyn Display,
}

impl Display for Msg<'_, '_> {
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
    pub pointers_offset: usize,
    pub pointers_count: usize,
}

impl Display for MsgWithCauseUnderText<'_, '_, '_, '_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let error_message = Colored {
            text: self.message.to_string(),
            fg: Fg::White,
            bg: Bg::Default,
            flags: Flag::Bold,
        };

        let pointers_and_cause = Colored {
            text: format!(
                "{spaces:^>pointers_count$} {cause}",
                spaces = "",
                pointers_count = self.pointers_count,
                cause = self.cause
            ),
            fg: Fg::LightRed,
            bg: Bg::Default,
            flags: Flag::Bold,
        };

        return write!(
            f,
            "{kind}: {error_message}\
            \n{BAR}\
            \n{BAR} {line_text}\
            \n{BAR} {spaces:>pointers_offset$}{pointers_and_cause}",
            kind = self.kind,
            line_text = self.line_text,
            pointers_offset = self.pointers_offset,
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
    pub line: offset,
    pub col: offset,
    pub source_code_col: offset,
    pub line_text: &'src dyn Display,
    pub pointers_count: offset,
}

impl Display for MsgWithCauseUnderTextWithLocation<'_, '_, '_, '_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let error_message = Colored {
            text: self.message.to_string(),
            fg: Fg::White,
            bg: Bg::Default,
            flags: Flag::Bold,
        };

        let line_number = Colored {
            text: self.line.to_string(),
            fg: Fg::LightBlue,
            bg: Bg::Default,
            flags: Flag::Bold,
        };

        let line_number_padding = line_number.text.len() + 1 + BAR.text.len();

        let pointers_and_cause = Colored {
            text: format!(
                "{spaces:^>pointers_count$} {cause}",
                spaces = "",
                pointers_count = self.pointers_count as usize,
                cause = self.cause
            ),
            fg: Fg::LightRed,
            bg: Bg::Default,
            flags: Flag::Bold,
        };

        return write!(
            f,
            "{kind}: {error_message}\
            \n{AT:>at_padding$}: {path}:{line}:{col} ({source_code_col})\
            \n{BAR:>line_number_padding$}\
            \n{line_number} {BAR} {line_text}\
            \n{BAR:>line_number_padding$}{spaces:>col$}{pointers_and_cause}",
            kind = self.kind,
            at_padding = line_number_padding - 1,
            path = self.file.display(),
            line = self.line,
            col = self.col as usize,
            source_code_col = self.source_code_col as usize,
            line_text = self.line_text,
            spaces = "",
        );
    }
}
