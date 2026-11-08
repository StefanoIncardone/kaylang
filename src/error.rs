use crate::{
    AT, BAR, color::{AnsiColor, Colored, ansi_flag}, front_end::{MsgSeverity, src_file::Position},
};
use back_to_front::{cli::{AnsiBg, AnsiFg, AnsiFlag}, usize32, ascii, utf32};
use unicode_width::UnicodeWidthChar as _;

// IDEA(stefano): move to back-to-front
pub(crate) trait PointersCount {
    #[must_use]
    fn pointers_count(&self) -> usize32;
}

impl PointersCount for str {
    fn pointers_count(&self) -> usize32 {
        let mut len = 0;
        for character in self.chars() {
            len += character.width_cjk().unwrap_or_default();
        }
        if len == 0 {
            len = 1;
        }
        return len as usize32;
    }
}

impl PointersCount for utf32 {
    #[inline]
    fn pointers_count(&self) -> usize32 {
        let mut len = self.width_cjk().unwrap_or_default();
        if len == 0 {
            len = 1;
        }
        return len as usize32;
    }
}

impl PointersCount for ascii {
    #[inline(always)]
    fn pointers_count(&self) -> usize32 {
        return (*self as char).pointers_count();
    }
}

// #[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
// pub enum MsgLocation<'loc> {
//     #[default]
//     None,
//     Path(&'loc Path, Option<(usize32, Option<usize32>)>),
//     Str(&'loc str, Option<(usize32, Option<usize32>)>),
// }

// #[derive(Clone, Debug)]
// pub struct Msg<'kind, 'message, 'loc> {
//     pub severity: MsgSeverity,
//     pub kind: &'kind str,
//     pub message: &'message str,
//     pub cause:
//     pub location: MsgLocation<'loc>,
// }

#[derive(Clone)]
#[must_use]
pub struct MsgSimple<'kind, 'message> {
    pub color: AnsiColor,
    pub kind: &'kind str,
    pub message: &'message str,
}

impl core::fmt::Display for MsgSimple<'_, '_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let kind = Colored { text: self.kind, color: self.color };
        return write!(f, "{kind}: {message}", message = self.message);
    }
}

#[derive(Clone)]
#[must_use]
pub struct MsgWithCause<'kind, 'message, 'cause> {
    pub color: AnsiColor,
    pub kind: &'kind str,
    pub message: &'message str,
    pub cause: &'cause str,
}

impl core::fmt::Display for MsgWithCause<'_, '_, '_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let kind = Colored { text: self.kind, color: self.color };
        let cause = Colored { text: self.cause, color: self.color };
        return write!(
            f,
            "{kind}: {message}\
            \n{cause}: {cause}",
            message = self.message,
        );
    }
}

#[derive(Clone)]
#[must_use]
pub struct MsgWithCauseUnderText<'kind, 'message, 'cause, 'src> {
    pub color: AnsiColor,
    pub kind: &'kind str,
    pub message: &'message str,
    pub cause: &'cause str,
    pub line_text: &'src str,
    pub pointers_count: usize32,
    pub pointers_offset: usize32,
}

impl core::fmt::Display for MsgWithCauseUnderText<'_, '_, '_, '_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let kind = Colored { text: self.kind, color: self.color };
        let cause = Colored { text: self.cause, color: self.color };
        let bar = Colored { text: BAR, color: MsgSeverity::ACCENT_COLOR };

        let error_message = Colored { text: self.message, color: AnsiColor { fg: AnsiFg::White, bg: AnsiBg::Default, flags: AnsiFlag::Bold as ansi_flag } };
        let pointers = Colored {
            text: &format!("{spaces:^>pointers_count$}", spaces = "", pointers_count = self.pointers_count as usize),
            color: self.color,
        };

        return write!(
            f,
            "{kind}: {error_message}\
            \n{bar}\
            \n{bar} {line_text}\
            \n{bar} {spaces:>pointers_offset$}{pointers} {cause}",
            line_text = self.line_text,
            pointers_offset = self.pointers_offset as usize,
            spaces = ""
        );
    }
}

#[derive(Clone)]
#[must_use]
pub struct MsgWithCauseUnderTextWithLocation<'kind, 'message, 'cause, 'src, 'path> {
    pub color: AnsiColor,
    pub kind: &'kind str,
    pub message: &'message str,
    pub cause: &'cause str,
    pub position: Position<'path>,
    pub line_text: &'src str,
    pub pointers_count: usize32,
}

// IDEA(stefano): add cli flag to control the amount of spaces (default 4) to display when printing tabs
impl core::fmt::Display for MsgWithCauseUnderTextWithLocation<'_, '_, '_, '_, '_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let kind = Colored { text: self.kind, color: self.color };
        let cause = Colored { text: self.cause, color: self.color };
        let at = Colored { text: AT, color: self.color };
        let bar = Colored { text: BAR, color: MsgSeverity::ACCENT_COLOR };

        // TODO(stefano): replace with own digits counting routine that does not allocate a string
        let line_number_text = self.position.line.to_string();
        let line_number = Colored { text: &line_number_text, color: MsgSeverity::ACCENT_COLOR };
        let line_number_padding = line_number_text.len() + 1 + BAR.len();

        let error_message = Colored { text: self.message, color: AnsiColor { fg: AnsiFg::White, bg: AnsiBg::Default, flags: AnsiFlag::Bold as ansi_flag } };
        let pointers = Colored {
            text: &format!("{spaces:^>pointers_count$}", spaces = "", pointers_count = self.pointers_count as usize),
            color: self.color,
        };

        return write!(
            f,
            "{kind}: {error_message}\
            \n{at:>at_padding$}: {position}\
            \n{bar:>line_number_padding$}\
            \n{line_number} {bar} {line_text}\
            \n{bar:>line_number_padding$}{spaces:>pointers_offset$}{pointers} {cause}",
            at_padding = kind.text.len(),
            position = self.position,
            line_text = self.line_text,
            pointers_offset = self.position.width as usize,
            spaces = "",
        );
    }
}
