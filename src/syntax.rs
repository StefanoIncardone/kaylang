pub mod ast;
pub mod tokenizer;

use crate::{
    src_file::{Position, SrcFile},
    Bg, Colored, Fg, Flag, AT, BAR, ERROR,
};
use core::fmt::{Debug, Display};
use std::{borrow::Cow, path::Path};

pub trait IntoErrorInfo: Debug + Clone {
    fn info(&self) -> ErrorInfo;
}

/* IDEA(stefano):
refactor to allow for errors without a proper or redundant cause message, i.e.:
Error: unrecognized character
at: file.kay:21:5
   |
21 | blah . dadasd
   |      ^ unrecognized

would become:
Error: unrecognized character
at: file.kay:21:5
   |
21 | blah . dadasd
   |      ^
*/
#[derive(Debug, Clone)]
pub struct ErrorInfo {
    pub error_message: Cow<'static, str>,
    pub error_cause_message: Cow<'static, str>,
}

#[derive(Debug, Clone)]
pub struct Error<K: IntoErrorInfo> {
    pub kind: K,
    /// absolute source code byte position
    pub col: u32,
    pub pointers_count: u32,
}

impl<K: IntoErrorInfo> Error<K> {
    pub fn display<'src>(&self, src: &'src SrcFile) -> ErrorDisplay<'src> {
        let Position { line, col } = src.position(self.col);
        let line_span = &src.lines[line as usize - 1];
        let line_text = &src.code[line_span.start as usize..line_span.end as usize];

        let ErrorInfo { error_message, error_cause_message } = self.kind.info();
        return ErrorDisplay {
            error_message,
            file: &src.path,
            line,
            col,
            line_text,
            pointers_count: self.pointers_count,
            error_cause_message,
        };
    }
}

#[derive(Debug, Clone)]
pub struct ErrorDisplay<'src> {
    pub error_message: Cow<'static, str>,
    pub file: &'src Path,
    pub line: u32,
    pub col: u32,
    pub line_text: &'src str,
    pub pointers_count: u32,
    pub error_cause_message: Cow<'static, str>,
}

impl Display for ErrorDisplay<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let error_message = Colored {
            text: &self.error_message,
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
                cause = self.error_cause_message
            ),
            fg: Fg::LightRed,
            bg: Bg::Default,
            flags: Flag::Bold,
        };

        return write!(
            f,
            "{ERROR}: {error_message}\
            \n{AT:>at_padding$}: {path}:{line}:{col}\
            \n{BAR:>line_number_padding$}\
            \n{line_number} {BAR} {line_text}\
            \n{BAR:>line_number_padding$}{spaces:>col$}{pointers_and_cause}",
            at_padding = line_number_padding - 1,
            path = self.file.display(),
            line = self.line,
            col = self.col as usize,
            line_text = self.line_text,
            spaces = "",
        );
    }
}

impl std::error::Error for ErrorDisplay<'_> {}
