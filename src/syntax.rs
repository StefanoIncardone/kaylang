/*
IDEA(stefano): rename this module to `error` and incorporate all error structs and enums, thus
removing the syntax module and bringing the ast and tokenizer modules into the root module
*/
pub mod ast;
pub mod tokenizer;

use crate::{
    src_file::{Position, SrcFile},
    Bg, Colored, Fg, Flag, AT, BAR, ERROR,
};
use std::{
    borrow::Cow,
    fmt::{Debug, Display},
    path::Path,
};

pub trait IntoErrorInfo: Debug + Clone {
    fn info(&self) -> ErrorInfo;
}

/*
IDEA(stefano): refactor to allow for errors without a proper or redundant cause message, i.e.:
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

// TODO(stefano): rename to RawError
#[derive(Debug, Clone)]
pub struct Error<K: IntoErrorInfo> {
    pub kind: K,
    /// absolute source code byte position
    pub col: usize,
    pub pointers_count: usize,
}

impl<K: IntoErrorInfo> Error<K> {
    pub fn display<'src>(&self, src: &'src SrcFile) -> ErrorDisplay<'src> {
        let Position { line, col, line_text } = src.position(self.col);
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

// TODO(stefano): rename to Error
#[derive(Debug, Clone)]
pub struct ErrorDisplay<'src> {
    pub error_message: Cow<'static, str>,
    pub file: &'src Path,
    pub line: usize,
    pub col: usize,
    pub line_text: &'src str,
    pub pointers_count: usize,
    pub error_cause_message: Cow<'static, str>,
}

impl Display for ErrorDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
                pointers_count = self.pointers_count,
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
            col = self.col,
            line_text = self.line_text,
            spaces = "",
        );
    }
}

impl std::error::Error for ErrorDisplay<'_> {}
