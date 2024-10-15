pub mod ast;
pub mod src_file;
pub mod syntax_tree;
pub mod tokenizer;

use self::src_file::{column32, line32, offset32, DisplayPosition, SrcCode};
use crate::{error::MsgWithCauseUnderTextWithLocation, ERROR};
use core::fmt::{Debug, Display};
use std::{borrow::Cow, path::Path};

pub trait IntoErrorInfo: Debug + Clone {
    fn info(&self) -> ErrorInfo;
}

#[derive(Debug, Clone)]
pub struct ErrorInfo {
    pub error_message: Cow<'static, str>,
    pub error_cause_message: Cow<'static, str>,
}

/* TODO(stefano):
allow pointers to start past the end of the line
IDEA: introduce a pointers_col field
*/
#[derive(Debug, Clone)]
pub struct Error<K: IntoErrorInfo> {
    pub kind: K,
    /// absolute source code byte position
    pub col: offset32,
    pub pointers_count: column32,
}

impl<K: IntoErrorInfo> Error<K> {
    pub fn display<'code, 'path: 'code>(
        &self,
        src: &SrcCode<'code, 'path>,
    ) -> ErrorDisplay<'code, 'path> {
        let DisplayPosition { line, column, display_column } = src.display_position(self.col);
        let line_span = src.lines[line as usize - 1];
        let line_text = &src.code()[line_span.start as usize..line_span.end as usize];

        let ErrorInfo { error_message, error_cause_message } = self.kind.info();
        return ErrorDisplay {
            error_message,
            file: src.path(),
            line,
            column,
            absolute_column: self.col,
            line_text,
            pointers_count: self.pointers_count,
            pointers_offset: display_column,
            error_cause_message,
        };
    }
}

#[derive(Debug, Clone)]
pub struct ErrorDisplay<'code, 'path: 'code> {
    pub error_message: Cow<'static, str>,
    pub file: &'path Path,
    pub line: line32,
    pub column: column32,
    pub absolute_column: offset32,
    pub line_text: &'code str,
    pub pointers_count: column32,
    pub pointers_offset: column32,
    pub error_cause_message: Cow<'static, str>,
}

impl Display for ErrorDisplay<'_, '_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let error = MsgWithCauseUnderTextWithLocation {
            kind: &ERROR,
            message: &self.error_message,
            cause: &self.error_cause_message,
            file: self.file,
            line: self.line,
            column: self.column,
            absolute_column: self.absolute_column,
            line_text: &self.line_text,
            pointers_count: self.pointers_count,
            pointers_offset: self.pointers_offset,
        };
        return write!(f, "{error}");
    }
}

impl core::error::Error for ErrorDisplay<'_, '_> {}
