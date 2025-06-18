// TODO(stefano): move everything in this file to `error.rs`
pub mod abstract_syntax_tree;
pub mod ast;
pub mod src_file;
pub mod tokenizer;

use self::src_file::{DisplayPosition, SrcCode};
use crate::{error::MsgWithCauseUnderTextWithLocation, ERROR};
use core::fmt::{Debug, Display};
extern crate alloc;
use alloc::borrow::Cow;
use back_to_front::offset32;
use std::path::Path;

pub trait IntoErrorInfo: Debug + Clone {
    fn info(&self) -> ErrorInfo;
}

#[derive(Debug, Clone)]
pub struct ErrorInfo {
    pub error_message: Cow<'static, str>,
    pub error_cause_message: Cow<'static, str>,
}

// IDEA(stefano): introduce a pointers_col field, allow pointers to start past the end of the line
#[derive(Debug, Clone)]
pub struct Error<K: IntoErrorInfo> {
    pub kind: K,
    /// absolute source code byte position
    pub col: offset32,
    // IDEA(stefano): rename to `display_len`
    pub pointers_count: offset32,
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
    pub line: offset32,
    pub column: offset32,
    pub absolute_column: offset32,
    pub line_text: &'code str,
    pub pointers_count: offset32,
    pub pointers_offset: offset32,
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

#[expect(clippy::missing_trait_methods, reason = "using default implementations")]
impl core::error::Error for ErrorDisplay<'_, '_> {}
