pub mod ast;
pub mod tokenizer;

use crate::{
    error::MsgWithCauseUnderTextWithLocation,
    src_file::{offset, Position, SrcFile},
    ERROR,
};
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

#[derive(Debug, Clone)]
pub struct Error<K: IntoErrorInfo> {
    pub kind: K,
    /// absolute source code byte position
    pub col: offset,
    pub pointers_count: offset,
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
    pub line: offset,
    pub col: offset,
    pub line_text: &'src str,
    pub pointers_count: offset,
    pub error_cause_message: Cow<'static, str>,
}

impl Display for ErrorDisplay<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let error = MsgWithCauseUnderTextWithLocation {
            kind: &ERROR,
            message: &self.error_message,
            cause: &self.error_cause_message,
            file: self.file,
            line: self.line,
            col: self.col,
            line_text: &self.line_text,
            pointers_count: self.pointers_count,
        };
        return write!(f, "{error}");
    }
}

impl std::error::Error for ErrorDisplay<'_> {}
