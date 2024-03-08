// IDEA(stefano): split error kind enums/structs into kind and msg/cause/... enums

use crate::{
    color::{Bg, Colored, Fg, Flag},
    logging::{AT, BAR, CAUSE, ERROR},
    src_file::{Position, SrcFile},
};
use std::{
    borrow::Cow,
    fmt::{Debug, Display},
    iter::FusedIterator,
    path::Path,
    slice::Iter,
    vec::IntoIter,
};

pub trait ErrorInfo {
    type Info;

    fn info(&self) -> Self::Info;
}

#[derive(Debug, Clone)]
pub struct BackEndErrorInfo {
    pub msg: Cow<'static, str>,
    pub cause: Cow<'static, str>,
}

pub trait BackEndErrorKind: Debug + ErrorInfo<Info = BackEndErrorInfo> {}

#[derive(Debug)]
pub struct BackEndError<K: BackEndErrorKind> {
    pub kind: K,
}

impl<K: BackEndErrorKind> Display for BackEndError<K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let BackEndErrorInfo { msg, cause } = self.kind.info();

        write!(
            f,
            "{ERROR}: {msg}\
            \n{CAUSE}: {cause}"
        )
    }
}

impl<K: BackEndErrorKind> std::error::Error for BackEndError<K> {}

#[derive(Debug, Clone)]
pub struct SrcFileErrorInfo {
    pub msg: Cow<'static, str>,
    pub cause: Cow<'static, str>,
}

pub trait SrcFileErrorKind: Debug + ErrorInfo<Info = SrcFileErrorInfo> {}

#[derive(Debug)]
pub struct SrcFileError<K: SrcFileErrorKind> {
    pub kind: K,
}

impl<K: SrcFileErrorKind> Display for SrcFileError<K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let SrcFileErrorInfo { msg, cause } = self.kind.info();

        write!(
            f,
            "{ERROR}: {msg}\
            \n{CAUSE}: {cause}"
        )
    }
}

impl<K: SrcFileErrorKind> std::error::Error for SrcFileError<K> {}

pub trait CliErrorKind: Debug + Clone + ErrorInfo<Info = CliErrorInfo> {}

#[derive(Debug, Clone)]
pub struct CliErrorInfo {
    pub msg: Cow<'static, str>,
}

// TODO(stefano): add information about the command line arguments and pointers to the place where
// the error occured (akin to syntax errors)
#[derive(Debug, Clone)]
pub struct CliError<K: CliErrorKind> {
    pub kind: K,
}

impl<K: CliErrorKind> Display for CliError<K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let CliErrorInfo { msg } = self.kind.info();

        write!(f, "{ERROR}: {msg}")
    }
}

impl<K: CliErrorKind> std::error::Error for CliError<K> {}

#[derive(Debug, Clone)]
pub struct SyntaxErrorInfo {
    pub msg: Cow<'static, str>,
    pub help_msg: Cow<'static, str>,
}

pub trait SyntaxErrorKind: Debug + Clone + ErrorInfo<Info = SyntaxErrorInfo> {}

#[derive(Debug, Clone)]
pub(crate) struct RawSyntaxError<K: SyntaxErrorKind> {
    pub(crate) kind: K,
    /// absolute source code byte position
    pub(crate) col: usize,
    pub(crate) len: usize,
}

#[derive(Debug, Clone)]
pub(crate) struct SyntaxErrorsIter<'err, 'src: 'err, K: SyntaxErrorKind> {
    pub(crate) src: &'src SrcFile,
    pub(crate) raw_errors: Iter<'err, RawSyntaxError<K>>,
}

impl<'err, 'src: 'err, K: SyntaxErrorKind> Iterator for SyntaxErrorsIter<'err, 'src, K> {
    type Item = SyntaxError<'src, K>;

    fn next(&mut self) -> Option<Self::Item> {
        let raw_error = self.raw_errors.next()?;
        Some(SyntaxError::from_raw(self.src, raw_error))
    }
}

impl<'err, 'src: 'err, K: SyntaxErrorKind> DoubleEndedIterator for SyntaxErrorsIter<'err, 'src, K> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let raw_error = self.raw_errors.next_back()?;
        Some(SyntaxError::from_raw(self.src, raw_error))
    }
}

impl<'err, 'src: 'err, K: SyntaxErrorKind> ExactSizeIterator for SyntaxErrorsIter<'err, 'src, K> {
    fn len(&self) -> usize {
        self.raw_errors.len()
    }
}

impl<'err, 'src: 'err, K: SyntaxErrorKind> FusedIterator for SyntaxErrorsIter<'err, 'src, K> {}

#[allow(dead_code)]
impl<'err, 'src: 'err, K: SyntaxErrorKind> SyntaxErrorsIter<'err, 'src, K> {
    pub(crate) fn src(&self) -> &SrcFile {
        self.src
    }

    pub(crate) fn as_raw(&self) -> &[RawSyntaxError<K>] {
        self.raw_errors.as_slice()
    }

    pub(crate) fn into_raw(self) -> Vec<RawSyntaxError<K>> {
        self.raw_errors.cloned().collect()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct SyntaxErrorsIntoIter<'src, K: SyntaxErrorKind> {
    pub(crate) src: &'src SrcFile,
    pub(crate) raw_errors: IntoIter<RawSyntaxError<K>>,
}

impl<'src, K: SyntaxErrorKind> Iterator for SyntaxErrorsIntoIter<'src, K> {
    type Item = SyntaxError<'src, K>;

    fn next(&mut self) -> Option<Self::Item> {
        let raw_error = self.raw_errors.next()?;
        Some(SyntaxError::from_raw(self.src, &raw_error))
    }
}

impl<'src, K: SyntaxErrorKind> DoubleEndedIterator for SyntaxErrorsIntoIter<'src, K> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let raw_error = self.raw_errors.next_back()?;
        Some(SyntaxError::from_raw(self.src, &raw_error))
    }
}

impl<'src, K: SyntaxErrorKind> ExactSizeIterator for SyntaxErrorsIntoIter<'src, K> {
    fn len(&self) -> usize {
        self.raw_errors.len()
    }
}

impl<'src, K: SyntaxErrorKind> FusedIterator for SyntaxErrorsIntoIter<'src, K> {}

#[allow(dead_code)]
impl<'src, K: SyntaxErrorKind> SyntaxErrorsIntoIter<'src, K> {
    pub(crate) fn src(&self) -> &SrcFile {
        self.src
    }

    pub(crate) fn as_raw(&self) -> &[RawSyntaxError<K>] {
        self.raw_errors.as_slice()
    }

    pub(crate) fn as_mut_raw(&mut self) -> &mut [RawSyntaxError<K>] {
        self.raw_errors.as_mut_slice()
    }

    pub(crate) fn into_raw(self) -> Vec<RawSyntaxError<K>> {
        self.raw_errors.collect()
    }
}

#[derive(Debug)]
pub(crate) struct SyntaxErrors<'src, K: SyntaxErrorKind> {
    pub(crate) src: &'src SrcFile,
    pub(crate) raw_errors: Vec<RawSyntaxError<K>>,
}

impl<'src, K: SyntaxErrorKind> IntoIterator for SyntaxErrors<'src, K> {
    type IntoIter = SyntaxErrorsIntoIter<'src, K>;
    type Item = SyntaxError<'src, K>;

    fn into_iter(self) -> Self::IntoIter {
        Self::IntoIter { src: self.src, raw_errors: self.raw_errors.into_iter() }
    }
}

#[allow(dead_code)]
impl<'src, K: SyntaxErrorKind> SyntaxErrors<'src, K> {
    pub(crate) fn iter(&self) -> SyntaxErrorsIter<'_, 'src, K> {
        SyntaxErrorsIter { src: self.src, raw_errors: self.raw_errors.iter() }
    }

    pub(crate) fn get(&self, idx: usize) -> Option<SyntaxError<'src, K>> {
        let raw_error = self.raw_errors.get(idx)?;
        Some(SyntaxError::from_raw(self.src, raw_error))
    }

    pub(crate) fn src(&self) -> &SrcFile {
        self.src
    }

    pub(crate) fn as_raw(&self) -> &[RawSyntaxError<K>] {
        &self.raw_errors
    }

    pub(crate) fn as_mut_raw(&mut self) -> &mut [RawSyntaxError<K>] {
        &mut self.raw_errors
    }

    pub(crate) fn into_raw(self) -> Vec<RawSyntaxError<K>> {
        self.raw_errors
    }
}

#[derive(Debug, Clone)]
pub struct SyntaxError<'src, K: SyntaxErrorKind> {
    pub path: &'src Path,
    pub position: Position,
    pub len: usize,
    pub line_text: &'src str,
    pub kind: K,
}

impl<'src, K: SyntaxErrorKind> SyntaxError<'src, K> {
    pub(crate) fn from_raw(src: &'src SrcFile, raw: &RawSyntaxError<K>) -> Self {
        let (position, line_text) = Position::new(src, raw.col);
        Self { path: &src.path, position, len: raw.len, line_text, kind: raw.kind.clone() }
    }
}

impl<K: SyntaxErrorKind> Display for SyntaxError<'_, K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let SyntaxErrorInfo { msg, help_msg } = self.kind.info();

        let line_number = self.position.line.to_string();
        let line_number_padding = line_number.len() + 1 + BAR.text.len();

        write!(
            f,
            "{ERROR}: {error_msg}\
            \n{AT:>at_padding$}: {path}:{line}:{col}\
            \n{BAR:>line_number_padding$}\
            \n{line_number} {BAR} {line_text}\
            \n{BAR:>line_number_padding$} {spaces:>pointers_padding$}{pointers_and_help_msg}",
            error_msg = Colored { text: msg, fg: Fg::White, bg: Bg::Default, flags: Flag::Bold },
            at_padding = line_number_padding - 1,
            path = self.path.display(),
            line = self.position.line,
            col = self.position.col,
            line_number = Colored { text: line_number, fg: Fg::LightBlue, bg: Bg::Default, flags: Flag::Bold },
            line_text = self.line_text,
            spaces = "",
            pointers_padding = self.position.col - 1,
            pointers_and_help_msg = Colored {
                text: format!("{spaces:^>len$} {help_msg}", spaces = "", len = self.len),
                fg: Fg::LightRed,
                bg: Bg::Default,
                flags: Flag::Bold,
            }
        )
    }
}

impl<K: SyntaxErrorKind> std::error::Error for SyntaxError<'_, K> {}
