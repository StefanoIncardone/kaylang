pub mod ast;
pub mod op;
pub mod tokenizer;
pub mod types;

use crate::{
    src_file::{Position, SrcFile},
    Bg, Colored, Fg, Flag, AT, BAR, ERROR,
};
use std::{
    fmt::{Debug, Display},
    iter::FusedIterator,
    path::Path,
    slice::Iter,
    vec::IntoIter,
};

pub trait ErrorKind: Debug + Display + Clone {}

pub trait ErrorCause: Debug + Display + Clone {}

#[derive(Debug, Clone)]
pub struct RawError<K: ErrorKind, C: ErrorCause> {
    pub kind: K,
    pub cause: C,
    /// absolute source code byte position
    pub col: usize,
    pub len: usize,
}

#[derive(Debug, Clone)]
pub struct Error<'src, K: ErrorKind, C: ErrorCause> {
    pub path: &'src Path,
    pub position: Position,
    pub len: usize,
    pub line_text: &'src str,
    pub kind: K,
    pub cause: C,
}

impl<'src, K: ErrorKind, C: ErrorCause> Error<'src, K, C> {
    pub(crate) fn from_raw(src: &'src SrcFile, raw: &RawError<K, C>) -> Self {
        let (position, line_text) = Position::new(src, raw.col);
        return Self {
            path: &src.path,
            position,
            len: raw.len,
            line_text,
            kind: raw.kind.clone(),
            cause: raw.cause.clone(),
        };
    }
}

impl<K: ErrorKind, C: ErrorCause> Display for Error<'_, K, C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let msg = Colored {
            text: &self.kind.to_string(),
            fg: Fg::White,
            bg: Bg::Default,
            flags: Flag::Bold,
        };
        let line_number = Colored {
            text: self.position.line.to_string(),
            fg: Fg::LightBlue,
            bg: Bg::Default,
            flags: Flag::Bold,
        };
        let line_number_padding = line_number.text.len() + 1 + BAR.text.len();
        let pointers_and_cause = Colored {
            text: format!(
                "{spaces:^>len$} {cause}",
                spaces = "",
                len = self.len,
                cause = self.cause
            ),
            fg: Fg::LightRed,
            bg: Bg::Default,
            flags: Flag::Bold,
        };

        return write!(
            f,
            "{ERROR}: {msg}\
            \n{AT:>at_padding$}: {path}:{line}:{col}\
            \n{BAR:>line_number_padding$}\
            \n{line_number} {BAR} {line_text}\
            \n{BAR:>line_number_padding$}{spaces:>col$}{pointers_and_cause}",
            at_padding = line_number_padding - 1,
            path = self.path.display(),
            line = self.position.line,
            col = self.position.col,
            line_text = self.line_text,
            spaces = "",
        );
    }
}

impl<K: ErrorKind, C: ErrorCause> std::error::Error for Error<'_, K, C> {}

#[derive(Debug, Clone)]
pub struct ErrorsIter<'err, 'src: 'err, K: ErrorKind, C: ErrorCause> {
    pub(crate) src: &'src SrcFile,
    pub(crate) raw_errors: Iter<'err, RawError<K, C>>,
}

impl<'err, 'src: 'err, K: ErrorKind, C: ErrorCause> Iterator for ErrorsIter<'err, 'src, K, C> {
    type Item = Error<'src, K, C>;

    fn next(&mut self) -> Option<Self::Item> {
        let raw_error = self.raw_errors.next()?;
        return Some(Error::from_raw(self.src, raw_error));
    }
}

impl<'err, 'src: 'err, K: ErrorKind, C: ErrorCause> DoubleEndedIterator
    for ErrorsIter<'err, 'src, K, C>
{
    fn next_back(&mut self) -> Option<Self::Item> {
        let raw_error = self.raw_errors.next_back()?;
        return Some(Error::from_raw(self.src, raw_error));
    }
}

impl<'err, 'src: 'err, K: ErrorKind, C: ErrorCause> ExactSizeIterator
    for ErrorsIter<'err, 'src, K, C>
{
    fn len(&self) -> usize {
        return self.raw_errors.len();
    }
}

impl<'err, 'src: 'err, K: ErrorKind, C: ErrorCause> FusedIterator for ErrorsIter<'err, 'src, K, C> {}

#[allow(dead_code)]
impl<'err, 'src: 'err, K: ErrorKind, C: ErrorCause> ErrorsIter<'err, 'src, K, C> {
    #[must_use]
    pub const fn src(&self) -> &SrcFile {
        return self.src;
    }

    #[must_use]
    pub fn as_raw(&self) -> &[RawError<K, C>] {
        return self.raw_errors.as_slice();
    }

    #[must_use]
    pub fn into_raw(self) -> Vec<RawError<K, C>> {
        return self.raw_errors.cloned().collect();
    }
}

#[derive(Debug, Clone)]
pub struct ErrorsIntoIter<'src, K: ErrorKind, C: ErrorCause> {
    pub(crate) src: &'src SrcFile,
    pub(crate) raw_errors: IntoIter<RawError<K, C>>,
}

impl<'src, K: ErrorKind, C: ErrorCause> Iterator for ErrorsIntoIter<'src, K, C> {
    type Item = Error<'src, K, C>;

    fn next(&mut self) -> Option<Self::Item> {
        let raw_error = self.raw_errors.next()?;
        return Some(Error::from_raw(self.src, &raw_error));
    }
}

impl<'src, K: ErrorKind, C: ErrorCause> DoubleEndedIterator for ErrorsIntoIter<'src, K, C> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let raw_error = self.raw_errors.next_back()?;
        return Some(Error::from_raw(self.src, &raw_error));
    }
}

impl<'src, K: ErrorKind, C: ErrorCause> ExactSizeIterator for ErrorsIntoIter<'src, K, C> {
    fn len(&self) -> usize {
        return self.raw_errors.len();
    }
}

impl<'src, K: ErrorKind, C: ErrorCause> FusedIterator for ErrorsIntoIter<'src, K, C> {}

#[allow(dead_code)]
impl<'src, K: ErrorKind, C: ErrorCause> ErrorsIntoIter<'src, K, C> {
    #[must_use]
    pub const fn src(&self) -> &SrcFile {
        return self.src;
    }

    #[must_use]
    pub fn as_raw(&self) -> &[RawError<K, C>] {
        return self.raw_errors.as_slice();
    }

    #[must_use]
    pub fn into_raw(self) -> Vec<RawError<K, C>> {
        return self.raw_errors.collect();
    }
}

#[derive(Debug)]
pub struct Errors<'src, K: ErrorKind, C: ErrorCause> {
    pub(crate) src: &'src SrcFile,
    pub(crate) raw_errors: Vec<RawError<K, C>>,
}

impl<'src, K: ErrorKind, C: ErrorCause> IntoIterator for Errors<'src, K, C> {
    type IntoIter = ErrorsIntoIter<'src, K, C>;
    type Item = Error<'src, K, C>;

    fn into_iter(self) -> Self::IntoIter {
        return Self::IntoIter { src: self.src, raw_errors: self.raw_errors.into_iter() };
    }
}

#[allow(clippy::iter_without_into_iter)]
impl<'src, K: ErrorKind, C: ErrorCause> Errors<'src, K, C> {
    #[must_use]
    pub fn iter(&self) -> ErrorsIter<'_, 'src, K, C> {
        return ErrorsIter { src: self.src, raw_errors: self.raw_errors.iter() };
    }

    #[must_use]
    pub fn get(&self, index: usize) -> Option<Error<'src, K, C>> {
        let raw_error = self.raw_errors.get(index)?;
        return Some(Error::from_raw(self.src, raw_error));
    }

    #[must_use]
    pub const fn src(&self) -> &SrcFile {
        return self.src;
    }

    #[must_use]
    pub fn as_raw(&self) -> &[RawError<K, C>] {
        return &self.raw_errors;
    }

    #[must_use]
    pub fn into_raw(self) -> Vec<RawError<K, C>> {
        return self.raw_errors;
    }
}
