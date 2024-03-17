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

pub trait SyntaxErrorKind: Debug + Display + Clone {}

pub trait SyntaxErrorCause: Debug + Display + Clone {}

#[derive(Debug, Clone)]
pub struct RawSyntaxError<K: SyntaxErrorKind, C: SyntaxErrorCause> {
    pub kind: K,
    pub cause: C,
    /// absolute source code byte position
    pub col: usize,
    pub len: usize,
}

#[derive(Debug, Clone)]
pub struct SyntaxError<'src, K: SyntaxErrorKind, C: SyntaxErrorCause> {
    pub path: &'src Path,
    pub position: Position,
    pub len: usize,
    pub line_text: &'src str,
    pub kind: K,
    pub cause: C,
}

impl<'src, K: SyntaxErrorKind, C: SyntaxErrorCause> SyntaxError<'src, K, C> {
    pub(crate) fn from_raw(src: &'src SrcFile, raw: &RawSyntaxError<K, C>) -> Self {
        let (position, line_text) = Position::new(src, raw.col);
        Self {
            path: &src.path.inner,
            position,
            len: raw.len,
            line_text,
            kind: raw.kind.clone(),
            cause: raw.cause.clone(),
        }
    }
}

impl<K: SyntaxErrorKind, C: SyntaxErrorCause> Display for SyntaxError<'_, K, C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let error_msg = Colored {
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

        write!(
            f,
            "{ERROR}: {error_msg}\
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
        )
    }
}

impl<K: SyntaxErrorKind, C: SyntaxErrorCause> std::error::Error for SyntaxError<'_, K, C> {}

#[derive(Debug, Clone)]
pub struct SyntaxErrorsIter<'err, 'src: 'err, K: SyntaxErrorKind, C: SyntaxErrorCause> {
    pub(crate) src: &'src SrcFile,
    pub(crate) raw_errors: Iter<'err, RawSyntaxError<K, C>>,
}

impl<'err, 'src: 'err, K: SyntaxErrorKind, C: SyntaxErrorCause> Iterator
    for SyntaxErrorsIter<'err, 'src, K, C>
{
    type Item = SyntaxError<'src, K, C>;

    fn next(&mut self) -> Option<Self::Item> {
        let raw_error = self.raw_errors.next()?;
        Some(SyntaxError::from_raw(self.src, raw_error))
    }
}

impl<'err, 'src: 'err, K: SyntaxErrorKind, C: SyntaxErrorCause> DoubleEndedIterator
    for SyntaxErrorsIter<'err, 'src, K, C>
{
    fn next_back(&mut self) -> Option<Self::Item> {
        let raw_error = self.raw_errors.next_back()?;
        Some(SyntaxError::from_raw(self.src, raw_error))
    }
}

impl<'err, 'src: 'err, K: SyntaxErrorKind, C: SyntaxErrorCause> ExactSizeIterator
    for SyntaxErrorsIter<'err, 'src, K, C>
{
    fn len(&self) -> usize {
        self.raw_errors.len()
    }
}

impl<'err, 'src: 'err, K: SyntaxErrorKind, C: SyntaxErrorCause> FusedIterator
    for SyntaxErrorsIter<'err, 'src, K, C>
{
}

#[allow(dead_code)]
impl<'err, 'src: 'err, K: SyntaxErrorKind, C: SyntaxErrorCause> SyntaxErrorsIter<'err, 'src, K, C> {
    pub fn src(&self) -> &SrcFile {
        self.src
    }

    pub fn as_raw(&self) -> &[RawSyntaxError<K, C>] {
        self.raw_errors.as_slice()
    }

    pub fn into_raw(self) -> Vec<RawSyntaxError<K, C>> {
        self.raw_errors.cloned().collect()
    }
}

#[derive(Debug, Clone)]
pub struct SyntaxErrorsIntoIter<'src, K: SyntaxErrorKind, C: SyntaxErrorCause> {
    pub(crate) src: &'src SrcFile,
    pub(crate) raw_errors: IntoIter<RawSyntaxError<K, C>>,
}

impl<'src, K: SyntaxErrorKind, C: SyntaxErrorCause> Iterator for SyntaxErrorsIntoIter<'src, K, C> {
    type Item = SyntaxError<'src, K, C>;

    fn next(&mut self) -> Option<Self::Item> {
        let raw_error = self.raw_errors.next()?;
        Some(SyntaxError::from_raw(self.src, &raw_error))
    }
}

impl<'src, K: SyntaxErrorKind, C: SyntaxErrorCause> DoubleEndedIterator
    for SyntaxErrorsIntoIter<'src, K, C>
{
    fn next_back(&mut self) -> Option<Self::Item> {
        let raw_error = self.raw_errors.next_back()?;
        Some(SyntaxError::from_raw(self.src, &raw_error))
    }
}

impl<'src, K: SyntaxErrorKind, C: SyntaxErrorCause> ExactSizeIterator
    for SyntaxErrorsIntoIter<'src, K, C>
{
    fn len(&self) -> usize {
        self.raw_errors.len()
    }
}

impl<'src, K: SyntaxErrorKind, C: SyntaxErrorCause> FusedIterator
    for SyntaxErrorsIntoIter<'src, K, C>
{
}

#[allow(dead_code)]
impl<'src, K: SyntaxErrorKind, C: SyntaxErrorCause> SyntaxErrorsIntoIter<'src, K, C> {
    pub fn src(&self) -> &SrcFile {
        self.src
    }

    pub fn as_raw(&self) -> &[RawSyntaxError<K, C>] {
        self.raw_errors.as_slice()
    }

    pub fn into_raw(self) -> Vec<RawSyntaxError<K, C>> {
        self.raw_errors.collect()
    }
}

#[derive(Debug)]
pub struct SyntaxErrors<'src, K: SyntaxErrorKind, C: SyntaxErrorCause> {
    pub(crate) src: &'src SrcFile,
    pub(crate) raw_errors: Vec<RawSyntaxError<K, C>>,
}

impl<'src, K: SyntaxErrorKind, C: SyntaxErrorCause> IntoIterator for SyntaxErrors<'src, K, C> {
    type IntoIter = SyntaxErrorsIntoIter<'src, K, C>;
    type Item = SyntaxError<'src, K, C>;

    fn into_iter(self) -> Self::IntoIter {
        Self::IntoIter { src: self.src, raw_errors: self.raw_errors.into_iter() }
    }
}

#[allow(dead_code)]
impl<'src, K: SyntaxErrorKind, C: SyntaxErrorCause> SyntaxErrors<'src, K, C> {
    pub fn iter(&self) -> SyntaxErrorsIter<'_, 'src, K, C> {
        SyntaxErrorsIter { src: self.src, raw_errors: self.raw_errors.iter() }
    }

    pub fn get(&self, index: usize) -> Option<SyntaxError<'src, K, C>> {
        let raw_error = self.raw_errors.get(index)?;
        Some(SyntaxError::from_raw(self.src, raw_error))
    }

    pub fn src(&self) -> &SrcFile {
        self.src
    }

    pub fn as_raw(&self) -> &[RawSyntaxError<K, C>] {
        &self.raw_errors
    }

    pub fn into_raw(self) -> Vec<RawSyntaxError<K, C>> {
        self.raw_errors
    }
}
