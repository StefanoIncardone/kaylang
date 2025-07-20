pub mod ast;
pub mod src_file;
pub mod syntax_tree;
pub mod tokenizer;
pub mod typed_abstract_syntax_tree;

use self::src_file::{DisplayPosition, SrcCode};
use crate::{error::MsgWithCauseUnderTextWithLocation, ERROR};
use core::fmt::{Debug, Display};
extern crate alloc;
use alloc::borrow::Cow;
use back_to_front::offset32;
use std::path::Path;

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub(crate) struct SliceIndexPtr<T>(pub(crate) offset32, core::marker::PhantomData<T>);

#[expect(clippy::missing_trait_methods)]
impl<T> Clone for SliceIndexPtr<T> {
    #[expect(clippy::non_canonical_clone_impl, reason = "false positive due to return")]
    fn clone(&self) -> Self {
        return *self;
    }
}

impl<T> Copy for SliceIndexPtr<T> {}

impl<T> SliceIndexPtr<T> {
    #[must_use]
    #[inline(always)]
    pub(crate) const fn new_offset32(index: offset32) -> Self {
        return Self(index, core::marker::PhantomData);
    }

    #[must_use]
    #[inline(always)]
    pub(crate) const fn new(index: usize) -> Self {
        #[expect(clippy::cast_possible_truncation)]
        return Self(index as offset32, core::marker::PhantomData);
    }

    #[track_caller]
    #[must_use]
    #[inline(always)]
    pub(crate) fn get(self, slice: &[T]) -> Option<&T> {
        return slice.get(self.0 as usize);
    }

    #[expect(dead_code)]
    #[track_caller]
    #[must_use]
    #[inline(always)]
    pub(crate) fn get_mut(self, slice: &mut [T]) -> Option<&mut T> {
        return slice.get_mut(self.0 as usize);
    }
}

impl<T> core::ops::Index<SliceIndexPtr<T>> for [T] {
    type Output = T;

    #[track_caller]
    #[must_use]
    #[inline(always)]
    fn index(&self, index: SliceIndexPtr<T>) -> &Self::Output {
        return &self[index.0 as usize];
    }
}

impl<T> core::ops::IndexMut<SliceIndexPtr<T>> for [T] {
    #[track_caller]
    #[must_use]
    #[inline(always)]
    fn index_mut(&mut self, index: SliceIndexPtr<T>) -> &mut Self::Output {
        return &mut self[index.0 as usize];
    }
}

impl<T> core::ops::Index<SliceIndexPtr<T>> for Vec<T> {
    type Output = T;

    #[track_caller]
    #[must_use]
    #[inline(always)]
    fn index(&self, index: SliceIndexPtr<T>) -> &Self::Output {
        return self.as_slice().index(index);
    }
}

impl<T> core::ops::IndexMut<SliceIndexPtr<T>> for Vec<T> {
    #[track_caller]
    #[must_use]
    #[inline(always)]
    fn index_mut(&mut self, index: SliceIndexPtr<T>) -> &mut Self::Output {
        return self.as_mut_slice().index_mut(index);
    }
}

pub trait IntoErrorInfo {
    fn info(&self) -> ErrorInfo;
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ErrorInfo {
    pub error_message: Cow<'static, str>,
    pub error_cause_message: Cow<'static, str>,
}

// IDEA(stefano): allow pointers to start past the end of the line
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Error<K: IntoErrorInfo> {
    pub kind: K,
    /// absolute source code byte position
    pub col: offset32,
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

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
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

#[expect(clippy::missing_trait_methods)]
impl core::error::Error for ErrorDisplay<'_, '_> {}
