pub mod ast;
pub mod src_file;
pub mod syntax_tree;
pub mod tokenizer;
pub mod typed_abstract_syntax_tree;
// pub mod ir;

use self::src_file::{DisplayPosition, SrcCode};
use crate::{error::MsgWithCauseUnderTextWithLocation, ERROR};
use core::fmt::{Debug, Display};
extern crate alloc;
use alloc::borrow::Cow;
use back_to_front::uoffset32;
use std::path::Path;

// IDEA(stefano): move to back-to-front
#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub(crate) struct Index32<T>(pub(crate) uoffset32, core::marker::PhantomData<T>);

#[expect(clippy::missing_trait_methods)]
impl<T> Clone for Index32<T> {
    #[expect(clippy::non_canonical_clone_impl, reason = "false positive due to return")]
    fn clone(&self) -> Self {
        return *self;
    }
}

impl<T> Copy for Index32<T> {}

impl<T> Index32<T> {
    #[must_use]
    #[inline(always)]
    pub(crate) const fn new_uoffset32(index: uoffset32) -> Self {
        return Self(index, core::marker::PhantomData);
    }

    #[must_use]
    #[inline(always)]
    pub(crate) const fn new(index: usize) -> Self {
        #[expect(clippy::cast_possible_truncation)]
        return Self(index as uoffset32, core::marker::PhantomData);
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

impl<T> core::ops::Index<Index32<T>> for [T] {
    type Output = T;

    #[track_caller]
    #[inline(always)]
    fn index(&self, index: Index32<T>) -> &Self::Output {
        return &self[index.0 as usize];
    }
}

impl<T> core::ops::IndexMut<Index32<T>> for [T] {
    #[track_caller]
    #[inline(always)]
    fn index_mut(&mut self, index: Index32<T>) -> &mut Self::Output {
        return &mut self[index.0 as usize];
    }
}

impl<T> core::ops::Index<Index32<T>> for Vec<T> {
    type Output = T;

    #[track_caller]
    #[inline(always)]
    fn index(&self, index: Index32<T>) -> &Self::Output {
        return self.as_slice().index(index);
    }
}

impl<T> core::ops::IndexMut<Index32<T>> for Vec<T> {
    #[track_caller]
    #[inline(always)]
    fn index_mut(&mut self, index: Index32<T>) -> &mut Self::Output {
        return self.as_mut_slice().index_mut(index);
    }
}

pub trait IntoMsgInfo {
    fn info(&self) -> MsgInfo;
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct MsgInfo {
    pub message: Cow<'static, str>,
    pub cause: Cow<'static, str>,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum MsgSeverity {
    Error,
    NonTerminalError,
}

// IDEA(stefano): allow pointers to start past the end of the line
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Msg<K: IntoMsgInfo> {
    pub severity: MsgSeverity,
    pub kind: K,
    /// absolute source code byte position
    pub col: uoffset32,
    pub pointers_count: uoffset32,
}

impl<K: IntoMsgInfo> Msg<K> {
    pub fn display<'code, 'path: 'code>(
        &self,
        src: &SrcCode<'code, 'path>,
    ) -> MsgDisplay<'code, 'path> {
        let DisplayPosition { line, column, display_column } = src.display_position(self.col);
        let line_span = src.lines[line as usize - 1];
        let line_text = &src.code()[line_span.start as usize..line_span.end as usize];

        let MsgInfo { message: error_message, cause: error_cause_message } = self.kind.info();
        return MsgDisplay {
            severity: self.severity,
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
pub struct MsgDisplay<'code, 'path: 'code> {
    pub severity: MsgSeverity,
    pub error_message: Cow<'static, str>,
    pub file: &'path Path,
    pub line: uoffset32,
    pub column: uoffset32,
    pub absolute_column: uoffset32,
    pub line_text: &'code str,
    pub pointers_count: uoffset32,
    pub pointers_offset: uoffset32,
    pub error_cause_message: Cow<'static, str>,
}

impl Display for MsgDisplay<'_, '_> {
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
impl core::error::Error for MsgDisplay<'_, '_> {}
