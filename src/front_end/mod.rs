pub mod ast;
pub mod src_file;
pub mod syntax_tree;
pub mod tokenizer;
pub mod typed_abstract_syntax_tree;
pub mod types;

use self::src_file::{Position, SrcCode};
use crate::{ERROR, error::MsgWithCauseUnderTextWithLocation, front_end::src_file::SrcFile};
extern crate alloc;
use alloc::borrow::Cow;
use back_to_front::{text, usize32};

// IDEA(stefano): move to back-to-front
#[derive(Hash, PartialEq, Eq, PartialOrd, Ord)]
#[must_use] #[repr(transparent)]
pub(crate) struct Index32<T>(pub(crate) usize32, core::marker::PhantomData<T>);

impl<T> core::fmt::Debug for Index32<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return write!(f, "Index32({})", self.0);
    }
}

#[expect(clippy::missing_trait_methods)]
impl<T> Clone for Index32<T> {
    #[inline(always)]
    fn clone(&self) -> Self {
        return *self;
    }
}

impl<T> Copy for Index32<T> {}

impl<T> Index32<T> {
    #[inline(always)]
    pub(crate) const fn new_usize32(index: usize32) -> Self {
        return Self(index, core::marker::PhantomData);
    }

    #[inline(always)]
    pub(crate) const fn new(index: usize) -> Self {
        return Self(index as usize32, core::marker::PhantomData);
    }

    #[must_use] #[inline(always)] #[track_caller]
    pub(crate) fn get(self, slice: &[T]) -> Option<&T> {
        return slice.get(self.0 as usize);
    }

    #[expect(dead_code)]
    #[must_use] #[inline(always)] #[track_caller]
    pub(crate) fn get_mut(self, slice: &mut [T]) -> Option<&mut T> {
        return slice.get_mut(self.0 as usize);
    }
}

impl<T> core::ops::Index<Index32<T>> for [T] {
    type Output = T;

    #[inline(always)] #[track_caller]
    fn index(&self, index: Index32<T>) -> &Self::Output {
        return &self[index.0 as usize];
    }
}

impl<T> core::ops::IndexMut<Index32<T>> for [T] {
    #[inline(always)] #[track_caller]
    fn index_mut(&mut self, index: Index32<T>) -> &mut Self::Output {
        return &mut self[index.0 as usize];
    }
}

impl<T> core::ops::Index<Index32<T>> for Vec<T> {
    type Output = T;

    #[inline(always)] #[track_caller]
    fn index(&self, index: Index32<T>) -> &Self::Output {
        return self.as_slice().index(index);
    }
}

impl<T> core::ops::IndexMut<Index32<T>> for Vec<T> {
    #[inline(always)] #[track_caller]
    fn index_mut(&mut self, index: Index32<T>) -> &mut Self::Output {
        return self.as_mut_slice().index_mut(index);
    }
}

pub trait IntoMsgInfo {
    fn info(
        &self,
        // HACK(stefano): this col is passes just because `ErrorKind`'s don't contain column information, but only `Msg`'s do
        col: usize32,
        src: &SrcFile<'_>,
    ) -> MsgInfo;
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub struct MsgInfo {
    pub message: Cow<'static, str>,
    pub cause: Cow<'static, str>,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub enum MsgSeverity {
    Error,
    NonTerminalError,
    Warning,
}

// IDEA(stefano): allow pointers to start past the end of the line
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub struct Msg<K: IntoMsgInfo> {
    // BUG(stefano): is ignored
    pub severity: MsgSeverity,
    pub kind: K,
    /// absolute source code byte position
    pub col: usize32,
    pub pointers_count: usize32,
}

impl<K: IntoMsgInfo> Msg<K> {
    pub fn display<'code, 'path>(&self, src: &SrcCode<'code, 'path>) -> MsgDisplay<'code, 'path> {
        let position = Position::new(src, self.col);
        let line_span = src.lines[position.line as usize - 1];
        let line_text = text::span(src.code(), line_span);

        let MsgInfo { message: error_message, cause: error_cause_message } = self.kind.info(self.col, src.src_file);
        return MsgDisplay {
            severity: self.severity,
            error_message,
            position,
            line_text,
            error_cause_message,
            pointers_count: self.pointers_count,
        };
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub struct MsgDisplay<'code, 'path> {
    pub severity: MsgSeverity,
    pub error_message: Cow<'static, str>,
    pub position: Position<'path>,
    pub line_text: &'code str,
    pub pointers_count: usize32,
    pub error_cause_message: Cow<'static, str>,
}

impl core::fmt::Display for MsgDisplay<'_, '_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let error = MsgWithCauseUnderTextWithLocation {
            severity: MsgSeverity::Error,
            kind: &ERROR,
            message: &self.error_message,
            cause: &self.error_cause_message,
            position: self.position.clone(),
            line_text: self.line_text,
            pointers_count: self.pointers_count,
        };
        return write!(f, "{error}");
    }
}

#[expect(clippy::missing_trait_methods)]
impl core::error::Error for MsgDisplay<'_, '_> {}
