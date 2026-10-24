// IDEA(stefano): add error for boolean conditions with `!=`, stating that this language uses `!==`
// IDEA(stefano): fuse tokenization and parsing, making the tokenizer a generator of tokens
// TODO(stefano): multidimensional arrays

use crate::{ERROR, error::MsgWithCauseUnderTextWithLocation, front_end::{
    Index32, MsgSeverity, src_file::{Position, SrcFile}, tokenizer::{self, Token, TokenKind}, types::BaseType,
}};
use back_to_front::{ascii, digit, usize32};

use super::{
    IntoMsgInfo, Msg, MsgInfo,
    syntax_tree::{self as st, SyntaxTree},
    tokenizer::Op,
};


#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub enum Type {
    Base(BaseType),
    Array {
        base_type: BaseType,
        // TODO(stefano): allow zero length arrays
        // TODO(stefano): allow nested arrays
        /// always greater than 0
        len: u64,
    },
}

impl Type {
    #[inline]
    pub const fn base_typ(&self) -> BaseType {
        return match self {
            Self::Base(typ) => *typ,
            Self::Array { base_type, .. } => *base_type,
        };
    }

    #[must_use] #[inline]
    pub const fn size_of(&self) -> usize {
        return match self {
            Self::Base(typ) => typ.size_of(),
            Self::Array { base_type, len } => base_type.size_of() * *len as usize,
        };
    }
}

impl core::fmt::Display for Type {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return match self {
            Self::Base(typ) => write!(f, "{typ}"),
            Self::Array { base_type, len } => write!(f, "{base_type}[{len}]"),
        };
    }
}

#[expect(dead_code)]
#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[must_use] #[repr(u8)]
pub(crate) enum PrefixOp {
    Len = st::PrefixOp::Len as u8,
    Not = st::PrefixOp::Not as u8,

    Plus           = st::PrefixOp::Plus as u8,
    WrappingPlus   = st::PrefixOp::WrappingPlus as u8,
    SaturatingPlus = st::PrefixOp::SaturatingPlus as u8,

    Minus           = st::PrefixOp::Minus as u8,
    WrappingMinus   = st::PrefixOp::WrappingMinus as u8,
    SaturatingMinus = st::PrefixOp::SaturatingMinus as u8,
}

impl PrefixOp {
    const BASE_TYPE: BaseType = BaseType::I64;
    const TYPE: Type = Type::Base(Self::BASE_TYPE);
}

impl Into<PrefixOp> for Op {
    #[inline(always)]
    fn into(self) -> PrefixOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Op> for PrefixOp {
    #[inline(always)]
    fn into(self) -> Op {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<st::PrefixOp> for PrefixOp {
    #[inline(always)]
    fn into(self) -> st::PrefixOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<PrefixOp> for st::PrefixOp {
    #[inline(always)]
    fn into(self) -> PrefixOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl core::fmt::Display for PrefixOp {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl PrefixOp {
    #[expect(dead_code)]
    #[inline(always)]
    pub(super) fn display_len(self) -> usize32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

#[expect(dead_code)]
#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[must_use] #[repr(u8)]
pub(crate) enum BooleanPrefixOp {
    Not = st::PrefixOp::Not as u8,
}

impl BooleanPrefixOp {
    const BASE_TYPE: BaseType = BaseType::Bool;
    const TYPE: Type = Type::Base(Self::BASE_TYPE);
}

impl Into<BooleanPrefixOp> for Op {
    #[inline(always)]
    fn into(self) -> BooleanPrefixOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Op> for BooleanPrefixOp {
    #[inline(always)]
    fn into(self) -> Op {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<st::PrefixOp> for BooleanPrefixOp {
    #[inline(always)]
    fn into(self) -> st::PrefixOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<BooleanPrefixOp> for st::PrefixOp {
    #[inline(always)]
    fn into(self) -> BooleanPrefixOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl core::fmt::Display for BooleanPrefixOp {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl BooleanPrefixOp {
    #[expect(dead_code)]
    #[inline(always)]
    pub(super) fn display_len(self) -> usize32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

#[expect(dead_code)]
#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[must_use] #[repr(u8)]
pub(crate) enum BinaryOp {
    Pow           = st::BinaryOp::Pow as u8,
    WrappingPow   = st::BinaryOp::WrappingPow as u8,
    SaturatingPow = st::BinaryOp::SaturatingPow as u8,

    Times           = st::BinaryOp::Times as u8,
    WrappingTimes   = st::BinaryOp::WrappingTimes as u8,
    SaturatingTimes = st::BinaryOp::SaturatingTimes as u8,

    Divide           = st::BinaryOp::Divide as u8,
    WrappingDivide   = st::BinaryOp::WrappingDivide as u8,
    SaturatingDivide = st::BinaryOp::SaturatingDivide as u8,

    Remainder = st::BinaryOp::Remainder as u8,

    Plus           = st::BinaryOp::Plus as u8,
    WrappingPlus   = st::BinaryOp::WrappingPlus as u8,
    SaturatingPlus = st::BinaryOp::SaturatingPlus as u8,

    Minus           = st::BinaryOp::Minus as u8,
    WrappingMinus   = st::BinaryOp::WrappingMinus as u8,
    SaturatingMinus = st::BinaryOp::SaturatingMinus as u8,

    LeftShift           = st::BinaryOp::LeftShift as u8,
    WrappingLeftShift   = st::BinaryOp::WrappingLeftShift as u8,
    SaturatingLeftShift = st::BinaryOp::SaturatingLeftShift as u8,

    RightShift = st::BinaryOp::RightShift as u8,

    LeftRotate  = st::BinaryOp::LeftRotate as u8,
    RightRotate = st::BinaryOp::RightRotate as u8,

    BitAnd = st::BinaryOp::BitAnd as u8,
    BitXor = st::BinaryOp::BitXor as u8,
    BitOr  = st::BinaryOp::BitOr as u8,
}

impl BinaryOp {
    const BASE_TYPE: BaseType = BaseType::I64;
    const TYPE: Type = Type::Base(Self::BASE_TYPE);
}

impl Into<BinaryOp> for Op {
    #[inline(always)]
    fn into(self) -> BinaryOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Op> for BinaryOp {
    #[inline(always)]
    fn into(self) -> Op {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<st::BinaryOp> for BinaryOp {
    #[inline(always)]
    fn into(self) -> st::BinaryOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<BinaryOp> for st::BinaryOp {
    #[inline(always)]
    fn into(self) -> BinaryOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl core::fmt::Display for BinaryOp {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl BinaryOp {
    #[expect(dead_code)]
    #[inline(always)]
    pub(super) fn display_len(self) -> usize32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

#[expect(dead_code)]
#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[must_use] #[repr(u8)]
pub(crate) enum BooleanBinaryOp {
    And = st::BinaryOp::And as u8,
    Or  = st::BinaryOp::Or as u8,
}

impl BooleanBinaryOp {
    const BASE_TYPE: BaseType = BaseType::Bool;
    const TYPE: Type = Type::Base(Self::BASE_TYPE);
}

impl Into<BooleanBinaryOp> for Op {
    #[inline(always)]
    fn into(self) -> BooleanBinaryOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Op> for BooleanBinaryOp {
    #[inline(always)]
    fn into(self) -> Op {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<st::BinaryOp> for BooleanBinaryOp {
    #[inline(always)]
    fn into(self) -> st::BinaryOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<BooleanBinaryOp> for st::BinaryOp {
    #[inline(always)]
    fn into(self) -> BooleanBinaryOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl core::fmt::Display for BooleanBinaryOp {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl BooleanBinaryOp {
    #[expect(dead_code)]
    #[inline(always)]
    pub(super) fn display_len(self) -> usize32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

#[expect(dead_code)]
#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[must_use] #[repr(u8)]
pub(crate) enum ComparisonOp {
    Compare = st::BinaryOp::Compare as u8,
}

impl ComparisonOp {
    const BASE_TYPE: BaseType = BaseType::I64;
    const TYPE: Type = Type::Base(Self::BASE_TYPE);
}

impl Into<ComparisonOp> for Op {
    #[inline(always)]
    fn into(self) -> ComparisonOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Op> for ComparisonOp {
    #[inline(always)]
    fn into(self) -> Op {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<st::BinaryOp> for ComparisonOp {
    #[inline(always)]
    fn into(self) -> st::BinaryOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<ComparisonOp> for st::BinaryOp {
    #[inline(always)]
    fn into(self) -> ComparisonOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl core::fmt::Display for ComparisonOp {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl ComparisonOp {
    #[expect(dead_code)]
    #[inline(always)]
    pub(super) fn display_len(self) -> usize32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

#[expect(dead_code)]
#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[must_use] #[repr(u8)]
pub(crate) enum BooleanComparisonOp {
    EqualsEquals    = st::BinaryOp::EqualsEquals as u8,
    NotEqualsEquals = st::BinaryOp::NotEqualsEquals as u8,

    Greater         = st::BinaryOp::Greater as u8,
    GreaterOrEquals = st::BinaryOp::GreaterOrEquals as u8,

    Less            = st::BinaryOp::Less as u8,
    LessOrEquals    = st::BinaryOp::LessOrEquals as u8,
}

impl BooleanComparisonOp {
    const BASE_TYPE: BaseType = BaseType::Bool;
    const TYPE: Type = Type::Base(Self::BASE_TYPE);
}

impl Into<BooleanComparisonOp> for Op {
    #[inline(always)]
    fn into(self) -> BooleanComparisonOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Op> for BooleanComparisonOp {
    #[inline(always)]
    fn into(self) -> Op {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<st::BinaryOp> for BooleanComparisonOp {
    #[inline(always)]
    fn into(self) -> st::BinaryOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<BooleanComparisonOp> for st::BinaryOp {
    #[inline(always)]
    fn into(self) -> BooleanComparisonOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl core::fmt::Display for BooleanComparisonOp {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl BooleanComparisonOp {
    #[expect(dead_code)]
    #[inline(always)]
    pub(super) fn display_len(self) -> usize32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[must_use] #[repr(u8)]
pub(crate) enum AssignmentOp {
    Equals        = st::BinaryAssignmentOp::Equals as u8,
}

impl Into<AssignmentOp> for Op {
    #[inline(always)]
    fn into(self) -> AssignmentOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Op> for AssignmentOp {
    #[inline(always)]
    fn into(self) -> Op {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<st::BinaryAssignmentOp> for AssignmentOp {
    #[inline(always)]
    fn into(self) -> st::BinaryAssignmentOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<AssignmentOp> for st::BinaryAssignmentOp {
    #[inline(always)]
    fn into(self) -> AssignmentOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl core::fmt::Display for AssignmentOp {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl AssignmentOp {
    #[expect(dead_code)]
    #[inline(always)]
    pub(super) fn display_len(self) -> usize32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

#[expect(dead_code)]
#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[must_use] #[repr(u8)]
pub(crate) enum PrefixAssignmentOp {
    NotEquals = st::PrefixAssignmentOp::NotEquals as u8,

    PlusEquals = st::PrefixAssignmentOp::PlusEquals as u8,
    WrappingPlusEquals = st::PrefixAssignmentOp::WrappingPlusEquals as u8,
    SaturatingPlusEquals = st::PrefixAssignmentOp::SaturatingPlusEquals as u8,

    MinusEquals = st::PrefixAssignmentOp::MinusEquals as u8,
    WrappingMinusEquals = st::PrefixAssignmentOp::WrappingMinusEquals as u8,
    SaturatingMinusEquals = st::PrefixAssignmentOp::SaturatingMinusEquals as u8,
}

impl Into<PrefixAssignmentOp> for Op {
    #[inline(always)]
    fn into(self) -> PrefixAssignmentOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Op> for PrefixAssignmentOp {
    #[inline(always)]
    fn into(self) -> Op {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<st::PrefixAssignmentOp> for PrefixAssignmentOp {
    #[inline(always)]
    fn into(self) -> st::PrefixAssignmentOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<PrefixAssignmentOp> for st::PrefixAssignmentOp {
    #[inline(always)]
    fn into(self) -> PrefixAssignmentOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl core::fmt::Display for PrefixAssignmentOp {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl PrefixAssignmentOp {
    #[expect(dead_code)]
    #[inline(always)]
    pub(super) fn display_len(self) -> usize32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

#[expect(dead_code)]
#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[must_use] #[repr(u8)]
pub(crate) enum BooleanPrefixAssignmentOp {
    NotEquals = st::PrefixAssignmentOp::NotEquals as u8,
}

impl Into<BooleanPrefixAssignmentOp> for Op {
    #[inline(always)]
    fn into(self) -> BooleanPrefixAssignmentOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Op> for BooleanPrefixAssignmentOp {
    #[inline(always)]
    fn into(self) -> Op {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<st::PrefixAssignmentOp> for BooleanPrefixAssignmentOp {
    #[inline(always)]
    fn into(self) -> st::PrefixAssignmentOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<BooleanPrefixAssignmentOp> for st::PrefixAssignmentOp {
    #[inline(always)]
    fn into(self) -> BooleanPrefixAssignmentOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl core::fmt::Display for BooleanPrefixAssignmentOp {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl BooleanPrefixAssignmentOp {
    #[expect(dead_code)]
    #[inline(always)]
    pub(super) fn display_len(self) -> usize32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

#[expect(dead_code)]
#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[must_use] #[repr(u8)]
pub(crate) enum BinaryAssignmentOp {
    Pow           = st::BinaryAssignmentOp::Pow as u8,
    WrappingPow   = st::BinaryAssignmentOp::WrappingPow as u8,
    SaturatingPow = st::BinaryAssignmentOp::SaturatingPow as u8,

    Times           = st::BinaryAssignmentOp::Times as u8,
    WrappingTimes   = st::BinaryAssignmentOp::WrappingTimes as u8,
    SaturatingTimes = st::BinaryAssignmentOp::SaturatingTimes as u8,

    Divide           = st::BinaryAssignmentOp::Divide as u8,
    WrappingDivide   = st::BinaryAssignmentOp::WrappingDivide as u8,
    SaturatingDivide = st::BinaryAssignmentOp::SaturatingDivide as u8,

    Remainder = st::BinaryAssignmentOp::Remainder as u8,

    Plus           = st::BinaryAssignmentOp::Plus as u8,
    WrappingPlus   = st::BinaryAssignmentOp::WrappingPlus as u8,
    SaturatingPlus = st::BinaryAssignmentOp::SaturatingPlus as u8,

    Minus           = st::BinaryAssignmentOp::Minus as u8,
    WrappingMinus   = st::BinaryAssignmentOp::WrappingMinus as u8,
    SaturatingMinus = st::BinaryAssignmentOp::SaturatingMinus as u8,

    LeftShift           = st::BinaryAssignmentOp::LeftShift as u8,
    WrappingLeftShift   = st::BinaryAssignmentOp::WrappingLeftShift as u8,
    SaturatingLeftShift = st::BinaryAssignmentOp::SaturatingLeftShift as u8,

    RightShift = st::BinaryAssignmentOp::RightShift as u8,

    LeftRotate  = st::BinaryAssignmentOp::LeftRotate as u8,
    RightRotate = st::BinaryAssignmentOp::RightRotate as u8,

    BitAnd = st::BinaryAssignmentOp::BitAnd as u8,
    BitXor = st::BinaryAssignmentOp::BitXor as u8,
    BitOr  = st::BinaryAssignmentOp::BitOr as u8,
}

impl Into<BinaryAssignmentOp> for Op {
    #[inline(always)]
    fn into(self) -> BinaryAssignmentOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Op> for BinaryAssignmentOp {
    #[inline(always)]
    fn into(self) -> Op {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<st::BinaryAssignmentOp> for BinaryAssignmentOp {
    #[inline(always)]
    fn into(self) -> st::BinaryAssignmentOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<BinaryAssignmentOp> for st::BinaryAssignmentOp {
    #[inline(always)]
    fn into(self) -> BinaryAssignmentOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl core::fmt::Display for BinaryAssignmentOp {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl BinaryAssignmentOp {
    #[expect(dead_code)]
    #[inline(always)]
    pub(super) fn display_len(self) -> usize32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

#[expect(dead_code)]
#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[must_use] #[repr(u8)]
pub(crate) enum BooleanBinaryAssignmentOp {
    And    = st::BinaryAssignmentOp::And as u8,
    Or     = st::BinaryAssignmentOp::Or as u8,
}

impl Into<BooleanBinaryAssignmentOp> for Op {
    #[inline(always)]
    fn into(self) -> BooleanBinaryAssignmentOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Op> for BooleanBinaryAssignmentOp {
    #[inline(always)]
    fn into(self) -> Op {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<st::BinaryAssignmentOp> for BooleanBinaryAssignmentOp {
    #[inline(always)]
    fn into(self) -> st::BinaryAssignmentOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<BooleanBinaryAssignmentOp> for st::BinaryAssignmentOp {
    #[inline(always)]
    fn into(self) -> BooleanBinaryAssignmentOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl core::fmt::Display for BooleanBinaryAssignmentOp {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl BooleanBinaryAssignmentOp {
    #[expect(dead_code)]
    #[inline(always)]
    pub(super) fn display_len(self) -> usize32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub(crate) enum Expression {
    False {
        column: usize32,
    },
    True {
        column: usize32,
    },
    I64 {
        value: i64,
        column: usize32,
    },
    Ascii {
        character: ascii,
        column: usize32,
    },
    Str {
        text_end: usize32,
        column: usize32,
    },
    Variable {
        variable: Index32<VariableDefinition>,
        column: usize32,
    },
    Array {
        base_type: BaseType,
        items_start: Index32<Self>,
        /// always greater than 0
        items_len: u64,
    },

    Prefix {
        operator: PrefixOp,
        operator_column: usize32,
        right_operand: Index32<Self>,
    },
    BooleanPrefix {
        operator: BooleanPrefixOp,
        operator_column: usize32,
        right_operand: Index32<Self>,
    },
    Binary {
        left_operand: Index32<Self>,
        operator: BinaryOp,
        operator_column: usize32,
        right_operand: Index32<Self>,
    },
    BooleanBinary {
        left_operand: Index32<Self>,
        operator: BooleanBinaryOp,
        operator_column: usize32,
        right_operand: Index32<Self>,
    },
    Comparison {
        left_operand: Index32<Self>,
        operator: ComparisonOp,
        operator_column: usize32,
        right_operand: Index32<Self>,
    },
    BooleanComparison {
        left_operand: Index32<Self>,
        operator: BooleanComparisonOp,
        operator_column: usize32,
        right_operand: Index32<Self>,
    },

    Index {
        indexed_expression: Index32<Self>,
        open_square_bracket_column: usize32,
        index_expression: Index32<Self>,
    },
}

impl Expression {
    pub(crate) fn typ(&self, ast: &TypedSyntaxTree<'_, '_, '_, '_>) -> Type {
        return match self {
            Self::False { .. } | Self::True { .. } => Type::Base(BaseType::Bool),
            Self::I64 { .. } => Type::Base(BaseType::I64),
            Self::Ascii { .. } => Type::Base(BaseType::Ascii),
            Self::Str { .. } => Type::Base(BaseType::Str),
            Self::Variable { variable, .. } => {
                let variable_definition = &ast.variables[*variable];
                variable_definition.typ.clone()
            },
            Self::Array { base_type, items_len, .. } => {
                Type::Array { base_type: *base_type, len: *items_len }
            },
            Self::Prefix { .. } => PrefixOp::TYPE,
            Self::BooleanPrefix { .. } => BooleanPrefixOp::TYPE,
            Self::Binary { .. } => BinaryOp::TYPE,
            Self::BooleanBinary { .. } => BooleanBinaryOp::TYPE,
            Self::Comparison { .. } => ComparisonOp::TYPE,
            Self::BooleanComparison { .. } => BooleanComparisonOp::TYPE,
            Self::Index { indexed_expression, .. } => {
                let expression = &ast.expressions[*indexed_expression];
                match expression.typ(ast) {
                    Type::Base(BaseType::Str) => Type::Base(BaseType::Ascii),
                    base_type @ Type::Base(BaseType::I64 | BaseType::Bool | BaseType::Ascii) => base_type,
                    Type::Array { base_type, .. } => Type::Base(base_type),
                }
            },
        };
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub(crate) struct Scope {
    pub(crate) parent: Index32<Self>,
    // pub(crate) types: Vec<BaseType>,
    pub(crate) let_variables: Vec<Index32<VariableDefinition>>,
    pub(crate) var_variables: Vec<Index32<VariableDefinition>>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub(crate) struct VariableDefinition {
    pub(crate) name_text_end: usize32,
    pub(crate) name_column: usize32,
    // NOTE(stefano): might remove and recalculate it each time
    pub(crate) typ: Type,
    pub(crate) value: Index32<Expression>,
}


#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub(crate) enum Node {
    Expression(Index32<Expression>),

    Print {
        print_column: usize32,
        argument: Index32<Expression>,
    },
    Println {
        println_column: usize32,
        argument: Index32<Expression>,
    },
    PrintlnNoArg {
        println_column: usize32,
    },
    Eprint {
        eprint_column: usize32,
        argument: Index32<Expression>,
    },
    Eprintln {
        eprintln_column: usize32,
        argument: Index32<Expression>,
    },
    EprintlnNoArg {
        eprintln_column: usize32,
    },

    LetVariableDefinition {
        variable: Index32<VariableDefinition>,
    },
    VarVariableDefinition {
        variable: Index32<VariableDefinition>,
    },
    Assignment {
        target: Index32<Expression>,
        operator: AssignmentOp,
        new_value: Index32<Expression>,
    },
    BinaryAssignment {
        target: Index32<Expression>,
        operator: BinaryAssignmentOp,
        operator_column: usize32,
        new_value: Index32<Expression>,
    },
    BooleanAssignmentExpression {
        target: Index32<Expression>,
        operator: BooleanBinaryAssignmentOp,
        operator_column: usize32,
        new_value: Index32<Expression>,
    },
    PrefixAssignmentExpression {
        target: Index32<Expression>,
        operator: PrefixAssignmentOp,
        operator_column: usize32,
    },
    BooleanPrefixAssignment {
        target: Index32<Expression>,
        operator: BooleanPrefixAssignmentOp,
        operator_column: usize32,
    },

    Scope {
        raw_nodes_in_scope_count: u32,
    },

    If {
        if_column: usize32,
        condition: Index32<Expression>,
    },
    ElseIf {
        else_column: usize32,
        if_column: usize32,
        condition: Index32<Expression>,
    },
    Else {
        else_column: usize32,
    },

    Loop {
        loop_column: usize32,
        condition: Index32<Expression>,
    },
    DoLoop {
        do_column: usize32,
        loop_column: usize32,
        condition: Index32<Expression>,
    },
    Break {
        break_column: usize32,
    },
    Continue {
        continue_column: usize32,
    },
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
enum ParsedNode {
    Node(Node),
    ScopeEnd,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub struct TypedSyntaxTree<'st, 'tokens: 'st, 'code: 'tokens, 'path> {
    pub(crate) syntax_tree: &'st SyntaxTree<'tokens, 'code, 'path>,

    pub(crate) nodes: Vec<Node>,

    pub(crate) expressions: Vec<Expression>,
    pub(crate) array_items: Vec<Index32<Expression>>,
    pub(crate) variables: Vec<VariableDefinition>,
}

impl TypedSyntaxTree<'_, '_, '_, '_> {
    #[inline]
    fn new_expression(&mut self, expression: Expression) -> Index32<Expression> {
        let index = Index32::new(self.expressions.len());
        self.expressions.push(expression);
        return index;
    }

    #[inline]
    fn new_variable(&mut self, variable: VariableDefinition) -> Index32<VariableDefinition> {
        let index = Index32::new(self.variables.len());
        self.variables.push(variable);
        return index;
    }
}

impl<'tokens> TypedSyntaxTree<'_, 'tokens, '_, '_> {
    #[inline(always)]
    pub const fn display(&self) -> TypedSyntaxTreeDisplay<'_, '_, 'tokens, '_, '_> {
        return TypedSyntaxTreeDisplay { tast: self };
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub struct Parser<'st, 'tokens: 'st, 'code: 'tokens, 'path> {
    errors: Vec<Msg<ErrorKind>>,
    tast: TypedSyntaxTree<'st, 'tokens, 'code, 'path>,

    node_index: Index32<st::Node>,

    temp_array_items: Vec<Index32<Expression>>,
    scope: Index32<Scope>,
    scopes: Vec<Scope>,
}

impl<'st, 'tokens: 'st, 'code: 'tokens, 'path> Parser<'st, 'tokens, 'code, 'path> {
    #[expect(clippy::missing_errors_doc)]
    pub fn parse(
        syntax_tree: &'st SyntaxTree<'tokens, 'code, 'path>,
    ) -> Result<TypedSyntaxTree<'st, 'tokens, 'code, 'path>, Vec<Msg<ErrorKind>>> {
        let mut parser = Self {
            errors: Vec::new(),
            tast: TypedSyntaxTree {
                syntax_tree,
                nodes: Vec::new(),
                expressions: Vec::new(),
                array_items: Vec::new(),
                variables: Vec::new(),
            },

            node_index: Index32::new(0),

            temp_array_items: Vec::new(),
            scope: Index32::new(0),
            scopes: vec![Scope {
                parent: Index32::new(0),
                // types: vec![BaseType::I64, BaseType::Ascii, BaseType::Bool, BaseType::Str],
                let_variables: Vec::new(),
                var_variables: Vec::new(),
            }],
        };

        while let Some(peeked) = parser.peek_next_node() {
            parser.node_index = peeked.index;
            match parser.any(peeked.node) {
                Ok(ParsedNode::Node(node)) => parser.tast.nodes.push(node),
                Ok(ParsedNode::ScopeEnd) => {},
                Err(err) => {
                    parser.errors.push(err);

                    // consuming all remaining nodes until the end of the file
                    parser.node_index = Index32::new(parser.tast.syntax_tree.nodes.len());
                    break;
                },
            }
        }

        return if parser.errors.len() == 0 {
            Ok(parser.tast)
        } else {
            Err(parser.errors)
        };
    }

    fn any(&mut self, node: &st::Node) -> Result<ParsedNode, Msg<ErrorKind>> {
        return match node {
            st::Node::Expression { expression, .. } => {
                let parsed_expression_index = self.parse_expression(*expression, None)?;
                Ok(ParsedNode::Node(Node::Expression(parsed_expression_index)))
            },
            st::Node::BinaryAssignment { target, operator, operator_column, new_value, .. } => {
                let parsed_assignment =
                    self.binary_assignment(*target, *operator, *operator_column, *new_value)?;
                Ok(ParsedNode::Node(parsed_assignment))
            },
            st::Node::PrefixAssignment { target, operator, operator_column, .. } => {
                let parsed_assignment = self.prefix_assignment(*target, *operator, *operator_column)?;
                Ok(ParsedNode::Node(parsed_assignment))
            },

            st::Node::Print { argument, print_column, .. } => {
                let parsed_argument_index = self.parse_expression(*argument, None)?;
                Ok(ParsedNode::Node(Node::Print { argument: parsed_argument_index, print_column: *print_column }))
            },
            st::Node::Println { argument, println_column, .. } => {
                let parsed_argument_index = self.parse_expression(*argument, None)?;
                Ok(ParsedNode::Node(Node::Println { argument: parsed_argument_index, println_column: *println_column }))
            },
            st::Node::PrintlnNoArg { println_column, .. } => {
                Ok(ParsedNode::Node(Node::PrintlnNoArg { println_column: *println_column }))
            },
            st::Node::Eprint { argument, eprint_column, .. } => {
                let parsed_argument_index = self.parse_expression(*argument, None)?;
                Ok(ParsedNode::Node(Node::Eprint { argument: parsed_argument_index, eprint_column: *eprint_column }))
            },
            st::Node::Eprintln { argument, eprintln_column, .. } => {
                let parsed_argument_index = self.parse_expression(*argument, None)?;
                Ok(ParsedNode::Node(Node::Eprintln { argument: parsed_argument_index, eprintln_column: *eprintln_column }))
            },
            st::Node::EprintlnNoArg { eprintln_column, .. } => {
                Ok(ParsedNode::Node(Node::EprintlnNoArg { eprintln_column: *eprintln_column }))
            },

            st::Node::LetVariableDefinition { variable_definition, .. } => {
                let variable = self.parse_variable(*variable_definition)?;
                self.scopes[self.scope].let_variables.push(variable);
                Ok(ParsedNode::Node(Node::LetVariableDefinition { variable }))
            },
            st::Node::VarVariableDefinition { variable_definition, .. } => {
                let variable = self.parse_variable(*variable_definition)?;
                self.scopes[self.scope].var_variables.push(variable);
                Ok(ParsedNode::Node(Node::VarVariableDefinition { variable }))
            },

            st::Node::Scope { open_curly_bracket_column, raw_nodes_in_scope_count, .. } => {
                let current_scope_index = self.scope;
                self.scope = Index32::new(self.scopes.len());
                self.scopes.push(Scope {
                    parent: current_scope_index,
                    // types: Vec::new(),
                    let_variables: Vec::new(),
                    var_variables: Vec::new(),
                });

                let placeholder_scope = Node::Scope { raw_nodes_in_scope_count: 0 };
                let placeholder_scope_node_index = Index32::new(self.tast.nodes.len());
                self.tast.nodes.push(placeholder_scope);

                let raw_nodes_in_scope_end = self.node_index.0 + *raw_nodes_in_scope_count;
                while self.node_index.0 < raw_nodes_in_scope_end {
                    let Some(peeked) = self.peek_next_node() else {
                        unreachable!();
                    };
                    self.node_index = peeked.index;
                    match self.any(peeked.node)? {
                        ParsedNode::Node(inner_node) => self.tast.nodes.push(inner_node),
                        ParsedNode::ScopeEnd => {},
                    }
                }

                let last_scope_node_index = Index32::<Node>::new(self.tast.nodes.len() - 1);
                let Node::Scope { raw_nodes_in_scope_count: placeholder_raw_nodes_in_scope_count } =
                    &mut self.tast.nodes[placeholder_scope_node_index]
                else {
                    self.invalid_scope_index(*open_curly_bracket_column);
                };

                *placeholder_raw_nodes_in_scope_count =
                    last_scope_node_index.0 - placeholder_scope_node_index.0;

                self.scope = current_scope_index;
                Ok(ParsedNode::ScopeEnd)
            },

            st::Node::If { if_column, condition } => {
                let parsed_condition = self.if_condition(*if_column, *condition)?;
                let parsed_condition_index = self.tast.new_expression(parsed_condition);
                self.tast.nodes.push(Node::If { condition: parsed_condition_index, if_column: *if_column });
                self.scope()
            },
            st::Node::ElseIf { if_column, else_column, condition, .. } => {
                let parsed_condition = self.if_condition(*if_column, *condition)?;
                let parsed_condition_index = self.tast.new_expression(parsed_condition);
                self.tast.nodes.push(Node::ElseIf { condition: parsed_condition_index, if_column: *if_column, else_column: *else_column });
                self.scope()
            },
            st::Node::Else { else_column, .. } => {
                self.tast.nodes.push(Node::Else { else_column: *else_column });
                self.scope()
            },
            st::Node::Loop { loop_column, condition } => {
                let parsed_condition = self.loop_condition(*loop_column, *condition)?;
                let parsed_condition_index = self.tast.new_expression(parsed_condition);
                self.tast.nodes.push(Node::Loop { condition: parsed_condition_index, loop_column: *loop_column });
                self.scope()
            },
            st::Node::DoLoop { do_column, loop_column, condition, .. } => {
                let parsed_condition = self.loop_condition(*loop_column, *condition)?;
                let parsed_condition_index = self.tast.new_expression(parsed_condition);
                self.tast.nodes.push(Node::DoLoop { condition: parsed_condition_index, do_column: *do_column, loop_column: *loop_column });
                self.scope()
            },
            st::Node::Break { break_column, .. } => Ok(ParsedNode::Node(Node::Break { break_column: *break_column })),
            st::Node::Continue { continue_column, .. } => Ok(ParsedNode::Node(Node::Continue { continue_column: *continue_column })),

            st::Node::Semicolon { column } => self.stray_semicolon(*column),
        };
    }

    fn scope(&mut self) -> Result<ParsedNode, Msg<ErrorKind>> {
        let Some(peeked) = self.peek_next_node() else {
            unreachable!();
        };
        self.node_index = peeked.index;
        let ParsedNode::ScopeEnd = self.any(peeked.node)? else {
            unreachable!();
        };
        return Ok(ParsedNode::ScopeEnd);
    }

    #[expect(clippy::panic)]
    #[track_caller]
    fn invalid_node(
        &self,
        absolute_column: usize32,
        pointers_count: usize32,
        error_message: &str,
        error_cause_message: &str,
    ) -> ! {
        let position = Position::new(&self.tast.syntax_tree.tokens.src, absolute_column);
        let line_span = self.tast.syntax_tree.tokens.src.lines[position.line as usize - 1];
        let line_text = &self.tast.syntax_tree.code()[line_span.start() as usize..line_span.end() as usize];

        let error = MsgWithCauseUnderTextWithLocation {
            severity: MsgSeverity::Error,
            kind: &ERROR,
            message: &error_message,
            cause: &error_cause_message,
            position,
            line_text,
            pointers_count,
        };
        panic!("{error}\n");
    }

    // REMOVE(stefano): could just ignore stray semicolons
    #[track_caller]
    fn stray_semicolon(&self, semicolon_colon: usize32) -> ! {
        self.invalid_node(
            semicolon_colon,
            1,
            "unexpected",
            "should have been skipped in the iteration of nodes",
        );
    }

    #[track_caller]
    fn invalid_scope_index(&self, open_curly_bracket_column: usize32) -> ! {
        self.invalid_node(
            open_curly_bracket_column,
            1,
            "invalid scope index",
            "should have been caught during syntax tree parsing",
        );
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
struct Peeked<'st> {
    node: &'st st::Node,
    index: Index32<st::Node>,
}

impl<'st, 'code: 'st> Parser<'st, '_, 'code, '_> {
    #[must_use]
    fn peek_next_node(&self) -> Option<Peeked<'st>> {
        let node_index_end = Index32::<Node>::new(self.tast.syntax_tree.nodes.len());
        for next_node_index in self.node_index.0..node_index_end.0 {
            let next_node_index_index = Index32::new_usize32(next_node_index);
            let next_node = &self.tast.syntax_tree.nodes[next_node_index_index];
            let st::Node::Semicolon { .. } = next_node else {
                let peeked_node_index_index = Index32::new_usize32(next_node_index_index.0 + 1);
                return Some(Peeked { node: next_node, index: peeked_node_index_index });
            };
        }

        return None;
    }
}

impl<'code> Parser<'_, '_, 'code, '_> {
    fn parse_positive_i64(
        literal_str: &'code str,
        base: digit::Base,
        extended_prefix: bool,
    ) -> Result<i64, ()> {
        let mut integer: i64 = 0;
        let mut digit_index = if extended_prefix {
            tokenizer::Base(base).prefix_extended().len()
        } else {
            tokenizer::Base(base).prefix().len()
        };

        let parse_fn = match base {
            digit::Base::Binary => digit::binary::parse,
            digit::Base::Octal => digit::octal::parse,
            digit::Base::Decimal => digit::decimal::parse,
            digit::Base::Hexadecimal => digit::hexadecimal::parse,
        };

        let literal = literal_str.as_bytes();
        while digit_index < literal.len() {
            let ascii_digit = literal[digit_index];
            digit_index += 1;
            if ascii_digit == b'_' {
                continue;
            }
            let digit = parse_fn(ascii_digit);
            debug_assert!(digit < base as u8, "invalid integer digit");

            integer = match integer.checked_mul(base as i64) {
                Some(integer_) => integer_,
                None => return Err(()),
            };
            integer = match integer.checked_add(digit as i64) {
                Some(integer_) => integer_,
                None => return Err(()),
            };
        }
        return Ok(integer);
    }

    fn parse_negative_i64(
        literal_str: &'code str,
        base: digit::Base,
        extended_prefix: bool,
    ) -> Result<i64, ()> {
        let mut integer: i64 = 0;
        let mut digit_index = if extended_prefix {
            tokenizer::Base(base).prefix_extended().len()
        } else {
            tokenizer::Base(base).prefix().len()
        };

        let parse_fn = match base {
            digit::Base::Binary => digit::binary::parse,
            digit::Base::Octal => digit::octal::parse,
            digit::Base::Decimal => digit::decimal::parse,
            digit::Base::Hexadecimal => digit::hexadecimal::parse,
        };

        let literal = literal_str.as_bytes();
        while digit_index < literal.len() {
            let ascii_digit = literal[digit_index];
            digit_index += 1;
            if ascii_digit == b'_' {
                continue;
            }
            let digit = parse_fn(ascii_digit);
            debug_assert!(digit < base as u8, "invalid integer digit");

            integer = match integer.checked_mul(base as i64) {
                Some(integer_) => integer_,
                None => return Err(()),
            };
            integer = match integer.checked_sub(digit as i64) {
                Some(integer_) => integer_,
                None => return Err(()),
            };
        }
        return Ok(integer);
    }

    // NOTE(stefano): only considering the first token in the expression until proper
    // multiline error messages are developed
    #[must_use]
    fn first_token_display_len(&self, expression: Index32<st::Expression>) -> usize32 {
        let st_expression = &self.tast.syntax_tree.expressions[expression];
        let token = match st_expression {
            st::Expression::False { column }                          => Token { kind: TokenKind::False,                                                       col: *column },
            st::Expression::True { column }                           => Token { kind: TokenKind::True,                                                        col: *column },
            st::Expression::DecimalInteger { text_end, column }       => Token { kind: TokenKind::DecimalInteger {       text_end: *text_end },                col: *column },
            st::Expression::DecimalIntegerPrefix { text_end, column } => Token { kind: TokenKind::DecimalIntegerPrefix { text_end: *text_end },                col: *column },
            st::Expression::BinaryInteger { text_end, column }        => Token { kind: TokenKind::BinaryInteger {        text_end: *text_end },                col: *column },
            st::Expression::OctalInteger { text_end, column }         => Token { kind: TokenKind::OctalInteger {         text_end: *text_end },                col: *column },
            st::Expression::HexadecimalInteger { text_end, column }   => Token { kind: TokenKind::HexadecimalInteger {   text_end: *text_end },                col: *column },
            st::Expression::Ascii { text_end, value, column }         => Token { kind: TokenKind::Ascii {                text_end: *text_end, value: *value }, col: *column },
            st::Expression::Str { text_end, column }                  => Token { kind: TokenKind::Str {                  text_end: *text_end },                col: *column },
            st::Expression::RawStr { text_end, column }               => Token { kind: TokenKind::RawStr {               text_end: *text_end },                col: *column },
            st::Expression::Identifier { text_end, column }           => Token { kind: TokenKind::Identifier {           text_end: *text_end },                col: *column },
            st::Expression::IdentifierStr { text_end, column }        => Token { kind: TokenKind::IdentifierStr {        text_end: *text_end },                col: *column },
            st::Expression::Array { open_square_bracket_column, .. }
            | st::Expression::ArrayTrailingItem { open_square_bracket_column, .. } => {
                Token { kind: TokenKind::OpenSquareBracket, col: *open_square_bracket_column }
            },
            st::Expression::Prefix { operator, operator_column, .. } => {
                let op: Op = (*operator).into();
                Token { kind: TokenKind::Op(op), col: *operator_column }
            },
            st::Expression::Binary { left_operand, .. } => {
                return self.first_token_display_len(*left_operand);
            },
            st::Expression::Parenthesis { open_round_bracket_column, .. } => Token { kind: TokenKind::OpenRoundBracket, col: *open_round_bracket_column },
            st::Expression::Index { indexed_expression, .. } => {
                return self.first_token_display_len(*indexed_expression);
            },
        };

        return token.display_len(self.tast.syntax_tree.tokens.src.src_file);
    }

    // NOTE(stefano): only considering the first token in the expression until proper
    // multiline error messages are developed
    #[must_use]
    fn first_token_column(&self, expression: Index32<st::Expression>) -> usize32 {
        let st_expression = &self.tast.syntax_tree.expressions[expression];
        let column = match st_expression {
            st::Expression::False { column }
            | st::Expression::True { column }
            | st::Expression::DecimalInteger { column, .. }
            | st::Expression::DecimalIntegerPrefix { column, .. }
            | st::Expression::BinaryInteger { column, .. }
            | st::Expression::OctalInteger { column, .. }
            | st::Expression::HexadecimalInteger { column, .. }
            | st::Expression::Ascii { column, .. }
            | st::Expression::Str { column, .. }
            | st::Expression::RawStr { column, .. }
            | st::Expression::Identifier { column, .. }
            | st::Expression::IdentifierStr { column, .. } => *column,
            st::Expression::Array { open_square_bracket_column, .. }
            | st::Expression::ArrayTrailingItem { open_square_bracket_column, .. } => {
                *open_square_bracket_column
            },
            st::Expression::Prefix { operator_column, .. } => *operator_column,
            st::Expression::Binary { left_operand, .. } => {
                return self.first_token_column(*left_operand);
            },
            st::Expression::Parenthesis { open_round_bracket_column, .. } => *open_round_bracket_column,
            st::Expression::Index { indexed_expression, .. } => {
                return self.first_token_column(*indexed_expression);
            },
        };

        return column;
    }

    #[must_use]
    fn resolve_variable(&self, name: &'code str) -> Option<Index32<VariableDefinition>> {
        if let Some(variable) = self.resolve_let_variable(name) {
            return Some(variable);
        }

        return self.resolve_var_variable(name);
    }

    #[must_use]
    fn resolve_let_variable(&self, name: &'code str) -> Option<Index32<VariableDefinition>> {
        let mut scope_index = self.scope;
        loop {
            let scope = &self.scopes[scope_index];
            for var_index in &scope.let_variables {
                let var = &self.tast.variables[*var_index];
                let var_name_text = self.tast.syntax_tree.tokens.src.src_file.text(var.name_column, var.name_text_end);
                if var_name_text == name {
                    return Some(*var_index);
                }
            }

            scope_index = match scope_index.0 {
                0 => return None,
                _ => scope.parent,
            };
        }
    }

    #[must_use]
    fn resolve_var_variable(&self, name: &'code str) -> Option<Index32<VariableDefinition>> {
        let mut scope_index = self.scope;
        loop {
            let scope = &self.scopes[scope_index];
            for var_index in &scope.var_variables {
                let var = &self.tast.variables[*var_index];
                let var_name_text = self.tast.syntax_tree.tokens.src.src_file.text(var.name_column, var.name_text_end);
                if var_name_text == name {
                    return Some(*var_index);
                }
            }

            scope_index = match scope_index.0 {
                0 => return None,
                _ => scope.parent,
            };
        }
    }

    #[expect(clippy::unused_self)]
    #[must_use]
    fn resolve_type(&self, name: &'code str) -> Option<BaseType> {
        return BaseType::from_str(name);

        // let mut scope_index = self.scope;
        // loop {
        //     let scope = &self.scopes[scope_index];
        //     for typ in &scope.types {
        //         if typ.matches(name) {
        //             return Some(*typ);
        //         }
        //     }

        //     scope_index = match scope_index.0 {
        //         0 => return None,
        //         _ => scope.parent,
        //     };
        // }
    }

    fn expression(
        &mut self,
        st_expression_index: Index32<st::Expression>,
        expected_type: Option<&Type>,
    ) -> Result<Expression, Msg<ErrorKind>> {
        let st_expression = &self.tast.syntax_tree.expressions[st_expression_index];
        let expression = match st_expression {
            st::Expression::False { column } => Expression::False { column: *column },
            st::Expression::True { column } => Expression::True { column: *column },
            st::Expression::DecimalInteger { text_end, column } => {
                let literal_text = self.tast.syntax_tree.tokens.src.src_file.text(*column, *text_end);
                let Ok(value) = Self::parse_positive_i64(literal_text, digit::Base::Decimal, false) else {
                    return Err(Msg {
                        severity: MsgSeverity::Error,
                        kind: ErrorKind::DecimalIntegerOverflow,
                        col: *column,
                        pointers_count: literal_text.len() as usize32,
                    });
                };
                Expression::I64 { value, column: *column }
            },
            st::Expression::DecimalIntegerPrefix { text_end, column } => {
                let literal_text = self.tast.syntax_tree.tokens.src.src_file.text(*column, *text_end);
                let Ok(value) = Self::parse_positive_i64(literal_text, digit::Base::Decimal, true) else {
                    return Err(Msg {
                        severity: MsgSeverity::Error,
                        kind: ErrorKind::DecimalIntegerOverflow,
                        col: *column,
                        pointers_count: literal_text.len() as usize32,
                    });
                };
                Expression::I64 { value, column: *column }
            },
            st::Expression::BinaryInteger { text_end, column } => {
                let literal_text = self.tast.syntax_tree.tokens.src.src_file.text(*column, *text_end);
                let Ok(value) = Self::parse_positive_i64(literal_text, digit::Base::Binary, false) else {
                    return Err(Msg {
                        severity: MsgSeverity::Error,
                        kind: ErrorKind::BinaryIntegerOverflow,
                        col: *column,
                        pointers_count: literal_text.len() as usize32,
                    });
                };
                Expression::I64 { value, column: *column }
            },
            st::Expression::OctalInteger { text_end, column } => {
                let literal_text = self.tast.syntax_tree.tokens.src.src_file.text(*column, *text_end);
                let Ok(value) = Self::parse_positive_i64(literal_text, digit::Base::Octal, false) else {
                    return Err(Msg {
                        severity: MsgSeverity::Error,
                        kind: ErrorKind::OctalIntegerOverflow,
                        col: *column,
                        pointers_count: literal_text.len() as usize32,
                    });
                };
                Expression::I64 { value, column: *column }
            },
            st::Expression::HexadecimalInteger { text_end, column } => {
                let literal_text = self.tast.syntax_tree.tokens.src.src_file.text(*column, *text_end);
                let Ok(value) = Self::parse_positive_i64(literal_text, digit::Base::Hexadecimal, false)
                else {
                    return Err(Msg {
                        severity: MsgSeverity::Error,
                        kind: ErrorKind::HexadecimalIntegerOverflow,
                        col: *column,
                        pointers_count: literal_text.len() as usize32,
                    });
                };
                Expression::I64 { value, column: *column }
            },
            st::Expression::Ascii { value, column, .. } => {
                Expression::Ascii { character: *value, column: *column }
            },
            st::Expression::Str { text_end, column } | st::Expression::RawStr { text_end, column } => {
                Expression::Str { text_end: *text_end, column: *column }
            },
            st::Expression::Identifier { text_end, column }
            | st::Expression::IdentifierStr { text_end, column } => {
                let identifier_text = self.tast.syntax_tree.tokens.src.src_file.text(*column, *text_end);
                if let Some(_) = self.resolve_type(identifier_text) {
                    return Err(Msg {
                        severity: MsgSeverity::Error,
                        kind: ErrorKind::TypeInExpression,
                        col: *column,
                        pointers_count: identifier_text.len() as usize32,
                    });
                }
                let Some(variable) = self.resolve_variable(identifier_text) else {
                    return Err(Msg {
                        severity: MsgSeverity::Error,
                        kind: ErrorKind::VariableNotPreviouslyDefined,
                        col: *column,
                        pointers_count: identifier_text.len() as usize32,
                    });
                };

                Expression::Variable { variable, column: *column }
            },

            st::Expression::Array { items_start, items_len, open_square_bracket_column, .. }
            | st::Expression::ArrayTrailingItem {
                items_start, items_len, open_square_bracket_column, ..
            } => {
                if *items_len == 0 {
                    return Err(Msg {
                        severity: MsgSeverity::Error,
                        kind: ErrorKind::ArrayOfZeroItems,
                        col: *open_square_bracket_column,
                        pointers_count: 1,
                    });
                }

                let temp_array_items_start = self.temp_array_items.len();
                let mut item_index = *items_start;

                let first_item = &self.tast.syntax_tree.array_items[item_index];
                item_index.0 += 1;

                // TODO(stefano): take into consideration the array base type instead of the
                // whole type
                let parsed_first_item = self.expression(first_item.expression, None)?;
                let mut expected_array_items_type = parsed_first_item.typ(&self.tast);
                if let Type::Array { .. } = expected_array_items_type {
                    return Err(Msg {
                        severity: MsgSeverity::Error,
                        kind: ErrorKind::NestedArrayNotSupportedYet,
                        col: self.first_token_column(first_item.expression),
                        pointers_count: self.first_token_display_len(first_item.expression),
                    });
                }

                if let Some(typ) = expected_type {
                    expected_array_items_type = Type::Base(typ.base_typ());
                    self.expect_expression_type(
                        first_item.expression, &expected_array_items_type, &parsed_first_item,
                    )?;
                }
                let expected_array_items_type_hint = Some(&expected_array_items_type);

                let parsed_first_item_index = self.tast.new_expression(parsed_first_item);
                self.temp_array_items.push(parsed_first_item_index);

                let items_end = (items_start.0 + items_len) as usize;
                while (item_index.0 as usize) < items_end {
                    let item = &self.tast.syntax_tree.array_items[item_index];
                    item_index.0 += 1;

                    let parsed_item = self.expression(item.expression, expected_array_items_type_hint)?;
                    let parsed_item_type = parsed_item.typ(&self.tast);
                    if let Type::Array { .. } = parsed_item_type {
                        return Err(Msg {
                            severity: MsgSeverity::Error,
                            kind: ErrorKind::NestedArrayNotSupportedYet,
                            col: self.first_token_column(item.expression),
                            pointers_count: self.first_token_display_len(item.expression),
                        });
                    }
                    let parsed_item_index = self.tast.new_expression(parsed_item);
                    self.temp_array_items.push(parsed_item_index);
                }

                let array_items = &self.temp_array_items[temp_array_items_start..];
                self.tast.array_items.extend_from_slice(array_items);

                let array_expression = Expression::Array {
                    base_type: expected_array_items_type.base_typ(),
                    items_start: Index32::new_usize32(items_start.0),
                    items_len: *items_len as u64,
                };

                unsafe {
                    self.temp_array_items.set_len(temp_array_items_start);
                }
                array_expression
            },

            st::Expression::Prefix { operator, operator_column, right_operand } => match operator {
                st::PrefixOp::Len => {
                    let right_operand_expression = self.expression(*right_operand, None)?;
                    let right_operand_type = right_operand_expression.typ(&self.tast);
                    match right_operand_type {
                        Type::Base(BaseType::Str) | Type::Array { .. } => Expression::Prefix {
                            operator: (*operator).into(),
                            operator_column: *operator_column,
                            right_operand: self.tast.new_expression(right_operand_expression),
                        },
                        Type::Base(BaseType::I64 | BaseType::Ascii | BaseType::Bool) => {
                            return Err(Msg {
                                severity: MsgSeverity::Error,
                                kind: ErrorKind::CannotTakeLenOf(right_operand_type),
                                col: self.first_token_column(*right_operand),
                                pointers_count: self.first_token_display_len(*right_operand),
                            });
                        },
                    }
                },
                st::PrefixOp::Not => {
                    let right_operand_expression = self.expression(*right_operand, None)?;
                    let right_operand_type = right_operand_expression.typ(&self.tast);
                    match right_operand_type {
                        Type::Base(BaseType::Ascii | BaseType::I64) => Expression::Prefix {
                            operator: (*operator).into(),
                            operator_column: *operator_column,
                            right_operand: self.tast.new_expression(right_operand_expression),
                        },
                        Type::Base(BaseType::Bool) => Expression::BooleanPrefix {
                            operator: (*operator).into(),
                            operator_column: *operator_column,
                            right_operand: self.tast.new_expression(right_operand_expression),
                        },
                        Type::Base(BaseType::Str) | Type::Array { .. } => {
                            return Err(Msg {
                                severity: MsgSeverity::Error,
                                kind: ErrorKind::CannotInvert(right_operand_type),
                                col: self.first_token_column(*right_operand),
                                pointers_count: self.first_token_display_len(*right_operand),
                            });
                        },
                    }
                },
                st::PrefixOp::Plus | st::PrefixOp::WrappingPlus | st::PrefixOp::SaturatingPlus => {
                    let right_operand_expression = self.expression(*right_operand, None)?;
                    let right_operand_type = right_operand_expression.typ(&self.tast);
                    match right_operand_type {
                        Type::Base(BaseType::I64) => Expression::Prefix {
                            operator: (*operator).into(),
                            operator_column: *operator_column,
                            right_operand: self.tast.new_expression(right_operand_expression),
                        },
                        Type::Base(BaseType::Bool | BaseType::Ascii | BaseType::Str) | Type::Array { .. } => {
                            return Err(Msg {
                                severity: MsgSeverity::Error,
                                kind: ErrorKind::CannotTakeAbsoluteValueOf(right_operand_type),
                                col: self.first_token_column(*right_operand),
                                pointers_count: self.first_token_display_len(*right_operand),
                            });
                        },
                    }
                },
                st::PrefixOp::Minus | st::PrefixOp::WrappingMinus | st::PrefixOp::SaturatingMinus => {
                    let st_right_operand = &self.tast.syntax_tree.expressions[*right_operand];
                    match st_right_operand {
                        st::Expression::BinaryInteger { text_end, column } => {
                            let literal_text = self.tast.syntax_tree.tokens.src.src_file.text(*column, *text_end);
                            let right_operand_expression =
                                match Self::parse_negative_i64(literal_text, digit::Base::Binary, false) {
                                    Ok(0) => {
                                        return Err(Msg {
                                            severity: MsgSeverity::Error,
                                            kind: ErrorKind::MinusZeroInteger,
                                            col: *column,
                                            pointers_count: literal_text.len() as usize32,
                                        });
                                    },
                                    Ok(integer) => Expression::I64 { value: integer, column: *column },
                                    Err(()) => {
                                        return Err(Msg {
                                            severity: MsgSeverity::Error,
                                            kind: ErrorKind::BinaryIntegerUnderflow,
                                            col: *column,
                                            pointers_count: literal_text.len() as usize32,
                                        });
                                    },
                                };

                            Expression::Prefix {
                                operator: (*operator).into(),
                                operator_column: *operator_column,
                                right_operand: self.tast.new_expression(right_operand_expression),
                            }
                        },
                        st::Expression::OctalInteger { text_end, column } => {
                            let literal_text = self.tast.syntax_tree.tokens.src.src_file.text(*column, *text_end);
                            let right_operand_expression =
                                match Self::parse_negative_i64(literal_text, digit::Base::Octal, false) {
                                    Ok(0) => {
                                        return Err(Msg {
                                            severity: MsgSeverity::Error,
                                            kind: ErrorKind::MinusZeroInteger,
                                            col: *column,
                                            pointers_count: literal_text.len() as usize32,
                                        });
                                    },
                                    Ok(integer) => Expression::I64 { value: integer, column: *column },
                                    Err(()) => {
                                        return Err(Msg {
                                            severity: MsgSeverity::Error,
                                            kind: ErrorKind::OctalIntegerUnderflow,
                                            col: *column,
                                            pointers_count: literal_text.len() as usize32,
                                        });
                                    },
                                };

                            Expression::Prefix {
                                operator: (*operator).into(),
                                operator_column: *operator_column,
                                right_operand: self.tast.new_expression(right_operand_expression),
                            }
                        },
                        st::Expression::DecimalInteger { text_end, column } => {
                            let literal_text = self.tast.syntax_tree.tokens.src.src_file.text(*column, *text_end);
                            let right_operand_expression =
                                match Self::parse_negative_i64(literal_text, digit::Base::Decimal, false) {
                                    Ok(0) => {
                                        return Err(Msg {
                                            severity: MsgSeverity::Error,
                                            kind: ErrorKind::MinusZeroInteger,
                                            col: *column,
                                            pointers_count: literal_text.len() as usize32,
                                        });
                                    },
                                    Ok(integer) => Expression::I64 { value: integer, column: *column },
                                    Err(()) => {
                                        return Err(Msg {
                                            severity: MsgSeverity::Error,
                                            kind: ErrorKind::DecimalIntegerUnderflow,
                                            col: *column,
                                            pointers_count: literal_text.len() as usize32,
                                        });
                                    },
                                };

                            Expression::Prefix {
                                operator: (*operator).into(),
                                operator_column: *operator_column,
                                right_operand: self.tast.new_expression(right_operand_expression),
                            }
                        },
                        st::Expression::DecimalIntegerPrefix { text_end, column } => {
                            let literal_text = self.tast.syntax_tree.tokens.src.src_file.text(*column, *text_end);
                            let right_operand_expression =
                                match Self::parse_negative_i64(literal_text, digit::Base::Decimal, true) {
                                    Ok(0) => {
                                        return Err(Msg {
                                            severity: MsgSeverity::Error,
                                            kind: ErrorKind::MinusZeroInteger,
                                            col: *column,
                                            pointers_count: literal_text.len() as usize32,
                                        });
                                    },
                                    Ok(integer) => Expression::I64 { value: integer, column: *column },
                                    Err(()) => {
                                        return Err(Msg {
                                            severity: MsgSeverity::Error,
                                            kind: ErrorKind::DecimalIntegerUnderflow,
                                            col: *column,
                                            pointers_count: literal_text.len() as usize32,
                                        });
                                    },
                                };

                            Expression::Prefix {
                                operator: (*operator).into(),
                                operator_column: *operator_column,
                                right_operand: self.tast.new_expression(right_operand_expression),
                            }
                        },
                        st::Expression::HexadecimalInteger { text_end, column } => {
                            let literal_text = self.tast.syntax_tree.tokens.src.src_file.text(*column, *text_end);
                            let right_operand_expression =
                                match Self::parse_negative_i64(literal_text, digit::Base::Hexadecimal, false)
                                {
                                    Ok(0) => {
                                        return Err(Msg {
                                            severity: MsgSeverity::Error,
                                            kind: ErrorKind::MinusZeroInteger,
                                            col: *column,
                                            pointers_count: literal_text.len() as usize32,
                                        });
                                    },
                                    Ok(integer) => Expression::I64 { value: integer, column: *column },
                                    Err(()) => {
                                        return Err(Msg {
                                            severity: MsgSeverity::Error,
                                            kind: ErrorKind::HexadecimalIntegerUnderflow,
                                            col: *column,
                                            pointers_count: literal_text.len() as usize32,
                                        });
                                    },
                                };

                            Expression::Prefix {
                                operator: (*operator).into(),
                                operator_column: *operator_column,
                                right_operand: self.tast.new_expression(right_operand_expression),
                            }
                        },
                        st::Expression::Array { .. }
                        | st::Expression::ArrayTrailingItem { .. }
                        | st::Expression::False { .. }
                        | st::Expression::True { .. }
                        | st::Expression::Ascii { .. }
                        | st::Expression::Str { .. }
                        | st::Expression::RawStr { .. }
                        | st::Expression::Binary { .. }
                        | st::Expression::Prefix { .. }
                        | st::Expression::Identifier { .. }
                        | st::Expression::IdentifierStr { .. }
                        | st::Expression::Parenthesis { .. }
                        | st::Expression::Index { .. } => {
                            let right_operand_expression = self.expression(*right_operand, None)?;
                            let right_operand_type = right_operand_expression.typ(&self.tast);
                            match right_operand_type {
                                Type::Base(BaseType::I64 | BaseType::Ascii)
                                | Type::Array { base_type: BaseType::I64 | BaseType::Ascii, .. } => {
                                    Expression::Prefix {
                                        operator: (*operator).into(),
                                        operator_column: *operator_column,
                                        right_operand: self.tast.new_expression(right_operand_expression),
                                    }
                                },
                                Type::Base(BaseType::Bool | BaseType::Str) | Type::Array { .. } => {
                                    return Err(Msg {
                                        severity: MsgSeverity::Error,
                                        kind: ErrorKind::CannotNegate(right_operand_type),
                                        col: self.first_token_column(*right_operand),
                                        pointers_count: self.first_token_display_len(*right_operand),
                                    });
                                },
                            }
                        },
                    }
                },
            },
            st::Expression::Binary { left_operand, operator, operator_column, right_operand } => {
                let left_operand_expression = self.expression(*left_operand, None)?;
                let left_operand_type = left_operand_expression.typ(&self.tast);

                let right_operand_expression = self.expression(*right_operand, None)?;
                let right_operand_type = right_operand_expression.typ(&self.tast);

                match operator {
                    st::BinaryOp::Pow
                    | st::BinaryOp::WrappingPow
                    | st::BinaryOp::SaturatingPow
                    | st::BinaryOp::Times
                    | st::BinaryOp::WrappingTimes
                    | st::BinaryOp::SaturatingTimes
                    | st::BinaryOp::Divide
                    | st::BinaryOp::WrappingDivide
                    | st::BinaryOp::SaturatingDivide
                    | st::BinaryOp::Remainder
                    | st::BinaryOp::Plus
                    | st::BinaryOp::WrappingPlus
                    | st::BinaryOp::SaturatingPlus
                    | st::BinaryOp::Minus
                    | st::BinaryOp::WrappingMinus
                    | st::BinaryOp::SaturatingMinus
                    | st::BinaryOp::LeftShift
                    | st::BinaryOp::WrappingLeftShift
                    | st::BinaryOp::SaturatingLeftShift
                    | st::BinaryOp::RightShift
                    | st::BinaryOp::LeftRotate
                    | st::BinaryOp::RightRotate
                    | st::BinaryOp::BitAnd
                    | st::BinaryOp::BitXor
                    | st::BinaryOp::BitOr => {
                        match left_operand_type {
                            Type::Base(BaseType::Ascii | BaseType::Bool | BaseType::I64) => {},
                            Type::Base(BaseType::Str) | Type::Array { .. } => {
                                return Err(Msg {
                                    severity: MsgSeverity::Error,
                                    kind: ErrorKind::LeftOperandTypeMismatch {
                                        expected: BinaryOp::TYPE,
                                        actual: left_operand_type,
                                    },
                                    col: *operator_column,
                                    pointers_count: operator.display_len(),
                                });
                            },
                        }

                        match right_operand_type {
                            Type::Base(BaseType::Ascii | BaseType::Bool | BaseType::I64) => {},
                            Type::Base(BaseType::Str) | Type::Array { .. } => {
                                return Err(Msg {
                                    severity: MsgSeverity::Error,
                                    kind: ErrorKind::RightOperandTypeMismatch {
                                        expected: BinaryOp::TYPE,
                                        actual: right_operand_type,
                                    },
                                    col: *operator_column,
                                    pointers_count: operator.display_len(),
                                });
                            },
                        }

                        Expression::Binary {
                            left_operand: self.tast.new_expression(left_operand_expression),
                            operator: (*operator).into(),
                            operator_column: *operator_column,
                            right_operand: self.tast.new_expression(right_operand_expression),
                        }
                    },

                    st::BinaryOp::And | st::BinaryOp::Or => {
                        match left_operand_type {
                            Type::Base(BaseType::Bool) => {},
                            Type::Base(BaseType::Ascii | BaseType::I64 | BaseType::Str)
                            | Type::Array { .. } => {
                                return Err(Msg {
                                    severity: MsgSeverity::Error,
                                    kind: ErrorKind::LeftOperandTypeMismatch {
                                        expected: BooleanBinaryOp::TYPE,
                                        actual: left_operand_type,
                                    },
                                    col: *operator_column,
                                    pointers_count: operator.display_len(),
                                });
                            },
                        }

                        match right_operand_type {
                            Type::Base(BaseType::Bool) => {},
                            Type::Base(BaseType::Ascii | BaseType::I64 | BaseType::Str)
                            | Type::Array { .. } => {
                                return Err(Msg {
                                    severity: MsgSeverity::Error,
                                    kind: ErrorKind::RightOperandTypeMismatch {
                                        expected: BooleanBinaryOp::TYPE,
                                        actual: right_operand_type,
                                    },
                                    col: *operator_column,
                                    pointers_count: operator.display_len(),
                                });
                            },
                        }

                        Expression::BooleanBinary {
                            left_operand: self.tast.new_expression(left_operand_expression),
                            operator: (*operator).into(),
                            operator_column: *operator_column,
                            right_operand: self.tast.new_expression(right_operand_expression),
                        }
                    },

                    st::BinaryOp::Compare => {
                        if left_operand_type != right_operand_type {
                            return Err(Msg {
                                severity: MsgSeverity::Error,
                                kind: ErrorKind::CannotCompareOperands {
                                    left_operand_type,
                                    right_operand_type,
                                },
                                col: *operator_column,
                                pointers_count: operator.display_len(),
                            });
                        }

                        if let Expression::Comparison { .. } = &left_operand_expression {
                            return Err(Msg {
                                severity: MsgSeverity::Error,
                                kind: ErrorKind::CannotChainComparisons,
                                col: *operator_column,
                                pointers_count: operator.display_len(),
                            });
                        }

                        Expression::Comparison {
                            left_operand: self.tast.new_expression(left_operand_expression),
                            operator: (*operator).into(),
                            operator_column: *operator_column,
                            right_operand: self.tast.new_expression(right_operand_expression),
                        }
                    },
                    st::BinaryOp::EqualsEquals
                    | st::BinaryOp::NotEqualsEquals
                    | st::BinaryOp::Greater
                    | st::BinaryOp::GreaterOrEquals
                    | st::BinaryOp::Less
                    | st::BinaryOp::LessOrEquals => {
                        if left_operand_type != right_operand_type {
                            return Err(Msg {
                                severity: MsgSeverity::Error,
                                kind: ErrorKind::CannotCompareOperands {
                                    left_operand_type,
                                    right_operand_type,
                                },
                                col: *operator_column,
                                pointers_count: operator.display_len(),
                            });
                        }

                        if let Expression::BooleanComparison { .. } = &left_operand_expression {
                            return Err(Msg {
                                severity: MsgSeverity::Error,
                                kind: ErrorKind::CannotChainComparisons,
                                col: *operator_column,
                                pointers_count: operator.display_len(),
                            });
                        }

                        Expression::BooleanComparison {
                            left_operand: self.tast.new_expression(left_operand_expression),
                            operator: (*operator).into(),
                            operator_column: *operator_column,
                            right_operand: self.tast.new_expression(right_operand_expression),
                        }
                    },
                }
            },
            st::Expression::Parenthesis { inner_expression, .. } => {
                return self.expression(*inner_expression, expected_type);
            },
            st::Expression::Index {
                indexed_expression,
                open_square_bracket_column,
                index_expression,
                ..
            } => {
                // IDEA(stefano): try parsing a complete type instead of just an identifier
                let indexed_expression_expression = self.expression(*indexed_expression, None)?;
                let Expression::Variable { .. } = indexed_expression_expression else {
                    return Err(Msg {
                        severity: MsgSeverity::Error,
                        kind: ErrorKind::CannotIndexIntoExpression,
                        col: self.first_token_column(*indexed_expression),
                        pointers_count: self.first_token_display_len(*indexed_expression),
                    });
                };

                let indexed_expression_type = indexed_expression_expression.typ(&self.tast);
                match indexed_expression_type {
                    Type::Base(BaseType::Str) | Type::Array { .. } => {
                        let index_expression_expression = self.expression(*index_expression, None)?;
                        let index_expression_type = index_expression_expression.typ(&self.tast);
                        match index_expression_type {
                            Type::Base(BaseType::I64) => {},
                            Type::Base(BaseType::Ascii | BaseType::Bool | BaseType::Str)
                            | Type::Array { .. } => {
                                return Err(Msg {
                                    severity: MsgSeverity::Error,
                                    kind: ErrorKind::ExpectedIntegerExpressionInArrayIndex,
                                    col: self.first_token_column(*index_expression),
                                    pointers_count: self.first_token_display_len(*index_expression),
                                });
                            },
                        }

                        Expression::Index {
                            indexed_expression: self.tast.new_expression(indexed_expression_expression),
                            open_square_bracket_column: *open_square_bracket_column,
                            index_expression: self.tast.new_expression(index_expression_expression),
                        }
                    },
                    Type::Base(BaseType::I64 | BaseType::Ascii | BaseType::Bool) => {
                        return Err(Msg {
                            severity: MsgSeverity::Error,
                            kind: ErrorKind::CannotIndexNonArrayLikeType(indexed_expression_type),
                            col: self.first_token_column(*indexed_expression),
                            pointers_count: self.first_token_display_len(*indexed_expression),
                        });
                    },
                }
            },
        };

        if let Some(expected_expression_type) = expected_type {
            self.expect_expression_type(st_expression_index, expected_expression_type, &expression)?;
        }

        return Ok(expression);
    }

    fn expect_expression_type(
        &self,
        st_expression_index: Index32<st::Expression>,
        expected_type: &Type,
        expression: &Expression,
    ) -> Result<(), Msg<ErrorKind>> {
        let expression_type = expression.typ(&self.tast);
        if expression_type != *expected_type {
            return Err(Msg {
                severity: MsgSeverity::Error,
                kind: ErrorKind::TypeMismatch { expected: expected_type.clone(), actual: expression_type },
                col: self.first_token_column(st_expression_index),
                pointers_count: self.first_token_display_len(st_expression_index),
            });
        }
        return Ok(());
    }

    #[inline]
    fn parse_expression(
        &mut self,
        st_expression_index: Index32<st::Expression>,
        expected_type: Option<&Type>,
    ) -> Result<Index32<Expression>, Msg<ErrorKind>> {
        let expression = self.expression(st_expression_index, expected_type)?;
        let expression_index = self.tast.new_expression(expression);
        return Ok(expression_index);
    }
}

impl Parser<'_, '_, '_, '_> {
    // BUG(stefano): "leaks" memory by parsing and storing expressions, but these expressions
    // should never be used
    fn parse_type_annotation(
        &mut self,
        st::TypeAnnotation {
            type_name_text_end,
            type_name_column,
            array_dimensions_start,
            array_dimensions_len,
        }: &st::TypeAnnotation,
    ) -> Result<Type, Msg<ErrorKind>> {
        let type_name_text = self.tast.syntax_tree.tokens.src.src_file.text(*type_name_column, *type_name_text_end);
        if let Some(_) = self.resolve_variable(type_name_text) {
            return Err(Msg {
                severity: MsgSeverity::Error,
                kind: ErrorKind::VariableInTypeAnnotation,
                col: *type_name_column,
                pointers_count: type_name_text.len() as usize32,
            });
        }
        let Some(base_type) = self.resolve_type(type_name_text) else {
            return Err(Msg {
                severity: MsgSeverity::Error,
                kind: ErrorKind::TypeNotPreviouslyDefined,
                col: *type_name_column,
                pointers_count: type_name_text.len() as usize32,
            });
        };

        let array_dimensions_end = array_dimensions_start.0 + array_dimensions_len;
        let array_dimensions = &self.tast.syntax_tree.array_dimensions
            [array_dimensions_start.0 as usize..array_dimensions_end as usize];
        let mut array_dimensions_iter = array_dimensions.iter();
        let Some(st::ArrayDimension { dimension_expression, open_square_bracket_column, .. }) =
            array_dimensions_iter.next()
        else {
            return Ok(Type::Base(base_type));
        };

        let first_dimension_expression = &self.expression(*dimension_expression, None)?;
        let Expression::I64 { value: len, column } = first_dimension_expression else {
            return Err(Msg {
                severity: MsgSeverity::Error,
                kind: ErrorKind::ExpectedIntegerLiteralInArrayType,
                col: *open_square_bracket_column,
                pointers_count: 1,
            });
        };
        if *len < 0 {
            return Err(Msg {
                severity: MsgSeverity::Error,
                kind: ErrorKind::ArrayOfNegativeLength,
                col: *column,
                pointers_count: self.first_token_display_len(*dimension_expression),
            });
        }
        if *len == 0 {
            return Err(Msg {
                severity: MsgSeverity::Error,
                kind: ErrorKind::ArrayOfZeroItems,
                col: *column,
                pointers_count: self.first_token_display_len(*dimension_expression),
            });
        }

        for st::ArrayDimension {
            dimension_expression: other_dimension_expression,
            open_square_bracket_column: other_open_square_bracket_column,
            ..
        } in array_dimensions_iter
        {
            let dimension_expression_expression = &self.expression(*other_dimension_expression, None)?;
            let Expression::I64 { value: other_dimension_len, column: other_dimension_column } =
                dimension_expression_expression
            else {
                return Err(Msg {
                    severity: MsgSeverity::Error,
                    kind: ErrorKind::ExpectedIntegerLiteralInArrayType,
                    col: *other_open_square_bracket_column,
                    pointers_count: 1,
                });
            };
            if *other_dimension_len < 0 {
                return Err(Msg {
                    severity: MsgSeverity::Error,
                    kind: ErrorKind::ArrayOfNegativeLength,
                    col: *other_dimension_column,
                    pointers_count: self.first_token_display_len(*other_dimension_expression),
                });
            }
            if *other_dimension_len == 0 {
                return Err(Msg {
                    severity: MsgSeverity::Error,
                    kind: ErrorKind::ArrayOfZeroItems,
                    col: *other_dimension_column,
                    pointers_count: self.first_token_display_len(*other_dimension_expression),
                });
            }
        }

        if *array_dimensions_len > 1 {
            return Err(Msg {
                severity: MsgSeverity::Error,
                kind: ErrorKind::NestedArrayNotSupportedYet,
                col: *type_name_column,
                pointers_count: type_name_text.len() as usize32,
            });
        }

        return Ok(Type::Array { base_type, len: *len as u64 });
    }

    fn parse_variable(
        &mut self,
        variable_definition: Index32<st::VariableDefinition>,
    ) -> Result<Index32<VariableDefinition>, Msg<ErrorKind>> {
        let st::VariableDefinition { name_text_end, name_column, type_annotation, expression: initial_value, .. } =
            &self.tast.syntax_tree.variable_definitions[variable_definition];

        let name_text = self.tast.syntax_tree.tokens.src.src_file.text(*name_column, *name_text_end);
        if let Some(_) = self.resolve_variable(name_text) {
            return Err(Msg {
                severity: MsgSeverity::Error,
                kind: ErrorKind::VariableAlreadyDefined,
                col: *name_column,
                pointers_count: name_text.len() as usize32,
            });
        }
        if let Some(_) = self.resolve_type(name_text) {
            return Err(Msg {
                severity: MsgSeverity::Error,
                kind: ErrorKind::TypeInVariableName,
                col: *name_column,
                pointers_count: name_text.len() as usize32,
            });
        }

        // let Some(st::InitialValue { expression, .. }) = initial_value else {
        //     let Some(_) = type_annotation else {
        //         return Err(Msg {
        //             severity: MsgSeverity::Error,
        //             kind: ErrorKind::CannotInferTypeOfVariable,
        //             col: *name_column,
        //             pointers_count: name_text.len() as usize32,
        //         });
        //     };
        //     return Err(Msg {
        //         severity: MsgSeverity::Error,
        //         kind: ErrorKind::VariablesMustBeInitialized,
        //         col: *name_column,
        //         pointers_count: name_text.len() as usize32,
        //     });
        // };

        let expression_type = self.parse_type_annotation(type_annotation)?;
        let parsed_expression = self.expression(*initial_value, Some(&expression_type))?;

        let variable = VariableDefinition {
            name_text_end: *name_text_end,
            name_column: *name_column,
            typ: expression_type,
            value: self.tast.new_expression(parsed_expression),
        };
        let variable_index = self.tast.new_variable(variable);
        return Ok(variable_index);
    }
}

impl Parser<'_, '_, '_, '_> {
    fn binary_assignment(
        &mut self,
        target: Index32<st::Expression>,
        operator: st::BinaryAssignmentOp,
        operator_column: usize32,
        new_value: Index32<st::Expression>,
    ) -> Result<Node, Msg<ErrorKind>> {
        let parsed_target = self.expression(target, None)?;

        let parsed_new_value = self.expression(new_value, None)?;
        let parsed_new_value_type = parsed_new_value.typ(&self.tast);
        let parsed_target_type = match &parsed_target {
            Expression::Variable { variable, column } => {
                let variable_definition = &self.tast.variables[*variable];
                let variable_name_text = self.tast.syntax_tree.tokens.src.src_file.text(variable_definition.name_column, variable_definition.name_text_end);
                if let Some(_) = self.resolve_let_variable(variable_name_text) {
                    return Err(Msg {
                        severity: MsgSeverity::Error,
                        kind: ErrorKind::CannotMutateVariable,
                        col: *column,
                        pointers_count: variable_name_text.len() as usize32,
                    });
                }
                variable_definition.typ.clone()
            },
            Expression::Index { indexed_expression, .. } => {
                let mut base_indexed = &self.tast.expressions[*indexed_expression];
                while let Expression::Index { indexed_expression: inner_indexed_expression, .. } =
                    base_indexed
                {
                    base_indexed = &self.tast.expressions[*inner_indexed_expression];
                }

                let Expression::Variable { variable, column } = base_indexed else {
                    return Err(Msg {
                        severity: MsgSeverity::Error,
                        kind: ErrorKind::CannotAssignToExpression,
                        col: operator_column,
                        pointers_count: operator.display_len(),
                    });
                };

                let variable_definition = &self.tast.variables[*variable];
                let variable_name_text = self.tast.syntax_tree.tokens.src.src_file.text(variable_definition.name_column, variable_definition.name_text_end);
                if let Some(_) = self.resolve_let_variable(variable_name_text) {
                    return Err(Msg {
                        severity: MsgSeverity::Error,
                        kind: ErrorKind::CannotMutateVariable,
                        col: *column,
                        pointers_count: variable_name_text.len() as usize32,
                    });
                }

                if let Type::Base(BaseType::Str) = variable_definition.typ {
                    if let BaseType::Ascii = parsed_new_value_type.base_typ() {
                        return Err(Msg {
                            severity: MsgSeverity::Error,
                            kind: ErrorKind::CannotMutateStringCharacters,
                            col: *column,
                            pointers_count: variable_name_text.len() as usize32,
                        });
                    }
                    Type::Base(BaseType::Str)
                } else {
                    Type::Base(variable_definition.typ.base_typ())
                }
            },
            Expression::False { .. }
            | Expression::True { .. }
            | Expression::I64 { .. }
            | Expression::Ascii { .. }
            | Expression::Str { .. }
            | Expression::Array { .. }
            | Expression::Prefix { .. }
            | Expression::BooleanPrefix { .. }
            | Expression::Binary { .. }
            | Expression::BooleanBinary { .. }
            | Expression::Comparison { .. }
            | Expression::BooleanComparison { .. } => {
                return Err(Msg {
                    severity: MsgSeverity::Error,
                    kind: ErrorKind::CannotAssignToExpression,
                    col: operator_column,
                    pointers_count: operator.display_len(),
                });
            },
        };

        let assignment_node = match operator {
            st::BinaryAssignmentOp::Equals => {
                if parsed_target_type != parsed_new_value_type {
                    return Err(Msg {
                        severity: MsgSeverity::Error,
                        kind: ErrorKind::RightOperandTypeMismatch {
                            expected: parsed_target_type,
                            actual: parsed_new_value_type,
                        },
                        col: operator_column,
                        pointers_count: operator.display_len(),
                    });
                }

                Node::Assignment {
                    target: self.tast.new_expression(parsed_target),
                    operator: AssignmentOp::Equals,
                    new_value: self.tast.new_expression(parsed_new_value),
                }
            },

            st::BinaryAssignmentOp::Pow
            | st::BinaryAssignmentOp::WrappingPow
            | st::BinaryAssignmentOp::SaturatingPow
            | st::BinaryAssignmentOp::Times
            | st::BinaryAssignmentOp::WrappingTimes
            | st::BinaryAssignmentOp::SaturatingTimes
            | st::BinaryAssignmentOp::Divide
            | st::BinaryAssignmentOp::WrappingDivide
            | st::BinaryAssignmentOp::SaturatingDivide
            | st::BinaryAssignmentOp::Remainder
            | st::BinaryAssignmentOp::Plus
            | st::BinaryAssignmentOp::WrappingPlus
            | st::BinaryAssignmentOp::SaturatingPlus
            | st::BinaryAssignmentOp::Minus
            | st::BinaryAssignmentOp::WrappingMinus
            | st::BinaryAssignmentOp::SaturatingMinus
            | st::BinaryAssignmentOp::LeftShift
            | st::BinaryAssignmentOp::WrappingLeftShift
            | st::BinaryAssignmentOp::SaturatingLeftShift
            | st::BinaryAssignmentOp::RightShift
            | st::BinaryAssignmentOp::LeftRotate
            | st::BinaryAssignmentOp::RightRotate
            | st::BinaryAssignmentOp::BitAnd
            | st::BinaryAssignmentOp::BitXor
            | st::BinaryAssignmentOp::BitOr => {
                match parsed_target_type {
                    Type::Base(BaseType::Ascii | BaseType::I64 | BaseType::Bool) => {},
                    Type::Base(BaseType::Str) | Type::Array { .. } => {
                        return Err(Msg {
                            severity: MsgSeverity::Error,
                            kind: ErrorKind::LeftOperandTypeMismatch {
                                expected: BinaryOp::TYPE,
                                actual: parsed_target_type,
                            },
                            col: operator_column,
                            pointers_count: operator.display_len(),
                        });
                    },
                }

                match parsed_new_value_type {
                    Type::Base(BaseType::Ascii | BaseType::I64 | BaseType::Bool) => {},
                    Type::Base(BaseType::Str) | Type::Array { .. } => {
                        return Err(Msg {
                            severity: MsgSeverity::Error,
                            kind: ErrorKind::RightOperandTypeMismatch {
                                expected: BinaryOp::TYPE,
                                actual: parsed_new_value_type,
                            },
                            col: operator_column,
                            pointers_count: operator.display_len(),
                        });
                    },
                }

                match (parsed_target_type, parsed_new_value_type) {
                    (
                        Type::Base(BaseType::I64),
                        Type::Base(BaseType::I64 | BaseType::Ascii | BaseType::Bool),
                    ) => {},
                    (target_type @ Type::Base(BaseType::Ascii | BaseType::Bool), _) => {
                        return Err(Msg {
                            severity: MsgSeverity::Error,
                            kind: ErrorKind::CannotModifyInplace(target_type),
                            col: operator_column,
                            pointers_count: operator.display_len(),
                        });
                    },
                    (target_type, new_value_type) => {
                        return Err(Msg {
                            severity: MsgSeverity::Error,
                            kind: ErrorKind::TypeMismatch { expected: target_type, actual: new_value_type },
                            col: self.first_token_column(new_value),
                            pointers_count: self.first_token_display_len(new_value),
                        });
                    },
                }

                Node::BinaryAssignment {
                    target: self.tast.new_expression(parsed_target),
                    operator: operator.into(),
                    operator_column,
                    new_value: self.tast.new_expression(parsed_new_value),
                }
            },
            st::BinaryAssignmentOp::And | st::BinaryAssignmentOp::Or => {
                match parsed_target_type {
                    Type::Base(BaseType::Bool) => {},
                    Type::Base(BaseType::Ascii | BaseType::I64 | BaseType::Str) | Type::Array { .. } => {
                        return Err(Msg {
                            severity: MsgSeverity::Error,
                            kind: ErrorKind::LeftOperandTypeMismatch {
                                expected: BooleanBinaryOp::TYPE,
                                actual: parsed_target_type,
                            },
                            col: operator_column,
                            pointers_count: operator.display_len(),
                        });
                    },
                }

                match parsed_new_value_type {
                    Type::Base(BaseType::Bool) => {},
                    Type::Base(BaseType::Ascii | BaseType::I64 | BaseType::Str) | Type::Array { .. } => {
                        return Err(Msg {
                            severity: MsgSeverity::Error,
                            kind: ErrorKind::RightOperandTypeMismatch {
                                expected: BooleanBinaryOp::TYPE,
                                actual: parsed_new_value_type,
                            },
                            col: operator_column,
                            pointers_count: operator.display_len(),
                        });
                    },
                }

                Node::BooleanAssignmentExpression {
                    target: self.tast.new_expression(parsed_target),
                    operator: operator.into(),
                    operator_column,
                    new_value: self.tast.new_expression(parsed_new_value),
                }
            },
        };

        return Ok(assignment_node);
    }

    fn prefix_assignment(
        &mut self,
        target: Index32<st::Expression>,
        operator: st::PrefixAssignmentOp,
        operator_column: usize32,
    ) -> Result<Node, Msg<ErrorKind>> {
        let parsed_target = self.expression(target, None)?;

        let parsed_target_type = match &parsed_target {
            Expression::Variable { variable, column } => {
                let variable_definition = &self.tast.variables[*variable];
                let variable_name_text = self.tast.syntax_tree.tokens.src.src_file.text(variable_definition.name_column, variable_definition.name_text_end);
                if let Some(_) = self.resolve_let_variable(variable_name_text) {
                    return Err(Msg {
                        severity: MsgSeverity::Error,
                        kind: ErrorKind::CannotMutateVariable,
                        col: *column,
                        pointers_count: variable_name_text.len() as usize32,
                    });
                }
                variable_definition.typ.clone()
            },
            Expression::Index { indexed_expression, .. } => {
                let mut base_indexed = &self.tast.expressions[*indexed_expression];
                while let Expression::Index { indexed_expression: inner_indexed_expression, .. } =
                    base_indexed
                {
                    base_indexed = &self.tast.expressions[*inner_indexed_expression];
                }

                let Expression::Variable { variable, column } = base_indexed else {
                    return Err(Msg {
                        severity: MsgSeverity::Error,
                        kind: ErrorKind::CannotAssignToExpression,
                        col: operator_column,
                        pointers_count: operator.display_len(),
                    });
                };

                let variable_definition = &self.tast.variables[*variable];
                let variable_name_text = self.tast.syntax_tree.tokens.src.src_file.text(variable_definition.name_column, variable_definition.name_text_end);
                if let Some(_) = self.resolve_let_variable(variable_name_text) {
                    return Err(Msg {
                        severity: MsgSeverity::Error,
                        kind: ErrorKind::CannotMutateVariable,
                        col: *column,
                        pointers_count: variable_name_text.len() as usize32,
                    });
                }

                variable_definition.typ.clone()
            },
            Expression::False { .. }
            | Expression::True { .. }
            | Expression::I64 { .. }
            | Expression::Ascii { .. }
            | Expression::Str { .. }
            | Expression::Array { .. }
            | Expression::Prefix { .. }
            | Expression::BooleanPrefix { .. }
            | Expression::Binary { .. }
            | Expression::BooleanBinary { .. }
            | Expression::Comparison { .. }
            | Expression::BooleanComparison { .. } => {
                return Err(Msg {
                    severity: MsgSeverity::Error,
                    kind: ErrorKind::CannotAssignToExpression,
                    col: operator_column,
                    pointers_count: operator.display_len(),
                });
            },
        };

        let assignment_node = match operator {
            st::PrefixAssignmentOp::NotEquals => match parsed_target_type {
                Type::Base(BaseType::Ascii | BaseType::I64) => Node::PrefixAssignmentExpression {
                    target: self.tast.new_expression(parsed_target),
                    operator: operator.into(),
                    operator_column,
                },
                Type::Base(BaseType::Bool) => Node::BooleanPrefixAssignment {
                    target: self.tast.new_expression(parsed_target),
                    operator: operator.into(),
                    operator_column,
                },
                Type::Base(BaseType::Str) | Type::Array { .. } => {
                    return Err(Msg {
                        severity: MsgSeverity::Error,
                        kind: ErrorKind::CannotInvert(parsed_target_type),
                        col: operator_column,
                        pointers_count: operator.display_len(),
                    });
                },
            },

            st::PrefixAssignmentOp::PlusEquals
            | st::PrefixAssignmentOp::WrappingPlusEquals
            | st::PrefixAssignmentOp::SaturatingPlusEquals => {
                match parsed_target_type {
                    Type::Base(BaseType::I64) => {},
                    Type::Base(BaseType::Str | BaseType::Ascii | BaseType::Bool) | Type::Array { .. } => {
                        return Err(Msg {
                            severity: MsgSeverity::Error,
                            kind: ErrorKind::CannotTakeAbsoluteValueOf(parsed_target_type),
                            col: operator_column,
                            pointers_count: operator.display_len(),
                        });
                    },
                }

                Node::PrefixAssignmentExpression {
                    target: self.tast.new_expression(parsed_target),
                    operator: operator.into(),
                    operator_column,
                }
            },

            st::PrefixAssignmentOp::MinusEquals
            | st::PrefixAssignmentOp::WrappingMinusEquals
            | st::PrefixAssignmentOp::SaturatingMinusEquals => {
                match parsed_target_type {
                    Type::Base(BaseType::Ascii | BaseType::I64) => {},
                    Type::Base(BaseType::Str | BaseType::Bool) | Type::Array { .. } => {
                        return Err(Msg {
                            severity: MsgSeverity::Error,
                            kind: ErrorKind::CannotNegate(parsed_target_type),
                            col: operator_column,
                            pointers_count: operator.display_len(),
                        });
                    },
                }

                Node::PrefixAssignmentExpression {
                    target: self.tast.new_expression(parsed_target),
                    operator: operator.into(),
                    operator_column,
                }
            },
        };

        return Ok(assignment_node);
    }
}

impl Parser<'_, '_, '_, '_> {
    fn if_condition(
        &mut self,
        if_column: usize32,
        condition: Index32<st::Expression>,
    ) -> Result<Expression, Msg<ErrorKind>> {
        let condition_expression = self.expression(condition, None)?;
        let condition_expression_type = condition_expression.typ(&self.tast);
        let Type::Base(BaseType::Bool) = condition_expression_type else {
            return Err(Msg {
                severity: MsgSeverity::Error,
                kind: ErrorKind::RightOperandTypeMismatch {
                    expected: Type::Base(BaseType::Bool),
                    actual: condition_expression_type,
                },
                col: if_column,
                pointers_count: Token { kind: TokenKind::If, col: if_column }.display_len(self.tast.syntax_tree.tokens.src.src_file),
            });
        };
        return Ok(condition_expression);
    }

    fn loop_condition(
        &mut self,
        loop_column: usize32,
        condition: Index32<st::Expression>,
    ) -> Result<Expression, Msg<ErrorKind>> {
        let condition_expression = self.expression(condition, None)?;
        let condition_expression_type = condition_expression.typ(&self.tast);
        let Type::Base(BaseType::Bool) = condition_expression_type else {
            return Err(Msg {
                severity: MsgSeverity::Error,
                kind: ErrorKind::RightOperandTypeMismatch {
                    expected: Type::Base(BaseType::Bool),
                    actual: condition_expression_type,
                },
                col: loop_column,
                pointers_count: Token { kind: TokenKind::Loop, col: loop_column }.display_len(self.tast.syntax_tree.tokens.src.src_file),
            });
        };
        return Ok(condition_expression);
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub struct TypedSyntaxTreeDisplay<'tast, 'st: 'tast, 'tokens: 'st, 'code: 'tokens, 'path> {
    pub(crate) tast: &'tast TypedSyntaxTree<'st, 'tokens, 'code, 'path>,
}

impl TypedSyntaxTreeDisplay<'_, '_, '_, '_, '_> {
    const INDENT_INCREMENT: usize = 2;

    fn info_if(
        &self,
        f: &mut core::fmt::Formatter<'_>,
        node_index: &mut Index32<Node>,
        indent: usize,
        condition: Index32<Expression>,
    ) -> core::fmt::Result {
        writeln!(f, "{:>indent$}If = if", "")?;
        let if_indent = indent + Self::INDENT_INCREMENT;
        self.info_expression(f, condition, if_indent)?;
        return self.info_node(f, node_index, if_indent);
    }

    fn info_node(
        &self,
        f: &mut core::fmt::Formatter<'_>,
        node_index: &mut Index32<Node>,
        indent: usize,
    ) -> core::fmt::Result {
        let node = &self.tast.nodes[*node_index];
        node_index.0 += 1;

        #[rustfmt::skip]
        return match node {
            Node::Expression(expression) => {
                self.info_expression(f, *expression, indent)
            }
            Node::Assignment { target, new_value, operator } => {
                writeln!(f, "{:>indent$}Assignment", "")?;
                let assignment_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *target, assignment_indent)?;
                writeln!(f, "{:>assignment_indent$}AssignmentOp = {operator}", "")?;
                self.info_expression(f, *new_value, assignment_indent)
            }
            Node::BinaryAssignment { target, operator, new_value, .. } => {
                writeln!(f, "{:>indent$}AssignmentExpression", "")?;
                let assignment_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *target, assignment_indent)?;
                writeln!(f, "{:>assignment_indent$}BinaryAssignmentOp = {operator}", "")?;
                self.info_expression(f, *new_value, assignment_indent)
            }
            Node::BooleanAssignmentExpression { target, operator, new_value, .. } => {
                writeln!(f, "{:>indent$}BooleanAssignmentExpression", "")?;
                let assignment_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *target, assignment_indent)?;
                writeln!(f, "{:>assignment_indent$}BooleanBinaryAssignmentOp = {operator}", "")?;
                self.info_expression(f, *new_value, assignment_indent)
            }
            Node::PrefixAssignmentExpression { target, operator, .. } => {
                writeln!(f, "{:>indent$}Assignment", "")?;
                let assignment_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *target, assignment_indent)?;
                writeln!(f, "{:>assignment_indent$}PrefixAssignmentOp = {operator}", "")
            }
            Node::BooleanPrefixAssignment { target, operator, .. } => {
                writeln!(f, "{:>indent$}Assignment", "")?;
                let assignment_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *target, assignment_indent)?;
                writeln!(f, "{:>assignment_indent$}BooleanPrefixAssignmentOp = {operator}", "")
            }

            Node::Print { argument, .. } => {
                writeln!(f, "{:>indent$}Print = print", "")?;
                let argument_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *argument, argument_indent)
            }
            Node::Println { argument, .. } => {
                writeln!(f, "{:>indent$}Println = println", "")?;
                let argument_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *argument, argument_indent)
            }
            Node::PrintlnNoArg { .. } => {
                writeln!(f, "{:>indent$}Println = println", "")
            }
            Node::Eprint { argument, .. } => {
                writeln!(f, "{:>indent$}Eprint = eprint", "")?;
                let argument_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *argument, argument_indent)
            }
            Node::Eprintln { argument, .. } => {
                writeln!(f, "{:>indent$}Eprintln = eprintln", "")?;
                let argument_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *argument, argument_indent)
            }
            Node::EprintlnNoArg { .. } => {
                writeln!(f, "{:>indent$}Eprintln = eprintln", "")
            }

            Node::LetVariableDefinition { variable } => {
                writeln!(f, "{:>indent$}VariableDefinition = let", "")?;
                let definition_indent = indent + Self::INDENT_INCREMENT;
                self.info_variable(f, *variable, definition_indent)
            }
            Node::VarVariableDefinition { variable } => {
                writeln!(f, "{:>indent$}VariableDefinition = var", "")?;
                let definition_indent = indent + Self::INDENT_INCREMENT;
                self.info_variable(f, *variable, definition_indent)
            }

            Node::Scope { raw_nodes_in_scope_count } => {
                writeln!(f, "{:>indent$}Scope", "")?;
                let scope_indent = indent + Self::INDENT_INCREMENT;
                let after_end_scope_node_index = node_index.0 + raw_nodes_in_scope_count;
                while node_index.0 < after_end_scope_node_index {
                    self.info_node(f, node_index, scope_indent)?;
                }
                Ok(())
            }

            Node::If { condition, .. } => {
                self.info_if(f, node_index, indent, *condition)
            }
            Node::ElseIf { condition, .. } => {
                writeln!(f, "{:>indent$}Else = else", "")?;
                self.info_if(f, node_index, indent, *condition)
            }
            Node::Else { .. } => {
                writeln!(f, "{:>indent$}Else = else", "")?;
                let else_indent = indent + Self::INDENT_INCREMENT;
                self.info_node(f, node_index, else_indent)
            }

            Node::Loop { condition, .. } => {
                writeln!(f, "{:>indent$}Loop = loop", "")?;
                let loop_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *condition, loop_indent)?;
                self.info_node(f, node_index, loop_indent)
            }
            Node::DoLoop { condition, .. } => {
                writeln!(f, "{:>indent$}Do = do", "")?;
                writeln!(f, "{:>indent$}Loop = loop", "")?;
                let loop_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *condition, loop_indent)?;
                self.info_node(f, node_index, loop_indent)
            }
            Node::Break { .. } => {
                writeln!(f, "{:>indent$}Break = break", "")
            }
            Node::Continue { .. } => {
                writeln!(f, "{:>indent$}Continue = continue", "")
            }
        };
    }

    fn info_expression(
        &self,
        f: &mut core::fmt::Formatter<'_>,
        expression_index: Index32<Expression>,
        indent: usize,
    ) -> core::fmt::Result {
        let expression = &self.tast.expressions[expression_index];

        #[rustfmt::skip]
        return match expression {
            Expression::False { .. } => writeln!(f, "{:>indent$}False = false", ""),
            Expression::True { .. } => writeln!(f, "{:>indent$}True = true", ""),
            Expression::I64 { value, .. } => {
                writeln!(f, "{:>indent$}I64 = {value}", "")
            }
            Expression::Ascii { character, .. } => {
                writeln!(f, "{:>indent$}Ascii = {character}", "")
            }
            Expression::Str { text_end, column } => {
                let literal_str = self.tast.syntax_tree.tokens.src.src_file.text(*column, *text_end);
                writeln!(f, "{:>indent$}Str = {literal_str}", "")
            }
            Expression::Variable { variable, .. } => {
                let variable_definition = &self.tast.variables[*variable];
                let identifier_str = self.tast.syntax_tree.tokens.src.src_file.text(variable_definition.name_column, variable_definition.name_text_end);
                writeln!(f, "{:>indent$}Identifier = {identifier_str}", "")
            }
            Expression::Array { items_start, items_len, .. } => {
                let expression_indent = indent + Self::INDENT_INCREMENT;
                writeln!(f, "{:>indent$}Array", "")?;

                let items_indent = expression_indent + Self::INDENT_INCREMENT;
                let items_end = items_start.0 as usize + *items_len as usize;
                let items = &self.tast.array_items[items_start.0 as usize..items_end];
                for item_expression in items {
                    self.info_expression(f, *item_expression, items_indent)?;
                }
                Ok(())
            }

            Expression::Prefix { operator, right_operand, .. } => {
                let expression_indent = indent + Self::INDENT_INCREMENT;
                writeln!(f, "{:>indent$}PrefixExpression", "")?;
                writeln!(f, "{:>expression_indent$}PrefixOp = {operator}", "")?;
                self.info_expression(f, *right_operand, expression_indent)
            }
            Expression::BooleanPrefix { operator, right_operand, .. } => {
                let expression_indent = indent + Self::INDENT_INCREMENT;
                writeln!(f, "{:>indent$}BooleanPrefixExpression", "")?;
                writeln!(f, "{:>expression_indent$}BooleanPrefixOp = {operator}", "")?;
                self.info_expression(f, *right_operand, expression_indent)
            }
            Expression::Binary { left_operand, operator, right_operand, .. } => {
                let expression_indent = indent + Self::INDENT_INCREMENT;
                writeln!(f, "{:>indent$}BinaryExpression", "")?;
                self.info_expression(f, *left_operand, expression_indent)?;
                writeln!(f, "{:>expression_indent$}BinaryOp = {operator}", "")?;
                self.info_expression(f, *right_operand, expression_indent)
            }
            Expression::BooleanBinary { left_operand, operator, right_operand, .. } => {
                let expression_indent = indent + Self::INDENT_INCREMENT;
                writeln!(f, "{:>indent$}BooleanBinaryExpression", "")?;
                self.info_expression(f, *left_operand, expression_indent)?;
                writeln!(f, "{:>expression_indent$}BooleanBinaryOp = {operator}", "")?;
                self.info_expression(f, *right_operand, expression_indent)
            }
            Expression::Comparison { left_operand, operator, right_operand, .. } => {
                let expression_indent = indent + Self::INDENT_INCREMENT;
                writeln!(f, "{:>indent$}Comparison", "")?;
                self.info_expression(f, *left_operand, expression_indent)?;
                writeln!(f, "{:>expression_indent$}ComparisonOp = {operator}", "")?;
                self.info_expression(f, *right_operand, expression_indent)
            }
            Expression::BooleanComparison { left_operand, operator, right_operand, .. } => {
                let expression_indent = indent + Self::INDENT_INCREMENT;
                writeln!(f, "{:>indent$}BooleanComparison", "")?;
                self.info_expression(f, *left_operand, expression_indent)?;
                writeln!(f, "{:>expression_indent$}BooleanComparisonOp = {operator}", "")?;
                self.info_expression(f, *right_operand, expression_indent)
            }

            Expression::Index {
                indexed_expression,
                index_expression,
                ..
            } => {
                let expression_indent = indent + Self::INDENT_INCREMENT;
                writeln!(f, "{:indent$}IndexExpression", "")?;
                self.info_expression(f, *indexed_expression, expression_indent)?;
                self.info_expression(f, *index_expression, expression_indent)
            }
        };
    }

    fn info_variable(
        &self,
        f: &mut core::fmt::Formatter<'_>,
        variable_index: Index32<VariableDefinition>,
        indent: usize,
    ) -> core::fmt::Result {
        let VariableDefinition { name_text_end, name_column, value, typ } = &self.tast.variables[variable_index];
        let name_str = self.tast.syntax_tree.tokens.src.src_file.text(*name_column, *name_text_end);
        writeln!(f, "{:>indent$}Name = {name_str}", "")?;
        writeln!(f, "{:>indent$}Type = {typ}", "")?;
        writeln!(f, "{:>indent$}InitialValue", "")?;

        let initial_value_indent = indent + Self::INDENT_INCREMENT;
        return self.info_expression(f, *value, initial_value_indent);
    }
}

impl core::fmt::Display for TypedSyntaxTreeDisplay<'_, '_, '_, '_, '_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let mut node_index = Index32::new(0);
        while (node_index.0 as usize) < self.tast.nodes.len() {
            self.info_node(f, &mut node_index, 0)?;
        }

        return Ok(());
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub enum ErrorKind {
    BinaryIntegerOverflow,
    OctalIntegerOverflow,
    DecimalIntegerOverflow,
    HexadecimalIntegerOverflow,

    BinaryIntegerUnderflow,
    OctalIntegerUnderflow,
    DecimalIntegerUnderflow,
    HexadecimalIntegerUnderflow,

    MinusZeroInteger,

    VariableNotPreviouslyDefined,
    VariableAlreadyDefined,
    VariableInTypeAnnotation,
    TypeNotPreviouslyDefined,
    ExpectedIntegerLiteralInArrayType,
    ArrayOfNegativeLength,
    ArrayOfZeroItems,
    NestedArrayNotSupportedYet,
    TypeInExpression,
    TypeInVariableName,
    CannotInferTypeOfVariable,
    VariablesMustBeInitialized,

    CannotTakeLenOf(Type),
    CannotTakeAbsoluteValueOf(Type),
    CannotNegate(Type),
    CannotInvert(Type),
    CannotCompareOperands { left_operand_type: Type, right_operand_type: Type },
    CannotChainComparisons,
    CannotIndexIntoExpression,
    CannotIndexNonArrayLikeType(Type),
    ExpectedIntegerExpressionInArrayIndex,
    LeftOperandTypeMismatch { expected: Type, actual: Type },
    RightOperandTypeMismatch { expected: Type, actual: Type },
    TypeMismatch { expected: Type, actual: Type },

    CannotAssignToExpression,
    CannotMutateVariable,
    CannotModifyInplace(Type),
    CannotMutateStringCharacters,
}

impl IntoMsgInfo for ErrorKind {
    fn info(&self, _col: usize32, _src: &SrcFile<'_>) -> MsgInfo {
        let (error_message, error_cause_message) = match self {
            Self::BinaryIntegerOverflow => (
                "integer literal overflow".into(),
                format!(
                    "overflows a {bits} bit signed integer, over {prefix}{max:0b} ({max})",
                    bits = i64::BITS,
                    prefix = tokenizer::Base(digit::Base::Binary).prefix(),
                    max = i64::MAX
                )
                .into(),
            ),
            Self::OctalIntegerOverflow => (
                "integer literal overflow".into(),
                format!(
                    "overflows a {bits} bit signed integer, over {prefix}{max:0o} ({max})",
                    bits = i64::BITS,
                    prefix = tokenizer::Base(digit::Base::Octal).prefix(),
                    max = i64::MAX
                )
                .into(),
            ),
            Self::DecimalIntegerOverflow => (
                "integer literal overflow".into(),
                format!(
                    "overflows a {bits} bit signed integer, over {max}",
                    bits = i64::BITS,
                    max = i64::MAX
                )
                .into(),
            ),
            Self::HexadecimalIntegerOverflow => (
                "integer literal overflow".into(),
                format!(
                    "overflows a {bits} bit signed integer, over {prefix}{max:0x} ({max})",
                    bits = i64::BITS,
                    prefix = tokenizer::Base(digit::Base::Hexadecimal).prefix(),
                    max = i64::MAX
                )
                .into(),
            ),
            Self::BinaryIntegerUnderflow => (
                "integer literal underflow".into(),
                format!(
                    "underflows a {bits} bit signed integer, under {prefix}{min:0b} ({min})",
                    bits = i64::BITS,
                    prefix = tokenizer::Base(digit::Base::Binary).prefix(),
                    min = i64::MIN
                )
                .into(),
            ),
            Self::OctalIntegerUnderflow => (
                "integer literal underflow".into(),
                format!(
                    "underflows a {bits} bit signed integer, under {prefix}{min:0o} ({min})",
                    bits = i64::BITS,
                    prefix = tokenizer::Base(digit::Base::Octal).prefix(),
                    min = i64::MIN
                )
                .into(),
            ),
            Self::DecimalIntegerUnderflow => (
                "integer literal underflow".into(),
                format!(
                    "underflows a {bits} bit signed integer, under {min}",
                    bits = i64::BITS,
                    min = i64::MIN
                )
                .into(),
            ),
            Self::HexadecimalIntegerUnderflow => (
                "integer literal underflow".into(),
                format!(
                    "underflows a {bits} bit signed integer, under {prefix}{min:0x} ({min})",
                    bits = i64::BITS,
                    prefix = tokenizer::Base(digit::Base::Hexadecimal).prefix(),
                    min = i64::MIN
                )
                .into(),
            ),

            Self::MinusZeroInteger => {
                ("invalid integer literal".into(), "-0 is not a valid two's complement integer".into())
            },

            Self::VariableNotPreviouslyDefined => {
                ("variable not previously defined".into(), "was not previously defined".into())
            },
            Self::VariableAlreadyDefined => ("variable already defined".into(), "was already defined".into()),
            Self::VariableInTypeAnnotation => (
                "invalid type annotation".into(),
                "variable names are not allowed in type annotations".into(),
            ),
            Self::TypeNotPreviouslyDefined => {
                ("type not previously defined".into(), "was not previously defined".into())
            },
            Self::ExpectedIntegerLiteralInArrayType => {
                ("invalid type".into(), "must be followed by an integer literal".into())
            },
            Self::ArrayOfNegativeLength => {
                ("invalid array length".into(), "array length must be greater than 1".into())
            },
            Self::ArrayOfZeroItems => {
                ("invalid array".into(), "arrays of zero items are not allowed yet".into())
            },
            Self::NestedArrayNotSupportedYet => {
                ("invalid array item".into(), "nested arrays are not supported yet".into())
            },
            Self::TypeInExpression => {
                ("invalid expression".into(), "types are not allowed in expressions".into())
            },
            Self::TypeInVariableName => {
                ("invalid variable name".into(), "types are not allowed in variable names".into())
            },
            Self::CannotInferTypeOfVariable => (
                "invalid variable definition".into(),
                "expected type annotation after here to infer the type of the variable".into(),
            ),
            Self::VariablesMustBeInitialized => (
                "invalid variable definition".into(),
                "variables must be initialized, provide an initial value".into(),
            ),

            Self::CannotTakeLenOf(invalid_type) => (
                "invalid expression".into(),
                format!("cannot take the length of '{invalid_type}', only of arrays and strings").into(),
            ),
            Self::CannotTakeAbsoluteValueOf(invalid_type) => (
                "invalid expression".into(),
                format!("cannot take the absolute value of '{invalid_type}'").into(),
            ),
            Self::CannotNegate(invalid_type) => {
                ("invalid expression".into(), format!("cannot negate value of type '{invalid_type}'").into())
            },
            Self::CannotInvert(invalid_type) => {
                ("invalid expression".into(), format!("cannot invert value of type '{invalid_type}'").into())
            },
            Self::CannotCompareOperands { left_operand_type, right_operand_type } => (
                "invalid expression".into(),
                format!("cannot compare '{left_operand_type}' to '{right_operand_type}'").into(),
            ),
            Self::CannotChainComparisons => {
                ("invalid expression".into(), "comparison operators cannot be chained".into())
            },
            Self::CannotIndexIntoExpression => {
                ("invalid expression".into(), "cannot index into an expression, only to variables".into())
            },
            Self::ExpectedIntegerExpressionInArrayIndex => {
                ("invalid expression".into(), "array index must be followed by an integer literal".into())
            },
            Self::CannotIndexNonArrayLikeType(non_indexable_type) => (
                "invalid expression".into(),
                format!("cannot index into a value of type '{non_indexable_type}'").into(),
            ),
            Self::LeftOperandTypeMismatch { expected, actual } => (
                "invalid expression".into(),
                format!("expected expression of type '{expected}', but is preceded by '{actual}'").into(),
            ),
            Self::RightOperandTypeMismatch { expected, actual } => (
                "invalid expression".into(),
                format!("expected expression of type '{expected}', but is followed by '{actual}'").into(),
            ),
            Self::TypeMismatch { expected, actual } => (
                "invalid expression".into(),
                format!("expected expression of type '{expected}', but got '{actual}'").into(),
            ),

            Self::CannotAssignToExpression => {
                ("invalid assignment".into(), "cannot assign to expression".into())
            },
            Self::CannotMutateVariable => {
                ("invalid assignment".into(), "cannot mutate immutable variable".into())
            },
            Self::CannotModifyInplace(typ) => (
                "invalid variable reassignment".into(),
                format!("cannot use inplace assignment operators on `{typ}` values").into(),
            ),
            Self::CannotMutateStringCharacters => {
                ("invalid variable reassignment".into(), "cannot mutate string characters".into())
            },
        };

        return MsgInfo { message: error_message, cause: error_cause_message };
    }
}
