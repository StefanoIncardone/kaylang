use crate::front_end::{src_file::DisplayPosition, tokenizer::{Base, TokenKind, Tokens}, ErrorDisplay, SliceIndexPtr};
use back_to_front::offset32;

use super::{
    syntax_tree::{self as st, SyntaxTree},
    src_file::SrcCode,
    tokenizer::{ascii, Op, TextIndex},
    Error, ErrorInfo, IntoErrorInfo,
};
use core::{fmt::Display, marker::PhantomData};
extern crate alloc;
use alloc::borrow::Cow;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum BaseType {
    I64,
    Ascii,
    Bool,
    Str,
}

impl BaseType {
    const I64_STR: &str = "i64";
    const ASCII_STR: &str = "ascii";
    const BOOL_STR: &str = "bool";
    const STR_STR: &str = "str";

    #[inline]
    fn matches(self, name: &str) -> bool {
        let self_str = match self {
            Self::I64 => Self::I64_STR,
            Self::Ascii => Self::ASCII_STR,
            Self::Bool => Self::BOOL_STR,
            Self::Str => Self::STR_STR,
        };
        return name == self_str;
    }
}

impl BaseType {
    #[inline(always)]
    pub(crate) const fn typ(self) -> Type {
        return Type::Base(self);
    }

    #[inline]
    pub(crate) const fn size(&self) -> usize {
        return match self {
            Self::I64 => size_of::<i64>(),
            Self::Ascii => size_of::<ascii>(),
            Self::Bool => size_of::<bool>(),
            Self::Str => size_of::<u64>() + size_of::<*const ascii>(),
        };
    }
}

impl Display for BaseType {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return match self {
            Self::I64 => f.write_str(Self::I64_STR),
            Self::Ascii => f.write_str(Self::ASCII_STR),
            Self::Bool => f.write_str(Self::BOOL_STR),
            Self::Str => f.write_str(Self::STR_STR),
        };
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
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
    pub(crate) const fn base_typ(&self) -> BaseType {
        return match self {
            Self::Base(typ) => *typ,
            Self::Array { base_type, .. } => *base_type,
        };
    }

    #[inline]
    pub(crate) const fn size(&self) -> usize {
        return match self {
            Self::Base(typ) => typ.size(),
            #[expect(clippy::cast_possible_truncation)]
            Self::Array { base_type, len } => base_type.size() * *len as usize,
        };
    }
}

impl Display for Type {
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
#[repr(u8)]
pub(crate) enum PrefixOperator {
    Len = st::PrefixOperator::Len as u8,
    Not = st::PrefixOperator::Not as u8,

    Plus           = st::PrefixOperator::Plus as u8,
    WrappingPlus   = st::PrefixOperator::WrappingPlus as u8,
    SaturatingPlus = st::PrefixOperator::SaturatingPlus as u8,

    Minus           = st::PrefixOperator::Minus as u8,
    WrappingMinus   = st::PrefixOperator::WrappingMinus as u8,
    SaturatingMinus = st::PrefixOperator::SaturatingMinus as u8,
}

impl PrefixOperator {
    const BASE_TYPE: BaseType = BaseType::I64;
    const TYPE: Type = Type::Base(Self::BASE_TYPE);
}

impl Into<PrefixOperator> for Op {
    #[inline(always)]
    fn into(self) -> PrefixOperator {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Op> for PrefixOperator {
    #[inline(always)]
    fn into(self) -> Op {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<st::PrefixOperator> for PrefixOperator {
    #[inline(always)]
    fn into(self) -> st::PrefixOperator {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<PrefixOperator> for st::PrefixOperator {
    #[inline(always)]
    fn into(self) -> PrefixOperator {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Display for PrefixOperator {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl PrefixOperator {
    #[expect(dead_code)]
    #[inline(always)]
    pub(super) fn display_len(self) -> offset32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

#[expect(dead_code)]
#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum BooleanPrefixOperator {
    Not = st::PrefixOperator::Not as u8,
}

impl BooleanPrefixOperator {
    const BASE_TYPE: BaseType = BaseType::Bool;
    const TYPE: Type = Type::Base(Self::BASE_TYPE);
}

impl Into<BooleanPrefixOperator> for Op {
    #[inline(always)]
    fn into(self) -> BooleanPrefixOperator {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Op> for BooleanPrefixOperator {
    #[inline(always)]
    fn into(self) -> Op {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<st::PrefixOperator> for BooleanPrefixOperator {
    #[inline(always)]
    fn into(self) -> st::PrefixOperator {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<BooleanPrefixOperator> for st::PrefixOperator {
    #[inline(always)]
    fn into(self) -> BooleanPrefixOperator {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Display for BooleanPrefixOperator {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl BooleanPrefixOperator {
    #[expect(dead_code)]
    #[inline(always)]
    pub(super) fn display_len(self) -> offset32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

#[expect(dead_code)]
#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum BinaryOperator {
    Pow           = st::BinaryOperator::Pow as u8,
    WrappingPow   = st::BinaryOperator::WrappingPow as u8,
    SaturatingPow = st::BinaryOperator::SaturatingPow as u8,

    Times           = st::BinaryOperator::Times as u8,
    WrappingTimes   = st::BinaryOperator::WrappingTimes as u8,
    SaturatingTimes = st::BinaryOperator::SaturatingTimes as u8,

    Divide           = st::BinaryOperator::Divide as u8,
    WrappingDivide   = st::BinaryOperator::WrappingDivide as u8,
    SaturatingDivide = st::BinaryOperator::SaturatingDivide as u8,

    Remainder = st::BinaryOperator::Remainder as u8,

    Plus           = st::BinaryOperator::Plus as u8,
    WrappingPlus   = st::BinaryOperator::WrappingPlus as u8,
    SaturatingPlus = st::BinaryOperator::SaturatingPlus as u8,

    Minus           = st::BinaryOperator::Minus as u8,
    WrappingMinus   = st::BinaryOperator::WrappingMinus as u8,
    SaturatingMinus = st::BinaryOperator::SaturatingMinus as u8,

    LeftShift           = st::BinaryOperator::LeftShift as u8,
    WrappingLeftShift   = st::BinaryOperator::WrappingLeftShift as u8,
    SaturatingLeftShift = st::BinaryOperator::SaturatingLeftShift as u8,

    RightShift = st::BinaryOperator::RightShift as u8,

    LeftRotate  = st::BinaryOperator::LeftRotate as u8,
    RightRotate = st::BinaryOperator::RightRotate as u8,

    BitAnd = st::BinaryOperator::BitAnd as u8,
    BitXor = st::BinaryOperator::BitXor as u8,
    BitOr  = st::BinaryOperator::BitOr as u8,
}

impl BinaryOperator {
    const BASE_TYPE: BaseType = BaseType::I64;
    const TYPE: Type = Type::Base(Self::BASE_TYPE);
}

impl Into<BinaryOperator> for Op {
    #[inline(always)]
    fn into(self) -> BinaryOperator {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Op> for BinaryOperator {
    #[inline(always)]
    fn into(self) -> Op {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<st::BinaryOperator> for BinaryOperator {
    #[inline(always)]
    fn into(self) -> st::BinaryOperator {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<BinaryOperator> for st::BinaryOperator {
    #[inline(always)]
    fn into(self) -> BinaryOperator {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Display for BinaryOperator {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl BinaryOperator {
    #[expect(dead_code)]
    #[inline(always)]
    pub(super) fn display_len(self) -> offset32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

#[expect(dead_code)]
#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum BooleanBinaryOperator {
    And = st::BinaryOperator::And as u8,
    Or  = st::BinaryOperator::Or as u8,
}

impl BooleanBinaryOperator {
    const BASE_TYPE: BaseType = BaseType::Bool;
    const TYPE: Type = Type::Base(Self::BASE_TYPE);
}

impl Into<BooleanBinaryOperator> for Op {
    #[inline(always)]
    fn into(self) -> BooleanBinaryOperator {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Op> for BooleanBinaryOperator {
    #[inline(always)]
    fn into(self) -> Op {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<st::BinaryOperator> for BooleanBinaryOperator {
    #[inline(always)]
    fn into(self) -> st::BinaryOperator {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<BooleanBinaryOperator> for st::BinaryOperator {
    #[inline(always)]
    fn into(self) -> BooleanBinaryOperator {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Display for BooleanBinaryOperator {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl BooleanBinaryOperator {
    #[expect(dead_code)]
    #[inline(always)]
    pub(super) fn display_len(self) -> offset32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

#[expect(dead_code)]
#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum ComparisonOperator {
    Compare = st::BinaryOperator::Compare as u8,
}

impl ComparisonOperator {
    const BASE_TYPE: BaseType = BaseType::I64;
    const TYPE: Type = Type::Base(Self::BASE_TYPE);
}

impl Into<ComparisonOperator> for Op {
    #[inline(always)]
    fn into(self) -> ComparisonOperator {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Op> for ComparisonOperator {
    #[inline(always)]
    fn into(self) -> Op {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<st::BinaryOperator> for ComparisonOperator {
    #[inline(always)]
    fn into(self) -> st::BinaryOperator {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<ComparisonOperator> for st::BinaryOperator {
    #[inline(always)]
    fn into(self) -> ComparisonOperator {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Display for ComparisonOperator {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl ComparisonOperator {
    #[expect(dead_code)]
    #[inline(always)]
    pub(super) fn display_len(self) -> offset32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

#[expect(dead_code)]
#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum BooleanComparisonOperator {
    EqualsEquals    = st::BinaryOperator::EqualsEquals as u8,
    NotEquals       = st::BinaryOperator::NotEquals as u8,

    Greater         = st::BinaryOperator::Greater as u8,
    GreaterOrEquals = st::BinaryOperator::GreaterOrEquals as u8,

    Less            = st::BinaryOperator::Less as u8,
    LessOrEquals    = st::BinaryOperator::LessOrEquals as u8,
}

impl BooleanComparisonOperator {
    const BASE_TYPE: BaseType = BaseType::Bool;
    const TYPE: Type = Type::Base(Self::BASE_TYPE);
}

impl Into<BooleanComparisonOperator> for Op {
    #[inline(always)]
    fn into(self) -> BooleanComparisonOperator {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Op> for BooleanComparisonOperator {
    #[inline(always)]
    fn into(self) -> Op {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<st::BinaryOperator> for BooleanComparisonOperator {
    #[inline(always)]
    fn into(self) -> st::BinaryOperator {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<BooleanComparisonOperator> for st::BinaryOperator {
    #[inline(always)]
    fn into(self) -> BooleanComparisonOperator {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Display for BooleanComparisonOperator {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl BooleanComparisonOperator {
    #[expect(dead_code)]
    #[inline(always)]
    pub(super) fn display_len(self) -> offset32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

#[expect(dead_code)]
#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum AssignmentOperator {
    Pow           = st::AssignmentOperator::Pow as u8,
    WrappingPow   = st::AssignmentOperator::WrappingPow as u8,
    SaturatingPow = st::AssignmentOperator::SaturatingPow as u8,

    Times           = st::AssignmentOperator::Times as u8,
    WrappingTimes   = st::AssignmentOperator::WrappingTimes as u8,
    SaturatingTimes = st::AssignmentOperator::SaturatingTimes as u8,

    Divide           = st::AssignmentOperator::Divide as u8,
    WrappingDivide   = st::AssignmentOperator::WrappingDivide as u8,
    SaturatingDivide = st::AssignmentOperator::SaturatingDivide as u8,

    Remainder = st::AssignmentOperator::Remainder as u8,

    Plus           = st::AssignmentOperator::Plus as u8,
    WrappingPlus   = st::AssignmentOperator::WrappingPlus as u8,
    SaturatingPlus = st::AssignmentOperator::SaturatingPlus as u8,

    Minus           = st::AssignmentOperator::Minus as u8,
    WrappingMinus   = st::AssignmentOperator::WrappingMinus as u8,
    SaturatingMinus = st::AssignmentOperator::SaturatingMinus as u8,

    LeftShift           = st::AssignmentOperator::LeftShift as u8,
    WrappingLeftShift   = st::AssignmentOperator::WrappingLeftShift as u8,
    SaturatingLeftShift = st::AssignmentOperator::SaturatingLeftShift as u8,

    RightShift = st::AssignmentOperator::RightShift as u8,

    LeftRotate  = st::AssignmentOperator::LeftRotate as u8,
    RightRotate = st::AssignmentOperator::RightRotate as u8,

    BitAnd = st::AssignmentOperator::BitAnd as u8,
    BitXor = st::AssignmentOperator::BitXor as u8,
    BitOr  = st::AssignmentOperator::BitOr as u8,
}

impl AssignmentOperator {
    const BASE_TYPE: BaseType = BaseType::I64;
    const TYPE: Type = Type::Base(Self::BASE_TYPE);
}

impl Into<AssignmentOperator> for Op {
    #[inline(always)]
    fn into(self) -> AssignmentOperator {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Op> for AssignmentOperator {
    #[inline(always)]
    fn into(self) -> Op {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<st::AssignmentOperator> for AssignmentOperator {
    #[inline(always)]
    fn into(self) -> st::AssignmentOperator {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<AssignmentOperator> for st::AssignmentOperator {
    #[inline(always)]
    fn into(self) -> AssignmentOperator {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Display for AssignmentOperator {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl AssignmentOperator {
    #[expect(dead_code)]
    #[inline(always)]
    pub(super) fn display_len(self) -> offset32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

#[expect(dead_code)]
#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum BooleanAssignmentOperator {
    And    = st::AssignmentOperator::And as u8,
    Or     = st::AssignmentOperator::Or as u8,
}

impl BooleanAssignmentOperator {
    const BASE_TYPE: BaseType = BaseType::Bool;
    const TYPE: Type = Type::Base(Self::BASE_TYPE);
}

impl Into<BooleanAssignmentOperator> for Op {
    #[inline(always)]
    fn into(self) -> BooleanAssignmentOperator {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Op> for BooleanAssignmentOperator {
    #[inline(always)]
    fn into(self) -> Op {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<st::AssignmentOperator> for BooleanAssignmentOperator {
    #[inline(always)]
    fn into(self) -> st::AssignmentOperator {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<BooleanAssignmentOperator> for st::AssignmentOperator {
    #[inline(always)]
    fn into(self) -> BooleanAssignmentOperator {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Display for BooleanAssignmentOperator {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl BooleanAssignmentOperator {
    #[expect(dead_code)]
    #[inline(always)]
    pub(super) fn display_len(self) -> offset32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

pub(crate) type ExpressionIndex<'code> = SliceIndexPtr<Expression<'code>>;
pub(crate) type ArrayItemsIndex<'code> = SliceIndexPtr<Expression<'code>>;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) enum Expression<'code> {
    False {
        column: offset32,
    },
    True {
        column: offset32,
    },
    I64 {
        value: i64,
        column: offset32,
    },
    Ascii {
        character: ascii,
        column: offset32,
    },
    Str {
        literal: TextIndex<'code>,
        column: offset32,
    },
    Variable {
        variable: VariableDefinitionIndex<'code>,
        column: offset32,
    },
    Array {
        base_type: BaseType,
        items_start: ArrayItemsIndex<'code>,
        /// always greater than 0
        items_len: u64,
    },

    Prefix {
        operator: PrefixOperator,
        operator_column: offset32,
        right_operand: ExpressionIndex<'code>,
    },
    BooleanPrefix {
        operator: BooleanPrefixOperator,
        operator_column: offset32,
        right_operand: ExpressionIndex<'code>,
    },
    Binary {
        left_operand: ExpressionIndex<'code>,
        operator: BinaryOperator,
        operator_column: offset32,
        right_operand: ExpressionIndex<'code>,
    },
    BooleanBinary {
        left_operand: ExpressionIndex<'code>,
        operator: BooleanBinaryOperator,
        operator_column: offset32,
        right_operand: ExpressionIndex<'code>,
    },
    Comparison {
        left_operand: ExpressionIndex<'code>,
        operator: ComparisonOperator,
        operator_column: offset32,
        right_operand: ExpressionIndex<'code>,
    },
    BooleanComparison {
        left_operand: ExpressionIndex<'code>,
        operator: BooleanComparisonOperator,
        operator_column: offset32,
        right_operand: ExpressionIndex<'code>,
    },

    Index {
        indexed_expression: ExpressionIndex<'code>,
        open_square_bracket_column: offset32,
        index_expression: ExpressionIndex<'code>,
    },
}

impl Expression<'_> {
    pub(crate) fn base_typ(&self, ast: &TypedSyntaxTree<'_, '_, '_>) -> BaseType {
        return match self {
            Self::False { .. } | Self::True { .. } => BaseType::Bool,
            Self::I64 { .. } => BaseType::I64,
            Self::Ascii { .. } => BaseType::Ascii,
            Self::Str { .. } => BaseType::Str,
            Self::Variable { variable, .. } => {
                let variable_definition = &ast.variables[*variable];
                variable_definition.typ.base_typ()
            }
            Self::Array { base_type, .. } => *base_type,
            Self::Prefix { .. } => PrefixOperator::BASE_TYPE,
            Self::BooleanPrefix { .. } => BooleanPrefixOperator::BASE_TYPE,
            Self::Binary { .. } => BinaryOperator::BASE_TYPE,
            Self::BooleanBinary { .. } => BooleanBinaryOperator::BASE_TYPE,
            Self::Comparison { .. } => ComparisonOperator::BASE_TYPE,
            Self::BooleanComparison { .. } => BooleanComparisonOperator::BASE_TYPE,
            Self::Index { indexed_expression, .. } => {
                let expression = &ast.expressions[*indexed_expression];
                expression.base_typ(ast)
            }
        };
    }

    pub(crate) fn typ(&self, ast: &TypedSyntaxTree<'_, '_, '_>) -> Type {
        return match self {
            Self::False { .. } | Self::True { .. } => Type::Base(BaseType::Bool),
            Self::I64 { .. } => Type::Base(BaseType::I64),
            Self::Ascii { .. } => Type::Base(BaseType::Ascii),
            Self::Str { .. } => Type::Base(BaseType::Str),
            Self::Variable { variable, .. } => {
                let variable_definition = &ast.variables[*variable];
                variable_definition.typ.clone()
            }
            Self::Array { base_type, items_len, .. } => {
                Type::Array { base_type: *base_type, len: *items_len }
            }
            Self::Prefix { .. } => PrefixOperator::TYPE,
            Self::BooleanPrefix { .. } => BooleanPrefixOperator::TYPE,
            Self::Binary { .. } => BinaryOperator::TYPE,
            Self::BooleanBinary { .. } => BooleanBinaryOperator::TYPE,
            Self::Comparison { .. } => ComparisonOperator::TYPE,
            Self::BooleanComparison { .. } => BooleanComparisonOperator::TYPE,
            Self::Index { indexed_expression, .. } => {
                let expression = &ast.expressions[*indexed_expression];
                expression.typ(ast)
            }
        };
    }
}

pub(crate) type ScopeIndex<'code> = SliceIndexPtr<Scope<'code>>;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) struct Scope<'code> {
    pub(crate) parent: ScopeIndex<'code>,
    pub(crate) types: Vec<BaseType>,
    pub(crate) let_variables: Vec<VariableDefinitionIndex<'code>>,
    pub(crate) var_variables: Vec<VariableDefinitionIndex<'code>>,
}

pub(crate) type VariableDefinitionIndex<'code> = SliceIndexPtr<VariableDefinition<'code>>;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) struct VariableDefinition<'code> {
    pub(crate) name: TextIndex<'code>,
    pub(crate) typ: Type,
    pub(crate) initial_value: ExpressionIndex<'code>,
}

pub(crate) type NodeIndex<'code> = SliceIndexPtr<Node<'code>>;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) enum Node<'code> {
    Expression(ExpressionIndex<'code>),

    Print {
        argument: ExpressionIndex<'code>,
    },
    Println {
        argument: ExpressionIndex<'code>,
    },
    PrintlnNoArg,
    Eprint {
        argument: ExpressionIndex<'code>,
    },
    Eprintln {
        argument: ExpressionIndex<'code>,
    },
    EprintlnNoArg,

    LetVariableDefinition {
        variable: VariableDefinitionIndex<'code>,
    },
    VarVariableDefinition {
        variable: VariableDefinitionIndex<'code>,
    },
    Assignment {
        target: ExpressionIndex<'code>,
        new_value: ExpressionIndex<'code>,
    },
    AssignmentExpression {
        target: ExpressionIndex<'code>,
        operator: AssignmentOperator,
        operator_column: offset32,
        new_value: ExpressionIndex<'code>,
    },
    BooleanAssignmentExpression {
        target: ExpressionIndex<'code>,
        operator: BooleanAssignmentOperator,
        operator_column: offset32,
        new_value: ExpressionIndex<'code>,
    },

    Scope {
        raw_nodes_in_scope_count: u32,
    },

    If {
        condition: ExpressionIndex<'code>,
    },
    ElseIf {
        condition: ExpressionIndex<'code>,
    },
    Else,

    Loop {
        condition: ExpressionIndex<'code>,
    },
    DoLoop {
        condition: ExpressionIndex<'code>,
    },
    Break,
    Continue,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum ParsedNode<'code> {
    Node(Node<'code>),
    ScopeEnd,
}

pub(crate) type ArrayItem<'code> = ExpressionIndex<'code>;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TypedSyntaxTree<'syntax_tree, 'tokens: 'syntax_tree, 'code: 'tokens> {
    pub(crate) nodes: Vec<Node<'code>>,

    pub(crate) expressions: Vec<Expression<'code>>,
    pub(crate) array_items: Vec<ArrayItem<'code>>,
    pub(crate) variables: Vec<VariableDefinition<'code>>,

    _syntax_tree: PhantomData<&'syntax_tree SyntaxTree<'tokens, 'code>>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TypedSyntaxTreeDisplay<'typed_syntax_tree, 'syntax_tree: 'typed_syntax_tree, 'tokens: 'syntax_tree, 'code: 'tokens> {
    pub(crate) typed_syntax_tree: &'typed_syntax_tree TypedSyntaxTree<'syntax_tree, 'tokens, 'code>,
    pub(crate) syntax_tree: &'syntax_tree SyntaxTree<'tokens, 'code>,
    pub(crate) tokens: &'tokens Tokens<'code>,
}

impl<'syntax_tree, 'tokens: 'syntax_tree, 'code: 'tokens> TypedSyntaxTree<'syntax_tree, 'tokens, 'code> {
    #[must_use]
    #[inline(always)]
    pub const fn display(
        &self,
        syntax_tree: &'syntax_tree SyntaxTree<'tokens, 'code>,
        tokens: &'tokens Tokens<'code>,
    ) -> TypedSyntaxTreeDisplay<'_, 'syntax_tree, 'tokens, 'code> {
        return TypedSyntaxTreeDisplay { typed_syntax_tree: self, syntax_tree, tokens };
    }
}

impl TypedSyntaxTreeDisplay<'_, '_, '_, '_> {
    const INDENT_INCREMENT: usize = 2;

    fn info_if(
        &self,
        f: &mut core::fmt::Formatter<'_>,
        node_index: &mut NodeIndex<'_>,
        indent: usize,
        condition: ExpressionIndex<'_>,
    ) -> core::fmt::Result {
        writeln!(f, "{:>indent$}If = if", "")?;
        let if_indent = indent + Self::INDENT_INCREMENT;
        self.info_expression(f, condition, if_indent)?;
        return self.info_node(f, node_index, if_indent);
    }

    fn info_node(
        &self,
        f: &mut core::fmt::Formatter<'_>,
        node_index: &mut NodeIndex<'_>,
        indent: usize,
    ) -> core::fmt::Result {
        let node = &self.typed_syntax_tree.nodes[*node_index];
        node_index.0 += 1;

        #[rustfmt::skip]
        return match node {
            Node::Expression(expression) => {
                self.info_expression(f, *expression, indent)
            }
            Node::Assignment { target, new_value, .. } => {
                writeln!(f, "{:>indent$}Assignment", "")?;
                let assignment_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *target, assignment_indent)?;
                writeln!(f, "{:>assignment_indent$}Equals = =", "")?;
                self.info_expression(f, *new_value, assignment_indent)
            }
            Node::AssignmentExpression { target, operator, new_value, .. } => {
                writeln!(f, "{:>indent$}AssignmentExpression", "")?;
                let assignment_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *target, assignment_indent)?;
                writeln!(f, "{:>assignment_indent$}AssignmentOp = {operator}", "")?;
                self.info_expression(f, *new_value, assignment_indent)
            }
            Node::BooleanAssignmentExpression { target, operator, new_value, .. } => {
                writeln!(f, "{:>indent$}BooleanAssignmentExpression", "")?;
                let assignment_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *target, assignment_indent)?;
                writeln!(f, "{:>assignment_indent$}BooleanAssignmentOp = {operator}", "")?;
                self.info_expression(f, *new_value, assignment_indent)
            }

            Node::Print { argument } => {
                writeln!(f, "{:>indent$}Print = print", "")?;
                let argument_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *argument, argument_indent)
            }
            Node::Println { argument } => {
                writeln!(f, "{:>indent$}Println = println", "")?;
                let argument_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *argument, argument_indent)
            }
            Node::PrintlnNoArg => {
                writeln!(f, "{:>indent$}Println = println", "")
            }
            Node::Eprint { argument } => {
                writeln!(f, "{:>indent$}Eprint = eprint", "")?;
                let argument_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *argument, argument_indent)
            }
            Node::Eprintln { argument } => {
                writeln!(f, "{:>indent$}Eprintln = eprintln", "")?;
                let argument_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *argument, argument_indent)
            }
            Node::EprintlnNoArg => {
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

            Node::If { condition } => {
                self.info_if(f, node_index, indent, *condition)
            }
            Node::ElseIf { condition } => {
                writeln!(f, "{:>indent$}Else = else", "")?;
                self.info_if(f, node_index, indent, *condition)
            }
            Node::Else => {
                writeln!(f, "{:>indent$}Else = else", "")?;
                let else_indent = indent + Self::INDENT_INCREMENT;
                self.info_node(f, node_index, else_indent)
            }

            Node::Loop { condition } => {
                writeln!(f, "{:>indent$}Loop = loop", "")?;
                let loop_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *condition, loop_indent)?;
                self.info_node(f, node_index, loop_indent)
            }
            Node::DoLoop { condition } => {
                writeln!(f, "{:>indent$}Do = do", "")?;
                writeln!(f, "{:>indent$}Loop = loop", "")?;
                let loop_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *condition, loop_indent)?;
                self.info_node(f, node_index, loop_indent)
            }
            Node::Break => {
                writeln!(f, "{:>indent$}Break = break", "")
            }
            Node::Continue => {
                writeln!(f, "{:>indent$}Continue = continue", "")
            }
        };
    }

    fn info_expression(
        &self,
        f: &mut core::fmt::Formatter<'_>,
        expression_index: ExpressionIndex<'_>,
        indent: usize,
    ) -> core::fmt::Result {
        let expression_indent = indent + Self::INDENT_INCREMENT;
        let expression = &self.typed_syntax_tree.expressions[expression_index];

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
            Expression::Str { literal, .. } => {
                let literal_str = self.tokens.text[*literal];
                writeln!(f, "{:>indent$}Str = {literal_str}", "")
            }
            Expression::Variable { variable, .. } => {
                let variable_definition = &self.typed_syntax_tree.variables[*variable];
                let identifier_str = self.tokens.text[variable_definition.name];
                writeln!(f, "{:>indent$}Identifier = {identifier_str}", "")
            },
            Expression::Array { items_start, items_len, .. } => {
                writeln!(f, "{:>indent$}Array", "")?;

                let items_indent = expression_indent + Self::INDENT_INCREMENT;
                #[expect(clippy::cast_possible_truncation)]
                let items_end = items_start.0 as usize + *items_len as usize;
                let items = &self.typed_syntax_tree.array_items[items_start.0 as usize..items_end];
                for item_expression in items {
                    self.info_expression(f, *item_expression, items_indent)?;
                }
                Ok(())
            },

            Expression::Prefix { operator, right_operand, .. } => {
                writeln!(f, "{:>indent$}PrefixExpression", "")?;
                writeln!(f, "{:>expression_indent$}PrefixOperator = {operator}", "")?;
                self.info_expression(f, *right_operand, expression_indent)
            }
            Expression::BooleanPrefix { operator, right_operand, .. } => {
                writeln!(f, "{:>indent$}BooleanPrefixExpression", "")?;
                writeln!(f, "{:>expression_indent$}BooleanPrefixOperator = {operator}", "")?;
                self.info_expression(f, *right_operand, expression_indent)
            }
            Expression::Binary { left_operand, operator, right_operand, .. } => {
                writeln!(f, "{:>indent$}BinaryExpression", "")?;
                self.info_expression(f, *left_operand, expression_indent)?;
                writeln!(f, "{:>expression_indent$}BinaryOperator = {operator}", "")?;
                self.info_expression(f, *right_operand, expression_indent)
            },
            Expression::BooleanBinary { left_operand, operator, right_operand, .. } => {
                writeln!(f, "{:>indent$}BooleanBinaryExpression", "")?;
                self.info_expression(f, *left_operand, expression_indent)?;
                writeln!(f, "{:>expression_indent$}BooleanBinaryOperator = {operator}", "")?;
                self.info_expression(f, *right_operand, expression_indent)
            },
            Expression::Comparison { left_operand, operator, right_operand, .. } => {
                writeln!(f, "{:>indent$}Comparison", "")?;
                self.info_expression(f, *left_operand, expression_indent)?;
                writeln!(f, "{:>expression_indent$}ComparisonOperator = {operator}", "")?;
                self.info_expression(f, *right_operand, expression_indent)
            },
            Expression::BooleanComparison { left_operand, operator, right_operand, .. } => {
                writeln!(f, "{:>indent$}BooleanComparison", "")?;
                self.info_expression(f, *left_operand, expression_indent)?;
                writeln!(f, "{:>expression_indent$}BooleanComparisonOperator = {operator}", "")?;
                self.info_expression(f, *right_operand, expression_indent)
            },

            Expression::Index {
                indexed_expression,
                index_expression,
                ..
            } => {
                writeln!(f, "{:indent$}IndexExpression", "")?;
                self.info_expression(f, *indexed_expression, expression_indent)?;
                self.info_expression(f, *index_expression, expression_indent)
            },
        };
    }

    fn info_variable(
        &self,
        f: &mut core::fmt::Formatter<'_>,
        variable_index: VariableDefinitionIndex<'_>,
        indent: usize,
    ) -> core::fmt::Result {
        let VariableDefinition { name, initial_value, typ } =
            &self.typed_syntax_tree.variables[variable_index];
        let name_str = self.tokens.text[*name];
        writeln!(f, "{:>indent$}Name = {name_str}", "")?;
        writeln!(f, "{:>indent$}Type = {typ}", "")?;
        writeln!(f, "{:>indent$}InitialValue", "")?;

        let initial_value_indent = indent + Self::INDENT_INCREMENT;
        return self.info_expression(f, *initial_value, initial_value_indent);
    }
}

impl Display for TypedSyntaxTreeDisplay<'_, '_, '_, '_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let mut node_index = NodeIndex::new(0);
        while (node_index.0 as usize) < self.typed_syntax_tree.nodes.len() {
            self.info_node(f, &mut node_index, 0)?;
        }

        return Ok(());
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Parser<'syntax_tree, 'tokens: 'syntax_tree, 'src: 'tokens, 'code: 'src, 'path: 'code> {
    src: &'src SrcCode<'code, 'path>,
    errors: Vec<Error<ErrorKind>>,

    tokens: &'tokens Tokens<'code>,
    node_index: st::NodeIndex<'code>,
    syntax_tree: &'syntax_tree SyntaxTree<'tokens, 'code>,

    temp_array_items: Vec<ArrayItem<'code>>,
    ast: TypedSyntaxTree<'syntax_tree, 'tokens, 'code>,
    scope: ScopeIndex<'code>,
    scopes: Vec<Scope<'code>>,
}

impl<'syntax_tree, 'tokens: 'syntax_tree, 'src: 'tokens, 'code: 'src, 'path: 'code>
    Parser<'syntax_tree, 'tokens, 'src, 'code, 'path>
{
    /* NOTE(stefano):
    only parsing until the first error until a fault tolerant parser is developed,
    this is because the first truly relevant error is the first one, which in turn causes a ripple
    effect that propagates to the rest of the parsing, causing subsequent errors to be wrong
    */
    #[expect(clippy::missing_errors_doc)]
    pub fn parse(
        src: &'src SrcCode<'code, 'path>,
        tokens: &'tokens Tokens<'code>,
        syntax_tree: &'syntax_tree SyntaxTree<'tokens, 'code>,
    ) -> Result<TypedSyntaxTree<'syntax_tree, 'tokens, 'code>, Vec<Error<ErrorKind>>> {
        let mut parser = Self {
            src,
            errors: Vec::new(),

            tokens,
            node_index: st::NodeIndex::new(0),
            syntax_tree,

            temp_array_items: Vec::new(),
            ast: TypedSyntaxTree {
                nodes: Vec::new(),
                expressions: Vec::new(),
                array_items: Vec::new(),
                variables: Vec::new(),
                _syntax_tree: PhantomData,
            },
            scope: ScopeIndex::new(0),
            scopes: vec![Scope {
                parent: ScopeIndex::new(0),
                types: vec![
                    BaseType::I64,
                    BaseType::Ascii,
                    BaseType::Bool,
                    BaseType::Str,
                ],
                let_variables: Vec::new(),
                var_variables: Vec::new(),
            }],
        };

        while let Some(peeked) = parser.peek_next_node() {
            parser.node_index = peeked.index;
            match parser.any(peeked.node) {
                Ok(ParsedNode::Node(node)) => parser.ast.nodes.push(node),
                Ok(ParsedNode::ScopeEnd) => continue,
                Err(err) => {
                    parser.errors.push(err);

                    // consuming all remaining nodes until the end of the file
                    parser.node_index = st::NodeIndex::new(parser.syntax_tree.nodes.len());
                    break;
                }
            };
        }

        return if parser.errors.is_empty() { Ok(parser.ast) } else { Err(parser.errors) };
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
struct Peeked<'syntax_tree, 'code: 'syntax_tree> {
    node: &'syntax_tree st::Node<'code>,
    index: st::NodeIndex<'code>,
}

impl<'syntax_tree, 'code: 'syntax_tree> Parser<'syntax_tree, '_, '_, 'code, '_> {
    fn peek_next_node(&self) -> Option<Peeked<'syntax_tree, 'code>> {
        let node_index_end = st::NodeIndex::new(self.syntax_tree.nodes.len());
        for next_node_index in self.node_index.0..node_index_end.0 {
            let next_node_index_index = st::NodeIndex::new_offset32(next_node_index);
            let next_node = &self.syntax_tree.nodes[next_node_index_index];
            let st::Node::Semicolon { .. } = next_node else {
                let peeked_node_index_index = st::NodeIndex::new_offset32(next_node_index_index.0 + 1);
                return Some(Peeked { node: next_node, index: peeked_node_index_index });
            };
        }

        return None;
    }
}

impl<'code> Parser<'_, '_, '_, 'code, '_> {
    fn any(&mut self, node: &st::Node<'code>) -> Result<ParsedNode<'code>, Error<ErrorKind>> {
        return match node {
            st::Node::Expression { expression, .. } => {
                let parsed_expression_index = self.parse_expression(*expression, None)?;
                Ok(ParsedNode::Node(Node::Expression(parsed_expression_index)))
            }
            st::Node::Assignment { target, operator, operator_column, new_value, .. } => {
                let parsed_assignment = self.assignment(
                    *target,
                    *operator,
                    *operator_column,
                    *new_value
                )?;
                Ok(ParsedNode::Node(parsed_assignment))
            }

            st::Node::Print { argument, .. } => {
                let parsed_argument_index = self.parse_expression(*argument, None)?;
                Ok(ParsedNode::Node(Node::Print { argument: parsed_argument_index }))
            }
            st::Node::Println { argument, .. } => {
                let parsed_argument_index = self.parse_expression(*argument, None)?;
                Ok(ParsedNode::Node(Node::Println { argument: parsed_argument_index }))
            }
            st::Node::PrintlnNoArg { .. } => {
                Ok(ParsedNode::Node(Node::PrintlnNoArg))
            }
            st::Node::Eprint { argument, .. } => {
                let parsed_argument_index = self.parse_expression(*argument, None)?;
                Ok(ParsedNode::Node(Node::Eprint { argument: parsed_argument_index }))
            }
            st::Node::Eprintln { argument, .. } => {
                let parsed_argument_index = self.parse_expression(*argument, None)?;
                Ok(ParsedNode::Node(Node::Eprintln { argument: parsed_argument_index }))
            }
            st::Node::EprintlnNoArg { .. } => {
                Ok(ParsedNode::Node(Node::EprintlnNoArg))
            }

            st::Node::LetVariableDefinition { variable_definition, .. } => {
                let variable = self.parse_variable(*variable_definition)?;
                self.scopes[self.scope].let_variables.push(variable);
                Ok(ParsedNode::Node(Node::LetVariableDefinition { variable }))
            }
            st::Node::VarVariableDefinition { variable_definition, .. } => {
                let variable = self.parse_variable(*variable_definition)?;
                self.scopes[self.scope].var_variables.push(variable);
                Ok(ParsedNode::Node(Node::VarVariableDefinition { variable }))
            }

            st::Node::Scope { open_curly_bracket_column, raw_nodes_in_scope_count, .. } => {
                let current_scope_index = self.scope;
                self.scope = ScopeIndex::new(self.scopes.len());
                self.scopes.push(Scope {
                    parent: current_scope_index,
                    types: Vec::new(),
                    let_variables: Vec::new(),
                    var_variables: Vec::new(),
                });

                let placeholder_scope = Node::Scope { raw_nodes_in_scope_count: 0 };
                let placeholder_scope_node_index = NodeIndex::new(self.ast.nodes.len());
                self.ast.nodes.push(placeholder_scope);

                let raw_nodes_in_scope_end = self.node_index.0 + *raw_nodes_in_scope_count;
                while self.node_index.0 < raw_nodes_in_scope_end {
                    let Some(peeked) = self.peek_next_node() else {
                        unreachable!();
                    };
                    self.node_index = peeked.index;
                    match self.any(peeked.node)? {
                        ParsedNode::Node(inner_node) => self.ast.nodes.push(inner_node),
                        ParsedNode::ScopeEnd => continue,
                    };
                }

                let last_scope_node_index = NodeIndex::new(self.ast.nodes.len() - 1);
                let Node::Scope {
                    raw_nodes_in_scope_count: placeholder_raw_nodes_in_scope_count,
                } = &mut self.ast.nodes[placeholder_scope_node_index]
                else {
                    self.invalid_scope_index(*open_curly_bracket_column);
                };

                *placeholder_raw_nodes_in_scope_count = last_scope_node_index.0 - placeholder_scope_node_index.0;

                self.scope = current_scope_index;
                Ok(ParsedNode::ScopeEnd)
            }

            st::Node::If { if_column, condition } => {
                let parsed_condition = self.if_condition(*if_column, *condition)?;
                let parsed_condition_index = self.ast.new_expression(parsed_condition);
                self.ast.nodes.push(Node::If { condition: parsed_condition_index });
                self.scope()
            }
            st::Node::ElseIf { if_column, condition, .. } => {
                let parsed_condition = self.if_condition(*if_column, *condition)?;
                let parsed_condition_index = self.ast.new_expression(parsed_condition);
                self.ast.nodes.push(Node::ElseIf { condition: parsed_condition_index });
                self.scope()
            }
            st::Node::Else { .. } => {
                self.ast.nodes.push(Node::Else);
                self.scope()
            }
            st::Node::Loop { loop_column, condition } => {
                let parsed_condition = self.loop_condition(*loop_column, *condition)?;
                let parsed_condition_index = self.ast.new_expression(parsed_condition);
                self.ast.nodes.push(Node::Loop { condition: parsed_condition_index });
                self.scope()
            }
            st::Node::DoLoop { loop_column, condition, .. } => {
                let parsed_condition = self.loop_condition(*loop_column, *condition)?;
                let parsed_condition_index = self.ast.new_expression(parsed_condition);
                self.ast.nodes.push(Node::DoLoop { condition: parsed_condition_index });
                self.scope()
            },
            st::Node::Break { .. } => Ok(ParsedNode::Node(Node::Break)),
            st::Node::Continue { .. } => Ok(ParsedNode::Node(Node::Continue)),

            st::Node::Semicolon { column } => self.stray_semicolon(*column),
        };
    }

    fn scope(&mut self) -> Result<ParsedNode<'code>, Error<ErrorKind>> {
        let Some(peeked) = self.peek_next_node() else{
            unreachable!();
        };
        self.node_index = peeked.index;
        let ParsedNode::ScopeEnd = self.any(peeked.node)? else {
            unreachable!();
        };
        return Ok(ParsedNode::ScopeEnd);
    }
}

impl<'code> Parser<'_, '_, '_, 'code, '_> {
    #[expect(clippy::panic)]
    #[track_caller]
    fn invalid_node(
        &self,
        absolute_column: offset32,
        pointers_count: offset32,
        error_message: Cow<'static, str>,
        error_cause_message: Cow<'static, str>,
    ) -> ! {
        let DisplayPosition { line, column, display_column } = self.src.display_position(absolute_column);
        let line_span = self.src.lines[line as usize - 1];
        let line_text = &self.src.code()[line_span.start as usize..line_span.end as usize];

        let error = ErrorDisplay {
            error_message,
            file: self.src.path(),
            line,
            column,
            absolute_column,
            line_text,
            pointers_count,
            pointers_offset: display_column,
            error_cause_message,
        };
        panic!("{error}\n");
    }

    #[track_caller]
    fn stray_semicolon(
        &self,
        semicolon_colon: offset32,
    ) -> ! {
        self.invalid_node(
            semicolon_colon,
            1,
            "unexpected".into(),
            "should have been skipped in the iteration of nodes".into(),
        );
    }

    #[track_caller]
    fn invalid_scope_index(&self, open_curly_bracket_column: offset32) -> ! {
        self.invalid_node(
            open_curly_bracket_column,
            1,
            "invalid scope index".into(),
            "should have been caught during syntax tree parsing".into(),
        );
    }
}

impl<'code> TypedSyntaxTree<'_, '_, 'code> {
    #[inline]
    fn new_expression(&mut self, expression: Expression<'code>) -> ExpressionIndex<'code> {
        let index = ExpressionIndex::new(self.expressions.len());
        self.expressions.push(expression);
        return index;
    }
}

impl<'code> Parser<'_, '_, '_, 'code, '_> {
    #[expect(clippy::single_call_fn)]
    const fn parse_positive_binary_i64(literal_str: &'code str) -> Result<i64, ()> {
        const BASE: Base = Base::Binary;
        let mut integer: i64 = 0;
        let mut digit_index = 1 + 1; // 1: leading zero, + 1: base prefix

        let literal = literal_str.as_bytes();
        while digit_index < literal.len() {
            let ascii_digit = literal[digit_index];
            digit_index += 1;
            if ascii_digit == b'_' {
                continue;
            }

            let digit = ascii_digit - b'0';
            debug_assert!(digit < BASE as u8, "invalid binary digit");

            integer = match integer.checked_mul(BASE as i64) {
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

    #[expect(clippy::single_call_fn)]
    const fn parse_positive_octal_i64(literal_str: &'code str) -> Result<i64, ()> {
        const BASE: Base = Base::Octal;
        let mut integer: i64 = 0;
        let mut digit_index = 1 + 1; // 1: leading zero, + 1: base prefix

        let literal = literal_str.as_bytes();
        while digit_index < literal.len() {
            let ascii_digit = literal[digit_index];
            digit_index += 1;
            if ascii_digit == b'_' {
                continue;
            }

            let digit = ascii_digit - b'0';
            debug_assert!(digit < BASE as u8, "invalid octal digit");

            integer = match integer.checked_mul(BASE as i64) {
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

    #[expect(clippy::single_call_fn)]
    const fn parse_positive_decimal_i64(literal_str: &'code str) -> Result<i64, ()> {
        const BASE: Base = Base::Decimal;
        let mut integer: i64 = 0;
        let mut digit_index = 0;

        let literal = literal_str.as_bytes();
        while digit_index < literal.len() {
            let ascii_digit = literal[digit_index];
            digit_index += 1;
            if ascii_digit == b'_' {
                continue;
            }

            let digit = ascii_digit - b'0';
            debug_assert!(digit < BASE as u8, "invalid decimal digit");

            integer = match integer.checked_mul(BASE as i64) {
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

    #[expect(clippy::single_call_fn)]
    const fn parse_positive_decimal_prefix_i64(literal_str: &'code str) -> Result<i64, ()> {
        const BASE: Base = Base::Decimal;
        let mut integer: i64 = 0;
        let mut digit_index = 1 + 1; // 1: leading zero, + 1: base prefix

        let literal = literal_str.as_bytes();
        while digit_index < literal.len() {
            let ascii_digit = literal[digit_index];
            digit_index += 1;
            if ascii_digit == b'_' {
                continue;
            }

            let digit = ascii_digit - b'0';
            debug_assert!(digit < BASE as u8, "invalid decimal digit");

            integer = match integer.checked_mul(BASE as i64) {
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

    #[expect(clippy::single_call_fn)]
    const fn parse_positive_hexadecimal_i64(literal_str: &'code str) -> Result<i64, ()> {
        const BASE: Base = Base::Hexadecimal;
        let mut integer: i64 = 0;
        let mut digit_index = 1 + 1; // 1: leading zero, + 1: base prefix

        let literal = literal_str.as_bytes();
        while digit_index < literal.len() {
            let ascii_digit = literal[digit_index];
            digit_index += 1;
            if ascii_digit == b'_' {
                continue;
            }

            let digit = if ascii_digit > b'a' {
                ascii_digit.wrapping_sub(b'a'.wrapping_add(10))
            } else if ascii_digit > b'A' {
                ascii_digit.wrapping_sub(b'A'.wrapping_add(10))
            } else {
                ascii_digit.wrapping_sub(b'0')
            };
            debug_assert!(digit < BASE as u8, "invalid hexadeximal digit");

            integer = match integer.checked_mul(BASE as i64) {
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

    #[expect(clippy::single_call_fn)]
    const fn parse_negative_binary_i64(literal_str: &'code str) -> Result<i64, ()> {
        const BASE: Base = Base::Binary;
        let mut integer: i64 = 0;
        let mut digit_index = 1 + 1; // 1: leading zero, + 1: base prefix

        let literal = literal_str.as_bytes();
        while digit_index < literal.len() {
            let ascii_digit = literal[digit_index];
            digit_index += 1;
            if ascii_digit == b'_' {
                continue;
            }

            let digit = ascii_digit - b'0';
            debug_assert!(digit < BASE as u8, "invalid binary digit");

            integer = match integer.checked_mul(BASE as i64) {
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

    #[expect(clippy::single_call_fn)]
    const fn parse_negative_octal_i64(literal_str: &'code str) -> Result<i64, ()> {
        const BASE: Base = Base::Octal;
        let mut integer: i64 = 0;
        let mut digit_index = 1 + 1; // 1: leading zero, + 1: base prefix

        let literal = literal_str.as_bytes();
        while digit_index < literal.len() {
            let ascii_digit = literal[digit_index];
            digit_index += 1;
            if ascii_digit == b'_' {
                continue;
            }

            let digit = ascii_digit - b'0';
            debug_assert!(digit < BASE as u8, "invalid octal digit");

            integer = match integer.checked_mul(BASE as i64) {
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

    #[expect(clippy::single_call_fn)]
    const fn parse_negative_decimal_i64(literal_str: &'code str) -> Result<i64, ()> {
        const BASE: Base = Base::Decimal;
        let mut integer: i64 = 0;
        let mut digit_index = 0;

        let literal = literal_str.as_bytes();
        while digit_index < literal.len() {
            let ascii_digit = literal[digit_index];
            digit_index += 1;
            if ascii_digit == b'_' {
                continue;
            }

            let digit = ascii_digit - b'0';
            debug_assert!(digit < BASE as u8, "invalid decimal digit");

            integer = match integer.checked_mul(BASE as i64) {
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

    #[expect(clippy::single_call_fn)]
    const fn parse_negative_decimal_prefix_i64(literal_str: &'code str) -> Result<i64, ()> {
        const BASE: Base = Base::Decimal;
        let mut integer: i64 = 0;
        let mut digit_index = 1 + 1; // 1: leading zero, + 1: base prefix

        let literal = literal_str.as_bytes();
        while digit_index < literal.len() {
            let ascii_digit = literal[digit_index];
            digit_index += 1;
            if ascii_digit == b'_' {
                continue;
            }

            let digit = ascii_digit - b'0';
            debug_assert!(digit < BASE as u8, "invalid decimal digit");

            integer = match integer.checked_mul(BASE as i64) {
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

    #[expect(clippy::single_call_fn)]
    const fn parse_negative_hexadecimal_i64(literal_str: &'code str) -> Result<i64, ()> {
        const BASE: Base = Base::Hexadecimal;
        let mut integer: i64 = 0;
        let mut digit_index = 1 + 1; // 1: leading zero, + 1: base prefix

        let literal = literal_str.as_bytes();
        while digit_index < literal.len() {
            let ascii_digit = literal[digit_index];
            digit_index += 1;
            if ascii_digit == b'_' {
                continue;
            }

            let digit = if ascii_digit > b'a' {
                ascii_digit.wrapping_sub(b'a'.wrapping_add(10))
            } else if ascii_digit > b'A' {
                ascii_digit.wrapping_sub(b'A'.wrapping_add(10))
            } else {
                ascii_digit.wrapping_sub(b'0')
            };
            debug_assert!(digit < BASE as u8, "invalid hexadeximal digit");

            integer = match integer.checked_mul(BASE as i64) {
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

    #[expect(clippy::single_call_fn)]
    const fn parse_ascii(literal_str: &'code str) -> ascii {
        let literal = literal_str.as_bytes();
        debug_assert!(literal.len() >= 3, "tokenization error");
        return match literal[1] {
            b'\\' => match literal[2] {
                b'\\' => b'\\',
                b'\'' => b'\'',
                b'"' => b'"',
                b'n' => b'\n',
                b'r' => b'\r',
                b't' => b'\t',
                b'0' => b'\0',
                _ => unreachable!(),
            },
            other => other,
        };
    }

    // NOTE(stefano): only considering the first token in the expression until proper
    // multiline error messages are developed
    fn first_token_display_len(&self, expression: st::ExpressionIndex<'code>) -> offset32 {
        let st_expression = &self.syntax_tree.expressions[expression];
        let token_kind = match st_expression {
            st::Expression::False { .. } => TokenKind::False,
            st::Expression::True { .. } => TokenKind::True,
            st::Expression::DecimalInteger { literal, .. } => TokenKind::DecimalInteger(*literal),
            st::Expression::DecimalIntegerPrefix { literal, .. } => TokenKind::DecimalIntegerPrefix(*literal),
            st::Expression::BinaryInteger { literal, .. } => TokenKind::BinaryInteger(*literal),
            st::Expression::OctalInteger { literal, .. } => TokenKind::OctalInteger(*literal),
            st::Expression::HexadecimalInteger { literal, .. } => TokenKind::HexadecimalInteger(*literal),
            st::Expression::Ascii { literal, .. } => TokenKind::Ascii(*literal),
            st::Expression::Str { literal, .. } => TokenKind::Str(*literal),
            st::Expression::RawStr { literal, .. } => TokenKind::RawStr(*literal),
            st::Expression::Identifier { identifier, .. } => TokenKind::Identifier(*identifier),
            st::Expression::IdentifierStr { identifier, .. } => TokenKind::IdentifierStr(*identifier),
            st::Expression::Array { .. } | st::Expression::ArrayTrailingItem { .. } => TokenKind::OpenSquareBracket,
            st::Expression::Prefix { operator, .. } => {
                let op: Op = (*operator).into();
                TokenKind::Op(op)
            },
            st::Expression::Binary { left_operand, .. } => {
                return self.first_token_display_len(*left_operand);
            },
            st::Expression::Parenthesis { .. } => TokenKind::OpenRoundBracket,
            st::Expression::Index { indexed_expression, .. } => {
                return self.first_token_display_len(*indexed_expression);
            },
        };

        return token_kind.display_len(self.tokens);
    }

    // NOTE(stefano): only considering the first token in the expression until proper
    // multiline error messages are developed
    fn first_token_column(&self, expression: st::ExpressionIndex<'code>) -> offset32 {
        let st_expression = &self.syntax_tree.expressions[expression];
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
            | st::Expression::ArrayTrailingItem { open_square_bracket_column, .. } => *open_square_bracket_column,
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

    fn resolve_variable(&self, name: &'code str) -> Option<VariableDefinitionIndex<'code>> {
        if let Some(variable) = self.resolve_let_variable(name) {
            return Some(variable);
        }

        return self.resolve_var_variable(name);
    }

    fn resolve_let_variable(&self, name: &'code str) -> Option<VariableDefinitionIndex<'code>> {
        let mut scope_index = self.scope;
        loop {
            let scope = &self.scopes[scope_index];
            for var_index in &scope.let_variables {
                let var = &self.ast.variables[*var_index];
                let var_name_text = self.tokens.text[var.name];
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

    fn resolve_var_variable(&self, name: &'code str) -> Option<VariableDefinitionIndex<'code>> {
        let mut scope_index = self.scope;
        loop {
            let scope = &self.scopes[scope_index];
            for var_index in &scope.var_variables {
                let var = &self.ast.variables[*var_index];
                let var_name_text = self.tokens.text[var.name];
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

    fn resolve_type(&self, name: &'code str) -> Option<BaseType> {
        let mut scope_index = self.scope;
        loop {
            let scope = &self.scopes[scope_index];
            for typ in &scope.types {
                if typ.matches(name) {
                    return Some(*typ);
                }
            }

            scope_index = match scope_index.0 {
                0 => return None,
                _ => scope.parent,
            };
        }
    }

    fn expression(
        &mut self,
        st_expression_index: st::ExpressionIndex<'code>,
        expected_type: Option<&Type>,
    ) -> Result<Expression<'code>, Error<ErrorKind>> {
        let st_expression = &self.syntax_tree.expressions[st_expression_index];
        let expression = match st_expression {
            st::Expression::False { column } => Expression::False { column: *column },
            st::Expression::True { column } => Expression::True { column: *column },
            st::Expression::DecimalInteger { literal, column } => {
                let literal_text = self.tokens.text[*literal];
                let Ok(value) = Self::parse_positive_decimal_i64(literal_text) else {
                    return Err(Error {
                        kind: ErrorKind::DecimalIntegerOverflow,
                        col: *column,
                        #[expect(clippy::cast_possible_truncation)]
                        pointers_count: literal_text.len() as offset32,
                    });
                };
                Expression::I64 { value, column: *column }
            }
            st::Expression::DecimalIntegerPrefix { literal, column } => {
                let literal_text = self.tokens.text[*literal];
                let Ok(value) = Self::parse_positive_decimal_prefix_i64(literal_text) else {
                    return Err(Error {
                        kind: ErrorKind::DecimalIntegerOverflow,
                        col: *column,
                        #[expect(clippy::cast_possible_truncation)]
                        pointers_count: literal_text.len() as offset32,
                    });
                };
                Expression::I64 { value, column: *column }
            }
            st::Expression::BinaryInteger { literal, column } => {
                let literal_text = self.tokens.text[*literal];
                let Ok(value) = Self::parse_positive_binary_i64(literal_text) else {
                    return Err(Error {
                        kind: ErrorKind::BinaryIntegerOverflow,
                        col: *column,
                        #[expect(clippy::cast_possible_truncation)]
                        pointers_count: literal_text.len() as offset32,
                    });
                };
                Expression::I64 { value, column: *column }
            }
            st::Expression::OctalInteger { literal, column } => {
                let literal_text = self.tokens.text[*literal];
                let Ok(value) = Self::parse_positive_octal_i64(literal_text) else {
                    return Err(Error {
                        kind: ErrorKind::OctalIntegerOverflow,
                        col: *column,
                        #[expect(clippy::cast_possible_truncation)]
                        pointers_count: literal_text.len() as offset32,
                    });
                };
                Expression::I64 { value, column: *column }
            }
            st::Expression::HexadecimalInteger { literal, column } => {
                let literal_text = self.tokens.text[*literal];
                let Ok(value) = Self::parse_positive_hexadecimal_i64(literal_text) else {
                    return Err(Error {
                        kind: ErrorKind::HexadecimalIntegerOverflow,
                        col: *column,
                        #[expect(clippy::cast_possible_truncation)]
                        pointers_count: literal_text.len() as offset32,
                    });
                };
                Expression::I64 { value, column: *column }
            }
            st::Expression::Ascii { literal, column } => {
                let ascii_literal = &self.tokens.text[*literal];
                let ascii_ch = Self::parse_ascii(ascii_literal);
                Expression::Ascii { character: ascii_ch, column: *column }
            }
            st::Expression::Str { literal, column } | st::Expression::RawStr { literal, column } => {
                Expression::Str { literal: *literal, column: *column }
            }
            st::Expression::Identifier { identifier, column }
            | st::Expression::IdentifierStr { identifier, column } => {
                let identifier_text = self.tokens.text[*identifier];
                if let Some(_) = self.resolve_type(identifier_text) {
                    return Err(Error {
                        kind: ErrorKind::TypeInExpression,
                        col: *column,
                        #[expect(clippy::cast_possible_truncation)]
                        pointers_count: identifier_text.len() as offset32,
                    });
                }
                let Some(variable) = self.resolve_variable(identifier_text) else {
                    return Err(Error {
                        kind: ErrorKind::VariableNotPreviouslyDefined,
                        col: *column,
                        #[expect(clippy::cast_possible_truncation)]
                        pointers_count: identifier_text.len() as offset32,
                    });
                };

                Expression::Variable { variable, column: *column }
            }

            st::Expression::Array { items_start, items_len, open_square_bracket_column, .. }
            | st::Expression::ArrayTrailingItem { items_start, items_len, open_square_bracket_column, .. } => {
                // TODO(stefano): take into consideration the array base type instead of the
                // whole type
                if *items_len == 0 {
                    return Err(Error {
                        kind: ErrorKind::ArrayOfZeroItems,
                        col: *open_square_bracket_column,
                        pointers_count: 1,
                    });
                }

                let temp_array_items_start = self.temp_array_items.len();
                let mut item_index = *items_start;

                let first_item = &self.syntax_tree.array_items[item_index];
                item_index.0 += 1;

                let parsed_first_item = self.expression(first_item.expression, None)?;
                let mut expected_array_items_type = parsed_first_item.typ(&self.ast);
                if let Type::Array { .. } = expected_array_items_type {
                    return Err(Error {
                        kind: ErrorKind::NestedArrayNotSupportedYet,
                        col: self.first_token_column(first_item.expression),
                        pointers_count: self.first_token_display_len(first_item.expression),
                    });
                }

                if let Some(typ) = expected_type {
                    expected_array_items_type = Type::Base(typ.base_typ());
                    self.expect_expression_type(first_item.expression, &expected_array_items_type, &parsed_first_item)?;
                }
                let expected_array_items_type_hint = Some(&expected_array_items_type);

                let parsed_first_item_index = self.ast.new_expression(parsed_first_item);
                self.temp_array_items.push(parsed_first_item_index);

                let items_end = (items_start.0 + items_len) as usize;
                while (item_index.0 as usize) < items_end {
                    let item = &self.syntax_tree.array_items[item_index];
                    item_index.0 += 1;

                    let parsed_item = self.expression(item.expression, expected_array_items_type_hint)?;
                    let parsed_item_type = parsed_item.typ(&self.ast);
                    if let Type::Array { .. } = parsed_item_type {
                        return Err(Error {
                            kind: ErrorKind::NestedArrayNotSupportedYet,
                            col: self.first_token_column(item.expression),
                            pointers_count: self.first_token_display_len(item.expression),
                        });
                    }
                    let parsed_item_index = self.ast.new_expression(parsed_item);
                    self.temp_array_items.push(parsed_item_index);
                }

                let array_items = &self.temp_array_items[temp_array_items_start..];
                self.ast.array_items.extend_from_slice(array_items);

                let array_expression = Expression::Array {
                    base_type: expected_array_items_type.base_typ(),
                    items_start: ExpressionIndex::new_offset32(items_start.0),
                    items_len: *items_len as u64,
                };

                unsafe { self.temp_array_items.set_len(temp_array_items_start); }
                array_expression
            }

            st::Expression::Prefix { operator, operator_column, right_operand } => match operator {
                st::PrefixOperator::Len => {
                    let right_operand_expression = self.expression(*right_operand, None)?;
                    let right_operand_type = right_operand_expression.typ(&self.ast);
                    match right_operand_type {
                        Type::Base(BaseType::Str) | Type::Array { .. } => Expression::Prefix {
                            operator: (*operator).into(),
                            operator_column: *operator_column,
                            right_operand: self.ast.new_expression(right_operand_expression),
                        },
                        Type::Base(BaseType::I64 | BaseType::Ascii | BaseType::Bool) => {
                            return Err(Error {
                                kind: ErrorKind::CannotTakeLenOf(right_operand_type),
                                col: self.first_token_column(*right_operand),
                                pointers_count: self.first_token_display_len(*right_operand),
                            });
                        }
                    }
                }
                st::PrefixOperator::Not => {
                    let right_operand_expression = self.expression(*right_operand, None)?;
                    let right_operand_type = right_operand_expression.typ(&self.ast);
                    match right_operand_type {
                        Type::Base(BaseType::Ascii | BaseType::I64) => Expression::Prefix {
                            operator: (*operator).into(),
                            operator_column: *operator_column,
                            right_operand: self.ast.new_expression(right_operand_expression),
                        },
                        Type::Base(BaseType::Bool) => Expression::BooleanPrefix {
                            operator: (*operator).into(),
                            operator_column: *operator_column,
                            right_operand: self.ast.new_expression(right_operand_expression),
                        },
                        Type::Base(BaseType::Str) | Type::Array { .. } => {
                            return Err(Error {
                                kind: ErrorKind::CannotInvert(right_operand_type),
                                col: self.first_token_column(*right_operand),
                                pointers_count: self.first_token_display_len(*right_operand),
                            });
                        }
                    }
                }
                st::PrefixOperator::Plus
                | st::PrefixOperator::WrappingPlus
                | st::PrefixOperator::SaturatingPlus => {
                    let right_operand_expression = self.expression(*right_operand, None)?;
                    let right_operand_type = right_operand_expression.typ(&self.ast);
                    match right_operand_type {
                        Type::Base(BaseType::I64) => Expression::Prefix {
                            operator: (*operator).into(),
                            operator_column: *operator_column,
                            right_operand: self.ast.new_expression(right_operand_expression),
                        },
                        Type::Base(BaseType::Bool |BaseType::Ascii | BaseType::Str) | Type::Array { .. } => {
                            return Err(Error {
                                kind: ErrorKind::CannotTakeAbsoluteValueOf(right_operand_type),
                                col: self.first_token_column(*right_operand),
                                pointers_count: self.first_token_display_len(*right_operand),
                            });
                        }
                    }
                }
                st::PrefixOperator::Minus
                | st::PrefixOperator::WrappingMinus
                | st::PrefixOperator::SaturatingMinus => {
                    let st_right_operand = &self.syntax_tree.expressions[*right_operand];
                    match st_right_operand {
                        st::Expression::BinaryInteger { literal, column } => {
                            let literal_text = self.tokens.text[*literal];
                            let right_operand_expression = match Self::parse_negative_binary_i64(literal_text) {
                                Ok(0) => return Err(Error {
                                    kind: ErrorKind::MinusZeroInteger,
                                    col: *column,
                                    #[expect(clippy::cast_possible_truncation)]
                                    pointers_count: literal_text.len() as offset32,
                                }),
                                Ok(integer) => Expression::I64 { value: integer, column: *column },
                                Err(()) => return Err(Error {
                                    kind: ErrorKind::BinaryIntegerUnderflow,
                                    col: *column,
                                    #[expect(clippy::cast_possible_truncation)]
                                    pointers_count: literal_text.len() as offset32,
                                }),
                            };

                            Expression::Prefix {
                                operator: (*operator).into(),
                                operator_column: *operator_column,
                                right_operand: self.ast.new_expression(right_operand_expression),
                            }
                        }
                        st::Expression::OctalInteger { literal, column } => {
                            let literal_text = self.tokens.text[*literal];
                            let right_operand_expression = match Self::parse_negative_octal_i64(literal_text) {
                                Ok(0) => return Err(Error {
                                    kind: ErrorKind::MinusZeroInteger,
                                    col: *column,
                                    #[expect(clippy::cast_possible_truncation)]
                                    pointers_count: literal_text.len() as offset32,
                                }),
                                Ok(integer) => Expression::I64 { value: integer, column: *column },
                                Err(()) => return Err(Error {
                                    kind: ErrorKind::OctalIntegerUnderflow,
                                    col: *column,
                                    #[expect(clippy::cast_possible_truncation)]
                                    pointers_count: literal_text.len() as offset32,
                                }),
                            };

                            Expression::Prefix {
                                operator: (*operator).into(),
                                operator_column: *operator_column,
                                right_operand: self.ast.new_expression(right_operand_expression),
                            }
                        }
                        st::Expression::DecimalInteger { literal, column } => {
                            let literal_text = self.tokens.text[*literal];
                            let right_operand_expression = match Self::parse_negative_decimal_i64(literal_text) {
                                Ok(0) => return Err(Error {
                                    kind: ErrorKind::MinusZeroInteger,
                                    col: *column,
                                    #[expect(clippy::cast_possible_truncation)]
                                    pointers_count: literal_text.len() as offset32,
                                }),
                                Ok(integer) => Expression::I64 { value: integer, column: *column },
                                Err(()) => return Err(Error {
                                    kind: ErrorKind::DecimalIntegerUnderflow,
                                    col: *column,
                                    #[expect(clippy::cast_possible_truncation)]
                                    pointers_count: literal_text.len() as offset32,
                                }),
                            };

                            Expression::Prefix {
                                operator: (*operator).into(),
                                operator_column: *operator_column,
                                right_operand: self.ast.new_expression(right_operand_expression),
                            }
                        }
                        st::Expression::DecimalIntegerPrefix { literal, column } => {
                            let literal_text = self.tokens.text[*literal];
                            let right_operand_expression = match Self::parse_negative_decimal_prefix_i64(literal_text) {
                                Ok(0) => return Err(Error {
                                    kind: ErrorKind::MinusZeroInteger,
                                    col: *column,
                                    #[expect(clippy::cast_possible_truncation)]
                                    pointers_count: literal_text.len() as offset32,
                                }),
                                Ok(integer) => Expression::I64 { value: integer, column: *column },
                                Err(()) => return Err(Error {
                                    kind: ErrorKind::DecimalIntegerUnderflow,
                                    col: *column,
                                    #[expect(clippy::cast_possible_truncation)]
                                    pointers_count: literal_text.len() as offset32,
                                }),
                            };

                            Expression::Prefix {
                                operator: (*operator).into(),
                                operator_column: *operator_column,
                                right_operand: self.ast.new_expression(right_operand_expression),
                            }
                        }
                        st::Expression::HexadecimalInteger { literal, column } => {
                            let literal_text = self.tokens.text[*literal];
                            let right_operand_expression = match Self::parse_negative_hexadecimal_i64(literal_text) {
                                Ok(0) => return Err(Error {
                                    kind: ErrorKind::MinusZeroInteger,
                                    col: *column,
                                    #[expect(clippy::cast_possible_truncation)]
                                    pointers_count: literal_text.len() as offset32,
                                }),
                                Ok(integer) => Expression::I64 { value: integer, column: *column },
                                Err(()) => return Err(Error {
                                    kind: ErrorKind::HexadecimalIntegerUnderflow,
                                    col: *column,
                                    #[expect(clippy::cast_possible_truncation)]
                                    pointers_count: literal_text.len() as offset32,
                                }),
                            };

                            Expression::Prefix {
                                operator: (*operator).into(),
                                operator_column: *operator_column,
                                right_operand: self.ast.new_expression(right_operand_expression),
                            }
                        }
                        st::Expression::Array { .. }
                        | st::Expression::ArrayTrailingItem { .. }
                        | st::Expression::False { .. } | st::Expression::True { .. }
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
                            let right_operand_type = right_operand_expression.typ(&self.ast);
                            match right_operand_type {
                                Type::Base(BaseType::I64 | BaseType::Ascii) => Expression::Prefix {
                                    operator: (*operator).into(),
                                    operator_column: *operator_column,
                                    right_operand: self.ast.new_expression(right_operand_expression),
                                },
                                Type::Base(BaseType::Bool | BaseType::Str) | Type::Array { .. } => {
                                    return Err(Error {
                                        kind: ErrorKind::CannotNegate(right_operand_type),
                                        col: self.first_token_column(*right_operand),
                                        pointers_count: self.first_token_display_len(*right_operand),
                                    });
                                }
                            }
                        }
                    }
                }
            }
            st::Expression::Binary { left_operand, operator, operator_column, right_operand } => {
                let left_operand_expression = self.expression(*left_operand, None)?;
                let left_operand_type = left_operand_expression.typ(&self.ast);

                let right_operand_expression = self.expression(*right_operand, None)?;
                let right_operand_type = right_operand_expression.typ(&self.ast);

                match operator {
                    st::BinaryOperator::Pow
                    | st::BinaryOperator::WrappingPow
                    | st::BinaryOperator::SaturatingPow

                    | st::BinaryOperator::Times
                    | st::BinaryOperator::WrappingTimes
                    | st::BinaryOperator::SaturatingTimes

                    | st::BinaryOperator::Divide
                    | st::BinaryOperator::WrappingDivide
                    | st::BinaryOperator::SaturatingDivide

                    | st::BinaryOperator::Remainder

                    | st::BinaryOperator::Plus
                    | st::BinaryOperator::WrappingPlus
                    | st::BinaryOperator::SaturatingPlus

                    | st::BinaryOperator::Minus
                    | st::BinaryOperator::WrappingMinus
                    | st::BinaryOperator::SaturatingMinus

                    | st::BinaryOperator::LeftShift
                    | st::BinaryOperator::WrappingLeftShift
                    | st::BinaryOperator::SaturatingLeftShift

                    | st::BinaryOperator::RightShift
                    | st::BinaryOperator::LeftRotate
                    | st::BinaryOperator::RightRotate
                    | st::BinaryOperator::BitAnd
                    | st::BinaryOperator::BitXor
                    | st::BinaryOperator::BitOr => {
                        match left_operand_type {
                            Type::Base(BaseType::Ascii | BaseType::Bool | BaseType::I64) => {}
                            Type::Base(BaseType::Str) | Type::Array { .. } => {
                                return Err(Error {
                                    kind: ErrorKind::LeftOperandTypeMismatch {
                                        expected: BinaryOperator::TYPE,
                                        actual: left_operand_type,
                                    },
                                    col: *operator_column,
                                    pointers_count: operator.display_len(),
                                });
                            }
                        }

                        match right_operand_type {
                            Type::Base(BaseType::Ascii | BaseType::Bool | BaseType::I64) => {}
                            Type::Base(BaseType::Str) | Type::Array { .. } => {
                                return Err(Error {
                                    kind: ErrorKind::RightOperandTypeMismatch {
                                        expected: BinaryOperator::TYPE,
                                        actual: right_operand_type,
                                    },
                                    col: *operator_column,
                                    pointers_count: operator.display_len(),
                                });
                            }
                        }

                        Expression::Binary {
                            left_operand: self.ast.new_expression(left_operand_expression),
                            operator: (*operator).into(),
                            operator_column: *operator_column,
                            right_operand: self.ast.new_expression(right_operand_expression),
                        }
                    },

                    st::BinaryOperator::And
                    | st::BinaryOperator::Or => {
                        match left_operand_type {
                            Type::Base(BaseType::Bool) => {},
                            Type::Base(BaseType::Ascii | BaseType::I64 | BaseType::Str)
                            | Type::Array { .. } => {
                                return Err(Error {
                                    kind: ErrorKind::LeftOperandTypeMismatch {
                                        expected: BooleanBinaryOperator::TYPE,
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
                                return Err(Error {
                                    kind: ErrorKind::RightOperandTypeMismatch {
                                        expected: BooleanBinaryOperator::TYPE,
                                        actual: right_operand_type,
                                    },
                                    col: *operator_column,
                                    pointers_count: operator.display_len(),
                                });
                            },
                        }

                        Expression::BooleanBinary {
                            left_operand: self.ast.new_expression(left_operand_expression),
                            operator: (*operator).into(),
                            operator_column: *operator_column,
                            right_operand: self.ast.new_expression(right_operand_expression),
                        }
                    },

                    st::BinaryOperator::Compare => {
                        if left_operand_type != right_operand_type {
                            return Err(Error {
                                kind: ErrorKind::CannotCompareOperands {
                                    left_operand_type,
                                    right_operand_type
                                },
                                col: *operator_column,
                                pointers_count: operator.display_len(),
                            });
                        }

                        if let Expression::Comparison { .. } = &left_operand_expression {
                            return Err(Error {
                                kind: ErrorKind::CannotChainComparisons,
                                col: *operator_column,
                                pointers_count: operator.display_len(),
                            });
                        }

                        Expression::Comparison {
                            left_operand: self.ast.new_expression(left_operand_expression),
                            operator: (*operator).into(),
                            operator_column: *operator_column,
                            right_operand: self.ast.new_expression(right_operand_expression),
                        }
                    },
                    st::BinaryOperator::EqualsEquals
                    | st::BinaryOperator::NotEquals
                    | st::BinaryOperator::Greater
                    | st::BinaryOperator::GreaterOrEquals
                    | st::BinaryOperator::Less
                    | st::BinaryOperator::LessOrEquals => {
                        if left_operand_type != right_operand_type {
                            return Err(Error {
                                kind: ErrorKind::CannotCompareOperands {
                                    left_operand_type,
                                    right_operand_type
                                },
                                col: *operator_column,
                                pointers_count: operator.display_len(),
                            });
                        }

                        if let Expression::BooleanComparison { .. } = &left_operand_expression {
                            return Err(Error {
                                kind: ErrorKind::CannotChainComparisons,
                                col: *operator_column,
                                pointers_count: operator.display_len(),
                            });
                        }

                        Expression::BooleanComparison {
                            left_operand: self.ast.new_expression(left_operand_expression),
                            operator: (*operator).into(),
                            operator_column: *operator_column,
                            right_operand: self.ast.new_expression(right_operand_expression),
                        }
                    },
                }
            }
            st::Expression::Parenthesis { inner_expression, .. } => {
                return self.expression(*inner_expression, expected_type);
            }
            st::Expression::Index { indexed_expression, open_square_bracket_column, index_expression, .. } => {
                // IDEA(stefano): try parsing a complete type instead of just an identifier
                let indexed_expression_expression = self.expression(*indexed_expression, None)?;
                let Expression::Variable { .. } = indexed_expression_expression else {
                    return Err(Error {
                        kind: ErrorKind::CannotIndexIntoExpression,
                        col: self.first_token_column(*indexed_expression),
                        pointers_count: self.first_token_display_len(*indexed_expression),
                    });
                };

                let indexed_expression_type = indexed_expression_expression.typ(&self.ast);
                match indexed_expression_type {
                    Type::Base(BaseType::Str) | Type::Array { .. } => {
                        let index_expression_expression = self.expression(*index_expression, None)?;
                        let index_expression_type = index_expression_expression.typ(&self.ast);
                        match index_expression_type {
                            Type::Base(BaseType::I64) => {},
                            Type::Base(BaseType::Ascii | BaseType::Bool | BaseType::Str)
                            | Type::Array { .. } => {
                                return Err(Error {
                                    kind: ErrorKind::ExpectedIntegerExpressionInArrayIndex,
                                    col: self.first_token_column(*index_expression),
                                    pointers_count: self.first_token_display_len(*index_expression),
                                });
                            },
                        }

                        Expression::Index {
                            indexed_expression: self.ast.new_expression(indexed_expression_expression),
                            open_square_bracket_column: *open_square_bracket_column,
                            index_expression: self.ast.new_expression(index_expression_expression),
                        }
                    }
                    Type::Base(BaseType::I64 | BaseType::Ascii | BaseType::Bool) => {
                        return Err(Error {
                            kind: ErrorKind::CannotIndexNonArrayLikeType(indexed_expression_type),
                            col: self.first_token_column(*indexed_expression),
                            pointers_count: self.first_token_display_len(*indexed_expression),
                        })
                    }
                }
            }
        };

        if let Some(expected_expression_type) = expected_type {
            self.expect_expression_type(st_expression_index, expected_expression_type, &expression)?;
        }

        return Ok(expression);
    }

    fn expect_expression_type(
        &self,
        st_expression_index: st::ExpressionIndex<'code>,
        expected_type: &Type,
        expression: &Expression<'code>,
    ) -> Result<(), Error<ErrorKind>> {
        let expression_type = expression.typ(&self.ast);
        if expression_type != *expected_type {
            return Err(Error {
                kind: ErrorKind::TypeMismatch {
                    expected: expected_type.clone(),
                    actual: expression_type,
                },
                col: self.first_token_column(st_expression_index),
                pointers_count: self.first_token_display_len(st_expression_index),
            });
        }
        return Ok(());
    }

    #[inline]
    fn parse_expression(
        &mut self,
        st_expression_index: st::ExpressionIndex<'code>,
        expected_type: Option<&Type>,
    ) -> Result<ExpressionIndex<'code>, Error<ErrorKind>> {
        let expression = self.expression(st_expression_index, expected_type)?;
        let expression_index = self.ast.new_expression(expression);
        return Ok(expression_index);
    }
}

impl<'code> TypedSyntaxTree<'_, '_, 'code> {
    #[inline]
    fn new_variable(&mut self, variable: VariableDefinition<'code>) -> VariableDefinitionIndex<'code> {
        let index = VariableDefinitionIndex::new(self.variables.len());
        self.variables.push(variable);
        return index;
    }
}

impl<'code> Parser<'_, '_, '_, 'code, '_> {
    // NOTE(stefano): "leaks" memory by parsing and storing expressions, but these expressions
    // should never be used
    fn parse_type_annotation(
        &mut self,
        st::TypeAnnotation {
            type_name,
            type_name_column,
            array_dimensions_start,
            array_dimensions_len,
            ..
        }: &st::TypeAnnotation<'code>,
    ) -> Result<Type, Error<ErrorKind>> {
        let type_name_text = self.tokens.text[*type_name];
        if let Some(_) = self.resolve_variable(type_name_text) {
            return Err(Error {
                kind: ErrorKind::VariableInTypeAnnotation,
                col: *type_name_column,
                #[expect(clippy::cast_possible_truncation)]
                pointers_count: type_name_text.len() as offset32,
            });
        }
        let Some(base_type) = self.resolve_type(type_name_text) else {
            return Err(Error {
                kind: ErrorKind::TypeNotPreviouslyDefined,
                col: *type_name_column,
                #[expect(clippy::cast_possible_truncation)]
                pointers_count: type_name_text.len() as offset32,
            });
        };

        let array_dimensions_end = array_dimensions_start.0 + array_dimensions_len;
        let array_dimensions = &self.syntax_tree.array_dimensions[
            array_dimensions_start.0 as usize..array_dimensions_end as usize
        ];
        let mut array_dimensions_iter = array_dimensions.iter();
        let Some(st::ArrayDimension {
            dimension_expression,
            open_square_bracket_column,
            ..
        }) = array_dimensions_iter.next() else {
            return Ok(Type::Base(base_type));
        };

        let first_dimension_expression = &self.expression(*dimension_expression, None)?;
        let Expression::I64 { value: len, column } = first_dimension_expression else {
            return Err(Error {
                kind: ErrorKind::ExpectedIntegerLiteralInArrayType,
                col: *open_square_bracket_column,
                pointers_count: 1,
            });
        };
        if *len < 0 {
            return Err(Error {
                kind: ErrorKind::ArrayOfNegativeLength,
                col: *column,
                pointers_count: self.first_token_display_len(*dimension_expression),
            });
        }
        if *len == 0 {
            return Err(Error {
                kind: ErrorKind::ArrayOfZeroItems,
                col: *column,
                pointers_count: self.first_token_display_len(*dimension_expression),
            });
        }

        for st::ArrayDimension {
            dimension_expression: other_dimension_expression,
            open_square_bracket_column: other_open_square_bracket_column,
            ..
        } in array_dimensions_iter {
            let dimension_expression_expression = &self.expression(*other_dimension_expression, None)?;
            let Expression::I64 { value: other_dimension_len, column: other_dimension_column } = dimension_expression_expression else {
                return Err(Error {
                    kind: ErrorKind::ExpectedIntegerLiteralInArrayType,
                    col: *other_open_square_bracket_column,
                    pointers_count: 1,
                });
            };
            if *other_dimension_len < 0 {
                return Err(Error {
                    kind: ErrorKind::ArrayOfNegativeLength,
                    col: *other_dimension_column,
                    pointers_count: self.first_token_display_len(*other_dimension_expression),
                });
            }
            if *other_dimension_len == 0 {
                return Err(Error {
                    kind: ErrorKind::ArrayOfZeroItems,
                    col: *other_dimension_column,
                    pointers_count: self.first_token_display_len(*other_dimension_expression),
                });
            }
        }

        if *array_dimensions_len > 1 {
            return Err(Error {
                kind: ErrorKind::NestedArrayNotSupportedYet,
                col: *type_name_column,
                #[expect(clippy::cast_possible_truncation)]
                pointers_count: type_name_text.len() as offset32,
            });
        }

        return Ok(Type::Array {
            base_type,
            #[expect(clippy::cast_sign_loss)]
            len: *len as u64,
        });
    }

    fn parse_variable(
        &mut self,
        variable_definition: st::VariableDefinitionIndex<'code>,
    ) -> Result<VariableDefinitionIndex<'code>, Error<ErrorKind>> {
        let st::VariableDefinition {
            name,
            name_column,
            type_annotation,
            initial_value,
        } = &self.syntax_tree.variable_definitions[variable_definition];

        let name_text = self.tokens.text[*name];
        if let Some(_) = self.resolve_variable(name_text) {
            return Err(Error {
                kind: ErrorKind::VariableAlreadyDefined,
                col: *name_column,
                #[expect(clippy::cast_possible_truncation)]
                pointers_count: name_text.len() as offset32,
            });
        }
        if let Some(_) = self.resolve_type(name_text) {
            return Err(Error {
                kind: ErrorKind::TypeInVariableName,
                col: *name_column,
                #[expect(clippy::cast_possible_truncation)]
                pointers_count: name_text.len() as offset32,
            });
        }

        let Some(st::InitialValue { expression, .. }) = initial_value else {
            let Some(_) = type_annotation else {
                return Err(Error {
                    kind: ErrorKind::CannotInferTypeOfVariable,
                    col: *name_column,
                    #[expect(clippy::cast_possible_truncation)]
                    pointers_count: name_text.len() as offset32,
                });
            };
            return Err(Error {
                kind: ErrorKind::VariablesMustBeInitialized,
                col: *name_column,
                #[expect(clippy::cast_possible_truncation)]
                pointers_count: name_text.len() as offset32,
            });
        };

        let (parsed_expression, expression_type) = if let Some(type_annotation_inner) = type_annotation {
            let expression_type = self.parse_type_annotation(type_annotation_inner)?;
            (self.expression(*expression, Some(&expression_type))?, expression_type)
        } else {
            let parsed_expression = self.expression(*expression, None)?;
            let expression_type = parsed_expression.typ(&self.ast);
            (parsed_expression, expression_type)
        };

        let variable = VariableDefinition {
            name: *name,
            typ: expression_type,
            initial_value: self.ast.new_expression(parsed_expression),
        };
        let variable_index = self.ast.new_variable(variable);
        return Ok(variable_index);
    }
}

impl<'code> Parser<'_, '_, '_, 'code, '_> {
    fn assignment(
        &mut self,
        target: st::ExpressionIndex<'code>,
        operator: st::AssignmentOperator,
        operator_column: offset32,
        new_value: st::ExpressionIndex<'code>,
    ) -> Result<Node<'code>, Error<ErrorKind>> {
        let parsed_target = self.expression(target, None)?;

        let parsed_new_value = self.expression(new_value, None)?;
        let parsed_new_value_type = parsed_new_value.typ(&self.ast);
        let parsed_target_type = match &parsed_target {
            Expression::Variable { variable, column } => {
                let variable_definition = &self.ast.variables[*variable];
                let variable_name_text = self.tokens.text[variable_definition.name];
                if let Some(_) = self.resolve_let_variable(variable_name_text) {
                    return Err(Error {
                        kind: ErrorKind::CannotMutateVariable,
                        col: *column,
                        #[expect(clippy::cast_possible_truncation)]
                        pointers_count: variable_name_text.len() as offset32,
                    });
                }
                variable_definition.typ.clone()
            }
            Expression::Index { indexed_expression, .. } => {
                let mut base_indexed = &self.ast.expressions[*indexed_expression];
                while let Expression::Index {
                    indexed_expression: inner_indexed_expression,
                    ..
                } = base_indexed {
                    base_indexed = &self.ast.expressions[*inner_indexed_expression];
                }

                let Expression::Variable { variable, column } = base_indexed else {
                    return Err(Error {
                        kind: ErrorKind::CannotAssignToExpression,
                        col: operator_column,
                        pointers_count: operator.display_len(),
                    });
                };

                let variable_definition = &self.ast.variables[*variable];
                let variable_name_text = self.tokens.text[variable_definition.name];
                if let Some(_) = self.resolve_let_variable(variable_name_text) {
                    return Err(Error {
                        kind: ErrorKind::CannotMutateVariable,
                        col: *column,
                        #[expect(clippy::cast_possible_truncation)]
                        pointers_count: variable_name_text.len() as offset32,
                    });
                }

                if let base_type @ BaseType::Str = variable_definition.typ.base_typ() {
                    if let BaseType::Ascii = parsed_new_value_type.base_typ() {
                        return Err(Error {
                            kind: ErrorKind::CannotMutateStringCharacters,
                            col: *column,
                            #[expect(clippy::cast_possible_truncation)]
                            pointers_count: variable_name_text.len() as offset32,
                        });
                    }
                    Type::Base(base_type)
                } else {
                    variable_definition.typ.clone()
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
                return Err(Error {
                    kind: ErrorKind::CannotAssignToExpression,
                    col: operator_column,
                    pointers_count: operator.display_len(),
                });
            }
        };

        let assignment_node = match operator {
            st::AssignmentOperator::Equals => {
                if parsed_target_type != parsed_new_value_type {
                    return Err(Error {
                        kind: ErrorKind::RightOperandTypeMismatch {
                            expected: parsed_target_type,
                            actual: parsed_new_value_type,
                        },
                        col: operator_column,
                        pointers_count: operator.display_len(),
                    });
                }

                Node::Assignment {
                    target: self.ast.new_expression(parsed_target),
                    new_value: self.ast.new_expression(parsed_new_value)
                }
            }

            st::AssignmentOperator::Pow
            | st::AssignmentOperator::WrappingPow
            | st::AssignmentOperator::SaturatingPow

            | st::AssignmentOperator::Times
            | st::AssignmentOperator::WrappingTimes
            | st::AssignmentOperator::SaturatingTimes

            | st::AssignmentOperator::Divide
            | st::AssignmentOperator::WrappingDivide
            | st::AssignmentOperator::SaturatingDivide

            | st::AssignmentOperator::Remainder

            | st::AssignmentOperator::Plus
            | st::AssignmentOperator::WrappingPlus
            | st::AssignmentOperator::SaturatingPlus

            | st::AssignmentOperator::Minus
            | st::AssignmentOperator::WrappingMinus
            | st::AssignmentOperator::SaturatingMinus

            | st::AssignmentOperator::LeftShift
            | st::AssignmentOperator::WrappingLeftShift
            | st::AssignmentOperator::SaturatingLeftShift

            | st::AssignmentOperator::RightShift
            | st::AssignmentOperator::LeftRotate
            | st::AssignmentOperator::RightRotate
            | st::AssignmentOperator::BitAnd
            | st::AssignmentOperator::BitXor
            | st::AssignmentOperator::BitOr => {
                match parsed_target_type {
                    Type::Base(BaseType::Ascii | BaseType::I64 | BaseType::Bool) => {}
                    Type::Base(BaseType::Str) | Type::Array { .. } => {
                        return Err(Error {
                            kind: ErrorKind::LeftOperandTypeMismatch {
                                expected: BinaryOperator::TYPE,
                                actual: parsed_target_type,
                            },
                            col: operator_column,
                            pointers_count: operator.display_len(),
                        });
                    }
                }

                match parsed_new_value_type {
                    Type::Base(BaseType::Ascii | BaseType::I64 | BaseType::Bool) => {}
                    Type::Base(BaseType::Str) | Type::Array { .. } => {
                        return Err(Error {
                            kind: ErrorKind::RightOperandTypeMismatch {
                                expected: BinaryOperator::TYPE,
                                actual: parsed_new_value_type,
                            },
                            col: operator_column,
                            pointers_count: operator.display_len(),
                        });
                    }
                }

                match (parsed_target_type, parsed_new_value_type) {
                    (
                        Type::Base(BaseType::I64),
                        Type::Base(BaseType::I64 | BaseType::Ascii | BaseType::Bool),
                    ) => {}
                    (target_type @ Type::Base(BaseType::Ascii | BaseType::Bool), _) => {
                        return Err(Error {
                            kind: ErrorKind::CannotModifyInplace(target_type),
                            col: operator_column,
                            pointers_count: operator.display_len(),
                        });
                    }
                    (target_type, new_value_type) => return Err(Error {
                        kind: ErrorKind::TypeMismatch {
                            expected: target_type,
                            actual: new_value_type,
                        },
                        col: self.first_token_column(new_value),
                        pointers_count: self.first_token_display_len(new_value),
                    }),
                }

                Node::AssignmentExpression {
                    target: self.ast.new_expression(parsed_target),
                    operator: operator.into(),
                    operator_column,
                    new_value: self.ast.new_expression(parsed_new_value),
                }
            },
            st::AssignmentOperator::And
            | st::AssignmentOperator::Or => {
                match parsed_target_type {
                    Type::Base(BaseType::Bool) => {},
                    Type::Base(BaseType::Ascii | BaseType::I64 | BaseType::Str)
                    | Type::Array { .. } => {
                        return Err(Error {
                            kind: ErrorKind::LeftOperandTypeMismatch {
                                expected: BooleanBinaryOperator::TYPE,
                                actual: parsed_target_type,
                            },
                            col: operator_column,
                            pointers_count: operator.display_len(),
                        });
                    },
                }

                match parsed_new_value_type {
                    Type::Base(BaseType::Bool) => {},
                    Type::Base(BaseType::Ascii | BaseType::I64 | BaseType::Str)
                    | Type::Array { .. } => {
                        return Err(Error {
                            kind: ErrorKind::RightOperandTypeMismatch {
                                expected: BooleanBinaryOperator::TYPE,
                                actual: parsed_new_value_type,
                            },
                            col: operator_column,
                            pointers_count: operator.display_len(),
                        });
                    },
                }

                Node::BooleanAssignmentExpression {
                    target: self.ast.new_expression(parsed_target),
                    operator: operator.into(),
                    operator_column,
                    new_value: self.ast.new_expression(parsed_new_value),
                }
            },
        };

        return Ok(assignment_node);
    }
}

impl<'code> Parser<'_, '_, '_, 'code, '_> {
    fn if_condition(
        &mut self,
        if_column: offset32,
        condition: st::ExpressionIndex<'code>,
    ) -> Result<Expression<'code>, Error<ErrorKind>> {
        let condition_expression = self.expression(condition, None)?;
        let condition_expression_type = condition_expression.typ(&self.ast);
        let Type::Base(BaseType::Bool) = condition_expression_type else {
            return Err(Error {
                kind: ErrorKind::RightOperandTypeMismatch {
                    expected: Type::Base(BaseType::Bool),
                    actual: condition_expression_type,
                },
                col: if_column,
                pointers_count: TokenKind::If.display_len(self.tokens),
            });
        };
        return Ok(condition_expression);
    }

    fn loop_condition(
        &mut self,
        loop_column: offset32,
        condition: st::ExpressionIndex<'code>,
    ) -> Result<Expression<'code>, Error<ErrorKind>> {
        let condition_expression = self.expression(condition, None)?;
        let condition_expression_type = condition_expression.typ(&self.ast);
        let Type::Base(BaseType::Bool) = condition_expression_type else {
            return Err(Error {
                kind: ErrorKind::RightOperandTypeMismatch {
                    expected: Type::Base(BaseType::Bool),
                    actual: condition_expression_type,
                },
                col: loop_column,
                pointers_count: TokenKind::Loop.display_len(self.tokens),
            });
        };
        return Ok(condition_expression);
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
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

impl IntoErrorInfo for ErrorKind {
    fn info(&self) -> ErrorInfo {
        let (error_message, error_cause_message) = match self {
            Self::BinaryIntegerOverflow => (
                "integer literal overflow".into(),
                format!(
                    "overflows a {bits} bit signed integer, over {prefix}{max:0b} ({max})",
                    bits = i64::BITS,
                    prefix = Base::Binary.prefix(),
                    max = i64::MAX
                )
                .into(),
            ),
            Self::OctalIntegerOverflow => (
                "integer literal overflow".into(),
                format!(
                    "overflows a {bits} bit signed integer, over {prefix}{max:0o} ({max})",
                    bits = i64::BITS,
                    prefix = Base::Octal.prefix(),
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
                    prefix = Base::Hexadecimal.prefix(),
                    max = i64::MAX
                )
                .into(),
            ),
            Self::BinaryIntegerUnderflow => (
                "integer literal underflow".into(),
                format!(
                    "underflows a {bits} bit signed integer, under {prefix}{min:0b} ({min})",
                    bits = i64::BITS,
                    prefix = Base::Binary.prefix(),
                    min = i64::MIN
                )
                .into(),
            ),
            Self::OctalIntegerUnderflow => (
                "integer literal underflow".into(),
                format!(
                    "underflows a {bits} bit signed integer, under {prefix}{min:0o} ({min})",
                    bits = i64::BITS,
                    prefix = Base::Octal.prefix(),
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
                    prefix = Base::Hexadecimal.prefix(),
                    min = i64::MIN
                )
                .into(),
            ),

            Self::MinusZeroInteger => (
                "invalid integer literal".into(),
                "-0 is not a valid two's complement integer".into(),
            ),

            Self::VariableNotPreviouslyDefined => (
                "variable not previously defined".into(),
                "was not previously defined".into(),
            ),
            Self::VariableAlreadyDefined => (
                "variable already defined".into(),
                "was already defined".into(),
            ),
            Self::VariableInTypeAnnotation => (
                "invalid type annotation".into(),
                "variable names are not allowed in type annotations".into(),
            ),
            Self::TypeNotPreviouslyDefined => (
                "type not previously defined".into(),
                "was not previously defined".into(),
            ),
            Self::ExpectedIntegerLiteralInArrayType => (
                "invalid type".into(),
                "must be followed by an integer literal".into(),
            ),
            Self::ArrayOfNegativeLength => (
                "invalid array length".into(),
                "array length must be greater than 1".into(),
            ),
            Self::ArrayOfZeroItems => (
                "invalid array".into(),
                "arrays of zero items are not allowed yet".into(),
            ),
            Self::NestedArrayNotSupportedYet => (
                "invalid array item".into(),
                "nested arrays are not supported yet".into(),
            ),
            Self::TypeInExpression => (
                "invalid expression".into(),
                "types are not allowed in expressions".into(),
            ),
            Self::TypeInVariableName => (
                "invalid variable name".into(),
                "types are not allowed in variable names".into(),
            ),
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
            Self::CannotNegate(invalid_type) => (
                "invalid expression".into(),
                format!("cannot negate value of type '{invalid_type}'").into(),
            ),
            Self::CannotInvert(invalid_type) => (
                "invalid expression".into(),
                format!("cannot invert value of type '{invalid_type}'").into(),
            ),
            Self::CannotCompareOperands { left_operand_type, right_operand_type  } => (
                "invalid expression".into(),
                format!("cannot compare '{left_operand_type}' to '{right_operand_type}'").into(),
            ),
            Self::CannotChainComparisons => (
                "invalid expression".into(),
                "comparison operators cannot be chained".into(),
            ),
            Self::CannotIndexIntoExpression => (
                "invalid expression".into(),
                "cannot index into an expression, only to variables".into(),
            ),
            Self::ExpectedIntegerExpressionInArrayIndex => (
                "invalid expression".into(),
                "array index must be followed by an integer literal".into(),
            ),
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

            Self::CannotAssignToExpression => (
                "invalid assignment".into(),
                "cannot assign to expression".into(),
            ),
            Self::CannotMutateVariable => (
                "invalid assignment".into(),
                "cannot mutate immutable variable".into(),
            ),
            Self::CannotModifyInplace(typ) => (
                "invalid variable reassignment".into(),
                format!("cannot use inplace assignment operators on `{typ}` values").into(),
            ),
            Self::CannotMutateStringCharacters => (
                "invalid variable reassignment".into(),
                "cannot mutate string characters".into(),
            ),
        };

        return ErrorInfo { error_message, error_cause_message };
    }
}
