// TODO(stefano): rename module to `abstract_syntax_tree` or `ast`
// - Reworked compilation stages:
//    - old:
//        - loading of source code file and line boundaries precalculations
//        - tokenization
//        - abstract syntax tree parsing
//        - compilation of abstract syntax tree
//    - new:
//        - loading of source code file
//        - tokenization and line boundaries calculations
//        - (added) parsing of syntax tree (phantom stage, does not affect other stages for now)
//        - abstract syntax tree parsing
//        - compilation of abstract syntax tree
//        - return the compiled code

use crate::front_end::{src_file::DisplayPosition, tokenizer::{Base, TokenKind, Tokens}, ErrorDisplay};
use back_to_front::offset32;

use super::{
    abstract_syntax_tree::{self as st, ExpressionIndex, NodeIndex, SyntaxTree},
    src_file::SrcCode,
    tokenizer::{ascii, Op, TextIndex},
    Error, ErrorInfo, IntoErrorInfo,
};
use core::{fmt::Display, marker::PhantomData};

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
    fn matches(self, name: &[ascii]) -> bool {
        let self_str = match self {
            Self::I64 => Self::I64_STR,
            Self::Ascii => Self::ASCII_STR,
            Self::Bool => Self::BOOL_STR,
            Self::Str => Self::STR_STR,
        };
        return name == self_str.as_bytes();
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

// #[derive(Clone, Debug, Hash, PartialEq, Eq)]
// pub(crate) enum ExpectedType<'typ> {
//     Exact(&'typ Type),
//     Array,
// }

// impl Display for ExpectedType<'_> {
//     fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
//         return match self {
//             Self::Exact(typ) => write!(f, "{typ}"),
//             Self::Array => write!(f, "any[]"),
//         };
//     }
// }

#[expect(dead_code, reason = "it's in reality created by trasmuting an `Op`")]
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
    #[expect(dead_code, reason = "kept for consistency")]
    #[inline(always)]
    pub(super) fn display_len(self) -> offset32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

#[expect(dead_code, reason = "it's in reality created by trasmuting an `Op`")]
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
    #[expect(dead_code, reason = "kept for consistency")]
    #[inline(always)]
    pub(super) fn display_len(self) -> offset32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

#[expect(dead_code, reason = "it's in reality created by trasmuting an `Op`")]
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
    #[expect(dead_code, reason = "kept for consistency")]
    #[inline(always)]
    pub(super) fn display_len(self) -> offset32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

#[expect(dead_code, reason = "it's in reality created by trasmuting an `Op`")]
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
    #[expect(dead_code, reason = "kept for consistency")]
    #[inline(always)]
    pub(super) fn display_len(self) -> offset32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

#[expect(dead_code, reason = "it's in reality created by trasmuting an `Op`")]
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
    #[expect(dead_code, reason = "kept for consistency")]
    #[inline(always)]
    pub(super) fn display_len(self) -> offset32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

#[expect(dead_code, reason = "it's in reality created by trasmuting an `Op`")]
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
    #[expect(dead_code, reason = "kept for consistency")]
    #[inline(always)]
    pub(super) fn display_len(self) -> offset32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

#[expect(dead_code, reason = "it's in reality created by trasmuting an `Op`")]
#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum AssignmentOperator {
    Equals = st::AssignmentOperator::Equals as u8,

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

    And    = st::AssignmentOperator::And as u8,
    Or     = st::AssignmentOperator::Or as u8,
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
    #[expect(dead_code, reason = "kept for consistency")]
    #[inline(always)]
    pub(super) fn display_len(self) -> offset32 {
        let op: Op = self.into();
        return op.display_len();
    }
}


pub(crate) type ArrayItemsIndex = offset32;
pub(crate) type VariableDefinitionIndex = offset32;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) enum Expression {
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
        literal: TextIndex,
        column: offset32,
    },
    Variable {
        variable: TextIndex,
        column: offset32,
    },
    Array {
        base_type: BaseType,
        items_start: ArrayItemsIndex,
        /// always greater than 0
        items_len: u64,
    },

    Prefix {
        operator: PrefixOperator,
        operator_column: offset32,
        right_operand: ExpressionIndex,
    },
    BooleanPrefix {
        operator: BooleanPrefixOperator,
        operator_column: offset32,
        right_operand: ExpressionIndex,
    },
    Binary {
        left_operand: ExpressionIndex,
        operator: BinaryOperator,
        operator_column: offset32,
        right_operand: ExpressionIndex,
    },
    BooleanBinary {
        left_operand: ExpressionIndex,
        operator: BooleanBinaryOperator,
        operator_column: offset32,
        right_operand: ExpressionIndex,
    },
    Comparison {
        left_operand: ExpressionIndex,
        operator: ComparisonOperator,
        operator_column: offset32,
        right_operand: ExpressionIndex,
    },
    BooleanComparison {
        left_operand: ExpressionIndex,
        operator: BooleanComparisonOperator,
        operator_column: offset32,
        right_operand: ExpressionIndex,
    },

    Index {
        indexed_expression: ExpressionIndex,
        open_square_bracket_column: offset32,
        index_expression: ExpressionIndex,
    },
}

impl Expression {
    pub(crate) fn base_typ(&self, ast: &TypedSyntaxTree<'_, '_, '_>) -> BaseType {
        return match self {
            Self::False { .. } | Self::True { .. } => BaseType::Bool,
            Self::I64 { .. } => BaseType::I64,
            Self::Ascii { .. } => BaseType::Ascii,
            Self::Str { .. } => BaseType::Str,
            Self::Variable { variable, .. } => {
                let variable_definition = &ast.variables[*variable as usize];
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
                let expression = &ast.expressions[*indexed_expression as usize];
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
                let variable_definition = &ast.variables[*variable as usize];
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
                let expression = &ast.expressions[*indexed_expression as usize];
                expression.typ(ast)
            }
        };
    }
}

pub(crate) type ScopeIndex = offset32;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) struct Scope {
    pub(crate) parent: ScopeIndex,
    pub(crate) types: Vec<BaseType>,
    pub(crate) let_variables: Vec<VariableDefinitionIndex>,
    pub(crate) var_variables: Vec<VariableDefinitionIndex>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) struct VariableDefinition {
    name: TextIndex,
    typ: Type,
    initial_value: ExpressionIndex,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) enum Node {
    Expression(ExpressionIndex),

    Print {
        argument: ExpressionIndex,
    },
    Println {
        argument: ExpressionIndex,
    },
    PrintlnNoArg,
    Eprint {
        argument: ExpressionIndex,
    },
    Eprintln {
        argument: ExpressionIndex,
    },
    EprintlnNoArg,

    LetVariableDefinition {
        variable: VariableDefinitionIndex,
    },
    VarVariableDefinition {
        variable: VariableDefinitionIndex,
    },

    Assignment {
        target: ExpressionIndex,
        operator: AssignmentOperator,
        operator_column: offset32,
        new_value: ExpressionIndex,
    },

    Scope {
        raw_nodes_in_scope_count: u32,
    },

    If {
        condition: ExpressionIndex,
        else_ifs_count: offset32,
    },
    IfTrailingElse {
        condition: ExpressionIndex,
        else_ifs_count: offset32,
    },
    ElseIf {
        condition: ExpressionIndex,
    },

    Loop {
        condition: ExpressionIndex,
    },
    DoLoop {
        condition: ExpressionIndex,
    },
    Break,
    Continue,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum ParsedNode {
    Node(Node),
    Scope,
    IfStatement,
    LoopStatement,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TypedSyntaxTree<'syntax_tree, 'tokens: 'syntax_tree, 'code: 'tokens> {
    pub(crate) nodes: Vec<Node>,

    pub(crate) expressions: Vec<Expression>,
    pub(crate) array_items: Vec<ExpressionIndex>,
    pub(crate) variables: Vec<VariableDefinition>,

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
        node_index: &mut NodeIndex,
        indent: usize,
        condition: ExpressionIndex,
    ) -> core::fmt::Result {
        writeln!(f, "{:>indent$}If = if", "")?;
        let if_indent = indent + Self::INDENT_INCREMENT;
        self.info_expression(f, condition, if_indent)?;
        return self.info_node(f, node_index, if_indent);
    }

    fn info_node(
        &self,
        f: &mut core::fmt::Formatter<'_>,
        node_index: &mut NodeIndex,
        indent: usize,
    ) -> core::fmt::Result {
        let node = &self.typed_syntax_tree.nodes[*node_index as usize];
        *node_index += 1;

        #[rustfmt::skip]
        return match node {
            Node::Expression(expression) => {
                self.info_expression(f, *expression, indent)
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
            Node::Assignment { target, operator, new_value, .. } => {
                writeln!(f, "{:>indent$}Assignment", "")?;
                let assignment_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *target, assignment_indent)?;
                writeln!(f, "{:>assignment_indent$}AssignmentOp = {operator}", "")?;
                self.info_expression(f, *new_value, assignment_indent)
            }

            Node::Scope { raw_nodes_in_scope_count } => {
                writeln!(f, "{:>indent$}Scope", "")?;
                let scope_indent = indent + Self::INDENT_INCREMENT;
                writeln!(f, "{:>scope_indent$}OpenCurlyBracket = {{", "")?;

                let after_end_scope_node_index = *node_index + raw_nodes_in_scope_count;
                while *node_index < after_end_scope_node_index {
                    self.info_node(f, node_index, scope_indent)?;
                }
                writeln!(f, "{:>scope_indent$}CloseCurlyBracket = }}", "")
            }

            Node::If { condition, mut else_ifs_count } => {
                self.info_if(f, node_index, indent, *condition)?;
                while else_ifs_count > 0 {
                    else_ifs_count -= 1;
                    self.info_node(f, node_index, indent)?;
                }
                Ok(())
            }
            Node::IfTrailingElse { condition, mut else_ifs_count } => {
                self.info_if(f, node_index, indent, *condition)?;
                while else_ifs_count > 0 {
                    else_ifs_count -= 1;
                    self.info_node(f, node_index, indent)?;
                }
                let else_indent = indent + Self::INDENT_INCREMENT;
                writeln!(f, "{:>indent$}Else = else", "")?;
                self.info_node(f, node_index, else_indent)
            }
            Node::ElseIf { condition } => {
                writeln!(f, "{:>indent$}Else = else", "")?;
                self.info_if(f, node_index, indent, *condition)
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
        expression_index: ExpressionIndex,
        indent: usize,
    ) -> core::fmt::Result {
        let expression_indent = indent + Self::INDENT_INCREMENT;
        let expression = &self.typed_syntax_tree.expressions[expression_index as usize];

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
                let literal_str = &self.tokens.text[*literal as usize];
                writeln!(f, "{:>indent$}Str = {literal_str}", "")
            }
            Expression::Variable { variable, .. } => {
                let identifier_str = self.tokens.text[*variable as usize];
                writeln!(f, "{:>indent$}Identifier = {identifier_str}", "")
            },
            Expression::Array { items_start, items_len, .. } => {
                writeln!(f, "{:>indent$}Array", "")?;

                let items_indent = expression_indent + Self::INDENT_INCREMENT;
                let items_end = *items_start as usize + *items_len as usize;
                let items = &self.typed_syntax_tree.array_items[*items_start as usize..items_end];
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
        variable_index: VariableDefinitionIndex,
        indent: usize,
    ) -> core::fmt::Result {
        let VariableDefinition { name, initial_value, typ } =
            &self.typed_syntax_tree.variables[variable_index as usize];
        let name_str = self.tokens.text[*name as usize];
        writeln!(f, "{:>indent$}Name = {name_str}", "")?;
        writeln!(f, "{:>indent$}Type = {typ}", "")?;
        writeln!(f, "{:>indent$}InitialValue = {typ}", "")?;
        return self.info_expression(f, *initial_value, indent);
    }
}

impl Display for TypedSyntaxTreeDisplay<'_, '_, '_, '_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let mut node_index = 0;
        #[expect(clippy::cast_possible_truncation)]
        while node_index < self.typed_syntax_tree.nodes.len() as NodeIndex {
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
    node_index: NodeIndex,
    syntax_tree: &'syntax_tree SyntaxTree<'tokens, 'code>,

    ast: TypedSyntaxTree<'syntax_tree, 'tokens, 'code>,
    scope: ScopeIndex,
    scopes: Vec<Scope>,
}

impl<'syntax_tree, 'tokens: 'syntax_tree, 'src: 'tokens, 'code: 'src, 'path: 'code>
    Parser<'syntax_tree, 'tokens, 'src, 'code, 'path>
{
    /* NOTE(stefano):
    only parsing until the first error until a fault tolerant parser is developed,
    this is because the first truly relevant error is the first one, which in turn causes a ripple
    effect that propagates to the rest of the parsing, causing subsequent errors to be wrong
    */
    #[expect(clippy::missing_errors_doc, reason = "syntax errors cannot be documented in docs")]
    pub fn parse(
        src: &'src SrcCode<'code, 'path>,
        tokens: &'tokens Tokens<'code>,
        syntax_tree: &'syntax_tree SyntaxTree<'tokens, 'code>,
    ) -> Result<TypedSyntaxTree<'syntax_tree, 'tokens, 'code>, Vec<Error<ErrorKind>>> {
        let mut parser = Self {
            src,
            errors: Vec::new(),

            tokens,
            node_index: 0,
            syntax_tree,

            ast: TypedSyntaxTree {
                nodes: Vec::new(),
                expressions: Vec::new(),
                array_items: Vec::new(),
                variables: Vec::new(),
                _syntax_tree: PhantomData,
            },
            scope: 0,
            scopes: vec![Scope {
                parent: 0,
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
                Ok(ParsedNode::Scope) => continue,
                Ok(ParsedNode::IfStatement) => continue,
                Ok(ParsedNode::LoopStatement) => continue,
                Err(err) => {
                    parser.errors.push(err);

                    // consuming all remaining nodes until the end of the file
                    #[expect(clippy::cast_possible_truncation)]
                    {
                        parser.node_index = parser.syntax_tree.nodes.len() as NodeIndex;
                    }
                    break;
                }
            };
        }

        return if parser.errors.is_empty() { Ok(parser.ast) } else { Err(parser.errors) };
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
struct Peeked<'node> {
    node: &'node st::Node,
    index: NodeIndex,
}

impl<'syntax_tree> Parser<'syntax_tree, '_, '_, '_, '_> {
    fn peek_next_node(&self) -> Option<Peeked<'syntax_tree>> {
        #[expect(clippy::cast_possible_truncation)]
        for next_node_index in self.node_index..self.syntax_tree.nodes.len() as NodeIndex {
            let next_node = &self.syntax_tree.nodes[next_node_index as usize];
            let st::Node::Semicolon { .. } = next_node else {
                return Some(Peeked { node: next_node, index: next_node_index + 1 });
            };
        }

        return None;
    }
}

impl<'syntax_tree> Parser<'syntax_tree, '_, '_, '_, '_> {
    fn any(&mut self, node: &'syntax_tree st::Node) -> Result<ParsedNode, Error<ErrorKind>> {
        return match node {
            st::Node::Expression { expression, .. } => {
                let parsed_expression_index = self.parse_expression(*expression, None)?;
                Ok(ParsedNode::Node(Node::Expression(parsed_expression_index)))
            }

            st::Node::Print { argument, .. } => {
                let parsed_argument_index = self.parse_expression(*argument, None)?;
                Ok(ParsedNode::Node(Node::Print { argument: parsed_argument_index }))
            },
            st::Node::Println { argument, .. } => {
                let parsed_argument_index = self.parse_expression(*argument, None)?;
                Ok(ParsedNode::Node(Node::Println { argument: parsed_argument_index }))
            },
            st::Node::PrintlnNoArg { .. } => {
                Ok(ParsedNode::Node(Node::PrintlnNoArg))
            },
            st::Node::Eprint { argument, .. } => {
                let parsed_argument_index = self.parse_expression(*argument, None)?;
                Ok(ParsedNode::Node(Node::Eprint { argument: parsed_argument_index }))
            },
            st::Node::Eprintln { argument, .. } => {
                let parsed_argument_index = self.parse_expression(*argument, None)?;
                Ok(ParsedNode::Node(Node::Eprintln { argument: parsed_argument_index }))
            },
            st::Node::EprintlnNoArg { .. } => {
                Ok(ParsedNode::Node(Node::EprintlnNoArg))
            },

            st::Node::LetVariableDefinition { variable_definition, .. } => {
                let variable = self.parse_variable(*variable_definition)?;
                self.scopes[self.scope as usize].let_variables.push(variable);
                Ok(ParsedNode::Node(Node::LetVariableDefinition { variable }))
            }
            st::Node::VarVariableDefinition { variable_definition, .. } => {
                let variable = self.parse_variable(*variable_definition)?;
                self.scopes[self.scope as usize].var_variables.push(variable);
                Ok(ParsedNode::Node(Node::VarVariableDefinition { variable }))
            }

            st::Node::Assignment {
                target,
                operator,
                operator_column,
                new_value,
                semicolon_column,
            } => unimplemented!(),

            st::Node::Scope {
                open_curly_bracket_column,
                raw_nodes_in_scope_count,
                close_curly_bracket_column,
            } => unimplemented!(),

            st::Node::If { if_column, condition, else_ifs_count } => unimplemented!(),
            st::Node::IfTrailingElse { if_column, condition, else_ifs_count, else_column } => unimplemented!(),
            st::Node::ElseIf { else_column, if_column, condition } => unimplemented!(),
            st::Node::Loop { loop_column, condition } => unimplemented!(),
            st::Node::DoLoop { do_column, loop_column, condition } => unimplemented!(),
            st::Node::Break { break_column, semicolon_column } => unimplemented!(),
            st::Node::Continue { continue_column, semicolon_column } => unimplemented!(),

            st::Node::Semicolon { column } => self.stray_semicolon(*column),
        };
    }
}

impl Parser<'_, '_, '_, '_, '_> {
    #[expect(clippy::panic, reason = "it's basically a more descriptive panic implementation")]
    #[track_caller]
    fn stray_semicolon(&self, semicolon_colon: offset32) -> ! {
        let DisplayPosition { line, column, display_column } = self.src.display_position(semicolon_colon);
        let line_span = self.src.lines[line as usize - 1];
        let line_text = &self.src.code()[line_span.start as usize..line_span.end as usize];

        let error = ErrorDisplay {
            error_message: "unexpected".into(),
            file: self.src.path(),
            line,
            column,
            absolute_column: semicolon_colon,
            line_text,
            pointers_count: 1,
            pointers_offset: display_column,
            error_cause_message: "should have been skipped in the iteration of tokens".into(),
        };
        panic!("{error}\n");
    }
}

impl TypedSyntaxTree<'_, '_, '_> {
    #[inline]
    fn new_expression(&mut self, expression: Expression) -> ExpressionIndex {
        #[expect(clippy::cast_possible_truncation)]
        let index = self.expressions.len() as ExpressionIndex;
        self.expressions.push(expression);
        return index;
    }

    #[inline]
    fn new_variable(&mut self, variable: VariableDefinition) -> VariableDefinitionIndex {
        #[expect(clippy::cast_possible_truncation)]
        let index = self.variables.len() as VariableDefinitionIndex;
        self.variables.push(variable);
        return index;
    }
}

impl<'syntax_tree> Parser<'syntax_tree, '_, '_, '_, '_> {
    #[expect(clippy::single_call_fn, reason = "readability")]
    const fn parse_positive_binary_i64(literal: &[ascii]) -> Result<i64, ()> {
        const BASE: Base = Base::Binary;
        let mut integer: i64 = 0;
        let mut digit_index = 1 + 1; // 1: leading zero, + 1: base prefix

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

    #[expect(clippy::single_call_fn, reason = "readability")]
    const fn parse_positive_octal_i64(literal: &[ascii]) -> Result<i64, ()> {
        const BASE: Base = Base::Octal;
        let mut integer: i64 = 0;
        let mut digit_index = 1 + 1; // 1: leading zero, + 1: base prefix

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

    #[expect(clippy::single_call_fn, reason = "readability")]
    const fn parse_positive_decimal_i64(literal: &[ascii]) -> Result<i64, ()> {
        const BASE: Base = Base::Decimal;
        let mut integer: i64 = 0;
        let mut digit_index = 0;

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

    #[expect(clippy::single_call_fn, reason = "readability")]
    const fn parse_positive_decimal_prefix_i64(literal: &[ascii]) -> Result<i64, ()> {
        const BASE: Base = Base::Decimal;
        let mut integer: i64 = 0;
        let mut digit_index = 1 + 1; // 1: leading zero, + 1: base prefix

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

    #[expect(clippy::single_call_fn, reason = "readability")]
    const fn parse_positive_hexadecimal_i64(literal: &[ascii]) -> Result<i64, ()> {
        const BASE: Base = Base::Hexadecimal;
        let mut integer: i64 = 0;
        let mut digit_index = 1 + 1; // 1: leading zero, + 1: base prefix

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

    #[expect(clippy::single_call_fn, reason = "readability")]
    const fn parse_negative_binary_i64(literal: &[ascii]) -> Result<i64, ()> {
        const BASE: Base = Base::Binary;
        let mut integer: i64 = 0;
        let mut digit_index = 1 + 1; // 1: leading zero, + 1: base prefix

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

    #[expect(clippy::single_call_fn, reason = "readability")]
    const fn parse_negative_octal_i64(literal: &[ascii]) -> Result<i64, ()> {
        const BASE: Base = Base::Octal;
        let mut integer: i64 = 0;
        let mut digit_index = 1 + 1; // 1: leading zero, + 1: base prefix

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

    #[expect(clippy::single_call_fn, reason = "readability")]
    const fn parse_negative_decimal_i64(literal: &[ascii]) -> Result<i64, ()> {
        const BASE: Base = Base::Decimal;
        let mut integer: i64 = 0;
        let mut digit_index = 0;

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

    #[expect(clippy::single_call_fn, reason = "readability")]
    const fn parse_negative_decimal_prefix_i64(literal: &[ascii]) -> Result<i64, ()> {
        const BASE: Base = Base::Decimal;
        let mut integer: i64 = 0;
        let mut digit_index = 1 + 1; // 1: leading zero, + 1: base prefix

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

    #[expect(clippy::single_call_fn, reason = "readability")]
    const fn parse_negative_hexadecimal_i64(literal: &[ascii]) -> Result<i64, ()> {
        const BASE: Base = Base::Hexadecimal;
        let mut integer: i64 = 0;
        let mut digit_index = 1 + 1; // 1: leading zero, + 1: base prefix

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

    #[expect(clippy::single_call_fn, reason = "readability")]
    const fn parse_ascii(literal: &[ascii]) -> ascii {
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
    fn first_token_display_len(
        &self,
        expression: ExpressionIndex,
    ) -> offset32 {
        let st_expression = &self.syntax_tree.expressions[expression as usize];
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
    fn first_token_column(
        &self,
        expression: ExpressionIndex,
    ) -> offset32 {
        let st_expression = &self.syntax_tree.expressions[expression as usize];
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

    fn resolve_variable(&self, name: TextIndex) -> Option<VariableDefinitionIndex> {
        if let Some(variable) = self.resolve_let_variable(name) {
            return Some(variable);
        }

        return self.resolve_var_variable(name);
    }

    fn resolve_let_variable(&self, name: TextIndex) -> Option<VariableDefinitionIndex> {
        let mut scope_index = self.scope;
        loop {
            let scope = &self.scopes[scope_index as usize];
            for var_index in &scope.let_variables {
                let var = &self.ast.variables[*var_index as usize];
                if var.name == name {
                    return Some(*var_index);
                }
            }

            scope_index = match scope_index {
                0 => return None,
                _ => scope.parent,
            };
        }
    }

    fn resolve_var_variable(&self, name: TextIndex) -> Option<VariableDefinitionIndex> {
        let mut scope_index = self.scope;
        loop {
            let scope = &self.scopes[scope_index as usize];
            for var_index in &scope.var_variables {
                let var = &self.ast.variables[*var_index as usize];
                if var.name == name {
                    return Some(*var_index);
                }
            }

            scope_index = match scope_index {
                0 => return None,
                _ => scope.parent,
            };
        }
    }

    fn resolve_type(&self, name: TextIndex) -> Option<BaseType> {
        let name_text = self.tokens.text[name as usize];
        let mut scope_index = self.scope;
        loop {
            let scope = &self.scopes[scope_index as usize];
            for typ in &scope.types {
                if typ.matches(name_text.as_bytes()) {
                    return Some(*typ);
                }
            }

            scope_index = match scope_index {
                0 => return None,
                _ => scope.parent,
            };
        }
    }

    // IDEA(stefano): provide version with explicit expected type
    fn expression(
        &mut self,
        st_expression_index: ExpressionIndex,
        expected_type: Option<&Type>,
    ) -> Result<Expression, Error<ErrorKind>> {
        let st_expression = &self.syntax_tree.expressions[st_expression_index as usize];
        let expression = match st_expression {
            st::Expression::False { column } => Expression::False { column: *column },
            st::Expression::True { column } => Expression::True { column: *column },
            st::Expression::DecimalInteger { literal, column } => {
                let literal_text = self.tokens.text[*literal as usize].as_bytes();
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
                let literal_text = self.tokens.text[*literal as usize].as_bytes();
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
                let literal_text = self.tokens.text[*literal as usize].as_bytes();
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
                let literal_text = self.tokens.text[*literal as usize].as_bytes();
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
                let literal_text = self.tokens.text[*literal as usize].as_bytes();
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
                let ascii_literal = &self.tokens.text[*literal as usize];
                let ascii_ch = Self::parse_ascii(ascii_literal.as_bytes());
                Expression::Ascii { character: ascii_ch, column: *column }
            }
            st::Expression::Str { literal, column } | st::Expression::RawStr { literal, column } => {
                Expression::Str { literal: *literal, column: *column }
            }
            st::Expression::Identifier { identifier, column }
            | st::Expression::IdentifierStr { identifier, column } => {
                if let Some(_) = self.resolve_type(*identifier) {
                    let name_text = self.tokens.text[*identifier as usize];
                    return Err(Error {
                        kind: ErrorKind::TypeInExpression,
                        col: *column,
                        #[expect(clippy::cast_possible_truncation)]
                        pointers_count: name_text.len() as offset32,
                    });
                }
                let Some(variable) = self.resolve_variable(*identifier) else {
                    let name_text = self.tokens.text[*identifier as usize];
                    return Err(Error {
                        kind: ErrorKind::VariableNotPreviouslyDefined,
                        col: *column,
                        #[expect(clippy::cast_possible_truncation)]
                        pointers_count: name_text.len() as offset32,
                    });
                };

                Expression::Variable { variable, column: *column }
            }

            st::Expression::Array { items_start, items_len, open_square_bracket_column, .. }
            | st::Expression::ArrayTrailingItem { items_start, items_len, open_square_bracket_column, .. } => {
                if *items_len == 0 {
                    return Err(Error {
                        kind: ErrorKind::EmptyArray,
                        col: *open_square_bracket_column,
                        pointers_count: 1,
                    });
                }

                let mut array_items = Vec::<ExpressionIndex>::new();
                let mut item_index = *items_start as usize;

                let first_item = &self.syntax_tree.array_items[item_index];
                item_index += 1;

                let parsed_first_item = self.expression(first_item.expression, expected_type)?;
                let parsed_first_item_type = parsed_first_item.typ(&self.ast);
                if let Type::Array { .. } = parsed_first_item_type {
                    return Err(Error {
                        kind: ErrorKind::NestedArrayNotSupportedYet,
                        col: self.first_token_column(first_item.expression),
                        pointers_count: self.first_token_display_len(first_item.expression),
                    });
                }
                let parsed_first_item_index = self.ast.new_expression(parsed_first_item);
                array_items.push(parsed_first_item_index);

                let expected_array_items_type = if let Some(_) = expected_type {
                    expected_type
                } else {
                    Some(&parsed_first_item_type)
                };

                let items_end = (*items_start + items_len) as usize;
                while item_index < items_end {
                    let item = &self.syntax_tree.array_items[item_index];
                    item_index += 1;

                    let parsed_item = self.expression(item.expression, expected_array_items_type)?;
                    let parsed_item_type = parsed_item.typ(&self.ast);
                    if let Type::Array { .. } = parsed_item_type {
                        return Err(Error {
                            kind: ErrorKind::NestedArrayNotSupportedYet,
                            col: self.first_token_column(item.expression),
                            pointers_count: self.first_token_display_len(item.expression),
                        });
                    }
                    let parsed_item_index = self.ast.new_expression(parsed_item);
                    array_items.push(parsed_item_index);
                }

                self.ast.array_items.extend_from_slice(&array_items);
                Expression::Array {
                    base_type: parsed_first_item_type.base_typ(),
                    items_start: *items_start,
                    items_len: *items_len as u64,
                }
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
                                col: *operator_column,
                                pointers_count: operator.display_len(),
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
                                col: *operator_column,
                                pointers_count: operator.display_len(),
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
                                col: *operator_column,
                                pointers_count: operator.display_len(),
                            });
                        }
                    }
                }
                st::PrefixOperator::Minus
                | st::PrefixOperator::WrappingMinus
                | st::PrefixOperator::SaturatingMinus => {
                    let st_right_operand = &self.syntax_tree.expressions[*right_operand as usize];
                    match st_right_operand {
                        st::Expression::BinaryInteger { literal, column } => {
                            let literal_text = self.tokens.text[*literal as usize];
                            let right_operand_expression = match Self::parse_negative_binary_i64(literal_text.as_bytes()) {
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
                            let literal_text = self.tokens.text[*literal as usize];
                            let right_operand_expression = match Self::parse_negative_octal_i64(literal_text.as_bytes()) {
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
                            let literal_text = self.tokens.text[*literal as usize];
                            let right_operand_expression = match Self::parse_negative_decimal_i64(literal_text.as_bytes()) {
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
                            let literal_text = self.tokens.text[*literal as usize];
                            let right_operand_expression = match Self::parse_negative_decimal_prefix_i64(literal_text.as_bytes()) {
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
                            let literal_text = self.tokens.text[*literal as usize];
                            let right_operand_expression = match Self::parse_negative_hexadecimal_i64(literal_text.as_bytes()) {
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
                                        col: *operator_column,
                                        pointers_count: operator.display_len(),
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
                                    kind: ErrorKind::LeftOperandTypeMismatch(left_operand_type),
                                    col: *operator_column,
                                    pointers_count: operator.display_len(),
                                });
                            }
                        }

                        match right_operand_type {
                            Type::Base(BaseType::Ascii | BaseType::Bool | BaseType::I64) => {}
                            Type::Base(BaseType::Str) | Type::Array { .. } => {
                                return Err(Error {
                                    kind: ErrorKind::RightOperandTypeMismatch(right_operand_type),
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

                    st::BinaryOperator::And
                    | st::BinaryOperator::Or => {
                        match left_operand_type {
                            Type::Base(BaseType::Bool) => {},
                            Type::Base(BaseType::Ascii | BaseType::I64 | BaseType::Str)
                            | Type::Array { .. } => {
                                return Err(Error {
                                    kind: ErrorKind::LeftOperandTypeMismatch(left_operand_type),
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
                                    kind: ErrorKind::RightOperandTypeMismatch(right_operand_type),
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
            let expression_type = expression.typ(&self.ast);
            if *expected_expression_type != expression_type {
                return Err(Error {
                    kind: ErrorKind::TypeMismatch {
                        expected: expected_expression_type.clone(),
                        actual: expression_type,
                    },
                    col: self.first_token_column(st_expression_index),
                    pointers_count: self.first_token_display_len(st_expression_index),
                });
            }
        }

        return Ok(expression);
    }

    #[inline]
    fn parse_expression(
        &mut self,
        st_expression_index: ExpressionIndex,
        expected_type: Option<&Type>,
    ) -> Result<ExpressionIndex, Error<ErrorKind>> {
        let expression = self.expression(st_expression_index, expected_type)?;
        let expression_index = self.ast.new_expression(expression);
        return Ok(expression_index);
    }
}

impl<'syntax_tree> Parser<'syntax_tree, '_, '_, '_, '_> {
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
        }: &'syntax_tree st::TypeAnnotation,
    ) -> Result<Type, Error<ErrorKind>> {
        if let Some(_) = self.resolve_variable(*type_name) {
            let type_name_text = self.tokens.text[*type_name as usize];
            return Err(Error {
                kind: ErrorKind::VariableInTypeAnnotation,
                col: *type_name_column,
                #[expect(clippy::cast_possible_truncation)]
                pointers_count: type_name_text.len() as offset32,
            });
        }
        let Some(base_type) = self.resolve_type(*type_name) else {
            let type_name_text = self.tokens.text[*type_name as usize];
            return Err(Error {
                kind: ErrorKind::TypeNotPreviouslyDefined,
                col: *type_name_column,
                #[expect(clippy::cast_possible_truncation)]
                pointers_count: type_name_text.len() as offset32,
            });
        };

        let array_dimensions_end = array_dimensions_start + array_dimensions_len;
        let array_dimensions = &self.syntax_tree.array_dimensions[
            *array_dimensions_start as usize..array_dimensions_end as usize
        ];
        let mut array_dimensions_iter = array_dimensions.iter();
        let Some(first_dimension) = array_dimensions_iter.next() else {
            return Ok(Type::Base(base_type));
        };

        let dimension_expression = first_dimension.dimension_expression;
        let first_dimension_expression = &self.expression(dimension_expression, None)?;
        let Expression::I64 { value: len, column } = first_dimension_expression else {
            return Err(Error {
                kind: ErrorKind::ExpectedIntegerLiteralInArrayType,
                col: self.first_token_column(dimension_expression),
                pointers_count: self.first_token_display_len(dimension_expression),
            });
        };
        if *len < 0 {
            return Err(Error {
                kind: ErrorKind::ArrayOfNegativeLength,
                col: *column,
                pointers_count: self.first_token_display_len(dimension_expression),
            });
        }
        if *len == 0 {
            return Err(Error {
                kind: ErrorKind::ArrayOfZeroElements,
                col: *column,
                pointers_count: self.first_token_display_len(dimension_expression),
            });
        }

        for st::ArrayDimension {
            dimension_expression: ignored_dimension_expression,
            ..
        } in array_dimensions_iter {
            let dimension_expression_expression = &self.expression(*ignored_dimension_expression, None)?;
            let Expression::I64 { value: ignored_len, column: ignored_column } = dimension_expression_expression else {
                return Err(Error {
                    kind: ErrorKind::ExpectedIntegerLiteralInArrayType,
                    col: self.first_token_column(*ignored_dimension_expression),
                    pointers_count: self.first_token_display_len(*ignored_dimension_expression),
                });
            };
            if *ignored_len < 0 {
                return Err(Error {
                    kind: ErrorKind::ArrayOfNegativeLength,
                    col: *ignored_column,
                    pointers_count: self.first_token_display_len(*ignored_dimension_expression),
                });
            }
            if *ignored_len == 0 {
                return Err(Error {
                    kind: ErrorKind::ArrayOfZeroElements,
                    col: *ignored_column,
                    pointers_count: self.first_token_display_len(*ignored_dimension_expression),
                });
            }
        }

        return Ok(Type::Array {
            base_type,
            #[expect(clippy::cast_sign_loss)]
            len: *len as u64,
        });
    }

    fn parse_variable(
        &mut self,
        variable_definition: st::VariableDefinitionIndex,
    ) -> Result<VariableDefinitionIndex, Error<ErrorKind>> {
        let st::VariableDefinition {
            name,
            name_column,
            type_annotation,
            initial_value,
        } = &self.syntax_tree.variable_definitions[variable_definition as usize];

        if let Some(_) = self.resolve_variable(*name) {
            let name_text = self.tokens.text[*name as usize];
            return Err(Error {
                kind: ErrorKind::VariableAlreadyDefined,
                col: *name_column,
                #[expect(clippy::cast_possible_truncation)]
                pointers_count: name_text.len() as offset32,
            });
        }
        if let Some(_) = self.resolve_type(*name) {
            let name_text = self.tokens.text[*name as usize];
            return Err(Error {
                kind: ErrorKind::TypeInVariableName,
                col: *name_column,
                #[expect(clippy::cast_possible_truncation)]
                pointers_count: name_text.len() as offset32,
            });
        }

        let Some(st::InitialValue { expression, .. }) = initial_value else {
            let Some(type_annotation_inner) = type_annotation else {
                let name_text = self.tokens.text[*name as usize];
                return Err(Error {
                    kind: ErrorKind::CannotInferTypeOfVariable,
                    col: *name_column,
                    #[expect(clippy::cast_possible_truncation)]
                    pointers_count: name_text.len() as offset32,
                });
            };
            let _typ = self.parse_type_annotation(type_annotation_inner)?;
            let name_text = self.tokens.text[*name as usize];
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

    VariableNotPreviouslyDefined,
    VariableAlreadyDefined,
    VariableInTypeAnnotation,
    TypeNotPreviouslyDefined,
    ExpectedIntegerLiteralInArrayType,
    ArrayOfNegativeLength,
    ArrayOfZeroElements,
    TypeInExpression,
    TypeInVariableName,
    CannotInferTypeOfVariable,
    VariablesMustBeInitialized,

    MinusZeroInteger,

    EmptyArray,
    NestedArrayNotSupportedYet,

    CannotTakeLenOf(Type),
    CannotTakeAbsoluteValueOf(Type),
    CannotNegate(Type),
    CannotInvert(Type),
    LeftOperandTypeMismatch(Type),
    RightOperandTypeMismatch(Type),
    CannotCompareOperands { left_operand_type: Type, right_operand_type: Type },
    CannotChainComparisons,
    CannotIndexIntoExpression,
    CannotIndexNonArrayLikeType(Type),
    ExpectedIntegerExpressionInArrayIndex,
    TypeMismatch { expected: Type, actual: Type },
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
            Self::ArrayOfZeroElements => (
                "invalid array".into(),
                "arrays of zero items are not allowed yet".into(),
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

            Self::MinusZeroInteger => (
                "invalid integer literal".into(),
                "-0 is not a valid two's complement integer".into(),
            ),

            Self::EmptyArray => (
                "invalid array".into(),
                "empty arrays are not allowed yet".into()
            ),
            Self::NestedArrayNotSupportedYet => (
                "invalid array element".into(),
                "nested arrays are not supported yet".into(),
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
            Self::LeftOperandTypeMismatch(invalid_type) => (
                "invalid expression".into(),
                format!("cannot be preceded by '{invalid_type}'").into(),
            ),
            Self::RightOperandTypeMismatch(invalid_type) => (
                "invalid expression".into(),
                format!("cannot be followed by '{invalid_type}'").into(),
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
                "cannot index into an expression".into(),
            ),
            Self::ExpectedIntegerExpressionInArrayIndex => (
                "invalid array index".into(),
                "must be followed by an integer literal".into(),
            ),
            Self::CannotIndexNonArrayLikeType(non_indexable_type) => (
                "invalid expression".into(),
                format!("cannot index into a value of type '{non_indexable_type}'").into(),
            ),
            Self::TypeMismatch { expected, actual } => (
                "invalid array element".into(),
                format!("expected expression of type '{expected}', but got '{actual}'").into(),
            ),
        };

        return ErrorInfo { error_message, error_cause_message };
    }
}
