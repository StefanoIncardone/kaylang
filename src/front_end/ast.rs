// IDEA(stefano): fuse tokenization and parsing, making the tokenizer a generator of tokens
// TODO(stefano): multidimensional arrays
// REMOVE(stefano, 0.7.0): remove unitialized variables initialized to default values

use back_to_front::offset32;

use super::{
    src_file::{Position, SrcCode},
    tokenizer::{ascii, Base, Op, Token, TokenIndex, TokenKind, Tokens},
    Error, ErrorInfo, IntoErrorInfo,
};
use core::fmt::{Debug, Display};

pub(crate) trait TypeOf {
    fn typ(&self) -> Type;
}

pub(crate) trait BaseTypeOf: TypeOf {
    fn base_typ(&self) -> BaseType;
}

pub(crate) trait SizeOf {
    fn size(&self) -> usize;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BaseType {
    I64,
    Ascii,
    Bool,
    Str,
}

impl Display for BaseType {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return match self {
            Self::I64 => write!(f, "i64"),
            Self::Ascii => write!(f, "ascii"),
            Self::Bool => write!(f, "bool"),
            Self::Str => write!(f, "str"),
        };
    }
}

impl TypeOf for BaseType {
    #[inline(always)]
    fn typ(&self) -> Type {
        return Type::Base(*self);
    }
}

impl BaseTypeOf for BaseType {
    #[inline(always)]
    fn base_typ(&self) -> BaseType {
        return *self;
    }
}

impl SizeOf for BaseType {
    #[inline]
    fn size(&self) -> usize {
        return match self {
            Self::I64 => size_of::<i64>(),
            Self::Ascii => size_of::<ascii>(),
            Self::Bool => size_of::<bool>(),
            Self::Str => size_of::<u64>() + size_of::<*const ascii>(),
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Base(BaseType),
    // TODO(stefano): enforce a max length
    Array {
        base_type: BaseType,
        /// always greater than 0, i.e: arrays always contain at least 1 item
        len: u64,
    },
}

impl Display for Type {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return match self {
            Self::Base(typ) => write!(f, "{typ}"),
            Self::Array { base_type, len } => {
                debug_assert!(*len > 0, "arrays of 0 items are not allowed");
                write!(f, "{base_type}[{len}]")
            }
        };
    }
}

impl TypeOf for Type {
    #[inline(always)]
    fn typ(&self) -> Type {
        return *self;
    }
}

impl BaseTypeOf for Type {
    #[inline]
    fn base_typ(&self) -> BaseType {
        return match self {
            Self::Base(typ) => *typ,
            Self::Array { base_type, .. } => *base_type,
        };
    }
}

impl SizeOf for Type {
    #[inline]
    fn size(&self) -> usize {
        return match self {
            Self::Base(typ) => typ.size(),
            Self::Array { base_type, len } => {
                debug_assert!(*len > 0, "arrays of 0 items are not allowed");
                #[expect(clippy::cast_possible_truncation)]
                {
                    base_type.size() * *len as usize
                }
            }
        };
    }
}

#[expect(dead_code, reason = "it's in reality created by trasmuting an `Op`")]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum UnaryOp {
    Len = Op::Len as u8,
    Not = Op::Not as u8,

    Plus = Op::Plus as u8,
    WrappingPlus = Op::WrappingPlus as u8,
    SaturatingPlus = Op::SaturatingPlus as u8,

    Minus = Op::Minus as u8,
    WrappingMinus = Op::WrappingMinus as u8,
    SaturatingMinus = Op::SaturatingMinus as u8,
}

impl Into<UnaryOp> for Op {
    #[inline(always)]
    fn into(self) -> UnaryOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Op> for UnaryOp {
    #[inline(always)]
    fn into(self) -> Op {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Display for UnaryOp {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl TypeOf for UnaryOp {
    #[inline(always)]
    fn typ(&self) -> Type {
        return Type::Base(self.base_typ());
    }
}

impl BaseTypeOf for UnaryOp {
    #[inline(always)]
    fn base_typ(&self) -> BaseType {
        return BaseType::I64;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum BooleanUnaryOp {
    Not = Op::Not as u8,
}

impl Into<BooleanUnaryOp> for Op {
    #[inline(always)]
    fn into(self) -> BooleanUnaryOp {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Op> for BooleanUnaryOp {
    #[inline(always)]
    fn into(self) -> Op {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Display for BooleanUnaryOp {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl TypeOf for BooleanUnaryOp {
    #[inline(always)]
    fn typ(&self) -> Type {
        return Type::Base(self.base_typ());
    }
}

impl BaseTypeOf for BooleanUnaryOp {
    #[inline(always)]
    fn base_typ(&self) -> BaseType {
        return BaseType::Bool;
    }
}

#[expect(dead_code, reason = "it's in reality created by trasmuting an `Op`")]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum BinaryOp {
    Pow = Op::Pow as u8,
    WrappingPow = Op::WrappingPow as u8,
    SaturatingPow = Op::SaturatingPow as u8,

    Times = Op::Times as u8,
    WrappingTimes = Op::WrappingTimes as u8,
    SaturatingTimes = Op::SaturatingTimes as u8,

    Divide = Op::Divide as u8,
    WrappingDivide = Op::WrappingDivide as u8,
    SaturatingDivide = Op::SaturatingDivide as u8,

    Remainder = Op::Remainder as u8,

    Plus = Op::Plus as u8,
    WrappingPlus = Op::WrappingPlus as u8,
    SaturatingPlus = Op::SaturatingPlus as u8,

    Minus = Op::Minus as u8,
    WrappingMinus = Op::WrappingMinus as u8,
    SaturatingMinus = Op::SaturatingMinus as u8,

    LeftShift = Op::LeftShift as u8,
    WrappingLeftShift = Op::WrappingLeftShift as u8,
    SaturatingLeftShift = Op::SaturatingLeftShift as u8,

    RightShift = Op::RightShift as u8,

    LeftRotate = Op::LeftRotate as u8,
    RightRotate = Op::RightRotate as u8,

    BitAnd = Op::BitAnd as u8,
    BitXor = Op::BitXor as u8,
    BitOr = Op::BitOr as u8,
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

impl Display for BinaryOp {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl TypeOf for BinaryOp {
    #[inline(always)]
    fn typ(&self) -> Type {
        return Type::Base(self.base_typ());
    }
}

impl BaseTypeOf for BinaryOp {
    #[inline(always)]
    fn base_typ(&self) -> BaseType {
        return BaseType::I64;
    }
}

#[expect(dead_code, reason = "it's in reality created by trasmuting an `Op`")]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum BooleanBinaryOp {
    And = Op::And as u8,
    Or = Op::Or as u8,
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

impl Display for BooleanBinaryOp {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl TypeOf for BooleanBinaryOp {
    #[inline(always)]
    fn typ(&self) -> Type {
        return Type::Base(self.base_typ());
    }
}

impl BaseTypeOf for BooleanBinaryOp {
    #[inline(always)]
    fn base_typ(&self) -> BaseType {
        return BaseType::Bool;
    }
}

#[expect(dead_code, reason = "it's in reality created by trasmuting an `Op`")]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum ComparisonOp {
    Compare = Op::Compare as u8,
    EqualsEquals = Op::EqualsEquals as u8,
    NotEquals = Op::NotEquals as u8,
    Greater = Op::Greater as u8,
    GreaterOrEquals = Op::GreaterOrEquals as u8,
    Less = Op::Less as u8,
    LessOrEquals = Op::LessOrEquals as u8,
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

impl Display for ComparisonOp {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl TypeOf for ComparisonOp {
    #[inline(always)]
    fn typ(&self) -> Type {
        return Type::Base(self.base_typ());
    }
}

impl BaseTypeOf for ComparisonOp {
    #[inline]
    fn base_typ(&self) -> BaseType {
        return match self {
            Self::Compare => BaseType::I64,
            Self::EqualsEquals
            | Self::NotEquals
            | Self::Greater
            | Self::GreaterOrEquals
            | Self::Less
            | Self::LessOrEquals => BaseType::Bool,
        };
    }
}

#[expect(dead_code, reason = "it's in reality created by trasmuting an `Op`")]
#[rustfmt::skip]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum AssignmentOp {
    Equals = Op::Equals as u8,

    Pow           = Op::PowEquals as u8,
    WrappingPow   = Op::WrappingPowEquals as u8,
    SaturatingPow = Op::SaturatingPowEquals as u8,

    Times           = Op::TimesEquals as u8,
    WrappingTimes   = Op::WrappingTimesEquals as u8,
    SaturatingTimes = Op::SaturatingTimesEquals as u8,

    Divide           = Op::DivideEquals as u8,
    WrappingDivide   = Op::WrappingDivideEquals as u8,
    SaturatingDivide = Op::SaturatingDivideEquals as u8,

    Remainder = Op::RemainderEquals as u8,

    Plus           = Op::PlusEquals as u8,
    WrappingPlus   = Op::WrappingPlusEquals as u8,
    SaturatingPlus = Op::SaturatingPlusEquals as u8,

    Minus           = Op::MinusEquals as u8,
    WrappingMinus   = Op::WrappingMinusEquals as u8,
    SaturatingMinus = Op::SaturatingMinusEquals as u8,

    LeftShift           = Op::LeftShiftEquals as u8,
    WrappingLeftShift   = Op::WrappingLeftShiftEquals as u8,
    SaturatingLeftShift = Op::SaturatingLeftShiftEquals as u8,

    RightShift = Op::RightShiftEquals as u8,

    LeftRotate  = Op::LeftRotateEquals as u8,
    RightRotate = Op::RightRotateEquals as u8,

    BitAnd = Op::BitAndEquals as u8,
    BitXor = Op::BitXorEquals as u8,
    BitOr  = Op::BitOrEquals as u8,

    And    = Op::AndEquals as u8,
    Or     = Op::OrEquals as u8,
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

impl Display for AssignmentOp {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

type StringLabel = offset32;
type VariableIndex = offset32;
type IfIndex = offset32;
type LoopIndex = offset32;
type ExpressionIndex = offset32;
pub(crate) type ScopeIndex = offset32;

#[derive(Debug, Clone)]
pub(crate) enum Expression {
    False,
    True,
    I64(i64),
    Ascii(ascii),
    Str {
        label: StringLabel,
    },
    Array {
        base_type: BaseType,
        /// arrays always contain at least 1 item
        items: Vec<Expression>, // TODO(stefano): flatten into a Vec<ExpressionIndex>
    },

    Parenthesis {
        typ: Type,
        expression_index: ExpressionIndex,
    },

    Unary {
        op: UnaryOp,
        op_col: offset32,
        operand_index: ExpressionIndex,
    },
    BooleanUnary {
        op: BooleanUnaryOp,
        operand_index: ExpressionIndex,
    },
    Binary {
        lhs_index: ExpressionIndex,
        op: BinaryOp,
        op_col: offset32,
        rhs_index: ExpressionIndex,
    },
    BooleanBinary {
        lhs_index: ExpressionIndex,
        op: BooleanBinaryOp,
        rhs_index: ExpressionIndex,
    },
    Comparison {
        lhs_index: ExpressionIndex,
        op: ComparisonOp,
        rhs_index: ExpressionIndex,
    },
    ArrayIndex {
        base_type: BaseType,
        indexable_index: ExpressionIndex,
        bracket_col: offset32,
        index_expression_index: ExpressionIndex,
    },

    Variable {
        typ: Type,
        variable_index: VariableIndex,
    },

    Temporary {
        typ: Type,
        temporary_value_index: ExpressionIndex,
    },
}

impl TypeOf for Expression {
    fn typ(&self) -> Type {
        return match self {
            Self::False | Self::True => Type::Base(BaseType::Bool),
            Self::I64(_) => Type::Base(BaseType::I64),
            Self::Ascii(_) => Type::Base(BaseType::Ascii),
            Self::Str { .. } => Type::Base(BaseType::Str),
            Self::Array { base_type, items } => {
                debug_assert!(items.len() > 0, "arrays of 0 items are not allowed");
                Type::Array { base_type: *base_type, len: items.len() as u64 }
            }
            Self::Parenthesis { typ, .. } => *typ,
            Self::Temporary { typ, .. } => *typ,
            Self::Unary { op, .. } => op.typ(),
            Self::BooleanUnary { op, .. } => op.typ(),
            Self::Binary { op, .. } => op.typ(),
            Self::BooleanBinary { op, .. } => op.typ(),
            Self::Comparison { op, .. } => op.typ(),
            Self::Variable { typ, .. } => *typ,
            Self::ArrayIndex { base_type, .. } => Type::Base(*base_type),
        };
    }
}

impl<'ast, 'code: 'ast> Expression {
    #[inline(always)]
    pub(crate) const fn display(
        &'ast self,
        ast: &'ast Ast<'code>,
    ) -> ExpressionDisplay<'ast, 'code> {
        return ExpressionDisplay { ast, expr: self };
    }
}

pub(crate) struct ExpressionDisplay<'ast, 'code: 'ast> {
    ast: &'ast Ast<'code>,
    expr: &'ast Expression,
}

impl<'ast, 'code: 'ast> ExpressionDisplay<'ast, 'code> {
    pub(crate) fn display(
        &self,
        f: &mut core::fmt::Formatter<'_>,
        expr: &'ast Expression,
    ) -> core::fmt::Result {
        return match expr {
            Expression::False => write!(f, "false"),
            Expression::True => write!(f, "true"),
            Expression::I64(integer) => write!(f, "{integer}"),
            Expression::Ascii(code) => write!(f, "'{}'", code.escape_ascii()),
            Expression::Str { label, .. } => write!(f, "str_{label}"),
            Expression::Array { items, .. } => {
                write!(f, "[")?;
                let mut items_iter = items.iter();
                let Some(last_item) = items_iter.next_back() else {
                    unreachable!("arrays should always contain at least 1 item");
                };

                for item in items_iter {
                    self.display(f, item)?;
                    write!(f, ", ")?;
                }

                self.display(f, last_item)?;
                write!(f, "]")
            }
            Expression::Parenthesis { expression_index, .. } => {
                let inner = &self.ast.expressions[*expression_index as usize];
                write!(f, "(")?;
                self.display(f, inner)?;
                write!(f, ")")
            }
            Expression::Unary { op: len @ UnaryOp::Len, operand_index, .. } => {
                let operand = &self.ast.expressions[*operand_index as usize];
                write!(f, "{len} ")?;
                self.display(f, operand)
            }
            Expression::Unary { op, operand_index, .. } => {
                let operand = &self.ast.expressions[*operand_index as usize];
                write!(f, "{op}")?;
                self.display(f, operand)
            }
            Expression::BooleanUnary { op, operand_index } => {
                let operand = &self.ast.expressions[*operand_index as usize];
                write!(f, "{op}")?;
                self.display(f, operand)
            }
            Expression::Binary { lhs_index, op, rhs_index, .. } => {
                let lhs = &self.ast.expressions[*lhs_index as usize];
                let rhs = &self.ast.expressions[*rhs_index as usize];
                self.display(f, lhs)?;
                write!(f, " {op} ")?;
                self.display(f, rhs)
            }
            Expression::BooleanBinary { lhs_index, op, rhs_index } => {
                let lhs = &self.ast.expressions[*lhs_index as usize];
                let rhs = &self.ast.expressions[*rhs_index as usize];
                self.display(f, lhs)?;
                write!(f, " {op} ")?;
                self.display(f, rhs)
            }
            Expression::Comparison { lhs_index, op, rhs_index } => {
                let lhs = &self.ast.expressions[*lhs_index as usize];
                let rhs = &self.ast.expressions[*rhs_index as usize];
                self.display(f, lhs)?;
                write!(f, " {op} ")?;
                self.display(f, rhs)
            }
            Expression::ArrayIndex { indexable_index, index_expression_index, .. } => {
                let indexable = &self.ast.expressions[*indexable_index as usize];
                let index_expression = &self.ast.expressions[*index_expression_index as usize];
                self.display(f, indexable)?;
                write!(f, "[")?;
                self.display(f, index_expression)?;
                write!(f, "]")
            }
            Expression::Temporary { temporary_value_index, .. } => {
                let temp = &self.ast.temporaries[*temporary_value_index as usize];
                self.display(f, temp)
            }
            Expression::Variable { variable_index, .. } => {
                let variable = &self.ast.variables[*variable_index as usize];
                let variable_name_str = unsafe { core::str::from_utf8_unchecked(variable.name) };
                write!(f, "{variable_name_str}")
            }
        };
    }
}

impl Display for ExpressionDisplay<'_, '_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return self.display(f, self.expr);
    }
}

#[derive(Debug, Clone)]
pub(crate) struct IfStatement {
    pub(crate) condition: Expression,
    pub(crate) statement: Node,
}

pub(crate) type Loop = IfStatement;

#[derive(Debug, Clone)]
pub(crate) struct If {
    pub(crate) ifs: Vec<IfStatement>,
    pub(crate) els: Option<Node>,
}

#[derive(Debug, Clone)]
pub(crate) enum Node {
    Expression(Expression),

    Print(Expression),
    Println(Option<Expression>),
    Eprint(Expression),
    Eprintln(Option<Expression>),

    If(IfIndex),

    Loop(LoopIndex),
    DoLoop(LoopIndex),
    Break,
    Continue,

    Definition { var_index: VariableIndex },
    Reassignment { target: Expression, op: AssignmentOp, op_col: offset32, new_value: Expression },

    Scope { index: ScopeIndex },

    // should never appear in the ast
    Semicolon,
    ScopeEnd,
}

#[derive(Debug, Clone)]
pub(crate) struct Scope {
    pub(crate) parent: ScopeIndex,
    pub(crate) base_types: Vec<BaseType>,
    pub(crate) let_variables: Vec<VariableIndex>,
    pub(crate) var_variables: Vec<VariableIndex>,
}

#[derive(Debug, Clone)]
pub(crate) struct Variable<'code> {
    pub(crate) name: &'code [ascii],
    pub(crate) value: Expression,
}

/* NOTE(stefano):
this is in reality closer to an intermediate representation than to an AST
TODO: introduce other representation before and after this Ast
*/
#[derive(Debug)]
pub struct Ast<'code> {
    pub(crate) nodes: Vec<Vec<Node>>,

    pub(crate) ifs: Vec<If>,
    pub(crate) loops: Vec<Loop>,

    pub(crate) expressions: Vec<Expression>,
    pub(crate) temporaries: Vec<Expression>,
    pub(crate) variables: Vec<Variable<'code>>,

    pub(crate) string_labels: Vec<(u32, &'code str)>,
    pub(crate) raw_string_labels: Vec<(u32, &'code str)>,
}

#[derive(Debug)]
pub struct Parser<'tokens, 'src: 'tokens, 'code: 'src, 'path: 'code> {
    src: &'src SrcCode<'code, 'path>,
    errors: Vec<Error<ErrorKind>>,

    token: TokenIndex,
    tokens: &'tokens Tokens<'code>,

    loop_depth: u32,
    string_label: u32,
    scope: ScopeIndex,
    scopes: Vec<Scope>,
    ast: Ast<'code>,
}

impl<'tokens, 'src: 'tokens, 'code: 'src, 'path: 'code> Parser<'tokens, 'src, 'code, 'path> {
    // IDEA(stefano): move into freestanding function
    #[expect(clippy::missing_errors_doc, reason = "syntax errors cannot be documented in docs")]
    pub fn parse(
        src: &'src SrcCode<'code, 'path>,
        tokens: &'tokens Tokens<'code>,
    ) -> Result<Ast<'code>, Vec<Error<ErrorKind>>> {
        let ast = Ast {
            nodes: vec![vec![]],

            ifs: Vec::new(),
            loops: Vec::new(),

            expressions: Vec::new(),
            temporaries: Vec::new(),
            variables: Vec::new(),

            string_labels: Vec::new(),
            raw_string_labels: Vec::new(),
        };

        if tokens.tokens.is_empty() {
            return Ok(ast);
        }

        // skipping to the first non-comment token
        let mut token: TokenIndex = 0;
        #[expect(clippy::cast_possible_truncation)]
        while token < tokens.tokens.len() as TokenIndex {
            let current = tokens.tokens[token as usize];
            let (TokenKind::Comment(_) | TokenKind::BlockComment(_)) = current.kind else {
                break;
            };

            token += 1;
        }

        let mut parser = Parser {
            src,
            errors: Vec::new(),
            token,
            tokens,
            loop_depth: 0,
            string_label: 0,
            scope: 0,
            scopes: vec![Scope {
                parent: 0,
                base_types: vec![BaseType::I64, BaseType::Ascii, BaseType::Bool, BaseType::Str],
                let_variables: Vec::new(),
                var_variables: Vec::new(),
            }],
            ast,
        };

        parser.scope();

        return if parser.errors.is_empty() { Ok(parser.ast) } else { Err(parser.errors) };
    }
}

// parsing of statements
impl Parser<'_, '_, '_, '_> {
    fn semicolon(&mut self) -> Result<(), Error<ErrorKind>> {
        let semicolon_token = self.current_token(Expected::Semicolon)?;
        let TokenKind::SemiColon = semicolon_token.kind else {
            let previous_token = self.peek_previous_token();
            return Err(Error {
                kind: ErrorKind::MissingSemicolon,
                col: previous_token.col,
                pointers_count: previous_token.kind.display_len(self.tokens),
            });
        };

        _ = self.next_token();
        return Ok(());
    }

    /* NOTE(stefano):
    only parsing until the first error until a fault tolerant parser is developed,
    this is because the first truly relevant error is the first one, which in turn causes a ripple
    effect that propagates to the rest of the parsing, causing subsequent errors to be wrong
    */
    fn scope(&mut self) {
        while let Some(token) = self.tokens.tokens.get(self.token as usize) {
            match self.any(*token) {
                // skip to the next token after a semicolon
                Ok(Node::Semicolon) => continue,
                Ok(Node::ScopeEnd) => break,
                Ok(node) => self.ast.nodes[self.scope as usize].push(node),
                Err(err) => {
                    self.errors.push(err);

                    // consuming all remaining tokens until the end of the file
                    #[expect(clippy::cast_possible_truncation)]
                    {
                        self.token = self.tokens.tokens.len() as TokenIndex;
                    }
                    break;
                }
            }
        }
    }

    fn statement(&mut self, token: Token) -> Result<Node, Error<ErrorKind>> {
        return match token.kind {
            TokenKind::False
            | TokenKind::True
            | TokenKind::BinaryInteger(_)
            | TokenKind::OctalInteger(_)
            | TokenKind::DecimalInteger(_)
            | TokenKind::HexadecimalInteger(_)
            | TokenKind::Ascii(_)
            | TokenKind::Str(_)
            | TokenKind::RawStr(_)
            | TokenKind::Identifier(_)
            | TokenKind::IdentifierStr(_)
            | TokenKind::OpenRoundBracket
            | TokenKind::OpenSquareBracket
            | TokenKind::Op(
                Op::Len
                | Op::Not
                | Op::Plus
                | Op::WrappingPlus
                | Op::SaturatingPlus
                | Op::Minus
                | Op::WrappingMinus
                | Op::SaturatingMinus,
            ) => {
                let mut expression = self.expression()?;

                // NOTE(stefano): going backwards and then forwards again to skip comments
                /* FIX(stefano):
                migrate iteration to using the rust model, such that calling next
                would return the current item and then advance.
                */
                self.token -= 1;
                let after_expression_token = self.next_token_bounded(Expected::Semicolon)?;
                match after_expression_token.kind {
                    TokenKind::SemiColon => {
                        if let Expression::Array { .. } = expression {
                            #[expect(clippy::cast_possible_truncation)]
                            let temporary_value_index =
                                self.ast.temporaries.len() as ExpressionIndex;
                            let expression_type = expression.typ();
                            self.ast.temporaries.push(expression);
                            expression = Expression::Temporary {
                                typ: expression_type,
                                temporary_value_index,
                            };
                        }

                        _ = self.next_token();
                        Ok(Node::Expression(expression))
                    }
                    TokenKind::Op(
                        op @ (Op::Equals
                        | Op::PowEquals
                        | Op::WrappingPowEquals
                        | Op::SaturatingPowEquals
                        | Op::TimesEquals
                        | Op::WrappingTimesEquals
                        | Op::SaturatingTimesEquals
                        | Op::DivideEquals
                        | Op::WrappingDivideEquals
                        | Op::SaturatingDivideEquals
                        | Op::RemainderEquals
                        | Op::PlusEquals
                        | Op::WrappingPlusEquals
                        | Op::SaturatingPlusEquals
                        | Op::MinusEquals
                        | Op::WrappingMinusEquals
                        | Op::SaturatingMinusEquals
                        | Op::LeftShiftEquals
                        | Op::WrappingLeftShiftEquals
                        | Op::SaturatingLeftShiftEquals
                        | Op::RightShiftEquals
                        | Op::LeftRotateEquals
                        | Op::RightRotateEquals
                        | Op::BitAndEquals
                        | Op::BitXorEquals
                        | Op::BitOrEquals
                        | Op::AndEquals
                        | Op::OrEquals),
                    ) => {
                        let assignment_op: AssignmentOp = op.into();
                        let reassignment = self.reassignment(
                            expression,
                            token,
                            assignment_op,
                            after_expression_token,
                        )?;

                        self.semicolon()?;
                        Ok(reassignment)
                    }

                    TokenKind::Op(
                        Op::Len
                        | Op::Not
                        | Op::Pow
                        | Op::WrappingPow
                        | Op::SaturatingPow
                        | Op::Times
                        | Op::WrappingTimes
                        | Op::SaturatingTimes
                        | Op::Divide
                        | Op::WrappingDivide
                        | Op::SaturatingDivide
                        | Op::Remainder
                        | Op::Plus
                        | Op::WrappingPlus
                        | Op::SaturatingPlus
                        | Op::Minus
                        | Op::WrappingMinus
                        | Op::SaturatingMinus
                        | Op::LeftShift
                        | Op::WrappingLeftShift
                        | Op::SaturatingLeftShift
                        | Op::RightShift
                        | Op::LeftRotate
                        | Op::RightRotate
                        | Op::BitAnd
                        | Op::BitXor
                        | Op::BitOr
                        | Op::And
                        | Op::Or
                        | Op::Compare
                        | Op::EqualsEquals
                        | Op::NotEquals
                        | Op::Greater
                        | Op::GreaterOrEquals
                        | Op::Less
                        | Op::LessOrEquals,
                    ) => {
                        let previous_token = self.peek_previous_token();
                        Err(Error {
                            kind: ErrorKind::MissingSemicolon,
                            col: previous_token.col,
                            pointers_count: previous_token.kind.display_len(self.tokens),
                        })
                    }

                    TokenKind::OpenRoundBracket
                    | TokenKind::CloseRoundBracket
                    | TokenKind::OpenSquareBracket
                    | TokenKind::CloseSquareBracket
                    | TokenKind::OpenCurlyBracket
                    | TokenKind::CloseCurlyBracket
                    | TokenKind::Colon
                    | TokenKind::Comma
                    | TokenKind::False
                    | TokenKind::True
                    | TokenKind::BinaryInteger(_)
                    | TokenKind::OctalInteger(_)
                    | TokenKind::DecimalInteger(_)
                    | TokenKind::HexadecimalInteger(_)
                    | TokenKind::Ascii(_)
                    | TokenKind::Str(_)
                    | TokenKind::RawStr(_)
                    | TokenKind::Identifier(_)
                    | TokenKind::IdentifierStr(_)
                    | TokenKind::Print
                    | TokenKind::PrintLn
                    | TokenKind::Eprint
                    | TokenKind::EprintLn
                    | TokenKind::Let
                    | TokenKind::Var
                    | TokenKind::Do
                    | TokenKind::If
                    | TokenKind::Else
                    | TokenKind::Loop
                    | TokenKind::Break
                    | TokenKind::Continue => {
                        let previous_token = self.peek_previous_token();
                        Err(Error {
                            kind: ErrorKind::MissingSemicolon,
                            col: previous_token.col,
                            pointers_count: previous_token.kind.display_len(self.tokens),
                        })
                    }
                    TokenKind::Comment(_) | TokenKind::BlockComment(_) => {
                        unreachable!("should be skipped by the token iterator")
                    }
                    TokenKind::Unexpected(_) => unreachable!("only valid tokens should be present"),
                }
            }
            TokenKind::Let => {
                let variable = self.variable_definition()?;

                #[expect(clippy::cast_possible_truncation)]
                let var_index = self.ast.variables.len() as VariableIndex;
                self.scopes[self.scope as usize].let_variables.push(var_index);
                self.ast.variables.push(variable);
                Ok(Node::Definition { var_index })
            }
            TokenKind::Var => {
                let variable = self.variable_definition()?;

                #[expect(clippy::cast_possible_truncation)]
                let var_index = self.ast.variables.len() as VariableIndex;
                self.scopes[self.scope as usize].var_variables.push(var_index);
                self.ast.variables.push(variable);
                Ok(Node::Definition { var_index })
            }
            TokenKind::Print => {
                let arg = self.print_arg()?;
                self.semicolon()?;
                Ok(Node::Print(arg))
            }
            TokenKind::PrintLn => {
                if let Some(Token { kind: TokenKind::SemiColon, .. }) = self.peek_next_token() {
                    _ = self.next_token();
                    return Ok(Node::Println(None));
                }

                let arg = self.print_arg()?;
                self.semicolon()?;
                Ok(Node::Println(Some(arg)))
            }
            TokenKind::Eprint => {
                let arg = self.print_arg()?;
                self.semicolon()?;
                Ok(Node::Eprint(arg))
            }
            TokenKind::EprintLn => {
                if let Some(Token { kind: TokenKind::SemiColon, .. }) = self.peek_next_token() {
                    _ = self.next_token();
                    return Ok(Node::Eprintln(None));
                }

                let arg = self.print_arg()?;
                self.semicolon()?;
                Ok(Node::Eprintln(Some(arg)))
            }
            TokenKind::If => Ok(self.iff()?),
            TokenKind::Else => {
                _ = self.next_token();
                Err(Error {
                    kind: ErrorKind::StrayElseBlock,
                    col: token.col,
                    pointers_count: token.kind.display_len(self.tokens),
                })
            }
            TokenKind::Do | TokenKind::Loop => {
                self.loop_depth += 1;
                let looop_statement = self.loop_statement();
                self.loop_depth -= 1;
                looop_statement
            }
            TokenKind::Break => {
                _ = self.next_token();
                if self.loop_depth == 0 {
                    return Err(Error {
                        kind: ErrorKind::StrayBreakStatement,
                        col: token.col,
                        pointers_count: token.kind.display_len(self.tokens),
                    });
                }

                self.semicolon()?;
                Ok(Node::Break)
            }
            TokenKind::Continue => {
                _ = self.next_token();
                if self.loop_depth == 0 {
                    return Err(Error {
                        kind: ErrorKind::StrayContinueStatement,
                        col: token.col,
                        pointers_count: token.kind.display_len(self.tokens),
                    });
                }

                self.semicolon()?;
                Ok(Node::Continue)
            }
            TokenKind::SemiColon => {
                _ = self.next_token();
                Ok(Node::Semicolon)
            }
            TokenKind::OpenCurlyBracket => {
                let Position { line, column } = self.src.position(token.col);
                unreachable!(
                    "blocks not allowed in single statements: {file}:{line}:{column}",
                    file = self.src.path().display(),
                );
            }
            TokenKind::CloseCurlyBracket
            | TokenKind::CloseSquareBracket
            | TokenKind::CloseRoundBracket => {
                let Position { line, column } = self.src.position(token.col);
                unreachable!(
                    "should have been cought during tokenization: {file}:{line}:{column}",
                    file = self.src.path().display(),
                );
            }
            TokenKind::Colon => {
                _ = self.next_token();
                Err(Error {
                    kind: ErrorKind::StrayColon,
                    col: token.col,
                    pointers_count: token.kind.display_len(self.tokens),
                })
            }
            TokenKind::Comma => {
                _ = self.next_token();
                Err(Error {
                    kind: ErrorKind::StrayComma,
                    col: token.col,
                    pointers_count: token.kind.display_len(self.tokens),
                })
            }
            TokenKind::Op(op) => {
                _ = self.next_token();
                Err(Error {
                    kind: ErrorKind::StrayOperator(op),
                    col: token.col,
                    pointers_count: token.kind.display_len(self.tokens),
                })
            }
            TokenKind::Comment(_) | TokenKind::BlockComment(_) => {
                unreachable!("should be skipped by the token iterator")
            }
            TokenKind::Unexpected(_) => unreachable!("only valid tokens should be present"),
        };
    }

    fn do_statement(&mut self) -> Result<Node, Error<ErrorKind>> {
        let token = self.next_token_bounded(Expected::StatementAfterDo)?;
        return match token.kind {
            TokenKind::OpenCurlyBracket => {
                _ = self.next_token();
                Err(Error {
                    kind: ErrorKind::BlockInDoStatement,
                    col: token.col,
                    pointers_count: token.kind.display_len(self.tokens),
                })
            }
            TokenKind::Let | TokenKind::Var => {
                _ = self.next_token();
                Err(Error {
                    kind: ErrorKind::VariableInDoStatement,
                    col: token.col,
                    pointers_count: token.kind.display_len(self.tokens),
                })
            }
            TokenKind::OpenRoundBracket
            | TokenKind::CloseRoundBracket
            | TokenKind::OpenSquareBracket
            | TokenKind::CloseSquareBracket
            | TokenKind::CloseCurlyBracket
            | TokenKind::Comment(_)
            | TokenKind::BlockComment(_)
            | TokenKind::Unexpected(_)
            | TokenKind::Colon
            | TokenKind::SemiColon
            | TokenKind::Comma
            | TokenKind::Op(_)
            | TokenKind::False
            | TokenKind::True
            | TokenKind::BinaryInteger(_)
            | TokenKind::OctalInteger(_)
            | TokenKind::DecimalInteger(_)
            | TokenKind::HexadecimalInteger(_)
            | TokenKind::Ascii(_)
            | TokenKind::Str(_)
            | TokenKind::RawStr(_)
            | TokenKind::Identifier(_)
            | TokenKind::IdentifierStr(_)
            | TokenKind::Print
            | TokenKind::PrintLn
            | TokenKind::Eprint
            | TokenKind::EprintLn
            | TokenKind::Do
            | TokenKind::If
            | TokenKind::Else
            | TokenKind::Loop
            | TokenKind::Break
            | TokenKind::Continue => self.statement(token),
        };
    }

    fn any(&mut self, token: Token) -> Result<Node, Error<ErrorKind>> {
        return match token.kind {
            TokenKind::OpenCurlyBracket => {
                #[expect(clippy::cast_possible_truncation)]
                let new_scope_index = self.scopes.len() as ScopeIndex;
                self.scopes.push(Scope {
                    parent: self.scope,
                    base_types: Vec::new(),
                    let_variables: Vec::new(),
                    var_variables: Vec::new(),
                });
                self.ast.nodes.push(Vec::new());
                self.scope = new_scope_index;

                _ = self.next_token();
                self.scope();
                Ok(Node::Scope { index: new_scope_index })
            }
            TokenKind::CloseCurlyBracket => {
                self.scope = self.scopes[self.scope as usize].parent;
                _ = self.next_token();
                Ok(Node::ScopeEnd)
            }
            TokenKind::OpenRoundBracket
            | TokenKind::CloseRoundBracket
            | TokenKind::OpenSquareBracket
            | TokenKind::CloseSquareBracket
            | TokenKind::Comment(_)
            | TokenKind::BlockComment(_)
            | TokenKind::Unexpected(_)
            | TokenKind::Colon
            | TokenKind::SemiColon
            | TokenKind::Comma
            | TokenKind::Op(_)
            | TokenKind::False
            | TokenKind::True
            | TokenKind::BinaryInteger(_)
            | TokenKind::OctalInteger(_)
            | TokenKind::DecimalInteger(_)
            | TokenKind::HexadecimalInteger(_)
            | TokenKind::Ascii(_)
            | TokenKind::Str(_)
            | TokenKind::RawStr(_)
            | TokenKind::Identifier(_)
            | TokenKind::IdentifierStr(_)
            | TokenKind::Let
            | TokenKind::Var
            | TokenKind::Print
            | TokenKind::PrintLn
            | TokenKind::Eprint
            | TokenKind::EprintLn
            | TokenKind::Do
            | TokenKind::If
            | TokenKind::Else
            | TokenKind::Loop
            | TokenKind::Break
            | TokenKind::Continue => self.statement(token),
        };
    }
}

// iteration over tokens
impl Parser<'_, '_, '_, '_> {
    // IDEA(stefano): remove self.current_token method and pass the current token around
    fn current_token(&self, expected: Expected) -> Result<Token, Error<ErrorKind>> {
        let Some(token) = self.tokens.tokens.get(self.token as usize) else {
            let previous = self.peek_previous_token();
            return Err(Error {
                kind: ErrorKind::PrematureEndOfFile(expected),
                col: previous.col,
                pointers_count: previous.kind.display_len(self.tokens),
            });
        };

        return Ok(*token);
    }

    fn next_token(&mut self) -> Option<Token> {
        loop {
            #[expect(clippy::cast_possible_truncation)]
            let tokens_len = self.tokens.tokens.len() as TokenIndex;
            if self.token >= tokens_len - 1 {
                self.token = tokens_len;
                return None;
            }

            self.token += 1;
            let next = self.tokens.tokens[self.token as usize];
            let (TokenKind::Comment(_) | TokenKind::BlockComment(_)) = next.kind else {
                return Some(next);
            };
        }
    }

    fn next_token_bounded(&mut self, expected: Expected) -> Result<Token, Error<ErrorKind>> {
        loop {
            #[expect(clippy::cast_possible_truncation)]
            let tokens_len = self.tokens.tokens.len() as TokenIndex;
            if self.token >= tokens_len - 1 {
                let previous = self.tokens.tokens[self.token as usize];
                self.token = tokens_len;
                return Err(Error {
                    kind: ErrorKind::PrematureEndOfFile(expected),
                    col: previous.col,
                    pointers_count: previous.kind.display_len(self.tokens),
                });
            }

            self.token += 1;
            let next = self.tokens.tokens[self.token as usize];
            let (TokenKind::Comment(_) | TokenKind::BlockComment(_)) = next.kind else {
                return Ok(next);
            };
        }
    }

    fn peek_next_token(&self) -> Option<Token> {
        let mut current_token = self.token;
        loop {
            #[expect(clippy::cast_possible_truncation)]
            if current_token >= self.tokens.tokens.len() as TokenIndex - 1 {
                return None;
            }

            current_token += 1;
            let next = self.tokens.tokens[current_token as usize];
            let (TokenKind::Comment(_) | TokenKind::BlockComment(_)) = next.kind else {
                return Some(next);
            };
        }
    }

    // Note: this function is always called when underflowing the tokens array is never the case,
    // so there is no need for bounds checking
    fn peek_previous_token(&self) -> Token {
        let mut current_token = self.token;
        loop {
            current_token -= 1;
            let previous = self.tokens.tokens[current_token as usize];
            let (TokenKind::Comment(_) | TokenKind::BlockComment(_)) = previous.kind else {
                return previous;
            };
        }
    }
}

// expressions
impl Parser<'_, '_, '_, '_> {
    fn new_expression(&mut self, expression: Expression) -> ExpressionIndex {
        #[expect(clippy::cast_possible_truncation)]
        let index = self.ast.expressions.len() as ExpressionIndex;
        self.ast.expressions.push(expression);
        return index;
    }

    fn assert_lhs_is_not_string_or_array(
        &self,
        op_token: Token,
        lhs: &Expression,
    ) -> Result<(), Error<ErrorKind>> {
        let lhs_type = lhs.typ();
        if let Type::Base(BaseType::Str) | Type::Array { .. } = lhs_type {
            return Err(Error {
                kind: ErrorKind::LeftOperandTypeMismatch(lhs_type),
                col: op_token.col,
                pointers_count: op_token.kind.display_len(self.tokens),
            });
        }

        return Ok(());
    }

    fn assert_rhs_is_not_string_or_array(
        &self,
        op_token: Token,
        rhs: &Expression,
    ) -> Result<(), Error<ErrorKind>> {
        let rhs_type = rhs.typ();
        if let Type::Base(BaseType::Str) | Type::Array { .. } = rhs_type {
            return Err(Error {
                kind: ErrorKind::RightOperandTypeMismatch(rhs_type),
                col: op_token.col,
                pointers_count: op_token.kind.display_len(self.tokens),
            });
        }

        return Ok(());
    }

    fn assert_lhs_is_bool(
        &self,
        op_token: Token,
        lhs: &Expression,
    ) -> Result<(), Error<ErrorKind>> {
        let lhs_type = lhs.typ();
        let Type::Base(BaseType::Bool) = lhs_type else {
            return Err(Error {
                kind: ErrorKind::LeftOperandTypeMismatch(lhs_type),
                col: op_token.col,
                pointers_count: op_token.kind.display_len(self.tokens),
            });
        };

        return Ok(());
    }

    fn assert_rhs_is_bool(
        &self,
        op_token: Token,
        rhs: &Expression,
    ) -> Result<(), Error<ErrorKind>> {
        let rhs_type = rhs.typ();
        let Type::Base(BaseType::Bool) = rhs_type else {
            return Err(Error {
                kind: ErrorKind::RightOperandTypeMismatch(rhs_type),
                col: op_token.col,
                pointers_count: op_token.kind.display_len(self.tokens),
            });
        };

        return Ok(());
    }

    fn operator(&mut self, ops: &[Op]) -> Result<Option<(Token, Op)>, Error<ErrorKind>> {
        let current_token = self.current_token(Expected::OperatorOrSemicolon)?;
        let TokenKind::Op(op) = current_token.kind else {
            return Ok(None);
        };

        return if ops.contains(&op) {
            _ = self.next_token();
            Ok(Some((current_token, op)))
        } else {
            Ok(None)
        };
    }

    #[expect(clippy::panic, clippy::panic_in_result_fn)]
    fn primary_expression(&mut self) -> Result<Expression, Error<ErrorKind>> {
        const fn parse_positive_binary_i64(literal: &[ascii]) -> Option<i64> {
            const BASE: Base = Base::Binary;
            let mut integer: i64 = 0;
            let mut digit_index = 0;
            digit_index += 1; // leading zero
            digit_index += 1; // base (b)

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
                    None => return None,
                };
                integer = match integer.checked_add(digit as i64) {
                    Some(integer_) => integer_,
                    None => return None,
                };
            }
            return Some(integer);
        }

        const fn parse_positive_octal_i64(literal: &[ascii]) -> Option<i64> {
            const BASE: Base = Base::Octal;
            let mut integer: i64 = 0;
            let mut digit_index = 0;
            digit_index += 1; // leading zero
            digit_index += 1; // base (o)

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
                    None => return None,
                };
                integer = match integer.checked_add(digit as i64) {
                    Some(integer_) => integer_,
                    None => return None,
                };
            }
            return Some(integer);
        }

        const fn parse_positive_decimal_i64(literal: &[ascii]) -> Option<i64> {
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
                    None => return None,
                };
                integer = match integer.checked_add(digit as i64) {
                    Some(integer_) => integer_,
                    None => return None,
                };
            }
            return Some(integer);
        }

        const fn parse_positive_hexadecimal_i64(literal: &[ascii]) -> Option<i64> {
            const BASE: Base = Base::Hexadecimal;
            let mut integer: i64 = 0;
            let mut digit_index = 0;
            digit_index += 1; // leading zero
            digit_index += 1; // base (x)

            while digit_index < literal.len() {
                let ascii_digit = literal[digit_index];
                digit_index += 1;
                if ascii_digit == b'_' {
                    continue;
                }

                let digit = match ascii_digit {
                    number @ b'0'..=b'9' => number - b'0',
                    uppercase_letter @ b'A'..=b'F' => uppercase_letter - b'A' + 10,
                    lowercase_letter @ b'a'..=b'f' => lowercase_letter - b'a' + 10,
                    _ => panic!("invalid hexadecimal digit"),
                };
                integer = match integer.checked_mul(BASE as i64) {
                    Some(integer_) => integer_,
                    None => return None,
                };
                integer = match integer.checked_add(digit as i64) {
                    Some(integer_) => integer_,
                    None => return None,
                };
            }
            return Some(integer);
        }

        #[expect(clippy::single_call_fn, reason = "readability")]
        const fn parse_negative_binary_i64(literal: &[ascii]) -> Option<i64> {
            const BASE: Base = Base::Binary;
            let mut integer: i64 = 0;
            let mut digit_index = 0;
            digit_index += 1; // leading zero
            digit_index += 1; // base (b)

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
                    None => return None,
                };
                integer = match integer.checked_sub(digit as i64) {
                    Some(integer_) => integer_,
                    None => return None,
                };
            }
            return Some(integer);
        }

        #[expect(clippy::single_call_fn, reason = "readability")]
        const fn parse_negative_octal_i64(literal: &[ascii]) -> Option<i64> {
            const BASE: Base = Base::Octal;
            let mut integer: i64 = 0;
            let mut digit_index = 0;
            digit_index += 1; // leading zero
            digit_index += 1; // base (o)

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
                    None => return None,
                };
                integer = match integer.checked_sub(digit as i64) {
                    Some(integer_) => integer_,
                    None => return None,
                };
            }
            return Some(integer);
        }

        #[expect(clippy::single_call_fn, reason = "readability")]
        const fn parse_negative_decimal_i64(literal: &[ascii]) -> Option<i64> {
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
                    None => return None,
                };
                integer = match integer.checked_sub(digit as i64) {
                    Some(integer_) => integer_,
                    None => return None,
                };
            }
            return Some(integer);
        }

        #[expect(clippy::single_call_fn, reason = "readability")]
        const fn parse_negative_hexadecimal_i64(literal: &[ascii]) -> Option<i64> {
            const BASE: Base = Base::Hexadecimal;
            let mut integer: i64 = 0;
            let mut digit_index = 0;
            digit_index += 1; // leading zero
            digit_index += 1; // base (x)

            while digit_index < literal.len() {
                let ascii_digit = literal[digit_index];
                digit_index += 1;
                if ascii_digit == b'_' {
                    continue;
                }

                let digit = match ascii_digit {
                    number @ b'0'..=b'9' => number - b'0',
                    uppercase_letter @ b'A'..=b'F' => uppercase_letter - b'A' + 10,
                    lowercase_letter @ b'a'..=b'f' => lowercase_letter - b'a' + 10,
                    _ => panic!("invalid hexadecimal digit"),
                };
                integer = match integer.checked_mul(BASE as i64) {
                    Some(integer_) => integer_,
                    None => return None,
                };
                integer = match integer.checked_sub(digit as i64) {
                    Some(integer_) => integer_,
                    None => return None,
                };
            }
            return Some(integer);
        }

        let current_token = self.current_token(Expected::Expression)?;
        let expression_result = match current_token.kind {
            TokenKind::False => Ok(Expression::False),
            TokenKind::True => Ok(Expression::True),
            TokenKind::BinaryInteger(literal_index) => {
                let literal = self.tokens.text[literal_index as usize];
                match parse_positive_binary_i64(literal.as_bytes()) {
                    Some(integer) => Ok(Expression::I64(integer)),
                    None => Err(Error {
                        kind: ErrorKind::BinaryIntegerOverflow,
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(self.tokens),
                    }),
                }
            }
            TokenKind::OctalInteger(literal_index) => {
                let literal = self.tokens.text[literal_index as usize];
                match parse_positive_octal_i64(literal.as_bytes()) {
                    Some(integer) => Ok(Expression::I64(integer)),
                    None => Err(Error {
                        kind: ErrorKind::OctalIntegerOverflow,
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(self.tokens),
                    }),
                }
            }
            TokenKind::DecimalInteger(literal_index) => {
                let literal = self.tokens.text[literal_index as usize];
                match parse_positive_decimal_i64(literal.as_bytes()) {
                    Some(integer) => Ok(Expression::I64(integer)),
                    None => Err(Error {
                        kind: ErrorKind::DecimalIntegerOverflow,
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(self.tokens),
                    }),
                }
            }
            TokenKind::HexadecimalInteger(literal_index) => {
                let literal = self.tokens.text[literal_index as usize];
                match parse_positive_hexadecimal_i64(literal.as_bytes()) {
                    Some(integer) => Ok(Expression::I64(integer)),
                    None => Err(Error {
                        kind: ErrorKind::HexadecimalIntegerOverflow,
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(self.tokens),
                    }),
                }
            }
            TokenKind::Ascii(literal_index) => {
                let string = self.tokens.text[literal_index as usize];
                let string_contents = &string[1..string.len() - 1];
                let Ok(literal) = string_contents.parse::<u8>() else {
                    panic!("wrong parsing of ascii literals");
                };
                Ok(Expression::Ascii(literal))
            }
            TokenKind::Str(string_index) => {
                let string_label = self.string_label;
                let string = self.tokens.text[string_index as usize];
                self.ast.string_labels.push((string_label, &string[1..string.len() - 1]));
                self.string_label += 1;

                Ok(Expression::Str { label: string_label })
            }
            TokenKind::RawStr(string_index) => {
                let string_label = self.string_label;
                let string = self.tokens.text[string_index as usize];
                self.ast.raw_string_labels.push((string_label, &string[2..string.len() - 1]));
                self.string_label += 1;

                Ok(Expression::Str { label: string_label })
            }
            TokenKind::Identifier(name_index) | TokenKind::IdentifierStr(name_index) => {
                let name = self.tokens.text[name_index as usize];
                match self.resolve_type(name.as_bytes()) {
                    None => match self.resolve_variable(name.as_bytes()) {
                        Some(variable_index) => {
                            let var = &self.ast.variables[variable_index as usize];
                            Ok(Expression::Variable { typ: var.value.typ(), variable_index })
                        }
                        None => Err(Error {
                            kind: ErrorKind::VariableNotPreviouslyDefined,
                            col: current_token.col,
                            pointers_count: current_token.kind.display_len(self.tokens),
                        }),
                    },
                    Some(_) => 'type_in_expression: {
                        let Some(possible_reassignment_operator) = self.peek_next_token() else {
                            break 'type_in_expression Err(Error {
                                kind: ErrorKind::TypeInExpression,
                                col: current_token.col,
                                pointers_count: current_token.kind.display_len(self.tokens),
                            });
                        };

                        let TokenKind::Op(op) = possible_reassignment_operator.kind else {
                            break 'type_in_expression Err(Error {
                                kind: ErrorKind::TypeInExpression,
                                col: current_token.col,
                                pointers_count: current_token.kind.display_len(self.tokens),
                            });
                        };

                        match op {
                            Op::Equals
                            | Op::PowEquals
                            | Op::WrappingPowEquals
                            | Op::SaturatingPowEquals
                            | Op::TimesEquals
                            | Op::WrappingTimesEquals
                            | Op::SaturatingTimesEquals
                            | Op::DivideEquals
                            | Op::WrappingDivideEquals
                            | Op::SaturatingDivideEquals
                            | Op::RemainderEquals
                            | Op::PlusEquals
                            | Op::WrappingPlusEquals
                            | Op::SaturatingPlusEquals
                            | Op::MinusEquals
                            | Op::WrappingMinusEquals
                            | Op::SaturatingMinusEquals
                            | Op::LeftShiftEquals
                            | Op::WrappingLeftShiftEquals
                            | Op::SaturatingLeftShiftEquals
                            | Op::RightShiftEquals
                            | Op::LeftRotateEquals
                            | Op::RightRotateEquals
                            | Op::BitAndEquals
                            | Op::BitXorEquals
                            | Op::BitOrEquals
                            | Op::AndEquals
                            | Op::OrEquals => {
                                break 'type_in_expression Err(Error {
                                    kind: ErrorKind::TypeInVariableReassignment,
                                    col: current_token.col,
                                    pointers_count: current_token.kind.display_len(self.tokens),
                                });
                            }

                            Op::Len
                            | Op::Not
                            | Op::Pow
                            | Op::WrappingPow
                            | Op::SaturatingPow
                            | Op::Times
                            | Op::WrappingTimes
                            | Op::SaturatingTimes
                            | Op::Divide
                            | Op::WrappingDivide
                            | Op::SaturatingDivide
                            | Op::Remainder
                            | Op::Plus
                            | Op::WrappingPlus
                            | Op::SaturatingPlus
                            | Op::Minus
                            | Op::WrappingMinus
                            | Op::SaturatingMinus
                            | Op::LeftShift
                            | Op::WrappingLeftShift
                            | Op::SaturatingLeftShift
                            | Op::RightShift
                            | Op::LeftRotate
                            | Op::RightRotate
                            | Op::BitAnd
                            | Op::BitXor
                            | Op::BitOr
                            | Op::And
                            | Op::Or
                            | Op::Compare
                            | Op::EqualsEquals
                            | Op::NotEquals
                            | Op::Greater
                            | Op::GreaterOrEquals
                            | Op::Less
                            | Op::LessOrEquals => {
                                break 'type_in_expression Err(Error {
                                    kind: ErrorKind::TypeInExpression,
                                    col: current_token.col,
                                    pointers_count: current_token.kind.display_len(self.tokens),
                                });
                            }
                        }
                    }
                }
            }
            TokenKind::OpenRoundBracket => 'parenthesis: {
                let expression_start_token = self.next_token_bounded(Expected::Expression)?;

                if let TokenKind::CloseRoundBracket = expression_start_token.kind {
                    break 'parenthesis Err(Error {
                        kind: ErrorKind::EmptyExpression,
                        col: expression_start_token.col,
                        pointers_count: expression_start_token.kind.display_len(self.tokens),
                    });
                }

                let expression = self.expression()?;
                let close_bracket_token = self.current_token(Expected::ClosingRoundBracket)?;

                let TokenKind::CloseRoundBracket = close_bracket_token.kind else {
                    return Err(Error {
                        kind: ErrorKind::UnclosedRoundBracket,
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(self.tokens),
                    });
                };

                Ok(Expression::Parenthesis {
                    typ: expression.typ(),
                    expression_index: self.new_expression(expression),
                })
            }
            TokenKind::OpenSquareBracket => 'array: {
                let mut bracket_or_comma_token =
                    self.next_token_bounded(Expected::ArrayElementOrClosingSquareBracket)?;

                // REMOVE(stefano): allow arrays of 0 elements
                if let TokenKind::CloseSquareBracket = bracket_or_comma_token.kind {
                    break 'array Err(Error {
                        kind: ErrorKind::ArrayOfZeroElements,
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(self.tokens),
                    });
                }

                let first_item = self.expression()?;

                bracket_or_comma_token =
                    self.current_token(Expected::CommaOrClosingSquareBracket)?;

                if let TokenKind::Comma = bracket_or_comma_token.kind {
                    bracket_or_comma_token =
                        self.next_token_bounded(Expected::ArrayElementOrClosingSquareBracket)?;
                }

                let items_type = match first_item.typ() {
                    Type::Base(base_type) => base_type,
                    Type::Array { .. } => {
                        break 'array Err(Error {
                            kind: ErrorKind::NestedArrayNotSupportedYet,
                            col: current_token.col,
                            pointers_count: current_token.kind.display_len(self.tokens),
                        })
                    }
                };
                let mut items = vec![first_item];

                if let TokenKind::CloseSquareBracket = bracket_or_comma_token.kind {
                    break 'array Ok(Expression::Array { base_type: items_type, items });
                }

                // IDEA(stefano): gather all the items and then check if they are of the correct type
                loop {
                    let item = self.expression()?;
                    let item_type = item.typ();
                    /* NOTE(stefano):
                    this wrapping of items_type will be removed once nested arrays are supported
                    */
                    if Type::Base(items_type) != item_type {
                        break 'array Err(Error {
                            kind: ErrorKind::ArrayElementTypeMismatch {
                                actual: item_type,
                                expected: Type::Base(items_type),
                            },
                            col: bracket_or_comma_token.col,
                            pointers_count: bracket_or_comma_token.kind.display_len(self.tokens),
                        });
                    }

                    if let Type::Array { .. } = item_type {
                        break 'array Err(Error {
                            kind: ErrorKind::NestedArrayNotSupportedYet,
                            col: current_token.col,
                            pointers_count: current_token.kind.display_len(self.tokens),
                        });
                    };

                    items.push(item);

                    bracket_or_comma_token =
                        self.current_token(Expected::CommaOrClosingSquareBracket)?;

                    if let TokenKind::Comma = bracket_or_comma_token.kind {
                        bracket_or_comma_token =
                            self.next_token_bounded(Expected::ArrayElementOrClosingSquareBracket)?;
                    }

                    if let TokenKind::CloseSquareBracket = bracket_or_comma_token.kind {
                        debug_assert!(items.len() > 0, "arrays of 0 items are not allowed");
                        break 'array Ok(Expression::Array { base_type: items_type, items });
                    }
                }
            }
            TokenKind::Op(Op::Len) => {
                _ = self.next_token();
                let operand = self.primary_expression()?;
                return match &operand {
                    Expression::Str { .. } => Ok(Expression::Unary {
                        op: UnaryOp::Len,
                        op_col: current_token.col,
                        operand_index: self.new_expression(operand),
                    }),
                    Expression::I64(_) => Err(Error {
                        kind: ErrorKind::CannotTakeLenOf(Type::Base(BaseType::I64)),
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(self.tokens),
                    }),
                    Expression::Ascii(_) => Err(Error {
                        kind: ErrorKind::CannotTakeLenOf(Type::Base(BaseType::Ascii)),
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(self.tokens),
                    }),
                    Expression::True | Expression::False => Err(Error {
                        kind: ErrorKind::CannotTakeLenOf(Type::Base(BaseType::Bool)),
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(self.tokens),
                    }),
                    Expression::Array { .. } => Ok(Expression::Unary {
                        op: UnaryOp::Len,
                        op_col: current_token.col,
                        operand_index: self.new_expression(operand),
                    }),
                    Expression::Variable { typ, .. } => match typ {
                        Type::Base(BaseType::Str) | Type::Array { .. } => Ok(Expression::Unary {
                            op: UnaryOp::Len,
                            op_col: current_token.col,
                            operand_index: self.new_expression(operand),
                        }),
                        Type::Base(BaseType::I64 | BaseType::Ascii | BaseType::Bool) => {
                            Err(Error {
                                kind: ErrorKind::CannotTakeLenOf(*typ),
                                col: current_token.col,
                                pointers_count: current_token.kind.display_len(self.tokens),
                            })
                        }
                    },
                    Expression::ArrayIndex { base_type, .. } => match base_type {
                        BaseType::Str => Ok(Expression::Unary {
                            op: UnaryOp::Len,
                            op_col: current_token.col,
                            operand_index: self.new_expression(operand),
                        }),
                        BaseType::I64 | BaseType::Ascii | BaseType::Bool => Err(Error {
                            kind: ErrorKind::CannotTakeLenOf(Type::Base(*base_type)),
                            col: current_token.col,
                            pointers_count: current_token.kind.display_len(self.tokens),
                        }),
                    },
                    Expression::Parenthesis { typ, .. } => Err(Error {
                        kind: ErrorKind::CannotTakeLenOf(*typ),
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(self.tokens),
                    }),
                    Expression::Unary { op, .. } => Err(Error {
                        kind: ErrorKind::CannotTakeLenOf(op.typ()),
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(self.tokens),
                    }),
                    Expression::BooleanUnary { op, .. } => Err(Error {
                        kind: ErrorKind::CannotTakeLenOf(op.typ()),
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(self.tokens),
                    }),
                    Expression::Binary { op, .. } => Err(Error {
                        kind: ErrorKind::CannotTakeLenOf(op.typ()),
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(self.tokens),
                    }),
                    Expression::BooleanBinary { op, .. } => Err(Error {
                        kind: ErrorKind::CannotTakeLenOf(op.typ()),
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(self.tokens),
                    }),
                    Expression::Comparison { op, .. } => Err(Error {
                        kind: ErrorKind::CannotTakeLenOf(op.typ()),
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(self.tokens),
                    }),
                    Expression::Temporary { .. } => {
                        unreachable!("should be returned from expressions");
                    }
                };
            }
            TokenKind::Op(Op::Plus) => {
                let mut should_be_made_positive = true;

                // NOTE(stefano): this optimization should be moved to later stages
                // removing extra "+" symbols
                while let Some(Token { kind: TokenKind::Op(Op::Plus), .. }) = self.next_token() {
                    should_be_made_positive = !should_be_made_positive;
                }

                let operand = self.primary_expression()?;

                // returning to avoid the call to tokens.next at the end of the function
                return match operand.typ() {
                    Type::Base(BaseType::I64) => {
                        if should_be_made_positive {
                            Ok(Expression::Unary {
                                op: UnaryOp::Plus,
                                op_col: current_token.col,
                                operand_index: self.new_expression(operand),
                            })
                        } else {
                            Ok(operand)
                        }
                    }
                    invalid_type @ (Type::Base(
                        BaseType::Ascii | BaseType::Bool | BaseType::Str,
                    )
                    | Type::Array { .. }) => Err(Error {
                        kind: ErrorKind::CannotTakeAbsoluteValueOf(invalid_type),
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(self.tokens),
                    }),
                };
            }
            TokenKind::Op(Op::WrappingPlus) => {
                let mut should_be_made_positive = true;

                // NOTE(stefano): this optimization should be moved to later stages
                // removing extra "+\" symbols
                while let Some(Token { kind: TokenKind::Op(Op::WrappingPlus), .. }) =
                    self.next_token()
                {
                    should_be_made_positive = !should_be_made_positive;
                }

                let operand = self.primary_expression()?;

                // returning to avoid the call to tokens.next at the end of the function
                return match operand.typ() {
                    Type::Base(BaseType::I64) => {
                        if should_be_made_positive {
                            Ok(Expression::Unary {
                                op: UnaryOp::WrappingPlus,
                                op_col: current_token.col,
                                operand_index: self.new_expression(operand),
                            })
                        } else {
                            Ok(operand)
                        }
                    }
                    invalid_type @ (Type::Base(
                        BaseType::Ascii | BaseType::Bool | BaseType::Str,
                    )
                    | Type::Array { .. }) => Err(Error {
                        kind: ErrorKind::CannotTakeAbsoluteValueOf(invalid_type),
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(self.tokens),
                    }),
                };
            }
            TokenKind::Op(Op::SaturatingPlus) => {
                let mut should_be_made_positive = true;

                // NOTE(stefano): this optimization should be moved to later stages
                // removing extra "+|" symbols
                while let Some(Token { kind: TokenKind::Op(Op::SaturatingPlus), .. }) =
                    self.next_token()
                {
                    should_be_made_positive = !should_be_made_positive;
                }

                let operand = self.primary_expression()?;

                // returning to avoid the call to tokens.next at the end of the function
                return match operand.typ() {
                    Type::Base(BaseType::I64) => {
                        if should_be_made_positive {
                            Ok(Expression::Unary {
                                op: UnaryOp::SaturatingPlus,
                                op_col: current_token.col,
                                operand_index: self.new_expression(operand),
                            })
                        } else {
                            Ok(operand)
                        }
                    }
                    invalid_typ @ (Type::Base(BaseType::Ascii | BaseType::Bool | BaseType::Str)
                    | Type::Array { .. }) => Err(Error {
                        kind: ErrorKind::CannotTakeAbsoluteValueOf(invalid_typ),
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(self.tokens),
                    }),
                };
            }
            TokenKind::Op(minus @ (Op::Minus | Op::WrappingMinus | Op::SaturatingMinus)) => {
                let mut should_be_negated = true;

                // NOTE(stefano): this optimization should be moved to later stages
                // removing extra `-`/`-\`/`-|` symbols
                while let Some(Token { kind: TokenKind::Op(op), .. }) = self.next_token() {
                    if op != minus {
                        break;
                    }
                    should_be_negated = !should_be_negated;
                }

                let start_of_expression = self.tokens.tokens[self.token as usize];
                #[expect(clippy::wildcard_enum_match_arm, reason = "readability")]
                match start_of_expression.kind {
                    TokenKind::BinaryInteger(literal_index) => {
                        let literal = self.tokens.text[literal_index as usize];
                        if should_be_negated {
                            match parse_negative_binary_i64(literal.as_bytes()) {
                                Some(0) => Err(Error {
                                    kind: ErrorKind::MinusZeroInteger,
                                    col: start_of_expression.col,
                                    pointers_count: start_of_expression
                                        .kind
                                        .display_len(self.tokens),
                                }),
                                Some(integer) => Ok(Expression::I64(integer)),
                                None => Err(Error {
                                    kind: ErrorKind::BinaryIntegerUnderflow,
                                    col: start_of_expression.col,
                                    pointers_count: start_of_expression
                                        .kind
                                        .display_len(self.tokens),
                                }),
                            }
                        } else {
                            match parse_positive_binary_i64(literal.as_bytes()) {
                                Some(integer) => Ok(Expression::I64(integer)),
                                None => Err(Error {
                                    kind: ErrorKind::BinaryIntegerOverflow,
                                    col: start_of_expression.col,
                                    pointers_count: start_of_expression
                                        .kind
                                        .display_len(self.tokens),
                                }),
                            }
                        }
                    }
                    TokenKind::OctalInteger(literal_index) => {
                        let literal = self.tokens.text[literal_index as usize];
                        if should_be_negated {
                            match parse_negative_octal_i64(literal.as_bytes()) {
                                Some(0) => Err(Error {
                                    kind: ErrorKind::MinusZeroInteger,
                                    col: start_of_expression.col,
                                    pointers_count: start_of_expression
                                        .kind
                                        .display_len(self.tokens),
                                }),
                                Some(integer) => Ok(Expression::I64(integer)),
                                None => Err(Error {
                                    kind: ErrorKind::OctalIntegerUnderflow,
                                    col: start_of_expression.col,
                                    pointers_count: start_of_expression
                                        .kind
                                        .display_len(self.tokens),
                                }),
                            }
                        } else {
                            match parse_positive_octal_i64(literal.as_bytes()) {
                                Some(integer) => Ok(Expression::I64(integer)),
                                None => Err(Error {
                                    kind: ErrorKind::OctalIntegerOverflow,
                                    col: start_of_expression.col,
                                    pointers_count: start_of_expression
                                        .kind
                                        .display_len(self.tokens),
                                }),
                            }
                        }
                    }
                    TokenKind::DecimalInteger(literal_index) => {
                        let literal = self.tokens.text[literal_index as usize];
                        if should_be_negated {
                            match parse_negative_decimal_i64(literal.as_bytes()) {
                                Some(0) => Err(Error {
                                    kind: ErrorKind::MinusZeroInteger,
                                    col: start_of_expression.col,
                                    pointers_count: start_of_expression
                                        .kind
                                        .display_len(self.tokens),
                                }),
                                Some(integer) => Ok(Expression::I64(integer)),
                                None => Err(Error {
                                    kind: ErrorKind::DecimalIntegerUnderflow,
                                    col: start_of_expression.col,
                                    pointers_count: start_of_expression
                                        .kind
                                        .display_len(self.tokens),
                                }),
                            }
                        } else {
                            match parse_positive_decimal_i64(literal.as_bytes()) {
                                Some(integer) => Ok(Expression::I64(integer)),
                                None => Err(Error {
                                    kind: ErrorKind::DecimalIntegerOverflow,
                                    col: start_of_expression.col,
                                    pointers_count: start_of_expression
                                        .kind
                                        .display_len(self.tokens),
                                }),
                            }
                        }
                    }
                    TokenKind::HexadecimalInteger(literal_index) => {
                        let literal = self.tokens.text[literal_index as usize];
                        if should_be_negated {
                            match parse_negative_hexadecimal_i64(literal.as_bytes()) {
                                Some(0) => Err(Error {
                                    kind: ErrorKind::MinusZeroInteger,
                                    col: start_of_expression.col,
                                    pointers_count: start_of_expression
                                        .kind
                                        .display_len(self.tokens),
                                }),
                                Some(integer) => Ok(Expression::I64(integer)),
                                None => Err(Error {
                                    kind: ErrorKind::HexadecimalIntegerUnderflow,
                                    col: start_of_expression.col,
                                    pointers_count: start_of_expression
                                        .kind
                                        .display_len(self.tokens),
                                }),
                            }
                        } else {
                            match parse_positive_hexadecimal_i64(literal.as_bytes()) {
                                Some(integer) => Ok(Expression::I64(integer)),
                                None => Err(Error {
                                    kind: ErrorKind::HexadecimalIntegerOverflow,
                                    col: start_of_expression.col,
                                    pointers_count: start_of_expression
                                        .kind
                                        .display_len(self.tokens),
                                }),
                            }
                        }
                    }
                    _ => {
                        let operand = self.primary_expression()?;

                        // returning to avoid the call to tokens.next at the end of the function
                        return match operand.typ() {
                            Type::Base(BaseType::I64 | BaseType::Ascii) => {
                                if should_be_negated {
                                    Ok(Expression::Unary {
                                        op: minus.into(),
                                        op_col: current_token.col,
                                        operand_index: self.new_expression(operand),
                                    })
                                } else {
                                    Ok(operand)
                                }
                            }
                            invalid_typ @ (Type::Base(BaseType::Bool | BaseType::Str)
                            | Type::Array { .. }) => Err(Error {
                                kind: ErrorKind::CannotNegate(invalid_typ),
                                col: current_token.col,
                                pointers_count: current_token.kind.display_len(self.tokens),
                            }),
                        };
                    }
                }
            }
            TokenKind::Op(Op::Not) => {
                let mut should_be_inverted = true;

                // NOTE(stefano): this optimization should be moved to later stages
                // removing extra "!" symbols
                while let Some(Token { kind: TokenKind::Op(Op::Not), .. }) = self.next_token() {
                    should_be_inverted = !should_be_inverted;
                }

                let operand = self.primary_expression()?;

                // returning to avoid the call to tokens.next at the end of the function
                return match operand.typ() {
                    Type::Base(BaseType::I64 | BaseType::Ascii) => {
                        if should_be_inverted {
                            Ok(Expression::Unary {
                                op: UnaryOp::Not,
                                op_col: current_token.col,
                                operand_index: self.new_expression(operand),
                            })
                        } else {
                            Ok(operand)
                        }
                    }
                    Type::Base(BaseType::Bool) => {
                        if should_be_inverted {
                            Ok(Expression::BooleanUnary {
                                op: BooleanUnaryOp::Not,
                                operand_index: self.new_expression(operand),
                            })
                        } else {
                            Ok(operand)
                        }
                    }
                    invalid_typ @ (Type::Base(BaseType::Str) | Type::Array { .. }) => Err(Error {
                        kind: ErrorKind::CannotInvert(invalid_typ),
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(self.tokens),
                    }),
                };
            }
            TokenKind::Let
            | TokenKind::Var
            | TokenKind::Print
            | TokenKind::PrintLn
            | TokenKind::If
            | TokenKind::Else
            | TokenKind::Loop
            | TokenKind::Break
            | TokenKind::Continue
            | TokenKind::Eprint
            | TokenKind::EprintLn
            | TokenKind::Do => Err(Error {
                kind: ErrorKind::KeywordInExpression,
                col: current_token.col,
                pointers_count: current_token.kind.display_len(self.tokens),
            }),
            TokenKind::CloseRoundBracket
            | TokenKind::CloseSquareBracket
            | TokenKind::OpenCurlyBracket
            | TokenKind::CloseCurlyBracket
            | TokenKind::Op(_)
            | TokenKind::Comment(_)
            | TokenKind::BlockComment(_)
            | TokenKind::Unexpected(_)
            | TokenKind::Colon
            | TokenKind::SemiColon
            | TokenKind::Comma => Err(Error {
                kind: ErrorKind::ExpectedOperand,
                col: current_token.col,
                pointers_count: current_token.kind.display_len(self.tokens),
            }),
        };

        let mut expression = expression_result?;
        while let Some(open_bracket_token @ Token { kind: TokenKind::OpenSquareBracket, .. }) =
            self.next_token()
        {
            let _start_of_index = self.next_token();
            let index = self.expression()?;
            let Type::Base(BaseType::I64) = index.typ() else {
                return Err(Error {
                    kind: ErrorKind::ExpectedNumberLiteralInArrayIndex,
                    col: open_bracket_token.col,
                    pointers_count: open_bracket_token.kind.display_len(self.tokens),
                });
            };

            let after_index_token = self.current_token(Expected::ClosingSquareBracket)?;

            let TokenKind::CloseSquareBracket = after_index_token.kind else {
                let before_index_token = self.peek_previous_token();
                return Err(Error {
                    kind: ErrorKind::MissingClosingSquareBracketInIndex,
                    col: before_index_token.col,
                    pointers_count: before_index_token.kind.display_len(self.tokens),
                });
            };

            let Expression::Variable { .. } = expression else {
                return Err(Error {
                    kind: ErrorKind::CannotIndexIntoExpression,
                    col: open_bracket_token.col,
                    pointers_count: open_bracket_token.kind.display_len(self.tokens),
                });
            };

            let expression_type = expression.typ();
            expression = match expression_type {
                Type::Base(base_type) => match base_type {
                    BaseType::Str => Expression::ArrayIndex {
                        base_type: BaseType::Ascii,
                        indexable_index: self.new_expression(expression),
                        bracket_col: open_bracket_token.col,
                        index_expression_index: self.new_expression(index),
                    },
                    BaseType::I64 | BaseType::Ascii | BaseType::Bool => {
                        return Err(Error {
                            kind: ErrorKind::CannotIndexNonArrayType(expression_type),
                            col: open_bracket_token.col,
                            pointers_count: open_bracket_token.kind.display_len(self.tokens),
                        })
                    }
                },
                Type::Array { base_type, .. } => Expression::ArrayIndex {
                    base_type,
                    indexable_index: self.new_expression(expression),
                    bracket_col: open_bracket_token.col,
                    index_expression_index: self.new_expression(index),
                },
            };
        }

        return Ok(expression);
    }

    fn exponentiative_expression(&mut self) -> Result<Expression, Error<ErrorKind>> {
        let mut lhs = self.primary_expression()?;

        let ops = [Op::Pow, Op::WrappingPow, Op::SaturatingPow];
        while let Some((op_token, op)) = self.operator(&ops)? {
            self.assert_lhs_is_not_string_or_array(op_token, &lhs)?;

            let rhs = self.primary_expression()?;
            self.assert_rhs_is_not_string_or_array(op_token, &rhs)?;

            lhs = Expression::Binary {
                lhs_index: self.new_expression(lhs),
                op: op.into(),
                op_col: op_token.col,
                rhs_index: self.new_expression(rhs),
            };
        }

        return Ok(lhs);
    }

    fn multiplicative_expression(&mut self) -> Result<Expression, Error<ErrorKind>> {
        let mut lhs = self.exponentiative_expression()?;

        let ops = [
            Op::Times,
            Op::WrappingTimes,
            Op::SaturatingTimes,
            Op::Divide,
            Op::WrappingDivide,
            Op::SaturatingDivide,
            Op::Remainder,
        ];
        while let Some((op_token, op)) = self.operator(&ops)? {
            self.assert_lhs_is_not_string_or_array(op_token, &lhs)?;

            let rhs = self.exponentiative_expression()?;
            self.assert_rhs_is_not_string_or_array(op_token, &rhs)?;

            lhs = Expression::Binary {
                lhs_index: self.new_expression(lhs),
                op: op.into(),
                op_col: op_token.col,
                rhs_index: self.new_expression(rhs),
            };
        }

        return Ok(lhs);
    }

    fn additive_expression(&mut self) -> Result<Expression, Error<ErrorKind>> {
        let mut lhs = self.multiplicative_expression()?;

        let ops = [
            Op::Plus,
            Op::WrappingPlus,
            Op::SaturatingPlus,
            Op::Minus,
            Op::WrappingMinus,
            Op::SaturatingMinus,
        ];
        while let Some((op_token, op)) = self.operator(&ops)? {
            self.assert_lhs_is_not_string_or_array(op_token, &lhs)?;

            let rhs = self.multiplicative_expression()?;
            self.assert_rhs_is_not_string_or_array(op_token, &rhs)?;

            lhs = Expression::Binary {
                lhs_index: self.new_expression(lhs),
                op: op.into(),
                op_col: op_token.col,
                rhs_index: self.new_expression(rhs),
            };
        }

        return Ok(lhs);
    }

    /* IDEA(stefano):
    when the lhs is a literal integer shifts could be optimized to throw errors
    when preconditions such as negative integers and shifts over 6bits are not met
    */
    fn shift_expression(&mut self) -> Result<Expression, Error<ErrorKind>> {
        let mut lhs = self.additive_expression()?;

        let ops = [
            Op::LeftShift,
            Op::WrappingLeftShift,
            Op::SaturatingLeftShift,
            Op::RightShift,
            Op::LeftRotate,
            Op::RightRotate,
        ];
        while let Some((op_token, op)) = self.operator(&ops)? {
            self.assert_lhs_is_not_string_or_array(op_token, &lhs)?;

            let rhs = self.additive_expression()?;
            self.assert_rhs_is_not_string_or_array(op_token, &rhs)?;

            lhs = Expression::Binary {
                lhs_index: self.new_expression(lhs),
                op: op.into(),
                op_col: op_token.col,
                rhs_index: self.new_expression(rhs),
            };
        }

        return Ok(lhs);
    }

    fn bitand_expression(&mut self) -> Result<Expression, Error<ErrorKind>> {
        let mut lhs = self.shift_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::BitAnd])? {
            self.assert_lhs_is_not_string_or_array(op_token, &lhs)?;

            let rhs = self.shift_expression()?;
            self.assert_rhs_is_not_string_or_array(op_token, &rhs)?;

            lhs = Expression::Binary {
                lhs_index: self.new_expression(lhs),
                op: op.into(),
                op_col: op_token.col,
                rhs_index: self.new_expression(rhs),
            };
        }

        return Ok(lhs);
    }

    fn bitxor_expression(&mut self) -> Result<Expression, Error<ErrorKind>> {
        let mut lhs = self.bitand_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::BitXor])? {
            self.assert_lhs_is_not_string_or_array(op_token, &lhs)?;

            let rhs = self.bitand_expression()?;
            self.assert_rhs_is_not_string_or_array(op_token, &rhs)?;

            lhs = Expression::Binary {
                lhs_index: self.new_expression(lhs),
                op: op.into(),
                op_col: op_token.col,
                rhs_index: self.new_expression(rhs),
            };
        }

        return Ok(lhs);
    }

    fn bitor_expression(&mut self) -> Result<Expression, Error<ErrorKind>> {
        let mut lhs = self.bitxor_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::BitOr])? {
            self.assert_lhs_is_not_string_or_array(op_token, &lhs)?;

            let rhs = self.bitxor_expression()?;
            self.assert_rhs_is_not_string_or_array(op_token, &rhs)?;

            lhs = Expression::Binary {
                lhs_index: self.new_expression(lhs),
                op: op.into(),
                op_col: op_token.col,
                rhs_index: self.new_expression(rhs),
            };
        }

        return Ok(lhs);
    }

    fn comparison_expression(&mut self) -> Result<Expression, Error<ErrorKind>> {
        let mut lhs = self.bitor_expression()?;

        let ops = [
            Op::Compare,
            Op::EqualsEquals,
            Op::NotEquals,
            Op::Greater,
            Op::GreaterOrEquals,
            Op::Less,
            Op::LessOrEquals,
        ];

        let mut is_chained = false;
        while let Some((op_token, op)) = self.operator(&ops)? {
            let rhs = self.bitor_expression()?;

            let lhs_type = lhs.typ();
            let rhs_type = rhs.typ();
            let can_compare = match (lhs_type, rhs_type) {
                (Type::Base(lhs_base_type), Type::Base(rhs_base_type)) => {
                    lhs_base_type == rhs_base_type
                }
                (
                    Type::Array { base_type: lhs_base_typ, len: lhs_len },
                    Type::Array { base_type: rhs_base_typ, len: rhs_len },
                ) => {
                    debug_assert!(lhs_len > 0, "arrays of 0 items are not allowed");
                    debug_assert!(rhs_len > 0, "arrays of 0 items are not allowed");
                    lhs_base_typ == rhs_base_typ && lhs_len == rhs_len
                }
                _ => false,
            };

            if !can_compare {
                return Err(Error {
                    kind: ErrorKind::CannotCompareOperands { lhs_type, rhs_type },
                    col: op_token.col,
                    pointers_count: op_token.kind.display_len(self.tokens),
                });
            }

            if is_chained {
                return Err(Error {
                    kind: ErrorKind::CannotChainComparisons,
                    col: op_token.col,
                    pointers_count: op_token.kind.display_len(self.tokens),
                });
            }
            is_chained = true;

            lhs = Expression::Comparison {
                lhs_index: self.new_expression(lhs),
                op: op.into(),
                rhs_index: self.new_expression(rhs),
            };
        }

        return Ok(lhs);
    }

    fn and_expression(&mut self) -> Result<Expression, Error<ErrorKind>> {
        let mut lhs = self.comparison_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::And])? {
            self.assert_lhs_is_bool(op_token, &lhs)?;

            let rhs = self.comparison_expression()?;
            self.assert_rhs_is_bool(op_token, &rhs)?;

            lhs = Expression::BooleanBinary {
                lhs_index: self.new_expression(lhs),
                op: op.into(),
                rhs_index: self.new_expression(rhs),
            };
        }

        return Ok(lhs);
    }

    fn or_expression(&mut self) -> Result<Expression, Error<ErrorKind>> {
        let mut lhs = self.and_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::Or])? {
            self.assert_lhs_is_bool(op_token, &lhs)?;

            let rhs = self.and_expression()?;
            self.assert_rhs_is_bool(op_token, &rhs)?;

            lhs = Expression::BooleanBinary {
                lhs_index: self.new_expression(lhs),
                op: op.into(),
                rhs_index: self.new_expression(rhs),
            };
        }

        return Ok(lhs);
    }

    fn expression(&mut self) -> Result<Expression, Error<ErrorKind>> {
        return self.or_expression();
    }
}

// variables and typesk
impl<'code> Parser<'_, '_, 'code, '_> {
    fn resolve_variable(&self, name: &[ascii]) -> Option<VariableIndex> {
        if let Some(variable) = self.resolve_let_variable(name) {
            return Some(variable);
        }

        return self.resolve_var_variable(name);
    }

    fn resolve_let_variable(&self, name: &[ascii]) -> Option<VariableIndex> {
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

    fn resolve_var_variable(&self, name: &[ascii]) -> Option<VariableIndex> {
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

    // NOTE(stefano): why accept a &[ascii] and not &str
    fn resolve_type(&self, name: &[ascii]) -> Option<BaseType> {
        let mut scope_index = self.scope;
        loop {
            let scope = &self.scopes[scope_index as usize];
            for typ in &scope.base_types {
                if typ.to_string().as_bytes() == name {
                    return Some(*typ);
                }
            }

            scope_index = match scope_index {
                0 => return None,
                _ => scope.parent,
            };
        }
    }

    fn type_annotation(&mut self) -> Result<Option<(Token, Type)>, Error<ErrorKind>> {
        let colon_token = self.next_token_bounded(Expected::TypeAnnotationOrVariableDefinition)?;

        let TokenKind::Colon = colon_token.kind else {
            self.token -= 1;
            return Ok(None);
        };

        let type_token = self.next_token_bounded(Expected::TypeAnnotation)?;
        let TokenKind::Identifier(type_name_index) = type_token.kind else {
            return Err(Error {
                kind: ErrorKind::ExpectedType,
                col: colon_token.col,
                pointers_count: colon_token.kind.display_len(self.tokens),
            });
        };

        let type_name = self.tokens.text[type_name_index as usize];
        let Some(base_type) = self.resolve_type(type_name.as_bytes()) else {
            // REMOVE(stefano): remove possibility of emulating `typeof` using other variables as type annotation
            return match self.resolve_variable(type_name.as_bytes()) {
                Some(var_index) => {
                    let var = &self.ast.variables[var_index as usize];
                    Ok(Some((type_token, var.value.typ())))
                }
                None => Err(Error {
                    kind: ErrorKind::VariableNotPreviouslyDefined,
                    col: type_token.col,
                    pointers_count: type_token.kind.display_len(self.tokens),
                }),
            };
        };

        let Some(open_square_bracket_token) = self.peek_next_token() else {
            return Ok(Some((type_token, Type::Base(base_type))));
        };

        let TokenKind::OpenSquareBracket = open_square_bracket_token.kind else {
            return Ok(Some((type_token, Type::Base(base_type))));
        };

        let _open_square_bracket = self.next_token();

        let len_token = self.next_token_bounded(Expected::ArrayLength)?;
        let len_expression = self.expression()?;
        let Expression::I64(len) = len_expression else {
            return Err(Error {
                kind: ErrorKind::ExpectedNumberLiteralInArrayType,
                col: open_square_bracket_token.col,
                pointers_count: open_square_bracket_token.kind.display_len(self.tokens),
            });
        };

        if len < 0 {
            return Err(Error {
                kind: ErrorKind::ArrayOfNegativeLength,
                col: len_token.col,
                pointers_count: len_token.kind.display_len(self.tokens),
            });
        }

        // REMOVE(stefano): allow arrays of 0 elements
        #[expect(clippy::cast_sign_loss, clippy::shadow_reuse)]
        let len = len as u64;
        if len == 0 {
            return Err(Error {
                kind: ErrorKind::ArrayOfZeroElements,
                col: len_token.col,
                pointers_count: len_token.kind.display_len(self.tokens),
            });
        }

        let close_square_bracket_token = self.current_token(Expected::ClosingSquareBracket)?;
        let TokenKind::CloseSquareBracket = close_square_bracket_token.kind else {
            return Err(Error {
                kind: ErrorKind::MissingClosingSquareBracketInArrayType,
                col: open_square_bracket_token.col,
                pointers_count: open_square_bracket_token.kind.display_len(self.tokens),
            });
        };

        return Ok(Some((close_square_bracket_token, Type::Array { base_type, len })));
    }

    fn expression_from_base_type(&mut self, typ: BaseType) -> Expression {
        return match typ {
            BaseType::I64 => Expression::I64(0),
            BaseType::Ascii => Expression::Ascii(b'0'),
            BaseType::Bool => Expression::False,
            BaseType::Str => {
                // FIX(stefano): proper empty string handling
                let string_label = self.string_label;
                let string = &self.src.code()[0..0];
                self.ast.string_labels.push((string_label, string));
                self.string_label += 1;
                Expression::Str { label: string_label }
            }
        };
    }

    fn variable_definition(&mut self) -> Result<Variable<'code>, Error<ErrorKind>> {
        let name_token = self.next_token_bounded(Expected::Identifier)?;
        let name = match name_token.kind {
            TokenKind::Identifier(name_index) | TokenKind::IdentifierStr(name_index) => {
                let name = self.tokens.text[name_index as usize];
                match self.resolve_type(name.as_bytes()) {
                    None => name,
                    Some(_) => {
                        return Err(Error {
                            kind: ErrorKind::TypeInVariableName,
                            col: name_token.col,
                            pointers_count: name_token.kind.display_len(self.tokens),
                        })
                    }
                }
            }
            TokenKind::Comment(_)
            | TokenKind::BlockComment(_)
            | TokenKind::Unexpected(_)
            | TokenKind::OpenRoundBracket
            | TokenKind::CloseRoundBracket
            | TokenKind::OpenSquareBracket
            | TokenKind::CloseSquareBracket
            | TokenKind::OpenCurlyBracket
            | TokenKind::CloseCurlyBracket
            | TokenKind::Colon
            | TokenKind::SemiColon
            | TokenKind::Comma
            | TokenKind::Op(_)
            | TokenKind::False
            | TokenKind::True
            | TokenKind::BinaryInteger(_)
            | TokenKind::OctalInteger(_)
            | TokenKind::DecimalInteger(_)
            | TokenKind::HexadecimalInteger(_)
            | TokenKind::Ascii(_)
            | TokenKind::Str(_)
            | TokenKind::RawStr(_) => {
                return Err(Error {
                    kind: ErrorKind::ExpectedVariableName,
                    col: name_token.col,
                    pointers_count: name_token.kind.display_len(self.tokens),
                })
            }
            TokenKind::Let
            | TokenKind::Var
            | TokenKind::Print
            | TokenKind::PrintLn
            | TokenKind::Eprint
            | TokenKind::EprintLn
            | TokenKind::Do
            | TokenKind::If
            | TokenKind::Else
            | TokenKind::Loop
            | TokenKind::Break
            | TokenKind::Continue => {
                return Err(Error {
                    kind: ErrorKind::KeywordInVariableName,
                    col: name_token.col,
                    pointers_count: name_token.kind.display_len(self.tokens),
                })
            }
        };

        let annotation = self.type_annotation()?;

        let equals_or_semicolon_token = self.next_token_bounded(Expected::EqualsOrSemicolon)?;

        let expression = match equals_or_semicolon_token.kind {
            TokenKind::Op(Op::Equals) => {
                _ = self.next_token();
                Some(self.expression()?)
            }
            TokenKind::SemiColon => None,
            TokenKind::Op(_)
            | TokenKind::Comment(_)
            | TokenKind::BlockComment(_)
            | TokenKind::Unexpected(_)
            | TokenKind::OpenRoundBracket
            | TokenKind::CloseRoundBracket
            | TokenKind::OpenSquareBracket
            | TokenKind::CloseSquareBracket
            | TokenKind::OpenCurlyBracket
            | TokenKind::CloseCurlyBracket
            | TokenKind::Colon
            | TokenKind::Comma
            | TokenKind::False
            | TokenKind::True
            | TokenKind::BinaryInteger(_)
            | TokenKind::OctalInteger(_)
            | TokenKind::DecimalInteger(_)
            | TokenKind::HexadecimalInteger(_)
            | TokenKind::Ascii(_)
            | TokenKind::Str(_)
            | TokenKind::RawStr(_)
            | TokenKind::Identifier(_)
            | TokenKind::IdentifierStr(_)
            | TokenKind::Let
            | TokenKind::Var
            | TokenKind::Print
            | TokenKind::PrintLn
            | TokenKind::Eprint
            | TokenKind::EprintLn
            | TokenKind::Do
            | TokenKind::If
            | TokenKind::Else
            | TokenKind::Loop
            | TokenKind::Break
            | TokenKind::Continue => match annotation {
                None => {
                    return Err(Error {
                        kind: ErrorKind::ExpectedEqualsOrSemicolonAfterVariableName,
                        col: name_token.col,
                        pointers_count: name_token.kind.display_len(self.tokens),
                    })
                }
                Some((annotation_token, _)) => {
                    return Err(Error {
                        kind: ErrorKind::ExpectedEqualsOrSemicolonAfterTypeAnnotation,
                        col: annotation_token.col,
                        pointers_count: annotation_token.kind.display_len(self.tokens),
                    })
                }
            },
        };

        let None = self.resolve_variable(name.as_bytes()) else {
            return Err(Error {
                kind: ErrorKind::VariableAlreadyDefined,
                col: name_token.col,
                pointers_count: name_token.kind.display_len(self.tokens),
            });
        };

        return match expression {
            Some(value) => {
                let value_typ = value.typ();
                if let Some((token, annotation_typ)) = annotation {
                    if annotation_typ != value_typ {
                        return Err(Error {
                            kind: ErrorKind::VariableDefinitionTypeMismatch {
                                expected: annotation_typ,
                                actual: value_typ,
                            },
                            col: token.col,
                            pointers_count: token.kind.display_len(self.tokens),
                        });
                    }
                }
                self.semicolon()?;
                Ok(Variable { name: name.as_bytes(), value })
            }
            None => match annotation {
                Some((_, typ)) => {
                    let value = match typ {
                        Type::Base(base_type) => self.expression_from_base_type(base_type),
                        Type::Array { base_type, len } => {
                            debug_assert!(len > 0, "arrays of 0 items are not allowed");
                            #[expect(clippy::cast_possible_truncation)]
                            let items =
                                vec![self.expression_from_base_type(base_type); len as usize];
                            debug_assert!(items.len() > 0, "arrays of 0 items are not allowed");
                            Expression::Array { base_type, items }
                        }
                    };
                    self.semicolon()?;
                    Ok(Variable { name: name.as_bytes(), value })
                }
                None => Err(Error {
                    kind: ErrorKind::CannotInferTypeOfVariable,
                    col: name_token.col,
                    pointers_count: name_token.kind.display_len(self.tokens),
                }),
            },
        };
    }

    // NOTE(stefano): mutations of string characters are disallowed until a sort of "borrow checker" is developed
    fn reassignment(
        &mut self,
        target: Expression,
        target_token: Token,
        op: AssignmentOp,
        op_token: Token,
    ) -> Result<Node, Error<ErrorKind>> {
        let (error_token, target_type) = match &target {
            Expression::ArrayIndex { base_type, indexable_index, .. } => {
                let indexable = &self.ast.expressions[*indexable_index as usize];
                let mut unwrapped_indexable = indexable;
                while let Expression::ArrayIndex {
                    indexable_index: inner_indexable_index, ..
                } = unwrapped_indexable
                {
                    let inner_indexable = &self.ast.expressions[*inner_indexable_index as usize];
                    unwrapped_indexable = inner_indexable;
                }

                let Expression::Variable { typ, .. } = unwrapped_indexable else {
                    return Err(Error {
                        kind: ErrorKind::CannotAssignToExpression,
                        col: op_token.col,
                        pointers_count: op_token.kind.display_len(self.tokens),
                    });
                };

                let error_token = if let TokenKind::Identifier(name_index) = target_token.kind {
                    let name = self.tokens.text[name_index as usize];
                    if let Some(_) = self.resolve_let_variable(name.as_bytes()) {
                        return Err(Error {
                            kind: ErrorKind::CannotMutateVariable,
                            col: target_token.col,
                            pointers_count: target_token.kind.display_len(self.tokens),
                        });
                    }

                    if let BaseType::Str = typ.base_typ() {
                        if let BaseType::Ascii = base_type {
                            return Err(Error {
                                kind: ErrorKind::CannotMutateStringCharacters,
                                col: target_token.col,
                                pointers_count: target_token.kind.display_len(self.tokens),
                            });
                        }
                    }

                    target_token
                } else {
                    op_token
                };

                (error_token, Type::Base(*base_type))
            }
            Expression::Variable { typ, variable_index } => {
                let var = &self.ast.variables[*variable_index as usize];
                if let Some(_) = self.resolve_let_variable(var.name) {
                    return Err(Error {
                        kind: ErrorKind::CannotMutateVariable,
                        col: target_token.col,
                        pointers_count: target_token.kind.display_len(self.tokens),
                    });
                }

                (target_token, *typ)
            }

            Expression::False
            | Expression::True
            | Expression::I64(_)
            | Expression::Ascii(_)
            | Expression::Str { .. }
            | Expression::Array { .. }
            | Expression::Parenthesis { .. }
            | Expression::Unary { .. }
            | Expression::BooleanUnary { .. }
            | Expression::Binary { .. }
            | Expression::BooleanBinary { .. }
            | Expression::Comparison { .. }
            | Expression::Temporary { .. } => {
                return Err(Error {
                    kind: ErrorKind::CannotAssignToExpression,
                    col: op_token.col,
                    pointers_count: op_token.kind.display_len(self.tokens),
                });
            }
        };

        _ = self.next_token();
        let new_value = self.expression()?;
        let new_value_type = new_value.typ();

        return match op {
            AssignmentOp::Equals if target_type == new_value_type => {
                Ok(Node::Reassignment { target, op, op_col: op_token.col, new_value })
            }
            AssignmentOp::Equals => Err(Error {
                kind: ErrorKind::VariableReassignmentTypeMismatch {
                    expected: target_type,
                    actual: new_value_type,
                },
                col: error_token.col,
                pointers_count: error_token.kind.display_len(self.tokens),
            }),
            AssignmentOp::Pow
            | AssignmentOp::WrappingPow
            | AssignmentOp::SaturatingPow
            | AssignmentOp::Times
            | AssignmentOp::WrappingTimes
            | AssignmentOp::SaturatingTimes
            | AssignmentOp::Divide
            | AssignmentOp::WrappingDivide
            | AssignmentOp::SaturatingDivide
            | AssignmentOp::Remainder
            | AssignmentOp::Plus
            | AssignmentOp::WrappingPlus
            | AssignmentOp::SaturatingPlus
            | AssignmentOp::Minus
            | AssignmentOp::WrappingMinus
            | AssignmentOp::SaturatingMinus
            | AssignmentOp::LeftShift
            | AssignmentOp::WrappingLeftShift
            | AssignmentOp::SaturatingLeftShift
            | AssignmentOp::RightShift
            | AssignmentOp::LeftRotate
            | AssignmentOp::RightRotate
            | AssignmentOp::And
            | AssignmentOp::BitAnd
            | AssignmentOp::BitXor
            | AssignmentOp::Or
            | AssignmentOp::BitOr => match (target_type, new_value_type) {
                (
                    Type::Base(BaseType::I64),
                    Type::Base(BaseType::I64 | BaseType::Ascii | BaseType::Bool),
                ) => Ok(Node::Reassignment { target, op, op_col: op_token.col, new_value }),
                /* IDEA(stefano):
                allow only certain kinds of *op*=:
                ```kay
                var condition = true;
                condition &&= false; # should instead be allowed
                ```
                */
                (Type::Base(BaseType::Ascii | BaseType::Bool), _) => Err(Error {
                    kind: ErrorKind::CannotModifyInplace(target_type),
                    col: op_token.col,
                    pointers_count: op_token.kind.display_len(self.tokens),
                }),
                _ => Err(Error {
                    kind: ErrorKind::VariableReassignmentTypeMismatch {
                        expected: target_type,
                        actual: new_value_type,
                    },
                    col: error_token.col,
                    pointers_count: error_token.kind.display_len(self.tokens),
                }),
            },
        };
    }
}

// print statements
impl Parser<'_, '_, '_, '_> {
    fn print_arg(&mut self) -> Result<Expression, Error<ErrorKind>> {
        let _start_of_expression_token = self.next_token_bounded(Expected::Expression)?;
        let argument = self.expression()?;
        if let Expression::Array { .. } = argument {
            #[expect(clippy::cast_possible_truncation)]
            let temporary_value_index = self.ast.temporaries.len() as ExpressionIndex;
            let argument_type = argument.typ();
            self.ast.temporaries.push(argument);
            return Ok(Expression::Temporary { typ: argument_type, temporary_value_index });
        };

        return Ok(argument);
    }
}

// if statements
impl Parser<'_, '_, '_, '_> {
    fn iff(&mut self) -> Result<Node, Error<ErrorKind>> {
        let mut ifs = Vec::new();
        let mut els = None;

        'iff: while let Some(if_token) = self.tokens.tokens.get(self.token as usize) {
            _ = self.next_token_bounded(Expected::BooleanExpression)?;

            let condition = self.expression()?;
            let Type::Base(BaseType::Bool) = condition.typ() else {
                return Err(Error {
                    kind: ErrorKind::IfMustBeFollowedByBooleanExpression,
                    col: if_token.col,
                    pointers_count: if_token.kind.display_len(self.tokens),
                });
            };

            let after_condition_token = self.current_token(Expected::DoOrBlock)?;
            let if_statement = match after_condition_token.kind {
                TokenKind::OpenCurlyBracket => {
                    let scope = self.any(after_condition_token)?;
                    IfStatement { condition, statement: scope }
                }
                TokenKind::Do => {
                    let statement = self.do_statement()?;
                    IfStatement { condition, statement }
                }
                TokenKind::OpenRoundBracket
                | TokenKind::CloseRoundBracket
                | TokenKind::OpenSquareBracket
                | TokenKind::CloseSquareBracket
                | TokenKind::CloseCurlyBracket
                | TokenKind::Comment(_)
                | TokenKind::BlockComment(_)
                | TokenKind::Unexpected(_)
                | TokenKind::Colon
                | TokenKind::SemiColon
                | TokenKind::Comma
                | TokenKind::Op(_)
                | TokenKind::False
                | TokenKind::True
                | TokenKind::BinaryInteger(_)
                | TokenKind::OctalInteger(_)
                | TokenKind::DecimalInteger(_)
                | TokenKind::HexadecimalInteger(_)
                | TokenKind::Ascii(_)
                | TokenKind::Str(_)
                | TokenKind::RawStr(_)
                | TokenKind::Identifier(_)
                | TokenKind::IdentifierStr(_)
                | TokenKind::Let
                | TokenKind::Var
                | TokenKind::Print
                | TokenKind::PrintLn
                | TokenKind::Eprint
                | TokenKind::EprintLn
                | TokenKind::If
                | TokenKind::Else
                | TokenKind::Loop
                | TokenKind::Break
                | TokenKind::Continue => {
                    let before_curly_bracket_token = self.peek_previous_token();
                    return Err(Error {
                        kind: ErrorKind::IfMustBeFollowedByDoOrBlock,
                        col: before_curly_bracket_token.col,
                        pointers_count: before_curly_bracket_token.kind.display_len(self.tokens),
                    });
                }
            };

            ifs.push(if_statement);

            while let Some(else_token) = self.tokens.tokens.get(self.token as usize) {
                let after_else_token = match else_token.kind {
                    TokenKind::Else => self.next_token_bounded(Expected::DoOrBlockOrIfStatement)?,
                    TokenKind::Comment(_)
                    | TokenKind::BlockComment(_)
                    | TokenKind::Unexpected(_)
                    | TokenKind::OpenRoundBracket
                    | TokenKind::CloseRoundBracket
                    | TokenKind::OpenSquareBracket
                    | TokenKind::CloseSquareBracket
                    | TokenKind::OpenCurlyBracket
                    | TokenKind::CloseCurlyBracket
                    | TokenKind::Colon
                    | TokenKind::SemiColon
                    | TokenKind::Comma
                    | TokenKind::Op(_)
                    | TokenKind::False
                    | TokenKind::True
                    | TokenKind::BinaryInteger(_)
                    | TokenKind::OctalInteger(_)
                    | TokenKind::DecimalInteger(_)
                    | TokenKind::HexadecimalInteger(_)
                    | TokenKind::Ascii(_)
                    | TokenKind::Str(_)
                    | TokenKind::RawStr(_)
                    | TokenKind::Identifier(_)
                    | TokenKind::IdentifierStr(_)
                    | TokenKind::Let
                    | TokenKind::Var
                    | TokenKind::Print
                    | TokenKind::PrintLn
                    | TokenKind::Eprint
                    | TokenKind::EprintLn
                    | TokenKind::Do
                    | TokenKind::If
                    | TokenKind::Loop
                    | TokenKind::Break
                    | TokenKind::Continue => break 'iff,
                };

                // we are now inside an else branch
                let else_if = match after_else_token.kind {
                    TokenKind::OpenCurlyBracket => {
                        let scope = self.any(after_else_token)?;
                        els = Some(scope);
                        break 'iff;
                    }
                    TokenKind::Do => {
                        let statement = self.do_statement()?;
                        els = Some(statement);
                        break 'iff;
                    }
                    TokenKind::If => break,
                    TokenKind::OpenRoundBracket
                    | TokenKind::CloseRoundBracket
                    | TokenKind::OpenSquareBracket
                    | TokenKind::CloseSquareBracket
                    | TokenKind::CloseCurlyBracket
                    | TokenKind::Comment(_)
                    | TokenKind::BlockComment(_)
                    | TokenKind::Unexpected(_)
                    | TokenKind::Colon
                    | TokenKind::SemiColon
                    | TokenKind::Comma
                    | TokenKind::Op(_)
                    | TokenKind::False
                    | TokenKind::True
                    | TokenKind::BinaryInteger(_)
                    | TokenKind::OctalInteger(_)
                    | TokenKind::DecimalInteger(_)
                    | TokenKind::HexadecimalInteger(_)
                    | TokenKind::Ascii(_)
                    | TokenKind::Str(_)
                    | TokenKind::RawStr(_)
                    | TokenKind::Identifier(_)
                    | TokenKind::IdentifierStr(_)
                    | TokenKind::Let
                    | TokenKind::Var
                    | TokenKind::Print
                    | TokenKind::PrintLn
                    | TokenKind::Eprint
                    | TokenKind::EprintLn
                    | TokenKind::Else
                    | TokenKind::Loop
                    | TokenKind::Break
                    | TokenKind::Continue => Err(Error {
                        kind: ErrorKind::MustBeFollowedByDoOrBlockOrIfStatement,
                        col: else_token.col,
                        pointers_count: else_token.kind.display_len(self.tokens),
                    }),
                };

                else_if?;
            }
        }

        #[expect(clippy::cast_possible_truncation)]
        let if_index = self.ast.ifs.len() as IfIndex;
        self.ast.ifs.push(If { ifs, els });
        return Ok(Node::If(if_index));
    }
}

// loop statements
impl Parser<'_, '_, '_, '_> {
    fn loop_statement(&mut self) -> Result<Node, Error<ErrorKind>> {
        let do_token = self.tokens.tokens[self.token as usize];
        let loop_token = match do_token.kind {
            TokenKind::Do => {
                let loop_token = self.next_token_bounded(Expected::LoopStatement)?;
                let TokenKind::Loop = loop_token.kind else {
                    return Err(Error {
                        kind: ErrorKind::DoMustBeFollowedByLoop,
                        col: do_token.col,
                        pointers_count: do_token.kind.display_len(self.tokens),
                    });
                };

                loop_token
            }
            TokenKind::Comment(_)
            | TokenKind::BlockComment(_)
            | TokenKind::Unexpected(_)
            | TokenKind::OpenRoundBracket
            | TokenKind::CloseRoundBracket
            | TokenKind::OpenSquareBracket
            | TokenKind::CloseSquareBracket
            | TokenKind::OpenCurlyBracket
            | TokenKind::CloseCurlyBracket
            | TokenKind::Colon
            | TokenKind::SemiColon
            | TokenKind::Comma
            | TokenKind::Op(_)
            | TokenKind::False
            | TokenKind::True
            | TokenKind::BinaryInteger(_)
            | TokenKind::OctalInteger(_)
            | TokenKind::DecimalInteger(_)
            | TokenKind::HexadecimalInteger(_)
            | TokenKind::Ascii(_)
            | TokenKind::Str(_)
            | TokenKind::RawStr(_)
            | TokenKind::Identifier(_)
            | TokenKind::IdentifierStr(_)
            | TokenKind::Let
            | TokenKind::Var
            | TokenKind::Print
            | TokenKind::PrintLn
            | TokenKind::Eprint
            | TokenKind::EprintLn
            | TokenKind::If
            | TokenKind::Else
            | TokenKind::Loop
            | TokenKind::Break
            | TokenKind::Continue => do_token,
        };

        _ = self.next_token_bounded(Expected::BooleanExpression)?;
        let condition = self.expression()?;
        let Type::Base(BaseType::Bool) = condition.typ() else {
            return Err(Error {
                kind: ErrorKind::LoopMustBeFollowedByBooleanExpression,
                col: loop_token.col,
                pointers_count: loop_token.kind.display_len(self.tokens),
            });
        };

        let after_condition_token = self.current_token(Expected::DoOrBlock)?;
        let statement_result = match after_condition_token.kind {
            TokenKind::OpenCurlyBracket => {
                let scope = self.any(after_condition_token)?;
                Ok(scope)
            }
            TokenKind::Do => {
                let statement = self.do_statement()?;
                Ok(statement)
            }
            TokenKind::OpenRoundBracket
            | TokenKind::CloseRoundBracket
            | TokenKind::OpenSquareBracket
            | TokenKind::CloseSquareBracket
            | TokenKind::CloseCurlyBracket
            | TokenKind::Comment(_)
            | TokenKind::BlockComment(_)
            | TokenKind::Unexpected(_)
            | TokenKind::Colon
            | TokenKind::SemiColon
            | TokenKind::Comma
            | TokenKind::Op(_)
            | TokenKind::False
            | TokenKind::True
            | TokenKind::BinaryInteger(_)
            | TokenKind::OctalInteger(_)
            | TokenKind::DecimalInteger(_)
            | TokenKind::HexadecimalInteger(_)
            | TokenKind::Ascii(_)
            | TokenKind::Str(_)
            | TokenKind::RawStr(_)
            | TokenKind::Identifier(_)
            | TokenKind::IdentifierStr(_)
            | TokenKind::Let
            | TokenKind::Var
            | TokenKind::Print
            | TokenKind::PrintLn
            | TokenKind::Eprint
            | TokenKind::EprintLn
            | TokenKind::If
            | TokenKind::Else
            | TokenKind::Loop
            | TokenKind::Break
            | TokenKind::Continue => {
                let before_curly_bracket_token = self.peek_previous_token();
                Err(Error {
                    kind: ErrorKind::LoopMustBeFollowedByDoOrBlock,
                    col: before_curly_bracket_token.col,
                    pointers_count: before_curly_bracket_token.kind.display_len(self.tokens),
                })
            }
        };

        let statement = statement_result?;
        #[expect(clippy::cast_possible_truncation)]
        let loop_index = self.ast.loops.len() as LoopIndex;
        self.ast.loops.push(Loop { condition, statement });
        return if let TokenKind::Do = do_token.kind {
            Ok(Node::DoLoop(loop_index))
        } else {
            Ok(Node::Loop(loop_index))
        };
    }
}

#[derive(Debug, Clone)]
pub enum Expected {
    StatementAfterDo,
    Semicolon,
    OperatorOrSemicolon,
    Expression,
    BooleanExpression,
    ClosingSquareBracket,
    ClosingRoundBracket,
    ArrayElementOrClosingSquareBracket,
    CommaOrClosingSquareBracket,
    TypeAnnotationOrVariableDefinition,
    TypeAnnotation,
    ArrayLength,
    Identifier,
    EqualsOrSemicolon,
    DoOrBlock,
    DoOrBlockOrIfStatement,
    LoopStatement,
}

impl Display for Expected {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return match self {
            Self::StatementAfterDo => write!(f, "statement after do keyword"),
            Self::Semicolon => write!(f, "semicolon"),
            Self::OperatorOrSemicolon => write!(f, "operator or semicolon"),
            Self::Expression => write!(f, "expression"),
            Self::BooleanExpression => write!(f, "boolean expression"),
            Self::ClosingSquareBracket => write!(f, "closing square bracket"),
            Self::ClosingRoundBracket => write!(f, "closing round bracket"),
            Self::ArrayElementOrClosingSquareBracket => {
                write!(f, "array item or closing square bracket")
            }
            Self::CommaOrClosingSquareBracket => write!(f, "comma or closing square bracket"),
            Self::TypeAnnotationOrVariableDefinition => {
                write!(f, "type annotation or variable definition")
            }
            Self::TypeAnnotation => write!(f, "type annotation"),
            Self::ArrayLength => write!(f, "array length"),
            Self::Identifier => write!(f, "identifier"),
            Self::EqualsOrSemicolon => write!(f, "'=' or ';'"),
            Self::DoOrBlock => write!(f, "do statement or block"),
            Self::DoOrBlockOrIfStatement => write!(f, "do statement, block or if statement"),
            Self::LoopStatement => write!(f, "loop statement"),
        };
    }
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    PrematureEndOfFile(Expected),

    MinusZeroInteger,

    BinaryIntegerOverflow,
    OctalIntegerOverflow,
    DecimalIntegerOverflow,
    HexadecimalIntegerOverflow,

    BinaryIntegerUnderflow,
    OctalIntegerUnderflow,
    DecimalIntegerUnderflow,
    HexadecimalIntegerUnderflow,

    MissingSemicolon,

    LeftOperandTypeMismatch(Type),
    RightOperandTypeMismatch(Type),
    ExpectedNumberLiteralInArrayType,
    ExpectedNumberLiteralInArrayIndex,
    MissingClosingSquareBracketInIndex,
    MissingClosingSquareBracketInArrayType,
    CannotIndexNonArrayType(Type),
    CannotMutateStringCharacters,
    CannotIndexIntoExpression,
    TypeInExpression,
    EmptyExpression,
    UnclosedRoundBracket,
    ArrayOfNegativeLength,
    ArrayOfZeroElements,
    NestedArrayNotSupportedYet,
    ArrayElementTypeMismatch { actual: Type, expected: Type },
    CannotTakeLenOf(Type),
    CannotTakeAbsoluteValueOf(Type),
    CannotNegate(Type),
    CannotInvert(Type),
    KeywordInExpression,
    ExpectedOperand,
    CannotCompareOperands { lhs_type: Type, rhs_type: Type },
    CannotChainComparisons,

    VariableNotPreviouslyDefined,
    VariableAlreadyDefined,
    ExpectedType,
    TypeInVariableName,
    TypeInVariableReassignment,
    ExpectedVariableName,
    KeywordInVariableName,
    ExpectedEqualsOrSemicolonAfterVariableName,
    ExpectedEqualsOrSemicolonAfterTypeAnnotation,
    VariableDefinitionTypeMismatch { actual: Type, expected: Type },
    VariableReassignmentTypeMismatch { actual: Type, expected: Type },
    CannotInferTypeOfVariable,
    CannotMutateVariable,
    CannotModifyInplace(Type),
    CannotAssignToExpression,

    StrayColon,
    StrayComma,
    StrayOperator(Op),

    StrayElseBlock,
    IfMustBeFollowedByBooleanExpression,
    IfMustBeFollowedByDoOrBlock,
    MustBeFollowedByDoOrBlockOrIfStatement,

    DoMustBeFollowedByLoop,
    LoopMustBeFollowedByBooleanExpression,
    LoopMustBeFollowedByDoOrBlock,
    StrayBreakStatement,
    StrayContinueStatement,

    BlockInDoStatement,
    VariableInDoStatement, // IDEA(stefano): allow variables and emit an unused variable warning instead
}

impl IntoErrorInfo for ErrorKind {
    fn info(&self) -> ErrorInfo {
        let (error_message, error_cause_message) = match self {
            Self::PrematureEndOfFile(expected) => (
                "premature end of file".into(),
                format!("expected {expected} after here").into(),
            ),

            Self::MinusZeroInteger => (
                "invalid integer literal".into(),
                "-0 is not a valid two's complement integer".into(),
            ),
            Self::BinaryIntegerOverflow => (
                "integer literal overflow".into(),
                format!(
                    "overflows a {bits} bit signed integer, over {prefix}{max:0b} ({max})",
                    bits = i64::BITS,
                    prefix = Base::Binary.prefix(),
                    max = i64::MAX
                ).into(),
            ),
            Self::OctalIntegerOverflow => (
                "integer literal overflow".into(),
                format!(
                    "overflows a {bits} bit signed integer, over {prefix}{max:0o} ({max})",
                    bits = i64::BITS,
                    prefix = Base::Octal.prefix(),
                    max = i64::MAX
                ).into(),
            ),
            Self::DecimalIntegerOverflow => (
                "integer literal overflow".into(),
                format!(
                    "overflows a {bits} bit signed integer, over {max}",
                    bits = i64::BITS,
                    max = i64::MAX
                ).into(),
            ),
            Self::HexadecimalIntegerOverflow => (
                "integer literal overflow".into(),
                format!(
                    "overflows a {bits} bit signed integer, over {prefix}{max:0x} ({max})",
                    bits = i64::BITS,
                    prefix = Base::Hexadecimal.prefix(),
                    max = i64::MAX
                ).into(),
            ),
            Self::BinaryIntegerUnderflow => (
                "integer literal underflow".into(),
                format!(
                    "underflows a {bits} bit signed integer, under {prefix}{min:0b} ({min})",
                    bits = i64::BITS,
                    prefix = Base::Binary.prefix(),
                    min = i64::MIN
                ).into(),
            ),
            Self::OctalIntegerUnderflow => (
                "integer literal underflow".into(),
                format!(
                    "underflows a {bits} bit signed integer, under {prefix}{min:0o} ({min})",
                    bits = i64::BITS,
                    prefix = Base::Octal.prefix(),
                    min = i64::MIN
                ).into(),
            ),
            Self::DecimalIntegerUnderflow => (
                "integer literal underflow".into(),
                format!(
                    "underflows a {bits} bit signed integer, under {min}",
                    bits = i64::BITS,
                    min = i64::MIN
                ).into(),
            ),
            Self::HexadecimalIntegerUnderflow => (
                "integer literal underflow".into(),
                format!(
                    "underflows a {bits} bit signed integer, under {prefix}{min:0x} ({min})",
                    bits = i64::BITS,
                    prefix = Base::Hexadecimal.prefix(),
                    min = i64::MIN
                ).into(),
            ),

            Self::MissingSemicolon => (
                "invalid statement".into(),
                "missing semicolon after here".into(),
            ),

            Self::LeftOperandTypeMismatch(invalid_type) => (
                "invalid expression".into(),
                format!("cannot be preceded by '{invalid_type}'").into(),
            ),
            Self::RightOperandTypeMismatch(invalid_type) => (
                "invalid expression".into(),
                format!("cannot be followed by '{invalid_type}'").into(),
            ),
            Self::ExpectedNumberLiteralInArrayType => (
                "invalid type".into(),
                "must be followed by an integer literal".into(),
            ),
            Self::ExpectedNumberLiteralInArrayIndex => (
                "invalid array index".into(),
                "must be followed by an integer literal".into(),
            ),
            Self::MissingClosingSquareBracketInIndex => (
                "invalid array index".into(),
                "must be followed by a closing square bracket".into(),
            ),
            Self::MissingClosingSquareBracketInArrayType => (
                "invalid type".into(),
                "must be followed by a closing square bracket".into(),
            ),
            Self::CannotMutateStringCharacters => (
                "invalid variable reassignment".into(),
                "cannot mutate string characters".into(),
            ),
            Self::CannotIndexNonArrayType(non_indexable_type) => (
                "invalid expression".into(),
                format!("cannot index into a value of type '{non_indexable_type}'").into(),
            ),
            Self::CannotIndexIntoExpression => (
                "invalid expression".into(),
                "cannot index into an expression".into(),
            ),
            Self::VariableNotPreviouslyDefined => (
                "variable not previously defined".into(),
                "was not previously defined".into(),
            ),
            Self::VariableAlreadyDefined => (
                "variable already defined".into(),
                "was already defined".into(),
            ),
            Self::TypeInExpression => (
                "invalid expression".into(),
                "types are not allowed in expressions".into(),
            ),
            Self::EmptyExpression => (
                "invalid expression".into(),
                "empty expressions are not allowed".into(),
            ),
            Self::UnclosedRoundBracket => (
                "invalid expression".into(),
                "'(' bracket was not closed".into(),
            ),
            Self::ArrayOfNegativeLength => (
                "invalid array length".into(),
                "array length must be greater than 1".into(),
            ),
            Self::ArrayOfZeroElements => (
                "invalid array".into(),
                "arrays of zero items are not allowed yet".into(),
            ),
            Self::NestedArrayNotSupportedYet => (
                "invalid array element".into(),
                "nested arrays are not supported yet".into(),
            ),
            Self::ArrayElementTypeMismatch { actual, expected } => (
                "invalid array element".into(),
                format!("expected item of type '{expected}', but got '{actual}'").into(),
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
            Self::KeywordInExpression => (
                "invalid expression".into(),
                "cannot be a keyword".into(),
            ),
            Self::ExpectedOperand => (
                "invalid expression".into(),
                "expected expression operand before this token".into(),
            ),
            Self::CannotCompareOperands { lhs_type, rhs_type } => (
                "invalid expression".into(),
                format!("cannot compare '{lhs_type}' to '{rhs_type}'").into(),
            ),
            Self::CannotChainComparisons => (
                "invalid expression".into(),
                "comparison operators cannot be chained".into(),
            ),
            Self::ExpectedType => (
                "invalid type annotation".into(),
                "expected type after here".into(),
            ),
            Self::TypeInVariableName => (
                "invalid variable name".into(),
                "types are not allowed in variable names".into(),
            ),
            Self::TypeInVariableReassignment => (
                "invalid variable reassignment".into(),
                "cannot assign to a type".into(),
            ),
            Self::ExpectedVariableName => (
                "invalid variable name".into(),
                "expected variable name after here".into(),
            ),
            Self::KeywordInVariableName => (
                "invalid variable name".into(),
                "cannot be a keyword".into(),
            ),
            Self::ExpectedEqualsOrSemicolonAfterVariableName => (
                "invalid variable definition".into(),
                "expected '=' or ';' after variable name".into(),
            ),
            Self::ExpectedEqualsOrSemicolonAfterTypeAnnotation => (
                "invalid variable definition".into(),
                "expected '=' or ';' after type annotation".into(),
            ),
            Self::VariableDefinitionTypeMismatch { actual, expected } => (
                "invalid variable definition".into(),
                format!("value of type '{actual}' doesn't match declared type of '{expected}'").into(),
            ),
            Self::VariableReassignmentTypeMismatch { actual, expected } => (
                "invalid variable definition".into(),
                format!("trying to assign a value of type '{actual}' to a variable of type '{expected}'").into(),
            ),
            Self::CannotInferTypeOfVariable => (
                "invalid variable definition".into(),
                "expected type annotation after here to infer the type of the variable".into(),
            ),
            Self::CannotMutateVariable => (
                "invalid variable reassignment".into(),
                "cannot mutate immutable variable".into(),
            ),
            Self::CannotModifyInplace(typ) => (
                "invalid variable reassignment".into(),
                format!("cannot use inplace assignment operators on `{typ}` values").into(),
            ),
            Self::CannotAssignToExpression => (
                "invalid variable reassignment".into(),
                "cannot assign to expression".into(),
            ),

            Self::StrayColon => (
                "stray colon".into(),
                "stray colon".into(),
            ),
            Self::StrayComma => (
                "stray comma".into(),
                "stray comma".into(),
            ),
            Self::StrayOperator(operator) => (
                format!("stray operator '{operator}'").into(),
                format!("stray operator '{operator}'").into(),
            ),

            Self::StrayElseBlock => (
                "stray else block".into(),
                "no matching if statement previously found".into(),
            ),
            Self::IfMustBeFollowedByBooleanExpression => (
                "invalid if condition".into(),
                "must be followed by a boolean expression".into(),
            ),
            Self::IfMustBeFollowedByDoOrBlock => (
                "invalid if statement".into(),
                "must be followed by a do statement or a block".into(),
            ),
            Self::MustBeFollowedByDoOrBlockOrIfStatement => (
                "invalid if statement".into(),
                "must be followed by a do statement, a block or an other if statement".into(),
            ),

            Self::DoMustBeFollowedByLoop => (
                "invalid do loop".into(),
                "must be followed by a loop statement".into(),
            ),
            Self::LoopMustBeFollowedByBooleanExpression => (
                "invalid if condition".into(),
                "must be followed by a boolean expression".into(),
            ),
            Self::LoopMustBeFollowedByDoOrBlock => (
                "invalid if statement".into(),
                "must be followed by a do statement or a block".into(),
            ),
            Self::StrayBreakStatement => (
                "stray break statement".into(),
                "cannot be used outside of loops".into(),
            ),
            Self::StrayContinueStatement => (
                "stray continue statement".into(),
                "cannot be used outside of loops".into(),
            ),

            Self::BlockInDoStatement => (
                "invalid block".into(),
                "blocks are not allowed in do statements".into(),
            ),
            Self::VariableInDoStatement => (
                "invalid variable definition".into(),
                "variable definitions are not allowed in do statements".into(),
            ),
        };

        return ErrorInfo { error_message, error_cause_message };
    }
}
