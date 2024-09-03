use super::{
    tokenizer::{
        ascii, int, uint, BracketKind, DisplayLen, Mutability, Op, RawStr, Str, Token, TokenKind,
    },
    Error, ErrorInfo, IntoErrorInfo,
};
use crate::src_file::{offset, Position, SrcFile};
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
    Int,
    Ascii,
    Bool,
    Str,
}

impl Display for BaseType {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return match self {
            Self::Int => write!(f, "int"),
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
    #[inline(always)]
    fn size(&self) -> usize {
        return match self {
            Self::Int => size_of::<int>(),
            Self::Ascii => size_of::<ascii>(),
            Self::Bool => size_of::<bool>(),
            Self::Str => size_of::<uint>() + size_of::<*const ascii>(),
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Base(BaseType),
    // TODO(stefano): enforce a max length
    Array {
        base_type: BaseType,
        /// always greater than 1, i.e: arrays always contain at least 2 items
        len: uint,
    },
}

impl Display for Type {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return match self {
            Self::Base(typ) => write!(f, "{typ}"),
            Self::Array { base_type, len } => write!(f, "{base_type}[{len}]"),
        };
    }
}

impl TypeOf for Type {
    fn typ(&self) -> Type {
        return *self;
    }
}

impl BaseTypeOf for Type {
    #[inline(always)]
    fn base_typ(&self) -> BaseType {
        return match self {
            Self::Base(typ) => *typ,
            Self::Array { base_type, .. } => *base_type,
        };
    }
}

impl SizeOf for Type {
    #[inline(always)]
    fn size(&self) -> usize {
        return match self {
            Self::Base(typ) => typ.size(),
            Self::Array { base_type, len } => base_type.size() * len,
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum UnaryOp {
    Len,
    Not,

    Plus,
    WrappingPlus,
    SaturatingPlus,

    Minus,
    WrappingMinus,
    SaturatingMinus,
}

impl Display for UnaryOp {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return match self {
            Self::Len               => write!(f, "len"),
            Self::Not               => write!(f, "!"),

            Self::Plus              => write!(f,  "+"),
            Self::WrappingPlus      => write!(f, r"+\"),
            Self::SaturatingPlus    => write!(f,  "+|"),

            Self::Minus             => write!(f,  "-"),
            Self::WrappingMinus     => write!(f, r"-\"),
            Self::SaturatingMinus   => write!(f,  "-|"),
        };
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
        return BaseType::Int;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BooleanUnaryOp {
    Not,
}

impl Display for BooleanUnaryOp {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return match self {
            Self::Not => write!(f, "!"),
        };
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BinaryOp {
    Pow,
    WrappingPow,
    SaturatingPow,

    Times,
    WrappingTimes,
    SaturatingTimes,

    Divide,
    WrappingDivide,
    SaturatingDivide,

    Remainder,

    Plus,
    WrappingPlus,
    SaturatingPlus,

    Minus,
    WrappingMinus,
    SaturatingMinus,

    LeftShift,
    WrappingLeftShift,
    SaturatingLeftShift,

    RightShift,

    LeftRotate,
    RightRotate,

    BitAnd,
    BitXor,
    BitOr,
}

impl Display for BinaryOp {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return match self {
            Self::Pow           => write!(f,  "**"),
            Self::WrappingPow   => write!(f, r"**\"),
            Self::SaturatingPow => write!(f,  "**|"),

            Self::Times             => write!(f,  "*"),
            Self::WrappingTimes     => write!(f, r"*\"),
            Self::SaturatingTimes   => write!(f,  "*|"),

            Self::Divide            => write!(f,  "/"),
            Self::WrappingDivide    => write!(f, r"/\"),
            Self::SaturatingDivide  => write!(f,  "/|"),

            Self::Remainder => write!(f, "%"),

            Self::Plus              => write!(f,  "+"),
            Self::WrappingPlus      => write!(f, r"+\"),
            Self::SaturatingPlus    => write!(f,  "+|"),

            Self::Minus             => write!(f,  "-"),
            Self::WrappingMinus     => write!(f, r"-\"),
            Self::SaturatingMinus   => write!(f,  "-|"),

            Self::LeftShift             => write!(f,  "<<"),
            Self::WrappingLeftShift     => write!(f, r"<<\"),
            Self::SaturatingLeftShift   => write!(f,  "<<|"),

            Self::RightShift    => write!(f,  ">>"),
            Self::LeftRotate    => write!(f, "<<<"),
            Self::RightRotate   => write!(f, ">>>"),

            Self::BitAnd    => write!(f, "&"),
            Self::BitOr     => write!(f, "|"),
            Self::BitXor    => write!(f, "^"),
        };
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
        return BaseType::Int;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BooleanBinaryOp {
    And,
    Or,
}

impl Display for BooleanBinaryOp {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return match self {
            Self::And   => write!(f, "&&"),
            Self::Or    => write!(f, "||"),
        };
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ComparisonOp {
    Compare,
    EqualsEquals,
    NotEquals,
    Greater,
    GreaterOrEquals,
    Less,
    LessOrEquals,
}

impl Display for ComparisonOp {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return match self {
            Self::Compare           => write!(f, "<=>"),
            Self::EqualsEquals      => write!(f, "=="),
            Self::NotEquals         => write!(f, "!="),
            Self::Greater           => write!(f, ">"),
            Self::GreaterOrEquals   => write!(f, ">="),
            Self::Less              => write!(f, "<"),
            Self::LessOrEquals      => write!(f, "<="),
        }
    }
}

impl TypeOf for ComparisonOp {
    #[inline(always)]
    fn typ(&self) -> Type {
        return Type::Base(self.base_typ());
    }
}

impl BaseTypeOf for ComparisonOp {
    #[inline(always)]
    fn base_typ(&self) -> BaseType {
        return match self {
            Self::Compare => BaseType::Int,
            Self::EqualsEquals
            | Self::NotEquals
            | Self::Greater
            | Self::GreaterOrEquals
            | Self::Less
            | Self::LessOrEquals => BaseType::Bool,
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AssignmentOp {
    Equals,

    Pow,
    WrappingPow,
    SaturatingPow,

    Times,
    WrappingTimes,
    SaturatingTimes,

    Divide,
    WrappingDivide,
    SaturatingDivide,

    Remainder,

    Plus,
    WrappingPlus,
    SaturatingPlus,

    Minus,
    WrappingMinus,
    SaturatingMinus,

    LeftShift,
    WrappingLeftShift,
    SaturatingLeftShift,

    RightShift,

    LeftRotate,
    RightRotate,

    And,
    BitAnd,
    BitXor,
    Or,
    BitOr,
}

impl Display for AssignmentOp {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return match self {
            Self::Equals => write!(f, "="),

            Self::Pow           => write!(f,  "**="),
            Self::WrappingPow   => write!(f, r"**\="),
            Self::SaturatingPow => write!(f,  "**|="),

            Self::Times             => write!(f,  "*="),
            Self::WrappingTimes     => write!(f, r"*\="),
            Self::SaturatingTimes   => write!(f,  "*|="),

            Self::Divide            => write!(f,  "/="),
            Self::WrappingDivide    => write!(f, r"/\="),
            Self::SaturatingDivide  => write!(f,  "/|="),

            Self::Remainder => write!(f, "%="),

            Self::Plus              => write!(f,  "+="),
            Self::WrappingPlus      => write!(f, r"+\="),
            Self::SaturatingPlus    => write!(f,  "+|="),

            Self::Minus             => write!(f,  "-="),
            Self::WrappingMinus     => write!(f, r"-\="),
            Self::SaturatingMinus   => write!(f,  "-|="),

            Self::And       => write!(f, "&&="),
            Self::BitAnd    => write!(f, "&="),
            Self::Or        => write!(f, "||="),
            Self::BitOr     => write!(f, "|="),
            Self::BitXor    => write!(f, "^="),

            Self::LeftShift             => write!(f,  "<<="),
            Self::WrappingLeftShift     => write!(f, r"<<\="),
            Self::SaturatingLeftShift   => write!(f,  "<<|="),

            Self::RightShift    => write!(f,  ">>="),
            Self::LeftRotate    => write!(f, "<<<="),
            Self::RightRotate   => write!(f, ">>>="),
        };
    }
}

type StringLabel = offset;
type TokenIndex = offset;
type VariableIndex = offset;
type IfIndex = offset;
type LoopIndex = offset;
type ExpressionIndex = offset;
pub(crate) type ScopeIndex = offset;

#[derive(Debug, Clone)]
pub(crate) enum Expression {
    False,
    True,
    Int(int),
    Ascii(ascii),
    Str {
        label: StringLabel,
    },
    Array {
        base_type: BaseType,
        /// arrays always contain at least 2 items
        items: Vec<Expression>, // TODO(stefano): flatten into a Vec<ExpressionIndex>
    },

    Parenthesis {
        typ: Type,
        expression_index: ExpressionIndex
    },

    Unary {
        op: UnaryOp,
        op_col: offset,
        operand_index: ExpressionIndex,
    },
    BooleanUnary {
        op: BooleanUnaryOp,
        operand_index: ExpressionIndex,
    },
    Binary {
        lhs_index: ExpressionIndex,
        op: BinaryOp,
        op_col: offset,
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
        bracket_col: offset,
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
            Self::Int(_) => Type::Base(BaseType::Int),
            Self::Ascii(_) => Type::Base(BaseType::Ascii),
            Self::Str { .. } => Type::Base(BaseType::Str),
            Self::Array { base_type, items } => {
                Type::Array { base_type: *base_type, len: items.len() }
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

impl<'src, 'ast: 'src> Expression {
    pub(crate) const fn display(&'ast self, ast: &'ast Ast<'src>) -> ExpressionDisplay<'src, 'ast> {
        return ExpressionDisplay { ast, expr: self };
    }
}

pub(crate) struct ExpressionDisplay<'src, 'ast: 'src> {
    ast: &'ast Ast<'src>,
    expr: &'ast Expression,
}

impl<'src, 'ast: 'src> ExpressionDisplay<'src, 'ast> {
    pub(crate) fn display(&self, f: &mut core::fmt::Formatter<'_>, expr: &'ast Expression) -> core::fmt::Result {
        return match expr {
            Expression::False => write!(f, "false"),
            Expression::True => write!(f, "true"),
            Expression::Int(integer) => write!(f, "{integer}"),
            Expression::Ascii(code) => write!(f, "'{}'", code.escape_ascii()),
            Expression::Str { label, .. } => write!(f, "str_{label}"),
            Expression::Array { items, .. } => {
                write!(f, "[")?;
                let mut items_iter = items.iter();
                let Some(last_item) = items_iter.next_back() else {
                    unreachable!("arrays should always contain at least 2 items");
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
                write!(f,"]")
            }
            Expression::Temporary { temporary_value_index, .. } => {
                let temp = &self.ast.temporaries[*temporary_value_index as usize];
                self.display(f, temp)
            }
            Expression::Variable { variable_index, .. } => {
                let variable = &self.ast.variables[*variable_index as usize];
                write!(f, "{}", variable.name)
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

    Definition {
        var_index: VariableIndex,
    },
    Reassignment {
        target: Expression,
        op: AssignmentOp,
        op_col: offset,
        new_value: Expression,
    },

    Scope {
        index: ScopeIndex,
    },

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
pub(crate) struct Variable<'src> {
    pub(crate) name: &'src str,
    pub(crate) value: Expression,
}

#[derive(Debug, Clone)]
pub(crate) enum StrKind {
    Str,
    RawStr,
}

// NOTE(stefano): this is in reality closer to an intermediate representation than to an AST
// TODO(stefano): introduce other representation before and after this Ast
#[derive(Debug)]
pub struct Ast<'src> {
    pub(crate) nodes: Vec<Vec<Node>>,

    pub(crate) ifs: Vec<If>,
    pub(crate) loops: Vec<Loop>,

    pub(crate) expressions: Vec<Expression>,
    pub(crate) temporaries: Vec<Expression>,
    pub(crate) variables: Vec<Variable<'src>>,

    pub(crate) strings: Vec<Str>,
    pub(crate) raw_strings: Vec<RawStr<'src>>,
    pub(crate) string_kinds: Vec<StrKind>,
}

// IDEA(stefano): build the AST, and then validate the AST afterwards
#[derive(Debug)]
pub struct Parser<'src, 'tokens: 'src> {
    src: &'src SrcFile,
    errors: Vec<Error<ErrorKind>>,

    token: TokenIndex,
    tokens: &'tokens [Token<'src>],

    loop_depth: offset,
    scope: ScopeIndex,
    scopes: Vec<Scope>,
    ast: Ast<'src>,
}

impl<'src, 'tokens: 'src> Parser<'src, 'tokens> {
    pub fn parse(
        src: &'src SrcFile,
        tokens: &'tokens [Token<'src>],
    ) -> Result<Ast<'src>, Vec<Error<ErrorKind>>> {
        let ast = Ast {
            nodes: vec![vec![]],

            ifs: Vec::new(),
            loops: Vec::new(),

            expressions: Vec::new(),
            temporaries: Vec::new(),
            variables: Vec::new(),

            strings: Vec::new(),
            raw_strings: Vec::new(),
            string_kinds: Vec::new(),
        };

        if tokens.is_empty() {
            return Ok(ast);
        }

        // skipping to the first non-comment token
        let mut token: offset = 0;
        let tokens_len = tokens.len() as offset;
        while token < tokens_len {
            let current = &tokens[token as usize];
            let TokenKind::Comment(_) = current.kind else {
                break;
            };

            token += 1;
        }

        let mut this = Self {
            src,
            errors: Vec::new(),
            token,
            tokens,
            loop_depth: 0,
            scope: 0,
            scopes: vec![Scope {
                parent: 0,
                base_types: vec![BaseType::Int, BaseType::Ascii, BaseType::Bool, BaseType::Str],
                let_variables: Vec::new(),
                var_variables: Vec::new(),
            }],
            ast
        };

        this.scope();

        return if this.errors.is_empty() { Ok(this.ast) } else { Err(this.errors) };
    }
}

// parsing of statements
impl<'src, 'tokens: 'src> Parser<'src, 'tokens> {
    fn semicolon(&mut self) -> Result<(), Error<ErrorKind>> {
        let semicolon_token = self.current_token(Expected::Semicolon)?;
        let TokenKind::SemiColon = semicolon_token.kind else {
            let previous_token = self.peek_previous_token();
            return Err(Error {
                kind: ErrorKind::MissingSemicolon,
                col: previous_token.col,
                pointers_count: previous_token.kind.display_len(),
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
        while let Some(token) = self.tokens.get(self.token as usize) {
            match self.any(token) {
                // skip to the next token after a semicolon
                Ok(Node::Semicolon) => continue,
                Ok(Node::ScopeEnd) => break,
                Ok(node) => self.ast.nodes[self.scope as usize].push(node),
                Err(err) => {
                    self.errors.push(err);

                    // consuming all remaining tokens until the end of the file
                    self.token = self.tokens.len() as offset;
                    break;
                }
            }
        }
    }

    fn statement(&mut self, token: &'tokens Token<'src>) -> Result<Node, Error<ErrorKind>> {
        return match token.kind {
            TokenKind::False
            | TokenKind::True
            | TokenKind::Integer(_)
            | TokenKind::Ascii(_)
            | TokenKind::Str(_)
            | TokenKind::RawStr(_)
            | TokenKind::Identifier(_)
            | TokenKind::Bracket(BracketKind::OpenRound | BracketKind::OpenSquare)
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
                            let temporary_value_index = self.ast.temporaries.len() as offset;
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
                    TokenKind::Op(op) => match op {
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
                            let assignment_op = match op {
                                Op::Equals => AssignmentOp::Equals,
                                Op::PowEquals => AssignmentOp::Pow,
                                Op::WrappingPowEquals => AssignmentOp::WrappingPow,
                                Op::SaturatingPowEquals => AssignmentOp::SaturatingPow,
                                Op::TimesEquals => AssignmentOp::Times,
                                Op::WrappingTimesEquals => AssignmentOp::WrappingTimes,
                                Op::SaturatingTimesEquals => AssignmentOp::SaturatingTimes,
                                Op::DivideEquals => AssignmentOp::Divide,
                                Op::WrappingDivideEquals => AssignmentOp::WrappingDivide,
                                Op::SaturatingDivideEquals => AssignmentOp::SaturatingDivide,
                                Op::RemainderEquals => AssignmentOp::Remainder,
                                Op::PlusEquals => AssignmentOp::Plus,
                                Op::WrappingPlusEquals => AssignmentOp::WrappingPlus,
                                Op::SaturatingPlusEquals => AssignmentOp::SaturatingPlus,
                                Op::MinusEquals => AssignmentOp::Minus,
                                Op::WrappingMinusEquals => AssignmentOp::WrappingMinus,
                                Op::SaturatingMinusEquals => AssignmentOp::SaturatingMinus,
                                Op::LeftShiftEquals => AssignmentOp::LeftShift,
                                Op::WrappingLeftShiftEquals => AssignmentOp::WrappingLeftShift,
                                Op::SaturatingLeftShiftEquals => AssignmentOp::SaturatingLeftShift,
                                Op::RightShiftEquals => AssignmentOp::RightShift,
                                Op::BitAndEquals => AssignmentOp::BitAnd,
                                Op::BitXorEquals => AssignmentOp::BitXor,
                                Op::BitOrEquals => AssignmentOp::BitOr,
                                Op::AndEquals => AssignmentOp::And,
                                Op::OrEquals => AssignmentOp::Or,
                                Op::LeftRotateEquals => AssignmentOp::LeftRotate,
                                Op::RightRotateEquals => AssignmentOp::RightRotate,
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
                                | Op::And
                                | Op::BitAnd
                                | Op::BitXor
                                | Op::Or
                                | Op::BitOr
                                | Op::Compare
                                | Op::EqualsEquals
                                | Op::NotEquals
                                | Op::Greater
                                | Op::GreaterOrEquals
                                | Op::Less
                                | Op::LessOrEquals => unreachable!("not an 'equals' operator"),
                            };

                            let reassignment = self.reassignment(
                                expression,
                                token,
                                assignment_op,
                                after_expression_token,
                            )?;

                            self.semicolon()?;
                            Ok(reassignment)
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
                            let previous_token = self.peek_previous_token();
                            Err(Error {
                                kind: ErrorKind::MissingSemicolon,
                                col: previous_token.col,
                                pointers_count: previous_token.kind.display_len(),
                            })
                        }
                    },

                    TokenKind::Bracket(_)
                    | TokenKind::Colon
                    | TokenKind::Comma
                    | TokenKind::False
                    | TokenKind::True
                    | TokenKind::Integer(_)
                    | TokenKind::Ascii(_)
                    | TokenKind::Str(_)
                    | TokenKind::RawStr(_)
                    | TokenKind::Identifier(_)
                    | TokenKind::Print
                    | TokenKind::PrintLn
                    | TokenKind::Eprint
                    | TokenKind::EprintLn
                    | TokenKind::Definition(_)
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
                            pointers_count: previous_token.kind.display_len(),
                        })
                    }
                    TokenKind::Comment(_) => {
                        unreachable!("should be skipped by the token iterator")
                    }
                    TokenKind::Unexpected(_) => unreachable!("only valid tokens should be present"),
                }
            }
            TokenKind::Definition(mutability) => {
                let definition = self.variable_definition(mutability)?;
                self.semicolon()?;
                Ok(definition)
            }
            TokenKind::Print => {
                let arg = self.print_arg()?;
                self.semicolon()?;
                Ok(Node::Print(arg))
            }
            TokenKind::PrintLn => {
                if let Some(&Token { kind: TokenKind::SemiColon, .. }) = self.peek_next_token() {
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
                if let Some(&Token { kind: TokenKind::SemiColon, .. }) = self.peek_next_token() {
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
                    pointers_count: token.kind.display_len(),
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
                        pointers_count: token.kind.display_len(),
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
                        pointers_count: token.kind.display_len(),
                    });
                }

                self.semicolon()?;
                Ok(Node::Continue)
            }
            TokenKind::SemiColon => {
                _ = self.next_token();
                Ok(Node::Semicolon)
            }
            TokenKind::Bracket(BracketKind::OpenCurly) => {
                let Position { line, col } = self.src.position(token.col);
                unreachable!(
                    "blocks not allowed in single statements: {file}:{line}:{col}",
                    file = self.src.path.display(),
                );
            }
            TokenKind::Bracket(
                BracketKind::CloseCurly | BracketKind::CloseSquare | BracketKind::CloseRound,
            ) => {
                let Position { line, col } = self.src.position(token.col);
                unreachable!(
                    "should have been cought during tokenization: {file}:{line}:{col}",
                    file = self.src.path.display(),
                );
            }
            TokenKind::Colon => {
                _ = self.next_token();
                Err(Error {
                    kind: ErrorKind::StrayColon,
                    col: token.col,
                    pointers_count: token.kind.display_len(),
                })
            }
            TokenKind::Comma => {
                _ = self.next_token();
                Err(Error {
                    kind: ErrorKind::StrayComma,
                    col: token.col,
                    pointers_count: token.kind.display_len(),
                })
            }
            TokenKind::Op(op) => {
                _ = self.next_token();
                Err(Error {
                    kind: ErrorKind::StrayOperator(op),
                    col: token.col,
                    pointers_count: token.kind.display_len(),
                })
            }
            TokenKind::Comment(_) => unreachable!("should be skipped by the token iterator"),
            TokenKind::Unexpected(_) => unreachable!("only valid tokens should be present"),
        };
    }

    fn do_statement(&mut self) -> Result<Node, Error<ErrorKind>> {
        let token = self.next_token_bounded(Expected::StatementAfterDo)?;
        return match token.kind {
            TokenKind::Bracket(BracketKind::OpenCurly) => {
                _ = self.next_token();
                Err(Error {
                    kind: ErrorKind::BlockInDoStatement,
                    col: token.col,
                    pointers_count: token.kind.display_len(),
                })
            }
            TokenKind::Definition(_) => {
                _ = self.next_token();
                Err(Error {
                    kind: ErrorKind::VariableInDoStatement,
                    col: token.col,
                    pointers_count: token.kind.display_len(),
                })
            }
            TokenKind::Bracket(_)
            | TokenKind::Comment(_)
            | TokenKind::Unexpected(_)
            | TokenKind::Colon
            | TokenKind::SemiColon
            | TokenKind::Comma
            | TokenKind::Op(_)
            | TokenKind::False
            | TokenKind::True
            | TokenKind::Integer(_)
            | TokenKind::Ascii(_)
            | TokenKind::Str(_)
            | TokenKind::RawStr(_)
            | TokenKind::Identifier(_)
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

    fn any(&mut self, token: &'tokens Token<'src>) -> Result<Node, Error<ErrorKind>> {
        return match token.kind {
            TokenKind::Bracket(BracketKind::OpenCurly) => {
                let new_scope_index = self.scopes.len() as offset;
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
            TokenKind::Bracket(BracketKind::CloseCurly) => {
                self.scope = self.scopes[self.scope as usize].parent;
                _ = self.next_token();
                Ok(Node::ScopeEnd)
            }
            TokenKind::Bracket(_)
            | TokenKind::Comment(_)
            | TokenKind::Unexpected(_)
            | TokenKind::Colon
            | TokenKind::SemiColon
            | TokenKind::Comma
            | TokenKind::Op(_)
            | TokenKind::False
            | TokenKind::True
            | TokenKind::Integer(_)
            | TokenKind::Ascii(_)
            | TokenKind::Str(_)
            | TokenKind::RawStr(_)
            | TokenKind::Identifier(_)
            | TokenKind::Definition(_)
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
impl<'src, 'tokens: 'src> Parser<'src, 'tokens> {
    fn current_token(&self, expected: Expected) -> Result<&'tokens Token<'src>, Error<ErrorKind>> {
        let Some(token) = self.tokens.get(self.token as usize) else {
            let previous = self.peek_previous_token();
            return Err(Error {
                kind: ErrorKind::PrematureEndOfFile(expected),
                col: previous.col,
                pointers_count: previous.kind.display_len(),
            });
        };

        return Ok(token);
    }

    fn next_token(&mut self) -> Option<&'tokens Token<'src>> {
        loop {
            let tokens_len = self.tokens.len() as offset;
            if self.token >= tokens_len - 1 {
                self.token = tokens_len;
                return None;
            }

            self.token += 1;
            let next = &self.tokens[self.token as usize];
            let TokenKind::Comment(_) = next.kind else {
                return Some(next);
            };
        }
    }

    fn next_token_bounded(
        &mut self,
        expected: Expected,
    ) -> Result<&'tokens Token<'src>, Error<ErrorKind>> {
        loop {
            let tokens_len = self.tokens.len() as offset;
            if self.token >= tokens_len - 1 {
                let previous = &self.tokens[self.token as usize];
                self.token = tokens_len;
                return Err(Error {
                    kind: ErrorKind::PrematureEndOfFile(expected),
                    col: previous.col,
                    pointers_count: previous.kind.display_len(),
                });
            }

            self.token += 1;
            let next = &self.tokens[self.token as usize];
            let TokenKind::Comment(_) = next.kind else {
                return Ok(next);
            };
        }
    }

    const fn peek_next_token(&self) -> Option<&'tokens Token<'src>> {
        let mut current_token = self.token;
        loop {
            if current_token >= self.tokens.len() as offset - 1 {
                return None;
            }

            current_token += 1;
            let next = &self.tokens[current_token as usize];
            let TokenKind::Comment(_) = next.kind else {
                return Some(next);
            };
        }
    }

    // Note: this function is always called when underflowing the tokens array is never the case,
    // so there is no need for bounds checking
    const fn peek_previous_token(&self) -> &'tokens Token<'src> {
        let mut current_token = self.token;
        loop {
            current_token -= 1;
            let previous = &self.tokens[current_token as usize];
            let TokenKind::Comment(_) = previous.kind else {
                return previous;
            };
        }
    }
}

// expressions
impl<'src, 'tokens: 'src> Parser<'src, 'tokens> {
    fn new_expression(&mut self, expression: Expression) -> ExpressionIndex {
        let index = self.ast.expressions.len() as ExpressionIndex;
        self.ast.expressions.push(expression);
        return index;
    }

    fn assert_lhs_is_not_string_or_array(
        op_token: &'tokens Token<'src>,
        lhs: &Expression,
    ) -> Result<(), Error<ErrorKind>> {
        let lhs_type = lhs.typ();
        if let Type::Base(BaseType::Str) | Type::Array { .. } = lhs_type {
            return Err(Error {
                kind: ErrorKind::LeftOperandTypeMismatch(lhs_type),
                col: op_token.col,
                pointers_count: op_token.kind.display_len(),
            });
        }

        return Ok(());
    }

    fn assert_rhs_is_not_string_or_array(
        op_token: &'tokens Token<'src>,
        rhs: &Expression,
    ) -> Result<(), Error<ErrorKind>> {
        let rhs_type = rhs.typ();
        if let Type::Base(BaseType::Str) | Type::Array { .. } = rhs_type {
            return Err(Error {
                kind: ErrorKind::RightOperandTypeMismatch(rhs_type),
                col: op_token.col,
                pointers_count: op_token.kind.display_len(),
            });
        }

        return Ok(());
    }

    fn assert_lhs_is_bool(
        op_token: &'tokens Token<'src>,
        lhs: &Expression,
    ) -> Result<(), Error<ErrorKind>> {
        let lhs_type = lhs.typ();
        let Type::Base(BaseType::Bool) = lhs_type else {
            return Err(Error {
                kind: ErrorKind::LeftOperandTypeMismatch(lhs_type),
                col: op_token.col,
                pointers_count: op_token.kind.display_len(),
            });
        };

        return Ok(());
    }

    fn assert_rhs_is_bool(
        op_token: &'tokens Token<'src>,
        rhs: &Expression,
    ) -> Result<(), Error<ErrorKind>> {
        let rhs_type = rhs.typ();
        let Type::Base(BaseType::Bool) = rhs_type else {
            return Err(Error {
                kind: ErrorKind::RightOperandTypeMismatch(rhs_type),
                col: op_token.col,
                pointers_count: op_token.kind.display_len(),
            });
        };

        return Ok(());
    }

    fn operator(
        &mut self,
        ops: &[Op],
    ) -> Result<Option<(&'tokens Token<'src>, Op)>, Error<ErrorKind>> {
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

    #[inline(always)]
    fn new_string(&mut self, string: Str) -> StringLabel {
        let label = self.ast.string_kinds.len() as StringLabel;
        self.ast.string_kinds.push(StrKind::Str);
        self.ast.strings.push(string);
        return label;
    }

    #[inline(always)]
    fn new_raw_string(&mut self, string: RawStr<'src>) -> StringLabel {
        let label = self.ast.string_kinds.len() as StringLabel;
        self.ast.string_kinds.push(StrKind::RawStr);
        self.ast.raw_strings.push(string);
        return label;
    }

    fn primary_expression(&mut self) -> Result<Expression, Error<ErrorKind>> {
        fn parse_positive_int(literal: &str) -> Option<int> {
            let mut integer: int = 0;
            for ascii_digit in literal.as_bytes() {
                let digit = (*ascii_digit - b'0') as usize;

                integer = integer.checked_mul(10)?;
                integer = integer.checked_add_unsigned(digit)?;
            }
            return Some(integer);
        }

        fn parse_negative_int(literal: &str) -> Option<int> {
            let mut integer: int = 0;
            for ascii_digit in literal.as_bytes() {
                let digit = (*ascii_digit - b'0') as usize;

                integer = integer.checked_mul(10)?;
                integer = integer.checked_sub_unsigned(digit)?;
            }
            return Some(integer);
        }

        let current_token = self.current_token(Expected::Expression)?;
        let expression_result = match &current_token.kind {
            TokenKind::False => Ok(Expression::False),
            TokenKind::True => Ok(Expression::True),
            TokenKind::Integer(integer_literal) => match parse_positive_int(integer_literal) {
                Some(integer) => Ok(Expression::Int(integer)),
                None => Err(Error {
                    kind: ErrorKind::IntOverflow,
                    col: current_token.col,
                    pointers_count: current_token.kind.display_len(),
                }),
            },
            TokenKind::Ascii(ascii_ch) => Ok(Expression::Ascii(*ascii_ch)),
            TokenKind::Str(string) => {
                Ok(Expression::Str { label: self.new_string(string.clone()) })
            }
            TokenKind::RawStr(string) => {
                Ok(Expression::Str { label: self.new_raw_string(string.clone()) })
            }
            TokenKind::Identifier(name) => match self.resolve_type(name) {
                None => match self.resolve_variable(name) {
                    Some((_, variable_index)) => {
                        let var = &self.ast.variables[variable_index as usize];
                        Ok(Expression::Variable { typ: var.value.typ(), variable_index })
                    }
                    None => Err(Error {
                        kind: ErrorKind::VariableNotPreviouslyDefined,
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(),
                    }),
                },
                Some(_) => 'type_in_expression: {
                    let Some(possible_reassignment_operator) = self.peek_next_token() else {
                        break 'type_in_expression Err(Error {
                            kind: ErrorKind::TypeInExpression,
                            col: current_token.col,
                            pointers_count: current_token.kind.display_len(),
                        });
                    };

                    let TokenKind::Op(op) = possible_reassignment_operator.kind else {
                        break 'type_in_expression Err(Error {
                            kind: ErrorKind::TypeInExpression,
                            col: current_token.col,
                            pointers_count: current_token.kind.display_len(),
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
                                pointers_count: current_token.kind.display_len(),
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
                                pointers_count: current_token.kind.display_len(),
                            });
                        }
                    }
                }
            },
            TokenKind::Bracket(BracketKind::OpenRound) => 'parenthesis: {
                let expression_start_token = self.next_token_bounded(Expected::Expression)?;

                if let TokenKind::Bracket(BracketKind::CloseRound) = expression_start_token.kind {
                    break 'parenthesis Err(Error {
                        kind: ErrorKind::EmptyExpression,
                        col: expression_start_token.col,
                        pointers_count: expression_start_token.kind.display_len(),
                    });
                }

                let expression = self.expression()?;
                let close_bracket_token = self.current_token(Expected::ClosingRoundBracket)?;

                let TokenKind::Bracket(BracketKind::CloseRound) = close_bracket_token.kind else {
                    return Err(Error {
                        kind: ErrorKind::UnclosedBracket(BracketKind::OpenRound),
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(),
                    });
                };

                Ok(Expression::Parenthesis { typ: expression.typ(), expression_index: self.new_expression(expression) })
            }
            TokenKind::Bracket(BracketKind::OpenSquare) => 'array: {
                let mut bracket_or_comma_token =
                    self.next_token_bounded(Expected::ArrayElementOrClosingSquareBracket)?;

                if let TokenKind::Bracket(BracketKind::CloseSquare) = bracket_or_comma_token.kind {
                    break 'array Err(Error {
                        kind: ErrorKind::ArrayOfZeroElements,
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(),
                    });
                }

                let first_item = self.expression()?;

                bracket_or_comma_token =
                    self.current_token(Expected::CommaOrClosingSquareBracket)?;

                if let TokenKind::Comma = bracket_or_comma_token.kind {
                    bracket_or_comma_token =
                        self.next_token_bounded(Expected::ArrayElementOrClosingSquareBracket)?;
                }

                if let TokenKind::Bracket(BracketKind::CloseSquare) = bracket_or_comma_token.kind {
                    break 'array Err(Error {
                        kind: ErrorKind::ArrayOfOneElement,
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(),
                    });
                }

                let items_type = match first_item.typ() {
                    Type::Base(base_type) => base_type,
                    Type::Array { .. } => {
                        break 'array Err(Error {
                            kind: ErrorKind::NestedArrayNotSupportedYet,
                            col: current_token.col,
                            pointers_count: current_token.kind.display_len(),
                        })
                    }
                };

                let mut items = vec![first_item];

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
                            pointers_count: bracket_or_comma_token.kind.display_len(),
                        });
                    }

                    if let Type::Array { .. } = item_type {
                        break 'array Err(Error {
                            kind: ErrorKind::NestedArrayNotSupportedYet,
                            col: current_token.col,
                            pointers_count: current_token.kind.display_len(),
                        });
                    };

                    items.push(item);

                    bracket_or_comma_token =
                        self.current_token(Expected::CommaOrClosingSquareBracket)?;

                    if let TokenKind::Comma = bracket_or_comma_token.kind {
                        bracket_or_comma_token =
                            self.next_token_bounded(Expected::ArrayElementOrClosingSquareBracket)?;
                    }

                    if let TokenKind::Bracket(BracketKind::CloseSquare) =
                        bracket_or_comma_token.kind
                    {
                        break 'array Ok(Expression::Array {
                            base_type: items_type,
                            items,
                        });
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
                    Expression::Int(_) => Err(Error {
                        kind: ErrorKind::CannotTakeLenOf(Type::Base(BaseType::Int)),
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(),
                    }),
                    Expression::Ascii(_) => Err(Error {
                        kind: ErrorKind::CannotTakeLenOf(Type::Base(BaseType::Ascii)),
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(),
                    }),
                    Expression::True | Expression::False => Err(Error {
                        kind: ErrorKind::CannotTakeLenOf(Type::Base(BaseType::Bool)),
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(),
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
                        Type::Base(BaseType::Int | BaseType::Ascii | BaseType::Bool) => {
                            Err(Error {
                                kind: ErrorKind::CannotTakeLenOf(*typ),
                                col: current_token.col,
                                pointers_count: current_token.kind.display_len(),
                            })
                        }
                    },
                    Expression::ArrayIndex { base_type, .. } => match base_type {
                        BaseType::Str => Ok(Expression::Unary {
                            op: UnaryOp::Len,
                            op_col: current_token.col,
                            operand_index: self.new_expression(operand),
                        }),
                        BaseType::Int | BaseType::Ascii | BaseType::Bool => Err(Error {
                            kind: ErrorKind::CannotTakeLenOf(Type::Base(*base_type)),
                            col: current_token.col,
                            pointers_count: current_token.kind.display_len(),
                        }),
                    },
                    Expression::Parenthesis { typ, .. } => Err(Error {
                        kind: ErrorKind::CannotTakeLenOf(*typ),
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(),
                    }),
                    Expression::Unary { op, .. } => Err(Error {
                        kind: ErrorKind::CannotTakeLenOf(op.typ()),
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(),
                    }),
                    Expression::BooleanUnary { op, .. } => Err(Error {
                        kind: ErrorKind::CannotTakeLenOf(op.typ()),
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(),
                    }),
                    Expression::Binary { op, .. } => Err(Error {
                        kind: ErrorKind::CannotTakeLenOf(op.typ()),
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(),
                    }),
                    Expression::BooleanBinary { op, .. } => Err(Error {
                        kind: ErrorKind::CannotTakeLenOf(op.typ()),
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(),
                    }),
                    Expression::Comparison { op, .. } => Err(Error {
                        kind: ErrorKind::CannotTakeLenOf(op.typ()),
                        col: current_token.col,
                        pointers_count: current_token.kind.display_len(),
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
                while let Some(&Token { kind: TokenKind::Op(Op::Plus), .. }) = self.next_token() {
                    should_be_made_positive = !should_be_made_positive;
                }

                let operand = self.primary_expression()?;

                // returning to avoid the call to tokens.next at the end of the function
                return match operand.typ() {
                    Type::Base(BaseType::Int) => {
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
                        pointers_count: current_token.kind.display_len(),
                    }),
                };
            }
            TokenKind::Op(Op::WrappingPlus) => {
                let mut should_be_made_positive = true;

                // NOTE(stefano): this optimization should be moved to later stages
                // removing extra "+\" symbols
                while let Some(&Token { kind: TokenKind::Op(Op::WrappingPlus), .. }) =
                    self.next_token()
                {
                    should_be_made_positive = !should_be_made_positive;
                }

                let operand = self.primary_expression()?;

                // returning to avoid the call to tokens.next at the end of the function
                return match operand.typ() {
                    Type::Base(BaseType::Int) => {
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
                        pointers_count: current_token.kind.display_len(),
                    }),
                };
            }
            TokenKind::Op(Op::SaturatingPlus) => {
                let mut should_be_made_positive = true;

                // NOTE(stefano): this optimization should be moved to later stages
                // removing extra "+|" symbols
                while let Some(&Token { kind: TokenKind::Op(Op::SaturatingPlus), .. }) =
                    self.next_token()
                {
                    should_be_made_positive = !should_be_made_positive;
                }

                let operand = self.primary_expression()?;

                // returning to avoid the call to tokens.next at the end of the function
                return match operand.typ() {
                    Type::Base(BaseType::Int) => {
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
                        pointers_count: current_token.kind.display_len(),
                    }),
                };
            }
            TokenKind::Op(Op::Minus) => {
                let mut should_be_negated = true;

                // NOTE(stefano): this optimization should be moved to later stages
                // removing extra "-" symbols
                while let Some(&Token { kind: TokenKind::Op(Op::Minus), .. }) = self.next_token() {
                    should_be_negated = !should_be_negated;
                }

                let start_of_expression = &self.tokens[self.token as usize];
                let TokenKind::Integer(literal) = start_of_expression.kind else {
                    let operand = self.primary_expression()?;

                    // returning to avoid the call to tokens.next at the end of the function
                    return match operand.typ() {
                        Type::Base(BaseType::Int | BaseType::Ascii) => {
                            if should_be_negated {
                                Ok(Expression::Unary {
                                    op: UnaryOp::Minus,
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
                            pointers_count: current_token.kind.display_len(),
                        }),
                    };
                };

                if should_be_negated {
                    match parse_negative_int(literal) {
                        Some(0) => Err(Error {
                            kind: ErrorKind::MinusZeroNumberLiteral,
                            col: current_token.col,
                            pointers_count: current_token.kind.display_len(),
                        }),
                        Some(integer) => Ok(Expression::Int(integer)),
                        None => Err(Error {
                            kind: ErrorKind::IntUnderflow,
                            col: current_token.col,
                            pointers_count: current_token.kind.display_len(),
                        }),
                    }
                } else {
                    match parse_positive_int(literal) {
                        Some(integer) => Ok(Expression::Int(integer)),
                        None => Err(Error {
                            kind: ErrorKind::IntOverflow,
                            col: current_token.col,
                            pointers_count: current_token.kind.display_len(),
                        }),
                    }
                }
            }
            TokenKind::Op(Op::WrappingMinus) => {
                let mut should_be_negated = true;

                // NOTE(stefano): this optimization should be moved to later stages
                // removing extra "-\" symbols
                while let Some(&Token { kind: TokenKind::Op(Op::WrappingMinus), .. }) =
                    self.next_token()
                {
                    should_be_negated = !should_be_negated;
                }

                let start_of_expression = &self.tokens[self.token as usize];
                let TokenKind::Integer(literal) = start_of_expression.kind else {
                    let operand = self.primary_expression()?;

                    // returning to avoid the call to tokens.next at the end of the function
                    return match operand.typ() {
                        Type::Base(BaseType::Int | BaseType::Ascii) => {
                            if should_be_negated {
                                Ok(Expression::Unary {
                                    op: UnaryOp::WrappingMinus,
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
                            pointers_count: current_token.kind.display_len(),
                        }),
                    };
                };

                if should_be_negated {
                    match parse_negative_int(literal) {
                        Some(0) => Err(Error {
                            kind: ErrorKind::MinusZeroNumberLiteral,
                            col: current_token.col,
                            pointers_count: current_token.kind.display_len(),
                        }),
                        Some(integer) => Ok(Expression::Int(integer)),
                        None => Err(Error {
                            kind: ErrorKind::IntUnderflow,
                            col: current_token.col,
                            pointers_count: current_token.kind.display_len(),
                        }),
                    }
                } else {
                    match parse_positive_int(literal) {
                        Some(integer) => Ok(Expression::Int(integer)),
                        None => Err(Error {
                            kind: ErrorKind::IntOverflow,
                            col: current_token.col,
                            pointers_count: current_token.kind.display_len(),
                        }),
                    }
                }
            }
            TokenKind::Op(Op::SaturatingMinus) => {
                let mut should_be_negated = true;

                // NOTE(stefano): this optimization should be moved to later stages
                // removing extra "-|" symbols
                while let Some(&Token { kind: TokenKind::Op(Op::SaturatingMinus), .. }) =
                    self.next_token()
                {
                    should_be_negated = !should_be_negated;
                }

                let start_of_expression = &self.tokens[self.token as usize];
                let TokenKind::Integer(literal) = start_of_expression.kind else {
                    let operand = self.primary_expression()?;

                    // returning to avoid the call to tokens.next at the end of the function
                    return match operand.typ() {
                        Type::Base(BaseType::Int | BaseType::Ascii) => {
                            if should_be_negated {
                                Ok(Expression::Unary {
                                    op: UnaryOp::SaturatingMinus,
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
                            pointers_count: current_token.kind.display_len(),
                        }),
                    };
                };

                if should_be_negated {
                    match parse_negative_int(literal) {
                        Some(0) => Err(Error {
                            kind: ErrorKind::MinusZeroNumberLiteral,
                            col: current_token.col,
                            pointers_count: current_token.kind.display_len(),
                        }),
                        Some(integer) => Ok(Expression::Int(integer)),
                        None => Err(Error {
                            kind: ErrorKind::IntUnderflow,
                            col: current_token.col,
                            pointers_count: current_token.kind.display_len(),
                        }),
                    }
                } else {
                    match parse_positive_int(literal) {
                        Some(integer) => Ok(Expression::Int(integer)),
                        None => Err(Error {
                            kind: ErrorKind::IntOverflow,
                            col: current_token.col,
                            pointers_count: current_token.kind.display_len(),
                        }),
                    }
                }
            }
            TokenKind::Op(Op::Not) => {
                let mut should_be_inverted = true;

                // NOTE(stefano): this optimization should be moved to later stages
                // removing extra "!" symbols
                while let Some(&Token { kind: TokenKind::Op(Op::Not), .. }) = self.next_token() {
                    should_be_inverted = !should_be_inverted;
                }

                let operand = self.primary_expression()?;

                // returning to avoid the call to tokens.next at the end of the function
                return match operand.typ() {
                    Type::Base(BaseType::Int | BaseType::Ascii) => {
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
                        pointers_count: current_token.kind.display_len(),
                    }),
                };
            }
            TokenKind::Definition(_)
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
                pointers_count: current_token.kind.display_len(),
            }),
            TokenKind::Bracket(_)
            | TokenKind::Op(_)
            | TokenKind::Comment(_)
            | TokenKind::Unexpected(_)
            | TokenKind::Colon
            | TokenKind::SemiColon
            | TokenKind::Comma => Err(Error {
                kind: ErrorKind::ExpectedOperand,
                col: current_token.col,
                pointers_count: current_token.kind.display_len(),
            }),
        };

        let mut expression = expression_result?;
        while let Some(
            open_bracket_token @ Token { kind: TokenKind::Bracket(BracketKind::OpenSquare), .. },
        ) = self.next_token()
        {
            let _start_of_index = self.next_token();
            let index = self.expression()?;
            let Type::Base(BaseType::Int) = index.typ() else {
                return Err(Error {
                    kind: ErrorKind::ExpectedNumberLiteralInArrayIndex,
                    col: open_bracket_token.col,
                    pointers_count: open_bracket_token.kind.display_len(),
                });
            };

            let after_index_token = self.current_token(Expected::ClosingSquareBracket)?;

            let TokenKind::Bracket(BracketKind::CloseSquare) = after_index_token.kind else {
                let before_index_token = self.peek_previous_token();
                return Err(Error {
                    kind: ErrorKind::MissingClosingSquareBracketInIndex,
                    col: before_index_token.col,
                    pointers_count: before_index_token.kind.display_len(),
                });
            };

            /* IDEA(stefano):
            disallow indexing into literal arrays, it's as if you were to access
            the actual element.
            could suggest the user to extract the literal array to a temporary variable first
            */
            if let Expression::Parenthesis { .. } = expression {
                return Err(Error {
                    kind: ErrorKind::CannotIndexIntoExpression,
                    col: open_bracket_token.col,
                    pointers_count: open_bracket_token.kind.display_len(),
                });
            }

            let expression_type = expression.typ();
            expression = match expression_type {
                Type::Base(base_type) => match base_type {
                    BaseType::Str => Expression::ArrayIndex {
                        base_type: BaseType::Ascii,
                        indexable_index: self.new_expression(expression),
                        bracket_col: open_bracket_token.col,
                        index_expression_index: self.new_expression(index),
                    },
                    BaseType::Int | BaseType::Ascii | BaseType::Bool => {
                        return Err(Error {
                            kind: ErrorKind::CannotIndexNonArrayType(expression_type),
                            col: open_bracket_token.col,
                            pointers_count: open_bracket_token.kind.display_len(),
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
            Self::assert_lhs_is_not_string_or_array(op_token, &lhs)?;

            let rhs = self.primary_expression()?;
            Self::assert_rhs_is_not_string_or_array(op_token, &rhs)?;

            #[allow(clippy::wildcard_enum_match_arm)]
            let binary_op = match op {
                Op::Pow => BinaryOp::Pow,
                Op::WrappingPow => BinaryOp::WrappingPow,
                Op::SaturatingPow => BinaryOp::SaturatingPow,
                _ => unreachable!(),
            };

            lhs = Expression::Binary {
                lhs_index: self.new_expression(lhs),
                op: binary_op,
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
            Self::assert_lhs_is_not_string_or_array(op_token, &lhs)?;

            let rhs = self.exponentiative_expression()?;
            Self::assert_rhs_is_not_string_or_array(op_token, &rhs)?;

            #[allow(clippy::wildcard_enum_match_arm)]
            let binary_op = match op {
                Op::Times => BinaryOp::Times,
                Op::WrappingTimes => BinaryOp::WrappingTimes,
                Op::SaturatingTimes => BinaryOp::SaturatingTimes,
                Op::Divide => BinaryOp::Divide,
                Op::WrappingDivide => BinaryOp::WrappingDivide,
                Op::SaturatingDivide => BinaryOp::SaturatingDivide,
                Op::Remainder => BinaryOp::Remainder,
                _ => unreachable!(),
            };

            lhs = Expression::Binary {
                lhs_index: self.new_expression(lhs),
                op: binary_op,
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
            Self::assert_lhs_is_not_string_or_array(op_token, &lhs)?;

            let rhs = self.multiplicative_expression()?;
            Self::assert_rhs_is_not_string_or_array(op_token, &rhs)?;

            #[allow(clippy::wildcard_enum_match_arm)]
            let binary_op = match op {
                Op::Plus => BinaryOp::Plus,
                Op::WrappingPlus => BinaryOp::WrappingPlus,
                Op::SaturatingPlus => BinaryOp::SaturatingPlus,
                Op::Minus => BinaryOp::Minus,
                Op::WrappingMinus => BinaryOp::WrappingMinus,
                Op::SaturatingMinus => BinaryOp::SaturatingMinus,
                _ => unreachable!(),
            };

            lhs = Expression::Binary {
                lhs_index: self.new_expression(lhs),
                op: binary_op,
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
            Self::assert_lhs_is_not_string_or_array(op_token, &lhs)?;

            let rhs = self.additive_expression()?;
            Self::assert_rhs_is_not_string_or_array(op_token, &rhs)?;

            #[allow(clippy::wildcard_enum_match_arm)]
            let binary_op = match op {
                Op::LeftShift => BinaryOp::LeftShift,
                Op::WrappingLeftShift => BinaryOp::WrappingLeftShift,
                Op::SaturatingLeftShift => BinaryOp::SaturatingLeftShift,
                Op::RightShift => BinaryOp::RightShift,
                Op::LeftRotate => BinaryOp::LeftRotate,
                Op::RightRotate => BinaryOp::RightRotate,
                _ => unreachable!(),
            };

            lhs = Expression::Binary {
                lhs_index: self.new_expression(lhs),
                op: binary_op,
                op_col: op_token.col,
                rhs_index: self.new_expression(rhs),
            };
        }

        return Ok(lhs);
    }

    fn bitand_expression(&mut self) -> Result<Expression, Error<ErrorKind>> {
        let mut lhs = self.shift_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::BitAnd])? {
            Self::assert_lhs_is_not_string_or_array(op_token, &lhs)?;

            let rhs = self.shift_expression()?;
            Self::assert_rhs_is_not_string_or_array(op_token, &rhs)?;

            #[allow(clippy::wildcard_enum_match_arm)]
            let binary_op = match op {
                Op::BitAnd => BinaryOp::BitAnd,
                _ => unreachable!(),
            };

            lhs = Expression::Binary {
                lhs_index: self.new_expression(lhs),
                op: binary_op,
                op_col: op_token.col,
                rhs_index: self.new_expression(rhs),
            };
        }

        return Ok(lhs);
    }

    fn bitxor_expression(&mut self) -> Result<Expression, Error<ErrorKind>> {
        let mut lhs = self.bitand_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::BitXor])? {
            Self::assert_lhs_is_not_string_or_array(op_token, &lhs)?;

            let rhs = self.bitand_expression()?;
            Self::assert_rhs_is_not_string_or_array(op_token, &rhs)?;

            #[allow(clippy::wildcard_enum_match_arm)]
            let binary_op = match op {
                Op::BitXor => BinaryOp::BitXor,
                _ => unreachable!(),
            };

            lhs = Expression::Binary {
                lhs_index: self.new_expression(lhs),
                op: binary_op,
                op_col: op_token.col,
                rhs_index: self.new_expression(rhs),
            };
        }

        return Ok(lhs);
    }

    fn bitor_expression(&mut self) -> Result<Expression, Error<ErrorKind>> {
        let mut lhs = self.bitxor_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::BitOr])? {
            Self::assert_lhs_is_not_string_or_array(op_token, &lhs)?;

            let rhs = self.bitxor_expression()?;
            Self::assert_rhs_is_not_string_or_array(op_token, &rhs)?;

            #[allow(clippy::wildcard_enum_match_arm)]
            let binary_op = match op {
                Op::BitOr => BinaryOp::BitOr,
                _ => unreachable!(),
            };

            lhs = Expression::Binary {
                lhs_index: self.new_expression(lhs),
                op: binary_op,
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
                ) => lhs_base_typ == rhs_base_typ && lhs_len == rhs_len,
                _ => false,
            };

            if !can_compare {
                return Err(Error {
                    kind: ErrorKind::CannotCompareOperands { lhs_type, rhs_type },
                    col: op_token.col,
                    pointers_count: op_token.kind.display_len(),
                });
            }

            if is_chained {
                return Err(Error {
                    kind: ErrorKind::CannotChainComparisons,
                    col: op_token.col,
                    pointers_count: op_token.kind.display_len(),
                });
            }
            is_chained = true;

            #[allow(clippy::wildcard_enum_match_arm)]
            let comparison_op = match op {
                Op::Compare => ComparisonOp::Compare,
                Op::EqualsEquals => ComparisonOp::EqualsEquals,
                Op::NotEquals => ComparisonOp::NotEquals,
                Op::Greater => ComparisonOp::Greater,
                Op::GreaterOrEquals => ComparisonOp::GreaterOrEquals,
                Op::Less => ComparisonOp::Less,
                Op::LessOrEquals => ComparisonOp::LessOrEquals,
                _ => unreachable!(),
            };

            lhs = Expression::Comparison {
                lhs_index: self.new_expression(lhs),
                op: comparison_op,
                rhs_index: self.new_expression(rhs),
            };
        }

        return Ok(lhs);
    }

    fn and_expression(&mut self) -> Result<Expression, Error<ErrorKind>> {
        let mut lhs = self.comparison_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::And])? {
            Self::assert_lhs_is_bool(op_token, &lhs)?;

            let rhs = self.comparison_expression()?;
            Self::assert_rhs_is_bool(op_token, &rhs)?;

            #[allow(clippy::wildcard_enum_match_arm)]
            let binary_op = match op {
                Op::And => BooleanBinaryOp::And,
                _ => unreachable!(),
            };

            lhs =
                Expression::BooleanBinary { lhs_index: self.new_expression(lhs), op: binary_op, rhs_index: self.new_expression(rhs) };
        }

        return Ok(lhs);
    }

    fn or_expression(&mut self) -> Result<Expression, Error<ErrorKind>> {
        let mut lhs = self.and_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::Or])? {
            Self::assert_lhs_is_bool(op_token, &lhs)?;

            let rhs = self.and_expression()?;
            Self::assert_rhs_is_bool(op_token, &rhs)?;

            #[allow(clippy::wildcard_enum_match_arm)]
            let binary_op = match op {
                Op::Or => BooleanBinaryOp::Or,
                _ => unreachable!(),
            };

            lhs =
                Expression::BooleanBinary { lhs_index: self.new_expression(lhs), op: binary_op, rhs_index: self.new_expression(rhs) };
        }

        return Ok(lhs);
    }

    // TODO(stefano): disallow implicit conversions
    // TODO(stefano): introduce casting operators
    fn expression(&mut self) -> Result<Expression, Error<ErrorKind>> {
        return self.or_expression();
    }
}

// variables and types
impl<'src, 'tokens: 'src> Parser<'src, 'tokens> {
    fn resolve_variable(&self, name: &'src str) -> Option<(Mutability, VariableIndex)> {
        let mut scope_index = self.scope;
        loop {
            let scope = &self.scopes[scope_index as usize];
            for var_index in &scope.let_variables {
                let var = &self.ast.variables[*var_index as usize];
                if var.name == name {
                    return Some((Mutability::Let, *var_index));
                }
            }

            for var_index in &scope.var_variables {
                let var = &self.ast.variables[*var_index as usize];
                if var.name == name {
                    return Some((Mutability::Var, *var_index));
                }
            }

            scope_index = match scope_index {
                0 => return None,
                _ => scope.parent,
            };
        }
    }

    fn resolve_type(&self, name: &'src str) -> Option<BaseType> {
        let mut scope_index = self.scope;
        loop {
            let scope = &self.scopes[scope_index as usize];
            for typ in &scope.base_types {
                if typ.to_string() == name {
                    return Some(*typ);
                }
            }

            scope_index = match scope_index {
                0 => return None,
                _ => scope.parent,
            };
        }
    }

    fn type_annotation(
        &mut self,
    ) -> Result<Option<(&'tokens Token<'src>, Type)>, Error<ErrorKind>> {
        let colon_token = self.next_token_bounded(Expected::TypeAnnotationOrVariableDefinition)?;

        let TokenKind::Colon = colon_token.kind else {
            self.token -= 1;
            return Ok(None);
        };

        let type_token = self.next_token_bounded(Expected::TypeAnnotation)?;
        let TokenKind::Identifier(type_name) = type_token.kind else {
            return Err(Error {
                kind: ErrorKind::ExpectedType,
                col: colon_token.col,
                pointers_count: colon_token.kind.display_len(),
            });
        };

        let Some(base_type) = self.resolve_type(type_name) else {
            return match self.resolve_variable(type_name) {
                Some((_, var_index)) => {
                    let var = &self.ast.variables[var_index as usize];
                    Ok(Some((type_token, var.value.typ())))
                }
                None => Err(Error {
                    kind: ErrorKind::VariableNotPreviouslyDefined,
                    col: type_token.col,
                    pointers_count: type_token.kind.display_len(),
                }),
            };
        };

        let Some(open_square_bracket_token) = self.peek_next_token() else {
            return Ok(Some((type_token, Type::Base(base_type))));
        };

        let TokenKind::Bracket(BracketKind::OpenSquare) = open_square_bracket_token.kind else {
            return Ok(Some((type_token, Type::Base(base_type))));
        };

        let _open_square_bracket = self.next_token();

        let len_token = self.next_token_bounded(Expected::ArrayLength)?;
        let len_expression = self.expression()?;
        let Expression::Int(literal_len) = len_expression else {
            return Err(Error {
                kind: ErrorKind::ExpectedNumberLiteralInArrayType,
                col: open_square_bracket_token.col,
                pointers_count: open_square_bracket_token.kind.display_len(),
            });
        };

        let len = match literal_len {
            len if len < 0 => {
                return Err(Error {
                    kind: ErrorKind::ArrayOfNegativeLength,
                    col: len_token.col,
                    pointers_count: len_token.kind.display_len(),
                });
            }
            0 => {
                return Err(Error {
                    kind: ErrorKind::ArrayOfZeroElements,
                    col: len_token.col,
                    pointers_count: len_token.kind.display_len(),
                });
            }
            1 => {
                return Err(Error {
                    kind: ErrorKind::ArrayOfOneElement,
                    col: len_token.col,
                    pointers_count: len_token.kind.display_len(),
                });
            }
            _ => literal_len as uint,
        };

        let close_square_bracket_token = self.current_token(Expected::ClosingSquareBracket)?;
        let TokenKind::Bracket(BracketKind::CloseSquare) = close_square_bracket_token.kind else {
            return Err(Error {
                kind: ErrorKind::MissingClosingSquareBracketInArrayType,
                col: open_square_bracket_token.col,
                pointers_count: open_square_bracket_token.kind.display_len(),
            });
        };

        return Ok(Some((close_square_bracket_token, Type::Array { base_type, len })));
    }

    fn expression_from_base_type(&mut self, typ: BaseType) -> Expression {
        return match typ {
            BaseType::Int => Expression::Int(0),
            BaseType::Ascii => Expression::Ascii(b'0'),
            BaseType::Bool => Expression::False,
            BaseType::Str => {
                let string = Str(Vec::new().into_boxed_slice());
                Expression::Str { label: self.new_string(string) }
            }
        };
    }

    fn variable_definition(
        &mut self,
        mutability: Mutability,
    ) -> Result<Node, Error<ErrorKind>> {
        let name_token = self.next_token_bounded(Expected::Identifier)?;
        let name = match name_token.kind {
            TokenKind::Identifier(name) => match self.resolve_type(name) {
                None => name,
                Some(_) => {
                    return Err(Error {
                        kind: ErrorKind::TypeInVariableName,
                        col: name_token.col,
                        pointers_count: name_token.kind.display_len(),
                    })
                }
            },
            TokenKind::Comment(_)
            | TokenKind::Unexpected(_)
            | TokenKind::Bracket(_)
            | TokenKind::Colon
            | TokenKind::SemiColon
            | TokenKind::Comma
            | TokenKind::Op(_)
            | TokenKind::False
            | TokenKind::True
            | TokenKind::Integer(_)
            | TokenKind::Ascii(_)
            | TokenKind::Str(_)
            | TokenKind::RawStr(_) => {
                return Err(Error {
                    kind: ErrorKind::ExpectedVariableName,
                    col: name_token.col,
                    pointers_count: name_token.kind.display_len(),
                })
            }
            TokenKind::Definition(_)
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
                    pointers_count: name_token.kind.display_len(),
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
            | TokenKind::Unexpected(_)
            | TokenKind::Bracket(_)
            | TokenKind::Colon
            | TokenKind::Comma
            | TokenKind::False
            | TokenKind::True
            | TokenKind::Integer(_)
            | TokenKind::Ascii(_)
            | TokenKind::Str(_)
            | TokenKind::RawStr(_)
            | TokenKind::Identifier(_)
            | TokenKind::Definition(_)
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
                        pointers_count: name_token.kind.display_len(),
                    })
                }
                Some((annotation_token, _)) => {
                    return Err(Error {
                        kind: ErrorKind::ExpectedEqualsOrSemicolonAfterTypeAnnotation,
                        col: annotation_token.col,
                        pointers_count: annotation_token.kind.display_len(),
                    })
                }
            },
        };

        let None = self.resolve_variable(name) else {
            return Err(Error {
                kind: ErrorKind::VariableAlreadyDefined,
                col: name_token.col,
                pointers_count: name_token.kind.display_len(),
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
                            pointers_count: token.kind.display_len(),
                        });
                    }
                }

                let scope_variables = match mutability {
                    Mutability::Let => &mut self.scopes[self.scope as usize].let_variables,
                    Mutability::Var => &mut self.scopes[self.scope as usize].var_variables,
                };

                let var_index = self.ast.variables.len() as offset;
                scope_variables.push(var_index);
                self.ast.variables.push(Variable { name, value });

                Ok(Node::Definition { var_index })
            }
            None => match annotation {
                Some((_, typ)) => {
                    let value = match typ {
                        Type::Base(base_type) => self.expression_from_base_type(base_type),
                        Type::Array { base_type, len } => {
                            let items = vec![self.expression_from_base_type(base_type); len];
                            Expression::Array { base_type, items }
                        }
                    };

                    let scope_variables = match mutability {
                        Mutability::Let => {
                            &mut self.scopes[self.scope as usize].let_variables
                        }
                        Mutability::Var => {
                            &mut self.scopes[self.scope as usize].var_variables
                        }
                    };

                    let var_index = self.ast.variables.len() as offset;
                    scope_variables.push(var_index);
                    self.ast.variables.push(Variable { name, value });

                    Ok(Node::Definition { var_index })
                }
                None => Err(Error {
                    kind: ErrorKind::CannotInferTypeOfVariable,
                    col: name_token.col,
                    pointers_count: name_token.kind.display_len(),
                }),
            },
        };
    }

    // NOTE(stefano): mutations of string characters are disallowed until a sort of "borrow checker" is developed
    fn reassignment(
        &mut self,
        target: Expression,
        target_token: &'tokens Token<'src>,
        op: AssignmentOp,
        op_token: &'tokens Token<'src>,
    ) -> Result<Node, Error<ErrorKind>> {
        let (error_token, target_type) = match &target {
            Expression::ArrayIndex { base_type, indexable_index, .. } => {
                let indexable = &self.ast.expressions[*indexable_index as usize];
                let mut unwrapped_indexable = indexable;
                while let Expression::ArrayIndex { indexable_index: inner_indexable_index, .. } = unwrapped_indexable {
                    let inner_indexable = &self.ast.expressions[*inner_indexable_index as usize];
                    unwrapped_indexable = inner_indexable;
                }

                let Expression::Variable { typ, .. } = unwrapped_indexable else {
                    return Err(Error {
                        kind: ErrorKind::CannotAssignToExpression,
                        col: op_token.col,
                        pointers_count: op_token.kind.display_len(),
                    });
                };

                let error_token = if let TokenKind::Identifier(name) = target_token.kind {
                    let Some((mutability, _)) = self.resolve_variable(name) else {
                        unreachable!("should have been checked during lhs parsing");
                    };

                    let Mutability::Var = mutability else {
                        return Err(Error {
                            kind: ErrorKind::CannotMutateVariable,
                            col: target_token.col,
                            pointers_count: target_token.kind.display_len(),
                        });
                    };

                    if let BaseType::Str = typ.base_typ() {
                        if let BaseType::Ascii = base_type {
                            return Err(Error {
                                kind: ErrorKind::CannotMutateStringCharacters,
                                col: target_token.col,
                                pointers_count: target_token.kind.display_len(),
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
                let Some((mutability, _)) = self.resolve_variable(var.name) else {
                    unreachable!("should have been checked during lhs parsing");
                };

                let Mutability::Var = mutability else {
                    return Err(Error {
                        kind: ErrorKind::CannotMutateVariable,
                        col: target_token.col,
                        pointers_count: target_token.kind.display_len(),
                    });
                };

                (target_token, *typ)
            }

            Expression::False
            | Expression::True
            | Expression::Int(_)
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
                    pointers_count: op_token.kind.display_len(),
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
                pointers_count: error_token.kind.display_len(),
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
                    Type::Base(BaseType::Int),
                    Type::Base(BaseType::Int | BaseType::Ascii | BaseType::Bool),
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
                    pointers_count: op_token.kind.display_len(),
                }),
                _ => Err(Error {
                    kind: ErrorKind::VariableReassignmentTypeMismatch {
                        expected: target_type,
                        actual: new_value_type,
                    },
                    col: error_token.col,
                    pointers_count: error_token.kind.display_len(),
                }),
            },
        };
    }
}

// print statements
impl<'src, 'tokens: 'src> Parser<'src, 'tokens> {
    fn print_arg(&mut self) -> Result<Expression, Error<ErrorKind>> {
        let _start_of_expression_token = self.next_token_bounded(Expected::Expression)?;
        let argument = self.expression()?;
        if let Expression::Array { .. } = argument {
            let temporary_value_index = self.ast.temporaries.len() as offset;
            let argument_type = argument.typ();
            self.ast.temporaries.push(argument);
            return Ok(Expression::Temporary { typ: argument_type, temporary_value_index });
        };

        return Ok(argument);
    }
}

// if statements
impl<'src, 'tokens: 'src> Parser<'src, 'tokens> {
    fn iff(&mut self) -> Result<Node, Error<ErrorKind>> {
        let mut ifs = Vec::new();
        let mut els = None;

        'iff: while let Some(if_token) = self.tokens.get(self.token as usize) {
            _ = self.next_token_bounded(Expected::BooleanExpression)?;

            let condition = self.expression()?;
            let Type::Base(BaseType::Bool) = condition.typ() else {
                return Err(Error {
                    kind: ErrorKind::IfMustBeFollowedByBooleanExpression,
                    col: if_token.col,
                    pointers_count: if_token.kind.display_len(),
                });
            };

            let after_condition_token = self.current_token(Expected::DoOrBlock)?;
            let if_statement = match after_condition_token.kind {
                TokenKind::Bracket(BracketKind::OpenCurly) => {
                    let scope = self.any(after_condition_token)?;
                    IfStatement { condition, statement: scope }
                }
                TokenKind::Do => {
                    let statement = self.do_statement()?;
                    IfStatement { condition, statement }
                }
                TokenKind::Bracket(_)
                | TokenKind::Comment(_)
                | TokenKind::Unexpected(_)
                | TokenKind::Colon
                | TokenKind::SemiColon
                | TokenKind::Comma
                | TokenKind::Op(_)
                | TokenKind::False
                | TokenKind::True
                | TokenKind::Integer(_)
                | TokenKind::Ascii(_)
                | TokenKind::Str(_)
                | TokenKind::RawStr(_)
                | TokenKind::Identifier(_)
                | TokenKind::Definition(_)
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
                        pointers_count: before_curly_bracket_token.kind.display_len(),
                    });
                }
            };

            ifs.push(if_statement);

            while let Some(else_token) = self.tokens.get(self.token as usize) {
                let after_else_token = match else_token.kind {
                    TokenKind::Else => self.next_token_bounded(Expected::DoOrBlockOrIfStatement)?,
                    TokenKind::Comment(_)
                    | TokenKind::Unexpected(_)
                    | TokenKind::Bracket(_)
                    | TokenKind::Colon
                    | TokenKind::SemiColon
                    | TokenKind::Comma
                    | TokenKind::Op(_)
                    | TokenKind::False
                    | TokenKind::True
                    | TokenKind::Integer(_)
                    | TokenKind::Ascii(_)
                    | TokenKind::Str(_)
                    | TokenKind::RawStr(_)
                    | TokenKind::Identifier(_)
                    | TokenKind::Definition(_)
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
                    TokenKind::Bracket(BracketKind::OpenCurly) => {
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
                    TokenKind::Bracket(_)
                    | TokenKind::Comment(_)
                    | TokenKind::Unexpected(_)
                    | TokenKind::Colon
                    | TokenKind::SemiColon
                    | TokenKind::Comma
                    | TokenKind::Op(_)
                    | TokenKind::False
                    | TokenKind::True
                    | TokenKind::Integer(_)
                    | TokenKind::Ascii(_)
                    | TokenKind::Str(_)
                    | TokenKind::RawStr(_)
                    | TokenKind::Identifier(_)
                    | TokenKind::Definition(_)
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
                        pointers_count: else_token.kind.display_len(),
                    }),
                };

                else_if?;
            }
        }

        let if_index = self.ast.ifs.len() as IfIndex;
        self.ast.ifs.push(If { ifs, els });
        return Ok(Node::If(if_index));
    }
}

// loop statements
impl<'src, 'tokens: 'src> Parser<'src, 'tokens> {
    fn loop_statement(&mut self) -> Result<Node, Error<ErrorKind>> {
        let do_token = &self.tokens[self.token as usize];
        let loop_token = match do_token.kind {
            TokenKind::Do => {
                let loop_token = self.next_token_bounded(Expected::LoopStatement)?;
                let TokenKind::Loop = loop_token.kind else {
                    return Err(Error {
                        kind: ErrorKind::DoMustBeFollowedByLoop,
                        col: do_token.col,
                        pointers_count: do_token.kind.display_len(),
                    });
                };

                loop_token
            }
            TokenKind::Comment(_)
            | TokenKind::Unexpected(_)
            | TokenKind::Bracket(_)
            | TokenKind::Colon
            | TokenKind::SemiColon
            | TokenKind::Comma
            | TokenKind::Op(_)
            | TokenKind::False
            | TokenKind::True
            | TokenKind::Integer(_)
            | TokenKind::Ascii(_)
            | TokenKind::Str(_)
            | TokenKind::RawStr(_)
            | TokenKind::Identifier(_)
            | TokenKind::Definition(_)
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
                pointers_count: loop_token.kind.display_len(),
            });
        };

        let after_condition_token = self.current_token(Expected::DoOrBlock)?;
        let statement_result = match after_condition_token.kind {
            TokenKind::Bracket(BracketKind::OpenCurly) => {
                let scope = self.any(after_condition_token)?;
                Ok(scope)
            }
            TokenKind::Do => {
                let statement = self.do_statement()?;
                Ok(statement)
            }
            TokenKind::Bracket(_)
            | TokenKind::Comment(_)
            | TokenKind::Unexpected(_)
            | TokenKind::Colon
            | TokenKind::SemiColon
            | TokenKind::Comma
            | TokenKind::Op(_)
            | TokenKind::False
            | TokenKind::True
            | TokenKind::Integer(_)
            | TokenKind::Ascii(_)
            | TokenKind::Str(_)
            | TokenKind::RawStr(_)
            | TokenKind::Identifier(_)
            | TokenKind::Definition(_)
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
                    pointers_count: before_curly_bracket_token.kind.display_len(),
                })
            }
        };

        let statement = statement_result?;
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

    MinusZeroNumberLiteral,
    IntOverflow,
    IntUnderflow,

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
    UnclosedBracket(BracketKind),
    ArrayOfNegativeLength,
    ArrayOfZeroElements,
    ArrayOfOneElement,
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
    VariableInDoStatement,
}

impl IntoErrorInfo for ErrorKind {
    fn info(&self) -> ErrorInfo {
        let (error_message, error_cause_message) = match self {
            Self::PrematureEndOfFile(expected) => (
                "premature end of file".into(),
                format!("expected {expected} after here").into(),
            ),

            Self::MinusZeroNumberLiteral => (
                "invalid integer literal".into(),
                "-0 is not a valid two's complement integer".into(),
            ),
            Self::IntOverflow => (
                "integer literal overflow".into(),
                format!("overflows a {bits} bit signed integer (over {max})", bits = int::BITS, max = int::MAX).into(),
            ),
            Self::IntUnderflow => (
                "integer literal underflow".into(),
                format!("underlows a {bits} bit signed integer (under {min})", bits = int::BITS, min = int::MIN).into(),
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
            Self::UnclosedBracket(bracket) => (
                "invalid expression".into(),
                format!("'{bracket}' bracket was not closed").into(),
            ),
            Self::ArrayOfNegativeLength => (
                "invalid array length".into(),
                "array length must be greater than 1".into(),
            ),
            Self::ArrayOfZeroElements => (
                "invalid array".into(),
                "arrays of zero items are not allowed, as they are practically phantom values".into(),
            ),
            Self::ArrayOfOneElement => (
                "invalid array".into(),
                "arrays of one element are not allowed, as they are practically the same as the value itself".into(),
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
