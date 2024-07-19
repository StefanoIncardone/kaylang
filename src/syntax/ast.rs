use super::{
    op::{AssignmentOp, BinaryOp, BooleanBinaryOp, BooleanUnaryOp, ComparisonOp, UnaryOp},
    tokenizer::{ascii, int, uint, BracketKind, DisplayLen, Literal, Mutability, Op, Token, TokenKind},
    Errors, RawError,
};
use crate::src_file::{Position, SrcFile};
use std::fmt::{Debug, Display};

pub(crate) trait BaseTypeOf {
    fn base_typ(&self) -> BaseType;
}

pub(crate) trait TypeOf {
    fn typ(&self) -> Type;
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Int => write!(f, "int"),
            Self::Ascii => write!(f, "ascii"),
            Self::Bool => write!(f, "bool"),
            Self::Str => write!(f, "str"),
        };
    }
}

impl TypeOf for BaseType {
    fn typ(&self) -> Type {
        return Type::Base(*self);
    }
}

impl SizeOf for BaseType {
    fn size(&self) -> usize {
        return match self {
            Self::Int => std::mem::size_of::<int>(),
            Self::Ascii => std::mem::size_of::<ascii>(),
            Self::Bool => std::mem::size_of::<bool>(),
            Self::Str => std::mem::size_of::<*const ascii>() + std::mem::size_of::<uint>(),
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Base(BaseType),
    // TODO(breaking)(stefano): enforce a max length
    Array {
        typ: BaseType,
        /// always greater than 1, i.e: arrays always contain at least 2 elements
        len: uint,
    },
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Base(typ) => write!(f, "{typ}"),
            Self::Array { typ, len } => write!(f, "{typ}[{len}]"),
        };
    }
}

impl BaseTypeOf for Type {
    fn base_typ(&self) -> BaseType {
        return match self {
            Self::Base(typ) => *typ,
            Self::Array { typ, .. } => *typ,
        };
    }
}

impl SizeOf for Type {
    fn size(&self) -> usize {
        return match self {
            Self::Base(typ) => typ.size(),
            Self::Array { typ, len } => typ.size() * len,
        };
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Expression<'src> {
    Literal(Literal),
    Unary {
        op_position: Position,
        op: UnaryOp,
        operand: Box<Expression<'src>>,
    },
    BooleanUnary {
        op: BooleanUnaryOp,
        operand: Box<Expression<'src>>,
    },
    Binary {
        lhs: Box<Expression<'src>>,
        op_position: Position,
        op: BinaryOp,
        rhs: Box<Expression<'src>>,
    },
    BooleanBinary {
        lhs: Box<Expression<'src>>,
        op: BooleanBinaryOp,
        rhs: Box<Expression<'src>>,
    },
    Comparison {
        lhs: Box<Expression<'src>>,
        op: ComparisonOp,
        rhs: Box<Expression<'src>>,
    },
    Identifier {
        typ: Type,
        name: &'src str,
    },
    Array {
        typ: BaseType,
        /// arrays always contain at least 2 elements
        items: Vec<Expression<'src>>,
    },
    ArrayIndex {
        typ: BaseType,
        var_name: &'src str,
        bracket_position: Position,
        index: Box<Expression<'src>>,
    },
}

impl Display for Expression<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Literal(literal) => write!(f, "{literal}"),
            Self::Unary { op: len @ UnaryOp::Len, operand, .. } => write!(f, "{len} {operand}"),
            Self::Unary { op, operand, .. } => write!(f, "{op}{operand}"),
            Self::BooleanUnary { op, operand } => write!(f, "{op}{operand}"),
            Self::Binary { lhs, op, rhs, .. } => write!(f, "({lhs} {op} {rhs})"),
            Self::BooleanBinary { lhs, op, rhs, .. } => write!(f, "({lhs} {op} {rhs})"),
            Self::Comparison { lhs, op, rhs } => write!(f, "({lhs} {op} {rhs})"),
            Self::Identifier { name, .. } => write!(f, "{name}"),
            Self::Array { items, .. } => {
                write!(f, "[")?;
                let mut items_iter = items.iter();
                let Some(last_item) = items_iter.next_back() else {
                    unreachable!("arrays should always contain at least 2 elements");
                };

                for item in items_iter {
                    write!(f, "{item}, ")?;
                }

                write!(f, "{last_item}]")
            }
            Self::ArrayIndex { var_name, index, .. } => write!(f, "{var_name}[{index}]"),
        };
    }
}

impl TypeOf for Expression<'_> {
    fn typ(&self) -> Type {
        return match self {
            Self::Literal(literal) => match literal {
                Literal::Int(_) => Type::Base(BaseType::Int),
                Literal::Ascii(_) => Type::Base(BaseType::Ascii),
                Literal::Bool(_) => Type::Base(BaseType::Bool),
                Literal::Str(_) => Type::Base(BaseType::Str),
            },
            Self::Unary { op, .. } => op.typ(),
            Self::BooleanUnary { op, .. } => op.typ(),
            Self::Binary { op, .. } => op.typ(),
            Self::BooleanBinary { op, .. } => op.typ(),
            Self::Comparison { op, .. } => op.typ(),
            Self::Identifier { typ, .. } => *typ,
            Self::Array { typ, items } => Type::Array { typ: *typ, len: items.len() },
            Self::ArrayIndex { typ, .. } => Type::Base(*typ),
        };
    }
}

impl From<BaseType> for Expression<'_> {
    fn from(typ: BaseType) -> Self {
        return match typ {
            BaseType::Int => Self::Literal(Literal::Int(0)),
            BaseType::Ascii => Self::Literal(Literal::Ascii(0)),
            BaseType::Bool => Self::Literal(Literal::Bool(false)),
            BaseType::Str => Self::Literal(Literal::Str(Vec::new())),
        };
    }
}

impl From<Type> for Expression<'_> {
    fn from(typ: Type) -> Self {
        return match typ {
            Type::Base(base_typ) => base_typ.into(),
            Type::Array { typ: items_type, len } => {
                Expression::Array { typ: items_type, items: vec![items_type.into(); len] }
            }
        };
    }
}

#[derive(Debug, Clone)]
pub(crate) struct IfStatement<'src> {
    pub(crate) condition: Expression<'src>,
    pub(crate) statement: Node<'src>,
}

impl Display for IfStatement<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(f, "if {}", self.condition);
    }
}

#[derive(Debug, Clone)]
pub(crate) struct If<'src> {
    pub(crate) ifs: Vec<IfStatement<'src>>,
    pub(crate) els: Option<Box<Node<'src>>>,
}

#[derive(Debug, Clone)]
pub(crate) struct Loop<'src> {
    pub(crate) condition: Expression<'src>,
    pub(crate) statement: Box<Node<'src>>,
}

impl Display for Loop<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(f, "loop {}", self.condition);
    }
}

#[derive(Debug, Clone)]
pub(crate) struct DoLoop<'src> {
    pub(crate) condition: Expression<'src>,
    pub(crate) statement: Box<Node<'src>>,
}

impl Display for DoLoop<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(f, "do loop {}", self.condition);
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Variable<'src> {
    pub(crate) name: &'src str,
    pub(crate) value: Expression<'src>,
}

#[derive(Debug, Clone)]
pub(crate) enum Node<'src> {
    Semicolon,

    Expression(Expression<'src>),

    Print(Expression<'src>),
    Println(Option<Expression<'src>>),
    Eprint(Expression<'src>),
    Eprintln(Option<Expression<'src>>),

    If(If<'src>),

    Loop(Loop<'src>),
    DoLoop(DoLoop<'src>),
    Break,
    Continue,

    Definition {
        mutability: Mutability,
        scope_index: usize,
        var_index: usize,
    },
    Assignment {
        scope_index: usize,
        var_index: usize,
        op: AssignmentOp,
        op_position: Position,
        new_value: Expression<'src>,
    },

    Scope {
        index: usize,
    },
}

impl Display for Node<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Semicolon => write!(f, ";"),

            Self::Expression(expression) => write!(f, "{expression}"),

            Self::Print(arg) => write!(f, "print {arg}"),
            Self::Println(Some(arg)) => write!(f, "println {arg}"),
            Self::Println(None) => write!(f, "println"),
            Self::Eprint(arg) => write!(f, "eprint {arg}"),
            Self::Eprintln(Some(arg)) => write!(f, "eprintln {arg}"),
            Self::Eprintln(None) => write!(f, "eprintln"),

            Self::If(iff) => write!(f, "{}", iff.ifs[0]),

            Self::Loop(looop) => write!(f, "{looop}"),
            Self::DoLoop(looop) => write!(f, "{looop}"),
            Self::Break => write!(f, "break"),
            Self::Continue => write!(f, "continue"),

            Self::Definition { .. } | Self::Assignment { .. } | Self::Scope { .. } => {
                unreachable!("should never be displayed");
            }
        };
    }
}

#[derive(Debug, Clone)]
pub struct Scope<'src> {
    pub(crate) parent: usize,
    pub(crate) base_types: Vec<BaseType>,
    pub(crate) let_variables: Vec<Variable<'src>>,
    pub(crate) var_variables: Vec<Variable<'src>>,
    pub(crate) nodes: Vec<Node<'src>>,
}

// IDEA(stefano): create Parser class that builds the AST, and then validate the AST afterwards
#[derive(Debug)]
pub struct Ast<'src, 'tokens: 'src> {
    src: &'src SrcFile,
    errors: Vec<RawError<ErrorKind, ErrorCause>>,

    token: usize,
    tokens: &'tokens [Token<'src>],

    scope_index: usize,
    scopes: Vec<Scope<'src>>,

    loop_depth: usize,
}

impl<'src, 'tokens: 'src> Ast<'src, 'tokens> {
    pub fn build(
        src: &'src SrcFile,
        tokens: &'tokens [Token<'src>],
    ) -> Result<Vec<Scope<'src>>, Errors<'src, ErrorKind, ErrorCause>> {
        if tokens.is_empty() {
            return Ok(Vec::new());
        }

        // skipping to the first non-comment token
        let mut token = 0;
        loop {
            if token >= tokens.len() {
                break;
            }

            let current = &tokens[token];
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
            scope_index: 0,
            scopes: vec![Scope {
                parent: 0,
                base_types: vec![BaseType::Int, BaseType::Ascii, BaseType::Bool, BaseType::Str],
                let_variables: Vec::new(),
                var_variables: Vec::new(),
                nodes: Vec::new(),
            }],
            loop_depth: 0,
        };

        this.parse_scope();

        return if this.errors.is_empty() {
            Ok(this.scopes)
        } else {
            Err(Errors { src, raw_errors: this.errors })
        };
    }

    fn semicolon(&mut self) -> Result<(), RawError<ErrorKind, ErrorCause>> {
        let semicolon_token = self.current_token_bounded(Expected::Semicolon)?;
        let TokenKind::SemiColon = &semicolon_token.kind else {
            let previous_token = self.peek_previous_token();
            return Err(RawError {
                kind: ErrorKind::Invalid(Statement::Statement),
                cause: ErrorCause::MissingSemicolon,
                col: previous_token.col,
                len: previous_token.kind.display_len()
            });
        };

        _ = self.next_token();
        return Ok(());
    }
}

// parsing of statements
impl<'src, 'tokens: 'src> Ast<'src, 'tokens> {
    fn parse_scope(&mut self) {
        loop {
            match self.parse_single_any() {
                Ok(Some(node)) => {
                    match node {
                        // skip to the next token after a semicolon
                        Node::Semicolon => continue,

                        // check to see if a terminating semicolon is present
                        Node::Definition { .. }
                        | Node::Assignment { .. }
                        | Node::Expression(_)
                        | Node::Break
                        | Node::Continue
                        | Node::Print(_)
                        | Node::Println(_)
                        | Node::Eprint(_)
                        | Node::Eprintln(_) => {
                            /*
                            NOTE(stefano): only parsing until the first error until a fault tolerant
                            parser is developed, this is because the first truly relevant error is
                            the first one, which in turn causes a ripple effect that propagates to
                            the rest of the parsing, causing subsequent errors to be wrong
                            */
                            if let Err(err) = self.semicolon() {
                                self.errors.push(err);

                                // consuming all remaining tokens until the end of the file
                                self.token = self.tokens.len();
                                break;
                            }
                        }

                        // no need to check for a terminating semicolon
                        Node::If(_) | Node::Loop(_) | Node::DoLoop(_) | Node::Scope { .. } => {}
                    }

                    self.scopes[self.scope_index].nodes.push(node);
                }
                Ok(None) => break,
                /*
                NOTE(stefano): only parsing until the first error until a fault tolerant parser is
                developed, this is because the first truly relevant error is the first one, which in
                turn causes a ripple effect that propagates to the rest of the parsing, causing
                subsequent errors to be wrong
                */
                Err(err) => {
                    self.errors.push(err);

                    // consuming all remaining tokens until the end of the file
                    self.token = self.tokens.len();
                    break;
                }
            }
        }
    }

    fn parse_single_statement(
        &mut self,
    ) -> Result<Option<Node<'src>>, RawError<ErrorKind, ErrorCause>> {
        let Some(current_token) = self.tokens.get(self.token) else { return Ok(None) };

        return match current_token.kind {
            TokenKind::Literal(_)
            | TokenKind::True
            | TokenKind::False
            | TokenKind::Bracket(BracketKind::OpenRound)
            | TokenKind::Op(
                Op::Len
                | Op::Not
                | Op::Plus
                | Op::WrappingPlus
                | Op::SaturatingPlus
                | Op::Minus
                | Op::WrappingMinus
                | Op::SaturatingMinus,
            ) => Ok(Some(Node::Expression(self.expression()?))),
            TokenKind::Identifier(name) => match self.peek_next_token() {
                Some(op) => match op.kind {
                    TokenKind::Op(
                        op_kind @ (Op::Equals
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
                        | Op::BitAndEquals
                        | Op::BitXorEquals
                        | Op::BitOrEquals
                        | Op::AndEquals
                        | Op::OrEquals
                        | Op::LeftRotateEquals
                        | Op::RightRotateEquals),
                    ) => Ok(Some(self.variable_reassignment(op_kind, name)?)),
                    TokenKind::Op(_)
                    | TokenKind::Comment(_)
                    | TokenKind::Unexpected(_)
                    | TokenKind::Bracket(_)
                    | TokenKind::Colon
                    | TokenKind::SemiColon
                    | TokenKind::Comma
                    | TokenKind::Literal(_)
                    | TokenKind::True
                    | TokenKind::False
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
                    | TokenKind::Continue => Ok(Some(Node::Expression(self.expression()?))),
                },
                None => Ok(Some(Node::Expression(self.expression()?))),
            },
            TokenKind::Definition(mutability) => Ok(Some(self.variable_definition(mutability)?)),
            TokenKind::Print => {
                let arg = self.print_arg()?;
                Ok(Some(Node::Print(arg)))
            }
            TokenKind::PrintLn => {
                if let Some(&Token { kind: TokenKind::SemiColon, .. }) = self.peek_next_token() {
                    _ = self.next_token();
                    return Ok(Some(Node::Println(None)));
                }

                let arg = self.print_arg()?;
                Ok(Some(Node::Println(Some(arg))))
            }
            TokenKind::Eprint => {
                let arg = self.print_arg()?;
                Ok(Some(Node::Eprint(arg)))
            }
            TokenKind::EprintLn => {
                if let Some(&Token { kind: TokenKind::SemiColon, .. }) = self.peek_next_token() {
                    _ = self.next_token();
                    return Ok(Some(Node::Eprintln(None)));
                }

                let arg = self.print_arg()?;
                Ok(Some(Node::Eprintln(Some(arg))))
            }
            TokenKind::If => Ok(Some(self.iff()?)),
            TokenKind::Else => {
                _ = self.next_token();
                Err(RawError {
                    kind: ErrorKind::Invalid(Statement::If),
                    cause: ErrorCause::StrayElseBlock,
                    col: current_token.col,
                    len: current_token.kind.display_len()
                })
            }
            TokenKind::Do | TokenKind::Loop => {
                self.loop_depth += 1;
                let looop_statement = self.loop_statement();
                self.loop_depth -= 1;
                match looop_statement {
                    Ok(looop) => Ok(Some(looop)),
                    Err(err) => Err(err),
                }
            }
            TokenKind::Break => {
                _ = self.next_token();
                match self.loop_depth {
                    0 => Err(RawError {
                        kind: ErrorKind::Invalid(Statement::Break),
                        cause: ErrorCause::CanOnlyBeUsedInLoops,
                        col: current_token.col,
                        len: current_token.kind.display_len()
                    }),
                    _ => Ok(Some(Node::Break)),
                }
            }
            TokenKind::Continue => {
                _ = self.next_token();
                match self.loop_depth {
                    0 => Err(RawError {
                        kind: ErrorKind::Invalid(Statement::Continue),
                        cause: ErrorCause::CanOnlyBeUsedInLoops,
                        col: current_token.col,
                        len: current_token.kind.display_len()
                    }),
                    _ => Ok(Some(Node::Continue)),
                }
            }
            TokenKind::SemiColon => {
                _ = self.next_token();
                Ok(Some(Node::Semicolon))
            }
            TokenKind::Bracket(BracketKind::OpenCurly) => {
                let (position, _) = Position::new(self.src, current_token.col);
                unreachable!(
                    "blocks not allowed in single statements: {path}:{line}:{col}",
                    path = self.src.path.display(),
                    line = position.line,
                    col = position.col,
                );
            }
            TokenKind::Bracket(BracketKind::OpenSquare) => {
                _ = self.expression()?;
                Err(RawError {
                    kind: ErrorKind::Invalid(Statement::Statement),
                    cause: ErrorCause::TemporaryArrayNotSupportedYet,
                    col: current_token.col,
                    len: current_token.kind.display_len()
                })
            }
            TokenKind::Bracket(
                BracketKind::CloseCurly | BracketKind::CloseSquare | BracketKind::CloseRound,
            ) => {
                let (position, _) = Position::new(self.src, current_token.col);
                unreachable!(
                    "should have been cought during tokenization: {path}:{line}:{col}",
                    path = self.src.path.display(),
                    line = position.line,
                    col = position.col,
                );
            }
            TokenKind::Colon => {
                _ = self.next_token();
                Err(RawError {
                    kind: ErrorKind::Invalid(Statement::TypeAnnotation),
                    cause: ErrorCause::StrayColon,
                    col: current_token.col,
                    len: current_token.kind.display_len()
                })
            }
            TokenKind::Comma => {
                _ = self.next_token();
                Err(RawError {
                    kind: ErrorKind::Invalid(Statement::ItemSeparator),
                    cause: ErrorCause::StrayComma,
                    col: current_token.col,
                    len: current_token.kind.display_len()
                })
            }
            TokenKind::Op(Op::Equals) => {
                _ = self.next_token();
                Err(RawError {
                    kind: ErrorKind::Invalid(Statement::Assignment),
                    cause: ErrorCause::StrayEquals,
                    col: current_token.col,
                    len: current_token.kind.display_len()
                })
            }
            TokenKind::Op(op) => {
                _ = self.next_token();
                Err(RawError {
                    kind: ErrorKind::Invalid(Statement::Expression),
                    cause: ErrorCause::StrayBinaryOperator(op),
                    col: current_token.col,
                    len: current_token.kind.display_len()
                })
            }
            TokenKind::Comment(_) => unreachable!("should be skipped by the token iterator"),
            TokenKind::Unexpected(_) => unreachable!("only valid tokens should be present"),
        };
    }

    fn parse_do_statement(
        &mut self,
    ) -> Result<Option<Node<'src>>, RawError<ErrorKind, ErrorCause>> {
        let current_token = self.next_token_bounded(Expected::StatementAfterDo)?;
        return match current_token.kind {
            TokenKind::Bracket(BracketKind::OpenCurly) => {
                _ = self.next_token();
                Err(RawError {
                    kind: ErrorKind::Invalid(Statement::Block),
                    cause: ErrorCause::NotAllowedIn {
                        not_allowed: Statement::Block,
                        in_: Statement::Do,
                    },
                    col: current_token.col,
                    len: current_token.kind.display_len()
                })
            }
            TokenKind::Definition(_) => {
                _ = self.next_token();
                Err(RawError {
                    kind: ErrorKind::Invalid(Statement::VariableDefinition),
                    cause: ErrorCause::NotAllowedIn {
                        not_allowed: Statement::VariableDefinition,
                        in_: Statement::Do,
                    },
                    col: current_token.col,
                    len: current_token.kind.display_len()
                })
            }
            TokenKind::Bracket(_)
            | TokenKind::Comment(_)
            | TokenKind::Unexpected(_)
            | TokenKind::Colon
            | TokenKind::SemiColon
            | TokenKind::Comma
            | TokenKind::Op(_)
            | TokenKind::Literal(_)
            | TokenKind::True
            | TokenKind::False
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
            | TokenKind::Continue => self.parse_single_statement(),
        };
    }

    fn parse_single_any(&mut self) -> Result<Option<Node<'src>>, RawError<ErrorKind, ErrorCause>> {
        let Some(current_token) = self.tokens.get(self.token) else { return Ok(None) };

        return match current_token.kind {
            TokenKind::Bracket(BracketKind::OpenCurly) => {
                let new_scope_index = self.scopes.len();
                self.scopes.push(Scope {
                    parent: self.scope_index,
                    base_types: Vec::new(),
                    let_variables: Vec::new(),
                    var_variables: Vec::new(),
                    nodes: Vec::new(),
                });
                self.scope_index = new_scope_index;

                _ = self.next_token();
                self.parse_scope();
                Ok(Some(Node::Scope { index: new_scope_index }))
            }
            TokenKind::Bracket(BracketKind::CloseCurly) => {
                self.scope_index = self.scopes[self.scope_index].parent;
                _ = self.next_token();
                Ok(None)
            }
            TokenKind::Bracket(_)
            | TokenKind::Comment(_)
            | TokenKind::Unexpected(_)
            | TokenKind::Colon
            | TokenKind::SemiColon
            | TokenKind::Comma
            | TokenKind::Op(_)
            | TokenKind::Literal(_)
            | TokenKind::True
            | TokenKind::False
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
            | TokenKind::Continue => self.parse_single_statement(),
        };
    }
}

// iteration over tokens
impl<'src, 'tokens: 'src> Ast<'src, 'tokens> {
    fn current_token_bounded(
        &self,
        expected: Expected,
    ) -> Result<&'tokens Token<'src>, RawError<ErrorKind, ErrorCause>> {
        let Some(token) = self.tokens.get(self.token) else {
            let previous = self.peek_previous_token();
            return Err(RawError {
                kind: ErrorKind::Expected(expected),
                cause: ErrorCause::NoMoreTokens,
                col: previous.col,
                len: previous.kind.display_len()
            });
        };

        return Ok(token);
    }

    fn next_token(&mut self) -> Option<&'tokens Token<'src>> {
        loop {
            if self.token >= self.tokens.len() - 1 {
                self.token = self.tokens.len();
                return None;
            }

            self.token += 1;
            let next = &self.tokens[self.token];
            let TokenKind::Comment(_) = next.kind else {
                return Some(next);
            };
        }
    }

    fn next_token_bounded(
        &mut self,
        expected: Expected,
    ) -> Result<&'tokens Token<'src>, RawError<ErrorKind, ErrorCause>> {
        loop {
            if self.token >= self.tokens.len() - 1 {
                let previous = &self.tokens[self.token];
                self.token = self.tokens.len();
                return Err(RawError {
                    kind: ErrorKind::Expected(expected),
                    cause: ErrorCause::NoMoreTokens,
                    col: previous.col,
                    len: previous.kind.display_len()
                });
            }

            self.token += 1;
            let next = &self.tokens[self.token];
            let TokenKind::Comment(_) = next.kind else {
                return Ok(next);
            };
        }
    }

    const fn peek_next_token(&self) -> Option<&'tokens Token<'src>> {
        let mut current_token = self.token;
        loop {
            if current_token >= self.tokens.len() - 1 {
                return None;
            }

            current_token += 1;
            let next = &self.tokens[current_token];
            let TokenKind::Comment(_) = next.kind else {
                return Some(next);
            };
        }
    }

    // Note: this function is always called when underflowing the tokens array is nevere the case, so there is no need for bounds checking
    const fn peek_previous_token(&self) -> &'tokens Token<'src> {
        let mut current_token = self.token;
        loop {
            current_token -= 1;
            let previous = &self.tokens[current_token];
            let TokenKind::Comment(_) = previous.kind else {
                return previous;
            };
        }
    }
}

// expressions
impl<'src, 'tokens: 'src> Ast<'src, 'tokens> {
    fn assert_lhs_is_not_string_or_array(
        op_token: &'tokens Token<'src>,
        lhs: &Expression<'src>,
    ) -> Result<(), RawError<ErrorKind, ErrorCause>> {
        if let Type::Base(BaseType::Str) = lhs.typ() {
            return Err(RawError {
                kind: ErrorKind::Invalid(Statement::Expression),
                cause: ErrorCause::StringLeftOperand,
                col: op_token.col,
                len: op_token.kind.display_len()
            });
        }
        if let Type::Array { .. } = lhs.typ() {
            return Err(RawError {
                kind: ErrorKind::Invalid(Statement::Expression),
                cause: ErrorCause::ArrayLeftOperand,
                col: op_token.col,
                len: op_token.kind.display_len()
            });
        }

        return Ok(());
    }

    fn assert_rhs_is_not_string_or_array(
        op_token: &'tokens Token<'src>,
        rhs: &Expression<'src>,
    ) -> Result<(), RawError<ErrorKind, ErrorCause>> {
        if let Type::Base(BaseType::Str) = rhs.typ() {
            return Err(RawError {
                kind: ErrorKind::Invalid(Statement::Expression),
                cause: ErrorCause::StringRightOperand,
                col: op_token.col,
                len: op_token.kind.display_len()
            });
        }
        if let Type::Array { .. } = rhs.typ() {
            return Err(RawError {
                kind: ErrorKind::Invalid(Statement::Expression),
                cause: ErrorCause::ArrayRightOperand,
                col: op_token.col,
                len: op_token.kind.display_len()
            });
        }

        return Ok(());
    }

    fn operator(
        &mut self,
        ops: &[Op],
    ) -> Result<Option<(&'tokens Token<'src>, Op)>, RawError<ErrorKind, ErrorCause>> {
        let current_token = self.current_token_bounded(Expected::OperatorOrSemicolon)?;
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

    fn index(
        &mut self,
        var_name: &'src str,
        var_type: Type,
    ) -> Result<Expression<'src>, RawError<ErrorKind, ErrorCause>> {
        let current_token = &self.tokens[self.token];

        let Some(open_bracket_token) = self.peek_next_token() else {
            return Ok(Expression::Identifier { typ: var_type, name: var_name });
        };

        let TokenKind::Bracket(BracketKind::OpenSquare) = open_bracket_token.kind else {
            return Ok(Expression::Identifier { typ: var_type, name: var_name });
        };

        let _open_square = self.next_token();
        let _start_of_index = self.next_token();

        let index = self.expression()?;
        let Type::Base(BaseType::Int) = index.typ() else {
            return Err(RawError {
                kind: ErrorKind::Invalid(Statement::ArrayIndex),
                cause: ErrorCause::MustBeFollowedByIntegerExpression,
                col: open_bracket_token.col,
                len: open_bracket_token.kind.display_len()
            });
        };

        let after_index_token = self.current_token_bounded(Expected::ClosingSquareBracket)?;

        let TokenKind::Bracket(BracketKind::CloseSquare) = after_index_token.kind else {
            let before_index_token = self.peek_previous_token();
            return Err(RawError {
                kind: ErrorKind::Invalid(Statement::ArrayIndex),
                cause: ErrorCause::MustBeFollowedByClosingSquareBracket,
                col: before_index_token.col,
                len: before_index_token.kind.display_len()
            });
        };

        return match var_type {
            Type::Base(primitive_type) => match primitive_type {
                BaseType::Int => Ok(Expression::ArrayIndex {
                    typ: BaseType::Int,
                    var_name,
                    bracket_position: Position::new(self.src, open_bracket_token.col).0,
                    index: Box::new(index),
                }),
                BaseType::Str => Ok(Expression::ArrayIndex {
                    typ: BaseType::Ascii,
                    var_name,
                    bracket_position: Position::new(self.src, open_bracket_token.col).0,
                    index: Box::new(index),
                }),
                BaseType::Ascii | BaseType::Bool => Err(RawError {
                    kind: ErrorKind::Invalid(Statement::Expression),
                    cause: ErrorCause::CannotIndexNonArrayType(var_type),
                    col: current_token.col,
                    len: current_token.kind.display_len()
                }),
            },
            Type::Array { typ, .. } => Ok(Expression::ArrayIndex {
                typ,
                var_name,
                bracket_position: Position::new(self.src, open_bracket_token.col).0,
                index: Box::new(index),
            }),
        };
    }

    fn primary_expression(&mut self) -> Result<Expression<'src>, RawError<ErrorKind, ErrorCause>> {
        let current_token = self.current_token_bounded(Expected::Expression)?;
        let factor = match &current_token.kind {
            TokenKind::Literal(literal) => Ok(Expression::Literal(literal.clone())),
            TokenKind::True => Ok(Expression::Literal(Literal::Bool(true))),
            TokenKind::False => Ok(Expression::Literal(Literal::Bool(false))),
            TokenKind::Identifier(name) => match self.resolve_type(name) {
                None => match self.resolve_variable(name) {
                    Some((_, _, _, var)) => self.index(var.name, var.value.typ()),
                    None => Err(RawError {
                        kind: ErrorKind::VariableNotPreviouslyDefined,
                        cause: ErrorCause::WasNotPreviouslyDefined,
                        col: current_token.col,
                        len: current_token.kind.display_len()
                    }),
                },
                Some(_) => Err(RawError {
                    kind: ErrorKind::Invalid(Statement::Expression),
                    cause: ErrorCause::CannotBeATypeName,
                    col: current_token.col,
                    len: current_token.kind.display_len()
                }),
            },
            TokenKind::Bracket(BracketKind::OpenRound) => 'parenthesis: {
                let expression_start_token = self.next_token_bounded(Expected::Expression)?;

                if let TokenKind::Bracket(BracketKind::CloseRound) = expression_start_token.kind {
                    break 'parenthesis Err(RawError {
                        kind: ErrorKind::Invalid(Statement::Expression),
                        cause: ErrorCause::EmptyExpression,
                        col: expression_start_token.col,
                        len: expression_start_token.kind.display_len()
                    });
                }

                let expression = self.expression()?;
                let close_bracket_token =
                    self.current_token_bounded(Expected::ClosingRoundBracket)?;

                let TokenKind::Bracket(BracketKind::CloseRound) = close_bracket_token.kind else {
                    return Err(RawError {
                        kind: ErrorKind::Invalid(Statement::Expression),
                        cause: ErrorCause::UnclosedBracket(BracketKind::OpenRound),
                        col: current_token.col,
                        len: current_token.kind.display_len()
                    });
                };

                Ok(expression)
            }
            TokenKind::Bracket(BracketKind::OpenSquare) => 'array: {
                let mut bracket_or_comma_token =
                    self.next_token_bounded(Expected::ArrayElementOrClosingSquareBracket)?;

                if let TokenKind::Bracket(BracketKind::CloseSquare) = bracket_or_comma_token.kind {
                    break 'array Err(RawError {
                        kind: ErrorKind::Invalid(Statement::Array),
                        cause: ErrorCause::ArrayOfZeroElements,
                        col: current_token.col,
                        len: current_token.kind.display_len()
                    });
                }

                let first_item = self.expression()?;

                bracket_or_comma_token =
                    self.current_token_bounded(Expected::CommaOrClosingSquareBracket)?;

                if let TokenKind::Comma = bracket_or_comma_token.kind {
                    bracket_or_comma_token =
                        self.next_token_bounded(Expected::ArrayElementOrClosingSquareBracket)?;
                }

                if let TokenKind::Bracket(BracketKind::CloseSquare) = bracket_or_comma_token.kind {
                    break 'array Err(RawError {
                        kind: ErrorKind::Invalid(Statement::Array),
                        cause: ErrorCause::ArrayOfOneElement,
                        col: current_token.col,
                        len: current_token.kind.display_len()
                    });
                }

                let items_type = match first_item.typ() {
                    Type::Base(base_type) => base_type,
                    Type::Array { .. } => {
                        break 'array Err(RawError {
                            kind: ErrorKind::Invalid(Statement::ArrayItem),
                            cause: ErrorCause::NestedArrayNotSupportedYet,
                            col: current_token.col,
                            len: current_token.kind.display_len()
                        })
                    }
                };

                let mut items = vec![first_item];

                // IDEA(stefano): gather all the elements and then check if they are of the correct type
                loop {
                    let item = self.expression()?;
                    let item_typ = item.typ();
                    /*
                    NOTE(stefano): this wrapping of items_type will be removed once nested arrays
                    are supported
                    */
                    if Type::Base(items_type) != item_typ {
                        break 'array Err(RawError {
                            kind: ErrorKind::Invalid(Statement::ArrayItem),
                            cause: ErrorCause::MismatchedArrayElementType {
                                expected: Type::Base(items_type),
                                actual: item_typ,
                            },
                            col: bracket_or_comma_token.col,
                            len: bracket_or_comma_token.kind.display_len()
                        });
                    }

                    if let Type::Array { .. } = item.typ() {
                        break 'array Err(RawError {
                            kind: ErrorKind::Invalid(Statement::ArrayItem),
                            cause: ErrorCause::NestedArrayNotSupportedYet,
                            col: current_token.col,
                            len: current_token.kind.display_len()
                        });
                    };

                    items.push(item);

                    bracket_or_comma_token =
                        self.current_token_bounded(Expected::CommaOrClosingSquareBracket)?;

                    if let TokenKind::Comma = bracket_or_comma_token.kind {
                        bracket_or_comma_token =
                            self.next_token_bounded(Expected::ArrayElementOrClosingSquareBracket)?;
                    }

                    if let TokenKind::Bracket(BracketKind::CloseSquare) =
                        bracket_or_comma_token.kind
                    {
                        break 'array Ok(Expression::Array { typ: items_type, items });
                    }
                }
            }
            TokenKind::Op(Op::Len) => {
                _ = self.next_token();
                let operand = self.primary_expression()?;
                return match &operand {
                    Expression::Literal(literal) => match literal {
                        Literal::Str(_) => Ok(Expression::Unary {
                            op_position: Position::new(self.src, current_token.col).0,
                            op: UnaryOp::Len,
                            operand: Box::new(operand),
                        }),
                        Literal::Int(_) => Err(RawError {
                            kind: ErrorKind::Invalid(Statement::Len),
                            cause: ErrorCause::CannotTakeLenOfNumericValue(Type::Base(
                                BaseType::Int,
                            )),
                            col: current_token.col,
                            len: current_token.kind.display_len()
                        }),
                        Literal::Ascii(_) => Err(RawError {
                            kind: ErrorKind::Invalid(Statement::Len),
                            cause: ErrorCause::CannotTakeLenOfNumericValue(Type::Base(
                                BaseType::Ascii,
                            )),
                            col: current_token.col,
                            len: current_token.kind.display_len()
                        }),
                        Literal::Bool(_) => Err(RawError {
                            kind: ErrorKind::Invalid(Statement::Len),
                            cause: ErrorCause::CannotTakeLenOfNumericValue(Type::Base(
                                BaseType::Bool,
                            )),
                            col: current_token.col,
                            len: current_token.kind.display_len()
                        }),
                    },
                    Expression::Identifier { typ, .. } => match typ {
                        Type::Base(BaseType::Str) | Type::Array { .. } => Ok(Expression::Unary {
                            op_position: Position::new(self.src, current_token.col).0,
                            op: UnaryOp::Len,
                            operand: Box::new(operand),
                        }),
                        Type::Base(
                            base_type @ (BaseType::Int | BaseType::Ascii | BaseType::Bool),
                        ) => Err(RawError {
                            kind: ErrorKind::Invalid(Statement::Len),
                            cause: ErrorCause::CannotTakeLenOfNumericValue(Type::Base(*base_type)),
                            col: current_token.col,
                            len: current_token.kind.display_len()
                        }),
                    },
                    Expression::Array { .. } => Ok(Expression::Unary {
                        op_position: Position::new(self.src, current_token.col).0,
                        op: UnaryOp::Len,
                        operand: Box::new(operand),
                    }),
                    Expression::ArrayIndex { typ, .. } => match typ {
                        BaseType::Str => Ok(Expression::Unary {
                            op_position: Position::new(self.src, current_token.col).0,
                            op: UnaryOp::Len,
                            operand: Box::new(operand),
                        }),
                        BaseType::Int | BaseType::Ascii | BaseType::Bool => Err(RawError {
                            kind: ErrorKind::Invalid(Statement::Len),
                            cause: ErrorCause::CannotTakeLenOfNumericValue(Type::Base(*typ)),
                            col: current_token.col,
                            len: current_token.kind.display_len()
                        }),
                    },
                    Expression::Unary { op, .. } => Err(RawError {
                        kind: ErrorKind::Invalid(Statement::Len),
                        cause: ErrorCause::CannotTakeLenOfNumericValue(op.typ()),
                        col: current_token.col,
                        len: current_token.kind.display_len()
                    }),
                    Expression::BooleanUnary { op, .. } => Err(RawError {
                        kind: ErrorKind::Invalid(Statement::Len),
                        cause: ErrorCause::CannotTakeLenOfNumericValue(op.typ()),
                        col: current_token.col,
                        len: current_token.kind.display_len()
                    }),
                    Expression::Binary { op, .. } => Err(RawError {
                        kind: ErrorKind::Invalid(Statement::Len),
                        cause: ErrorCause::CannotTakeLenOfNumericValue(op.typ()),
                        col: current_token.col,
                        len: current_token.kind.display_len()
                    }),
                    Expression::BooleanBinary { op, .. } => Err(RawError {
                        kind: ErrorKind::Invalid(Statement::Len),
                        cause: ErrorCause::CannotTakeLenOfNumericValue(op.typ()),
                        col: current_token.col,
                        len: current_token.kind.display_len()
                    }),
                    Expression::Comparison { op, .. } => Err(RawError {
                        kind: ErrorKind::Invalid(Statement::Len),
                        cause: ErrorCause::CannotTakeLenOfNumericValue(op.typ()),
                        col: current_token.col,
                        len: current_token.kind.display_len()
                    }),
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
                                op_position: Position::new(self.src, current_token.col).0,
                                op: UnaryOp::Plus,
                                operand: Box::new(operand),
                            })
                        } else {
                            Ok(operand)
                        }
                    }
                    invalid_type @ (Type::Base(
                        BaseType::Ascii | BaseType::Bool | BaseType::Str,
                    )
                    | Type::Array { .. }) => Err(RawError {
                        kind: ErrorKind::Invalid(Statement::Expression),
                        cause: ErrorCause::CannotTakeAbsValueOf(invalid_type),
                        col: current_token.col,
                        len: current_token.kind.display_len()
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
                                op_position: Position::new(self.src, current_token.col).0,
                                op: UnaryOp::WrappingPlus,
                                operand: Box::new(operand),
                            })
                        } else {
                            Ok(operand)
                        }
                    }
                    invalid_type @ (Type::Base(
                        BaseType::Ascii | BaseType::Bool | BaseType::Str,
                    )
                    | Type::Array { .. }) => Err(RawError {
                        kind: ErrorKind::Invalid(Statement::Expression),
                        cause: ErrorCause::CannotTakeAbsValueOf(invalid_type),
                        col: current_token.col,
                        len: current_token.kind.display_len()
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
                                op_position: Position::new(self.src, current_token.col).0,
                                op: UnaryOp::SaturatingPlus,
                                operand: Box::new(operand),
                            })
                        } else {
                            Ok(operand)
                        }
                    }
                    invalid_typ @ (Type::Base(BaseType::Ascii | BaseType::Bool | BaseType::Str)
                    | Type::Array { .. }) => Err(RawError {
                        kind: ErrorKind::Invalid(Statement::Expression),
                        cause: ErrorCause::CannotTakeAbsValueOf(invalid_typ),
                        col: current_token.col,
                        len: current_token.kind.display_len()
                    }),
                };
            }
            /*
            TODO(stefano): move parsing of numbers to here to allow for negative numbers natively
            i.e.: -9223372036854775808 (INT_MIN) is currently not allowed
            */
            TokenKind::Op(Op::Minus) => {
                let mut should_be_negated = true;

                // NOTE(stefano): this optimization should be moved to later stages
                // removing extra "-" symbols
                while let Some(&Token { kind: TokenKind::Op(Op::Minus), .. }) = self.next_token() {
                    should_be_negated = !should_be_negated;
                }

                let operand = self.primary_expression()?;

                // returning to avoid the call to tokens.next at the end of the function
                return match operand.typ() {
                    Type::Base(BaseType::Int | BaseType::Ascii) => {
                        if should_be_negated {
                            Ok(Expression::Unary {
                                op_position: Position::new(self.src, current_token.col).0,
                                op: UnaryOp::Minus,
                                operand: Box::new(operand),
                            })
                        } else {
                            Ok(operand)
                        }
                    }
                    invalid_typ @ (Type::Base(BaseType::Bool | BaseType::Str)
                    | Type::Array { .. }) => Err(RawError {
                        kind: ErrorKind::Invalid(Statement::Expression),
                        cause: ErrorCause::CannotNegate(invalid_typ),
                        col: current_token.col,
                        len: current_token.kind.display_len()
                    }),
                };
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

                let operand = self.primary_expression()?;

                // returning to avoid the call to tokens.next at the end of the function
                return match operand.typ() {
                    Type::Base(BaseType::Int | BaseType::Ascii) => {
                        if should_be_negated {
                            Ok(Expression::Unary {
                                op_position: Position::new(self.src, current_token.col).0,
                                op: UnaryOp::WrappingMinus,
                                operand: Box::new(operand),
                            })
                        } else {
                            Ok(operand)
                        }
                    }
                    invalid_typ @ (Type::Base(BaseType::Bool | BaseType::Str)
                    | Type::Array { .. }) => Err(RawError {
                        kind: ErrorKind::Invalid(Statement::Expression),
                        cause: ErrorCause::CannotNegate(invalid_typ),
                        col: current_token.col,
                        len: current_token.kind.display_len()
                    }),
                };
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

                let operand = self.primary_expression()?;

                // returning to avoid the call to tokens.next at the end of the function
                return match operand.typ() {
                    Type::Base(BaseType::Int | BaseType::Ascii) => {
                        if should_be_negated {
                            Ok(Expression::Unary {
                                op_position: Position::new(self.src, current_token.col).0,
                                op: UnaryOp::SaturatingMinus,
                                operand: Box::new(operand),
                            })
                        } else {
                            Ok(operand)
                        }
                    }
                    invalid_typ @ (Type::Base(BaseType::Bool | BaseType::Str)
                    | Type::Array { .. }) => Err(RawError {
                        kind: ErrorKind::Invalid(Statement::Expression),
                        cause: ErrorCause::CannotNegate(invalid_typ),
                        col: current_token.col,
                        len: current_token.kind.display_len()
                    }),
                };
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
                                op_position: Position::new(self.src, current_token.col).0,
                                op: UnaryOp::Not,
                                operand: Box::new(operand),
                            })
                        } else {
                            Ok(operand)
                        }
                    }
                    Type::Base(BaseType::Bool) => {
                        if should_be_inverted {
                            Ok(Expression::BooleanUnary {
                                op: BooleanUnaryOp::Not,
                                operand: Box::new(operand),
                            })
                        } else {
                            Ok(operand)
                        }
                    }
                    invalid_typ @ (Type::Base(BaseType::Str) | Type::Array { .. }) => {
                        Err(RawError {
                            kind: ErrorKind::Invalid(Statement::Expression),
                            cause: ErrorCause::CannotInvert(invalid_typ),
                            col: current_token.col,
                            len: current_token.kind.display_len()
                        })
                    }
                };
            }
            TokenKind::Definition(_)
            | TokenKind::Print
            | TokenKind::PrintLn
            | TokenKind::If
            | TokenKind::Else
            | TokenKind::Loop
            | TokenKind::Break
            | TokenKind::Continue => Err(RawError {
                kind: ErrorKind::Invalid(Statement::Expression),
                cause: ErrorCause::KeywordInExpression,
                col: current_token.col,
                len: current_token.kind.display_len()
            }),
            TokenKind::Bracket(_)
            | TokenKind::Op(_)
            | TokenKind::Comment(_)
            | TokenKind::Unexpected(_)
            | TokenKind::Colon
            | TokenKind::SemiColon
            | TokenKind::Comma
            | TokenKind::Eprint
            | TokenKind::EprintLn
            | TokenKind::Do => Err(RawError {
                kind: ErrorKind::Invalid(Statement::Expression),
                cause: ErrorCause::ExpectedOperand,
                col: current_token.col,
                len: current_token.kind.display_len()
            }),
        };

        _ = self.next_token();
        return factor;
    }

    fn exponentiative_expression(
        &mut self,
    ) -> Result<Expression<'src>, RawError<ErrorKind, ErrorCause>> {
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
                lhs: Box::new(lhs),
                op_position: Position::new(self.src, op_token.col).0,
                op: binary_op,
                rhs: Box::new(rhs),
            };
        }

        return Ok(lhs);
    }

    fn multiplicative_expression(
        &mut self,
    ) -> Result<Expression<'src>, RawError<ErrorKind, ErrorCause>> {
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
                lhs: Box::new(lhs),
                op_position: Position::new(self.src, op_token.col).0,
                op: binary_op,
                rhs: Box::new(rhs),
            };
        }

        return Ok(lhs);
    }

    fn additive_expression(&mut self) -> Result<Expression<'src>, RawError<ErrorKind, ErrorCause>> {
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
                lhs: Box::new(lhs),
                op_position: Position::new(self.src, op_token.col).0,
                op: binary_op,
                rhs: Box::new(rhs),
            };
        }

        return Ok(lhs);
    }

    /*
    IDEA(stefano): when the lhs is a literal integer shifts could be optimized to throw errors
    when preconditions such as negative numbers and shifts over 6bits are not met
    */
    fn shift_expression(&mut self) -> Result<Expression<'src>, RawError<ErrorKind, ErrorCause>> {
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
                lhs: Box::new(lhs),
                op_position: Position::new(self.src, op_token.col).0,
                op: binary_op,
                rhs: Box::new(rhs),
            };
        }

        return Ok(lhs);
    }

    fn bitand_expression(&mut self) -> Result<Expression<'src>, RawError<ErrorKind, ErrorCause>> {
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
                lhs: Box::new(lhs),
                op_position: Position::new(self.src, op_token.col).0,
                op: binary_op,
                rhs: Box::new(rhs),
            };
        }

        return Ok(lhs);
    }

    fn bitxor_expression(&mut self) -> Result<Expression<'src>, RawError<ErrorKind, ErrorCause>> {
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
                lhs: Box::new(lhs),
                op_position: Position::new(self.src, op_token.col).0,
                op: binary_op,
                rhs: Box::new(rhs),
            };
        }

        return Ok(lhs);
    }

    fn bitor_expression(&mut self) -> Result<Expression<'src>, RawError<ErrorKind, ErrorCause>> {
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
                lhs: Box::new(lhs),
                op_position: Position::new(self.src, op_token.col).0,
                op: binary_op,
                rhs: Box::new(rhs),
            };
        }

        return Ok(lhs);
    }

    fn comparison_expression(
        &mut self,
    ) -> Result<Expression<'src>, RawError<ErrorKind, ErrorCause>> {
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

            let lhs_typ = lhs.typ();
            let rhs_typ = rhs.typ();
            let can_compare = match (lhs_typ, rhs_typ) {
                (Type::Base(lhs_base_typ), Type::Base(rhs_base_typ)) => {
                    lhs_base_typ == rhs_base_typ
                }
                (
                    Type::Array { typ: lhs_base_typ, len: lhs_len },
                    Type::Array { typ: rhs_base_typ, len: rhs_len },
                ) => {
                    // comparing empty arrays makes no sense, so it's not going to be allowed
                    lhs_base_typ == rhs_base_typ && lhs_len > 0 && rhs_len > 0 && lhs_len == rhs_len
                }
                _ => false,
            };

            if !can_compare {
                return Err(RawError {
                    kind: ErrorKind::Invalid(Statement::Expression),
                    cause: ErrorCause::CannotCompareOperands { lhs_typ, rhs_typ },
                    col: op_token.col,
                    len: op_token.kind.display_len()
                });
            }

            if is_chained {
                return Err(RawError {
                    kind: ErrorKind::Invalid(Statement::Expression),
                    cause: ErrorCause::CannotChainComparisons,
                    col: op_token.col,
                    len: op_token.kind.display_len()
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
                lhs: Box::new(lhs),
                op: comparison_op,
                rhs: Box::new(rhs),
            };
        }

        return Ok(lhs);
    }

    fn and_expression(&mut self) -> Result<Expression<'src>, RawError<ErrorKind, ErrorCause>> {
        let mut lhs = self.comparison_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::And])? {
            let Type::Base(BaseType::Bool) = lhs.typ() else {
                return Err(RawError {
                    kind: ErrorKind::Invalid(Statement::Expression),
                    cause: ErrorCause::NonBooleanLeftOperand,
                    col: op_token.col,
                    len: op_token.kind.display_len()
                });
            };

            let rhs = self.comparison_expression()?;
            let Type::Base(BaseType::Bool) = rhs.typ() else {
                return Err(RawError {
                    kind: ErrorKind::Invalid(Statement::Expression),
                    cause: ErrorCause::NonBooleanRightOperand,
                    col: op_token.col,
                    len: op_token.kind.display_len()
                });
            };

            #[allow(clippy::wildcard_enum_match_arm)]
            let binary_op = match op {
                Op::And => BooleanBinaryOp::And,
                _ => unreachable!(),
            };

            lhs =
                Expression::BooleanBinary { lhs: Box::new(lhs), op: binary_op, rhs: Box::new(rhs) };
        }

        return Ok(lhs);
    }

    fn or_expression(&mut self) -> Result<Expression<'src>, RawError<ErrorKind, ErrorCause>> {
        let mut lhs = self.and_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::Or])? {
            let Type::Base(BaseType::Bool) = lhs.typ() else {
                return Err(RawError {
                    kind: ErrorKind::Invalid(Statement::Expression),
                    cause: ErrorCause::NonBooleanLeftOperand,
                    col: op_token.col,
                    len: op_token.kind.display_len()
                });
            };

            let rhs = self.and_expression()?;
            let Type::Base(BaseType::Bool) = rhs.typ() else {
                return Err(RawError {
                    kind: ErrorKind::Invalid(Statement::Expression),
                    cause: ErrorCause::NonBooleanRightOperand,
                    col: op_token.col,
                    len: op_token.kind.display_len()
                });
            };

            #[allow(clippy::wildcard_enum_match_arm)]
            let binary_op = match op {
                Op::Or => BooleanBinaryOp::Or,
                _ => unreachable!(),
            };

            lhs =
                Expression::BooleanBinary { lhs: Box::new(lhs), op: binary_op, rhs: Box::new(rhs) };
        }

        return Ok(lhs);
    }

    // TODO(stefano): disallow implicit conversions
    // TODO(stefano): introduce casting operators
    fn expression(&mut self) -> Result<Expression<'src>, RawError<ErrorKind, ErrorCause>> {
        return self.or_expression();
    }
}

// variables and types
impl<'src, 'tokens: 'src> Ast<'src, 'tokens> {
    fn resolve_variable(
        &self,
        name: &'src str,
    ) -> Option<(
        Mutability,
        usize, /* scope index */
        usize, /* variable index */
        &Variable<'src>,
    )> {
        let mut scope_index = self.scope_index;
        loop {
            let scope = &self.scopes[scope_index];
            for (var_index, var) in scope.let_variables.iter().enumerate() {
                if var.name == name {
                    return Some((Mutability::Let, scope_index, var_index, var));
                }
            }

            for (var_index, var) in scope.var_variables.iter().enumerate() {
                if var.name == name {
                    return Some((Mutability::Var, scope_index, var_index, var));
                }
            }

            scope_index = match scope_index {
                0 => return None,
                _ => scope.parent,
            };
        }
    }

    fn resolve_type(&self, name: &'src str) -> Option<BaseType> {
        let mut scope_index = self.scope_index;
        loop {
            let scope = &self.scopes[scope_index];
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
    ) -> Result<Option<(&'tokens Token<'src>, Type)>, RawError<ErrorKind, ErrorCause>> {
        let colon_token = self.next_token_bounded(Expected::TypeAnnotationOrVariableDefinition)?;

        let TokenKind::Colon = colon_token.kind else {
            self.token -= 1;
            return Ok(None);
        };

        let type_token = self.next_token_bounded(Expected::TypeAnnotation)?;
        let TokenKind::Identifier(type_name) = type_token.kind else {
            return Err(RawError {
                kind: ErrorKind::Invalid(Statement::TypeAnnotation),
                cause: ErrorCause::ExpectedTypeName,
                col: colon_token.col,
                len: colon_token.kind.display_len()
            });
        };

        let Some(base_type) = self.resolve_type(type_name) else {
            return match self.resolve_variable(type_name) {
                Some((_, _, _, var)) => Ok(Some((type_token, var.value.typ()))),
                None => Err(RawError {
                    kind: ErrorKind::Invalid(Statement::TypeAnnotation),
                    cause: ErrorCause::WasNotPreviouslyDefined,
                    col: type_token.col,
                    len: type_token.kind.display_len()
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
        let len = match len_token.kind {
            TokenKind::Literal(Literal::Int(len)) => match len {
                0 => {
                    return Err(RawError {
                        kind: ErrorKind::Invalid(Statement::Array),
                        cause: ErrorCause::ArrayOfZeroElements,
                        col: len_token.col,
                        len: len_token.kind.display_len()
                    })
                }
                1 => {
                    return Err(RawError {
                        kind: ErrorKind::Invalid(Statement::Array),
                        cause: ErrorCause::ArrayOfOneElement,
                        col: len_token.col,
                        len: len_token.kind.display_len()
                    })
                }
                _ => len as uint,
            },
            TokenKind::Literal(_)
            | TokenKind::Comment(_)
            | TokenKind::Unexpected(_)
            | TokenKind::Bracket(_)
            | TokenKind::Colon
            | TokenKind::SemiColon
            | TokenKind::Comma
            | TokenKind::Op(_)
            | TokenKind::True
            | TokenKind::False
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
            | TokenKind::Continue => {
                return Err(RawError {
                    kind: ErrorKind::Invalid(Statement::TypeAnnotation),
                    cause: ErrorCause::MustBeFollowedByIntegerExpression,
                    col: open_square_bracket_token.col,
                    len: open_square_bracket_token.kind.display_len()
                })
            }
        };

        let Some(
            close_square_bracket_token @ Token {
                kind: TokenKind::Bracket(BracketKind::CloseSquare),
                ..
            },
        ) = self.next_token()
        else {
            return Err(RawError {
                kind: ErrorKind::Invalid(Statement::TypeAnnotation),
                cause: ErrorCause::MustBeFollowedByClosingSquareBracket,
                col: open_square_bracket_token.col,
                len: open_square_bracket_token.kind.display_len()
            });
        };

        return Ok(Some((close_square_bracket_token, Type::Array { typ: base_type, len })));
    }

    fn variable_definition(
        &mut self,
        mutability: Mutability,
    ) -> Result<Node<'src>, RawError<ErrorKind, ErrorCause>> {
        let name_token = self.next_token_bounded(Expected::Identifier)?;
        let name = match name_token.kind {
            TokenKind::Identifier(name) => match self.resolve_type(name) {
                None => name,
                Some(_) => {
                    return Err(RawError {
                        kind: ErrorKind::Invalid(Statement::VariableName),
                        cause: ErrorCause::CannotBeATypeName,
                        col: name_token.col,
                        len: name_token.kind.display_len()
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
            | TokenKind::Literal(_)
            | TokenKind::True
            | TokenKind::False
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
            | TokenKind::Continue => {
                return Err(RawError {
                    kind: ErrorKind::Invalid(Statement::VariableDefinition),
                    cause: ErrorCause::ExpectedVariableName,
                    col: name_token.col,
                    len: name_token.kind.display_len()
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
            | TokenKind::Literal(_)
            | TokenKind::True
            | TokenKind::False
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
                    return Err(RawError {
                        kind: ErrorKind::Invalid(Statement::VariableDefinition),
                        cause: ErrorCause::ExpectedEqualsOrSemicolonAfterVariableName,
                        col: name_token.col,
                        len: name_token.kind.display_len()
                    })
                }
                Some((annotation_token, _)) => {
                    return Err(RawError {
                        kind: ErrorKind::Invalid(Statement::VariableDefinition),
                        cause: ErrorCause::ExpectedEqualsOrSemicolonAfterTypeAnnotation,
                        col: annotation_token.col,
                        len: annotation_token.kind.display_len()
                    })
                }
            },
        };

        let None = self.resolve_variable(name) else {
            return Err(RawError {
                kind: ErrorKind::VariableRedefinition,
                cause: ErrorCause::WasPreviouslyDefined,
                col: name_token.col,
                len: name_token.kind.display_len()
            });
        };

        return match expression {
            Some(value) => {
                if let Some((token, annotation_typ)) = annotation {
                    let value_typ = value.typ();
                    if annotation_typ != value_typ {
                        return Err(RawError {
                            kind: ErrorKind::Invalid(Statement::VariableDefinition),
                            cause: ErrorCause::VariableDefinitionTypeMismatch {
                                expected: annotation_typ,
                                actual: value_typ,
                            },
                            col: token.col,
                            len: token.kind.display_len()
                        });
                    }
                }

                let variables = match mutability {
                    Mutability::Let => &mut self.scopes[self.scope_index].let_variables,
                    Mutability::Var => &mut self.scopes[self.scope_index].var_variables,
                };

                variables.push(Variable { name, value });
                Ok(Node::Definition {
                    mutability,
                    scope_index: self.scope_index,
                    var_index: variables.len() - 1,
                })
            }
            None => match annotation {
                Some((_, typ)) => {
                    let variables = match mutability {
                        Mutability::Let => &mut self.scopes[self.scope_index].let_variables,
                        Mutability::Var => &mut self.scopes[self.scope_index].var_variables,
                    };

                    variables.push(Variable { name, value: typ.into() });
                    Ok(Node::Definition {
                        mutability,
                        scope_index: self.scope_index,
                        var_index: variables.len() - 1,
                    })
                }
                None => Err(RawError {
                    kind: ErrorKind::Invalid(Statement::VariableDefinition),
                    cause: ErrorCause::ExpectedTypeAnnotationOrValue,
                    col: name_token.col,
                    len: name_token.kind.display_len()
                }),
            },
        };
    }

    fn variable_reassignment(
        &mut self,
        op: Op,
        name: &'src str,
    ) -> Result<Node<'src>, RawError<ErrorKind, ErrorCause>> {
        let name_token = &self.tokens[self.token];

        if self.resolve_type(name).is_some() {
            return Err(RawError {
                kind: ErrorKind::Invalid(Statement::VariableAssignment),
                cause: ErrorCause::CannotBeATypeName,
                col: name_token.col,
                len: name_token.kind.display_len()
            });
        }

        let Some(op_token) = self.next_token() else {
            unreachable!("the presence of an op token should have been checked before the call to this function");
        };

        _ = self.next_token();
        let rhs = self.expression()?;

        let Some((mutability, scope_index, var_index, var)) = self.resolve_variable(name) else {
            return Err(RawError {
                kind: ErrorKind::Invalid(Statement::VariableAssignment),
                cause: ErrorCause::WasNotPreviouslyDefined,
                col: name_token.col,
                len: name_token.kind.display_len()
            });
        };

        let Mutability::Var = mutability else {
            return Err(RawError {
                kind: ErrorKind::Invalid(Statement::VariableAssignment),
                cause: ErrorCause::CannotMutateImmutableVariable,
                col: name_token.col,
                len: name_token.kind.display_len()
            });
        };

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

        let var_type = var.value.typ();
        let rhs_type = rhs.typ();

        /*
        NOTE(stefano): this entire match statement could be collapsed to just check if the lhs is of
        the same type as the rhs once implicit conversions are removed
        */
        return match assignment_op {
            AssignmentOp::Equals if var_type == rhs_type => Ok(Node::Assignment {
                scope_index,
                var_index,
                op: assignment_op,
                op_position: Position::new(self.src, op_token.col).0,
                new_value: rhs,
            }),
            AssignmentOp::Equals => Err(RawError {
                kind: ErrorKind::Invalid(Statement::VariableAssignment),
                cause: ErrorCause::VariableAssignmentTypeMismatch {
                    expected: var_type,
                    actual: rhs_type,
                },
                col: name_token.col,
                len: name_token.kind.display_len()
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
            | AssignmentOp::BitOr => match (var_type, rhs_type) {
                (
                    Type::Base(BaseType::Int | BaseType::Ascii | BaseType::Bool),
                    Type::Base(BaseType::Int | BaseType::Ascii | BaseType::Bool),
                ) => Ok(Node::Assignment {
                    scope_index,
                    var_index,
                    op: assignment_op,
                    op_position: Position::new(self.src, op_token.col).0,
                    new_value: rhs,
                }),
                _ => Err(RawError {
                    kind: ErrorKind::Invalid(Statement::VariableAssignment),
                    cause: ErrorCause::VariableAssignmentTypeMismatch {
                        expected: var_type,
                        actual: rhs_type,
                    },
                    col: name_token.col,
                    len: name_token.kind.display_len()
                }),
            },
        };
    }
}

// print statements
impl<'src, 'tokens: 'src> Ast<'src, 'tokens> {
    fn print_arg(&mut self) -> Result<Expression<'src>, RawError<ErrorKind, ErrorCause>> {
        let start_of_expression_token = self.next_token_bounded(Expected::Expression)?;
        let argument = self.expression()?;
        let Expression::Array { .. } = argument else {
            return Ok(argument);
        };

        return Err(RawError {
            kind: ErrorKind::Invalid(Statement::Expression),
            cause: ErrorCause::TemporaryArrayNotSupportedYet,
            col: start_of_expression_token.col,
            len: start_of_expression_token.kind.display_len()
        });
    }
}

// if statements
impl<'src, 'tokens: 'src> Ast<'src, 'tokens> {
    fn iff(&mut self) -> Result<Node<'src>, RawError<ErrorKind, ErrorCause>> {
        let mut if_statement = If { ifs: Vec::new(), els: None };

        'iff: while let Some(if_token) = self.tokens.get(self.token) {
            _ = self.next_token_bounded(Expected::BooleanExpression)?;

            let condition = self.expression()?;
            let Type::Base(BaseType::Bool) = condition.typ() else {
                return Err(RawError {
                    kind: ErrorKind::Invalid(Statement::If),
                    cause: ErrorCause::MustBeFollowedByABooleanExpression,
                    col: if_token.col,
                    len: if_token.kind.display_len()
                });
            };

            let after_condition_token = self.current_token_bounded(Expected::DoOrBlock)?;
            let iff = match after_condition_token.kind {
                TokenKind::Bracket(BracketKind::OpenCurly) => {
                    let Some(scope) = self.parse_single_any()? else {
                        unreachable!("this branch ensures there will be a block to be parsed");
                    };
                    IfStatement { condition, statement: scope }
                }
                TokenKind::Do => {
                    let Some(statement) = self.parse_do_statement()? else {
                        unreachable!(
                            "this branch ensures there will be a do statement to be parsed"
                        );
                    };
                    self.semicolon()?;
                    IfStatement { condition, statement }
                }
                TokenKind::Bracket(_)
                | TokenKind::Comment(_)
                | TokenKind::Unexpected(_)
                | TokenKind::Colon
                | TokenKind::SemiColon
                | TokenKind::Comma
                | TokenKind::Op(_)
                | TokenKind::Literal(_)
                | TokenKind::True
                | TokenKind::False
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
                    return Err(RawError {
                        kind: ErrorKind::Invalid(Statement::If),
                        cause: ErrorCause::MustBeFollowedByDoOrBlock,
                        col: before_curly_bracket_token.col,
                        len: before_curly_bracket_token.kind.display_len()
                    });
                }
            };

            if_statement.ifs.push(iff);

            while let Some(else_token) = self.tokens.get(self.token) {
                let after_else_token = match else_token.kind {
                    TokenKind::Else => self.next_token_bounded(Expected::DoOrBlockOrIfStatement)?,
                    TokenKind::Comment(_)
                    | TokenKind::Unexpected(_)
                    | TokenKind::Bracket(_)
                    | TokenKind::Colon
                    | TokenKind::SemiColon
                    | TokenKind::Comma
                    | TokenKind::Op(_)
                    | TokenKind::Literal(_)
                    | TokenKind::True
                    | TokenKind::False
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
                        let Some(scope) = self.parse_single_any()? else {
                            unreachable!("this branch ensures there will be a block to be parsed");
                        };
                        if_statement.els = Some(Box::new(scope));
                        break 'iff;
                    }
                    TokenKind::Do => {
                        let Some(statement) = self.parse_do_statement()? else {
                            unreachable!(
                                "this branch ensures there will be a do statement to be parsed"
                            );
                        };
                        self.semicolon()?;
                        if_statement.els = Some(Box::new(statement));
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
                    | TokenKind::Literal(_)
                    | TokenKind::True
                    | TokenKind::False
                    | TokenKind::Identifier(_)
                    | TokenKind::Definition(_)
                    | TokenKind::Print
                    | TokenKind::PrintLn
                    | TokenKind::Eprint
                    | TokenKind::EprintLn
                    | TokenKind::Else
                    | TokenKind::Loop
                    | TokenKind::Break
                    | TokenKind::Continue => Err(RawError {
                        kind: ErrorKind::Invalid(Statement::If),
                        cause: ErrorCause::MustBeFollowedByDoOrBlockOrIfStatement,
                        col: else_token.col,
                        len: else_token.kind.display_len()
                    }),
                };

                else_if?;
            }
        }

        return Ok(Node::If(if_statement));
    }
}

// loop statements
impl<'src, 'tokens: 'src> Ast<'src, 'tokens> {
    fn loop_statement(&mut self) -> Result<Node<'src>, RawError<ErrorKind, ErrorCause>> {
        let do_token = &self.tokens[self.token];
        let loop_token = match do_token.kind {
            TokenKind::Do => {
                let loop_token = self.next_token_bounded(Expected::LoopStatement)?;
                let TokenKind::Loop = loop_token.kind else {
                    return Err(RawError {
                        kind: ErrorKind::Invalid(Statement::Loop),
                        cause: ErrorCause::MustBeFollowedByLoop,
                        col: do_token.col,
                        len: do_token.kind.display_len()
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
            | TokenKind::Literal(_)
            | TokenKind::True
            | TokenKind::False
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
            return Err(RawError {
                kind: ErrorKind::Invalid(Statement::Loop),
                cause: ErrorCause::MustBeFollowedByABooleanExpression,
                col: loop_token.col,
                len: loop_token.kind.display_len()
            });
        };

        let after_condition_token = self.current_token_bounded(Expected::DoOrBlock)?;
        let statement_result = match after_condition_token.kind {
            TokenKind::Bracket(BracketKind::OpenCurly) => {
                let Some(scope) = self.parse_single_any()? else {
                    unreachable!("this branch ensures there will be a block to be parsed");
                };
                Ok(scope)
            }
            TokenKind::Do => {
                let Some(statement) = self.parse_do_statement()? else {
                    unreachable!("this branch ensures there will be a do statement to be parsed");
                };
                self.semicolon()?;
                Ok(statement)
            }
            TokenKind::Bracket(_)
            | TokenKind::Comment(_)
            | TokenKind::Unexpected(_)
            | TokenKind::Colon
            | TokenKind::SemiColon
            | TokenKind::Comma
            | TokenKind::Op(_)
            | TokenKind::Literal(_)
            | TokenKind::True
            | TokenKind::False
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
                Err(RawError {
                    kind: ErrorKind::Invalid(Statement::Loop),
                    cause: ErrorCause::MustBeFollowedByDoOrBlock,
                    col: before_curly_bracket_token.col,
                    len: before_curly_bracket_token.kind.display_len()
                })
            }
        };

        let statement = statement_result?;
        if let TokenKind::Do = do_token.kind {
            return Ok(Node::DoLoop(DoLoop { condition, statement: Box::new(statement) }));
        }
        return Ok(Node::Loop(Loop { condition, statement: Box::new(statement) }));
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
            Self::CommaOrClosingSquareBracket => {
                write!(f, "comma or closing square bracket")
            }
            Self::TypeAnnotationOrVariableDefinition => {
                write!(f, "type annotation or variable definition")
            }
            Self::TypeAnnotation => write!(f, "type annotation"),
            Self::ArrayLength => write!(f, "array length"),
            Self::Identifier => write!(f, "identifier"),
            Self::EqualsOrSemicolon => write!(f, "'=' or ';'"),
            Self::DoOrBlock => write!(f, "do statement or block"),
            Self::DoOrBlockOrIfStatement => {
                write!(f, "do statement, block or if statement")
            }
            Self::LoopStatement => write!(f, "loop statement"),
        };
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Statement,
    If,
    Block,
    Do,
    Loop,
    Break,
    Continue,
    TypeAnnotation,
    ItemSeparator,
    Assignment,
    Expression,
    Len,
    Array,
    ArrayIndex,
    ArrayItem,
    VariableName,
    VariableDefinition,
    VariableAssignment,
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Statement => write!(f, "statement"),
            Self::If => write!(f, "if statement"),
            Self::Block => write!(f, "block statement"),
            Self::Do => write!(f, "do statement"),
            Self::Loop => write!(f, "loop statement"),
            Self::Break => write!(f, "break statement"),
            Self::Continue => write!(f, "continue statement"),
            Self::TypeAnnotation => write!(f, "type annotation"),
            Self::ItemSeparator => write!(f, "item separator"),
            Self::Assignment => write!(f, "assignment"),
            Self::Expression => write!(f, "expression"),
            Self::Len => write!(f, "len expression"),
            Self::Array => write!(f, "array"),
            Self::ArrayIndex => write!(f, "array index"),
            Self::ArrayItem => write!(f, "array item"),
            Self::VariableName => write!(f, "variable name"),
            Self::VariableDefinition => write!(f, "variable definition"),
            Self::VariableAssignment => write!(f, "variable assignment"),
        };
    }
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    Expected(Expected),
    Invalid(Statement),
    VariableNotPreviouslyDefined,
    VariableRedefinition,
}

impl super::ErrorKind for ErrorKind {}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Expected(expected) => write!(f, "expected {expected}"),
            Self::Invalid(invalid) => write!(f, "invalid {invalid}"),
            Self::VariableNotPreviouslyDefined => write!(f, "variable not previously defined"),
            Self::VariableRedefinition => write!(f, "variable redefinition"),
        };
    }
}

#[derive(Debug, Clone)]
pub enum ErrorCause {
    MissingSemicolon,
    StrayElseBlock,
    StrayColon,
    StrayComma,
    StrayEquals,
    StrayBinaryOperator(Op),

    NoMoreTokens,
    StringLeftOperand,
    StringRightOperand,
    ArrayLeftOperand,
    ArrayRightOperand,
    NonBooleanLeftOperand,
    NonBooleanRightOperand,

    CanOnlyBeUsedInLoops,

    NotAllowedIn { not_allowed: Statement, in_: Statement },
    TemporaryArrayNotSupportedYet,
    NestedArrayNotSupportedYet,
    EmptyExpression,
    UnclosedBracket(BracketKind),

    ArrayOfZeroElements,
    ArrayOfOneElement,
    MismatchedArrayElementType { expected: Type, actual: Type },

    WasNotPreviouslyDefined,
    WasPreviouslyDefined,

    CannotBeATypeName,
    CannotNegate(Type),
    CannotInvert(Type),
    CannotTakeAbsValueOf(Type),
    CannotMutateImmutableVariable,
    CannotChainComparisons,
    CannotIndexNonArrayType(Type),
    CannotCompareOperands { lhs_typ: Type, rhs_typ: Type },
    CannotTakeLenOfNumericValue(Type),

    KeywordInExpression,
    VariableDefinitionTypeMismatch { expected: Type, actual: Type },
    VariableAssignmentTypeMismatch { expected: Type, actual: Type },

    MustBeFollowedByABooleanExpression,
    MustBeFollowedByDoOrBlock,
    MustBeFollowedByDoOrBlockOrIfStatement,
    MustBeFollowedByIntegerExpression,
    MustBeFollowedByClosingSquareBracket,
    MustBeFollowedByLoop,

    ExpectedOperand,
    ExpectedTypeName,
    ExpectedTypeAnnotation,
    ExpectedTypeAnnotationOrValue,
    ExpectedVariableName,
    ExpectedEqualsOrSemicolonAfterVariableName,
    ExpectedEqualsOrSemicolonAfterTypeAnnotation,
}

impl super::ErrorCause for ErrorCause {}

impl Display for ErrorCause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::MissingSemicolon => write!(f, "expected semicolon after here"),
            Self::StrayElseBlock => write!(f, "stray else block"),
            Self::CanOnlyBeUsedInLoops => write!(f, "can only be used in loops"),
            Self::NotAllowedIn { not_allowed, in_ } => {
                write!(f, "{not_allowed} are not allowed in {in_}")
            }
            Self::TemporaryArrayNotSupportedYet => {
                write!(
                    f,
                    "temporary arrays are not supported yet, extract this to a variable first"
                )
            }
            Self::StrayColon => write!(f, "stray colon"),
            Self::StrayComma => write!(f, "stray comma"),
            Self::StrayEquals => write!(f, "stray equals"),
            Self::StrayBinaryOperator(op) => write!(f, "stray '{op}' operator"),
            Self::NoMoreTokens => write!(f, "no more tokens left after here"),
            Self::StringLeftOperand => write!(f, "cannot be preceded by a string"),
            Self::StringRightOperand => write!(f, "cannot be followed by a string"),
            Self::ArrayLeftOperand => write!(f, "cannot be preceded by an array"),
            Self::ArrayRightOperand => write!(f, "cannot be followed by an array"),
            Self::MustBeFollowedByIntegerExpression => {
                write!(f, "must be followed by an integer expression")
            }
            Self::MustBeFollowedByClosingSquareBracket => {
                write!(f, "must be followed by a close square bracket")
            }
            Self::ArrayOfZeroElements => {
                write!(f, "arrays of zero elements are not allowed, as they are pratically phantom values")
            }
            Self::ArrayOfOneElement => {
                write!(f, "arrays of one element are not allowed, as they are pratically the same as the value itself")
            }
            Self::NestedArrayNotSupportedYet => write!(f, "nested arrays not supported yet"),
            Self::CannotIndexNonArrayType(typ) => {
                write!(f, "cannot index into a value of type '{typ}'")
            }
            Self::WasNotPreviouslyDefined => write!(f, "was not previously defined in this scope"),
            Self::WasPreviouslyDefined => write!(f, "was previously defined in this scope"),
            Self::CannotBeATypeName => write!(f, "cannot be a type name"),
            Self::EmptyExpression => write!(f, "empty expressions are not allowed"),
            Self::UnclosedBracket(bracket) => write!(f, "'{bracket}' bracket was not closed"),
            Self::MismatchedArrayElementType { expected, actual } => {
                write!(f, "expected item of type '{expected}', but got '{actual}'")
            }
            Self::CannotNegate(typ) => write!(f, "cannot negate a value of type '{typ}'"),
            Self::CannotInvert(typ) => write!(f, "cannot invert a value of type '{typ}'"),
            Self::CannotTakeAbsValueOf(typ) => {
                write!(f, "cannot take the absolute value of a variable of type '{typ}'")
            }
            Self::CannotTakeLenOfNumericValue(typ) => {
                write!(f, "cannot take the length of '{typ}', only of arrays and strings")
            }
            Self::KeywordInExpression => write!(f, "cannot be a keyword"),
            Self::ExpectedOperand => write!(f, "expected expression operand before this token"),
            Self::CannotChainComparisons => write!(f, "comparison operators cannot be chained"),
            Self::NonBooleanLeftOperand => write!(f, "must be preceded by a boolean expression"),
            Self::NonBooleanRightOperand => write!(f, "must be followed by a boolean expression"),
            Self::CannotCompareOperands { lhs_typ, rhs_typ } => {
                write!(f, "cannot compare '{lhs_typ}' to '{rhs_typ}'")
            }
            Self::ExpectedTypeName => write!(f, "expected type name after here"),
            Self::ExpectedTypeAnnotation => {
                write!(f, "expected type annotation after here to infer the type of the variable")
            }
            Self::ExpectedTypeAnnotationOrValue => {
                write!(f, "expected type annotation or value after here to infer the type of the variable")
            }
            Self::ExpectedVariableName => write!(f, "expected variable name after here"),
            Self::ExpectedEqualsOrSemicolonAfterVariableName => {
                write!(f, "expected '=' or ';' after variable name")
            }
            Self::ExpectedEqualsOrSemicolonAfterTypeAnnotation => {
                write!(f, "expected '=' or ';' after type annotation")
            }
            Self::VariableDefinitionTypeMismatch { expected, actual } => {
                write!(f, "declared type of '{expected}' doesn't match value of type '{actual}'")
            }
            Self::VariableAssignmentTypeMismatch { expected, actual } => {
                write!(f, "trying to assign an expression of type '{actual}' to a variable of type '{expected}'")
            }
            Self::CannotMutateImmutableVariable => write!(f, "cannot mutate immutable variable"),
            Self::MustBeFollowedByABooleanExpression => {
                write!(f, "must be followed by a boolean expression")
            }
            Self::MustBeFollowedByDoOrBlock => {
                write!(f, "must be followed by a do statement or a block")
            }
            Self::MustBeFollowedByDoOrBlockOrIfStatement => {
                write!(f, "must be followed by a do statement, a block or an if statement")
            }
            Self::MustBeFollowedByLoop => {
                write!(f, "must be followed by a loop statement")
            }
        };
    }
}
