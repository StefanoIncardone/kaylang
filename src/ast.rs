use super::{
    src_file::{Position, SrcFile},
    tokenizer::{BracketKind, Literal, Mutability, Op, SrcCodeLen, Token, TokenKind},
};
use crate::{
    error::{RawSyntaxError, SyntaxErrorCause, SyntaxErrorKind, SyntaxErrors},
    tokenizer::{ascii, int, uint},
};
use std::fmt::{Debug, Display};

pub(crate) trait TypeOf {
    fn typ(&self) -> Type;
}

#[derive(Debug, Clone)]
pub enum Type {
    Infer,
    Int,
    Ascii,
    Bool,
    Str,
    // TODO(breaking)(stefano): enforce a max length
    Array { typ: Box<Type>, len: uint },
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Infer, Self::Infer)
            | (Self::Int, Self::Int)
            | (Self::Ascii, Self::Ascii)
            | (Self::Bool, Self::Bool)
            | (Self::Str, Self::Str) => true,

            (Self::Array { typ: typ_1, len: len_1 }, Self::Array { typ: typ_2, len: len_2 }) => {
                len_1 == len_2 && typ_1 == typ_2
            }

            _ => false,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int => write!(f, "int"),
            Self::Ascii => write!(f, "ascii"),
            Self::Bool => write!(f, "bool"),
            Self::Str => write!(f, "str"),
            Self::Array { typ, len } => write!(f, "{typ}[{len}]"),

            Self::Infer => write!(f, "infer"),
        }
    }
}

impl Type {
    pub(crate) fn size(&self) -> usize {
        match self {
            Self::Int => core::mem::size_of::<int>(),
            Self::Ascii => core::mem::size_of::<ascii>(),
            Self::Bool => core::mem::size_of::<bool>(),
            Self::Str => core::mem::size_of::<*const ascii>() + core::mem::size_of::<int>(),
            Self::Array { typ, len } => typ.size() * len,

            Self::Infer => unreachable!("should have been coerced to a concrete type"),
        }
    }

    pub(crate) fn should_be_inferred(&self) -> bool {
        match self {
            Self::Infer => true,
            Self::Array { typ, .. } => typ.should_be_inferred(),
            Self::Int | Self::Ascii | Self::Bool | Self::Str => false,
        }
    }

    pub(crate) fn inner(&self) -> Self {
        match self {
            Self::Array { typ, .. } => typ.inner(),
            _ => self.clone(),
        }
    }

    pub(crate) fn can_be_compared_to(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Str, Self::Str)
            | (Self::Int | Self::Bool | Self::Ascii, Self::Int | Self::Bool | Self::Ascii) => true,

            (Self::Array { typ: typ_1, len: len_1 }, Self::Array { typ: typ_2, len: len_2 }) => {
                // comparing empty arrays makes no sense, so it's not going to be allowed
                *len_1 != 0 && *len_2 != 0 && len_1 == len_2 && typ_1.can_be_compared_to(typ_2)
            }

            (Self::Str | Self::Array { .. } | Self::Infer, _)
            | (_, Self::Str | Self::Array { .. } | Self::Infer) => false,
        }
    }
}

impl TypeOf for Literal {
    fn typ(&self) -> Type {
        match self {
            Self::Int(_) => Type::Int,
            Self::Ascii(_) => Type::Ascii,
            Self::Bool(_) => Type::Bool,
            Self::Str(_) => Type::Str,
        }
    }
}

impl TypeOf for Op {
    fn typ(&self) -> Type {
        match self {
            Self::Compare
            | Self::Pow
            | Self::PowEquals
            | Self::Times
            | Self::TimesEquals
            | Self::Divide
            | Self::DivideEquals
            | Self::Remainder
            | Self::RemainderEquals
            | Self::Plus
            | Self::PlusEquals
            | Self::Minus
            | Self::MinusEquals
            | Self::BitAnd
            | Self::BitAndEquals
            | Self::BitOr
            | Self::BitOrEquals
            | Self::BitXor
            | Self::BitXorEquals
            | Self::LeftShift
            | Self::LeftShiftEquals
            | Self::RightShift
            | Self::RightShiftEquals => Type::Int,

            Self::EqualsEquals
            | Self::NotEquals
            | Self::Greater
            | Self::GreaterOrEquals
            | Self::Less
            | Self::LessOrEquals
            | Self::Not
            | Self::And
            | Self::AndEquals
            | Self::Or
            | Self::OrEquals => Type::Bool,

            Self::Equals => unreachable!("equals operator doesn't have a type"),
        }
    }
}

#[derive(Clone)]
pub(crate) enum Expression<'src> {
    Literal(Literal),
    Unary {
        op: Op,
        operand: Box<Expression<'src>>,
    },
    Binary {
        lhs: Box<Expression<'src>>,
        op_position: Position,
        op: Op,
        rhs: Box<Expression<'src>>,
    },
    Identifier {
        typ: Type,
        name: &'src str,
    },
    Array {
        typ: Type,
        items: Vec<Expression<'src>>,
    },
    ArrayIndex {
        typ: Type,
        var_name: &'src str,
        bracket_position: Position,
        index: Box<Expression<'src>>,
    },
}

impl Debug for Expression<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(literal) => write!(f, "{literal}"),
            Self::Unary { op, operand } => write!(f, "{op}{operand:?}"),
            Self::Binary { lhs, op, rhs, .. } => write!(f, "({lhs:?} {op} {rhs:?})"),
            Self::Identifier { name, .. } => write!(f, "{name}"),
            Self::Array { items, .. } => {
                write!(f, "[")?;
                if !items.is_empty() {
                    let mut items_iter = items.iter();
                    let last = items_iter.next_back().unwrap(); // we have already checked for a non empty array
                    for item in items_iter {
                        write!(f, "{item:?}, ")?;
                    }

                    write!(f, "{last:?}")?;
                }
                write!(f, "]")
            }
            Self::ArrayIndex { var_name, index, .. } => write!(f, "{var_name}[{index:?}]"),
        }
    }
}

impl TypeOf for Expression<'_> {
    fn typ(&self) -> Type {
        match self {
            Self::Literal(literal) => literal.typ(),
            Self::Unary { operand, .. } => operand.typ(),
            Self::Binary { op, .. } => op.typ(),
            Self::Identifier { typ, .. } => typ.clone(),
            Self::Array { typ, items } => {
                Type::Array { typ: Box::new(typ.clone()), len: items.len() }
            }
            Self::ArrayIndex { typ, .. } => typ.clone(),
        }
    }
}

impl From<Type> for Expression<'_> {
    fn from(typ: Type) -> Self {
        match typ {
            Type::Bool => Self::Literal(Literal::Bool(false)),
            Type::Ascii => Self::Literal(Literal::Ascii(0)),
            Type::Int => Self::Literal(Literal::Int(0)),
            Type::Str => Self::Literal(Literal::Str(Vec::new())),
            Type::Array { typ, len } => {
                let mut items = Vec::<Expression<'_>>::with_capacity(len);
                for _ in 0..=len {
                    let inner_typ = *typ.clone();
                    items.push(inner_typ.into());
                }
                Self::Array { typ: *typ, items }
            }
            Type::Infer => unreachable!("should have been coerced to a concrete type"),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Variable<'src> {
    pub(crate) mutability: Mutability,
    pub(crate) name: &'src str,
    pub(crate) value: Expression<'src>,
}

#[derive(Debug, Clone)]
pub(crate) struct IfStatement<'src> {
    pub(crate) condition: Expression<'src>,
    pub(crate) statement: Node<'src>,
}

impl Display for IfStatement<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if {:?}", self.condition)
    }
}

#[derive(Debug, Clone)]
pub(crate) struct If<'src> {
    pub(crate) ifs: Vec<IfStatement<'src>>,
    pub(crate) els: Option<Box<Node<'src>>>,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub(crate) enum LoopCondition<'src> {
    Loop(Expression<'src>),
    DoLoop(Expression<'src>),
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub(crate) struct Loop<'src> {
    pub(crate) condition: LoopCondition<'src>,
    pub(crate) statement: Box<Node<'src>>,
}

impl Display for Loop<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.condition {
            LoopCondition::Loop(condition) => write!(f, "loop {condition:?}"),
            LoopCondition::DoLoop(condition) => write!(f, "do loop {condition:?}"),
        }
    }
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
    Break,
    Continue,

    Definition { scope_index: usize, var_index: usize },
    Assignment { scope_index: usize, var_index: usize, new_value: Expression<'src> },

    Scope { index: usize },
}

impl Display for Node<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Semicolon => write!(f, ";"),
            Self::Expression(expression) => write!(f, "{expression:?}"),
            Self::Print(arg) => write!(f, "print {arg:?}"),
            Self::Println(Some(arg)) => write!(f, "println {arg:?}"),
            Self::Println(None) => write!(f, "println"),
            Self::Eprint(arg) => write!(f, "eprint {arg:?}"),
            Self::Eprintln(Some(arg)) => write!(f, "eprintln {arg:?}"),
            Self::Eprintln(None) => write!(f, "eprintln"),
            Self::If(iff) => write!(f, "{}", iff.ifs[0]),
            Self::Loop(looop) => write!(f, "{looop}"),
            Self::Break => write!(f, "break"),
            Self::Continue => write!(f, "continue"),

            Self::Definition { .. } | Self::Assignment { .. } | Self::Scope { .. } => {
                unreachable!("should never be displayed")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Scope<'src> {
    pub(crate) parent: usize,
    pub(crate) types: Vec<Type>,
    pub(crate) variables: Vec<Variable<'src>>,
    pub(crate) nodes: Vec<Node<'src>>,
}

// IDEA(stefano): create Parser class that builds the AST, and then validate the AST afterwards
#[derive(Debug)]
pub struct Ast<'src, 'tokens: 'src> {
    src: &'src SrcFile,
    errors: Vec<RawSyntaxError<ErrorKind, ErrorCause>>,

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
    ) -> Result<Vec<Scope<'src>>, SyntaxErrors<'src, ErrorKind, ErrorCause>> {
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
                types: vec![Type::Int, Type::Ascii, Type::Bool, Type::Str],
                variables: Vec::new(),
                nodes: Vec::new(),
            }],
            loop_depth: 0,
        };

        this.parse_scope();

        if this.errors.is_empty() {
            Ok(this.scopes)
        } else {
            Err(SyntaxErrors { src, raw_errors: this.errors })
        }
    }

    fn semicolon(&mut self) -> Result<(), RawSyntaxError<ErrorKind, ErrorCause>> {
        let semicolon_token = self.current_token_bounded(Expected::Semicolon)?;
        let TokenKind::SemiColon = &semicolon_token.kind else {
            let previous_token = self.peek_previous_token();
            return Err(RawSyntaxError {
                kind: ErrorKind::Invalid(Statement::Statement),
                cause: ErrorCause::MissingSemicolon,
                col: previous_token.col,
                len: previous_token.kind.src_code_len(),
            });
        };

        let _ = self.next_token();
        Ok(())
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
                            if let Err(err) = self.semicolon() {
                                self.errors.push(err);

                                // consuming all remaining tokens until the end of the file
                                self.token = self.tokens.len();
                                break;
                            }
                        }

                        // no need to check for a terminating semicolon
                        Node::If(_) | Node::Loop(_) | Node::Scope { .. } => {}
                    }

                    self.scopes[self.scope_index].nodes.push(node);
                }
                Ok(None) => break,
                // NOTE(stefano): only parsing until the first error until a fault tolerant parser is developed,
                // this is because the first truly relevant error is the first one, which in turn
                // causes a ripple effect that propagates to the rest of the parsing, causing
                // subsequent errors to be wrong
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
    ) -> Result<Option<Node<'src>>, RawSyntaxError<ErrorKind, ErrorCause>> {
        let Some(current_token) = self.tokens.get(self.token) else { return Ok(None) };

        match current_token.kind {
            TokenKind::Literal(_)
            | TokenKind::True
            | TokenKind::False
            | TokenKind::Bracket(BracketKind::OpenRound)
            | TokenKind::Op(Op::Plus | Op::Minus | Op::Not) => {
                Ok(Some(Node::Expression(self.expression()?)))
            }
            TokenKind::Identifier(_) => match self.peek_next_token() {
                Some(op) => match op.kind {
                    TokenKind::Op(
                        Op::Equals
                        | Op::PowEquals
                        | Op::TimesEquals
                        | Op::DivideEquals
                        | Op::RemainderEquals
                        | Op::PlusEquals
                        | Op::MinusEquals
                        | Op::LeftShiftEquals
                        | Op::RightShiftEquals
                        | Op::BitAndEquals
                        | Op::BitXorEquals
                        | Op::BitOrEquals
                        | Op::AndEquals
                        | Op::OrEquals,
                    ) => Ok(Some(self.variable_reassignment()?)),
                    _ => Ok(Some(Node::Expression(self.expression()?))),
                },
                None => Ok(Some(Node::Expression(self.expression()?))),
            },
            TokenKind::Definition(_) => Ok(Some(self.variable_definition()?)),
            TokenKind::Print => {
                let arg = self.print_arg()?;
                Ok(Some(Node::Print(arg)))
            }
            TokenKind::PrintLn => {
                if let Some(&Token { kind: TokenKind::SemiColon, .. }) = self.peek_next_token() {
                    let _ = self.next_token();
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
                    let _ = self.next_token();
                    return Ok(Some(Node::Eprintln(None)));
                }

                let arg = self.print_arg()?;
                Ok(Some(Node::Eprintln(Some(arg))))
            }
            TokenKind::If => Ok(Some(self.iff()?)),
            TokenKind::Else => {
                let _ = self.next_token();
                Err(RawSyntaxError {
                    kind: ErrorKind::Invalid(Statement::If),
                    cause: ErrorCause::StrayElseBlock,
                    col: current_token.col,
                    len: current_token.kind.src_code_len(),
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
                let _ = self.next_token();
                match self.loop_depth {
                    0 => Err(RawSyntaxError {
                        kind: ErrorKind::Invalid(Statement::Break),
                        cause: ErrorCause::CanOnlyBeUsedInLoops,
                        col: current_token.col,
                        len: current_token.kind.src_code_len(),
                    }),
                    _ => Ok(Some(Node::Break)),
                }
            }
            TokenKind::Continue => {
                let _ = self.next_token();
                match self.loop_depth {
                    0 => Err(RawSyntaxError {
                        kind: ErrorKind::Invalid(Statement::Continue),
                        cause: ErrorCause::CanOnlyBeUsedInLoops,
                        col: current_token.col,
                        len: current_token.kind.src_code_len(),
                    }),
                    _ => Ok(Some(Node::Continue)),
                }
            }
            TokenKind::SemiColon => {
                let _ = self.next_token();
                Ok(Some(Node::Semicolon))
            }
            TokenKind::Bracket(BracketKind::OpenCurly) => {
                let (position, _) = Position::new(self.src, current_token.col);
                unreachable!(
                    "blocks not allowed in single statements: {path}:{line}:{col}",
                    path = self.src.path.display(),
                    line = position.line,
                    col = position.col,
                )
            }
            TokenKind::Bracket(BracketKind::OpenSquare) => {
                let _ = self.expression()?;
                Err(RawSyntaxError {
                    kind: ErrorKind::Invalid(Statement::Statement),
                    cause: ErrorCause::TemporaryArrayNotSupportedYet,
                    col: current_token.col,
                    len: current_token.kind.src_code_len(),
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
                )
            }
            TokenKind::Colon => {
                let _ = self.next_token();
                Err(RawSyntaxError {
                    kind: ErrorKind::Invalid(Statement::TypeAnnotation),
                    cause: ErrorCause::StrayColon,
                    col: current_token.col,
                    len: current_token.kind.src_code_len(),
                })
            }
            TokenKind::Comma => {
                let _ = self.next_token();
                Err(RawSyntaxError {
                    kind: ErrorKind::Invalid(Statement::ItemSeparator),
                    cause: ErrorCause::StrayComma,
                    col: current_token.col,
                    len: current_token.kind.src_code_len(),
                })
            }
            TokenKind::Op(Op::Equals) => {
                let _ = self.next_token();
                Err(RawSyntaxError {
                    kind: ErrorKind::Invalid(Statement::Assignment),
                    cause: ErrorCause::StrayEquals,
                    col: current_token.col,
                    len: current_token.kind.src_code_len(),
                })
            }
            TokenKind::Op(op) => {
                let _ = self.next_token();
                Err(RawSyntaxError {
                    kind: ErrorKind::Invalid(Statement::Expression),
                    cause: ErrorCause::StrayBinaryOperator(op),
                    col: current_token.col,
                    len: current_token.kind.src_code_len(),
                })
            }
            TokenKind::Comment(_) => unreachable!("should be skipped by the token iterator"),
            TokenKind::Unexpected(_) => unreachable!("only valid tokens should be present"),
        }
    }

    fn parse_do_statement(
        &mut self,
    ) -> Result<Option<Node<'src>>, RawSyntaxError<ErrorKind, ErrorCause>> {
        let current_token = self.next_token_bounded(Expected::StatementAfterDo)?;
        match current_token.kind {
            TokenKind::Bracket(BracketKind::OpenCurly) => {
                let _ = self.next_token();
                Err(RawSyntaxError {
                    kind: ErrorKind::Invalid(Statement::Block),
                    cause: ErrorCause::NotAllowedIn {
                        not_allowed: Statement::Block,
                        in_: Statement::Do,
                    },
                    col: current_token.col,
                    len: current_token.kind.src_code_len(),
                })
            }
            TokenKind::Definition(_) => {
                let _ = self.next_token();
                Err(RawSyntaxError {
                    kind: ErrorKind::Invalid(Statement::VariableDefinition),
                    cause: ErrorCause::NotAllowedIn {
                        not_allowed: Statement::VariableDefinition,
                        in_: Statement::Do,
                    },
                    col: current_token.col,
                    len: current_token.kind.src_code_len(),
                })
            }
            _ => self.parse_single_statement(),
        }
    }

    fn parse_single_any(
        &mut self,
    ) -> Result<Option<Node<'src>>, RawSyntaxError<ErrorKind, ErrorCause>> {
        let Some(current_token) = self.tokens.get(self.token) else { return Ok(None) };

        match current_token.kind {
            TokenKind::Bracket(BracketKind::OpenCurly) => {
                let new_scope_index = self.scopes.len();
                self.scopes.push(Scope {
                    parent: self.scope_index,
                    types: Vec::new(),
                    variables: Vec::new(),
                    nodes: Vec::new(),
                });
                self.scope_index = new_scope_index;

                let _ = self.next_token();
                self.parse_scope();
                Ok(Some(Node::Scope { index: new_scope_index }))
            }
            TokenKind::Bracket(BracketKind::CloseCurly) => {
                self.scope_index = self.scopes[self.scope_index].parent;
                let _ = self.next_token();
                Ok(None)
            }
            _ => self.parse_single_statement(),
        }
    }
}

// iteration over tokens
impl<'src, 'tokens: 'src> Ast<'src, 'tokens> {
    fn current_token_bounded(
        &self,
        expected: Expected,
    ) -> Result<&'tokens Token<'src>, RawSyntaxError<ErrorKind, ErrorCause>> {
        let Some(token) = self.tokens.get(self.token) else {
            let previous = self.peek_previous_token();
            return Err(RawSyntaxError {
                kind: ErrorKind::Expected(expected),
                cause: ErrorCause::NoMoreTokens,
                col: previous.col,
                len: previous.kind.src_code_len(),
            });
        };

        Ok(token)
    }

    fn next_token(&mut self) -> Option<&'tokens Token<'src>> {
        loop {
            if self.token >= self.tokens.len() - 1 {
                self.token = self.tokens.len();
                break None;
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
    ) -> Result<&'tokens Token<'src>, RawSyntaxError<ErrorKind, ErrorCause>> {
        loop {
            if self.token >= self.tokens.len() - 1 {
                let previous = &self.tokens[self.token];
                self.token = self.tokens.len();
                return Err(RawSyntaxError {
                    kind: ErrorKind::Expected(expected),
                    cause: ErrorCause::NoMoreTokens,
                    col: previous.col,
                    len: previous.kind.src_code_len(),
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
    ) -> Result<(), RawSyntaxError<ErrorKind, ErrorCause>> {
        if let Type::Str = lhs.typ() {
            return Err(RawSyntaxError {
                kind: ErrorKind::Invalid(Statement::Expression),
                cause: ErrorCause::StringLeftOperand,
                col: op_token.col,
                len: op_token.kind.src_code_len(),
            });
        }
        if let Type::Array { .. } = lhs.typ() {
            return Err(RawSyntaxError {
                kind: ErrorKind::Invalid(Statement::Expression),
                cause: ErrorCause::ArrayLeftOperand,
                col: op_token.col,
                len: op_token.kind.src_code_len(),
            });
        }

        Ok(())
    }

    fn assert_rhs_is_not_string_or_array(
        op_token: &'tokens Token<'src>,
        rhs: &Expression<'src>,
    ) -> Result<(), RawSyntaxError<ErrorKind, ErrorCause>> {
        if let Type::Str = rhs.typ() {
            return Err(RawSyntaxError {
                kind: ErrorKind::Invalid(Statement::Expression),
                cause: ErrorCause::StringRightOperand,
                col: op_token.col,
                len: op_token.kind.src_code_len(),
            });
        }
        if let Type::Array { .. } = rhs.typ() {
            return Err(RawSyntaxError {
                kind: ErrorKind::Invalid(Statement::Expression),
                cause: ErrorCause::ArrayRightOperand,
                col: op_token.col,
                len: op_token.kind.src_code_len(),
            });
        }

        Ok(())
    }

    fn operator(
        &mut self,
        ops: &[Op],
    ) -> Result<Option<(&'tokens Token<'src>, Op)>, RawSyntaxError<ErrorKind, ErrorCause>> {
        let current_token = self.current_token_bounded(Expected::OperatorOrSemicolon)?;
        let TokenKind::Op(op) = current_token.kind else {
            return Ok(None);
        };

        if ops.contains(&op) {
            let _ = self.next_token();
            Ok(Some((&current_token, op)))
        } else {
            Ok(None)
        }
    }

    fn index(
        &mut self,
        var_name: &'src str,
        var_typ: Type,
    ) -> Result<Expression<'src>, RawSyntaxError<ErrorKind, ErrorCause>> {
        let current_token = &self.tokens[self.token];

        let Some(open_bracket_token) = self.peek_next_token() else {
            return Ok(Expression::Identifier { typ: var_typ, name: var_name });
        };

        let TokenKind::Bracket(BracketKind::OpenSquare) = open_bracket_token.kind else {
            return Ok(Expression::Identifier { typ: var_typ, name: var_name });
        };

        let _open_square = self.next_token();
        let _start_of_index = self.next_token();

        let index = self.expression()?;
        let Type::Int = index.typ() else {
            return Err(RawSyntaxError {
                kind: ErrorKind::Invalid(Statement::ArrayIndex),
                cause: ErrorCause::MustBeFollowedByIntegerExpression,
                col: open_bracket_token.col,
                len: open_bracket_token.kind.src_code_len(),
            });
        };

        let after_index_token = self.current_token_bounded(Expected::ClosingSquareBracket)?;

        let TokenKind::Bracket(BracketKind::CloseSquare) = after_index_token.kind else {
            let before_index_token = self.peek_previous_token();
            return Err(RawSyntaxError {
                kind: ErrorKind::Invalid(Statement::ArrayIndex),
                cause: ErrorCause::MustBeFollowedByClosingSquareBracket,
                col: before_index_token.col,
                len: before_index_token.kind.src_code_len(),
            });
        };

        match var_typ {
            Type::Array { typ, .. } => match &*typ {
                Type::Int | Type::Bool | Type::Infer | Type::Ascii | Type::Str => {
                    Ok(Expression::ArrayIndex {
                        typ: *typ,
                        var_name,
                        bracket_position: Position::new(self.src, open_bracket_token.col).0,
                        index: Box::new(index),
                    })
                }
                Type::Array { .. } => Err(RawSyntaxError {
                    kind: ErrorKind::Invalid(Statement::ArrayItem),
                    cause: ErrorCause::NestedArrayNotSupportedYet,
                    col: current_token.col,
                    len: current_token.kind.src_code_len(),
                }),
            },
            Type::Str => Ok(Expression::ArrayIndex {
                typ: Type::Ascii,
                var_name,
                bracket_position: Position::new(self.src, open_bracket_token.col).0,
                index: Box::new(index),
            }),
            Type::Int => Ok(Expression::ArrayIndex {
                typ: Type::Int,
                var_name,
                bracket_position: Position::new(self.src, open_bracket_token.col).0,
                index: Box::new(index),
            }),
            Type::Bool | Type::Infer | Type::Ascii => Err(RawSyntaxError {
                kind: ErrorKind::Invalid(Statement::Expression),
                cause: ErrorCause::CannotIndexNonArrayType(var_typ),
                col: current_token.col,
                len: current_token.kind.src_code_len(),
            }),
        }
    }

    fn primary_expression(
        &mut self,
    ) -> Result<Expression<'src>, RawSyntaxError<ErrorKind, ErrorCause>> {
        let current_token = self.current_token_bounded(Expected::Expression)?;
        let factor = match &current_token.kind {
            TokenKind::Literal(literal) => Ok(Expression::Literal(literal.clone())),
            TokenKind::True => Ok(Expression::Literal(Literal::Bool(true))),
            TokenKind::False => Ok(Expression::Literal(Literal::Bool(false))),
            TokenKind::Identifier(name) => match self.resolve_type(name) {
                None => match self.resolve_variable(name) {
                    Some((_, _, var)) => self.index(var.name, var.value.typ()),
                    None => Err(RawSyntaxError {
                        kind: ErrorKind::VariableNotPreviouslyDefined,
                        cause: ErrorCause::WasNotPreviouslyDefined,
                        col: current_token.col,
                        len: current_token.kind.src_code_len(),
                    }),
                },
                Some(_) => Err(RawSyntaxError {
                    kind: ErrorKind::Invalid(Statement::Expression),
                    cause: ErrorCause::CannotBeATypeName,
                    col: current_token.col,
                    len: current_token.kind.src_code_len(),
                }),
            },
            TokenKind::Bracket(BracketKind::OpenRound) => 'parenthesis: {
                let expression_start_token = self.next_token_bounded(Expected::Expression)?;

                if let TokenKind::Bracket(BracketKind::CloseRound) = expression_start_token.kind {
                    break 'parenthesis Err(RawSyntaxError {
                        kind: ErrorKind::Invalid(Statement::Expression),
                        cause: ErrorCause::EmptyExpression,
                        col: expression_start_token.col,
                        len: expression_start_token.kind.src_code_len(),
                    });
                }

                let expression = self.expression()?;
                let close_bracket_token =
                    self.current_token_bounded(Expected::ClosingRoundBracket)?;

                let TokenKind::Bracket(BracketKind::CloseRound) = close_bracket_token.kind else {
                    return Err(RawSyntaxError {
                        kind: ErrorKind::Invalid(Statement::Expression),
                        cause: ErrorCause::UnclosedBracket(BracketKind::OpenRound),
                        col: current_token.col,
                        len: current_token.kind.src_code_len(),
                    });
                };

                Ok(expression)
            }
            TokenKind::Bracket(BracketKind::OpenSquare) => 'array: {
                let _ = self.next_token();
                let mut items = Vec::<Expression<'src>>::new();
                let mut items_typ = Type::Infer;

                loop {
                    let item_token =
                        self.current_token_bounded(Expected::ArrayElementOrClosingSquareBracket)?;

                    if let TokenKind::Bracket(BracketKind::CloseSquare) = item_token.kind {
                        break 'array Ok(Expression::Array { typ: items_typ, items });
                    }

                    let item = self.expression()?;
                    match (&items_typ, item.typ()) {
                        (_, Type::Array { .. }) => {
                            break 'array Err(RawSyntaxError {
                                kind: ErrorKind::Invalid(Statement::ArrayItem),
                                cause: ErrorCause::NestedArrayNotSupportedYet,
                                col: current_token.col,
                                len: current_token.kind.src_code_len(),
                            })
                        }
                        (Type::Infer, other) => {
                            items_typ = other;
                            items.push(item);
                        }
                        (expected, actual) if *expected != actual => {
                            break 'array Err(RawSyntaxError {
                                kind: ErrorKind::Invalid(Statement::ArrayItem),
                                cause: ErrorCause::MismatchedArrayElementType {
                                    expected: expected.clone(),
                                    actual,
                                },
                                col: item_token.col,
                                len: item_token.kind.src_code_len(),
                            })
                        }
                        (_, _) => items.push(item),
                    }

                    let comma_token =
                        self.current_token_bounded(Expected::CommaOrClosingSquareBracket)?;

                    if let TokenKind::Comma = comma_token.kind {
                        let _ = self.next_token();
                    }
                }
            }
            TokenKind::Op(Op::Plus) => {
                // NOTE(stefano): this optimization should be moved to later stages
                while let Some(&Token { kind: TokenKind::Op(Op::Plus), .. }) = self.next_token() {
                    // removing extra plus symbols
                }

                let operand = self.primary_expression()?;

                // returning to avoid the call to tokens.next at the end of the function
                let operand_typ = operand.typ();
                return match operand_typ {
                    Type::Int => Ok(Expression::Unary { op: Op::Plus, operand: Box::new(operand) }),
                    Type::Ascii => Err(RawSyntaxError {
                        kind: ErrorKind::Invalid(Statement::Expression),
                        cause: ErrorCause::CannotTakeAbsValueOfAscii,
                        col: current_token.col,
                        len: current_token.kind.src_code_len(),
                    }),
                    Type::Bool => Err(RawSyntaxError {
                        kind: ErrorKind::Invalid(Statement::Expression),
                        cause: ErrorCause::CannotTakeAbsValueOfBoolean,
                        col: current_token.col,
                        len: current_token.kind.src_code_len(),
                    }),
                    Type::Str => Err(RawSyntaxError {
                        kind: ErrorKind::Invalid(Statement::Expression),
                        cause: ErrorCause::CannotTakeAbsValueOfString,
                        col: current_token.col,
                        len: current_token.kind.src_code_len(),
                    }),
                    Type::Array { .. } => Err(RawSyntaxError {
                        kind: ErrorKind::Invalid(Statement::Expression),
                        cause: ErrorCause::CannotTakeAbsValueOfArray,
                        col: current_token.col,
                        len: current_token.kind.src_code_len(),
                    }),
                    Type::Infer => unreachable!("should have been coerced to a concrete type"),
                };
            }
            TokenKind::Op(Op::Minus) => {
                let mut should_be_negated = true;
                // NOTE(stefano): this optimization should be moved to later stages
                while let Some(&Token { kind: TokenKind::Op(Op::Minus), .. }) = self.next_token() {
                    should_be_negated = !should_be_negated;
                }

                let operand = self.primary_expression()?;

                // returning to avoid the call to tokens.next at the end of the function
                return match operand.typ() {
                    Type::Int | Type::Ascii => {
                        if should_be_negated {
                            Ok(Expression::Unary { op: Op::Minus, operand: Box::new(operand) })
                        } else {
                            Ok(operand)
                        }
                    }
                    Type::Bool => Err(RawSyntaxError {
                        kind: ErrorKind::Invalid(Statement::Expression),
                        cause: ErrorCause::CannotNegateBoolean,
                        col: current_token.col,
                        len: current_token.kind.src_code_len(),
                    }),
                    Type::Str => Err(RawSyntaxError {
                        kind: ErrorKind::Invalid(Statement::Expression),
                        cause: ErrorCause::CannotNegateString,
                        col: current_token.col,
                        len: current_token.kind.src_code_len(),
                    }),
                    Type::Array { .. } => Err(RawSyntaxError {
                        kind: ErrorKind::Invalid(Statement::Expression),
                        cause: ErrorCause::CannotNegateArray,
                        col: current_token.col,
                        len: current_token.kind.src_code_len(),
                    }),
                    Type::Infer => unreachable!("should have been coerced to a concrete type"),
                };
            }
            TokenKind::Op(Op::Not) => {
                let mut should_be_inverted = true;
                // NOTE(stefano): this optimization should be moved to later stages
                while let Some(&Token { kind: TokenKind::Op(Op::Not), .. }) = self.next_token() {
                    should_be_inverted = !should_be_inverted;
                }

                let operand = self.primary_expression()?;
                // returning to avoid the call to tokens.next at the end of the function
                return match operand.typ() {
                    Type::Int | Type::Ascii | Type::Bool => {
                        if should_be_inverted {
                            Ok(Expression::Unary { op: Op::Not, operand: Box::new(operand) })
                        } else {
                            Ok(operand)
                        }
                    }
                    Type::Str => Err(RawSyntaxError {
                        kind: ErrorKind::Invalid(Statement::Expression),
                        cause: ErrorCause::CannotInvertString,
                        col: current_token.col,
                        len: current_token.kind.src_code_len(),
                    }),
                    Type::Array { .. } => Err(RawSyntaxError {
                        kind: ErrorKind::Invalid(Statement::Expression),
                        cause: ErrorCause::CannotInvertArray,
                        col: current_token.col,
                        len: current_token.kind.src_code_len(),
                    }),
                    Type::Infer => unreachable!("should have been coerced to a concrete type"),
                };
            }
            TokenKind::Definition(_)
            | TokenKind::Print
            | TokenKind::PrintLn
            | TokenKind::If
            | TokenKind::Else
            | TokenKind::Loop
            | TokenKind::Break
            | TokenKind::Continue => Err(RawSyntaxError {
                kind: ErrorKind::Invalid(Statement::Expression),
                cause: ErrorCause::KeywordInExpression,
                col: current_token.col,
                len: current_token.kind.src_code_len(),
            }),
            _ => Err(RawSyntaxError {
                kind: ErrorKind::Invalid(Statement::Expression),
                cause: ErrorCause::ExpectedOperand,
                col: current_token.col,
                len: current_token.kind.src_code_len(),
            }),
        };

        let _ = self.next_token();
        factor
    }

    fn exponentiative_expression(
        &mut self,
    ) -> Result<Expression<'src>, RawSyntaxError<ErrorKind, ErrorCause>> {
        let mut lhs = self.primary_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::Pow])? {
            Self::assert_lhs_is_not_string_or_array(op_token, &lhs)?;

            let rhs = self.primary_expression()?;
            Self::assert_rhs_is_not_string_or_array(op_token, &rhs)?;

            lhs = Expression::Binary {
                lhs: Box::new(lhs),
                op_position: Position::new(self.src, op_token.col).0,
                op,
                rhs: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    fn multiplicative_expression(
        &mut self,
    ) -> Result<Expression<'src>, RawSyntaxError<ErrorKind, ErrorCause>> {
        let mut lhs = self.exponentiative_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::Times, Op::Divide, Op::Remainder])? {
            Self::assert_lhs_is_not_string_or_array(op_token, &lhs)?;

            let rhs = self.exponentiative_expression()?;
            Self::assert_rhs_is_not_string_or_array(op_token, &rhs)?;

            lhs = Expression::Binary {
                lhs: Box::new(lhs),
                op_position: Position::new(self.src, op_token.col).0,
                op,
                rhs: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    fn additive_expression(
        &mut self,
    ) -> Result<Expression<'src>, RawSyntaxError<ErrorKind, ErrorCause>> {
        let mut lhs = self.multiplicative_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::Plus, Op::Minus])? {
            Self::assert_lhs_is_not_string_or_array(op_token, &lhs)?;

            let rhs = self.multiplicative_expression()?;
            Self::assert_rhs_is_not_string_or_array(op_token, &rhs)?;

            lhs = Expression::Binary {
                lhs: Box::new(lhs),
                op_position: Position::new(self.src, op_token.col).0,
                op,
                rhs: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    fn shift_expression(
        &mut self,
    ) -> Result<Expression<'src>, RawSyntaxError<ErrorKind, ErrorCause>> {
        let mut lhs = self.additive_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::LeftShift, Op::RightShift])? {
            Self::assert_lhs_is_not_string_or_array(op_token, &lhs)?;

            let rhs = self.additive_expression()?;
            Self::assert_rhs_is_not_string_or_array(op_token, &rhs)?;

            lhs = Expression::Binary {
                lhs: Box::new(lhs),
                op_position: Position::new(self.src, op_token.col).0,
                op,
                rhs: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    fn bitand_expression(
        &mut self,
    ) -> Result<Expression<'src>, RawSyntaxError<ErrorKind, ErrorCause>> {
        let mut lhs = self.shift_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::BitAnd])? {
            Self::assert_lhs_is_not_string_or_array(op_token, &lhs)?;

            let rhs = self.shift_expression()?;
            Self::assert_rhs_is_not_string_or_array(op_token, &rhs)?;

            lhs = Expression::Binary {
                lhs: Box::new(lhs),
                op_position: Position::new(self.src, op_token.col).0,
                op,
                rhs: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    fn bitxor_expression(
        &mut self,
    ) -> Result<Expression<'src>, RawSyntaxError<ErrorKind, ErrorCause>> {
        let mut lhs = self.bitand_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::BitXor])? {
            Self::assert_lhs_is_not_string_or_array(op_token, &lhs)?;

            let rhs = self.bitand_expression()?;
            Self::assert_rhs_is_not_string_or_array(op_token, &rhs)?;

            lhs = Expression::Binary {
                lhs: Box::new(lhs),
                op_position: Position::new(self.src, op_token.col).0,
                op,
                rhs: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    fn bitor_expression(
        &mut self,
    ) -> Result<Expression<'src>, RawSyntaxError<ErrorKind, ErrorCause>> {
        let mut lhs = self.bitxor_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::BitOr])? {
            Self::assert_lhs_is_not_string_or_array(op_token, &lhs)?;

            let rhs = self.bitxor_expression()?;
            Self::assert_rhs_is_not_string_or_array(op_token, &rhs)?;

            lhs = Expression::Binary {
                lhs: Box::new(lhs),
                op_position: Position::new(self.src, op_token.col).0,
                op,
                rhs: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    fn comparison_expression(
        &mut self,
    ) -> Result<Expression<'src>, RawSyntaxError<ErrorKind, ErrorCause>> {
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
            if !lhs_typ.can_be_compared_to(&rhs_typ) {
                return Err(RawSyntaxError {
                    kind: ErrorKind::Invalid(Statement::Expression),
                    cause: ErrorCause::CannotCompareOperands { lhs_typ, rhs_typ },
                    col: op_token.col,
                    len: op_token.kind.src_code_len(),
                });
            }

            if is_chained {
                return Err(RawSyntaxError {
                    kind: ErrorKind::Invalid(Statement::Expression),
                    cause: ErrorCause::ChainedComparison,
                    col: op_token.col,
                    len: op_token.kind.src_code_len(),
                });
            }
            is_chained = true;

            lhs = Expression::Binary {
                lhs: Box::new(lhs),
                op_position: Position::new(self.src, op_token.col).0,
                op,
                rhs: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    fn and_expression(
        &mut self,
    ) -> Result<Expression<'src>, RawSyntaxError<ErrorKind, ErrorCause>> {
        let mut lhs = self.comparison_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::And])? {
            let Type::Bool = lhs.typ() else {
                return Err(RawSyntaxError {
                    kind: ErrorKind::Invalid(Statement::Expression),
                    cause: ErrorCause::NonBooleanLeftOperand,
                    col: op_token.col,
                    len: op_token.kind.src_code_len(),
                });
            };

            let rhs = self.comparison_expression()?;
            let Type::Bool = rhs.typ() else {
                return Err(RawSyntaxError {
                    kind: ErrorKind::Invalid(Statement::Expression),
                    cause: ErrorCause::NonBooleanRightOperand,
                    col: op_token.col,
                    len: op_token.kind.src_code_len(),
                });
            };

            lhs = Expression::Binary {
                lhs: Box::new(lhs),
                op_position: Position::new(self.src, op_token.col).0,
                op,
                rhs: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    fn or_expression(&mut self) -> Result<Expression<'src>, RawSyntaxError<ErrorKind, ErrorCause>> {
        let mut lhs = self.and_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::Or])? {
            let Type::Bool = lhs.typ() else {
                return Err(RawSyntaxError {
                    kind: ErrorKind::Invalid(Statement::Expression),
                    cause: ErrorCause::NonBooleanLeftOperand,
                    col: op_token.col,
                    len: op_token.kind.src_code_len(),
                });
            };

            let rhs = self.and_expression()?;
            let Type::Bool = rhs.typ() else {
                return Err(RawSyntaxError {
                    kind: ErrorKind::Invalid(Statement::Expression),
                    cause: ErrorCause::NonBooleanRightOperand,
                    col: op_token.col,
                    len: op_token.kind.src_code_len(),
                });
            };

            lhs = Expression::Binary {
                lhs: Box::new(lhs),
                op_position: Position::new(self.src, op_token.col).0,
                op,
                rhs: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    // TODO(stefano): disallow implicit conversions
    // TODO(stefano): introduce casting operators
    fn expression(&mut self) -> Result<Expression<'src>, RawSyntaxError<ErrorKind, ErrorCause>> {
        self.or_expression()
    }
}

// variables and types
impl<'src, 'tokens: 'src> Ast<'src, 'tokens> {
    fn resolve_variable(
        &self,
        name: &'src str,
    ) -> Option<(usize /* scope index */, usize /* variable index */, &Variable<'src>)> {
        let mut scope_index = self.scope_index;
        loop {
            let scope = &self.scopes[scope_index];
            for (var_index, var) in scope.variables.iter().enumerate() {
                if var.name == name {
                    return Some((scope_index, var_index, var));
                }
            }

            scope_index = match scope_index {
                0 => return None,
                _ => scope.parent,
            };
        }
    }

    fn resolve_type(&self, name: &'src str) -> Option<&Type> {
        let mut scope_index = self.scope_index;
        loop {
            let scope = &self.scopes[scope_index];
            for typ in &scope.types {
                if typ.to_string() == name {
                    return Some(typ);
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
    ) -> Result<Option<(&'tokens Token<'src>, Type)>, RawSyntaxError<ErrorKind, ErrorCause>> {
        let colon_token = self.next_token_bounded(Expected::TypeAnnotationOrVariableDefinition)?;

        let TokenKind::Colon = colon_token.kind else {
            self.token -= 1;
            return Ok(None);
        };

        let type_token = self.next_token_bounded(Expected::TypeAnnotation)?;
        let TokenKind::Identifier(type_name) = type_token.kind else {
            return Err(RawSyntaxError {
                kind: ErrorKind::Invalid(Statement::TypeAnnotation),
                cause: ErrorCause::ExpectedTypeName,
                col: colon_token.col,
                len: colon_token.kind.src_code_len(),
            });
        };

        let Some(typ) = self.resolve_type(type_name) else {
            return match self.resolve_variable(type_name) {
                Some((_, _, var)) => Ok(Some((type_token, var.value.typ()))),
                None => Err(RawSyntaxError {
                    kind: ErrorKind::Invalid(Statement::TypeAnnotation),
                    cause: ErrorCause::WasNotPreviouslyDefined,
                    col: type_token.col,
                    len: type_token.kind.src_code_len(),
                }),
            };
        };

        let Some(open_square_bracket_token) = self.peek_next_token() else {
            return Ok(Some((type_token, typ.clone())));
        };

        let TokenKind::Bracket(BracketKind::OpenSquare) = open_square_bracket_token.kind else {
            return Ok(Some((type_token, typ.clone())));
        };

        let typ = Box::new(typ.clone());
        let _open_square_bracket = self.next_token();

        let len_token = self.next_token_bounded(Expected::ArrayLength)?;
        let len = match len_token.kind {
            TokenKind::Literal(Literal::Int(len)) => len as uint,
            _ => {
                return Err(RawSyntaxError {
                    kind: ErrorKind::Invalid(Statement::TypeAnnotation),
                    cause: ErrorCause::MustBeFollowedByIntegerExpression,
                    col: open_square_bracket_token.col,
                    len: open_square_bracket_token.kind.src_code_len(),
                })
            }
        };

        match self.next_token() {
            Some(
                close_square_bracket_token @ Token {
                    kind: TokenKind::Bracket(BracketKind::CloseSquare),
                    ..
                },
            ) => Ok(Some((close_square_bracket_token, Type::Array { typ, len }))),
            Some(_) | None => Err(RawSyntaxError {
                kind: ErrorKind::Invalid(Statement::TypeAnnotation),
                cause: ErrorCause::MustBeFollowedByClosingSquareBracket,
                col: open_square_bracket_token.col,
                len: open_square_bracket_token.kind.src_code_len(),
            }),
        }
    }

    fn variable_definition(&mut self) -> Result<Node<'src>, RawSyntaxError<ErrorKind, ErrorCause>> {
        let definition_token = &self.tokens[self.token];
        let TokenKind::Definition(mutability) = definition_token.kind else {
            unreachable!("cannot be anything different from 'let' or 'var'");
        };

        let name_token = self.next_token_bounded(Expected::Identifier)?;
        let name = match name_token.kind {
            TokenKind::Identifier(name) => match self.resolve_type(name) {
                None => Ok(name),
                Some(_) => Err(RawSyntaxError {
                    kind: ErrorKind::Invalid(Statement::VariableName),
                    cause: ErrorCause::CannotBeATypeName,
                    col: name_token.col,
                    len: name_token.kind.src_code_len(),
                }),
            },
            _ => Err(RawSyntaxError {
                kind: ErrorKind::Invalid(Statement::VariableDefinition),
                cause: ErrorCause::ExpectedVariableName,
                col: name_token.col,
                len: name_token.kind.src_code_len(),
            }),
        };
        let name = name?;

        let annotation = self.type_annotation()?;

        let equals_or_semicolon_token = self.next_token_bounded(Expected::EqualsOrSemicolon)?;

        let expression = match equals_or_semicolon_token.kind {
            TokenKind::Op(Op::Equals) => {
                let _ = self.next_token();
                match self.expression() {
                    Ok(expr) => Ok(Some(expr)),
                    Err(err) => Err(err),
                }
            }
            TokenKind::SemiColon => Ok(None),
            _ => match annotation {
                None => Err(RawSyntaxError {
                    kind: ErrorKind::Invalid(Statement::VariableDefinition),
                    cause: ErrorCause::ExpectedEqualsOrSemicolonAfterVariableName,
                    col: name_token.col,
                    len: name_token.kind.src_code_len(),
                }),
                Some((annotation_token, _)) => Err(RawSyntaxError {
                    kind: ErrorKind::Invalid(Statement::VariableDefinition),
                    cause: ErrorCause::ExpectedEqualsOrSemicolonAfterTypeAnnotation,
                    col: annotation_token.col,
                    len: annotation_token.kind.src_code_len(),
                }),
            },
        };
        let expression = expression?;

        if self.resolve_variable(name).is_some() {
            return Err(RawSyntaxError {
                kind: ErrorKind::VariableRedefinition,
                cause: ErrorCause::WasPreviouslyDefined,
                col: name_token.col,
                len: name_token.kind.src_code_len(),
            });
        }

        match expression {
            Some(mut value) => {
                if let Some((token, annotation_typ)) = &annotation {
                    if let Expression::Array { typ, .. } = &mut value {
                        if let Type::Infer = typ {
                            *typ = annotation_typ.inner();
                        }
                    }

                    if *annotation_typ != value.typ() {
                        return Err(RawSyntaxError {
                            kind: ErrorKind::Invalid(Statement::VariableDefinition),
                            cause: ErrorCause::VariableDefinitionTypeMismatch {
                                expected: annotation_typ.clone(),
                                actual: value.typ(),
                            },
                            col: token.col,
                            len: token.kind.src_code_len(),
                        });
                    }
                } else if value.typ().should_be_inferred() {
                    return Err(RawSyntaxError {
                        kind: ErrorKind::Invalid(Statement::VariableDefinition),
                        cause: ErrorCause::ExpectedTypeAnnotation,
                        col: name_token.col,
                        len: name_token.kind.src_code_len(),
                    });
                }

                let variables = &mut self.scopes[self.scope_index].variables;
                variables.push(Variable { mutability, name, value });
                Ok(Node::Definition {
                    scope_index: self.scope_index,
                    var_index: variables.len() - 1,
                })
            }
            None => match annotation {
                Some((_, typ)) => {
                    let variables = &mut self.scopes[self.scope_index].variables;
                    variables.push(Variable { mutability, name, value: typ.into() });
                    Ok(Node::Definition {
                        scope_index: self.scope_index,
                        var_index: variables.len() - 1,
                    })
                }
                None => Err(RawSyntaxError {
                    kind: ErrorKind::Invalid(Statement::VariableDefinition),
                    cause: ErrorCause::ExpectedTypeAnnotationOrValue,
                    col: name_token.col,
                    len: name_token.kind.src_code_len(),
                }),
            },
        }
    }

    fn variable_reassignment(
        &mut self,
    ) -> Result<Node<'src>, RawSyntaxError<ErrorKind, ErrorCause>> {
        let name_token = &self.tokens[self.token];
        let TokenKind::Identifier(name) = name_token.kind else {
            unreachable!("cannot be different from an identifier");
        };

        if self.resolve_type(name).is_some() {
            return Err(RawSyntaxError {
                kind: ErrorKind::Invalid(Statement::VariableAssignment),
                cause: ErrorCause::CannotBeATypeName,
                col: name_token.col,
                len: name_token.kind.src_code_len(),
            });
        }

        // we checked for the presence of an op token before the call to this function
        let op_token = self.next_token().unwrap();

        let _ = self.next_token();
        let rhs = self.expression()?;
        match self.resolve_variable(name) {
            Some((scope_index, var_index, var)) => match var.mutability {
                Mutability::Let => Err(RawSyntaxError {
                    kind: ErrorKind::Invalid(Statement::VariableAssignment),
                    cause: ErrorCause::CannotMutateImmutableVariable,
                    col: name_token.col,
                    len: name_token.kind.src_code_len(),
                }),
                Mutability::Var => {
                    let new_value = match &op_token.kind {
                        TokenKind::Op(Op::Equals) => rhs,
                        TokenKind::Op(op) => Expression::Binary {
                            lhs: Box::new(Expression::Identifier { typ: op.typ(), name }),
                            op_position: Position::new(self.src, op_token.col).0,
                            op: *op,
                            rhs: Box::new(rhs),
                        },
                        _ => unreachable!("cannot be different from an operator"),
                    };

                    if var.value.typ() == new_value.typ() {
                        Ok(Node::Assignment { scope_index, var_index, new_value })
                    } else {
                        Err(RawSyntaxError {
                            kind: ErrorKind::Invalid(Statement::VariableAssignment),
                            cause: ErrorCause::VariableAssignmentTypeMismatch {
                                expected: var.value.typ(),
                                actual: new_value.typ(),
                            },
                            col: name_token.col,
                            len: name_token.kind.src_code_len(),
                        })
                    }
                }
            },
            None => Err(RawSyntaxError {
                kind: ErrorKind::Invalid(Statement::VariableAssignment),
                cause: ErrorCause::WasNotPreviouslyDefined,
                col: name_token.col,
                len: name_token.kind.src_code_len(),
            }),
        }
    }
}

// print statements
impl<'src, 'tokens: 'src> Ast<'src, 'tokens> {
    fn print_arg(&mut self) -> Result<Expression<'src>, RawSyntaxError<ErrorKind, ErrorCause>> {
        let start_of_expression_token = self.next_token_bounded(Expected::Expression)?;
        let argument = self.expression()?;
        let Expression::Array { .. } = argument else {
            return Ok(argument);
        };

        Err(RawSyntaxError {
            kind: ErrorKind::Invalid(Statement::Expression),
            cause: ErrorCause::TemporaryArrayNotSupportedYet,
            col: start_of_expression_token.col,
            len: start_of_expression_token.kind.src_code_len(),
        })
    }
}

// if statements
impl<'src, 'tokens: 'src> Ast<'src, 'tokens> {
    fn iff(&mut self) -> Result<Node<'src>, RawSyntaxError<ErrorKind, ErrorCause>> {
        let mut if_statement = If { ifs: Vec::new(), els: None };

        'iff: while let Some(if_token) = self.tokens.get(self.token) {
            let _ = self.next_token_bounded(Expected::BooleanExpression)?;

            let expression = self.expression()?;
            let condition = match &expression.typ() {
                Type::Bool => Ok(expression),
                Type::Ascii | Type::Int | Type::Str | Type::Array { .. } | Type::Infer => {
                    Err(RawSyntaxError {
                        kind: ErrorKind::Invalid(Statement::If),
                        cause: ErrorCause::MustBeFollowedByABooleanExpression,
                        col: if_token.col,
                        len: if_token.kind.src_code_len(),
                    })
                }
            };

            let condition = condition?;
            let after_condition_token = self.current_token_bounded(Expected::DoOrBlock)?;
            let iff = match after_condition_token.kind {
                TokenKind::Bracket(BracketKind::OpenCurly) => {
                    let scope = self.parse_single_any()?.unwrap();
                    Ok(IfStatement { condition, statement: scope })
                }
                TokenKind::Do => {
                    let statement = self.parse_do_statement()?.unwrap();
                    self.semicolon()?;
                    Ok(IfStatement { condition, statement })
                }
                _ => {
                    let before_curly_bracket_token = self.peek_previous_token();
                    Err(RawSyntaxError {
                        kind: ErrorKind::Invalid(Statement::If),
                        cause: ErrorCause::MustBeFollowedByDoOrBlock,
                        col: before_curly_bracket_token.col,
                        len: before_curly_bracket_token.kind.src_code_len(),
                    })
                }
            };

            if_statement.ifs.push(iff?);

            while let Some(else_token) = self.tokens.get(self.token) {
                let after_else_token = match else_token.kind {
                    TokenKind::Else => self.next_token_bounded(Expected::DoOrBlockOrIfStatement)?,
                    _ => break 'iff,
                };

                // we are now inside an else branch
                let else_if = match after_else_token.kind {
                    TokenKind::Bracket(BracketKind::OpenCurly) => {
                        let scope = self.parse_single_any()?.unwrap();
                        if_statement.els = Some(Box::new(scope));
                        break 'iff;
                    }
                    TokenKind::Do => {
                        let statement = self.parse_do_statement()?.unwrap();
                        self.semicolon()?;
                        if_statement.els = Some(Box::new(statement));
                        break 'iff;
                    }
                    TokenKind::If => break,
                    _ => Err(RawSyntaxError {
                        kind: ErrorKind::Invalid(Statement::If),
                        cause: ErrorCause::MustBeFollowedByDoOrBlockOrIfStatement,
                        col: else_token.col,
                        len: else_token.kind.src_code_len(),
                    }),
                };

                else_if?;
            }
        }

        Ok(Node::If(if_statement))
    }
}

// for statements
impl<'src, 'tokens: 'src> Ast<'src, 'tokens> {
    fn loop_statement(&mut self) -> Result<Node<'src>, RawSyntaxError<ErrorKind, ErrorCause>> {
        let do_token = &self.tokens[self.token];
        let loop_token = match do_token.kind {
            TokenKind::Do => self.next_token_bounded(Expected::LoopStatement)?,
            _ => do_token,
        };

        let _ = self.next_token_bounded(Expected::BooleanExpression)?;
        let expression = self.expression()?;
        let condition = match &expression.typ() {
            Type::Bool => Ok(expression),
            Type::Ascii | Type::Int | Type::Str | Type::Array { .. } | Type::Infer => {
                Err(RawSyntaxError {
                    kind: ErrorKind::Invalid(Statement::Loop),
                    cause: ErrorCause::MustBeFollowedByABooleanExpression,
                    col: loop_token.col,
                    len: loop_token.kind.src_code_len(),
                })
            }
        };

        let after_condition_token = self.current_token_bounded(Expected::DoOrBlock)?;
        let statement = match after_condition_token.kind {
            TokenKind::Bracket(BracketKind::OpenCurly) => {
                let scope = self.parse_single_any()?.unwrap();
                Ok(scope)
            }
            TokenKind::Do => {
                let statement = self.parse_do_statement()?.unwrap();
                self.semicolon()?;
                Ok(statement)
            }
            _ => {
                let before_curly_bracket_token = self.peek_previous_token();
                Err(RawSyntaxError {
                    kind: ErrorKind::Invalid(Statement::Loop),
                    cause: ErrorCause::MustBeFollowedByDoOrBlock,
                    col: before_curly_bracket_token.col,
                    len: before_curly_bracket_token.kind.src_code_len(),
                })
            }
        };

        let condition = condition?;
        let statement = statement?;
        let condition = if let TokenKind::Do = do_token.kind {
            LoopCondition::DoLoop(condition)
        } else {
            LoopCondition::Loop(condition)
        };

        Ok(Node::Loop(Loop { condition, statement: Box::new(statement) }))
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
        match self {
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
        }
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
    ArrayIndex,
    ArrayItem,
    VariableName,
    VariableDefinition,
    VariableAssignment,
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
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
            Self::ArrayIndex => write!(f, "array index"),
            Self::ArrayItem => write!(f, "array item"),
            Self::VariableName => write!(f, "variable name"),
            Self::VariableDefinition => write!(f, "variable definition"),
            Self::VariableAssignment => write!(f, "variable assignment"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    Expected(Expected),
    Invalid(Statement),
    VariableNotPreviouslyDefined,
    VariableRedefinition,
}

impl SyntaxErrorKind for ErrorKind {}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expected(expected) => write!(f, "expected {expected}"),
            Self::Invalid(invalid) => write!(f, "invalid {invalid}"),
            Self::VariableNotPreviouslyDefined => write!(f, "variable not previously defined"),
            Self::VariableRedefinition => write!(f, "variable redefinition"),
        }
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
    CannotIndexNonArrayType(Type),
    EmptyExpression,
    UnclosedBracket(BracketKind),
    MismatchedArrayElementType { expected: Type, actual: Type },

    WasNotPreviouslyDefined,
    WasPreviouslyDefined,

    CannotBeATypeName,
    CannotNegateBoolean,
    CannotNegateString,
    CannotNegateArray,
    CannotInvertString,
    CannotInvertArray,
    CannotTakeAbsValueOfAscii,
    CannotTakeAbsValueOfBoolean,
    CannotTakeAbsValueOfString,
    CannotTakeAbsValueOfArray,

    KeywordInExpression,
    ChainedComparison,
    CannotCompareOperands { lhs_typ: Type, rhs_typ: Type },
    CannotMutateImmutableVariable,
    VariableDefinitionTypeMismatch { expected: Type, actual: Type },
    VariableAssignmentTypeMismatch { expected: Type, actual: Type },

    MustBeFollowedByABooleanExpression,
    MustBeFollowedByDoOrBlock,
    MustBeFollowedByDoOrBlockOrIfStatement,
    MustBeFollowedByIntegerExpression,
    MustBeFollowedByClosingSquareBracket,

    ExpectedOperand,
    ExpectedTypeName,
    ExpectedTypeAnnotation,
    ExpectedTypeAnnotationOrValue,
    ExpectedVariableName,
    ExpectedEqualsOrSemicolonAfterVariableName,
    ExpectedEqualsOrSemicolonAfterTypeAnnotation,
}

impl SyntaxErrorCause for ErrorCause {}

impl Display for ErrorCause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
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
            Self::CannotNegateBoolean => {
                write!(
                    f,
                    "cannot negate a boolean value, use the '!' operator instead to invert it"
                )
            }
            Self::CannotNegateString => write!(f, "cannot negate a string"),
            Self::CannotNegateArray => write!(f, "cannot negate an array"),
            Self::CannotInvertString => write!(f, "cannot invert a string"),
            Self::CannotInvertArray => write!(f, "cannot invert an array"),
            Self::CannotTakeAbsValueOfAscii => {
                write!(f, "cannot take the absolute of an ascii character")
            }
            Self::CannotTakeAbsValueOfBoolean => write!(f, "cannot take the absolute of a boolean"),
            Self::CannotTakeAbsValueOfString => write!(f, "cannot take the absolute of a string"),
            Self::CannotTakeAbsValueOfArray => write!(f, "cannot take the absolute of an array"),
            Self::KeywordInExpression => write!(f, "cannot be a keyword"),
            Self::ExpectedOperand => write!(f, "expected expression operand before this token"),
            Self::ChainedComparison => write!(f, "comparison operators cannot be chained"),
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
        }
    }
}
