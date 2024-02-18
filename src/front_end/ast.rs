use super::tokenizer::{BracketKind, Len, Literal, Mutability, Op, Token, TokenKind};
use crate::{
    color::{Bg, Colored, Fg, Flag},
    logging::{AT, BAR, ERROR},
    src_file::{Position, SrcFile},
};
use std::{
    borrow::Cow,
    fmt::{Debug, Display},
    path::Path,
};

// TODO(stefano): inline this struct in the AST struct
#[derive(Debug)]
struct Tokens<'src: 'tokens, 'tokens> {
    src: &'src SrcFile,
    token: usize,
    tokens: &'tokens [Token<'src>],
    // TODO(stefano): try removing this option
    current: Option<&'tokens Token<'src>>,
}

impl<'src: 'tokens, 'tokens> Tokens<'src, 'tokens> {
    const fn new(src: &'src SrcFile, tokens: &'tokens [Token<'src>]) -> Self {
        let mut token = 0;

        // skipping to the first non-comment token
        let current = loop {
            if token >= tokens.len() {
                break None;
            }

            let current = &tokens[token];
            match current.kind {
                TokenKind::Comment(_) => token += 1,
                _ => break Some(current),
            }
        };

        Self { src, token, tokens, current }
    }
}

impl<'src: 'tokens, 'tokens> Tokens<'src, 'tokens> {
    fn next(&mut self) -> Option<&'tokens Token<'src>> {
        self.current = loop {
            if self.token >= self.tokens.len() - 1 {
                self.token = self.tokens.len();
                break None;
            }

            self.token += 1;
            let next = &self.tokens[self.token];
            let TokenKind::Comment(_) = next.kind else {
                break Some(next);
            };
        };

        self.current
    }

    const fn peek_next(&self) -> Option<&'tokens Token<'src>> {
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

    const fn peek_previous(&self) -> &'tokens Token<'src> {
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

trait Bounded<'src: 'tokens, 'tokens> {
    type Error;

    fn bounded(
        self,
        tokens: &mut Tokens<'src, 'tokens>,
        expected_before_eof: ExpectedBeforeEof,
    ) -> Result<&'tokens Token<'src>, Self::Error>;
}

impl<'src: 'tokens, 'tokens> Bounded<'src, 'tokens> for Option<&'tokens Token<'src>> {
    type Error = Error<'src>;

    fn bounded(
        self,
        tokens: &mut Tokens<'src, 'tokens>,
        expected_before_eof: ExpectedBeforeEof,
    ) -> Result<&'tokens Token<'src>, Self::Error> {
        let Some(token) = self else {
            let previous = tokens.peek_previous();
            return Err(Error::new(
                tokens.src,
                previous.col,
                previous.kind.len(),
                ErrorKind::NoMoreTokens(expected_before_eof),
            ));
        };

        Ok(token)
    }
}

pub(crate) trait TypeOf {
    fn typ(&self) -> Type;
}

#[derive(Debug, Clone)]
pub enum Type {
    Infer,

    Int,
    Char,
    Bool,
    Str,
    Array(usize, Box<Type>),
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Infer, Self::Infer)
            | (Self::Int, Self::Int)
            | (Self::Char, Self::Char)
            | (Self::Bool, Self::Bool)
            | (Self::Str, Self::Str) => true,

            (Self::Array(array_len_1, array_typ_1), Self::Array(array_len_2, array_typ_2)) => {
                array_len_1 == array_len_2 && array_typ_1 == array_typ_2
            }

            _ => false,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int => write!(f, "int"),
            Self::Char => write!(f, "char"),
            Self::Bool => write!(f, "bool"),
            Self::Str => write!(f, "str"),
            Self::Array(len, typ) => write!(f, "{typ}[{len}]"),

            Self::Infer => write!(f, "infer"),
        }
    }
}

impl Type {
    pub(crate) fn size(&self) -> usize {
        match self {
            Self::Int => core::mem::size_of::<isize>(),
            Self::Char => core::mem::size_of::<u8>(),
            Self::Bool => core::mem::size_of::<bool>(),
            Self::Str => core::mem::size_of::<*const u8>() + core::mem::size_of::<usize>(),
            Self::Array(len, typ) => typ.size() * len,

            Self::Infer => unreachable!("should have been coerced to a concrete type"),
        }
    }

    pub(crate) fn should_be_inferred(&self) -> bool {
        match self {
            Self::Infer => true,
            Self::Array(_, typ) => typ.should_be_inferred(),
            Self::Int | Self::Char | Self::Bool | Self::Str => false,
        }
    }

    pub(crate) fn inner(&self) -> Self {
        match self {
            Self::Array(_, typ) => typ.inner(),
            _ => self.clone(),
        }
    }
}

impl TypeOf for Literal {
    fn typ(&self) -> Type {
        match self {
            Self::Int(_) => Type::Int,
            Self::Char(_) => Type::Char,
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
    Unary { op: Op, operand: Box<Expression<'src>> },
    Binary { lhs: Box<Expression<'src>>, op_position: Position, op: Op, rhs: Box<Expression<'src>> },
    Identifier(&'src str, Type),
    Array(Vec<Expression<'src>>, Type),

    // TODO(stefano): hard-code the length of the array that this index refers to
    ArrayIndex { array: &'src str, typ: Type, bracket_position: Position, index: Box<Expression<'src>> },
}

impl Debug for Expression<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(literal) => write!(f, "{literal}"),
            Self::Unary { op, operand } => write!(f, "{op}{operand:?}"),
            Self::Binary { lhs, op, rhs, .. } => write!(f, "({lhs:?} {op} {rhs:?})"),
            Self::Identifier(name, _) => write!(f, "{name}"),
            Self::Array(array, _) => {
                write!(f, "[")?;
                if !array.is_empty() {
                    let mut elements = array.iter();
                    let last = elements.next_back().unwrap(); // we have already checked for a non empty array
                    for element in elements {
                        write!(f, "{element:?}, ")?;
                    }

                    write!(f, "{last:?}")?;
                }
                write!(f, "]")
            }
            Self::ArrayIndex { array, index, .. } => write!(f, "{array}[{index:?}]"),
        }
    }
}

impl TypeOf for Expression<'_> {
    fn typ(&self) -> Type {
        match self {
            Self::Literal(literal) => literal.typ(),
            Self::Unary { operand, .. } => operand.typ(),
            Self::Binary { op, .. } => op.typ(),
            Self::Identifier(_, typ) => typ.clone(),
            Self::Array(array, typ) => Type::Array(array.len(), Box::new(typ.clone())),
            Self::ArrayIndex { typ, .. } => typ.clone(),
        }
    }
}

impl From<Type> for Expression<'_> {
    fn from(typ: Type) -> Self {
        match typ {
            Type::Bool => Self::Literal(Literal::Bool(false)),
            Type::Char => Self::Literal(Literal::Char(0)),
            Type::Int => Self::Literal(Literal::Int(0)),
            Type::Str => Self::Literal(Literal::Str(Vec::new())),
            Type::Array(len, typ) => {
                let mut array = Vec::<Expression<'_>>::with_capacity(len);
                for _ in 1..len {
                    let inner_typ = *typ.clone();
                    array.push(inner_typ.into());
                }
                Self::Array(array, *typ)
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
        write!(f, "if {condition:?}", condition = self.condition)
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
    Pre(Expression<'src>),
    Post(Expression<'src>),
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
            LoopCondition::Pre(condition) => write!(f, "loop {condition:?}"),
            LoopCondition::Post(condition) => write!(f, "do loop {condition:?}"),
        }
    }
}

// TODO(stefano): have each node and sub-node be made of a reference to the token/s from which the got generated
#[derive(Debug, Clone)]
pub(crate) enum Node<'src> {
    Semicolon,

    Expression(Expression<'src>),

    Print(Expression<'src>),
    Println(Option<Expression<'src>>),

    If(If<'src>),

    Loop(Loop<'src>),
    Break,
    Continue,

    Definition(usize /* scope idx */, usize /* variable idx */),
    Assignment(usize /* scope idx */, usize /* variable idx */, Expression<'src>),

    Scope(usize),
}

impl Display for Node<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Semicolon => write!(f, ";"),
            Self::Expression(expression) => write!(f, "{expression:?}"),
            Self::Print(argument) => write!(f, "print {argument:?}"),
            Self::Println(Some(arg)) => write!(f, "println {arg:?}"),
            Self::Println(None) => write!(f, "println"),
            Self::If(iff) => write!(f, "{iff}", iff = iff.ifs[0]),
            Self::Loop(looop) => write!(f, "{looop}"),
            Self::Break => write!(f, "break"),
            Self::Continue => write!(f, "continue"),

            Self::Definition(_, _) | Self::Assignment(_, _, _) | Self::Scope(_) => {
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
pub struct Ast<'src: 'tokens, 'tokens> {
    scopes: Vec<Scope<'src>>,
    scope: usize,
    loop_depth: usize,

    tokens: Tokens<'src, 'tokens>,
    errors: Vec<Error<'src>>,
}

impl<'src: 'tokens, 'tokens> Ast<'src, 'tokens> {
    pub fn build(src: &'src SrcFile, tokens: &'tokens [Token<'src>]) -> Result<Vec<Scope<'src>>, Vec<Error<'src>>> {
        if tokens.is_empty() {
            return Ok(Vec::new());
        }

        let mut this = Self {
            scopes: vec![Scope {
                parent: 0,
                types: vec![Type::Int, Type::Char, Type::Bool, Type::Str],
                variables: Vec::new(),
                nodes: Vec::new(),
            }],
            scope: 0,
            tokens: Tokens::new(src, tokens),
            loop_depth: 0,
            errors: Vec::new(),
        };

        this.parse_scope();

        if this.errors.is_empty() {
            Ok(this.scopes)
        } else {
            this.errors.sort_by(|e1, e2| e1.position.line.cmp(&e2.position.line));
            Err(this.errors)
        }
    }
}

// parsing of nodes
impl<'src: 'tokens, 'tokens> Ast<'src, 'tokens> {
    fn parse_scope(&mut self) {
        loop {
            match self.parse_single_any() {
                Ok(Some(node)) => {
                    match node {
                        // skip to the next token after a semicolon
                        Node::Semicolon => continue,

                        // check to see if a terminating semicolon is present
                        Node::Definition(_, _)
                        | Node::Assignment(_, _, _)
                        | Node::Expression(_)
                        | Node::Break
                        | Node::Continue
                        | Node::Print(_)
                        | Node::Println(_) => {
                            if let Err(err) = self.semicolon() {
                                self.errors.push(err);

                                // consuming all remaining tokens until the end of the file
                                self.tokens.token = self.tokens.tokens.len();
                                break;
                            }
                        }

                        // no need to check for a terminating semicolon
                        Node::If(_) | Node::Loop(_) | Node::Scope(_) => {}
                    }

                    self.scopes[self.scope].nodes.push(node);
                }
                Ok(None) => break,
                // NOTE(stefano):only parsing until the first error until a fault tolerant parser is developed,
                // this is because the first truly relevant error is the first one, which in turn
                // causes a ripple effect that propagates to the rest of the parsing, causing
                // subsequent errors to be wrong
                Err(err) => {
                    self.errors.push(err);

                    // consuming all remaining tokens until the end of the file
                    self.tokens.token = self.tokens.tokens.len();
                    break;
                }
            }
        }
    }

    fn parse_single_statement(&mut self) -> Result<Option<Node<'src>>, Error<'src>> {
        let Some(current_token) = self.tokens.current else { return Ok(None) };

        match current_token.kind {
            TokenKind::Literal(_)
            | TokenKind::True
            | TokenKind::False
            | TokenKind::Bracket(BracketKind::OpenRound)
            | TokenKind::Op(Op::Minus | Op::Not) => Ok(Some(Node::Expression(self.expression()?))),
            TokenKind::Identifier(_) => match self.tokens.peek_next() {
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
            TokenKind::Print | TokenKind::PrintLn => Ok(Some(self.print()?)),
            TokenKind::If => Ok(Some(self.iff()?)),
            TokenKind::Else => {
                let _ = self.tokens.next();
                Err(Error::new(self.tokens.src, current_token.col, current_token.kind.len(), ErrorKind::StrayElseBlock))
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
                let _ = self.tokens.next();
                match self.loop_depth {
                    0 => Err(Error::new(
                        self.tokens.src,
                        current_token.col,
                        current_token.kind.len(),
                        ErrorKind::BreakOutsideOfLoop,
                    )),
                    _ => Ok(Some(Node::Break)),
                }
            }
            TokenKind::Continue => {
                let _ = self.tokens.next();
                match self.loop_depth {
                    0 => Err(Error::new(
                        self.tokens.src,
                        current_token.col,
                        current_token.kind.len(),
                        ErrorKind::ContinueOutsideOfLoop,
                    )),
                    _ => Ok(Some(Node::Continue)),
                }
            }
            TokenKind::Bracket(BracketKind::OpenCurly) => {
                todo!("// TODO(stefano): check if we can ever reach this branch");

                // let _ = self.tokens.next();
                // Err(Error::new(
                //     self.tokens.src,
                //     current_token.col,
                //     current_token.kind.len(),
                //     ErrorKind::BlocksNotAllowed(BlocksNotAllowedIn::DoStatement),
                // ))
            }
            TokenKind::Bracket(BracketKind::CloseCurly) => {
                todo!("// TODO(stefano): check if we can ever reach this branch");

                // let _ = self.tokens.next();
                // Err(Error::new(
                //     self.tokens.src,
                //     current_token.col,
                //     current_token.kind.len(),
                //     ErrorKind::UnopenedBracket(BracketKind::CloseCurly),
                // ))
            }
            TokenKind::Bracket(BracketKind::OpenSquare) => {
                let _ = self.expression()?;
                Err(Error::new(
                    self.tokens.src,
                    current_token.col,
                    current_token.kind.len(),
                    ErrorKind::TemporaryArrayNotSupportedYet,
                ))
            }
            TokenKind::Bracket(BracketKind::CloseSquare) => {
                todo!("// TODO(stefano): check if we can ever reach this branch");

                // let _ = self.tokens.next();
                // Err(Error::new(
                //     self.tokens.src,
                //     current_token.col,
                //     current_token.kind.len(),
                //     ErrorKind::UnopenedBracket(BracketKind::CloseSquare),
                // ))
            }
            TokenKind::Bracket(BracketKind::CloseRound) => {
                todo!("// TODO(stefano): check if we can ever reach this branch");

                // let _ = self.tokens.next();
                // Err(Error::new(
                //     self.tokens.src,
                //     current_token.col,
                //     current_token.kind.len(),
                //     ErrorKind::UnopenedBracket(BracketKind::CloseRound),
                // ))
            }
            TokenKind::Colon => {
                let _ = self.tokens.next();
                Err(Error::new(self.tokens.src, current_token.col, current_token.kind.len(), ErrorKind::StrayColon))
            }
            TokenKind::Comma => {
                let _ = self.tokens.next();
                Err(Error::new(self.tokens.src, current_token.col, current_token.kind.len(), ErrorKind::StrayComma))
            }
            TokenKind::Op(Op::Equals) => {
                let _ = self.tokens.next();
                Err(Error::new(self.tokens.src, current_token.col, current_token.kind.len(), ErrorKind::StrayEquals))
            }
            TokenKind::Op(op) => {
                let _ = self.tokens.next();
                Err(Error::new(
                    self.tokens.src,
                    current_token.col,
                    current_token.kind.len(),
                    ErrorKind::StrayBinaryOperator(op),
                ))
            }
            // TODO(stefano): move this branch up top
            TokenKind::SemiColon => {
                let _ = self.tokens.next();
                Ok(Some(Node::Semicolon))
            }
            TokenKind::Comment(_) => unreachable!("should be skipped by the token iterator"),
            TokenKind::Unexpected(_) => unreachable!("only valid tokens should be present"),
        }
    }

    fn parse_do_single_statement(&mut self) -> Result<Option<Node<'src>>, Error<'src>> {
        let current_token = self.tokens.next().bounded(&mut self.tokens, ExpectedBeforeEof::StatementAfterDo)?;
        match current_token.kind {
            TokenKind::Bracket(BracketKind::OpenCurly) => {
                let _ = self.tokens.next();
                Err(Error::new(
                    self.tokens.src,
                    current_token.col,
                    current_token.kind.len(),
                    ErrorKind::BlocksNotAllowed(BlocksNotAllowedIn::DoStatement),
                ))
            }
            TokenKind::Definition(_) => {
                let _ = self.tokens.next();
                Err(Error::new(
                    self.tokens.src,
                    current_token.col,
                    current_token.kind.len(),
                    ErrorKind::VariableDefinitionNotAllowed(VariableDefinitionNotAllowedIn::DoStatement),
                ))
            }
            _ => self.parse_single_statement(),
        }
    }

    fn parse_single_any(&mut self) -> Result<Option<Node<'src>>, Error<'src>> {
        let Some(current_token) = self.tokens.current else { return Ok(None) };

        match current_token.kind {
            TokenKind::Bracket(BracketKind::OpenCurly) => {
                let new_scope = self.scopes.len();
                self.scopes.push(Scope {
                    parent: self.scope,
                    types: Vec::new(),
                    variables: Vec::new(),
                    nodes: Vec::new(),
                });
                self.scope = new_scope;

                let _ = self.tokens.next();
                self.parse_scope();
                Ok(Some(Node::Scope(new_scope)))
            }
            TokenKind::Bracket(BracketKind::CloseCurly) => {
                self.scope = self.scopes[self.scope].parent;
                let _ = self.tokens.next();
                Ok(None)
            }
            _ => self.parse_single_statement(),
        }
    }
}

// semicolons
impl<'src: 'tokens, 'tokens> Ast<'src, 'tokens> {
    fn semicolon(&mut self) -> Result<(), Error<'src>> {
        let semicolon_token = self.tokens.current.bounded(&mut self.tokens, ExpectedBeforeEof::Semicolon)?;
        match &semicolon_token.kind {
            TokenKind::SemiColon => {
                let _ = self.tokens.next();
                Ok(())
            }
            _ => {
                let previous_token = self.tokens.peek_previous();
                Err(Error::new(
                    self.tokens.src,
                    previous_token.col,
                    previous_token.kind.len(),
                    ErrorKind::MissingSemicolon,
                ))
            }
        }
    }
}

// expressions
impl<'src: 'tokens, 'tokens> Ast<'src, 'tokens> {
    fn operator(&mut self, ops: &[Op]) -> Result<Option<(&'tokens Token<'src>, Op)>, Error<'src>> {
        let current_token = self.tokens.current.bounded(&mut self.tokens, ExpectedBeforeEof::OperatorOrSemicolon)?;
        match current_token.kind {
            TokenKind::Op(op) => {
                if ops.contains(&op) {
                    let _ = self.tokens.next();
                    Ok(Some((&current_token, op)))
                } else {
                    Ok(None)
                }
            }
            _ => Ok(None),
        }
    }

    fn primary_expression(&mut self) -> Result<Expression<'src>, Error<'src>> {
        let current_token = self.tokens.current.bounded(&mut self.tokens, ExpectedBeforeEof::Expression)?;
        let factor = match &current_token.kind {
            TokenKind::Literal(literal) => Ok(Expression::Literal(literal.clone())),
            TokenKind::True => Ok(Expression::Literal(Literal::Bool(true))),
            TokenKind::False => Ok(Expression::Literal(Literal::Bool(false))),
            TokenKind::Identifier(name) => match self.resolve_type(name) {
                None => match self.resolve_variable(name) {
                    Some((_, _, var)) => match self.tokens.peek_next() {
                        Some(open_bracket_token @ Token { kind: TokenKind::Bracket(BracketKind::OpenSquare), .. }) => {
                            let array = var.name;
                            let array_typ = var.value.typ().inner();

                            let _open_square = self.tokens.next();
                            let _start_of_index = self.tokens.next();

                            let index = self.expression()?;
                            match index.typ() {
                                Type::Int => {
                                    let after_index_token = self
                                        .tokens
                                        .current
                                        .bounded(&mut self.tokens, ExpectedBeforeEof::ClosingSquareBracket)?;

                                    match after_index_token.kind {
                                        TokenKind::Bracket(BracketKind::CloseSquare) => Ok(Expression::ArrayIndex {
                                            array,
                                            typ: array_typ,
                                            bracket_position: self.tokens.src.position(open_bracket_token.col),
                                            index: Box::new(index),
                                        }),
                                        _ => {
                                            let before_index_token = self.tokens.peek_previous();
                                            Err(Error::new(
                                                self.tokens.src,
                                                before_index_token.col,
                                                before_index_token.kind.len(),
                                                ErrorKind::MissingSquareBracketInArrayIndex,
                                            ))
                                        }
                                    }
                                }
                                Type::Array(_, _) | Type::Bool | Type::Infer | Type::Char | Type::Str => {
                                    Err(Error::new(
                                        self.tokens.src,
                                        open_bracket_token.col,
                                        open_bracket_token.kind.len(),
                                        ErrorKind::ExpectedIntegerExpressionInArrayIndex,
                                    ))
                                }
                            }
                        }
                        Some(_) | None => Ok(Expression::Identifier(var.name, var.value.typ())),
                    },
                    None => Err(Error::new(
                        self.tokens.src,
                        current_token.col,
                        current_token.kind.len(),
                        ErrorKind::VariableNotPreviouslyDefined,
                    )),
                },
                Some(_) => Err(Error::new(
                    self.tokens.src,
                    current_token.col,
                    current_token.kind.len(),
                    ErrorKind::TypeNameInExpressions,
                )),
            },
            TokenKind::Bracket(open_bracket_kind @ BracketKind::OpenRound) => 'parenthesis: {
                let expression_start_token =
                    self.tokens.next().bounded(&mut self.tokens, ExpectedBeforeEof::Expression)?;
                if let TokenKind::Bracket(BracketKind::CloseRound) = expression_start_token.kind {
                    break 'parenthesis Err(Error::new(
                        self.tokens.src,
                        expression_start_token.col,
                        expression_start_token.kind.len(),
                        ErrorKind::EmptyExpression,
                    ));
                }

                let expression = self.expression()?;
                let close_bracket_token =
                    self.tokens.current.bounded(&mut self.tokens, ExpectedBeforeEof::ClosingRoundBracket)?;
                match close_bracket_token.kind {
                    TokenKind::Bracket(BracketKind::CloseRound) => Ok(expression),
                    _ => Err(Error::new(
                        self.tokens.src,
                        current_token.col,
                        current_token.kind.len(),
                        ErrorKind::UnclosedBracket(*open_bracket_kind),
                    )),
                }
            }
            TokenKind::Bracket(BracketKind::OpenSquare) => 'array: {
                let _ = self.tokens.next();
                let mut array = Vec::<Expression<'src>>::new();
                let mut array_typ = Type::Infer;

                loop {
                    let element_token = self
                        .tokens
                        .current
                        .bounded(&mut self.tokens, ExpectedBeforeEof::ArrayElementOrClosingSquareBracket)?;

                    if let TokenKind::Bracket(BracketKind::CloseSquare) = element_token.kind {
                        break 'array Ok(Expression::Array(array, array_typ));
                    }

                    let element = self.expression()?;
                    match (&array_typ, element.typ()) {
                        (_, Type::Array(_, _)) => {
                            break 'array Err(Error::new(
                                self.tokens.src,
                                element_token.col,
                                element_token.kind.len(),
                                ErrorKind::NestedArrayNotSupportedYet,
                            ))
                        }
                        (Type::Infer, other) => {
                            array_typ = other;
                            array.push(element);
                        }
                        (expected, actual) if *expected != actual => {
                            break 'array Err(Error::new(
                                self.tokens.src,
                                element_token.col,
                                element_token.kind.len(),
                                ErrorKind::MismatchedArrayElementType { expected: expected.clone(), actual },
                            ))
                        }
                        (_, _) => array.push(element),
                    }

                    let comma_token = self
                        .tokens
                        .current
                        .bounded(&mut self.tokens, ExpectedBeforeEof::CommaOrClosingSquareBracket)?;
                    if let TokenKind::Comma = comma_token.kind {
                        let _ = self.tokens.next();
                    }
                }
            }
            TokenKind::Op(Op::Minus) => {
                let mut sign: isize = -1;
                // NOTE(stefano): this optimization should be moved to later stages
                while let Some(&Token { kind: TokenKind::Op(Op::Minus), .. }) = self.tokens.next() {
                    sign *= -1;
                }

                let operand = self.primary_expression()?;

                // returning to avoid the call to tokens.next at the end of the function
                return match operand.typ() {
                    Type::Int | Type::Char => {
                        if sign < 0 {
                            Ok(Expression::Unary { op: Op::Minus, operand: Box::new(operand) })
                        } else {
                            Ok(operand)
                        }
                    }
                    Type::Bool => Err(Error::new(
                        self.tokens.src,
                        current_token.col,
                        current_token.kind.len(),
                        ErrorKind::CannotNegateBoolean,
                    )),
                    Type::Str => Err(Error::new(
                        self.tokens.src,
                        current_token.col,
                        current_token.kind.len(),
                        ErrorKind::CannotNegateString,
                    )),
                    Type::Array(_, _) => Err(Error::new(
                        self.tokens.src,
                        current_token.col,
                        current_token.kind.len(),
                        ErrorKind::CannotNegateArray,
                    )),
                    Type::Infer => unreachable!("should have been coerced to a concrete type"),
                };
            }
            TokenKind::Op(Op::Not) => {
                let mut should_be_inverted = true;
                // NOTE(stefano): this optimization should be moved to later stages
                while let Some(&Token { kind: TokenKind::Op(Op::Not), .. }) = self.tokens.next() {
                    should_be_inverted = !should_be_inverted;
                }

                let operand = self.primary_expression()?;
                // returning to avoid the call to tokens.next at the end of the function
                return match operand.typ() {
                    Type::Int | Type::Char | Type::Bool => {
                        if should_be_inverted {
                            Ok(Expression::Unary { op: Op::Not, operand: Box::new(operand) })
                        } else {
                            Ok(operand)
                        }
                    }
                    Type::Str => Err(Error::new(
                        self.tokens.src,
                        current_token.col,
                        current_token.kind.len(),
                        ErrorKind::CannotInvertString,
                    )),
                    Type::Array(_, _) => Err(Error::new(
                        self.tokens.src,
                        current_token.col,
                        current_token.kind.len(),
                        ErrorKind::CannotInvertArray,
                    )),
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
            | TokenKind::Continue => Err(Error::new(
                self.tokens.src,
                current_token.col,
                current_token.kind.len(),
                ErrorKind::KeywordInExpression,
            )),
            _ => Err(Error::new(
                self.tokens.src,
                current_token.col,
                current_token.kind.len(),
                ErrorKind::ExpectedOperand,
            )),
        };

        let _ = self.tokens.next();
        factor
    }

    fn exponentiative_expression(&mut self) -> Result<Expression<'src>, Error<'src>> {
        let mut lhs = self.primary_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::Pow])? {
            let rhs = self.primary_expression()?;
            lhs = match (lhs.typ(), rhs.typ()) {
                (Type::Int | Type::Char | Type::Bool, Type::Str) | (Type::Str, Type::Int | Type::Char | Type::Bool) => {
                    return Err(Error::new(
                        self.tokens.src,
                        op_token.col,
                        op_token.kind.len(),
                        ErrorKind::StringsInExpression,
                    ))
                }
                _ => Expression::Binary {
                    lhs: Box::new(lhs),
                    op_position: self.tokens.src.position(op_token.col),
                    op,
                    rhs: Box::new(rhs),
                },
            }
        }

        Ok(lhs)
    }

    fn multiplicative_expression(&mut self) -> Result<Expression<'src>, Error<'src>> {
        let mut lhs = self.exponentiative_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::Times, Op::Divide, Op::Remainder])? {
            let rhs = self.exponentiative_expression()?;
            lhs = match (lhs.typ(), rhs.typ()) {
                (Type::Int | Type::Char | Type::Bool, Type::Str) | (Type::Str, Type::Int | Type::Char | Type::Bool) => {
                    return Err(Error::new(
                        self.tokens.src,
                        op_token.col,
                        op_token.kind.len(),
                        ErrorKind::StringsInExpression,
                    ))
                }
                _ => Expression::Binary {
                    lhs: Box::new(lhs),
                    op_position: self.tokens.src.position(op_token.col),
                    op,
                    rhs: Box::new(rhs),
                },
            }
        }

        Ok(lhs)
    }

    fn additive_expression(&mut self) -> Result<Expression<'src>, Error<'src>> {
        let mut lhs = self.multiplicative_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::Plus, Op::Minus])? {
            let rhs = self.multiplicative_expression()?;
            lhs = match (lhs.typ(), rhs.typ()) {
                (Type::Int | Type::Char | Type::Bool, Type::Str) | (Type::Str, Type::Int | Type::Char | Type::Bool) => {
                    return Err(Error::new(
                        self.tokens.src,
                        op_token.col,
                        op_token.kind.len(),
                        ErrorKind::StringsInExpression,
                    ))
                }
                _ => Expression::Binary {
                    lhs: Box::new(lhs),
                    op_position: self.tokens.src.position(op_token.col),
                    op,
                    rhs: Box::new(rhs),
                },
            }
        }

        Ok(lhs)
    }

    fn shift_expression(&mut self) -> Result<Expression<'src>, Error<'src>> {
        let mut lhs = self.additive_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::LeftShift, Op::RightShift])? {
            let rhs = self.additive_expression()?;
            lhs = match (lhs.typ(), rhs.typ()) {
                (Type::Int | Type::Char | Type::Bool, Type::Str) | (Type::Str, Type::Int | Type::Char | Type::Bool) => {
                    return Err(Error::new(
                        self.tokens.src,
                        op_token.col,
                        op_token.kind.len(),
                        ErrorKind::StringsInExpression,
                    ))
                }
                _ => Expression::Binary {
                    lhs: Box::new(lhs),
                    op_position: self.tokens.src.position(op_token.col),
                    op,
                    rhs: Box::new(rhs),
                },
            }
        }

        Ok(lhs)
    }

    fn bitand_expression(&mut self) -> Result<Expression<'src>, Error<'src>> {
        let mut lhs = self.shift_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::BitAnd])? {
            let rhs = self.shift_expression()?;
            lhs = match (lhs.typ(), rhs.typ()) {
                (Type::Int | Type::Char | Type::Bool, Type::Str) | (Type::Str, Type::Int | Type::Char | Type::Bool) => {
                    return Err(Error::new(
                        self.tokens.src,
                        op_token.col,
                        op_token.kind.len(),
                        ErrorKind::StringsInExpression,
                    ))
                }
                _ => Expression::Binary {
                    lhs: Box::new(lhs),
                    op_position: self.tokens.src.position(op_token.col),
                    op,
                    rhs: Box::new(rhs),
                },
            }
        }

        Ok(lhs)
    }

    fn bitxor_expression(&mut self) -> Result<Expression<'src>, Error<'src>> {
        let mut lhs = self.bitand_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::BitXor])? {
            let rhs = self.bitand_expression()?;
            lhs = match (lhs.typ(), rhs.typ()) {
                (Type::Int | Type::Char | Type::Bool, Type::Str) | (Type::Str, Type::Int | Type::Char | Type::Bool) => {
                    return Err(Error::new(
                        self.tokens.src,
                        op_token.col,
                        op_token.kind.len(),
                        ErrorKind::StringsInExpression,
                    ))
                }
                _ => Expression::Binary {
                    lhs: Box::new(lhs),
                    op_position: self.tokens.src.position(op_token.col),
                    op,
                    rhs: Box::new(rhs),
                },
            }
        }

        Ok(lhs)
    }

    fn bitor_expression(&mut self) -> Result<Expression<'src>, Error<'src>> {
        let mut lhs = self.bitxor_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::BitOr])? {
            let rhs = self.bitxor_expression()?;
            lhs = match (lhs.typ(), rhs.typ()) {
                (Type::Int | Type::Char | Type::Bool, Type::Str) | (Type::Str, Type::Int | Type::Char | Type::Bool) => {
                    return Err(Error::new(
                        self.tokens.src,
                        op_token.col,
                        op_token.kind.len(),
                        ErrorKind::StringsInExpression,
                    ))
                }
                _ => Expression::Binary {
                    lhs: Box::new(lhs),
                    op_position: self.tokens.src.position(op_token.col),
                    op,
                    rhs: Box::new(rhs),
                },
            }
        }

        Ok(lhs)
    }

    fn comparative_expression(&mut self) -> Result<Expression<'src>, Error<'src>> {
        let mut lhs = self.bitor_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::Compare])? {
            let rhs = self.bitor_expression()?;
            lhs = match (lhs.typ(), rhs.typ()) {
                (Type::Int | Type::Char | Type::Bool, Type::Str) | (Type::Str, Type::Int | Type::Char | Type::Bool) => {
                    return Err(Error::new(
                        self.tokens.src,
                        op_token.col,
                        op_token.kind.len(),
                        ErrorKind::StringsInExpression,
                    ))
                }
                _ => Expression::Binary {
                    lhs: Box::new(lhs),
                    op_position: self.tokens.src.position(op_token.col),
                    op,
                    rhs: Box::new(rhs),
                },
            }
        }

        Ok(lhs)
    }

    fn comparison_expression(&mut self) -> Result<Expression<'src>, Error<'src>> {
        let mut lhs = self.comparative_expression()?;

        let ops = [Op::EqualsEquals, Op::NotEquals, Op::Greater, Op::GreaterOrEquals, Op::Less, Op::LessOrEquals];

        let mut is_chained = false;
        while let Some((op_token, op)) = self.operator(&ops)? {
            let rhs = self.comparative_expression()?;

            if is_chained {
                return Err(Error::new(
                    self.tokens.src,
                    op_token.col,
                    op_token.kind.len(),
                    ErrorKind::ChainedComparison,
                ));
            }
            is_chained = true;

            lhs = Expression::Binary {
                lhs: Box::new(lhs),
                op_position: self.tokens.src.position(op_token.col),
                op,
                rhs: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    fn and_expression(&mut self) -> Result<Expression<'src>, Error<'src>> {
        let mut lhs = self.comparison_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::And])? {
            if lhs.typ() != Type::Bool {
                return Err(Error::new(
                    self.tokens.src,
                    op_token.col,
                    op_token.kind.len(),
                    ErrorKind::NonBooleanLeftOperand,
                ));
            }

            let rhs = self.comparison_expression()?;
            if rhs.typ() != Type::Bool {
                return Err(Error::new(
                    self.tokens.src,
                    op_token.col,
                    op_token.kind.len(),
                    ErrorKind::NonBooleanRightOperand,
                ));
            }

            lhs = Expression::Binary {
                lhs: Box::new(lhs),
                op_position: self.tokens.src.position(op_token.col),
                op,
                rhs: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    fn or_expression(&mut self) -> Result<Expression<'src>, Error<'src>> {
        let mut lhs = self.and_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::Or])? {
            if lhs.typ() != Type::Bool {
                return Err(Error::new(
                    self.tokens.src,
                    op_token.col,
                    op_token.kind.len(),
                    ErrorKind::NonBooleanLeftOperand,
                ));
            }

            let rhs = self.and_expression()?;
            if rhs.typ() != Type::Bool {
                return Err(Error::new(
                    self.tokens.src,
                    op_token.col,
                    op_token.kind.len(),
                    ErrorKind::NonBooleanRightOperand,
                ));
            }

            lhs = Expression::Binary {
                lhs: Box::new(lhs),
                op_position: self.tokens.src.position(op_token.col),
                op,
                rhs: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    // TODO(stefano): implement boolean operators for strings
    // TODO(stefano): disallow implicit conversions
    // IDEA(stefano): introduce casting operators
    // TODO(stefano): implement boolean operator chaining
    fn expression(&mut self) -> Result<Expression<'src>, Error<'src>> {
        self.or_expression()
    }
}

// variables and types
impl<'src: 'tokens, 'tokens> Ast<'src, 'tokens> {
    fn resolve_variable(
        &self,
        name: &'src str,
    ) -> Option<(usize /* scope idx */, usize /* variable idx */, &Variable<'src>)> {
        let mut scope_idx = self.scope;
        loop {
            let scope = &self.scopes[scope_idx];
            for (variable_idx, variable) in scope.variables.iter().enumerate() {
                if variable.name == name {
                    return Some((scope_idx, variable_idx, variable));
                }
            }

            scope_idx = match scope_idx {
                0 => return None,
                _ => scope.parent,
            };
        }
    }

    fn resolve_type(&self, name: &'src str) -> Option<&Type> {
        let mut scope_idx = self.scope;
        loop {
            let scope = &self.scopes[scope_idx];
            for typ in &scope.types {
                if typ.to_string() == name {
                    return Some(typ);
                }
            }

            scope_idx = match scope_idx {
                0 => return None,
                _ => scope.parent,
            };
        }
    }

    fn type_annotation(&mut self) -> Result<Option<(&'tokens Token<'src>, Type)>, Error<'src>> {
        let colon_token =
            self.tokens.next().bounded(&mut self.tokens, ExpectedBeforeEof::TypeAnnotationOrVariableDefinition)?;
        match &colon_token.kind {
            TokenKind::Colon => {
                let type_token = self.tokens.next().bounded(&mut self.tokens, ExpectedBeforeEof::TypeAnnotation)?;
                match &type_token.kind {
                    TokenKind::Identifier(type_name) => match self.resolve_type(type_name) {
                        Some(typ) => match self.tokens.peek_next() {
                            Some(
                                open_square_bracket_token @ Token {
                                    kind: TokenKind::Bracket(BracketKind::OpenSquare),
                                    ..
                                },
                            ) => {
                                let array_type = Box::new(typ.clone());
                                let _ = self.tokens.next();
                                let len_token =
                                    self.tokens.next().bounded(&mut self.tokens, ExpectedBeforeEof::ArrayLength)?;
                                match len_token.kind {
                                    TokenKind::Literal(Literal::Int(len)) => match self.tokens.next() {
                                        Some(
                                            close_square_bracket_token @ Token {
                                                kind: TokenKind::Bracket(BracketKind::CloseSquare),
                                                ..
                                            },
                                        ) => Ok(Some((
                                            close_square_bracket_token,
                                            Type::Array(len.try_into().unwrap(), array_type),
                                        ))),
                                        Some(_) | None => Err(Error::new(
                                            self.tokens.src,
                                            open_square_bracket_token.col,
                                            open_square_bracket_token.kind.len(),
                                            ErrorKind::MissingSquareBracketInArrayTypeAnnotation,
                                        )),
                                    },
                                    TokenKind::Bracket(BracketKind::CloseSquare) => Err(Error::new(
                                        self.tokens.src,
                                        len_token.col,
                                        len_token.kind.len(),
                                        ErrorKind::MissingArrayLength,
                                    )),
                                    _ => Err(Error::new(
                                        self.tokens.src,
                                        len_token.col,
                                        len_token.kind.len(),
                                        ErrorKind::NonLiteralIntegerArrayLength,
                                    )),
                                }
                            }
                            Some(_) | None => Ok(Some((type_token, typ.clone()))),
                        },
                        None => match self.resolve_variable(type_name) {
                            Some((_, _, var)) => Ok(Some((type_token, var.value.typ()))),
                            None => Err(Error::new(
                                self.tokens.src,
                                type_token.col,
                                type_token.kind.len(),
                                ErrorKind::VariableAsTypeAnnotationNotPreviouslyDefined,
                            )),
                        },
                    },
                    _ => Err(Error::new(
                        self.tokens.src,
                        colon_token.col,
                        colon_token.kind.len(),
                        ErrorKind::ExpectedTypeName,
                    )),
                }
            }
            _ => {
                self.tokens.token -= 1;
                Ok(None)
            }
        }
    }

    fn variable_definition(&mut self) -> Result<Node<'src>, Error<'src>> {
        let definition_token = self.tokens.current.unwrap();
        let TokenKind::Definition(mutability) = definition_token.kind else {
            unreachable!("cannot be anything different from 'let' or 'var'");
        };

        let name_token = self.tokens.next().bounded(&mut self.tokens, ExpectedBeforeEof::Identifier)?;
        let name = match &name_token.kind {
            TokenKind::Identifier(name) => match self.resolve_type(name) {
                None => Ok(name),
                Some(_) => Err(Error::new(
                    self.tokens.src,
                    name_token.col,
                    name_token.kind.len(),
                    ErrorKind::TypeNameInVariableName,
                )),
            },
            _ => {
                Err(Error::new(self.tokens.src, name_token.col, name_token.kind.len(), ErrorKind::ExpectedVariableName))
            }
        };
        let name = name?;

        let annotation = self.type_annotation()?;

        let equals_or_semicolon_token =
            self.tokens.next().bounded(&mut self.tokens, ExpectedBeforeEof::EqualsOrSemicolon)?;
        let expression = match equals_or_semicolon_token.kind {
            TokenKind::Op(Op::Equals) => {
                let _ = self.tokens.next();
                match self.expression() {
                    Ok(expr) => Ok(Some(expr)),
                    Err(err) => Err(err),
                }
            }
            TokenKind::SemiColon => Ok(None),
            _ => match annotation {
                None => Err(Error::new(
                    self.tokens.src,
                    name_token.col,
                    name_token.kind.len(),
                    ErrorKind::ExpectedEqualsOrSemicolonAfterVariableName,
                )),
                Some((annotation_token, _)) => Err(Error::new(
                    self.tokens.src,
                    annotation_token.col,
                    annotation_token.kind.len(),
                    ErrorKind::ExpectedEqualsOrSemicolonAfterTypeAnnotation,
                )),
            },
        };
        let expression = expression?;

        if self.resolve_variable(name).is_some() {
            return Err(Error::new(
                self.tokens.src,
                name_token.col,
                name_token.kind.len(),
                ErrorKind::VariableRedefinition,
            ));
        }

        match expression {
            Some(mut value) => {
                if let Some((token, typ)) = &annotation {
                    if let Expression::Array(_, array_typ @ Type::Infer) = &mut value {
                        *array_typ = typ.inner();
                    }

                    if *typ != value.typ() {
                        return Err(Error::new(
                            self.tokens.src,
                            token.col,
                            token.kind.len(),
                            ErrorKind::VariableDefinitionTypeMismatch { expected: typ.clone(), actual: value.typ() },
                        ));
                    }
                } else if value.typ().should_be_inferred() {
                    return Err(Error::new(
                        self.tokens.src,
                        name_token.col,
                        name_token.kind.len(),
                        ErrorKind::ExpectedTypeAnnotation,
                    ));
                }

                let variables = &mut self.scopes[self.scope].variables;
                variables.push(Variable { mutability, name, value });
                Ok(Node::Definition(self.scope, variables.len() - 1))
            }
            None => match annotation {
                Some((_, typ)) => {
                    let variables = &mut self.scopes[self.scope].variables;
                    variables.push(Variable { mutability, name, value: typ.into() });
                    Ok(Node::Definition(self.scope, variables.len() - 1))
                }
                None => Err(Error::new(
                    self.tokens.src,
                    name_token.col,
                    name_token.kind.len(),
                    ErrorKind::ExpectedTypeAnnotationOrValue,
                )),
            },
        }
    }

    fn variable_reassignment(&mut self) -> Result<Node<'src>, Error<'src>> {
        let name_token = self.tokens.current.unwrap();
        let TokenKind::Identifier(name) = name_token.kind else {
            unreachable!("cannot be different from an identifier");
        };

        if self.resolve_type(name).is_some() {
            return Err(Error::new(
                self.tokens.src,
                name_token.col,
                name_token.kind.len(),
                ErrorKind::TypeNameInVariableReassignment,
            ));
        }

        let op_token = self.tokens.next().unwrap();

        let _ = self.tokens.next();
        let rhs = self.expression()?;
        match self.resolve_variable(name) {
            Some((scope_idx, var_idx, var)) => match var.mutability {
                Mutability::Let => Err(Error::new(
                    self.tokens.src,
                    name_token.col,
                    name_token.kind.len(),
                    ErrorKind::TryingToMutateImmutableVariable,
                )),
                Mutability::Var => {
                    let value = match &op_token.kind {
                        TokenKind::Op(Op::Equals) => rhs,
                        TokenKind::Op(op) => Expression::Binary {
                            lhs: Box::new(Expression::Identifier(name, op.typ())),
                            op_position: self.tokens.src.position(op_token.col),
                            op: *op,
                            rhs: Box::new(rhs),
                        },
                        _ => unreachable!("cannot be different from an operator"),
                    };

                    if var.value.typ() == value.typ() {
                        Ok(Node::Assignment(scope_idx, var_idx, value))
                    } else {
                        Err(Error::new(
                            self.tokens.src,
                            name_token.col,
                            name_token.kind.len(),
                            ErrorKind::VariableAssignmentTypeMismatch {
                                expected: var.value.typ(),
                                actual: value.typ(),
                            },
                        ))
                    }
                }
            },
            None => Err(Error::new(
                self.tokens.src,
                name_token.col,
                name_token.kind.len(),
                ErrorKind::VariableNotPreviouslyDefined,
            )),
        }
    }
}

// print statements
impl<'src: 'tokens, 'tokens> Ast<'src, 'tokens> {
    fn print(&mut self) -> Result<Node<'src>, Error<'src>> {
        let print_token = self.tokens.current.unwrap();
        if let TokenKind::PrintLn = print_token.kind {
            if let Some(&Token { kind: TokenKind::SemiColon, .. }) = self.tokens.peek_next() {
                let _ = self.tokens.next();
                return Ok(Node::Println(None));
            }
        }

        let start_of_expression_token = self.tokens.next().bounded(&mut self.tokens, ExpectedBeforeEof::Expression)?;
        let argument = self.expression()?;
        if let Expression::Array(_, _) = argument {
            return Err(Error::new(
                self.tokens.src,
                start_of_expression_token.col,
                start_of_expression_token.kind.len(),
                ErrorKind::TemporaryArrayNotSupportedYet,
            ));
        }

        match print_token.kind {
            TokenKind::Print => Ok(Node::Print(argument)),
            TokenKind::PrintLn => Ok(Node::Println(Some(argument))),
            _ => unreachable!("cannot be different from 'print' or 'println'"),
        }
    }
}

// if statements
impl<'src: 'tokens, 'tokens> Ast<'src, 'tokens> {
    fn iff(&mut self) -> Result<Node<'src>, Error<'src>> {
        let mut if_statement = If { ifs: Vec::new(), els: None };

        'iff: while let Some(if_token) = self.tokens.current {
            let _ = self.tokens.next().bounded(&mut self.tokens, ExpectedBeforeEof::BooleanExpression)?;

            let expression = self.expression()?;
            let condition = match &expression.typ() {
                Type::Bool => Ok(expression),
                Type::Char | Type::Int | Type::Str | Type::Array(_, _) | Type::Infer => Err(Error::new(
                    self.tokens.src,
                    if_token.col,
                    if_token.kind.len(),
                    ErrorKind::ExpectedBooleanExpressionInIfStatement,
                )),
            };

            let condition = condition?;
            let after_condition_token = self.tokens.current.bounded(&mut self.tokens, ExpectedBeforeEof::DoOrBlock)?;
            let iff = match after_condition_token.kind {
                TokenKind::Bracket(BracketKind::OpenCurly) => {
                    let scope = self.parse_single_any()?.unwrap();
                    Ok(IfStatement { condition, statement: scope })
                }
                TokenKind::Do => {
                    let statement = self.parse_do_single_statement()?.unwrap();
                    self.semicolon()?;
                    Ok(IfStatement { condition, statement })
                }
                _ => {
                    let before_curly_bracket_token = self.tokens.peek_previous();
                    Err(Error::new(
                        self.tokens.src,
                        before_curly_bracket_token.col,
                        before_curly_bracket_token.kind.len(),
                        ErrorKind::ExpectedDoOrBlockAfterIfStatement,
                    ))
                }
            };

            if_statement.ifs.push(iff?);

            while let Some(else_token) = self.tokens.current {
                let after_else_token = match else_token.kind {
                    TokenKind::Else => {
                        self.tokens.next().bounded(&mut self.tokens, ExpectedBeforeEof::DoOrBlockOrIfStatement)?
                    }
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
                        let statement = self.parse_do_single_statement()?.unwrap();
                        self.semicolon()?;
                        if_statement.els = Some(Box::new(statement));
                        break 'iff;
                    }
                    TokenKind::If => break,
                    _ => Err(Error::new(
                        self.tokens.src,
                        else_token.col,
                        else_token.kind.len(),
                        ErrorKind::ExpectedDoOrBlockOrIfStatementAfterIfStatement,
                    )),
                };

                else_if?;
            }
        }

        Ok(Node::If(if_statement))
    }
}

// for statements
impl<'src: 'tokens, 'tokens> Ast<'src, 'tokens> {
    fn loop_statement(&mut self) -> Result<Node<'src>, Error<'src>> {
        let do_token = self.tokens.current.unwrap();
        let loop_token = match do_token.kind {
            TokenKind::Do => self.tokens.next().bounded(&mut self.tokens, ExpectedBeforeEof::LoopStatement)?,
            _ => do_token,
        };

        let _ = self.tokens.next().bounded(&mut self.tokens, ExpectedBeforeEof::BooleanExpression)?;
        let expression = self.expression()?;
        let condition = match &expression.typ() {
            Type::Bool => Ok(expression),
            Type::Char | Type::Int | Type::Str | Type::Array(_, _) | Type::Infer => Err(Error::new(
                self.tokens.src,
                loop_token.col,
                loop_token.kind.len(),
                ErrorKind::ExpectedBooleanExpressionInLoopStatement,
            )),
        };

        let after_condition_token = self.tokens.current.bounded(&mut self.tokens, ExpectedBeforeEof::DoOrBlock)?;
        let statement = match after_condition_token.kind {
            TokenKind::Bracket(BracketKind::OpenCurly) => {
                let scope = self.parse_single_any()?.unwrap();
                Ok(scope)
            }
            TokenKind::Do => {
                let statement = self.parse_do_single_statement()?.unwrap();
                self.semicolon()?;
                Ok(statement)
            }
            _ => {
                let before_curly_bracket_token = self.tokens.peek_previous();
                Err(Error::new(
                    self.tokens.src,
                    before_curly_bracket_token.col,
                    before_curly_bracket_token.kind.len(),
                    ErrorKind::ExpectedBooleanExpressionInLoopStatement,
                ))
            }
        };

        let condition = condition?;
        let statement = statement?;
        let condition = if let TokenKind::Do = do_token.kind {
            LoopCondition::Post(Expression::Unary { op: Op::Not, operand: Box::new(condition) })
        } else {
            LoopCondition::Pre(condition)
        };

        Ok(Node::Loop(Loop { condition, statement: Box::new(statement) }))
    }
}

#[derive(Debug)]
pub enum ExpectedBeforeEof {
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

impl Display for ExpectedBeforeEof {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::StatementAfterDo => write!(f, "expected statement after do keyword"),
            Self::Semicolon => write!(f, "expected semicolon"),
            Self::OperatorOrSemicolon => write!(f, "expected operator or semicolon"),
            Self::Expression => write!(f, "expected expression"),
            Self::BooleanExpression => write!(f, "expected boolean expression"),
            Self::ClosingSquareBracket => write!(f, "expected closing square bracket"),
            Self::ClosingRoundBracket => write!(f, "expected closing round bracket"),
            Self::ArrayElementOrClosingSquareBracket => write!(f, "expected array element or closing square bracket"),
            Self::CommaOrClosingSquareBracket => write!(f, "expected comma or closing square bracket"),
            Self::TypeAnnotationOrVariableDefinition => write!(f, "expected type annotation or variable definition"),
            Self::TypeAnnotation => write!(f, "expected type annotation"),
            Self::ArrayLength => write!(f, "expected array length"),
            Self::Identifier => write!(f, "expected identifier"),
            Self::EqualsOrSemicolon => write!(f, "expected '=' or ';'"),
            Self::DoOrBlock => write!(f, "expected do statement or block"),
            Self::DoOrBlockOrIfStatement => write!(f, "expected do statement, block or if statement"),
            Self::LoopStatement => write!(f, "expected loop statement"),
        }
    }
}

#[derive(Debug)]
pub enum BlocksNotAllowedIn {
    DoStatement,
}

impl Display for BlocksNotAllowedIn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DoStatement => write!(f, "blocks are not allowed in do statements"),
        }
    }
}

#[derive(Debug)]
pub enum VariableDefinitionNotAllowedIn {
    DoStatement,
}

impl Display for VariableDefinitionNotAllowedIn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DoStatement => write!(f, "variable definitions are not allowed in do statements"),
        }
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    NoMoreTokens(ExpectedBeforeEof),
    StrayElseBlock,
    BreakOutsideOfLoop,
    ContinueOutsideOfLoop,
    BlocksNotAllowed(BlocksNotAllowedIn),
    UnopenedBracket(BracketKind),
    UnclosedBracket(BracketKind),
    TemporaryArrayNotSupportedYet,
    NestedArrayNotSupportedYet,
    MismatchedArrayElementType { expected: Type, actual: Type },
    StrayColon,
    StrayComma,
    StrayEquals,
    StrayBinaryOperator(Op),
    VariableDefinitionNotAllowed(VariableDefinitionNotAllowedIn),
    MissingSemicolon,
    MissingSquareBracketInArrayIndex,
    MissingSquareBracketInArrayTypeAnnotation,
    MissingArrayLength,
    NonLiteralIntegerArrayLength,
    ExpectedIntegerExpressionInArrayIndex,
    VariableNotPreviouslyDefined,
    VariableRedefinition,
    VariableAsTypeAnnotationNotPreviouslyDefined,
    VariableDefinitionTypeMismatch { expected: Type, actual: Type },
    VariableAssignmentTypeMismatch { expected: Type, actual: Type },
    ExpectedTypeAnnotation,
    ExpectedTypeAnnotationOrValue,
    TypeNameInExpressions,
    TypeNameInVariableName,
    TypeNameInVariableReassignment,
    TryingToMutateImmutableVariable,
    ExpectedVariableName,
    ExpectedTypeName,
    ExpectedEqualsOrSemicolonAfterVariableName,
    ExpectedEqualsOrSemicolonAfterTypeAnnotation,
    EmptyExpression,
    CannotNegateBoolean,
    CannotNegateString,
    CannotNegateArray,
    CannotInvertString,
    CannotInvertArray,
    KeywordInExpression,
    ExpectedOperand,
    StringsInExpression,
    ChainedComparison,
    NonBooleanLeftOperand,
    NonBooleanRightOperand,
    ExpectedBooleanExpressionInIfStatement,
    ExpectedDoOrBlockAfterIfStatement,
    ExpectedDoOrBlockOrIfStatementAfterIfStatement,
    ExpectedBooleanExpressionInLoopStatement,
    ExpectedDoOrBlockAfterLoopStatement,
}

#[derive(Debug)]
pub struct Error<'src> {
    pub path: &'src Path,
    pub position: Position,
    pub len: usize,
    pub line_text: &'src str,
    pub kind: ErrorKind,
}

impl<'src> Error<'src> {
    fn new(src: &'src SrcFile, col: usize, len: usize, kind: ErrorKind) -> Self {
        let position = src.position(col);
        let line_text = src.line_text(position);
        Self { path: &src.path, position, len, line_text, kind }
    }
}

impl Display for Error<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (msg, help_msg): (Cow<'_, str>, Cow<'_, str>) = match &self.kind {
            ErrorKind::NoMoreTokens(kind) => (kind.to_string().into(), "no more tokens left after here".into()),
            ErrorKind::StrayElseBlock => ("invalid if statement".into(), "stray else block".into()),
            ErrorKind::BreakOutsideOfLoop => {
                ("invalid break statement".into(), "cannot be used outside of loops".into())
            }
            ErrorKind::ContinueOutsideOfLoop => {
                ("invalid continue statement".into(), "cannot be used outside of loops".into())
            }
            ErrorKind::BlocksNotAllowed(context) => ("invalid statement".into(), context.to_string().into()),
            ErrorKind::UnopenedBracket(bracket) => {
                ("invalid statement".into(), format!("'{bracket}' bracket was not opened").into())
            }
            ErrorKind::UnclosedBracket(bracket) => {
                ("invalid expression".into(), format!("'{bracket}' bracket was not closed").into())
            }
            ErrorKind::TemporaryArrayNotSupportedYet => (
                "invalid expression".into(),
                "temporary arrays are not supported yet, extract this to a variable first".into(),
            ),
            ErrorKind::NestedArrayNotSupportedYet => {
                ("invalid array element".into(), "nested arrays are not supported yet".into())
            }
            ErrorKind::MismatchedArrayElementType { expected, actual } => (
                "invalid array element".into(),
                format!("expected element of type '{expected}', but got '{actual}'").into(),
            ),
            ErrorKind::StrayColon => ("invalid type annotation".into(), "stray colon".into()),
            ErrorKind::StrayComma => ("invalid array element separator".into(), "stray comma".into()),
            ErrorKind::StrayEquals => ("invalid assignment".into(), "stray assigment".into()),
            ErrorKind::StrayBinaryOperator(op) => {
                ("invalid expression".into(), format!("stray '{op}' operator").into())
            }
            ErrorKind::VariableDefinitionNotAllowed(context) => {
                ("invalid statement".into(), context.to_string().into())
            }
            ErrorKind::MissingSemicolon => ("invalid statement".into(), "expected semicolon after here".into()),
            ErrorKind::MissingSquareBracketInArrayIndex => {
                ("invalid array index".into(), "must be followed by a close square bracket".into())
            }
            ErrorKind::MissingSquareBracketInArrayTypeAnnotation => {
                ("invalid array type annotation".into(), "missing closing square bracket".into())
            }
            ErrorKind::MissingArrayLength => {
                ("invalid array type annotation".into(), "missing array length before this token".into())
            }
            ErrorKind::NonLiteralIntegerArrayLength => {
                ("invalid array type annotation".into(), "must be a literal integer".into())
            }
            ErrorKind::ExpectedIntegerExpressionInArrayIndex => {
                ("invalid array index".into(), "must be followed by an integer expression".into())
            }
            ErrorKind::VariableNotPreviouslyDefined => {
                ("variable not defined".into(), "was not previously defined in this scope".into())
            }
            ErrorKind::VariableRedefinition => {
                ("variable redefinition".into(), "was previously defined in this scope".into())
            }
            ErrorKind::VariableDefinitionTypeMismatch { expected, actual } => (
                "invalid variable definition".into(),
                format!("declared type of '{expected}' doesn't match value of type '{actual}'").into(),
            ),
            ErrorKind::VariableAssignmentTypeMismatch { expected, actual } => (
                "invalid variable assignment".into(),
                format!("trying to assign an expression of type '{actual}' to a variable of type '{expected}'").into(),
            ),
            ErrorKind::VariableAsTypeAnnotationNotPreviouslyDefined => {
                ("invalid type annotation".into(), "was not previously defined in this scope".into())
            }
            ErrorKind::ExpectedTypeName => ("invalid type annotation".into(), "expected type name after here".into()),
            ErrorKind::ExpectedTypeAnnotation => (
                "invalid variable definition".into(),
                "expected type annotation after here to infer the type of the variable".into(),
            ),
            ErrorKind::ExpectedTypeAnnotationOrValue => (
                "invalid variable definition".into(),
                "expected type annotation or value after here to infer the type of the variable".into(),
            ),
            ErrorKind::TypeNameInExpressions => ("invalid expression".into(), "cannot be a type name".into()),
            ErrorKind::TypeNameInVariableName => ("invalid variable name".into(), "cannot be a type name".into()),
            ErrorKind::TypeNameInVariableReassignment => {
                ("invalid variable assignment".into(), "cannot be a type name".into())
            }
            ErrorKind::TryingToMutateImmutableVariable => {
                ("invalid variable assignment".into(), "cannot mutate immutable variable".into())
            }
            ErrorKind::ExpectedVariableName => ("invalid variable definition".into(), "expected variable name".into()),
            ErrorKind::ExpectedEqualsOrSemicolonAfterVariableName => {
                ("invalid variable definition".into(), "expected '=' or ';' after variable name".into())
            }
            ErrorKind::ExpectedEqualsOrSemicolonAfterTypeAnnotation => {
                ("invalid variable definition".into(), "expected '=' or ';' after type annotation".into())
            }
            ErrorKind::EmptyExpression => ("invalid expression".into(), "empty expressions are not allowed".into()),
            ErrorKind::CannotNegateBoolean => (
                "invalid expression".into(),
                "cannot negate a boolean value, use the '!' operator instead to invert it".into(),
            ),
            ErrorKind::CannotNegateString => ("invalid expression".into(), "cannot negate a string".into()),
            ErrorKind::CannotNegateArray => ("invalid expression".into(), "cannot negate an array".into()),
            ErrorKind::CannotInvertString => ("invalid expression".into(), "cannot invert a string".into()),
            ErrorKind::CannotInvertArray => ("invalid expression".into(), "cannot invert an array".into()),
            ErrorKind::KeywordInExpression => ("invalid expression".into(), "cannot be a keyword".into()),
            ErrorKind::ExpectedOperand => {
                ("invalid expression".into(), "expected expression operand before this token".into())
            }
            ErrorKind::StringsInExpression => {
                ("invalid expression".into(), "strings are not allowed inside expressions".into())
            }
            ErrorKind::ChainedComparison => {
                ("invalid boolean expression".into(), "comparison operators cannot be chained".into())
            }
            ErrorKind::NonBooleanLeftOperand => {
                ("invalid boolean expression".into(), "must be preceded by a boolean expression".into())
            }
            ErrorKind::NonBooleanRightOperand => {
                ("invalid boolean expression".into(), "must be followed by a boolean expression".into())
            }
            ErrorKind::ExpectedBooleanExpressionInIfStatement => {
                ("invalid if statement".into(), "must be followed by a boolean expression".into())
            }
            ErrorKind::ExpectedDoOrBlockAfterIfStatement => {
                ("invalid if statement".into(), "must be followed by a do statement or a block".into())
            }
            ErrorKind::ExpectedDoOrBlockOrIfStatementAfterIfStatement => {
                ("invalid if statement".into(), "must be followed by a do statement, a block or an if statement".into())
            }
            ErrorKind::ExpectedBooleanExpressionInLoopStatement => {
                ("invalid loop statement".into(), "must be followed by a boolean expression".into())
            }
            ErrorKind::ExpectedDoOrBlockAfterLoopStatement => {
                ("invalid loop statement".into(), "must be followed by a do statement or a block".into())
            }
        };

        let error_msg = Colored { text: msg.to_string(), fg: Fg::White, bg: Bg::Default, flags: Flag::Bold };

        let line_number_text =
            Colored { text: self.position.line.to_string(), fg: Fg::LightBlue, bg: Bg::Default, flags: Flag::Bold };

        let visualization_padding = line_number_text.text.len() + 1 + BAR.text.len();
        let at_padding = visualization_padding - 1;

        let pointers_col = self.position.col - 1;
        let pointers_len = self.len;

        let pointers_and_help_msg = Colored {
            text: format!("{:>pointers_col$}{:^>pointers_len$} {help_msg}", "", ""),
            fg: Fg::LightRed,
            bg: Bg::Default,
            flags: Flag::Bold,
        };

        write!(
            f,
            "{ERROR}: {error_msg}\
            \n{AT:>at_padding$}: {path}:{line}:{col}\
            \n{BAR:>visualization_padding$}\
            \n{line_number_text} {BAR} {line_text}\
            \n{BAR:>visualization_padding$} {pointers_and_help_msg}",
            path = self.path.display(),
            line = self.position.line,
            col = self.position.col,
            line_text = self.line_text,
        )
    }
}

impl std::error::Error for Error<'_> {}
