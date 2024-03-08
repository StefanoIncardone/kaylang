use super::{
    error::{SyntaxError, SyntaxErrorInfo},
    src_file::{Position, SrcFile},
    tokenizer::{BracketKind, Literal, Mutability, Op, SrcCodeLen, Token, TokenKind},
};
use crate::error::{ErrorInfo, RawSyntaxError, SyntaxErrorKind, SyntaxErrors};
use std::fmt::{Debug, Display};

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

    // TODO(stefano): enforce a max length
    Array { len: usize, elements_type: Box<Type> },
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Infer, Self::Infer)
            | (Self::Int, Self::Int)
            | (Self::Char, Self::Char)
            | (Self::Bool, Self::Bool)
            | (Self::Str, Self::Str) => true,

            (Self::Array { len: len_1, elements_type: typ_1 }, Self::Array { len: len_2, elements_type: typ_2 }) => {
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
            Self::Char => write!(f, "char"),
            Self::Bool => write!(f, "bool"),
            Self::Str => write!(f, "str"),
            Self::Array { len, elements_type } => write!(f, "{elements_type}[{len}]"),

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
            Self::Array { len, elements_type } => elements_type.size() * len,

            Self::Infer => unreachable!("should have been coerced to a concrete type"),
        }
    }

    pub(crate) fn should_be_inferred(&self) -> bool {
        match self {
            Self::Infer => true,
            Self::Array { elements_type, .. } => elements_type.should_be_inferred(),
            Self::Int | Self::Char | Self::Bool | Self::Str => false,
        }
    }

    pub(crate) fn inner(&self) -> Self {
        match self {
            Self::Array { elements_type, .. } => elements_type.inner(),
            _ => self.clone(),
        }
    }

    pub(crate) fn can_be_compared_to(&self, other: &Type) -> bool {
        match (self, other) {
            (Self::Str, Self::Str) | (Self::Int | Self::Bool | Self::Char, Self::Int | Self::Bool | Self::Char) => true,

            (Self::Str, _)
            | (_, Self::Str)
            | (Self::Array { .. }, _)
            | (_, Self::Array { .. })
            | (Self::Infer, _)
            | (_, Self::Infer) => false,
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
    Identifier { name: &'src str, typ: Type },
    Array { elements: Vec<Expression<'src>>, elements_type: Type },
    ArrayIndex { var_name: &'src str, element_type: Type, bracket_position: Position, index: Box<Expression<'src>> },
}

impl Debug for Expression<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(literal) => write!(f, "{literal}"),
            Self::Unary { op, operand } => write!(f, "{op}{operand:?}"),
            Self::Binary { lhs, op, rhs, .. } => write!(f, "({lhs:?} {op} {rhs:?})"),
            Self::Identifier { name, .. } => write!(f, "{name}"),
            Self::Array { elements, .. } => {
                write!(f, "[")?;
                if !elements.is_empty() {
                    let mut elements_iter = elements.iter();
                    let last = elements_iter.next_back().unwrap(); // we have already checked for a non empty array
                    for element in elements_iter {
                        write!(f, "{element:?}, ")?;
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
            Self::Array { elements, elements_type } => {
                Type::Array { len: elements.len(), elements_type: Box::new(elements_type.clone()) }
            }
            Self::ArrayIndex { element_type, .. } => element_type.clone(),
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
            Type::Array { len, elements_type } => {
                let mut elements = Vec::<Expression<'_>>::with_capacity(len);
                for _ in 0..=len {
                    let inner_typ = *elements_type.clone();
                    elements.push(inner_typ.into());
                }
                Self::Array { elements, elements_type: *elements_type }
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
pub struct Ast<'src, 'tokens: 'src> {
    src: &'src SrcFile,

    token: usize,
    tokens: &'tokens [Token<'src>],

    scope: usize,
    scopes: Vec<Scope<'src>>,

    loop_depth: usize,

    errors: Vec<RawSyntaxError<ErrorKind>>,
}

impl<'src, 'tokens: 'src> Ast<'src, 'tokens> {
    pub fn build(src: &'src SrcFile, tokens: &'tokens [Token<'src>]) -> Result<Vec<Scope<'src>>, Vec<Error<'src>>> {
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
            token,
            tokens,
            scope: 0,
            scopes: vec![Scope {
                parent: 0,
                types: vec![Type::Int, Type::Char, Type::Bool, Type::Str],
                variables: Vec::new(),
                nodes: Vec::new(),
            }],
            loop_depth: 0,
            errors: Vec::new(),
        };

        this.parse_scope();

        if this.errors.is_empty() {
            Ok(this.scopes)
        } else {
            let errors = SyntaxErrors { src, raw_errors: this.errors };
            Err(errors.iter().collect())
        }
    }

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
                                self.token = self.tokens.len();
                                break;
                            }
                        }

                        // no need to check for a terminating semicolon
                        Node::If(_) | Node::Loop(_) | Node::Scope(_) => {}
                    }

                    self.scopes[self.scope].nodes.push(node);
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

    fn parse_single_statement(&mut self) -> Result<Option<Node<'src>>, RawSyntaxError<ErrorKind>> {
        let Some(current_token) = self.tokens.get(self.token) else { return Ok(None) };

        match current_token.kind {
            TokenKind::Literal(_)
            | TokenKind::True
            | TokenKind::False
            | TokenKind::Bracket(BracketKind::OpenRound)
            | TokenKind::Op(Op::Minus | Op::Not) => Ok(Some(Node::Expression(self.expression()?))),
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
            TokenKind::Print => Ok(Some(self.print()?)),
            TokenKind::PrintLn => Ok(Some(self.println()?)),
            TokenKind::If => Ok(Some(self.iff()?)),
            TokenKind::Else => {
                let _ = self.next_token();
                Err(RawSyntaxError {
                    kind: ErrorKind::StrayElseBlock,
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
                        kind: ErrorKind::BreakOutsideOfLoop,
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
                        kind: ErrorKind::ContinueOutsideOfLoop,
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
                let _ = self.next_token();
                Err(RawSyntaxError {
                    kind: ErrorKind::BlocksNotAllowed(BlocksNotAllowedIn::DoStatement),
                    col: current_token.col,
                    len: current_token.kind.src_code_len(),
                })
            }
            TokenKind::Bracket(BracketKind::CloseCurly) => {
                let _ = self.next_token();
                Err(RawSyntaxError {
                    kind: ErrorKind::UnopenedBracket(BracketKind::CloseCurly),
                    col: current_token.col,
                    len: current_token.kind.src_code_len(),
                })
            }
            TokenKind::Bracket(BracketKind::OpenSquare) => {
                let _ = self.expression()?;
                Err(RawSyntaxError {
                    kind: ErrorKind::TemporaryArrayNotSupportedYet,
                    col: current_token.col,
                    len: current_token.kind.src_code_len(),
                })
            }
            TokenKind::Bracket(BracketKind::CloseSquare) => {
                let _ = self.next_token();
                Err(RawSyntaxError {
                    kind: ErrorKind::UnopenedBracket(BracketKind::CloseSquare),
                    col: current_token.col,
                    len: current_token.kind.src_code_len(),
                })
            }
            TokenKind::Bracket(BracketKind::CloseRound) => {
                let _ = self.next_token();
                Err(RawSyntaxError {
                    kind: ErrorKind::UnopenedBracket(BracketKind::CloseRound),
                    col: current_token.col,
                    len: current_token.kind.src_code_len(),
                })
            }
            TokenKind::Colon => {
                let _ = self.next_token();
                Err(RawSyntaxError {
                    kind: ErrorKind::StrayColon,
                    col: current_token.col,
                    len: current_token.kind.src_code_len(),
                })
            }
            TokenKind::Comma => {
                let _ = self.next_token();
                Err(RawSyntaxError {
                    kind: ErrorKind::StrayComma,
                    col: current_token.col,
                    len: current_token.kind.src_code_len(),
                })
            }
            TokenKind::Op(Op::Equals) => {
                let _ = self.next_token();
                Err(RawSyntaxError {
                    kind: ErrorKind::StrayEquals,
                    col: current_token.col,
                    len: current_token.kind.src_code_len(),
                })
            }
            TokenKind::Op(op) => {
                let _ = self.next_token();
                Err(RawSyntaxError {
                    kind: ErrorKind::StrayBinaryOperator(op),
                    col: current_token.col,
                    len: current_token.kind.src_code_len(),
                })
            }
            TokenKind::Comment(_) => unreachable!("should be skipped by the token iterator"),
            TokenKind::Unexpected(_) => unreachable!("only valid tokens should be present"),
        }
    }

    fn parse_do_single_statement(&mut self) -> Result<Option<Node<'src>>, RawSyntaxError<ErrorKind>> {
        let current_token = self.next_token_bounded(ExpectedBeforeEof::StatementAfterDo)?;
        match current_token.kind {
            TokenKind::Bracket(BracketKind::OpenCurly) => {
                let _ = self.next_token();
                Err(RawSyntaxError {
                    kind: ErrorKind::BlocksNotAllowed(BlocksNotAllowedIn::DoStatement),
                    col: current_token.col,
                    len: current_token.kind.src_code_len(),
                })
            }
            TokenKind::Definition(_) => {
                let _ = self.next_token();
                Err(RawSyntaxError {
                    kind: ErrorKind::VariableDefinitionNotAllowed(VariableDefinitionNotAllowedIn::DoStatement),
                    col: current_token.col,
                    len: current_token.kind.src_code_len(),
                })
            }
            _ => self.parse_single_statement(),
        }
    }

    fn parse_single_any(&mut self) -> Result<Option<Node<'src>>, RawSyntaxError<ErrorKind>> {
        let Some(current_token) = self.tokens.get(self.token) else { return Ok(None) };

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

                let _ = self.next_token();
                self.parse_scope();
                Ok(Some(Node::Scope(new_scope)))
            }
            TokenKind::Bracket(BracketKind::CloseCurly) => {
                self.scope = self.scopes[self.scope].parent;
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
        expected: ExpectedBeforeEof,
    ) -> Result<&'tokens Token<'src>, RawSyntaxError<ErrorKind>> {
        let Some(token) = self.tokens.get(self.token) else {
            let previous = self.peek_previous_token();
            return Err(RawSyntaxError {
                kind: ErrorKind::NoMoreTokens(expected),
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
        expected_before_eof: ExpectedBeforeEof,
    ) -> Result<&'tokens Token<'src>, RawSyntaxError<ErrorKind>> {
        loop {
            if self.token >= self.tokens.len() - 1 {
                let previous = &self.tokens[self.token];
                self.token = self.tokens.len();
                return Err(RawSyntaxError {
                    kind: ErrorKind::NoMoreTokens(expected_before_eof),
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

// semicolons
impl<'src, 'tokens: 'src> Ast<'src, 'tokens> {
    fn semicolon(&mut self) -> Result<(), RawSyntaxError<ErrorKind>> {
        let semicolon_token = self.current_token_bounded(ExpectedBeforeEof::Semicolon)?;
        if let TokenKind::SemiColon = &semicolon_token.kind {
            let _ = self.next_token();
            Ok(())
        } else {
            let previous_token = self.peek_previous_token();
            Err(RawSyntaxError {
                kind: ErrorKind::MissingSemicolon,
                col: previous_token.col,
                len: previous_token.kind.src_code_len(),
            })
        }
    }
}

// expressions
impl<'src, 'tokens: 'src> Ast<'src, 'tokens> {
    fn assert_lhs_is_not_string_or_array(
        op_token: &'tokens Token<'src>,
        lhs: &Expression<'src>,
    ) -> Result<(), RawSyntaxError<ErrorKind>> {
        if let Type::Str = lhs.typ() {
            return Err(RawSyntaxError {
                kind: ErrorKind::StringLeftOperand,
                col: op_token.col,
                len: op_token.kind.src_code_len(),
            });
        }
        if let Type::Array { .. } = lhs.typ() {
            return Err(RawSyntaxError {
                kind: ErrorKind::ArrayLeftOperand,
                col: op_token.col,
                len: op_token.kind.src_code_len(),
            });
        }

        Ok(())
    }

    fn assert_rhs_is_not_string_or_array(
        op_token: &'tokens Token<'src>,
        rhs: &Expression<'src>,
    ) -> Result<(), RawSyntaxError<ErrorKind>> {
        if let Type::Str = rhs.typ() {
            return Err(RawSyntaxError {
                kind: ErrorKind::StringRightOperand,
                col: op_token.col,
                len: op_token.kind.src_code_len(),
            });
        }
        if let Type::Array { .. } = rhs.typ() {
            return Err(RawSyntaxError {
                kind: ErrorKind::ArrayRightOperand,
                col: op_token.col,
                len: op_token.kind.src_code_len(),
            });
        }

        Ok(())
    }

    fn operator(&mut self, ops: &[Op]) -> Result<Option<(&'tokens Token<'src>, Op)>, RawSyntaxError<ErrorKind>> {
        let current_token = self.current_token_bounded(ExpectedBeforeEof::OperatorOrSemicolon)?;
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

    fn index(&mut self, var_name: &'src str, var_typ: Type) -> Result<Expression<'src>, RawSyntaxError<ErrorKind>> {
        let current_token = &self.tokens[self.token];
        let Some(open_bracket_token @ Token { kind: TokenKind::Bracket(BracketKind::OpenSquare), .. }) =
            self.peek_next_token()
        else {
            return Ok(Expression::Identifier { name: var_name, typ: var_typ });
        };

        let _open_square = self.next_token();
        let _start_of_index = self.next_token();

        let index = self.expression()?;
        let Type::Int = index.typ() else {
            return Err(RawSyntaxError {
                kind: ErrorKind::ExpectedIntegerExpressionInArrayIndex,
                col: open_bracket_token.col,
                len: open_bracket_token.kind.src_code_len(),
            });
        };

        let after_index_token = self.current_token_bounded(ExpectedBeforeEof::ClosingSquareBracket)?;
        let TokenKind::Bracket(BracketKind::CloseSquare) = after_index_token.kind else {
            let before_index_token = self.peek_previous_token();
            return Err(RawSyntaxError {
                kind: ErrorKind::MissingSquareBracketInArrayIndex,
                col: before_index_token.col,
                len: before_index_token.kind.src_code_len(),
            });
        };

        match var_typ {
            Type::Array { elements_type, .. } => match *elements_type {
                Type::Int | Type::Bool | Type::Infer | Type::Char | Type::Str => Ok(Expression::ArrayIndex {
                    var_name,
                    element_type: *elements_type,
                    bracket_position: Position::new(self.src, open_bracket_token.col).0,
                    index: Box::new(index),
                }),
                Type::Array { .. } => Err(RawSyntaxError {
                    kind: ErrorKind::NestedArrayNotSupportedYet,
                    col: current_token.col,
                    len: current_token.kind.src_code_len(),
                }),
            },
            Type::Str => Ok(Expression::ArrayIndex {
                var_name,
                element_type: Type::Char,
                bracket_position: Position::new(self.src, open_bracket_token.col).0,
                index: Box::new(index),
            }),
            Type::Int => Ok(Expression::ArrayIndex {
                var_name,
                element_type: Type::Int,
                bracket_position: Position::new(self.src, open_bracket_token.col).0,
                index: Box::new(index),
            }),
            Type::Bool | Type::Infer | Type::Char => Err(RawSyntaxError {
                kind: ErrorKind::CannotIndexNonArrayType(var_typ),
                col: current_token.col,
                len: current_token.kind.src_code_len(),
            }),
        }
    }

    fn primary_expression(&mut self) -> Result<Expression<'src>, RawSyntaxError<ErrorKind>> {
        let current_token = self.current_token_bounded(ExpectedBeforeEof::Expression)?;
        let factor = match &current_token.kind {
            TokenKind::Literal(literal) => Ok(Expression::Literal(literal.clone())),
            TokenKind::True => Ok(Expression::Literal(Literal::Bool(true))),
            TokenKind::False => Ok(Expression::Literal(Literal::Bool(false))),
            TokenKind::Identifier(name) => match self.resolve_type(name) {
                None => match self.resolve_variable(name) {
                    Some((_, _, var)) => self.index(var.name, var.value.typ()),
                    None => Err(RawSyntaxError {
                        kind: ErrorKind::VariableNotPreviouslyDefined,
                        col: current_token.col,
                        len: current_token.kind.src_code_len(),
                    }),
                },
                Some(_) => Err(RawSyntaxError {
                    kind: ErrorKind::TypeNameInExpressions,
                    col: current_token.col,
                    len: current_token.kind.src_code_len(),
                }),
            },
            TokenKind::Bracket(open_bracket_kind @ BracketKind::OpenRound) => 'parenthesis: {
                let expression_start_token = self.next_token_bounded(ExpectedBeforeEof::Expression)?;
                if let TokenKind::Bracket(BracketKind::CloseRound) = expression_start_token.kind {
                    break 'parenthesis Err(RawSyntaxError {
                        kind: ErrorKind::EmptyExpression,
                        col: expression_start_token.col,
                        len: expression_start_token.kind.src_code_len(),
                    });
                }

                let expression = self.expression()?;
                let close_bracket_token = self.current_token_bounded(ExpectedBeforeEof::ClosingRoundBracket)?;
                match close_bracket_token.kind {
                    TokenKind::Bracket(BracketKind::CloseRound) => Ok(expression),
                    _ => Err(RawSyntaxError {
                        kind: ErrorKind::UnclosedBracket(*open_bracket_kind),
                        col: current_token.col,
                        len: current_token.kind.src_code_len(),
                    }),
                }
            }
            TokenKind::Bracket(BracketKind::OpenSquare) => 'array: {
                let _ = self.next_token();
                let mut elements = Vec::<Expression<'src>>::new();
                let mut elements_type = Type::Infer;

                loop {
                    let element_token =
                        self.current_token_bounded(ExpectedBeforeEof::ArrayElementOrClosingSquareBracket)?;

                    if let TokenKind::Bracket(BracketKind::CloseSquare) = element_token.kind {
                        break 'array Ok(Expression::Array { elements, elements_type });
                    }

                    let element = self.expression()?;
                    match (&elements_type, element.typ()) {
                        (_, Type::Array { .. }) => {
                            break 'array Err(RawSyntaxError {
                                kind: ErrorKind::NestedArrayNotSupportedYet,
                                col: element_token.col,
                                len: element_token.kind.src_code_len(),
                            })
                        }
                        (Type::Infer, other) => {
                            elements_type = other;
                            elements.push(element);
                        }
                        (expected, actual) if *expected != actual => {
                            break 'array Err(RawSyntaxError {
                                kind: ErrorKind::MismatchedArrayElementType { expected: expected.clone(), actual },
                                col: element_token.col,
                                len: element_token.kind.src_code_len(),
                            })
                        }
                        (_, _) => elements.push(element),
                    }

                    let comma_token = self.current_token_bounded(ExpectedBeforeEof::CommaOrClosingSquareBracket)?;
                    if let TokenKind::Comma = comma_token.kind {
                        let _ = self.next_token();
                    }
                }
            }
            TokenKind::Op(Op::Minus) => {
                let mut sign: isize = -1;
                // NOTE(stefano): this optimization should be moved to later stages
                while let Some(&Token { kind: TokenKind::Op(Op::Minus), .. }) = self.next_token() {
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
                    Type::Bool => Err(RawSyntaxError {
                        kind: ErrorKind::CannotNegateBoolean,
                        col: current_token.col,
                        len: current_token.kind.src_code_len(),
                    }),
                    Type::Str => Err(RawSyntaxError {
                        kind: ErrorKind::CannotNegateString,
                        col: current_token.col,
                        len: current_token.kind.src_code_len(),
                    }),
                    Type::Array { .. } => Err(RawSyntaxError {
                        kind: ErrorKind::CannotNegateArray,
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
                    Type::Int | Type::Char | Type::Bool => {
                        if should_be_inverted {
                            Ok(Expression::Unary { op: Op::Not, operand: Box::new(operand) })
                        } else {
                            Ok(operand)
                        }
                    }
                    Type::Str => Err(RawSyntaxError {
                        kind: ErrorKind::CannotInvertString,
                        col: current_token.col,
                        len: current_token.kind.src_code_len(),
                    }),
                    Type::Array { .. } => Err(RawSyntaxError {
                        kind: ErrorKind::CannotInvertArray,
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
                kind: ErrorKind::KeywordInExpression,
                col: current_token.col,
                len: current_token.kind.src_code_len(),
            }),
            _ => Err(RawSyntaxError {
                kind: ErrorKind::ExpectedOperand,
                col: current_token.col,
                len: current_token.kind.src_code_len(),
            }),
        };

        let _ = self.next_token();
        factor
    }

    fn exponentiative_expression(&mut self) -> Result<Expression<'src>, RawSyntaxError<ErrorKind>> {
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

    fn multiplicative_expression(&mut self) -> Result<Expression<'src>, RawSyntaxError<ErrorKind>> {
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

    fn additive_expression(&mut self) -> Result<Expression<'src>, RawSyntaxError<ErrorKind>> {
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

    fn shift_expression(&mut self) -> Result<Expression<'src>, RawSyntaxError<ErrorKind>> {
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

    fn bitand_expression(&mut self) -> Result<Expression<'src>, RawSyntaxError<ErrorKind>> {
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

    fn bitxor_expression(&mut self) -> Result<Expression<'src>, RawSyntaxError<ErrorKind>> {
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

    fn bitor_expression(&mut self) -> Result<Expression<'src>, RawSyntaxError<ErrorKind>> {
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

    fn comparative_expression(&mut self) -> Result<Expression<'src>, RawSyntaxError<ErrorKind>> {
        let mut lhs = self.bitor_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::Compare])? {
            let rhs = self.bitor_expression()?;

            let lhs_typ = lhs.typ();
            let rhs_typ = rhs.typ();
            if !lhs_typ.can_be_compared_to(&rhs_typ) {
                return Err(RawSyntaxError {
                    kind: ErrorKind::CannotCompareOperands { lhs_typ, rhs_typ },
                    col: op_token.col,
                    len: op_token.kind.src_code_len(),
                });
            }

            lhs = Expression::Binary {
                lhs: Box::new(lhs),
                op_position: Position::new(self.src, op_token.col).0,
                op,
                rhs: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    fn comparison_expression(&mut self) -> Result<Expression<'src>, RawSyntaxError<ErrorKind>> {
        let mut lhs = self.comparative_expression()?;

        let ops = [Op::EqualsEquals, Op::NotEquals, Op::Greater, Op::GreaterOrEquals, Op::Less, Op::LessOrEquals];

        let mut is_chained = false;
        while let Some((op_token, op)) = self.operator(&ops)? {
            let rhs = self.comparative_expression()?;

            let lhs_typ = lhs.typ();
            let rhs_typ = rhs.typ();
            if !lhs_typ.can_be_compared_to(&rhs_typ) {
                return Err(RawSyntaxError {
                    kind: ErrorKind::CannotCompareOperands { lhs_typ, rhs_typ },
                    col: op_token.col,
                    len: op_token.kind.src_code_len(),
                });
            }

            if is_chained {
                return Err(RawSyntaxError {
                    kind: ErrorKind::ChainedComparison,
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

    fn and_expression(&mut self) -> Result<Expression<'src>, RawSyntaxError<ErrorKind>> {
        let mut lhs = self.comparison_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::And])? {
            let Type::Bool = lhs.typ() else {
                return Err(RawSyntaxError {
                    kind: ErrorKind::NonBooleanLeftOperand,
                    col: op_token.col,
                    len: op_token.kind.src_code_len(),
                });
            };

            let rhs = self.comparison_expression()?;
            let Type::Bool = rhs.typ() else {
                return Err(RawSyntaxError {
                    kind: ErrorKind::NonBooleanRightOperand,
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

    fn or_expression(&mut self) -> Result<Expression<'src>, RawSyntaxError<ErrorKind>> {
        let mut lhs = self.and_expression()?;

        while let Some((op_token, op)) = self.operator(&[Op::Or])? {
            let Type::Bool = lhs.typ() else {
                return Err(RawSyntaxError {
                    kind: ErrorKind::NonBooleanLeftOperand,
                    col: op_token.col,
                    len: op_token.kind.src_code_len(),
                });
            };

            let rhs = self.and_expression()?;
            let Type::Bool = rhs.typ() else {
                return Err(RawSyntaxError {
                    kind: ErrorKind::NonBooleanRightOperand,
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

    // TODO(stefano): implement boolean operators for strings
    // TODO(stefano): disallow implicit conversions
    // TODO(stefano): introduce casting operators
    fn expression(&mut self) -> Result<Expression<'src>, RawSyntaxError<ErrorKind>> {
        self.or_expression()
    }
}

// variables and types
impl<'src, 'tokens: 'src> Ast<'src, 'tokens> {
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

    fn type_annotation(&mut self) -> Result<Option<(&'tokens Token<'src>, Type)>, RawSyntaxError<ErrorKind>> {
        let colon_token = self.next_token_bounded(ExpectedBeforeEof::TypeAnnotationOrVariableDefinition)?;
        let TokenKind::Colon = colon_token.kind else {
            self.token -= 1;
            return Ok(None);
        };

        let type_token = self.next_token_bounded(ExpectedBeforeEof::TypeAnnotation)?;
        let TokenKind::Identifier(type_name) = type_token.kind else {
            return Err(RawSyntaxError {
                kind: ErrorKind::ExpectedTypeName,
                col: colon_token.col,
                len: colon_token.kind.src_code_len(),
            });
        };

        let Some(typ) = self.resolve_type(type_name) else {
            return match self.resolve_variable(type_name) {
                Some((_, _, var)) => Ok(Some((type_token, var.value.typ()))),
                None => Err(RawSyntaxError {
                    kind: ErrorKind::VariableAsTypeAnnotationNotPreviouslyDefined,
                    col: type_token.col,
                    len: type_token.kind.src_code_len(),
                }),
            };
        };

        let Some(open_square_bracket_token @ Token { kind: TokenKind::Bracket(BracketKind::OpenSquare), .. }) =
            self.peek_next_token()
        else {
            return Ok(Some((type_token, typ.clone())));
        };

        let elements_type = Box::new(typ.clone());
        let _open_square_bracket = self.next_token();

        let len_token = self.next_token_bounded(ExpectedBeforeEof::ArrayLength)?;
        let len = match len_token.kind {
            TokenKind::Literal(Literal::Int(len)) => len,
            TokenKind::Bracket(BracketKind::CloseSquare) => {
                return Err(RawSyntaxError {
                    kind: ErrorKind::MissingArrayLength,
                    col: len_token.col,
                    len: len_token.kind.src_code_len(),
                })
            }
            _ => {
                return Err(RawSyntaxError {
                    kind: ErrorKind::NonLiteralIntegerArrayLength,
                    col: len_token.col,
                    len: len_token.kind.src_code_len(),
                })
            }
        };

        match self.next_token() {
            Some(close_square_bracket_token @ Token { kind: TokenKind::Bracket(BracketKind::CloseSquare), .. }) => {
                Ok(Some((close_square_bracket_token, Type::Array { len: len as usize, elements_type })))
            }
            Some(_) | None => Err(RawSyntaxError {
                kind: ErrorKind::MissingSquareBracketInArrayTypeAnnotation,
                col: open_square_bracket_token.col,
                len: open_square_bracket_token.kind.src_code_len(),
            }),
        }
    }

    fn variable_definition(&mut self) -> Result<Node<'src>, RawSyntaxError<ErrorKind>> {
        let definition_token = &self.tokens[self.token];
        let TokenKind::Definition(mutability) = definition_token.kind else {
            unreachable!("cannot be anything different from 'let' or 'var'");
        };

        let name_token = self.next_token_bounded(ExpectedBeforeEof::Identifier)?;
        let name = match &name_token.kind {
            TokenKind::Identifier(name) => match self.resolve_type(name) {
                None => Ok(name),
                Some(_) => Err(RawSyntaxError {
                    kind: ErrorKind::TypeNameInVariableName,
                    col: name_token.col,
                    len: name_token.kind.src_code_len(),
                }),
            },
            _ => Err(RawSyntaxError {
                kind: ErrorKind::ExpectedVariableName,
                col: name_token.col,
                len: name_token.kind.src_code_len(),
            }),
        };
        let name = name?;

        let annotation = self.type_annotation()?;

        let equals_or_semicolon_token = self.next_token_bounded(ExpectedBeforeEof::EqualsOrSemicolon)?;
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
                    kind: ErrorKind::ExpectedEqualsOrSemicolonAfterVariableName,
                    col: name_token.col,
                    len: name_token.kind.src_code_len(),
                }),
                Some((annotation_token, _)) => Err(RawSyntaxError {
                    kind: ErrorKind::ExpectedEqualsOrSemicolonAfterTypeAnnotation,
                    col: annotation_token.col,
                    len: annotation_token.kind.src_code_len(),
                }),
            },
        };
        let expression = expression?;

        if self.resolve_variable(name).is_some() {
            return Err(RawSyntaxError {
                kind: ErrorKind::VariableRedefinition,
                col: name_token.col,
                len: name_token.kind.src_code_len(),
            });
        }

        match expression {
            Some(mut value) => {
                if let Some((token, typ)) = &annotation {
                    if let Expression::Array { elements_type, .. } = &mut value {
                        if let Type::Infer = elements_type {
                            *elements_type = typ.inner();
                        }
                    }

                    if *typ != value.typ() {
                        return Err(RawSyntaxError {
                            kind: ErrorKind::VariableDefinitionTypeMismatch {
                                expected: typ.clone(),
                                actual: value.typ(),
                            },
                            col: token.col,
                            len: token.kind.src_code_len(),
                        });
                    }
                } else if value.typ().should_be_inferred() {
                    return Err(RawSyntaxError {
                        kind: ErrorKind::ExpectedTypeAnnotation,
                        col: name_token.col,
                        len: name_token.kind.src_code_len(),
                    });
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
                None => Err(RawSyntaxError {
                    kind: ErrorKind::ExpectedTypeAnnotationOrValue,
                    col: name_token.col,
                    len: name_token.kind.src_code_len(),
                }),
            },
        }
    }

    fn variable_reassignment(&mut self) -> Result<Node<'src>, RawSyntaxError<ErrorKind>> {
        let name_token = &self.tokens[self.token];
        let TokenKind::Identifier(name) = name_token.kind else {
            unreachable!("cannot be different from an identifier");
        };

        if self.resolve_type(name).is_some() {
            return Err(RawSyntaxError {
                kind: ErrorKind::TypeNameInVariableReassignment,
                col: name_token.col,
                len: name_token.kind.src_code_len(),
            });
        }

        // we checked for the presence of an op token before the call to this function
        let op_token = self.next_token().unwrap();

        let _ = self.next_token();
        let rhs = self.expression()?;
        match self.resolve_variable(name) {
            Some((scope_idx, var_idx, var)) => match var.mutability {
                Mutability::Let => Err(RawSyntaxError {
                    kind: ErrorKind::TryingToMutateImmutableVariable,
                    col: name_token.col,
                    len: name_token.kind.src_code_len(),
                }),
                Mutability::Var => {
                    let value = match &op_token.kind {
                        TokenKind::Op(Op::Equals) => rhs,
                        TokenKind::Op(op) => Expression::Binary {
                            lhs: Box::new(Expression::Identifier { name, typ: op.typ() }),
                            op_position: Position::new(self.src, op_token.col).0,
                            op: *op,
                            rhs: Box::new(rhs),
                        },
                        _ => unreachable!("cannot be different from an operator"),
                    };

                    if var.value.typ() == value.typ() {
                        Ok(Node::Assignment(scope_idx, var_idx, value))
                    } else {
                        Err(RawSyntaxError {
                            kind: ErrorKind::VariableAssignmentTypeMismatch {
                                expected: var.value.typ(),
                                actual: value.typ(),
                            },
                            col: name_token.col,
                            len: name_token.kind.src_code_len(),
                        })
                    }
                }
            },
            None => Err(RawSyntaxError {
                kind: ErrorKind::VariableNotPreviouslyDefined,
                col: name_token.col,
                len: name_token.kind.src_code_len(),
            }),
        }
    }
}

// print statements
impl<'src, 'tokens: 'src> Ast<'src, 'tokens> {
    fn print(&mut self) -> Result<Node<'src>, RawSyntaxError<ErrorKind>> {
        let arg = self.print_arg()?;
        Ok(Node::Print(arg))
    }

    fn println(&mut self) -> Result<Node<'src>, RawSyntaxError<ErrorKind>> {
        if let Some(&Token { kind: TokenKind::SemiColon, .. }) = self.peek_next_token() {
            let _ = self.next_token();
            return Ok(Node::Println(None));
        }

        let arg = self.print_arg()?;
        Ok(Node::Println(Some(arg)))
    }

    fn print_arg(&mut self) -> Result<Expression<'src>, RawSyntaxError<ErrorKind>> {
        let start_of_expression_token = self.next_token_bounded(ExpectedBeforeEof::Expression)?;
        let argument = self.expression()?;
        let Expression::Array { .. } = argument else {
            return Ok(argument);
        };

        Err(RawSyntaxError {
            kind: ErrorKind::TemporaryArrayNotSupportedYet,
            col: start_of_expression_token.col,
            len: start_of_expression_token.kind.src_code_len(),
        })
    }
}

// if statements
impl<'src, 'tokens: 'src> Ast<'src, 'tokens> {
    fn iff(&mut self) -> Result<Node<'src>, RawSyntaxError<ErrorKind>> {
        let mut if_statement = If { ifs: Vec::new(), els: None };

        'iff: while let Some(if_token) = self.tokens.get(self.token) {
            let _ = self.next_token_bounded(ExpectedBeforeEof::BooleanExpression)?;

            let expression = self.expression()?;
            let condition = match &expression.typ() {
                Type::Bool => Ok(expression),
                Type::Char | Type::Int | Type::Str | Type::Array { .. } | Type::Infer => Err(RawSyntaxError {
                    kind: ErrorKind::ExpectedBooleanExpressionInIfStatement,
                    col: if_token.col,
                    len: if_token.kind.src_code_len(),
                }),
            };

            let condition = condition?;
            let after_condition_token = self.current_token_bounded(ExpectedBeforeEof::DoOrBlock)?;
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
                    let before_curly_bracket_token = self.peek_previous_token();
                    Err(RawSyntaxError {
                        kind: ErrorKind::ExpectedDoOrBlockAfterIfStatement,
                        col: before_curly_bracket_token.col,
                        len: before_curly_bracket_token.kind.src_code_len(),
                    })
                }
            };

            if_statement.ifs.push(iff?);

            while let Some(else_token) = self.tokens.get(self.token) {
                let after_else_token = match else_token.kind {
                    TokenKind::Else => self.next_token_bounded(ExpectedBeforeEof::DoOrBlockOrIfStatement)?,
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
                    _ => Err(RawSyntaxError {
                        kind: ErrorKind::ExpectedDoOrBlockOrIfStatementAfterIfStatement,
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
    fn loop_statement(&mut self) -> Result<Node<'src>, RawSyntaxError<ErrorKind>> {
        let do_token = &self.tokens[self.token];
        let loop_token = match do_token.kind {
            TokenKind::Do => self.next_token_bounded(ExpectedBeforeEof::LoopStatement)?,
            _ => do_token,
        };

        let _ = self.next_token_bounded(ExpectedBeforeEof::BooleanExpression)?;
        let expression = self.expression()?;
        let condition = match &expression.typ() {
            Type::Bool => Ok(expression),
            Type::Char | Type::Int | Type::Str | Type::Array { .. } | Type::Infer => Err(RawSyntaxError {
                kind: ErrorKind::ExpectedBooleanExpressionInLoopStatement,
                col: loop_token.col,
                len: loop_token.kind.src_code_len(),
            }),
        };

        let after_condition_token = self.current_token_bounded(ExpectedBeforeEof::DoOrBlock)?;
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
                let before_curly_bracket_token = self.peek_previous_token();
                Err(RawSyntaxError {
                    kind: ErrorKind::ExpectedBooleanExpressionInLoopStatement,
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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
    StringLeftOperand,
    StringRightOperand,
    ArrayLeftOperand,
    ArrayRightOperand,
    ChainedComparison,
    NonBooleanLeftOperand,
    NonBooleanRightOperand,
    ExpectedBooleanExpressionInIfStatement,
    ExpectedDoOrBlockAfterIfStatement,
    ExpectedDoOrBlockOrIfStatementAfterIfStatement,
    ExpectedBooleanExpressionInLoopStatement,
    ExpectedDoOrBlockAfterLoopStatement,
    CannotIndexNonArrayType(Type),
    CannotCompareOperands { lhs_typ: Type, rhs_typ: Type },
}

impl ErrorInfo for ErrorKind {
    type Info = SyntaxErrorInfo;

    fn info(&self) -> Self::Info {
        let (msg, help_msg) = match &self {
            Self::NoMoreTokens(kind) => (kind.to_string().into(), "no more tokens left after here".into()),
            Self::StrayElseBlock => ("invalid if statement".into(), "stray else block".into()),
            Self::BreakOutsideOfLoop => ("invalid break statement".into(), "cannot be used outside of loops".into()),
            Self::ContinueOutsideOfLoop => {
                ("invalid continue statement".into(), "cannot be used outside of loops".into())
            }
            Self::BlocksNotAllowed(context) => ("invalid statement".into(), context.to_string().into()),
            Self::UnopenedBracket(bracket) => {
                ("invalid statement".into(), format!("'{bracket}' bracket was not opened").into())
            }
            Self::UnclosedBracket(bracket) => {
                ("invalid expression".into(), format!("'{bracket}' bracket was not closed").into())
            }
            Self::TemporaryArrayNotSupportedYet => (
                "invalid expression".into(),
                "temporary arrays are not supported yet, extract this to a variable first".into(),
            ),
            Self::NestedArrayNotSupportedYet => {
                ("invalid array element".into(), "nested arrays are not supported yet".into())
            }
            Self::MismatchedArrayElementType { expected, actual } => (
                "invalid array element".into(),
                format!("expected element of type '{expected}', but got '{actual}'").into(),
            ),
            Self::StrayColon => ("invalid type annotation".into(), "stray colon".into()),
            Self::StrayComma => ("invalid array element separator".into(), "stray comma".into()),
            Self::StrayEquals => ("invalid assignment".into(), "stray assigment".into()),
            Self::StrayBinaryOperator(op) => ("invalid expression".into(), format!("stray '{op}' operator").into()),
            Self::VariableDefinitionNotAllowed(context) => ("invalid statement".into(), context.to_string().into()),
            Self::MissingSemicolon => ("invalid statement".into(), "expected semicolon after here".into()),
            Self::MissingSquareBracketInArrayIndex => {
                ("invalid array index".into(), "must be followed by a close square bracket".into())
            }
            Self::MissingSquareBracketInArrayTypeAnnotation => {
                ("invalid array type annotation".into(), "missing closing square bracket".into())
            }
            Self::MissingArrayLength => {
                ("invalid array type annotation".into(), "missing array length before this token".into())
            }
            Self::NonLiteralIntegerArrayLength => {
                ("invalid array type annotation".into(), "must be a literal integer".into())
            }
            Self::ExpectedIntegerExpressionInArrayIndex => {
                ("invalid array index".into(), "must be followed by an integer expression".into())
            }
            Self::VariableNotPreviouslyDefined => {
                ("variable not defined".into(), "was not previously defined in this scope".into())
            }
            Self::VariableRedefinition => {
                ("variable redefinition".into(), "was previously defined in this scope".into())
            }
            Self::VariableDefinitionTypeMismatch { expected, actual } => (
                "invalid variable definition".into(),
                format!("declared type of '{expected}' doesn't match value of type '{actual}'").into(),
            ),
            Self::VariableAssignmentTypeMismatch { expected, actual } => (
                "invalid variable assignment".into(),
                format!("trying to assign an expression of type '{actual}' to a variable of type '{expected}'").into(),
            ),
            Self::VariableAsTypeAnnotationNotPreviouslyDefined => {
                ("invalid type annotation".into(), "was not previously defined in this scope".into())
            }
            Self::ExpectedTypeName => ("invalid type annotation".into(), "expected type name after here".into()),
            Self::ExpectedTypeAnnotation => (
                "invalid variable definition".into(),
                "expected type annotation after here to infer the type of the variable".into(),
            ),
            Self::ExpectedTypeAnnotationOrValue => (
                "invalid variable definition".into(),
                "expected type annotation or value after here to infer the type of the variable".into(),
            ),
            Self::TypeNameInExpressions => ("invalid expression".into(), "cannot be a type name".into()),
            Self::TypeNameInVariableName => ("invalid variable name".into(), "cannot be a type name".into()),
            Self::TypeNameInVariableReassignment => {
                ("invalid variable assignment".into(), "cannot be a type name".into())
            }
            Self::TryingToMutateImmutableVariable => {
                ("invalid variable assignment".into(), "cannot mutate immutable variable".into())
            }
            Self::ExpectedVariableName => ("invalid variable definition".into(), "expected variable name".into()),
            Self::ExpectedEqualsOrSemicolonAfterVariableName => {
                ("invalid variable definition".into(), "expected '=' or ';' after variable name".into())
            }
            Self::ExpectedEqualsOrSemicolonAfterTypeAnnotation => {
                ("invalid variable definition".into(), "expected '=' or ';' after type annotation".into())
            }
            Self::EmptyExpression => ("invalid expression".into(), "empty expressions are not allowed".into()),
            Self::CannotNegateBoolean => (
                "invalid expression".into(),
                "cannot negate a boolean value, use the '!' operator instead to invert it".into(),
            ),
            Self::CannotNegateString => ("invalid expression".into(), "cannot negate a string".into()),
            Self::CannotNegateArray => ("invalid expression".into(), "cannot negate an array".into()),
            Self::CannotInvertString => ("invalid expression".into(), "cannot invert a string".into()),
            Self::CannotInvertArray => ("invalid expression".into(), "cannot invert an array".into()),
            Self::KeywordInExpression => ("invalid expression".into(), "cannot be a keyword".into()),
            Self::ExpectedOperand => {
                ("invalid expression".into(), "expected expression operand before this token".into())
            }
            Self::StringLeftOperand => ("invalid expression".into(), "cannot be preceded by a string".into()),
            Self::StringRightOperand => ("invalid expression".into(), "cannot be followed by a string".into()),
            Self::ArrayLeftOperand => ("invalid expression".into(), "cannot be preceded by an array".into()),
            Self::ArrayRightOperand => ("invalid expression".into(), "cannot be followed by an array".into()),
            Self::ChainedComparison => {
                ("invalid boolean expression".into(), "comparison operators cannot be chained".into())
            }
            Self::NonBooleanLeftOperand => {
                ("invalid boolean expression".into(), "must be preceded by a boolean expression".into())
            }
            Self::NonBooleanRightOperand => {
                ("invalid boolean expression".into(), "must be followed by a boolean expression".into())
            }
            Self::ExpectedBooleanExpressionInIfStatement => {
                ("invalid if statement".into(), "must be followed by a boolean expression".into())
            }
            Self::ExpectedDoOrBlockAfterIfStatement => {
                ("invalid if statement".into(), "must be followed by a do statement or a block".into())
            }
            Self::ExpectedDoOrBlockOrIfStatementAfterIfStatement => {
                ("invalid if statement".into(), "must be followed by a do statement, a block or an if statement".into())
            }
            Self::ExpectedBooleanExpressionInLoopStatement => {
                ("invalid loop statement".into(), "must be followed by a boolean expression".into())
            }
            Self::ExpectedDoOrBlockAfterLoopStatement => {
                ("invalid loop statement".into(), "must be followed by a do statement or a block".into())
            }
            Self::CannotIndexNonArrayType(typ) => {
                ("invalid expression".into(), format!("cannot index into a value of type '{typ}'").into())
            }
            Self::CannotCompareOperands { lhs_typ, rhs_typ } => (
                "invalid expression".into(),
                format!("cannot compare left operand of type '{lhs_typ}' to right operand of type '{rhs_typ}'").into(),
            ),
        };

        Self::Info { msg, help_msg }
    }
}

impl SyntaxErrorKind for ErrorKind {}

#[deprecated(since = "0.5.3", note = "will be removed to allow for more explicit function signatures")]
pub type Error<'src> = SyntaxError<'src, ErrorKind>;
