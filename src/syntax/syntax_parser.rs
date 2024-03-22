use std::{fmt::Display, marker::PhantomData};
use crate::src_file::SrcFile;
use super::{tokenizer::{Literal, Token, TokenKind}, RawSyntaxError, SyntaxErrorCause, SyntaxErrorKind, SyntaxErrors};


#[derive(Debug, Clone)]
pub(crate) enum Expression<'src> {
    Literal(Literal),
    TODO(PhantomData<&'src ()>),
}

impl Display for Expression<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(literal) => write!(f, "{literal}"),
            Self::TODO(_) => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct Scope<'src> {
    // pub(crate) types: Vec<Type>,
    // pub(crate) variables: Vec<Variable<'src>>,
    pub(crate) nodes: Vec<Node<'src>>,
}

#[derive(Debug)]
pub(crate) enum Node<'src> {
    Expression(Expression<'src>),
    Scope(Scope<'src>),
}

impl Display for Node<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expression(expression) => write!(f, "{expression}"),
            Self::Scope(scope) => {
                for node in &scope.nodes {
                    write!(f, "{node}")?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug)]
pub struct SyntaxParser<'src, 'tokens: 'src> {
    src: &'src SrcFile,
    errors: Vec<RawSyntaxError<ErrorKind, ErrorCause>>,

    token: usize,
    tokens: &'tokens [Token<'src>],

    ast: Scope<'src>,
}

impl <'src, 'tokens: 'src> SyntaxParser<'src, 'tokens> {
    pub fn build(
        src: &'src SrcFile,
        tokens: &'tokens [Token<'src>],
    ) -> Result<Scope<'src>, SyntaxErrors<'src, ErrorKind, ErrorCause>> {
        if tokens.is_empty() {
            return Ok(Scope { nodes: Vec::new() });
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
            ast: Scope {
                nodes: Vec::new(),
            },
        };

        // this.parse_scope();

        if this.errors.is_empty() {
            Ok(this.ast)
        } else {
            Err(SyntaxErrors { src, raw_errors: this.errors })
        }
    }
}

impl<'src, 'tokens: 'src> SyntaxParser<'src, 'tokens> {

}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    TODO,
}

impl SyntaxErrorKind for ErrorKind {}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TODO => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ErrorCause {
    TODO
}

impl SyntaxErrorCause for ErrorCause {}

impl Display for ErrorCause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TODO => todo!()
        }
    }
}
