use std::{fmt::Display, borrow::Cow};

use crate::{lexer::*, logging::*};


trait Bounded<'src: 'tokens, 'tokens> {
    type Error;

    fn bounded( self, tokens: &mut Tokens<'src, 'tokens>, err_msg: impl Into<Cow<'static, str>> ) -> Result<&'tokens Token<'src>, Self::Error>;
}


#[derive( Debug )]
struct Tokens<'src: 'tokens, 'tokens> {
    token: usize,
    tokens: &'tokens [Token<'src>],
    current: Option<&'tokens Token<'src>>,
}

impl<'src: 'tokens, 'tokens> From<&'tokens [Token<'src>]> for Tokens<'src, 'tokens> {
    fn from( tokens: &'tokens [Token<'src>] ) -> Self {
        let mut token = 0;

        // skipping to the first non-comment token
        let current = loop {
            if token >= tokens.len() {
                break None;
            }

            let current = &tokens[ token ];
            match current.kind {
                TokenKind::Comment( _ ) => token += 1,
                _ => break Some( current ),
            }
        };

        return Self { token, tokens, current };
    }
}

impl<'src: 'tokens, 'tokens> Tokens<'src, 'tokens> {
    fn next( &mut self ) -> Option<&'tokens Token<'src>> {
        self.current = loop {
            if self.token >= self.tokens.len() - 1 {
                self.token = self.tokens.len();
                break None;
            }

            self.token += 1;
            let next = &self.tokens[ self.token ];
            let TokenKind::Comment( _ ) = next.kind else {
                break Some( next );
            };
        };

        return self.current
    }

    fn peek_next( &self ) -> Option<&'tokens Token<'src>> {
        let mut current_token = self.token;
        loop {
            if current_token >= self.tokens.len() - 1 {
                return None;
            }

            current_token += 1;
            let next = &self.tokens[ current_token ];
            let TokenKind::Comment( _ ) = next.kind else {
                return Some( next );
            };
        }
    }

    fn peek_previous( &self ) -> &'tokens Token<'src> {
        let mut current_token = self.token;
        loop {
            current_token -= 1;
            let previous = &self.tokens[ current_token ];
            let TokenKind::Comment( _ ) = previous.kind else {
                return previous;
            };
        }
    }
}

impl<'src: 'tokens, 'tokens> Bounded<'src, 'tokens> for Option<&'tokens Token<'src>> {
    type Error = RawSyntaxError;

    fn bounded( self, tokens: &mut Tokens<'src, 'tokens>, err_msg: impl Into<Cow<'static, str>> ) -> Result<&'tokens Token<'src>, Self::Error> {
        return match self {
            Some( token ) => Ok( token ),
            None => {
                let previous = tokens.peek_previous();
                Err( RawSyntaxError {
                    col: previous.col,
                    len: previous.kind.len(),
                    msg: err_msg.into(),
                    help_msg: "no more tokens left after here".into(),
                } )
            },
        }
    }
}


pub(crate) trait TypeOf {
    fn typ( &self ) -> Type;
}

#[derive( Debug, PartialEq, Clone )]
pub(crate) enum Type {
    Int,
    Char,
    Bool,
    Str,
    Array( Box<Type>, usize ),
}

impl Display for Type {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Int                => write!( f, "int" ),
            Self::Char               => write!( f, "char" ),
            Self::Bool               => write!( f, "bool" ),
            Self::Str                => write!( f, "str" ),
            Self::Array( typ, len ) => write!( f, "{}[{}]", typ, len ),
        }
    }
}

impl Type {
    pub(crate) fn size( &self ) -> usize {
        match self {
            Self::Int  => core::mem::size_of::<isize>(),
            Self::Char => core::mem::size_of::<u8>(),
            Self::Bool => core::mem::size_of::<bool>(),
            Self::Str  => core::mem::size_of::<*const u8>() + core::mem::size_of::<usize>(),
            Self::Array( typ, len ) => typ.size() * len,
        }
    }
}


impl TypeOf for Literal {
    fn typ( &self ) -> Type {
        return match self {
            Self::Int( _ )  => Type::Int,
            Self::Char( _ ) => Type::Char,
            Self::Bool( _ ) => Type::Bool,
            Self::Str( _ )  => Type::Str,
        }
    }
}

impl TypeOf for Op {
    fn typ( &self ) -> Type {
        return match self {
            Self::Compare
            | Self::Pow | Self::PowEquals
            | Self::Times | Self::TimesEquals
            | Self::Divide | Self::DivideEquals
            | Self::Remainder | Self::RemainderEquals
            | Self::Plus | Self::PlusEquals
            | Self::Minus | Self::MinusEquals
            | Self::BitAnd | Self::BitAndEquals
            | Self::BitOr | Self::BitOrEquals
            | Self::BitXor | Self::BitXorEquals
            | Self::LeftShift | Self::LeftShiftEquals
            | Self::RightShift | Self::RightShiftEquals => Type::Int,

            Self::EqualsEquals | Self::NotEquals
            | Self::Greater | Self::GreaterOrEquals
            | Self::Less | Self::LessOrEquals
            | Self::Not
            | Self::And | Self::AndEquals
            | Self::Or | Self::OrEquals => Type::Bool,

            Self::Equals => unreachable!(),
        }
    }
}


#[derive( Debug, Clone )]
pub(crate) enum Expression<'src: 'tokens, 'tokens> {
    Literal( Literal ),
    Unary { op: Op, operand: Box<Expression<'src, 'tokens>> },
    Binary {
        lhs: Box<Expression<'src, 'tokens>>,
        // this reference to usize exists to prepare to add column information to every ast node
        // this is the first place where it is needed for now
        // it is used for error reporting (division by zero, modulo of 0, raising to a negative power)
        op_col: &'tokens usize,
        op: Op,
        rhs: Box<Expression<'src, 'tokens>>
    },
    Identifier( &'src str, Type ),
    Array( Vec<Literal>, Type ),
}

impl Display for Expression<'_, '_> {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Literal( literal )          => write!( f, "{}", literal ),
            Self::Unary { op, operand }       => write!( f, "{}{}", op, operand ),
            Self::Binary { lhs, op, rhs, .. } => write!( f, "({} {} {})", lhs, op, rhs ),
            Self::Identifier( name, _ )       => write!( f, "{}", name ),
            Self::Array( array, _ )           => {
                write!( f, "[" )?;
                if !array.is_empty() {
                    let max_size = array.len().max( 5 );
                    for element in array.iter().take( max_size - 1 ) {
                        write!( f, "{},", element )?;
                    }

                    write!( f, "{}", array[ array.len() - 1 ] )?;
                }
                write!( f, "]" )
            },
        }
    }
}

impl TypeOf for Expression<'_, '_> {
    fn typ( &self ) -> Type {
        return match self {
            Self::Literal( literal )    => literal.typ(),
            Self::Unary { operand, .. } => operand.typ(),
            Self::Binary { op, .. }     => op.typ(),
            Self::Identifier( _, typ )  => typ.clone(),
            Self::Array( _, typ )       => typ.clone(),
        }
    }
}

impl From<Type> for Expression<'_, '_> {
    fn from( typ: Type ) -> Self {
        match typ {
            Type::Bool              => Self::Literal( Literal::Bool( false ) ),
            Type::Char              => Self::Literal( Literal::Char( 0 ) ),
            Type::Int               => Self::Literal( Literal::Int( 0 ) ),
            Type::Str               => Self::Literal( Literal::Str( Vec::new() ) ),
            Type::Array( typ, len ) => Self::Array( Vec::with_capacity( len ), *typ ),
        }
    }
}


#[derive( Debug, Clone )]
pub(crate) struct Variable<'src: 'tokens, 'tokens> {
    pub(crate) mutability: Mutability,
    pub(crate) name: &'src str,
    pub(crate) value: Expression<'src, 'tokens>,
}


#[derive( Debug, Clone )]
pub(crate) struct IfStatement<'src: 'tokens, 'tokens> {
    pub(crate) condition: Expression<'src, 'tokens>,
    pub(crate) statement: Node<'src, 'tokens>,
}

impl Display for IfStatement<'_, '_> {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return write!( f, "if {}", self.condition );
    }
}

#[derive( Debug, Clone )]
pub(crate) struct If<'src: 'tokens, 'tokens> {
    pub(crate) ifs: Vec<IfStatement<'src, 'tokens>>,
    pub(crate) els: Option<Box<Node<'src, 'tokens>>>,
}


#[allow( dead_code )]
#[derive( Debug, Clone )]
pub(crate) enum LoopCondition<'src: 'tokens, 'tokens> {
    Pre( Expression<'src, 'tokens> ),
    Post( Expression<'src, 'tokens> )
}

#[allow( dead_code )]
#[derive( Debug, Clone )]
pub(crate) struct Loop<'src: 'tokens, 'tokens> {
    pub(crate) condition: LoopCondition<'src, 'tokens>,
    pub(crate) statement: Box<Node<'src, 'tokens>>,
}

impl Display for Loop<'_, '_> {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match &self.condition {
            LoopCondition::Pre( condition )  => write!( f, "loop {}", condition ),
            LoopCondition::Post( condition ) => write!( f, "do loop {}", condition ),
        }
    }
}


// TODO have each node and sub-node be made of a reference to the token/s from which the got generated
#[derive( Debug, Clone )]
pub(crate) enum Node<'src: 'tokens, 'tokens> {
    Empty,

    Expression( Expression<'src, 'tokens> ),
    Print( Expression<'src, 'tokens> ),
    Println( Option<Expression<'src, 'tokens>> ),
    If( If<'src, 'tokens> ),
    Loop( Loop<'src, 'tokens> ),
    Break,
    Continue,

    Definition( usize /* scope idx */, usize /* variable idx */ ),
    Assignment( usize /* scope idx */, usize /* variable idx */, Expression<'src, 'tokens> ),
    Scope( usize ),
}

impl Display for Node<'_, '_> {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Expression( expression ) => write!( f, "{}", expression ),
            Self::Print( argument )        => write!( f, "print {}", argument ),
            Self::Println( Some( arg ) )   => write!( f, "println {}", arg ),
            Self::Println( None )          => write!( f, "println" ),
            Self::If( iff )                => write!( f, "{}", iff.ifs[ 0 ] ),
            Self::Loop( looop )            => write!( f, "{}", looop ),

            Self::Empty
            | Self::Break | Self::Continue
            | Self::Definition( _, _ ) | Self::Assignment( _, _, _ )
            | Self::Scope( _ )             => unreachable!(),
        }
    }
}

#[derive( Debug, Clone )]
pub struct Scope<'src: 'tokens, 'tokens> {
    pub(crate) parent: usize,
    pub(crate) types: Vec<Type>,
    pub(crate) variables: Vec<Variable<'src, 'tokens>>,
    pub(crate) nodes: Vec<Node<'src, 'tokens>>,
}


// IDEA create Parser class that builds the AST, and then validate the AST afterwards
#[derive( Debug )]
pub(crate) struct Ast<'src: 'tokens, 'tokens> {
    src: &'src Src,

    scopes: Vec<Scope<'src, 'tokens>>,
    scope: usize,
    loop_depth: usize,

    tokens: Tokens<'src, 'tokens>,
    errors: Vec<SyntaxError>,
}

impl<'src: 'tokens, 'tokens> Ast<'src, 'tokens> {
    pub(crate) fn build( src: &'src Src, tokens: &'tokens [Token<'src>] ) -> Result<Vec<Scope<'src, 'tokens>>, SyntaxErrors<'src>> {
        if tokens.is_empty() {
            return Ok( Vec::new() );
        }

        let mut this = Self {
            src,
            scopes: vec![Scope {
                parent: 0,
                types: vec![Type::Int, Type::Char, Type::Bool, Type::Str],
                variables: Vec::new(),
                nodes: Vec::new(),
            }],
            scope: 0,
            tokens: Tokens::from( tokens ),
            loop_depth: 0,
            errors: Vec::new()
        };

        this.parse_scope();

        return match this.errors.is_empty() {
            true => Ok( this.scopes ),
            false => {
                this.errors.sort_by( |e1, e2| e1.line.cmp( &e2.line ) );
                Err( SyntaxErrors { src: this.src, errors: this.errors } )
            },
        }
    }
}

// parsing of nodes
impl<'src: 'tokens, 'tokens> Ast<'src, 'tokens> {
    fn parse_scope( &mut self ) {
        loop {
            match self.parse_single_any() {
                Ok( Some( node ) ) => {
                    match node {
                        // skip to the next token after a semicolon
                        Node::Empty => continue,

                        // check to see if a terminating semicolon is present
                        Node::Definition( _, _ ) | Node::Assignment( _, _, _ )
                        | Node::Expression( _ )
                        | Node::Break | Node::Continue
                        | Node::Print( _ ) | Node::Println( _ ) =>
                            if let Err( err ) = self.semicolon() {
                                self.errors.add( self.src, err );

                                self.tokens.token = self.tokens.tokens.len();
                                break;
                            },

                        // no need to check for a terminating semicolon
                        Node::If( _ ) | Node::Loop( _ ) | Node::Scope( _ ) => {},
                    }

                    self.scopes[ self.scope ].nodes.push( node );
                },
                Ok( None ) => break,
                // only parsing until the first error until a fault tolerant parser is developed,
                // this is because the first truly relevant error is the first one, which in turn
                // causes a ripple effect that propagates to the rest of the parsing, causing
                // subsequent errors to be wrong
                Err( err ) => {
                    self.errors.add( self.src, err );

                    // consuming all remaining tokens until the end of the file
                    self.tokens.token = self.tokens.tokens.len();
                    break;
                },
            }
        }
    }

    fn parse_single_statement( &mut self ) -> Result<Option<Node<'src, 'tokens>>, RawSyntaxError> {
        let current_token = match self.tokens.current {
            Some( token ) => token,
            None => return Ok( None ),
        };

        return match current_token.kind {
            TokenKind::Literal( _ )
            | TokenKind::True | TokenKind::False
            | TokenKind::Bracket( BracketKind::OpenRound )
            | TokenKind::Op( Op::Minus | Op::Not ) => Ok( Some( Node::Expression( self.expression()? ) ) ),
            TokenKind::Identifier( _ ) => match self.tokens.peek_next() {
                Some( op ) => match op.kind {
                    TokenKind::Op(
                        Op::Equals
                        | Op::PowEquals
                        | Op::TimesEquals | Op::DivideEquals | Op::RemainderEquals
                        | Op::PlusEquals | Op::MinusEquals
                        | Op::LeftShiftEquals | Op::RightShiftEquals
                        | Op::BitAndEquals | Op::BitXorEquals | Op::BitOrEquals
                        | Op::AndEquals | Op::OrEquals
                    ) => Ok( Some( self.variable_reassignment()? ) ),
                    _ => Ok( Some( Node::Expression( self.expression()? ) ) ),
                },
                None => Ok( Some( Node::Expression( self.expression()? ) ) ),
            },
            TokenKind::Definition( _ )            => Ok( Some( self.variable_definition()? ) ),
            TokenKind::Print | TokenKind::PrintLn => Ok( Some( self.print()? ) ),
            TokenKind::If                         => Ok( Some( self.iff()? ) ),
            TokenKind::Else => {
                self.tokens.next();
                Err( RawSyntaxError {
                    col: current_token.col,
                    len: current_token.kind.len(),
                    msg: "invalid if statement".into(),
                    help_msg: "stray else block".into(),
                } )
            },
            TokenKind::Do | TokenKind::Loop => {
                self.loop_depth += 1;
                let looop_statement = self.loop_statement();
                self.loop_depth -= 1;
                match looop_statement {
                    Ok( looop ) => Ok( Some( looop ) ),
                    Err( err ) => Err( err ),
                }
            },
            TokenKind::Break => {
                self.tokens.next();
                match self.loop_depth {
                    0 => Err( RawSyntaxError {
                        col: current_token.col,
                        len: current_token.kind.len(),
                        msg: "invalid break statement".into(),
                        help_msg: "cannot be used outside of loops".into(),
                    } ),
                    _ => Ok( Some( Node::Break ) ),
                }
            },
            TokenKind::Continue => {
                self.tokens.next();
                match self.loop_depth {
                    0 => Err( RawSyntaxError {
                        col: current_token.col,
                        len: current_token.kind.len(),
                        msg: "invalid continue statement".into(),
                        help_msg: "cannot be used outside of loops".into(),
                    } ),
                    _ => Ok( Some( Node::Continue ) ),
                }
            },
            TokenKind::Bracket( BracketKind::OpenCurly ) => {
                self.tokens.next();
                Err( RawSyntaxError {
                    col: current_token.col,
                    len: current_token.kind.len(),
                    msg: "invalid statement".into(),
                    help_msg: "blocks are not allowed in this context".into(),
                } )
            },
            TokenKind::Bracket( BracketKind::CloseCurly ) => {
                self.tokens.next();
                Err( RawSyntaxError {
                    col: current_token.col,
                    len: current_token.kind.len(),
                    msg: "invalid statement".into(),
                    help_msg: "stray closed curly bracket".into(),
                } )
            },
            TokenKind::Bracket( BracketKind::OpenSquare ) => {
                self.tokens.next();
                Err( RawSyntaxError {
                    col: current_token.col,
                    len: current_token.kind.len(),
                    msg: "invalid array type".into(),
                    help_msg: "stray open square bracket".into(),
                } )
            },
            TokenKind::Bracket( BracketKind::CloseSquare ) => {
                self.tokens.next();
                Err( RawSyntaxError {
                    col: current_token.col,
                    len: current_token.kind.len(),
                    msg: "invalid array type".into(),
                    help_msg: "stray closed square bracket".into(),
                } )
            },
            TokenKind::Bracket( BracketKind::CloseRound ) => {
                self.tokens.next();
                Err( RawSyntaxError {
                    col: current_token.col,
                    len: current_token.kind.len(),
                    msg: "invalid expression".into(),
                    help_msg: "stray closed parenthesis".into(),
                } )
            },
            TokenKind::Colon => {
                self.tokens.next();
                Err( RawSyntaxError {
                    col: current_token.col,
                    len: current_token.kind.len(),
                    msg: "invalid type annotation".into(),
                    help_msg: "stray colon".into(),
                } )
            },
            TokenKind::Op( Op::Equals ) => {
                self.tokens.next();
                Err( RawSyntaxError {
                    col: current_token.col,
                    len: current_token.kind.len(),
                    msg: "invalid assignment".into(),
                    help_msg: "stray assignment".into(),
                } )
            },
            TokenKind::Op( _ ) => {
                self.tokens.next();
                Err( RawSyntaxError {
                    col: current_token.col,
                    len: current_token.kind.len(),
                    msg: "invalid expression".into(),
                    help_msg: "stray binary operator".into(),
                } )
            },
            TokenKind::SemiColon => {
                self.tokens.next();
                Ok( Some( Node::Empty ) )
            },
            TokenKind::Comment( _ ) | TokenKind::Unexpected( _ ) => unreachable!(),
        }
    }

    fn parse_do_single_statement( &mut self ) -> Result<Option<Node<'src, 'tokens>>, RawSyntaxError> {
        let current_token = self.tokens.next().bounded( &mut self.tokens, "expected statement" )?;
        return match current_token.kind {
            TokenKind::Bracket( BracketKind::OpenCurly ) => {
                self.tokens.next();
                Err( RawSyntaxError {
                    col: current_token.col,
                    len: current_token.kind.len(),
                    msg: "invalid statement".into(),
                    help_msg: "blocks are not allowed in do statements".into(),
                } )
            },
            TokenKind::Definition( _ ) => {
                self.tokens.next();
                Err( RawSyntaxError {
                    col: current_token.col,
                    len: current_token.kind.len(),
                    msg: "invalid statement".into(),
                    help_msg: "variable definitions are not allowed in do statements".into(),
                } )
            },
            _ => self.parse_single_statement(),
        }
    }

    fn parse_single_any( &mut self ) -> Result<Option<Node<'src, 'tokens>>, RawSyntaxError> {
        let current_token = match self.tokens.current {
            Some( token ) => token,
            None => return Ok( None ),
        };

        return match current_token.kind {
            TokenKind::Bracket( BracketKind::OpenCurly ) => {
                let new_scope = self.scopes.len();
                self.scopes.push( Scope {
                    parent: self.scope,
                    types: Vec::new(),
                    variables: Vec::new(),
                    nodes: Vec::new(),
                } );
                self.scope = new_scope;

                self.tokens.next();
                self.parse_scope();
                Ok( Some( Node::Scope( new_scope ) ) )
            },
            TokenKind::Bracket( BracketKind::CloseCurly ) => {
                self.scope = self.scopes[ self.scope ].parent;
                self.tokens.next();
                Ok( None )
            },
            _ => self.parse_single_statement(),
        }
    }
}

// semicolons
impl<'src: 'tokens, 'tokens> Ast<'src, 'tokens> {
    fn semicolon( &mut self ) -> Result<(), RawSyntaxError> {
        let semicolon_token = self.tokens.current.bounded( &mut self.tokens, "expected semicolon" )?;
        return match &semicolon_token.kind {
            TokenKind::SemiColon => {
                self.tokens.next();
                Ok( () )
            },
            _ => {
                let previous_token = self.tokens.peek_previous();
                Err( RawSyntaxError {
                    col: previous_token.col,
                    len: previous_token.kind.len(),
                    msg: "invalid statement".into(),
                    help_msg: "expected semicolon after this token".into(),
                } )
            }
        }
    }
}

// expressions
impl<'src: 'tokens, 'tokens> Ast<'src, 'tokens> {
    fn operator( &mut self, ops: &[Op] ) -> Result<Option<(&'tokens Token<'src>, Op)>, RawSyntaxError> {
        let current_token = self.tokens.current.bounded( &mut self.tokens, "expected operator or semicolon" )?;
        return match current_token.kind {
            TokenKind::Op( op ) =>
                if ops.contains( &op ) {
                    self.tokens.next();
                    Ok( Some( (&current_token, op) ) )
                }
                else {
                    Ok( None )
                },
            _ => Ok( None ),
        }
    }

    fn primary_expression( &mut self ) -> Result<Expression<'src, 'tokens>, RawSyntaxError> {
        let current_token = self.tokens.current.bounded( &mut self.tokens, "expected expression" )?;
        let factor = match &current_token.kind {
            TokenKind::Literal( literal ) => Ok( Expression::Literal( literal.clone() ) ),
            TokenKind::True => Ok( Expression::Literal( Literal::Bool( true ) ) ),
            TokenKind::False => Ok( Expression::Literal( Literal::Bool( false ) ) ),
            TokenKind::Identifier( name ) => match self.resolve_type( name ) {
                None => match self.resolve_variable( name ) {
                    Some( (_, _, var) ) => Ok( Expression::Identifier( var.name, var.value.typ() ) ),
                    None => Err( RawSyntaxError {
                        col: current_token.col,
                        len: current_token.kind.len(),
                        msg: "variable not defined".into(),
                        help_msg: "was not previously defined in this scope".into(),
                    } ),
                },
                Some( _ ) => Err( RawSyntaxError {
                    col: current_token.col,
                    len: current_token.kind.len(),
                    msg: "invalid expression".into(),
                    help_msg: "cannot be a type name".into(),
                } ),
            },
            TokenKind::Bracket( BracketKind::OpenRound ) => 'parenthesis: {
                let expression_start_token = self.tokens.next().bounded( &mut self.tokens, "expected expression" )?;
                match expression_start_token.kind {
                    TokenKind::Bracket( BracketKind::CloseRound ) => Err( RawSyntaxError {
                        col: expression_start_token.col,
                        len: expression_start_token.kind.len(),
                        msg: "invalid expression".into(),
                        help_msg: "empty expressions are not allowed".into(),
                    } ),
                    _ => {
                        let expression = self.expression()?;
                        let close_bracket_token = self.tokens.current.bounded( &mut self.tokens, "expected closed parenthesis" )?;
                        match close_bracket_token.kind {
                            TokenKind::Bracket( BracketKind::CloseRound ) => Ok( expression ),
                            _ => Err( RawSyntaxError {
                                col: current_token.col,
                                len: current_token.kind.len(),
                                msg: "invalid expression".into(),
                                help_msg: "was not closed".into(),
                            } ),
                        }
                    }
                }
            },
            TokenKind::Op( Op::Minus ) => {
                let mut sign: isize = -1;
                // NOTE this optimization should be moved to later stages
                while let Some( &Token { kind: TokenKind::Op( Op::Minus ), .. } ) = self.tokens.next() {
                    sign *= -1;
                }

                let operand = self.primary_expression()?;

                // returning to avoid the call to tokens.next at the end of the function
                return match operand.typ() {
                    Type::Int | Type::Char =>
                        if sign < 0 {
                            Ok( Expression::Unary { op: Op::Minus, operand: Box::new( operand ) } )
                        }
                        else {
                            Ok( operand )
                        }
                    Type::Bool => Err( RawSyntaxError {
                        col: current_token.col,
                        len: current_token.kind.len(),
                        msg: "invalid expression".into(),
                        help_msg: "cannot negate a boolean, use the '!' operator instead to invert them".into(),
                    } ),
                    Type::Str => Err( RawSyntaxError {
                        col: current_token.col,
                        len: current_token.kind.len(),
                        msg: "invalid expression".into(),
                        help_msg: "cannot negate a string".into(),
                    } ),
                    Type::Array( _, _ ) => Err( RawSyntaxError {
                        col: current_token.col,
                        len: current_token.kind.len(),
                        msg: "invalid expression".into(),
                        help_msg: "cannot negate an array".into(),
                    } ),
                }
            },
            TokenKind::Op( Op::Not ) => {
                let mut should_be_inverted = true;
                // NOTE this optimization should be moved to later stages
                while let Some( &Token { kind: TokenKind::Op( Op::Not ), .. } ) = self.tokens.next() {
                    should_be_inverted = !should_be_inverted;
                }

                let operand = self.primary_expression()?;
                // returning to avoid the call to tokens.next at the end of the function
                return match operand.typ() {
                    Type::Int | Type::Char | Type::Bool =>
                        if should_be_inverted {
                            Ok( Expression::Unary { op: Op::Not, operand: Box::new( operand ) } )
                        }
                        else {
                            Ok( operand )
                        },
                    Type::Str => Err( RawSyntaxError {
                        col: current_token.col,
                        len: current_token.kind.len(),
                        msg: "invalid expression".into(),
                        help_msg: "cannot invert a string".into(),
                    } ),
                    Type::Array( _, _ ) => Err( RawSyntaxError {
                        col: current_token.col,
                        len: current_token.kind.len(),
                        msg: "invalid expression".into(),
                        help_msg: "cannot invert an array".into(),
                    } ),
                }
            },
            TokenKind::Definition( _ )
            | TokenKind::Print | TokenKind::PrintLn
            | TokenKind::If | TokenKind::Else
            | TokenKind::Loop | TokenKind::Break | TokenKind::Continue => Err( RawSyntaxError {
                col: current_token.col,
                len: current_token.kind.len(),
                msg: "invalid expression".into(),
                help_msg: "cannot be a keyword".into(),
            } ),
            _ => Err( RawSyntaxError {
                col: current_token.col,
                len: current_token.kind.len(),
                msg: "invalid expression".into(),
                help_msg: "expected expression operand before this token".into(),
            } ),
        };

        self.tokens.next();
        return factor;
    }

    fn exponentiative_expression( &mut self ) -> Result<Expression<'src, 'tokens>, RawSyntaxError> {
        let mut lhs = self.primary_expression()?;

        while let Some( (op_token, op) ) = self.operator( &[Op::Pow] )? {
            let rhs = self.primary_expression()?;
            lhs = match (lhs.typ(), rhs.typ()) {
                (Type::Int | Type::Char | Type::Bool, Type::Str)
                | (Type::Str, Type::Int | Type::Char | Type::Bool) => return Err( RawSyntaxError {
                    col: op_token.col,
                    len: op_token.kind.len(),
                    msg: "invalid expression".into(),
                    help_msg: "strings are not allowed inside expressions".into(),
                } ),
                _ => Expression::Binary { lhs: Box::new( lhs ), op_col: &op_token.col, op, rhs: Box::new( rhs ) },
            }
        }

        return Ok( lhs );
    }

    fn multiplicative_expression( &mut self ) -> Result<Expression<'src, 'tokens>, RawSyntaxError> {
        let mut lhs = self.exponentiative_expression()?;

        while let Some( (op_token, op) ) = self.operator( &[Op::Times, Op::Divide, Op::Remainder] )? {
            let rhs = self.exponentiative_expression()?;
            lhs = match (lhs.typ(), rhs.typ()) {
                (Type::Int | Type::Char | Type::Bool, Type::Str)
                | (Type::Str, Type::Int | Type::Char | Type::Bool) => return Err( RawSyntaxError {
                    col: op_token.col,
                    len: op_token.kind.len(),
                    msg: "invalid expression".into(),
                    help_msg: "strings are not allowed inside expressions".into(),
                } ),
                _ => Expression::Binary { lhs: Box::new( lhs ), op_col: &op_token.col, op, rhs: Box::new( rhs ) },
            }
        }

        return Ok( lhs );
    }

    fn additive_expression( &mut self ) -> Result<Expression<'src, 'tokens>, RawSyntaxError> {
        let mut lhs = self.multiplicative_expression()?;

        while let Some( (op_token, op) ) = self.operator( &[Op::Plus, Op::Minus] )? {
            let rhs = self.multiplicative_expression()?;
            lhs = match (lhs.typ(), rhs.typ()) {
                (Type::Int | Type::Char | Type::Bool, Type::Str)
                | (Type::Str, Type::Int | Type::Char | Type::Bool) => return Err( RawSyntaxError {
                    col: op_token.col,
                    len: op_token.kind.len(),
                    msg: "invalid expression".into(),
                    help_msg: "strings are not allowed inside expressions".into(),
                } ),
                _ => Expression::Binary { lhs: Box::new( lhs ), op_col: &op_token.col, op, rhs: Box::new( rhs ) },
            }
        }

        return Ok( lhs );
    }

    fn shift_expression( &mut self ) -> Result<Expression<'src, 'tokens>, RawSyntaxError> {
        let mut lhs = self.additive_expression()?;

        while let Some( (op_token, op) ) = self.operator( &[Op::LeftShift, Op::RightShift] )? {
            let rhs = self.additive_expression()?;
            lhs = match (lhs.typ(), rhs.typ()) {
                (Type::Int | Type::Char | Type::Bool, Type::Str)
                | (Type::Str, Type::Int | Type::Char | Type::Bool) => return Err( RawSyntaxError {
                    col: op_token.col,
                    len: op_token.kind.len(),
                    msg: "invalid expression".into(),
                    help_msg: "strings are not allowed inside expressions".into(),
                } ),
                _ => Expression::Binary { lhs: Box::new( lhs ), op_col: &op_token.col, op, rhs: Box::new( rhs ) },
            }
        }

        return Ok( lhs );
    }

    fn bitand_expression( &mut self ) -> Result<Expression<'src, 'tokens>, RawSyntaxError> {
        let mut lhs = self.shift_expression()?;

        while let Some( (op_token, op) ) = self.operator( &[Op::BitAnd] )? {
            let rhs = self.shift_expression()?;
            lhs = match (lhs.typ(), rhs.typ()) {
                (Type::Int | Type::Char | Type::Bool, Type::Str)
                | (Type::Str, Type::Int | Type::Char | Type::Bool) => return Err( RawSyntaxError {
                    col: op_token.col,
                    len: op_token.kind.len(),
                    msg: "invalid expression".into(),
                    help_msg: "strings are not allowed inside expressions".into(),
                } ),
                _ => Expression::Binary { lhs: Box::new( lhs ), op_col: &op_token.col, op, rhs: Box::new( rhs ) },
            }
        }

        return Ok( lhs );
    }

    fn bitxor_expression( &mut self ) -> Result<Expression<'src, 'tokens>, RawSyntaxError> {
        let mut lhs = self.bitand_expression()?;

        while let Some( (op_token, op) ) = self.operator( &[Op::BitXor] )? {
            let rhs = self.bitand_expression()?;
            lhs = match (lhs.typ(), rhs.typ()) {
                (Type::Int | Type::Char | Type::Bool, Type::Str)
                | (Type::Str, Type::Int | Type::Char | Type::Bool) => return Err( RawSyntaxError {
                    col: op_token.col,
                    len: op_token.kind.len(),
                    msg: "invalid expression".into(),
                    help_msg: "strings are not allowed inside expressions".into(),
                } ),
                _ => Expression::Binary { lhs: Box::new( lhs ), op_col: &op_token.col, op, rhs: Box::new( rhs ) },
            }
        }

        return Ok( lhs );
    }

    fn bitor_expression( &mut self ) -> Result<Expression<'src, 'tokens>, RawSyntaxError> {
        let mut lhs = self.bitxor_expression()?;

        while let Some( (op_token, op) ) = self.operator( &[Op::BitOr] )? {
            let rhs = self.bitxor_expression()?;
            lhs = match (lhs.typ(), rhs.typ()) {
                (Type::Int | Type::Char | Type::Bool, Type::Str)
                | (Type::Str, Type::Int | Type::Char | Type::Bool) => return Err( RawSyntaxError {
                    col: op_token.col,
                    len: op_token.kind.len(),
                    msg: "invalid expression".into(),
                    help_msg: "strings are not allowed inside expressions".into(),
                } ),
                _ => Expression::Binary { lhs: Box::new( lhs ), op_col: &op_token.col, op, rhs: Box::new( rhs ) },
            }
        }

        return Ok( lhs );
    }

    fn comparative_expression( &mut self ) -> Result<Expression<'src, 'tokens>, RawSyntaxError> {
        let mut lhs = self.bitor_expression()?;

        while let Some( (op_token, op) ) = self.operator( &[Op::Compare] )? {
            let rhs = self.bitor_expression()?;
            lhs = match (lhs.typ(), rhs.typ()) {
                (Type::Int | Type::Char | Type::Bool, Type::Str)
                | (Type::Str, Type::Int | Type::Char | Type::Bool) => return Err( RawSyntaxError {
                    col: op_token.col,
                    len: op_token.kind.len(),
                    msg: "invalid expression".into(),
                    help_msg: "strings are not allowed inside expressions".into(),
                } ),
                _ => Expression::Binary { lhs: Box::new( lhs ), op_col: &op_token.col, op, rhs: Box::new( rhs ) },
            }
        }

        return Ok( lhs );
    }

    fn comparison_expression( &mut self ) -> Result<Expression<'src, 'tokens>, RawSyntaxError> {
        let mut lhs = self.comparative_expression()?;

        let ops = [
            Op::EqualsEquals, Op::NotEquals,
            Op::Greater, Op::GreaterOrEquals,
            Op::Less, Op::LessOrEquals
        ];

        let mut is_chained = false;
        while let Some( (op_token, op) ) = self.operator( &ops )? {
            let rhs = self.comparative_expression()?;

            if is_chained {
                return Err( RawSyntaxError {
                    col: op_token.col,
                    len: op_token.kind.len(),
                    msg: "invalid boolean expression".into(),
                    help_msg: "comparison operators cannot be chained".into(),
                } );
            }
            is_chained = true;

            lhs = Expression::Binary { lhs: Box::new( lhs ), op_col: &op_token.col, op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn and_expression( &mut self ) -> Result<Expression<'src, 'tokens>, RawSyntaxError> {
        let mut lhs = self.comparison_expression()?;

        while let Some( (op_token, op) ) = self.operator( &[Op::And] )? {
            if lhs.typ() != Type::Bool {
                return Err( RawSyntaxError {
                    col: op_token.col,
                    len: op_token.kind.len(),
                    msg: "invalid boolean expression".into(),
                    help_msg: "must be preceded by a boolean expression".into(),
                } );
            }

            let rhs = self.comparison_expression()?;
            if rhs.typ() != Type::Bool {
                return Err( RawSyntaxError {
                    col: op_token.col,
                    len: op_token.kind.len(),
                    msg: "invalid boolean expression".into(),
                    help_msg: "must be followed by a boolean expression".into(),
                } );
            }

            lhs = Expression::Binary { lhs: Box::new( lhs ), op_col: &op_token.col, op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn or_expression( &mut self ) -> Result<Expression<'src, 'tokens>, RawSyntaxError> {
        let mut lhs = self.and_expression()?;

        while let Some( (op_token, op) ) = self.operator( &[Op::Or] )? {
            if lhs.typ() != Type::Bool {
                return Err( RawSyntaxError {
                    col: op_token.col,
                    len: op_token.kind.len(),
                    msg: "invalid boolean expression".into(),
                    help_msg: "must be preceded by a boolean expression".into(),
                } );
            }

            let rhs = self.and_expression()?;
            if rhs.typ() != Type::Bool {
                return Err( RawSyntaxError {
                    col: op_token.col,
                    len: op_token.kind.len(),
                    msg: "invalid boolean expression".into(),
                    help_msg: "must be followed by a boolean expression".into(),
                } );
            }

            lhs = Expression::Binary { lhs: Box::new( lhs ), op_col: &op_token.col, op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    // TODO implement boolean operators for strings
    // TODO disallow implicit conversions
        // IDEA introduce casting operators
    // TODO implement boolean operator chaining
    fn expression( &mut self ) -> Result<Expression<'src, 'tokens>, RawSyntaxError> {
        return self.or_expression();
    }
}

// variables and types
impl<'src: 'tokens, 'tokens> Ast<'src, 'tokens> {
    fn resolve_variable( &self, name: &'src str ) -> Option<(usize /* scope idx */, usize /* variable idx */, &Variable<'src, 'tokens>)> {
        let mut scope_idx = self.scope;
        loop {
            let scope = &self.scopes[ scope_idx ];
            for (variable_idx, variable) in scope.variables.iter().enumerate() {
                if variable.name == name {
                    return Some( (scope_idx, variable_idx, variable) );
                }
            }

            scope_idx = match scope_idx {
                0 => return None,
                _ => scope.parent,
            };
        }
    }

    fn resolve_type( &self, name: &'src str ) -> Option<&Type> {
        let mut scope_idx = self.scope;
        loop {
            let scope = &self.scopes[ scope_idx ];
            for typ in &scope.types {
                if typ.to_string() == name {
                    return Some( typ );
                }
            }

            scope_idx = match scope_idx {
                0 => return None,
                _ => scope.parent,
            };
        }
    }

    fn type_annotation( &mut self ) -> Result<Option<(&'tokens Token<'src>, Type)>, RawSyntaxError> {
        let colon_token = self.tokens.next().bounded( &mut self.tokens, "expected type annotation or variable definition" )?;
        return match &colon_token.kind {
            TokenKind::Colon => {
                let type_token = self.tokens.next().bounded( &mut self.tokens, "expected type" )?;
                match &type_token.kind {
                    TokenKind::Identifier( type_name ) => match self.resolve_type( type_name ) {
                        Some( typ ) => match self.tokens.peek_next() {
                            Some( open_square_bracket_token @ Token { kind: TokenKind::Bracket( BracketKind::OpenSquare ), .. } ) => {
                                let array_type = Box::new( typ.clone() );
                                self.tokens.next();
                                let len_token = self.tokens.next().bounded( &mut self.tokens, "expected array length" )?;
                                match len_token.kind {
                                    TokenKind::Literal( Literal::Int( len ) ) => match self.tokens.next() {
                                        Some( close_square_bracket_token @ Token { kind: TokenKind::Bracket( BracketKind::CloseSquare ), .. } ) =>
                                            Ok( Some( (close_square_bracket_token, Type::Array( array_type, len.try_into().unwrap() )) ) ),
                                        Some( _ ) | None => Err( RawSyntaxError {
                                            col: open_square_bracket_token.col,
                                            len: open_square_bracket_token.kind.len(),
                                            msg: "invalid array type".into(),
                                            help_msg: "was not closed".into(),
                                        } ),
                                    },
                                    _ => Err( RawSyntaxError {
                                        col: len_token.col,
                                        len: len_token.kind.len(),
                                        msg: "invalid array type".into(),
                                        help_msg: "must be a literal integer".into(),
                                    } )
                                }
                            },
                            Some( _ ) | None => Ok( Some( (type_token, typ.clone()) ) ),
                        },
                        None => match self.resolve_variable( type_name ) {
                            Some( (_, _, var) ) => Ok( Some( (type_token, var.value.typ()) ) ),
                            None => Err( RawSyntaxError {
                                col: type_token.col,
                                len: type_token.kind.len(),
                                msg: "invalid type annotation".into(),
                                help_msg: "was not previously defined".into(),
                            } ),
                        },
                    },
                    _ => Err( RawSyntaxError {
                        col: colon_token.col,
                        len: colon_token.kind.len(),
                        msg: "invalid type annotation".into(),
                        help_msg: "expected type name after here".into(),
                    } )
                }
            },
            _ => {
                self.tokens.token -= 1;
                Ok( None )
            },
        }
    }


    fn variable_definition( &mut self ) -> Result<Node<'src, 'tokens>, RawSyntaxError> {
        let definition_token = self.tokens.current.unwrap();
        let mutability = match definition_token.kind {
            TokenKind::Definition( kind ) => kind,
            _ => unreachable!(),
        };

        let name_token = self.tokens.next().bounded( &mut self.tokens, "expected identifier" )?;
        let name = match &name_token.kind {
            TokenKind::Identifier( name ) => match self.resolve_type( name ) {
                None => Ok( name ),
                Some( _ ) => Err( RawSyntaxError {
                    col: name_token.col,
                    len: name_token.kind.len(),
                    msg: "invalid variable name".into(),
                    help_msg: "cannot be a type name".into(),
                } ),
            },
            _ => Err( RawSyntaxError {
                col: name_token.col,
                len: name_token.kind.len(),
                msg: "invalid assignment".into(),
                help_msg: "expected variable name".into(),
            } ),
        };
        let name = name?;

        let annotation = self.type_annotation()?;

        let equals_or_semicolon_token = self.tokens.next().bounded( &mut self.tokens, "expected equals" )?;
        let expression = match equals_or_semicolon_token.kind {
            TokenKind::Op( Op::Equals ) => {
                self.tokens.next();
                match self.expression() {
                    Ok( expr ) => Ok( Some( expr ) ),
                    Err( err ) => Err( err ),
                }
            },
            TokenKind::SemiColon => Ok( None ),
            _ => match annotation {
                None => Err( RawSyntaxError {
                    col: name_token.col,
                    len: name_token.kind.len(),
                    msg: "invalid assignment".into(),
                    help_msg: "expected '=' or ';' after the variable name".into(),
                } ),
                Some( (annotation_token, _) ) => Err( RawSyntaxError {
                    col: annotation_token.col,
                    len: annotation_token.kind.len(),
                    msg: "invalid assignment".into(),
                    help_msg: "expected '=' or ';' after the type annotation".into(),
                } ),
            }
        };
        let expression = expression?;

        if self.resolve_variable( name ).is_some() {
            return Err( RawSyntaxError {
                col: name_token.col,
                len: name_token.kind.len(),
                msg: "variable redefinition".into(),
                help_msg: "was previously defined".into(),
            } );
        }

        return match expression {
            Some( value ) => {
                if let Some( (token, typ) ) = annotation {
                    if typ != value.typ() {
                        return Err( RawSyntaxError {
                            col: token.col,
                            len: token.kind.len(),
                            msg: "invalid definition".into(),
                            help_msg: format!(
                                "declared type of '{}' doesn't match expression of type '{}'",
                                typ, value.typ()
                            ).into(),
                        } );
                    }
                }

                let variables = &mut self.scopes[ self.scope ].variables;
                variables.push( Variable { mutability, name, value } );
                Ok( Node::Definition( self.scope, variables.len() - 1 ) )
            },
            None => match annotation {
                Some( (_, typ) ) => {
                    let variables = &mut self.scopes[ self.scope ].variables;
                    variables.push( Variable { mutability, name, value: typ.into() } );
                    Ok( Node::Definition( self.scope, variables.len() - 1 ) )
                },
                None => Err( RawSyntaxError {
                    col: name_token.col,
                    len: name_token.kind.len(),
                    msg: "invalid definition".into(),
                    help_msg: "expected type annotation or value after here to infer the type of the variable".into(),
                }),
            },
        }
    }

    fn variable_reassignment( &mut self ) -> Result<Node<'src, 'tokens>, RawSyntaxError> {
        let name_token = self.tokens.current.unwrap();
        let name = match name_token.kind {
            TokenKind::Identifier( name ) => name,
            _ => unreachable!(),
        };

        if self.resolve_type( name ).is_some() {
            return Err( RawSyntaxError {
                col: name_token.col,
                len: name_token.kind.len(),
                msg: "invalid assignment".into(),
                help_msg: "cannot be a type name".into(),
            } );
        }

        let op_token = self.tokens.next().unwrap();

        self.tokens.next();
        let rhs = self.expression()?;
        return match self.resolve_variable( name ) {
            Some( (scope_idx, var_idx, var) ) => match var.mutability {
                Mutability::Let => Err( RawSyntaxError {
                    col: name_token.col,
                    len: name_token.kind.len(),
                    msg: "invalid assignment".into(),
                    help_msg: "was defined as immutable".into(),
                } ),
                Mutability::Var => {
                    let value = match &op_token.kind {
                        TokenKind::Op( Op::Equals ) => rhs,
                        TokenKind::Op( op ) => Expression::Binary {
                            lhs: Box::new( Expression::Identifier( name, op.typ() ) ),
                            op_col: &op_token.col,
                            op: *op,
                            rhs: Box::new( rhs )
                        },
                        _ => unreachable!(),
                    };

                    if var.value.typ() != value.typ() {
                        return Err( RawSyntaxError {
                            col: name_token.col,
                            len: name_token.kind.len(),
                            msg: "mismatched types".into(),
                            help_msg: format!(
                                "trying to assign an expression of type '{}' to a variable of type '{}'",
                                value.typ(),
                                var.value.typ(),
                            ).into(),
                        } );
                    }

                    Ok( Node::Assignment( scope_idx, var_idx, value ) )
                },
            },
            None => Err( RawSyntaxError {
                col: name_token.col,
                len: name_token.kind.len(),
                msg: "variable redefinition".into(),
                help_msg: "was not previously defined in this scope".into(),
            }),
        }
    }
}

// print statements
impl<'src: 'tokens, 'tokens> Ast<'src, 'tokens> {
    fn print( &mut self ) -> Result<Node<'src, 'tokens>, RawSyntaxError> {
        let print_token = self.tokens.current.unwrap();
        if let TokenKind::PrintLn = print_token.kind {
            if let Some( &Token { kind: TokenKind::SemiColon, .. } ) = self.tokens.peek_next() {
                self.tokens.next();
                return Ok( Node::Println( None ) );
            }
        }

        self.tokens.next();
        let argument = self.expression()?;

        return match print_token.kind {
            TokenKind::Print => Ok( Node::Print( argument ) ),
            TokenKind::PrintLn => Ok( Node::Println( Some( argument ) ) ),
            _ => unreachable!(),
        }
    }
}

// if statements
impl<'src: 'tokens, 'tokens> Ast<'src, 'tokens> {
    fn iff( &mut self ) -> Result<Node<'src, 'tokens>, RawSyntaxError> {
        let mut if_statement = If { ifs: Vec::new(), els: None };

        'iff: while let Some( if_token ) = self.tokens.current {
            self.tokens.next().bounded( &mut self.tokens, "expected boolean expression" )?;

            let expression = self.expression()?;
            let condition = match &expression.typ() {
                Type::Bool => Ok( expression ),
                Type::Char | Type::Int | Type::Str | Type::Array( _, _ ) => Err( RawSyntaxError {
                    col: if_token.col,
                    len: if_token.kind.len(),
                    msg: "expected boolean expression".into(),
                    help_msg: "must be followed by a boolean expression".into(),
                } ),
            };

            let condition = condition?;
            let after_condition_token = self.tokens.current.bounded( &mut self.tokens, "expected do or block" )?;
            let iff = match after_condition_token.kind {
                TokenKind::Bracket( BracketKind::OpenCurly ) => {
                    let scope = self.parse_single_any()?.unwrap();
                    Ok( IfStatement { condition, statement: scope } )
                },
                TokenKind::Do => {
                    let statement = self.parse_do_single_statement()?.unwrap();
                    self.semicolon()?;
                    Ok( IfStatement { condition, statement } )
                },
                _ => {
                    let before_curly_bracket_token = self.tokens.peek_previous();
                    Err( RawSyntaxError {
                        col: before_curly_bracket_token.col,
                        len: before_curly_bracket_token.kind.len(),
                        msg: "invalid if statement".into(),
                        help_msg: "must be followed by a do or a block".into(),
                    } )
                },
            };

            if_statement.ifs.push( iff? );

            while let Some( else_token ) = self.tokens.current {
                let after_else_token = match else_token.kind {
                    TokenKind::Else => self.tokens.next().bounded( &mut self.tokens, "expected do, block or if statement" )?,
                    _ => break 'iff,
                };

                // we are now inside an else branch
                let else_if = match after_else_token.kind {
                    TokenKind::Bracket( BracketKind::OpenCurly ) => {
                        let scope = self.parse_single_any()?.unwrap();
                        if_statement.els = Some( Box::new( scope ) );
                        break 'iff;
                    },
                    TokenKind::Do => {
                        let statement = self.parse_do_single_statement()?.unwrap();
                        self.semicolon()?;
                        if_statement.els = Some( Box::new( statement ) );
                        break 'iff;
                    },
                    TokenKind::If => break,
                    _ => Err( RawSyntaxError {
                        col: else_token.col,
                        len: else_token.kind.len(),
                        msg: "invalid else statement".into(),
                        help_msg: "must be followed by a do, a block or an if statement".into(),
                    } ),
                };

                else_if?;
            }
        }

        return Ok( Node::If( if_statement ) );
    }
}

// for statements
impl<'src: 'tokens, 'tokens> Ast<'src, 'tokens>{
    fn loop_statement( &mut self ) -> Result<Node<'src, 'tokens>, RawSyntaxError> {
        let do_token = self.tokens.current.unwrap();
        let loop_token = match do_token.kind {
            TokenKind::Do => self.tokens.next().bounded( &mut self.tokens, "expected loop statement" )?,
            _ => do_token,
        };

        self.tokens.next().bounded( &mut self.tokens, "expected boolean expression" )?;
        let expression = self.expression()?;
        let condition = match &expression.typ() {
            Type::Bool => Ok( expression ),
            Type::Char | Type::Int | Type::Str | Type::Array( _, _ ) => Err( RawSyntaxError {
                col: loop_token.col,
                len: loop_token.kind.len(),
                msg: "expected boolean expression".into(),
                help_msg: "must be followed by a boolean expression".into(),
            } ),
        };

        let after_condition_token = self.tokens.current.bounded( &mut self.tokens, "expected do or block" )?;
        let statement = match after_condition_token.kind {
            TokenKind::Bracket( BracketKind::OpenCurly ) => {
                let scope = self.parse_single_any()?.unwrap();
                Ok( scope )
            },
            TokenKind::Do => {
                let statement = self.parse_do_single_statement()?.unwrap();
                self.semicolon()?;
                Ok( statement )
            },
            _ => {
                let before_curly_bracket_token = self.tokens.peek_previous();
                Err( RawSyntaxError {
                    col: before_curly_bracket_token.col,
                    len: before_curly_bracket_token.kind.len(),
                    msg: "invalid for statement".into(),
                    help_msg: "must be followed by a do or a block".into(),
                } )
            },
        };

        let condition = condition?;
        let statement = statement?;
        let condition = if let TokenKind::Do = do_token.kind {
            LoopCondition::Post( Expression::Unary { op: Op::Not, operand: Box::new( condition ) } )
        }
        else {
            LoopCondition::Pre( condition )
        };

        return Ok( Node::Loop( Loop { condition, statement: Box::new( statement ) } ) );
    }
}
