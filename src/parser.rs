use std::fmt::Display;

use crate::{lexer::*, errors::*};


#[derive( Debug )]
struct TokenIter<'tokens, 'src: 'tokens> {
    token: usize,
    tokens: &'tokens [Token<'src>],
}

impl<'tokens, 'src: 'tokens> TokenIter<'tokens, 'src> {
    // TODO remove this method and make the next/previous methods return their current position and
    // then move (i.e. like a normal iterator)
        // NOTE its going to be required to pass a position object around instead of calling current
        // when needed, or store the current position and refer to that
    fn current( &mut self ) -> Option<&'tokens Token<'src>> {
        if self.token >= self.tokens.len() {
            return None;
        }

        return Some( &self.tokens[ self.token ] ).or_next( self );
    }

    fn next( &mut self ) -> Option<&'tokens Token<'src>> {
        if self.token >= self.tokens.len() - 1 {
            self.token += 1;
            return None;
        }

        self.token += 1;
        return Some( &self.tokens[ self.token ] ).or_next( self );
    }

    fn previous( &mut self ) -> Option<&'tokens Token<'src>> {
        if self.token == 0 {
            return None;
        }

        self.token -= 1;
        return Some( &self.tokens[ self.token ] ).or_previous( self );
    }

    fn peek_next( &mut self ) -> Option<&'tokens Token<'src>> {
        let starting_token = self.token;
        let next = self.next();
        self.token = starting_token;
        return next;
    }

    fn peek_previous( &mut self ) -> Option<&'tokens Token<'src>> {
        let starting_token = self.token;
        let previous = self.previous();
        self.token = starting_token;
        return previous;
    }
}

trait BoundedToken<'tokens, 'src: 'tokens> {
    type Error;

    fn bounded( self, tokens: &mut TokenIter<'tokens, 'src>, err_msg: impl Into<String> ) -> Result<&'tokens Token<'src>, Self::Error>;
    fn or_next( self, tokens: &mut TokenIter<'tokens, 'src> ) -> Self;
    fn or_previous( self, tokens: &mut TokenIter<'tokens, 'src> ) -> Self;
}

impl<'tokens, 'src: 'tokens> BoundedToken<'tokens, 'src> for Option<&'tokens Token<'src>> {
    type Error = SyntaxError;

    fn bounded( self, tokens: &mut TokenIter<'tokens, 'src>, err_msg: impl Into<String> ) -> Result<&'tokens Token<'src>, Self::Error> {
        return match self {
            Some( token ) => Ok( token ),
            None => {
                // this function is never called without a previous token, so we can safely unwrap
                let previous = unsafe{ tokens.peek_previous().unwrap_unchecked() };
                Err( SyntaxError {
                    col: previous.col,
                    len: previous.kind.len(),
                    msg: err_msg.into().into(),
                    help_msg: "file ended after here instead".into(),
                } )
            },
        }
    }

    fn or_next( self, tokens: &mut TokenIter<'tokens, 'src> ) -> Self {
        return match self?.kind {
            TokenKind::Comment( _ ) => tokens.next(),
            _ => self,
        }
    }

    fn or_previous( self, tokens: &mut TokenIter<'tokens, 'src> ) -> Self {
        return match self?.kind {
            TokenKind::Comment( _ ) => tokens.previous(),
            _ => self,
        }
    }
}


#[derive( Debug, Clone )]
pub(crate) enum Expression<'src> {
    Literal( Literal ),
    Unary { op: Operator, operand: Box<Expression<'src>> },
    Binary { lhs: Box<Expression<'src>>, op: Operator, rhs: Box<Expression<'src>> },
    Identifier( &'src str, Type ),
}

impl Display for Expression<'_> {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Literal( literal )      => write!( f, "{}", literal ),
            Self::Unary { op, operand }   => write!( f, "{}{}", op, operand ),
            Self::Binary { lhs, op, rhs } => write!( f, "({} {} {})", lhs, op, rhs ),
            Self::Identifier( name, _ )   => write!( f, "{}", name ),
        }
    }
}

impl TypeOf for Expression<'_> {
    fn typ( &self ) -> Type {
        return match self {
            Self::Literal( literal )    => literal.typ(),
            Self::Unary { operand, .. } => operand.typ(),
            Self::Binary { op, .. }     => op.typ(),
            Self::Identifier( _, typ )  => *typ,
        }
    }
}

#[derive( Debug, Clone )]
pub(crate) struct Variable<'src> {
    pub(crate) mutability: Mutability,
    pub(crate) name: &'src str,
    pub(crate) value: Expression<'src>,
}

#[derive( Debug, Clone )]
pub(crate) struct IfStatement<'src> {
    pub(crate) condition: Expression<'src>,
    pub(crate) statement: Node<'src>,
}

impl Display for IfStatement<'_> {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return write!( f, "if {}", self.condition );
    }
}

#[derive( Debug, Clone )]
pub(crate) struct If<'src> {
    pub(crate) ifs: Vec<IfStatement<'src>>,
    pub(crate) els: Option<Box<Node<'src>>>,
}

#[allow( dead_code )]
#[derive( Debug, Clone )]
pub(crate) enum LoopCondition<'src> {
    Infinite,
    Pre( Expression<'src> ),
    Post( Expression<'src> )
}

#[allow( dead_code )]
#[derive( Debug, Clone )]
pub(crate) struct Loop<'src> {
    pub(crate) pre: Option<Box<Node<'src>>>,
    pub(crate) condition: LoopCondition<'src>,
    pub(crate) post: Option<Box<Node<'src>>>,
    pub(crate) statement: Box<Node<'src>>,
}

impl Display for Loop<'_> {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match &self.condition {
            LoopCondition::Infinite          => write!( f, "loop" ),
            LoopCondition::Pre( condition )  => write!( f, "loop {}", condition ),
            LoopCondition::Post( condition ) => write!( f, "do loop {}", condition ),
        }
    }
}

// TODO create node struct that contains the line and token of the start of the node
#[derive( Debug, Clone )]
pub(crate) enum Node<'src> {
    Empty,

    Expression( Expression<'src> ),
    Print( Expression<'src> ),
    Println( Option<Expression<'src>> ),
    If( If<'src> ),
    Loop( Loop<'src> ),
    Break,
    Continue,

    Definition( usize /* scope idx */, usize /* variable idx */ ),
    Assignment( usize /* scope idx */, usize /* variable idx */, Expression<'src> ),
    Scope( usize ),
}

impl Display for Node<'_> {
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
pub(crate) struct Scope<'src> {
    pub(crate) parent: usize,
    pub(crate) types: Vec<Type>,
    pub(crate) variables: Vec<Variable<'src>>,
    pub(crate) nodes: Vec<Node<'src>>,
}

// IDEA create Parser class that builds the AST, and then validate the AST afterwards
#[derive( Debug )]
pub(crate) struct Ast<'tokens, 'ast, 'src: 'ast> {
    scopes: Vec<Scope<'ast>>,
    scope: usize,
    loop_depth: usize,

    tokens: TokenIter<'tokens, 'src>,
    errors: Vec<SyntaxError>,
}

impl<'tokens, 'ast, 'src: 'ast> Ast<'tokens, 'ast, 'src> {
    pub(crate) fn build( tokens: &'tokens [Token<'src>] ) -> Result<Vec<Scope<'ast>>, Vec<SyntaxError>> {
        if tokens.is_empty() {
            return Ok( Vec::new() );
        }

        let mut this = Self {
            scopes: vec![Scope {
                parent: 0,
                types: vec![Type::Int, Type::Char, Type::Bool, Type::Str],
                variables: Vec::new(),
                nodes: Vec::new(),
            }],
            scope: 0,
            tokens: TokenIter { token: 0, tokens },
            loop_depth: 0,
            errors: Vec::new()
        };

        this.parse_scope();

        return match this.errors.is_empty() {
            true => Ok( this.scopes ),
            false => Err( this.errors ),
        }
    }
}

// scopes
impl<'tokens, 'ast, 'src: 'ast> Ast<'tokens, 'ast, 'src> {
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
                                self.errors.push( err );

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
                    self.errors.push( err );

                    // consuming all remaining tokens until the end of the file
                    self.tokens.token = self.tokens.tokens.len();
                    break;
                },
            }
        }
    }

    fn parse_single_statement( &mut self ) -> Result<Option<Node<'ast>>, SyntaxError> {
        let current_token = match self.tokens.current() {
            Some( token ) => token,
            None => return Ok( None ),
        };

        return match current_token.kind {
            TokenKind::Literal( _ )
            | TokenKind::True | TokenKind::False
            | TokenKind::Bracket( BracketKind::OpenRound )
            | TokenKind::Op( Operator::Minus | Operator::Not )
            | TokenKind::Identifier( _ )          => Ok( Some( self.variable_reassignment_or_expression()? ) ),
            TokenKind::Definition( _ )            => Ok( Some( self.variable_definition()? ) ),
            TokenKind::Print | TokenKind::PrintLn => Ok( Some( self.print()? ) ),
            TokenKind::If                         => Ok( Some( self.iff()? ) ),
            TokenKind::Else => {
                self.tokens.next();
                Err( SyntaxError {
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
                    0 => Err( SyntaxError {
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
                    0 => Err( SyntaxError {
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
                Err( SyntaxError {
                    col: current_token.col,
                    len: current_token.kind.len(),
                    msg: "invalid statement".into(),
                    help_msg: "blocks are not allowed in this context".into(),
                } )
            },
            TokenKind::Bracket( BracketKind::CloseCurly ) => {
                self.tokens.next();
                Err( SyntaxError {
                    col: current_token.col,
                    len: current_token.kind.len(),
                    msg: "invalid statement".into(),
                    help_msg: "stray closed curly bracket".into(),
                } )
            },
            TokenKind::Bracket( BracketKind::CloseRound ) => {
                self.tokens.next();
                Err( SyntaxError {
                    col: current_token.col,
                    len: current_token.kind.len(),
                    msg: "invalid expression".into(),
                    help_msg: "stray closed parenthesis".into(),
                } )
            },
            TokenKind::Colon => {
                self.tokens.next();
                Err( SyntaxError {
                    col: current_token.col,
                    len: current_token.kind.len(),
                    msg: "invalid type annotation".into(),
                    help_msg: "stray colon".into(),
                } )
            },
            TokenKind::Equals => {
                self.tokens.next();
                Err( SyntaxError {
                    col: current_token.col,
                    len: current_token.kind.len(),
                    msg: "invalid assignment".into(),
                    help_msg: "stray assignment".into(),
                } )
            },
            TokenKind::Op( _ ) => {
                self.tokens.next();
                Err( SyntaxError {
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

    fn parse_do_single_statement( &mut self ) -> Result<Option<Node<'ast>>, SyntaxError> {
        let current_token = self.tokens.next().bounded( &mut self.tokens, "expected statement" )?;
        return match current_token.kind {
            TokenKind::Bracket( BracketKind::OpenCurly ) => {
                self.tokens.next();
                Err( SyntaxError {
                    col: current_token.col,
                    len: current_token.kind.len(),
                    msg: "invalid statement".into(),
                    help_msg: "blocks are not allowed in do statements".into(),
                } )
            },
            TokenKind::Definition( _ ) => {
                self.tokens.next();
                Err( SyntaxError {
                    col: current_token.col,
                    len: current_token.kind.len(),
                    msg: "invalid statement".into(),
                    help_msg: "variable definitions are not allowed in do statements".into(),
                } )
            },
            _ => self.parse_single_statement(),
        }
    }

    fn parse_single_any( &mut self ) -> Result<Option<Node<'ast>>, SyntaxError> {
        let current_token = match self.tokens.current() {
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
impl<'tokens, 'ast, 'src: 'ast> Ast<'tokens, 'ast, 'src> {
    fn semicolon( &mut self ) -> Result<(), SyntaxError> {
        let semicolon_token = self.tokens.current().bounded( &mut self.tokens, "expected semicolon" )?;
        return match &semicolon_token.kind {
            TokenKind::SemiColon => {
                self.tokens.next();
                Ok( () )
            },
            _ => {
                // this function is never called without a previous token, so we can safely unwrap
                let previous_token = unsafe{ self.tokens.peek_previous().unwrap_unchecked() };
                Err( SyntaxError {
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
impl<'tokens, 'ast, 'src: 'ast> Ast<'tokens, 'ast, 'src> {
    fn operator( &mut self, ops: &[Operator] ) -> Result<Option<Operator>, SyntaxError> {
        let current_token = self.tokens.current().bounded( &mut self.tokens, "expected operator or semicolon" )?;
        return match current_token.kind {
            TokenKind::Op( op ) =>
                if ops.contains( &op ) {
                    self.tokens.next();
                    Ok( Some( op ) )
                }
                else {
                    Ok( None )
                },
            _ => Ok( None ),
        }
    }

    fn primary_expression( &mut self ) -> Result<Expression<'ast>, SyntaxError> {
        let current_token = self.tokens.current().bounded( &mut self.tokens, "expected expression" )?;
        let factor = match &current_token.kind {
            TokenKind::Literal( literal ) => Ok( Expression::Literal( literal.clone() ) ),
            TokenKind::True => Ok( Expression::Literal( Literal::Bool( true ) ) ),
            TokenKind::False => Ok( Expression::Literal( Literal::Bool( false ) ) ),
            TokenKind::Identifier( name ) => match self.resolve_type( name ) {
                None => match self.resolve_variable( name ) {
                    Some( (variable, _, _) ) =>
                        Ok( Expression::Identifier( variable.name, variable.value.typ() ) ),
                    None => Err( SyntaxError {
                        col: current_token.col,
                        len: current_token.kind.len(),
                        msg: "variable not defined".into(),
                        help_msg: "was not previously defined in this scope".into(),
                    } ),
                },
                Some( _ ) => Err( SyntaxError {
                    col: current_token.col,
                    len: current_token.kind.len(),
                    msg: "invalid expression".into(),
                    help_msg: "cannot be a type name".into(),
                } ),
            },
            TokenKind::Bracket( BracketKind::OpenRound ) => {
                let expression_start_token = self.tokens.next().bounded( &mut self.tokens, "expected expression" )?;
                match expression_start_token.kind {
                    TokenKind::Bracket( BracketKind::CloseRound ) => Err( SyntaxError {
                        col: expression_start_token.col,
                        len: expression_start_token.kind.len(),
                        msg: "invalid expression".into(),
                        help_msg: "empty expressions are not allowed".into(),
                    } ),
                    _ => {
                        let expression = self.expression()?;
                        let close_bracket_token = self.tokens.current().bounded( &mut self.tokens, "expected closed parenthesis" )?;
                        match close_bracket_token.kind {
                            TokenKind::Bracket( BracketKind::CloseRound ) => Ok( expression ),
                            _ => Err( SyntaxError {
                                col: current_token.col,
                                len: current_token.kind.len(),
                                msg: "invalid expression".into(),
                                help_msg: "unclosed parenthesis".into(),
                            } ),
                        }
                    }
                }
            },
            TokenKind::Op( Operator::Minus ) => {
                let mut sign: isize = -1;
                // NOTE this optimization should be moved to later stages
                while let Some( &Token { kind: TokenKind::Op( Operator::Minus ), .. } ) = self.tokens.next() {
                    sign *= -1;
                }

                let operand = self.primary_expression()?;

                // returning to avoid the call to tokens.next at the end of the function
                return match operand.typ() {
                    Type::Bool => Err( SyntaxError {
                        col: current_token.col,
                        len: current_token.kind.len(),
                        msg: "invalid expression".into(),
                        help_msg: "cannot negate boolean values, use the '!' operator instead to invert them".into(),
                    } ),
                    Type::Int | Type::Char | Type::Str =>
                        if sign < 0 {
                            Ok( Expression::Unary { op: Operator::Minus, operand: Box::new( operand ) } )
                        }
                        else {
                            Ok( operand )
                        }
                }
            },
            TokenKind::Op( Operator::Not ) => {
                let mut should_be_inverted = true;
                // NOTE this optimization should be moved to later stages
                while let Some( &Token { kind: TokenKind::Op( Operator::Not ), .. } ) = self.tokens.next() {
                    should_be_inverted = !should_be_inverted;
                }

                let operand = self.primary_expression()?;
                // returning to avoid the call to tokens.next at the end of the function
                return if should_be_inverted {
                    Ok( Expression::Unary { op: Operator::Not, operand: Box::new( operand ) } )
                }
                else {
                    Ok( operand )
                }
            },
            TokenKind::Definition( _ )
            | TokenKind::Print | TokenKind::PrintLn
            | TokenKind::If | TokenKind::Else
            | TokenKind::Loop | TokenKind::Break | TokenKind::Continue => Err( SyntaxError {
                col: current_token.col,
                len: current_token.kind.len(),
                msg: "invalid expression".into(),
                help_msg: "cannot be a keyword".into(),
            } ),
            _ => Err( SyntaxError {
                col: current_token.col,
                len: current_token.kind.len(),
                msg: "invalid expression".into(),
                help_msg: "expected expression operand before this token".into(),
            } ),
        };

        self.tokens.next();
        return factor;
    }

    fn exponentiative_expression( &mut self ) -> Result<Expression<'ast>, SyntaxError> {
        let mut lhs = self.primary_expression()?;

        while let Some( op ) = self.operator( &[Operator::Pow] )? {
            let rhs = self.primary_expression()?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn multiplicative_expression( &mut self ) -> Result<Expression<'ast>, SyntaxError> {
        let mut lhs = self.exponentiative_expression()?;

        while let Some( op ) = self.operator( &[Operator::Times, Operator::Divide, Operator::Remainder] )? {
            let rhs = self.exponentiative_expression()?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn additive_expression( &mut self ) -> Result<Expression<'ast>, SyntaxError> {
        let mut lhs = self.multiplicative_expression()?;

        while let Some( op ) = self.operator( &[Operator::Plus, Operator::Minus] )? {
            let rhs = self.multiplicative_expression()?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn shift_expression( &mut self ) -> Result<Expression<'ast>, SyntaxError> {
        let mut lhs = self.additive_expression()?;

        while let Some( op ) = self.operator( &[Operator::LeftShift, Operator::RightShift] )? {
            let rhs = self.additive_expression()?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn bitand_expression( &mut self ) -> Result<Expression<'ast>, SyntaxError> {
        let mut lhs = self.shift_expression()?;

        while let Some( op ) = self.operator( &[Operator::BitAnd] )? {
            let rhs = self.shift_expression()?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn bitxor_expression( &mut self ) -> Result<Expression<'ast>, SyntaxError> {
        let mut lhs = self.bitand_expression()?;

        while let Some( op ) = self.operator( &[Operator::BitXor] )? {
            let rhs = self.bitand_expression()?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn bitor_expression( &mut self ) -> Result<Expression<'ast>, SyntaxError> {
        let mut lhs = self.bitxor_expression()?;

        while let Some( op ) = self.operator( &[Operator::BitOr] )? {
            let rhs = self.bitxor_expression()?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn comparative_expression( &mut self ) -> Result<Expression<'ast>, SyntaxError> {
        let mut lhs = self.bitor_expression()?;

        while let Some( op ) = self.operator( &[Operator::Compare] )? {
            let rhs = self.bitor_expression()?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn comparison_expression( &mut self ) -> Result<Expression<'ast>, SyntaxError> {
        let mut lhs = self.comparative_expression()?;

        let ops = [
            Operator::EqualsEquals, Operator::NotEquals,
            Operator::Greater, Operator::GreaterOrEquals,
            Operator::Less, Operator::LessOrEquals
        ];

        let mut is_chained = false;
        while let Some( op ) = self.operator( &ops )? {
            let op_token = self.tokens.peek_previous().unwrap();
            let rhs = self.comparative_expression()?;

            if is_chained {
                return Err( SyntaxError {
                    col: op_token.col,
                    len: op_token.kind.len(),
                    msg: "invalid boolean expression".into(),
                    help_msg: "comparison operators cannot be chained".into(),
                } );
            }
            is_chained = true;

            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn and_expression( &mut self ) -> Result<Expression<'ast>, SyntaxError> {
        let mut lhs = self.comparison_expression()?;

        while let Some( op ) = self.operator( &[Operator::And] )? {
            let op_token = self.tokens.peek_previous().unwrap();

            if lhs.typ() != Type::Bool {
                return Err( SyntaxError {
                    col: op_token.col,
                    len: op_token.kind.len(),
                    msg: "invalid boolean expression".into(),
                    help_msg: "must be preceded by a boolean expression".into(),
                } );
            }

            let rhs = self.comparison_expression()?;
            if rhs.typ() != Type::Bool {
                return Err( SyntaxError {
                    col: op_token.col,
                    len: op_token.kind.len(),
                    msg: "invalid boolean expression".into(),
                    help_msg: "must be followed by a boolean expression".into(),
                } );
            }

            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    // pub(crate) fn xor_expression( &mut self ) -> Result<Expression, SyntaxError> {
    //     let mut lhs = self.and_expression( tokens )?;

    //     while let Some( op ) = self.operator( tokens, &[Operator::Xor] )? {
    //         let op_pos = tokens.peek_previous().unwrap();

    //         if lhs.typ() != Type::Bool {
    //             return Err( SyntaxError {
    //                 line_byte_start: op_pos.line.byte_start,
    //                 line: op_pos.line.number,
    //                 col: op_pos.token.col,
    //                 len: op_pos.token.kind.len(),
    //                 msg: "invalid boolean expression".into(),
    //                 help_msg: "must be preceded by a boolean expression".into(),
    //             } );
    //         }

    //         let rhs = self.and_expression( tokens )?;
    //         if rhs.typ() != Type::Bool {
    //             return Err( SyntaxError {
    //                 line_byte_start: op_pos.line.byte_start,
    //                 line: op_pos.line.number,
    //                 col: op_pos.token.col,
    //                 len: op_pos.token.kind.len(),
    //                 msg: "invalid boolean expression".into(),
    //                 help_msg: "must be followed by a boolean expression".into(),
    //             } );
    //         }

    //         lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
    //     }

    //     return Ok( lhs );
    // }

    fn or_expression( &mut self ) -> Result<Expression<'ast>, SyntaxError> {
        let mut lhs = self.and_expression()?;

        while let Some( op ) = self.operator( &[Operator::Or] )? {
            let op_token = self.tokens.peek_previous().unwrap();

            if lhs.typ() != Type::Bool {
                return Err( SyntaxError {
                    col: op_token.col,
                    len: op_token.kind.len(),
                    msg: "invalid boolean expression".into(),
                    help_msg: "must be preceded by a boolean expression".into(),
                } );
            }

            let rhs = self.and_expression()?;
            if rhs.typ() != Type::Bool {
                return Err( SyntaxError {
                    col: op_token.col,
                    len: op_token.kind.len(),
                    msg: "invalid boolean expression".into(),
                    help_msg: "must be followed by a boolean expression".into(),
                } );
            }

            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    // TODO implement boolean operators for strings
    // TODO disallow implicit conversions (str + i64, char + i64, str + char or str + str
        // IDEA introduce casting operators
    // TODO implement boolean operator chaining
    fn expression( &mut self ) -> Result<Expression<'ast>, SyntaxError> {
        return self.or_expression();
    }
}

// variable definitions and assignments
impl<'tokens, 'ast, 'src: 'ast> Ast<'tokens, 'ast, 'src> {
    fn resolve_variable( &self, name: &'src str ) -> Option<(&Variable<'ast>, usize /* scope idx */, usize /* variable idx */)> {
        let mut current_scope = self.scope;
        loop {
            let scope = &self.scopes[ current_scope ];

            for (current_variable, variable) in scope.variables.iter().enumerate() {
                if variable.name == name {
                    return Some( (variable, current_scope, current_variable) );
                }
            }

            if current_scope == 0 && scope.parent == 0 {
                return None;
            }

            current_scope = scope.parent;
        }
    }

    fn resolve_type( &self, name: &'src str ) -> Option<&Type> {
        let mut current_scope = self.scope;
        loop {
            let scope = &self.scopes[ current_scope ];

            for typ in &scope.types {
                if typ.to_string() == name {
                    return Some( typ );
                }
            }

            if current_scope == 0 && scope.parent == 0 {
                return None;
            }

            current_scope = scope.parent;
        }
    }


    fn variable_definition( &mut self ) -> Result<Node<'ast>, SyntaxError> {
        let definition_token = self.tokens.current().unwrap();
        let mutability = match definition_token.kind {
            TokenKind::Definition( kind ) => kind,
            _ => unreachable!(),
        };

        let name_token = self.tokens.next().bounded( &mut self.tokens, "expected identifier" )?;
        let name = match &name_token.kind {
            TokenKind::Identifier( name ) => match self.resolve_type( name ) {
                None => Ok( name ),
                Some( _ ) => Err( SyntaxError {
                    col: name_token.col,
                    len: name_token.kind.len(),
                    msg: "invalid variable name".into(),
                    help_msg: "cannot be a type name".into(),
                } ),
            },
            _ => Err( SyntaxError {
                col: name_token.col,
                len: name_token.kind.len(),
                msg: "invalid assignment".into(),
                help_msg: "expected variable name".into(),
            } ),
        };

        let colon_token = self.tokens.peek_next().bounded( &mut self.tokens, "expected type annotation or variable definition" )?;
        let annotation = match &colon_token.kind {
            TokenKind::Colon => {
                self.tokens.next();
                let annotation_token = self.tokens.next().bounded( &mut self.tokens, "expected type" )?;
                match &annotation_token.kind {
                    TokenKind::Identifier( type_name ) => match self.resolve_type( type_name ) {
                        Some( typ ) => Ok( Some( (*typ, annotation_token) ) ),
                        None => match self.resolve_variable( type_name ) {
                            Some( (var, _, _) ) => Ok( Some( (var.value.typ(), annotation_token) ) ),
                            None => Err( SyntaxError {
                                col: annotation_token.col,
                                len: annotation_token.kind.len(),
                                msg: "invalid type annotation".into(),
                                help_msg: "was not previously defined".into(),
                            } )
                        },
                    },
                    _ => Err( SyntaxError {
                        col: colon_token.col,
                        len: colon_token.kind.len(),
                        msg: "invalid type annotation".into(),
                        help_msg: "expected type name after here".into(),
                    } )
                }
            },
            _ => Ok( None ),
        };

        let equals_or_semicolon_token = self.tokens.next().bounded( &mut self.tokens, "expected equals" )?;
        let expression = match equals_or_semicolon_token.kind {
            TokenKind::Equals => {
                self.tokens.next().bounded( &mut self.tokens, "expected expression" )?;
                match self.expression() {
                    Ok( expr ) => Ok( Some( expr ) ),
                    Err( err ) => Err( err ),
                }
            },
            TokenKind::SemiColon => Ok( None ),
            _ => Err( SyntaxError {
                col: name_token.col,
                len: name_token.kind.len(),
                msg: "invalid assignment".into(),
                help_msg: "expected '=' or ';' after the variable name".into(),
            } ),
        };

        let name = name?;
        let annotation = annotation?;
        let expression = expression?;

        if self.resolve_variable( name ).is_some() {
            return Err( SyntaxError {
                col: name_token.col,
                len: name_token.kind.len(),
                msg: "variable redefinition".into(),
                help_msg: "was previously defined".into(),
            } );
        }

        return match expression {
            Some( value ) => {
                if let Some( (typ, token) ) = annotation {
                    if typ != value.typ() {
                        return Err( SyntaxError {
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
                Some( (typ, _) ) => {
                    let variables = &mut self.scopes[ self.scope ].variables;
                    variables.push( Variable { mutability, name, value: Expression::Literal( typ.default() ) } );
                    Ok( Node::Definition( self.scope, variables.len() - 1 ) )
                },
                None => Err( SyntaxError {
                    col: name_token.col,
                    len: name_token.kind.len(),
                    msg: "invalid definition".into(),
                    help_msg: "expected type annotation or value after here to infer the type of the variable".into(),
                }),
            },
        }
    }

    fn variable_reassignment_or_expression( &mut self ) -> Result<Node<'ast>, SyntaxError> {
        let name_token = self.tokens.current().unwrap();
        let op_token = match self.tokens.peek_next() {
            Some( token ) => match token.kind {
                TokenKind::Equals
                | TokenKind::Op(
                    Operator::PowEquals
                    | Operator::TimesEquals
                    | Operator::DivideEquals
                    | Operator::PlusEquals
                    | Operator::MinusEquals
                    | Operator::AndEquals | Operator::BitAndEquals
                    | Operator::OrEquals | Operator::BitOrEquals
                    /* | Operator::XorEquals */ | Operator::BitXorEquals
                    | Operator::LeftShiftEquals | Operator::RightShiftEquals
                ) => token,
                _ => return Ok( Node::Expression( self.expression()? ) ),
            },
            None => return Ok( Node::Expression( self.expression()? ) ),
        };

        self.tokens.next();
        self.tokens.next().bounded( &mut self.tokens, "expected expression" )?;

        let name = match name_token.kind {
            TokenKind::Identifier( name ) => name,
            _ => unreachable!(),
        };

        if self.resolve_type( name ).is_some() {
            return Err( SyntaxError {
                col: name_token.col,
                len: name_token.kind.len(),
                msg: "invalid assignment".into(),
                help_msg: "cannot be a type name".into(),
            } );
        }

        let rhs = self.expression()?;
        match self.resolve_variable( name ) {
            Some( (var, scope, var_idx) ) => match var.mutability {
                Mutability::Let => Err( SyntaxError {
                    col: name_token.col,
                    len: name_token.kind.len(),
                    msg: "invalid assignment".into(),
                    help_msg: "was defined as immutable".into(),
                } ),
                Mutability::Var => {
                    let value = match &op_token.kind {
                        TokenKind::Equals => rhs,
                        TokenKind::Op( op ) => {
                            let lhs = Expression::Identifier( name, op.typ() );
                            Expression::Binary { lhs: Box::new( lhs ), op: *op, rhs: Box::new( rhs ) }
                        },
                        _ => unreachable!(),
                    };

                    if var.value.typ() != value.typ() {
                        return Err( SyntaxError {
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

                    Ok( Node::Assignment( scope, var_idx, value ) )
                },
            },
            None => Err( SyntaxError {
                col: name_token.col,
                len: name_token.kind.len(),
                msg: "variable redefinition".into(),
                help_msg: "was not previously defined in this scope".into(),
            }),
        }
    }
}

// print statements
impl<'tokens, 'ast, 'src: 'ast> Ast<'tokens, 'ast, 'src> {
    fn print( &mut self ) -> Result<Node<'ast>, SyntaxError> {
        let print_token = self.tokens.current().unwrap();
        if let TokenKind::PrintLn = print_token.kind {
            if let Some( &Token { kind: TokenKind::SemiColon, .. } ) = self.tokens.peek_next() {
                self.tokens.next();
                return Ok( Node::Println( None ) );
            }
        }

        self.tokens.next().bounded( &mut self.tokens, "expected expression" )?;
        let argument = self.expression()?;

        return match print_token.kind {
            TokenKind::Print => Ok( Node::Print( argument ) ),
            TokenKind::PrintLn => Ok( Node::Println( Some( argument ) ) ),
            _ => unreachable!(),
        }
    }
}

// if statements
impl<'tokens, 'ast, 'src: 'ast> Ast<'tokens, 'ast, 'src> {
    fn iff( &mut self ) -> Result<Node<'ast>, SyntaxError> {
        let mut if_statement = If { ifs: Vec::new(), els: None };

        'iff: while let Some( if_token ) = self.tokens.current() {
            self.tokens.next().bounded( &mut self.tokens, "expected boolean expression" )?;

            let expression = self.expression()?;
            let condition = match &expression.typ() {
                Type::Bool => Ok( expression ),
                Type::Char | Type::Int | Type::Str => Err( SyntaxError {
                    col: if_token.col,
                    len: if_token.kind.len(),
                    msg: "expected boolean expression".into(),
                    help_msg: "must be followed by a boolean expression".into(),
                } ),
            };

            let condition = condition?;
            let after_condition_token = self.tokens.current().bounded( &mut self.tokens, "expected do or block" )?;
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
                    let before_curly_bracket_token = self.tokens.peek_previous().unwrap();
                    Err( SyntaxError {
                        col: before_curly_bracket_token.col,
                        len: before_curly_bracket_token.kind.len(),
                        msg: "invalid if statement".into(),
                        help_msg: "must be followed by a do or a block".into(),
                    } )
                },
            };

            if_statement.ifs.push( iff? );

            while let Some( else_token ) = self.tokens.current() {
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
                    _ => Err( SyntaxError {
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
impl<'tokens, 'ast, 'src: 'ast> Ast<'tokens, 'ast, 'src>{
    fn loop_statement( &mut self ) -> Result<Node<'ast>, SyntaxError> {
        let do_token = self.tokens.current().unwrap();
        let loop_token = match do_token.kind {
            TokenKind::Do => self.tokens.next().bounded( &mut self.tokens, "expected loop statement" )?,
            _ => do_token,
        };

        self.tokens.next().bounded( &mut self.tokens, "expected boolean expression" )?;
        let expression = self.expression()?;
        let condition = match &expression.typ() {
            Type::Bool => Ok( expression ),
            Type::Char | Type::Int | Type::Str => Err( SyntaxError {
                col: loop_token.col,
                len: loop_token.kind.len(),
                msg: "expected boolean expression".into(),
                help_msg: "must be followed by a boolean expression".into(),
            } ),
        };

        let after_condition_token = self.tokens.current().bounded( &mut self.tokens, "expected do or block" )?;
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
                let before_curly_bracket_token = self.tokens.peek_previous().unwrap();
                Err( SyntaxError {
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
            LoopCondition::Post( Expression::Unary { op: Operator::Not, operand: Box::new( condition ) } )
        }
        else {
            LoopCondition::Pre( condition )
        };

        return Ok( Node::Loop( Loop {
            pre: None,
            condition,
            post: None,
            statement: Box::new( statement )
        } ) );
    }
}
