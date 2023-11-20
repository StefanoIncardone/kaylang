use std::fmt::Display;

use crate::{lexer::*, errors::*};


#[derive( Debug, Clone )]
pub(crate) enum Expression {
    Literal( Literal ),
    Unary { op: Operator, operand: Box<Expression> },
    Binary { lhs: Box<Expression>, op: Operator, rhs: Box<Expression> },
    Identifier( String, Type ),
}

impl Display for Expression {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Literal( literal )      => write!( f, "{}", literal ),
            Self::Unary { op, operand }   => write!( f, "{}{}", op, operand ),
            Self::Binary { lhs, op, rhs } => write!( f, "({} {} {})", lhs, op, rhs ),
            Self::Identifier( name, _ )   => write!( f, "{}", name ),
        }
    }
}

impl TypeOf for Expression {
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
pub(crate) struct Variable {
    pub(crate) mutability: Mutability,
    pub(crate) name: String,
    pub(crate) value: Expression,
}

#[derive( Debug, Clone )]
pub(crate) struct IfStatement {
    pub(crate) condition: Expression,
    pub(crate) statement: Node,
}

impl Display for IfStatement {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return write!( f, "if {}", self.condition );
    }
}

#[derive( Debug, Clone )]
pub(crate) struct If {
    pub(crate) ifs: Vec<IfStatement>,
    pub(crate) els: Option<Box<Node>>,
}

#[allow( dead_code )]
#[derive( Debug, Clone )]
pub(crate) enum LoopCondition {
    Infinite,
    Pre( Expression ),
    Post( Expression )
}

#[allow( dead_code )]
#[derive( Debug, Clone )]
pub(crate) struct Loop {
    pub(crate) pre: Option<Box<Node>>,
    pub(crate) condition: LoopCondition,
    pub(crate) post: Option<Box<Node>>,
    pub(crate) statement: Box<Node>,
}

impl Display for Loop {
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
pub(crate) enum Node {
    Empty,

    Expression( Expression ),
    Print( Expression ),
    Println( Option<Expression> ),
    If( If ),
    Loop( Loop ),
    Break,
    Continue,

    Definition( usize /* scope idx */, usize /* variable idx */ ),
    Assignment( usize /* scope idx */, usize /* variable idx */, Expression ),
    Scope( usize ),
}

impl Display for Node {
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
pub(crate) struct Scope {
    pub(crate) parent: usize,
    pub(crate) types: Vec<Type>,
    pub(crate) variables: Vec<Variable>,
    pub(crate) nodes: Vec<Node>,
}


// TODO process entire statement for syntactical correctness and then report all the errors
    // IDEA create Parser class that builds the AST, and then validate the AST afterwards
#[derive( Debug )]
pub(crate) struct AST {
    pub(crate) scopes: Vec<Scope>,
    scope: usize,
    loop_depth: usize,

    errors: Vec<SyntaxError>,
}

impl TryFrom<&mut Lexer> for AST {
    type Error = SyntaxErrors;

    fn try_from( lexer: &mut Lexer ) -> Result<Self, Self::Error> {
        let mut this = Self {
            scopes: vec![Scope {
                parent: 0,
                types: vec![Type::Int, Type::Char, Type::Bool, Type::Str],
                variables: Vec::new(),
                nodes: Vec::new(),
            }],
            scope: 0,
            loop_depth: 0,
            errors: Vec::new()
        };

        let mut tokens = lexer.iter();
        this.parse_scope( &mut tokens );

        return match this.errors.is_empty() {
            true => Ok( this ),
            false => Err( SyntaxErrors::new( this.errors, &mut lexer.src ) ),
        }
    }
}

// scopes
impl AST {
    fn parse_scope( &mut self, tokens: &mut TokenCursor ) {
        loop {
            match self.parse_single_any( tokens ) {
                Ok( Some( node ) ) => {
                    match node {
                        // skip to the next token after a semicolon
                        Node::Empty => continue,

                        // check to see if a terminating semicolon is present
                        Node::Definition( _, _ ) | Node::Assignment( _, _, _ )
                        | Node::Expression( _ )
                        | Node::Break | Node::Continue
                        | Node::Print( _ ) | Node::Println( _ ) =>
                            if let Err( err ) = self.semicolon( tokens ) {
                                self.errors.push( err );

                                tokens.line = tokens.lines.len();
                                tokens.token = tokens.tokens.len();
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
                    tokens.line = tokens.lines.len();
                    tokens.token = tokens.tokens.len();
                    break;
                },
            }
        }
    }

    fn parse_single_statement( &mut self, tokens: &mut TokenCursor ) -> Result<Option<Node>, SyntaxError> {
        let current = match tokens.current() {
            Some( position ) => position,
            None => return Ok( None ),
        };

        return match current.token.kind {
            TokenKind::Literal( _ )
            | TokenKind::True | TokenKind::False
            | TokenKind::Bracket( BracketKind::OpenRound )
            | TokenKind::Op( Operator::Minus | Operator::Not )
            | TokenKind::Identifier( _ )          => Ok( Some( self.variable_reassignment_or_expression( tokens )? ) ),
            TokenKind::Definition( _ )            => Ok( Some( self.variable_definition( tokens )? ) ),
            TokenKind::Print | TokenKind::PrintLn => Ok( Some( self.print( tokens )? ) ),
            TokenKind::If                         => Ok( Some( self.iff( tokens )? ) ),
            TokenKind::Else => {
                tokens.next();
                Err( SyntaxError {
                    line_byte_start: current.line.byte_start,
                    line: current.line.number,
                    col: current.token.col,
                    len: current.token.kind.len(),
                    msg: "invalid if statement".into(),
                    help_msg: "stray else block".into(),
                } )
            },
            TokenKind::Do | TokenKind::Loop => {
                self.loop_depth += 1;
                let looop_statement = self.loop_statement( tokens );
                self.loop_depth -= 1;
                match looop_statement {
                    Ok( looop ) => Ok( Some( looop ) ),
                    Err( err ) => Err( err ),
                }
            },
            TokenKind::Break => {
                tokens.next();
                match self.loop_depth {
                    0 => Err( SyntaxError {
                        line_byte_start: current.line.byte_start,
                        line: current.line.number,
                        col: current.token.col,
                        len: current.token.kind.len(),
                        msg: "invalid break statement".into(),
                        help_msg: "cannot be used outside of loops".into(),
                    } ),
                    _ => Ok( Some( Node::Break ) ),
                }
            },
            TokenKind::Continue => {
                tokens.next();
                match self.loop_depth {
                    0 => Err( SyntaxError {
                        line_byte_start: current.line.byte_start,
                        line: current.line.number,
                        col: current.token.col,
                        len: current.token.kind.len(),
                        msg: "invalid continue statement".into(),
                        help_msg: "cannot be used outside of loops".into(),
                    } ),
                    _ => Ok( Some( Node::Continue ) ),
                }
            },
            TokenKind::Bracket( BracketKind::OpenCurly ) => {
                tokens.next();
                Err( SyntaxError {
                    line_byte_start: current.line.byte_start,
                    line: current.line.number,
                    col: current.token.col,
                    len: current.token.kind.len(),
                    msg: "invalid statement".into(),
                    help_msg: "blocks are not allowed in this context".into(),
                } )
            },
            TokenKind::Bracket( BracketKind::CloseCurly ) => {
                tokens.next();
                Err( SyntaxError {
                    line_byte_start: current.line.byte_start,
                    line: current.line.number,
                    col: current.token.col,
                    len: current.token.kind.len(),
                    msg: "invalid statement".into(),
                    help_msg: "stray closed curly bracket".into(),
                } )
            },
            TokenKind::Bracket( BracketKind::CloseRound ) => {
                tokens.next();
                Err( SyntaxError {
                    line_byte_start: current.line.byte_start,
                    line: current.line.number,
                    col: current.token.col,
                    len: current.token.kind.len(),
                    msg: "invalid expression".into(),
                    help_msg: "stray closed parenthesis".into(),
                } )
            },
            TokenKind::Colon => {
                tokens.next();
                Err( SyntaxError {
                    line_byte_start: current.line.byte_start,
                    line: current.line.number,
                    col: current.token.col,
                    len: current.token.kind.len(),
                    msg: "invalid type annotation".into(),
                    help_msg: "stray colon".into(),
                } )
            },
            TokenKind::Equals => {
                tokens.next();
                Err( SyntaxError {
                    line_byte_start: current.line.byte_start,
                    line: current.line.number,
                    col: current.token.col,
                    len: current.token.kind.len(),
                    msg: "invalid assignment".into(),
                    help_msg: "stray assignment".into(),
                } )
            },
            TokenKind::Op( _ ) => {
                tokens.next();
                Err( SyntaxError {
                    line_byte_start: current.line.byte_start,
                    line: current.line.number,
                    col: current.token.col,
                    len: current.token.kind.len(),
                    msg: "invalid expression".into(),
                    help_msg: "stray binary operator".into(),
                } )
            },
            TokenKind::SemiColon => {
                tokens.next();
                Ok( Some( Node::Empty ) )
            },
            TokenKind::Comment( _ ) | TokenKind::Empty | TokenKind::Unexpected( _ ) => unreachable!(),
        }
    }

    fn parse_do_single_statement( &mut self, tokens: &mut TokenCursor ) -> Result<Option<Node>, SyntaxError> {
        let current = tokens.next().bounded( tokens, "expected statement" )?;
        return match current.token.kind {
            TokenKind::Bracket( BracketKind::OpenCurly ) => {
                tokens.next();
                Err( SyntaxError {
                    line_byte_start: current.line.byte_start,
                    line: current.line.number,
                    col: current.token.col,
                    len: current.token.kind.len(),
                    msg: "invalid statement".into(),
                    help_msg: "blocks are not allowed in do statements".into(),
                } )
            },
            TokenKind::Definition( _ ) => {
                tokens.next();
                Err( SyntaxError {
                    line_byte_start: current.line.byte_start,
                    line: current.line.number,
                    col: current.token.col,
                    len: current.token.kind.len(),
                    msg: "invalid statement".into(),
                    help_msg: "variable definitions are not allowed in do statements".into(),
                } )
            },
            _ => self.parse_single_statement( tokens ),
        }
    }

    fn parse_single_any( &mut self, tokens: &mut TokenCursor ) -> Result<Option<Node>, SyntaxError> {
        let current = match tokens.current() {
            Some( position ) => position,
            None => return Ok( None ),
        };

        return match current.token.kind {
            TokenKind::Bracket( BracketKind::OpenCurly ) => {
                let new_scope = self.scopes.len();
                self.scopes.push( Scope {
                    parent: self.scope,
                    types: Vec::new(),
                    variables: Vec::new(),
                    nodes: Vec::new(),
                } );
                self.scope = new_scope;

                tokens.next();
                self.parse_scope( tokens );
                Ok( Some( Node::Scope( new_scope ) ) )
            },
            TokenKind::Bracket( BracketKind::CloseCurly ) => {
                self.scope = self.scopes[ self.scope ].parent;
                tokens.next();
                Ok( None )
            },
            _ => self.parse_single_statement( tokens ),
        }
    }
}

// semicolons
impl AST {
    fn semicolon( &mut self, tokens: &mut TokenCursor ) -> Result<(), SyntaxError> {
        let semicolon = tokens.current().bounded( tokens, "expected semicolon" )?;
        return match &semicolon.token.kind {
            TokenKind::SemiColon => {
                tokens.next();
                Ok( () )
            },
            _ => {
                // this function is never called without a previous token, so we can safely unwrap
                let previous = unsafe{ tokens.peek_previous().unwrap_unchecked() };
                Err( SyntaxError {
                    line_byte_start: previous.line.byte_start,
                    line: previous.line.number,
                    col: previous.token.col,
                    len: previous.token.kind.len(),
                    msg: "invalid statement".into(),
                    help_msg: "expected semicolon after this token".into(),
                } )
            }
        }
    }
}

// expressions
impl AST {
    fn operator( &mut self, tokens: &mut TokenCursor, ops: &[Operator] ) -> Result<Option<Operator>, SyntaxError> {
        let current_pos = tokens.current().bounded( tokens, "expected operator or semicolon" )?;
        return match current_pos.token.kind {
            TokenKind::Op( op ) =>
                if ops.contains( &op ) {
                    tokens.next();
                    Ok( Some( op ) )
                }
                else {
                    Ok( None )
                },
            _ => Ok( None ),
        }
    }

    fn primary_expression( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
        let current = tokens.current().bounded( tokens, "expected expression" )?;
        let factor = match &current.token.kind {
            TokenKind::Literal( literal ) => Ok( Expression::Literal( literal.clone() ) ),
            TokenKind::True => Ok( Expression::Literal( Literal::Bool( true ) ) ),
            TokenKind::False => Ok( Expression::Literal( Literal::Bool( false ) ) ),
            TokenKind::Identifier( name ) => match self.resolve_type( name ) {
                None => match self.resolve_variable( name ) {
                    Some( (variable, _, _) ) =>
                        Ok( Expression::Identifier( variable.name.clone(), variable.value.typ() ) ),
                    None => Err( SyntaxError {
                        line_byte_start: current.line.byte_start,
                        line: current.line.number,
                        col: current.token.col,
                        len: current.token.kind.len(),
                        msg: "variable not defined".into(),
                        help_msg: "was not previously defined in this scope".into(),
                    } ),
                },
                Some( _ ) => Err( SyntaxError {
                    line_byte_start: current.line.byte_start,
                    line: current.line.number,
                    col: current.token.col,
                    len: current.token.kind.len(),
                    msg: "invalid expression".into(),
                    help_msg: "cannot be a type name".into(),
                } ),
            },
            TokenKind::Bracket( BracketKind::OpenRound ) => {
                let expression_start_pos = tokens.next().bounded( tokens, "expected expression" )?;
                match expression_start_pos.token.kind {
                    TokenKind::Bracket( BracketKind::CloseRound ) => Err( SyntaxError {
                        line_byte_start: expression_start_pos.line.byte_start,
                        line: expression_start_pos.line.number,
                        col: expression_start_pos.token.col,
                        len: expression_start_pos.token.kind.len(),
                        msg: "invalid expression".into(),
                        help_msg: "empty expressions are not allowed".into(),
                    } ),
                    _ => {
                        let expression = self.expression( tokens )?;
                        let close_bracket_pos = tokens.current().bounded( tokens, "expected closed parenthesis" )?;
                        match close_bracket_pos.token.kind {
                            TokenKind::Bracket( BracketKind::CloseRound ) => Ok( expression ),
                            _ => Err( SyntaxError {
                                line_byte_start: current.line.byte_start,
                                line: current.line.number,
                                col: current.token.col,
                                len: current.token.kind.len(),
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
                while let Some( TokenPosition { token: Token { kind: TokenKind::Op( Operator::Minus ), .. }, .. } ) = tokens.next() {
                    sign *= -1;
                }

                let operand = self.primary_expression( tokens )?;

                // returning to avoid the call to tokens.next at the end of the function
                return match operand.typ() {
                    Type::Bool => Err( SyntaxError {
                        line_byte_start: current.line.byte_start,
                        line: current.line.number,
                        col: current.token.col,
                        len: current.token.kind.len(),
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
                while let Some( TokenPosition { token: Token { kind: TokenKind::Op( Operator::Not ), .. }, .. } ) = tokens.next() {
                    should_be_inverted = !should_be_inverted;
                }

                let operand = self.primary_expression( tokens )?;
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
                line_byte_start: current.line.byte_start,
                line: current.line.number,
                col: current.token.col,
                len: current.token.kind.len(),
                msg: "invalid expression".into(),
                help_msg: "cannot be a keyword".into(),
            } ),
            _ => Err( SyntaxError {
                line_byte_start: current.line.byte_start,
                line: current.line.number,
                col: current.token.col,
                len: current.token.kind.len(),
                msg: "invalid expression".into(),
                help_msg: "expected expression operand before this token".into(),
            } ),
        };

        tokens.next();
        return factor;
    }

    fn exponentiative_expression( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
        let mut lhs = self.primary_expression( tokens )?;

        while let Some( op ) = self.operator( tokens, &[Operator::Pow] )? {
            let rhs = self.primary_expression( tokens )?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn multiplicative_expression( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
        let mut lhs = self.exponentiative_expression( tokens )?;

        while let Some( op ) = self.operator( tokens, &[Operator::Times, Operator::Divide, Operator::Remainder] )? {
            let rhs = self.exponentiative_expression( tokens )?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn additive_expression( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
        let mut lhs = self.multiplicative_expression( tokens )?;

        while let Some( op ) = self.operator( tokens, &[Operator::Plus, Operator::Minus] )? {
            let rhs = self.multiplicative_expression( tokens )?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn shift_expression( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
        let mut lhs = self.additive_expression( tokens )?;

        while let Some( op ) = self.operator( tokens, &[Operator::LeftShift, Operator::RightShift] )? {
            let rhs = self.additive_expression( tokens )?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn bitand_expression( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
        let mut lhs = self.shift_expression( tokens )?;

        while let Some( op ) = self.operator( tokens, &[Operator::BitAnd] )? {
            let rhs = self.shift_expression( tokens )?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn bitxor_expression( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
        let mut lhs = self.bitand_expression( tokens )?;

        while let Some( op ) = self.operator( tokens, &[Operator::BitXor] )? {
            let rhs = self.bitand_expression( tokens )?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn bitor_expression( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
        let mut lhs = self.bitxor_expression( tokens )?;

        while let Some( op ) = self.operator( tokens, &[Operator::BitOr] )? {
            let rhs = self.bitxor_expression( tokens )?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn comparative_expression( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
        let mut lhs = self.bitor_expression( tokens )?;

        while let Some( op ) = self.operator( tokens, &[Operator::Compare] )? {
            let rhs = self.bitor_expression( tokens )?;
            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn comparison_expression( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
        let mut lhs = self.comparative_expression( tokens )?;

        let ops = [
            Operator::EqualsEquals, Operator::NotEquals,
            Operator::Greater, Operator::GreaterOrEquals,
            Operator::Less, Operator::LessOrEquals
        ];

        let mut is_chained = false;
        while let Some( op ) = self.operator( tokens, &ops )? {
            let op_pos = tokens.peek_previous().unwrap();
            let rhs = self.comparative_expression( tokens )?;

            if is_chained {
                return Err( SyntaxError {
                    line_byte_start: op_pos.line.byte_start,
                    line: op_pos.line.number,
                    col: op_pos.token.col,
                    len: op_pos.token.kind.len(),
                    msg: "invalid boolean expression".into(),
                    help_msg: "comparison operators cannot be chained".into(),
                } );
            }
            is_chained = true;

            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    fn and_expression( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
        let mut lhs = self.comparison_expression( tokens )?;

        while let Some( op ) = self.operator( tokens, &[Operator::And] )? {
            let op_pos = tokens.peek_previous().unwrap();

            if lhs.typ() != Type::Bool {
                return Err( SyntaxError {
                    line_byte_start: op_pos.line.byte_start,
                    line: op_pos.line.number,
                    col: op_pos.token.col,
                    len: op_pos.token.kind.len(),
                    msg: "invalid boolean expression".into(),
                    help_msg: "must be preceded by a boolean expression".into(),
                } );
            }

            let rhs = self.comparison_expression( tokens )?;
            if rhs.typ() != Type::Bool {
                return Err( SyntaxError {
                    line_byte_start: op_pos.line.byte_start,
                    line: op_pos.line.number,
                    col: op_pos.token.col,
                    len: op_pos.token.kind.len(),
                    msg: "invalid boolean expression".into(),
                    help_msg: "must be followed by a boolean expression".into(),
                } );
            }

            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    // pub(crate) fn xor_expression( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
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

    fn or_expression( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
        let mut lhs = self.and_expression( tokens )?;

        while let Some( op ) = self.operator( tokens, &[Operator::Or] )? {
            let op_pos = tokens.peek_previous().unwrap();

            if lhs.typ() != Type::Bool {
                return Err( SyntaxError {
                    line_byte_start: op_pos.line.byte_start,
                    line: op_pos.line.number,
                    col: op_pos.token.col,
                    len: op_pos.token.kind.len(),
                    msg: "invalid boolean expression".into(),
                    help_msg: "must be preceded by a boolean expression".into(),
                } );
            }

            let rhs = self.and_expression( tokens )?;
            if rhs.typ() != Type::Bool {
                return Err( SyntaxError {
                    line_byte_start: op_pos.line.byte_start,
                    line: op_pos.line.number,
                    col: op_pos.token.col,
                    len: op_pos.token.kind.len(),
                    msg: "invalid boolean expression".into(),
                    help_msg: "must be followed by a boolean expression".into(),
                } );
            }

            lhs = Expression::Binary { lhs: Box::new( lhs ), op, rhs: Box::new( rhs ) };
        }

        return Ok( lhs );
    }

    // TODO implement boolean operators for strings
    // IDEA optimize expression building by implementing associativity rules
    // TODO disallow implicit conversions (str + i64, char + i64, str + char or str + str
        // IDEA introduce casting operators
    // TODO implement boolean operator chaining
    fn expression( &mut self, tokens: &mut TokenCursor ) -> Result<Expression, SyntaxError> {
        return self.or_expression( tokens );
    }
}

// variable definitions and assignments
impl<'this> AST {
    fn resolve_variable( &'this self, name: &str ) -> Option<(&'this Variable, usize /* scope idx */, usize /* variable idx */)> {
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

    fn resolve_type( &'this self, name: &str ) -> Option<&'this Type> {
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


    fn variable_definition( &mut self, tokens: &mut TokenCursor ) -> Result<Node, SyntaxError> {
        let definition_pos = tokens.current().unwrap();
        let mutability = match definition_pos.token.kind {
            TokenKind::Definition( kind ) => kind,
            _ => unreachable!(),
        };

        let name_pos = tokens.next().bounded( tokens, "expected identifier" )?;
        let name = match &name_pos.token.kind {
            TokenKind::Identifier( name ) => match self.resolve_type( name ) {
                None => Ok( name.clone() ),
                Some( _ ) => Err( SyntaxError {
                    line_byte_start: name_pos.line.byte_start,
                    line: name_pos.line.number,
                    col: name_pos.token.col,
                    len: name_pos.token.kind.len(),
                    msg: "invalid variable name".into(),
                    help_msg: "cannot be a type name".into(),
                } ),
            },
            _ => Err( SyntaxError {
                line_byte_start: name_pos.line.byte_start,
                line: name_pos.line.number,
                col: name_pos.token.col,
                len: name_pos.token.kind.len(),
                msg: "invalid assignment".into(),
                help_msg: "expected variable name".into(),
            } ),
        };

        let colon_pos = tokens.peek_next().bounded( tokens, "expected type annotation or variable definition" )?;
        let annotation = match &colon_pos.token.kind {
            TokenKind::Colon => {
                tokens.next();
                let annotation_pos = tokens.next().bounded( tokens, "expected type" )?;
                match &annotation_pos.token.kind {
                    TokenKind::Identifier( type_name ) => match self.resolve_type( type_name ) {
                        Some( typ ) => Ok( Some( (*typ, annotation_pos) ) ),
                        None => match self.resolve_variable( type_name ) {
                            Some( (var, _, _) ) => Ok( Some( (var.value.typ(), annotation_pos) ) ),
                            None => Err( SyntaxError {
                                line_byte_start: annotation_pos.line.byte_start,
                                line: annotation_pos.line.number,
                                col: annotation_pos.token.col,
                                len: annotation_pos.token.kind.len(),
                                msg: "invalid type annotation".into(),
                                help_msg: "was not previously defined".into(),
                            } )
                        },
                    },
                    _ => Err( SyntaxError {
                        line_byte_start: colon_pos.line.byte_start,
                        line: colon_pos.line.number,
                        col: colon_pos.token.col,
                        len: colon_pos.token.kind.len(),
                        msg: "invalid type annotation".into(),
                        help_msg: "expected type name after here".into(),
                    } )
                }
            },
            _ => Ok( None ),
        };

        let equals_or_semicolon_pos = tokens.next().bounded( tokens, "expected equals" )?;
        let expression = match equals_or_semicolon_pos.token.kind {
            TokenKind::Equals => {
                tokens.next().bounded( tokens, "expected expression" )?;
                match self.expression( tokens ) {
                    Ok( expr ) => Ok( Some( expr ) ),
                    Err( err ) => Err( err ),
                }
            },
            TokenKind::SemiColon => Ok( None ),
            _ => Err( SyntaxError {
                line_byte_start: name_pos.line.byte_start,
                line: name_pos.line.number,
                col: name_pos.token.col,
                len: name_pos.token.kind.len(),
                msg: "invalid assignment".into(),
                help_msg: "expected '=' or ';' after the variable name".into(),
            } ),
        };

        let name = name?;
        let annotation = annotation?;
        let expression = expression?;

        if self.resolve_variable( &name ).is_some() {
            return Err( SyntaxError {
                line_byte_start: name_pos.line.byte_start,
                line: name_pos.line.number,
                col: name_pos.token.col,
                len: name_pos.token.kind.len(),
                msg: "variable redefinition".into(),
                help_msg: "was previously defined".into(),
            } );
        }

        return match expression {
            Some( value ) => {
                if let Some( (typ, pos) ) = annotation {
                    if typ != value.typ() {
                        return Err( SyntaxError {
                            line_byte_start: pos.line.byte_start,
                            line: pos.line.number,
                            col: pos.token.col,
                            len: pos.token.kind.len(),
                            msg: "invalid definition".into(),
                            help_msg: format!(
                                "declared type of '{}' doesn't match expression of type '{}'",
                                typ, value.typ()
                            ).into(),
                        } );
                    }
                }

                let variables = &mut self.scopes[ self.scope ].variables;
                variables.push( Variable {
                    mutability,
                    name: name.clone(),
                    value
                } );

                Ok( Node::Definition( self.scope, variables.len() - 1 ) )
            },
            None => match annotation {
                Some( (typ, _) ) => {
                    let variables = &mut self.scopes[ self.scope ].variables;
                    variables.push( Variable {
                        mutability,
                        name: name.clone(),
                        value: Expression::Literal( typ.default() )
                    } );

                    Ok( Node::Definition( self.scope, variables.len() - 1 ) )
                },
                None => Err( SyntaxError {
                    line_byte_start: name_pos.line.byte_start,
                    line: name_pos.line.number,
                    col: name_pos.token.col,
                    len: name_pos.token.kind.len(),
                    msg: "invalid definition".into(),
                    help_msg: "expected type annotation or value after here to infer the type of the variable".into(),
                }),
            },
        }
    }

    fn variable_reassignment_or_expression( &mut self, tokens: &mut TokenCursor ) -> Result<Node, SyntaxError> {
        let name_pos = tokens.current().unwrap();
        let op_pos = match tokens.peek_next() {
            Some( pos ) => match pos.token.kind {
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
                ) => pos,
                _ => return Ok( Node::Expression( self.expression( tokens )? ) ),
            },
            None => return Ok( Node::Expression( self.expression( tokens )? ) ),
        };

        tokens.next();
        tokens.next().bounded( tokens, "expected expression" )?;

        let name = name_pos.token.kind.to_string();

        if self.resolve_type( &name ).is_some() {
            return Err( SyntaxError {
                line_byte_start: name_pos.line.byte_start,
                line: name_pos.line.number,
                col: name_pos.token.col,
                len: name_pos.token.kind.len(),
                msg: "invalid assignment".into(),
                help_msg: "cannot be a type name".into(),
            } );
        }

        let rhs = self.expression( tokens )?;
        match self.resolve_variable( &name ) {
            Some( (var, scope, var_idx) ) => match var.mutability {
                Mutability::Let => Err( SyntaxError {
                    line_byte_start: name_pos.line.byte_start,
                    line: name_pos.line.number,
                    col: name_pos.token.col,
                    len: name_pos.token.kind.len(),
                    msg: "invalid assignment".into(),
                    help_msg: "was defined as immutable".into(),
                } ),
                Mutability::Var => {
                    let value = match &op_pos.token.kind {
                        TokenKind::Equals => rhs,
                        TokenKind::Op( op ) => {
                            let lhs = Expression::Identifier( name.clone(), op.typ() );
                            Expression::Binary { lhs: Box::new( lhs ), op: *op, rhs: Box::new( rhs ) }
                        },
                        _ => unreachable!(),
                    };

                    if var.value.typ() != value.typ() {
                        return Err( SyntaxError {
                            line_byte_start: name_pos.line.byte_start,
                            line: name_pos.line.number,
                            col: name_pos.token.col,
                            len: name_pos.token.kind.len(),
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
                line_byte_start: name_pos.line.byte_start,
                line: name_pos.line.number,
                col: name_pos.token.col,
                len: name_pos.token.kind.len(),
                msg: "variable redefinition".into(),
                help_msg: "was not previously defined in this scope".into(),
            }),
        }
    }
}

// print statements
impl AST {
    fn print( &mut self, tokens: &mut TokenCursor ) -> Result<Node, SyntaxError> {
        let print_pos = tokens.current().unwrap();
        if let TokenKind::PrintLn = print_pos.token.kind {
            if let Some( TokenPosition { token: &Token { kind: TokenKind::SemiColon, .. }, .. } ) = tokens.peek_next() {
                tokens.next();
                return Ok( Node::Println( None ) );
            }
        }

        tokens.next().bounded( tokens, "expected expression" )?;
        let argument = self.expression( tokens )?;

        return match print_pos.token.kind {
            TokenKind::Print => Ok( Node::Print( argument ) ),
            TokenKind::PrintLn => Ok( Node::Println( Some( argument ) ) ),
            _ => unreachable!(),
        }
    }
}

// if statements
impl AST {
    fn iff( &mut self, tokens: &mut TokenCursor ) -> Result<Node, SyntaxError> {
        let mut if_statement = If { ifs: Vec::new(), els: None };

        'iff: while let Some( if_pos ) = tokens.current() {
            tokens.next().bounded( tokens, "expected boolean expression" )?;

            let expression = self.expression( tokens )?;
            let condition = match &expression.typ() {
                Type::Bool => Ok( expression ),
                Type::Char | Type::Int | Type::Str => Err( SyntaxError {
                    line_byte_start: if_pos.line.byte_start,
                    line: if_pos.line.number,
                    col: if_pos.token.col,
                    len: if_pos.token.kind.len(),
                    msg: "expected boolean expression".into(),
                    help_msg: "must be followed by a boolean expression".into(),
                } ),
            };

            let condition = condition?;
            let after_condition_pos = tokens.current().bounded( tokens, "expected do or block" )?;
            let iff = match after_condition_pos.token.kind {
                TokenKind::Bracket( BracketKind::OpenCurly ) => {
                    let scope = self.parse_single_any( tokens )?.unwrap();
                    Ok( IfStatement { condition, statement: scope } )
                },
                TokenKind::Do => {
                    let statement = self.parse_do_single_statement( tokens )?.unwrap();
                    self.semicolon( tokens )?;
                    Ok( IfStatement { condition, statement } )
                },
                _ => {
                    let before_curly_bracket_pos = tokens.peek_previous().unwrap();
                    Err( SyntaxError {
                        line_byte_start: before_curly_bracket_pos.line.byte_start,
                        line: before_curly_bracket_pos.line.number,
                        col: before_curly_bracket_pos.token.col,
                        len: before_curly_bracket_pos.token.kind.len(),
                        msg: "invalid if statement".into(),
                        help_msg: "must be followed by a do or a block".into(),
                    } )
                },
            };

            if_statement.ifs.push( iff? );

            while let Some( else_pos ) = tokens.current() {
                let after_else_pos = match else_pos.token.kind {
                    TokenKind::Else => tokens.next().bounded( tokens, "expected do, block or if statement" )?,
                    _ => break 'iff,
                };

                // we are now inside an else branch
                let else_if = match after_else_pos.token.kind {
                    TokenKind::Bracket( BracketKind::OpenCurly ) => {
                        let scope = self.parse_single_any( tokens )?.unwrap();
                        if_statement.els = Some( Box::new( scope ) );
                        break 'iff;
                    },
                    TokenKind::Do => {
                        let statement = self.parse_do_single_statement( tokens )?.unwrap();
                        self.semicolon( tokens )?;
                        if_statement.els = Some( Box::new( statement ) );
                        break 'iff;
                    },
                    TokenKind::If => break,
                    _ => Err( SyntaxError {
                        line_byte_start: else_pos.line.byte_start,
                        line: else_pos.line.number,
                        col: else_pos.token.col,
                        len: else_pos.token.kind.len(),
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
impl AST {
    fn loop_statement( &mut self, tokens: &mut TokenCursor ) -> Result<Node, SyntaxError> {
        let do_pos = tokens.current().unwrap();
        let loop_pos = match do_pos.token.kind {
            TokenKind::Do => tokens.next().bounded( tokens, "expected loop statement" )?,
            _ => do_pos,
        };

        tokens.next().bounded( tokens, "expected boolean expression" )?;
        let expression = self.expression( tokens )?;
        let condition = match &expression.typ() {
            Type::Bool => Ok( expression ),
            Type::Char | Type::Int | Type::Str => Err( SyntaxError {
                line_byte_start: loop_pos.line.byte_start,
                line: loop_pos.line.number,
                col: loop_pos.token.col,
                len: loop_pos.token.kind.len(),
                msg: "expected boolean expression".into(),
                help_msg: "must be followed by a boolean expression".into(),
            } ),
        };

        let after_condition_pos = tokens.current().bounded( tokens, "expected do or block" )?;
        let statement = match after_condition_pos.token.kind {
            TokenKind::Bracket( BracketKind::OpenCurly ) => {
                let scope = self.parse_single_any( tokens )?.unwrap();
                Ok( scope )
            },
            TokenKind::Do => {
                let statement = self.parse_do_single_statement( tokens )?.unwrap();
                self.semicolon( tokens )?;
                Ok( statement )
            },
            _ => {
                let before_curly_bracket_pos = tokens.peek_previous().unwrap();
                Err( SyntaxError {
                    line_byte_start: before_curly_bracket_pos.line.byte_start,
                    line: before_curly_bracket_pos.line.number,
                    col: before_curly_bracket_pos.token.col,
                    len: before_curly_bracket_pos.token.kind.len(),
                    msg: "invalid for statement".into(),
                    help_msg: "must be followed by a do or a block".into(),
                } )
            },
        };

        let condition = condition?;
        let statement = statement?;
        let condition = if let TokenKind::Do = do_pos.token.kind {
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
