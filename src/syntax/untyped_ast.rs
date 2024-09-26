use crate::{
    src_file::{offset, Position, SrcFile},
    syntax::tokenizer::{Base, BracketKind, DisplayLen, Op, Token},
};
use core::fmt::Display;
use std::borrow::Cow;
use super::{tokenizer::{ascii, utf8, Integer, Str, TokenKind}, Error, ErrorDisplay, ErrorInfo, IntoErrorInfo};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PrefixOp {
    Len,
    Not,

    Plus,
    WrappingPlus,
    SaturatingPlus,

    Minus,
    WrappingMinus,
    SaturatingMinus,
}

impl Display for PrefixOp {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return match self {
            Self::Len             => write!(f, "len"),
            Self::Not             => write!(f, "!"),

            Self::Plus            => write!(f,  "+"),
            Self::WrappingPlus    => write!(f, r"+\"),
            Self::SaturatingPlus  => write!(f,  "+|"),

            Self::Minus           => write!(f,  "-"),
            Self::WrappingMinus   => write!(f, r"-\"),
            Self::SaturatingMinus => write!(f,  "-|"),
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BinaryOp {
    // binary operators
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

    // boolean binary operators
    And,
    Or,

    // comparison operators
    Compare,

    // boolean comparison operators
    EqualsEquals,
    NotEquals,
    Greater,
    GreaterOrEquals,
    Less,
    LessOrEquals,
}

impl Display for BinaryOp {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return match self {
            Self::Pow                 => write!(f,  "**"),
            Self::WrappingPow         => write!(f, r"**\"),
            Self::SaturatingPow       => write!(f,  "**|"),

            Self::Times               => write!(f,  "*"),
            Self::WrappingTimes       => write!(f, r"*\"),
            Self::SaturatingTimes     => write!(f,  "*|"),

            Self::Divide              => write!(f,  "/"),
            Self::WrappingDivide      => write!(f, r"/\"),
            Self::SaturatingDivide    => write!(f,  "/|"),

            Self::Remainder           => write!(f, "%"),

            Self::Plus                => write!(f,  "+"),
            Self::WrappingPlus        => write!(f, r"+\"),
            Self::SaturatingPlus      => write!(f,  "+|"),

            Self::Minus               => write!(f,  "-"),
            Self::WrappingMinus       => write!(f, r"-\"),
            Self::SaturatingMinus     => write!(f,  "-|"),

            Self::LeftShift           => write!(f,  "<<"),
            Self::WrappingLeftShift   => write!(f, r"<<\"),
            Self::SaturatingLeftShift => write!(f,  "<<|"),

            Self::RightShift          => write!(f,  ">>"),
            Self::LeftRotate          => write!(f, "<<<"),
            Self::RightRotate         => write!(f, ">>>"),

            Self::BitAnd              => write!(f, "&"),
            Self::BitOr               => write!(f, "|"),
            Self::BitXor              => write!(f, "^"),

            Self::And => write!(f, "&&"),
            Self::Or  => write!(f, "||"),

            Self::Compare => write!(f, "<=>"),

            Self::EqualsEquals    => write!(f, "=="),
            Self::NotEquals       => write!(f, "!="),
            Self::Greater         => write!(f, ">"),
            Self::GreaterOrEquals => write!(f, ">="),
            Self::Less            => write!(f, "<"),
            Self::LessOrEquals    => write!(f, "<="),
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
            Self::Equals              => write!(f, "="),

            Self::Pow                 => write!(f,  "**="),
            Self::WrappingPow         => write!(f, r"**\="),
            Self::SaturatingPow       => write!(f,  "**|="),

            Self::Times               => write!(f,  "*="),
            Self::WrappingTimes       => write!(f, r"*\="),
            Self::SaturatingTimes     => write!(f,  "*|="),

            Self::Divide              => write!(f,  "/="),
            Self::WrappingDivide      => write!(f, r"/\="),
            Self::SaturatingDivide    => write!(f,  "/|="),

            Self::Remainder           => write!(f, "%="),

            Self::Plus                => write!(f,  "+="),
            Self::WrappingPlus        => write!(f, r"+\="),
            Self::SaturatingPlus      => write!(f,  "+|="),

            Self::Minus               => write!(f,  "-="),
            Self::WrappingMinus       => write!(f, r"-\="),
            Self::SaturatingMinus     => write!(f,  "-|="),

            Self::And                 => write!(f, "&&="),
            Self::BitAnd              => write!(f, "&="),
            Self::Or                  => write!(f, "||="),
            Self::BitOr               => write!(f, "|="),
            Self::BitXor              => write!(f, "^="),

            Self::LeftShift           => write!(f,  "<<="),
            Self::WrappingLeftShift   => write!(f, r"<<\="),
            Self::SaturatingLeftShift => write!(f,  "<<|="),

            Self::RightShift          => write!(f,  ">>="),
            Self::LeftRotate          => write!(f, "<<<="),
            Self::RightRotate         => write!(f, ">>>="),
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ArrayItem {
    comma_col: offset,
    item: ExpressionIndex,
}

// TODO(stefano): allow for arrays of 0 and 1 items
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Array {
    first_item: ExpressionIndex,
    first_comma_column: offset,
    second_item: ExpressionIndex,
    items: Vec<ArrayItem>,
    trailing_comma_column: Option<offset>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
#[repr(transparent)]
#[non_exhaustive]
pub(crate) struct ExpressionIndex(pub(crate) offset);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
#[repr(transparent)]
#[non_exhaustive]
pub(crate) struct ArrayIndex(pub(crate) offset);

#[derive(Debug, Clone)]
pub(crate) enum Expression<'src, 'tokens: 'src> {
    False { column: offset },
    True { column: offset },
    Integer { base: Base, literal: &'tokens Integer<'src>, column: offset },
    Ascii { character: ascii, column: offset },
    Str { literal: &'tokens Str, column: offset },
    RawStr { literal: &'tokens Str, column: offset },
    Identifier { identifier: &'tokens &'src str, column: offset },
    Array {
        opening_square_bracket_column: offset,
        array_index: ArrayIndex,
        closing_square_bracket_column: offset,
    },

    Prefix {
        operator: PrefixOp,
        operator_column: offset,
        prefix_expression_index: ExpressionIndex,
    },
    Binary {
        lhs_index: ExpressionIndex,
        operator: BinaryOp,
        operator_column: offset,
        rhs_index: ExpressionIndex,
    },

    Parenthesis {
        opening_round_bracket_column: offset,
        inner_expression_index: ExpressionIndex,
        closing_round_bracket_column: offset,
    },

    Index {
        indexed_expression_index: ExpressionIndex,
        opening_square_bracket_column: offset,
        index_expression_index: ExpressionIndex,
        closing_square_bracket_column: offset,
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Node {
    Expression(ExpressionIndex),

    Print { print_column: offset, argument: ExpressionIndex },
    Println { println_column: offset, argument: Option<ExpressionIndex> },
    Eprint { eprint_column: offset, argument: ExpressionIndex },
    Eprintln { eprintln_column: offset, argument: Option<ExpressionIndex> },

    Assignment { lhs: ExpressionIndex, operator: AssignmentOp, operator_column: offset, rhs: ExpressionIndex },
}

#[derive(Debug)]
pub struct UntypedAst<'src, 'tokens: 'src> {
    nodes: Vec<Node>,

    expressions: Vec<Expression<'src, 'tokens>>,
    arrays: Vec<Array>,
}

impl<'src, 'tokens: 'src> UntypedAst<'src, 'tokens> {
    #[inline]
    fn new_expression(&mut self, expression: Expression<'src, 'tokens>) -> ExpressionIndex {
        let index = ExpressionIndex(self.expressions.len() as offset);
        self.expressions.push(expression);
        return index;
    }

    #[inline]
    fn new_array(&mut self, array: Array) -> ArrayIndex {
        let index = ArrayIndex(self.arrays.len() as offset);
        self.arrays.push(array);
        return index;
    }
}

const INDENT_INCREMENT: usize = 2;

impl UntypedAst<'_, '_> {
    fn info_node(&self, f: &mut core::fmt::Formatter<'_>, node: &Node, indent: usize) -> core::fmt::Result {
        return match node {
            Node::Expression(expression_index) => self.info_expression(f, *expression_index, indent),

            Node::Print { print_column, argument } => {
                let expression_indent = indent + INDENT_INCREMENT;
                writeln!(f, "{:>indent$}Print: {print_column} = print", "")?;
                self.info_expression(f, *argument, expression_indent)
            }
            Node::Println { println_column, argument } => {
                let expression_indent = indent + INDENT_INCREMENT;
                writeln!(f, "{:>indent$}Println: {println_column} = println", "")?;
                if let Some(arg) = argument {
                    self.info_expression(f, *arg, expression_indent)
                } else {
                    Ok(())
                }
            }
            Node::Eprint { eprint_column, argument } => {
                let expression_indent = indent + INDENT_INCREMENT;
                writeln!(f, "{:>indent$}Eprint: {eprint_column} = eprint", "")?;
                self.info_expression(f, *argument, expression_indent)
            }
            Node::Eprintln { eprintln_column, argument } => {
                let expression_indent = indent + INDENT_INCREMENT;
                writeln!(f, "{:>indent$}Eprintln: {eprintln_column} = eprintln", "")?;
                if let Some(arg) = argument {
                    self.info_expression(f, *arg, expression_indent)
                } else {
                    Ok(())
                }
            }
            Node::Assignment { lhs, operator, operator_column, rhs } => {
                let expression_indent = indent + INDENT_INCREMENT;
                writeln!(f, "{:>indent$}Reassignment", "")?;
                self.info_expression(f, *lhs, expression_indent)?;
                writeln!(f, "{:>expression_indent$}AssignmentOp: {operator_column} = {operator}", "")?;
                self.info_expression(f, *rhs, expression_indent)
            },
        };
    }

    fn info_expression(&self, f: &mut core::fmt::Formatter<'_>, expression_index: ExpressionIndex, indent: usize) -> core::fmt::Result {
        let expression_indent = indent + INDENT_INCREMENT;
        let expression = &self.expressions[expression_index.0 as usize];
        return match expression {
            Expression::False { column } => writeln!(f, "{:>indent$}False: {column} = false", ""),
            Expression::True { column } => writeln!(f, "{:>indent$}True: {column} = true", ""),
            Expression::Integer { base, literal, column } => {
                let literal_str = unsafe { core::str::from_utf8_unchecked(literal.0) };
                writeln!(f, "{:>indent$}Integer: {column} = {prefix}{literal_str}", "", prefix = base.prefix())
            }
            Expression::Ascii { character, column } => writeln!(f,
                "{:>indent$}Ascii: {column} = {character}",
                "",
                character = *character as utf8,
            ),
            Expression::Str { literal, column } => {
                let literal_str = unsafe { core::str::from_utf8_unchecked(&literal.0) };
                writeln!(f, "{:>indent$}Str: {column} = {literal_str}", "")
            }
            Expression::RawStr { literal, column } => {
                let literal_str = unsafe { core::str::from_utf8_unchecked(&literal.0) };
                writeln!(f, "{:>indent$}RawStr: {column} = {literal_str}", "")
            }
            Expression::Identifier { identifier, column } => {
                writeln!(f, "{:>indent$}Identifier: {column} = {identifier}", "")
            },
            Expression::Array {
                opening_square_bracket_column,
                array_index: array_expression_index,
                closing_square_bracket_column
            } => {
                let array_expression = &self.arrays[array_expression_index.0 as usize];
                writeln!(f, "{:>indent$}Array", "")?;
                writeln!(f, "{:>expression_indent$}OpeningBracket: {opening_square_bracket_column} = [", "")?;

                self.info_expression(f, array_expression.first_item, expression_indent)?;
                writeln!(f, "{:>expression_indent$}Comma: {first_comma_col} = ,", "", first_comma_col = array_expression.first_comma_column)?;

                self.info_expression(f, array_expression.second_item, expression_indent)?;

                for item in &array_expression.items {
                    writeln!(f, "{:>expression_indent$}Comma: {comma_col} = ,", "", comma_col = item.comma_col)?;
                    self.info_expression(f, item.item, expression_indent)?;
                }

                if let Some(trailing_comma_col) = array_expression.trailing_comma_column {
                    writeln!(f, "{:>expression_indent$}Comma: {trailing_comma_col} = ,", "")?;
                }

                writeln!(f, "{:>expression_indent$}ClosingBracket: {closing_square_bracket_column} = [", "")
            },

            Expression::Prefix { operator, operator_column, prefix_expression_index } => {
                writeln!(f, "{:>indent$}PrefixExpression", "")?;
                writeln!(f, "{:>expression_indent$}PrefixOp: {operator_column} = {operator}", "")?;
                self.info_expression(f, *prefix_expression_index, expression_indent)
            }
            Expression::Binary { lhs_index, operator, operator_column, rhs_index } => {
                writeln!(f, "{:>indent$}BinaryExpression", "")?;
                self.info_expression(f, *lhs_index, expression_indent)?;
                writeln!(f, "{:>expression_indent$}BinaryOp: {operator_column} = {operator}", "")?;
                self.info_expression(f, *rhs_index, expression_indent)
            },

            Expression::Parenthesis {
                opening_round_bracket_column,
                inner_expression_index,
                closing_round_bracket_column
            } => {
                writeln!(f, "{:indent$}ParenthesisExpression", "")?;
                writeln!(f, "{:>expression_indent$}OpeningParenthesis: {opening_round_bracket_column} = (", "")?;
                self.info_expression(f, *inner_expression_index, expression_indent)?;
                writeln!(f, "{:>expression_indent$}ClosingParenthesis: {closing_round_bracket_column} = )", "")
            },

            Expression::Index {
                indexed_expression_index,
                opening_square_bracket_column,
                index_expression_index,
                closing_square_bracket_column
            } => {
                writeln!(f, "{:indent$}ArrayIndex", "")?;
                self.info_expression(f, *indexed_expression_index, expression_indent)?;
                writeln!(f, "{:>expression_indent$}OpeningBracket: {opening_square_bracket_column} = [", "")?;
                self.info_expression(f, *index_expression_index, expression_indent)?;
                writeln!(f, "{:>expression_indent$}ClosingBracket: {closing_square_bracket_column} = ]", "")
            },
        };
    }
}

impl Display for UntypedAst<'_, '_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        for node in &self.nodes {
            self.info_node(f, node, 0)?;
        }

        return Ok(());
    }
}

type TokenIndex = u32;

#[derive(Debug)]
pub struct Parser<'src, 'tokens: 'src> {
    src: &'src SrcFile,
    errors: Vec<Error<ErrorKind>>,

    token_index: TokenIndex,
    tokens: &'tokens [Token<'src>],

    ast: UntypedAst<'src, 'tokens>,
}

impl<'src, 'tokens: 'src> Parser<'src, 'tokens> {
    pub fn parse(src: &'src SrcFile, tokens: &'tokens [Token<'src>]) -> Result<UntypedAst<'src, 'tokens>, Vec<Error<ErrorKind>>> {
        let ast = UntypedAst {
            nodes: Vec::new(),

            expressions: Vec::new(),
            arrays: Vec::new(),
        };

        if tokens.is_empty() {
            return Ok(ast);
        }

        let mut this = Self {
            src,
            errors: Vec::new(),

            token_index: 0,
            tokens,

            ast,
        };

        this.parse_tokens();

        return if this.errors.is_empty() { Ok(this.ast) } else { Err(this.errors) };
    }

    /* NOTE(stefano):
    only parsing until the first error until a fault tolerant parser is developed,
    this is because the first truly relevant error is the first one, which in turn causes a ripple
    effect that propagates to the rest of the parsing, causing subsequent errors to be wrong
    */
    fn parse_tokens(&mut self) {
        while let Some(token) = self.next_token() {
            let node = match self.any(token) {
                Ok(Some(node)) => node,
                Ok(None) => break,
                Err(err) => {
                    self.errors.push(err);

                    // consuming all remaining tokens until the end of the file
                    self.token_index = self.tokens.len() as offset;
                    break;
                },
            };

            self.ast.nodes.push(node);
        }
    }

    fn semicolon(&mut self) -> Result<(), Error<ErrorKind>> {
        let Some(PeekedToken {
            token: semicolon_token,
            index: semicolon_token_index
        }) = self.peek_next_token() else {
            /* IDEA(stefano):
            suggest multiple expected places when encountering block comments:

            ```kay
            # either
            let i = 3 #{ comment #}
                     ^ missing semicolon

            # or
            let i = 3 #{ comment #}
                                   ^ missing semicolon
            ```
            */

            let PeekedToken { token: previous_token, .. } = self.peek_previous_token();
            return Err(Error {
                kind: ErrorKind::PrematureEndOfFile(Expected::Semicolon),
                col: previous_token.col,
                pointers_count: 1,
            });
        };

        let TokenKind::SemiColon = semicolon_token.kind else {
            let PeekedToken { token: previous_token, .. } = self.peek_previous_token();
            return Err(Error {
                kind: ErrorKind::MissingSemicolon,
                col: previous_token.col,
                pointers_count: 1,
            });
        };

        self.token_index = semicolon_token_index;
        return Ok(());
    }

    fn any(&mut self, token: &'tokens Token<'src>) -> Result<Option<Node>, Error<ErrorKind>> {
        return match &token.kind {
            TokenKind::True
            | TokenKind::False
            | TokenKind::Integer(_, _)
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
                let expression_index = self.expression(token)?;
                let PeekedToken { token: end_of_expression_token, .. } = self.peek_previous_token();

                let after_expression_token = self.next_expected_token(Expected::Semicolon)?;
                match after_expression_token.kind {
                    TokenKind::SemiColon => Ok(Some(Node::Expression(expression_index))),
                    TokenKind::Op(operator) => match operator {
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
                            #[allow(clippy::wildcard_enum_match_arm)]
                            let assignment_operator = match operator {
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
                                Op::LeftRotateEquals => AssignmentOp::LeftRotate,
                                Op::RightRotateEquals => AssignmentOp::RightRotate,
                                Op::BitAndEquals => AssignmentOp::BitAnd,
                                Op::BitXorEquals => AssignmentOp::BitXor,
                                Op::BitOrEquals => AssignmentOp::BitOr,
                                Op::AndEquals => AssignmentOp::And,
                                Op::OrEquals => AssignmentOp::Or,
                                _ => self.invalid_token(
                                    after_expression_token,
                                    "unexpected operator".into(),
                                    "not an 'equals' operator".into(),
                                )
                            };

                            let start_of_new_value_token = self.next_expected_token(Expected::Expression)?;
                            let new_value = self.expression(start_of_new_value_token)?;

                            self.semicolon()?;
                            Ok(Some(Node::Assignment {
                                lhs: expression_index,
                                operator: assignment_operator,
                                operator_column: after_expression_token.col,
                                rhs: new_value
                            }))
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
                        | Op::LessOrEquals => self.invalid_token(
                            after_expression_token,
                            "unexpected operator".into(),
                            "should have been part of the lhs".into(),
                        )
                    },
                    TokenKind::Bracket(_)
                    | TokenKind::Colon
                    | TokenKind::Comma
                    | TokenKind::False
                    | TokenKind::True
                    | TokenKind::Integer(_, _)
                    | TokenKind::Ascii(_)
                    | TokenKind::Str(_)
                    | TokenKind::RawStr(_)
                    | TokenKind::Identifier(_)
                    | TokenKind::Print
                    | TokenKind::PrintLn
                    | TokenKind::Eprint
                    | TokenKind::EprintLn
                    | TokenKind::Mutability(_)
                    | TokenKind::Do
                    | TokenKind::If
                    | TokenKind::Else
                    | TokenKind::Loop
                    | TokenKind::Break
                    | TokenKind::Continue => {
                        Err(Error {
                            kind: ErrorKind::MissingSemicolon,
                            col: end_of_expression_token.col,
                            pointers_count: end_of_expression_token.kind.display_len(),
                        })
                    }
                    TokenKind::Unexpected(_)
                    | TokenKind::Comment(_)
                    | TokenKind::BlockComment(_) => self.should_have_been_skipped(token),
                }
            }

            TokenKind::SemiColon => Ok(None),

            TokenKind::Bracket(_) => todo!(),

            TokenKind::Print => {
                let start_of_argument_token = self.next_expected_token(Expected::Expression)?;
                let argument = self.expression(start_of_argument_token)?;
                self.semicolon()?;
                Ok(Some(Node::Print { print_column: token.col, argument }))
            }
            TokenKind::PrintLn => {
                if let Some(PeekedToken {
                    token: &Token { kind: TokenKind::SemiColon, .. },
                    index: semicolon_token_index
                }) = self.peek_next_token() {
                    self.token_index = semicolon_token_index;
                    return Ok(Some(Node::Println { println_column: token.col, argument: None }));
                }

                let start_of_argument_token = self.next_expected_token(Expected::Expression)?;
                let argument = self.expression(start_of_argument_token)?;
                self.semicolon()?;
                Ok(Some(Node::Println { println_column: token.col, argument: Some(argument) }))
            }
            TokenKind::Eprint => {
                let start_of_argument_token = self.next_expected_token(Expected::Expression)?;
                let argument = self.expression(start_of_argument_token)?;
                self.semicolon()?;
                Ok(Some(Node::Eprint { eprint_column: token.col, argument }))
            }
            TokenKind::EprintLn => {
                if let Some(PeekedToken {
                    token: &Token { kind: TokenKind::SemiColon, .. },
                    index: semicolon_token_index
                }) = self.peek_next_token() {
                    self.token_index = semicolon_token_index;
                    return Ok(Some(Node::Eprintln { eprintln_column: token.col, argument: None }));
                }

                let start_of_argument_token = self.next_expected_token(Expected::Expression)?;
                let argument = self.expression(start_of_argument_token)?;
                self.semicolon()?;
                Ok(Some(Node::Eprintln { eprintln_column: token.col, argument: Some(argument) }))
            }

            TokenKind::Mutability(_) => todo!(),

            TokenKind::If => todo!(),
            TokenKind::Else => todo!(),

            TokenKind::Do => todo!(),
            TokenKind::Loop => todo!(),
            TokenKind::Break => todo!(),
            TokenKind::Continue => todo!(),

            TokenKind::Colon => Err(Error {
                kind: ErrorKind::StrayColon,
                col: token.col,
                pointers_count: token.kind.display_len(),
            }),
            TokenKind::Comma => Err(Error {
                kind: ErrorKind::StrayComma,
                col: token.col,
                pointers_count: token.kind.display_len(),
            }),
            TokenKind::Op(op) => Err(Error {
                kind: ErrorKind::StrayOperator(*op),
                col: token.col,
                pointers_count: token.kind.display_len(),
            }),
            TokenKind::Unexpected(_)
            | TokenKind::Comment(_)
            | TokenKind::BlockComment(_) => self.should_have_been_skipped(token),
        }
    }
}

impl<'src, 'tokens: 'src> Parser<'src, 'tokens> {
    #[allow(clippy::panic)]
    #[track_caller]
    fn invalid_token(
        &self,
        token: &'tokens Token<'src>,
        error_message: Cow<'static, str>,
        error_cause_message: Cow<'static, str>,
    ) -> ! {
        let Position { line, col } = self.src.position(token.col);
        let line_span = &self.src.lines[line as usize - 1];
        let line_text = &self.src.code[line_span.start as usize..line_span.end as usize];

        let error = ErrorDisplay {
            error_message,
            file: &self.src.path,
            line,
            col,
            line_text,
            pointers_count: token.kind.display_len(),
            error_cause_message,
        };
        panic!("{error}\n");
    }

    #[track_caller]
    fn should_have_been_skipped(&self, token: &'tokens Token<'src>) -> ! {
        self.invalid_token(
            token,
            "unexpected".into(),
            "should have been skipped in the iteration of tokens".into(),
        );
    }

    #[track_caller]
    fn should_have_been_caught_during_tokenization(&self, token: &'tokens Token<'src>) -> ! {
        self.invalid_token(
            token,
            "unexpected".into(),
            "should have been caught during tokenization".into(),
        );
    }
}

#[derive(Debug, Clone, Copy)]
struct PeekedToken<'src, 'tokens: 'src> {
    token: &'tokens Token<'src>,
    index: offset,
}

impl<'src, 'tokens: 'src> Parser<'src, 'tokens> {
    fn next_token(&mut self) -> Option<&'tokens Token<'src>> {
        let PeekedToken { token: next, index: next_index } = self.peek_next_token()?;
        self.token_index = next_index;
        return Some(next);
    }

    fn next_expected_token(
        &mut self,
        expected: Expected,
    ) -> Result<&'tokens Token<'src>, Error<ErrorKind>> {
        let Some(PeekedToken { token: next, index: next_index }) = self.peek_next_token() else {
            /* IDEA(stefano):
            suggest multiple expected places when encountering block comments:

            ```kay
            # either
            let i = 3 #{ comment #}
                     ^ missing semicolon

            # or
            let i = 3 #{ comment #}
                                   ^ missing semicolon
            ```
            */

            let PeekedToken { token: previous_token, .. } = self.peek_previous_token();
            return Err(Error {
                kind: ErrorKind::PrematureEndOfFile(expected),
                col: previous_token.col,
                pointers_count: 1,
            });
        };

        self.token_index = next_index;
        return Ok(next);
    }

    fn peek_next_token(&self) -> Option<PeekedToken<'src, 'tokens>> {
        for token_index in self.token_index..self.tokens.len() as offset {
            let next = &self.tokens[token_index as usize];

            match &next.kind {
                TokenKind::Bracket(_)
                | TokenKind::Colon
                | TokenKind::SemiColon
                | TokenKind::Comma
                | TokenKind::Op(_)
                | TokenKind::False
                | TokenKind::True
                | TokenKind::Integer(_, _)
                | TokenKind::Ascii(_)
                | TokenKind::Str(_)
                | TokenKind::RawStr(_)
                | TokenKind::Identifier(_)
                | TokenKind::Print
                | TokenKind::PrintLn
                | TokenKind::Eprint
                | TokenKind::EprintLn
                | TokenKind::Mutability(_)
                | TokenKind::Do
                | TokenKind::If
                | TokenKind::Else
                | TokenKind::Loop
                | TokenKind::Break
                | TokenKind::Continue => return Some(PeekedToken {
                    token: next,
                    index: token_index + 1,
                }),
                TokenKind::Comment(_) | TokenKind::BlockComment(_) => {},
                TokenKind::Unexpected(_) => self.should_have_been_caught_during_tokenization(next),
            }
        }
        return None;
    }

    // Note: this function is always called when underflowing the tokens array is never the case,
    // so there is no need for bounds checking
    fn peek_previous_token(&self) -> PeekedToken<'src, 'tokens> {
        for token_index in (0..self.token_index).rev() {
            let previous = &self.tokens[token_index as usize];

            match &previous.kind {
                TokenKind::Bracket(_)
                | TokenKind::Colon
                | TokenKind::SemiColon
                | TokenKind::Comma
                | TokenKind::Op(_)
                | TokenKind::False
                | TokenKind::True
                | TokenKind::Integer(_, _)
                | TokenKind::Ascii(_)
                | TokenKind::Str(_)
                | TokenKind::RawStr(_)
                | TokenKind::Identifier(_)
                | TokenKind::Print
                | TokenKind::PrintLn
                | TokenKind::Eprint
                | TokenKind::EprintLn
                | TokenKind::Mutability(_)
                | TokenKind::Do
                | TokenKind::If
                | TokenKind::Else
                | TokenKind::Loop
                | TokenKind::Break
                | TokenKind::Continue => return PeekedToken {
                    token: previous,
                    index: token_index
                },
                TokenKind::Comment(_) | TokenKind::BlockComment(_) => {},
                TokenKind::Unexpected(_) => self.should_have_been_caught_during_tokenization(previous),
            }
        }

        unreachable!("should never be called with no previous token");
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Operator<'src, 'tokens: 'src> {
    token: &'tokens Token<'src>,
    operator: Op,
}

// TODO(stefano): make less recursive by only recursing based on operator precedence
impl<'src, 'tokens: 'src> Parser<'src, 'tokens> {
    fn operator(&mut self, accepted_operators: &[Op]) -> Option<Operator<'src, 'tokens>> {
        let PeekedToken { token: operator_token, index: operator_index } = self.peek_next_token()?;
        let TokenKind::Op(operator) = operator_token.kind else {
            return None;
        };

        for accepted_operator in accepted_operators {
            if *accepted_operator == operator {
                self.token_index = operator_index;
                return Some(Operator { token: operator_token, operator });
            }
        }
        return None;
    }

    fn primary_expression(&mut self, token: &'tokens Token<'src>) -> Result<ExpressionIndex, Error<ErrorKind>> {
        let expression_result = match &token.kind {
            TokenKind::False => Ok(Expression::False { column: token.col }),
            TokenKind::True => Ok(Expression::True { column: token.col }),
            TokenKind::Integer(base, literal) => Ok(Expression::Integer {
                base: *base,
                literal,
                column: token.col,
            }),
            TokenKind::Ascii(character) => Ok(Expression::Ascii {
                character: *character,
                column: token.col,
            }),
            TokenKind::Str(literal) => Ok(Expression::Str { literal, column: token.col }),
            TokenKind::RawStr(literal) => Ok(Expression::RawStr { literal, column: token.col }),
            TokenKind::Identifier(identifier) => Ok(Expression::Identifier {
                identifier,
                column: token.col,
            }),
            TokenKind::Bracket(BracketKind::OpenRound) => 'parenthesis: {
                let opening_round_bracket_token = token;

                let start_of_inner_expression_token = self.next_expected_token(Expected::Operand)?;
                // NOTE(stefano): maybe move this check to later stages
                if let TokenKind::Bracket(BracketKind::CloseRound) = start_of_inner_expression_token.kind {
                    break 'parenthesis Err(Error {
                        kind: ErrorKind::EmptyParenthesisExpression,
                        col: start_of_inner_expression_token.col,
                        pointers_count: start_of_inner_expression_token.kind.display_len(),
                    });
                }

                let inner_expression_index = self.expression(start_of_inner_expression_token)?;

                let closing_round_bracket_token = self.next_expected_token(Expected::ClosingRoundBracket)?;
                let TokenKind::Bracket(BracketKind::CloseRound) = closing_round_bracket_token.kind else {
                    break 'parenthesis Err(Error {
                        kind: ErrorKind::ExpectedBracket(BracketKind::CloseRound),
                        col: closing_round_bracket_token.col,
                        pointers_count: closing_round_bracket_token.kind.display_len(),
                    });
                };

                Ok(Expression::Parenthesis {
                    opening_round_bracket_column: opening_round_bracket_token.col,
                    inner_expression_index,
                    closing_round_bracket_column: closing_round_bracket_token.col,
                })
            }
            TokenKind::Bracket(BracketKind::OpenSquare) => 'array: {
                let opening_square_bracket_token = token;

                let start_of_first_item_token = self.next_expected_token(Expected::Expression)?;
                // REMOVE(stefano): allow arrays of 0 elements
                if let TokenKind::Bracket(BracketKind::CloseSquare) = start_of_first_item_token.kind {
                    break 'array Err(Error {
                        kind: ErrorKind::ArrayOfZeroItems,
                        col: opening_square_bracket_token.col,
                        pointers_count: opening_square_bracket_token.kind.display_len(),
                    });
                }

                let first_item = self.expression(start_of_first_item_token)?;

                let first_comma_token = self.next_expected_token(Expected::Comma)?;
                let TokenKind::Comma = first_comma_token.kind else {
                    break 'array Err(Error {
                        kind: ErrorKind::ExpectedComma,
                        col: first_comma_token.col,
                        pointers_count: first_comma_token.kind.display_len(),
                    });
                };

                let start_of_second_item_token = self.next_expected_token(Expected::Expression)?;
                // REMOVE(stefano): allow arrays of 1 element
                if let TokenKind::Bracket(BracketKind::CloseSquare) = start_of_second_item_token.kind {
                    break 'array Err(Error {
                        kind: ErrorKind::ArrayOfOneItem,
                        col: opening_square_bracket_token.col,
                        pointers_count: opening_square_bracket_token.kind.display_len(),
                    });
                }

                let second_item = self.expression(start_of_second_item_token)?;

                let mut items = Vec::<ArrayItem>::new();

                loop {
                    let comma_or_closing_square_bracket_token = self.next_expected_token(Expected::CommaOrClosingSquareBracket)?;
                    match comma_or_closing_square_bracket_token.kind {
                        TokenKind::Comma => {},
                        TokenKind::Bracket(BracketKind::CloseSquare) => {
                            let array_index = self.ast.new_array(Array {
                                first_item,
                                first_comma_column: first_comma_token.col,
                                second_item,
                                items,
                                trailing_comma_column: None,
                            });

                            break 'array Ok(Expression::Array {
                                opening_square_bracket_column: opening_square_bracket_token.col,
                                array_index,
                                closing_square_bracket_column: comma_or_closing_square_bracket_token.col,
                            });
                        }
                        TokenKind::Colon
                        | TokenKind::SemiColon
                        | TokenKind::Op(_)
                        | TokenKind::Bracket(_)
                        | TokenKind::False
                        | TokenKind::True
                        | TokenKind::Integer(_, _)
                        | TokenKind::Ascii(_)
                        | TokenKind::Str(_)
                        | TokenKind::RawStr(_)
                        | TokenKind::Identifier(_)
                        | TokenKind::Print
                        | TokenKind::PrintLn
                        | TokenKind::Eprint
                        | TokenKind::EprintLn
                        | TokenKind::Mutability(_)
                        | TokenKind::Do
                        | TokenKind::If
                        | TokenKind::Else
                        | TokenKind::Loop
                        | TokenKind::Break
                        | TokenKind::Continue => break 'array Err(Error {
                            kind: ErrorKind::ExpectedComma,
                            col: comma_or_closing_square_bracket_token.col,
                            pointers_count: comma_or_closing_square_bracket_token.kind.display_len(),
                        }),
                        TokenKind::Unexpected(_)
                        | TokenKind::Comment(_)
                        | TokenKind::BlockComment(_) => self.invalid_token(
                            token,
                            "unexpected".into(),
                            "should have been skipped in the iteration of tokens".into()
                        ),

                    }

                    let start_of_item_token = self.next_expected_token(Expected::ArrayItemOrClosingSquareBracket)?;
                    if let TokenKind::Bracket(BracketKind::CloseSquare) = start_of_item_token.kind {
                        let array_index = self.ast.new_array(Array {
                            first_item,
                            first_comma_column: first_comma_token.col,
                            second_item,
                            items,
                            trailing_comma_column: Some(comma_or_closing_square_bracket_token.col),
                        });

                        break 'array Ok(Expression::Array {
                            opening_square_bracket_column: opening_square_bracket_token.col,
                            array_index,
                            closing_square_bracket_column: start_of_item_token.col,
                        });
                    }

                    let item = self.expression(start_of_item_token)?;
                    items.push(ArrayItem { comma_col: start_of_item_token.col, item });
                }
            }
            TokenKind::Op(
                operator @ (Op::Len
                | Op::Plus
                | Op::WrappingPlus
                | Op::SaturatingPlus
                | Op::Minus
                | Op::WrappingMinus
                | Op::SaturatingMinus
                | Op::Not)
            ) => {
                let start_of_prefix_expression = self.next_expected_token(Expected::Expression)?;
                let prefix_expression_index = self.primary_expression(start_of_prefix_expression)?;

                #[allow(clippy::wildcard_enum_match_arm)]
                let prefix_operator = match operator {
                    Op::Len => PrefixOp::Len,
                    Op::Plus => PrefixOp::Plus,
                    Op::WrappingPlus => PrefixOp::WrappingPlus,
                    Op::SaturatingPlus => PrefixOp::SaturatingPlus,
                    Op::Minus => PrefixOp::Minus,
                    Op::WrappingMinus => PrefixOp::WrappingMinus,
                    Op::SaturatingMinus => PrefixOp::SaturatingMinus,
                    Op::Not => PrefixOp::Not,
                    _ => self.invalid_token(
                        token,
                        "unexpected operator".into(),
                        "not a prefix operator".into(),
                    ),
                };

                Ok(Expression::Prefix {
                    operator: prefix_operator,
                    operator_column: token.col,
                    prefix_expression_index,
                })
            }
            TokenKind::Mutability(_)
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
                col: token.col,
                pointers_count: token.kind.display_len(),
            }),
            TokenKind::Bracket(_)
            | TokenKind::Op(_)
            | TokenKind::Colon
            | TokenKind::SemiColon
            | TokenKind::Comma => Err(Error {
                kind: ErrorKind::ExpectedOperand,
                col: token.col,
                pointers_count: token.kind.display_len(),
            }),
            TokenKind::Unexpected(_)
            | TokenKind::Comment(_)
            | TokenKind::BlockComment(_) => self.invalid_token(
                token,
                "unexpected".into(),
                "should have been skipped in the iteration of tokens".into()
            ),
        };

        let mut expression = expression_result?;
        loop {
            let Some(PeekedToken {
                token: opening_square_bracket_token,
                index: opening_square_bracket_token_index
            }) = self.peek_next_token() else {
                break;
            };

            let TokenKind::Bracket(BracketKind::OpenSquare) = opening_square_bracket_token.kind else {
                break;
            };

            self.token_index = opening_square_bracket_token_index;

            let start_of_index_expression_token = self.next_expected_token(Expected::Expression)?;
            let index_expression_index = self.expression(start_of_index_expression_token)?;
            let PeekedToken { token: end_of_expression_token, .. } = self.peek_previous_token();

            let after_expression_token = self.next_expected_token(Expected::ClosingSquareBracket)?;
            let TokenKind::Bracket(BracketKind::CloseSquare) = after_expression_token.kind else {
                return Err(Error {
                    kind: ErrorKind::MissingClosingSquareBracketInIndex,
                    col: end_of_expression_token.col,
                    pointers_count: end_of_expression_token.kind.display_len(),
                });
            };

            expression = Expression::Index {
                indexed_expression_index: self.ast.new_expression(expression),
                opening_square_bracket_column: opening_square_bracket_token.col,
                index_expression_index,
                closing_square_bracket_column: after_expression_token.col,
            };
        }

        let expression_index = self.ast.new_expression(expression);
        return Ok(expression_index);
    }

    fn exponentiative_expression(&mut self, token: &'tokens Token<'src>) -> Result<ExpressionIndex, Error<ErrorKind>> {
        static OPS: [Op; 3] = [Op::Pow, Op::WrappingPow, Op::SaturatingPow];

        let mut lhs_index = self.primary_expression(token)?;
        while let Some(Operator { token: operator_token, operator }) = self.operator(&OPS) {
            let start_of_rhs_token = self.next_expected_token(Expected::Operand)?;
            let rhs_index = self.primary_expression(start_of_rhs_token)?;

            #[allow(clippy::wildcard_enum_match_arm)]
            let binary_operator = match operator {
                Op::Pow => BinaryOp::Pow,
                Op::WrappingPow => BinaryOp::WrappingPow,
                Op::SaturatingPow => BinaryOp::SaturatingPow,
                _ => self.invalid_token(
                    operator_token,
                    "unexpected operator".into(),
                    "not an exponentiative operator".into(),
                ),
            };

            lhs_index = self.ast.new_expression(Expression::Binary {
                lhs_index,
                operator: binary_operator,
                operator_column: operator_token.col,
                rhs_index,
            });
        }

        return Ok(lhs_index);
    }

    fn multiplicative_expression(&mut self, token: &'tokens Token<'src>) -> Result<ExpressionIndex, Error<ErrorKind>> {
        static OPS: [Op; 7] = [
            Op::Times,
            Op::WrappingTimes,
            Op::SaturatingTimes,
            Op::Divide,
            Op::WrappingDivide,
            Op::SaturatingDivide,
            Op::Remainder,
        ];

        let mut lhs_index = self.exponentiative_expression(token)?;
        while let Some(Operator { token: operator_token, operator }) = self.operator(&OPS) {
            let start_of_rhs_token = self.next_expected_token(Expected::Operand)?;
            let rhs_index = self.exponentiative_expression(start_of_rhs_token)?;

            #[allow(clippy::wildcard_enum_match_arm)]
            let binary_operator = match operator {
                Op::Times => BinaryOp::Times,
                Op::WrappingTimes => BinaryOp::WrappingTimes,
                Op::SaturatingTimes => BinaryOp::SaturatingTimes,
                Op::Divide => BinaryOp::Divide,
                Op::WrappingDivide => BinaryOp::WrappingDivide,
                Op::SaturatingDivide => BinaryOp::SaturatingDivide,
                Op::Remainder => BinaryOp::Remainder,
                _ => self.invalid_token(
                    operator_token,
                    "unexpected operator".into(),
                    "not a multiplicative operator".into(),
                ),
            };

            lhs_index = self.ast.new_expression(Expression::Binary {
                lhs_index,
                operator: binary_operator,
                operator_column: operator_token.col,
                rhs_index,
            });
        }

        return Ok(lhs_index);
    }

    fn additive_expression(&mut self, token: &'tokens Token<'src>) -> Result<ExpressionIndex, Error<ErrorKind>> {
        static OPS: [Op; 6] = [
            Op::Plus,
            Op::WrappingPlus,
            Op::SaturatingPlus,
            Op::Minus,
            Op::WrappingMinus,
            Op::SaturatingMinus,
        ];

        let mut lhs_index = self.multiplicative_expression(token)?;
        while let Some(Operator { token: operator_token, operator }) = self.operator(&OPS) {
            let start_of_rhs_token = self.next_expected_token(Expected::Operand)?;
            let rhs_index = self.multiplicative_expression(start_of_rhs_token)?;

            #[allow(clippy::wildcard_enum_match_arm)]
            let binary_operator = match operator {
                Op::Plus => BinaryOp::Plus,
                Op::WrappingPlus => BinaryOp::WrappingPlus,
                Op::SaturatingPlus => BinaryOp::SaturatingPlus,
                Op::Minus => BinaryOp::Minus,
                Op::WrappingMinus => BinaryOp::WrappingMinus,
                Op::SaturatingMinus => BinaryOp::SaturatingMinus,
                _ => self.invalid_token(
                    operator_token,
                    "unexpected operator".into(),
                    "not an additive operator".into(),
                ),
            };

            lhs_index = self.ast.new_expression(Expression::Binary {
                lhs_index,
                operator: binary_operator,
                operator_column: operator_token.col,
                rhs_index,
            });
        }

        return Ok(lhs_index);
    }

    fn shift_expression(&mut self, token: &'tokens Token<'src>) -> Result<ExpressionIndex, Error<ErrorKind>> {
        static OPS: [Op; 6] = [
            Op::LeftShift,
            Op::WrappingLeftShift,
            Op::SaturatingLeftShift,
            Op::RightShift,
            Op::LeftRotate,
            Op::RightRotate,
        ];

        let mut lhs_index = self.additive_expression(token)?;
        while let Some(Operator { token: operator_token, operator }) = self.operator(&OPS) {
            let start_of_rhs_token = self.next_expected_token(Expected::Operand)?;
            let rhs_index = self.additive_expression(start_of_rhs_token)?;

            #[allow(clippy::wildcard_enum_match_arm)]
            let binary_operator = match operator {
                Op::LeftShift => BinaryOp::LeftShift,
                Op::WrappingLeftShift => BinaryOp::WrappingLeftShift,
                Op::SaturatingLeftShift => BinaryOp::SaturatingLeftShift,
                Op::RightShift => BinaryOp::RightShift,
                Op::LeftRotate => BinaryOp::LeftRotate,
                Op::RightRotate => BinaryOp::RightRotate,
                _ => self.invalid_token(
                    operator_token,
                    "unexpected operator".into(),
                    "not a shift operator".into(),
                ),
            };

            lhs_index = self.ast.new_expression(Expression::Binary {
                lhs_index,
                operator: binary_operator,
                operator_column: operator_token.col,
                rhs_index,
            });
        }

        return Ok(lhs_index);
    }

    fn bitand_expression(&mut self, token: &'tokens Token<'src>) -> Result<ExpressionIndex, Error<ErrorKind>> {
        static OPS: [Op; 1] = [Op::BitAnd];

        let mut lhs_index = self.shift_expression(token)?;
        while let Some(Operator { token: operator_token, operator }) = self.operator(&OPS) {
            let start_of_rhs_token = self.next_expected_token(Expected::Operand)?;
            let rhs_index = self.shift_expression(start_of_rhs_token)?;

            #[allow(clippy::wildcard_enum_match_arm)]
            let binary_operator = match operator {
                Op::BitAnd => BinaryOp::BitAnd,
                _ => self.invalid_token(
                    operator_token,
                    "unexpected operator".into(),
                    "not a bitand operator".into(),
                ),
            };

            lhs_index = self.ast.new_expression(Expression::Binary {
                lhs_index,
                operator: binary_operator,
                operator_column: operator_token.col,
                rhs_index,
            });
        }

        return Ok(lhs_index);
    }

    fn bitxor_expression(&mut self, token: &'tokens Token<'src>) -> Result<ExpressionIndex, Error<ErrorKind>> {
        static OPS: [Op; 1] = [Op::BitXor];

        let mut lhs_index = self.bitand_expression(token)?;
        while let Some(Operator { token: operator_token, operator }) = self.operator(&OPS) {
            let start_of_rhs_token = self.next_expected_token(Expected::Operand)?;
            let rhs_index = self.bitand_expression(start_of_rhs_token)?;

            #[allow(clippy::wildcard_enum_match_arm)]
            let binary_operator = match operator {
                Op::BitXor => BinaryOp::BitXor,
                _ => self.invalid_token(
                    operator_token,
                    "unexpected operator".into(),
                    "not a bitxor operator".into(),
                ),
            };

            lhs_index = self.ast.new_expression(Expression::Binary {
                lhs_index,
                operator: binary_operator,
                operator_column: operator_token.col,
                rhs_index,
            });
        }

        return Ok(lhs_index);
    }

    fn bitor_expression(&mut self, token: &'tokens Token<'src>) -> Result<ExpressionIndex, Error<ErrorKind>> {
        static OPS: [Op; 1] = [Op::BitOr];

        let mut lhs_index = self.bitxor_expression(token)?;
        while let Some(Operator { token: operator_token, operator }) = self.operator(&OPS) {
            let start_of_rhs_token = self.next_expected_token(Expected::Operand)?;
            let rhs_index = self.bitxor_expression(start_of_rhs_token)?;

            #[allow(clippy::wildcard_enum_match_arm)]
            let binary_operator = match operator {
                Op::BitOr => BinaryOp::BitOr,
                _ => self.invalid_token(
                    operator_token,
                    "unexpected operator".into(),
                    "not a bitor operator".into(),
                ),
            };

            lhs_index = self.ast.new_expression(Expression::Binary {
                lhs_index,
                operator: binary_operator,
                operator_column: operator_token.col,
                rhs_index,
            });
        }

        return Ok(lhs_index);
    }

    fn comparison_expression(&mut self, token: &'tokens Token<'src>) -> Result<ExpressionIndex, Error<ErrorKind>> {
        static OPS: [Op; 7] = [
            Op::Compare,
            Op::EqualsEquals,
            Op::NotEquals,
            Op::Greater,
            Op::GreaterOrEquals,
            Op::Less,
            Op::LessOrEquals,
        ];

        let mut lhs_index = self.bitor_expression(token)?;
        while let Some(Operator { token: operator_token, operator }) = self.operator(&OPS) {
            let start_of_rhs_token = self.next_expected_token(Expected::Operand)?;
            let rhs_index = self.bitor_expression(start_of_rhs_token)?;

            #[allow(clippy::wildcard_enum_match_arm)]
            let comparison_operator = match operator {
                Op::Compare => BinaryOp::Compare,
                Op::EqualsEquals => BinaryOp::EqualsEquals,
                Op::NotEquals => BinaryOp::NotEquals,
                Op::Greater => BinaryOp::Greater,
                Op::GreaterOrEquals => BinaryOp::GreaterOrEquals,
                Op::Less => BinaryOp::Less,
                Op::LessOrEquals => BinaryOp::LessOrEquals,
                _ => self.invalid_token(
                    operator_token,
                    "unexpected operator".into(),
                    "not a comparison operator".into(),
                ),
            };
            lhs_index = self.ast.new_expression(Expression::Binary {
                lhs_index,
                operator: comparison_operator,
                operator_column: operator_token.col,
                rhs_index,
            });
        }

        return Ok(lhs_index);
    }

    fn and_expression(&mut self, token: &'tokens Token<'src>) -> Result<ExpressionIndex, Error<ErrorKind>> {
        static OPS: [Op; 1] = [Op::And];

        let mut lhs_index = self.comparison_expression(token)?;
        while let Some(Operator { token: operator_token, operator }) = self.operator(&OPS) {
            let start_of_rhs_token = self.next_expected_token(Expected::Operand)?;
            let rhs_index = self.comparison_expression(start_of_rhs_token)?;

            #[allow(clippy::wildcard_enum_match_arm)]
            let binary_operator = match operator {
                Op::And => BinaryOp::And,
                _ => self.invalid_token(
                    operator_token,
                    "unexpected operator".into(),
                    "not an and operator".into(),
                ),
            };

            lhs_index = self.ast.new_expression(Expression::Binary {
                lhs_index,
                operator: binary_operator,
                operator_column: operator_token.col,
                rhs_index,
            });
        }

        return Ok(lhs_index);
    }

    fn or_expression(&mut self, token: &'tokens Token<'src>) -> Result<ExpressionIndex, Error<ErrorKind>> {
        static OPS: [Op; 1] = [Op::Or];

        let mut lhs_index = self.and_expression(token)?;
        while let Some(Operator { token: operator_token, operator }) = self.operator(&OPS) {
            let start_of_rhs_token = self.next_expected_token(Expected::Operand)?;
            let rhs_index = self.and_expression(start_of_rhs_token)?;

            #[allow(clippy::wildcard_enum_match_arm)]
            let binary_operator = match operator {
                Op::Or => BinaryOp::Or,
                _ => self.invalid_token(
                    operator_token,
                    "unexpected operator".into(),
                    "not an or operator".into(),
                ),
            };

            lhs_index = self.ast.new_expression(Expression::Binary {
                lhs_index,
                operator: binary_operator,
                operator_column: operator_token.col,
                rhs_index,
            });
        }

        return Ok(lhs_index);
    }

    fn expression(&mut self, token: &'tokens Token<'src>) -> Result<ExpressionIndex, Error<ErrorKind>> {
        todo!("check that self.token_index points to the token right after the end of the expression");
        return self.or_expression(token);
    }
}

#[derive(Debug, Clone)]
pub enum Expected {
    OperatorOrSemicolon,
    Operand,
    ClosingRoundBracket,
    ClosingSquareBracket,
    Expression,
    Comma,
    CommaOrClosingSquareBracket,
    ArrayItemOrClosingSquareBracket,
    Semicolon,
}

impl Display for Expected {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return match self {
            Self::OperatorOrSemicolon => write!(f, "operator or semicolon"),
            Self::Operand => write!(f, "operand"),
            Self::ClosingRoundBracket => write!(f, "closing round bracket"),
            Self::ClosingSquareBracket => write!(f, "closing round bracket"),
            Self::Expression => write!(f, "expression"),
            Self::Comma => write!(f, "comma"),
            Self::CommaOrClosingSquareBracket => write!(f, "comma or closing square bracket"),
            Self::ArrayItemOrClosingSquareBracket => write!(f, "array item or closing square bracket"),
            Self::Semicolon => write!(f, "semicolon"),
        };
    }
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    PrematureEndOfFile(Expected),
    MissingSemicolon,
    StrayColon,
    StrayComma,
    StrayOperator(Op),

    // expressions
    CannotChainComparisons,
    KeywordInExpression,
    ExpectedOperand,
    EmptyParenthesisExpression,
    ExpectedBracket(BracketKind),
    ArrayOfZeroItems,
    ArrayOfOneItem,
    ExpectedComma,
    MissingClosingSquareBracketInIndex,
}

impl IntoErrorInfo for ErrorKind {
    fn info(&self) -> ErrorInfo {
        let (error_message, error_cause_message) = match self {
            Self::PrematureEndOfFile(expected) => (
                "premature end of file".into(),
                format!("expected {expected} after here").into(),
            ),
            Self::MissingSemicolon => (
                "invalid statement".into(),
                "expected semicolon after here".into(),
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

            Self::CannotChainComparisons => (
                "invalid expression".into(),
                "comparison operators cannot be chained".into(),
            ),
            Self::KeywordInExpression => (
                "invalid expression".into(),
                "cannot be a keyword".into(),
            ),
            Self::ExpectedOperand => (
                "invalid expression".into(),
                "expected operand before this token".into(),
            ),
            Self::EmptyParenthesisExpression => (
                "invalid expression".into(),
                "empty parenthesis expressions are not allowed".into(),
            ),
            Self::ExpectedBracket(bracket) => (
                "invalid expression".into(),
                format!("expected '{bracket}' bracket before this token").into(),
            ),
            Self::ArrayOfZeroItems => (
                "invalid array".into(),
                "arrays of zero items are not allowed, as they are practically phantom values".into(),
            ),
            Self::ArrayOfOneItem => (
                "invalid array".into(),
                "arrays of one item are not allowed, as they are practically the same as the item itself".into(),
            ),
            Self::ExpectedComma => (
                "invalid array".into(),
                "expected ',' before this token".into(),
            ),
            Self::MissingClosingSquareBracketInIndex => (
                "invalid array index".into(),
                "must be followed by a closing square bracket".into(),
            ),
        };

        return ErrorInfo { error_message, error_cause_message };
    }
}
