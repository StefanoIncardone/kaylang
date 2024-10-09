// IDEA(stefano): make every offset that can never be zero into NonZero<offset>

use super::{
    src_file::{index32, offset32, DisplayPosition, SrcFile},
    tokenizer::{
        ascii, utf8, Base, Bracket, DisplayLen, Integer, Mutability, Op, Str, Token, TokenKind,
    },
    Error, ErrorDisplay, ErrorInfo, IntoErrorInfo,
};
use core::{fmt::Display, num::NonZero};
use std::borrow::Cow;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
#[allow(dead_code)]
#[rustfmt::skip]
pub(crate) enum PrefixOperator {
    Len = Op::Len as u8,
    Not = Op::Not as u8,

    Plus           = Op::Plus as u8,
    WrappingPlus   = Op::WrappingPlus as u8,
    SaturatingPlus = Op::SaturatingPlus as u8,

    Minus           = Op::Minus as u8,
    WrappingMinus   = Op::WrappingMinus as u8,
    SaturatingMinus = Op::SaturatingMinus as u8,
}

impl Into<PrefixOperator> for Op {
    #[inline(always)]
    fn into(self) -> PrefixOperator {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Op> for PrefixOperator {
    #[inline(always)]
    fn into(self) -> Op {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Display for PrefixOperator {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl DisplayLen for PrefixOperator {
    #[inline(always)]
    fn display_len(&self) -> offset32 {
        let op: Op = (*self).into();
        return op.display_len();
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
#[allow(dead_code)]
#[rustfmt::skip]
pub(crate) enum BinaryOperator {
    // binary operators
    Pow           = Op::Pow as u8,
    WrappingPow   = Op::WrappingPow as u8,
    SaturatingPow = Op::SaturatingPow as u8,

    Times           = Op::Times as u8,
    WrappingTimes   = Op::WrappingTimes as u8,
    SaturatingTimes = Op::SaturatingTimes as u8,

    Divide           = Op::Divide as u8,
    WrappingDivide   = Op::WrappingDivide as u8,
    SaturatingDivide = Op::SaturatingDivide as u8,

    Remainder = Op::Remainder as u8,

    Plus           = Op::Plus as u8,
    WrappingPlus   = Op::WrappingPlus as u8,
    SaturatingPlus = Op::SaturatingPlus as u8,

    Minus           = Op::Minus as u8,
    WrappingMinus   = Op::WrappingMinus as u8,
    SaturatingMinus = Op::SaturatingMinus as u8,

    LeftShift           = Op::LeftShift as u8,
    WrappingLeftShift   = Op::WrappingLeftShift as u8,
    SaturatingLeftShift = Op::SaturatingLeftShift as u8,

    RightShift = Op::RightShift as u8,

    LeftRotate  = Op::LeftRotate as u8,
    RightRotate = Op::RightRotate as u8,

    BitAnd = Op::BitAnd as u8,
    BitXor = Op::BitXor as u8,
    BitOr  = Op::BitOr as u8,

    // boolean binary operators
    And = Op::And as u8,
    Or  = Op::Or as u8,

    // comparison operators
    Compare = Op::Compare as u8,

    // boolean comparison operators
    EqualsEquals    = Op::EqualsEquals as u8,
    NotEquals       = Op::NotEquals as u8,
    Greater         = Op::Greater as u8,
    GreaterOrEquals = Op::GreaterOrEquals as u8,
    Less            = Op::Less as u8,
    LessOrEquals    = Op::LessOrEquals as u8,
}

impl Into<BinaryOperator> for Op {
    #[inline(always)]
    fn into(self) -> BinaryOperator {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Op> for BinaryOperator {
    #[inline(always)]
    fn into(self) -> Op {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Display for BinaryOperator {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl DisplayLen for BinaryOperator {
    #[inline(always)]
    fn display_len(&self) -> offset32 {
        let op: Op = (*self).into();
        return op.display_len();
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
#[allow(dead_code)]
#[rustfmt::skip]
pub(crate) enum AssignmentOperator {
    Equals = Op::Equals as u8,

    Pow           = Op::PowEquals as u8,
    WrappingPow   = Op::WrappingPowEquals as u8,
    SaturatingPow = Op::SaturatingPowEquals as u8,

    Times           = Op::TimesEquals as u8,
    WrappingTimes   = Op::WrappingTimesEquals as u8,
    SaturatingTimes = Op::SaturatingTimesEquals as u8,

    Divide           = Op::DivideEquals as u8,
    WrappingDivide   = Op::WrappingDivideEquals as u8,
    SaturatingDivide = Op::SaturatingDivideEquals as u8,

    Remainder = Op::RemainderEquals as u8,

    Plus           = Op::PlusEquals as u8,
    WrappingPlus   = Op::WrappingPlusEquals as u8,
    SaturatingPlus = Op::SaturatingPlusEquals as u8,

    Minus           = Op::MinusEquals as u8,
    WrappingMinus   = Op::WrappingMinusEquals as u8,
    SaturatingMinus = Op::SaturatingMinusEquals as u8,

    LeftShift           = Op::LeftShiftEquals as u8,
    WrappingLeftShift   = Op::WrappingLeftShiftEquals as u8,
    SaturatingLeftShift = Op::SaturatingLeftShiftEquals as u8,

    RightShift = Op::RightShiftEquals as u8,

    LeftRotate  = Op::LeftRotateEquals as u8,
    RightRotate = Op::RightRotateEquals as u8,

    BitAnd = Op::BitAndEquals as u8,
    BitXor = Op::BitXorEquals as u8,
    BitOr  = Op::BitOrEquals as u8,

    And    = Op::AndEquals as u8,
    Or     = Op::OrEquals as u8,
}

impl Into<AssignmentOperator> for Op {
    #[inline(always)]
    fn into(self) -> AssignmentOperator {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Op> for AssignmentOperator {
    #[inline(always)]
    fn into(self) -> Op {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Display for AssignmentOperator {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl DisplayLen for AssignmentOperator {
    #[inline(always)]
    fn display_len(&self) -> offset32 {
        let op: Op = (*self).into();
        return op.display_len();
    }
}

pub(crate) type ArrayIndex = index32;
pub(crate) type ExpressionIndex = index32;

#[derive(Debug, Clone)]
pub(crate) enum Expression<'src, 'tokens: 'src> {
    False {
        column: offset32,
    },
    True {
        column: offset32,
    },
    Integer {
        base: Base,
        literal: &'tokens Integer<'src>,
        column: offset32,
    },
    Ascii {
        character: ascii,
        column: offset32,
    },
    Str {
        literal: &'tokens Str,
        column: offset32,
    },
    RawStr {
        literal: &'tokens Str,
        column: offset32,
    },
    Identifier {
        identifier: &'tokens &'src str,
        column: offset32,
    },
    Array {
        open_square_bracket_column: offset32,
        array: ArrayIndex,
        close_square_bracket_column: offset32,
    },

    Prefix {
        operator: PrefixOperator,
        operator_column: offset32,
        right_operand: ExpressionIndex,
    },
    Binary {
        left_operand: ExpressionIndex,
        operator: BinaryOperator,
        operator_column: offset32,
        right_operand: ExpressionIndex,
    },

    Parenthesis {
        open_round_bracket_column: offset32,
        inner_expression: ExpressionIndex,
        close_round_bracket_column: offset32,
    },
    EmptyParenthesis {
        open_round_bracket_column: offset32,
        close_round_bracket_column: offset32,
    },

    Index {
        indexed_expression: ExpressionIndex,
        open_square_bracket_column: offset32,
        index_expression: ExpressionIndex,
        close_square_bracket_column: offset32,
    },
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct ArrayDimension {
    open_square_bracket_column: offset32,
    dimension_expression: ExpressionIndex,
    close_square_bracket_column: offset32,
}

#[derive(Debug, Clone)]
pub(crate) struct TypeAnnotation<'src, 'tokens: 'src> {
    colon_column: offset32,
    type_name: &'tokens &'src str,
    type_name_column: offset32,
    array_dimensions: Vec<ArrayDimension>,
}

#[derive(Debug, Clone)]
pub(crate) struct InitialValue {
    equals_column: NonZero<offset32>,
    expression: ExpressionIndex,
}

#[derive(Debug, Clone)]
pub(crate) struct VariableDefinition<'src, 'tokens: 'src> {
    name: &'tokens &'src str,
    name_column: offset32,
    type_annotation: Option<TypeAnnotation<'src, 'tokens>>,
    initial_value: Option<InitialValue>,
}

pub(crate) type VariableDefinitionIndex = index32;

#[derive(Debug, Clone)]
pub(crate) struct If {
    pub(crate) if_column: offset32,
    pub(crate) condition: ExpressionIndex,
}

pub(crate) type IfIndex = index32;

#[derive(Debug, Clone)]
pub(crate) struct ElseIf {
    pub(crate) else_column: NonZero<offset32>,
    pub(crate) iff: If,
}

#[derive(Debug, Clone)]
pub(crate) enum Node {
    Expression(ExpressionIndex),

    Print {
        print_column: offset32,
        argument: ExpressionIndex,
    },
    Println {
        println_column: offset32,
        argument: ExpressionIndex,
    },
    PrintlnNoArg {
        println_column: offset32,
    },
    Eprint {
        eprint_column: offset32,
        argument: ExpressionIndex,
    },
    Eprintln {
        eprintln_column: offset32,
        argument: ExpressionIndex,
    },
    EprintlnNoArg {
        eprintln_column: offset32,
    },

    LetVariableDefinition {
        let_column: offset32,
        variable_definition: VariableDefinitionIndex,
    },
    VarVariableDefinition {
        var_column: offset32,
        variable_definition: VariableDefinitionIndex,
    },
    // IDEA(stefano): maybe move into expressions enum
    Assignment {
        target: ExpressionIndex,
        operator: AssignmentOperator,
        operator_column: offset32,
        new_value: ExpressionIndex,
    },

    Scope {
        open_curly_bracket_column: offset32,
        raw_nodes_in_scope_count: u32,
        close_curly_bracket_column: offset32,
    },

    If {
        if_index: IfIndex,
    },
    IfElse {
        if_index: IfIndex,
        else_column: NonZero<offset32>,
    },

    Loop {
        loop_column: offset32,
        condition: ExpressionIndex,
    },
    DoLoop {
        do_column: offset32,
        loop_column: offset32,
        condition: ExpressionIndex,
    },
    Break {
        break_column: NonZero<offset32>,
    },
    Continue {
        continue_column: NonZero<offset32>,
    },
}

#[derive(Debug, Clone)]
enum ParsedNode {
    Node(Node),
    SemiColon,
    Scope,
    IfStatement,
    LoopStatement,
}

#[derive(Debug)]
pub struct SyntaxTree<'src, 'tokens: 'src> {
    nodes: Vec<Node>,

    expressions: Vec<Expression<'src, 'tokens>>,
    array_items: Vec<Vec<ExpressionIndex>>,
    array_commas_columns: Vec<Vec<NonZero<offset32>>>,

    variable_definitions: Vec<VariableDefinition<'src, 'tokens>>,

    ifs: Vec<If>,
    else_ifs: Vec<Vec<ElseIf>>,
}

impl<'src, 'tokens: 'src> SyntaxTree<'src, 'tokens> {
    #[inline]
    fn new_expression(&mut self, expression: Expression<'src, 'tokens>) -> ExpressionIndex {
        self.expressions.push(expression);
        return self.expressions.len() as index32 - 1;
    }
}

const INDENT_INCREMENT: usize = 2;

impl SyntaxTree<'_, '_> {
    fn info_node(
        &self,
        f: &mut core::fmt::Formatter<'_>,
        node_index: &mut index32,
        indent: usize,
    ) -> core::fmt::Result {
        let node = &self.nodes[*node_index as usize];
        *node_index += 1;

        #[rustfmt::skip]
        return match node {
            Node::Expression(expression_index) => self.info_expression(f, *expression_index, indent),

            Node::Print { print_column, argument } => {
                writeln!(f, "{:>indent$}Print: {print_column} = print", "")?;
                let argument_indent = indent + INDENT_INCREMENT;
                self.info_expression(f, *argument, argument_indent)
            }
            Node::Println { println_column, argument } => {
                writeln!(f, "{:>indent$}Println: {println_column} = println", "")?;
                let argument_indent = indent + INDENT_INCREMENT;
                self.info_expression(f, *argument, argument_indent)
            }
            Node::PrintlnNoArg { println_column } => {
                writeln!(f, "{:>indent$}Println: {println_column} = println", "")
            }
            Node::Eprint { eprint_column, argument } => {
                writeln!(f, "{:>indent$}Eprint: {eprint_column} = eprint", "")?;
                let argument_indent = indent + INDENT_INCREMENT;
                self.info_expression(f, *argument, argument_indent)
            }
            Node::Eprintln { eprintln_column, argument } => {
                writeln!(f, "{:>indent$}Eprintln: {eprintln_column} = eprintln", "")?;
                let argument_indent = indent + INDENT_INCREMENT;
                self.info_expression(f, *argument, argument_indent)
            }
            Node::EprintlnNoArg { eprintln_column } => {
                writeln!(f, "{:>indent$}Eprintln: {eprintln_column} = eprintln", "")
            }

            Node::LetVariableDefinition { let_column, variable_definition } => {
                writeln!(f, "{:>indent$}VariableDefinition: {let_column} = let", "")?;
                let definition_indent = indent + INDENT_INCREMENT;
                self.info_variable_definition(f, *variable_definition, definition_indent)
            }
            Node::VarVariableDefinition { var_column, variable_definition } => {
                writeln!(f, "{:>indent$}VariableDefinition: {var_column} = var", "")?;
                let definition_indent = indent + INDENT_INCREMENT;
                self.info_variable_definition(f, *variable_definition, definition_indent)
            }
            Node::Assignment { target, operator, operator_column, new_value } => {
                writeln!(f, "{:>indent$}Assignment", "")?;
                let assignment_indent = indent + INDENT_INCREMENT;
                self.info_expression(f, *target, assignment_indent)?;
                writeln!(f, "{:>assignment_indent$}AssignmentOp: {operator_column} = {operator}", "")?;
                self.info_expression(f, *new_value, assignment_indent)
            }

            Node::Scope { open_curly_bracket_column, raw_nodes_in_scope_count, close_curly_bracket_column } => {
                writeln!(f, "{:>indent$}Scope", "")?;
                let scope_indent = indent + INDENT_INCREMENT;
                writeln!(f, "{:>scope_indent$}OpenCurlyBracket: {open_curly_bracket_column} = {{", "")?;

                let after_end_scope_node_index = *node_index + raw_nodes_in_scope_count;
                while *node_index < after_end_scope_node_index {
                    self.info_node(f, node_index, scope_indent)?;
                }
                writeln!(f, "{:>scope_indent$}CloseCurlyBracket: {close_curly_bracket_column} = }}", "")
            }

            Node::If { if_index } => {
                let if_indent = indent + INDENT_INCREMENT;

                let If { if_column, condition } = &self.ifs[*if_index as usize];
                let else_ifs = &self.else_ifs[*if_index as usize];

                writeln!(f, "{:>indent$}If: {if_column} = if", "")?;
                self.info_expression(f, *condition, if_indent)?;
                self.info_node(f, node_index, if_indent)?;

                for ElseIf {
                    else_column,
                    iff: If { if_column: else_if_column, condition: else_if_condition },
                } in else_ifs {
                    writeln!(f, "{:>indent$}Else: {else_column} = else", "")?;
                    writeln!(f, "{:>indent$}If: {else_if_column} = if", "")?;
                    self.info_expression(f, *else_if_condition, if_indent)?;
                    self.info_node(f, node_index, if_indent)?;
                }

                Ok(())
            }
            Node::IfElse { if_index, else_column } => {
                let if_indent = indent + INDENT_INCREMENT;

                let If { if_column, condition } = &self.ifs[*if_index as usize];
                let else_ifs = &self.else_ifs[*if_index as usize];

                writeln!(f, "{:>indent$}If: {if_column} = if", "")?;
                self.info_expression(f, *condition, if_indent)?;
                self.info_node(f, node_index, if_indent)?;

                for ElseIf {
                    else_column: else_in_else_if_column,
                    iff: If { if_column: else_if_column, condition: else_if_condition },
                } in else_ifs {
                    writeln!(f, "{:>indent$}Else: {else_in_else_if_column} = else", "")?;
                    writeln!(f, "{:>indent$}If: {else_if_column} = if", "")?;
                    self.info_expression(f, *else_if_condition, if_indent)?;
                    self.info_node(f, node_index, if_indent)?;
                }

                writeln!(f, "{:>indent$}Else: {else_column} = else", "")?;
                self.info_node(f, node_index, if_indent)
            }

            Node::Loop { loop_column, condition } => {
                writeln!(f, "{:>indent$}Loop: {loop_column} = loop", "")?;
                let loop_indent = indent + INDENT_INCREMENT;
                self.info_expression(f, *condition, loop_indent)?;
                self.info_node(f, node_index, loop_indent)
            }
            Node::DoLoop { do_column, loop_column, condition } => {
                writeln!(f, "{:>indent$}Do: {do_column} = do", "")?;
                writeln!(f, "{:>indent$}Loop: {loop_column} = loop", "")?;
                let loop_indent = indent + INDENT_INCREMENT;
                self.info_expression(f, *condition, loop_indent)?;
                self.info_node(f, node_index, loop_indent)
            }
            Node::Break { break_column } => {
                writeln!(f, "{:>indent$}Break: {break_column} = break", "")
            }
            Node::Continue { continue_column } => {
                writeln!(f, "{:>indent$}Continue: {continue_column} = continue", "")
            }
        };
    }

    fn info_expression(
        &self,
        f: &mut core::fmt::Formatter<'_>,
        expression_index: ExpressionIndex,
        indent: usize,
    ) -> core::fmt::Result {
        let expression_indent = indent + INDENT_INCREMENT;
        let expression = &self.expressions[expression_index as usize];

        #[rustfmt::skip]
        return match expression {
            Expression::False { column } => writeln!(f, "{:>indent$}False: {column} = false", ""),
            Expression::True { column } => writeln!(f, "{:>indent$}True: {column} = true", ""),
            Expression::Integer { base, literal, column } => {
                let literal_str = unsafe { core::str::from_utf8_unchecked(literal.0) };
                writeln!(f, "{:>indent$}Integer: {column} = {prefix}{literal_str}", "", prefix = base.prefix())
            }
            Expression::Ascii { character, column } => {
                writeln!(f, "{:>indent$}Ascii: {column} = {character}", "", character = *character as utf8)
            }
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
                open_square_bracket_column,
                array,
                close_square_bracket_column
            } => {
                writeln!(f, "{:>indent$}Array", "")?;
                writeln!(f, "{:>expression_indent$}OpenBracket: {open_square_bracket_column} = [", "")?;

                let items = &self.array_items[*array as usize];
                let commas = &self.array_commas_columns[*array as usize];

                let mut item_index = 0;

                let items_indent = expression_indent + INDENT_INCREMENT;
                while item_index < commas.len() {
                    let item_expression = items[item_index];
                    let comma_column = commas[item_index].get();
                    item_index += 1;

                    self.info_expression(f, item_expression, items_indent)?;
                    writeln!(f, "{:>items_indent$}Comma: {comma_column} = ,", "")?;
                }

                if items.len() > commas.len() {
                    let item_expression = items[item_index];
                    self.info_expression(f, item_expression, items_indent)?;
                }

                writeln!(f, "{:>expression_indent$}CloseBracket: {close_square_bracket_column} = [", "")
            },

            Expression::Prefix { operator, operator_column, right_operand } => {
                writeln!(f, "{:>indent$}PrefixExpression", "")?;
                writeln!(f, "{:>expression_indent$}PrefixOperator: {operator_column} = {operator}", "")?;
                self.info_expression(f, *right_operand, expression_indent)
            }
            Expression::Binary { left_operand, operator, operator_column, right_operand } => {
                writeln!(f, "{:>indent$}BinaryExpression", "")?;
                self.info_expression(f, *left_operand, expression_indent)?;
                writeln!(f, "{:>expression_indent$}BinaryOperator: {operator_column} = {operator}", "")?;
                self.info_expression(f, *right_operand, expression_indent)
            },

            Expression::Parenthesis {
                open_round_bracket_column,
                inner_expression,
                close_round_bracket_column
            } => {
                writeln!(f, "{:indent$}ParenthesisExpression", "")?;
                writeln!(f, "{:>expression_indent$}OpenRoundBracket: {open_round_bracket_column} = (", "")?;
                self.info_expression(f, *inner_expression, expression_indent)?;
                writeln!(f, "{:>expression_indent$}CloseRoundBracket: {close_round_bracket_column} = )", "")
            },
            Expression::EmptyParenthesis {
                open_round_bracket_column,
                close_round_bracket_column
            } => {
                writeln!(f, "{:indent$}ParenthesisExpression", "")?;
                writeln!(f, "{:>expression_indent$}OpenRoundBracket: {open_round_bracket_column} = (", "")?;
                writeln!(f, "{:>expression_indent$}CloseRoundBracket: {close_round_bracket_column} = )", "")
            },

            Expression::Index {
                indexed_expression,
                open_square_bracket_column,
                index_expression,
                close_square_bracket_column
            } => {
                writeln!(f, "{:indent$}IndexExpression", "")?;
                self.info_expression(f, *indexed_expression, expression_indent)?;
                writeln!(f, "{:>expression_indent$}OpenSquareBracket: {open_square_bracket_column} = [", "")?;
                self.info_expression(f, *index_expression, expression_indent)?;
                writeln!(f, "{:>expression_indent$}CloseSquareBracket: {close_square_bracket_column} = ]", "")
            },
        };
    }

    fn info_variable_definition(
        &self,
        f: &mut core::fmt::Formatter<'_>,
        variable_definition_index: VariableDefinitionIndex,
        indent: usize,
    ) -> core::fmt::Result {
        let VariableDefinition { name, name_column, type_annotation, initial_value } =
            &self.variable_definitions[variable_definition_index as usize];
        writeln!(f, "{:>indent$}Name: {name_column} = {name}", "")?;

        if let Some(TypeAnnotation {
            colon_column,
            type_name,
            type_name_column,
            array_dimensions,
        }) = type_annotation
        {
            writeln!(f, "{:>indent$}Colon: {colon_column} = :", "")?;
            writeln!(f, "{:>indent$}TypeName: {type_name_column} = {type_name}", "")?;

            let dimension_indent = indent + INDENT_INCREMENT;
            for ArrayDimension {
                open_square_bracket_column,
                dimension_expression,
                close_square_bracket_column,
            } in array_dimensions
            {
                writeln!(f, "{:>indent$}OpenSquareBracket: {open_square_bracket_column} = [", "")?;
                self.info_expression(f, *dimension_expression, dimension_indent)?;
                writeln!(
                    f,
                    "{:>indent$}CloseSquareBracket: {close_square_bracket_column} = ]",
                    ""
                )?;
            }
        }

        if let Some(InitialValue { equals_column, expression }) = initial_value {
            writeln!(f, "{:>indent$}Equals: {equals_column} = =", "")?;
            self.info_expression(f, *expression, indent)?;
        }

        return Ok(());
    }
}

impl Display for SyntaxTree<'_, '_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let mut node_index = 0;
        while node_index < self.nodes.len() as index32 {
            self.info_node(f, &mut node_index, 0)?;
        }

        return Ok(());
    }
}

#[derive(Debug)]
pub struct Parser<'src, 'tokens: 'src> {
    src: &'src SrcFile,
    errors: Vec<Error<ErrorKind>>,

    token_index: index32,
    tokens: &'tokens [Token<'src>],

    loop_depth: u32,
    syntax_tree: SyntaxTree<'src, 'tokens>,
}

/* NOTE(stefano):
only parsing until the first error until a fault tolerant parser is developed,
this is because the first truly relevant error is the first one, which in turn causes a ripple
effect that propagates to the rest of the parsing, causing subsequent errors to be wrong
*/
impl<'src, 'tokens: 'src> Parser<'src, 'tokens> {
    pub fn parse(
        src: &'src SrcFile,
        tokens: &'tokens [Token<'src>],
    ) -> Result<SyntaxTree<'src, 'tokens>, Vec<Error<ErrorKind>>> {
        let syntax_tree = SyntaxTree {
            nodes: Vec::new(),

            expressions: Vec::new(),
            array_items: Vec::new(),
            array_commas_columns: Vec::new(),

            variable_definitions: Vec::new(),

            ifs: Vec::new(),
            else_ifs: Vec::new(),
        };

        if tokens.is_empty() {
            return Ok(syntax_tree);
        }

        let mut this = Self {
            src,
            errors: Vec::new(),
            token_index: 0,
            tokens,
            loop_depth: 0,
            syntax_tree,
        };

        while let Some(peeked) = this.peek_next_token() {
            this.token_index = peeked.index;

            let node = match this.any(peeked.token) {
                Ok(ParsedNode::Node(node)) => node,
                Ok(ParsedNode::SemiColon) => continue,
                Ok(ParsedNode::Scope) => continue,
                Ok(ParsedNode::IfStatement) => continue,
                Ok(ParsedNode::LoopStatement) => continue,
                Err(err) => {
                    this.errors.push(err);

                    // consuming all remaining tokens until the end of the file
                    this.token_index = this.tokens.len() as index32;
                    break;
                }
            };

            this.syntax_tree.nodes.push(node);
        }

        return if this.errors.is_empty() { Ok(this.syntax_tree) } else { Err(this.errors) };
    }

    fn any(&mut self, token: &'tokens Token<'src>) -> Result<ParsedNode, Error<ErrorKind>> {
        return match &token.kind {
            TokenKind::True
            | TokenKind::False
            | TokenKind::Integer(_, _)
            | TokenKind::Ascii(_)
            | TokenKind::Str(_)
            | TokenKind::RawStr(_)
            | TokenKind::Identifier(_)
            | TokenKind::Bracket(Bracket::OpenRound | Bracket::OpenSquare)
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
                let expression = self.expression(token)?;
                let end_of_expression_token = self.peek_previous_token();

                let after_expression_token = self.next_expected_token(Expected::Semicolon)?;
                match after_expression_token.kind {
                    TokenKind::SemiColon => Ok(ParsedNode::Node(Node::Expression(expression))),
                    TokenKind::Op(
                        operator @ (Op::Equals
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
                        | Op::OrEquals),
                    ) => {
                        let start_of_new_value_token =
                            self.next_expected_token(Expected::Expression)?;
                        let new_value = self.expression(start_of_new_value_token)?;
                        self.semicolon()?;

                        Ok(ParsedNode::Node(Node::Assignment {
                            target: expression,
                            operator: operator.into(),
                            operator_column: after_expression_token.col,
                            new_value,
                        }))
                    }
                    TokenKind::Op(
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
                        | Op::LessOrEquals,
                    ) => self.invalid_token(
                        after_expression_token,
                        "unexpected operator".into(),
                        "should have been part of the left operand".into(),
                    ),
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
                    | TokenKind::Continue => Err(Error {
                        kind: ErrorKind::MissingSemicolon,
                        col: end_of_expression_token.col,
                        pointers_count: end_of_expression_token.kind.display_len(),
                    }),
                    TokenKind::Unexpected(_)
                    | TokenKind::Comment(_)
                    | TokenKind::BlockComment(_) => self.should_have_been_skipped(token),
                }
            }

            TokenKind::SemiColon => Ok(ParsedNode::SemiColon),

            TokenKind::Print => {
                let start_of_argument_token = self.next_expected_token(Expected::Expression)?;
                let argument = self.expression(start_of_argument_token)?;
                self.semicolon()?;
                Ok(ParsedNode::Node(Node::Print { print_column: token.col, argument }))
            }
            TokenKind::PrintLn => {
                let start_of_argument_token =
                    self.next_expected_token(Expected::ExpressionOrSemicolon)?;
                if let TokenKind::SemiColon = start_of_argument_token.kind {
                    return Ok(ParsedNode::Node(Node::PrintlnNoArg { println_column: token.col }));
                }

                let argument = self.expression(start_of_argument_token)?;
                self.semicolon()?;
                Ok(ParsedNode::Node(Node::Println { println_column: token.col, argument }))
            }
            TokenKind::Eprint => {
                let start_of_argument_token = self.next_expected_token(Expected::Expression)?;
                let argument = self.expression(start_of_argument_token)?;
                self.semicolon()?;
                Ok(ParsedNode::Node(Node::Eprint { eprint_column: token.col, argument }))
            }
            TokenKind::EprintLn => {
                let start_of_argument_token =
                    self.next_expected_token(Expected::ExpressionOrSemicolon)?;
                if let TokenKind::SemiColon = start_of_argument_token.kind {
                    return Ok(ParsedNode::Node(Node::EprintlnNoArg {
                        eprintln_column: token.col,
                    }));
                }

                let argument = self.expression(start_of_argument_token)?;
                self.semicolon()?;
                Ok(ParsedNode::Node(Node::Eprintln { eprintln_column: token.col, argument }))
            }

            TokenKind::Mutability(Mutability::Let) => {
                let variable_definition = self.variable_definition(token)?;
                self.syntax_tree.variable_definitions.push(variable_definition);
                Ok(ParsedNode::Node(Node::LetVariableDefinition {
                    let_column: token.col,
                    variable_definition: self.syntax_tree.variable_definitions.len() as index32 - 1,
                }))
            }
            TokenKind::Mutability(Mutability::Var) => {
                let variable_definition = self.variable_definition(token)?;
                self.syntax_tree.variable_definitions.push(variable_definition);
                Ok(ParsedNode::Node(Node::VarVariableDefinition {
                    var_column: token.col,
                    variable_definition: self.syntax_tree.variable_definitions.len() as index32 - 1,
                }))
            }

            TokenKind::Bracket(Bracket::OpenCurly) => {
                let placeholder_scope = Node::Scope {
                    open_curly_bracket_column: token.col,
                    raw_nodes_in_scope_count: 0,
                    close_curly_bracket_column: 0,
                };
                self.syntax_tree.nodes.push(placeholder_scope);
                let placeholder_scope_node_index = self.syntax_tree.nodes.len() as index32 - 1;

                while let Some(peeked) = self.peek_next_token() {
                    self.token_index = peeked.index;

                    if let TokenKind::Bracket(Bracket::CloseCurly) = peeked.token.kind {
                        let last_scope_node_index = self.syntax_tree.nodes.len() as index32 - 1;
                        let Node::Scope {
                            raw_nodes_in_scope_count,
                            close_curly_bracket_column,
                            ..
                        } = &mut self.syntax_tree.nodes[placeholder_scope_node_index as usize]
                        else {
                            self.unbalanced_bracket(token);
                        };

                        *raw_nodes_in_scope_count =
                            last_scope_node_index - placeholder_scope_node_index;
                        *close_curly_bracket_column = peeked.token.col;
                        break;
                    }

                    let node = match self.any(peeked.token)? {
                        ParsedNode::Node(node) => node,
                        ParsedNode::SemiColon => continue,
                        ParsedNode::Scope => continue,
                        ParsedNode::IfStatement => continue,
                        ParsedNode::LoopStatement => continue,
                    };

                    self.syntax_tree.nodes.push(node);
                }

                Ok(ParsedNode::Scope)
            }
            TokenKind::If => match self.if_statement(token.col) {
                Ok(()) => Ok(ParsedNode::IfStatement),
                Err(err) => Err(err),
            },

            TokenKind::Do => {
                let do_column = Some(token.col);
                let loop_token = self.next_expected_token(Expected::LoopStatement)?;
                let TokenKind::Loop = loop_token.kind else {
                    return Err(Error {
                        kind: ErrorKind::DoMustBeFollowedByLoop,
                        col: token.col,
                        pointers_count: token.kind.display_len(),
                    });
                };

                self.loop_depth += 1;
                let loop_result = self.loop_statement(do_column, loop_token.col);
                self.loop_depth -= 1;

                match loop_result {
                    Ok(()) => Ok(ParsedNode::LoopStatement),
                    Err(err) => Err(err),
                }
            }
            TokenKind::Loop => {
                let do_column = None;
                let loop_token = token;

                self.loop_depth += 1;
                let loop_result = self.loop_statement(do_column, loop_token.col);
                self.loop_depth -= 1;

                match loop_result {
                    Ok(()) => Ok(ParsedNode::LoopStatement),
                    Err(err) => Err(err),
                }
            }
            TokenKind::Break => {
                self.semicolon()?;

                if self.loop_depth == 0 {
                    return Err(Error {
                        kind: ErrorKind::BreakOutsideOfLoop,
                        col: token.col,
                        pointers_count: token.kind.display_len(),
                    });
                }

                let Some(break_column) = NonZero::new(token.col) else {
                    unreachable!("valid `break` should have non-zero column");
                };

                Ok(ParsedNode::Node(Node::Break { break_column }))
            }
            TokenKind::Continue => {
                self.semicolon()?;

                if self.loop_depth == 0 {
                    return Err(Error {
                        kind: ErrorKind::ContinueOutsideOfLoop,
                        col: token.col,
                        pointers_count: token.kind.display_len(),
                    });
                }

                let Some(continue_column) = NonZero::new(token.col) else {
                    unreachable!("valid `continue` should have non-zero column");
                };

                Ok(ParsedNode::Node(Node::Continue { continue_column }))
            }

            TokenKind::Else => Err(Error {
                kind: ErrorKind::StrayElse,
                col: token.col,
                pointers_count: token.kind.display_len(),
            }),
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
            TokenKind::Bracket(
                Bracket::CloseRound | Bracket::CloseSquare | Bracket::CloseCurly,
            ) => self.unbalanced_bracket(token),
            TokenKind::Unexpected(_) | TokenKind::Comment(_) | TokenKind::BlockComment(_) => {
                self.should_have_been_skipped(token)
            }
        };
    }

    fn semicolon(&mut self) -> Result<(), Error<ErrorKind>> {
        let peeked = self.peek_next_expected_token(Expected::Semicolon)?;
        let TokenKind::SemiColon = peeked.token.kind else {
            let previous_token = self.peek_previous_token();
            return Err(Error {
                kind: ErrorKind::MissingSemicolon,
                col: previous_token.col,
                pointers_count: previous_token.kind.display_len(),
            });
        };

        self.token_index = peeked.index;
        return Ok(());
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
        let DisplayPosition { line, column, display_column } = self.src.display_position(token.col);
        let line_span = &self.src.lines[line as usize - 1];
        let line_text = &self.src.code[line_span.start as usize..line_span.end as usize];

        let error = ErrorDisplay {
            error_message,
            file: &self.src.path,
            line,
            column,
            absolute_column: token.col,
            line_text,
            pointers_count: token.kind.display_len(),
            pointers_offset: display_column,
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
    fn unexpected(&self, token: &'tokens Token<'src>) -> ! {
        self.invalid_token(
            token,
            "unexpected".into(),
            "should have been caught during tokenization".into(),
        );
    }

    #[track_caller]
    fn unbalanced_bracket(&self, token: &'tokens Token<'src>) -> ! {
        self.invalid_token(
            token,
            "unbalanced bracket".into(),
            "should have been caught during tokenization".into(),
        );
    }
}

#[derive(Debug, Clone, Copy)]
struct Peeked<'src, 'tokens: 'src> {
    token: &'tokens Token<'src>,
    index: index32,
}

impl<'src, 'tokens: 'src> Parser<'src, 'tokens> {
    fn peek_next_token(&self) -> Option<Peeked<'src, 'tokens>> {
        for next_token_index in self.token_index..self.tokens.len() as index32 {
            let next_token = &self.tokens[next_token_index as usize];

            match &next_token.kind {
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
                | TokenKind::Continue => {
                    return Some(Peeked { token: next_token, index: next_token_index + 1 })
                }
                TokenKind::Comment(_) | TokenKind::BlockComment(_) => {}
                TokenKind::Unexpected(_) => self.unexpected(next_token),
            }
        }

        return None;
    }

    fn peek_next_expected_token(
        &self,
        expected: Expected,
    ) -> Result<Peeked<'src, 'tokens>, Error<ErrorKind>> {
        let Some(peeked) = self.peek_next_token() else {
            /* IDEA(stefano):
            suggest multiple expected places when encountering block comments:

            ```kay
            # either
            let i = 3 ## comment ##
                     ^ missing semicolon

            # or
            let i = 3 ## comment ##
                                   ^ missing semicolon
            ```
            */

            let previous_token = self.peek_previous_token();
            return Err(Error {
                kind: ErrorKind::PrematureEndOfFile(expected),
                col: previous_token.col,
                pointers_count: previous_token.kind.display_len(),
            });
        };

        return Ok(peeked);
    }

    fn next_expected_token(
        &mut self,
        expected: Expected,
    ) -> Result<&'tokens Token<'src>, Error<ErrorKind>> {
        let peeked = self.peek_next_expected_token(expected)?;
        self.token_index = peeked.index;
        return Ok(peeked.token);
    }

    /// Warning: should always be called with at least a previus token
    fn peek_previous_token(&self) -> &'tokens Token<'src> {
        for previous_token_index in (0..self.token_index).rev() {
            let previous_token = &self.tokens[previous_token_index as usize];

            match &previous_token.kind {
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
                | TokenKind::Continue => return previous_token,
                TokenKind::Comment(_) | TokenKind::BlockComment(_) => {}
                TokenKind::Unexpected(_) => self.unexpected(previous_token),
            }
        }

        unreachable!("should never be called with no previous token");
    }
}

#[derive(Debug, Clone, Copy)]
struct Operator<'src, 'tokens: 'src> {
    token: &'tokens Token<'src>,
    operator: Op,
}

// TODO(stefano): make less recursive by only recursing based on operator precedence
impl<'src, 'tokens: 'src> Parser<'src, 'tokens> {
    fn operator(&mut self, accepted_operators: &[Op]) -> Option<Operator<'src, 'tokens>> {
        let peeked = self.peek_next_token()?;
        let TokenKind::Op(operator) = peeked.token.kind else {
            return None;
        };

        for accepted_operator in accepted_operators {
            if *accepted_operator == operator {
                self.token_index = peeked.index;
                return Some(Operator { token: peeked.token, operator });
            }
        }

        return None;
    }

    fn primary_expression(
        &mut self,
        token: &'tokens Token<'src>,
    ) -> Result<ExpressionIndex, Error<ErrorKind>> {
        let mut expression = match &token.kind {
            TokenKind::False => Expression::False { column: token.col },
            TokenKind::True => Expression::True { column: token.col },
            TokenKind::Integer(base, literal) => {
                Expression::Integer { base: *base, literal, column: token.col }
            }
            TokenKind::Ascii(character) => {
                Expression::Ascii { character: *character, column: token.col }
            }
            TokenKind::Str(literal) => Expression::Str { literal, column: token.col },
            TokenKind::RawStr(literal) => Expression::RawStr { literal, column: token.col },
            TokenKind::Identifier(identifier) => {
                Expression::Identifier { identifier, column: token.col }
            }
            TokenKind::Bracket(Bracket::OpenRound) => 'bracket: {
                let open_round_bracket_token = token;

                let start_of_inner_expression_token =
                    self.next_expected_token(Expected::Operand)?;
                if let TokenKind::Bracket(Bracket::CloseRound) =
                    start_of_inner_expression_token.kind
                {
                    break 'bracket Expression::EmptyParenthesis {
                        open_round_bracket_column: open_round_bracket_token.col,
                        close_round_bracket_column: start_of_inner_expression_token.col,
                    };
                }

                let inner_expression = self.expression(start_of_inner_expression_token)?;

                let close_round_bracket_token =
                    self.next_expected_token(Expected::CloseRoundBracket)?;
                let TokenKind::Bracket(Bracket::CloseRound) = close_round_bracket_token.kind else {
                    return Err(Error {
                        kind: ErrorKind::ExpectedBracket(Bracket::CloseRound),
                        col: close_round_bracket_token.col,
                        pointers_count: close_round_bracket_token.kind.display_len(),
                    });
                };

                Expression::Parenthesis {
                    open_round_bracket_column: open_round_bracket_token.col,
                    inner_expression,
                    close_round_bracket_column: close_round_bracket_token.col,
                }
            }
            TokenKind::Bracket(Bracket::OpenSquare) => {
                let open_square_bracket_token = token;

                let mut items = Vec::<ExpressionIndex>::new();
                let mut commas_columns = Vec::<NonZero<offset32>>::new();

                let close_square_bracket_column = 'items: loop {
                    let start_of_item_token =
                        self.next_expected_token(Expected::ArrayItemOrCloseSquareBracket)?;
                    if let TokenKind::Bracket(Bracket::CloseSquare) = start_of_item_token.kind {
                        break 'items start_of_item_token.col;
                    }

                    let item = self.expression(start_of_item_token)?;
                    items.push(item);

                    let comma_or_close_square_bracket_token =
                        self.next_expected_token(Expected::CommaOrCloseSquareBracket)?;
                    match comma_or_close_square_bracket_token.kind {
                        TokenKind::Comma => {
                            let Some(comma_column) =
                                NonZero::new(comma_or_close_square_bracket_token.col)
                            else {
                                unreachable!("valid `,` should have non-zero column");
                            };
                            commas_columns.push(comma_column);
                        }
                        TokenKind::Bracket(Bracket::CloseSquare) => {
                            break 'items comma_or_close_square_bracket_token.col
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
                        | TokenKind::Continue => {
                            return Err(Error {
                                kind: ErrorKind::ExpectedComma,
                                col: comma_or_close_square_bracket_token.col,
                                pointers_count: comma_or_close_square_bracket_token
                                    .kind
                                    .display_len(),
                            });
                        }
                        TokenKind::Unexpected(_)
                        | TokenKind::Comment(_)
                        | TokenKind::BlockComment(_) => self.should_have_been_skipped(token),
                    }
                };

                self.syntax_tree.array_items.push(items);
                self.syntax_tree.array_commas_columns.push(commas_columns);

                Expression::Array {
                    open_square_bracket_column: open_square_bracket_token.col,
                    array: self.syntax_tree.array_items.len() as index32 - 1,
                    close_square_bracket_column,
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
                | Op::Not),
            ) => {
                let start_of_prefix_expression = self.next_expected_token(Expected::Expression)?;
                let right_operand = self.primary_expression(start_of_prefix_expression)?;

                Expression::Prefix {
                    operator: (*operator).into(),
                    operator_column: token.col,
                    right_operand,
                }
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
            | TokenKind::Do => {
                return Err(Error {
                    kind: ErrorKind::KeywordInExpression,
                    col: token.col,
                    pointers_count: token.kind.display_len(),
                })
            }
            TokenKind::Bracket(_)
            | TokenKind::Op(_)
            | TokenKind::Colon
            | TokenKind::SemiColon
            | TokenKind::Comma => {
                return Err(Error {
                    kind: ErrorKind::ExpectedOperand,
                    col: token.col,
                    pointers_count: token.kind.display_len(),
                })
            }
            TokenKind::Unexpected(_) | TokenKind::Comment(_) | TokenKind::BlockComment(_) => {
                self.should_have_been_skipped(token)
            }
        };

        loop {
            let Some(Peeked {
                token:
                    &Token {
                        kind: TokenKind::Bracket(Bracket::OpenSquare),
                        col: open_square_bracket_column,
                    },
                index: open_square_bracket_token_index,
            }) = self.peek_next_token()
            else {
                break;
            };
            self.token_index = open_square_bracket_token_index;

            let start_of_index_expression_token = self.next_expected_token(Expected::Expression)?;
            let index_expression = self.expression(start_of_index_expression_token)?;
            let end_of_index_expression_token = self.peek_previous_token();

            let after_expression_token = self.next_expected_token(Expected::CloseSquareBracket)?;
            let TokenKind::Bracket(Bracket::CloseSquare) = after_expression_token.kind else {
                return Err(Error {
                    kind: ErrorKind::MissingCloseSquareBracketInIndex,
                    col: end_of_index_expression_token.col,
                    pointers_count: end_of_index_expression_token.kind.display_len(),
                });
            };

            expression = Expression::Index {
                indexed_expression: self.syntax_tree.new_expression(expression),
                open_square_bracket_column,
                index_expression,
                close_square_bracket_column: after_expression_token.col,
            };
        }

        return Ok(self.syntax_tree.new_expression(expression));
    }

    fn exponentiative_expression(
        &mut self,
        token: &'tokens Token<'src>,
    ) -> Result<ExpressionIndex, Error<ErrorKind>> {
        static OPS: [Op; 3] = [Op::Pow, Op::WrappingPow, Op::SaturatingPow];

        let mut left_operand = self.primary_expression(token)?;
        while let Some(Operator { token: operator_token, operator }) = self.operator(&OPS) {
            let start_of_right_operand_token = self.next_expected_token(Expected::Operand)?;
            let right_operand = self.primary_expression(start_of_right_operand_token)?;

            left_operand = self.syntax_tree.new_expression(Expression::Binary {
                left_operand,
                operator: operator.into(),
                operator_column: operator_token.col,
                right_operand,
            });
        }

        return Ok(left_operand);
    }

    fn multiplicative_expression(
        &mut self,
        token: &'tokens Token<'src>,
    ) -> Result<ExpressionIndex, Error<ErrorKind>> {
        static OPS: [Op; 7] = [
            Op::Times,
            Op::WrappingTimes,
            Op::SaturatingTimes,
            Op::Divide,
            Op::WrappingDivide,
            Op::SaturatingDivide,
            Op::Remainder,
        ];

        let mut left_operand = self.exponentiative_expression(token)?;
        while let Some(Operator { token: operator_token, operator }) = self.operator(&OPS) {
            let start_of_right_operand_token = self.next_expected_token(Expected::Operand)?;
            let right_operand = self.exponentiative_expression(start_of_right_operand_token)?;

            left_operand = self.syntax_tree.new_expression(Expression::Binary {
                left_operand,
                operator: operator.into(),
                operator_column: operator_token.col,
                right_operand,
            });
        }

        return Ok(left_operand);
    }

    fn additive_expression(
        &mut self,
        token: &'tokens Token<'src>,
    ) -> Result<ExpressionIndex, Error<ErrorKind>> {
        static OPS: [Op; 6] = [
            Op::Plus,
            Op::WrappingPlus,
            Op::SaturatingPlus,
            Op::Minus,
            Op::WrappingMinus,
            Op::SaturatingMinus,
        ];

        let mut left_operand = self.multiplicative_expression(token)?;
        while let Some(Operator { token: operator_token, operator }) = self.operator(&OPS) {
            let start_of_right_operand_token = self.next_expected_token(Expected::Operand)?;
            let right_operand = self.multiplicative_expression(start_of_right_operand_token)?;

            left_operand = self.syntax_tree.new_expression(Expression::Binary {
                left_operand,
                operator: operator.into(),
                operator_column: operator_token.col,
                right_operand,
            });
        }

        return Ok(left_operand);
    }

    fn shift_expression(
        &mut self,
        token: &'tokens Token<'src>,
    ) -> Result<ExpressionIndex, Error<ErrorKind>> {
        static OPS: [Op; 6] = [
            Op::LeftShift,
            Op::WrappingLeftShift,
            Op::SaturatingLeftShift,
            Op::RightShift,
            Op::LeftRotate,
            Op::RightRotate,
        ];

        let mut left_operand = self.additive_expression(token)?;
        while let Some(Operator { token: operator_token, operator }) = self.operator(&OPS) {
            let start_of_right_operand_token = self.next_expected_token(Expected::Operand)?;
            let right_operand = self.additive_expression(start_of_right_operand_token)?;

            left_operand = self.syntax_tree.new_expression(Expression::Binary {
                left_operand,
                operator: operator.into(),
                operator_column: operator_token.col,
                right_operand,
            });
        }

        return Ok(left_operand);
    }

    fn bitand_expression(
        &mut self,
        token: &'tokens Token<'src>,
    ) -> Result<ExpressionIndex, Error<ErrorKind>> {
        static OPS: [Op; 1] = [Op::BitAnd];

        let mut left_operand = self.shift_expression(token)?;
        while let Some(Operator { token: operator_token, operator }) = self.operator(&OPS) {
            let start_of_right_operand_token = self.next_expected_token(Expected::Operand)?;
            let right_operand = self.shift_expression(start_of_right_operand_token)?;

            left_operand = self.syntax_tree.new_expression(Expression::Binary {
                left_operand,
                operator: operator.into(),
                operator_column: operator_token.col,
                right_operand,
            });
        }

        return Ok(left_operand);
    }

    fn bitxor_expression(
        &mut self,
        token: &'tokens Token<'src>,
    ) -> Result<ExpressionIndex, Error<ErrorKind>> {
        static OPS: [Op; 1] = [Op::BitXor];

        let mut left_operand = self.bitand_expression(token)?;
        while let Some(Operator { token: operator_token, operator }) = self.operator(&OPS) {
            let start_of_right_operand_token = self.next_expected_token(Expected::Operand)?;
            let right_operand = self.bitand_expression(start_of_right_operand_token)?;

            left_operand = self.syntax_tree.new_expression(Expression::Binary {
                left_operand,
                operator: operator.into(),
                operator_column: operator_token.col,
                right_operand,
            });
        }

        return Ok(left_operand);
    }

    fn bitor_expression(
        &mut self,
        token: &'tokens Token<'src>,
    ) -> Result<ExpressionIndex, Error<ErrorKind>> {
        static OPS: [Op; 1] = [Op::BitOr];

        let mut left_operand = self.bitxor_expression(token)?;
        while let Some(Operator { token: operator_token, operator }) = self.operator(&OPS) {
            let start_of_right_operand_token = self.next_expected_token(Expected::Operand)?;
            let right_operand = self.bitxor_expression(start_of_right_operand_token)?;

            left_operand = self.syntax_tree.new_expression(Expression::Binary {
                left_operand,
                operator: operator.into(),
                operator_column: operator_token.col,
                right_operand,
            });
        }

        return Ok(left_operand);
    }

    fn comparison_expression(
        &mut self,
        token: &'tokens Token<'src>,
    ) -> Result<ExpressionIndex, Error<ErrorKind>> {
        static OPS: [Op; 7] = [
            Op::Compare,
            Op::EqualsEquals,
            Op::NotEquals,
            Op::Greater,
            Op::GreaterOrEquals,
            Op::Less,
            Op::LessOrEquals,
        ];

        let mut left_operand = self.bitor_expression(token)?;
        while let Some(Operator { token: operator_token, operator }) = self.operator(&OPS) {
            let start_of_right_operand_token = self.next_expected_token(Expected::Operand)?;
            let right_operand = self.bitor_expression(start_of_right_operand_token)?;

            left_operand = self.syntax_tree.new_expression(Expression::Binary {
                left_operand,
                operator: operator.into(),
                operator_column: operator_token.col,
                right_operand,
            });
        }

        return Ok(left_operand);
    }

    fn and_expression(
        &mut self,
        token: &'tokens Token<'src>,
    ) -> Result<ExpressionIndex, Error<ErrorKind>> {
        static OPS: [Op; 1] = [Op::And];

        let mut left_operand = self.comparison_expression(token)?;
        while let Some(Operator { token: operator_token, operator }) = self.operator(&OPS) {
            let start_of_right_operand_token = self.next_expected_token(Expected::Operand)?;
            let right_operand = self.comparison_expression(start_of_right_operand_token)?;

            left_operand = self.syntax_tree.new_expression(Expression::Binary {
                left_operand,
                operator: operator.into(),
                operator_column: operator_token.col,
                right_operand,
            });
        }

        return Ok(left_operand);
    }

    fn or_expression(
        &mut self,
        token: &'tokens Token<'src>,
    ) -> Result<ExpressionIndex, Error<ErrorKind>> {
        static OPS: [Op; 1] = [Op::Or];

        let mut left_operand = self.and_expression(token)?;
        while let Some(Operator { token: operator_token, operator }) = self.operator(&OPS) {
            let start_of_right_operand_token = self.next_expected_token(Expected::Operand)?;
            let right_operand = self.and_expression(start_of_right_operand_token)?;

            left_operand = self.syntax_tree.new_expression(Expression::Binary {
                left_operand,
                operator: operator.into(),
                operator_column: operator_token.col,
                right_operand,
            });
        }

        return Ok(left_operand);
    }

    fn expression(
        &mut self,
        token: &'tokens Token<'src>,
    ) -> Result<ExpressionIndex, Error<ErrorKind>> {
        return self.or_expression(token);
    }
}

impl<'src, 'tokens: 'src> Parser<'src, 'tokens> {
    fn variable_definition(
        &mut self,
        mutability_token: &'tokens Token<'src>,
    ) -> Result<VariableDefinition<'src, 'tokens>, Error<ErrorKind>> {
        let variable_name_token = self.next_expected_token(Expected::VariableName)?;
        let variable_name = match &variable_name_token.kind {
            TokenKind::Identifier(name) => name,
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
            | TokenKind::RawStr(_) => {
                return Err(Error {
                    kind: ErrorKind::ExpectedVariableName,
                    col: mutability_token.col,
                    pointers_count: mutability_token.kind.display_len(),
                })
            }
            TokenKind::Mutability(_)
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
                    col: mutability_token.col,
                    pointers_count: mutability_token.kind.display_len(),
                })
            }
            TokenKind::Unexpected(_) | TokenKind::Comment(_) | TokenKind::BlockComment(_) => {
                self.should_have_been_skipped(variable_name_token)
            }
        };

        let type_annotation = 'type_annotation: {
            let after_variable_name_token_index = self.token_index;
            let after_variable_name_token =
                self.next_expected_token(Expected::ColonOrEqualsOrSemicolon)?;
            let TokenKind::Colon = after_variable_name_token.kind else {
                self.token_index = after_variable_name_token_index;
                break 'type_annotation None;
            };

            let type_name_token = self.next_expected_token(Expected::TypeName)?;
            let type_name = match &type_name_token.kind {
                TokenKind::Identifier(name) => name,
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
                | TokenKind::RawStr(_) => {
                    return Err(Error {
                        kind: ErrorKind::ExpectedTypeName,
                        col: after_variable_name_token.col,
                        pointers_count: after_variable_name_token.kind.display_len(),
                    })
                }
                TokenKind::Mutability(_)
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
                        kind: ErrorKind::KeywordInTypeName,
                        col: after_variable_name_token.col,
                        pointers_count: after_variable_name_token.kind.display_len(),
                    })
                }
                TokenKind::Unexpected(_) | TokenKind::Comment(_) | TokenKind::BlockComment(_) => {
                    self.should_have_been_skipped(after_variable_name_token)
                }
            };

            let mut array_dimensions = Vec::<ArrayDimension>::new();
            loop {
                let Some(Peeked {
                    token:
                        &Token {
                            kind: TokenKind::Bracket(Bracket::OpenSquare),
                            col: open_square_bracket_column,
                        },
                    index: open_square_bracket_token_index,
                }) = self.peek_next_token()
                else {
                    break 'type_annotation Some(TypeAnnotation {
                        colon_column: after_variable_name_token.col,
                        type_name,
                        type_name_column: type_name_token.col,
                        array_dimensions,
                    });
                };
                self.token_index = open_square_bracket_token_index;

                let dimension_expression_token = self.next_expected_token(Expected::Expression)?;
                let dimension_expression = self.expression(dimension_expression_token)?;

                let Some(Peeked {
                    token:
                        &Token {
                            kind: TokenKind::Bracket(Bracket::CloseSquare),
                            col: close_square_bracket_column,
                        },
                    index: close_square_bracket_token_index,
                }) = self.peek_next_token()
                else {
                    return Err(Error {
                        kind: ErrorKind::MissingCloseSquareBracketInArrayType,
                        col: dimension_expression_token.col,
                        pointers_count: dimension_expression_token.kind.display_len(),
                    });
                };
                self.token_index = close_square_bracket_token_index;

                array_dimensions.push(ArrayDimension {
                    open_square_bracket_column,
                    dimension_expression,
                    close_square_bracket_column,
                });
            }
        };

        let equals_or_semicolon_token = self.next_expected_token(Expected::EqualsOrSemicolon)?;
        return match equals_or_semicolon_token.kind {
            TokenKind::SemiColon => Ok(VariableDefinition {
                name: variable_name,
                name_column: variable_name_token.col,
                type_annotation,
                initial_value: None,
            }),
            TokenKind::Op(Op::Equals) => {
                let start_of_initial_value_token =
                    self.next_expected_token(Expected::Expression)?;
                let expression = self.expression(start_of_initial_value_token)?;
                self.semicolon()?;

                let Some(equals_column) = NonZero::new(equals_or_semicolon_token.col) else {
                    unreachable!("valid `=` should have non-zero column");
                };

                Ok(VariableDefinition {
                    name: variable_name,
                    name_column: variable_name_token.col,
                    type_annotation,
                    initial_value: Some(InitialValue { equals_column, expression }),
                })
            }
            TokenKind::Bracket(_)
            | TokenKind::Colon
            | TokenKind::Comma
            | TokenKind::Op(_)
            | TokenKind::False
            | TokenKind::True
            | TokenKind::Integer(_, _)
            | TokenKind::Ascii(_)
            | TokenKind::Str(_)
            | TokenKind::RawStr(_)
            | TokenKind::Identifier(_)
            | TokenKind::Mutability(_)
            | TokenKind::Print
            | TokenKind::PrintLn
            | TokenKind::Eprint
            | TokenKind::EprintLn
            | TokenKind::Do
            | TokenKind::If
            | TokenKind::Else
            | TokenKind::Loop
            | TokenKind::Break
            | TokenKind::Continue => match type_annotation {
                None => Err(Error {
                    kind: ErrorKind::ExpectedEqualsOrSemicolonAfterVariableName,
                    col: variable_name_token.col,
                    pointers_count: variable_name_token.kind.display_len(),
                }),
                Some(_) => Err(Error {
                    kind: ErrorKind::ExpectedEqualsOrSemicolonAfterTypeAnnotation,
                    col: equals_or_semicolon_token.col,
                    pointers_count: equals_or_semicolon_token.kind.display_len(),
                }),
            },
            TokenKind::Unexpected(_) | TokenKind::Comment(_) | TokenKind::BlockComment(_) => {
                self.should_have_been_skipped(equals_or_semicolon_token)
            }
        };
    }
}

// control flow statements
impl<'src, 'tokens: 'src> Parser<'src, 'tokens> {
    fn if_statement(&mut self, if_column: offset32) -> Result<(), Error<ErrorKind>> {
        let start_of_condition_token = self.next_expected_token(Expected::Expression)?;
        let condition = self.expression(start_of_condition_token)?;
        let end_of_condition_token = self.peek_previous_token();

        let after_if_condition_token = self.next_expected_token(Expected::OpenCurlyBracket)?;
        let TokenKind::Bracket(Bracket::OpenCurly) = after_if_condition_token.kind else {
            return Err(Error {
                kind: ErrorKind::IfMustBeFollowedByBlock,
                col: end_of_condition_token.col,
                pointers_count: end_of_condition_token.kind.display_len(),
            });
        };

        self.syntax_tree.ifs.push(If { if_column, condition });
        self.syntax_tree.else_ifs.push(Vec::new());
        let if_index = self.syntax_tree.ifs.len() as index32 - 1;

        self.syntax_tree.nodes.push(Node::If { if_index });
        let placeholder_if_index = self.syntax_tree.nodes.len() - 1;

        let ParsedNode::Scope = self.any(after_if_condition_token)? else {
            unreachable!();
        };

        while let Some(Peeked {
            token: else_token @ &Token { kind: TokenKind::Else, col: else_token_column },
            index: else_token_index,
        }) = self.peek_next_token()
        {
            self.token_index = else_token_index;

            let after_else_token = self.next_expected_token(Expected::OpenCurlyBracketOrIf)?;
            match after_else_token.kind {
                TokenKind::Bracket(Bracket::OpenCurly) => {
                    let ParsedNode::Scope = self.any(after_else_token)? else {
                        unreachable!();
                    };

                    let Some(else_column) = NonZero::new(else_token_column) else {
                        unreachable!("valid `else` should have non-zero column");
                    };

                    self.syntax_tree.nodes[placeholder_if_index] = Node::IfElse { if_index, else_column };
                    break;
                }
                TokenKind::If => {
                    let start_of_else_if_condition_token = self.next_expected_token(Expected::Expression)?;
                    let else_if_condition = self.expression(start_of_else_if_condition_token)?;
                    let end_of_else_if_condition_token = self.peek_previous_token();

                    let after_else_if_condition_token = self.next_expected_token(Expected::OpenCurlyBracket)?;
                    let TokenKind::Bracket(Bracket::OpenCurly) = after_else_if_condition_token.kind else {
                        return Err(Error {
                            kind: ErrorKind::IfMustBeFollowedByBlock,
                            col: end_of_else_if_condition_token.col,
                            pointers_count: end_of_else_if_condition_token.kind.display_len(),
                        });
                    };

                    let Some(else_column) = NonZero::new(else_token_column) else {
                        unreachable!("valid `else` should have non-zero column");
                    };

                    let else_if = ElseIf {
                        else_column,
                        iff: If { if_column: after_else_token.col, condition: else_if_condition }
                    };
                    self.syntax_tree.else_ifs[if_index as usize].push(else_if);

                    let ParsedNode::Scope = self.any(after_else_if_condition_token)? else {
                        unreachable!();
                    };
                }
                TokenKind::Do
                | TokenKind::Colon
                /* NOTE(stefano):
                warn on semicolons after if statements followed by else branches
                ```
                if true { println "1"; }; # < here
                else if true { println "2"; }
                else if true { println 3; }
                else { println "ciao"; }
                ```
                */
                | TokenKind::SemiColon
                | TokenKind::Comma
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
                | TokenKind::Else
                | TokenKind::Loop
                | TokenKind::Break
                | TokenKind::Continue => {
                    return Err(Error {
                        kind: ErrorKind::ElseMustBeFollowedByBlockOrIf,
                        col: else_token.col,
                        pointers_count: else_token.kind.display_len(),
                    });
                }
                TokenKind::Unexpected(_) | TokenKind::Comment(_) | TokenKind::BlockComment(_) => {
                    self.should_have_been_skipped(after_else_token)
                }
            }
        }

        return Ok(());
    }

    fn loop_statement(
        &mut self,
        do_column: Option<offset32>,
        loop_column: offset32,
    ) -> Result<(), Error<ErrorKind>> {
        let start_of_condition_token = self.next_expected_token(Expected::Expression)?;
        let condition = self.expression(start_of_condition_token)?;
        let end_of_condition_token = self.peek_previous_token();

        let after_condition_token = self.next_expected_token(Expected::OpenCurlyBracket)?;
        let TokenKind::Bracket(Bracket::OpenCurly) = after_condition_token.kind else {
            return Err(Error {
                kind: ErrorKind::LoopMustBeFollowedByBlock,
                col: end_of_condition_token.col,
                pointers_count: end_of_condition_token.kind.display_len(),
            });
        };

        let loop_node = match do_column {
            Some(column) => Node::DoLoop { do_column: column, loop_column, condition },
            None => Node::Loop { loop_column, condition },
        };
        self.syntax_tree.nodes.push(loop_node);

        let ParsedNode::Scope = self.any(after_condition_token)? else {
            unreachable!();
        };
        return Ok(());
    }
}

#[derive(Debug, Clone)]
pub enum Expected {
    Statement,
    OperatorOrSemicolon,
    Operand,
    CloseRoundBracket,
    CloseSquareBracket,
    Expression,
    ExpressionOrSemicolon,
    Comma,
    CommaOrCloseSquareBracket,
    ArrayItemOrCloseSquareBracket,
    Semicolon,
    VariableName,
    TypeName,
    EqualsOrSemicolon,
    ColonOrEqualsOrSemicolon,
    OpenCurlyBracket,
    OpenCurlyBracketOrIf,
    LoopStatement,
}

impl Display for Expected {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return match self {
            Self::Statement => write!(f, "statement"),
            Self::OperatorOrSemicolon => write!(f, "operator or ';'"),
            Self::Operand => write!(f, "operand"),
            Self::CloseRoundBracket => write!(f, "')'"),
            Self::CloseSquareBracket => write!(f, "']'"),
            Self::Expression => write!(f, "expression"),
            Self::ExpressionOrSemicolon => write!(f, "expression or ';'"),
            Self::Comma => write!(f, "','"),
            Self::CommaOrCloseSquareBracket => write!(f, "',' or ']'"),
            Self::ArrayItemOrCloseSquareBracket => write!(f, "array item or ']'"),
            Self::Semicolon => write!(f, "';'"),
            Self::VariableName => write!(f, "variable name"),
            Self::TypeName => write!(f, "type name"),
            Self::EqualsOrSemicolon => write!(f, "'=' or ';'"),
            Self::ColonOrEqualsOrSemicolon => write!(f, "':', '=' or ';'"),
            Self::OpenCurlyBracket => write!(f, "'{{'"),
            Self::OpenCurlyBracketOrIf => write!(f, "'{{' or 'if'"),
            Self::LoopStatement => write!(f, "loop statement"),
        };
    }
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    PrematureEndOfFile(Expected),
    MissingSemicolon,
    StrayElse,
    StrayColon,
    StrayComma,
    StrayOperator(Op),

    // expressions
    KeywordInExpression,
    ExpectedOperand,
    ExpectedBracket(Bracket),
    ExpectedComma,
    MissingCloseSquareBracketInIndex,

    // variables
    ExpectedVariableName,
    KeywordInVariableName,
    ExpectedTypeName,
    KeywordInTypeName,
    MissingCloseSquareBracketInArrayType,
    ExpectedEqualsOrSemicolonAfterVariableName,
    ExpectedEqualsOrSemicolonAfterTypeAnnotation,

    // if statements
    IfMustBeFollowedByBlock,
    ElseMustBeFollowedByBlockOrIf,

    // loop statements
    DoMustBeFollowedByLoop,
    BreakOutsideOfLoop,
    ContinueOutsideOfLoop,
    LoopMustBeFollowedByBlock,
}

impl IntoErrorInfo for ErrorKind {
    fn info(&self) -> ErrorInfo {
        #[rustfmt::skip]
        let (error_message, error_cause_message) = match self {
            Self::PrematureEndOfFile(expected) => (
                "premature end of file".into(),
                format!("expected {expected} after here").into(),
            ),
            Self::MissingSemicolon => (
                "invalid statement".into(),
                "expected ';' after here".into(),
            ),
            Self::StrayElse => (
                "stray 'else'".into(),
                "stray 'else'".into(),
            ),
            Self::StrayColon => (
                "stray ':'".into(),
                "stray ':'".into(),
            ),
            Self::StrayComma => (
                "stray ','".into(),
                "stray ','".into(),
            ),
            Self::StrayOperator(operator) => (
                format!("stray operator '{operator}'").into(),
                format!("stray operator '{operator}'").into(),
            ),

            Self::KeywordInExpression => (
                "invalid expression".into(),
                "cannot be a keyword".into(),
            ),
            Self::ExpectedOperand => (
                "invalid expression".into(),
                "expected operand before this token".into(),
            ),
            Self::ExpectedBracket(bracket) => (
                "invalid expression".into(),
                format!("expected '{bracket}' bracket before this token").into(),
            ),
            Self::ExpectedComma => (
                "invalid array".into(),
                "expected ',' before this token".into(),
            ),
            Self::MissingCloseSquareBracketInIndex => (
                "invalid array index".into(),
                "must be followed by a ']'".into(),
            ),

            Self::ExpectedVariableName => (
                "invalid variable name".into(),
                "expected variable name after here".into(),
            ),
            Self::KeywordInVariableName => (
                "invalid variable name".into(),
                "cannot be a keyword".into(),
            ),
            Self::ExpectedTypeName => (
                "invalid type annotation".into(),
                "expected type name after here".into(),
            ),
            Self::KeywordInTypeName => (
                "invalid type name".into(),
                "cannot be a keyword".into(),
            ),
            Self::MissingCloseSquareBracketInArrayType => (
                "invalid type".into(),
                "must be followed by a ']'".into(),
            ),
            Self::ExpectedEqualsOrSemicolonAfterVariableName => (
                "invalid variable definition".into(),
                "expected '=' or ';' after variable name".into(),
            ),
            Self::ExpectedEqualsOrSemicolonAfterTypeAnnotation => (
                "invalid variable definition".into(),
                "expected '=' or ';' after type annotation".into(),
            ),

            Self::IfMustBeFollowedByBlock => (
                "invalid if statement".into(),
                "must be followed by '{'".into(),
            ),
            Self::ElseMustBeFollowedByBlockOrIf => (
                "invalid else statement".into(),
                "must be followed by '{' or 'if'".into(),
            ),

            Self::DoMustBeFollowedByLoop => (
                "invalid do loop".into(),
                "must be followed by a loop statement".into(),
            ),
            Self::BreakOutsideOfLoop => (
                "invalid break statement".into(),
                "cannot be used outside of loops".into(),
            ),
            Self::ContinueOutsideOfLoop => (
                "invalid continue statement".into(),
                "cannot be used outside of loops".into(),
            ),
            Self::LoopMustBeFollowedByBlock => (
                "invalid loop statement".into(),
                "must be followed by '{'".into(),
            ),
        };

        return ErrorInfo { error_message, error_cause_message };
    }
}
