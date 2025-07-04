use super::{
    src_file::{DisplayPosition, SrcCode},
    tokenizer::{Op, TextIndex, Token, TokenIndex, TokenKind, Tokens},
    Error, ErrorDisplay, ErrorInfo, IntoErrorInfo,
};
use core::{fmt::Display, marker::PhantomData, num::NonZero};
extern crate alloc;
use alloc::borrow::Cow;
use back_to_front::offset32;

#[expect(dead_code, reason = "it's in reality created by trasmuting an `Op`")]
#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
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
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl PrefixOperator {
    #[expect(dead_code, reason = "kept for consistency")]
    #[inline(always)]
    pub(super) fn display_len(self) -> offset32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

#[expect(dead_code, reason = "it's in reality created by trasmuting an `Op`")]
#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
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
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl BinaryOperator {
    #[expect(dead_code, reason = "kept for consistency")]
    #[inline(always)]
    pub(super) fn display_len(self) -> offset32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

#[expect(dead_code, reason = "it's in reality created by trasmuting an `Op`")]
#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
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
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op: Op = (*self).into();
        return write!(f, "{op}");
    }
}

impl AssignmentOperator {
    #[expect(dead_code, reason = "kept for consistency")]
    #[inline(always)]
    pub(super) fn display_len(self) -> offset32 {
        let op: Op = self.into();
        return op.display_len();
    }
}

pub(crate) type ArrayItemsIndex = offset32;
pub(crate) type ExpressionIndex = offset32;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) enum Expression {
    False {
        column: offset32,
    },
    True {
        column: offset32,
    },
    DecimalInteger {
        literal: TextIndex,
        column: offset32,
    },
    BinaryInteger {
        literal: TextIndex,
        column: offset32,
    },
    OctalInteger {
        literal: TextIndex,
        column: offset32,
    },
    HexadecimalInteger {
        literal: TextIndex,
        column: offset32,
    },
    Ascii {
        literal: TextIndex,
        column: offset32,
    },
    Str {
        literal: TextIndex,
        column: offset32,
    },
    RawStr {
        literal: TextIndex,
        column: offset32,
    },
    Identifier {
        identifier: TextIndex,
        column: offset32,
    },
    IdentifierStr {
        identifier: TextIndex,
        column: offset32,
    },
    Array {
        open_square_bracket_column: offset32,
        items_start: ArrayItemsIndex,
        items_len: offset32,
        close_square_bracket_column: offset32,
    },
    ArrayTrailingItem {
        open_square_bracket_column: offset32,
        items_start: ArrayItemsIndex,
        items_len: offset32,
        last_item: ExpressionIndex,
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

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum ArrayItemSeparator {
    Comma,
    Semicolon,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) struct ArrayItem {
    item: ExpressionIndex,
    separator_column: offset32,
    separator: ArrayItemSeparator,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) struct ArrayDimension {
    open_square_bracket_column: offset32,
    dimension_expression: ExpressionIndex,
    close_square_bracket_column: offset32,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) struct TypeAnnotation {
    colon_column: NonZero<offset32>,
    type_name: TextIndex,
    type_name_column: offset32,
    array_dimensions_start: offset32,
    array_dimensions_len: offset32,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) struct InitialValue {
    equals_column: NonZero<offset32>,
    expression: ExpressionIndex,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) struct VariableDefinition {
    name: TextIndex,
    name_column: offset32,
    type_annotation: Option<TypeAnnotation>,
    initial_value: Option<InitialValue>,
}

pub(crate) type VariableDefinitionIndex = offset32;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) enum Node {
    Semicolon {
        column: offset32,
    },
    Expression {
        expression: ExpressionIndex,
        semicolon_column: offset32,
    },

    Print {
        print_column: offset32,
        argument: ExpressionIndex,
        semicolon_column: offset32,
    },
    Println {
        println_column: offset32,
        argument: ExpressionIndex,
        semicolon_column: offset32,
    },
    PrintlnNoArg {
        println_column: offset32,
        semicolon_column: offset32,
    },
    Eprint {
        eprint_column: offset32,
        argument: ExpressionIndex,
        semicolon_column: offset32,
    },
    Eprintln {
        eprintln_column: offset32,
        argument: ExpressionIndex,
        semicolon_column: offset32,
    },
    EprintlnNoArg {
        eprintln_column: offset32,
        semicolon_column: offset32,
    },

    LetVariableDefinition {
        let_column: offset32,
        variable_definition: VariableDefinitionIndex,
        semicolon_column: offset32,
    },
    VarVariableDefinition {
        var_column: offset32,
        variable_definition: VariableDefinitionIndex,
        semicolon_column: offset32,
    },
    // IDEA(stefano): maybe move into expressions enum
    Assignment {
        target: ExpressionIndex,
        operator: AssignmentOperator,
        operator_column: offset32,
        new_value: ExpressionIndex,
        semicolon_column: offset32,
    },

    Scope {
        open_curly_bracket_column: offset32,
        raw_nodes_in_scope_count: offset32,
        close_curly_bracket_column: offset32,
    },

    If {
        if_column: offset32,
        condition: ExpressionIndex,
        else_ifs_count: offset32,
    },
    IfElse {
        if_column: offset32,
        condition: ExpressionIndex,
        else_ifs_count: offset32,
        else_column: offset32,
    },
    ElseIf {
        else_column: offset32,
        if_column: offset32,
        condition: ExpressionIndex,
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
        break_column: offset32,
        semicolon_column: offset32,
    },
    Continue {
        continue_column: offset32,
        semicolon_column: offset32,
    },
}

pub(crate) type NodeIndex = offset32;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum ParsedNode {
    Node(Node),
    Scope,
    IfStatement,
    LoopStatement,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SyntaxTree<'tokens, 'code: 'tokens> {
    pub(crate) nodes: Vec<Node>,

    pub(crate) expressions: Vec<Expression>,
    pub(crate) array_items: Vec<ArrayItem>,

    pub(crate) variable_definitions: Vec<VariableDefinition>,
    pub(crate) array_dimensions: Vec<ArrayDimension>,

    _tokens: PhantomData<&'tokens Tokens<'code>>,
}

impl SyntaxTree<'_, '_> {
    #[inline]
    fn new_expression(&mut self, expression: Expression) -> ExpressionIndex {
        self.expressions.push(expression);
        #[expect(clippy::cast_possible_truncation)]
        return self.expressions.len() as ExpressionIndex - 1;
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SyntaxTreeDisplay<'syntax_tree, 'tokens: 'syntax_tree, 'code: 'tokens> {
    pub(crate) syntax_tree: &'syntax_tree SyntaxTree<'tokens, 'code>,
    pub(crate) tokens: &'tokens Tokens<'code>,
}

impl<'tokens, 'code: 'tokens> SyntaxTree<'tokens, 'code> {
    #[must_use]
    #[inline(always)]
    pub const fn display(
        &self,
        tokens: &'tokens Tokens<'code>,
    ) -> SyntaxTreeDisplay<'_, 'tokens, 'code> {
        return SyntaxTreeDisplay { syntax_tree: self, tokens };
    }
}

impl SyntaxTreeDisplay<'_, '_, '_> {
    const INDENT_INCREMENT: usize = 2;

    #[inline(always)]
    fn info_semicolon(
        f: &mut core::fmt::Formatter<'_>,
        indent: usize,
        column: offset32,
    ) -> core::fmt::Result {
        return writeln!(f, "{:>indent$}Semicolon: {column} = ;", "");
    }

    fn info_if(
        &self,
        f: &mut core::fmt::Formatter<'_>,
        node_index: &mut NodeIndex,
        indent: usize,
        if_column: offset32,
        condition: ExpressionIndex,
    ) -> core::fmt::Result {
        writeln!(f, "{:>indent$}If: {if_column} = if", "")?;
        let if_indent = indent + Self::INDENT_INCREMENT;
        self.info_expression(f, condition, if_indent)?;
        return self.info_node(f, node_index, if_indent);
    }

    fn info_node(
        &self,
        f: &mut core::fmt::Formatter<'_>,
        node_index: &mut NodeIndex,
        indent: usize,
    ) -> core::fmt::Result {
        let node = &self.syntax_tree.nodes[*node_index as usize];
        *node_index += 1;

        #[rustfmt::skip]
        return match node {
            Node::Semicolon { column } => Self::info_semicolon(f, indent, *column),
            Node::Expression { expression, semicolon_column } => {
                self.info_expression(f, *expression, indent)?;
                Self::info_semicolon(f, indent, *semicolon_column)
            }

            Node::Print { print_column, argument, semicolon_column } => {
                writeln!(f, "{:>indent$}Print: {print_column} = print", "")?;
                let argument_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *argument, argument_indent)?;
                Self::info_semicolon(f, argument_indent, *semicolon_column)
            }
            Node::Println { println_column, argument, semicolon_column } => {
                writeln!(f, "{:>indent$}Println: {println_column} = println", "")?;
                let argument_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *argument, argument_indent)?;
                Self::info_semicolon(f, argument_indent, *semicolon_column)
            }
            Node::PrintlnNoArg { println_column, semicolon_column } => {
                writeln!(f, "{:>indent$}Println: {println_column} = println", "")?;
                Self::info_semicolon(f, indent, *semicolon_column)
            }
            Node::Eprint { eprint_column, argument, semicolon_column } => {
                writeln!(f, "{:>indent$}Eprint: {eprint_column} = eprint", "")?;
                let argument_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *argument, argument_indent)?;
                Self::info_semicolon(f, argument_indent, *semicolon_column)
            }
            Node::Eprintln { eprintln_column, argument, semicolon_column } => {
                writeln!(f, "{:>indent$}Eprintln: {eprintln_column} = eprintln", "")?;
                let argument_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *argument, argument_indent)?;
                Self::info_semicolon(f, argument_indent, *semicolon_column)
            }
            Node::EprintlnNoArg { eprintln_column, semicolon_column } => {
                writeln!(f, "{:>indent$}Eprintln: {eprintln_column} = eprintln", "")?;
                Self::info_semicolon(f, indent, *semicolon_column)
            }

            Node::LetVariableDefinition { let_column, variable_definition, semicolon_column } => {
                writeln!(f, "{:>indent$}VariableDefinition: {let_column} = let", "")?;
                let definition_indent = indent + Self::INDENT_INCREMENT;
                self.info_variable_definition(f, *variable_definition, definition_indent)?;
                Self::info_semicolon(f, definition_indent, *semicolon_column)
            }
            Node::VarVariableDefinition { var_column, variable_definition, semicolon_column } => {
                writeln!(f, "{:>indent$}VariableDefinition: {var_column} = var", "")?;
                let definition_indent = indent + Self::INDENT_INCREMENT;
                self.info_variable_definition(f, *variable_definition, definition_indent)?;
                Self::info_semicolon(f, definition_indent, *semicolon_column)
            }
            Node::Assignment { target, operator, operator_column, new_value, semicolon_column } => {
                writeln!(f, "{:>indent$}Assignment", "")?;
                let assignment_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *target, assignment_indent)?;
                writeln!(f, "{:>assignment_indent$}AssignmentOp: {operator_column} = {operator}", "")?;
                self.info_expression(f, *new_value, assignment_indent)?;
                Self::info_semicolon(f, assignment_indent, *semicolon_column)
            }

            Node::Scope { open_curly_bracket_column, raw_nodes_in_scope_count, close_curly_bracket_column } => {
                writeln!(f, "{:>indent$}Scope", "")?;
                let scope_indent = indent + Self::INDENT_INCREMENT;
                writeln!(f, "{:>scope_indent$}OpenCurlyBracket: {open_curly_bracket_column} = {{", "")?;

                let after_end_scope_node_index = *node_index + raw_nodes_in_scope_count;
                while *node_index < after_end_scope_node_index {
                    self.info_node(f, node_index, scope_indent)?;
                }
                writeln!(f, "{:>scope_indent$}CloseCurlyBracket: {close_curly_bracket_column} = }}", "")
            }

            Node::If { if_column, condition, mut else_ifs_count } => {
                self.info_if(f, node_index, indent, *if_column, *condition)?;
                while else_ifs_count > 0 {
                    else_ifs_count -= 1;
                    self.info_node(f, node_index, indent)?;
                }
                Ok(())
            }
            Node::IfElse { if_column, condition, mut else_ifs_count, else_column } => {
                self.info_if(f, node_index, indent, *if_column, *condition)?;
                while else_ifs_count > 0 {
                    else_ifs_count -= 1;
                    self.info_node(f, node_index, indent)?;
                }
                let else_indent = indent + Self::INDENT_INCREMENT;
                writeln!(f, "{:>indent$}Else: {else_column} = else", "")?;
                self.info_node(f, node_index, else_indent)
            }
            Node::ElseIf { if_column, condition, else_column } => {
                writeln!(f, "{:>indent$}Else: {else_column} = else", "")?;
                self.info_if(f, node_index, indent, *if_column, *condition)
            }

            Node::Loop { loop_column, condition } => {
                writeln!(f, "{:>indent$}Loop: {loop_column} = loop", "")?;
                let loop_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *condition, loop_indent)?;
                self.info_node(f, node_index, loop_indent)
            }
            Node::DoLoop { do_column, loop_column, condition } => {
                writeln!(f, "{:>indent$}Do: {do_column} = do", "")?;
                writeln!(f, "{:>indent$}Loop: {loop_column} = loop", "")?;
                let loop_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *condition, loop_indent)?;
                self.info_node(f, node_index, loop_indent)
            }
            Node::Break { break_column, semicolon_column } => {
                writeln!(f, "{:>indent$}Break: {break_column} = break", "")?;
                Self::info_semicolon(f, indent, *semicolon_column)
            }
            Node::Continue { continue_column, semicolon_column } => {
                writeln!(f, "{:>indent$}Continue: {continue_column} = continue", "")?;
                Self::info_semicolon(f, indent, *semicolon_column)
            }
        };
    }

    fn info_expression(
        &self,
        f: &mut core::fmt::Formatter<'_>,
        expression_index: ExpressionIndex,
        indent: usize,
    ) -> core::fmt::Result {
        let expression_indent = indent + Self::INDENT_INCREMENT;
        let expression = &self.syntax_tree.expressions[expression_index as usize];

        #[rustfmt::skip]
        return match expression {
            Expression::False { column } => writeln!(f, "{:>indent$}False: {column} = false", ""),
            Expression::True { column } => writeln!(f, "{:>indent$}True: {column} = true", ""),
            Expression::DecimalInteger { literal, column } => {
                let literal_str = self.tokens.text[*literal as usize];
                writeln!(f, "{:>indent$}DecimalInteger: {column} = {literal_str}", "")
            }
            Expression::BinaryInteger { literal, column } => {
                let literal_str = self.tokens.text[*literal as usize];
                writeln!(f, "{:>indent$}BinaryInteger: {column} = {literal_str}", "")
            }
            Expression::OctalInteger { literal, column } => {
                let literal_str = self.tokens.text[*literal as usize];
                writeln!(f, "{:>indent$}OctalInteger: {column} = {literal_str}", "")
            }
            Expression::HexadecimalInteger { literal, column } => {
                let literal_str = self.tokens.text[*literal as usize];
                writeln!(f, "{:>indent$}HexadecimalInteger: {column} = {literal_str}", "")
            }
            Expression::Ascii { literal, column } => {
                let literal_str = &self.tokens.text[*literal as usize];
                writeln!(f, "{:>indent$}Ascii: {column} = {literal_str}", "")
            }
            Expression::Str { literal, column } => {
                let literal_str = &self.tokens.text[*literal as usize];
                writeln!(f, "{:>indent$}Str: {column} = {literal_str}", "")
            }
            Expression::RawStr { literal, column } => {
                let literal_str = &self.tokens.text[*literal as usize];
                writeln!(f, "{:>indent$}RawStr: {column} = {literal_str}", "")
            }
            Expression::Identifier { identifier, column } => {
                let identifier_str = self.tokens.text[*identifier as usize];
                writeln!(f, "{:>indent$}Identifier: {column} = {identifier_str}", "")
            },
            Expression::IdentifierStr { identifier, column } => {
                let identifier_str = self.tokens.text[*identifier as usize];
                writeln!(f, "{:>indent$}IdentifierStr: {column} = {identifier_str}", "")
            },
            Expression::Array {
                open_square_bracket_column,
                items_start,
                items_len,
                close_square_bracket_column
            } => {
                writeln!(f, "{:>indent$}Array", "")?;
                writeln!(f, "{:>expression_indent$}OpenSquareBracket: {open_square_bracket_column} = [", "")?;

                let items_indent = expression_indent + Self::INDENT_INCREMENT;
                let items_end = items_start + items_len;
                let items = &self.syntax_tree.array_items[*items_start as usize..items_end as usize];
                for ArrayItem { item, separator_column, separator } in items {
                self.info_expression(f, *item, items_indent)?;
                    match separator {
                        ArrayItemSeparator::Comma => writeln!(f, "{:>items_indent$}Comma: {separator_column} = ,", "")?,
                        ArrayItemSeparator::Semicolon => writeln!(f, "{:>items_indent$}Semicolon: {separator_column} = ;", "")?,
                    }
                }

                writeln!(f, "{:>expression_indent$}CloseSquareBracket: {close_square_bracket_column} = ]", "")
            },
            Expression::ArrayTrailingItem {
                open_square_bracket_column,
                items_start,
                items_len,
                last_item,
                close_square_bracket_column
            } => {
                writeln!(f, "{:>indent$}Array", "")?;
                writeln!(f, "{:>expression_indent$}OpenSquareBracket: {open_square_bracket_column} = [", "")?;

                let items_indent = expression_indent + Self::INDENT_INCREMENT;
                let items_end = items_start + items_len;
                let items = &self.syntax_tree.array_items[*items_start as usize..items_end as usize];
                for ArrayItem { item, separator_column, separator } in items {
                self.info_expression(f, *item, items_indent)?;
                    match separator {
                        ArrayItemSeparator::Comma => writeln!(f, "{:>items_indent$}Comma: {separator_column} = ,", "")?,
                        ArrayItemSeparator::Semicolon => writeln!(f, "{:>items_indent$}Semicolon: {separator_column} = ;", "")?,
                    }
                }
                self.info_expression(f, *last_item, items_indent)?;
                writeln!(f, "{:>expression_indent$}CloseSquareBracket: {close_square_bracket_column} = ]", "")
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
            &self.syntax_tree.variable_definitions[variable_definition_index as usize];
        let name_str = self.tokens.text[*name as usize];
        writeln!(f, "{:>indent$}Name: {name_column} = {name_str}", "")?;

        if let Some(TypeAnnotation {
            colon_column,
            type_name,
            type_name_column,
            array_dimensions_start,
            array_dimensions_len,
        }) = type_annotation
        {
            let annotation_indent = indent + Self::INDENT_INCREMENT;
            writeln!(f, "{:>indent$}TypeAnnotation", "")?;
            writeln!(f, "{:>annotation_indent$}Colon: {colon_column} = :", "")?;

            let type_name_str = self.tokens.text[*type_name as usize];
            writeln!(
                f,
                "{:>annotation_indent$}TypeName: {type_name_column} = {type_name_str}",
                ""
            )?;

            let dimension_indent = annotation_indent + Self::INDENT_INCREMENT;
            let array_dimensions_end = array_dimensions_start + array_dimensions_len;
            let array_dimensions = &self.syntax_tree.array_dimensions
                [*array_dimensions_start as usize..array_dimensions_end as usize];
            for ArrayDimension {
                open_square_bracket_column,
                dimension_expression,
                close_square_bracket_column,
            } in array_dimensions
            {
                writeln!(
                    f,
                    "{:>annotation_indent$}OpenSquareBracket: {open_square_bracket_column} = [",
                    ""
                )?;
                self.info_expression(f, *dimension_expression, dimension_indent)?;
                writeln!(
                    f,
                    "{:>annotation_indent$}CloseSquareBracket: {close_square_bracket_column} = ]",
                    ""
                )?;
            }
        }

        return if let Some(InitialValue { equals_column, expression }) = initial_value {
            writeln!(f, "{:>indent$}Equals: {equals_column} = =", "")?;
            self.info_expression(f, *expression, indent)
        } else {
            Ok(())
        };
    }
}

impl Display for SyntaxTreeDisplay<'_, '_, '_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let mut node_index = 0;
        #[expect(clippy::cast_possible_truncation)]
        while node_index < self.syntax_tree.nodes.len() as NodeIndex {
            self.info_node(f, &mut node_index, 0)?;
        }

        return Ok(());
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Parser<'tokens, 'src: 'tokens, 'code: 'src, 'path: 'code> {
    src: &'src SrcCode<'code, 'path>,
    errors: Vec<Error<ErrorKind>>,

    token_index: TokenIndex,
    tokens: &'tokens Tokens<'code>,

    loop_depth: u32,
    syntax_tree: SyntaxTree<'tokens, 'code>,
}

/* NOTE(stefano):
only parsing until the first error until a fault tolerant parser is developed,
this is because the first truly relevant error is the first one, which in turn causes a ripple
effect that propagates to the rest of the parsing, causing subsequent errors to be wrong
*/
impl<'tokens, 'src: 'tokens, 'code: 'src, 'path: 'code> Parser<'tokens, 'src, 'code, 'path> {
    #[expect(clippy::missing_errors_doc, reason = "syntax errors cannot be documented in docs")]
    pub fn parse(
        src: &'src SrcCode<'code, 'path>,
        tokens: &'tokens Tokens<'code>,
    ) -> Result<SyntaxTree<'tokens, 'code>, Vec<Error<ErrorKind>>> {
        let mut parser = Self {
            src,
            errors: Vec::new(),
            token_index: 0,
            tokens,
            loop_depth: 0,
            syntax_tree: SyntaxTree {
                nodes: Vec::new(),

                expressions: Vec::new(),
                array_items: Vec::new(),

                variable_definitions: Vec::new(),
                array_dimensions: Vec::new(),

                _tokens: PhantomData,
            },
        };

        while let Some(peeked) = parser.peek_next_token() {
            parser.token_index = peeked.index;
            match parser.any(peeked.token) {
                Ok(ParsedNode::Node(node)) => parser.syntax_tree.nodes.push(node),
                Ok(ParsedNode::Scope) => continue,
                Ok(ParsedNode::IfStatement) => continue,
                Ok(ParsedNode::LoopStatement) => continue,
                Err(err) => {
                    parser.errors.push(err);

                    // consuming all remaining tokens until the end of the file
                    #[expect(clippy::cast_possible_truncation)]
                    {
                        parser.token_index = parser.tokens.tokens.len() as TokenIndex;
                    }
                    break;
                }
            };
        }

        return if parser.errors.is_empty() { Ok(parser.syntax_tree) } else { Err(parser.errors) };
    }
}

impl<'tokens, 'src: 'tokens, 'code: 'src, 'path: 'code> Parser<'tokens, 'src, 'code, 'path> {
    fn any(&mut self, token: Token) -> Result<ParsedNode, Error<ErrorKind>> {
        return match token.kind {
            TokenKind::True
            | TokenKind::False
            | TokenKind::DecimalInteger(_)
            | TokenKind::BinaryInteger(_)
            | TokenKind::OctalInteger(_)
            | TokenKind::HexadecimalInteger(_)
            | TokenKind::Ascii(_)
            | TokenKind::Str(_)
            | TokenKind::RawStr(_)
            | TokenKind::Identifier(_)
            | TokenKind::IdentifierStr(_)
            | TokenKind::OpenRoundBracket
            | TokenKind::OpenSquareBracket
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
                    TokenKind::SemiColon => Ok(ParsedNode::Node(Node::Expression {
                        expression,
                        semicolon_column: after_expression_token.col,
                    })),
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
                        let semicolon_column = self.semicolon()?;
                        Ok(ParsedNode::Node(Node::Assignment {
                            target: expression,
                            operator: operator.into(),
                            operator_column: after_expression_token.col,
                            new_value,
                            semicolon_column,
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
                    TokenKind::OpenRoundBracket
                    | TokenKind::CloseRoundBracket
                    | TokenKind::OpenSquareBracket
                    | TokenKind::CloseSquareBracket
                    | TokenKind::OpenCurlyBracket
                    | TokenKind::CloseCurlyBracket
                    | TokenKind::Colon
                    | TokenKind::Comma
                    | TokenKind::False
                    | TokenKind::True
                    | TokenKind::DecimalInteger(_)
                    | TokenKind::BinaryInteger(_)
                    | TokenKind::OctalInteger(_)
                    | TokenKind::HexadecimalInteger(_)
                    | TokenKind::Ascii(_)
                    | TokenKind::Str(_)
                    | TokenKind::RawStr(_)
                    | TokenKind::Identifier(_)
                    | TokenKind::IdentifierStr(_)
                    | TokenKind::Print
                    | TokenKind::PrintLn
                    | TokenKind::Eprint
                    | TokenKind::EprintLn
                    | TokenKind::Let
                    | TokenKind::Var
                    | TokenKind::Do
                    | TokenKind::If
                    | TokenKind::Else
                    | TokenKind::Loop
                    | TokenKind::Break
                    | TokenKind::Continue => Err(Error {
                        kind: ErrorKind::MissingSemicolon,
                        col: end_of_expression_token.col,
                        pointers_count: end_of_expression_token.kind.display_len(self.tokens),
                    }),
                    TokenKind::Unexpected(_)
                    | TokenKind::Comment(_)
                    | TokenKind::BlockComment(_) => self.should_have_been_skipped(token),
                }
            }

            TokenKind::SemiColon => Ok(ParsedNode::Node(Node::Semicolon { column: token.col })),

            TokenKind::Print => {
                let start_of_argument_token = self.next_expected_token(Expected::Expression)?;
                let argument = self.expression(start_of_argument_token)?;
                let semicolon_column = self.semicolon()?;
                Ok(ParsedNode::Node(Node::Print {
                    print_column: token.col,
                    argument,
                    semicolon_column,
                }))
            }
            TokenKind::PrintLn => {
                let start_of_argument_token =
                    self.next_expected_token(Expected::ExpressionOrSemicolon)?;
                if let TokenKind::SemiColon = start_of_argument_token.kind {
                    return Ok(ParsedNode::Node(Node::PrintlnNoArg {
                        println_column: token.col,
                        semicolon_column: start_of_argument_token.col,
                    }));
                }

                let argument = self.expression(start_of_argument_token)?;
                let semicolon_column = self.semicolon()?;
                Ok(ParsedNode::Node(Node::Println {
                    println_column: token.col,
                    argument,
                    semicolon_column,
                }))
            }
            TokenKind::Eprint => {
                let start_of_argument_token = self.next_expected_token(Expected::Expression)?;
                let argument = self.expression(start_of_argument_token)?;
                let semicolon_column = self.semicolon()?;
                Ok(ParsedNode::Node(Node::Eprint {
                    eprint_column: token.col,
                    argument,
                    semicolon_column,
                }))
            }
            TokenKind::EprintLn => {
                let start_of_argument_token =
                    self.next_expected_token(Expected::ExpressionOrSemicolon)?;
                if let TokenKind::SemiColon = start_of_argument_token.kind {
                    return Ok(ParsedNode::Node(Node::EprintlnNoArg {
                        eprintln_column: token.col,
                        semicolon_column: start_of_argument_token.col,
                    }));
                }

                let argument = self.expression(start_of_argument_token)?;
                let semicolon_column = self.semicolon()?;
                Ok(ParsedNode::Node(Node::Eprintln {
                    eprintln_column: token.col,
                    argument,
                    semicolon_column,
                }))
            }

            TokenKind::Let => {
                let (variable_definition, semicolon_column) = self.variable_definition(token)?;
                self.syntax_tree.variable_definitions.push(variable_definition);
                Ok(ParsedNode::Node(Node::LetVariableDefinition {
                    let_column: token.col,
                    #[expect(clippy::cast_possible_truncation)]
                    variable_definition: self.syntax_tree.variable_definitions.len()
                        as VariableDefinitionIndex
                        - 1,
                    semicolon_column,
                }))
            }
            TokenKind::Var => {
                let (variable_definition, semicolon_column) = self.variable_definition(token)?;
                self.syntax_tree.variable_definitions.push(variable_definition);
                Ok(ParsedNode::Node(Node::VarVariableDefinition {
                    var_column: token.col,
                    #[expect(clippy::cast_possible_truncation)]
                    variable_definition: self.syntax_tree.variable_definitions.len()
                        as VariableDefinitionIndex
                        - 1,
                    semicolon_column,
                }))
            }

            TokenKind::OpenCurlyBracket => {
                let placeholder_scope = Node::Scope {
                    open_curly_bracket_column: token.col,
                    raw_nodes_in_scope_count: 0,
                    close_curly_bracket_column: 0,
                };
                self.syntax_tree.nodes.push(placeholder_scope);
                #[expect(clippy::cast_possible_truncation)]
                let placeholder_scope_node_index = self.syntax_tree.nodes.len() as NodeIndex - 1;

                while let Some(peeked) = self.peek_next_token() {
                    self.token_index = peeked.index;
                    if let TokenKind::CloseCurlyBracket = peeked.token.kind {
                        #[expect(clippy::cast_possible_truncation)]
                        let last_scope_node_index = self.syntax_tree.nodes.len() as NodeIndex - 1;
                        let Node::Scope {
                            raw_nodes_in_scope_count,
                            close_curly_bracket_column,
                            open_curly_bracket_column: _open_curly_bracket_column,
                        } = &mut self.syntax_tree.nodes[placeholder_scope_node_index as usize]
                        else {
                            self.unbalanced_bracket(token);
                        };

                        *raw_nodes_in_scope_count =
                            last_scope_node_index - placeholder_scope_node_index;
                        *close_curly_bracket_column = peeked.token.col;
                        break;
                    }

                    match self.any(peeked.token)? {
                        ParsedNode::Node(node) => self.syntax_tree.nodes.push(node),
                        ParsedNode::Scope => continue,
                        ParsedNode::IfStatement => continue,
                        ParsedNode::LoopStatement => continue,
                    };
                }

                Ok(ParsedNode::Scope)
            }
            TokenKind::If => match self.if_statement(token.col) {
                Ok(()) => Ok(ParsedNode::IfStatement),
                Err(err) => Err(err),
            },

            TokenKind::Do => {
                let do_column = token.col;
                let loop_token = self.next_expected_token(Expected::LoopStatement)?;
                let TokenKind::Loop = loop_token.kind else {
                    return Err(Error {
                        kind: ErrorKind::DoMustBeFollowedByLoop,
                        col: token.col,
                        pointers_count: token.kind.display_len(self.tokens),
                    });
                };

                self.loop_depth += 1;
                let loop_result = self.do_loop_statement(do_column, loop_token.col);
                self.loop_depth -= 1;

                match loop_result {
                    Ok(()) => Ok(ParsedNode::LoopStatement),
                    Err(err) => Err(err),
                }
            }
            TokenKind::Loop => {
                let loop_token = token;

                self.loop_depth += 1;
                let loop_result = self.loop_statement(loop_token.col);
                self.loop_depth -= 1;

                match loop_result {
                    Ok(()) => Ok(ParsedNode::LoopStatement),
                    Err(err) => Err(err),
                }
            }
            TokenKind::Break => {
                let semicolon_column = self.semicolon()?;

                if self.loop_depth == 0 {
                    return Err(Error {
                        kind: ErrorKind::BreakOutsideOfLoop,
                        col: token.col,
                        pointers_count: token.kind.display_len(self.tokens),
                    });
                }

                Ok(ParsedNode::Node(Node::Break { break_column: token.col, semicolon_column }))
            }
            TokenKind::Continue => {
                let semicolon_column = self.semicolon()?;

                if self.loop_depth == 0 {
                    return Err(Error {
                        kind: ErrorKind::ContinueOutsideOfLoop,
                        col: token.col,
                        pointers_count: token.kind.display_len(self.tokens),
                    });
                }

                Ok(ParsedNode::Node(Node::Continue {
                    continue_column: token.col,
                    semicolon_column,
                }))
            }

            TokenKind::Else => Err(Error {
                kind: ErrorKind::StrayElse,
                col: token.col,
                pointers_count: token.kind.display_len(self.tokens),
            }),
            TokenKind::Colon => Err(Error {
                kind: ErrorKind::StrayColon,
                col: token.col,
                pointers_count: token.kind.display_len(self.tokens),
            }),
            TokenKind::Comma => Err(Error {
                kind: ErrorKind::StrayComma,
                col: token.col,
                pointers_count: token.kind.display_len(self.tokens),
            }),
            TokenKind::Op(op) => Err(Error {
                kind: ErrorKind::StrayOperator(op),
                col: token.col,
                pointers_count: token.kind.display_len(self.tokens),
            }),
            TokenKind::CloseRoundBracket
            | TokenKind::CloseSquareBracket
            | TokenKind::CloseCurlyBracket => self.unbalanced_bracket(token),
            TokenKind::Unexpected(_) | TokenKind::Comment(_) | TokenKind::BlockComment(_) => {
                self.should_have_been_skipped(token)
            }
        };
    }

    fn semicolon(&mut self) -> Result<offset32, Error<ErrorKind>> {
        let peeked = self.peek_next_expected_token(Expected::Semicolon)?;
        let TokenKind::SemiColon = peeked.token.kind else {
            let previous_token = self.peek_previous_token();
            return Err(Error {
                kind: ErrorKind::MissingSemicolon,
                col: previous_token.col,
                pointers_count: previous_token.kind.display_len(self.tokens),
            });
        };

        self.token_index = peeked.index;
        return Ok(peeked.token.col);
    }
}

impl Parser<'_, '_, '_, '_> {
    #[expect(clippy::panic, reason = "it's basically a more descriptive panic implementation")]
    #[track_caller]
    fn invalid_token(
        &self,
        token: Token,
        error_message: Cow<'static, str>,
        error_cause_message: Cow<'static, str>,
    ) -> ! {
        let DisplayPosition { line, column, display_column } = self.src.display_position(token.col);
        let line_span = self.src.lines[line as usize - 1];
        let line_text = &self.src.code()[line_span.start as usize..line_span.end as usize];

        let error = ErrorDisplay {
            error_message,
            file: self.src.path(),
            line,
            column,
            absolute_column: token.col,
            line_text,
            pointers_count: token.kind.display_len(self.tokens),
            pointers_offset: display_column,
            error_cause_message,
        };
        panic!("{error}\n");
    }

    #[track_caller]
    fn should_have_been_skipped(&self, token: Token) -> ! {
        self.invalid_token(
            token,
            "unexpected".into(),
            "should have been skipped in the iteration of tokens".into(),
        );
    }

    #[track_caller]
    fn unexpected(&self, token: Token) -> ! {
        self.invalid_token(
            token,
            "unexpected".into(),
            "should have been caught during tokenization".into(),
        );
    }

    #[track_caller]
    fn unbalanced_bracket(&self, token: Token) -> ! {
        self.invalid_token(
            token,
            "unbalanced bracket".into(),
            "should have been caught during tokenization".into(),
        );
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
struct Peeked {
    token: Token,
    index: TokenIndex,
}

impl Parser<'_, '_, '_, '_> {
    fn peek_next_token(&self) -> Option<Peeked> {
        #[expect(clippy::cast_possible_truncation)]
        for next_token_index in self.token_index..self.tokens.tokens.len() as TokenIndex {
            let next_token = self.tokens.tokens[next_token_index as usize];
            match next_token.kind {
                TokenKind::OpenRoundBracket
                | TokenKind::CloseRoundBracket
                | TokenKind::OpenSquareBracket
                | TokenKind::CloseSquareBracket
                | TokenKind::OpenCurlyBracket
                | TokenKind::CloseCurlyBracket
                | TokenKind::Colon
                | TokenKind::SemiColon
                | TokenKind::Comma
                | TokenKind::Op(_)
                | TokenKind::False
                | TokenKind::True
                | TokenKind::DecimalInteger(_)
                | TokenKind::BinaryInteger(_)
                | TokenKind::OctalInteger(_)
                | TokenKind::HexadecimalInteger(_)
                | TokenKind::Ascii(_)
                | TokenKind::Str(_)
                | TokenKind::RawStr(_)
                | TokenKind::Identifier(_)
                | TokenKind::IdentifierStr(_)
                | TokenKind::Print
                | TokenKind::PrintLn
                | TokenKind::Eprint
                | TokenKind::EprintLn
                | TokenKind::Let
                | TokenKind::Var
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

    fn peek_next_expected_token(&self, expected: Expected) -> Result<Peeked, Error<ErrorKind>> {
        let Some(peeked) = self.peek_next_token() else {
            let previous_token = self.peek_previous_token();
            return Err(Error {
                kind: ErrorKind::PrematureEndOfFile(expected),
                col: previous_token.col,
                pointers_count: previous_token.kind.display_len(self.tokens),
            });
        };

        return Ok(peeked);
    }

    fn next_expected_token(&mut self, expected: Expected) -> Result<Token, Error<ErrorKind>> {
        let peeked = self.peek_next_expected_token(expected)?;
        self.token_index = peeked.index;
        return Ok(peeked.token);
    }

    /// Warning: should always be called with at least a previus token
    fn peek_previous_token(&self) -> Token {
        for previous_token_index in (0..self.token_index).rev() {
            let previous_token = self.tokens.tokens[previous_token_index as usize];
            match previous_token.kind {
                TokenKind::OpenRoundBracket
                | TokenKind::CloseRoundBracket
                | TokenKind::OpenSquareBracket
                | TokenKind::CloseSquareBracket
                | TokenKind::OpenCurlyBracket
                | TokenKind::CloseCurlyBracket
                | TokenKind::Colon
                | TokenKind::SemiColon
                | TokenKind::Comma
                | TokenKind::Op(_)
                | TokenKind::False
                | TokenKind::True
                | TokenKind::DecimalInteger(_)
                | TokenKind::BinaryInteger(_)
                | TokenKind::OctalInteger(_)
                | TokenKind::HexadecimalInteger(_)
                | TokenKind::Ascii(_)
                | TokenKind::Str(_)
                | TokenKind::RawStr(_)
                | TokenKind::Identifier(_)
                | TokenKind::IdentifierStr(_)
                | TokenKind::Print
                | TokenKind::PrintLn
                | TokenKind::Eprint
                | TokenKind::EprintLn
                | TokenKind::Let
                | TokenKind::Var
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

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
struct Operator {
    token: Token,
    operator: Op,
}

// TODO(stefano): make less recursive by only recursing based on operator precedence
impl Parser<'_, '_, '_, '_> {
    fn operator(&mut self, accepted_operators: &[Op]) -> Option<Operator> {
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

    fn primary_expression(&mut self, token: Token) -> Result<ExpressionIndex, Error<ErrorKind>> {
        let mut expression = match token.kind {
            TokenKind::False => Expression::False { column: token.col },
            TokenKind::True => Expression::True { column: token.col },
            TokenKind::DecimalInteger(literal) => {
                Expression::DecimalInteger { literal, column: token.col }
            }
            TokenKind::BinaryInteger(literal) => {
                Expression::BinaryInteger { literal, column: token.col }
            }
            TokenKind::OctalInteger(literal) => {
                Expression::OctalInteger { literal, column: token.col }
            }
            TokenKind::HexadecimalInteger(literal) => {
                Expression::HexadecimalInteger { literal, column: token.col }
            }
            TokenKind::Ascii(literal) => Expression::Ascii { literal, column: token.col },
            TokenKind::Str(literal) => Expression::Str { literal, column: token.col },
            TokenKind::RawStr(literal) => Expression::RawStr { literal, column: token.col },
            TokenKind::Identifier(identifier) => {
                Expression::Identifier { identifier, column: token.col }
            }
            TokenKind::IdentifierStr(identifier) => {
                Expression::IdentifierStr { identifier, column: token.col }
            }
            TokenKind::OpenRoundBracket => 'bracket: {
                let open_round_bracket_token = token;

                let start_of_inner_expression_token =
                    self.next_expected_token(Expected::Operand)?;
                if let TokenKind::CloseRoundBracket = start_of_inner_expression_token.kind {
                    break 'bracket Expression::EmptyParenthesis {
                        open_round_bracket_column: open_round_bracket_token.col,
                        close_round_bracket_column: start_of_inner_expression_token.col,
                    };
                }

                let inner_expression = self.expression(start_of_inner_expression_token)?;

                let close_round_bracket_token =
                    self.next_expected_token(Expected::CloseRoundBracket)?;
                let TokenKind::CloseRoundBracket = close_round_bracket_token.kind else {
                    return Err(Error {
                        kind: ErrorKind::ExpectedCloseRoundBracket,
                        col: close_round_bracket_token.col,
                        pointers_count: close_round_bracket_token.kind.display_len(self.tokens),
                    });
                };

                Expression::Parenthesis {
                    open_round_bracket_column: open_round_bracket_token.col,
                    inner_expression,
                    close_round_bracket_column: close_round_bracket_token.col,
                }
            }
            TokenKind::OpenSquareBracket => 'array: {
                let open_square_bracket_token = token;

                #[expect(clippy::cast_possible_truncation)]
                let items_start = self.syntax_tree.array_items.len() as ArrayItemsIndex;
                loop {
                    let start_of_item_token =
                        self.next_expected_token(Expected::ArrayItemOrCloseSquareBracket)?;
                    if let TokenKind::CloseSquareBracket = start_of_item_token.kind {
                        break 'array Expression::Array {
                            open_square_bracket_column: open_square_bracket_token.col,
                            items_start,
                            #[expect(clippy::cast_possible_truncation)]
                            items_len: self.syntax_tree.array_items.len() as ArrayItemsIndex
                                - items_start,
                            close_square_bracket_column: start_of_item_token.col,
                        };
                    }

                    let item = self.expression(start_of_item_token)?;

                    let comma_or_close_square_bracket_token =
                        self.next_expected_token(Expected::CommaOrSemicolonCloseSquareBracket)?;
                    match comma_or_close_square_bracket_token.kind {
                        TokenKind::Comma => {
                            self.syntax_tree.array_items.push(ArrayItem {
                                item,
                                separator_column: comma_or_close_square_bracket_token.col,
                                separator: ArrayItemSeparator::Comma,
                            });
                        }
                        TokenKind::SemiColon => {
                            self.syntax_tree.array_items.push(ArrayItem {
                                item,
                                separator_column: comma_or_close_square_bracket_token.col,
                                separator: ArrayItemSeparator::Semicolon,
                            });
                        }
                        TokenKind::CloseSquareBracket => {
                            break 'array Expression::ArrayTrailingItem {
                                open_square_bracket_column: open_square_bracket_token.col,
                                items_start,
                                #[expect(clippy::cast_possible_truncation)]
                                items_len: self.syntax_tree.array_items.len() as ArrayItemsIndex
                                    - items_start,
                                last_item: item,
                                close_square_bracket_column: comma_or_close_square_bracket_token
                                    .col,
                            };
                        }
                        TokenKind::Colon
                        | TokenKind::Op(_)
                        | TokenKind::OpenRoundBracket
                        | TokenKind::CloseRoundBracket
                        | TokenKind::OpenSquareBracket
                        | TokenKind::OpenCurlyBracket
                        | TokenKind::CloseCurlyBracket
                        | TokenKind::False
                        | TokenKind::True
                        | TokenKind::DecimalInteger(_)
                        | TokenKind::BinaryInteger(_)
                        | TokenKind::OctalInteger(_)
                        | TokenKind::HexadecimalInteger(_)
                        | TokenKind::Ascii(_)
                        | TokenKind::Str(_)
                        | TokenKind::RawStr(_)
                        | TokenKind::Identifier(_)
                        | TokenKind::IdentifierStr(_)
                        | TokenKind::Print
                        | TokenKind::PrintLn
                        | TokenKind::Eprint
                        | TokenKind::EprintLn
                        | TokenKind::Let
                        | TokenKind::Var
                        | TokenKind::Do
                        | TokenKind::If
                        | TokenKind::Else
                        | TokenKind::Loop
                        | TokenKind::Break
                        | TokenKind::Continue => {
                            return Err(Error {
                                kind: ErrorKind::ExpectedCommaOrCloseSquareBracket,
                                col: comma_or_close_square_bracket_token.col,
                                pointers_count: comma_or_close_square_bracket_token
                                    .kind
                                    .display_len(self.tokens),
                            });
                        }
                        TokenKind::Unexpected(_)
                        | TokenKind::Comment(_)
                        | TokenKind::BlockComment(_) => self.should_have_been_skipped(token),
                    }
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
                    operator: operator.into(),
                    operator_column: token.col,
                    right_operand,
                }
            }
            TokenKind::Let
            | TokenKind::Var
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
                    pointers_count: token.kind.display_len(self.tokens),
                })
            }
            TokenKind::CloseRoundBracket
            | TokenKind::CloseSquareBracket
            | TokenKind::OpenCurlyBracket
            | TokenKind::CloseCurlyBracket
            | TokenKind::Op(_)
            | TokenKind::Colon
            | TokenKind::SemiColon
            | TokenKind::Comma => {
                return Err(Error {
                    kind: ErrorKind::ExpectedOperand,
                    col: token.col,
                    pointers_count: token.kind.display_len(self.tokens),
                })
            }
            TokenKind::Unexpected(_) | TokenKind::Comment(_) | TokenKind::BlockComment(_) => {
                self.should_have_been_skipped(token)
            }
        };

        while let Some(Peeked {
            token: Token { kind: TokenKind::OpenSquareBracket, col: open_square_bracket_column },
            index: open_square_bracket_token_index,
        }) = self.peek_next_token()
        {
            self.token_index = open_square_bracket_token_index;

            let start_of_index_expression_token = self.next_expected_token(Expected::Expression)?;
            let index_expression = self.expression(start_of_index_expression_token)?;
            let end_of_index_expression_token = self.peek_previous_token();

            let after_expression_token = self.next_expected_token(Expected::CloseSquareBracket)?;
            let TokenKind::CloseSquareBracket = after_expression_token.kind else {
                return Err(Error {
                    kind: ErrorKind::MissingCloseSquareBracketInIndex,
                    col: end_of_index_expression_token.col,
                    pointers_count: end_of_index_expression_token.kind.display_len(self.tokens),
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
        token: Token,
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
        token: Token,
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

    fn additive_expression(&mut self, token: Token) -> Result<ExpressionIndex, Error<ErrorKind>> {
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

    fn shift_expression(&mut self, token: Token) -> Result<ExpressionIndex, Error<ErrorKind>> {
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

    fn bitand_expression(&mut self, token: Token) -> Result<ExpressionIndex, Error<ErrorKind>> {
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

    fn bitxor_expression(&mut self, token: Token) -> Result<ExpressionIndex, Error<ErrorKind>> {
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

    fn bitor_expression(&mut self, token: Token) -> Result<ExpressionIndex, Error<ErrorKind>> {
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

    fn comparison_expression(&mut self, token: Token) -> Result<ExpressionIndex, Error<ErrorKind>> {
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

    fn and_expression(&mut self, token: Token) -> Result<ExpressionIndex, Error<ErrorKind>> {
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

    fn or_expression(&mut self, token: Token) -> Result<ExpressionIndex, Error<ErrorKind>> {
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

    fn expression(&mut self, token: Token) -> Result<ExpressionIndex, Error<ErrorKind>> {
        return self.or_expression(token);
    }
}

impl Parser<'_, '_, '_, '_> {
    fn variable_definition(
        &mut self,
        mutability_token: Token,
    ) -> Result<(VariableDefinition, offset32), Error<ErrorKind>> {
        let variable_name_token = self.next_expected_token(Expected::VariableName)?;
        let variable_name = match variable_name_token.kind {
            TokenKind::Identifier(name) | TokenKind::IdentifierStr(name) => name,
            TokenKind::OpenRoundBracket
            | TokenKind::CloseRoundBracket
            | TokenKind::OpenSquareBracket
            | TokenKind::CloseSquareBracket
            | TokenKind::OpenCurlyBracket
            | TokenKind::CloseCurlyBracket
            | TokenKind::Colon
            | TokenKind::SemiColon
            | TokenKind::Comma
            | TokenKind::Op(_)
            | TokenKind::False
            | TokenKind::True
            | TokenKind::DecimalInteger(_)
            | TokenKind::BinaryInteger(_)
            | TokenKind::OctalInteger(_)
            | TokenKind::HexadecimalInteger(_)
            | TokenKind::Ascii(_)
            | TokenKind::Str(_)
            | TokenKind::RawStr(_) => {
                return Err(Error {
                    kind: ErrorKind::ExpectedVariableName,
                    col: mutability_token.col,
                    pointers_count: mutability_token.kind.display_len(self.tokens),
                })
            }
            TokenKind::Let
            | TokenKind::Var
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
                    pointers_count: mutability_token.kind.display_len(self.tokens),
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
            let type_name = match type_name_token.kind {
                TokenKind::Identifier(name) | TokenKind::IdentifierStr(name) => name,
                TokenKind::OpenRoundBracket
                | TokenKind::CloseRoundBracket
                | TokenKind::OpenSquareBracket
                | TokenKind::CloseSquareBracket
                | TokenKind::OpenCurlyBracket
                | TokenKind::CloseCurlyBracket
                | TokenKind::Colon
                | TokenKind::SemiColon
                | TokenKind::Comma
                | TokenKind::Op(_)
                | TokenKind::False
                | TokenKind::True
                | TokenKind::DecimalInteger(_)
                | TokenKind::BinaryInteger(_)
                | TokenKind::OctalInteger(_)
                | TokenKind::HexadecimalInteger(_)
                | TokenKind::Ascii(_)
                | TokenKind::Str(_)
                | TokenKind::RawStr(_) => {
                    return Err(Error {
                        kind: ErrorKind::ExpectedTypeName,
                        col: after_variable_name_token.col,
                        pointers_count: after_variable_name_token.kind.display_len(self.tokens),
                    })
                }
                TokenKind::Let
                | TokenKind::Var
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
                        pointers_count: after_variable_name_token.kind.display_len(self.tokens),
                    })
                }
                TokenKind::Unexpected(_) | TokenKind::Comment(_) | TokenKind::BlockComment(_) => {
                    self.should_have_been_skipped(after_variable_name_token)
                }
            };

            #[expect(clippy::cast_possible_truncation)]
            let array_dimensions_start = self.syntax_tree.array_dimensions.len() as offset32;
            while let Some(Peeked {
                token: Token { kind: TokenKind::OpenSquareBracket, col: open_square_bracket_column },
                index: open_square_bracket_token_index,
            }) = self.peek_next_token()
            {
                self.token_index = open_square_bracket_token_index;

                let dimension_expression_token = self.next_expected_token(Expected::Expression)?;
                let dimension_expression = self.expression(dimension_expression_token)?;

                let Some(Peeked {
                    token:
                        Token { kind: TokenKind::CloseSquareBracket, col: close_square_bracket_column },
                    index: close_square_bracket_token_index,
                }) = self.peek_next_token()
                else {
                    return Err(Error {
                        kind: ErrorKind::MissingCloseSquareBracketInArrayType,
                        col: dimension_expression_token.col,
                        pointers_count: dimension_expression_token.kind.display_len(self.tokens),
                    });
                };
                self.token_index = close_square_bracket_token_index;

                self.syntax_tree.array_dimensions.push(ArrayDimension {
                    open_square_bracket_column,
                    dimension_expression,
                    close_square_bracket_column,
                });
            }

            let Some(colon_column) = NonZero::new(after_variable_name_token.col) else {
                unreachable!("valid `:` should have non-zero column");
            };

            Some(TypeAnnotation {
                colon_column,
                type_name,
                type_name_column: type_name_token.col,
                array_dimensions_start,
                #[expect(clippy::cast_possible_truncation)]
                array_dimensions_len: self.syntax_tree.array_dimensions.len() as offset32
                    - array_dimensions_start,
            })
        };

        let equals_or_semicolon_token = self.next_expected_token(Expected::EqualsOrSemicolon)?;
        return match equals_or_semicolon_token.kind {
            TokenKind::SemiColon => Ok((
                VariableDefinition {
                    name: variable_name,
                    name_column: variable_name_token.col,
                    type_annotation,
                    initial_value: None,
                },
                equals_or_semicolon_token.col,
            )),
            TokenKind::Op(Op::Equals) => {
                let start_of_initial_value_token =
                    self.next_expected_token(Expected::Expression)?;
                let expression = self.expression(start_of_initial_value_token)?;
                let semicolon_column = self.semicolon()?;

                let Some(equals_column) = NonZero::new(equals_or_semicolon_token.col) else {
                    unreachable!("valid `=` should have non-zero column");
                };

                Ok((
                    VariableDefinition {
                        name: variable_name,
                        name_column: variable_name_token.col,
                        type_annotation,
                        initial_value: Some(InitialValue { equals_column, expression }),
                    },
                    semicolon_column,
                ))
            }
            TokenKind::OpenRoundBracket
            | TokenKind::CloseRoundBracket
            | TokenKind::OpenSquareBracket
            | TokenKind::CloseSquareBracket
            | TokenKind::OpenCurlyBracket
            | TokenKind::CloseCurlyBracket
            | TokenKind::Colon
            | TokenKind::Comma
            | TokenKind::Op(_)
            | TokenKind::False
            | TokenKind::True
            | TokenKind::DecimalInteger(_)
            | TokenKind::BinaryInteger(_)
            | TokenKind::OctalInteger(_)
            | TokenKind::HexadecimalInteger(_)
            | TokenKind::Ascii(_)
            | TokenKind::Str(_)
            | TokenKind::RawStr(_)
            | TokenKind::Identifier(_)
            | TokenKind::IdentifierStr(_)
            | TokenKind::Let
            | TokenKind::Var
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
                    pointers_count: variable_name_token.kind.display_len(self.tokens),
                }),
                Some(_) => Err(Error {
                    kind: ErrorKind::ExpectedEqualsOrSemicolonAfterTypeAnnotation,
                    col: equals_or_semicolon_token.col,
                    pointers_count: equals_or_semicolon_token.kind.display_len(self.tokens),
                }),
            },
            TokenKind::Unexpected(_) | TokenKind::Comment(_) | TokenKind::BlockComment(_) => {
                self.should_have_been_skipped(equals_or_semicolon_token)
            }
        };
    }
}

// control flow statements
impl Parser<'_, '_, '_, '_> {
    fn if_statement(&mut self, if_column: offset32) -> Result<(), Error<ErrorKind>> {
        let start_of_condition_token = self.next_expected_token(Expected::Expression)?;
        let condition = self.expression(start_of_condition_token)?;
        let end_of_condition_token = self.peek_previous_token();

        let after_if_condition_token = self.next_expected_token(Expected::OpenCurlyBracket)?;
        let TokenKind::OpenCurlyBracket = after_if_condition_token.kind else {
            return Err(Error {
                kind: ErrorKind::IfMustBeFollowedByBlock,
                col: end_of_condition_token.col,
                pointers_count: end_of_condition_token.kind.display_len(self.tokens),
            });
        };

        let mut placeholder_if_else_ifs_count = 0;
        self.syntax_tree.nodes.push(Node::If { if_column, condition, else_ifs_count: 0 });
        let placeholder_if_node_index = self.syntax_tree.nodes.len() - 1;

        let ParsedNode::Scope = self.any(after_if_condition_token)? else {
            unreachable!();
        };

        while let Some(Peeked {
            token: else_token @ Token { kind: TokenKind::Else, col: else_column },
            index: else_token_index,
        }) = self.peek_next_token()
        {
            self.token_index = else_token_index;

            let after_else_token = self.next_expected_token(Expected::OpenCurlyBracketOrIf)?;
            match after_else_token.kind {
                TokenKind::OpenCurlyBracket => {
                    let ParsedNode::Scope = self.any(after_else_token)? else {
                        unreachable!();
                    };

                    self.syntax_tree.nodes[placeholder_if_node_index] = Node::IfElse {
                        if_column,
                        condition,
                        else_ifs_count: placeholder_if_else_ifs_count,
                        else_column,
                    };
                    return Ok(());
                }
                TokenKind::If => {
                    let start_of_else_if_condition_token = self.next_expected_token(Expected::Expression)?;
                    let else_if_condition = self.expression(start_of_else_if_condition_token)?;
                    let end_of_else_if_condition_token = self.peek_previous_token();

                    let after_else_if_condition_token = self.next_expected_token(Expected::OpenCurlyBracket)?;
                    let TokenKind::OpenCurlyBracket = after_else_if_condition_token.kind else {
                        return Err(Error {
                            kind: ErrorKind::IfMustBeFollowedByBlock,
                            col: end_of_else_if_condition_token.col,
                            pointers_count: end_of_else_if_condition_token.kind.display_len(self.tokens),
                        });
                    };

                    placeholder_if_else_ifs_count += 1;
                    self.syntax_tree.nodes.push(Node::ElseIf {
                        else_column,
                        if_column: after_else_token.col,
                        condition: else_if_condition,
                    });

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
                | TokenKind::OpenRoundBracket
                | TokenKind::CloseRoundBracket
                | TokenKind::OpenSquareBracket
                | TokenKind::CloseSquareBracket
                | TokenKind::CloseCurlyBracket
                | TokenKind::False
                | TokenKind::True
                | TokenKind::DecimalInteger(_)
                | TokenKind::BinaryInteger(_)
                | TokenKind::OctalInteger(_)
                | TokenKind::HexadecimalInteger(_)
                | TokenKind::Ascii(_)
                | TokenKind::Str(_)
                | TokenKind::RawStr(_)
                | TokenKind::Identifier(_)
                | TokenKind::IdentifierStr(_)
                | TokenKind::Print
                | TokenKind::PrintLn
                | TokenKind::Eprint
                | TokenKind::EprintLn
                | TokenKind::Let
                | TokenKind::Var
                | TokenKind::Else
                | TokenKind::Loop
                | TokenKind::Break
                | TokenKind::Continue => {
                    return Err(Error {
                        kind: ErrorKind::ElseMustBeFollowedByBlockOrIf,
                        col: else_token.col,
                        pointers_count: else_token.kind.display_len(self.tokens),
                    });
                }
                TokenKind::Unexpected(_) | TokenKind::Comment(_) | TokenKind::BlockComment(_) => {
                    self.should_have_been_skipped(after_else_token)
                }
            }
        }

        self.syntax_tree.nodes[placeholder_if_node_index] =
            Node::If { if_column, condition, else_ifs_count: placeholder_if_else_ifs_count };
        return Ok(());
    }

    fn do_loop_statement(
        &mut self,
        do_column: offset32,
        loop_column: offset32,
    ) -> Result<(), Error<ErrorKind>> {
        let start_of_condition_token = self.next_expected_token(Expected::Expression)?;
        let condition = self.expression(start_of_condition_token)?;
        let end_of_condition_token = self.peek_previous_token();

        let after_condition_token = self.next_expected_token(Expected::OpenCurlyBracket)?;
        let TokenKind::OpenCurlyBracket = after_condition_token.kind else {
            return Err(Error {
                kind: ErrorKind::LoopMustBeFollowedByBlock,
                col: end_of_condition_token.col,
                pointers_count: end_of_condition_token.kind.display_len(self.tokens),
            });
        };

        self.syntax_tree.nodes.push(Node::DoLoop { do_column, loop_column, condition });

        let ParsedNode::Scope = self.any(after_condition_token)? else {
            unreachable!();
        };
        return Ok(());
    }

    fn loop_statement(&mut self, loop_column: offset32) -> Result<(), Error<ErrorKind>> {
        let start_of_condition_token = self.next_expected_token(Expected::Expression)?;
        let condition = self.expression(start_of_condition_token)?;
        let end_of_condition_token = self.peek_previous_token();

        let after_condition_token = self.next_expected_token(Expected::OpenCurlyBracket)?;
        let TokenKind::OpenCurlyBracket = after_condition_token.kind else {
            return Err(Error {
                kind: ErrorKind::LoopMustBeFollowedByBlock,
                col: end_of_condition_token.col,
                pointers_count: end_of_condition_token.kind.display_len(self.tokens),
            });
        };

        self.syntax_tree.nodes.push(Node::Loop { loop_column, condition });

        let ParsedNode::Scope = self.any(after_condition_token)? else {
            unreachable!();
        };
        return Ok(());
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum Expected {
    Statement,
    OperatorOrSemicolon,
    Operand,
    CloseRoundBracket,
    CloseSquareBracket,
    Expression,
    ExpressionOrSemicolon,
    Comma,
    CommaOrSemicolonCloseSquareBracket,
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
            Self::CommaOrSemicolonCloseSquareBracket => write!(f, "',', ';' or ']'"),
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

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
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
    ExpectedCloseRoundBracket,
    ExpectedCommaOrCloseSquareBracket,
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
                "cannot be a keyword, if you meant to reference a variable with this name, try wrapping this in `".into(),
            ),
            Self::ExpectedOperand => (
                "invalid expression".into(),
                "expected operand before this token".into(),
            ),
            Self::ExpectedCloseRoundBracket => (
                "invalid expression".into(),
                "expected ')' bracket before this token".into(),
            ),
            Self::ExpectedCommaOrCloseSquareBracket => (
                "invalid array".into(),
                "expected ',' or ']' before this token".into(),
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
                "cannot be a keyword, if you meant to reference a variable with this name, try wrapping this in `".into(),
            ),
            Self::ExpectedTypeName => (
                "invalid type annotation".into(),
                "expected type name after here".into(),
            ),
            Self::KeywordInTypeName => (
                "invalid type name".into(),
                "cannot be a keyword, if you meant to reference a variable with this name, try wrapping this in `".into(),
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
