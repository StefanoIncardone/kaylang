use core::{fmt::Display, marker::PhantomData};
use back_to_front::offset32;
use crate::front_end::{
    src_file::SrcCode, tokenizer::{ascii, TextIndex, Tokens}, typed_abstract_syntax_tree::{self as tast, TypedSyntaxTree}, SliceIndexPtr
};

pub(crate) type ExpressionIndex<'code> = SliceIndexPtr<Expression<'code>>;
pub(crate) type OperandIndex<'code> = SliceIndexPtr<Operand<'code>>;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) enum Operand<'code> {
    False,
    True,
    I64 {
        value: i64,
    },
    Ascii {
        character: ascii,
    },
    Str {
        literal: TextIndex<'code>,
    },

    LetVariable {
        variable: VariableDefinitionIndex<'code>,
    },
    VarVariable {
        variable: VariableDefinitionIndex<'code>,
    },
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) enum Expression<'code> {
    Operand {
        operand: OperandIndex<'code>,
    },

    // Prefix {
    //     operator: tast::PrefixOp,
    //     operator_column: offset32,
    //     right_operand: OperandIndex<'code>,
    // },
    // BooleanPrefix {
    //     operator: tast::BooleanPrefixOp,
    //     operator_column: offset32,
    //     right_operand: OperandIndex<'code>,
    // },
    // Binary {
    //     left_operand: OperandIndex<'code>,
    //     operator: tast::BinaryOp,
    //     operator_column: offset32,
    //     right_operand: OperandIndex<'code>,
    // },
    // BooleanBinary {
    //     left_operand: OperandIndex<'code>,
    //     operator: tast::BooleanBinaryOp,
    //     operator_column: offset32,
    //     right_operand: OperandIndex<'code>,
    // },
    // Comparison {
    //     left_operand: OperandIndex<'code>,
    //     operator: tast::ComparisonOp,
    //     operator_column: offset32,
    //     right_operand: OperandIndex<'code>,
    // },
    // BooleanComparison {
    //     left_operand: OperandIndex<'code>,
    //     operator: tast::BooleanComparisonOp,
    //     operator_column: offset32,
    //     right_operand: OperandIndex<'code>,
    // },

    // Index {
    //     variable: VariableDefinitionIndex<'code>,
    //     open_square_bracket_column: offset32,
    //     index_expression: OperandIndex<'code>,
    // }
}

pub(crate) type NameIndex = SliceIndexPtr<String>;
pub(crate) type VariableDefinitionIndex<'code> = SliceIndexPtr<VariableDefinition<'code>>;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) struct VariableDefinition<'code> {
    pub(crate) name: NameIndex,
    // pub(crate) typ: Type,
    pub(crate) value: ExpressionIndex<'code>,
}

pub(crate) type NodeIndex<'code> = SliceIndexPtr<Node<'code>>;
pub(crate) type LabelIndex = SliceIndexPtr<String>;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) enum Node<'code> {
    LetVariable { variable: VariableDefinitionIndex<'code> },
    VarVariable { variable: VariableDefinitionIndex<'code> },

    // Assignment {
    //     target: ExpressionIndex<'code>,
    //     operator: AssignmentOp,
    //     new_value: ExpressionIndex<'code>,
    // },
    // BinaryAssignment {
    //     target: ExpressionIndex<'code>,
    //     operator: BinaryAssignmentOp,
    //     operator_column: offset32,
    //     new_value: ExpressionIndex<'code>,
    // },
    // BooleanAssignmentExpression {
    //     target: ExpressionIndex<'code>,
    //     operator: BooleanBinaryAssignmentOp,
    //     operator_column: offset32,
    //     new_value: ExpressionIndex<'code>,
    // },
    // PrefixAssignmentExpression {
    //     target: ExpressionIndex<'code>,
    //     operator: PrefixAssignmentOp,
    //     operator_column: offset32,
    // },
    // BooleanPrefixAssignment {
    //     target: ExpressionIndex<'code>,
    //     operator: BooleanPrefixAssignmentOp,
    //     operator_column: offset32,
    // },

    Print {
        argument: ExpressionIndex<'code>,
    },
    // Eprint {
    //     argument: ExpressionIndex<'code>,
    // },

    // Label { label: LabelIndex },
    // If { condition: ExpressionIndex<'code> },
    // Break { label: LabelIndex },
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Ir<'tast, 'st: 'tast, 'tokens: 'st, 'code: 'tokens> {
    pub(crate) nodes: Vec<Node<'code>>,

    pub(crate) operands: Vec<Operand<'code>>,
    pub(crate) expressions: Vec<Expression<'code>>,
    pub(crate) variable_names: Vec<String>,
    pub(crate) variables: Vec<VariableDefinition<'code>>,

    pub(crate) labels: Vec<String>,

    _typed_syntax_tree: PhantomData<&'tast TypedSyntaxTree<'st, 'tokens, 'code>>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct IrDisplay<
    'ir,
    'tast,
    'st: 'tast,
    'tokens: 'st,
    'code: 'tokens,
> {
    pub(crate) ir: &'ir Ir<'tast, 'st, 'tokens, 'code>,
    pub(crate) typed_syntax_tree: &'tast TypedSyntaxTree<'st, 'tokens, 'code>,
    pub(crate) tokens: &'tokens Tokens<'code>,
}

impl<'tast, 'tokens: 'tast> Ir<'tast, '_, 'tokens, '_> {
    #[must_use]
    #[inline(always)]
    pub const fn display(
        &self,
        typed_syntax_tree: &'tast TypedSyntaxTree<'_, 'tokens, '_>,
        tokens: &'tokens Tokens<'_>,
    ) -> IrDisplay<'_, 'tast, '_, 'tokens, '_> {
        return IrDisplay { ir: self, typed_syntax_tree, tokens };
    }
}

impl IrDisplay<'_, '_, '_, '_, '_> {
    const INDENT_INCREMENT: usize = 2;

    fn info_node(
        &self,
        f: &mut core::fmt::Formatter<'_>,
        node_index: &mut NodeIndex<'_>,
        indent: usize,
    ) -> core::fmt::Result {
        let node = &self.ir.nodes[*node_index];
        node_index.0 += 1;

        #[rustfmt::skip]
        return match node {
            Node::LetVariable { variable } => {
                writeln!(f, "{:>indent$}Let", "")?;
                let variable_indent = indent + Self::INDENT_INCREMENT;
                let VariableDefinition { name, value: initial_value, .. } = &self.ir.variables[*variable];
                let variable_text = &self.ir.variable_names[*name];
                writeln!(f, "{:>variable_indent$}Name = {variable_text}", "")?;
                self.info_expression(f, *initial_value, variable_indent)
            }
            Node::VarVariable { variable } => {
                writeln!(f, "{:>indent$}Var", "")?;
                let variable_indent = indent + Self::INDENT_INCREMENT;
                let VariableDefinition { name, value: initial_value, .. } = &self.ir.variables[*variable];
                let variable_text = &self.ir.variable_names[*name];
                writeln!(f, "{:>variable_indent$}Name = {variable_text}", "")?;
                self.info_expression(f, *initial_value, variable_indent)
            }

            // Node::Assignment { target, new_value, operator } => {
            //     writeln!(f, "{:>indent$}Assignment", "")?;
            //     let assignment_indent = indent + Self::INDENT_INCREMENT;
            //     self.info_expression(f, *target, assignment_indent)?;
            //     writeln!(f, "{:>assignment_indent$}AssignmentOp = {operator}", "")?;
            //     self.info_expression(f, *new_value, assignment_indent)
            // }
            // Node::BinaryAssignment { target, operator, new_value, .. } => {
            //     writeln!(f, "{:>indent$}AssignmentExpression", "")?;
            //     let assignment_indent = indent + Self::INDENT_INCREMENT;
            //     self.info_expression(f, *target, assignment_indent)?;
            //     writeln!(f, "{:>assignment_indent$}BinaryAssignmentOp = {operator}", "")?;
            //     self.info_expression(f, *new_value, assignment_indent)
            // }
            // Node::BooleanAssignmentExpression { target, operator, new_value, .. } => {
            //     writeln!(f, "{:>indent$}BooleanAssignmentExpression", "")?;
            //     let assignment_indent = indent + Self::INDENT_INCREMENT;
            //     self.info_expression(f, *target, assignment_indent)?;
            //     writeln!(f, "{:>assignment_indent$}BooleanBinaryAssignmentOp = {operator}", "")?;
            //     self.info_expression(f, *new_value, assignment_indent)
            // }
            // Node::PrefixAssignmentExpression { target, operator, .. } => {
            //     writeln!(f, "{:>indent$}Assignment", "")?;
            //     let assignment_indent = indent + Self::INDENT_INCREMENT;
            //     self.info_expression(f, *target, assignment_indent)?;
            //     writeln!(f, "{:>assignment_indent$}PrefixAssignmentOp = {operator}", "")
            // }
            // Node::BooleanPrefixAssignment { target, operator, .. } => {
            //     writeln!(f, "{:>indent$}Assignment", "")?;
            //     let assignment_indent = indent + Self::INDENT_INCREMENT;
            //     self.info_expression(f, *target, assignment_indent)?;
            //     writeln!(f, "{:>assignment_indent$}BooleanPrefixAssignmentOp = {operator}", "")
            // }

            Node::Print { argument } => {
                writeln!(f, "{:>indent$}Print = print", "")?;
                let argument_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *argument, argument_indent)
            }
            // Node::Eprint { argument } => {
            //     writeln!(f, "{:>indent$}Eprint = eprint", "")?;
            //     let argument_indent = indent + Self::INDENT_INCREMENT;
            //     self.info_expression(f, *argument, argument_indent)
            // }

            // Node::Label { label } => {
            //     let label_text = &self.ir.labels[*label];
            //     writeln!(f, "{:>indent$}Label = {label_text}", "")
            // }
            // Node::If { condition } => {
            //     writeln!(f, "{:>indent$}If", "")?;
            //     let if_indent = indent + Self::INDENT_INCREMENT;
            //     self.info_expression(f, *condition, if_indent)?;

            //     node_index.0 += 1;
            //     let Node::Break { label } = &self.ir.nodes[*node_index] else {
            //         unreachable!()
            //     };
            //     let label_text = &self.ir.labels[*label];
            //     writeln!(f, "{:>if_indent$}Break = {label_text}", "")
            // }
            // Node::Break { label } => {
            //     let label_text = &self.ir.labels[*label];
            //     writeln!(f, "{:>indent$}Break = {label_text}", "")
            // }
        };
    }

    fn info_operand(
        &self,
        f: &mut core::fmt::Formatter<'_>,
        operand_index: OperandIndex<'_>,
        indent: usize,
    ) -> core::fmt::Result {
        let operand = &self.ir.operands[operand_index];

        #[rustfmt::skip]
        return match operand {
            Operand::False { .. } => writeln!(f, "{:>indent$}False = false", ""),
            Operand::True { .. } => writeln!(f, "{:>indent$}True = true", ""),
            Operand::I64 { value, .. } => {
                writeln!(f, "{:>indent$}I64 = {value}", "")
            }
            Operand::Ascii { character, .. } => {
                writeln!(f, "{:>indent$}Ascii = {character}", "")
            }
            Operand::Str { literal, .. } => {
                let literal_str = self.tokens.text[*literal];
                writeln!(f, "{:>indent$}Str = {literal_str}", "")
            }
            Operand::LetVariable { variable, .. } => {
                let VariableDefinition { name, .. } = &self.ir.variables[*variable];
                let name_text = &self.ir.variable_names[*name];
                writeln!(f, "{:>indent$}Name = {name_text}", "")
            }
            Operand::VarVariable { variable, .. } => {
                let VariableDefinition { name, .. } = &self.ir.variables[*variable];
                let name_text = &self.ir.variable_names[*name];
                writeln!(f, "{:>indent$}Name = {name_text}", "")
            }
        };
    }

    fn info_expression(
        &self,
        f: &mut core::fmt::Formatter<'_>,
        expression_index: ExpressionIndex<'_>,
        indent: usize,
    ) -> core::fmt::Result {
        let expression = &self.ir.expressions[expression_index];

        #[rustfmt::skip]
        return match expression {
            Expression::Operand { operand } => self.info_operand(f, *operand, indent),
            // Expression::Prefix { operator, operator_column, right_operand } => {
            //     let expression_indent = indent + Self::INDENT_INCREMENT;
            //     writeln!(f, "{:>indent$}PrefixExpression", "")?;
            //     writeln!(f, "{:>expression_indent$}PrefixOp: {operator_column} = {operator}", "")?;
            //     self.info_operand(f, *right_operand, expression_indent)
            // }
            // Expression::BooleanPrefix { operator, right_operand, .. } => {
            //     let expression_indent = indent + Self::INDENT_INCREMENT;
            //     writeln!(f, "{:>indent$}BooleanPrefixExpression", "")?;
            //     writeln!(f, "{:>expression_indent$}BooleanPrefixOp = {operator}", "")?;
            //     self.info_operand(f, *right_operand, expression_indent)
            // }
            // Expression::Binary { left_operand, operator, right_operand, .. } => {
            //     let expression_indent = indent + Self::INDENT_INCREMENT;
            //     writeln!(f, "{:>indent$}BinaryExpression", "")?;
            //     self.info_operand(f, *left_operand, expression_indent)?;
            //     writeln!(f, "{:>expression_indent$}BinaryOp = {operator}", "")?;
            //     self.info_operand(f, *right_operand, expression_indent)
            // }
            // Expression::BooleanBinary { left_operand, operator, right_operand, .. } => {
            //     let expression_indent = indent + Self::INDENT_INCREMENT;
            //     writeln!(f, "{:>indent$}BooleanBinaryExpression", "")?;
            //     self.info_operand(f, *left_operand, expression_indent)?;
            //     writeln!(f, "{:>expression_indent$}BooleanBinaryOp = {operator}", "")?;
            //     self.info_operand(f, *right_operand, expression_indent)
            // }
            // Expression::Comparison { left_operand, operator, right_operand, .. } => {
            //     let expression_indent = indent + Self::INDENT_INCREMENT;
            //     writeln!(f, "{:>indent$}Comparison", "")?;
            //     self.info_operand(f, *left_operand, expression_indent)?;
            //     writeln!(f, "{:>expression_indent$}ComparisonOp = {operator}", "")?;
            //     self.info_operand(f, *right_operand, expression_indent)
            // }
            // Expression::BooleanComparison { left_operand, operator, right_operand, .. } => {
            //     let expression_indent = indent + Self::INDENT_INCREMENT;
            //     writeln!(f, "{:>indent$}BooleanComparison", "")?;
            //     self.info_operand(f, *left_operand, expression_indent)?;
            //     writeln!(f, "{:>expression_indent$}BooleanComparisonOp = {operator}", "")?;
            //     self.info_operand(f, *right_operand, expression_indent)
            // }
        };
    }
}

impl Display for IrDisplay<'_, '_, '_, '_, '_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let mut node_index = NodeIndex::new(0);
        while (node_index.0 as usize) < self.ir.nodes.len() {
            self.info_node(f, &mut node_index, 0)?;
        }

        return Ok(());
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Parser<'tast, 'st: 'tast, 'tokens: 'st, 'src: 'tokens, 'code: 'src, 'path: 'code> {
    src: &'src SrcCode<'code, 'path>,
    // errors: Vec<Msg<ErrorKind>>,

    tokens: &'tokens Tokens<'code>,
    node_index: tast::NodeIndex<'code>,
    tast: &'tast TypedSyntaxTree<'st, 'tokens, 'code>,

    ir: Ir<'tast, 'st, 'tokens, 'code>,
}

impl<'tast, 'st: 'tast, 'tokens: 'st, 'src: 'tokens, 'code: 'src, 'path: 'code>
    Parser<'tast, 'st, 'tokens, 'src, 'code, 'path>
{
    #[expect(clippy::missing_errors_doc)]
    pub fn parse(
        src: &'src SrcCode<'code, 'path>,
        tokens: &'tokens Tokens<'code>,
        typed_syntax_tree: &'tast TypedSyntaxTree<'st, 'tokens, 'code>,
    ) -> Ir<'tast, 'st, 'tokens, 'code> {
        let mut parser = Self {
            src,
            // errors: Vec::new(),

            tokens,
            node_index: tast::NodeIndex::new(0),
            tast: typed_syntax_tree,

            ir: Ir {
                nodes: Vec::new(),
                operands: Vec::new(),
                expressions: Vec::new(),
                variable_names: Vec::new(),
                variables: Vec::new(),
                labels: Vec::new(),
                _typed_syntax_tree: PhantomData,
            },
        };

        while let Some(peeked) = parser.peek_next_node() {
            parser.node_index = peeked.index;
            parser.any(peeked.node);
        }

        return parser.ir;
    }

    fn any(&mut self, node: &tast::Node<'code>) {
        match node {
            tast::Node::Expression(expression_index) => {
                let variable_index = self.new_variable(*expression_index);
                let let_variable_node = Node::LetVariable { variable: variable_index };
                self.ir.nodes.push(let_variable_node);
            },
            tast::Node::Print { argument } => {
                unimplemented!()
            },
            tast::Node::Println { argument } => {
                let expression = self.expression(*argument);
                let expression_index = self.ir.new_expression(expression);
                let println_node = Node::Print { argument: expression_index };
                self.ir.nodes.push(println_node);

                let new_line = Operand::Ascii { character: b'\n' };
                let new_line_index = self.ir.new_operand(new_line);
                let new_line_expression = Expression::Operand { operand: new_line_index };
                let new_line_expression_index = self.ir.new_expression(new_line_expression);
                let print_new_line_node = Node::Print { argument: new_line_expression_index };
                self.ir.nodes.push(print_new_line_node);
            },
            tast::Node::PrintlnNoArg => {
                unimplemented!()
            },
            tast::Node::Eprint { argument } => {
                unimplemented!()
            },
            tast::Node::Eprintln { argument } => {
                unimplemented!()
            },
            tast::Node::EprintlnNoArg => {
                unimplemented!()
            },
            tast::Node::LetVariableDefinition { variable } => {
                unimplemented!()
            },
            tast::Node::VarVariableDefinition { variable } => {
                unimplemented!()
            },
            tast::Node::Assignment { target, operator, new_value } => {
                unimplemented!()
            },
            tast::Node::BinaryAssignment { target, operator, operator_column, new_value } => {
                unimplemented!()
            },
            tast::Node::BooleanAssignmentExpression { target, operator, operator_column, new_value } => {
                unimplemented!()
            },
            tast::Node::PrefixAssignmentExpression { target, operator, operator_column } => {
                unimplemented!()
            },
            tast::Node::BooleanPrefixAssignment { target, operator, operator_column } => {
                unimplemented!()
            },
            tast::Node::Scope { raw_nodes_in_scope_count } => {
                unimplemented!()
            },
            tast::Node::If { condition } => {
                unimplemented!()
            },
            tast::Node::ElseIf { condition } => {
                unimplemented!()
            },
            tast::Node::Else => {
                unimplemented!()
            },
            tast::Node::Loop { condition } => {
                unimplemented!()
            },
            tast::Node::DoLoop { condition } => {
                unimplemented!()
            },
            tast::Node::Break => {
                unimplemented!()
            },
            tast::Node::Continue => {
                unimplemented!()
            },
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
struct Peeked<'tast, 'code: 'tast> {
    node: &'tast tast::Node<'code>,
    index: tast::NodeIndex<'code>,
}

impl<'tast, 'code: 'tast> Parser<'tast, '_, '_, '_, 'code, '_> {
    fn peek_next_node(&self) -> Option<Peeked<'tast, 'code>> {
        let next_node = self.node_index.get(&self.tast.nodes)?;
        let peeked_node_index_index = tast::NodeIndex::new_offset32(self.node_index.0 + 1);
        return Some(Peeked { node: next_node, index: peeked_node_index_index });
    }
}

impl<'code> Ir<'_, '_, '_, 'code> {
    #[inline]
    fn new_operand(&mut self, operand: Operand<'code>) -> OperandIndex<'code> {
        let index = OperandIndex::new(self.operands.len());
        self.operands.push(operand);
        return index;
    }

    #[inline]
    fn new_expression(&mut self, expression: Expression<'code>) -> ExpressionIndex<'code> {
        let index = ExpressionIndex::new(self.expressions.len());
        self.expressions.push(expression);
        return index;
    }
}

impl<'tast, 'code: 'tast> Parser<'tast, '_, '_, '_, 'code, '_> {
    fn expression(&mut self, expression_index: tast::ExpressionIndex<'code>) -> Expression<'code> {
        let tast_expression = &self.tast.expressions[expression_index];
        return match tast_expression {
            tast::Expression::False { .. } => unimplemented!(),
            tast::Expression::True { .. } => unimplemented!(),
            tast::Expression::I64 { value, .. } => {
                let operand = Operand::I64 { value: *value };
                let operand_index = self.ir.new_operand(operand);
                Expression::Operand { operand: operand_index }
            },
            tast::Expression::Ascii { character, .. } => unimplemented!(),
            tast::Expression::Str { literal, .. } => unimplemented!(),
            tast::Expression::Variable { variable, .. } => unimplemented!(),
            tast::Expression::Array { base_type, items_start, items_len } => unimplemented!(),
            tast::Expression::Prefix { operator, operator_column, right_operand } => unimplemented!(),
            tast::Expression::BooleanPrefix { operator, operator_column, right_operand } => unimplemented!(),
            tast::Expression::Binary { left_operand, operator, operator_column, right_operand } => unimplemented!(),
            tast::Expression::BooleanBinary { left_operand, operator, operator_column, right_operand } => unimplemented!(),
            tast::Expression::Comparison { left_operand, operator, operator_column, right_operand } => unimplemented!(),
            tast::Expression::BooleanComparison { left_operand, operator, operator_column, right_operand } => unimplemented!(),
            tast::Expression::Index { indexed_expression, open_square_bracket_column, index_expression } => unimplemented!(),
        };
    }

    #[inline]
    fn new_variable(
        &mut self,
        tast_expression_index: tast::ExpressionIndex<'code>,
    ) -> VariableDefinitionIndex<'code> {
        let variable_name_index = NameIndex::new(self.ir.variable_names.len());
        let variable_name = format!("t{}", variable_name_index.0);
        self.ir.variable_names.push(variable_name);
        let expression = self.expression(tast_expression_index);
        let expression_index = self.ir.new_expression(expression);
        let variable = VariableDefinition {
            name: variable_name_index,
            value: expression_index,
        };
        let variable_index = VariableDefinitionIndex::new(self.ir.variables.len());
        self.ir.variables.push(variable);
        return variable_index;
    }
}

// #[derive(Clone, Debug, Hash, PartialEq, Eq)]
// pub enum ErrorKind {
// }

// impl IntoMsgInfo for ErrorKind {
//     fn info(&self) -> MsgInfo {
//         let (error_message, error_cause_message) = match self {
//             _ => unimplemented!(),
//         };

//         return MsgInfo { message: error_message, cause: error_cause_message };
//     }
// }
