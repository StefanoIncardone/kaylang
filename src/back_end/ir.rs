use core::fmt::Display;
use back_to_front::{ascii, usize32};
use crate::front_end::{
    Index32, src_file::Position, tokenizer::{EscapedAscii, TextIndex}, typed_abstract_syntax_tree::{self as tast, TypedSyntaxTree}, types::BaseType,
};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub enum Type {
    Base(BaseType),
    Array {
        base_type: BaseType,
        // TODO(stefano): allow zero length arrays
        // TODO(stefano): allow nested arrays
        /// always greater than 0
        len: u64,
    },
}

impl Display for Type {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return match self {
            Self::Base(typ) => write!(f, "{typ}"),
            Self::Array { base_type, len } => write!(f, "{base_type}[{len}]"),
        };
    }
}

pub(crate) type ExpressionIndex<'code> = Index32<Expression<'code>>;
pub(crate) type OperandIndex<'code> = Index32<Operand<'code>>;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
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

    TempVariable {
        value: ExpressionIndex<'code>,
    },
    Variable {
        value: ExpressionIndex<'code>,
        name: TextIndex<'code>,
    },
}

impl Operand<'_> {
    pub(crate) fn typ(&self, ir: &Ir<'_, '_, '_, '_, '_>) -> Type {
        return match self {
            Self::False { .. } | Self::True { .. } => Type::Base(BaseType::Bool),
            Self::I64 { .. }                       => Type::Base(BaseType::I64),
            Self::Ascii { .. }                     => Type::Base(BaseType::Ascii),
            Self::Str { .. }                       => Type::Base(BaseType::Str),
            Self::TempVariable { value, .. } | Self::Variable { value, .. } => {
                let expression = &ir.expressions[*value];
                expression.typ(ir)
            },
        };
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub(crate) enum Expression<'code> {
    Operand {
        operand: OperandIndex<'code>,
        column: usize32,
    },
    TempOperand {
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

impl Expression<'_> {
    pub(crate) fn typ(&self, ir: &Ir<'_, '_, '_, '_, '_>) -> Type {
        return match self {
            Self::Operand { operand, .. } | Self::TempOperand { operand } => {
                let ir_operand = &ir.operands[*operand];
                ir_operand.typ(ir)
            },
            // Self::False { .. } | Self::True { .. } => Type::Base(BaseType::Bool),
            // Self::I64 { .. } => Type::Base(BaseType::I64),
            // Self::Ascii { .. } => Type::Base(BaseType::Ascii),
            // Self::Str { .. } => Type::Base(BaseType::Str),
            // Self::Variable { variable, .. } => {
            //     let variable_definition = &ast.variables[*variable];
            //     variable_definition.typ.clone()
            // },
            // Self::Array { base_type, items_len, .. } => {
            //     Type::Array { base_type: *base_type, len: *items_len }
            // },
            // Self::Prefix { .. } => PrefixOp::TYPE,
            // Self::BooleanPrefix { .. } => BooleanPrefixOp::TYPE,
            // Self::Binary { .. } => BinaryOp::TYPE,
            // Self::BooleanBinary { .. } => BooleanBinaryOp::TYPE,
            // Self::Comparison { .. } => ComparisonOp::TYPE,
            // Self::BooleanComparison { .. } => BooleanComparisonOp::TYPE,
            // Self::Index { indexed_expression, .. } => {
            //     let expression = &ast.expressions[*indexed_expression];
            //     match expression.typ(ast) {
            //         Type::Base(BaseType::Str) => Type::Base(BaseType::Ascii),
            //         base_type @ Type::Base(BaseType::I64 | BaseType::Bool | BaseType::Ascii) => base_type,
            //         Type::Array { base_type, .. } => Type::Base(base_type),
            //     }
            // },
        };
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub struct TempVariableDisplay<'code> {
    pub(crate) value: ExpressionIndex<'code>,
}

impl Display for TempVariableDisplay<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return write!(f, "t{}", self.value.0);
    }
}

pub(crate) type NodeIndex<'code> = Index32<Node<'code>>;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub(crate) enum Node<'code> {
    Expression { value: ExpressionIndex<'code> },
    TempVariable { value: ExpressionIndex<'code> },
    Variable { value: ExpressionIndex<'code>, name: TextIndex<'code>, name_column: usize32 },

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
        print_column: usize32,
        argument: ExpressionIndex<'code>,
    },
    Eprint {
        eprint_column: usize32,
        argument: ExpressionIndex<'code>,
    },

    // Label { label: LabelIndex },
    // If { condition: ExpressionIndex<'code> },
    // Break { label: LabelIndex },
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub struct Ir<'tast, 'st: 'tast, 'tokens: 'st, 'code: 'tokens, 'path> {
    pub(crate) tast: &'tast TypedSyntaxTree<'st, 'tokens, 'code, 'path>,
    pub(crate) nodes: Vec<Node<'code>>,

    pub(crate) operands: Vec<Operand<'code>>,
    pub(crate) expressions: Vec<Expression<'code>>,

    pub(crate) labels: Vec<String>,

}

impl<'tast, 'tokens: 'tast> Ir<'tast, '_, 'tokens, '_, '_> {
    #[inline(always)]
    pub const fn display_debug(&self) -> IrDebug<'_, 'tast, '_, 'tokens, '_, '_> {
        return IrDebug { ir: self };
    }

    #[inline(always)]
    pub const fn display(&self) -> IrDisplay<'_, 'tast, '_, 'tokens, '_, '_> {
        return IrDisplay { ir: self };
    }
}

impl<'code> Ir<'_, '_, '_, 'code, '_> {
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

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub struct Parser<'tast, 'st: 'tast, 'tokens: 'st, 'code: 'tokens, 'path> {
    ir: Ir<'tast, 'st, 'tokens, 'code, 'path>,
    node_index: tast::NodeIndex<'code>,
}

impl<'tast, 'st: 'tast, 'tokens: 'st, 'code: 'tokens, 'path> Parser<'tast, 'st, 'tokens, 'code, 'path> {
    pub fn parse(
        tast: &'tast TypedSyntaxTree<'st, 'tokens, 'code, 'path>
    ) -> Ir<'tast, 'st, 'tokens, 'code, 'path> {
        let mut parser = Self {
            node_index: tast::NodeIndex::new(0),

            ir: Ir {
                nodes: Vec::new(),
                operands: Vec::new(),
                expressions: Vec::new(),
                labels: Vec::new(),
                tast,
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
                let ir_expression_index = self.new_expression(*expression_index);
                let ir_variable = Node::TempVariable { value: ir_expression_index };
                self.ir.nodes.push(ir_variable);
            },

            tast::Node::LetVariableDefinition { variable }
            | tast::Node::VarVariableDefinition { variable } => {
                let tast_variable = &self.ir.tast.variables[*variable];
                let ir_expression_index = self.new_expression(tast_variable.value);
                let ir_variable = Node::Variable { value: ir_expression_index, name: tast_variable.name, name_column: tast_variable.name_column };
                self.ir.nodes.push(ir_variable);
            },

            tast::Node::Print { argument, print_column } => {
                let ir_expression_index = self.new_expression(*argument);
                let println_node = Node::Print { argument: ir_expression_index, print_column: *print_column };
                self.ir.nodes.push(println_node);
            },
            tast::Node::Println { argument, println_column } => {
                let ir_expression_index = self.new_expression(*argument);
                let println_node = Node::Print { argument: ir_expression_index, print_column: *println_column };
                self.ir.nodes.push(println_node);

                let new_line_operand_index = self.ir.new_operand(Operand::Ascii { character: b'\n' });
                let new_line_expression_index = self.ir.new_expression(Expression::TempOperand { operand: new_line_operand_index });
                let new_line_expression_node = Node::TempVariable { value: new_line_expression_index };
                self.ir.nodes.push(new_line_expression_node);

                let temp_operand_index = self.ir.new_operand(Operand::TempVariable { value: new_line_expression_index });
                let temp_expression_index = self.ir.new_expression(Expression::TempOperand { operand: temp_operand_index });
                let print_new_line_node = Node::Print { argument: temp_expression_index, print_column: *println_column };
                self.ir.nodes.push(print_new_line_node);
            },
            tast::Node::PrintlnNoArg { println_column } => {
                let new_line_operand_index = self.ir.new_operand(Operand::Ascii { character: b'\n' });
                let new_line_expression_index = self.ir.new_expression(Expression::TempOperand { operand: new_line_operand_index });
                let new_line_expression_node = Node::TempVariable { value: new_line_expression_index };
                self.ir.nodes.push(new_line_expression_node);

                let temp_operand_index = self.ir.new_operand(Operand::TempVariable { value: new_line_expression_index });
                let temp_expression_index = self.ir.new_expression(Expression::TempOperand { operand: temp_operand_index });
                let print_new_line_node = Node::Print { argument: temp_expression_index, print_column: *println_column };
                self.ir.nodes.push(print_new_line_node);
            },
            tast::Node::Eprint { argument, eprint_column } => {
                let ir_expression = self.expression(*argument);
                let ir_expression_index = self.ir.new_expression(ir_expression);
                let eprintln_node = Node::Eprint { argument: ir_expression_index, eprint_column: *eprint_column };
                self.ir.nodes.push(eprintln_node);
            },
            tast::Node::Eprintln { argument, eprintln_column } => {
                let ir_expression = self.expression(*argument);
                let ir_expression_index = self.ir.new_expression(ir_expression);
                let eprintln_node = Node::Eprint { argument: ir_expression_index, eprint_column: *eprintln_column };
                self.ir.nodes.push(eprintln_node);

                let new_line_operand_index = self.ir.new_operand(Operand::Ascii { character: b'\n' });
                let new_line_expression_index = self.ir.new_expression(Expression::TempOperand { operand: new_line_operand_index });
                let new_line_expression_node = Node::TempVariable { value: new_line_expression_index };
                self.ir.nodes.push(new_line_expression_node);

                let temp_operand_index = self.ir.new_operand(Operand::TempVariable { value: new_line_expression_index });
                let temp_expression_index = self.ir.new_expression(Expression::TempOperand { operand: temp_operand_index });
                let eprint_new_line_node = Node::Eprint { argument: temp_expression_index, eprint_column: *eprintln_column };
                self.ir.nodes.push(eprint_new_line_node);
            },
            tast::Node::EprintlnNoArg { eprintln_column } => {
                let new_line_operan_index = self.ir.new_operand(Operand::Ascii { character: b'\n' });
                let new_line_expression_index = self.ir.new_expression(Expression::TempOperand { operand: new_line_operan_index });
                let new_line_expression_node = Node::TempVariable { value: new_line_expression_index };
                self.ir.nodes.push(new_line_expression_node);

                let temp_operand_index = self.ir.new_operand(Operand::TempVariable { value: new_line_expression_index });
                let temp_expression_index = self.ir.new_expression(Expression::TempOperand { operand: temp_operand_index });
                let eprint_new_line_node = Node::Eprint { argument: temp_expression_index, eprint_column: *eprintln_column };
                self.ir.nodes.push(eprint_new_line_node);
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
            tast::Node::If { condition, .. } => {
                unimplemented!()
            },
            tast::Node::ElseIf { condition, .. } => {
                unimplemented!()
            },
            tast::Node::Else { .. } => {
                unimplemented!()
            },
            tast::Node::Loop { condition, .. } => {
                unimplemented!()
            },
            tast::Node::DoLoop { condition, .. } => {
                unimplemented!()
            },
            tast::Node::Break { .. } => {
                unimplemented!()
            },
            tast::Node::Continue { .. } => {
                unimplemented!()
            },
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
struct Peeked<'tast, 'code: 'tast> {
    node: &'tast tast::Node<'code>,
    index: tast::NodeIndex<'code>,
}

impl<'tast, 'code: 'tast> Parser<'tast, '_, '_, 'code, '_> {
    #[must_use]
    fn peek_next_node(&self) -> Option<Peeked<'tast, 'code>> {
        let next_node = self.node_index.get(&self.ir.tast.nodes)?;
        let peeked_node_index_index = tast::NodeIndex::new_usize32(self.node_index.0 + 1);
        return Some(Peeked { node: next_node, index: peeked_node_index_index });
    }
}

impl<'tast, 'code: 'tast> Parser<'tast, '_, '_, 'code, '_> {
    #[inline]
    fn operand(&mut self, column: usize32, operand: Operand<'code>) -> Expression<'code> {
        let operand_index = self.ir.new_operand(operand);
        return Expression::Operand { operand: operand_index, column }
    }

    fn expression(&mut self, expression_index: tast::ExpressionIndex<'code>) -> Expression<'code> {
        let tast_expression = &self.ir.tast.expressions[expression_index];
        return match tast_expression {
            tast::Expression::False { column, .. } => self.operand(*column, Operand::False),
            tast::Expression::True { column, .. } => self.operand(*column, Operand::True),
            tast::Expression::I64 { value, column, .. } => self.operand(*column, Operand::I64 { value: *value }),
            tast::Expression::Ascii { character, column, .. } => self.operand(*column, Operand::Ascii { character: *character }),
            tast::Expression::Str { literal, column, .. } => self.operand(*column, Operand::Str { literal: *literal }),
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

    fn new_expression(&mut self, expression_index: tast::ExpressionIndex<'code>) -> ExpressionIndex<'code> {
        let ir_expression = self.expression(expression_index);
        let ir_expression_index = self.ir.new_expression(ir_expression);
        return ir_expression_index;
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub struct IrDebug<'ir, 'tast, 'st: 'tast, 'tokens: 'st, 'code: 'tokens, 'path> {
    pub(crate) ir: &'ir Ir<'tast, 'st, 'tokens, 'code, 'path>,
}

impl IrDebug<'_, '_, '_, '_, '_, '_> {
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
            Node::Expression { value } => self.info_expression(f, *value, indent),
            Node::TempVariable { value } => {
                writeln!(f, "{:>indent$}Let", "")?;
                let variable_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *value, variable_indent)
            }
            Node::Variable { value, name, .. } => {
                writeln!(f, "{:>indent$}Let", "")?;
                let variable_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *value, variable_indent)
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

            Node::Print { argument, .. } => {
                writeln!(f, "{:>indent$}Print = print", "")?;
                let argument_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *argument, argument_indent)
            }
            Node::Eprint { argument, .. } => {
                writeln!(f, "{:>indent$}Eprint = eprint", "")?;
                let argument_indent = indent + Self::INDENT_INCREMENT;
                self.info_expression(f, *argument, argument_indent)
            }

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
                let literal_str = self.ir.tast.syntax_tree.text()[*literal];
                writeln!(f, "{:>indent$}Str = {literal_str}", "")
            }
            Operand::TempVariable { value } => {
                let variable_name = TempVariableDisplay { value: *value };
                writeln!(f, "{:>indent$}Name = {variable_name}", "")
            }
            Operand::Variable { name, .. } => {
                let variable_name = self.ir.tast.syntax_tree.text()[*name];
                writeln!(f, "{:>indent$}Name = {variable_name}", "")
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
            Expression::Operand { operand, .. } => self.info_operand(f, *operand, indent),
            Expression::TempOperand { operand } => self.info_operand(f, *operand, indent),
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

impl Display for IrDebug<'_, '_, '_, '_, '_, '_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let mut node_index = NodeIndex::new(0);
        while (node_index.0 as usize) < self.ir.nodes.len() {
            self.info_node(f, &mut node_index, 0)?;
        }

        return Ok(());
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub struct SrcAttribute<'path> {
    pub(crate) position: Position<'path>,
}

impl<'path> SrcAttribute<'path> {
    fn new(ir: &Ir<'_, '_, '_, '_, 'path>, column: usize32) -> Self {
        let position = Position::new(&ir.tast.syntax_tree.tokens.src, column);
        return Self { position };
    }
}

impl Display for SrcAttribute<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        // BUG(stefano): i may have found a bug in VsCode: when two file locations appear on the same line
            // any location following the first point to first instead of at the correct location
            // IDEA(stefano): submit an issue
        return write!(f, "#src({})", self.position);
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub struct IrDisplay<'ir, 'tast, 'st: 'tast, 'tokens: 'st, 'code: 'tokens, 'path> {
    pub(crate) ir: &'ir Ir<'tast, 'st, 'tokens, 'code, 'path>,
}

impl IrDisplay<'_, '_, '_, '_, '_, '_> {
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
            Node::Expression { value } => {
                let variable_value = &self.ir.expressions[*value];
                self.info_expression(f, variable_value, indent)?;
                writeln!(f, ";")
            }
            Node::TempVariable { value } => {
                let ir_variable_name = TempVariableDisplay { value: *value };
                let ir_variable_value = &self.ir.expressions[*value];
                let ir_variable_type = ir_variable_value.typ(self.ir);
                write!(f, "{:>indent$}let {ir_variable_name}: {ir_variable_type} = ", "")?;
                self.info_expression(f, ir_variable_value, indent)?;
                writeln!(f, ";")
            }
            Node::Variable { value, name, name_column, .. } => {
                let variable_name = self.ir.tast.syntax_tree.text()[*name];
                let ir_variable_value = &self.ir.expressions[*value];
                let ir_variable_type = ir_variable_value.typ(self.ir);
                let src_attribute = SrcAttribute::new(self.ir, *name_column);
                write!(f, "{:>indent$}let {variable_name} {src_attribute}: {ir_variable_type} = ", "")?;
                self.info_expression(f, ir_variable_value, indent)?;
                writeln!(f, ";")
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

            Node::Print { argument, .. } => {
                write!(f, "{:>indent$}print ", "")?;
                let argument_indent = indent + Self::INDENT_INCREMENT;
                let ir_argument = &self.ir.expressions[*argument];
                self.info_expression(f, ir_argument, argument_indent)?;
                writeln!(f, ";")
            }
            Node::Eprint { argument, .. } => {
                write!(f, "{:>indent$}eprint ", "")?;
                let argument_indent = indent + Self::INDENT_INCREMENT;
                let ir_argument = &self.ir.expressions[*argument];
                self.info_expression(f, ir_argument, argument_indent)?;
                writeln!(f, ";")
            }

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
        operand: &Operand<'_>,
    ) -> core::fmt::Result {
        #[rustfmt::skip]
        return match operand {
            Operand::False => write!(f, "false"),
            Operand::True => write!(f, "true"),
            Operand::I64 { value } => write!(f, "{value}"),
            Operand::Ascii { character } => {
                let escaped_character = EscapedAscii { ch: *character };
                write!(f, "'{escaped_character}'")
            },
            Operand::Str { literal } => {
                let literal_str = self.ir.tast.syntax_tree.text()[*literal];
                write!(f, "{literal_str}")
            }
            Operand::TempVariable { value } => {
                let variable_name = TempVariableDisplay { value: *value };
                write!(f, "{variable_name}")
            }
            Operand::Variable { name, .. } => {
                let variable_name = self.ir.tast.syntax_tree.text()[*name];
                write!(f, "{variable_name}")
            }
        };
    }

    fn info_expression(
        &self,
        f: &mut core::fmt::Formatter<'_>,
        expression: &Expression<'_>,
        indent: usize,
    ) -> core::fmt::Result {
        #[rustfmt::skip]
        return match expression {
            Expression::Operand { operand, column } => {
                let ir_operand = &self.ir.operands[*operand];
                self.info_operand(f, ir_operand)?;

                let src_attribute = SrcAttribute::new(self.ir, *column);
                write!(f, " {src_attribute}")
            }
            Expression::TempOperand { operand } => {
                let ir_operand = &self.ir.operands[*operand];
                self.info_operand(f, ir_operand)
            }

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

impl Display for IrDisplay<'_, '_, '_, '_, '_, '_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let mut node_index = NodeIndex::new(0);
        while (node_index.0 as usize) < self.ir.nodes.len() {
            self.info_node(f, &mut node_index, 0)?;
        }

        return Ok(());
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
