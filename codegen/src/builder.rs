use std::{cell::RefCell, collections::HashMap};

use inkwell::{
    values::{BasicValueEnum, PointerValue},
};
use llvm::Llvm;
use parser::ast::{Node, Program};

use super::expression::ExpressionBuilder;

pub struct RecursiveBuilder<'a, 'b> {
    pub llvm: &'b Llvm<'a>,

    pub builtins: RefCell<HashMap<String, PointerValue<'a>>>,
    pub variables: RefCell<HashMap<String, PointerValue<'a>>>,
}

impl<'ink, 'b> RecursiveBuilder<'ink, 'b> {
    pub fn new(llvm: &'b Llvm<'ink>) -> RecursiveBuilder<'ink, 'b> {
        RecursiveBuilder {
            llvm,
            builtins: RefCell::new(HashMap::new()),
            variables: RefCell::new(HashMap::new()),
        }
    }

    /// Build a node
    pub fn build(&self, ast: &Node) -> BasicValueEnum<'ink> {
        match ast {
            Node::Program(n) => self.build_program(n),
            Node::Statement(n) => self.build_statement(n),
            Node::Expression(n) => {
                let expression_builder = ExpressionBuilder::new(self.llvm);
                expression_builder.build_expression(n)
            }
            _ => panic!("Unknown node"),
        }
    }

    /// Build a program
    fn build_program(&self, program: &Program) -> BasicValueEnum<'ink> {
        let mut results: Vec<BasicValueEnum<'_>> = vec![];

        for statement in &program.statements {
            results.push(self.build_statement(statement));
        }

        let last = results
            .last()
            .cloned()
            .unwrap_or_else(|| BasicValueEnum::IntValue(self.llvm.i32_type().const_int(0, false)));

        last
    }
}
