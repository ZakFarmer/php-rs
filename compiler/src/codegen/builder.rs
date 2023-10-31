use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    types::IntType,
    values::{BasicValueEnum, PointerValue},
};
use parser::ast::{Identifier, Node, Program};

pub struct RecursiveBuilder<'a> {
    pub builder: &'a Builder<'a>,
    pub i32_type: IntType<'a>,

    pub builtins: HashMap<String, PointerValue<'a>>,
    pub variables: HashMap<String, PointerValue<'a>>,
}

impl<'a> RecursiveBuilder<'a> {
    pub fn new(i32_type: IntType<'a>, builder: &'a Builder) -> Self {
        Self {
            builder,
            i32_type,
            builtins: HashMap::new(),
            variables: HashMap::new(),
        }
    }

    pub fn build(&self, ast: &Node) -> BasicValueEnum {
        match ast {
            Node::Program(n) => self.build_program(n),
            Node::Statement(n) => self.build_statement(n),
            Node::Expression(n) => self.build_expression(n),
            _ => panic!("Unknown node"),
        }
    }

    fn build_program(&self, program: &Program) -> BasicValueEnum {
        let mut last: BasicValueEnum = BasicValueEnum::IntValue(self.i32_type.const_int(0, false));

        for statement in &program.statements {
            last = self.build_statement(statement);
        }

        last
    }
}
