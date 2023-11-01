use std::{collections::HashMap, cell::RefCell};

use inkwell::{
    builder::Builder,
    types::IntType,
    values::{BasicValueEnum, PointerValue},
};
use parser::ast::{Identifier, Node, Program};

pub struct RecursiveBuilder<'a> {
    pub builder: &'a Builder<'a>,
    pub i32_type: IntType<'a>,

    pub builtins: RefCell<HashMap<String, PointerValue<'a>>>,
    pub variables: RefCell<HashMap<String, PointerValue<'a>>>,
}

impl<'a> RecursiveBuilder<'a> {
    pub fn new(i32_type: IntType<'a>, builder: &'a Builder<'_>) -> Self {
        Self {
            builder,
            i32_type,
            builtins: RefCell::new(HashMap::new()),
            variables: RefCell::new(HashMap::new()),
        }
    }

    pub fn build(&self, ast: &Node) -> BasicValueEnum<'_> {
        match ast {
            Node::Program(n) => self.build_program(n),
            Node::Statement(n) => self.build_statement(n),
            Node::Expression(n) => self.build_expression(n),
            _ => panic!("Unknown node"),
        }
    }

    fn build_program(&self, program: &Program) -> BasicValueEnum<'_> {
        let mut results: Vec<BasicValueEnum<'_>> = vec![];
    
        for statement in &program.statements {
            results.push(self.build_statement(statement));
        }
    
        let last = results.last().cloned().unwrap_or_else(|| BasicValueEnum::IntValue(self.i32_type.const_int(0, false)));
    
        last
    }
    
    
}
