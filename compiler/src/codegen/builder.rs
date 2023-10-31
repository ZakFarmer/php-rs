use std::collections::HashMap;

use anyhow::Error;
use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    passes::PassManager,
    types::IntType,
    values::{
        AnyValueEnum, BasicValue, BasicValueEnum, FunctionValue, InstructionValue, IntValue,
        PointerValue,
    },
    OptimizationLevel,
};
use parser::ast::{
    Assignment, Expression, FunctionLiteral, Identifier, Literal, Node, Program, Statement,
};
use token::TokenType;

pub struct RecursiveBuilder<'a> {
    pub builder: &'a Builder<'a>,
    pub i32_type: IntType<'a>,

    builtins: HashMap<String, PointerValue<'a>>,
    variables: HashMap<String, PointerValue<'a>>,
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

    pub fn build_identifier(&self, identifier: &Identifier) -> BasicValueEnum {
        let built = match self.variables.get(&identifier.token.value) {
            Some(value) => self
                .builder
                .build_load(self.i32_type, *value, identifier.token.value.as_str())
                .into_int_value(),
            None => panic!("Unknown identifier"),
        };

        BasicValueEnum::IntValue(built)
    }
}
