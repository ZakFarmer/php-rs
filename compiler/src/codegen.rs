use std::collections::HashMap;

use anyhow::Error;
use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    passes::PassManager,
    types::IntType,
    values::{AnyValueEnum, BasicValue, FunctionValue, InstructionValue, IntValue, PointerValue},
    OptimizationLevel,
};
use parser::ast::{Expression, FunctionLiteral, Literal, Node, Program, Statement};

pub struct RecursiveBuilder<'a> {
    pub builder: &'a Builder<'a>,
    i32_type: IntType<'a>,
}

impl<'a> RecursiveBuilder<'a> {
    pub fn new(i32_type: IntType<'a>, builder: &'a Builder) -> Self {
        Self { i32_type, builder }
    }

    pub fn build(&self, ast: &Node) -> IntValue {
        match ast {
            Node::Program(n) => self.build_program(n),
            Node::Statement(n) => self.build_statement(n),
            Node::Expression(n) => self.build_expression(n),
            _ => panic!("Unknown node"),
        }
    }

    fn build_program(&self, program: &Program) -> IntValue {
        let mut last = self.i32_type.const_int(0, false);

        for statement in &program.statements {
            last = self.build_statement(statement);
        }

        last
    }

    fn build_statement(&self, statement: &Statement) -> IntValue {
        match statement {
            Statement::Expr(expression) => self.build_expression(expression),
            _ => panic!("Unknown statement"),
        }
    }

    fn build_expression(&self, expression: &Expression) -> IntValue {
        match expression {
            Expression::Literal(literal) => self.build_literal(literal),
            _ => panic!("Unknown expression"),
        }
    }

    fn build_literal(&self, literal: &Literal) -> IntValue {
        match literal {
            Literal::Integer(value) => self.i32_type.const_int(*value as u64, false),
            _ => panic!("Unknown literal"),
        }
    }
}
