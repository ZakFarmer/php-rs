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
use token::TokenType;

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
            Statement::Assign(assign_statement) => todo!(),
            Statement::Expr(expression) => self.build_expression(expression),
            Statement::Return(return_statement) => todo!(),
            _ => panic!("Unknown statement"),
        }
    }

    fn build_expression(&self, expression: &Expression) -> IntValue {
        match expression {
            Expression::Call(call_expression) => todo!(),
            Expression::Function(function_literal) => todo!(),
            Expression::Identifier(identifier) => todo!(),
            Expression::If(if_expression) => todo!(),
            Expression::Index(index_expression) => todo!(),
            Expression::Infix(infix_expression) => self.build_infix_expression(infix_expression),
            Expression::Literal(literal) => self.build_literal(literal),
            Expression::Prefix(prefix_expression) => todo!(),
            _ => panic!("Unknown expression"),
        }
    }

    fn build_literal(&self, literal: &Literal) -> IntValue {
        match literal {
            Literal::Array(array) => todo!(),
            Literal::Boolean(value) => todo!(),
            Literal::Float(value) => todo!(),
            Literal::Integer(value) => self.i32_type.const_int(*value as u64, false),
            Literal::String(value) => todo!(),
            _ => panic!("Unknown literal"),
        }
    }

    fn build_infix_expression(&self, infix_expression: &parser::ast::InfixExpression) -> IntValue {
        let left = self.build_expression(&infix_expression.left);
        let right = self.build_expression(&infix_expression.right);

        match infix_expression.operator.token_type {
            TokenType::Plus => self.builder.build_int_add(left, right, "add"),
            TokenType::Minus => self.builder.build_int_sub(left, right, "sub"),
            TokenType::Asterisk => self.builder.build_int_mul(left, right, "mul"),
            TokenType::Slash => self.builder.build_int_unsigned_div(left, right, "div"),
            TokenType::Lt => self.builder.build_int_compare(
                inkwell::IntPredicate::SLT,
                left,
                right,
                "less_than",
            ),
            TokenType::Gt => self.builder.build_int_compare(
                inkwell::IntPredicate::SGT,
                left,
                right,
                "greater_than",
            ),
            TokenType::Eq => self.builder.build_int_compare(
                inkwell::IntPredicate::EQ,
                left,
                right,
                "equal_to",
            ),
            TokenType::NotEq => self.builder.build_int_compare(
                inkwell::IntPredicate::NE,
                left,
                right,
                "not_equal_to",
            ),
            _ => panic!("Unknown operator"),
        }
    }
}
