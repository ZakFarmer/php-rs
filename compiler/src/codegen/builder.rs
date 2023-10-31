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

    fn build_statement(&self, statement: &Statement) -> BasicValueEnum {
        match statement {
            Statement::Assign(assign_statement) => self.build_assignment(assign_statement),
            Statement::Expr(expression) => self.build_expression(expression),
            Statement::Return(return_statement) => todo!(),
            _ => panic!("Unknown statement"),
        }
    }

    fn build_assignment(&self, assignment: &Assignment) -> BasicValueEnum {
        let value = self.build_expression(&assignment.value);

        match &assignment.name {
            Identifier { token } => {
                let name = token.value.as_str();

                let alloca = self.builder.build_alloca(self.i32_type, name);

                self.builder.build_store(alloca, value);

                value
            }
            _ => panic!("Unknown identifier"),
        }
    }

    pub fn build_expression(&self, expression: &Expression) -> BasicValueEnum {
        match expression {
            Expression::Call(call_expression) => todo!(),
            Expression::Function(function_literal) => todo!(),
            Expression::Identifier(ref identifier) => self.build_identifier(identifier),
            Expression::If(if_expression) => todo!(),
            Expression::Index(index_expression) => todo!(),
            Expression::Infix(infix_expression) => self.build_infix_expression(infix_expression),
            Expression::Literal(literal) => self.build_literal(literal),
            Expression::Prefix(prefix_expression) => {
                self.build_prefix_expression(prefix_expression)
            }
            _ => panic!("Unknown expression"),
        }
    }

    fn build_identifier(&self, identifier: &Identifier) -> BasicValueEnum {
        let built = match self.variables.get(&identifier.token.value) {
            Some(value) => self
                .builder
                .build_load(self.i32_type, *value, identifier.token.value.as_str())
                .into_int_value(),
            None => panic!("Unknown identifier"),
        };

        BasicValueEnum::IntValue(built)
    }

    fn build_literal(&self, literal: &Literal) -> BasicValueEnum {
        match literal {
            Literal::Array(array) => todo!(),
            Literal::Boolean(value) => todo!(),
            Literal::Float(value) => todo!(),
            Literal::Integer(value) => self.build_integer_literal(value),
            Literal::String(value) => self.build_string_literal(value),
            _ => panic!("Unknown literal"),
        }
    }

    fn build_prefix_expression(
        &self,
        prefix_expression: &parser::ast::PrefixExpression,
    ) -> BasicValueEnum {
        let right = self.build_expression(&prefix_expression.right);

        let built = match prefix_expression.operator.token_type {
            TokenType::Bang => self.builder.build_int_compare(
                inkwell::IntPredicate::EQ,
                right.into_int_value(),
                self.i32_type.const_int(0, false),
                "not",
            ),
            TokenType::Minus => self.builder.build_int_neg(right.into_int_value(), "neg"),
            _ => panic!("Unknown operator"),
        };

        BasicValueEnum::IntValue(built)
    }
}
