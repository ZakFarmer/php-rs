use anyhow::Error;
use inkwell::values::{BasicValueEnum, PointerValue};
use llvm::Llvm;
use parser::ast::{Expression, Literal};
use token::TokenType;

use super::builder::RecursiveBuilder;

pub enum ExpressionValue<'ev> {
    LValue(PointerValue<'ev>),
    RValue(BasicValueEnum<'ev>),
}

impl<'ev> ExpressionValue<'ev> {
    pub fn as_basic_value_enum(&self) -> BasicValueEnum<'ev> {
        match self {
            ExpressionValue::LValue(pointer) => BasicValueEnum::PointerValue(*pointer),
            ExpressionValue::RValue(value) => *value,
        }
    }

    pub fn as_r_value(&self, llvm: &Llvm<'ev>, name: Option<&str>) -> BasicValueEnum<'ev> {
        match self {
            ExpressionValue::LValue(pointer) => llvm.load_pointer(pointer, name.as_deref().unwrap_or("")),
            ExpressionValue::RValue(value) => *value,
        }
    }
}

pub struct ExpressionBuilder<'a, 'b> {
    pub llvm: &'b Llvm<'a>,
}

impl<'ink, 'b> ExpressionBuilder<'ink, 'b> {
    pub fn new(llvm: &'b Llvm<'ink>) -> ExpressionBuilder<'ink, 'b> {
        ExpressionBuilder { llvm }
    }

    pub fn build_expression(&self, expression: &Expression) -> BasicValueEnum<'ink> {
        match expression {
            Expression::Call(_call_expression) => todo!(),
            Expression::Function(_function_literal) => todo!(),
            Expression::Identifier(ref _identifier) => todo!(),
            Expression::If(_if_expression) => todo!(),
            Expression::Index(_index_expression) => todo!(),
            Expression::Infix(infix_expression) => self.build_infix_expression(infix_expression),
            Expression::Literal(literal) => self.build_literal(literal),
            Expression::Prefix(prefix_expression) => {
                self.build_prefix_expression(prefix_expression)
            }
            _ => panic!("Unknown expression"),
        }
    }

    /// Build a literal
    pub fn build_literal(&self, literal: &Literal) -> BasicValueEnum<'ink> {
        match literal {
            Literal::Array(_array) => todo!(),
            Literal::Boolean(value) => self.build_boolean_literal(value),
            Literal::Float(_value) => todo!(),
            Literal::Integer(value) => self.build_integer_literal(value),
            Literal::String(value) => self.build_string_literal(value),
            _ => panic!("Unknown literal"),
        }
    }

    /// Build a boolean literal
    pub fn build_boolean_literal(&self, value: &bool) -> BasicValueEnum<'ink> {
        BasicValueEnum::IntValue(self.llvm.bool_type().const_int(*value as u64, false))
    }

    /// Build an integer literal
    pub fn build_integer_literal(&self, value: &i64) -> BasicValueEnum<'ink> {
        BasicValueEnum::IntValue(self.llvm.i32_type().const_int(*value as u64, false))
    }

    /// Build a string literal
    pub fn build_string_literal(&self, value: &String) -> BasicValueEnum<'ink> {
        let string_value = value.as_str();

        let string = self
            .llvm
            .builder
            .build_global_string_ptr(string_value, value.as_str())
            .as_pointer_value();

        BasicValueEnum::PointerValue(string)
    }

    /// Build a prefix expression
    pub fn build_prefix_expression(
        &self,
        prefix_expression: &parser::ast::PrefixExpression,
    ) -> BasicValueEnum<'ink> {
        let right = self.build_expression(&prefix_expression.right);

        let built = match prefix_expression.operator.token_type {
            TokenType::Bang => self.llvm.builder.build_int_compare(
                inkwell::IntPredicate::EQ,
                right.into_int_value(),
                self.llvm.i32_type().const_int(0, false),
                "not",
            ),
            TokenType::Minus => self.llvm.builder.build_int_neg(right.into_int_value(), "neg"),
            _ => panic!("Unknown operator"),
        };

        BasicValueEnum::IntValue(built)
    }

    /// Build an infix expression
    pub fn build_infix_expression<'a>(
        &self,
        infix_expression: &parser::ast::InfixExpression,
    ) -> BasicValueEnum<'ink> {
        let left_value = self.build_expression(&infix_expression.left);
        let right_value = self.build_expression(&infix_expression.right);

        let left = match left_value {
            BasicValueEnum::IntValue(val) => val,
            _ => panic!("Expected IntValue for left operand"),
        };

        let right = match right_value {
            BasicValueEnum::IntValue(val) => val,
            _ => panic!("Expected IntValue for right operand"),
        };

        let result = match infix_expression.operator.token_type {
            TokenType::Plus => self.llvm.builder.build_int_add(left, right, "add"),
            TokenType::Minus => self.llvm.builder.build_int_sub(left, right, "sub"),
            TokenType::Asterisk => self.llvm.builder.build_int_mul(left, right, "mul"),
            TokenType::Slash => self.llvm.builder.build_int_unsigned_div(left, right, "div"),
            TokenType::Lt => {
                self.llvm.builder
                    .build_int_compare(inkwell::IntPredicate::SLT, left, right, "less_than")
            }
            TokenType::Gt => self.llvm.builder.build_int_compare(
                inkwell::IntPredicate::SGT,
                left,
                right,
                "greater_than",
            ),
            TokenType::Eq => {
                self.llvm.builder
                    .build_int_compare(inkwell::IntPredicate::EQ, left, right, "equal_to")
            }
            TokenType::NotEq => self.llvm.builder.build_int_compare(
                inkwell::IntPredicate::NE,
                left,
                right,
                "not_equal_to",
            ),
            _ => panic!("Unknown operator"),
        };

        BasicValueEnum::IntValue(result)
    }

    /// Build an infix expression
    fn build_add_infix_expression<'ctx>(
        &'ctx self,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        match left {
            BasicValueEnum::IntValue(left) => match right {
                BasicValueEnum::IntValue(right) => {
                    BasicValueEnum::IntValue(self.llvm.builder.build_int_add(left, right, "add"))
                }
                _ => panic!("Expected IntValue for right operand"),
            },
            _ => panic!("Expected IntValue for left operand"),
        }
    }
}