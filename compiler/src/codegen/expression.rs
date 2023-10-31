use inkwell::values::BasicValueEnum;
use parser::ast::Expression;
use token::TokenType;

use super::builder::RecursiveBuilder;

impl <'a> RecursiveBuilder <'a> {
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

    pub fn build_prefix_expression(
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