use inkwell::values::BasicValueEnum;
use token::TokenType;

use super::builder::RecursiveBuilder;

impl<'a> RecursiveBuilder<'a> {
    pub fn build_infix_expression(
        &self,
        infix_expression: &parser::ast::InfixExpression,
    ) -> BasicValueEnum<'_> {
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
            TokenType::Plus => self.builder.build_int_add(left, right, "add"),
            TokenType::Minus => self.builder.build_int_sub(left, right, "sub"),
            TokenType::Asterisk => self.builder.build_int_mul(left, right, "mul"),
            TokenType::Slash => self.builder.build_int_unsigned_div(left, right, "div"),
            TokenType::Lt => {
                self.builder
                    .build_int_compare(inkwell::IntPredicate::SLT, left, right, "less_than")
            }
            TokenType::Gt => self.builder.build_int_compare(
                inkwell::IntPredicate::SGT,
                left,
                right,
                "greater_than",
            ),
            TokenType::Eq => {
                self.builder
                    .build_int_compare(inkwell::IntPredicate::EQ, left, right, "equal_to")
            }
            TokenType::NotEq => self.builder.build_int_compare(
                inkwell::IntPredicate::NE,
                left,
                right,
                "not_equal_to",
            ),
            _ => panic!("Unknown operator"),
        };

        BasicValueEnum::IntValue(result)
    }

    fn build_add_infix_expression<'ctx>(
        &'ctx self,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        match left {
            BasicValueEnum::IntValue(left) => match right {
                BasicValueEnum::IntValue(right) => {
                    BasicValueEnum::IntValue(self.builder.build_int_add(left, right, "add"))
                }
                _ => panic!("Expected IntValue for right operand"),
            },
            _ => panic!("Expected IntValue for left operand"),
        }
    }
}
