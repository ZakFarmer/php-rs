use inkwell::values::BasicValueEnum;
use parser::ast::Literal;

use super::builder::RecursiveBuilder;

impl<'a> RecursiveBuilder<'a> {
    pub fn build_literal(&self, literal: &Literal) -> BasicValueEnum {
        match literal {
            Literal::Array(array) => todo!(),
            Literal::Boolean(value) => todo!(),
            Literal::Float(value) => todo!(),
            Literal::Integer(value) => self.build_integer_literal(value),
            Literal::String(value) => self.build_string_literal(value),
            _ => panic!("Unknown literal"),
        }
    }

    pub fn build_integer_literal(&self, value: &i64) -> BasicValueEnum {
        BasicValueEnum::IntValue(self.i32_type.const_int(*value as u64, false))
    }

    pub fn build_string_literal(&self, value: &String) -> BasicValueEnum {
        let string_value = value.as_str();

        let string = self
            .builder
            .build_global_string_ptr(string_value, value.as_str())
            .as_pointer_value();

        BasicValueEnum::PointerValue(string)
    }
}
