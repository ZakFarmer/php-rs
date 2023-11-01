use inkwell::values::BasicValueEnum;
use parser::ast::Literal;

use super::builder::RecursiveBuilder;

impl<'a> RecursiveBuilder<'a> {
    pub fn build_literal(&self, literal: &Literal) -> BasicValueEnum<'_> {
        match literal {
            Literal::Array(_array) => todo!(),
            Literal::Boolean(_value) => todo!(),
            Literal::Float(_value) => todo!(),
            Literal::Integer(value) => self.build_integer_literal(value),
            Literal::String(value) => self.build_string_literal(value),
            _ => panic!("Unknown literal"),
        }
    }

    pub fn build_integer_literal(&self, value: &i64) -> BasicValueEnum<'_> {
        BasicValueEnum::IntValue(self.i32_type.const_int(*value as u64, false))
    }

    pub fn build_string_literal(&self, value: &String) -> BasicValueEnum<'_> {
        let string_value = value.as_str();

        let string = self
            .builder
            .build_global_string_ptr(string_value, value.as_str())
            .as_pointer_value();

        BasicValueEnum::PointerValue(string)
    }
}
