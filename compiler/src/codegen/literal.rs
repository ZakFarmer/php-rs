use inkwell::values::BasicValueEnum;

use super::builder::RecursiveBuilder;

impl<'a> RecursiveBuilder<'a> {
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
