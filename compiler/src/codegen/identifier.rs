use inkwell::values::BasicValueEnum;
use parser::ast::Identifier;

use super::builder::RecursiveBuilder;

impl <'a> RecursiveBuilder <'a> {
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