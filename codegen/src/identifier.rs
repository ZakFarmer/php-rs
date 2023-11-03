use inkwell::values::BasicValueEnum;
use parser::ast::Identifier;

use super::builder::RecursiveBuilder;

impl<'ink, 'b> RecursiveBuilder<'ink, 'b> {
    /// Build an identifier
    pub fn build_identifier(&self, identifier: &Identifier) -> BasicValueEnum<'_> {
        let built = match self.variables.borrow().get(&identifier.token.value) {
            Some(value) => self
                .llvm
                .builder
                .build_load(
                    self.llvm.i32_type(),
                    *value,
                    identifier.token.value.as_str(),
                )
                .into_int_value(),
            None => panic!("Unknown identifier"),
        };

        BasicValueEnum::IntValue(built)
    }
}
