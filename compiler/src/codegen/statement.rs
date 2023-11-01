use inkwell::values::BasicValueEnum;
use parser::ast::{Assignment, Identifier, Statement};

use super::builder::RecursiveBuilder;

impl<'a> RecursiveBuilder<'a> {
    pub fn build_assignment(&self, assignment: &Assignment) -> BasicValueEnum<'_> {
        // Compute without mutating self
        let (token_value, expression_value) = {
            let token_value = match &assignment.name {
                Identifier { token } => token.value.clone(),
                _ => panic!("Unknown identifier"),
            };

            let expression_value = self.build_expression(&assignment.value);

            (token_value, expression_value)
        };

        // Mutate self
        let name = token_value.as_str();
        let alloca = self.builder.build_alloca(self.i32_type, name);
        self.builder.build_store(alloca, expression_value);

        // Store a pointer to the value
        self.variables
            .borrow_mut()
            .insert(token_value, alloca);

        expression_value
    }

    pub fn build_statement(&self, statement: &Statement) -> BasicValueEnum<'_> {
        match statement {
            Statement::Assign(assign_statement) => self.build_assignment(assign_statement),
            Statement::Expr(expression) => self.build_expression(expression),
            Statement::Return(_return_statement) => todo!(),
            _ => panic!("Unknown statement"),
        }
    }
}
