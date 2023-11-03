use inkwell::values::BasicValueEnum;
use parser::ast::{Assignment, Identifier, Statement};

use super::{builder::RecursiveBuilder, expression::ExpressionBuilder};

impl<'ink, 'b> RecursiveBuilder<'ink, 'b> {
    /// Builds an assignment statement
    pub fn build_assignment(&self, assignment: &Assignment) -> BasicValueEnum<'ink> {
        // Compute without mutating self
        let (token_value, expression_value) = {
            let token_value = match &assignment.name {
                Identifier { token } => token.value.clone(),
                _ => panic!("Unknown identifier"),
            };

            let expression_builder = ExpressionBuilder::new(self.llvm);
            let expression_value = expression_builder.build_expression(&assignment.value);

            (token_value, expression_value)
        };

        // Mutate self
        let name = token_value.as_str();
        let alloca = self.llvm.builder.build_alloca(self.llvm.i32_type(), name);
        self.llvm.builder.build_store(alloca, expression_value);

        // Store a pointer to the value
        self.variables.borrow_mut().insert(token_value, alloca);

        expression_value
    }

    /// Builds a statement
    pub fn build_statement(&self, statement: &Statement) -> BasicValueEnum<'ink> {
        match statement {
            Statement::Assign(assign_statement) => self.build_assignment(assign_statement),
            Statement::Expr(expression) => {
                let expression_builder = ExpressionBuilder::new(self.llvm);
                expression_builder.build_expression(expression)
            }
            Statement::Return(_return_statement) => todo!(),
            _ => panic!("Unknown statement"),
        }
    }
}
