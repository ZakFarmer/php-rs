use inkwell::values::BasicValueEnum;
use parser::ast::{Identifier, Assignment, Statement};

use super::builder::RecursiveBuilder;

impl <'a> RecursiveBuilder <'a> {
    pub fn build_assignment(&self, assignment: &Assignment) -> BasicValueEnum {
        let value = self.build_expression(&assignment.value);

        match &assignment.name {
            Identifier { token } => {
                let name = token.value.as_str();

                let alloca = self.builder.build_alloca(self.i32_type, name);

                self.builder.build_store(alloca, value);

                value
            }
            _ => panic!("Unknown identifier"),
        }
    }

    pub fn build_statement(&self, statement: &Statement) -> BasicValueEnum {
        match statement {
            Statement::Assign(assign_statement) => self.build_assignment(assign_statement),
            Statement::Expr(expression) => self.build_expression(expression),
            Statement::Return(return_statement) => todo!(),
            _ => panic!("Unknown statement"),
        }
    }
}