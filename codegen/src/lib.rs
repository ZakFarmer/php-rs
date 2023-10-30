use std::collections::HashMap;

use anyhow::Error;
use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    passes::PassManager,
    values::{FunctionValue, PointerValue},
};
use parser::ast::{Expression, FunctionLiteral, Literal, Node, Program, Statement};

pub struct Codegen<'a, 'ctx> {
    pub builder: &'a Builder<'ctx>,
    pub context: &'ctx Context,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: Module<'ctx>,

    pub builtins: HashMap<String, FunctionValue<'ctx>>,
    pub variables: HashMap<String, PointerValue<'ctx>>,
}

impl<'a, 'ctx> Codegen<'a, 'ctx> {
    fn generate_ir(&mut self, program: Program) {
        for statement in program.statements {
            match self.compile_statement(statement) {
                Err(x) => {
                    dbg!(x);
                }
                _ => {}
            }
        }

        self.builder
            .build_return(
                Some(
                    &self.context
                    .i64_type()
                    .const_int(0, false)
                ),
            );
    }

    fn compile_expression(&mut self, expression: Expression) -> Result<(), Error> {
        match expression {
            Expression::Literal(Literal::Integer(value)) => {
                let i64_type = self.context.i64_type();
                let i64_value = i64_type.const_int(value as u64, false);

                self.builder.build_return(Some(&i64_value));
            }
            _ => {}
        }

        Ok(())
    }

    fn compile_statement(&mut self, statement: Statement) -> Result<(), Error> {
        match statement {
            Statement::Expr(expression) => {
                self.compile_expression(expression)?;
            }
            _ => {}
        }

        Ok(())
    }

    pub fn compile(
        builder: &'a Builder<'ctx>,
        context: &'ctx Context,
        fpm: &'a PassManager<FunctionValue<'ctx>>,
        module: Module<'ctx>,
        node: &Node,
    ) -> Result<(), Error> {
        let mut codegen = Codegen {
            builder,
            context,
            fpm,
            module,

            builtins: HashMap::new(),
            variables: HashMap::new(),
        };

        match node {
            Node::Program(program) => {
                for statement in &program.statements {
                    codegen.compile_statement(statement.clone())?;
                }
            }
            _ => {}
        }

        Ok(())
    }
}
