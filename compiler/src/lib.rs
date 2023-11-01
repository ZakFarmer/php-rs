#![deny(elided_lifetimes_in_paths)]

use anyhow::Error;
use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::{self, ExecutionEngine},
    module::Module,
    OptimizationLevel,
};
use jit::JitValue;

pub mod codegen;
pub mod jit;
pub mod symbol_table;

pub struct Compiler<'ctx> {
    jit: jit::Jit<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context) -> Compiler<'ctx> {
        let builder = context.create_builder();
        let module = context.create_module("main");
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        let jit = jit::Jit::new(&context, module, execution_engine, builder);

        Compiler { jit }
    }

    pub fn compile(&mut self, node: &parser::ast::Node) -> Result<i32, Error> {
        self.jit.compile(node)
    }
}
