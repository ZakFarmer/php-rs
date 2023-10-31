use anyhow::Error;
use inkwell::{context::Context, execution_engine::JitFunction, OptimizationLevel};
use parser::ast::Node;

use crate::codegen::builder::RecursiveBuilder;

type JitFn = unsafe extern "C" fn() -> i32;

pub struct Jit;

impl Jit {
    pub fn compile(ast: &Node) -> Result<i32, Error> {
        let context = Context::create();
        let module = context.create_module("tmp");
        let builder = context.create_builder();

        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        let i32_type = context.i32_type();

        let main_function_type = i32_type.fn_type(&[], false);
        let main_function = module.add_function("main", main_function_type, None);

        let basic_block = context.append_basic_block(main_function, "entry");

        builder.position_at_end(basic_block);

        // Build the program
        let recursive_builder = RecursiveBuilder::new(i32_type, &builder);
        let return_value = recursive_builder.build(ast);

        _ = builder.build_return(Some(&return_value));

        println!("IR: {}", module.print_to_string().to_string());

        unsafe {
            let jit_function: JitFunction<JitFn> = execution_engine.get_function("main").unwrap();

            Ok(jit_function.call())
        }
    }

    pub fn from_source(source: &str) -> Result<i32, Error> {
        let mut parser = parser::Parser::new(source);
        let program = parser.parse_program()?;

        Self::compile(&Node::Program(program))
    }
}
