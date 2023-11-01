use anyhow::Error;
use inkwell::{context::Context, execution_engine::{JitFunction, ExecutionEngine}, OptimizationLevel, builder::Builder, module::Module};
use parser::ast::Node;

use crate::codegen::builder::RecursiveBuilder;

type JitFn = unsafe extern "C" fn() -> i32;

pub struct Jit<'ctx> {
    builder: Builder<'ctx>,
    context: &'ctx Context,
    execution_engine: ExecutionEngine<'ctx>,
    module: Module<'ctx>,
}

impl<'ctx> Jit<'ctx> {
    pub fn new(context: &'ctx Context, module: Module<'ctx>, execution_engine: ExecutionEngine<'ctx>, builder: Builder<'ctx>) -> Self {
        Self {
            builder,
            context,
            execution_engine,
            module,
        }
    }
}

#[repr(C)]
#[derive(Debug, PartialEq)]
pub enum JitValue {
    Bool(bool),
    Int(i32),
    Float(f32),
    Str(String),
}

impl JitValue {
    pub fn as_int(&self) -> Option<i32> {
        match self {
            JitValue::Int(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_float(&self) -> Option<f32> {
        match self {
            JitValue::Float(f) => Some(*f),
            _ => None,
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            JitValue::Str(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_string(&self) -> Option<String> {
        match self {
            JitValue::Str(s) => Some(s.clone()),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            JitValue::Int(i) => Some(*i != 0),
            _ => None,
        }
    }
}

impl<'ctx> Jit<'ctx> {
    pub fn compile(&self, ast: &Node) -> Result<i32, Error> {
        let i32_type = self.context.i32_type();

        let main_function_type = i32_type.fn_type(&[], false);
        let main_function = self.module.add_function("main", main_function_type, None);

        let basic_block = self.context.append_basic_block(main_function, "entry");

        self.builder.position_at_end(basic_block);

        // Build the program
        let recursive_builder = RecursiveBuilder::new(i32_type, &self.builder);
        let return_value = recursive_builder.build(ast);

        _ = self.builder.build_return(Some(&return_value));

        println!("IR: {}", self.module.print_to_string().to_string());

        unsafe {
            let jit_function: JitFunction<'_, JitFn> = self.execution_engine.get_function("main").unwrap();
            // dbg!(&jit_function);
        
            Ok(jit_function.call())
        }
    }
}
