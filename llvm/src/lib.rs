use anyhow::Error;
use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
    module::Module, values::{PointerValue, BasicValueEnum, GlobalValue}, types::BasicTypeEnum,
};

type MainFn = unsafe extern "C" fn() -> i32;

pub trait GlobalValueExt { 
    fn make_constant(self) -> Self;
    fn make_external(self) -> Self;
    fn make_private(self) -> Self;
}

impl<'ink> GlobalValueExt for GlobalValue<'ink> {
    fn make_constant(self) -> Self {
        self.set_constant(true);
        self.set_unnamed_addr(true);

        self
    }

    fn make_external(self) -> Self {
        self.set_linkage(inkwell::module::Linkage::External);
        self.set_unnamed_addr(true);

        self
    }

    fn make_private(self) -> Self {
        self.set_linkage(inkwell::module::Linkage::Private);
        self.set_unnamed_addr(true);

        self
    }
}

/// Wrapper around LLVM dependencies - used to codegen LLVM IR
pub struct Llvm<'ctx> {
    pub builder: Builder<'ctx>,
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
}

impl<'ctx> Llvm<'ctx> {
    /// LLVM constructor
    pub fn new(
        builder: Builder<'ctx>,
        context: &'ctx Context,
        module: Module<'ctx>,
    ) -> Llvm<'ctx> {
        Llvm {
            builder,
            context,
            module,
        }
    }

    pub fn create_bool_constant(&self, value: bool) -> Result<BasicValueEnum<'ctx>, Error> {
        let data_type = self.context.bool_type();

        let value = if value {
            data_type.const_all_ones()
        } else {
            data_type.const_zero()
        };

        Ok(BasicValueEnum::IntValue(value))
    }

    pub fn create_numeric_constant(&self, value: i64) -> Result<BasicValueEnum<'ctx>, Error> {
        let data_type = self.context.i32_type();

        let value = data_type.const_int(value as u64, false);

        Ok(BasicValueEnum::IntValue(value))
    }

    pub fn create_global_variable(
        &self,
        module: &Module<'ctx>,
        name: &str,
        value: &BasicValueEnum<'ctx>,
    ) -> GlobalValue<'ctx> {
        let global = module.add_global(value.get_type(), None, name);
        global.set_thread_local_mode(None);

        global
    }

    pub fn create_local_variable(
        &self,
        name: &str,
        data_type: &BasicTypeEnum<'ctx>,
        value: &BasicValueEnum<'ctx>,
    ) -> PointerValue<'ctx> {
        self.builder.build_alloca(*data_type, name)
    }

    pub fn load_pointer(&self, pointer: &PointerValue<'ctx>, name: &str) -> BasicValueEnum<'ctx> {
        self.builder.build_load(self.i32_type(), *pointer, name)
    }

    pub fn bool_type(&self) -> inkwell::types::IntType<'ctx> {
        self.context.bool_type()
    }

    pub fn i32_type(&self) -> inkwell::types::IntType<'ctx> {
        self.context.i32_type()
    }
}
