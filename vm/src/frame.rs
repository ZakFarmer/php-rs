use object::CompiledFunction;
use opcode::Instructions;

#[derive(Clone, Debug, PartialEq)]
pub struct Frame {
    pub function: CompiledFunction,

    pub base_pointer: usize,
    pub instruction_pointer: i32,
}

impl Frame {
    pub fn new(function: CompiledFunction, base_pointer: usize) -> Self {
        Self {
            function,
            base_pointer,
            instruction_pointer: -1,
        }
    }

    pub fn instructions(&self) -> Instructions {
        self.function.instructions.clone()
    }
}
