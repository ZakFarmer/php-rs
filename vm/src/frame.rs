use object::CompiledFunction;
use opcode::Instructions;

#[derive(Clone, Debug, PartialEq)]
pub struct Frame {
    pub function: CompiledFunction,
    pub instruction_pointer: i32,
}

impl Frame {
    pub fn new(function: CompiledFunction) -> Self {
        Self {
            function,
            instruction_pointer: -1,
        }
    }

    pub fn instructions(&self) -> Instructions {
        self.function.instructions.clone()
    }
}