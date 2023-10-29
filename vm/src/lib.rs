use std::{borrow::Borrow, rc::Rc};

use anyhow::Error;
use byteorder::{BigEndian, ByteOrder};
use compiler::Bytecode;
use object::{CompiledFunction, Object};
use opcode::{Instructions, Opcode};

mod frame;

pub const GLOBALS_SIZE: usize = 65536;
pub const STACK_SIZE: usize = 2048;

pub const MAX_FRAMES: usize = 1024;

pub struct Vm {
    constants: Vec<Rc<Object>>,

    pub globals: Vec<Rc<Object>>,

    frames: Vec<frame::Frame>,
    frame_index: usize,

    stack: Vec<Rc<Object>>,
    stack_pointer: usize,
}

impl Vm {
    pub fn current_frame(&mut self) -> &mut frame::Frame {
        &mut self.frames[self.frame_index - 1]
    }

    pub fn push_frame(&mut self, frame: frame::Frame) {
        self.frames[self.frame_index] = frame;
        self.frame_index += 1;
    }

    pub fn pop_frame(&mut self) -> frame::Frame {
        self.frame_index -= 1;
        self.frames[self.frame_index].clone()
    }

    pub fn globals(&self) -> &Vec<Rc<Object>> {
        &self.globals
    }

    fn call_function(&mut self, num_args: usize) {
        let function = &*self.stack[self.stack_pointer - 1 - num_args];

        match function {
            Object::CompiledFunction(compiled_function) => {
                let base_pointer = self.stack_pointer - num_args;
                let cloned_function = compiled_function.as_ref().clone();

                let frame = frame::Frame::new(
                    cloned_function,
                    base_pointer,
                );

                self.stack_pointer = base_pointer + compiled_function.num_locals as usize;
                self.push_frame(frame);
            }
            _ => {
                panic!("calling non-function object: {}", function);
            }
        }
    }

    pub fn new(bytecode: Bytecode) -> Self {
        let empty_frame = frame::Frame::new(CompiledFunction::new(Instructions(vec![]), 0), 0);

        let main_function = CompiledFunction::new(
            bytecode.instructions.clone(),
             0
        );

        let main_frame = frame::Frame::new(main_function, 0);

        let mut frames = vec![empty_frame; MAX_FRAMES];
        frames[0] = main_frame;

        Self {
            constants: bytecode.constants,

            globals: vec![Rc::new(Object::Null); GLOBALS_SIZE],

            frames,
            frame_index: 1,

            stack: vec![Rc::new(Object::Null); STACK_SIZE],
            stack_pointer: 0,
        }
    }

    pub fn new_with_globals_store(bytecode: Bytecode, globals: Vec<Rc<Object>>) -> Self {
        let mut compiler = Self::new(bytecode);

        compiler.globals = globals;

        compiler
    }

    pub fn run(&mut self) -> Result<(), Error> {
        let mut instruction_pointer: usize;
        let mut instructions: Vec<u8>;

        while self.current_frame().instruction_pointer
            < self.current_frame().instructions().0.len() as i32 - 1
        {
            self.current_frame().instruction_pointer += 1;

            instruction_pointer = self.current_frame().instruction_pointer as usize;
            instructions = self.current_frame().instructions().0.clone();

            let op = *instructions.get(instruction_pointer).ok_or_else(|| {
                Error::msg(format!(
                    "no instruction at index {} in function {:?}",
                    instruction_pointer,
                    self.current_frame().function
                ))
            })?;

            let opcode = Opcode::from(op);

            match opcode {
                Opcode::OpJump => {
                    let jump_position = BigEndian::read_u16(
                        &instructions[instruction_pointer + 1..instruction_pointer + 3],
                    ) as usize;

                    self.current_frame().instruction_pointer = jump_position as i32 - 1;
                }
                Opcode::OpJumpNotTruthy => {
                    let jump_position = BigEndian::read_u16(
                        &instructions[instruction_pointer + 1..instruction_pointer + 3],
                    ) as usize;

                    self.current_frame().instruction_pointer += 2;

                    let condition = self.pop();

                    if !is_truthy(&condition) {
                        self.current_frame().instruction_pointer = jump_position as i32 - 1;
                    }
                }
                Opcode::OpPop => {
                    self.pop();
                }
                Opcode::OpGetGlobal => {
                    let global_index = BigEndian::read_u16(
                        &instructions[instruction_pointer + 1..instruction_pointer + 3],
                    ) as usize;

                    self.current_frame().instruction_pointer += 2;

                    self.push(Rc::clone(&self.globals[global_index]));
                }
                Opcode::OpSetGlobal => {
                    let global_index = BigEndian::read_u16(
                        &instructions[instruction_pointer + 1..instruction_pointer + 3],
                    ) as usize;

                    self.current_frame().instruction_pointer += 2;

                    self.globals[global_index] = self.pop();
                }
                Opcode::OpGetLocal => {
                    let local_index = instructions[instruction_pointer + 1] as usize;

                    self.current_frame().instruction_pointer += 1;

                    let base_pointer = self.current_frame().base_pointer;

                    self.push(Rc::clone(&self.stack[base_pointer + local_index]));
                }
                Opcode::OpSetLocal => {
                    let local_index = instructions[instruction_pointer + 1] as usize;

                    self.current_frame().instruction_pointer += 1;

                    let base_pointer = self.current_frame().base_pointer;

                    self.stack[base_pointer + local_index] = self.pop();
                }
                Opcode::OpCall => {
                    let num_args = instructions[instruction_pointer + 1] as usize;

                    self.current_frame().instruction_pointer += 1;

                    self.call_function(num_args);
                }
                Opcode::OpReturn => {
                    let frame = self.pop_frame();

                    self.stack_pointer = frame.base_pointer - 1;

                    self.push(Rc::new(Object::Null));
                }
                Opcode::OpReturnValue => {
                    let return_value = self.pop();

                    let frame = self.pop_frame();

                    self.stack_pointer = frame.base_pointer - 1;

                    self.push(return_value);
                }
                Opcode::OpNull => {
                    self.push(Rc::new(Object::Null));
                }
                Opcode::OpConst => {
                    let const_index = BigEndian::read_u16(
                        &instructions[instruction_pointer + 1..instruction_pointer + 3],
                    ) as usize;

                    self.current_frame().instruction_pointer += 2;

                    self.push(Rc::clone(&self.constants[const_index]));
                }
                Opcode::OpAdd => {
                    let right = self.pop();
                    let left = self.pop();

                    let result = match (&*left, &*right) {
                        (Object::Integer(l), Object::Integer(r)) => Object::Integer(l + r),
                        (Object::String(l), Object::String(r)) => {
                            Object::String(format!("{}{}", l, r))
                        }
                        _ => {
                            return Err(Error::msg(format!(
                                "unsupported types for addition: {} + {}",
                                left, right
                            )));
                        }
                    };

                    self.push(Rc::new(result));
                }
                Opcode::OpDiv => {
                    let right = self.stack[self.stack_pointer - 1].borrow();
                    let left = self.stack[self.stack_pointer - 2].borrow();

                    let result = match (left, right) {
                        (Object::Integer(l), Object::Integer(r)) => Object::Integer(l / r),
                        _ => {
                            return Err(Error::msg(format!(
                                "unsupported types for division: {} / {}",
                                left, right
                            )));
                        }
                    };

                    self.stack_pointer -= 1;
                    self.stack[self.stack_pointer - 1] = Rc::new(result);
                }
                Opcode::OpMul => {
                    let right = self.stack[self.stack_pointer - 1].borrow();
                    let left = self.stack[self.stack_pointer - 2].borrow();

                    let result = match (left, right) {
                        (Object::Integer(l), Object::Integer(r)) => Object::Integer(l * r),
                        _ => {
                            return Err(Error::msg(format!(
                                "unsupported types for multiplication: {} * {}",
                                left, right
                            )));
                        }
                    };

                    self.stack_pointer -= 1;
                    self.stack[self.stack_pointer - 1] = Rc::new(result);
                }
                Opcode::OpSub => {
                    let right = self.stack[self.stack_pointer - 1].borrow();
                    let left = self.stack[self.stack_pointer - 2].borrow();

                    let result = match (left, right) {
                        (Object::Integer(l), Object::Integer(r)) => Object::Integer(l - r),
                        _ => {
                            return Err(Error::msg(format!(
                                "unsupported types for subtraction: {} - {}",
                                left, right
                            )));
                        }
                    };

                    self.stack_pointer -= 1;
                    self.stack[self.stack_pointer - 1] = Rc::new(result);
                }
                Opcode::OpTrue => {
                    self.push(Rc::new(Object::Boolean(true)));
                }
                Opcode::OpFalse => {
                    self.push(Rc::new(Object::Boolean(false)));
                }
                Opcode::OpEqual => {
                    let right = self.stack[self.stack_pointer - 1].borrow();
                    let left = self.stack[self.stack_pointer - 2].borrow();

                    let result = match (left, right) {
                        (Object::Integer(l), Object::Integer(r)) => Object::Boolean(l == r),
                        (Object::Boolean(l), Object::Boolean(r)) => Object::Boolean(l == r),
                        _ => {
                            return Err(Error::msg(format!(
                                "unsupported types for equality: {} == {}",
                                left, right
                            )));
                        }
                    };

                    self.stack_pointer -= 1;
                    self.stack[self.stack_pointer - 1] = Rc::new(result);
                }
                Opcode::OpNotEqual => {
                    let right = self.stack[self.stack_pointer - 1].borrow();
                    let left = self.stack[self.stack_pointer - 2].borrow();

                    let result = match (left, right) {
                        (Object::Integer(l), Object::Integer(r)) => Object::Boolean(l != r),
                        (Object::Boolean(l), Object::Boolean(r)) => Object::Boolean(l != r),
                        _ => {
                            return Err(Error::msg(format!(
                                "unsupported types for inequality: {} != {}",
                                left, right
                            )));
                        }
                    };

                    self.stack_pointer -= 1;
                    self.stack[self.stack_pointer - 1] = Rc::new(result);
                }
                Opcode::OpGreaterThan => {
                    let right = self.stack[self.stack_pointer - 1].borrow();
                    let left = self.stack[self.stack_pointer - 2].borrow();

                    let result = match (left, right) {
                        (Object::Integer(l), Object::Integer(r)) => Object::Boolean(l > r),
                        _ => {
                            return Err(Error::msg(format!(
                                "unsupported types for greater than: {} > {}",
                                left, right
                            )));
                        }
                    };

                    self.stack_pointer -= 1;
                    self.stack[self.stack_pointer - 1] = Rc::new(result);
                }
                Opcode::OpBang => {
                    let operand = self.pop();

                    let result = match &*operand {
                        Object::Boolean(boolean) => Object::Boolean(!boolean),
                        Object::Integer(integer) => Object::Boolean(!integer == 0),
                        Object::Null => Object::Boolean(true),
                        _ => {
                            return Err(Error::msg(format!(
                                "unsupported type for negation: !{}",
                                operand
                            )));
                        }
                    };

                    self.push(Rc::new(result));
                }
                Opcode::OpMinus => {
                    let operand = self.pop();

                    let result = match &*operand {
                        Object::Integer(integer) => Object::Integer(-integer),
                        _ => {
                            return Err(Error::msg(format!(
                                "unsupported type for negation: -{}",
                                operand
                            )));
                        }
                    };

                    self.push(Rc::new(result));
                }
                Opcode::OpArray => {
                    let num_elements = BigEndian::read_u16(
                        &instructions[instruction_pointer + 1..instruction_pointer + 3],
                    ) as usize;

                    self.current_frame().instruction_pointer += 2;

                    let mut elements = Vec::with_capacity(num_elements);

                    for _ in 0..num_elements {
                        elements.push(self.pop());
                    }

                    elements.reverse();

                    self.push(Rc::new(Object::Array(elements)));
                }
                Opcode::OpIndex => {
                    let index = self.pop();
                    let left = self.pop();

                    let result = match (&*left, &*index) {
                        (Object::Array(elements), Object::Integer(integer)) => {
                            let idx = *integer as usize;

                            if idx >= elements.len() {
                                return Err(Error::msg(format!(
                                    "index out of bounds: index={}, length={}",
                                    idx,
                                    elements.len()
                                )));
                            }

                            Rc::clone(&elements[idx])
                        }
                        _ => {
                            return Err(Error::msg(format!(
                                "unsupported types for index: {}[{}]",
                                left, index
                            )));
                        }
                    };

                    self.push(result);
                }
                _ => {
                    return Err(Error::msg(format!("unknown opcode: {}", op)));
                }
            }
        }

        Ok(())
    }

    pub fn last_popped_stack_elem(&self) -> Rc<Object> {
        Rc::clone(&self.stack[self.stack_pointer])
    }

    pub fn pop(&mut self) -> Rc<Object> {
        self.stack_pointer -= 1;
        Rc::clone(&self.stack[self.stack_pointer])
    }

    pub fn push(&mut self, object: Rc<Object>) {
        self.stack[self.stack_pointer] = object;
        self.stack_pointer += 1;
    }
}

fn is_truthy(object: &Object) -> bool {
    match object {
        Object::Boolean(boolean) => *boolean,
        Object::Integer(integer) => *integer != 0,
        _ => true,
    }
}
