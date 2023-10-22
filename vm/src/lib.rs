use std::{rc::Rc, borrow::Borrow};

use anyhow::Error;
use byteorder::{BigEndian, ByteOrder};
use compiler::Bytecode;
use object::Object;
use opcode::Opcode;

const STACK_SIZE: usize = 2048;

pub struct Vm {
    constants: Vec<Rc<Object>>,
    instructions: opcode::Instructions,

    stack: Vec<Rc<Object>>,
    stack_pointer: usize
}

impl Vm {
    pub fn new(bytecode: Bytecode) -> Self {
        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,

            stack: vec![Rc::new(Object::Null); STACK_SIZE],
            stack_pointer: 0
        }
    }

    pub fn run(&mut self) -> Result<(), Error> {
        let mut ip = 0;

        while ip < self.instructions.0.len() {
            let op = Opcode::from(self.instructions.0[ip]);
            ip += 1;

            match op {
                Opcode::OpConst => {
                    let const_index = BigEndian::read_u16(&self.instructions.0[ip..ip + 2]) as usize;
                    ip += 2;

                    self.push(Rc::clone(&self.constants[const_index]));
                }
                Opcode::OpAdd => {
                    let right = self.pop();
                    let left = self.pop();

                    let result = match (&* left, &* right) {
                        (Object::Integer(l), Object::Integer(r)) => Object::Integer(l + r),
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
                Opcode::OpPop => {
                    self.pop();
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

    pub fn push(&mut self, obj: Rc<Object>) {
        self.stack[self.stack_pointer] = obj;
        self.stack_pointer += 1;
    }

    pub fn stack_top(&self) -> Rc<Object> {
        Rc::clone(&self.stack[self.stack_pointer - 1])
    }
}