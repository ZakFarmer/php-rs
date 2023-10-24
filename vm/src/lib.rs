use std::{borrow::Borrow, rc::Rc};

use anyhow::Error;
use byteorder::{BigEndian, ByteOrder};
use compiler::Bytecode;
use object::Object;
use opcode::Opcode;

const GLOBALS_SIZE: usize = 65536;
const STACK_SIZE: usize = 2048;

pub struct Vm {
    constants: Vec<Rc<Object>>,
    instructions: opcode::Instructions,

    globals: Vec<Rc<Object>>,

    stack: Vec<Rc<Object>>,
    stack_pointer: usize,
}

impl Vm {
    pub fn new(bytecode: Bytecode) -> Self {
        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,

            globals: vec![Rc::new(Object::Null); GLOBALS_SIZE],

            stack: vec![Rc::new(Object::Null); STACK_SIZE],
            stack_pointer: 0,
        }
    }

    pub fn run(&mut self) -> Result<(), Error> {
        let mut ip = 0;

        while ip < self.instructions.0.len() {
            let op = Opcode::from(self.instructions.0[ip]);
            ip += 1;

            match op {
                Opcode::OpJump => {
                    let jump_position =
                        BigEndian::read_u16(&self.instructions.0[ip..ip + 2]) as usize;

                    ip = jump_position;
                }
                Opcode::OpJumpNotTruthy => {
                    let jump_position =
                        BigEndian::read_u16(&self.instructions.0[ip..ip + 2]) as usize;

                    ip += 2;

                    let condition = self.pop();

                    if !is_truthy(&condition) {
                        ip = jump_position;
                    }
                }
                Opcode::OpGetGlobal => {
                    let global_index =
                        BigEndian::read_u16(&self.instructions.0[ip..ip + 2]) as usize;

                    ip += 2;

                    self.push(Rc::clone(&self.globals[global_index]));
                }
                Opcode::OpSetGlobal => {
                    let global_index =
                        BigEndian::read_u16(&self.instructions.0[ip..ip + 2]) as usize;

                    ip += 2;

                    self.globals[global_index] = self.pop();
                }
                Opcode::OpNull => {
                    self.push(Rc::new(Object::Null));
                }
                Opcode::OpConst => {
                    let const_index =
                        BigEndian::read_u16(&self.instructions.0[ip..ip + 2]) as usize;
                    ip += 2;

                    self.push(Rc::clone(&self.constants[const_index]));
                }
                Opcode::OpAdd => {
                    let right = self.pop();
                    let left = self.pop();

                    let result = match (&*left, &*right) {
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

fn is_truthy(object: &Object) -> bool {
    match object {
        Object::Boolean(boolean) => *boolean,
        Object::Integer(integer) => *integer != 0,
        _ => true,
    }
}