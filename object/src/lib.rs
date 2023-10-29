use std::rc::Rc;

use opcode::Instructions;
use parser::ast::{BlockStatement, Identifier};

use self::environment::Env;

pub mod environment;

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(String),
    Function(Vec<Identifier>, BlockStatement, Env),
    CompiledFunction(Rc<CompiledFunction>),
    Return(Rc<Object>),
    Array(Vec<Rc<Object>>),
    Null,
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Object::Integer(integer) => write!(f, "{}", integer),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
            Object::String(string) => write!(f, "{}", string),
            Object::Function(parameters, body, _env) => {
                let mut parameters_string = String::new();

                for (index, parameter) in parameters.iter().enumerate() {
                    parameters_string.push_str(&parameter.to_string());

                    if index < parameters.len() - 1 {
                        parameters_string.push_str(", ");
                    }
                }

                write!(f, "fn({}) {{\n{}\n}}", parameters_string, body)
            }
            Object::CompiledFunction(function) => write!(f, "{}", function),
            Object::Array(elements) => {
                let mut elements_string = String::new();

                for (index, element) in elements.iter().enumerate() {
                    elements_string.push_str(&element.to_string());

                    if index < elements.len() - 1 {
                        elements_string.push_str(", ");
                    }
                }

                write!(f, "[{}]", elements_string)
            }
            Object::Return(value) => write!(f, "{}", value),
            Object::Null => write!(f, "null"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct CompiledFunction {
    pub instructions: Instructions,
}

impl CompiledFunction {
    pub fn new(instructions: Instructions) -> Self {
        Self { instructions }
    }

    pub fn instructions(&self) -> &Instructions {
        &self.instructions
    }
}

impl std::fmt::Display for CompiledFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "function ({})", self.instructions)
    }
}
