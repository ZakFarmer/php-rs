use std::rc::Rc;

use crate::ast::{BlockStatement, Identifier};

use self::environment::Env;

pub mod environment;

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(String),
    Function(Vec<Identifier>, BlockStatement, Env),
    Return(Rc<Object>),
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
            Object::Return(value) => write!(f, "{}", value),
            Object::Null => write!(f, "null"),
        }
    }
}
