use std::rc::Rc;

use lazy_static::lazy_static;

use crate::{BuiltinFunction, Object};

lazy_static! {
    pub static ref BUILTINS: Vec<(&'static str, BuiltinFunction)> = vec![("len", len),];
}

pub fn get_builtin_by_name(name: &str) -> Option<BuiltinFunction> {
    match BUILTINS
        .iter()
        .find(|(builtin_name, _)| *builtin_name == name)
    {
        Some((_, builtin)) => Some(*builtin),
        None => None,
    }
}

pub fn len(args: Vec<Rc<Object>>) -> Rc<Object> {
    if args.len() != 1 {
        return Rc::new(Object::Null);
    }

    match &*args[0] {
        Object::Array(elements) => Rc::new(Object::Integer(elements.len() as i64)),
        Object::String(s) => Rc::new(Object::Integer(s.len() as i64)),
        _ => Rc::new(Object::Null),
    }
}
