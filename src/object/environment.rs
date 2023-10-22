use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::Object;

pub type Env = Rc<RefCell<Environment>>;

#[derive(Debug)]
pub struct Environment {
    store: HashMap<String, Rc<Object>>,
    outer: Option<Env>,
}

impl PartialEq for Environment {
    fn eq(&self, other: &Self) -> bool {
        self.store == other.store
    }
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed_environment(outer: &Env) -> Self {
        let mut env = Self::new();
        env.outer = Some(Rc::clone(outer));

        env
    }

    pub fn get(&self, name: &str) -> Option<Rc<Object>> {
        match self.store.get(name) {
            Some(value) => Some(Rc::clone(value)),
            None => None,
        }
    }

    pub fn set(&mut self, name: String, value: Rc<Object>) -> Option<Rc<Object>> {
        self.store.insert(name, value)
    }
}
