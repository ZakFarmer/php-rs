use std::{collections::HashMap, sync::Arc, cell::RefCell};

use super::Object;

pub type Env = Arc<RefCell<Environment>>;

pub struct Environment {
    store: HashMap<String, Arc<Object>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<Arc<Object>> {
        match self.store.get(name) {
            Some(value) => Some(Arc::clone(value)),
            None => None,
        }
    }

    pub fn set(&mut self, name: String, value: Arc<Object>) -> Option<Arc<Object>> {
        self.store.insert(name, value)
    }
}