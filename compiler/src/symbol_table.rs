use std::{collections::HashMap, rc::Rc};

#[derive(Clone, Debug)]
pub enum SymbolScope {
    Global,
    Local,
    Builtin,
    Free,
    Function,
}

#[derive(Clone, Debug)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: usize,
}

#[derive(Clone, Debug)]
pub struct SymbolTable {
    pub outer: Option<Rc<Self>>,
    pub store: HashMap<String, Rc<Symbol>>,
    pub num_definitions: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            outer: None,
            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn new_enclosed(outer: Self) -> Self {
        Self {
            store: HashMap::new(),
            num_definitions: 0,
            outer: Some(Rc::new(outer)),
        }
    }

    pub fn define(&mut self, name: &str) -> Rc<Symbol> {
        let symbol = Rc::new(Symbol {
            name: name.to_string(),
            scope: if self.outer.is_none() {
                SymbolScope::Global
            } else {
                SymbolScope::Local
            },
            index: self.num_definitions,
        });

        self.store.insert(name.to_string(), Rc::clone(&symbol));
        self.num_definitions += 1;

        symbol
    }

    pub fn resolve(&self, name: &str) -> Option<Rc<Symbol>> {
        match self.store.get(name) {
            Some(symbol) => Some(Rc::clone(symbol)),
            None => None,
        }
    }
}
