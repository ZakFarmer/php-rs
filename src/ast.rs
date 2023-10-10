use std::any::Any;

use crate::token::Token;

pub trait Node {
    fn as_any(&self) -> &dyn Any;
    fn token_literal(&self) -> String;
}

pub trait Statement: Node {
    fn statement_node(&self);
}

impl std::fmt::Display for dyn Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.token_literal())
    }
}

pub trait Expression: Node {
    fn expression_node(&self);
}

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Node for Identifier {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn token_literal(&self) -> String {
        match &self.token {
            Token::Ident(x) => x.to_owned(),
            _ => String::from(""),
        }
    }
}

impl Expression for Identifier {
    fn expression_node(&self) {
        
    }
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<Box<dyn Expression>>,
}

impl std::fmt::Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} {};", self.token_literal(), self.name)
    }
}

impl Node for LetStatement {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn token_literal(&self) -> String {
        match &self.token {
            Token::Let => String::from("let"),
            _ => String::from(""),
        }
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) {}
}

pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Box<dyn Expression>>
}

impl Node for ReturnStatement {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn token_literal(&self) -> String {
        match &self.token {
            Token::Return => String::from("return"),
            _ => String::from(""),
        }
    }
}

impl Statement for ReturnStatement {
    fn statement_node(&self) {}
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Default for Program {
    fn default() -> Self {
        Self {
            statements: vec![]
        }
    }
}

impl Node for Program {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            String::new()
        }
    }
}