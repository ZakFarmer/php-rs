use std::any::Any;

use crate::token::Token;

pub trait Node {
    fn as_any(&self) -> &dyn Any;
    fn token_literal(&self) -> &str;
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

impl std::fmt::Display for dyn Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.token_literal())
    }
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

    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

impl Expression for Identifier {
    fn expression_node(&self) {}
}

pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl std::fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Node for IntegerLiteral {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

impl Expression for IntegerLiteral {
    fn expression_node(&self) {}
}

pub struct InfixExpression {
    pub token: Token,
    pub left: Box<dyn Expression>,
    pub operator: String,
    pub right: Box<dyn Expression>,
}

impl std::fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

impl Node for InfixExpression {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

impl Expression for InfixExpression {
    fn expression_node(&self) {}
}

pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<dyn Expression>,
}

impl std::fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

impl Node for PrefixExpression {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

impl Expression for PrefixExpression {
    fn expression_node(&self) {}
}

pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Box<dyn Expression>>,
}

impl Node for ExpressionStatement {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

impl Statement for ExpressionStatement {
    fn statement_node(&self) {}
}

impl std::fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.expression {
            Some(expr) => write!(f, "{}", expr),
            None => Ok(())
        }
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

    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) {}
}

pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Box<dyn Expression>>
}

impl std::fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{};", self.token_literal())
    }
}

impl Node for ReturnStatement {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

impl Statement for ReturnStatement {
    fn statement_node(&self) {}
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut output = String::new();

        for statement in &self.statements {
            output.push_str(&format!("{}", statement));
        }

        write!(f, "{}", output)
    }
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

    fn token_literal(&self) -> &str {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            ""
        }
    }
}