use std::{any::Any, fmt::Debug};

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
        let statement_string = match self.as_any().type_id() {
            id if id == std::any::TypeId::of::<ExpressionStatement>() => {
                let expression_statement = self.as_any().downcast_ref::<ExpressionStatement>().unwrap();
                format!("{:?}", expression_statement)
            },
            id if id == std::any::TypeId::of::<ReturnStatement>() => {
                let return_statement = self.as_any().downcast_ref::<ReturnStatement>().unwrap();
                format!("{:?}", return_statement)
            },
            id if id == std::any::TypeId::of::<VariableAssignment>() => {
                let variable_assignment = self.as_any().downcast_ref::<VariableAssignment>().unwrap();
                format!("{:?}", variable_assignment)
            },
            _ => "".to_string()
        };

        write!(f, "{}", statement_string)
    }
}

impl Debug for dyn Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let statement_string = match self.as_any().type_id() {
            id if id == std::any::TypeId::of::<ExpressionStatement>() => {
                let expression_statement = self.as_any().downcast_ref::<ExpressionStatement>().unwrap();
                format!("Expression: {:?}", expression_statement)
            },
            id if id == std::any::TypeId::of::<ReturnStatement>() => {
                let return_statement = self.as_any().downcast_ref::<ReturnStatement>().unwrap();
                format!("Return: {:?}", return_statement)
            },
            id if id == std::any::TypeId::of::<VariableAssignment>() => {
                let variable_assignment = self.as_any().downcast_ref::<VariableAssignment>().unwrap();
                format!("Assignment: {:?}", variable_assignment)
            },
            _ => "".to_string()
        };

        write!(f, "{}", statement_string)
    }
}

impl Expression for dyn Statement {
    fn expression_node(&self) {}
}

pub trait Expression: Node {
    fn expression_node(&self);
}

impl std::fmt::Debug for dyn Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.as_any().type_id() == std::any::TypeId::of::<InfixExpression>() {
            let infix_expression = self.as_any().downcast_ref::<InfixExpression>().unwrap();
            write!(f, "({:?} {} {:?})", infix_expression.left, infix_expression.operator, infix_expression.right)
        } else if self.as_any().type_id() == std::any::TypeId::of::<PrefixExpression>() {
            let prefix_expression = self.as_any().downcast_ref::<PrefixExpression>().unwrap();
            write!(f, "({}{:?})", prefix_expression.operator, prefix_expression.right)
        } else {
            write!(f, "{}", self.token_literal())
        }
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

impl std::fmt::Debug for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        println!("left: {:?}", self.left);
        println!("right: {:?}", self.right);

        write!(f, "({:?} {} {:?})", self.left, self.operator, self.right)
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

impl std::fmt::Debug for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({}{:?})", self.operator, self.right)
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

impl std::fmt::Debug for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.expression {
            Some(expr) => write!(f, "{:?}", expr),
            None => Ok(())
        }
    }
}

pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Box<dyn Expression>>
}

impl std::fmt::Debug for ReturnStatement {
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

pub struct VariableAssignment {
    pub token: Token,
    pub name: String,
    pub value: Box<dyn Expression>,
}

impl std::fmt::Debug for VariableAssignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${} = {:?};", self.name, self.value)
    }
}

impl Node for VariableAssignment {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

impl Statement for VariableAssignment {
    fn statement_node(&self) {}
}

pub struct VariableReference {
    pub token: Token,
    pub name: String,
}

impl std::fmt::Debug for VariableReference {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "${}", self.name.to_string())
    }
}

impl Node for VariableReference {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

impl Expression for VariableReference {
    fn expression_node(&self) {}
}

impl Statement for VariableReference {
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

impl std::fmt::Debug for Program {
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