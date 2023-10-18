
use lazy_static::lazy_static;

use crate::{
    ast::{Boolean, Expression, Literal, Node, Statement},
    object::{Object},
};

lazy_static! {
    static ref TRUE: Object = Object::Boolean(true);
    static ref FALSE: Object = Object::Boolean(false);
}

pub fn eval(node: Node) -> Option<Object> {
    match node {
        Node::Expression(expression) => eval_expression(&expression),
        Node::Program(program) => eval_statements(&program.statements),
        Node::Statement(statement) => eval_statement(&statement),
        _ => None,
    }
}

pub fn eval_statements(statements: &Vec<Statement>) -> Option<Object> {
    let mut result = None;

    for statement in statements {
        let val = eval_statement(statement);

        match val {
            Some(Object::Return(value)) => return Some(*value),
            Some(_) => result = val,
            None => return None,
        }
    }

    result
}

fn eval_expression(expression: &Expression) -> Option<Object> {
    match expression {
        Expression::Identifier(identifier) => eval_identifier(identifier.to_string()),
        Expression::Literal(literal) => eval_literal(&literal),
        _ => None,
    }
}

fn eval_statement(statement: &Statement) -> Option<Object> {
    match statement {
        Statement::Expr(expression) => eval_expression(expression),
        _ => None,
    }
}

fn eval_identifier(_identifier: String) -> Option<Object> {
    unimplemented!("eval_identifier");
}

fn eval_literal(literal: &Literal) -> Option<Object> {
    match literal {
        Literal::Integer(integer) => Some(Object::Integer(integer.value)),
        Literal::Boolean(Boolean { value, .. }) => Some(Object::Boolean(*value)),
        Literal::String(string) => Some(Object::String(string.value.clone())),
        _ => None,
    }
}

fn native_bool_to_bool_object(input: bool) -> &'static Object {
    if input {
        &TRUE
    } else {
        &FALSE
    }
}

#[cfg(test)]
mod tests {
    use anyhow::Error;

    use crate::{lexer::Lexer, object::Object, parser::Parser};

    use super::*;

    #[test]
    fn test_eval_boolean_literals() -> Result<(), Error> {
        let tests = vec![("true", true), ("false", false)];

        for (input, expected) in tests {
            let evaluated = assert_eval(input)?;
            assert_boolean_literal_object(evaluated, expected)?;
        }

        Ok(())
    }

    #[test]
    fn test_eval_integer_expression() -> Result<(), Error> {
        let tests = vec![("5", 5), ("10", 10)];

        for (input, expected) in tests {
            let evaluated = assert_eval(input)?;
            assert_integer_object(evaluated, expected)?;
        }

        Ok(())
    }

    fn assert_eval(input: &str) -> Result<Object, Error> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        parser.check_errors()?;

        let evaluated = eval_statements(&program.statements)
            .ok_or_else(|| anyhow::anyhow!("eval returned None"))?;

        Ok(evaluated)
    }

    fn assert_boolean_literal_object(object: Object, expected: bool) -> Result<(), Error> {
        if let Object::Boolean(boolean) = object {
            assert_eq!(boolean, expected);
        } else {
            return Err(anyhow::anyhow!("Object is not a Boolean."));
        }

        Ok(())
    }

    fn assert_integer_object(object: Object, expected: i64) -> Result<(), Error> {
        if let Object::Integer(integer) = object {
            assert_eq!(integer, expected);
        } else {
            return Err(anyhow::anyhow!("Object is not an Integer."));
        }

        Ok(())
    }
}
