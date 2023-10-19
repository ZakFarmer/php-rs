
use lazy_static::lazy_static;

use crate::{
    ast::{Boolean, Expression, Literal, Node, Statement},
    object::{Object},
};

lazy_static! {
    static ref TRUE: Object = Object::Boolean(true);
    static ref FALSE: Object = Object::Boolean(false);
    static ref NULL: Object = Object::Null;
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
        Expression::Infix(infix) => eval_infix_expression(
            infix.operator.to_string(),
            &infix.left,
            &infix.right,
        ),
        Expression::Prefix(prefix) => eval_prefix_expression(
            prefix.operator.to_string(), 
            &prefix.right
        ),
        _ => None,
    }
}

fn eval_infix_expression(operator: String, left: &Expression, right: &Expression) -> Option<Object> {
    let left = eval_expression(left)?;
    let right = eval_expression(right)?;

    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => {
            eval_integer_infix_expression(operator, left, right)
        }
        (Object::Boolean(left), Object::Boolean(right)) => {
            eval_boolean_infix_expression(operator, left, right)
        }
        (Object::String(left), Object::String(right)) => {
            eval_string_infix_expression(operator, left, right)
        }
        _ => None,
    }
}

fn eval_boolean_infix_expression(operator: String, left: bool, right: bool) -> Option<Object> {
    match operator.as_str() {
        "==" => Some(native_bool_to_bool_object(left == right)),
        "!=" => Some(native_bool_to_bool_object(left != right)),
        _ => None,
    }
}

fn eval_integer_infix_expression(operator: String, left: i64, right: i64) -> Option<Object> {
    match operator.as_str() {
        "+" => Some(Object::Integer(left + right)),
        "-" => Some(Object::Integer(left - right)),
        "*" => Some(Object::Integer(left * right)),
        "/" => Some(Object::Integer(left / right)),
        "<" => Some(native_bool_to_bool_object(left < right)),
        ">" => Some(native_bool_to_bool_object(left > right)),
        "==" => Some(native_bool_to_bool_object(left == right)),
        "!=" => Some(native_bool_to_bool_object(left != right)),
        _ => None,
    }
}

fn eval_string_infix_expression(operator: String, left: String, right: String) -> Option<Object> {
    match operator.as_str() {
        "+" => Some(Object::String(format!("{}{}", left, right))),
        _ => None,
    }
}

fn eval_prefix_expression(operator: String, right: &Expression) -> Option<Object> {
    let right = eval_expression(right)?;

    match operator.as_str() {
        "-" => Some(eval_minus_prefix_operator_expression(right)),
        "!" => Some(eval_bang_operator_expression(right)),
        _ => None,
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        val if val == *TRUE => FALSE.clone(),
        val if val == *FALSE => TRUE.clone(),
        val if val == *NULL => TRUE.clone(),
        _ => FALSE.clone(),
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(integer) => Object::Integer(-integer),
        _ => NULL.clone(),
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

fn native_bool_to_bool_object(input: bool) -> Object {
    if input {
        TRUE.clone()
    } else {
        FALSE.clone()
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
    fn test_eval_boolean_expression() -> Result<(), Error> {
        let tests = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for (input, expected) in tests {
            let evaluated = assert_eval(input)?;
            assert_boolean_literal_object(evaluated, expected)?;
        }

        Ok(())
    }

    #[test]
    fn test_eval_integer_expression() -> Result<(), Error> {
        let tests = vec![
            ("5", 5), 
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for (input, expected) in tests {
            let evaluated = assert_eval(input)?;
            assert_integer_object(evaluated, expected)?;
        }

        Ok(())
    }

    #[test]
    fn test_eval_bang_operator() -> Result<(), Error> {
        let tests = vec![("!true", false), ("!false", true), ("!5", false), ("!!true", true)];

        for (input, expected) in tests {
            let evaluated = assert_eval(input)?;
            assert_boolean_literal_object(evaluated, expected)?;
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
