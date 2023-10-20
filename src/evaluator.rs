
use anyhow::{Result, Ok, Error};
use lazy_static::lazy_static;

use crate::{ast::{Boolean, Expression, Literal, Node, Statement, IfExpression}, object::Object};

lazy_static! {
    static ref TRUE: Object = Object::Boolean(true);
    static ref FALSE: Object = Object::Boolean(false);
    static ref NULL: Object = Object::Null;
}

pub fn eval(node: Node) -> Result<Object> {
    match node {
        Node::Expression(expression) => eval_expression(&expression),
        Node::Program(program) => eval_statements(&program.statements),
        Node::Statement(statement) => eval_statement(&statement),
        _ => Err(Error::msg(format!("Unknown node type: {}", node))),
    }
}

pub fn eval_statements(statements: &Vec<Statement>) -> Result<Object> {
    let mut result: Object = NULL.clone();

    for statement in statements {
        let val = eval_statement(statement);

        if let Err(err) = val {
            return Err(err);
        }

        result = val.unwrap();

        if let Object::Return(value) = result {
            return Ok(*value);
        }
    }

    Ok(result)
}

fn is_truthy(object: &Object) -> bool {
    match object {
        Object::Null => false,
        Object::Boolean(boolean) => *boolean,
        _ => true,
    }
}

fn eval_expression(expression: &Expression) -> Result<Object> {
    match expression {
        Expression::Identifier(identifier_expression) => eval_identifier(identifier_expression.to_string()),
        Expression::Literal(literal) => eval_literal(&literal),
        Expression::Infix(infix_expression) => eval_infix_expression(
            infix_expression.operator.to_string(),
            &infix_expression.left,
            &infix_expression.right,
        ),
        Expression::Prefix(prefix_expression) => eval_prefix_expression(
            prefix_expression.operator.to_string(), 
            &prefix_expression.right
        ),
        Expression::If(if_expression) => eval_if_expression(expression),
        _ => Err(Error::msg(format!("Unknown expression type: {}", expression))),
    }
}

fn eval_if_expression(expression: &Expression) -> Result<Object> {
    if let Expression::If(IfExpression {
        condition,
        consequence,
        alternative,
        ..
    }) = expression
    {
        let condition = eval_expression(condition)?;

        if is_truthy(&condition) {
            eval_statements(&consequence.statements)
        } else {
            match alternative {
                Some(alternative) => eval_statements(&alternative.statements),
                None => Ok(NULL.clone()),
            }
        }
    } else {
        Err(Error::msg(format!("Unknown expression type: {}", expression)))
    }
}

fn eval_infix_expression(operator: String, left: &Expression, right: &Expression) -> Result<Object> {
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
        _ => Err(Error::msg(format!(
            "Unknown operator: {}",
            operator
        ))),
    }
}

fn eval_boolean_infix_expression(operator: String, left: bool, right: bool) -> Result<Object> {
    match operator.as_str() {
        "==" => Ok(native_bool_to_bool_object(left == right)),
        "!=" => Ok(native_bool_to_bool_object(left != right)),
        _ => Err(Error::msg(format!(
            "Unknown operator: {} {} {}",
            left, operator, right
        ))),
    }
}

fn eval_integer_infix_expression(operator: String, left: i64, right: i64) -> Result<Object> {
    match operator.as_str() {
        "+" => Ok(Object::Integer(left + right)),
        "-" => Ok(Object::Integer(left - right)),
        "*" => Ok(Object::Integer(left * right)),
        "/" => Ok(Object::Integer(left / right)),
        "<" => Ok(native_bool_to_bool_object(left < right)),
        ">" => Ok(native_bool_to_bool_object(left > right)),
        "==" => Ok(native_bool_to_bool_object(left == right)),
        "!=" => Ok(native_bool_to_bool_object(left != right)),
        _ => Err(Error::msg(format!(
            "Unknown operator: {} {} {}",
            left, operator, right
        ))),
    }
}

fn eval_string_infix_expression(operator: String, left: String, right: String) -> Result<Object> {
    match operator.as_str() {
        "+" => Ok(Object::String(format!("{}{}", left, right))),
        _ => Err(Error::msg(format!(
            "Unknown operator: {} {} {}",
            left, operator, right
        ))),
    }
}

fn eval_prefix_expression(operator: String, right: &Expression) -> Result<Object> {
    let right = eval_expression(right)?;

    match operator.as_str() {
        "-" => Ok(eval_minus_prefix_operator_expression(right)),
        "!" => Ok(eval_bang_operator_expression(right)),
        _ => Err(Error::msg(format!(
            "Unknown operator: {}{}",
            operator, right
        ))),
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

fn eval_statement(statement: &Statement) -> Result<Object> {
    match statement {
        Statement::Expr(expression) => eval_expression(expression),
        _ => Err(Error::msg(format!("Unknown statement type: {}", statement))),
    }
}

fn eval_identifier(_identifier: String) -> Result<Object> {
    unimplemented!("eval_identifier");
}

fn eval_literal(literal: &Literal) -> Result<Object> {
    match literal {
        Literal::Integer(integer) => Ok(Object::Integer(integer.value)),
        Literal::Boolean(Boolean { value, .. }) => Ok(Object::Boolean(*value)),
        Literal::String(string) => Ok(Object::String(string.value.clone())),
        _ => Err(Error::msg(format!("Unknown literal type: {}", literal))),
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

    #[test]
    fn test_eval_if_else_expressions() -> Result<(), Error> {
        let tests = vec![
            ("if (true) { 10 }", Some(10)),
            ("if (false) { 10 }", None),
            ("if (1) { 10 }", Some(10)),
            ("if (1 < 2) { 10 }", Some(10)),
            ("if (1 > 2) { 10 }", None),
            ("if (1 > 2) { 10 } else { 20 }", Some(20)),
            ("if (1 < 2) { 10 } else { 20 }", Some(10)),
        ];

        for (input, expected) in tests {
            let evaluated = assert_eval(input)?;

            if let Some(expected) = expected {
                assert_integer_object(evaluated, expected)?;
            } else {
                assert_eq!(evaluated, *NULL);
            }
        }

        Ok(())
    }

    fn assert_eval(input: &str) -> Result<Object, Error> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        parser.check_errors()?;

        let evaluated = eval_statements(&program.statements)
            .or_else(|_| Err(anyhow::anyhow!("eval returned None")))?;

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
