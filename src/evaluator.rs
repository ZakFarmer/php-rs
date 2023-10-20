
use std::sync::Arc;

use anyhow::{Result, Ok, Error};
use lazy_static::lazy_static;

use crate::{ast::{Boolean, Expression, Literal, Node, Statement, IfExpression}, object::{Object, environment::Env}};

lazy_static! {
    static ref TRUE: Object = Object::Boolean(true);
    static ref FALSE: Object = Object::Boolean(false);
    static ref NULL: Object = Object::Null;
}

pub fn eval(node: Node, env: &Env) -> Result<Arc<Object>> {
    match node {
        Node::Expression(expression) => eval_expression(&expression, env),
        Node::Program(program) => eval_statements(&program.statements, env),
        Node::Statement(statement) => eval_statement(&statement, env),
        _ => Err(Error::msg(format!("Unknown node type: {}", node))),
    }
}

pub fn eval_statements(statements: &Vec<Statement>, env: &Env) -> Result<Arc<Object>> {
    let mut result: Arc<Object> = Arc::new(Object::Null);

    for statement in statements {
        let val = eval_statement(statement, env)?;

        match *val {
            Object::Return(_) => return Ok(val),
            _ => result = val,
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

fn eval_expression(expression: &Expression, env: &Env) -> Result<Arc<Object>> {
    match expression {
        Expression::Identifier(identifier_expression) => eval_identifier(identifier_expression.to_string(), env),
        Expression::Literal(literal) => eval_literal(&literal),
        Expression::Infix(infix_expression) => {
            let left = eval_expression(&infix_expression.left, &Arc::clone(env))?;
            let right = eval_expression(&infix_expression.right, &Arc::clone(env))?;

            eval_infix_expression(
                infix_expression.operator.to_string(),
                &left,
                &right,
                env
            )
        },
        Expression::Prefix(prefix_expression) => {
            let right = eval_expression(&prefix_expression.right, &Arc::clone(env))?;

            eval_prefix_expression(
                prefix_expression.operator.to_string(), 
                &right,
                env
            )
        },
        Expression::If(if_expression) => eval_if_expression(expression, env),
        _ => Err(Error::msg(format!("Unknown expression type: {}", expression))),
    }
}

fn eval_if_expression(expression: &Expression, env: &Env) -> Result<Arc<Object>> {
    if let Expression::If(IfExpression {
        condition,
        consequence,
        alternative,
        ..
    }) = expression
    {
        let condition = eval_expression(condition, env)?;

        if is_truthy(&condition) {
            eval_statements(&consequence.statements, env)
        } else {
            match alternative {
                Some(alternative) => eval_statements(&alternative.statements, env),
                None => Ok(Object::Null.into()),
            }
        }
    } else {
        Err(Error::msg(format!("Unknown expression type: {}", expression)))
    }
}

fn eval_infix_expression(operator: String, left: &Object, right: &Object, env: &Env) -> Result<Arc<Object>> {
    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => {
            eval_integer_infix_expression(operator, *left, *right)
        }
        (Object::Boolean(left), Object::Boolean(right)) => {
            eval_boolean_infix_expression(operator, *left, *right)
        }
        (Object::String(left), Object::String(right)) => {
            eval_string_infix_expression(operator, left.to_string(), right.to_string())
        }
        _ => Err(Error::msg(format!(
            "Unknown operator: {}",
            operator
        ))),
    }
}

fn eval_boolean_infix_expression(operator: String, left: bool, right: bool) -> Result<Arc<Object>> {
    let result = match operator.as_str() {
        "==" => native_bool_to_bool_object(left == right),
        "!=" => native_bool_to_bool_object(left != right),
        _ => return Err(Error::msg(format!(
            "Unknown operator: {} {} {}",
            left, operator, right
        ))),
    };

    Ok(result.into())
}

fn eval_integer_infix_expression(operator: String, left: i64, right: i64) -> Result<Arc<Object>> {
    let result = match operator.as_str() {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "*" => Object::Integer(left * right),
        "/" => Object::Integer(left / right),
        "<" => native_bool_to_bool_object(left < right),
        ">" => native_bool_to_bool_object(left > right),
        "==" => native_bool_to_bool_object(left == right),
        "!=" => native_bool_to_bool_object(left != right),
        _ => return Err(Error::msg(format!(
            "Unknown operator: {} {} {}",
            left, operator, right
        ))),
    };

    Ok(result.into())
}

fn eval_string_infix_expression(operator: String, left: String, right: String) -> Result<Arc<Object>> {
    let result = match operator.as_str() {
        "+" => Object::String(format!("{}{}", left, right)),
        _ => return Err(Error::msg(format!(
            "Unknown operator: {} {} {}",
            left, operator, right
        ))),
    };

    Ok(result.into())
}

fn eval_prefix_expression(operator: String, right: &Object, env: &Env) -> Result<Arc<Object>> {
    match operator.as_str() {
        "-" => eval_minus_prefix_operator_expression(right),
        "!" => eval_bang_operator_expression(right),
        _ => return Err(Error::msg(format!(
            "Unknown operator: {}{}",
            operator, right
        ))),
    }
}

fn eval_bang_operator_expression(right: &Object) -> Result<Arc<Object>> {
    let result = match *right {
        Object::Null => Arc::new(Object::Boolean(true)),
        Object::Boolean(boolean) => Arc::new(Object::Boolean(!boolean)),
        _ => Arc::new(Object::Boolean(false)),
    };

    Ok(result.into())
}

fn eval_minus_prefix_operator_expression(right: &Object) -> Result<Arc<Object>> {
    let result = match *right {
        Object::Integer(integer) => Arc::from(Object::Integer(-integer)),
        _ => return Err(Error::msg(format!(
            "Invalid use of minus operator: -{}",
            right
        ))),
    };

    Ok(result.into())
}

fn eval_statement(statement: &Statement, env: &Env) -> Result<Arc<Object>> {
    match statement {
        Statement::Expr(expression) => eval_expression(expression, env),
        Statement::Assign(assignment) => {
            let value = eval_expression(&assignment.value, env)?;
            let object = Arc::clone(&value);

            env.borrow_mut().set(assignment.name.to_string(), object);

            Ok(value)
        },
        Statement::Return(return_statement) => {
            let value = eval_expression(&return_statement.return_value, env)?;

            Ok(Arc::new(Object::Return(value)))
        },
        _ => Err(Error::msg(format!("Unknown statement type: {}", statement))),
    }
}

fn eval_identifier(identifier: String, env: &Env) -> Result<Arc<Object>> {
    match env.borrow().get(&identifier) {
        Some(value) => Ok(value),
        None => Err(Error::msg(format!("Identifier not found: {}", identifier))),
    }
}

fn eval_literal(literal: &Literal) -> Result<Arc<Object>> {
    let result = match literal {
        Literal::Integer(integer) => Object::Integer(integer.value),
        Literal::Boolean(Boolean { value, .. }) => Object::Boolean(*value),
        Literal::String(string) => Object::String(string.value.clone()),
        _ => return Err(Error::msg(format!("Unknown literal type: {}", literal))),
    };

    Ok(result.into())
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
    use std::cell::RefCell;

    use anyhow::Error;

    use crate::{lexer::Lexer, object::{Object, environment::Environment}, parser::Parser};

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
                assert_eq!(*evaluated, *NULL);
            }
        }

        Ok(())
    }

    #[test]
    fn test_eval_assertions() -> Result<(), Error> {
        let tests = vec![
            ("$a = 5; $a;", 5),
            ("$a = 5 * 5; $a;", 25),
            ("$a = 5; $a;", 5),
        ];

        for (input, expected) in tests {
            let evaluated = assert_eval(input)?;

            dbg!(&evaluated);

            assert_integer_object(evaluated, expected)?;
        }

        Ok(())
    }

    fn assert_eval(input: &str) -> Result<Arc<Object>, Error> {
        let env = Arc::new(RefCell::new(Environment::new()));
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        parser.check_errors()?;

        let evaluated = eval(Node::Program(program), &env);

        evaluated
    }

    fn assert_boolean_literal_object(object: Arc<Object>, expected: bool) -> Result<(), Error> {
        if let Object::Boolean(boolean) = *object {
            assert_eq!(boolean, expected);
        } else {
            return Err(anyhow::anyhow!("Object is not a Boolean."));
        }

        Ok(())
    }

    fn assert_integer_object(object: Arc<Object>, expected: i64) -> Result<(), Error> {
        if let Object::Integer(integer) = *object {
            assert_eq!(integer, expected);
        } else {
            return Err(anyhow::anyhow!("Object is not an Integer."));
        }

        Ok(())
    }
}
