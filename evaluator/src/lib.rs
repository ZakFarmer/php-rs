use std::{cell::RefCell, rc::Rc};

use anyhow::{Error, Ok, Result};

use parser::ast::{
        BooleanLiteral, CallExpression, Expression, FunctionLiteral, IfExpression, Literal, Node,
        Statement, ArrayLiteral,
};

use object::{
    environment::{Env, Environment},
    Object,
};

pub fn eval(node: Node, env: &Env) -> Result<Rc<Object>> {
    match node {
        Node::Expression(expression) => eval_expression(&expression, env),
        Node::Program(program) => eval_statements(&program.statements, env),
        Node::Statement(statement) => eval_statement(&statement, env),
        _ => Err(Error::msg(format!("Unknown node type: {}", node))),
    }
}

pub fn eval_statements(statements: &Vec<Statement>, env: &Env) -> Result<Rc<Object>> {
    let mut result: Rc<Object> = Rc::new(Object::Null);

    for statement in statements {
        let val = eval_statement(statement, env)?;

        match *val {
            Object::Return(_) => return Ok(val),
            _ => result = val,
        }
    }

    Ok(result)
}

fn apply_function(function: &Rc<Object>, args: &Vec<Rc<Object>>) -> Result<Rc<Object>> {
    match &**function {
        Object::Function(params, body, env) => {
            let mut env = Environment::new_enclosed_environment(&env);

            params.iter().enumerate().for_each(|(i, param)| {
                env.set(param.value.clone(), args[i].clone());
            });

            let evaluated = eval_statements(&body.statements, &Rc::new(RefCell::new(env)))?;
            return unwrap_return_value(evaluated);
        }
        f => Err(Error::msg(format!("expected {} to be a function", f))),
    }
}

fn is_truthy(object: &Object) -> bool {
    match object {
        Object::Null => false,
        Object::Boolean(boolean) => *boolean,
        _ => true,
    }
}

fn unwrap_return_value(object: Rc<Object>) -> Result<Rc<Object>> {
    match &*object {
        Object::Return(value) => Ok(Rc::clone(value)),
        _ => Ok(object),
    }
}

fn eval_expression(expression: &Expression, env: &Env) -> Result<Rc<Object>> {
    match expression {
        Expression::Identifier(identifier_expression) => {
            eval_identifier(identifier_expression.to_string(), env)
        }
        Expression::Literal(literal) => eval_literal(&literal, env),
        Expression::Index(index_expression) => {
            let left = eval_expression(&index_expression.left, &Rc::clone(env))?;
            let index = eval_expression(&index_expression.index, &Rc::clone(env))?;

            eval_index_expression(left, index)
        },
        Expression::Infix(infix_expression) => {
            let left = eval_expression(&infix_expression.left, &Rc::clone(env))?;
            let right = eval_expression(&infix_expression.right, &Rc::clone(env))?;

            eval_infix_expression(infix_expression.operator.to_string(), &left, &right, env)
        }
        Expression::Prefix(prefix_expression) => {
            let right = eval_expression(&prefix_expression.right, &Rc::clone(env))?;

            eval_prefix_expression(prefix_expression.operator.to_string(), &right, env)
        }
        Expression::If(_if_expression) => eval_if_expression(expression, env),
        Expression::Call(CallExpression {
            function,
            token: _,
            arguments,
        }) => {
            let function = eval_expression(function, env)?; // This should give you a Function object.
            let arguments = eval_expressions(arguments, env)?;

            apply_function(&function, &arguments)
        }
        Expression::Function(FunctionLiteral {
            parameters, body, ..
        }) => Ok(Object::Function(parameters.clone(), body.clone(), Rc::clone(env)).into()),
        _ => Err(Error::msg(format!(
            "Unknown expression type: {}",
            expression
        ))),
    }
}

fn eval_expressions(exprs: &Vec<Expression>, env: &Env) -> Result<Vec<Rc<Object>>> {
    let mut list = Vec::new();
    for expr in exprs {
        let val = eval_expression(expr, &Rc::clone(env))?;
        list.push(val);
    }

    Ok(list)
}

fn eval_if_expression(expression: &Expression, env: &Env) -> Result<Rc<Object>> {
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
        Err(Error::msg(format!(
            "Unknown expression type: {}",
            expression
        )))
    }
}

fn eval_index_expression(
    left: Rc<Object>,
    index: Rc<Object>,
) -> Result<Rc<Object>> {
    match (&*left, &*index) {
        (Object::Array(elements), Object::Integer(index)) => {
            let max = elements.len() - 1;

            if *index < 0 || *index > max as i64 {
                return Ok(Object::Null.into());
            }

            return Ok(Rc::clone(&elements[*index as usize]));
        }
        _ => Err(Error::msg(format!(
            "Unknown index expression: {}[{}]",
            left, index
        ))),
    }
}

fn eval_infix_expression(
    operator: String,
    left: &Object,
    right: &Object,
    _env: &Env,
) -> Result<Rc<Object>> {
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
        _ => Err(Error::msg(format!("Unknown operator: {}", operator))),
    }
}

fn eval_boolean_infix_expression(operator: String, left: bool, right: bool) -> Result<Rc<Object>> {
    let result = match operator.as_str() {
        "==" => native_bool_to_bool_object(left == right),
        "!=" => native_bool_to_bool_object(left != right),
        _ => {
            return Err(Error::msg(format!(
                "Unknown operator: {} {} {}",
                left, operator, right
            )))
        }
    };

    Ok(result.into())
}

fn eval_integer_infix_expression(operator: String, left: i64, right: i64) -> Result<Rc<Object>> {
    let result = match operator.as_str() {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "*" => Object::Integer(left * right),
        "/" => Object::Integer(left / right),
        "<" => native_bool_to_bool_object(left < right),
        ">" => native_bool_to_bool_object(left > right),
        "==" => native_bool_to_bool_object(left == right),
        "!=" => native_bool_to_bool_object(left != right),
        _ => {
            return Err(Error::msg(format!(
                "Unknown operator: {} {} {}",
                left, operator, right
            )))
        }
    };

    Ok(result.into())
}

fn eval_string_infix_expression(
    operator: String,
    left: String,
    right: String,
) -> Result<Rc<Object>> {
    let result = match operator.as_str() {
        "+" => Object::String(format!("{}{}", left, right)),
        _ => {
            return Err(Error::msg(format!(
                "Unknown operator: {} {} {}",
                left, operator, right
            )))
        }
    };

    Ok(result.into())
}

fn eval_prefix_expression(operator: String, right: &Object, _env: &Env) -> Result<Rc<Object>> {
    match operator.as_str() {
        "-" => eval_minus_prefix_operator_expression(right),
        "!" => eval_bang_operator_expression(right),
        _ => {
            return Err(Error::msg(format!(
                "Unknown operator: {}{}",
                operator, right
            )))
        }
    }
}

fn eval_bang_operator_expression(right: &Object) -> Result<Rc<Object>> {
    let result = match *right {
        Object::Null => Rc::new(Object::Boolean(true)),
        Object::Boolean(boolean) => Rc::new(Object::Boolean(!boolean)),
        _ => Rc::new(Object::Boolean(false)),
    };

    Ok(result.into())
}

fn eval_minus_prefix_operator_expression(right: &Object) -> Result<Rc<Object>> {
    let result = match *right {
        Object::Integer(integer) => Rc::from(Object::Integer(-integer)),
        _ => {
            return Err(Error::msg(format!(
                "Invalid use of minus operator: -{}",
                right
            )))
        }
    };

    Ok(result.into())
}

fn eval_statement(statement: &Statement, env: &Env) -> Result<Rc<Object>> {
    match statement {
        Statement::Expr(expression) => eval_expression(expression, env),
        Statement::Assign(assignment) => {
            let value = eval_expression(&assignment.value, env)?;
            let object = Rc::clone(&value);

            env.borrow_mut().set(assignment.name.to_string(), object);

            Ok(value)
        }
        Statement::Return(return_statement) => {
            let value = eval_expression(&return_statement.return_value, env)?;

            Ok(Rc::new(Object::Return(value)))
        }
        _ => Err(Error::msg(format!("Unknown statement type: {}", statement))),
    }
}

fn eval_identifier(identifier: String, env: &Env) -> Result<Rc<Object>> {
    match env.borrow().get(&identifier) {
        Some(value) => Ok(value),
        None => Err(Error::msg(format!("Identifier not found: {}", identifier))),
    }
}

fn eval_literal(literal: &Literal, env: &Env) -> Result<Rc<Object>> {
    let result = match literal {
        Literal::Integer(integer) => Object::Integer(integer.value),
        Literal::Boolean(BooleanLiteral { value, .. }) => Object::Boolean(*value),
        Literal::String(string) => Object::String(string.value.clone()),
        Literal::Array(ArrayLiteral { elements, ..}) => {
            let elements = eval_expressions(elements, env)?;

            return Ok(Rc::from(Object::Array(elements)));
        }
        _ => return Err(Error::msg(format!("Unknown literal type: {}", literal))),
    };

    Ok(result.into())
}

fn native_bool_to_bool_object(input: bool) -> Object {
    Object::Boolean(input)
}