use std::{cell::RefCell, rc::Rc};

use anyhow::Error;

use evaluator::eval;
use lexer::Lexer;
use object::{environment::Environment, Object};
use parser::{ast::Node, Parser};

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
        assert_integer_literal_object(evaluated, expected)?;
    }

    Ok(())
}

#[test]
fn test_eval_string_expressions() -> Result<(), Error> {
    let tests = vec![("\"Hello World!\"", "Hello World!")];

    for (input, expected) in tests {
        let evaluated = assert_eval(input)?;
        assert_string_literal_object(evaluated, expected)?;
    }

    Ok(())
}

#[test]
fn test_eval_string_concatenations() -> Result<(), Error> {
    let tests = vec![
        ("\"Hello\" + \" \" + \"World!\"", "Hello World!"),
        ("\"Hello\" + \"World!\"", "HelloWorld!"),
    ];

    for (input, expected) in tests {
        let evaluated = assert_eval(input)?;
        assert_string_literal_object(evaluated, expected)?;
    }

    Ok(())
}

#[test]
fn test_eval_arrays() -> Result<(), Error> {
    let tests = vec![
        ("[1, 2, 3]", vec![1, 2, 3]),
        ("[1 + 2, 3 * 4, 5 + 6]", vec![3, 12, 11]),
    ];

    for (input, expected) in tests {
        let evaluated = assert_eval(input)?;

        if let Object::Array(elements) = &*evaluated {
            assert_eq!(elements.len(), expected.len());

            for (index, element) in elements.iter().enumerate() {
                assert_integer_literal_object(Rc::clone(element), expected[index])?;
            }
        } else {
            return Err(Error::msg("Object is not an Array."));
        }
    }

    Ok(())
}

#[test]
fn test_eval_bang_operator() -> Result<(), Error> {
    let tests = vec![
        ("!true", false),
        ("!false", true),
        ("!5", false),
        ("!!true", true),
    ];

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
            assert_integer_literal_object(evaluated, expected)?;
        } else {
            assert_eq!(*evaluated, Object::Null);
        }
    }

    Ok(())
}

#[test]
fn test_eval_index_expressions() -> Result<(), Error> {
    let tests = vec![
        ("[1, 2, 3][0]", 1),
        ("[1, 2, 3][1]", 2),
        ("[1, 2, 3][2]", 3),
        ("$i = 0; [1][$i];", 1),
        ("[1, 2, 3][1 + 1];", 3),
        ("$myArray = [1, 2, 3]; $myArray[2];", 3),
        (
            "$myArray = [1, 2, 3]; $myArray[0] + $myArray[1] + $myArray[2];",
            6,
        ),
    ];

    for (input, expected) in tests {
        let evaluated = assert_eval(input)?;

        assert_integer_literal_object(evaluated, expected)?;
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

        assert_integer_literal_object(evaluated, expected)?;
    }

    Ok(())
}

#[test]
fn test_eval_functions() -> Result<(), Error> {
    let tests = vec![("function ($x) { $x + 2; }(2);", 4)];

    for (input, expected) in tests {
        let evaluated = assert_eval(input)?;

        assert_integer_literal_object(evaluated, expected)?;
    }

    Ok(())
}

fn assert_eval(input: &str) -> Result<Rc<Object>, Error> {
    let env = Rc::new(RefCell::new(Environment::new()));

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program()?;
    parser.check_errors()?;

    let evaluated = eval(Node::Program(program), &env);

    evaluated
}

fn assert_boolean_literal_object(object: Rc<Object>, expected: bool) -> Result<(), Error> {
    if let Object::Boolean(boolean) = *object {
        assert_eq!(boolean, expected);
    } else {
        return Err(Error::msg("Object is not a Boolean."));
    }

    Ok(())
}

fn assert_integer_literal_object(object: Rc<Object>, expected: i64) -> Result<(), Error> {
    if let Object::Integer(integer) = *object {
        assert_eq!(integer, expected);
    } else {
        return Err(Error::msg("Object is not an Integer."));
    }

    Ok(())
}

fn assert_string_literal_object(object: Rc<Object>, expected: &str) -> Result<(), Error> {
    // Dereference the Rc<Object> to get an Object and then reference it again for pattern matching.
    match &*object {
        Object::String(string) if string == expected => Ok(()),
        Object::String(_) => Err(anyhow::anyhow!(
            "String value does not match expected value."
        )),
        _ => Err(Error::msg("Object is not a String.")),
    }
}
