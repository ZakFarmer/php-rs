use lexer::{
    token::{Token, TokenType},
    Lexer,
};
use parser::{
    ast::{BlockStatement, Expression, Literal},
    *,
};

use anyhow::{Error, Result};

use parser::ast::{ArrayLiteral, Statement};

#[test]
fn test_assignment_statements() -> Result<(), Error> {
    let input = "
        $x = 5;
        $y = 10;
        $foobar = 812303;
    ";

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program()?;
    parser.check_errors()?;

    assert_eq!(3, program.statements.len());

    let expected_identifiers = ["$x", "$y", "$foobar"];

    for i in 0..expected_identifiers.len() {
        let statement = &program.statements[i];

        assert_let_statement(statement, expected_identifiers[i])?;
    }

    Ok(())
}

#[test]
fn test_boolean_expression() -> Result<(), Error> {
    let input = "
        true;
        false;

        $x = true;
        $y = false;
    ";

    let expected_values = [true, false, true, false];

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program()?;
    parser.check_errors()?;

    assert_eq!(4, program.statements.len());

    for i in 0..expected_values.len() {
        let statement = &program.statements[i];

        match statement {
            Statement::Expr(expression) => {
                if let Expression::Literal(Literal::Boolean(boolean)) = &expression {
                    assert_eq!(expected_values[i], boolean.value);
                } else {
                    assert!(false, "Expected Boolean expression");
                }
            }
            Statement::Assign(variable_assignment) => {
                if let Expression::Literal(Literal::Boolean(boolean)) = &variable_assignment.value {
                    assert_eq!(expected_values[i], boolean.value);
                } else {
                    assert!(false, "Expected Boolean expression");
                }
            }
            _ => {
                assert!(false, "Expected ExpressionStatement or VariableAssignment");
            }
        }
    }

    Ok(())
}

#[test]
fn test_call_expression() -> Result<(), Error> {
    let input = "add(1, 2 * 3, 4 + 5);";

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program()?;
    parser.check_errors()?;

    assert_eq!(1, program.statements.len());

    if let Statement::Expr(expression) = &program.statements[0] {
        if let Expression::Call(call_expression) = &expression {
            assert_eq!("add", call_expression.function.to_string());

            assert_eq!(3, call_expression.arguments.len());

            assert_literal_expression(&call_expression.arguments[0], "1")?;

            assert_infix_expression(
                &call_expression.arguments[1],
                "2",
                &Token {
                    literal: "*".to_string(),
                    token_type: TokenType::Asterisk,
                },
                "3",
            )?;

            assert_infix_expression(
                &call_expression.arguments[2],
                "4",
                &Token {
                    literal: "+".to_string(),
                    token_type: TokenType::Plus,
                },
                "5",
            )?;
        } else {
            assert!(false, "Expected CallExpression");
        }
    } else {
        assert!(false, "Expected ExpressionStatement");
    }

    Ok(())
}

#[test]
fn test_if_expression() -> Result<(), Error> {
    let input = "
        if ($x < $y) { return $x; }
    ";

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program()?;

    parser.check_errors()?;

    assert_eq!(1, program.statements.len());

    if let Statement::Expr(expression) = &program.statements[0] {
        if let Expression::If(if_expression) = &expression {
            assert_infix_expression(
                &if_expression.condition,
                "$x",
                &Token {
                    literal: "<".to_string(),
                    token_type: TokenType::Lt,
                },
                "$y",
            )?;

            assert_eq!(1, if_expression.consequence.statements.len());

            if let Statement::Return(return_statement) = &if_expression.consequence.statements[0] {
                assert_identifier(&return_statement.return_value, "$x")?;
            } else {
                assert!(false, "Expected ReturnStatement");
            }
        } else {
            assert!(false, "Expected IfExpression");
        }
    } else {
        assert!(false, "Expected ExpressionStatement");
    }

    Ok(())
}

#[test]
fn test_if_else_expression() -> Result<(), Error> {
    let input = "
        if ($x < $y) { return $x; } else { return $y; }
    ";

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program()?;

    parser.check_errors()?;

    assert_eq!(1, program.statements.len());

    if let Statement::Expr(expression) = &program.statements[0] {
        if let Expression::If(if_expression) = &expression {
            assert_infix_expression(
                &if_expression.condition,
                "$x",
                &Token {
                    literal: "<".to_string(),
                    token_type: TokenType::Lt,
                },
                "$y",
            )?;

            assert_eq!(1, if_expression.consequence.statements.len());

            if let Statement::Return(return_statement) = &if_expression.consequence.statements[0] {
                assert_identifier(&return_statement.return_value, "$x")?;
            } else {
                assert!(false, "Expected ReturnStatement");
            }

            assert_eq!(
                1,
                if_expression.alternative.as_ref().unwrap().statements.len()
            );

            if let Statement::Return(return_statement) =
                &if_expression.alternative.as_ref().unwrap().statements[0]
            {
                assert_identifier(&return_statement.return_value, "$y")?;
            } else {
                assert!(false, "Expected ReturnStatement");
            }
        } else {
            assert!(false, "Expected IfExpression");
        }
    } else {
        assert!(false, "Expected ExpressionStatement");
    }

    Ok(())
}

#[test]
fn test_index_expressions() -> Result<(), Error> {
    let input = "$myArray[1 + 1]";

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program()?;

    parser.check_errors()?;

    assert_eq!(1, program.statements.len());

    if let Statement::Expr(expression) = &program.statements[0] {
        if let Expression::Index(index_expression) = &expression {
            assert_identifier(&index_expression.left, "$myArray")?;

            assert_infix_expression(
                &index_expression.index,
                "1",
                &Token {
                    literal: "+".to_string(),
                    token_type: TokenType::Plus,
                },
                "1",
            )?;
        } else {
            assert!(false, "Expected IndexExpression");
        }
    } else {
        assert!(false, "Expected ExpressionStatement");
    }

    Ok(())
}

#[test]
fn test_return_statements() -> Result<(), Error> {
    let input = "
        return 5;
        return 10;
        return 993322;
    ";

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program()?;
    parser.check_errors()?;

    assert_eq!(3, program.statements.len());

    assert_eq!("return 5", program.statements[0].to_string());
    assert_eq!("return 10", program.statements[1].to_string());
    assert_eq!("return 993322", program.statements[2].to_string());

    Ok(())
}

#[test]
fn test_function_literal_parsing() -> Result<(), Error> {
    let input = "function ($x, $y) { return $x + $y; }";

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program()?;
    parser.check_errors()?;

    assert_eq!(1, program.statements.len());

    if let Statement::Expr(expression) = &program.statements[0] {
        assert_function_literal(&expression)?;
    } else {
        assert!(false, "Expected ExpressionStatement");
    }

    Ok(())
}

#[test]
fn test_integer_literal_expression() -> Result<(), Error> {
    let input = "5;";

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program()?;
    parser.check_errors()?;

    assert_eq!(1, program.statements.len());

    if let Statement::Expr(expression) = &program.statements[0] {
        assert_integer_literal(&expression, 5)?;
    } else {
        assert!(false, "Expected ExpressionStatement");
    }

    Ok(())
}

#[test]
fn test_array_literal_expression() -> Result<(), Error> {
    let input = "[1, 2 * 2, 3 + 3]";

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program()?;
    parser.check_errors()?;

    if let Statement::Expr(expression) = &program.statements[0] {
        if let Expression::Literal(Literal::Array(ArrayLiteral { token: _, elements })) =
            &expression
        {
            assert_eq!(3, elements.len());

            assert_integer_literal(&elements[0], 1)?;
            assert_infix_expression(
                &elements[1],
                "2",
                &Token {
                    literal: "*".to_string(),
                    token_type: TokenType::Asterisk,
                },
                "2",
            )?;
            assert_infix_expression(
                &elements[2],
                "3",
                &Token {
                    literal: "+".to_string(),
                    token_type: TokenType::Plus,
                },
                "3",
            )?;
        } else {
            assert!(false, "Expected ArrayLiteral");
        }
    } else {
        assert!(false, "Expected ExpressionStatement");
    }

    Ok(())
}

#[test]
fn test_operator_precedence_parsing() -> Result<(), Error> {
    let tests = [
        ("-a * b", "((-a) * b)"),
        ("!-a", "(!(-a))"),
        ("a + b + c", "((a + b) + c)"),
        ("a + b - c", "((a + b) - c)"),
        ("a * b * c", "((a * b) * c)"),
        ("a * b / c", "((a * b) / c)"),
        ("a + b / c", "(a + (b / c))"),
        ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
        ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
        ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ),
        ("true", "true"),
        ("false", "false"),
        ("3 > 5 == false", "((3 > 5) == false)"),
        ("3 < 5 == true", "((3 < 5) == true)"),
        ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
        ("(5 + 5) * 2", "((5 + 5) * 2)"),
        ("2 / (5 + 5)", "(2 / (5 + 5))"),
        ("-(5 + 5)", "(-(5 + 5))"),
        ("!(true == true)", "(!(true == true))"),
        ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
        (
            "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        ),
        (
            "add(a + b + c * d / f + g)",
            "add((((a + b) + ((c * d) / f)) + g))",
        ),
    ];

    for (input, expected) in tests.iter() {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program()?;
        parser.check_errors()?;

        assert_eq!(*expected, program.to_string());
    }

    Ok(())
}

#[test]
fn test_prefix_expressions() -> Result<(), Error> {
    let prefix_tests: [(&str, &Token, i64); 2] = [
        (
            "!5;",
            &Token {
                literal: "!".to_string(),
                token_type: TokenType::Bang,
            },
            5,
        ),
        (
            "-15;",
            &Token {
                literal: "-".to_string(),
                token_type: TokenType::Minus,
            },
            15,
        ),
    ];

    for (input, _operator, value) in prefix_tests.iter() {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program()?;
        parser.check_errors()?;

        assert_eq!(1, program.statements.len());

        if let Statement::Expr(expression) = &program.statements[0] {
            assert_prefix_expression(&expression, *_operator, *value)?;
        } else {
            assert!(false, "Expected ExpressionStatement");
        }
    }

    Ok(())
}

#[test]
fn test_strings() -> Result<(), Error> {
    let input = r#""hello world";"#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program()?;
    parser.check_errors()?;

    assert_eq!(1, program.statements.len());

    if let Statement::Expr(expression) = &program.statements[0] {
        assert_string_literal(&expression, "hello world")?;
    } else {
        assert!(false, "Expected ExpressionStatement");
    }

    Ok(())
}

fn assert_string_literal(expression: &Expression, value: &str) -> Result<(), Error> {
    match expression {
        Expression::Literal(Literal::String(string_literal)) => {
            assert_eq!(value, string_literal.value);
        }
        _ => {
            assert!(false, "Expected StringLiteral");
        }
    }

    Ok(())
}

fn assert_let_statement(statement: &Statement, name: &str) -> Result<(), Error> {
    match statement {
        Statement::Assign(variable_assignment) => {
            assert_eq!(name, variable_assignment.name.value);
        }
        _ => {
            assert!(false, "Expected VariableAssignment");
        }
    }

    Ok(())
}

fn assert_identifier(expression: &Expression, value: &str) -> Result<(), Error> {
    match expression {
        Expression::Identifier(identifier) => {
            assert_eq!(value, identifier.value);
        }
        _ => {
            assert!(false, "Expected Identifier");
        }
    }

    Ok(())
}

fn assert_integer_literal(expression: &Expression, value: i64) -> Result<(), Error> {
    match expression {
        Expression::Literal(Literal::Integer(integer_literal)) => {
            assert_eq!(value, integer_literal.value);
        }
        _ => {
            assert!(false, "Expected IntegerLiteral");
        }
    }

    Ok(())
}

fn assert_function_literal(expression: &Expression) -> Result<(), Error> {
    if let Expression::Function(function_literal) = expression {
        assert_eq!(2, function_literal.parameters.len());

        assert_eq!("$x", function_literal.parameters[0].value);
        assert_eq!("$y", function_literal.parameters[1].value);

        if let BlockStatement { statements, .. } = &function_literal.body {
            assert_eq!(1, statements.len());

            match &statements[0] {
                Statement::Return(return_statement) => {
                    assert_infix_expression(
                        &return_statement.return_value,
                        "$x",
                        &Token {
                            literal: "+".to_string(),
                            token_type: TokenType::Plus,
                        },
                        "$y",
                    )?;
                }
                Statement::Expr(expression) => {
                    assert_infix_expression(
                        expression,
                        "$x",
                        &Token {
                            literal: "+".to_string(),
                            token_type: TokenType::Plus,
                        },
                        "$y",
                    )?;
                }
                _ => assert!(false, "Expected ReturnStatement or ExpressionStatement"),
            }
        } else {
            assert!(false, "Expected BlockStatement");
        }
    } else {
        assert!(false, "Expected FunctionLiteral");
    }

    Ok(())
}

fn assert_call_expression(expression: &Expression) -> Result<(), Error> {
    if let Expression::Call(call_expression) = expression {
        assert_eq!(3, call_expression.arguments.len());

        assert_literal_expression(&call_expression.arguments[0], "1")?;

        assert_infix_expression(
            &call_expression.arguments[1],
            "2",
            &Token {
                token_type: TokenType::Asterisk,
                literal: "*".to_string(),
            },
            "3",
        )?;

        assert_infix_expression(
            &call_expression.arguments[2],
            "4",
            &Token {
                token_type: TokenType::Plus,
                literal: "+".to_string(),
            },
            "5",
        )?;
    } else {
        assert!(false, "Expected CallExpression");
    }

    Ok(())
}

fn assert_infix_expression(
    expression: &Expression,
    left_value: &str,
    operator: &Token,
    right_value: &str,
) -> Result<(), Error> {
    match expression {
        Expression::Infix(infix_expression) => {
            assert_literal_expression(&infix_expression.left, left_value)?;
            assert_eq!(*operator, infix_expression.operator);
            assert_literal_expression(&infix_expression.right, right_value)?;
        }
        _ => {
            assert!(false, "Expected InfixExpression");
        }
    }

    Ok(())
}

fn assert_prefix_expression(
    expression: &Expression,
    operator: &Token,
    right_value: i64,
) -> Result<(), Error> {
    match expression {
        Expression::Prefix(prefix_expression) => {
            assert_eq!(*operator, prefix_expression.operator);
            assert_integer_literal(&prefix_expression.right, right_value)?;
        }
        _ => {
            assert!(false, "Expected PrefixExpression");
        }
    }

    Ok(())
}

fn assert_literal_expression(expression: &Expression, expected: &str) -> Result<bool, Error> {
    match expression {
        Expression::Literal(Literal::Integer(integer_literal)) => {
            assert_eq!(expected, integer_literal.value.to_string());
            Ok(true)
        }
        Expression::Identifier(identifier) => {
            assert_eq!(expected, identifier.value);
            Ok(true)
        }
        _ => {
            assert!(false, "Expected IntegerLiteral or Identifier");
            Ok(false)
        }
    }
}
