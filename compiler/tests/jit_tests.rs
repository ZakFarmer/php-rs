use anyhow::Error;
use compiler::jit::Jit;
use parser::{ast::Node, Parser};

#[test]
fn test_integer_arithmetic() -> Result<(), Error> {
    let tests = vec![
        ("1", 1),
        ("2", 2),
        ("1 + 2", 3),
        ("1 - 2", -1),
        ("1 * 2", 2),
        ("4 / 2", 2),
        ("50 / 2 * 2 + 10 - 5", 55),
        ("5 + 5 + 5 + 5 - 10", 10),
        ("2 * 2 * 2 * 2 * 2", 32),
        ("5 * 2 + 10", 20),
        ("5 + 2 * 10", 25),
        ("5 * (2 + 10)", 60),
        ("-5", -5),
        ("-10", -10),
        ("-50 + 100 + -50", 0),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input)?;

        assert_eq!(evaluated, expected);
    }

    Ok(())
}

fn test_eval(input: &str) -> Result<i32, Error> {
    let mut parser = Parser::new(input);
    let program = parser.parse_program().unwrap();

    Jit::compile(&Node::Program(program))
}
