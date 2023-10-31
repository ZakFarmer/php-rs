use anyhow::Error;
use compiler::jit::Jit;
use parser::{ast::Node, Parser};

#[test]
fn test_integer_arithmetic() -> Result<(), Error> {
    let tests = vec![("1", 1), ("2", 2)];

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
