use anyhow::Error;
use compiler::{jit::{Jit, JitValue}, Compiler};
use inkwell::context::Context;
use parser::{ast::Node, Parser};

#[test]
fn test_assign_statements() -> Result<(), Error> {
    let tests = vec![
        ("$a = 5; $a;", JitValue::Int(5)),
        ("$a = 5 * 5; $a;", JitValue::Int(25)),
        ("$a = 5; $b = $a; $b;", JitValue::Int(5)),
        ("$a = 5; $b = $a; $c = $a + $b + 5; $c;", JitValue::Int(15)),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input)?;

        assert_eq!(evaluated, expected.as_int().unwrap());
    }

    Ok(())
}

#[test]
fn test_integer_arithmetic() -> Result<(), Error> {
    let tests = vec![
        ("1", JitValue::Int(1)),
        ("2", JitValue::Int(2)),
        ("1 + 2", JitValue::Int(3)),
        ("1 - 2", JitValue::Int(-1)),
        ("1 * 2", JitValue::Int(2)),
        ("4 / 2", JitValue::Int(2)),
        ("50 / 2 * 2 + 10 - 5", JitValue::Int(55)),
        ("5 + 5 + 5 + 5 - 10", JitValue::Int(10)),
        ("2 * 2 * 2 * 2 * 2", JitValue::Int(32)),
        ("5 * 2 + 10", JitValue::Int(20)),
        ("5 + 2 * 10", JitValue::Int(25)),
        ("5 * (2 + 10)", JitValue::Int(60)),
        ("-5", JitValue::Int(-5)),
        ("-10", JitValue::Int(-10)),
        ("-50 + 100 + -50", JitValue::Int(0)),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input)?;

        assert_eq!(evaluated, expected.as_int().unwrap());
    }

    Ok(())
}

// #[test]
// fn test_strings() -> Result<(), Error> {
//     let tests = vec![
//         (r#""Hello World!""#, JitValue::Str("Hello World!".to_string())),
//         (r#""Hello" . " " . "World!""#, JitValue::Str("Hello World!".to_string())),
//         (r#""Hello" . " " . "World!" . " " . "from" . " " . "php-rs""#, JitValue::Str("Hello World! from php-rs".to_string())),
//     ];

//     for (input, expected) in tests {
//         let evaluated = test_eval(input)?;

//         assert_eq!(evaluated, expected);
//     }

//     Ok(())
// }

fn test_eval(input: &str) -> Result<i32, Error> {
    let mut parser = Parser::new(input);
    let program = parser.parse_program().unwrap();

    let context = Context::create();
    let mut compiler = Compiler::new(&context);

    compiler.compile(&Node::Program(program))
}
