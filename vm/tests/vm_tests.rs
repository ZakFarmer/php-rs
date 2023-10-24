use anyhow::Error;
use compiler::Compiler;
use lexer::Lexer;
use parser::{ast::Node, Parser};
use vm::Vm;

struct VmTestCase {
    input: String,
    expected: String,
}

fn run_vm_tests(tests: Vec<VmTestCase>) -> Result<(), Error> {
    for test in tests {
        let mut parser = Parser::new(Lexer::new(&test.input));

        let program = parser.parse_program()?;
        let mut compiler = Compiler::new();

        let bytecode = compiler.compile(&Node::Program(program))?;

        let mut vm = Vm::new(bytecode);

        vm.run()?;

        let stack_elem = vm.last_popped_stack_elem();

        assert_eq!(stack_elem.to_string(), test.expected);
    }

    Ok(())
}

#[test]
fn test_boolean_expressions() -> Result<(), Error> {
    let tests = vec![
        VmTestCase {
            input: "true".to_string(),
            expected: "true".to_string(),
        },
        VmTestCase {
            input: "false".to_string(),
            expected: "false".to_string(),
        },
        VmTestCase {
            input: "1 < 2".to_string(),
            expected: "true".to_string(),
        },
        VmTestCase {
            input: "1 > 2".to_string(),
            expected: "false".to_string(),
        },
        VmTestCase {
            input: "1 < 1".to_string(),
            expected: "false".to_string(),
        },
        VmTestCase {
            input: "1 > 1".to_string(),
            expected: "false".to_string(),
        },
        VmTestCase {
            input: "1 == 1".to_string(),
            expected: "true".to_string(),
        },
        VmTestCase {
            input: "1 != 1".to_string(),
            expected: "false".to_string(),
        },
        VmTestCase {
            input: "1 == 2".to_string(),
            expected: "false".to_string(),
        },
        VmTestCase {
            input: "1 != 2".to_string(),
            expected: "true".to_string(),
        },
        VmTestCase {
            input: "true == true".to_string(),
            expected: "true".to_string(),
        },
        VmTestCase {
            input: "false == false".to_string(),
            expected: "true".to_string(),
        },
        VmTestCase {
            input: "true == false".to_string(),
            expected: "false".to_string(),
        },
        VmTestCase {
            input: "true != false".to_string(),
            expected: "true".to_string(),
        },
        VmTestCase {
            input: "false != true".to_string(),
            expected: "true".to_string(),
        },
        VmTestCase {
            input: "!true".to_string(),
            expected: "false".to_string(),
        },
        VmTestCase {
            input: "!false".to_string(),
            expected: "true".to_string(),
        },
        VmTestCase {
            input: "!5".to_string(),
            expected: "false".to_string(),
        },
        VmTestCase {
            input: "!!true".to_string(),
            expected: "true".to_string(),
        },
        VmTestCase {
            input: "!!false".to_string(),
            expected: "false".to_string(),
        },
        VmTestCase {
            input: "!!5".to_string(),
            expected: "true".to_string(),
        },
        VmTestCase {
            input: "!(if (false) { 5; })".to_string(),
            expected: "true".to_string(),
        }
    ];

    run_vm_tests(tests)?;

    Ok(())
}

#[test]
fn test_conditionals() -> Result<(), Error> {
    let tests = vec![
        VmTestCase {
            input: "if (true) { 10 }".to_string(),
            expected: "10".to_string(),
        },
        VmTestCase {
            input: "if (true) { 10 } else { 20 }".to_string(),
            expected: "10".to_string(),
        },
        VmTestCase {
            input: "if (false) { 10 } else { 20 }".to_string(),
            expected: "20".to_string(),
        },
        VmTestCase {
            input: "if (1) { 10 }".to_string(),
            expected: "10".to_string(),
        },
        VmTestCase {
            input: "if (1 < 2) { 10 }".to_string(),
            expected: "10".to_string(),
        },
        VmTestCase {
            input: "if (1 < 2) { 10 } else { 20 }".to_string(),
            expected: "10".to_string(),
        },
        VmTestCase {
            input: "if (1 > 2) { 10 } else { 20 }".to_string(),
            expected: "20".to_string(),
        },
        VmTestCase {
            input: "if (1 > 2) { 10 }".to_string(),
            expected: "null".to_string(),
        },
        VmTestCase {
            input: "if (false) { 10 }".to_string(),
            expected: "null".to_string(),
        },
    ];

    run_vm_tests(tests)?;

    Ok(())
}

#[test]
fn test_integer_arithmetic() -> Result<(), Error> {
    let tests = vec![
        VmTestCase {
            input: "1".to_string(),
            expected: "1".to_string(),
        },
        VmTestCase {
            input: "2".to_string(),
            expected: "2".to_string(),
        },
        VmTestCase {
            input: "1 + 2".to_string(),
            expected: "3".to_string(),
        },
        VmTestCase {
            input: "1 - 2".to_string(),
            expected: "-1".to_string(),
        },
        VmTestCase {
            input: "1 * 2".to_string(),
            expected: "2".to_string(),
        },
        VmTestCase {
            input: "4 / 2".to_string(),
            expected: "2".to_string(),
        },
        VmTestCase {
            input: "50 / 2 * 2 + 10 - 5".to_string(),
            expected: "55".to_string(),
        },
        VmTestCase {
            input: "-5".to_string(),
            expected: "-5".to_string(),
        },
        VmTestCase {
            input: "-10".to_string(),
            expected: "-10".to_string(),
        },
        VmTestCase {
            input: "-50 + 100 + -50".to_string(),
            expected: "0".to_string(),
        },
        VmTestCase {
            input: "(5 + 10 * 2 + 15 / 3) * 2 + -10".to_string(),
            expected: "50".to_string(),
        },
    ];

    run_vm_tests(tests)?;

    Ok(())
}
