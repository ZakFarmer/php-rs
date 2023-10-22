use anyhow::Error;
use compiler::Compiler;
use lexer::Lexer;
use parser::{Parser, ast::Node};
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
    ];

    run_vm_tests(tests)?;

    Ok(())
}