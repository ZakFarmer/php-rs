use std::{borrow::Borrow, rc::Rc};

use anyhow::Error;
use compiler::Compiler;
use lexer::Lexer;
use object::Object;
use parser::{ast::Node, Parser};
use vm::Vm;

struct VmTestCase {
    input: String,
    expected: Object,
}

fn run_vm_tests(tests: Vec<VmTestCase>) -> Result<(), Error> {
    for test in tests {
        let mut parser = Parser::new(Lexer::new(&test.input));

        let program = parser.parse_program()?;

        let mut compiler = Compiler::new();

        let bytecode = compiler.compile(&Node::Program(program))?;

        let mut vm = Vm::new(bytecode);

        vm.run()?;

        let expected = test.expected;
        let stack_elem = vm.last_popped_stack_elem();

        assert_constants(&vec![expected], &vec![stack_elem]);
    }

    Ok(())
}

#[test]
fn test_array_expressions() -> Result<(), Error> {
    let tests = vec![
        VmTestCase {
            input: "[]".to_string(),
            expected: Object::Array(vec![]),
        },
        VmTestCase {
            input: "[1, 2, 3]".to_string(),
            expected: Object::Array(vec![
                Object::Integer(1).into(),
                Object::Integer(2).into(),
                Object::Integer(3).into(),
            ]),
        },
        VmTestCase {
            input: "[1 + 2, 3 * 4, 5 + 6]".to_string(),
            expected: Object::Array(vec![
                Object::Integer(3).into(),
                Object::Integer(12).into(),
                Object::Integer(11).into(),
            ]),
        },
    ];

    run_vm_tests(tests)?;

    Ok(())
}

#[test]
fn test_boolean_expressions() -> Result<(), Error> {
    let tests = vec![
        VmTestCase {
            input: "true".to_string(),
            expected: Object::Boolean(true),
        },
        VmTestCase {
            input: "false".to_string(),
            expected: Object::Boolean(false),
        },
        VmTestCase {
            input: "1 < 2".to_string(),
            expected: Object::Boolean(true),
        },
        VmTestCase {
            input: "1 > 2".to_string(),
            expected: Object::Boolean(false),
        },
        VmTestCase {
            input: "1 < 1".to_string(),
            expected: Object::Boolean(false),
        },
        VmTestCase {
            input: "1 > 1".to_string(),
            expected: Object::Boolean(false),
        },
        VmTestCase {
            input: "1 == 1".to_string(),
            expected: Object::Boolean(true),
        },
        VmTestCase {
            input: "1 != 1".to_string(),
            expected: Object::Boolean(false),
        },
        VmTestCase {
            input: "1 == 2".to_string(),
            expected: Object::Boolean(false),
        },
        VmTestCase {
            input: "1 != 2".to_string(),
            expected: Object::Boolean(true),
        },
        VmTestCase {
            input: "true == true".to_string(),
            expected: Object::Boolean(true),
        },
        VmTestCase {
            input: "false == false".to_string(),
            expected: Object::Boolean(true),
        },
        VmTestCase {
            input: "true == false".to_string(),
            expected: Object::Boolean(false),
        },
        VmTestCase {
            input: "true != false".to_string(),
            expected: Object::Boolean(true),
        },
        VmTestCase {
            input: "false != true".to_string(),
            expected: Object::Boolean(true),
        },
        VmTestCase {
            input: "!true".to_string(),
            expected: Object::Boolean(false),
        },
        VmTestCase {
            input: "!false".to_string(),
            expected: Object::Boolean(true),
        },
        VmTestCase {
            input: "!5".to_string(),
            expected: Object::Boolean(false),
        },
        VmTestCase {
            input: "!!true".to_string(),
            expected: Object::Boolean(true),
        },
        VmTestCase {
            input: "!!false".to_string(),
            expected: Object::Boolean(false),
        },
        VmTestCase {
            input: "!!5".to_string(),
            expected: Object::Boolean(true),
        },
        VmTestCase {
            input: "!(if (false) { 5; })".to_string(),
            expected: Object::Boolean(true),
        },
    ];

    run_vm_tests(tests)?;

    Ok(())
}

#[test]
fn test_conditionals() -> Result<(), Error> {
    let tests = vec![
        VmTestCase {
            input: "if (true) { 10 }".to_string(),
            expected: Object::Integer(10),
        },
        VmTestCase {
            input: "if (true) { 10 } else { 20 }".to_string(),
            expected: Object::Integer(10),
        },
        VmTestCase {
            input: "if (false) { 10 } else { 20 }".to_string(),
            expected: Object::Integer(20),
        },
        VmTestCase {
            input: "if (1) { 10 }".to_string(),
            expected: Object::Integer(10),
        },
        VmTestCase {
            input: "if (1 < 2) { 10 }".to_string(),
            expected: Object::Integer(10),
        },
        VmTestCase {
            input: "if (1 < 2) { 10 } else { 20 }".to_string(),
            expected: Object::Integer(10),
        },
        VmTestCase {
            input: "if (1 > 2) { 10 } else { 20 }".to_string(),
            expected: Object::Integer(20),
        },
        VmTestCase {
            input: "if (1 > 2) { 10 }".to_string(),
            expected: Object::Null,
        },
        VmTestCase {
            input: "if (false) { 10 }".to_string(),
            expected: Object::Null,
        },
    ];

    run_vm_tests(tests)?;

    Ok(())
}

#[test]
fn test_functions_with_no_arguments() -> Result<(), Error> {
    let tests = vec![
        VmTestCase {
            input: "$x = function () { 50 * 2; }; $x();".to_string(),
            expected: Object::Integer(100),
        },
        VmTestCase {
            input: "$one = function () { 1; }; $two = function () { 2; }; $one() + $two();"
                .to_string(),
            expected: Object::Integer(3),
        },
        VmTestCase {
            input: "$a = function () { 1; }; $b = function () { $a() + 1; }; $c = function () { $b() + 1; }; $c();"
                .to_string(),
            expected: Object::Integer(3),
        },
    ];

    run_vm_tests(tests)?;

    Ok(())
}

#[test]
fn test_functions_with_arguments() -> Result<(), Error> {
    let tests = vec![
        VmTestCase {
            input: "$identity = function ($x) { $x; }; $identity(4);".to_string(),
            expected: Object::Integer(4),
        },
        VmTestCase {
            input: "$identity = function ($x) { return $x; }; $identity(4);".to_string(),
            expected: Object::Integer(4),
        },
        VmTestCase {
            input: "$double = function ($x) { $x * 2; }; $double(4);".to_string(),
            expected: Object::Integer(8),
        },
        VmTestCase {
            input: "$add = function ($x, $y) { $x + $y; }; $add(4, 5);".to_string(),
            expected: Object::Integer(9),
        },
    ];

    run_vm_tests(tests)?;

    Ok(())
}

#[test]
fn test_functions_with_no_return_value() -> Result<(), Error> {
    let tests = vec![VmTestCase {
        input: "$x = function () { }; $x();".to_string(),
        expected: Object::Null,
    }];

    run_vm_tests(tests)?;

    Ok(())
}

#[test]
fn test_first_class_functions() -> Result<(), Error> {
    let tests = vec![VmTestCase {
        input: "$returnsOne = function () { 1; }; 
                    $returnsOneReturner = function () { $returnsOne; };
                    $returnsOneReturner()();"
            .to_string(),
        expected: Object::Integer(1),
    }];

    run_vm_tests(tests)?;

    Ok(())
}

#[test]
fn test_integer_arithmetic() -> Result<(), Error> {
    let tests = vec![
        VmTestCase {
            input: "1".to_string(),
            expected: Object::Integer(1),
        },
        VmTestCase {
            input: "2".to_string(),
            expected: Object::Integer(2),
        },
        VmTestCase {
            input: "1 + 2".to_string(),
            expected: Object::Integer(3),
        },
        VmTestCase {
            input: "1 - 2".to_string(),
            expected: Object::Integer(-1),
        },
        VmTestCase {
            input: "1 * 2".to_string(),
            expected: Object::Integer(2),
        },
        VmTestCase {
            input: "4 / 2".to_string(),
            expected: Object::Integer(2),
        },
        VmTestCase {
            input: "50 / 2 * 2 + 10 - 5".to_string(),
            expected: Object::Integer(55),
        },
        VmTestCase {
            input: "-5".to_string(),
            expected: Object::Integer(-5),
        },
        VmTestCase {
            input: "-10".to_string(),
            expected: Object::Integer(-10),
        },
        VmTestCase {
            input: "-50 + 100 + -50".to_string(),
            expected: Object::Integer(0),
        },
        VmTestCase {
            input: "(5 + 10 * 2 + 15 / 3) * 2 + -10".to_string(),
            expected: Object::Integer(50),
        },
    ];

    run_vm_tests(tests)?;

    Ok(())
}

#[test]
fn test_global_dollar_statements() -> Result<(), Error> {
    let tests = vec![
        VmTestCase {
            input: "$one = 1; $one".to_string(),
            expected: Object::Integer(1),
        },
        VmTestCase {
            input: "$one = 1; $two = 2; $one + $two".to_string(),
            expected: Object::Integer(3),
        },
        VmTestCase {
            input: "$one = 1; $two = $one + $one; $one + $two".to_string(),
            expected: Object::Integer(3),
        },
    ];

    run_vm_tests(tests)?;

    Ok(())
}

#[test]
fn test_index_expressions() -> Result<(), Error> {
    let tests = vec![
        VmTestCase {
            input: "[1, 2, 3][1]".to_string(),
            expected: Object::Integer(2),
        },
        VmTestCase {
            input: "[1, 2, 3][0 + 2]".to_string(),
            expected: Object::Integer(3),
        },
        VmTestCase {
            input: "[[1, 1, 1]][0][0]".to_string(),
            expected: Object::Integer(1),
        },
    ];

    run_vm_tests(tests)?;

    Ok(())
}

#[test]
fn test_string_expressions() -> Result<(), Error> {
    let tests = vec![
        VmTestCase {
            input: r#""hello""#.to_string(),
            expected: Object::String("hello".to_string()),
        },
        VmTestCase {
            input: r#""hello" + "world""#.to_string(),
            expected: Object::String("helloworld".to_string()),
        },
        VmTestCase {
            input: r#""hello" + "world" + "!""#.to_string(),
            expected: Object::String("helloworld!".to_string()),
        },
    ];

    run_vm_tests(tests)?;

    Ok(())
}

#[test]
fn test_variable_scopes() -> Result<(), Error> {
    let tests = vec![
        VmTestCase {
            input: "$one = 1; $two = 2; $one + $two".to_string(),
            expected: Object::Integer(3),
        },
        VmTestCase {
            input: "$one = 1; $one".to_string(),
            expected: Object::Integer(1),
        },
        VmTestCase {
            input: "$one = 1; $two = $one; $two".to_string(),
            expected: Object::Integer(1),
        },
        VmTestCase {
            input: "$one = 1; $two = $one; $three = $one + $two + 3; $three".to_string(),
            expected: Object::Integer(5),
        },
        VmTestCase {
            input: "$num = 50; function () { $num }();".to_string(),
            expected: Object::Integer(50),
        },
    ];

    run_vm_tests(tests)?;

    Ok(())
}

// TODO: remove duplication and extract helper function for use in multiple tests
fn assert_constants(expected: &Vec<Object>, actual: &Vec<Rc<Object>>) {
    assert_eq!(expected.len(), actual.len());
    for (exp, b_got) in expected.iter().zip(actual) {
        let got = b_got.borrow();
        assert_eq!(exp, got);
    }
}
