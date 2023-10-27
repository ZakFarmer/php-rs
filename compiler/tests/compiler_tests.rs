use std::{borrow::Borrow, rc::Rc};

use anyhow::Error;
use compiler::Compiler;
use lexer::Lexer;
use object::Object;
use opcode::{concat_instructions, Instructions};
use parser::ast::Node;

struct CompilerTestCase {
    input: String,
    expected_constants: Vec<Object>,
    expected_instructions: Vec<opcode::Instructions>,
}

#[test]
fn test_array_expressions() -> Result<(), Error> {
    let tests = vec![
        CompilerTestCase {
            input: "[]".to_string(),
            expected_constants: vec![],
            expected_instructions: vec![
                opcode::make(opcode::Opcode::OpArray, &vec![0]),
                opcode::make(opcode::Opcode::OpPop, &vec![]),
            ],
        },
        CompilerTestCase {
            input: "[1, 2, 3]".to_string(),
            expected_constants: vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::Integer(3),
            ],
            expected_instructions: vec![
                opcode::make(opcode::Opcode::OpConst, &vec![0]),
                opcode::make(opcode::Opcode::OpConst, &vec![1]),
                opcode::make(opcode::Opcode::OpConst, &vec![2]),
                opcode::make(opcode::Opcode::OpArray, &vec![3]),
                opcode::make(opcode::Opcode::OpPop, &vec![]),
            ],
        },
        CompilerTestCase {
            input: "[1 + 2, 3 - 4, 5 * 6]".to_string(),
            expected_constants: vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::Integer(3),
                Object::Integer(4),
                Object::Integer(5),
                Object::Integer(6),
            ],
            expected_instructions: vec![
                opcode::make(opcode::Opcode::OpConst, &vec![0]),
                opcode::make(opcode::Opcode::OpConst, &vec![1]),
                opcode::make(opcode::Opcode::OpAdd, &vec![1]),
                opcode::make(opcode::Opcode::OpConst, &vec![2]),
                opcode::make(opcode::Opcode::OpConst, &vec![3]),
                opcode::make(opcode::Opcode::OpSub, &vec![1]),
                opcode::make(opcode::Opcode::OpConst, &vec![4]),
                opcode::make(opcode::Opcode::OpConst, &vec![5]),
                opcode::make(opcode::Opcode::OpMul, &vec![1]),
                opcode::make(opcode::Opcode::OpArray, &vec![3]),
                opcode::make(opcode::Opcode::OpPop, &vec![]),
            ],
        },
    ];

    run_compiler_tests(tests)?;

    Ok(())
}

#[test]
fn test_boolean_expressions() -> Result<(), Error> {
    let tests = vec![
        CompilerTestCase {
            input: "true".to_string(),
            expected_constants: vec![],
            expected_instructions: vec![opcode::make(opcode::Opcode::OpTrue, &vec![])],
        },
        CompilerTestCase {
            input: "false".to_string(),
            expected_constants: vec![],
            expected_instructions: vec![opcode::make(opcode::Opcode::OpFalse, &vec![])],
        },
        CompilerTestCase {
            input: "1 > 2".to_string(),
            expected_constants: vec![Object::Integer(1), Object::Integer(2)],
            expected_instructions: vec![
                opcode::make(opcode::Opcode::OpConst, &vec![0]),
                opcode::make(opcode::Opcode::OpConst, &vec![1]),
                opcode::make(opcode::Opcode::OpGreaterThan, &vec![]),
            ],
        },
        CompilerTestCase {
            input: "1 < 2".to_string(),
            expected_constants: vec![Object::Integer(2), Object::Integer(1)],
            expected_instructions: vec![
                opcode::make(opcode::Opcode::OpConst, &vec![0]),
                opcode::make(opcode::Opcode::OpConst, &vec![1]),
                opcode::make(opcode::Opcode::OpGreaterThan, &vec![]),
            ],
        },
        CompilerTestCase {
            input: "1 == 2".to_string(),
            expected_constants: vec![Object::Integer(1), Object::Integer(2)],
            expected_instructions: vec![
                opcode::make(opcode::Opcode::OpConst, &vec![0]),
                opcode::make(opcode::Opcode::OpConst, &vec![1]),
                opcode::make(opcode::Opcode::OpEqual, &vec![]),
            ],
        },
        CompilerTestCase {
            input: "1 != 2".to_string(),
            expected_constants: vec![Object::Integer(1), Object::Integer(2)],
            expected_instructions: vec![
                opcode::make(opcode::Opcode::OpConst, &vec![0]),
                opcode::make(opcode::Opcode::OpConst, &vec![1]),
                opcode::make(opcode::Opcode::OpNotEqual, &vec![]),
            ],
        },
        CompilerTestCase {
            input: "true == false".to_string(),
            expected_constants: vec![],
            expected_instructions: vec![
                opcode::make(opcode::Opcode::OpTrue, &vec![]),
                opcode::make(opcode::Opcode::OpFalse, &vec![]),
                opcode::make(opcode::Opcode::OpEqual, &vec![]),
            ],
        },
        CompilerTestCase {
            input: "true != false".to_string(),
            expected_constants: vec![],
            expected_instructions: vec![
                opcode::make(opcode::Opcode::OpTrue, &vec![]),
                opcode::make(opcode::Opcode::OpFalse, &vec![]),
                opcode::make(opcode::Opcode::OpNotEqual, &vec![]),
            ],
        },
        CompilerTestCase {
            input: "!true".to_string(),
            expected_constants: vec![],
            expected_instructions: vec![
                opcode::make(opcode::Opcode::OpTrue, &vec![]),
                opcode::make(opcode::Opcode::OpBang, &vec![]),
            ],
        },
    ];

    run_compiler_tests(tests)?;

    Ok(())
}

#[test]
fn test_compilation_scopes() -> Result<(), Error> {
    let mut compiler = Compiler::new();

    compiler.emit(opcode::Opcode::OpMul, vec![]);

    {
        compiler.enter_scope();

        assert_eq!(compiler.scope_index(), 1);

        compiler.emit(opcode::Opcode::OpSub, vec![]);

        assert_eq!(compiler.scopes()[compiler.scope_index()].instructions.0.len(), 1);

        let last = compiler.scopes()[compiler.scope_index()].last_instruction;
        assert_eq!(last.opcode, opcode::Opcode::OpSub);

        compiler.exit_scope();

        assert_eq!(compiler.scope_index(), 0);

        compiler.emit(opcode::Opcode::OpAdd, vec![]);

        assert_eq!(compiler.scopes()[compiler.scope_index()].instructions.0.len(), 2);
    }

    let last = compiler.scopes()[compiler.scope_index()].last_instruction;

    assert_eq!(last.opcode, opcode::Opcode::OpAdd);
    assert_eq!(compiler.scopes()[compiler.scope_index()].previous_instruction.opcode, opcode::Opcode::OpMul);

    let previous = compiler.scopes()[compiler.scope_index()].previous_instruction;
    assert_eq!(previous.opcode, opcode::Opcode::OpMul);

    Ok(())
}

#[test]
fn test_conditionals() -> Result<(), Error> {
    let tests = vec![
        CompilerTestCase {
            input: "if (true) { 10 }; 3333;".to_string(),
            expected_constants: vec![Object::Integer(10), Object::Integer(3333)],
            expected_instructions: vec![
                opcode::make(opcode::Opcode::OpTrue, &vec![]),
                opcode::make(opcode::Opcode::OpJumpNotTruthy, &vec![10]),
                opcode::make(opcode::Opcode::OpConst, &vec![0]),
                opcode::make(opcode::Opcode::OpJump, &vec![11]),
                opcode::make(opcode::Opcode::OpNull, &vec![]),
                opcode::make(opcode::Opcode::OpPop, &vec![]),
                opcode::make(opcode::Opcode::OpConst, &vec![1]),
                opcode::make(opcode::Opcode::OpPop, &vec![]),
            ],
        },
        CompilerTestCase {
            input: "if (true) { 10 } else { 20 }; 3333;".to_string(),
            expected_constants: vec![
                Object::Integer(10),
                Object::Integer(20),
                Object::Integer(3333),
            ],
            expected_instructions: vec![
                opcode::make(opcode::Opcode::OpTrue, &vec![]),
                opcode::make(opcode::Opcode::OpJumpNotTruthy, &vec![10]),
                opcode::make(opcode::Opcode::OpConst, &vec![0]),
                opcode::make(opcode::Opcode::OpJump, &vec![13]),
                opcode::make(opcode::Opcode::OpConst, &vec![1]),
                opcode::make(opcode::Opcode::OpPop, &vec![]),
                opcode::make(opcode::Opcode::OpConst, &vec![2]),
                opcode::make(opcode::Opcode::OpPop, &vec![]),
            ],
        },
    ];

    run_compiler_tests(tests)?;

    Ok(())
}

#[test]
fn test_functions() -> Result<(), Error> {
    let tests = vec![
        CompilerTestCase {
            input: "function () { return 5 + 10; }".to_string(),
            expected_constants: vec![
                Object::Integer(5),
                Object::Integer(10),
                Object::CompiledFunction(Rc::new(object::CompiledFunction::new(concat_instructions(&vec![
                        opcode::make(opcode::Opcode::OpConst, &vec![0]),
                        opcode::make(opcode::Opcode::OpConst, &vec![1]),
                        opcode::make(opcode::Opcode::OpAdd, &vec![]),
                        opcode::make(opcode::Opcode::OpReturnValue, &vec![]),
                    ]
                )))),
            ],
            expected_instructions: vec![
                opcode::make(opcode::Opcode::OpConst, &vec![2]),
                opcode::make(opcode::Opcode::OpPop, &vec![]),
            ],
        },
    ];

    run_compiler_tests(tests)?;

    Ok(())

}

#[test]
fn test_functions_with_no_return_value() -> Result<(), Error> {
    let tests = vec![
        CompilerTestCase {
            input: "function() { }".to_string(),
            expected_constants: vec![
                Object::CompiledFunction(Rc::new(object::CompiledFunction::new(concat_instructions(&vec![
                        opcode::make(opcode::Opcode::OpReturn, &vec![]),
                    ]
                )))),
            ],
            expected_instructions: vec![
                opcode::make(opcode::Opcode::OpConst, &vec![0]),
                opcode::make(opcode::Opcode::OpPop, &vec![]),
            ],
        },
    ];

    run_compiler_tests(tests)?;

    Ok(())
}

#[test]
fn test_function_calls() -> Result<(), Error> {
    let tests = vec![
        CompilerTestCase {
            input: "$noArg = function () { return 24; }; $noArg();".to_string(),
            expected_constants: vec![
                Object::Integer(24),
                Object::CompiledFunction(Rc::new(object::CompiledFunction::new(concat_instructions(&vec![
                        opcode::make(opcode::Opcode::OpConst, &vec![0]),
                        opcode::make(opcode::Opcode::OpReturnValue, &vec![]),
                    ]
                )))),
            ],
            expected_instructions: vec![
                opcode::make(opcode::Opcode::OpConst, &vec![1]),
                opcode::make(opcode::Opcode::OpSetGlobal, &vec![0]),
                opcode::make(opcode::Opcode::OpGetGlobal, &vec![0]),
                opcode::make(opcode::Opcode::OpCall, &vec![0]),
                opcode::make(opcode::Opcode::OpPop, &vec![]),
            ],
        },
    ];

    run_compiler_tests(tests)?;

    Ok(())
}

#[test]
fn test_index_expressions() -> Result<(), Error> {
    let tests = vec![
        CompilerTestCase {
            input: "[1, 2, 3][1 + 1]".to_string(),
            expected_constants: vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::Integer(3),
                Object::Integer(1),
                Object::Integer(1),
            ],
            expected_instructions: vec![
                opcode::make(opcode::Opcode::OpConst, &vec![0]),
                opcode::make(opcode::Opcode::OpConst, &vec![1]),
                opcode::make(opcode::Opcode::OpConst, &vec![2]),
                opcode::make(opcode::Opcode::OpArray, &vec![3]),
                opcode::make(opcode::Opcode::OpConst, &vec![3]),
                opcode::make(opcode::Opcode::OpConst, &vec![4]),
                opcode::make(opcode::Opcode::OpAdd, &vec![4]),
                opcode::make(opcode::Opcode::OpIndex, &vec![]),
                opcode::make(opcode::Opcode::OpPop, &vec![]),
            ],
        },
        CompilerTestCase {
            input: "[1, 2, 3][2 - 1]".to_string(),
            expected_constants: vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::Integer(3),
                Object::Integer(2),
                Object::Integer(1),
            ],
            expected_instructions: vec![
                opcode::make(opcode::Opcode::OpConst, &vec![0]),
                opcode::make(opcode::Opcode::OpConst, &vec![1]),
                opcode::make(opcode::Opcode::OpConst, &vec![2]),
                opcode::make(opcode::Opcode::OpArray, &vec![3]),
                opcode::make(opcode::Opcode::OpConst, &vec![3]),
                opcode::make(opcode::Opcode::OpConst, &vec![4]),
                opcode::make(opcode::Opcode::OpSub, &vec![4]),
                opcode::make(opcode::Opcode::OpIndex, &vec![]),
                opcode::make(opcode::Opcode::OpPop, &vec![]),
            ],
        },
    ];

    run_compiler_tests(tests)?;

    Ok(())
}

#[test]
fn test_integer_arithmetic() -> Result<(), Error> {
    let tests = vec![
        CompilerTestCase {
            input: "1 + 2".to_string(),
            expected_constants: vec![Object::Integer(1), Object::Integer(2)],
            expected_instructions: vec![
                opcode::make(opcode::Opcode::OpConst, &vec![0]),
                opcode::make(opcode::Opcode::OpConst, &vec![1]),
                opcode::make(opcode::Opcode::OpAdd, &vec![1]),
                opcode::make(opcode::Opcode::OpPop, &vec![0]),
            ],
        },
        CompilerTestCase {
            input: "2 - 1".to_string(),
            expected_constants: vec![Object::Integer(2), Object::Integer(1)],
            expected_instructions: vec![
                opcode::make(opcode::Opcode::OpConst, &vec![0]),
                opcode::make(opcode::Opcode::OpConst, &vec![1]),
                opcode::make(opcode::Opcode::OpSub, &vec![1]),
                opcode::make(opcode::Opcode::OpPop, &vec![0]),
            ],
        },
        CompilerTestCase {
            input: "2 * 4".to_string(),
            expected_constants: vec![Object::Integer(2), Object::Integer(4)],
            expected_instructions: vec![
                opcode::make(opcode::Opcode::OpConst, &vec![0]),
                opcode::make(opcode::Opcode::OpConst, &vec![1]),
                opcode::make(opcode::Opcode::OpMul, &vec![1]),
                opcode::make(opcode::Opcode::OpPop, &vec![0]),
            ],
        },
        CompilerTestCase {
            input: "4 / 2".to_string(),
            expected_constants: vec![Object::Integer(4), Object::Integer(2)],
            expected_instructions: vec![
                opcode::make(opcode::Opcode::OpConst, &vec![0]),
                opcode::make(opcode::Opcode::OpConst, &vec![1]),
                opcode::make(opcode::Opcode::OpDiv, &vec![1]),
                opcode::make(opcode::Opcode::OpPop, &vec![0]),
            ],
        },
        CompilerTestCase {
            input: "-1".to_string(),
            expected_constants: vec![Object::Integer(1)],
            expected_instructions: vec![
                opcode::make(opcode::Opcode::OpConst, &vec![0]),
                opcode::make(opcode::Opcode::OpMinus, &vec![]),
                opcode::make(opcode::Opcode::OpPop, &vec![0]),
            ],
        },
    ];

    run_compiler_tests(tests)?;

    Ok(())
}

#[test]
fn test_string_expressions() -> Result<(), Error> {
    let tests = vec![
        CompilerTestCase {
            input: "\"hello\"".to_string(),
            expected_constants: vec![Object::String("hello".to_string())],
            expected_instructions: vec![
                opcode::make(opcode::Opcode::OpConst, &vec![0]),
                opcode::make(opcode::Opcode::OpPop, &vec![]),
            ],
        },
        CompilerTestCase {
            input: "\"hello\" + \"world\"".to_string(),
            expected_constants: vec![
                Object::String("hello".to_string()),
                Object::String("world".to_string()),
            ],
            expected_instructions: vec![
                opcode::make(opcode::Opcode::OpConst, &vec![0]),
                opcode::make(opcode::Opcode::OpConst, &vec![1]),
                opcode::make(opcode::Opcode::OpAdd, &vec![1]),
                opcode::make(opcode::Opcode::OpPop, &vec![0]),
            ]
        }
    ];

    run_compiler_tests(tests)?;

    Ok(())
}

fn run_compiler_tests(tests: Vec<CompilerTestCase>) -> Result<(), Error> {
    for test in tests {
        let mut parser = parser::Parser::new(Lexer::new(&test.input));

        let program = parser.parse_program()?;
        let mut compiler = Compiler::new();

        let bytecode = compiler.compile(&Node::Program(program))?;

        println!("Testing input: {}", test.input.to_string());
        println!("Constants: {:?}", bytecode.constants);

        test_constants(&test.expected_constants, &bytecode.constants);
        test_instructions(&test.expected_instructions, &bytecode.instructions);
    }

    Ok(())
}

pub fn test_constants(expected: &Vec<Object>, actual: &Vec<Rc<Object>>) {
    assert_eq!(expected.len(), actual.len());
    for (exp, b_got) in expected.iter().zip(actual) {
        let got = b_got.borrow();
        assert_eq!(exp, got);
    }
}

fn test_instructions(expected: &Vec<opcode::Instructions>, actual: &opcode::Instructions) {
    let expected_instructions = concat_instructions(expected);

    for (&exp, got) in expected_instructions.0.iter().zip(actual.0.clone()) {
        assert_eq!(
            exp,
            got,
            "instruction not equal\n actual  : \n{}\n expected: \n{}",
            actual.to_string(),
            expected_instructions.to_string()
        );
    }
}
