use std::rc::Rc;

use anyhow::{Error, Result};

use compiler::{symbol_table::SymbolTable, Compiler};
use lexer::Lexer;

use object::Object;
use parser::{ast::Node, Parser};
use rustyline::error::ReadlineError;
use vm::{Vm, GLOBALS_SIZE};

const PROMPT: &str = ">> ";

pub fn init_repl() -> Result<(), Error> {
    let mut rl = rustyline::DefaultEditor::new()?;

    let mut constants = vec![];
    let mut globals = vec![Rc::new(Object::Null); GLOBALS_SIZE];
    let mut symbol_table = SymbolTable::new();

    #[cfg(feature = "with-file-history")]
    if rl.load_history("history.txt").is_err() {
        info!("No previous history.");
    }

    println!("php-rs interpreter v{}", env!("CARGO_PKG_VERSION"));

    loop {
        let readline = rl.readline(format!("{}", PROMPT).as_str());

        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;

                let lexer = Lexer::new(&line);
                let mut parser = Parser::new(lexer);

                let program = parser.parse_program()?;
                parser.check_errors()?;

                let mut compiler = Compiler::new_with_state(constants, symbol_table);

                match compiler.compile(&Node::Program(program)) {
                    Ok(bytecode) => {
                        let mut vm = Vm::new_with_globals_store(bytecode, globals);

                        match vm.run() {
                            Ok(_) => {
                                let last_popped = vm.last_popped_stack_elem();
                                println!("{}", last_popped);
                            }
                            Err(err) => {
                                println!("Error: {}", err);
                            }
                        }

                        globals = vm.globals;
                    }
                    Err(err) => {
                        println!("Compilation failed: {}", err);
                    }
                }

                symbol_table = compiler.symbol_table;
                constants = compiler.constants;
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
            }
        }
    }

    Ok(())
}
