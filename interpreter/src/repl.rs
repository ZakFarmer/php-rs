use std::rc::Rc;

use anyhow::{anyhow, Error, Result};

use compiler::{jit::Jit, symbol_table::SymbolTable, Compiler};
use inkwell::{builder::Builder, context::Context, module::Module, passes::PassManager};
use lexer::Lexer;

use object::Object;
use parser::{
    ast::{Expression, Identifier, Node, Program, Statement},
    Parser,
};
use rustyline::error::ReadlineError;
use vm::{Vm, GLOBALS_SIZE};

const PROMPT: &str = ">> ";

pub fn init_repl() -> Result<(), Error> {
    let mut rl = rustyline::DefaultEditor::new()?;

    #[cfg(feature = "with-file-history")]
    if rl.load_history("history.txt").is_err() {
        info!("No previous history.");
    }

    println!("php-rs interpreter v{}", env!("CARGO_PKG_VERSION"));

    let previous_expressions = Vec::new();

    loop {
        let readline = rl.readline(format!("{}", PROMPT).as_str());

        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;

                for previous in &previous_expressions {
                    Jit::compile(previous)?;
                }

                let parsed_program = Parser::new(&line).parse_program()?;

                let _llvm_values = Jit::compile(&Node::Program(parsed_program.clone()))?;
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
