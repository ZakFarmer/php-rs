use std::rc::Rc;

use anyhow::{anyhow, Error, Result};

use codegen::Codegen;
use compiler::{symbol_table::SymbolTable, Compiler};
use inkwell::{context::Context, passes::PassManager};
use lexer::Lexer;

use object::Object;
use parser::{ast::{Node, Statement, Expression, Program, Identifier}, Parser};
use rustyline::error::ReadlineError;
use vm::{Vm, GLOBALS_SIZE};

const PROMPT: &str = ">> ";

pub fn init_repl() -> Result<(), Error> {
    let mut rl = rustyline::DefaultEditor::new()?;

    let context = Context::create();
    let module = context.create_module("main");
    let builder = context.create_builder();

    let fpm = PassManager::create(&module);

    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.add_gvn_pass();
    fpm.add_cfg_simplification_pass();
    fpm.add_basic_alias_analysis_pass();
    fpm.add_promote_memory_to_register_pass();
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();

    fpm.initialize();

    #[cfg(feature = "with-file-history")]
    if rl.load_history("history.txt").is_err() {
        info!("No previous history.");
    }

    println!("php-rs interpreter v{}", env!("CARGO_PKG_VERSION"));

    let mut previous_expressions = Vec::new();

    loop {
        let readline = rl.readline(format!("{}", PROMPT).as_str());

        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;

                for previous in &previous_expressions {
                    Codegen::compile(&builder, &context, &fpm, module.clone(), previous)?;
                }

                let parsed_program = Parser::new(&line).parse_program()?;

                let _llvm_values = Codegen::compile(
                    &builder, 
                    &context, 
                    &fpm, 
                    module.clone(), 
                    &Node::Program(parsed_program.clone())
                )?;

                let name = match &parsed_program {
                    Program { statements } => match statements.first() {
                        Some(statement) => match statement {
                            Statement::Assign(assign_statement) => match assign_statement.name {
                                Identifier { token: ref identifier, .. } => identifier.value.clone(),
                                _ => {
                                    return Err(Error::msg(format!(
                                        "Expected identifier, got: {:?}",
                                        assign_statement.name
                                    )));
                                }
                            },
                            _ => "".to_string(),
                        },
                        None => return Err(Error::msg("Expected statement".to_string())),
                    },
                    _ => return Err(Error::msg("Expected program".to_string())),
                };
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
