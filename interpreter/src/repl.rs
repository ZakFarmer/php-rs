use anyhow::{Error, Result};

use inkwell::context::Context;
use parser::{ast::Node, Parser};
use rustyline::error::ReadlineError;

const PROMPT: &str = ">> ";

pub fn init_repl() -> Result<(), Error> {
    let mut rl = rustyline::DefaultEditor::new()?;

    #[cfg(feature = "with-file-history")]
    if rl.load_history("history.txt").is_err() {
        info!("No previous history.");
    }

    println!("php-rs interpreter v{}", env!("CARGO_PKG_VERSION"));

    loop {
        let readline = rl.readline(format!("{}", PROMPT).as_str());

        match readline {
            Ok(line) => {
                // let context = Context::create();
                // let mut compiler = Compiler::new(&context);

                rl.add_history_entry(line.as_str())?;

                let parsed_program = Parser::new(&line).parse_program()?;

                // let value = compiler.compile(&Node::Program(parsed_program))?;

                // println!("{}", value);
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
