use std::{cell::RefCell, rc::Rc};

use anyhow::{Error, Result};

use lexer::Lexer;
use object::environment::Environment;
use parser::Parser;
use rustyline::error::ReadlineError;

const PROMPT: &str = ">> ";

pub fn init_repl() -> Result<(), Error> {
    let mut rl = rustyline::DefaultEditor::new()?;

    #[cfg(feature = "with-file-history")]
    if rl.load_history("history.txt").is_err() {
        info!("No previous history.");
    }

    println!("php-rs interpreter v{}", env!("CARGO_PKG_VERSION"));

    let env = Rc::new(RefCell::new(Environment::new()));

    loop {
        let readline = rl.readline(format!("{}", PROMPT).as_str());

        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;

                let result: Result<(), Error> = (|| {
                    let lexer = Lexer::new(&line);
                    let mut parser = Parser::new(lexer);

                    let program = parser.parse_program()?;

                    parser.check_errors()?;

                    let evaluated = evaluator::eval_statements(&program.statements, &env)?;

                    println!("{}", evaluated);
                    Ok(())
                })();

                if let Err(e) = result {
                    println!("Error: {}", e);
                }
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
