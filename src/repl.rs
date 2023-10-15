use anyhow::{Error, Result};

use rustyline::error::ReadlineError;

use crate::{lexer::Lexer, parser::Parser};

const PROMPT: &str = ">> ";

pub fn init_repl() -> Result<(), Error> {
    let mut rl = rustyline::DefaultEditor::new()?;

    #[cfg(feature = "with-file-history")]
    if rl.load_history("history.txt").is_err() {
        info!("No previous history.");
    }

    println!(
        "{} interpreter v{}",
        env!("CARGO_PKG_NAME"),
        env!("CARGO_PKG_VERSION")
    );

    loop {
        let readline = rl.readline(format!("{}", PROMPT).as_str());

        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;

                let lexer = Lexer::new(&line);
                let mut parser = Parser::new(lexer);

                let program = parser.parse_program();
                parser.check_errors()?;

                for statement in program.statements {
                    println!("{:?}", &statement);
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
                break;
            }
        }
    }

    Ok(())
}
