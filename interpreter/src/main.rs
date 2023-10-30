use std::env;

pub mod repl;

use anyhow::{Error, Result};
use repl::init_repl;

pub const NAME: &str = env!("CARGO_PKG_NAME");
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() -> Result<(), Error> {
    env_logger::init();

    let args: Vec<String> = env::args().collect();

    let file_path = match args.len() {
        1 => None,
        2 => Some(args[1].clone()),
        _ => {
            println!("Usage: {} [FILE]", NAME);
            std::process::exit(1);
        }
    };

    if let Some(file_path) = file_path {
        println!("Running script: {}", file_path);

        // TODO: Split this out into a separate function
        let file = std::fs::read_to_string(file_path)?;

        let mut parser = parser::Parser::new(&file);

        let program = parser.parse_program()?;

        parser.check_errors()?;

        println!("{}", program);
    } else {
        println!("Loading REPL...");
        init_repl()?;
    }

    Ok(())
}
