use std::env;

use anyhow::{Error, Result};
use repl::init_repl;

mod ast;
mod lexer;
mod parser;
mod repl;
mod string;
mod token;

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
        // TODO: Split this out into a separate function
        println!("Running script: {}", file_path);

        let file = std::fs::read_to_string(file_path)?;

        let lexer = lexer::Lexer::new(&file);
        let mut parser = parser::Parser::new(lexer);

        let program = parser.parse_program();

        parser.check_errors()?;

        println!("{:?}", program);
    } else {
        init_repl()?;
    }

    Ok(())
}
