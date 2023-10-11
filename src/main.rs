use anyhow::{Result, Error};
use repl::init_repl;

mod ast;
mod lexer;
mod parser;
mod repl;
mod string;
mod token;

const NAME: &str = env!("CARGO_PKG_NAME");
const VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() -> Result<(), Error>{
    println!("{} interpreter {}", NAME, VERSION);
    init_repl()?;

    Ok(())
}
