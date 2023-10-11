use std::io::{self, Write};

use anyhow::{Result, Error};

use crate::{lexer::Lexer, parser::Parser, ast::LetStatement};

const PROMPT: &str = ">> ";

pub fn init_repl() -> Result<(), Error> {
    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();

        let mut input = String::new();

        std::io::stdin().read_line(&mut input).unwrap();

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        parser.check_errors()?;

        for statement in program.statements {
            let let_statement = statement.as_any().downcast_ref::<LetStatement>();

            if let Some(let_statement) = let_statement {
                println!("{}{} = {}", let_statement.token, let_statement.name.value, let_statement.value.as_ref().unwrap());
            }
        }
    }
}