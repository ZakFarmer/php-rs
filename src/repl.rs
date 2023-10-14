use std::{io::{self, Write}, any::{TypeId, Any}};

use anyhow::{Result, Error};

use crate::{lexer::Lexer, parser::Parser, ast::{LetStatement, Statement}};

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
            print_statement(statement);
        }
    }
}

fn print_statement(statement: Box<dyn Statement>) {
    match statement.as_any().type_id() {
        id if id == TypeId::of::<LetStatement>() => {
            let let_statement = statement.as_any().downcast_ref::<LetStatement>().unwrap();
            println!("{:?}", let_statement);
        },
        _ => println!("Unknown statement type: {:?}", statement.as_any().type_id()),
    }
}