use crate::{lexer::Lexer, token::Token};

const PROMPT: &str = ">> ";

pub fn init_repl() {
    loop {
        print!("{}", PROMPT);

        let mut input = String::new();

        std::io::stdin().read_line(&mut input).unwrap();

        let mut lexer = Lexer::new(&input);

        while let token = lexer.next_token() {
            if token == Token::Eof {
                break;
            }

            println!("{:?}", token);
        }
    }
}