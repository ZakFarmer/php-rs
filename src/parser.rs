use crate::{token::Token, lexer::Lexer, ast::{Program, Statement, LetStatement, Identifier}};

pub struct Parser<'a> {
    lexer: Lexer<'a>,

    errors: Vec<String>,

    current_token: Option<Token>,
    peek_token: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    fn peek_error(&mut self, token: Token) -> () {
        let message = format!("Expected next token to be {}, got {}", token, self.peek_token.as_ref().unwrap());

        self.errors.push(message);
    }

    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer,
            errors: vec![],
            current_token: None,
            peek_token: None
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn next_token(&mut self) {
        self.current_token = self.peek_token.take();
        self.peek_token = Some(self.lexer.next_token());
    }

    fn current_token_is(&mut self, token: Token) -> bool {
        self.current_token == Some(token)
    }

    pub fn expect_peek(&mut self, token: Token) -> bool {
        if self.peek_token_is(token) {
            self.next_token();

            true
        } else {
            false
        }
    }

    fn peek_token_is(&mut self, token: Token) -> bool {
        self.peek_token == Some(token)
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::default();

        while self.current_token.as_ref() != Some(&Token::Eof) {
            let statement = self.parse_statement();

            if let Some(statement) = statement {
                program.statements.push(statement);
            }

            self.next_token();
        }

        program
    }

    pub fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.current_token {
            Some(Token::Let) => self.parse_let_statement(),
            _ => None
        }
    }

    fn parse_let_statement(&mut self) -> Option<Box<dyn Statement>> {
        // Ensure the current token is `let`
        let current_token = self.current_token.clone().unwrap();
        if !matches!(current_token, Token::Let) {
            self.errors.push(format!("Expected 'let', got {:?}", current_token));
            return None;
        }
    
        // Expect the next token to be an identifier
        self.next_token(); 
        let name_token = self.current_token.clone().unwrap();
        let name_value = match &name_token {
            Token::Ident(x) => x.clone(),
            _ => {
                self.errors.push(format!("Expected identifier, got {:?}", name_token));
                return None;
            }
        };
    
        // Expect the next token to be `=`
        if !self.expect_peek(Token::Assign) {
            return None;
        }
    
        // Skip tokens until `;`
        while !self.current_token_is(Token::Semicolon) {
            self.next_token();
        }
    
        // Construct and return the `LetStatement`
        Some(Box::new(LetStatement {
            name: Identifier {
                token: name_token,
                value: name_value,
            },
            token: current_token,
            value: None  // Placeholder, you might parse expressions here in the future
        }))
    }
    
}

#[cfg(test)]
mod tests {
    use super::*;

    use anyhow::{Result, Error};

    use crate::ast::{Statement, LetStatement};

    #[test]
    fn test_let_statements() -> Result<(), Error> {
        let input = "
            let x = 5;
            let y = 10;
            let foobar = 812303;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser)?;

        assert_eq!(3, program.statements.len());

        let expected_identifiers = [
            "x",
            "y",
            "foobar"
        ];

        for i in 0..expected_identifiers.len() {
            let statement = &program.statements[i];

            assert_let_statement(statement, expected_identifiers[i])?;
        }

        Ok(())
    }
    
    fn check_parser_errors(parser: &Parser) -> Result<(), Error> {
        let errors = parser.errors();

        if errors.len() == 0 {
            return Ok(());
        }

        let mut error_message = String::from("Parser has {} errors\n");
        for error in errors {
            error_message.push_str(&format!("Parser error: {}\n", error));
        }

        Err(Error::msg(error_message))
    }

    fn assert_let_statement(statement: &Box<dyn Statement>, name: &str) -> Result<(), Error> {
        assert_eq!("let", statement.token_literal());

        let _let_statement = statement.as_any().downcast_ref::<LetStatement>();

        assert!(_let_statement.is_some());
        
        let let_statement = _let_statement.unwrap();

        assert_eq!(name, let_statement.name.value);

        Ok(())
    }
}