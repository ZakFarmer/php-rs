use std::collections::HashMap;

use anyhow::Error;

use crate::{token::{Token, TokenType}, lexer::Lexer, ast::{Program, Statement, LetStatement, Identifier, ReturnStatement, Expression, ExpressionStatement}};

type ParseExpressionResult = Option<Box<dyn Expression>>;
type ParseIdentifierResult = Option<Identifier>;

type PrefixParseFn = fn(&mut Parser) -> ParseExpressionResult;
type InfixParseFn = fn(&mut Parser, Box<dyn Expression>) -> ParseExpressionResult;

enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,

    errors: Vec<String>,

    current_token: Option<Token>,
    peek_token: Option<Token>,

    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

impl<'a> Parser<'a> {
    pub fn check_errors(&self) -> Result<(), Error> {
        if self.errors().len() == 0 {
            return Ok(());
        }

        let mut error_message = format!("Parser has {} errors\n", self.errors().len());

        for error in self.errors() {
            error_message.push_str(&format!("Parser error: {}\n", error));
        }

        Err(Error::msg(error_message))
    }

    pub fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    fn peek_error(&mut self, token_type: &TokenType) -> () {
        let message = format!("Expected next token to be {}, got {}", token_type, self.peek_token.as_ref().unwrap());

        self.errors.push(message);
    }

    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer,
            errors: vec![],
            current_token: None,
            peek_token: None,
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        parser.register_prefix(TokenType::Ident, |p| Parser::parse_identifier(p));
        parser.register_prefix(TokenType::Int, |p| Parser::parse_identifier(p));

        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.current_token = self.peek_token.take();
        self.peek_token = Some(self.lexer.next_token());

        self.current_token.clone()
    }

    fn current_token_is(&mut self, token_type: TokenType) -> bool {
        match &self.current_token {
            Some(token) => token.token_type == token_type,
            None => false,
        }
    }

    pub fn expect_peek(&mut self, token_type: TokenType) -> bool {
        if self.peek_token_is(&token_type) {
            self.next_token();

            true
        } else {
            self.peek_error(&token_type);

            false
        }
    }

    fn peek_token_is(&mut self, token_type: &TokenType) -> bool {
        self.peek_token
            .as_ref()
            .unwrap()
            .token_type == token_type.to_owned()
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::default();

        while self.current_token.as_ref().unwrap().token_type != TokenType::Eof {
            let statement = self.parse_statement();

            if let Some(statement) = statement {
                program.statements.push(statement);
            }

            self.next_token();
        }

        program
    }

    pub fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        if let Some(token) = &self.current_token {
            match token.token_type {
                TokenType::Dollar => self.parse_dollar_statement(),
                TokenType::Return => self.parse_return_statement(),
                _ => {
                    if self.peek_token_is(&TokenType::Semicolon) {
                        None
                    } else {
                        self.parse_expression_statement()
                    }
                },
            }
        } else {
            None
        }
    }

    fn parse_dollar_statement(&mut self) -> Option<Box<dyn Statement>> {
        let current_token = self.current_token.clone().unwrap();
    
        if !matches!(current_token.token_type, TokenType::Dollar) {
            self.errors.push(format!("Expected '$', got {:?}", current_token));
            return None;
        }
    
        self.next_token(); // Move to identifier
    
        let name_token = self.current_token.clone().unwrap();
        let name_value = match name_token.token_type {
            TokenType::Ident => name_token.literal.clone(),
            _ => {
                self.errors.push(format!("Expected identifier, got {:?}", name_token));
                return None;
            }
        };
    
        let mut value_expression: Option<Box<dyn Expression>> = None;
    
        // Check if next token is an assignment
        if self.peek_token_is(&TokenType::Assign) {
            self.next_token();
            self.next_token();
            value_expression = self.parse_expression(Precedence::Lowest); // Parse the expression
        }
    
        // Ensure the statement is terminated with a semicolon
        if !self.expect_peek(TokenType::Semicolon) {
            return None;
        }
    
        Some(Box::new(LetStatement {
            name: Identifier {
                token: name_token,
                value: name_value.to_string(),
            },
            token: current_token,
            value: value_expression,
        }))
    }    

    fn parse_expression(&mut self, precedence: Precedence) -> ParseExpressionResult {
        let prefix_fn = self.prefix_parse_fns
            .get(
                &self.current_token
                    .as_ref()
                    .unwrap()
                    .token_type
            ).map(|x| *x);

        if prefix_fn.is_none() {
            self.errors.push(format!("No prefix parse function for {:?}", self.current_token.as_ref().unwrap().token_type));

            return None;
        }

        prefix_fn.unwrap()(self)
    }

    fn parse_expression_statement(&mut self) -> Option<Box<dyn Statement>> {
        let current_token = self.current_token.clone().unwrap();

        let expression = if self.peek_token_is(&TokenType::Semicolon) {
            // If the next token is a semicolon, there's no expression, move forward
            None
        } else {
            // Else, parse the expression
            self.parse_expression(Precedence::Lowest)
        };

        // Move to the next token if it's a semicolon
        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Some(Box::new(ExpressionStatement {
            token: current_token,
            expression,
        }))
    }


    fn parse_identifier(&mut self) -> ParseExpressionResult {
        let current_token = self.current_token.clone().unwrap();

        let identifier = Identifier {
            token: current_token,
            value: self.current_token.as_ref().unwrap().to_string(),
        };

        Some(Box::new(identifier))
    }

    fn parse_return_statement(&mut self) -> Option<Box<dyn Statement>> {
        let current_token = self.current_token.clone().unwrap();

        if !matches!(current_token.token_type, TokenType::Return) {
            self.errors.push(format!("Expected 'return', got {:?}", current_token));
            return None;
        }

        self.next_token();

        while !self.current_token_is(TokenType::Semicolon) {
            self.next_token();
        }
    
        Some(Box::new(ReturnStatement {
            token: current_token,
            return_value: None  // Placeholder, you might parse expressions here in the future
        }))
    }

    pub fn register_prefix(&mut self, token_type: TokenType, function: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, function);
    }

    pub fn register_infix(&mut self, token_type: TokenType, function: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, function);
    }
    
}

#[cfg(test)]
mod tests {
    use super::*;

    use anyhow::{Result, Error};

    use crate::ast::{Statement, LetStatement, ExpressionStatement};

    #[test]
    fn test_assignment_statements() -> Result<(), Error> {
        let input = "
            $x = 5;
            $y = 10;
            $foobar = 812303;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        parser.check_errors()?;

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

    #[test]
    fn test_return_statements() -> Result<(), Error> {
        let input = "
            return 5;
            return 10;
            return 993322;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        parser.check_errors()?;

        assert_eq!(3, program.statements.len());

        for statement in program.statements {
            assert_eq!("return", statement.token_literal());
        }

        Ok(())
    }

    #[test]
    fn test_identifier_expression() -> Result<(), Error> {
        let input = "$foobar;";
    
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
    
        let program = parser.parse_program();
        parser.check_errors();
    
        assert_eq!(1, program.statements.len());
    
        let statement = program.statements[0].as_any().downcast_ref::<LetStatement>().unwrap();
    
        assert_eq!("foobar", statement.name.value);
    
        Ok(())
    }

    fn assert_let_statement(statement: &Box<dyn Statement>, name: &str) -> Result<(), Error> {
        assert_eq!("$", statement.token_literal());

        let _let_statement = statement.as_any().downcast_ref::<LetStatement>();

        assert!(_let_statement.is_some());
        
        let let_statement = _let_statement.unwrap();

        assert_eq!(name, let_statement.name.value);

        Ok(())
    }
}