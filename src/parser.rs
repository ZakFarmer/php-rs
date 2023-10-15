use std::collections::HashMap;

use anyhow::Error;
use log::trace;

use crate::{
    ast::{
        Expression, ExpressionStatement, Identifier, InfixExpression, IntegerLiteral,
        PrefixExpression, Program, ReturnStatement, Statement, VariableAssignment,
        VariableReference, Boolean, IfExpression, BlockStatement, FunctionLiteral, Node, CallExpression,
    },
    lexer::Lexer,
    token::{Token, TokenType},
};

type ParseResult = Option<Box<dyn Expression>>;

type PrefixParseFn = fn(&mut Parser) -> ParseResult;
type InfixParseFn = fn(&mut Parser, Box<dyn Expression>) -> ParseResult;

#[derive(Copy, Clone, PartialOrd, PartialEq)]
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

    precedences: HashMap<TokenType, Precedence>,
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
        let message = format!(
            "Expected next token to be {}, got {}",
            token_type,
            self.peek_token.as_ref().unwrap()
        );

        self.errors.push(message);
    }

    fn current_precedence(&mut self) -> Precedence {
        *self
            .precedences
            .get(&self.current_token.as_ref().unwrap().token_type)
            .unwrap_or(&Precedence::Lowest)
    }

    fn peek_precedence(&mut self) -> Precedence {
        *self
            .precedences
            .get(&self.peek_token.as_ref().unwrap().token_type)
            .unwrap_or(&Precedence::Lowest)
    }

    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer,
            errors: vec![],
            current_token: None,
            peek_token: None,
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
            precedences: HashMap::from([
                (TokenType::Eq, Precedence::Equals),
                (TokenType::NotEq, Precedence::Equals),
                (TokenType::Lt, Precedence::LessGreater),
                (TokenType::Gt, Precedence::LessGreater),
                (TokenType::Plus, Precedence::Sum),
                (TokenType::Minus, Precedence::Sum),
                (TokenType::Slash, Precedence::Product),
                (TokenType::Asterisk, Precedence::Product),
                (TokenType::LParen, Precedence::Call),
            ]),
        };

        parser.register_prefix(TokenType::Ident, |p| Parser::parse_identifier(p));
        parser.register_prefix(TokenType::True, |p| Parser::parse_boolean(p));
        parser.register_prefix(TokenType::False, |p| Parser::parse_boolean(p));
        parser.register_prefix(TokenType::Int, |p| Parser::parse_integer_literal(p));
        parser.register_prefix(TokenType::Function, |p| Parser::parse_function_literal(p));
        parser.register_prefix(TokenType::LParen, |p| Parser::parse_grouped_expression(p));
        parser.register_prefix(TokenType::If, |p| Parser::parse_if_expression(p));
        parser.register_prefix(TokenType::Bang, |p| Parser::parse_prefix_expression(p));
        parser.register_prefix(TokenType::Minus, |p| Parser::parse_prefix_expression(p));
        parser.register_prefix(TokenType::Dollar, |p| Parser::parse_variable_reference_expression(p));

        parser.register_infix(TokenType::LParen, |p, left| {
            Parser::parse_call_expression(p, left)
        });

        parser.register_infix(TokenType::Plus, |p, left| {
            Parser::parse_infix_expression(p, left)
        });
        parser.register_infix(TokenType::Minus, |p, left| {
            Parser::parse_infix_expression(p, left)
        });
        parser.register_infix(TokenType::Slash, |p, left| {
            Parser::parse_infix_expression(p, left)
        });
        parser.register_infix(TokenType::Asterisk, |p, left| {
            Parser::parse_infix_expression(p, left)
        });
        parser.register_infix(TokenType::Eq, |p, left| {
            Parser::parse_infix_expression(p, left)
        });
        parser.register_infix(TokenType::NotEq, |p, left| {
            Parser::parse_infix_expression(p, left)
        });
        parser.register_infix(TokenType::Lt, |p, left| {
            Parser::parse_infix_expression(p, left)
        });
        parser.register_infix(TokenType::Gt, |p, left| {
            Parser::parse_infix_expression(p, left)
        });

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
        self.peek_token.as_ref().unwrap().token_type == token_type.to_owned()
    }

    fn parse_boolean(&mut self) -> Option<Box<dyn Expression>> {
        let current_token = self.current_token.clone().unwrap();

        let value = match self.current_token.as_ref().unwrap().token_type {
            TokenType::True => true,
            TokenType::False => false,
            _ => {
                self.errors.push(format!(
                    "Expected true or false, got {:?}",
                    self.current_token.as_ref().unwrap().token_type
                ));
                return None;
            }
        };

        Some(Box::new(Boolean {
            token: current_token,
            value,
        }))
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
                TokenType::Return => self.parse_return_statement(),
                TokenType::Dollar => self.parse_assignment_statement(),
                _ => self.parse_expression_statement(),
            }
        } else {
            None
        }
    }

    fn parse_variable_reference_expression(&mut self) -> Option<Box<dyn Expression>> {
        if let Some(token) = &self.current_token {
            if token.token_type != TokenType::Dollar {
                self.errors.push(format!("Expected $, got {:?}", token));
                return None;
            }
        } else {
            self.errors.push("Expected $, got None".to_string());
            return None;
        }

        self.next_token();

        // Ensure the variable name is an identifier.
        let name_token = if let Some(token) = &self.current_token {
            if token.token_type == TokenType::Ident {
                token.clone()
            } else {
                self.errors
                    .push(format!("Expected identifier, got {:?}", token));
                return None;
            }
        } else {
            self.errors
                .push("Expected identifier, got None".to_string());
            return None;
        };

        // Parse as reference
        let identifier = Identifier {
            token: name_token.clone(),
            value: name_token.literal.clone(),
        };

        Some(Box::new(identifier) as Box<dyn Expression>)
    }

    fn parse_assignment_statement(&mut self) -> Option<Box<dyn Statement>> {
        trace!("Current token: {:?}", self.current_token);
        // Ensure the current token is a Dollar sign.
        if let Some(token) = &self.current_token {
            if token.token_type != TokenType::Dollar {
                self.errors.push(format!("Expected $, got {:?}", token));
                return None;
            }
        } else {
            self.errors.push("Expected $, got None".to_string());
            return None;
        }

        // Move to the variable name.
        self.next_token();

        // Ensure the variable name is an identifier.
        let name_token = if let Some(token) = &self.current_token {
            if token.token_type == TokenType::Ident {
                token.clone()
            } else {
                self.errors
                    .push(format!("Expected identifier, got {:?}", token));
                return None;
            }
        } else {
            self.errors
                .push("Expected identifier, got None".to_string());
            return None;
        };

        // Move to the next token and check if it's an assignment or a reference.
        self.next_token();

        match &self.current_token {
            Some(token) if token.token_type == TokenType::Assign => {
                // Parse as assignment
                self.next_token();
                let value_expression = self.parse_expression(Precedence::Lowest);

                trace!("Expression: {:?}", value_expression);

                if let Some(value_expression) = value_expression {
                    // Ensure the semicolon is present.
                    if !self.expect_peek(TokenType::Semicolon) {
                        return None;
                    }

                    let variable_assignment = VariableAssignment {
                        token: name_token.clone(),
                        name: name_token.literal.clone(),
                        value: value_expression,
                    };

                    Some(Box::new(variable_assignment) as Box<dyn Statement>)
                } else {
                    None
                }
            }
            Some(token) => {
                self.errors
                    .push(format!("Expected = or ;, got {:?}", token));
                None
            }
            None => {
                self.errors.push("Expected = or ;, got None".to_string());
                None
            }
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<dyn Expression>> {
        // Get prefix parse function (if it exists)
        let prefix_fn = self
            .prefix_parse_fns
            .get(&self.current_token.as_ref().unwrap().token_type)
            .map(|x| *x);

        if prefix_fn.is_none() {
            self.errors.push(format!(
                "No prefix parse function for {:?}",
                self.current_token.as_ref().unwrap().token_type
            ));

            return None;
        }

        // Call the prefix parse function
        let mut left = prefix_fn.unwrap()(self);

        while !self.peek_token_is(&TokenType::Semicolon) && precedence < self.peek_precedence() {
            let infix_fn = self
                .infix_parse_fns
                .get(&self.peek_token.as_ref().unwrap().token_type)
                .map(|x| *x);

            if infix_fn.is_none() {
                return left;
            }

            self.next_token();

            left = infix_fn.unwrap()(self, left.unwrap());
        }

        left
    }

    fn parse_expression_statement(&mut self) -> Option<Box<dyn Statement>> {
        let current_token = self.current_token.clone().unwrap();

        let expression = self.parse_expression(Precedence::Lowest);

        // Move to the next token if it's a semicolon
        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Some(Box::new(ExpressionStatement {
            token: current_token,
            expression,
        }))
    }

    fn parse_block_statement(&mut self) -> Box<dyn Statement> {
        let current_token = self.current_token.clone().unwrap();

        let mut statements = vec![];

        self.next_token();

        while !self.current_token_is(TokenType::RBrace)
            && !self.current_token_is(TokenType::Eof)
        {
            let statement = self.parse_statement();

            if let Some(statement) = statement {
                statements.push(statement);
            }

            self.next_token();
        }

        Box::new(BlockStatement {
            token: current_token,
            statements,
        })
    }

    fn parse_function_literal(&mut self) -> Option<Box<dyn Expression>> {
        let current_token = self.current_token.clone().unwrap();
    
        if !self.expect_peek(TokenType::LParen) {
            return None;
        }
    
        let parameters = self.parse_function_parameters();
    
        if !self.expect_peek(TokenType::LBrace) {
            return None;
        }
    
        let body = self.parse_block_statement();
    
        if body.as_any().is::<BlockStatement>() {
            Some(Box::new(FunctionLiteral {
                token: current_token,
                parameters,
                body: body,
            }))
        } else {
            None
        }
    }
    

    fn parse_function_parameters(&mut self) -> Vec<Identifier> {
        let mut identifiers = vec![];
    
        if self.peek_token_is(&TokenType::RParen) {
            self.next_token(); // Consume the RParen and exit
            return identifiers;
        }
    
        // Expecting a Dollar sign and identifier (variable name)
        if self.expect_peek(TokenType::Dollar) {
            self.next_token(); // Consume Dollar
            let identifier = Identifier {
                token: self.current_token.clone().unwrap(),
                value: self.current_token.as_ref().unwrap().to_string(),
            };
            identifiers.push(identifier);
        }
    
        // Expecting comma-separated identifiers
        while self.peek_token_is(&TokenType::Comma) {
            self.next_token(); // Consume Comma
            if self.expect_peek(TokenType::Dollar) {
                self.next_token(); // Consume Dollar
                let identifier = Identifier {
                    token: self.current_token.clone().unwrap(),
                    value: self.current_token.as_ref().unwrap().to_string(),
                };
                identifiers.push(identifier);
            }
        }
    
        // Expecting RParen to close the parameters list
        if !self.expect_peek(TokenType::RParen) {
            return vec![];
        }
    
        identifiers
    }    

    fn parse_grouped_expression(&mut self) -> Option<Box<dyn Expression>> {
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(TokenType::RParen) {
            return None;
        }

        expression
    }

    fn parse_identifier(&mut self) -> ParseResult {
        let current_token = self.current_token.clone().unwrap();

        let identifier = Identifier {
            token: current_token,
            value: self.current_token.as_ref().unwrap().to_string(),
        };

        Some(Box::new(identifier))
    }

    fn parse_integer_literal(&mut self) -> ParseResult {
        let current_token = self.current_token.clone().unwrap();

        let value = self
            .current_token
            .as_ref()
            .unwrap()
            .to_string()
            .parse::<i64>()
            .unwrap();

        Some(Box::new(IntegerLiteral {
            token: current_token,
            value,
        }))
    }

    fn parse_call_arguments(&mut self) -> Vec<Box<dyn Expression>> {
        let mut arguments = vec![];

        if self.peek_token_is(&TokenType::RParen) {
            self.next_token(); // Consume the RParen and exit
            return arguments;
        }

        self.next_token(); // Consume the LParen

        arguments.push(self.parse_expression(Precedence::Lowest).unwrap());

        while self.peek_token_is(&TokenType::Comma) {
            self.next_token(); // Consume the comma
            self.next_token(); // Consume the next token
            arguments.push(self.parse_expression(Precedence::Lowest).unwrap());
        }

        if !self.expect_peek(TokenType::RParen) {
            return vec![];
        }

        arguments
    }

    fn parse_call_expression(&mut self, function: Box<dyn Expression>) -> Option<Box<dyn Expression>> {
        let current_token = self.current_token.clone().unwrap();

        let arguments = self.parse_call_arguments();

        Some(Box::new(CallExpression {
            token: current_token,
            function,
            arguments,
        }))
    }

    fn parse_if_expression(&mut self) -> Option<Box<dyn Expression>> {
        let current_token = self.current_token.clone().unwrap();

        if !self.expect_peek(TokenType::LParen) {
            return None;
        }

        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest);

        if let None = condition {
            return None;
        }

        if !self.expect_peek(TokenType::RParen) {
            return None;
        }

        if !self.expect_peek(TokenType::LBrace) {
            return None;
        }

        let consequence = self.parse_block_statement();

        let alternative = if self.peek_token_is(&TokenType::Else) {
            self.next_token();

            if !self.expect_peek(TokenType::LBrace) {
                return None;
            }

            Some(self.parse_block_statement())
        } else {
            None
        };

        Some(Box::new(IfExpression {
            token: current_token,
            condition: condition.unwrap(),
            consequence,
            alternative,
        }))
    }

    fn parse_infix_expression(&mut self, left: Box<dyn Expression>) -> Option<Box<dyn Expression>> {
        let current_token = self.current_token.clone().unwrap();

        let operator = self.current_token.as_ref().unwrap().to_string();

        let precedence = self.current_precedence();

        self.next_token();

        match self.parse_expression(precedence) {
            Some(right) => Some(Box::new(InfixExpression {
                token: current_token,
                operator,
                left,
                right,
            })),
            None => {
                self.errors
                    .push("Expected an expression, but found none.".to_string());
                return None;
            }
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<Box<dyn Expression>> {
        let current_token = self.current_token.clone().unwrap();

        let operator = self.current_token.as_ref().unwrap().to_string();

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix).unwrap();

        Some(Box::new(PrefixExpression {
            token: current_token,
            operator,
            right,
        }))
    }

    fn parse_return_statement(&mut self) -> Option<Box<dyn Statement>> {
        let current_token = self.current_token.clone().unwrap();

        if !matches!(current_token.token_type, TokenType::Return) {
            self.errors
                .push(format!("Expected 'return', got {:?}", current_token));
            return None;
        }

        self.next_token();

        let return_value = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(TokenType::Semicolon) {
            return None;
        }

        Some(Box::new(ReturnStatement {
            token: current_token,
            return_value,
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

    use anyhow::{Error, Result};

    use crate::ast::{InfixExpression, PrefixExpression, Statement, VariableAssignment, IfExpression, FunctionLiteral, CallExpression};

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

        let expected_identifiers = ["x", "y", "foobar"];

        for i in 0..expected_identifiers.len() {
            let statement = &program.statements[i];

            assert_let_statement(statement, expected_identifiers[i])?;
        }

        Ok(())
    }

    #[test]
    fn test_boolean_expression() -> Result<(), Error> {
        let input = "
            true;
            false;

            $x = true;
            $y = false;
        ";

        let expected_values = [true, false, true, false];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        parser.check_errors()?;

        assert_eq!(4, program.statements.len());

        for i in 0..expected_values.len() {
            let statement = &program.statements[i];

            if let Some(statement) = statement.as_any().downcast_ref::<ExpressionStatement>() {
                let boolean = statement
                    .expression
                    .as_ref()
                    .unwrap()
                    .as_any()
                    .downcast_ref::<Boolean>()
                    .unwrap();

                assert_eq!(expected_values[i], boolean.value);
            } else if let Some(statement) = statement.as_any().downcast_ref::<VariableAssignment>() {
                let boolean = statement
                    .value
                    .as_ref()
                    .as_any()
                    .downcast_ref::<Boolean>()
                    .unwrap();

                assert_eq!(expected_values[i], boolean.value);
            } else {
                assert!(false, "Expected ExpressionStatement or VariableAssignment");
            }
        }

        Ok(())
    }

    #[test]
    fn test_call_expression() -> Result<(), Error> {
        let input = "add(1, 2 * 3, 4 + 5);";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        parser.check_errors()?;

        assert_eq!(1, program.statements.len());

        let statement = program.statements[0]
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .unwrap();

        let call_expression = statement
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<CallExpression>()
            .unwrap();

        assert_eq!("add", call_expression.function.token_literal());

        assert_eq!(3, call_expression.arguments.len());

        assert_literal_expression(
            &call_expression.arguments[0],
            "1",
        )?;

        assert_infix_expression(
            &call_expression.arguments[1],
            "2",
            "*",
            "3",
        )?;

        assert_infix_expression(
            &call_expression.arguments[2],
            "4",
            "+",
            "5",
        )?;

        Ok(())
    }

    #[test]
    fn test_if_expression() -> Result<(), Error> {
        let input = "
            if ($x < $y) { return $x; }
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        parser.check_errors()?;

        assert_eq!(1, program.statements.len());

        let statement = program.statements[0]
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .unwrap();

        let if_expression = statement
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<IfExpression>()
            .unwrap();

        assert_infix_expression(
            &if_expression.condition,
            "x",
            "<",
            "y",
        )?;

        Ok(())
    }

    #[test]
    fn test_if_else_expression() -> Result<(), Error> {
        let input = "
            if ($x < $y) { return $x; } else { return $y; }
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        parser.check_errors()?;

        assert_eq!(1, program.statements.len());

        let statement = program.statements[0]
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .unwrap();

        let if_expression = statement
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<IfExpression>()
            .unwrap();

        assert_infix_expression(
            &if_expression.condition,
            "x",
            "<",
            "y",
        )?;

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
    fn test_function_literal_parsing() -> Result<(), Error> {
        let input = "function ($x, $y) { return $x + $y; }";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        parser.check_errors()?;

        assert_eq!(1, program.statements.len());

        let statement = program.statements[0]
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .unwrap();

        let function_literal = statement
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<FunctionLiteral>()
            .unwrap();

        assert_eq!(2, function_literal.parameters.len());

        assert_eq!("x", function_literal.parameters[0].value);
        assert_eq!("y", function_literal.parameters[1].value);

        if let Some(body) = function_literal.body.as_any().downcast_ref::<BlockStatement>() {
            assert_eq!(1, body.statements.len());

            let expression = if let Some(return_statement) = body.statements[0]
                .as_any()
                .downcast_ref::<ReturnStatement>()
            {
                return_statement.return_value.as_ref().unwrap()
            } else if let Some(expression_statement) = body.statements[0]
                .as_any()
                .downcast_ref::<ExpressionStatement>()
            {
                expression_statement.expression.as_ref().unwrap()
            } else {
                assert!(false, "Expected ReturnStatement or ExpressionStatement");
                return Ok(());
            };

            assert_infix_expression(
                expression,
                "x",
                "+",
                "y",
            )?;
        } else {
            assert!(false, "Expected BlockStatement");
        }

        Ok(())
    }

    #[test]
    fn test_integer_literal_expression() -> Result<(), Error> {
        let input = "5;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        parser.check_errors()?;

        assert_eq!(1, program.statements.len());

        let statement = program.statements[0]
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .unwrap();
        let integer_literal = statement
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<IntegerLiteral>()
            .unwrap();

        assert_eq!(5, integer_literal.value);

        Ok(())
    }

    #[test]
    fn test_operator_precedence_parsing() -> Result<(), Error> {
        let tests = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
        ];

        for (input, expected) in tests.iter() {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            parser.check_errors()?;

            for statement in &program.statements {
                println!("{}", statement);
            }

            assert_eq!(*expected, program.to_string());
        }

        Ok(())
    }

    #[test]
    fn test_parsing_infix_expressions() -> Result<(), Error> {
        let infix_tests: [(&str, i64, &str, i64); 8] = [
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
        ];

        for (input, left_value, operator, right_value) in infix_tests.iter() {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            parser.check_errors()?;

            assert_eq!(1, program.statements.len());

            let statement = program.statements[0]
                .as_any()
                .downcast_ref::<ExpressionStatement>()
                .unwrap();
            let expression = statement
                .expression
                .as_ref()
                .unwrap()
                .as_any()
                .downcast_ref::<InfixExpression>()
                .unwrap();

            assert_integer_literal(&expression.left, *left_value)?;
            assert_eq!(*operator, expression.operator);
            assert_integer_literal(&expression.right, *right_value)?;
        }

        Ok(())
    }

    #[test]
    fn test_parsing_prefix_expressions() -> Result<(), Error> {
        let prefix_tests: [(&str, &str, i64); 2] = [("!5;", "!", 5), ("-15;", "-", 15)];

        for (input, _operator, value) in prefix_tests.iter() {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            parser.check_errors()?;

            assert_eq!(1, program.statements.len());

            let statement = program.statements[0]
                .as_any()
                .downcast_ref::<ExpressionStatement>()
                .unwrap();
            let expression = statement
                .expression
                .as_ref()
                .unwrap()
                .as_any()
                .downcast_ref::<PrefixExpression>()
                .unwrap();

            assert_integer_literal(&expression.right, *value)?;
        }

        Ok(())
    }

    fn assert_boolean_literal(expression: &Box<dyn Expression>, value: bool) -> Result<(), Error> {
        let boolean = expression.as_any().downcast_ref::<Boolean>();

        assert!(boolean.is_some());

        let boolean = boolean.unwrap();

        assert_eq!(value, boolean.value);

        Ok(())
    }

    fn assert_let_statement(statement: &Box<dyn Statement>, name: &str) -> Result<(), Error> {
        let _let_statement = statement.as_any().downcast_ref::<VariableAssignment>();

        assert!(_let_statement.is_some());

        let let_statement = _let_statement.unwrap();

        assert_eq!(name, let_statement.name);

        Ok(())
    }

    fn assert_identifier(expression: &Box<dyn Expression>, value: &str) -> Result<(), Error> {
        let identifier = expression.as_any().downcast_ref::<Identifier>();

        assert!(identifier.is_some());

        let identifier = identifier.unwrap();

        assert_eq!(value, identifier.value);

        Ok(())
    }

    fn assert_integer_literal(expression: &Box<dyn Expression>, value: i64) -> Result<(), Error> {
        let integer_literal = expression.as_any().downcast_ref::<IntegerLiteral>();

        assert!(integer_literal.is_some());

        let integer_literal = integer_literal.unwrap();

        assert_eq!(value, integer_literal.value);

        Ok(())
    }

    fn assert_infix_expression(
        expression: &Box<dyn Expression>,
        left_value: &str,
        operator: &str,
        right_value: &str,
    ) -> Result<(), Error> {
        let infix_expression = expression.as_any().downcast_ref::<InfixExpression>();

        assert!(infix_expression.is_some());

        let infix_expression = infix_expression.unwrap();

        assert_literal_expression(&infix_expression.left, left_value)?;
        assert_eq!(operator, infix_expression.operator);
        assert_literal_expression(&infix_expression.right, right_value)?;

        Ok(())
    }

    fn assert_literal_expression(
        expression: &Box<dyn Expression>,
        expected: &str,
    ) -> Result<bool, Error> {
        if let Some(integer_literal) = expression.as_any().downcast_ref::<IntegerLiteral>() {
            assert_eq!(expected, integer_literal.value.to_string());
            Ok(true)
        } else if let Some(identifier) = expression.as_any().downcast_ref::<Identifier>() {
            assert_eq!(expected, identifier.value);
            Ok(true)
        }
        else {

            Ok(false)
        }
    }
}
