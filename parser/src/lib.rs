use std::collections::HashMap;

use anyhow::{Error, Result};
use ast::{ArrayLiteral, Literal};

pub mod ast;

use crate::ast::{
    Assignment, BlockStatement, CallExpression, Expression, FunctionLiteral, Identifier,
    IfExpression, IndexExpression, InfixExpression, PrefixExpression, Program, ReturnStatement,
    Statement,
};

use lexer::Lexer;
use token::{Token, TokenType};

type ParseResult = Result<Expression>;

type PrefixParseFn = fn(&mut Parser) -> ParseResult;
type InfixParseFn = fn(&mut Parser, Expression) -> ParseResult;

#[derive(Copy, Clone, PartialOrd, PartialEq)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
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

    pub fn new(input: &'a str) -> Self {
        let lexer = Lexer::new(input);
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
                (TokenType::LBracket, Precedence::Index),
            ]),
        };

        parser.register_prefix(TokenType::Ident, |p| Parser::parse_identifier(p));
        parser.register_prefix(TokenType::Function, |p| Parser::parse_function_literal(p));
        parser.register_prefix(TokenType::LParen, |p| Parser::parse_grouped_expression(p));
        parser.register_prefix(TokenType::If, |p| Parser::parse_if_expression(p));
        parser.register_prefix(TokenType::Bang, |p| Parser::parse_prefix_expression(p));
        parser.register_prefix(TokenType::Minus, |p| Parser::parse_prefix_expression(p));

        parser.register_prefix(TokenType::Dollar, |p| {
            Parser::parse_variable_reference_expression(p)
        });

        parser.register_prefix(TokenType::True, |p| Parser::parse_boolean_literal(p));
        parser.register_prefix(TokenType::False, |p| Parser::parse_boolean_literal(p));
        parser.register_prefix(TokenType::Int, |p| Parser::parse_integer_literal(p));
        parser.register_prefix(TokenType::String, |p| Parser::parse_string_literal(p));
        parser.register_prefix(TokenType::LBracket, |p| Parser::parse_array_literal(p));

        parser.register_infix(TokenType::LParen, |p, left| {
            Parser::parse_call_expression(p, left)
        });
        parser.register_infix(TokenType::LBracket, |p, left| {
            Parser::parse_index_expression(p, left)
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

    fn expect_current(&mut self, t: TokenType) -> bool {
        if self.current_token.is_some() && self.current_token.as_ref().unwrap().token_type == t {
            true
        } else {
            self.errors
                .push(format!("Expected {:?}, got {:?}", t, self.current_token));
            false
        }
    }

    pub fn expect_peek(&mut self, token_type: &TokenType) -> bool {
        if self.peek_token_is(token_type) {
            self.next_token();

            true
        } else {
            self.peek_error(token_type);

            false
        }
    }

    fn peek_token_is(&mut self, token_type: &TokenType) -> bool {
        self.peek_token.as_ref().unwrap().token_type == token_type.to_owned()
    }

    fn parse_array_literal(&mut self) -> Result<Expression> {
        let _current_token = self.current_token.clone().unwrap();

        let elements = self.parse_expression_list(TokenType::RBracket)?;

        Ok(Expression::Literal(Literal::Array(ArrayLiteral {
            elements,
        })))
    }

    fn parse_boolean_literal(&mut self) -> Result<Expression> {
        let _current_token = self.current_token.clone().unwrap();

        let value = match self.current_token.as_ref().unwrap().token_type {
            TokenType::True => true,
            TokenType::False => false,
            _ => {
                return Err(Error::msg(format!(
                    "Expected true or false, got {:?}",
                    self.current_token.as_ref().unwrap().token_type
                )));
            }
        };

        Ok(Expression::Literal(Literal::Boolean(value)))
    }

    fn parse_string_literal(&mut self) -> Result<Expression> {
        let _current_token = self.current_token.clone().unwrap();

        let value = self.current_token.as_ref().unwrap().to_string();

        Ok(Expression::Literal(Literal::String(value)))
    }

    pub fn parse_program(&mut self) -> Result<Program> {
        let mut program = Program::default();

        while !self.current_token_is(TokenType::Eof) {
            match self.parse_statement() {
                Ok(stmt) => program.statements.push(stmt),
                Err(e) => self.errors.push(e.to_string()),
            }
            self.next_token();
        }

        if self.errors.is_empty() {
            Ok(program)
        } else {
            Err(Error::msg(format!(
                "Parser has errors: {}",
                self.errors.join(", ")
            )))
        }
    }

    pub fn parse_statement(&mut self) -> Result<Statement> {
        if let Some(token) = &self.current_token {
            match &token.token_type {
                TokenType::Return => self.parse_return_statement(),
                TokenType::Ident if token.value.starts_with('$') => {
                    if self.peek_token_is(&TokenType::Assign) {
                        self.parse_assignment_statement()
                    } else {
                        self.parse_expression_statement()
                    }
                }
                _ => self.parse_expression_statement(),
            }
        } else {
            Err(Error::msg("Unexpected end of input"))
        }
    }

    fn parse_variable_reference_expression(&mut self) -> Result<Expression> {
        // Expect the next token to be an identifier
        if let Some(token) = &self.current_token {
            if token.token_type == TokenType::Ident {
                let identifier = Identifier {
                    token: token.clone(),
                };

                self.next_token();

                return Ok(Expression::Identifier(identifier));
            } else {
                return Err(Error::msg(format!(
                    "Expected identifier after $, got {:?}",
                    token
                )));
            }
        } else {
            return Err(Error::msg("Unexpected end of input"));
        }
    }

    fn parse_assignment_statement(&mut self) -> Result<Statement> {
        // Ensure the variable name is an identifier.
        let name_token = if let Some(token) = &self.current_token {
            if token.token_type == TokenType::Ident {
                token.clone()
            } else {
                self.errors
                    .push(format!("Expected identifier, got {:?}", token));
                return Err(Error::msg(format!("Expected identifier, got {:?}", token)));
            }
        } else {
            self.errors
                .push("Expected identifier, got None".to_string());
            return Err(Error::msg("Expected identifier, got None"));
        };

        // Move to the next token and check if it's an assignment.
        self.next_token();

        let current_token = self.current_token.clone();

        if let Some(token) = &current_token {
            if token.token_type == TokenType::Assign {
                // Parse as assignment
                self.next_token();

                let value_expression = self.parse_expression(Precedence::Lowest);

                if let Ok(value_expression) = value_expression {
                    let variable_assignment = Assignment {
                        token: name_token.clone(),
                        name: Identifier {
                            token: name_token.clone(),
                        },
                        value: value_expression,
                    };

                    if self.peek_token_is(&TokenType::Semicolon) {
                        self.next_token();
                    }

                    return Ok(Statement::Assign(variable_assignment));
                } else {
                    self.errors
                        .push("Expected an expression after =".to_string());
                    return Err(Error::msg("Expected an expression after ="));
                }
            } else {
                // If it's not an assignment, then it's not an assignment statement.
                return Err(Error::msg(format!(
                    "Expected assignment, got {:?}",
                    token.token_type
                )));
            }
        } else {
            self.errors.push("Unexpected end of input".to_string());
            return Err(Error::msg("Unexpected end of input"));
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression> {
        // Get prefix parse function (if it exists)
        let prefix_fn = self
            .prefix_parse_fns
            .get(&self.current_token.as_ref().unwrap().token_type)
            .map(|x| *x);

        if prefix_fn.is_none() {
            return Err(Error::msg(format!(
                "No prefix parse function for {:?}",
                self.current_token.as_ref().unwrap().token_type
            )));
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

    fn parse_expression_list(&mut self, end: TokenType) -> Result<Vec<Expression>> {
        let mut list = vec![];

        if self.peek_token_is(&end) {
            self.next_token();
            return Ok(list);
        }

        self.next_token();

        list.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token_is(&TokenType::Comma) {
            self.next_token();
            self.next_token();
            list.push(self.parse_expression(Precedence::Lowest)?);
        }

        if !self.expect_peek(&end) {
            return Err(Error::msg(format!(
                "Expected {:?} or comma, got {:?}",
                end, self.peek_token
            )));
        }

        Ok(list)
    }

    fn parse_expression_statement(&mut self) -> Result<Statement> {
        let expr = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Expr(expr))
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement> {
        let current_token = self.current_token.clone().unwrap();

        let mut statements = vec![];

        self.next_token();

        while !self.current_token_is(TokenType::RBrace) && !self.current_token_is(TokenType::Eof) {
            let statement = self.parse_statement();

            if let Ok(statement) = statement {
                statements.push(statement);
            }

            self.next_token();
        }

        Ok(BlockStatement {
            token: current_token,
            statements,
        })
    }

    fn parse_function_literal(&mut self) -> Result<Expression> {
        let current_token = self.current_token.clone().unwrap();

        if !self.expect_peek(&TokenType::LParen) {
            return Err(Error::msg("Expected LParen"));
        }

        let parameters = self.parse_function_parameters()?;

        if !self.expect_peek(&TokenType::LBrace) {
            return Err(Error::msg("Expected LBrace"));
        }

        let body = self.parse_block_statement();

        if let Ok(body) = body {
            Ok(Expression::Function(FunctionLiteral {
                token: current_token,
                parameters,
                body: body,
            }))
        } else {
            Err(Error::msg("Expected a block statement"))
        }
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>> {
        let mut identifiers = vec![];

        if self.peek_token_is(&TokenType::RParen) {
            self.next_token(); // Consume the RParen and exit
            return Ok(identifiers);
        }

        self.next_token();

        loop {
            if let Some(token) = &self.current_token {
                if token.token_type == TokenType::Ident && token.value.starts_with('$') {
                    let identifier = Identifier {
                        token: token.clone(),
                    };
                    identifiers.push(identifier);
                } else {
                    return Err(Error::msg(format!(
                        "Expected identifier starting with '$', got {:?}",
                        token
                    )));
                }
            }

            if self.peek_token_is(&TokenType::Comma) {
                self.next_token();
                self.next_token();
            } else if self.peek_token_is(&TokenType::RParen) {
                self.next_token();

                break;
            } else {
                return Err(Error::msg(
                    "Expected ',' or ')' after identifier".to_string(),
                ));
            }
        }

        Ok(identifiers)
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression> {
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(&TokenType::RParen) {
            return Err(Error::msg("Expected RParen"));
        }

        expression
    }

    fn parse_identifier(&mut self) -> Result<Expression> {
        let current_token = self.current_token.clone().unwrap();

        let identifier = Identifier {
            token: current_token,
        };

        Ok(Expression::Identifier(identifier))
    }

    fn parse_integer_literal(&mut self) -> Result<Expression> {
        let _current_token = self.current_token.clone().unwrap();

        let value = self
            .current_token
            .as_ref()
            .unwrap()
            .to_string()
            .parse::<i64>()
            .unwrap();

        Ok(Expression::Literal(Literal::Integer(value)))
    }

    fn parse_call_arguments(&mut self) -> Vec<Expression> {
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

        if !self.expect_peek(&TokenType::RParen) {
            return vec![];
        }

        arguments
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression> {
        let current_token = self.current_token.clone().unwrap();

        let arguments = self.parse_call_arguments();

        Ok(Expression::Call(CallExpression {
            token: current_token,
            function: Box::new(function),
            arguments,
        }))
    }

    fn parse_if_expression(&mut self) -> Result<Expression> {
        self.expect_peek(&TokenType::LParen);
        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(&TokenType::RParen);
        self.expect_peek(&TokenType::LBrace);

        let consequence = self.parse_block_statement()?;

        let alternative = if self.peek_token_is(&TokenType::Else) {
            self.next_token();
            self.expect_peek(&TokenType::LBrace);
            Some(self.parse_block_statement()?)
        } else {
            None
        };

        return Ok(Expression::If(IfExpression {
            condition: Box::new(condition),
            token: self.current_token.clone().unwrap(),
            consequence,
            alternative,
        }));
    }

    fn parse_index_expression(&mut self, left: Expression) -> Result<Expression> {
        let current_token = self.current_token.clone().unwrap();

        self.next_token();

        let index = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(&TokenType::RBracket) {
            return Err(Error::msg("Expected RBracket"));
        }

        Ok(Expression::Index(IndexExpression {
            token: current_token,
            left: Box::new(left),
            index: Box::new(index),
        }))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression> {
        let current_token = self.current_token.clone().unwrap();

        let operator = self.current_token.as_ref().unwrap().clone();

        let precedence = self.current_precedence();

        self.next_token();

        match self.parse_expression(precedence) {
            Ok(right) => Ok(Expression::Infix(InfixExpression {
                token: current_token,
                operator,
                left: Box::new(left),
                right: Box::new(right),
            })),
            Err(e) => {
                self.errors
                    .push("Expected an expression, but found none.".to_string());
                return Err(Error::msg(e.to_string()));
            }
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression> {
        let current_token = self.current_token.clone().unwrap();

        let operator = self.current_token.as_ref().unwrap().clone();

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix).unwrap();

        Ok(Expression::Prefix(PrefixExpression {
            token: current_token,
            operator,
            right: Box::new(right),
        }))
    }

    fn parse_return_statement(&mut self) -> Result<Statement> {
        let current_token = self.current_token.clone().unwrap();

        if !matches!(current_token.token_type, TokenType::Return) {
            return Err(Error::msg(format!(
                "Expected 'return', got {:?}",
                current_token
            )));
        }

        self.next_token();

        let return_value = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(&TokenType::Semicolon) {
            return Err(Error::msg("Expected Semicolon"));
        }

        Ok(Statement::Return(ReturnStatement {
            token: current_token,
            return_value: return_value.unwrap(),
        }))
    }

    pub fn register_prefix(&mut self, token_type: TokenType, function: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, function);
    }

    pub fn register_infix(&mut self, token_type: TokenType, function: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, function);
    }
}
