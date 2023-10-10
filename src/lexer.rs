use std::collections::HashMap;

use anyhow::Result;
use phf::phf_map;

use crate::token::Token;

pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    read_position: usize,
    ch: Option<char>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: None,
        };

        lexer.read_char();

        lexer
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.ch {
            Some('=') => {
                if self.peek_char() == '=' {
                    self.read_char();

                    Token::Eq
                } else {
                    Token::Assign
                }
            },
            Some(';') => Token::Semicolon,
            Some('(') => Token::LParen,
            Some(')') => Token::RParen,
            Some('{') => Token::LBrace,
            Some('}') => Token::RBrace,
            Some(',') => Token::Comma,
            Some('+') => Token::Plus,
            Some('!') => {
                if self.peek_char() == '=' {
                    self.read_char();

                    Token::NotEq
                } else {
                    Token::Bang
                }
            },
            Some('-') => Token::Minus,
            Some('/') => Token::Slash,
            Some('*') => Token::Asterisk,
            Some('<') => Token::Lt,
            Some('>') => Token::Gt,
            None => Token::Eof,
            Some(ch) => {
                if ch.is_alphabetic() {        
                    let literal = self.read_identifier();

                    return Token::lookup_ident(&literal);
                } else if ch.is_ascii_digit() {
                    let literal = self.read_number();

                    return Token::Int(literal);
                } else {
                    return Token::Illegal;
                }
            }
        };

        self.read_char();

        token
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '0'
        } else {
            self.input.as_bytes()[self.read_position as usize] as char
        }
    }

    fn read_char(&mut self) -> () {
        if self.read_position >= self.input.len() {
            self.ch = None;
        } else {
            self.ch = Some(self.input.as_bytes()[self.read_position as usize] as char);
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;

        while match self.ch {
            Some(ch) => ch.is_alphabetic(),
            _ => false,
        } {
            self.read_char();
        }

        self.input[position..self.position].to_owned()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;

        while match self.ch {
            Some(ch) => ch.is_numeric(),
            _ => false,
        } {
            self.read_char();
        }

        self.input[position..self.position].to_owned()
    }

    fn skip_whitespace(&mut self) -> () {
        while match self.ch {
            Some(ch) => ch.is_whitespace(),
            _ => false,
        } {
            self.read_char();
        }
    }
}

#[cfg(test)]
mod tests {
    use anyhow::Error;

    use super::*;

    #[test]
    fn test_lexer() -> Result<(), Error>{
        let input = String::from("
            let five = 5;

            let ten = five + 5;

            let add = fn (x, y) {
                x + y
            }

            let result = add(five, ten);

            !-/*10;

            5 < 10 > 5;
            five < ten > 5;

            5 != 10;
            10 == 10;
        ");

        let expected_tokens = [
            Token::Let,
            Token::Ident(String::from("five")),
            Token::Assign,
            Token::Int(String::from("5")),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("ten")),
            Token::Assign,
            Token::Ident(String::from("five")),
            Token::Plus,
            Token::Int(String::from("5")),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("add")),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident(String::from("x")),
            Token::Comma,
            Token::Ident(String::from("y")),
            Token::RParen,
            Token::LBrace,
            Token::Ident(String::from("x")),
            Token::Plus,
            Token::Ident(String::from("y")),
            Token::RBrace,
            Token::Let,
            Token::Ident(String::from("result")),
            Token::Assign,
            Token::Ident(String::from("add")),
            Token::LParen,
            Token::Ident(String::from("five")),
            Token::Comma,
            Token::Ident(String::from("ten")),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(String::from("10")),
            Token::Semicolon,
            Token::Int(String::from("5")),
            Token::Lt,
            Token::Int(String::from("10")),
            Token::Gt,
            Token::Int(String::from("5")),
            Token::Semicolon,
            Token::Ident(String::from("five")),
            Token::Lt,
            Token::Ident(String::from("ten")),
            Token::Gt,
            Token::Int(String::from("5")),
            Token::Semicolon,
            Token::Int(String::from("5")),
            Token::NotEq,
            Token::Int(String::from("10")),
            Token::Semicolon,
            Token::Int(String::from("10")),
            Token::Eq,
            Token::Int(String::from("10")),
            Token::Semicolon,
        ];

        let mut lexer = Lexer::new(input.as_str());

        for expected_token in expected_tokens {
            let token = lexer.next_token();

            assert_eq!(token, expected_token);
        }

        Ok(())
    }
}