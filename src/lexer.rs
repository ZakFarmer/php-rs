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
            Some('$') => Token::Dollar,
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
            $five = 5;

            $ten = five + 5;

            $add = function (x, y) {
                return x + y;
            }

            $result = add(five, ten);
        ");

        let expected_tokens = [
            Token::Dollar,
            Token::Ident(String::from("five")),
            Token::Assign,
            Token::Int(String::from("5")),
            Token::Semicolon,
            Token::Dollar,
            Token::Ident(String::from("ten")),
            Token::Assign,
            Token::Ident(String::from("five")),
            Token::Plus,
            Token::Int(String::from("5")),
            Token::Semicolon,
            Token::Dollar,
            Token::Ident(String::from("add")),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident(String::from("x")),
            Token::Comma,
            Token::Ident(String::from("y")),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::Ident(String::from("x")),
            Token::Plus,
            Token::Ident(String::from("y")),
            Token::Semicolon,
            Token::RBrace,
            Token::Dollar,
            Token::Ident(String::from("result")),
            Token::Assign,
            Token::Ident(String::from("add")),
            Token::LParen,
            Token::Ident(String::from("five")),
            Token::Comma,
            Token::Ident(String::from("ten")),
            Token::RParen,
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