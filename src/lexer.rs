use std::collections::HashMap;

use anyhow::Result;
use phf::phf_map;

struct Lexer {
    input: String,
    position: u32,
    read_position: u32,
    ch: u8,
}

static KEYWORDS: phf::Map<&'static str, TokenType> = phf_map! {
    "fn" => TokenType::Function,
    "let" => TokenType::Let,
};

fn lookup_ident(ident: &String) -> TokenType {
    if let Some(token) = KEYWORDS.get(ident).cloned() {
        token
    } else {
        TokenType::Ident
    }
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: 0
        };

        lexer.read_char();

        lexer
    }

    fn next_token(&mut self) -> Result<Token> {
        self.skip_whitespace();

        let ch = self.ch as char;

        let token_type = match self.ch {
            b'=' => TokenType::Assign,
            b';' => TokenType::Semicolon,
            b'(' => TokenType::LParen,
            b')' => TokenType::RParen,
            b'{' => TokenType::LBrace,
            b'}' => TokenType::RBrace,
            b',' => TokenType::Comma,
            b'+' => TokenType::Plus,
            0 => TokenType::Eof,
            _ => {
                if ch.is_alphabetic() {        
                    let literal = self.read_identifier();

                    let token_type = lookup_ident(&literal);

                    return Ok(Token {
                        token_type,
                        literal
                    });
                } else if ch.is_ascii_digit() {
                    let literal = self.read_number();

                    return Ok(Token {
                        token_type: TokenType::Int,
                        literal
                    });
                } else {
                    TokenType::Illegal
                }
            }
        };

        let token = Token {
            token_type,
            literal: (self.ch as char).to_string(),
        };

        self.read_char();

        Ok(token)
    }

    fn read_char(&mut self) -> () {
        if self.read_position >= self.input.len() as u32 {
            self.ch = 0;
        } else {
            self.ch = self.input.as_bytes()[self.read_position as usize];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;

        while self.ch.is_ascii_alphabetic() {
            self.read_char();
        }

        self.input[position as usize..self.position as usize].to_string()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;

        while self.ch.is_ascii_digit() {
            self.read_char();
        }

        self.input[position as usize..self.position as usize].to_string()
    }

    fn skip_whitespace(&mut self) -> () {
        while self.ch == b' ' || self.ch == b'\t' || self.ch == b'\n' || self.ch == b'\r' {
            self.read_char();
        }
    }
}

#[derive(Clone, Debug)]
struct Token {
    token_type: TokenType,
    literal: String
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TokenType {
    Illegal,
    Eof,
    Ident,
    Int,
    Assign,
    Plus,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Function,
    Let
}

#[cfg(test)]
mod tests {
    use anyhow::Error;

    use super::*;

    #[test]
    fn test_lexer() -> Result<(), Error>{
        let input = String::from("
            let x = 5;

            let y = x + 10;

            let add = fn (x, y) {
                x + y
            }

            let result = add(x, y);
        ");

        let expected_tokens = [
            Token {
                token_type: TokenType::Let,
                literal: String::from("let"),
            },
            Token {
                token_type: TokenType::Ident,
                literal: String::from("x"),
            },
            Token {
                token_type: TokenType::Assign,
                literal: String::from("="),
            },
            Token {
                token_type: TokenType::Int,
                literal: String::from("5"),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: String::from(";"),
            },
            Token {
                token_type: TokenType::Let,
                literal: String::from("let"),
            },
            Token {
                token_type: TokenType::Ident,
                literal: String::from("y"),
            },
            Token {
                token_type: TokenType::Assign,
                literal: String::from("="),
            },
            Token {
                token_type: TokenType::Ident,
                literal: String::from("x"),
            },
            Token {
                token_type: TokenType::Plus,
                literal: String::from("+"),
            },
            Token {
                token_type: TokenType::Int,
                literal: String::from("10"),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: String::from(";"),
            },
            Token {
                token_type: TokenType::Let,
                literal: String::from("let"),
            },
            Token {
                token_type: TokenType::Ident,
                literal: String::from("add"),
            },
            Token {
                token_type: TokenType::Assign,
                literal: String::from("="),
            },
            Token {
                token_type: TokenType::Function,
                literal: String::from("fn"),
            },
            Token {
                token_type: TokenType::LParen,
                literal: String::from("("),
            },
            Token {
                token_type: TokenType::Ident,
                literal: String::from("x"),
            },
            Token {
                token_type: TokenType::Comma,
                literal: String::from(","),
            },
            Token {
                token_type: TokenType::Ident,
                literal: String::from("y"),
            },
            Token {
                token_type: TokenType::RParen,
                literal: String::from(")"),
            },
            Token {
                token_type: TokenType::LBrace,
                literal: String::from("{"),
            },
            Token {
                token_type: TokenType::Ident,
                literal: String::from("x"),
            },
            Token {
                token_type: TokenType::Plus,
                literal: String::from("+"),
            },
            Token {
                token_type: TokenType::Ident,
                literal: String::from("y"),
            },
            Token {
                token_type: TokenType::RBrace,
                literal: String::from("}"),
            },
            Token {
                token_type: TokenType::Let,
                literal: String::from("let"),
            },
            Token {
                token_type: TokenType::Ident,
                literal: String::from("result"),
            },
            Token {
                token_type: TokenType::Assign,
                literal: String::from("="),
            },
            Token {
                token_type: TokenType::Ident,
                literal: String::from("add"),
            },
            Token {
                token_type: TokenType::LParen,
                literal: String::from("("),
            },
            Token {
                token_type: TokenType::Ident,
                literal: String::from("x"),
            },
            Token {
                token_type: TokenType::Comma,
                literal: String::from(","),
            },
            Token {
                token_type: TokenType::Ident,
                literal: String::from("y"),
            },
            Token {
                token_type: TokenType::RParen,
                literal: String::from(")"),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: String::from(";"),
            },
        ];

        let mut lexer = Lexer::new(input);

        for expected_token in expected_tokens {
            let token = lexer.next_token()?;

            println!("{:?}", token);

            // assert_eq!(token.literal, expected_token.literal);
            assert_eq!(token.token_type, expected_token.token_type);
        }

        Ok(())
    }
}