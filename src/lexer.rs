use crate::token::{Token, TokenType};

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

        let (token_type, literal) = match self.ch {
            Some('=') => {
                if self.peek_char() == '=' {
                    self.read_char();
                    (TokenType::Eq, "==".to_string())
                } else {
                    (TokenType::Assign, "=".to_string())
                }
            }
            Some('"') => {
                self.read_char();

                let literal = self.read_string();

                (TokenType::String, literal)
            }
            Some(';') => (TokenType::Semicolon, ";".to_string()),
            Some('(') => (TokenType::LParen, "(".to_string()),
            Some(')') => (TokenType::RParen, ")".to_string()),
            Some(',') => (TokenType::Comma, ",".to_string()),
            Some('+') => (TokenType::Plus, "+".to_string()),
            Some('{') => (TokenType::LBrace, "{".to_string()),
            Some('}') => (TokenType::RBrace, "}".to_string()),
            Some('[') => (TokenType::LBracket, "[".to_string()),
            Some(']') => (TokenType::RBracket, "]".to_string()),
            Some('-') => (TokenType::Minus, "-".to_string()),
            Some('!') => {
                if self.peek_char() == '=' {
                    self.read_char();
                    (TokenType::NotEq, "!=".to_string())
                } else {
                    (TokenType::Bang, "!".to_string())
                }
            }
            Some('/') => (TokenType::Slash, "/".to_string()),
            Some('*') => (TokenType::Asterisk, "*".to_string()),
            Some('<') => (TokenType::Lt, "<".to_string()),
            Some('>') => (TokenType::Gt, ">".to_string()),
            Some('$') => {
                self.read_char();

                if let Some(ch) = self.ch {
                    if ch.is_alphabetic() || ch == '_' {
                        let identifier = self.read_identifier();
                        let var_name = format!("${}", identifier);

                        return Token {
                            token_type: TokenType::Ident,
                            literal: var_name,
                        };
                    } else {
                        (TokenType::Illegal, "$".to_string())
                    }
                } else {
                    (TokenType::Illegal, "$".to_string())
                }
            }
            Some(ch) => {
                if ch.is_alphabetic() {
                    let literal = self.read_identifier();

                    return Token {
                        token_type: TokenType::lookup_ident(&literal),
                        literal,
                    };
                } else if ch.is_ascii_digit() {
                    let literal = self.read_number();

                    return Token {
                        token_type: TokenType::Int,
                        literal,
                    };
                } else {
                    (TokenType::Illegal, ch.to_string())
                }
            }
            None => (TokenType::Eof, "".to_string()),
        };

        self.read_char();

        Token {
            token_type,
            literal,
        }
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

    fn read_string(&mut self) -> String {
        let position = self.position;

        while match self.ch {
            Some(ch) => ch != '"',
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

    use crate::token::TokenType;

    use super::*;

    #[test]
    fn test_lexer() -> Result<(), Error> {
        let input = "
            $five = 5;

            $ten = $five + 5;

            $add = function ($x, $y) {
                return $x + $y;
            }

            $result = $add($five, $ten);
        "
        .to_string();

        let expected_tokens = [
            Token {
                token_type: TokenType::Ident,
                literal: "$five".to_string(),
            },
            Token {
                token_type: TokenType::Assign,
                literal: "=".to_string(),
            },
            Token {
                token_type: TokenType::Int,
                literal: "5".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "$ten".to_string(),
            },
            Token {
                token_type: TokenType::Assign,
                literal: "=".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "$five".to_string(),
            },
            Token {
                token_type: TokenType::Plus,
                literal: "+".to_string(),
            },
            Token {
                token_type: TokenType::Int,
                literal: "5".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "$add".to_string(),
            },
            Token {
                token_type: TokenType::Assign,
                literal: "=".to_string(),
            },
            Token {
                token_type: TokenType::Function,
                literal: "function".to_string(),
            },
            Token {
                token_type: TokenType::LParen,
                literal: "(".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "$x".to_string(),
            },
            Token {
                token_type: TokenType::Comma,
                literal: ",".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "$y".to_string(),
            },
            Token {
                token_type: TokenType::RParen,
                literal: ")".to_string(),
            },
            Token {
                token_type: TokenType::LBrace,
                literal: "{".to_string(),
            },
            Token {
                token_type: TokenType::Return,
                literal: "return".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "$x".to_string(),
            },
            Token {
                token_type: TokenType::Plus,
                literal: "+".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "$y".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::RBrace,
                literal: "}".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "$result".to_string(),
            },
            Token {
                token_type: TokenType::Assign,
                literal: "=".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "$add".to_string(),
            },
            Token {
                token_type: TokenType::LParen,
                literal: "(".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "$five".to_string(),
            },
            Token {
                token_type: TokenType::Comma,
                literal: ",".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "$ten".to_string(),
            },
            Token {
                token_type: TokenType::RParen,
                literal: ")".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
        ];

        let mut lexer = Lexer::new(input.as_str());

        for expected_token in expected_tokens {
            let token = lexer.next_token();

            if matches!(token.token_type, TokenType::Eof) {
                return Ok(());
            }

            assert_eq!(token, expected_token);
        }

        Ok(())
    }
}
