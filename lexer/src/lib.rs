use token::{Token, TokenType};

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

        let (token_type, value) = match self.ch {
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

                let value = self.read_string();

                (TokenType::String, value)
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
                            value: var_name,
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
                    let value = self.read_identifier();

                    return Token {
                        token_type: TokenType::lookup_ident(&value),
                        value,
                    };
                } else if ch.is_ascii_digit() {
                    let value = self.read_number();

                    return Token {
                        token_type: TokenType::Int,
                        value,
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
            value,
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
