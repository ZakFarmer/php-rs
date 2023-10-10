#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Token {
    Illegal,
    Eof,

    Ident(String),
    Int(String),
    Bool(bool),

    Eq,
    NotEq,

    Assign,
    Plus,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Function,
    Let,
    Minus,
    Slash,
    Asterisk,
    Lt,
    Gt,
    Bang,

    If,
    Else,
    Return,
}

impl Token {
    pub fn lookup_ident(ident: &str) -> Token {
        match ident {
            "fn" => Token::Function,
            "let" => Token::Let,
            "true" => Token::Bool(true),
            "false" => Token::Bool(false),
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            _ => Token::Ident(ident.to_string()),
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let token = match self {
            Token::Illegal => "Illegal",
            Token::Eof => "Eof",
            Token::Ident(x) => x,
            Token::Int(x) => x,
            Token::Eq => "==",
            Token::NotEq => "!=",
            Token::Assign => "=",
            Token::Plus => "+",
            Token::Comma => ",",
            Token::Semicolon => ";",
            Token::LParen => "(",
            Token::RParen => ")",
            Token::LBrace => "{",
            Token::RBrace => "}",
            Token::Function => "fn",
            Token::Let => "let",
            Token::Minus => "-",
            Token::Slash => "/",
            Token::Asterisk => "*",
            Token::Lt => "<",
            Token::Gt => ">",
            Token::Bang => "!",
            Token::If => "if",
            Token::Else => "else",
            Token::Return => "return",
            _ => "",
        };

        write!(f, "{}", token)
    }
}