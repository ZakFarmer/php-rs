#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum TokenType {
    Illegal,
    Eof,

    Ident,
    Int,
    Bool,

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
    Dollar,
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl TokenType {
    pub fn lookup_ident(ident: &str) -> TokenType {
        match ident {
            "function" => TokenType::Function,
            "true" | "false" => TokenType::Bool,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "return" => TokenType::Return,
            _ => TokenType::Ident,
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.literal)
    }
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let token_type = match self {
            TokenType::Illegal => "Illegal",
            TokenType::Eof => "Eof",
            TokenType::Ident => "Ident",
            TokenType::Int => "Int",
            TokenType::Bool => "Bool",
            TokenType::Eq => "Eq",
            TokenType::NotEq => "NotEq",
            TokenType::Assign => "Assign",
            TokenType::Plus => "Plus",
            TokenType::Comma => "Comma",
            TokenType::Semicolon => "Semicolon",
            TokenType::LParen => "LParen",
            TokenType::RParen => "RParen",
            TokenType::LBrace => "LBrace",
            TokenType::RBrace => "RBrace",
            TokenType::Function => "Function",
            TokenType::Dollar => "Dollar",
            TokenType::Minus => "Minus",
            TokenType::Slash => "Slash",
            TokenType::Asterisk => "Asterisk",
            TokenType::Lt => "Lt",
            TokenType::Gt => "Gt",
            TokenType::Bang => "Bang",
            TokenType::If => "If",
            TokenType::Else => "Else",
            TokenType::Return => "Return",
        };

        write!(f, "{}", token_type)
    }
}