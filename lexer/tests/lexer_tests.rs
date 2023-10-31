use anyhow::Error;

use token::{Token, TokenType};

use lexer::*;

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
            value: "$five".to_string(),
        },
        Token {
            token_type: TokenType::Assign,
            value: "=".to_string(),
        },
        Token {
            token_type: TokenType::Int,
            value: "5".to_string(),
        },
        Token {
            token_type: TokenType::Semicolon,
            value: ";".to_string(),
        },
        Token {
            token_type: TokenType::Ident,
            value: "$ten".to_string(),
        },
        Token {
            token_type: TokenType::Assign,
            value: "=".to_string(),
        },
        Token {
            token_type: TokenType::Ident,
            value: "$five".to_string(),
        },
        Token {
            token_type: TokenType::Plus,
            value: "+".to_string(),
        },
        Token {
            token_type: TokenType::Int,
            value: "5".to_string(),
        },
        Token {
            token_type: TokenType::Semicolon,
            value: ";".to_string(),
        },
        Token {
            token_type: TokenType::Ident,
            value: "$add".to_string(),
        },
        Token {
            token_type: TokenType::Assign,
            value: "=".to_string(),
        },
        Token {
            token_type: TokenType::Function,
            value: "function".to_string(),
        },
        Token {
            token_type: TokenType::LParen,
            value: "(".to_string(),
        },
        Token {
            token_type: TokenType::Ident,
            value: "$x".to_string(),
        },
        Token {
            token_type: TokenType::Comma,
            value: ",".to_string(),
        },
        Token {
            token_type: TokenType::Ident,
            value: "$y".to_string(),
        },
        Token {
            token_type: TokenType::RParen,
            value: ")".to_string(),
        },
        Token {
            token_type: TokenType::LBrace,
            value: "{".to_string(),
        },
        Token {
            token_type: TokenType::Return,
            value: "return".to_string(),
        },
        Token {
            token_type: TokenType::Ident,
            value: "$x".to_string(),
        },
        Token {
            token_type: TokenType::Plus,
            value: "+".to_string(),
        },
        Token {
            token_type: TokenType::Ident,
            value: "$y".to_string(),
        },
        Token {
            token_type: TokenType::Semicolon,
            value: ";".to_string(),
        },
        Token {
            token_type: TokenType::RBrace,
            value: "}".to_string(),
        },
        Token {
            token_type: TokenType::Ident,
            value: "$result".to_string(),
        },
        Token {
            token_type: TokenType::Assign,
            value: "=".to_string(),
        },
        Token {
            token_type: TokenType::Ident,
            value: "$add".to_string(),
        },
        Token {
            token_type: TokenType::LParen,
            value: "(".to_string(),
        },
        Token {
            token_type: TokenType::Ident,
            value: "$five".to_string(),
        },
        Token {
            token_type: TokenType::Comma,
            value: ",".to_string(),
        },
        Token {
            token_type: TokenType::Ident,
            value: "$ten".to_string(),
        },
        Token {
            token_type: TokenType::RParen,
            value: ")".to_string(),
        },
        Token {
            token_type: TokenType::Semicolon,
            value: ";".to_string(),
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
