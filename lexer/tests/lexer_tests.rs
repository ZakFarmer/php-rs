use anyhow::Error;

use token::TokenType;

use lexer::{token::Token, *};

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
