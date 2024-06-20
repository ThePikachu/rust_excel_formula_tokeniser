use std::collections::VecDeque;

use crate::token;
use crate::enums;

use token::Token;
use enums::{TokenType, TokenSubType};

pub struct TokenStack {
    items: VecDeque<Token>,
}

impl TokenStack {
    pub fn new() -> Self {
        TokenStack {
            items: VecDeque::new(),
        }
    }

    pub fn push(&mut self, token: Token) {
        self.items.push_back(token);
    }

    // JS POP has name, should not be required since value is required
    pub fn pop(&mut self, name: Option<String>) -> Token {
        let token = self.items.pop_back().unwrap();
        Token::new(name.unwrap_or_default(), token.token_type, Some(TokenSubType::Stop))
    }

    pub fn token(&self) -> Option<&Token> {
        self.items.back()
    }

    pub fn value(&self) -> String {
        self.token().map(|t| t.token_value.to_string()).expect("token stack value should return a string")
    }

    pub fn token_type(&self) -> TokenType {
        self.token().map(|t| t.token_type.clone()).unwrap()
    }

    pub fn subtype(&self) -> Option<TokenSubType> {
        self.token().map(|t| t.token_subtype.clone()).unwrap_or_default()
    }
}