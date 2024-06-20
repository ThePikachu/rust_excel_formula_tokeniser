use std::collections::VecDeque;
use crate::token;
use crate::enums;

use enums::{TokenType, TokenSubType};
use token::Token;

pub struct Tokens {
    pub items: VecDeque<Token>,
    pub index: isize,
}

impl Tokens {
    pub fn new() -> Self {
        Tokens {
            items: VecDeque::new(),
            index: -1,
        }
    }

    // need to clone?
    pub fn add(&mut self, value: String, token_type: TokenType, token_subtype: Option<TokenSubType>) -> Token {
        let token = Token::new(  value, token_type, token_subtype,);
        self.items.push_back(token.clone());
        token
    }

    // need to clone?
    pub fn add_ref(&mut self, token: Token) {
        self.items.push_back(token.clone());
    }

    pub fn reset(&mut self) {
        self.index = -1;
    }

    pub fn bof(&self) -> bool {
        return self.index <= 0;
    }

    pub fn eof(&self) -> bool {
        return self.index >= self.items.len().saturating_sub(1) as isize;
    }


    pub fn move_next(&mut self) -> bool {
        if self.eof() { return false };
        self.index += 1;
        true
    }

    pub fn current(&mut self) -> Option<&mut Token> {
        if self.index == -1 || self.index as usize >= self.items.len() {
            None
        } else {
            Some(&mut self.items[self.index as usize])
        }
    }

    pub fn previous(&self) -> Option<Token> {
        if self.index < 1 { return None };
        return Some(self.items[self.index as usize - 1].clone());
    }

    pub fn next(&self) -> Option<Token> {
       if self.eof() { return None };
        return Some(self.items[self.index as usize + 1].clone());
    }
}