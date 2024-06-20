
use crate::enums;
use enums::{TokenType, TokenSubType};


#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub token_subtype: Option<TokenSubType>,
    pub token_value: String,
}

impl Token {
    pub fn new(token_value: String, token_type: TokenType, token_subtype: Option<TokenSubType>, ) -> Token {
        Token {
            token_value,
            token_type,
            token_subtype,
        }
    }
}