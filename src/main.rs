
use std::collections::VecDeque;
use std::str::Chars;
use regex::Regex;
use lazy_static::lazy_static;

#[derive(Debug, PartialEq, Clone)]
enum TokenType {
    NoOp,
    Operand,
    Function,
    Subexpression,
    Argument,
    OperatorPrefix,
    OperatorInfix,
    OperatorPostfix,
    WhiteSpace,
    Unknown,
}

#[derive(Debug, PartialEq, Clone)]
enum TokenSubType {
    Start,
    Stop,
    Text,
    Number,
    Logical,
    Error,
    Range,
    Math,
    Concatenate,
    Intersect,
    Union,
}

#[derive(Debug, PartialEq, Clone)]
enum TokenValue {
    Array,
    ArrayRow,
    True,
    False,
    String
}

// Define a struct to hold the token information
#[derive(Debug, Clone)]
struct Token {
    token_type: TokenType,
    token_subtype: Option<TokenSubType>,
    token_value: String,
}

impl Token {
    fn new(token_value: String, token_type: TokenType, token_subtype: Option<TokenSubType>, ) -> Token {
        Token {
            token_value,
            token_type,
            token_subtype,
        }
    }
}

// const TOK_VALUE_TRUE: bool = true;
// const TOK_VALUE_FALSE: bool = false;

struct Tokens {
    items: VecDeque<Token>,
    index: isize,
}

impl Tokens {
    fn new() -> Self {
        Tokens {
            items: VecDeque::new(),
            index: -1,
        }
    }

    // need to clone?
    fn add(&mut self, value: String, token_type: TokenType, token_subtype: Option<TokenSubType>) -> Token {
        let token = Token::new(  value, token_type, token_subtype,);
        self.items.push_back(token.clone());
        token
    }

    // need to clone?
    fn add_ref(&mut self, token: Token) {
        self.items.push_back(token.clone());
    }

    fn reset(&mut self) {
        self.index = -1;
    }

    fn move_next(&mut self) -> bool {
        if (self.index as usize) >= self.items.len() - 1 {
            false
        } else {
            self.index += 1;
            true
        }
    }

    fn current(&self) -> Option<Token> {
        if self.index == -1 || self.index as usize >= self.items.len() {
            None
        } else {
            Some(self.items[self.index as usize].clone())
        }
    }
}

struct TokenStack {
    items: VecDeque<Token>,
}

impl TokenStack {
    fn new() -> Self {
        TokenStack {
            items: VecDeque::new(),
        }
    }

    fn push(&mut self, token: Token) {
        self.items.push_back(token);
    }

    // JS POP has name, should not be required since value is required
    fn pop(&mut self) -> Token {
        let token = self.items.pop_back().unwrap();
        Token::new(token.token_value, token.token_type, Some(TokenSubType::Stop))
    }

    fn token(&self) -> Option<&Token> {
        self.items.back()
    }

    fn value(&self) -> String {
        self.token().map(|t| t.token_value.to_string()).expect("token stack value should return a string")
    }

    fn token_type(&self) -> TokenType {
        self.token().map(|t| t.token_type.clone()).unwrap()
    }

    fn subtype(&self) -> Option<TokenSubType> {
        self.token().map(|t| t.token_subtype.clone()).unwrap_or_default()
    }
}

// Function to create a new Token with a default subtype
// TODO :: redudant
fn create_token(value: String, token_type: TokenType, token_subtype: Option<TokenSubType>) -> Token {
    Token::new(value, token_type, token_subtype)
}


fn current_char(formula: &str, offset: usize) -> char {
    formula.chars().nth(offset).unwrap_or('\0')
}

fn double_char(formula: &str, offset: usize) -> &str {
    // handle out of bounds gracefully
    if offset + 2 > formula.len() {
        return "";
    }

    &formula[offset..offset+2]
}

fn next_char(formula: &str, offset: usize) -> char {
    formula.chars().nth(offset + 1).unwrap_or('\0')
}

fn eof(formula: &str, offset: usize) -> bool {
    offset >= formula.len()
}

fn is_previous_non_digit_blank(formula: &str, offset: usize) -> bool {
    let mut offset_copy = offset;
    if offset_copy == 0 {
        return true;
    }

    while offset_copy > 0 {
        if !formula.chars().nth(offset_copy - 1).unwrap().is_ascii_digit() {
            return formula.chars().nth(offset_copy - 1).unwrap().is_whitespace();
        }
        offset_copy -= 1;
    }
    false
}

fn is_next_non_digit_the_range_operator(formula: &str, offset: usize) -> bool {
    let mut offset_copy = offset;

    while offset_copy < formula.len() {
        if !formula.chars().nth(offset_copy).unwrap().is_ascii_digit() {
            return formula.chars().nth(offset_copy).unwrap() == ':';
        }
        offset_copy += 1;
    }
    false
}

fn check_and_add_token(token: &mut String, token_type: TokenType,  tokens: &mut Tokens) -> Result<Token,bool> {
    let cleaned_token_str = NEWLINE_MATCHER.replace_all(token, "");
    let cleaned_token = cleaned_token_str.to_string();

    // Clear regardless
    *token = String::new();

    if !cleaned_token.is_empty() {
        return Ok(tokens.add(cleaned_token, token_type, None));
        
    }

    Err(false)
}

fn is_scientific_notation(token: &str) -> bool {
    SCIENTIFIC_REGEX.is_match(token)
}

lazy_static! {
    static ref SCIENTIFIC_REGEX: Regex = Regex::new(r"^[1-9](\.[0-9]+)?E$").unwrap();
    static ref NEWLINE_MATCHER: Regex = Regex::new(r"\r\n|\r|\n").unwrap();
}

// TODO: refactor current_char(formula, offset)
fn tokenize(mut formula: &str) -> Tokens {

    // trim string
    while formula.len() > 0 {
        if formula.starts_with(" ") {
            formula = &formula[1..];
        } else {
            if formula.starts_with("=") {
                formula = &formula[1..];
            }
            break;
        }
    }

    let mut tokens = Tokens::new();
    let mut token_stack = TokenStack::new();

    // current index of the string
    let mut offset = 0;
    // current token string value
    let mut token = String::new();

    let mut inString = false;
    let mut inPath = false;
    let mut inRange = false;
    let mut inError = false;
    let mut inNumeric = false;

    while !eof(formula, offset) {

        // handle double quoted strings e.g "FOO"
        if inString {
            if current_char(formula, offset) == '"' {
                if next_char(formula, offset) == '"' {
                    token.push('"');
                    offset += 1;
                } 
                else {
                    inString = false;
                    tokens.add(token.to_string(), TokenType::Operand, Some(TokenSubType::Text));
                }
            }
            else {
                token.push(current_char(formula, offset))
            }
            offset += 1;
            continue;
        }

        // handle single quoted strings e.g 'FOO'
        if inPath {
            if current_char(formula, offset) == '\'' {
                if next_char(formula, offset) == '\'' {
                    token.push('\'');
                    offset += 1;
                } else {
                    inPath = false;
                    token.push('\''); 
                }
            } else {
                token.push(current_char(formula, offset));
            }
            offset += 1;
            continue;
        }

        if inRange {
            if current_char(formula, offset) == ']' {
                inRange = false;
            } 

            token.push(current_char(formula, offset));
            offset += 1;
            continue;
        }

        // handle error values
        if inError {
            token.push(current_char(formula, offset));
            offset += 1;

            if ",#NULL!,#DIV/0!,#VALUE!,#REF!,#NAME?,#NUM!,#N/A,".contains(&format!(",{},", token)) {
                inError = false;
                tokens.add(token.to_string(), TokenType::Operand, Some(TokenSubType::Error));
                token = String::new();
            }
            continue;
        }

        // handle numbers
        if inNumeric {
            if current_char(formula, offset).is_ascii_digit() {
                token.push(current_char(formula, offset));
                offset += 1;
                continue;
            }
            else if "+=".contains(current_char(formula, offset)) && is_scientific_notation(&token) {
                token.push(current_char(formula, offset));
                offset += 1;
                continue;
            }
            else {
                inNumeric = false;
                tokens.add(token.to_string(), TokenType::Operand, Some(TokenSubType::Number));
            }
        }

        // handle scientific notations
        if "+=".contains(current_char(formula, offset)) {
            if token.len() > 1 && is_scientific_notation(&token) {
                token.push(current_char(formula, offset));
                offset += 1;
                continue;
            }
        }

        // handle argument separator 
        // TODO:: add support for locale specific argument separators US = ',' EU = ';'
        if current_char(formula, offset) == ',' &&  ["ARRAY", "ARRAYROW"].contains(&token.as_str()) {
           check_and_add_token(&mut token, TokenType::Operand, &mut tokens);

           if (token_stack.token_type() == TokenType::Function) {
                tokens.add(",".to_string(), TokenType::Argument, None);

                offset += 1;
                continue;
           }
        }

        // TODO:: add support for locale specific horizontal separators US = ',' EU = '.'
        if current_char(formula, offset) == ',' {
            check_and_add_token(&mut token, TokenType::Operand, &mut tokens);

            if (token_stack.token_type() == TokenType::Function) {
                tokens.add(",".to_string(), TokenType::Argument, None);
           }
           else {
            tokens.add(current_char(formula, offset).to_string(), TokenType::OperatorInfix, Some(TokenSubType::Union));
           }

           offset += 1;
           continue;
        }

        if current_char(formula,offset).is_ascii_digit() && (token.is_empty() || 
        is_previous_non_digit_blank(formula, offset)  && !is_next_non_digit_the_range_operator(formula, offset)) {
            inNumeric = true;
            token.push(current_char(formula, offset));
            offset += 1;
            continue;
        }

        if current_char(formula, offset) == '"' {
            check_and_add_token(&mut token, TokenType::Unknown, &mut tokens);

            inString = true;
            offset += 1;
            continue;
        }

        if current_char(formula, offset) == '\'' {
            check_and_add_token(&mut token, TokenType::Unknown, &mut tokens);

            token = "'".to_string();
            inPath = true;
            offset += 1;
            continue;
        }

        if current_char(formula, offset) == '[' {
            inRange = true;
            token.push(current_char(formula, offset));
            offset += 1;
            continue;
        }

        if current_char(formula, offset) == '#' {
            check_and_add_token(&mut token, TokenType::Unknown, &mut tokens);

            inError = true;
            token.push(current_char(formula, offset));
            offset += 1;
            continue;  
        }

        // handle array

        if current_char(formula, offset) == '{' {
            check_and_add_token(&mut token, TokenType::Unknown, &mut tokens);

            token_stack.push(tokens.add("ARRAY".to_string(), TokenType::Function, Some(TokenSubType::Start)));
            token_stack.push(tokens.add("ARRAYROW".to_string(), TokenType::Function, Some(TokenSubType::Start)));
            offset += 1;
            continue;
        }

        // vertical separator
        if current_char(formula, offset) == ';' {
            check_and_add_token(&mut token, TokenType::Operand, &mut tokens);

            tokens.add_ref(token_stack.pop());

            if token_stack.token_type() == TokenType::Function {
                tokens.add(";".to_string(), TokenType::Argument, None);
            }

            token_stack.push(tokens.add("ARRAYROW".to_string(), TokenType::Function, Some(TokenSubType::Start)));
            offset += 1;
            continue;
        }


        if current_char(formula, offset) == '}' {
            check_and_add_token(&mut token, TokenType::Operand, &mut tokens);

            tokens.add_ref(token_stack.pop());
            tokens.add_ref(token_stack.pop());
            offset += 1;
            continue;
        }

        // trim whitespace
        if current_char(formula, offset) == ' ' {
            check_and_add_token(&mut token, TokenType::Operand, &mut tokens);

            tokens.add(' '.to_string(), TokenType::WhiteSpace, None);
            offset += 1;
            while (current_char(formula, offset) == ' ' && !eof(formula, offset)) {
                offset += 1;
            }
            continue;
        }

        // multi character comparators

        if ["<>", "<=", ">="].contains(&double_char(formula, offset)) {
            check_and_add_token(&mut token, TokenType::Operand, &mut tokens);

            tokens.add(double_char(formula, offset).to_string(), TokenType::OperatorInfix, Some(TokenSubType::Logical));
            offset += 2;
            continue;
        }

        // standard infix operators
        if "+-*/^&=><".contains(current_char(formula, offset)) {
            check_and_add_token(&mut token, TokenType::Operand, &mut tokens);

            tokens.add(current_char(formula, offset).to_string(), TokenType::OperatorInfix, None);
            offset += 1;
            continue;
        }

        // standard postfix operators
        if "%".contains(current_char(formula, offset)) {
            check_and_add_token(&mut token, TokenType::Operand, &mut tokens);

            tokens.add("%".to_string(), TokenType::OperatorPostfix, None);
            offset += 1;
            continue;
        }

        // subexpression or function
        if current_char(formula, offset) == '(' {
            match check_and_add_token(&mut token, TokenType::Function, &mut tokens) {
                Ok(cleaned_token) => {
                    token_stack.push(cleaned_token);
                }
                Err(_) => {
                    token_stack.push(tokens.add("".to_string(), TokenType::Subexpression, Some(TokenSubType::Start)));
                }
            }
            offset += 1;
            continue;
        }

        if current_char(formula, offset) == ')' {
            let _ = check_and_add_token(&mut token, TokenType::Operand, &mut tokens);

            tokens.add_ref(token_stack.pop());
            offset += 1;
            continue;
        }

        token.push(current_char(formula, offset));
        offset += 1;

    } // EOF

    // dump last token
    let _ = check_and_add_token(&mut token, TokenType::Operand, &mut tokens);

    return tokens;
    
}

fn main() {
    println!("Hello, world!");
}
