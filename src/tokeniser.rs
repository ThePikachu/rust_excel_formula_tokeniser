use crate::token;
use crate::enums;
use crate::tokens;
use crate::token_stack;

use token_stack::TokenStack;
use token::Token;
use tokens::Tokens;
use enums::{TokenType, TokenSubType};
use regex::Regex;
use lazy_static::lazy_static;

lazy_static! {
    static ref SCIENTIFIC_REGEX: Regex = Regex::new(r"^[1-9](\.[0-9]+)?E$").unwrap();
    static ref NEWLINE_MATCHER: Regex = Regex::new(r"\r\n|\r|\n").unwrap();
}

fn get_current_char(formula: &str, offset: usize) -> char {
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
    formula.chars().nth(offset + 1).unwrap()
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

fn check_and_add_token(token: &mut String, token_type: TokenType, token_subtype: Option<TokenSubType>, tokens: &mut Tokens) -> Result<Token,bool> {
    let cleaned_token_str = NEWLINE_MATCHER.replace_all(token, "");
    let cleaned_token = cleaned_token_str.to_string();

    // Clear regardless
    *token = String::new();

    if !cleaned_token.is_empty() {
        // let subtype = token_subtype.unwrap();
        return Ok(tokens.add(cleaned_token, token_type, token_subtype));
        
    }

    Err(false)
}

fn is_scientific_notation(token: &str) -> bool {
    SCIENTIFIC_REGEX.is_match(token)
}

pub fn tokenize(mut formula: &str) -> Tokens {

    // trim string, remove =
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

        let current_char = get_current_char(formula, offset);

        // handle double quoted strings e.g "FOO"
        if inString {
            if current_char == '"' {
                if next_char(formula, offset) == '"' {
                    token.push('"');
                    offset += 1;
                } 
                else {
                    inString = false;
                    tokens.add(token.to_string(), TokenType::Operand, Some(TokenSubType::Text));
                    token = String::new();
                }
            }
            else {
                token.push(current_char)
            }
            offset += 1;
            continue;
        }

        // handle single quoted strings e.g 'FOO'
        if inPath {
            if current_char == '\'' {
                if next_char(formula, offset) == '\'' {
                    token.push('\'');
                    offset += 1;
                } else {
                    inPath = false;
                    token.push('\''); 
                }
            } else {
                token.push(current_char);
            }
            offset += 1;
            continue;
        }

        if inRange {
            if current_char == ']' {
                inRange = false;
            } 

            token.push(current_char);
            offset += 1;
            continue;
        }

        // handle error values
        if inError {
            token.push(current_char);
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
            if current_char.is_ascii_digit() {
                token.push(current_char);
                offset += 1;
                continue;
            }
            else if "+=".contains(current_char) && is_scientific_notation(&token) {
                token.push(current_char);
                offset += 1;
                continue;
            }
            else {
                inNumeric = false;
                tokens.add(token.to_string(), TokenType::Operand, Some(TokenSubType::Number));
                token = String::new();
            }
        }

        // handle scientific notations
        if "+=".contains(current_char) {
            if token.len() > 1 && is_scientific_notation(&token) {
                token.push(current_char);
                offset += 1;
                continue;
            }
        }

        // handle argument separator 
        // TODO:: add support for locale specific argument separators US = ',' EU = ';' if needed
        if current_char == ',' &&  ["ARRAY", "ARRAYROW"].contains(&token.as_str()) {
           check_and_add_token(&mut token, TokenType::Operand, None, &mut tokens);

           if (token_stack.token_type() == TokenType::Function) {
                tokens.add(",".to_string(), TokenType::Argument, None);

                offset += 1;
                continue;
           }
        }

        // handle horizontal array separator
        // TODO:: add support for locale specific horizontal separators US = ',' EU = '.'
        if current_char == ',' {
            check_and_add_token(&mut token, TokenType::Operand, None, &mut tokens);

            if (token_stack.token_type() == TokenType::Function) {
                tokens.add(",".to_string(), TokenType::Argument, None);
           }
           else {
            tokens.add(current_char.to_string(), TokenType::OperatorInfix, Some(TokenSubType::Union));
           }

           offset += 1;
           continue;
        }



        if current_char.is_ascii_digit() && (token.is_empty() || 
        is_previous_non_digit_blank(formula, offset)  && !is_next_non_digit_the_range_operator(formula, offset)) {
            inNumeric = true;
            token.push(current_char);
            offset += 1;
            continue;
        }

        if current_char == '"' {
            let _ = check_and_add_token(&mut token, TokenType::Unknown, None, &mut tokens);

            inString = true;
            offset += 1;
            continue;
        }

        if current_char == '\'' {
            let _ = check_and_add_token(&mut token, TokenType::Unknown, None, &mut tokens);
            token = "'".to_string();
            inPath = true;
            offset += 1;
            continue;
        }

        if current_char == '[' {
            inRange = true;
            token.push(current_char);
            offset += 1;
            continue;
        }

        if current_char == '#' {
            check_and_add_token(&mut token, TokenType::Unknown, None,&mut tokens);

            inError = true;
            token.push(current_char);
            offset += 1;
            continue;  
        }

        // handle array formula start
        if current_char == '{' {
            check_and_add_token(&mut token, TokenType::Unknown, None, &mut tokens);

            token_stack.push(tokens.add("ARRAY".to_string(), TokenType::Function, Some(TokenSubType::Start)));
            token_stack.push(tokens.add("ARRAYROW".to_string(), TokenType::Function, Some(TokenSubType::Start)));
            offset += 1;
            continue;
        }

        // vertical array separator
        if current_char == ';' {
            check_and_add_token(&mut token, TokenType::Operand, None,&mut tokens);

            tokens.add_ref(token_stack.pop(Some("ARRAYROW".to_string())));

            if token_stack.token_type() == TokenType::Function {
                tokens.add(";".to_string(), TokenType::Argument, None);
            }

            token_stack.push(tokens.add("ARRAYROW".to_string(), TokenType::Function, Some(TokenSubType::Start)));
            offset += 1;
            continue;
        }

        // handle array formula end
        if current_char == '}' {
            check_and_add_token(&mut token, TokenType::Operand, None, &mut tokens);

            tokens.add_ref(token_stack.pop(Some("ARRAYROW".to_string())));
            tokens.add_ref(token_stack.pop(Some("ARRAY".to_string())));
            offset += 1;
            continue;
        }

        // trim whitespace
        if current_char == ' ' {
            check_and_add_token(&mut token, TokenType::Operand, None, &mut tokens);

            tokens.add(' '.to_string(), TokenType::WhiteSpace, None);
            offset += 1;
            while get_current_char(formula, offset) == ' ' && !eof(formula, offset) {
                offset += 1;
            }
            continue;
        }

        // multi character comparators
        if ["<>", "<=", ">="].contains(&double_char(formula, offset)) {
            check_and_add_token(&mut token, TokenType::Operand, None, &mut tokens);

            tokens.add(double_char(formula, offset).to_string(), TokenType::OperatorInfix, Some(TokenSubType::Logical));
            offset += 2;
            continue;
        }

        // standard infix operators
        if "+-*/^&=><".contains(current_char) {
            check_and_add_token(&mut token, TokenType::Operand, None,&mut tokens);

            tokens.add(current_char.to_string(), TokenType::OperatorInfix, None);
            offset += 1;
            continue;
        }

        // standard postfix operators
        if "%".contains(current_char) {
            check_and_add_token(&mut token, TokenType::Operand, None,&mut tokens);

            tokens.add("%".to_string(), TokenType::OperatorPostfix, None);
            offset += 1;
            continue;
        }

        // subexpression or function start
        if current_char == '(' {
            match check_and_add_token(&mut token, TokenType::Function, Some(TokenSubType::Start), &mut tokens) {
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

        // subexpression or function end
        if current_char == ')' {
            let _ = check_and_add_token(&mut token, TokenType::Operand, None, &mut tokens);

            tokens.add_ref(token_stack.pop(None));
            offset += 1;
            continue;
        }

        token.push(current_char);
        offset += 1;

    } // EOF

    // dump last token
    let _ = check_and_add_token(&mut token, TokenType::Operand, None, &mut tokens);

    // IMPORTANT NOTE: 
    // THIS SECTION ONWARDS IS NOT IMPORTANT IF YOU ARE ONLY INTERESTED IN GENERAL TOKENIZATION OF TYPES (FUNCTIONS, OPERANDS, OPERATORS, ETC.)
    // THIS SECTION ONWARD MAKES THE TYPE AND SUBTYPE MORE SPECIFIC FOR SOME TYPES AND REMOVES WHITESPACE (not relevant to functions)
    // SIMPLY RETURN TOKENS IF THIS IS NOT UNNECESSARY
    // return tokens;

    let mut tokens2 = Tokens::new();

    // remove white space tokens
    while tokens.move_next() {
        let at_eof = tokens.eof();
        let at_bof = tokens.bof();
        let previous_token = tokens.previous();
        let next_token = tokens.next();
        
        if let Some(token) = tokens.current() {
            if token.token_type == TokenType::WhiteSpace {
                if at_bof || at_eof {
                    // no-op
                }
                else {
                    let prev_valid = if let Some(prev_token) = previous_token {
                        matches!(
                            prev_token,
                            Token {
                                token_type: TokenType::Function,
                                token_subtype: Some(TokenSubType::Stop),
                                ..
                            }
                            | Token {
                                token_type: TokenType::Subexpression,
                                token_subtype: Some(TokenSubType::Stop),
                                ..
                            }
                            | Token {
                                token_type: TokenType::Operand,
                                ..
                            }
                        )
                    } else {
                        false
                    };

                    let next_valid = if let Some(next_token) = next_token {
                        matches!(
                            next_token,
                            Token {
                                token_type: TokenType::Function,
                                token_subtype: Some(TokenSubType::Start),
                                ..
                            }
                            | Token {
                                token_type: TokenType::Subexpression,
                                token_subtype: Some(TokenSubType::Start),
                                ..
                            }
                            | Token {
                                token_type: TokenType::Operand,
                                ..
                            }
                        )
                    } else {
                        false
                    };

                    if prev_valid && next_valid {
                        tokens2.add(
                            token.token_value.clone(),
                            TokenType::OperatorInfix,
                            Some(TokenSubType::Intersect),
                        );
                    }
                }
                continue;
        }
        tokens2.add_ref(token.clone());
        }
    } // white space removal end

    // switch infix "-" operator to prefix
    // switch infix "+" operator to noop
    // identify operand and infix-operator subtypes
    // pull "@" from in front of function names
    while (tokens2.move_next()) {
        let at_bof = tokens2.bof();
        let prev_token_type_and_subtype = tokens2.previous().map(|prev| (prev.token_type.clone(), prev.token_subtype.clone()));
        
        if let Some(token) = tokens2.current() {
            if token.token_type == TokenType::OperatorInfix && token.token_value == "-" {
                if (at_bof) {
                    token.token_type = TokenType::OperatorPrefix;
                }
                else if let Some((prev_type, prev_subtype)) = prev_token_type_and_subtype {
                    match (prev_type, prev_subtype) {
                        (TokenType::Function, Some(TokenSubType::Stop)) |
                        (TokenType::Subexpression, Some(TokenSubType::Stop)) |
                        (TokenType::OperatorPostfix, _) |
                        (TokenType::Operand, _) => {
                            token.token_subtype = Some(TokenSubType::Math);
                        }
                        _ => {
                            token.token_type = TokenType::OperatorPrefix;
                        }
                    
                    }
                }
                continue;
            } // end of infix "-" operator to prefix

            if token.token_type == TokenType::OperatorInfix && token.token_value == "+" {
                if (at_bof) {
                    token.token_type = TokenType::NoOp;
                }
                else if let Some((prev_type, prev_subtype)) = prev_token_type_and_subtype {
                    match (prev_type, prev_subtype) {
                        (TokenType::Function, Some(TokenSubType::Stop)) |
                        (TokenType::Subexpression, Some(TokenSubType::Stop)) |
                        (TokenType::OperatorPostfix, _) |
                        (TokenType::Operand, _) => {
                            token.token_subtype = Some(TokenSubType::Math);
                        }
                        _ => {
                            token.token_type = TokenType::NoOp;
                        }
                    }
                }
                continue;
            }  // end of infix "+" operator to noop

            if token.token_type == TokenType::OperatorInfix && token.token_subtype.is_none() {
                if let Some(first_char) = token.token_value.chars().next() {
                    if "<>=".contains(first_char) {
                        token.token_subtype = Some(TokenSubType::Logical);
                    } else if first_char == '&' {
                        token.token_subtype = Some(TokenSubType::Concatenate);
                    } else {
                        token.token_subtype = Some(TokenSubType::Math);
                    }
                }
                continue;
            } // end of infix operator subtypes

            // MISSING CODE: convert number operands token_value to use 100,000 (default) or 100.000 based on language (LINE:624-640 of js)
            if token.token_type == TokenType::Operand && token.token_subtype.is_none() {
                match token.token_value.parse::<i32>() {
                    Ok(number) => {
                        token.token_subtype = Some(TokenSubType::Number);
                    },
                    Err(_) => {
                        if ["TRUE", "FALSE"].contains(&token.token_value.as_str()) {
                            token.token_subtype = Some(TokenSubType::Logical); 
                        }
                        else {
                            token.token_subtype = Some(TokenSubType::Range);
                        }
                    }
                }
                continue;
            } // end of operand subtypes

            if token.token_type == TokenType::Function {
                if let Some(first_char) = token.token_value.chars().next() {
                    if first_char == '@' {
                        token.token_value = token.token_value.chars().skip(1).collect();
                    }
                }
                continue;
            } // end of removing @ from function
        } 

    } // end of tokens2 loop

    // reset tracker index
    tokens2.reset(); 

    tokens = Tokens::new();

    while tokens2.move_next() {
        let token = tokens2.current().unwrap();

        if token.token_type != TokenType::NoOp {
            tokens.add_ref(token.clone());
            continue;
        }
    }
    
    return tokens;  
}