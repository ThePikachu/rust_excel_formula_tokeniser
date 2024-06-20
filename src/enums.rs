
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
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
pub enum TokenSubType {
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