use std::fmt::{Debug, Display};

use crate::tokenizer::{Location, TokenType};

#[derive(Clone, Copy)]
pub enum ProgrammingLangError {
    Parsing(ProgrammingLangParsingError),
    Tokenization(ProgrammingLangTokenizationError),
    Generic(&'static str),
}

impl Debug for ProgrammingLangError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Parsing(v) => Debug::fmt(v, f),
            Self::Tokenization(v) => Debug::fmt(v, f),
            Self::Generic(v) => f.write_str(v),
        }
    }
}

impl From<ProgrammingLangParsingError> for ProgrammingLangError {
    fn from(value: ProgrammingLangParsingError) -> Self {
        Self::Parsing(value)
    }
}

impl From<ProgrammingLangTokenizationError> for ProgrammingLangError {
    fn from(value: ProgrammingLangTokenizationError) -> Self {
        Self::Tokenization(value)
    }
}

#[derive(Clone, Copy)]
pub enum ProgrammingLangParsingError {
    ExpectedFunctionArgument {
        loc: Location,
        found: TokenType,
    },
    ExpectedArrayElement {
        loc: Location,
        found: TokenType,
    },
    ExpectedKey {
        loc: Location,
        found: TokenType,
    },
    ExpectedObjectElement {
        loc: Location,
        found: TokenType,
    },
    ExpectedFunctionArgumentExpression {
        loc: Location,
        found: TokenType,
    },
    ExpectedExpression {
        loc: Location,
        found: TokenType,
    },
    ExpectedIdentifier {
        loc: Location,
        found: TokenType,
    },
    CannotDoUnaryOperation {
        loc: Location,
        operation_type: OperationType,
        right: &'static str,
    },
    CannotDoBinaryOperation {
        loc: Location,
        operation_type: OperationType,
        left: &'static str,
        right: &'static str,
    },
    InvalidOperand {
        loc: Location,
        operand_type: TokenType,
    },
    InvalidUnaryOperand {
        loc: Location,
        operand_type: TokenType,
    },
    InvalidTokenization {
        loc: Location,
    },
    AssignmentInvalidLeftSide {
        loc: Location,
    },
    ExpectedArbitrary {
        loc: Location,
        expected: TokenType,
        found: TokenType,
    },
}

#[derive(Clone, Copy)]
pub enum OperationType {
    Plus,
    Minus,
    Multiply,
    Divide,
    IntDivide,
    Modulo,
    LogicalNot,
    LogicalAnd,
    LogicalOr,
    BitwiseAnd,
    BitwiseOr,
    BitwiseNot,
    BitwiseXor,
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,
}
impl OperationType {
    fn to_string(&self) -> &'static str {
        match self {
            Self::BitwiseAnd => "bitwise and",
            Self::BitwiseOr => "bitwise or",
            Self::BitwiseXor => "bitwise xor",
            Self::BitwiseNot => "bitwise not",
            Self::LogicalAnd => "logical and",
            Self::LogicalOr => "logical or",
            Self::LogicalNot => "logical not",
            Self::Plus => "plus",
            Self::Minus => "minus",
            Self::Multiply => "multiply",
            Self::Divide => "divide",
            Self::IntDivide => "integer devide",
            Self::Modulo => "modulo",
            Self::Equals => "equals",
            Self::NotEquals => "not equals",
            Self::LessThan => "less than",
            Self::GreaterThan => "greater than",
            Self::LessThanEquals => "less than or equals",
            Self::GreaterThanEquals => "greater than or equals",
        }
    }
}

impl ProgrammingLangParsingError {
    pub fn get_loc(&self) -> &Location {
        match self {
            Self::CannotDoBinaryOperation { loc, .. }
            | Self::CannotDoUnaryOperation { loc, .. }
            | Self::ExpectedExpression { loc, .. }
            | Self::ExpectedIdentifier { loc, .. }
            | Self::InvalidOperand { loc, .. }
            | Self::ExpectedArrayElement { loc, .. }
            | Self::ExpectedObjectElement { loc, .. }
            | Self::ExpectedKey { loc, .. }
            | Self::ExpectedFunctionArgument { loc, .. }
            | Self::ExpectedFunctionArgumentExpression { loc, .. }
            | Self::InvalidTokenization { loc }
            | Self::AssignmentInvalidLeftSide { loc }
            | Self::ExpectedArbitrary { loc, .. }
            | Self::InvalidUnaryOperand { loc, .. } => loc,
        }
    }
}

impl Debug for ProgrammingLangParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CannotDoBinaryOperation {
                operation_type,
                left,
                right,
                loc,
            } => f.write_fmt(format_args!(
                "{loc}: Cannot do Binary Operation {} on {left} and {right}",
                operation_type.to_string()
            )),
            Self::CannotDoUnaryOperation {
                loc,
                operation_type,
                right,
            } => f.write_fmt(format_args!(
                "{loc}: Cannot do Unary Operation {} on {right}",
                operation_type.to_string()
            )),
            Self::ExpectedExpression { loc, found } => f.write_fmt(format_args!(
                "{loc}: Expected `(` or an expression, but found {found:?}"
            )),
            Self::InvalidOperand { loc, operand_type } => {
                f.write_fmt(format_args!("{loc}: Invalid operand: {operand_type:?}"))
            }
            Self::InvalidUnaryOperand { loc, operand_type } => f.write_fmt(format_args!(
                "{loc}: Invalid unary operand: {operand_type:?}"
            )),
            Self::ExpectedFunctionArgument { loc, found } => f.write_fmt(format_args!(
                "{loc}: Expected `,` or `)`, but found {found:?}"
            )),
            Self::ExpectedFunctionArgumentExpression { loc, found } => f.write_fmt(format_args!(
                "{loc}: Expected an expression or `)`, but found {found:?}"
            )),
            Self::ExpectedIdentifier { loc, found } => f.write_fmt(format_args!(
                "{loc}: Expected an identifier, but found {found:?}"
            )),
            Self::InvalidTokenization { loc } => f.write_fmt(format_args!(
                "{loc}: Incorrect Tokenization (this is an error of the compiler! Report it!)"
            )),
            Self::AssignmentInvalidLeftSide { loc } => f.write_fmt(format_args!(
                "{loc}: Invalid left-hand side of the assignment"
            )),
            Self::ExpectedArbitrary {
                loc,
                expected,
                found,
            } => f.write_fmt(format_args!("{loc}: Expected {expected:?} but found {found:?}")),
            Self::ExpectedArrayElement { loc, found } => f.write_fmt(format_args!("{loc}: Expected , or ], but found {found:?}")),
            Self::ExpectedObjectElement { loc, found } => f.write_fmt(format_args!("{loc}: Expected , or }}, but found {found:?}")),
            Self::ExpectedKey { loc, found } => f.write_fmt(format_args!("{loc}: Expected a key (a string, number or identifier), but found {found:?}")),
        }
    }
}

#[derive(Clone, Copy)]
pub enum ProgrammingLangTokenizationError {
    UnknownTokenError { loc: Location, character: char },
    InvalidNumberError { loc: Location },
    UnclosedString { loc: Location },
}

impl ProgrammingLangTokenizationError {
    pub fn invalid_number(loc: Location) -> Self {
        Self::InvalidNumberError { loc }
    }
    pub fn unclosed_string(loc: Location) -> Self {
        Self::UnclosedString { loc }
    }
    pub fn unknown_token(loc: Location, character: char) -> Self {
        Self::UnknownTokenError { loc, character }
    }

    pub fn get_loc(&self) -> &Location {
        match self {
            Self::UnclosedString { loc }
            | Self::InvalidNumberError { loc }
            | Self::UnknownTokenError { loc, .. } => &loc,
        }
    }
}

impl Debug for ProgrammingLangTokenizationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.get_loc(), f)?;
        f.write_str(": ")?;
        match self {
            Self::UnclosedString { .. } => f.write_str("Expected `\"`, but found nothing"),
            Self::InvalidNumberError { .. } => f.write_str("Could not parse the number"),
            Self::UnknownTokenError { character, .. } => {
                f.write_fmt(format_args!("Unknown Token: `{character}`"))
            }
        }
    }
}
