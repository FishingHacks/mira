use std::{fmt::{Debug, Display}, rc::Rc};

use crate::tokenizer::{Location, TokenType};

#[derive(Clone)]
pub enum ProgrammingLangError {
    Parsing(ProgrammingLangParsingError),
    Tokenization(ProgrammingLangTokenizationError),
    ProgramForming(ProgrammingLangProgramFormingError),
    Generic(&'static str),
}

impl Debug for ProgrammingLangError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Parsing(v) => Debug::fmt(v, f),
            Self::Tokenization(v) => Debug::fmt(v, f),
            Self::ProgramForming(v) => Debug::fmt(v, f),
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

#[derive(Clone)]
pub enum ProgrammingLangParsingError {
    ExpectedType {
        loc: Location,
        found: TokenType,
    },
    ExpectedFunctionCall {
        loc: Location,
    },
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
    InvalidKeyword {
        keyword: &'static str,
        loc: Location,
    },
    FunctionAlreadyDefined {
        loc: Location,
        name: Rc<str>,
        first_func_loc: Location,
    },
    StructImplRegionExpect {
        loc: Location,
        found: TokenType,
        is_trait_impl: bool,
    },
    Eof {
        loc: Location,
    },
    ExpectedStatement {
        loc: Location,
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
            | Self::ExpectedType { loc, .. }
            | Self::ExpectedExpression { loc, .. }
            | Self::ExpectedIdentifier { loc, .. }
            | Self::InvalidOperand { loc, .. }
            | Self::ExpectedArrayElement { loc, .. }
            | Self::ExpectedObjectElement { loc, .. }
            | Self::ExpectedKey { loc, .. }
            | Self::ExpectedFunctionArgument { loc, .. }
            | Self::ExpectedFunctionArgumentExpression { loc, .. }
            | Self::ExpectedFunctionCall { loc }
            | Self::InvalidTokenization { loc }
            | Self::AssignmentInvalidLeftSide { loc }
            | Self::Eof { loc }
            | Self::StructImplRegionExpect { loc, .. }
            | Self::ExpectedArbitrary { loc, .. }
            | Self::InvalidUnaryOperand { loc, .. }
            | Self::FunctionAlreadyDefined { loc, .. }
            | Self::ExpectedStatement { loc, .. }
            | Self::InvalidKeyword { loc, .. } => loc,
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
            Self::ExpectedFunctionCall { loc } => f.write_fmt(format_args!(
                "{loc}: Expected a function call"
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
            } => f.write_fmt(format_args!(
                "{loc}: Expected {expected:?} but found {found:?}"
            )),
            Self::ExpectedArrayElement { loc, found } => {
                f.write_fmt(format_args!("{loc}: Expected , or ], but found {found:?}"))
            }
            Self::ExpectedObjectElement { loc, found } => {
                f.write_fmt(format_args!("{loc}: Expected , or }}, but found {found:?}"))
            }
            Self::ExpectedKey { loc, found } => f.write_fmt(format_args!(
                "{loc}: Expected a key (a string, number or identifier), but found {found:?}"
            )),
            Self::ExpectedType { loc, found } => {
                f.write_fmt(format_args!("{loc}: Expected a type, but found {found:?}"))
            }
            Self::InvalidKeyword { keyword, loc } => {
                f.write_fmt(format_args!("{loc}: {keyword:?} is not allowed here"))
            }
            Self::FunctionAlreadyDefined { loc, name, first_func_loc } => {
                f.write_fmt(format_args!("{loc}: Function `{name}` is already defined\n{first_func_loc}: `{name}` is already defined here"))
            }
            Self::StructImplRegionExpect { loc, found, is_trait_impl: false } => {
                f.write_fmt(format_args!("{loc}: Expected impl, fn or }}, but found {found:?}"))
            }
            Self::StructImplRegionExpect { loc, found, is_trait_impl: true } => {
                f.write_fmt(format_args!("{loc}: Expected fn or }}, but found {found:?}"))
            }
            Self::Eof { loc } => f.write_fmt(format_args!("{loc}: End-of-file")),
            Self::ExpectedStatement { loc }  => f.write_fmt(format_args!("{loc}: Expected a statement")),
        }
    }
}

#[derive(Clone)]
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

#[derive(Clone)]
pub enum ProgrammingLangProgramFormingError {
    NoCodeOutsideOfFunctions(Location),
    AnonymousFunctionAtGlobalLevel(Location),
    GlobalValueNoLiteral(Location),
    GlobalValueNoType(Location),
}

impl Debug for ProgrammingLangProgramFormingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoCodeOutsideOfFunctions(loc) => {
                f.write_fmt(format_args!("{loc}: Code outside of a function boundary"))
            }
            Self::AnonymousFunctionAtGlobalLevel(loc) => f.write_fmt(format_args!(
                "{loc}: There are no anonymous functions at global level allowed"
            )),
            Self::GlobalValueNoLiteral(loc) => f.write_fmt(format_args!(
                "{loc}: global-level let or const expects you to pass a literal"
            )),
            Self::GlobalValueNoType(loc) => f.write_fmt(format_args!(
                "{loc}: global-level const expects you to pass a type"
            )),
        }
    }
}
