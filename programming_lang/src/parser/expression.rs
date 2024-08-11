use std::{
    collections::HashMap,
    fmt::{Debug, Display, Write},
};

use crate::{
    error::{OperationType, ProgrammingLangParsingError},
    tokenizer::{Literal, Location, Token, TokenType},
};

use super::{
    statement::{display_contract, FunctionContract}, types::TypeRef, Parser, Statement
};

#[derive(Debug, Clone)]
pub enum LiteralValue {
    String(Box<str>),
    Array(Vec<Expression>),
    Struct(HashMap<Box<str>, Expression>, Box<str>),
    Object(HashMap<Box<str>, Expression>),
    Number(f64),
    Bool(bool),
    Dynamic(Box<str>),
    AnonymousFunction(FunctionContract, Box<Statement>),
    Null,
}

impl PartialEq for LiteralValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(l), Self::String(r)) => l == r,
            (Self::Number(l), Self::Number(r)) => l == r,
            (Self::Bool(l), Self::Bool(r)) => l == r,
            (Self::Dynamic(l), Self::Dynamic(r)) => l == r,
            _ => false,
        }
    }
}

impl Display for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let LiteralValue::Struct(_, name) = &self {
            f.write_str(&**name)?;
            f.write_char(' ')?;
        }
        match self {
            LiteralValue::Bool(b) => f.write_str(if *b { "true" } else { "false" }),
            LiteralValue::Dynamic(d) => f.write_str(&**d),
            LiteralValue::Null => f.write_str("null"),
            LiteralValue::Number(v) => f.write_fmt(format_args!("{}", *v)),
            LiteralValue::String(v) => f.write_fmt(format_args!("\"{}\"", &**v)),
            LiteralValue::Array(v) => {
                f.write_char('[')?;
                for i in 0..v.len() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }
                    Display::fmt(&v[i], f)?;
                }
                f.write_char(']')
            }
            LiteralValue::Struct(v, _) | LiteralValue::Object(v) => {
                f.write_char('{')?;

                for (k, v) in v {
                    f.write_char(' ')?;
                    f.write_str(&**k)?;
                    f.write_str(": ")?;
                    Display::fmt(v, f)?;
                    f.write_char(',')?;
                }

                f.write_str(" }")
            }
            LiteralValue::AnonymousFunction(v, body) => {
                display_contract(f, v, false)?;
                Display::fmt(body, f)?;
                f.write_char(')')
            }
        }
    }
}

impl LiteralValue {
    // NOTE: **ONLY** FOR ERROR MESSAGES!!!!!
    pub fn type_name(&self) -> &'static str {
        match self {
            Self::Bool(..) => "boolean",
            LiteralValue::Dynamic(..) => "{unknown}",
            LiteralValue::Null => "null",
            LiteralValue::Number(..) => "number",
            LiteralValue::String(..) => "string",
            LiteralValue::Array(..) => "array",
            LiteralValue::Object(..) => "object",
            LiteralValue::Struct(..) => "struct",
            LiteralValue::AnonymousFunction(..) => "function",
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(LiteralValue, Location),
    Unary {
        operator: Token,
        right_side: Box<Expression>,
    },
    Binary {
        operator: Token,
        right_side: Box<Expression>,
        left_side: Box<Expression>,
    },
    FunctionCall {
        identifier: Box<Expression>,
        arguments: Vec<Expression>,
    },
    Indexing {
        left_side: Box<Expression>,
        right_side: Box<Expression>,
    },
    Assignment {
        left_side: Box<Expression>,
        right_side: Box<Expression>,
        loc: Location,
    },
    Range {
        left_side: Box<Expression>,
        right_side: Box<Expression>,
        inclusive: bool,
        loc: Location,
    },
    TypeCast {
        left_side: Box<Expression>,
        new_type: TypeRef,
        loc: Location,
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Literal(v, ..) => Display::fmt(v, f),
            Expression::Binary {
                operator,
                right_side,
                left_side,
            } => {
                f.write_char('(')?;
                operator.typ.fmt(f)?;
                f.write_char(' ')?;
                Display::fmt(left_side, f)?;
                f.write_char(' ')?;
                Display::fmt(right_side, f)?;
                f.write_char(')')
            }
            Expression::Unary {
                operator,
                right_side,
            } => {
                f.write_char('(')?;
                operator.typ.fmt(f)?;
                f.write_char(' ')?;
                Display::fmt(right_side, f)?;
                f.write_char(')')
            }
            Expression::FunctionCall {
                identifier,
                arguments,
                ..
            } => {
                f.write_str("(call ")?;
                Display::fmt(identifier, f)?;
                for i in 0..arguments.len() {
                    f.write_char(' ')?;
                    Display::fmt(&arguments[i], f)?;
                }
                f.write_char(')')
            }
            Expression::Indexing {
                left_side,
                right_side,
            } => {
                f.write_str("(index ")?;
                Display::fmt(left_side, f)?;
                f.write_char(' ')?;
                Display::fmt(right_side, f)?;
                f.write_char(')')
            }
            Expression::Assignment {
                left_side,
                right_side,
                ..
            } => {
                f.write_str("(assignment ")?;
                Display::fmt(left_side, f)?;
                f.write_char(' ')?;
                Display::fmt(right_side, f)?;
                f.write_char(')')
            }
            Expression::Range {
                left_side,
                right_side,
                inclusive,
                loc: _,
            } => {
                f.write_str("(range ")?;
                if *inclusive {
                    f.write_str("inclusive ")?;
                }
                Display::fmt(left_side, f)?;
                f.write_char(' ')?;
                Display::fmt(right_side, f)?;
                f.write_char(')')
            }
            Expression::TypeCast { left_side, new_type, loc: _ } => {
                f.write_str("(type-cast ")?;
                Display::fmt(left_side, f)?;
                f.write_str(" -> ")?;
                Display::fmt(new_type, f)?;
                f.write_char(')')
            }
        }
    }
}

impl Expression {
    pub fn bool(value: bool, loc: Location) -> Self {
        Self::Literal(LiteralValue::Bool(value), loc)
    }

    pub fn number(value: f64, loc: Location) -> Self {
        Self::Literal(LiteralValue::Number(value), loc)
    }

    pub fn string(value: Box<str>, loc: Location) -> Self {
        Self::Literal(LiteralValue::String(value), loc)
    }

    pub fn unary(operator: Token, right: Expression) -> Self {
        Self::Unary {
            operator,
            right_side: Box::new(right),
        }
    }

    pub fn binary(operator: Token, left: Expression, right: Expression) -> Self {
        Self::Binary {
            operator,
            right_side: Box::new(right),
            left_side: Box::new(left),
        }
    }

    pub fn loc(&self) -> Location {
        match self {
            Self::Literal(_, v) => *v,
            Self::Unary { operator, .. } => operator.location,
            Self::Binary { operator, .. } => operator.location,
            Self::FunctionCall { identifier, .. } => identifier.loc(),
            Self::Indexing { right_side, .. } => right_side.loc(),
            Self::Assignment { loc, .. } => *loc,
            Self::Range { loc, .. } => *loc,
            Self::TypeCast { loc, .. } => *loc,
        }
    }

    pub fn optimize(&mut self) -> Result<(), ProgrammingLangParsingError> {
        let loc = self.loc().clone();

        Ok(match self {
            Self::Literal(val, _) => match val {
                LiteralValue::Array(array) => {
                    for val in array {
                        val.optimize()?;
                    }
                }
                LiteralValue::Object(obj) | LiteralValue::Struct(obj, _) => {
                    for v in obj.values_mut() {
                        v.optimize()?;
                    }
                }
                _ => (),
            },
            Self::Unary {
                operator,
                right_side,
            } => {
                // right_side = Box::new(right_side.optimize()?);
                right_side.optimize()?;

                match &mut **right_side {
                    Expression::Literal(LiteralValue::Dynamic(..), ..) => (),
                    Expression::Literal(v, ..) => {
                        // valid tokens: + plus, - minus, ~ bitwise not, ! logical not
                        match operator.typ {
                            TokenType::Plus => match v {
                                LiteralValue::Dynamic(..) => (),
                                LiteralValue::Number(..) => (),
                                v @ _ => {
                                    return Err(
                                        ProgrammingLangParsingError::CannotDoUnaryOperation {
                                            loc,
                                            operation_type: OperationType::Plus,
                                            right: v.type_name(),
                                        },
                                    )
                                }
                            },
                            TokenType::Minus => match v {
                                LiteralValue::Dynamic(..) => (),
                                LiteralValue::Number(v) => *self = Self::number(-*v, loc),
                                v @ _ => {
                                    return Err(
                                        ProgrammingLangParsingError::CannotDoUnaryOperation {
                                            loc,
                                            operation_type: OperationType::Minus,
                                            right: v.type_name(),
                                        },
                                    )
                                }
                            },
                            TokenType::BitwiseNot => match v {
                                LiteralValue::Dynamic(..) => (),
                                LiteralValue::Number(v) => {
                                    *self = Self::number(!(v.floor() as i64) as f64, loc)
                                }
                                LiteralValue::Bool(v) => *self = Self::bool(!*v, loc),
                                v @ _ => {
                                    return Err(
                                        ProgrammingLangParsingError::CannotDoUnaryOperation {
                                            loc,
                                            operation_type: OperationType::BitwiseNot,
                                            right: v.type_name(),
                                        },
                                    )
                                }
                            },
                            TokenType::LogicalNot => match v {
                                LiteralValue::Dynamic(..) => (),
                                LiteralValue::Bool(v) => *self = Self::bool(!*v, loc),
                                v @ _ => {
                                    return Err(
                                        ProgrammingLangParsingError::CannotDoUnaryOperation {
                                            loc,
                                            operation_type: OperationType::LogicalNot,
                                            right: v.type_name(),
                                        },
                                    )
                                }
                            },
                            tok @ _ => {
                                return Err(ProgrammingLangParsingError::InvalidUnaryOperand {
                                    loc,
                                    operand_type: tok,
                                })
                            }
                        }
                    }
                    _ => (),
                }
            }
            Self::Binary {
                operator,
                right_side,
                left_side,
            } => {
                left_side.optimize()?;
                right_side.optimize()?;

                match (&mut **left_side, &mut **right_side) {
                    (Expression::Literal(LiteralValue::Dynamic(..), ..), _)
                    | (_, Expression::Literal(LiteralValue::Dynamic(..), ..)) => (),

                    (Expression::Literal(left, ..), Expression::Literal(right, ..)) => {
                        // +, -, *, /, %, &, |, ^, &&, ||, idiv, >=, <=, >, <, ==, !=
                        match operator.typ {
                            TokenType::Plus => match (left, right) {
                                (LiteralValue::Dynamic(..), _) | (_, LiteralValue::Dynamic(..)) => {
                                }
                                (LiteralValue::Number(left), LiteralValue::Number(right)) => {
                                    *self = Self::number(*left + *right, loc);
                                }
                                (LiteralValue::String(left), LiteralValue::String(right)) => {
                                    let mut new_str = String::from(&**left);
                                    new_str.push_str(&*right);
                                    *self = Self::string(new_str.into_boxed_str(), loc);
                                }
                                (left, right) => {
                                    return Err(
                                        ProgrammingLangParsingError::CannotDoBinaryOperation {
                                            loc,
                                            operation_type: OperationType::Plus,
                                            left: left.type_name(),
                                            right: right.type_name(),
                                        },
                                    )
                                }
                            },
                            TokenType::Minus => match (left, right) {
                                (LiteralValue::Dynamic(..), _) | (_, LiteralValue::Dynamic(..)) => {
                                }
                                (LiteralValue::Number(left), LiteralValue::Number(right)) => {
                                    *self = Self::number(*left - *right, loc);
                                }
                                (left, right) => {
                                    return Err(
                                        ProgrammingLangParsingError::CannotDoBinaryOperation {
                                            loc,
                                            operation_type: OperationType::Minus,
                                            left: left.type_name(),
                                            right: right.type_name(),
                                        },
                                    )
                                }
                            },
                            TokenType::MultiplyOrDeref => match (left, right) {
                                (LiteralValue::Dynamic(..), _) | (_, LiteralValue::Dynamic(..)) => {
                                }
                                (LiteralValue::Number(left), LiteralValue::Number(right)) => {
                                    *self = Self::number((*left) * (*right), loc);
                                }
                                (LiteralValue::String(left), LiteralValue::Number(right))
                                | (LiteralValue::Number(right), LiteralValue::String(left)) => {
                                    let mut str =
                                        String::with_capacity(left.len() * right.floor() as usize);
                                    for _ in 0..right.floor() as usize {
                                        str.push_str(&left);
                                    }

                                    *self = Self::string(str.into_boxed_str(), loc);
                                }
                                (left, right) => {
                                    return Err(
                                        ProgrammingLangParsingError::CannotDoBinaryOperation {
                                            loc,
                                            operation_type: OperationType::Multiply,
                                            left: left.type_name(),
                                            right: right.type_name(),
                                        },
                                    )
                                }
                            },
                            TokenType::Divide => match (left, right) {
                                (LiteralValue::Dynamic(..), _) | (_, LiteralValue::Dynamic(..)) => {
                                }
                                (LiteralValue::Number(left), LiteralValue::Number(right)) => {
                                    *self = Self::number(*left / *right, loc);
                                }
                                (left, right) => {
                                    return Err(
                                        ProgrammingLangParsingError::CannotDoBinaryOperation {
                                            loc,
                                            operation_type: OperationType::Divide,
                                            left: left.type_name(),
                                            right: right.type_name(),
                                        },
                                    )
                                }
                            },
                            TokenType::Modulo => match (left, right) {
                                (LiteralValue::Dynamic(..), _) | (_, LiteralValue::Dynamic(..)) => {
                                }
                                (LiteralValue::Number(left), LiteralValue::Number(right)) => {
                                    *self = Self::number(*left % *right, loc);
                                }
                                (left, right) => {
                                    return Err(
                                        ProgrammingLangParsingError::CannotDoBinaryOperation {
                                            loc,
                                            operation_type: OperationType::Divide,
                                            left: left.type_name(),
                                            right: right.type_name(),
                                        },
                                    )
                                }
                            },
                            TokenType::IntegerDivide => match (left, right) {
                                (LiteralValue::Dynamic(..), _) | (_, LiteralValue::Dynamic(..)) => {
                                }
                                (LiteralValue::Number(left), LiteralValue::Number(right)) => {
                                    *self = Self::number((*left / *right).floor(), loc);
                                }
                                (left, right) => {
                                    return Err(
                                        ProgrammingLangParsingError::CannotDoBinaryOperation {
                                            loc,
                                            operation_type: OperationType::IntDivide,
                                            left: left.type_name(),
                                            right: right.type_name(),
                                        },
                                    )
                                }
                            },
                            TokenType::LogicalAnd => match (left, right) {
                                (LiteralValue::Bool(false), _) | (_, LiteralValue::Bool(false)) => {
                                    *self = Self::bool(false, loc)
                                }
                                (LiteralValue::Bool(left), LiteralValue::Bool(right)) => {
                                    *self = Self::bool(*left && *right, loc)
                                }
                                (LiteralValue::Dynamic(..), _) | (_, LiteralValue::Dynamic(..)) => {
                                    ()
                                }
                                (left, right) => {
                                    return Err(
                                        ProgrammingLangParsingError::CannotDoBinaryOperation {
                                            loc,
                                            operation_type: OperationType::LogicalAnd,
                                            left: left.type_name(),
                                            right: right.type_name(),
                                        },
                                    )
                                }
                            },
                            TokenType::LogicalOr => match (left, right) {
                                (LiteralValue::Bool(true), _) | (_, LiteralValue::Bool(true)) => {
                                    *self = Self::bool(true, loc)
                                }
                                (LiteralValue::Dynamic(..), _) | (_, LiteralValue::Dynamic(..)) => {
                                    ()
                                }

                                (LiteralValue::Bool(left), LiteralValue::Bool(right)) => {
                                    *self = Self::bool(*left && *right, loc);
                                }

                                (left, right) => {
                                    return Err(
                                        ProgrammingLangParsingError::CannotDoBinaryOperation {
                                            loc,
                                            operation_type: OperationType::LogicalOr,
                                            left: left.type_name(),
                                            right: right.type_name(),
                                        },
                                    )
                                }
                            },
                            TokenType::BitwiseXor => match (left, right) {
                                (LiteralValue::Dynamic(..), _) | (_, LiteralValue::Dynamic(..)) => {
                                }
                                (LiteralValue::Bool(left), LiteralValue::Bool(right)) => {
                                    *self = Self::bool(*left ^ *right, loc);
                                }
                                (LiteralValue::Number(left), LiteralValue::Number(right)) => {
                                    *self = Self::number(
                                        (left.floor() as i64 ^ right.floor() as i64) as f64,
                                        loc,
                                    );
                                }
                                (left, right) => {
                                    return Err(
                                        ProgrammingLangParsingError::CannotDoBinaryOperation {
                                            loc,
                                            operation_type: OperationType::BitwiseXor,
                                            left: left.type_name(),
                                            right: right.type_name(),
                                        },
                                    )
                                }
                            },
                            TokenType::BitwiseAndOrReference => match (left, right) {
                                (LiteralValue::Dynamic(..), _) | (_, LiteralValue::Dynamic(..)) => {
                                }
                                (LiteralValue::Bool(left), LiteralValue::Bool(right)) => {
                                    *self = Self::bool(*left & *right, loc)
                                }
                                (LiteralValue::Number(left), LiteralValue::Number(right)) => {
                                    *self = Self::number(
                                        (left.floor() as i64 & right.floor() as i64) as f64,
                                        loc,
                                    );
                                }
                                (left, right) => {
                                    return Err(
                                        ProgrammingLangParsingError::CannotDoBinaryOperation {
                                            loc,
                                            operation_type: OperationType::BitwiseAnd,
                                            left: left.type_name(),
                                            right: right.type_name(),
                                        },
                                    )
                                }
                            },
                            TokenType::BitwiseOr => match (left, right) {
                                (LiteralValue::Dynamic(..), _) | (_, LiteralValue::Dynamic(..)) => {
                                }
                                (LiteralValue::Bool(left), LiteralValue::Bool(right)) => {
                                    *self = Self::bool(*left | *right, loc)
                                }
                                (LiteralValue::Number(left), LiteralValue::Number(right)) => {
                                    *self = Self::number(
                                        (left.floor() as i64 | right.floor() as i64) as f64,
                                        loc,
                                    );
                                }
                                (left, right) => {
                                    return Err(
                                        ProgrammingLangParsingError::CannotDoBinaryOperation {
                                            loc,
                                            operation_type: OperationType::BitwiseOr,
                                            left: left.type_name(),
                                            right: right.type_name(),
                                        },
                                    )
                                }
                            },
                            TokenType::GreaterThan => match (left, right) {
                                (LiteralValue::Dynamic(..), _) | (_, LiteralValue::Dynamic(..)) => {
                                }
                                (LiteralValue::Number(l), LiteralValue::Number(r)) => {
                                    *self = Self::bool(*l > *r, loc)
                                }
                                (left, right) => {
                                    return Err(
                                        ProgrammingLangParsingError::CannotDoBinaryOperation {
                                            loc,
                                            operation_type: OperationType::GreaterThan,
                                            left: left.type_name(),
                                            right: right.type_name(),
                                        },
                                    )
                                }
                            },
                            TokenType::LessThan => match (left, right) {
                                (LiteralValue::Dynamic(..), _) | (_, LiteralValue::Dynamic(..)) => {
                                }
                                (LiteralValue::Number(l), LiteralValue::Number(r)) => {
                                    *self = Self::bool(*l < *r, loc)
                                }
                                (left, right) => {
                                    return Err(
                                        ProgrammingLangParsingError::CannotDoBinaryOperation {
                                            loc,
                                            operation_type: OperationType::LessThan,
                                            left: left.type_name(),
                                            right: right.type_name(),
                                        },
                                    )
                                }
                            },
                            TokenType::GreaterThanEquals => match (left, right) {
                                (LiteralValue::Dynamic(..), _) | (_, LiteralValue::Dynamic(..)) => {
                                }
                                (LiteralValue::Number(l), LiteralValue::Number(r)) => {
                                    *self = Self::bool(*l >= *r, loc)
                                }
                                (left, right) => {
                                    return Err(
                                        ProgrammingLangParsingError::CannotDoBinaryOperation {
                                            loc,
                                            operation_type: OperationType::GreaterThanEquals,
                                            left: left.type_name(),
                                            right: right.type_name(),
                                        },
                                    )
                                }
                            },
                            TokenType::LessThanEquals => match (left, right) {
                                (LiteralValue::Dynamic(..), _) | (_, LiteralValue::Dynamic(..)) => {
                                }
                                (LiteralValue::Number(l), LiteralValue::Number(r)) => {
                                    *self = Self::bool(*l <= *r, loc)
                                }
                                (left, right) => {
                                    return Err(
                                        ProgrammingLangParsingError::CannotDoBinaryOperation {
                                            loc,
                                            operation_type: OperationType::LessThanEquals,
                                            left: left.type_name(),
                                            right: right.type_name(),
                                        },
                                    )
                                }
                            },
                            e @ (TokenType::Equals | TokenType::NotEquals) => match (left, right) {
                                // dynamic values, functions, arrays, objects and structs cannot be compared at compile time
                                (
                                    LiteralValue::Dynamic(..)
                                    | LiteralValue::Array(..)
                                    | LiteralValue::Object(..)
                                    | LiteralValue::Struct(..)
                                    | LiteralValue::AnonymousFunction(..),
                                    _,
                                )
                                | (
                                    _,
                                    LiteralValue::Dynamic(..)
                                    | LiteralValue::Array(..)
                                    | LiteralValue::Object(..)
                                    | LiteralValue::Struct(..)
                                    | LiteralValue::AnonymousFunction(..),
                                ) => {}
                                (l, r) => {
                                    *self = Self::bool(
                                        (*l == *r) == (e == TokenType::Equals), /* this basically does the same as (e == TokenType::Equals) ? (*l == *r) : (*l != *r) */
                                        // false == false (l != r && e == not-equals) => true
                                        // true == false (l == r && e == not-equals) => false
                                        // false == true ( l != r && e == equals) => false
                                        // true == true ( l == r && e == equals) => true
                                        loc,
                                    )
                                }
                            },
                            tok @ _ => {
                                return Err(ProgrammingLangParsingError::InvalidOperand {
                                    loc,
                                    operand_type: tok,
                                })
                            }
                        }
                    }
                    _ => (),
                }
            }
            Self::FunctionCall {
                arguments,
                identifier,
            } => {
                identifier.optimize()?;
                for argument in arguments {
                    argument.optimize()?;
                }
            }
            Self::Indexing {
                left_side,
                right_side,
            } => {
                left_side.optimize()?;
                right_side.optimize()?;
                let real_loc = left_side.loc();

                match (&mut **left_side, &mut **right_side) {
                    (Expression::Literal(left_side, ..), Expression::Literal(right_side, ..)) => {
                        match (left_side, right_side) {
                            (LiteralValue::Dynamic(..), _) | (_, LiteralValue::Dynamic(..)) => (),
                            (LiteralValue::String(str), LiteralValue::Number(num)) => {
                                if *num < 0.0 || *num != num.floor() || *num as usize >= str.len() {
                                    *self = Self::Literal(LiteralValue::Null, real_loc)
                                } else {
                                    if let Some(c) = str.chars().nth(*num as usize) {
                                        *self =
                                            Self::string(String::from(c).into_boxed_str(), real_loc)
                                    } else {
                                        *self = Self::Literal(LiteralValue::Null, real_loc)
                                    }
                                }
                            }
                            (LiteralValue::Array(a), LiteralValue::Number(idx)) => {
                                if idx.floor() == *idx && *idx >= 0.0 {
                                    *self = a[*idx as usize].clone();
                                } else {
                                    *self = Expression::Literal(LiteralValue::Null, real_loc);
                                }
                            }
                            (LiteralValue::Object(obj), LiteralValue::String(str)) => {
                                if let Some(v) = obj.remove(str) {
                                    *self = v;
                                }
                            }
                            _ => (),
                        }
                    }
                    _ => (),
                }
            }
            Self::Assignment {
                left_side,
                right_side: _,
                loc,
            } => {
                if let Self::Literal(v, _) = &**left_side {
                    match v {
                        LiteralValue::Dynamic(..) => (),
                        _ => {
                            return Err(ProgrammingLangParsingError::AssignmentInvalidLeftSide {
                                loc: *loc,
                            })
                        }
                    }
                }
            }
            Self::Range {
                left_side,
                right_side,
                ..
            } => {
                left_side.optimize()?;
                right_side.optimize()?;
            }
            Self::TypeCast { left_side, .. } => left_side.optimize()?,
        })
    }
}

macro_rules! assign_set {
    ($expr: expr, $right: expr, $operator: expr) => {
        let loc = $operator.location;
        $expr = Expression::Assignment {
            left_side: Box::new($expr.clone()),
            right_side: Box::new(Expression::binary($operator, $expr, $right)),
            loc,
        }
    };
}

impl Parser {
    pub fn parse_expression(&mut self) -> Result<Expression, ProgrammingLangParsingError> {
        let mut expr = self.expression()?;
        expr.optimize()?;
        Ok(expr)
    }

    fn expression(&mut self) -> Result<Expression, ProgrammingLangParsingError> {
        self.comparison()
    }

    fn comparison(&mut self) -> Result<Expression, ProgrammingLangParsingError> {
        let mut expr = self.range()?;

        while self.matches(&[
            TokenType::Equals,
            TokenType::NotEquals,
            TokenType::LessThan,
            TokenType::GreaterThan,
            TokenType::LessThanEquals,
            TokenType::GreaterThanEquals,
            TokenType::LogicalAnd,
            TokenType::LogicalOr,
        ]) {
            let operator = self.previous().clone();
            let right = self.range()?;
            expr = Expression::binary(operator, expr, right);
        }

        Ok(expr)
    }

    fn range(&mut self) -> Result<Expression, ProgrammingLangParsingError> {
        let mut expr = self.term()?;

        while self.matches(&[TokenType::Range, TokenType::RangeInclusive]) {
            let inclusive = self.previous().typ == TokenType::RangeInclusive;
            let loc = self.previous().location;
            let right_side = Box::new(self.expression()?);
            expr = Expression::Range {
                left_side: Box::new(expr),
                right_side,
                inclusive,
                loc,
            };
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expression, ProgrammingLangParsingError> {
        let mut expr = self.factor()?;

        while self.matches(&[
            TokenType::Minus,
            TokenType::Plus,
            TokenType::PlusAssign,
            TokenType::MinusAssign,
        ]) {
            let mut operator = self.previous().clone();
            let right = self.factor()?;

            match operator.typ {
                TokenType::PlusAssign => {
                    operator.typ = TokenType::Plus;
                    assign_set!(expr, right, operator);
                }
                TokenType::MinusAssign => {
                    operator.typ = TokenType::Minus;
                    assign_set!(expr, right, operator);
                }
                _ => expr = Expression::binary(operator, expr, right),
            }
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expression, ProgrammingLangParsingError> {
        let mut expr = self.unary()?;

        while self.matches(&[
            TokenType::Divide,
            TokenType::IntegerDivide,
            TokenType::Modulo,
            TokenType::MultiplyOrDeref,
            TokenType::BitwiseAndOrReference,
            TokenType::BitwiseOr,
            TokenType::BitwiseXor,
            TokenType::DivideAssign,
            TokenType::ModuloAssign,
            TokenType::MultiplyAssign,
            TokenType::BitwiseAndAssign,
            TokenType::BitwiseOrAssign,
            TokenType::BitwiseXorAssign,
        ]) {
            let mut operator = self.previous().clone();
            let right = self.unary()?;

            match operator.typ {
                TokenType::DivideAssign => {
                    operator.typ = TokenType::Divide;
                    assign_set!(expr, right, operator);
                }
                TokenType::ModuloAssign => {
                    operator.typ = TokenType::Modulo;
                    assign_set!(expr, right, operator);
                }
                TokenType::MultiplyAssign => {
                    operator.typ = TokenType::MultiplyOrDeref;
                    assign_set!(expr, right, operator);
                }
                TokenType::BitwiseAndAssign => {
                    operator.typ = TokenType::BitwiseAndOrReference;
                    assign_set!(expr, right, operator);
                }
                TokenType::BitwiseOrAssign => {
                    operator.typ = TokenType::BitwiseOr;
                    assign_set!(expr, right, operator);
                }
                TokenType::BitwiseXorAssign => {
                    operator.typ = TokenType::BitwiseXor;
                    assign_set!(expr, right, operator);
                }
                _ => expr = Expression::binary(operator, expr, right),
            }
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expression, ProgrammingLangParsingError> {
        if self.matches(&[
            TokenType::Plus,
            TokenType::Minus,
            TokenType::BitwiseNot,
            TokenType::LogicalNot,
        ]) {
            let operator = self.previous().clone();
            return Ok(Expression::unary(operator, self.unary()?));
        }

        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expression, ProgrammingLangParsingError> {
        let expr = self.type_cast()?;
        if self.match_tok(TokenType::AssignValue) {
            let loc = self.previous().location;
            let value = self.expression()?;
            return Ok(Expression::Assignment {
                left_side: Box::new(expr),
                right_side: Box::new(value),
                loc,
            });
        }
        Ok(expr)
    }

    fn type_cast(&mut self) -> Result<Expression, ProgrammingLangParsingError> {
        let mut expr = self.indexed()?;

        while self.match_tok(TokenType::As) {
            let loc = self.previous().location;
            let new_type = TypeRef::parse(self)?;
            expr = Expression::TypeCast { left_side: Box::new(expr), new_type, loc };
        }

        Ok(expr)
    }

    fn indexed(&mut self) -> Result<Expression, ProgrammingLangParsingError> {
        let mut expr = self.primary()?;

        while self.matches(&[TokenType::BracketLeft, TokenType::ParenLeft, TokenType::Dot]) {
            if self.previous().typ == TokenType::ParenLeft {
                // function call
                let mut arguments: Vec<Expression> = vec![];
                loop {
                    if self.peek().typ == TokenType::ParenRight {
                        self.advance();
                        break;
                    } else if arguments.len() != 0 {
                        // , or ), but ) is alr handled by the thing above
                        if self.advance().typ != TokenType::Comma {
                            return Err(ProgrammingLangParsingError::ExpectedFunctionArgument {
                                loc: self.previous().location,
                                found: self.previous().typ,
                            });
                        }
                    }

                    let next_loc = self.peek().location;
                    let next_typ = self.peek().typ;
                    if let Ok(expr) = self.expression() {
                        arguments.push(expr);
                    } else {
                        return Err(
                            ProgrammingLangParsingError::ExpectedFunctionArgumentExpression {
                                loc: next_loc,
                                found: next_typ,
                            },
                        );
                    }
                }
                expr = Expression::FunctionCall {
                    identifier: Box::new(expr),
                    arguments,
                };
            } else if self.previous().typ == TokenType::BracketLeft {
                let next_loc = self.peek().location;
                let next_typ = self.peek().typ;
                let Ok(indexing_expr) = self.expression() else {
                    return Err(
                        ProgrammingLangParsingError::ExpectedFunctionArgumentExpression {
                            loc: next_loc,
                            found: next_typ,
                        },
                    );
                };

                if self.match_tok(TokenType::BracketRight) {
                    expr = Expression::Indexing {
                        left_side: Box::new(expr),
                        right_side: Box::new(indexing_expr),
                    };
                    continue;
                }

                let found_token = self.peek();
                return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                    loc: found_token.location,
                    found: found_token.typ,
                    expected: TokenType::BracketRight,
                });
            } else if self.previous().typ == TokenType::Dot {
                let identifier = self.advance();
                if identifier.typ != TokenType::IdentifierLiteral {
                    return Err(ProgrammingLangParsingError::ExpectedIdentifier {
                        loc: identifier.location,
                        found: identifier.typ,
                    });
                } else if let Some(Literal::String(str)) = &identifier.literal {
                    expr = Expression::Indexing {
                        left_side: Box::new(expr),
                        right_side: Box::new(Expression::Literal(
                            LiteralValue::String(str.clone()),
                            identifier.location,
                        )),
                    };
                } else {
                    return Err(ProgrammingLangParsingError::InvalidTokenization {
                        loc: identifier.location,
                    });
                }
            } else {
                unreachable!();
            }
        }

        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expression, ProgrammingLangParsingError> {
        // arrays
        match self.try_array() {
            Some(v) => return v,
            _ => (),
        }

        // objects
        {
            let loc = self.peek().location;
            match self.try_object() {
                Some(Ok(v)) => return Ok(Expression::Literal(LiteralValue::Object(v), loc)),
                Some(Err(e)) => return Err(e),
                _ => (),
            }
        }

        // functions/callables
        if self.peek().typ == TokenType::Fn {
            let loc = self.peek().location;
            return Ok(Expression::Literal(
                self.parse_callable(true)
                    .map(|(contract, body)| LiteralValue::AnonymousFunction(contract, body))?,
                loc,
            ));
        }

        if let Some(val) = self.peek().to_literal_value() {
            if let LiteralValue::Dynamic(identifier) = val {
                // StructName { ... };
                let loc = self.advance().location;
                if let Some(obj) = self.try_object() {
                    return match obj {
                        Ok(v) => Ok(Expression::Literal(
                            LiteralValue::Struct(v, identifier),
                            loc,
                        )),
                        Err(e) => Err(e),
                    };
                }
                return Ok(Expression::Literal(LiteralValue::Dynamic(identifier), loc));
            } else {
                return Ok(Expression::Literal(val, self.advance().location));
            }
        }

        if self.match_tok(TokenType::ParenLeft) {
            let expr = self.expression()?;
            if self.match_tok(TokenType::ParenRight) {
                return Ok(expr);
            }

            let found_token = self.peek();
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: found_token.location,
                found: found_token.typ,
                expected: TokenType::ParenRight,
            });
        }

        let found_token = self.peek();
        return Err(ProgrammingLangParsingError::ExpectedExpression {
            loc: found_token.location,
            found: found_token.typ,
        });
    }

    fn try_array(&mut self) -> Option<Result<Expression, ProgrammingLangParsingError>> {
        if self.match_tok(TokenType::BracketLeft) {
            let loc = self.previous().location;
            let mut arr = vec![];
            while !self.match_tok(TokenType::BracketRight) {
                if arr.len() > 0 {
                    // expect a comma
                    if !self.match_tok(TokenType::Comma) {
                        return Some(Err(ProgrammingLangParsingError::ExpectedArrayElement {
                            loc: self.peek().location,
                            found: self.peek().typ,
                        }));
                    }
                    // in order to allow stuff like [1, 2, 3, ]
                    if self.match_tok(TokenType::BracketRight) {
                        break;
                    }
                }

                match self.expression() {
                    Ok(v) => arr.push(v),
                    e @ Err(_) => return Some(e),
                }
            }

            return Some(Ok(Expression::Literal(LiteralValue::Array(arr), loc)));
        } else {
            None
        }
    }

    // { key : value, }
    fn try_object(
        &mut self,
    ) -> Option<Result<HashMap<Box<str>, Expression>, ProgrammingLangParsingError>> {
        if self.match_tok(TokenType::CurlyLeft) {
            let mut obj = HashMap::new();
            while !self.match_tok(TokenType::CurlyRight) {
                if obj.len() > 0 {
                    // expect a comma
                    if !self.match_tok(TokenType::Comma) {
                        return Some(Err(ProgrammingLangParsingError::ExpectedObjectElement {
                            loc: self.peek().location,
                            found: self.peek().typ,
                        }));
                    }
                    // in order to allow stuff like [1, 2, 3, ]
                    if self.match_tok(TokenType::CurlyRight) {
                        break;
                    }
                }

                // parse key : value
                let key = if !self.matches(&[
                    TokenType::StringLiteral,
                    TokenType::IdentifierLiteral,
                    TokenType::NumberLiteral,
                ]) {
                    return Some(Err(ProgrammingLangParsingError::ExpectedKey {
                        loc: self.peek().location,
                        found: self.peek().typ,
                    }));
                } else {
                    match self.previous().literal {
                        Some(Literal::String(ref v)) => v.clone(),
                        Some(Literal::Number(v)) => v.to_string().into_boxed_str(),
                        _ => {
                            return Some(Err(ProgrammingLangParsingError::InvalidTokenization {
                                loc: self.previous().location,
                            }))
                        }
                    }
                };

                if !self.match_tok(TokenType::AssignTypeOrStructValue) {
                    return Some(Err(ProgrammingLangParsingError::ExpectedArbitrary {
                        loc: self.peek().location,
                        expected: TokenType::AssignTypeOrStructValue,
                        found: self.peek().typ,
                    }));
                }

                match self.expression() {
                    Ok(expr) => obj.insert(key, expr),
                    Err(e) => return Some(Err(e)),
                };
            }

            return Some(Ok(obj));
        } else {
            None
        }
    }

    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }
}
