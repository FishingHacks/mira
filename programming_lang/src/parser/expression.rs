use std::{
    collections::HashMap,
    fmt::{Debug, Display, Write},
    rc::Rc,
};

use crate::{
    error::{OperationType, ProgrammingLangParsingError},
    module::{FunctionId, Module},
    tokenizer::{Literal, Location, Token, TokenType},
};

use super::{
    statement::{display_contract, FunctionContract},
    types::TypeRef,
    Parser, Statement,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Path {
    entries: Vec<(Rc<str>, Vec<TypeRef>)>,
    generics: Vec<TypeRef>,
}

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in 0..self.entries.len() {
            if i != 0 {
                f.write_str("::")?;
            }

            f.write_str(&*self.entries[i].0)?;
            if self.entries[i].1.len() > 0 {
                f.write_char('<')?;
                for type_idx in 0..self.entries[i].1.len() {
                    if type_idx != 0 {
                        f.write_str(", ")?;
                    }
                    Display::fmt(&self.entries[i].1[type_idx], f)?;
                }
                f.write_char('>')?;
            }
        }
        Ok(())
    }
}

impl Path {
    pub fn push(&mut self, name: Rc<str>, generics: Vec<TypeRef>) {
        self.entries.push((name, generics));
    }
    pub fn pop(&mut self) -> Option<(Rc<str>, Vec<TypeRef>)> {
        // ensure this is at least 1 element
        if self.entries.len() > 1 {
            self.entries.pop()
        } else {
            None
        }
    }
    pub fn new(entry: Rc<str>, generics: Vec<TypeRef>) -> Self {
        Self {
            entries: vec![(entry, generics)],
            generics: vec![],
        }
    }
}

#[derive(Debug, Clone)]
pub enum LiteralValue {
    String(Rc<str>),
    Array(Vec<Expression>),
    Struct(HashMap<Rc<str>, Expression>, Path),
    Float(f64),
    SInt(i64),
    UInt(u64),
    Bool(bool),
    Dynamic(Path),
    AnonymousFunction(FunctionContract, Box<Statement>),
    BakedAnonymousFunction(FunctionId),
}

impl PartialEq for LiteralValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(l), Self::String(r)) => l == r,
            (Self::SInt(l), Self::SInt(r)) => *l == *r,
            (Self::UInt(l), Self::UInt(r)) => *l == *r,
            (Self::Float(l), Self::Float(r)) => *l == *r,
            (Self::UInt(l), Self::SInt(r)) | (Self::SInt(r), Self::UInt(l)) => {
                *r >= 0 && (*r as u64) == *l
            }
            (Self::Float(l), Self::SInt(r)) | (Self::SInt(r), Self::Float(l)) => {
                (*l == l.floor()) && (*l as i64) == *r
            }
            (Self::Float(l), Self::UInt(r)) | (Self::UInt(r), Self::Float(l)) => {
                (*l >= 0.0) && (*l == l.floor()) && (*l as u64) == *r
            }
            (Self::Bool(l), Self::Bool(r)) => l == r,
            (Self::Dynamic(l), Self::Dynamic(r)) => l == r,
            _ => false,
        }
    }
}

impl Display for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralValue::BakedAnonymousFunction(id) => {
                f.write_fmt(format_args!("(module-fn {id})"))
            }
            LiteralValue::Bool(b) => f.write_str(if *b { "true" } else { "false" }),
            LiteralValue::Dynamic(d) => Display::fmt(d, f),
            LiteralValue::UInt(v) => f.write_fmt(format_args!("{}", *v)),
            LiteralValue::SInt(v) => f.write_fmt(format_args!("{}", *v)),
            LiteralValue::Float(v) => f.write_fmt(format_args!("{}", *v)),
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
            LiteralValue::Struct(v, name) => {
                Display::fmt(name, f)?;
                f.write_str(" {")?;

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
            LiteralValue::Float(..) | LiteralValue::SInt(..) | LiteralValue::UInt(..) => "number",
            LiteralValue::String(..) => "string",
            LiteralValue::Array(..) => "array",
            LiteralValue::Struct(..) => "struct",
            LiteralValue::AnonymousFunction(..) | Self::BakedAnonymousFunction(..) => "function",
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
    MemberAccess {
        left_side: Box<Expression>,
        right_side: Rc<str>,
        loc: Location,
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
    },
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
            Expression::MemberAccess {
                left_side,
                right_side,
                ..
            } => {
                f.write_str("(member ")?;
                Display::fmt(left_side, f)?;
                f.write_char(' ')?;
                f.write_str(&**right_side)?;
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
            Expression::TypeCast {
                left_side,
                new_type,
                loc: _,
            } => {
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

    pub fn float(value: f64, loc: Location) -> Self {
        Self::Literal(LiteralValue::Float(value), loc)
    }

    pub fn signed_int(value: i64, loc: Location) -> Self {
        Self::Literal(LiteralValue::SInt(value), loc)
    }
    pub fn unsigned_int(value: u64, loc: Location) -> Self {
        Self::Literal(LiteralValue::UInt(value), loc)
    }

    pub fn string(value: Rc<str>, loc: Location) -> Self {
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

    pub fn loc(&self) -> &Location {
        match self {
            Self::Literal(_, v) => v,
            Self::Unary { operator, .. } | Self::Binary { operator, .. } => &operator.location,
            Self::Indexing {
                right_side: identifier,
                ..
            }
            | Self::FunctionCall { identifier, .. } => identifier.loc(),
            Self::MemberAccess { loc, .. }
            | Self::Assignment { loc, .. }
            | Self::Range { loc, .. }
            | Self::TypeCast { loc, .. } => loc,
        }
    }

    pub fn bake_functions(&mut self, module: &mut Module) {
        match self {
            Self::Literal(val, ..) => {
                if let LiteralValue::AnonymousFunction(contract, statements) = val {
                    let id = module.push_fn(contract.clone(), *(statements.clone()));
                    *val = LiteralValue::BakedAnonymousFunction(id)
                }
            }
            Self::Binary {
                right_side,
                left_side,
                ..
            }
            | Self::Assignment {
                left_side,
                right_side,
                ..
            }
            | Self::Indexing {
                left_side,
                right_side,
            }
            | Self::Range {
                left_side,
                right_side,
                ..
            } => {
                left_side.bake_functions(module);
                right_side.bake_functions(module);
            }
            Self::FunctionCall {
                identifier,
                arguments,
            } => {
                identifier.bake_functions(module);
                arguments
                    .iter_mut()
                    .for_each(|el| el.bake_functions(module));
            }
            Self::MemberAccess { left_side, .. }
            | Self::TypeCast { left_side, .. }
            | Self::Unary {
                right_side: left_side,
                ..
            } => left_side.bake_functions(module),
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
                                LiteralValue::Float(..)
                                | LiteralValue::SInt(..)
                                | LiteralValue::UInt(..) => (),
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
                                LiteralValue::Float(..)
                                | LiteralValue::SInt(..)
                                | LiteralValue::UInt(..) => (),
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
                                LiteralValue::Float(..)
                                | LiteralValue::SInt(..)
                                | LiteralValue::UInt(..) => (),
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
                            TokenType::Ampersand => {}
                            TokenType::Asterix => {}
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
                            TokenType::Plus => (),
                            TokenType::Minus => (),
                            TokenType::Asterix => (),
                            TokenType::Divide => (),
                            TokenType::Modulo => (),
                            TokenType::IntegerDivide => (),
                            TokenType::LogicalAnd => match (left, right) {
                                (LiteralValue::Bool(false), _) | (_, LiteralValue::Bool(false)) => {
                                    *self = Self::bool(false, loc)
                                }
                                (LiteralValue::Bool(left), LiteralValue::Bool(right)) => {
                                    *self = Self::bool(*left && *right, loc)
                                }
                                _ => (),
                            },
                            TokenType::LogicalOr => match (left, right) {
                                (LiteralValue::Bool(true), _) | (_, LiteralValue::Bool(true)) => {
                                    *self = Self::bool(true, loc)
                                }
                                (LiteralValue::Bool(left), LiteralValue::Bool(right)) => {
                                    *self = Self::bool(*left && *right, loc);
                                }
                                _ => (),
                            },
                            TokenType::BitwiseXor => match (left, right) {
                                (LiteralValue::Bool(left), LiteralValue::Bool(right)) => {
                                    *self = Self::bool(*left ^ *right, loc);
                                }
                                _ => (),
                            },
                            TokenType::BitwiseLShift => (),
                            TokenType::Ampersand => (),
                            TokenType::BitwiseOr => match (left, right) {
                                (LiteralValue::Bool(left), LiteralValue::Bool(right)) => {
                                    *self = Self::bool(*left | *right, loc)
                                }
                                _ => (),
                            },
                            TokenType::GreaterThan => (),
                            TokenType::LessThan => (),
                            TokenType::GreaterThanEquals => (),
                            TokenType::LessThanEquals => (),
                            e @ (TokenType::Equals | TokenType::NotEquals) => match (left, right) {
                                // dynamic values, functions, arrays, objects and structs cannot be compared at compile time
                                (
                                    LiteralValue::Dynamic(..)
                                    | LiteralValue::Array(..)
                                    | LiteralValue::Struct(..)
                                    | LiteralValue::AnonymousFunction(..),
                                    _,
                                )
                                | (
                                    _,
                                    LiteralValue::Dynamic(..)
                                    | LiteralValue::Array(..)
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

                match (&mut **left_side, &mut **right_side) {
                    (Expression::Literal(left_side, ..), Expression::Literal(right_side, ..)) => {
                        match (left_side, right_side) {
                            (LiteralValue::Dynamic(..), _) | (_, LiteralValue::Dynamic(..)) => (),
                            (LiteralValue::Array(a), LiteralValue::UInt(idx)) => {
                                *self = a[*idx as usize].clone();
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
                                loc: loc.clone(),
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
            Self::MemberAccess { left_side, .. } | Self::TypeCast { left_side, .. } => {
                left_side.optimize()?
            }
        })
    }
}

macro_rules! assign_set {
    ($expr: expr, $right: expr, $operator: expr) => {
        let loc = $operator.location.clone();
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
        let mut expr = self.pipe_operator()?;

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
            let right = self.pipe_operator()?;
            expr = Expression::binary(operator, expr, right);
        }

        Ok(expr)
    }

    fn pipe_operator(&mut self) -> Result<Expression, ProgrammingLangParsingError> {
        let mut expr = self.range()?;

        while self.match_tok(TokenType::PipeOperator) {
            let loc = self.peek().location.clone();
            let call = self.indexed()?;
            match call {
                Expression::FunctionCall {
                    identifier,
                    mut arguments,
                } => {
                    arguments.insert(0, expr);
                    expr = Expression::FunctionCall {
                        identifier,
                        arguments,
                    };
                }
                _ => return Err(ProgrammingLangParsingError::ExpectedFunctionCall { loc }),
            }
        }

        Ok(expr)
    }

    fn range(&mut self) -> Result<Expression, ProgrammingLangParsingError> {
        let mut expr = self.term()?;

        while self.matches(&[TokenType::Range, TokenType::RangeInclusive]) {
            let inclusive = self.previous().typ == TokenType::RangeInclusive;
            let loc = self.previous().location.clone();
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
            TokenType::Asterix,
            TokenType::Ampersand,
            TokenType::BitwiseOr,
            TokenType::BitwiseXor,
            TokenType::BitwiseLShift,
            TokenType::BitwiseRShift,
            TokenType::DivideAssign,
            TokenType::ModuloAssign,
            TokenType::MultiplyAssign,
            TokenType::BitwiseAndAssign,
            TokenType::BitwiseOrAssign,
            TokenType::BitwiseXorAssign,
            TokenType::BitwiseLShiftAssign,
            TokenType::BitwiseRShiftAssign,
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
                    operator.typ = TokenType::Asterix;
                    assign_set!(expr, right, operator);
                }
                TokenType::BitwiseAndAssign => {
                    operator.typ = TokenType::Ampersand;
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
                TokenType::BitwiseLShiftAssign => {
                    operator.typ = TokenType::BitwiseLShift;
                    assign_set!(expr, right, operator);
                }
                TokenType::BitwiseRShiftAssign => {
                    operator.typ = TokenType::BitwiseRShift;
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
            let loc = self.previous().location.clone();
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
        let mut expr = self.references()?;

        while self.match_tok(TokenType::As) {
            let loc = self.previous().location.clone();
            let new_type = TypeRef::parse(self)?;
            expr = Expression::TypeCast {
                left_side: Box::new(expr),
                new_type,
                loc,
            };
        }

        Ok(expr)
    }

    fn references(&mut self) -> Result<Expression, ProgrammingLangParsingError> {
        if self.match_tok(TokenType::Ampersand) {
            Ok(Expression::Unary {
                operator: self.previous().clone(),
                right_side: Box::new(self.references()?),
            })
        } else if self.match_tok(TokenType::Asterix) {
            Ok(Expression::Unary {
                operator: self.previous().clone(),
                right_side: Box::new(self.references()?),
            })
        } else {
            self.indexed()
        }
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
                                loc: self.previous().location.clone(),
                                found: self.previous().typ,
                            });
                        }
                    }

                    let next_loc = self.peek().location.clone();
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
                let next_loc = self.peek().location.clone();
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
                    loc: found_token.location.clone(),
                    found: found_token.typ,
                    expected: TokenType::BracketRight,
                });
            } else if self.previous().typ == TokenType::Dot {
                let identifier = self.advance();
                if identifier.typ != TokenType::IdentifierLiteral {
                    return Err(ProgrammingLangParsingError::ExpectedIdentifier {
                        loc: identifier.location.clone(),
                        found: identifier.typ,
                    });
                } else if let Some(Literal::String(str)) = &identifier.literal {
                    expr = Expression::MemberAccess {
                        left_side: Box::new(expr),
                        right_side: str.clone(),
                        loc: identifier.location.clone(),
                    };
                } else {
                    return Err(ProgrammingLangParsingError::InvalidTokenization {
                        loc: identifier.location.clone(),
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

        // functions/callables
        if self.peek().typ == TokenType::Fn {
            let loc = self.peek().location.clone();
            return Ok(Expression::Literal(
                self.parse_callable(true)
                    .map(|(contract, body)| LiteralValue::AnonymousFunction(contract, body))?,
                loc,
            ));
        }

        if matches!(self.peek().typ, TokenType::IdentifierLiteral) {
            let identifier = self.parse_identifier()?;

            // StructName { ... };
            let loc = self.peek().location.clone();
            if let Some(obj) = self.try_object() {
                return match obj {
                    Ok(v) => Ok(Expression::Literal(
                        LiteralValue::Struct(v, identifier),
                        loc,
                    )),
                    Err(e) => Err(e),
                };
            } else {
                return Ok(Expression::Literal(LiteralValue::Dynamic(identifier), loc));
            }
        }

        if let Some(lit) = self.peek().to_literal_value() {
            return Ok(Expression::Literal(lit, self.advance().location.clone()));
        }

        if self.match_tok(TokenType::ParenLeft) {
            let expr = self.expression()?;
            if self.match_tok(TokenType::ParenRight) {
                return Ok(expr);
            }

            let found_token = self.peek();
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: found_token.location.clone(),
                found: found_token.typ,
                expected: TokenType::ParenRight,
            });
        }

        let found_token = self.peek();
        return Err(ProgrammingLangParsingError::ExpectedExpression {
            loc: found_token.location.clone(),
            found: found_token.typ,
        });
    }

    fn parse_identifier(&mut self) -> Result<Path, ProgrammingLangParsingError> {
        let Some(Literal::String(ref identifier)) = self.advance().literal else {
            return Err(ProgrammingLangParsingError::InvalidTokenization {
                loc: self.tokens[self.current].location.clone(),
            });
        };
        let identifier = identifier.clone();

        let generics = if self.peek().typ == TokenType::LessThan {
            self.parse_generics()?
        } else {
            Vec::new()
        };
        let mut path = Path::new(identifier, generics);
        
        while self.match_tok(TokenType::NamespaceAccess) {
            match self.peek().typ {
                TokenType::IdentifierLiteral => {}
                _ => {
                    return Err(ProgrammingLangParsingError::ExpectedIdentifier {
                        loc: self.peek().location.clone(),
                        found: self.peek().typ,
                    })
                }
            }

            let Some(Literal::String(ref identifier)) = self.advance().literal else {
                return Err(ProgrammingLangParsingError::InvalidTokenization {
                    loc: self.tokens[self.current].location.clone(),
                });
            };
            let identifier = identifier.clone();
            let generics = if self.peek().typ == TokenType::LessThan {
                self.parse_generics()?
            } else {
                Vec::new()
            };
            path.push(identifier, generics);
        }
        Ok(path)
    }

    fn parse_generics(&mut self) -> Result<Vec<TypeRef>, ProgrammingLangParsingError> {
        if !self.match_tok(TokenType::LessThan) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location.clone(),
                expected: TokenType::LessThan,
                found: self.peek().typ,
            });
        }
        let mut types = vec![];

        while !self.match_tok(TokenType::GreaterThan) {
            if types.len() > 0 && !self.match_tok(TokenType::Comma) {
                return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                    loc: self.peek().location.clone(),
                    expected: TokenType::Comma,
                    found: self.peek().typ,
                });
            }
            types.push(TypeRef::parse(self)?);
        }
        Ok(types)
    }

    fn try_array(&mut self) -> Option<Result<Expression, ProgrammingLangParsingError>> {
        if self.match_tok(TokenType::BracketLeft) {
            let loc = self.previous().location.clone();
            let mut arr = vec![];
            while !self.match_tok(TokenType::BracketRight) {
                if arr.len() > 0 {
                    // expect a comma
                    if !self.match_tok(TokenType::Comma) {
                        return Some(Err(ProgrammingLangParsingError::ExpectedArrayElement {
                            loc: self.peek().location.clone(),
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
    ) -> Option<Result<HashMap<Rc<str>, Expression>, ProgrammingLangParsingError>> {
        if self.match_tok(TokenType::CurlyLeft) {
            let mut obj = HashMap::new();
            while !self.match_tok(TokenType::CurlyRight) {
                if obj.len() > 0 {
                    // expect a comma
                    if !self.match_tok(TokenType::Comma) {
                        return Some(Err(ProgrammingLangParsingError::ExpectedObjectElement {
                            loc: self.peek().location.clone(),
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
                    TokenType::FloatLiteral,
                ]) {
                    return Some(Err(ProgrammingLangParsingError::ExpectedKey {
                        loc: self.peek().location.clone(),
                        found: self.peek().typ,
                    }));
                } else {
                    match self.previous().literal {
                        Some(Literal::String(ref v)) => v.clone(),
                        Some(Literal::UInt(v)) => v.to_string().into(),
                        _ => {
                            return Some(Err(ProgrammingLangParsingError::InvalidTokenization {
                                loc: self.previous().location.clone(),
                            }))
                        }
                    }
                };

                if !self.match_tok(TokenType::Colon) {
                    return Some(Err(ProgrammingLangParsingError::ExpectedArbitrary {
                        loc: self.peek().location.clone(),
                        expected: TokenType::Colon,
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
        Self {
            tokens,
            current: 0,
            current_annotations: vec![],
        }
    }
}
