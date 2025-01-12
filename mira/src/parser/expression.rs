use parking_lot::RwLock;
use std::{
    collections::HashMap,
    fmt::{Debug, Display, Write},
    sync::Arc,
};

use crate::{
    annotations::AnnotationReceiver,
    error::ParsingError,
    globals::GlobalStr,
    module::{FunctionId, Module, ModuleId},
    tokenizer::{Literal, Location, NumberType, Token, TokenType},
};

use super::{
    statement::{display_contract, FunctionContract},
    types::TypeRef,
    Parser, ParserQueueEntry, Statement,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Path {
    pub entries: Vec<(GlobalStr, Vec<TypeRef>)>,
}

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in 0..self.entries.len() {
            if i != 0 {
                f.write_str("::")?;
            }

            Display::fmt(&self.entries[i].0, f)?;
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
    pub fn push(&mut self, name: GlobalStr, generics: Vec<TypeRef>) {
        self.entries.push((name, generics));
    }
    pub fn pop(&mut self) -> Option<(GlobalStr, Vec<TypeRef>)> {
        // ensure this is at least 1 element
        if self.entries.len() > 1 {
            self.entries.pop()
        } else {
            None
        }
    }
    pub fn new(entry: GlobalStr, generics: Vec<TypeRef>) -> Self {
        Self {
            entries: vec![(entry, generics)],
        }
    }

    fn parse_generics(parser: &mut Parser) -> Result<Vec<TypeRef>, ParsingError> {
        let mut types = vec![];

        while !parser.match_tok(TokenType::GreaterThan) {
            if types.len() > 0 && !parser.match_tok(TokenType::Comma) {
                return Err(ParsingError::ExpectedArbitrary {
                    loc: parser.peek().location.clone(),
                    expected: TokenType::Comma,
                    found: parser.peek().typ,
                });
            }
            types.push(TypeRef::parse(parser)?);
        }
        Ok(types)
    }

    pub fn parse(parser: &mut Parser) -> Result<Self, ParsingError> {
        let generics = if parser.match_tok(TokenType::LessThan) {
            Self::parse_generics(parser)?
        } else {
            Default::default()
        };

        let mut path = Self::new(parser.expect_identifier()?, generics);

        while parser.match_tok(TokenType::NamespaceAccess) {
            let subpath = parser.expect_identifier()?;
            let generics = if parser.match_tok(TokenType::LessThan) {
                Self::parse_generics(parser)?
            } else {
                Default::default()
            };

            path.push(subpath, generics);
        }

        Ok(path)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PathWithoutGenerics {
    pub entries: Vec<GlobalStr>,
}

impl PathWithoutGenerics {
    pub fn push(&mut self, name: GlobalStr) {
        self.entries.push(name)
    }
    pub fn pop(&mut self) -> Option<GlobalStr> {
        // ensure this is at least 1 element
        if self.entries.len() > 1 {
            self.entries.pop()
        } else {
            None
        }
    }
    pub fn new(entry: GlobalStr) -> Self {
        Self {
            entries: vec![entry],
        }
    }
    pub fn parse(parser: &mut Parser) -> Result<Self, ParsingError> {
        let mut path = Self::new(parser.expect_identifier()?);

        while parser.match_tok(TokenType::NamespaceAccess) {
            let subpath = parser.expect_identifier()?;
            path.push(subpath);
        }

        Ok(path)
    }
}

impl Display for PathWithoutGenerics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in 0..self.entries.len() {
            if i != 0 {
                f.write_str("::")?;
            }
            Display::fmt(&self.entries[i], f)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum LiteralValue {
    String(GlobalStr),
    Array(Vec<Expression>),
    Struct(HashMap<GlobalStr, (Location, Expression)>, Path),
    Float(f64, NumberType),
    SInt(i64, NumberType),
    UInt(u64, NumberType),
    Bool(bool),
    Dynamic(Path),
    AnonymousFunction(FunctionContract, Box<Statement>),
    BakedAnonymousFunction(FunctionId),
    Void,
}

impl PartialEq for LiteralValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::Float(_, num_type) | Self::UInt(_, num_type) | Self::SInt(_, num_type),
                Self::Float(_, other) | Self::UInt(_, other) | Self::SInt(_, other),
            ) if *num_type != *other => return false,
            _ => (),
        }
        match (self, other) {
            (Self::String(l), Self::String(r)) => l == r,
            (Self::SInt(l, _), Self::SInt(r, _)) => *l == *r,
            (Self::UInt(l, _), Self::UInt(r, _)) => *l == *r,
            (Self::Float(l, _), Self::Float(r, _)) => *l == *r,
            (Self::UInt(l, _), Self::SInt(r, _)) | (Self::SInt(r, _), Self::UInt(l, _)) => {
                *r >= 0 && (*r as u64) == *l
            }
            (Self::Float(l, _), Self::SInt(r, _)) | (Self::SInt(r, _), Self::Float(l, _)) => {
                (*l == l.floor()) && (*l as i64) == *r
            }
            (Self::Float(l, _), Self::UInt(r, _)) | (Self::UInt(r, _), Self::Float(l, _)) => {
                (*l >= 0.0) && (*l == l.floor()) && (*l as u64) == *r
            }
            (Self::Bool(l), Self::Bool(r)) => l == r,
            (Self::Dynamic(l), Self::Dynamic(r)) => l == r,
            (Self::Void, Self::Void) => true,
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
            LiteralValue::UInt(v, typ) => f.write_fmt(format_args!("{}{}", *v, *typ)),
            LiteralValue::SInt(v, typ) => f.write_fmt(format_args!("{}{}", *v, *typ)),
            LiteralValue::Float(v, typ) => f.write_fmt(format_args!("{}{}", *v, *typ)),
            LiteralValue::String(v) => Debug::fmt(v, f),
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

                for (k, (_, v)) in v {
                    f.write_char(' ')?;
                    Display::fmt(k, f)?;
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
            LiteralValue::Void => f.write_str("void"),
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
            LiteralValue::Void => "void",
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
    MemberCall {
        identifier: GlobalStr,
        lhs: Box<Expression>,
        arguments: Vec<Expression>,
    },
    Indexing {
        left_side: Box<Expression>,
        right_side: Box<Expression>,
    },
    MemberAccess {
        left_side: Box<Expression>,
        index: Vec<GlobalStr>,
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
                left_side, index, ..
            } => {
                f.write_str("(member ")?;
                Display::fmt(left_side, f)?;
                for value in index {
                    f.write_char(' ')?;
                    Display::fmt(value, f)?;
                }

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
            Expression::MemberCall {
                identifier,
                lhs,
                arguments,
            } => {
                f.write_str("(member-call ")?;
                Display::fmt(lhs, f)?;
                f.write_char(' ')?;
                Display::fmt(identifier, f)?;
                for arg in arguments {
                    f.write_char(' ')?;
                    Display::fmt(arg, f)?;
                }
                f.write_char(')')
            }
        }
    }
}

impl Expression {
    pub fn bool(value: bool, loc: Location) -> Self {
        Self::Literal(LiteralValue::Bool(value), loc)
    }

    pub fn string(value: GlobalStr, loc: Location) -> Self {
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
            | Self::MemberCall {
                lhs: identifier, ..
            }
            | Self::FunctionCall { identifier, .. } => identifier.loc(),
            Self::MemberAccess { loc, .. }
            | Self::Assignment { loc, .. }
            | Self::Range { loc, .. }
            | Self::TypeCast { loc, .. } => loc,
        }
    }

    pub fn bake_functions(&mut self, module: &mut Module, module_id: ModuleId) {
        match self {
            Self::Literal(val, ..) => {
                if let LiteralValue::AnonymousFunction(contract, statements) = val {
                    let id = module.push_fn(
                        FunctionContract {
                            name: contract.name.take(),
                            arguments: std::mem::take(&mut contract.arguments),
                            return_type: contract.return_type.clone(),
                            annotations: std::mem::take(&mut contract.annotations),
                            location: contract.location.clone(),
                            generics: std::mem::take(&mut contract.generics),
                        },
                        std::mem::replace(
                            &mut **statements,
                            Statement::BakedTrait(0, contract.location.clone()),
                        ),
                        module_id,
                    );
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
                left_side.bake_functions(module, module_id);
                right_side.bake_functions(module, module_id);
            }
            Self::FunctionCall {
                identifier,
                arguments,
            } => {
                identifier.bake_functions(module, module_id);
                arguments
                    .iter_mut()
                    .for_each(|el| el.bake_functions(module, module_id));
            }
            Self::MemberCall { lhs, arguments, .. } => {
                lhs.bake_functions(module, module_id);
                arguments
                    .iter_mut()
                    .for_each(|el| el.bake_functions(module, module_id));
            }
            Self::MemberAccess { left_side, .. }
            | Self::TypeCast { left_side, .. }
            | Self::Unary {
                right_side: left_side,
                ..
            } => left_side.bake_functions(module, module_id),
        }
    }

    pub fn optimize(&mut self) -> Result<(), ParsingError> {
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
                            TokenType::Plus
                            | TokenType::Minus
                            | TokenType::BitwiseNot
                            | TokenType::LogicalNot
                            | TokenType::Ampersand
                            | TokenType::Asterix => {}
                            tok @ _ => {
                                return Err(ParsingError::InvalidUnaryOperand {
                                    loc,
                                    operand_type: tok,
                                })
                            }
                        }
                    }
                    // *&
                    Expression::Unary {
                        operator: r_op,
                        right_side,
                    } if operator.typ == TokenType::Asterix && r_op.typ == TokenType::Ampersand => {
                        *self = (&**right_side).clone();
                    }
                    // &*
                    Expression::Unary {
                        operator: r_op,
                        right_side,
                    } if operator.typ == TokenType::Ampersand && r_op.typ == TokenType::Asterix => {
                        *self = (&**right_side).clone();
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
                        // +, -, *, /, %, &, |, ^, &&, ||, >=, <=, >, <, ==, !=
                        match operator.typ {
                            TokenType::Plus => (),
                            TokenType::Minus => (),
                            TokenType::Asterix => (),
                            TokenType::Divide => (),
                            TokenType::Modulo => (),
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
                            TokenType::LShift => (),
                            TokenType::RShift => (),
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
                            e @ (TokenType::EqualEqual | TokenType::NotEquals) => {
                                match (left, right) {
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
                                            (*l == *r) == (e == TokenType::EqualEqual), /* this basically does the same as (e == TokenType::Equals) ? (*l == *r) : (*l != *r) */
                                            // false == false (l != r && e == not-equals) => true
                                            // true == false (l == r && e == not-equals) => false
                                            // false == true ( l != r && e == equals) => false
                                            // true == true ( l == r && e == equals) => true
                                            loc,
                                        )
                                    }
                                }
                            }
                            tok @ _ => {
                                return Err(ParsingError::InvalidOperand {
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
                            return Err(ParsingError::AssignmentInvalidLeftSide {
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
            Self::MemberAccess {
                left_side, index, ..
            } => {
                left_side.optimize()?;
                if let Self::MemberAccess {
                    left_side: left_left_side,
                    index: left_idx,
                    loc,
                } = &mut **left_side
                {
                    left_idx.extend(index.drain(..));
                    std::mem::swap(index, left_idx);
                    index.append(left_idx);
                    let left_left_side = std::mem::replace(
                        &mut **left_left_side,
                        Self::Literal(LiteralValue::Void, loc.clone()),
                    );
                    drop(std::mem::replace(&mut **left_side, left_left_side));
                }
                if index.len() == 0 {
                    let mut dummy_expr =
                        Expression::Literal(LiteralValue::Void, left_side.loc().clone());
                    std::mem::swap(&mut dummy_expr, &mut **left_side);
                    std::mem::swap(self, &mut dummy_expr);
                }
            }
            Self::TypeCast { left_side, .. } => left_side.optimize()?,
            Self::MemberCall { lhs, arguments, .. } => {
                lhs.optimize()?;
                for arg in arguments {
                    arg.optimize()?;
                }
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
    pub fn parse_expression(&mut self) -> Result<Expression, ParsingError> {
        let mut expr = self.expression()?;
        expr.optimize()?;
        Ok(expr)
    }

    fn expression(&mut self) -> Result<Expression, ParsingError> {
        self.comparison()
    }

    fn comparison(&mut self) -> Result<Expression, ParsingError> {
        let mut expr = self.pipe_operator()?;

        while self.matches(&[
            TokenType::EqualEqual,
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

    fn pipe_operator(&mut self) -> Result<Expression, ParsingError> {
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
                _ => return Err(ParsingError::ExpectedFunctionCall { loc }),
            }
        }

        Ok(expr)
    }

    fn range(&mut self) -> Result<Expression, ParsingError> {
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

    fn term(&mut self) -> Result<Expression, ParsingError> {
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

    fn factor(&mut self) -> Result<Expression, ParsingError> {
        let mut expr = self.unary()?;

        while self.matches(&[
            TokenType::Divide,
            TokenType::Modulo,
            TokenType::Asterix,
            TokenType::Ampersand,
            TokenType::BitwiseOr,
            TokenType::BitwiseXor,
            TokenType::LShift,
            TokenType::RShift,
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
                    operator.typ = TokenType::LShift;
                    assign_set!(expr, right, operator);
                }
                TokenType::BitwiseRShiftAssign => {
                    operator.typ = TokenType::RShift;
                    assign_set!(expr, right, operator);
                }
                _ => expr = Expression::binary(operator, expr, right),
            }
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expression, ParsingError> {
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

    fn assignment(&mut self) -> Result<Expression, ParsingError> {
        let expr = self.type_cast()?;
        if self.match_tok(TokenType::Equal) {
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

    fn type_cast(&mut self) -> Result<Expression, ParsingError> {
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

    fn references(&mut self) -> Result<Expression, ParsingError> {
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

    fn indexed(&mut self) -> Result<Expression, ParsingError> {
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
                            return Err(ParsingError::ExpectedFunctionArgument {
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
                        return Err(ParsingError::ExpectedFunctionArgumentExpression {
                            loc: next_loc,
                            found: next_typ,
                        });
                    }
                }

                if self.tokens[self.current.saturating_sub(2)].typ == TokenType::ParenRight {
                    expr = Expression::FunctionCall {
                        identifier: Box::new(expr),
                        arguments,
                    };
                } else if let Expression::MemberAccess {
                    left_side,
                    mut index,
                    loc,
                } = expr
                {
                    expr = Expression::MemberCall {
                        identifier: index
                            .pop()
                            .expect("member access did not access any members"),
                        lhs: Box::new(Expression::MemberAccess {
                            left_side,
                            index,
                            loc,
                        }),
                        arguments,
                    }
                } else {
                    expr = Expression::FunctionCall {
                        identifier: Box::new(expr),
                        arguments,
                    };
                }
            } else if self.previous().typ == TokenType::BracketLeft {
                let next_loc = self.peek().location.clone();
                let next_typ = self.peek().typ;
                let Ok(indexing_expr) = self.expression() else {
                    return Err(ParsingError::ExpectedFunctionArgumentExpression {
                        loc: next_loc,
                        found: next_typ,
                    });
                };

                if self.match_tok(TokenType::BracketRight) {
                    expr = Expression::Indexing {
                        left_side: Box::new(expr),
                        right_side: Box::new(indexing_expr),
                    };
                    continue;
                }

                let found_token = self.peek();
                return Err(ParsingError::ExpectedArbitrary {
                    loc: found_token.location.clone(),
                    found: found_token.typ,
                    expected: TokenType::BracketRight,
                });
            } else if self.previous().typ == TokenType::Dot {
                let identifier = self.advance();
                if identifier.typ != TokenType::IdentifierLiteral {
                    return Err(ParsingError::ExpectedIdentifier {
                        loc: identifier.location.clone(),
                        found: identifier.typ,
                    });
                } else if let Some(Literal::String(str)) = &identifier.literal {
                    if let Expression::MemberAccess { index, .. } = &mut expr {
                        index.push(str.clone());
                        continue;
                    }
                    expr = Expression::MemberAccess {
                        left_side: Box::new(expr),
                        index: vec![str.clone()],
                        loc: identifier.location.clone(),
                    };
                } else {
                    return Err(ParsingError::InvalidTokenization {
                        loc: identifier.location.clone(),
                    });
                }
            } else {
                unreachable!();
            }
        }

        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expression, ParsingError> {
        // arrays
        match self.try_array() {
            Some(v) => return v,
            _ => (),
        }

        // functions/callables
        if self.peek().typ == TokenType::Fn {
            let loc = self.peek().location.clone();
            return Ok(Expression::Literal(
                self.parse_callable(true).and_then(|(contract, body)| {
                    contract
                        .annotations
                        .are_annotations_valid_for(AnnotationReceiver::Function)?;
                    Ok(LiteralValue::AnonymousFunction(contract, Box::new(body)))
                })?,
                loc,
            ));
        }

        if matches!(self.peek().typ, TokenType::IdentifierLiteral) {
            let path = Path::parse(self)?;

            // StructName { ... };
            let loc = self.peek().location.clone();
            if let Some(obj) = self.try_object() {
                return match obj {
                    Ok(v) => Ok(Expression::Literal(LiteralValue::Struct(v, path), loc)),
                    Err(e) => Err(e),
                };
            } else {
                return Ok(Expression::Literal(LiteralValue::Dynamic(path), loc));
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
            return Err(ParsingError::ExpectedArbitrary {
                loc: found_token.location.clone(),
                found: found_token.typ,
                expected: TokenType::ParenRight,
            });
        }

        let found_token = self.peek();
        return Err(ParsingError::ExpectedExpression {
            loc: found_token.location.clone(),
            found: found_token.typ,
        });
    }

    fn try_array(&mut self) -> Option<Result<Expression, ParsingError>> {
        if self.match_tok(TokenType::BracketLeft) {
            let loc = self.previous().location.clone();
            let mut arr = vec![];
            while !self.match_tok(TokenType::BracketRight) {
                if arr.len() > 0 {
                    // expect a comma
                    if !self.match_tok(TokenType::Comma) {
                        return Some(Err(ParsingError::ExpectedArrayElement {
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
    ) -> Option<Result<HashMap<GlobalStr, (Location, Expression)>, ParsingError>> {
        if self.match_tok(TokenType::CurlyLeft) {
            let mut obj = HashMap::new();
            while !self.match_tok(TokenType::CurlyRight) {
                if obj.len() > 0 {
                    // expect a comma
                    if !self.match_tok(TokenType::Comma) {
                        return Some(Err(ParsingError::ExpectedObjectElement {
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
                let key = if !self.match_tok(TokenType::IdentifierLiteral) {
                    return Some(Err(ParsingError::ExpectedIdentifier {
                        loc: self.peek().location.clone(),
                        found: self.peek().typ,
                    }));
                } else {
                    match self.previous().literal {
                        Some(Literal::String(ref v)) => v.clone(),
                        _ => {
                            return Some(Err(ParsingError::InvalidTokenization {
                                loc: self.previous().location.clone(),
                            }))
                        }
                    }
                };
                let location = self.previous().location.clone();

                if !self.match_tok(TokenType::Colon) {
                    return Some(Err(ParsingError::ExpectedArbitrary {
                        loc: self.peek().location.clone(),
                        expected: TokenType::Colon,
                        found: self.peek().typ,
                    }));
                }

                match self.expression() {
                    Ok(expr) => obj.insert(key, (location, expr)),
                    Err(e) => return Some(Err(e)),
                };
            }
            return Some(Ok(obj));
        } else {
            None
        }
    }

    pub fn new(
        tokens: Vec<Token>,
        modules: Arc<RwLock<Vec<ParserQueueEntry>>>,
        file: Arc<std::path::Path>,
        root_directory: Arc<std::path::Path>,
    ) -> Self {
        Self {
            tokens,
            current: 0,
            current_annotations: Default::default(),
            imports: HashMap::new(),
            modules,
            file,
            root_directory,
        }
    }
}
