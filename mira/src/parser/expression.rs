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
        let name = parser.expect_identifier()?;
        let generics = if parser.match_tok(TokenType::LessThan) {
            Self::parse_generics(parser)?
        } else {
            Default::default()
        };

        let mut path = Self::new(name, generics);

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
    AnonymousStruct(HashMap<GlobalStr, (Location, Expression)>),
    Tuple(Vec<(Location, Expression)>),
    Float(f64, NumberType),
    SInt(i64, NumberType),
    UInt(u64, NumberType),
    Bool(bool),
    Dynamic(Path),
    AnonymousFunction(FunctionContract, Box<Statement>),
    BakedAnonymousFunction(FunctionId),
    Void,
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
            LiteralValue::AnonymousStruct(v) => {
                f.write_str(".{")?;

                for (k, (_, v)) in v {
                    f.write_char(' ')?;
                    Display::fmt(k, f)?;
                    f.write_str(": ")?;
                    Display::fmt(v, f)?;
                    f.write_char(',')?;
                }

                f.write_str(" }")
            }
            LiteralValue::Tuple(v) => {
                f.write_str(".(")?;

                for i in 0..v.len() {
                    if i != 0 {
                        f.write_str(", ")?
                    }
                    Display::fmt(&v[i].1, f)?;
                }

                f.write_char(')')
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
            LiteralValue::AnonymousStruct(..) => "anonymous struct",
            LiteralValue::Tuple(..) => "tuple",
            LiteralValue::Void => "void",
            LiteralValue::AnonymousFunction(..) | Self::BakedAnonymousFunction(..) => "function",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Plus,
    Minus,
    Divide,
    Multiply,
    Modulo,
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    LShift,
    RShift,
    LogicalAnd,
    LogicalOr,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Equals,
    NotEquals,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Plus,
    Minus,
    BitwiseNot,
    LogicalNot,
    Reference,
    Dereference,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(LiteralValue, Location),
    Unary {
        operator: UnaryOp,
        loc: Location,
        right_side: Box<Expression>,
    },
    Binary {
        operator: BinaryOp,
        loc: Location,
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
                ..
            } => {
                f.write_char('(')?;
                operator.fmt(f)?;
                f.write_char(' ')?;
                Display::fmt(left_side, f)?;
                f.write_char(' ')?;
                Display::fmt(right_side, f)?;
                f.write_char(')')
            }
            Expression::Unary {
                operator,
                right_side,
                ..
            } => {
                f.write_char('(')?;
                operator.fmt(f)?;
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

    pub fn unary(operator: UnaryOp, loc: Location, right: Expression) -> Self {
        Self::Unary {
            operator,
            loc,
            right_side: Box::new(right),
        }
    }

    pub fn binary(operator: BinaryOp, loc: Location, left: Expression, right: Expression) -> Self {
        Self::Binary {
            operator,
            loc,
            right_side: Box::new(right),
            left_side: Box::new(left),
        }
    }

    pub fn loc(&self) -> &Location {
        match self {
            Self::Indexing { right_side: i, .. }
            | Self::MemberCall { lhs: i, .. }
            | Self::FunctionCall { identifier: i, .. } => i.loc(),
            Self::MemberAccess { loc, .. }
            | Self::Literal(_, loc)
            | Self::Unary { loc, .. }
            | Self::Binary { loc, .. }
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
}

macro_rules! assign_set {
    ($expr: expr, $right: expr, $operator: expr, $loc: expr) => {
        let loc = $loc;
        $expr = Expression::Assignment {
            left_side: Box::new($expr.clone()),
            right_side: Box::new(Expression::binary($operator, loc.clone(), $expr, $right)),
            loc,
        }
    };
}

impl Parser {
    pub fn parse_expression(&mut self) -> Result<Expression, ParsingError> {
        self.comparison()
    }

    fn comparison(&mut self) -> Result<Expression, ParsingError> {
        let mut expr = self.pipe_operator()?;

        loop {
            let loc = self.peek().location.clone();
            let op = match self.peek().typ {
                TokenType::EqualEqual => BinaryOp::Equals,
                TokenType::NotEquals => BinaryOp::NotEquals,
                TokenType::LogicalAnd => BinaryOp::LogicalAnd,
                TokenType::LogicalOr => BinaryOp::LogicalOr,
                TokenType::LessThan if self.peekpeek().typ == TokenType::Equal => {
                    BinaryOp::LessThanEq
                }
                TokenType::LessThan => BinaryOp::LessThan,
                TokenType::GreaterThan if self.peekpeek().typ == TokenType::Equal => {
                    BinaryOp::GreaterThanEq
                }
                TokenType::GreaterThan => BinaryOp::GreaterThan,
                _ => break,
            };
            self.advance();
            let right = self.pipe_operator()?;
            expr = Expression::binary(op, loc, expr, right);
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
            let right_side = Box::new(self.parse_expression()?);
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
                    assign_set!(expr, right, BinaryOp::Plus, operator.location);
                }
                TokenType::MinusAssign => {
                    operator.typ = TokenType::Minus;
                    assign_set!(expr, right, BinaryOp::Minus, operator.location);
                }
                TokenType::Plus => {
                    expr = Expression::binary(BinaryOp::Plus, operator.location, expr, right)
                }
                TokenType::Minus => {
                    expr = Expression::binary(BinaryOp::Minus, operator.location, expr, right)
                }
                _ => unreachable!(),
            }
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expression, ParsingError> {
        let mut expr = self.unary()?;

        loop {
            let op = match (self.peek().typ, self.peekpeek().typ) {
                (TokenType::Divide, _) => BinaryOp::Divide,
                (TokenType::Modulo, _) => BinaryOp::Modulo,
                (TokenType::Asterix, _) => BinaryOp::Multiply,
                (TokenType::Ampersand, _) => BinaryOp::BitwiseAnd,
                (TokenType::BitwiseOr, _) => BinaryOp::BitwiseOr,
                (TokenType::BitwiseXor, _) => BinaryOp::BitwiseXor,
                (TokenType::LessThan, TokenType::LessThan) => BinaryOp::LShift,
                (TokenType::GreaterThan, TokenType::GreaterThan) => BinaryOp::RShift,
                _ => break,
            };
            let loc = self.advance().location.clone();
            if op == BinaryOp::LShift || op == BinaryOp::RShift {
                self.advance();
            }
            let right = self.unary()?;
            if self.match_tok(TokenType::Equal) {
                assign_set!(expr, right, op, loc);
            } else {
                expr = Expression::binary(op, loc, expr, right);
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
            let operator = match self.previous().typ {
                TokenType::Plus => UnaryOp::Plus,
                TokenType::Minus => UnaryOp::Minus,
                TokenType::BitwiseNot => UnaryOp::BitwiseNot,
                TokenType::LogicalNot => UnaryOp::LogicalNot,
                _ => unreachable!(),
            };
            let loc = self.previous().location.clone();
            return Ok(Expression::unary(operator, loc, self.unary()?));
        }

        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expression, ParsingError> {
        let expr = self.type_cast()?;
        if self.match_tok(TokenType::Equal) {
            let loc = self.previous().location.clone();
            let value = self.parse_expression()?;
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
                operator: UnaryOp::Reference,
                loc: self.previous().location.clone(),
                right_side: Box::new(self.references()?),
            })
        } else if self.match_tok(TokenType::LogicalAnd) {
            let expr = Expression::Unary {
                operator: UnaryOp::Reference,
                loc: self.previous().location.clone(),
                right_side: Box::new(self.references()?),
            };
            let mut loc = self.previous().location.clone();
            loc.column += 1;
            Ok(Expression::Unary {
                operator: UnaryOp::Reference,
                loc,
                right_side: Box::new(expr),
            })
        } else if self.match_tok(TokenType::Asterix) {
            Ok(Expression::Unary {
                operator: UnaryOp::Dereference,
                loc: self.previous().location.clone(),
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
                    if let Ok(expr) = self.parse_expression() {
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
                let Ok(indexing_expr) = self.parse_expression() else {
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
        if self.match_tok(TokenType::Dot) {
            // .{} / .[] / .()
            let typ = self.peek().typ;
            let loc = self.peek().location.clone();
            if typ == TokenType::ParenLeft || typ == TokenType::BracketLeft {
                self.advance();
                let matching_tok = if typ == TokenType::ParenLeft {
                    TokenType::ParenRight
                } else {
                    TokenType::BracketRight
                };
                let mut elements = Vec::new();
                while !self.match_tok(matching_tok) {
                    if elements.len() > 0 {
                        if !self.match_tok(TokenType::Comma) {
                            return Err(ParsingError::ExpectedArbitrary {
                                loc: self.peek().location.clone(),
                                expected: TokenType::Comma,
                                found: self.peek().typ,
                            });
                        }
                        if self.match_tok(matching_tok) {
                            break;
                        }
                    }

                    elements.push((self.peek().location.clone(), self.parse_expression()?));
                }
                return Ok(Expression::Literal(LiteralValue::Tuple(elements), loc));
            } else if typ == TokenType::CurlyLeft {
                return self
                    .try_object()
                    .expect("this should never return None as we ensured the next token is of type CurlyLeft.")
                    .map(LiteralValue::AnonymousStruct)
                    .map(move |v| Expression::Literal(v, loc));
            } else {
                unreachable!()
            }
        }

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
            let expr = self.parse_expression()?;
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

                match self.parse_expression() {
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

                match self.parse_expression() {
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
