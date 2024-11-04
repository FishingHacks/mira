use crate::{globals::GlobalStr, module::FunctionId, parser::LiteralValue, tokenizer::Location};

use super::types::Type;

#[derive(Debug)]
pub enum TypecheckedExpression {
    // 1, "a", ["a", "b"], meow { test: 12 }, ...
    Literal(Location, Type, LiteralValue),
    // 1..=2
    Range {
        location: Location,
        typ: Type,
        lhs: Box<TypecheckedExpression>,
        rhs: Box<TypecheckedExpression>,
        inclusive: bool,
    },
    // a, meow, ...
    Variable(Location, Type, GlobalStr),
    // a = b
    Assignment(
        Location,
        Box<TypecheckedExpression>,
        Box<TypecheckedExpression>,
    ),
    // some function
    Function(Location, FunctionId),
    // some external function
    ExternalFunction(Location, FunctionId),
    // a[b]
    Index(
        Location,
        Box<TypecheckedExpression>,
        Box<TypecheckedExpression>,
    ),
    // a(b, c, d, ...)
    Call(
        Location,
        Box<TypecheckedExpression>,
        Vec<TypecheckedExpression>,
    ),
    // +a
    Pos(Location, Box<TypecheckedExpression>),
    // -a
    Neg(Location, Box<TypecheckedExpression>),
    // !a
    LNot(Location, Box<TypecheckedExpression>),
    // ~a
    BNot(Location, Box<TypecheckedExpression>),
    // a + b
    Add(
        Location,
        Box<TypecheckedExpression>,
        Box<TypecheckedExpression>,
    ),
    // a - b
    Sub(
        Location,
        Box<TypecheckedExpression>,
        Box<TypecheckedExpression>,
    ),
    // a * b
    Mul(
        Location,
        Box<TypecheckedExpression>,
        Box<TypecheckedExpression>,
    ),
    // a / b
    Div(
        Location,
        Box<TypecheckedExpression>,
        Box<TypecheckedExpression>,
    ),
    // a % b
    Mod(
        Location,
        Box<TypecheckedExpression>,
        Box<TypecheckedExpression>,
    ),
    // a & b
    BAnd(
        Location,
        Box<TypecheckedExpression>,
        Box<TypecheckedExpression>,
    ),
    // a | b
    BOr(
        Location,
        Box<TypecheckedExpression>,
        Box<TypecheckedExpression>,
    ),
    // a ^ b
    BXor(
        Location,
        Box<TypecheckedExpression>,
        Box<TypecheckedExpression>,
    ),
    // a > b
    GreaterThan(
        Location,
        Box<TypecheckedExpression>,
        Box<TypecheckedExpression>,
    ),
    // a > b
    LessThan(
        Location,
        Box<TypecheckedExpression>,
        Box<TypecheckedExpression>,
    ),
    // a && b
    LAnd(
        Location,
        Box<TypecheckedExpression>,
        Box<TypecheckedExpression>,
    ),
    // a || b
    LOr(
        Location,
        Box<TypecheckedExpression>,
        Box<TypecheckedExpression>,
    ),
    // a >= b
    GreaterThanEq(
        Location,
        Box<TypecheckedExpression>,
        Box<TypecheckedExpression>,
    ),
    // a <= b
    LessThanEq(
        Location,
        Box<TypecheckedExpression>,
        Box<TypecheckedExpression>,
    ),
    // a == b
    Eq(
        Location,
        Box<TypecheckedExpression>,
        Box<TypecheckedExpression>,
    ),
    // a != b
    Neq(
        Location,
        Box<TypecheckedExpression>,
        Box<TypecheckedExpression>,
    ),
    // a << b
    LShift(
        Location,
        Box<TypecheckedExpression>,
        Box<TypecheckedExpression>,
    ),
    // a >> b
    RShift(
        Location,
        Box<TypecheckedExpression>,
        Box<TypecheckedExpression>,
    ),
    None,
}
