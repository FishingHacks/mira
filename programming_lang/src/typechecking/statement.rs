use crate::{parser::Annotations, tokenizer::Location};

use super::{expression::TypecheckedExpression, types::Type};

#[derive(Debug)]
pub enum TypecheckedStatement {
    Return {
        location: Location,
        typ: Type,
        expr: TypecheckedExpression,
    },
    Block {
        location: Location,
        statements: Box<[TypecheckedStatement]>,
        annotations: Annotations,
    },
    Expression(TypecheckedExpression),
    None,
}
