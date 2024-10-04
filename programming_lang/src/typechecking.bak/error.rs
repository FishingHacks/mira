use std::fmt::Debug;

use crate::{globals::GlobalStr, tokenizer::Location};

use super::{type_resolution::TypeName, Type};

#[derive(Clone)]
pub enum ProgrammingLangTypecheckingError {
    /// An unsized type was found in the struct in a field that is not the last one
    UnsizedTypeInsideStruct {
        loc: Location,
        field_name: GlobalStr,
    },
    UnboundType {
        loc: Location,
        type_name: GlobalStr,
    },
    UnexpectedType {
        loc: Location,
        expected: Type,
        found: Type,
    },
    VariableAlreadyDeclared {
        loc: Location,
        name: GlobalStr,
    },
}

impl Debug for ProgrammingLangTypecheckingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnsizedTypeInsideStruct { loc, field_name } => f.write_fmt(format_args!("{loc}: Unsized types are only valid as the last field of a struct (field `{field_name}` is unsized)")),
            Self::UnboundType { loc, type_name } => f.write_fmt(format_args!("{loc}: Unbound or recursive type `{type_name}`")),
            Self::UnexpectedType { loc, expected, found } => f.write_fmt(format_args!("{loc}: Expected type {}, but found {}", TypeName(expected), TypeName(found))),
            Self::VariableAlreadyDeclared { loc, name } => f.write_fmt(format_args!("{loc}: Variable {name} is already in use!")),
        }
    }
}
