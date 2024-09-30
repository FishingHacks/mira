use std::{fmt::Debug, rc::Rc};

use crate::tokenizer::Location;

#[derive(Clone)]
pub enum ProgrammingLangTypecheckingError {
    /// An unsized type was found in the struct in a field that is not the last one
    UnsizedTypeInsideStruct {
        loc: Location,
        field_name: Rc<str>,
    },
    UnboundType {
        loc: Location,
        type_name: Rc<str>,
    },
}

impl Debug for ProgrammingLangTypecheckingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnsizedTypeInsideStruct { loc, field_name } => f.write_fmt(format_args!("{loc}: Unsized types are only valid as the last field of a struct (field `{field_name}` is unsized)")),
            Self::UnboundType { loc, type_name } => f.write_fmt(format_args!("{loc}: Unbound or recursive type `{type_name}`")),
        }
    }
}
