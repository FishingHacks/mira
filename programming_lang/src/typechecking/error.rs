use crate::{globals::GlobalStr, tokenizer::Location};
use std::fmt::Debug;

use super::{types::Type, ScopeKind};

#[derive(Clone)]
pub enum TypecheckingError {
    ExportNotFound {
        location: Location,
        name: GlobalStr,
    },
    CyclicDependency {
        location: Location,
    },
    UnboundIdent {
        location: Location,
        name: GlobalStr,
    },
    MismatchingScopeType {
        location: Location,
        expected: ScopeKind,
        found: ScopeKind,
    },
    RecursiveTypeDetected {
        location: Location,
    },
    BodyDoesNotAlwaysReturn {
        location: Location,
    },
    MismatchingType {
        expected: Type,
        found: Type,
        location: Location,
    },
    GenericFunctionPointer {
        location: Location,
    },
    IdentifierIsNotStruct {
        location: Location,
        name: GlobalStr,
    },
    NoSuchFieldFound {
        location: Location,
        name: GlobalStr,
    },
    MissingField {
        location: Location,
        name: GlobalStr,
    },
    TypeIsNotAFunction {
        location: Location,
    },
    MissingArguments {
        location: Location,
    },
    TooManyArguments {
        location: Location,
    },
    UnexpectedGenerics {
        location: Location,
    },
}

impl Debug for TypecheckingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExportNotFound { location, name } => {
                f.write_fmt(format_args!("{location}: could not find export {name}"))
            }
            Self::CyclicDependency { location } => {
                f.write_fmt(format_args!("{location}: cyclic dependency detected"))
            }
            Self::UnboundIdent { location, name } => {
                f.write_fmt(format_args!("{location}: Unbound identifier `{name}`"))
            }
            Self::MismatchingScopeType {
                location,
                expected,
                found,
            } => f.write_fmt(format_args!(
                "{location}: Expected a {expected:?}, but foun a {found:?}"
            )),
            Self::RecursiveTypeDetected { location } => {
                f.write_fmt(format_args!("{location}: Recursive type detected"))
            }
            Self::BodyDoesNotAlwaysReturn { location } => {
                f.write_fmt(format_args!("{location}: body does not always return"))
            }
            Self::MismatchingType {
                expected,
                found,
                location,
            } => f.write_fmt(format_args!(
                "{location}: Expected {expected}, but found {found}"
            )),
            Self::GenericFunctionPointer { location } => f.write_fmt(format_args!(
                "{location}: Function pointers can't have generics"
            )),
            Self::IdentifierIsNotStruct { location, name } => {
                f.write_fmt(format_args!("{location}: {name} is not a struct type"))
            }
            Self::NoSuchFieldFound { location, name } => {
                f.write_fmt(format_args!("{location}: such field named `{name}` found!"))
            }
            Self::MissingField { location, name } => {
                f.write_fmt(format_args!("{location}: missing field `{name}`"))
            }
            Self::TypeIsNotAFunction { location } => {
                f.write_fmt(format_args!("{location}: Expected a function"))
            }
            Self::MissingArguments { location } => {
                f.write_fmt(format_args!("{location}: Function misses arguments"))
            }
            Self::TooManyArguments { location } => f.write_fmt(format_args!(
                "{location}: Function expects no more arguments"
            )),
            Self::UnexpectedGenerics { location } => {
                f.write_fmt(format_args!("{location}: Did not expect a generic here."))
            }
        }
    }
}
