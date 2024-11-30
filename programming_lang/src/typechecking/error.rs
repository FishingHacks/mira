use thiserror::Error;

use crate::{
    error::FunctionList,
    globals::GlobalStr,
    lang_items::{LangItemAssignmentError, LangItemError},
    tokenizer::Location,
};

use super::{types::Type, ScopeKind};

#[derive(Clone, Debug, Error)]
pub enum TypecheckingError {
    #[error("{0}")]
    LangItemError(#[from] LangItemError),
    #[error("{0}")]
    LangItemAssignment(#[from] LangItemAssignmentError),
    #[error("{location}: could not find export `{name}`")]
    ExportNotFound { location: Location, name: GlobalStr },
    #[error("{location}: cyclic dependency detected")]
    CyclicDependency { location: Location },
    #[error("{location}: Unbound identifier `{name}`")]
    UnboundIdent { location: Location, name: GlobalStr },
    #[error("{location}: Expected a {expected:?}, but found a {found:?}")]
    MismatchingScopeType {
        location: Location,
        expected: ScopeKind,
        found: ScopeKind,
    },
    #[error("{location}: Recursive type detected")]
    RecursiveTypeDetected { location: Location },
    #[error("{location}: Body does not always return")]
    BodyDoesNotAlwaysReturn { location: Location },
    #[error("{location}: Expected {expected}, but found {found}")]
    MismatchingType {
        expected: Type,
        found: Type,
        location: Location,
    },
    #[error("{location}: Function pointers can't have generics")]
    GenericFunctionPointer { location: Location },
    #[error("{location}: `{name}` is not a struct type")]
    IdentifierIsNotStruct { location: Location, name: GlobalStr },
    #[error("{location}: no such field named `{name}` found!")]
    NoSuchFieldFound { location: Location, name: GlobalStr },
    #[error("{location}: missing firld `{name}`")]
    MissingField { location: Location, name: GlobalStr },
    #[error("{location}: Expected a function")]
    TypeIsNotAFunction { location: Location },
    #[error("{location}: Function misses arguments")]
    MissingArguments { location: Location },
    #[error("{location}: Function expects no more arguments")]
    TooManyArguments { location: Location },
    #[error("{location}: Did not expected a generic here.")]
    UnexpectedGenerics { location: Location },
    #[error("{location}: `{name}` is not a member of the trait.")]
    IsNotTraitMember { location: Location, name: GlobalStr },
    #[error("{location}: missing trait item `{name}`")]
    MissingTraitItem { location: Location, name: GlobalStr },
    #[error("{location}: Expected {}, but found {}", FunctionList(.expected), FunctionList(.found))]
    MismatchingArguments {
        location: Location,
        expected: Vec<Type>,
        found: Vec<Type>,
    },
    #[error("{location}: Expected fn(...) -> {expected} but fund fn(...) -> {found}")]
    MismatchingReturnType {
        location: Location,
        expected: Type,
        found: Type,
    },
}
