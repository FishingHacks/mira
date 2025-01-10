use thiserror::Error;

use crate::{
    error::FunctionList,
    globals::GlobalStr,
    lang_items::{LangItemAssignmentError, LangItemError},
    parser::Path,
    tokenizer::Location,
};

use super::{types::Type, ScopeKind};

#[derive(Clone, Debug, Error)]
pub enum TypecheckingError {
    #[error("{0}")]
    LangItemError(#[from] LangItemError),
    #[error("{0}")]
    LangItemAssignment(#[from] LangItemAssignmentError),
    #[error("{0}: Statics can only have literal values")]
    StaticsNeedToBeLiteral(Location),
    #[error(
        "{0}: Cannot infer array type. Use `[] as [<type>;0]` to explicitly specify the expected array type"
    )]
    CannotInferArrayType(Location),
    #[error("{0}: Function `{1}` of type `{2}` is not a method as it doesn't have the signature (Self, ...) or (&Self, ...)")]
    NonMemberFunction(Location, GlobalStr, Type),
    #[error("{0}: Cannot find function `{1}` on type `{2}`")]
    CannotFindFunctionOnType(Location, GlobalStr, Type),
    #[error("{0}: Cannot find value `{1}`")]
    CannotFindValue(Location, Path),
    #[error("{0}: Tried to access a member of a non-struct value `{1}`")]
    AccessNonStructValue(Location, Type),
    #[error("{0}: Tried to index into non-array type `{1}`")]
    IndexNonArrayElem(Location, Type),
    #[error("{0}: Could not find field `{2}` on `{1}`")]
    FieldNotFound(Location, Type, GlobalStr),
    #[error("{0}: Invalid pointer cast, can only cast pointers with the same reference count. Did you mean to use `unsafe_pointer_cast`? (lhs: `{1}`, rhs: `{2}`)")]
    DisallowedPointerCast(Location, Type, Type),
    #[error("{0}: Cannot assign to this expression")]
    CannotAssign(Location),
    #[error("{0}: Cannot shift by a non-uint value (found `{1}`)")]
    CannotShiftByNonUInt(Location, Type),
    #[error("{0}: Cannot add `{1}`")]
    CannotAdd(Location, Type),
    #[error("{0}: Cannot subtract `{1}`")]
    CannotSub(Location, Type),
    #[error("{0}: Cannot multiply `{1}`")]
    CannotMul(Location, Type),
    #[error("{0}: Cannot divide `{1}`")]
    CannotDiv(Location, Type),
    #[error("{0}: Cannot take the remainder of `{1}`")]
    CannotMod(Location, Type),
    #[error("{0}: Cannot binary and `{1}`")]
    CannotBAnd(Location, Type),
    #[error("{0}: Cannot binary or `{1}`")]
    CannotBOr(Location, Type),
    #[error("{0}: Cannot xor `{1}`")]
    CannotBXor(Location, Type),
    #[error("{0}: Cannot and `{1}`")]
    CannotLAnd(Location, Type),
    #[error("{0}: Cannot or `{1}`")]
    CannotLOr(Location, Type),
    #[error("{0}: Cannot compare `{1}`")]
    CannotCompare(Location, Type),
    #[error("{0}: Cannot compare the equality of `{1}`")]
    CannotEq(Location, Type),
    #[error("{0}: Cannot shift `{1}` left")]
    CannotShl(Location, Type),
    #[error("{0}: Cannot shift `{1}` right")]
    CannotShr(Location, Type),
    #[error("{0}: Expected both sides of the expression to be the same, lhs: `{1}`, rhs: `{2}`")]
    LhsNotRhs(Location, Type, Type),
    #[error("{0}: Cannot negate a `{1}`")]
    CannotNeg(Location, Type),
    #[error("{0}: Cannot use unary `+` on a `{1}`")]
    CannotPos(Location, Type),
    #[error("{0}: Cannot not a `{1}`")]
    CannotLNot(Location, Type),
    #[error("{0}: Cannot invert a `{1}`")]
    CannotBNot(Location, Type),
    #[error("{0}: cannot dereference a `{1}`")]
    CannotDeref(Location, Type),
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
    #[error("{location}: missing field `{name}`")]
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
