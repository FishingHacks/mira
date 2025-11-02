use std::{
    fmt::{Display, Write},
    sync::Arc,
};

use mira_lexer::token::IdentDisplay;
use mira_macros::ErrorData;
use mira_parser::{Path, PathWithoutGenerics};
use mira_spans::{Span, interner::Symbol};

use super::{ScopeKind, Ty};

#[derive(Clone, Debug, ErrorData)]
pub enum TypecheckingError<'arena> {
    #[error("No main function defined")]
    #[note("consider adding a main function to `{}`", _0.display())]
    #[note("main function signature: `pub fn main() {{ }}`")]
    MainFuncNotFound(Arc<std::path::Path>),
    #[error("Main function has wrong type")]
    #[note("Expected signature: `fn main()`")]
    MainFuncWrongType {
        #[primary_label("")]
        func_span: Span<'arena>,
    },
    #[error("Expected a sized type, but got {_1}")]
    UnsizedForSizedGeneric(
        #[primary_label("expected a sized type")] Span<'arena>,
        Ty<'arena>,
    ),
    #[error("Unsized Type {_1} is not a return type")]
    UnsizedReturnType(
        #[primary_label("type has to be sized")] Span<'arena>,
        Ty<'arena>,
    ),
    #[error("Unsized Type {_1} is not a valid argument")]
    #[note("Try using `&{_1}`")]
    UnsizedArgument(
        #[primary_label("type has to be sized")] Span<'arena>,
        Ty<'arena>,
    ),
    #[error("Function {0} on trait {1} is not valid for &dyn {1} types", IdentDisplay(*_1), IdentDisplay(*_2))]
    InvalidDynTypeFunc(
        #[primary_label("dyn-incompatible trait")] Span<'arena>,
        Symbol<'arena>,
        Symbol<'arena>,
    ),
    #[error("Cannot find trait {_1}")]
    CannotFindTrait(
        #[primary_label("Cannot find trait")] Span<'arena>,
        PathWithoutGenerics<'arena>,
    ),
    #[error("Expected {_1} generics, but found {_2}")]
    MismatchingGenericCount(
        #[primary_label("expected {} generics", if _2 < _1 { "more" } else { "less" })]
        Span<'arena>,
        usize,
        usize,
    ),
    #[error("The size of {_1} needs to be known at compiletime")]
    #[note("Try using `&{_1}`")]
    NonSizedType(
        #[primary_label("the size needs to be known")] Span<'arena>,
        Ty<'arena>,
    ),
    #[error("This Intrinsic accepts only integers, but got: {_1}")]
    IntOnlyIntrinsic(#[primary_label("")] Span<'arena>, Ty<'arena>),
    #[error("Inline assembly only accepts numeric types (i_, u_, f_ and bool), but got `{_1}`")]
    AsmNonNumericTypeResolved(
        #[primary_label("expected a numeric type")] Span<'arena>,
        Ty<'arena>,
    ),
    #[error("Inline assembly only accepts numeric types (i_, u_, f_ and bool), but got {}", IdentDisplay(*_1))]
    AsmNonNumericType(
        #[primary_label("expected a numeric type")] Span<'arena>,
        Symbol<'arena>,
    ),
    #[error("Index `{_1}` is out of bounds for an array of size `{_2}`")]
    ArrayIndexOutOfBounds(#[primary_label("")] Span<'arena>, usize, usize),
    #[error("No field `{_2}` on a tuple with {_1} fields")]
    TupleIndexOutOfBounds(#[primary_label("unknown field")] Span<'arena>, usize, usize),
    #[error("Cannot index into a tuple with a non-static literal")]
    TupleDynamicIndex(
        #[primary_label("tried to index a tuple with something other than a number")] Span<'arena>,
    ),
    #[error("Cannot infer type for anonymous struct")]
    #[note("Use `<struct> {{}}` instead")]
    CannotInferAnonStructType(#[primary_label("Cannot infer type")] Span<'arena>),
    #[error("Statics can only have literal values")]
    StaticsNeedToBeLiteral(#[primary_label("statics have to be literal values")] Span<'arena>),
    #[error("Cannot infer the array's item type.")]
    #[note("Try using `[] as [<type>;0]`")]
    CannotInferArrayType(#[primary_label("cannot infer type")] Span<'arena>),
    #[error(
        "Function {} of type `{_2}` is not a method as it doesn't have the signature (Self, ...) or (&Self, ...)", IdentDisplay(*_1)
    )]
    NonMemberFunction(
        #[primary_label("Cannot find method {}", IdentDisplay(*_1))] Span<'arena>,
        Symbol<'arena>,
        Ty<'arena>,
    ),
    #[error("Cannot find function {} on type `{_2}`", IdentDisplay(*_1))]
    CannotFindFunctionOnType(
        #[primary_label("No function named {} is associated with `{_2}`", IdentDisplay(*_1))]
        Span<'arena>,
        Symbol<'arena>,
        Ty<'arena>,
    ),
    #[error("Cannot find function {} on trait `{}`", IdentDisplay(*func_name), IdentDisplay(*trait_name))]
    CannotFindFunctionOnTrait {
        #[primary_label("No function named {} is associated with trait `{}`", IdentDisplay(*func_name), IdentDisplay(*trait_name))]
        span: Span<'arena>,
        func_name: Symbol<'arena>,
        trait_name: Symbol<'arena>,
    },
    #[error("Cannot find value `{_1}` in this scope")]
    CannotFindValue(
        #[primary_label("not found in this scope")] Span<'arena>,
        Path<'arena>,
    ),
    #[error("No such field on `{_1}`")]
    AccessNonStructValue(
        #[primary_label("no such field found")] Span<'arena>,
        Ty<'arena>,
    ),
    #[error("Tried to index into value of type `{_1}`")]
    IndexNonArrayElem(
        #[primary_label("Cannot index into this value")] Span<'arena>,
        Ty<'arena>,
    ),
    #[error("Could not find field {} on `{_1}`", IdentDisplay(*_2))]
    FieldNotFound(
        #[primary_label("no such field found")] Span<'arena>,
        Ty<'arena>,
        Symbol<'arena>,
    ),
    #[error("Invalid cast `{_1}` -> `{_2}`")]
    DisallowedCast(
        #[primary_label("invalid cast")] Span<'arena>,
        Ty<'arena>,
        Ty<'arena>,
    ),
    #[error("Cannot assign to this expression")]
    CannotAssign(#[primary_label("invalid assignment")] Span<'arena>),
    #[error("Cannot shift by a non-uint value (found `{_1}`)")]
    CannotShiftByNonUInt(
        #[primary_label("invalid value for bit-shifting")] Span<'arena>,
        Ty<'arena>,
    ),
    #[error("Cannot add `{_1}`")]
    CannotAdd(#[primary_label("cannot add")] Span<'arena>, Ty<'arena>),
    #[error("Cannot subtract `{_1}`")]
    CannotSub(#[primary_label("cannot subtract")] Span<'arena>, Ty<'arena>),
    #[error("Cannot multiply `{_1}`")]
    CannotMul(#[primary_label("cannot multiple")] Span<'arena>, Ty<'arena>),
    #[error("Cannot divide `{_1}`")]
    CannotDiv(#[primary_label("cannot divide")] Span<'arena>, Ty<'arena>),
    #[error("Cannot take the remainder of `{_1}`")]
    CannotMod(
        #[primary_label("cannot take the remainder")] Span<'arena>,
        Ty<'arena>,
    ),
    #[error("Cannot binary and `{_1}`")]
    CannotBAnd(
        #[primary_label("cannot binary and")] Span<'arena>,
        Ty<'arena>,
    ),
    #[error("Cannot binary or `{_1}`")]
    CannotBOr(
        #[primary_label("cannot binary or")] Span<'arena>,
        Ty<'arena>,
    ),
    #[error("Cannot xor `{_1}`")]
    CannotBXor(
        #[primary_label("cannot binary xor")] Span<'arena>,
        Ty<'arena>,
    ),
    #[error("Cannot compare `{_1}`")]
    CannotCompare(#[primary_label("cannot compare")] Span<'arena>, Ty<'arena>),
    #[error("Cannot compare the equality of `{_1}`")]
    CannotEq(#[primary_label("cannot equate")] Span<'arena>, Ty<'arena>),
    #[error("Cannot shift `{_1}` left")]
    CannotShl(#[primary_label("cannot leftshit")] Span<'arena>, Ty<'arena>),
    #[error("Cannot shift `{_1}` right")]
    CannotShr(
        #[primary_label("cannot right shift")] Span<'arena>,
        Ty<'arena>,
    ),
    #[error("Expected both sides of the expression to be the same, lhs: `{_1}`, rhs: `{_2}`")]
    LhsNotRhs(
        #[primary_label("the left and right side don't match")] Span<'arena>,
        Ty<'arena>,
        Ty<'arena>,
    ),
    #[error("Cannot negate a `{_1}`")]
    CannotNeg(#[primary_label("cannot negate")] Span<'arena>, Ty<'arena>),
    #[error("Cannot use unary `+` on a `{_1}`")]
    CannotPos(
        #[primary_label("cannot use unary `+`")] Span<'arena>,
        Ty<'arena>,
    ),
    #[error("Cannot not a `{_1}`")]
    CannotLNot(#[primary_label("cannot not")] Span<'arena>, Ty<'arena>),
    #[error("Cannot invert a `{_1}`")]
    CannotBNot(#[primary_label("cannot invert")] Span<'arena>, Ty<'arena>),
    #[error("cannot dereference a `{_1}`")]
    CannotDeref(
        #[primary_label("cannot dereference")] Span<'arena>,
        Ty<'arena>,
    ),
    #[error("could not find item {}", IdentDisplay(*name))]
    ItemNotFound {
        #[primary_label("No such item found")]
        span: Span<'arena>,
        name: Symbol<'arena>,
    },
    #[error("cyclic dependency detected")]
    CyclicDependency(#[primary_label("cyclic dependency")] Span<'arena>),
    #[error("Unbound identifier {}", IdentDisplay(*name))]
    UnboundIdent {
        #[primary_label("unbound identifier")]
        span: Span<'arena>,
        name: Symbol<'arena>,
    },
    #[error("Expected a {expected}, but found a {found}")]
    MismatchingScopeType {
        #[primary_label("Expected a {expected}")]
        span: Span<'arena>,
        expected: ScopeKind,
        found: ScopeKind,
    },
    #[error("Recursive type detected")]
    RecursiveTypeDetected {
        #[primary_label("recursive type")]
        span: Span<'arena>,
    },
    #[error("The function does not always return but is also not void")]
    BodyDoesNotAlwaysReturn {
        #[primary_label("Function sometimes returns void")]
        span: Span<'arena>,
    },
    #[error("Expected {expected}, but found {found}")]
    MismatchingType {
        #[primary_label("types don't match")]
        span: Span<'arena>,
        expected: Ty<'arena>,
        found: Ty<'arena>,
    },
    #[error("{} is not a struct type", IdentDisplay(*name))]
    IdentifierIsNotStruct {
        #[primary_label("Tried to construct a non-struct type as a struct")]
        span: Span<'arena>,
        name: Symbol<'arena>,
    },
    #[error("no such field named {} found!", IdentDisplay(*name))]
    NoSuchFieldFound {
        #[primary_label("no such field found")]
        span: Span<'arena>,
        name: Symbol<'arena>,
    },
    #[error("missing field {}", IdentDisplay(*name))]
    MissingField {
        #[primary_label("missing {}", IdentDisplay(*name))]
        span: Span<'arena>,
        name: Symbol<'arena>,
    },
    #[error("Expected a function")]
    TypeIsNotAFunction {
        #[primary_label("expected a function")]
        span: Span<'arena>,
    },
    #[error("Function misses arguments")]
    MissingArguments {
        #[primary_label("too little arguments supplied")]
        span: Span<'arena>,
    },
    #[error("Function expects no more arguments")]
    TooManyArguments {
        #[primary_label("Last expected argument")]
        span: Span<'arena>,
    },
    #[error("A module cannot have generics")]
    UnexpectedGenerics {
        #[primary_label("generics not allowed")]
        span: Span<'arena>,
    },
    #[error("{} is not a member of the trait.", IdentDisplay(*name))]
    IsNotTraitMember {
        #[primary_label("No such method exists on the trait")]
        span: Span<'arena>,
        name: Symbol<'arena>,
    },
    #[error("missing trait item {}", IdentDisplay(*name))]
    MissingTraitItem {
        #[primary_label("Missing implementation for trait item")]
        span: Span<'arena>,
        name: Symbol<'arena>,
    },
    #[error("Type {_1} is expected to implement the traits{}", _2.iter().map(|v| format!(" {}", IdentDisplay(*v))).collect::<String>())]
    MismatchingTraits(
        #[primary_label("Trait is missing dependency traits")] Span<'arena>,
        Ty<'arena>,
        Vec<Symbol<'arena>>,
    ),
    #[error(
        "Expected {}, but found {}",
        FunctionList(expected),
        FunctionList(found)
    )]
    MismatchingArguments {
        #[primary_label("Mismatching arguments")]
        span: Span<'arena>,
        expected: Vec<Ty<'arena>>,
        found: Vec<Ty<'arena>>,
    },
    #[error("Expected fn(...) -> {expected} but fund fn(...) -> {found}")]
    MismatchingReturnType {
        #[primary_label("Mismatching return type")]
        span: Span<'arena>,
        expected: Ty<'arena>,
        found: Ty<'arena>,
    },
}

pub struct FunctionList<'a>(pub &'a [Ty<'a>]);

impl Display for FunctionList<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("fn(")?;
        for i in 0..self.0.len() {
            if i != 0 {
                f.write_str(", ")?;
            }
            Display::fmt(&self.0[i], f)?;
        }
        f.write_char(')')
    }
}
