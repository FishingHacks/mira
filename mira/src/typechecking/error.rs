use crate::{
    error::FunctionList,
    parser::{Path, PathWithoutGenerics},
};
use mira_macros::ErrorData;
use mira_spans::{interner::Symbol, Span};

use super::{types::Type, ScopeKind};

#[derive(Clone, Debug, ErrorData)]
pub enum TypecheckingError<'arena> {
    #[error("Expected a sized type, but got {_1}")]
    UnsizedForSizedGeneric(
        #[primary_label("expected a sized type")] Span<'arena>,
        Type<'arena>,
    ),
    // NOTE: This is due to each ABI handling how to return values differently. LLVM doesn't handle
    // this for us, but, as far as i can tell, it's fine if we return structs from non-extern
    // functions as that should never produce incorrect behavior. IF how ever, it turns out to,
    // then the "internal" ABI will pass all structs whose size is greather than 128 as an sret
    // first argument (fn(_) -> v turns into fn(&v, _) if sizeof(v) > 128)
    //
    // For further references: Zig's implementation of if something needs to be passed as an sret:
    // https://github.com/ziglang/zig/blob/master/src/codegen/llvm.zig#L12021
    #[error("Extern functions can as of now only return primitive values or thin pointers")]
    InvalidExternReturnType(
        #[primary_label("external function returns a fat pointer or non-primitive")] Span<'arena>,
    ),
    #[error("Unsized Type {_1} is not a return type")]
    UnsizedReturnType(
        #[primary_label("type has to be sized")] Span<'arena>,
        Type<'arena>,
    ),
    #[error("Unsized Type {_1} is not a valid argument")]
    #[note("Try using `&{_1}`")]
    UnsizedArgument(
        #[primary_label("type has to be sized")] Span<'arena>,
        Type<'arena>,
    ),
    #[error("Function {_1} on trait {_2} is not valid for &dyn {_2} types")]
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
        Type<'arena>,
    ),
    #[error("This Intrinsic accepts only integers, but got: {_1}")]
    IntOnlyIntrinsic(#[primary_label("")] Span<'arena>, Type<'arena>),
    #[error("Inline assembly only accepts numeric types (i_, u_, f_ and bool), but got `{_1}`")]
    AsmNonNumericTypeResolved(
        #[primary_label("expected a numeric type")] Span<'arena>,
        Type<'arena>,
    ),
    #[error("Inline assembly only accepts numeric types (i_, u_, f_ and bool), but got `{_1}`")]
    AsmNonNumericType(
        #[primary_label("expected a numeric type")] Span<'arena>,
        Symbol<'arena>,
    ),
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
    #[error("Function `{_1}` of type `{_2}` is not a method as it doesn't have the signature (Self, ...) or (&Self, ...)")]
    NonMemberFunction(
        #[primary_label("Cannot find method `{_1}`")] Span<'arena>,
        Symbol<'arena>,
        Type<'arena>,
    ),
    #[error("Cannot find function `{_1}` on type `{_2}`")]
    CannotFindFunctionOnType(
        #[primary_label("No function named `{_1}` is associated with `{_2}`")] Span<'arena>,
        Symbol<'arena>,
        Type<'arena>,
    ),
    #[error("Cannot find value `{_1}` in this scope")]
    CannotFindValue(
        #[primary_label("not found in this scope")] Span<'arena>,
        Path<'arena>,
    ),
    #[error("No such field on `{_1}`")]
    AccessNonStructValue(
        #[primary_label("no such field found")] Span<'arena>,
        Type<'arena>,
    ),
    #[error("Tried to index into value of type `{_1}`")]
    IndexNonArrayElem(
        #[primary_label("Cannot index into this value")] Span<'arena>,
        Type<'arena>,
    ),
    #[error("Could not find field `{_2}` on `{_1}`")]
    FieldNotFound(
        #[primary_label("no such field found")] Span<'arena>,
        Type<'arena>,
        Symbol<'arena>,
    ),
    #[error("Invalid cast `{_1}` -> `{_2}`")]
    DisallowedCast(
        #[primary_label("invalid cast")] Span<'arena>,
        Type<'arena>,
        Type<'arena>,
    ),
    #[error("Cannot assign to this expression")]
    CannotAssign(#[primary_label("invalid assignment")] Span<'arena>),
    #[error("Cannot shift by a non-uint value (found `{_1}`)")]
    CannotShiftByNonUInt(
        #[primary_label("invalid value for bit-shifting")] Span<'arena>,
        Type<'arena>,
    ),
    #[error("Cannot add `{_1}`")]
    CannotAdd(#[primary_label("cannot add")] Span<'arena>, Type<'arena>),
    #[error("Cannot subtract `{_1}`")]
    CannotSub(
        #[primary_label("cannot subtract")] Span<'arena>,
        Type<'arena>,
    ),
    #[error("Cannot multiply `{_1}`")]
    CannotMul(
        #[primary_label("cannot multiple")] Span<'arena>,
        Type<'arena>,
    ),
    #[error("Cannot divide `{_1}`")]
    CannotDiv(#[primary_label("cannot divide")] Span<'arena>, Type<'arena>),
    #[error("Cannot take the remainder of `{_1}`")]
    CannotMod(
        #[primary_label("cannot take the remainder")] Span<'arena>,
        Type<'arena>,
    ),
    #[error("Cannot binary and `{_1}`")]
    CannotBAnd(
        #[primary_label("cannot binary and")] Span<'arena>,
        Type<'arena>,
    ),
    #[error("Cannot binary or `{_1}`")]
    CannotBOr(
        #[primary_label("cannot binary or")] Span<'arena>,
        Type<'arena>,
    ),
    #[error("Cannot xor `{_1}`")]
    CannotBXor(
        #[primary_label("cannot binary xor")] Span<'arena>,
        Type<'arena>,
    ),
    #[error("Cannot and `{_1}`")]
    CannotLAnd(
        #[primary_label("cannot logically and")] Span<'arena>,
        Type<'arena>,
    ),
    #[error("Cannot or `{_1}`")]
    CannotLOr(
        #[primary_label("cannot logically or")] Span<'arena>,
        Type<'arena>,
    ),
    #[error("Cannot compare `{_1}`")]
    CannotCompare(
        #[primary_label("cannot compare")] Span<'arena>,
        Type<'arena>,
    ),
    #[error("Cannot compare the equality of `{_1}`")]
    CannotEq(#[primary_label("cannot equate")] Span<'arena>, Type<'arena>),
    #[error("Cannot shift `{_1}` left")]
    CannotShl(
        #[primary_label("cannot leftshit")] Span<'arena>,
        Type<'arena>,
    ),
    #[error("Cannot shift `{_1}` right")]
    CannotShr(
        #[primary_label("cannot right shift")] Span<'arena>,
        Type<'arena>,
    ),
    #[error("Expected both sides of the expression to be the same, lhs: `{_1}`, rhs: `{_2}`")]
    LhsNotRhs(
        #[primary_label("the left and right side don't match")] Span<'arena>,
        Type<'arena>,
        Type<'arena>,
    ),
    #[error("Cannot negate a `{_1}`")]
    CannotNeg(#[primary_label("cannot negate")] Span<'arena>, Type<'arena>),
    #[error("Cannot use unary `+` on a `{_1}`")]
    CannotPos(
        #[primary_label("cannot use unary `+`")] Span<'arena>,
        Type<'arena>,
    ),
    #[error("Cannot not a `{_1}`")]
    CannotLNot(#[primary_label("cannot not")] Span<'arena>, Type<'arena>),
    #[error("Cannot invert a `{_1}`")]
    CannotBNot(#[primary_label("cannot invert")] Span<'arena>, Type<'arena>),
    #[error("cannot dereference a `{_1}`")]
    CannotDeref(
        #[primary_label("cannot dereference")] Span<'arena>,
        Type<'arena>,
    ),
    #[error("could not find export `{name}`")]
    ExportNotFound {
        #[primary_label("No such export found")]
        location: Span<'arena>,
        name: Symbol<'arena>,
    },
    #[error("cyclic dependency detected")]
    CyclicDependency {
        #[primary_label("cyclic dependency")]
        location: Span<'arena>,
    },
    #[error("Unbound identifier `{name}`")]
    UnboundIdent {
        #[primary_label("unbound identifier")]
        location: Span<'arena>,
        name: Symbol<'arena>,
    },
    // TODO: implement Display for ScopeKind
    #[error("Expected a {expected:?}, but found a {found:?}")]
    MismatchingScopeType {
        #[primary_label("")]
        location: Span<'arena>,
        expected: ScopeKind,
        found: ScopeKind,
    },
    #[error("Recursive type detected")]
    RecursiveTypeDetected {
        #[primary_label("recursive type")]
        location: Span<'arena>,
    },
    #[error("The function does not always return but is also not void")]
    BodyDoesNotAlwaysReturn {
        #[primary_label("Function sometimes returns void")]
        location: Span<'arena>,
    },
    #[error("Expected {expected}, but found {found}")]
    MismatchingType {
        #[primary_label("types don't match")]
        location: Span<'arena>,
        expected: Type<'arena>,
        found: Type<'arena>,
    },
    #[error("`{name}` is not a struct type")]
    IdentifierIsNotStruct {
        #[primary_label("Tried to construct a non-struct type as a struct")]
        location: Span<'arena>,
        name: Symbol<'arena>,
    },
    #[error("no such field named `{name}` found!")]
    NoSuchFieldFound {
        #[primary_label("no such field found")]
        location: Span<'arena>,
        name: Symbol<'arena>,
    },
    #[error("missing field `{name}`")]
    MissingField {
        #[primary_label("missing `{name}`")]
        location: Span<'arena>,
        name: Symbol<'arena>,
    },
    #[error("Expected a function")]
    TypeIsNotAFunction {
        #[primary_label("expected a function")]
        location: Span<'arena>,
    },
    #[error("Function misses arguments")]
    MissingArguments {
        #[primary_label("too little arguments supplied")]
        location: Span<'arena>,
    },
    #[error("Function expects no more arguments")]
    TooManyArguments {
        #[primary_label("Last expected argument")]
        location: Span<'arena>,
    },
    #[error("A module cannot have generics")]
    UnexpectedGenerics {
        #[primary_label("generics not allowed")]
        location: Span<'arena>,
    },
    #[error("`{name}` is not a member of the trait.")]
    IsNotTraitMember {
        #[primary_label("No such method exists on the trait")]
        location: Span<'arena>,
        name: Symbol<'arena>,
    },
    #[error("missing trait item `{name}`")]
    MissingTraitItem {
        #[primary_label("Missing implementation for trait item")]
        location: Span<'arena>,
        name: Symbol<'arena>,
    },
    #[error("Type {_1} is expected to implement the traits {_2:?}")]
    MismatchingTraits(
        #[primary_label("Trait is missing dependency traits")] Span<'arena>,
        Type<'arena>,
        Vec<Symbol<'arena>>,
    ),
    #[error(
        "Expected {}, but found {}",
        FunctionList(expected),
        FunctionList(found)
    )]
    MismatchingArguments {
        #[primary_label("Mismatching arguments")]
        location: Span<'arena>,
        expected: Vec<Type<'arena>>,
        found: Vec<Type<'arena>>,
    },
    #[error("Expected fn(...) -> {expected} but fund fn(...) -> {found}")]
    MismatchingReturnType {
        #[primary_label("Mismatching return type")]
        location: Span<'arena>,
        expected: Type<'arena>,
        found: Type<'arena>,
    },
}
