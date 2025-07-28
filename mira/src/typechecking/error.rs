use thiserror::Error;

use crate::{
    error::FunctionList,
    lang_items::{LangItemAssignmentError, LangItemError},
    parser::{Path, PathWithoutGenerics},
};
use mira_spans::{interner::InternedStr, Span};

use super::{types::Type, ScopeKind};

impl<'arena> From<LangItemAssignmentError<'arena>> for TypecheckingError<'arena> {
    fn from(value: LangItemAssignmentError<'arena>) -> Self {
        Self::LangItemAssignment(value)
    }
}
impl<'arena> From<LangItemError<'arena>> for TypecheckingError<'arena> {
    fn from(value: LangItemError<'arena>) -> Self {
        Self::LangItemError(value)
    }
}

#[derive(Clone, Debug, Error)]
pub enum TypecheckingError<'arena> {
    #[error("{0:?}")]
    LangItemError(LangItemError<'arena>),
    #[error("{0:?}")]
    LangItemAssignment(LangItemAssignmentError<'arena>),
    #[error("{0:?}: Expected a sized type, but got an unsized one")]
    UnsizedForSizedGeneric(Span<'arena>, Type<'arena>),
    // NOTE: This is due to each ABI handling how to return values differently. LLVM doesn't handle
    // this for us, but, as far as i can tell, it's fine if we return structs from non-extern
    // functions as that should never produce incorrect behavior. IF how ever, it turns out to,
    // then the "internal" ABI will pass all structs whose size is greather than 128 as an sret
    // first argument (fn(_) -> v turns into fn(&v, _) if sizeof(v) > 128)
    //
    // For further references: Zig's implementation of if something needs to be passed as an sret:
    // https://github.com/ziglang/zig/blob/master/src/codegen/llvm.zig#L12021
    #[error(
        "{0:?}: Extern functions can as of now only return primitive values and thin pointers"
    )]
    InvalidExternReturnType(Span<'arena>),
    #[error("{0:?}: Unsized Type {1} is not a return type")]
    UnsizedReturnType(Span<'arena>, Type<'arena>),
    #[error("{0:?}: Unsized Type {1} is not a valid argument")]
    UnsizedArgument(Span<'arena>, Type<'arena>),
    #[error("{0:?}: Function {1} on trait {2} is not valid for &dyn {2} types")]
    InvalidDynTypeFunc(Span<'arena>, InternedStr<'arena>, InternedStr<'arena>),
    #[error("{0:?}: Cannot find trait {1}")]
    CannotFindTrait(Span<'arena>, PathWithoutGenerics<'arena>),
    #[error("{0:?}: Expected {1} generics, but found {2}")]
    MismatchingGenericCount(Span<'arena>, usize, usize),
    #[error("{0:?}: The size of {1} needs to be known at compiletime")]
    NonSizedType(Span<'arena>, Type<'arena>),
    #[error("{0:?}: Intrinsics accepts only integers, supplied: {1}")]
    IntOnlyIntrinsic(Span<'arena>, Type<'arena>),
    #[error("{0:?}: Assembly instructions can only accept numeric types (i_, u_, f_ and bool). Specified Type: `{1}`")]
    AsmNonNumericTypeResolved(Span<'arena>, Type<'arena>),
    #[error("{0:?}: Assembly instructions can only accept numeric types (i_, u_, f_ and bool). Specified Type: `{1}`")]
    AsmNonNumericType(Span<'arena>, InternedStr<'arena>),
    #[error("{0:?}: Tuple only has {1} fields, but tried to get field {2}")]
    TupleIndexOutOfBounds(Span<'arena>, usize, usize),
    #[error("{0:?}: Cannot index a tuple with a dynamic value")]
    TupleDynamicIndex(Span<'arena>),
    #[error("{0:?}: Cannot infer type for anonymous struct")]
    CannotInferAnonStructType(Span<'arena>),
    #[error("{0:?}: Statics can only have literal values")]
    StaticsNeedToBeLiteral(Span<'arena>),
    #[error(
        "{0:?}: Cannot infer array type. Use `[] as [<type>;0]` to explicitly specify the expected array type"
    )]
    CannotInferArrayType(Span<'arena>),
    #[error("{0:?}: Function `{1}` of type `{2}` is not a method as it doesn't have the signature (Self, ...) or (&Self, ...)")]
    NonMemberFunction(Span<'arena>, InternedStr<'arena>, Type<'arena>),
    #[error("{0:?}: Cannot find function `{1}` on type `{2}`")]
    CannotFindFunctionOnType(Span<'arena>, InternedStr<'arena>, Type<'arena>),
    #[error("{0:?}: Cannot find value `{1}`")]
    CannotFindValue(Span<'arena>, Path<'arena>),
    #[error("{0:?}: Tried to access a member of a non-struct value `{1}`")]
    AccessNonStructValue(Span<'arena>, Type<'arena>),
    #[error("{0:?}: Tried to index into non-array type `{1}`")]
    IndexNonArrayElem(Span<'arena>, Type<'arena>),
    #[error("{0:?}: Could not find field `{2}` on `{1}`")]
    FieldNotFound(Span<'arena>, Type<'arena>, InternedStr<'arena>),
    #[error("{0:?}: Invalid cast (lhs: `{1}`, rhs: `{2}`)")]
    DisallowedCast(Span<'arena>, Type<'arena>, Type<'arena>),
    #[error("{0:?}: Cannot assign to this expression")]
    CannotAssign(Span<'arena>),
    #[error("{0:?}: Cannot shift by a non-uint value (found `{1}`)")]
    CannotShiftByNonUInt(Span<'arena>, Type<'arena>),
    #[error("{0:?}: Cannot add `{1}`")]
    CannotAdd(Span<'arena>, Type<'arena>),
    #[error("{0:?}: Cannot subtract `{1}`")]
    CannotSub(Span<'arena>, Type<'arena>),
    #[error("{0:?}: Cannot multiply `{1}`")]
    CannotMul(Span<'arena>, Type<'arena>),
    #[error("{0:?}: Cannot divide `{1}`")]
    CannotDiv(Span<'arena>, Type<'arena>),
    #[error("{0:?}: Cannot take the remainder of `{1}`")]
    CannotMod(Span<'arena>, Type<'arena>),
    #[error("{0:?}: Cannot binary and `{1}`")]
    CannotBAnd(Span<'arena>, Type<'arena>),
    #[error("{0:?}: Cannot binary or `{1}`")]
    CannotBOr(Span<'arena>, Type<'arena>),
    #[error("{0:?}: Cannot xor `{1}`")]
    CannotBXor(Span<'arena>, Type<'arena>),
    #[error("{0:?}: Cannot and `{1}`")]
    CannotLAnd(Span<'arena>, Type<'arena>),
    #[error("{0:?}: Cannot or `{1}`")]
    CannotLOr(Span<'arena>, Type<'arena>),
    #[error("{0:?}: Cannot compare `{1}`")]
    CannotCompare(Span<'arena>, Type<'arena>),
    #[error("{0:?}: Cannot compare the equality of `{1}`")]
    CannotEq(Span<'arena>, Type<'arena>),
    #[error("{0:?}: Cannot shift `{1}` left")]
    CannotShl(Span<'arena>, Type<'arena>),
    #[error("{0:?}: Cannot shift `{1}` right")]
    CannotShr(Span<'arena>, Type<'arena>),
    #[error("{0:?}: Expected both sides of the expression to be the same, lhs: `{1}`, rhs: `{2}`")]
    LhsNotRhs(Span<'arena>, Type<'arena>, Type<'arena>),
    #[error("{0:?}: Cannot negate a `{1}`")]
    CannotNeg(Span<'arena>, Type<'arena>),
    #[error("{0:?}: Cannot use unary `+` on a `{1}`")]
    CannotPos(Span<'arena>, Type<'arena>),
    #[error("{0:?}: Cannot not a `{1}`")]
    CannotLNot(Span<'arena>, Type<'arena>),
    #[error("{0:?}: Cannot invert a `{1}`")]
    CannotBNot(Span<'arena>, Type<'arena>),
    #[error("{0:?}: cannot dereference a `{1}`")]
    CannotDeref(Span<'arena>, Type<'arena>),
    #[error("{location:?}: could not find export `{name}`")]
    ExportNotFound {
        location: Span<'arena>,
        name: InternedStr<'arena>,
    },
    #[error("{location:?}: cyclic dependency detected")]
    CyclicDependency { location: Span<'arena> },
    #[error("{location:?}: Unbound identifier `{name}`")]
    UnboundIdent {
        location: Span<'arena>,
        name: InternedStr<'arena>,
    },
    #[error("{location:?}: Expected a {expected:?}, but found a {found:?}")]
    MismatchingScopeType {
        location: Span<'arena>,
        expected: ScopeKind,
        found: ScopeKind,
    },
    #[error("{location:?}: Recursive type detected")]
    RecursiveTypeDetected { location: Span<'arena> },
    #[error("{location:?}: Body does not always return")]
    BodyDoesNotAlwaysReturn { location: Span<'arena> },
    #[error("{location:?}: Expected {expected}, but found {found}")]
    MismatchingType {
        expected: Type<'arena>,
        found: Type<'arena>,
        location: Span<'arena>,
    },
    #[error("{location:?}: Function pointers can't have generics")]
    GenericFunctionPointer { location: Span<'arena> },
    #[error("{location:?}: `{name}` is not a struct type")]
    IdentifierIsNotStruct {
        location: Span<'arena>,
        name: InternedStr<'arena>,
    },
    #[error("{location:?}: no such field named `{name}` found!")]
    NoSuchFieldFound {
        location: Span<'arena>,
        name: InternedStr<'arena>,
    },
    #[error("{location:?}: missing field `{name}`")]
    MissingField {
        location: Span<'arena>,
        name: InternedStr<'arena>,
    },
    #[error("{location:?}: Expected a function")]
    TypeIsNotAFunction { location: Span<'arena> },
    #[error("{location:?}: Function misses arguments")]
    MissingArguments { location: Span<'arena> },
    #[error("{location:?}: Function expects no more arguments")]
    TooManyArguments { location: Span<'arena> },
    #[error("{location:?}: Did not expected a generic here.")]
    UnexpectedGenerics { location: Span<'arena> },
    #[error("{location:?}: `{name}` is not a member of the trait.")]
    IsNotTraitMember {
        location: Span<'arena>,
        name: InternedStr<'arena>,
    },
    #[error("{location:?}: missing trait item `{name}`")]
    MissingTraitItem {
        location: Span<'arena>,
        name: InternedStr<'arena>,
    },
    #[error("{0:?}: Type {1} is expected to implement the traits {2:?}")]
    MismatchingTraits(Span<'arena>, Type<'arena>, Vec<InternedStr<'arena>>),
    #[error("{location:?}: Expected {}, but found {}", FunctionList(.expected), FunctionList(.found))]
    MismatchingArguments {
        location: Span<'arena>,
        expected: Vec<Type<'arena>>,
        found: Vec<Type<'arena>>,
    },
    #[error("{location:?}: Expected fn(...) -> {expected} but fund fn(...) -> {found}")]
    MismatchingReturnType {
        location: Span<'arena>,
        expected: Type<'arena>,
        found: Type<'arena>,
    },
}
