use std::{
    borrow::Cow,
    fmt::{Display, Write},
};

use crate::{annotations::Annotations, store::StoreKey, typechecking::types::FunctionType};
use mira_spans::{interner::InternedStr, Span};

use super::{
    intrinsics::Intrinsic,
    typechecking::{ScopeTypeMetadata, ScopeValueId},
    types::Type,
    TypecheckingContext, TypedExternalFunction, TypedFunction, TypedStatic, TypedStruct,
    TypedTrait,
};

#[derive(Debug, Clone)]
pub enum TypedLiteral<'arena> {
    Void,
    Dynamic(ScopeValueId),
    Function(StoreKey<TypedFunction<'arena>>, Vec<Type<'arena>>),
    ExternalFunction(StoreKey<TypedExternalFunction<'arena>>),
    Static(StoreKey<TypedStatic<'arena>>),
    String(InternedStr<'arena>),
    Array(Type<'arena>, Vec<TypedLiteral<'arena>>),
    ArrayInit(Type<'arena>, Box<TypedLiteral<'arena>>, usize),
    Struct(StoreKey<TypedStruct<'arena>>, Vec<TypedLiteral<'arena>>),
    Tuple(Vec<TypedLiteral<'arena>>),
    F64(f64),
    F32(f32),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    USize(usize),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    ISize(isize),
    Bool(bool),
    Intrinsic(Intrinsic, Vec<Type<'arena>>),
}

impl<'arena> TypedLiteral<'arena> {
    pub fn to_type<'a>(
        &self,
        scope: &'a [(Type<'arena>, ScopeTypeMetadata)],
        ctx: &TypecheckingContext<'arena>,
    ) -> Cow<'a, Type<'arena>> {
        match self {
            TypedLiteral::Void => Cow::Owned(Type::PrimitiveVoid(0)),
            TypedLiteral::Dynamic(id) => Cow::Borrowed(&scope[*id].0),
            TypedLiteral::Function(id, _) => {
                let contract = &ctx.functions.read()[*id].0;
                let fn_type = FunctionType {
                    return_type: contract.return_type.clone(),
                    arguments: contract.arguments.iter().map(|v| v.1.clone()).collect(),
                };
                Cow::Owned(Type::Function(fn_type.into(), 0))
            }
            TypedLiteral::ExternalFunction(id) => {
                let contract = &ctx.external_functions.read()[*id].0;
                let fn_type = FunctionType {
                    return_type: contract.return_type.clone(),
                    arguments: contract.arguments.iter().map(|v| v.1.clone()).collect(),
                };
                Cow::Owned(Type::Function(fn_type.into(), 0))
            }
            TypedLiteral::Static(id) => Cow::Owned(ctx.statics.read()[*id].type_.clone()),
            TypedLiteral::String(_) => Cow::Owned(Type::PrimitiveStr(1)),
            TypedLiteral::Array(ty, elems) => Cow::Owned(Type::SizedArray {
                typ: ty.clone().into(),
                num_references: 0,
                number_elements: elems.len(),
            }),
            TypedLiteral::ArrayInit(ty, _, elems) => Cow::Owned(Type::SizedArray {
                typ: ty.clone().into(),
                num_references: 0,
                number_elements: *elems,
            }),
            TypedLiteral::Struct(struct_id, _) => Cow::Owned(Type::Struct {
                struct_id: *struct_id,
                name: ctx.structs.read()[*struct_id].name,
                num_references: 0,
            }),
            TypedLiteral::Tuple(elems) => Cow::Owned(Type::Tuple {
                elements: elems
                    .iter()
                    .map(|v| v.to_type(scope, ctx).into_owned())
                    .collect(),
                num_references: 0,
            }),
            TypedLiteral::F64(_) => Cow::Owned(Type::PrimitiveF64(0)),
            TypedLiteral::F32(_) => Cow::Owned(Type::PrimitiveF32(0)),
            TypedLiteral::U8(_) => Cow::Owned(Type::PrimitiveU8(0)),
            TypedLiteral::U16(_) => Cow::Owned(Type::PrimitiveU16(0)),
            TypedLiteral::U32(_) => Cow::Owned(Type::PrimitiveU32(0)),
            TypedLiteral::U64(_) => Cow::Owned(Type::PrimitiveU64(0)),
            TypedLiteral::USize(_) => Cow::Owned(Type::PrimitiveUSize(0)),
            TypedLiteral::I8(_) => Cow::Owned(Type::PrimitiveI8(0)),
            TypedLiteral::I16(_) => Cow::Owned(Type::PrimitiveI16(0)),
            TypedLiteral::I32(_) => Cow::Owned(Type::PrimitiveI32(0)),
            TypedLiteral::I64(_) => Cow::Owned(Type::PrimitiveI64(0)),
            TypedLiteral::ISize(_) => Cow::Owned(Type::PrimitiveISize(0)),
            TypedLiteral::Bool(_) => Cow::Owned(Type::PrimitiveBool(0)),
            TypedLiteral::Intrinsic(..) => panic!("intrinsic is no type"),
        }
    }

    pub fn to_primitive_type(
        &self,
        scope: &[(Type<'arena>, ScopeTypeMetadata)],
        ctx: &TypecheckingContext<'arena>,
    ) -> Option<Type<'arena>> {
        match self {
            TypedLiteral::Void => Some(Type::PrimitiveVoid(0)),
            TypedLiteral::String(_) => Some(Type::PrimitiveStr(1)),
            TypedLiteral::F64(_) => Some(Type::PrimitiveF64(0)),
            TypedLiteral::F32(_) => Some(Type::PrimitiveF32(0)),
            TypedLiteral::U8(_) => Some(Type::PrimitiveU8(0)),
            TypedLiteral::U16(_) => Some(Type::PrimitiveU16(0)),
            TypedLiteral::U32(_) => Some(Type::PrimitiveU32(0)),
            TypedLiteral::U64(_) => Some(Type::PrimitiveU64(0)),
            TypedLiteral::USize(_) => Some(Type::PrimitiveUSize(0)),
            TypedLiteral::I8(_) => Some(Type::PrimitiveI8(0)),
            TypedLiteral::I16(_) => Some(Type::PrimitiveI16(0)),
            TypedLiteral::I32(_) => Some(Type::PrimitiveI32(0)),
            TypedLiteral::I64(_) => Some(Type::PrimitiveI64(0)),
            TypedLiteral::ISize(_) => Some(Type::PrimitiveISize(0)),
            TypedLiteral::Bool(_) => Some(Type::PrimitiveBool(0)),
            TypedLiteral::Dynamic(id) => {
                let ty = &scope[*id].0;
                ty.is_primitive().then(|| ty.clone())
            }
            TypedLiteral::Static(id) => {
                let ty = &ctx.statics.read()[*id].type_;
                ty.is_primitive().then(|| ty.clone())
            }
            TypedLiteral::Array(..)
            | TypedLiteral::ArrayInit(..)
            | TypedLiteral::Struct(..)
            | TypedLiteral::Tuple(..)
            | TypedLiteral::Function(..)
            | TypedLiteral::Intrinsic(..)
            | TypedLiteral::ExternalFunction(..) => None,
        }
    }

    pub fn is_entirely_literal(&self) -> bool {
        match self {
            TypedLiteral::Array(_, vec)
            | TypedLiteral::Struct(_, vec)
            | TypedLiteral::Tuple(vec) => vec
                .iter()
                .map(TypedLiteral::is_entirely_literal)
                .fold(true, std::ops::BitAnd::bitand),
            TypedLiteral::ArrayInit(_, elem, amount) => *amount == 0 || elem.is_entirely_literal(),
            TypedLiteral::Static(_) | TypedLiteral::Dynamic(_) | TypedLiteral::Intrinsic(..) => {
                false
            }
            TypedLiteral::String(_)
            | TypedLiteral::Function(..)
            | TypedLiteral::ExternalFunction(..)
            | TypedLiteral::Void
            | TypedLiteral::F64(_)
            | TypedLiteral::F32(_)
            | TypedLiteral::U8(_)
            | TypedLiteral::U16(_)
            | TypedLiteral::U32(_)
            | TypedLiteral::U64(_)
            | TypedLiteral::USize(_)
            | TypedLiteral::I8(_)
            | TypedLiteral::I16(_)
            | TypedLiteral::I32(_)
            | TypedLiteral::I64(_)
            | TypedLiteral::ISize(_)
            | TypedLiteral::Bool(_) => true,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum OffsetValue {
    // its guaranteed the type of this is usize.
    Dynamic(ScopeValueId),
    Static(usize),
}

impl Display for OffsetValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OffsetValue::Dynamic(id) => {
                f.write_char('_')?;
                Display::fmt(id, f)
            }
            OffsetValue::Static(v) => Display::fmt(v, f),
        }
    }
}

#[derive(Debug)]
pub enum TypecheckedExpression<'arena> {
    Return(Span<'arena>, TypedLiteral<'arena>),
    Block(
        Span<'arena>,
        Box<[TypecheckedExpression<'arena>]>,
        Annotations<'arena>,
    ),
    If {
        span: Span<'arena>,
        cond: TypedLiteral<'arena>,
        if_block: (Box<[TypecheckedExpression<'arena>]>, Span<'arena>),
        else_block: Option<(Box<[TypecheckedExpression<'arena>]>, Span<'arena>)>,
        annotations: Annotations<'arena>,
    },
    While {
        span: Span<'arena>,
        cond_block: Box<[TypecheckedExpression<'arena>]>,
        cond: TypedLiteral<'arena>,
        body: (Box<[TypecheckedExpression<'arena>]>, Span<'arena>),
    },

    // _dst = _lhs..=_rhs
    // _dst = _lhs.._rhs
    Range {
        span: Span<'arena>,
        typ: Type<'arena>,
        lhs: TypedLiteral<'arena>,
        rhs: TypedLiteral<'arena>,
        inclusive: bool,
        dst: ScopeValueId,
    },
    // _1 = asm(...)
    Asm {
        span: Span<'arena>,
        dst: ScopeValueId,
        inputs: Vec<ScopeValueId>,
        registers: String,
        volatile: bool,
        asm: String,
    },
    // *_1 = _2
    StoreAssignment(Span<'arena>, TypedLiteral<'arena>, TypedLiteral<'arena>),
    // _1 = _2(_3.1, _3.2, d, ...)
    Call(
        Span<'arena>,
        ScopeValueId,
        TypedLiteral<'arena>,
        Vec<TypedLiteral<'arena>>,
    ),
    // _1 = func(_3.1, _3.2, d, ...)
    DirectCall(
        Span<'arena>,
        ScopeValueId,
        StoreKey<TypedFunction<'arena>>,
        Vec<TypedLiteral<'arena>>,
        Vec<Type<'arena>>,
    ),
    // _1 = func(_3.1, _3.2, d, ...)
    DirectExternCall(
        Span<'arena>,
        ScopeValueId,
        StoreKey<TypedExternalFunction<'arena>>,
        Vec<TypedLiteral<'arena>>,
    ),
    // _1 = intrinsic(_3.1, _3.2)
    IntrinsicCall(
        Span<'arena>,
        ScopeValueId,
        Intrinsic,
        Vec<TypedLiteral<'arena>>,
        Vec<Type<'arena>>,
    ),
    // _1 = +_2
    Pos(Span<'arena>, ScopeValueId, TypedLiteral<'arena>),
    // _1 = -_2
    Neg(Span<'arena>, ScopeValueId, TypedLiteral<'arena>),
    // _1 = !_2
    LNot(Span<'arena>, ScopeValueId, TypedLiteral<'arena>),
    // _1 = ~_2
    BNot(Span<'arena>, ScopeValueId, TypedLiteral<'arena>),
    // _1 = _2 + _3
    Add(
        Span<'arena>,
        ScopeValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _2 - _3
    Sub(
        Span<'arena>,
        ScopeValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _2 * _3
    Mul(
        Span<'arena>,
        ScopeValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _2 / _3
    Div(
        Span<'arena>,
        ScopeValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _2 % _3
    Mod(
        Span<'arena>,
        ScopeValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _2 & _3
    BAnd(
        Span<'arena>,
        ScopeValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _2 | _3
    BOr(
        Span<'arena>,
        ScopeValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _2 ^ _3
    BXor(
        Span<'arena>,
        ScopeValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _1 > _2
    GreaterThan(
        Span<'arena>,
        ScopeValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _1 > _2
    LessThan(
        Span<'arena>,
        ScopeValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _1 && _2
    LAnd(
        Span<'arena>,
        ScopeValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _1 || _2
    LOr(
        Span<'arena>,
        ScopeValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _1 >= _2
    GreaterThanEq(
        Span<'arena>,
        ScopeValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _1 <= _2
    LessThanEq(
        Span<'arena>,
        ScopeValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _1 == _2
    Eq(
        Span<'arena>,
        ScopeValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _1 != _2
    Neq(
        Span<'arena>,
        ScopeValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _1 << _2
    LShift(
        Span<'arena>,
        ScopeValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _1 >> _2
    RShift(
        Span<'arena>,
        ScopeValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = &_1
    //                                v- guaranteed to either be `Dynamic`, `Static` or `Void`
    Reference(Span<'arena>, ScopeValueId, TypedLiteral<'arena>),
    // _1 = *_1
    Dereference(Span<'arena>, ScopeValueId, TypedLiteral<'arena>),
    // NOTE: the indexes into structs will be turned into their respective index
    // e.g. on a struct { a: i32, b: i32 }, a `.a` will be turned into 0 and a `.b` into a 1.
    // _1 = &(*_2).a[3].c.d; This is required, because we will offset the _2 pointer by the required
    // offsets
    Offset(
        Span<'arena>,
        ScopeValueId,
        TypedLiteral<'arena>,
        OffsetValue,
    ),
    // NOTE: the indexes into structs will be turned into their respective index
    // e.g. on a struct { a: i32, b: i32 }, a `.a` will be turned into 0 and a `.b` into a 1.
    // _1 = _2.a.b.c.d
    OffsetNonPointer(Span<'arena>, ScopeValueId, TypedLiteral<'arena>, usize),
    // Eq::val(&dyn Eq, ...)
    // The last value is the offset into the function pointer part of the vtable.
    DynCall(Span<'arena>, ScopeValueId, Vec<TypedLiteral<'arena>>, u32),
    // let _1 = <literal>; This **should never** contain a TypedLiteral::Dynamic as its 3rd element.
    Literal(Span<'arena>, ScopeValueId, TypedLiteral<'arena>),
    DeclareVariable(
        Span<'arena>,
        ScopeValueId,
        Type<'arena>,
        InternedStr<'arena>,
    ),
    Empty(Span<'arena>),
    Unreachable(Span<'arena>),
    // ### CASTS ###
    // NOTE: All casts copy the value.
    //
    // Does not change the value. Used when 2 types are represented the same in
    // llvm ir but differently in the type system.
    // Examples: &str to &[u8]
    // &[T] to (&T, usize)
    // &T to &[T; 1]
    // &A to &B
    Alias(Span<'arena>, ScopeValueId, TypedLiteral<'arena>),
    // casting u_ to i_ (for integers of the same size)
    Bitcast(Span<'arena>, ScopeValueId, TypedLiteral<'arena>),
    // i_ or u_ or f_ or bool to i_ or u_ or f_ or bool
    IntCast(Span<'arena>, ScopeValueId, TypedLiteral<'arena>),
    // casts &void to usize (only valid for thin pointers)
    PtrToInt(Span<'arena>, ScopeValueId, TypedLiteral<'arena>),
    // casts usize to &void (only valid for thin pointers)
    IntToPtr(Span<'arena>, ScopeValueId, TypedLiteral<'arena>),
    // Strips the metadata from a fat pointer, &[T] to &T
    StripMetadata(Span<'arena>, ScopeValueId, TypedLiteral<'arena>),
    // _2: &[_; _3]
    // let _1 = attach_metadata(_2, _3)
    MakeUnsizedSlice(Span<'arena>, ScopeValueId, TypedLiteral<'arena>, usize),
    // _2: &<value>
    // let _1 = attach_vtable(_2, trait_1, trait_2)
    AttachVtable(
        Span<'arena>,
        ScopeValueId,
        TypedLiteral<'arena>,
        (Type<'arena>, Vec<StoreKey<TypedTrait<'arena>>>),
    ),
    None,
}

impl<'arena> TypecheckedExpression<'arena> {
    pub fn span(&self) -> Span<'arena> {
        match self {
            TypecheckedExpression::Range { span, .. }
            | TypecheckedExpression::Asm { span, .. }
            | TypecheckedExpression::If { span, .. }
            | TypecheckedExpression::While { span, .. }
            | TypecheckedExpression::AttachVtable(span, ..)
            | TypecheckedExpression::DeclareVariable(span, ..)
            | TypecheckedExpression::IntrinsicCall(span, ..)
            | TypecheckedExpression::DirectCall(span, ..)
            | TypecheckedExpression::DirectExternCall(span, ..)
            | TypecheckedExpression::DynCall(span, ..)
            | TypecheckedExpression::StoreAssignment(span, ..)
            | TypecheckedExpression::OffsetNonPointer(span, ..)
            | TypecheckedExpression::MakeUnsizedSlice(span, ..)
            | TypecheckedExpression::StripMetadata(span, ..)
            | TypecheckedExpression::Bitcast(span, ..)
            | TypecheckedExpression::IntCast(span, ..)
            | TypecheckedExpression::PtrToInt(span, ..)
            | TypecheckedExpression::IntToPtr(span, ..)
            | TypecheckedExpression::Offset(span, ..)
            | TypecheckedExpression::Literal(span, ..)
            | TypecheckedExpression::Call(span, ..)
            | TypecheckedExpression::Pos(span, ..)
            | TypecheckedExpression::Neg(span, ..)
            | TypecheckedExpression::LNot(span, ..)
            | TypecheckedExpression::BNot(span, ..)
            | TypecheckedExpression::Add(span, ..)
            | TypecheckedExpression::Sub(span, ..)
            | TypecheckedExpression::Mul(span, ..)
            | TypecheckedExpression::Div(span, ..)
            | TypecheckedExpression::Mod(span, ..)
            | TypecheckedExpression::BAnd(span, ..)
            | TypecheckedExpression::BOr(span, ..)
            | TypecheckedExpression::BXor(span, ..)
            | TypecheckedExpression::GreaterThan(span, ..)
            | TypecheckedExpression::LessThan(span, ..)
            | TypecheckedExpression::LAnd(span, ..)
            | TypecheckedExpression::LOr(span, ..)
            | TypecheckedExpression::GreaterThanEq(span, ..)
            | TypecheckedExpression::LessThanEq(span, ..)
            | TypecheckedExpression::Eq(span, ..)
            | TypecheckedExpression::Neq(span, ..)
            | TypecheckedExpression::LShift(span, ..)
            | TypecheckedExpression::RShift(span, ..)
            | TypecheckedExpression::Reference(span, ..)
            | TypecheckedExpression::Dereference(span, ..)
            | TypecheckedExpression::Alias(span, ..)
            | TypecheckedExpression::Block(span, ..)
            | TypecheckedExpression::Return(span, ..)
            | TypecheckedExpression::Unreachable(span)
            | TypecheckedExpression::Empty(span) => *span,
            TypecheckedExpression::None => unreachable!("none expression"),
        }
    }
}
