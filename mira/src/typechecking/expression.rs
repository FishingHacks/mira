use std::{
    borrow::Cow,
    fmt::{Display, Write},
};

use crate::{
    annotations::Annotations,
    globals::GlobalStr,
    module::{ExternalFunctionId, FunctionId, StaticId, StructId, TraitId},
    tokenizer::Location,
    typechecking::types::FunctionType,
};

use super::{
    intrinsics::Intrinsic,
    typechecking::{ScopeTypeMetadata, ScopeValueId},
    types::Type,
    TypecheckingContext,
};

#[derive(Debug, Clone)]
pub enum TypedLiteral {
    Void,
    Dynamic(ScopeValueId),
    Function(FunctionId),
    ExternalFunction(ExternalFunctionId),
    Static(StaticId),
    String(GlobalStr),
    Array(Type, Vec<TypedLiteral>),
    Struct(StructId, Vec<TypedLiteral>),
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
    Intrinsic(Intrinsic),
}

impl TypedLiteral {
    pub fn to_type<'a>(
        &self,
        scope: &'a [(Type, ScopeTypeMetadata)],
        ctx: &TypecheckingContext,
    ) -> Cow<'a, Type> {
        match self {
            TypedLiteral::Void => Cow::Owned(Type::PrimitiveVoid(0)),
            TypedLiteral::Dynamic(id) => Cow::Borrowed(&scope[*id].0),
            TypedLiteral::Function(id) | TypedLiteral::ExternalFunction(id) => {
                let function_reader = ctx.functions.read();
                let ext_function_reader = ctx.external_functions.read();
                let contract = if matches!(self, TypedLiteral::Function(_)) {
                    &function_reader[*id].0
                } else {
                    &ext_function_reader[*id].0
                };
                let fn_type = FunctionType {
                    return_type: contract.return_type.clone(),
                    arguments: contract.arguments.iter().map(|v| v.1.clone()).collect(),
                };
                Cow::Owned(Type::Function(fn_type.into(), 0))
            }
            TypedLiteral::Static(id) => Cow::Owned(ctx.statics.read()[*id].0.clone()),
            TypedLiteral::String(_) => Cow::Owned(Type::PrimitiveStr(1)),
            TypedLiteral::Array(ty, elems) => Cow::Owned(Type::SizedArray {
                typ: ty.clone().into(),
                num_references: 0,
                number_elements: elems.len(),
            }),
            TypedLiteral::Struct(struct_id, _) => Cow::Owned(Type::Struct {
                struct_id: *struct_id,
                name: ctx.structs.read()[*struct_id].name.clone(),
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
            TypedLiteral::Intrinsic(intrinsic) => panic!("intrinsic is no type"),
        }
    }

    pub fn is_entirely_literal(&self) -> bool {
        match self {
            TypedLiteral::Array(_, vec) | TypedLiteral::Struct(_, vec) => vec
                .iter()
                .map(TypedLiteral::is_entirely_literal)
                .fold(true, std::ops::BitAnd::bitand),
            TypedLiteral::Static(_) | TypedLiteral::Dynamic(_) | TypedLiteral::Intrinsic(_) => {
                false
            }
            TypedLiteral::String(_)
            | TypedLiteral::Function(_)
            | TypedLiteral::ExternalFunction(_)
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
pub enum TypecheckedExpression {
    Return(Location, TypedLiteral),
    Block(Location, Box<[TypecheckedExpression]>, Annotations),
    If {
        loc: Location,
        cond: TypedLiteral,
        if_block: Box<[TypecheckedExpression]>,
        else_block: Option<Box<[TypecheckedExpression]>>,
        annotations: Annotations,
    },
    While {
        loc: Location,
        cond_block: Box<[TypecheckedExpression]>,
        cond: TypedLiteral,
        body: Box<[TypecheckedExpression]>,
    },

    // _dst = _lhs..=_rhs
    // _dst = _lhs.._rhs
    Range {
        location: Location,
        typ: Type,
        lhs: TypedLiteral,
        rhs: TypedLiteral,
        inclusive: bool,
        dst: ScopeValueId,
    },
    // *_1 = _2
    StoreAssignment(Location, TypedLiteral, TypedLiteral),
    // _1 = _2(_3.1, _3.2, d, ...)
    Call(Location, ScopeValueId, TypedLiteral, Vec<TypedLiteral>),
    // _1 = func(_3.1, _3.2, d, ...)
    DirectCall(Location, ScopeValueId, FunctionId, Vec<TypedLiteral>),
    // _1 = func(_3.1, _3.2, d, ...)
    DirectExternCall(
        Location,
        ScopeValueId,
        ExternalFunctionId,
        Vec<TypedLiteral>,
    ),
    // _1 = intrinsic(_3.1, _3.2)
    IntrinsicCall(Location, ScopeValueId, Intrinsic, Vec<TypedLiteral>),
    // _1 = +_2
    Pos(Location, ScopeValueId, TypedLiteral),
    // _1 = -_2
    Neg(Location, ScopeValueId, TypedLiteral),
    // _1 = !_2
    LNot(Location, ScopeValueId, TypedLiteral),
    // _1 = ~_2
    BNot(Location, ScopeValueId, TypedLiteral),
    // _1 = _2 + _3
    Add(Location, ScopeValueId, TypedLiteral, TypedLiteral),
    // _1 = _2 - _3
    Sub(Location, ScopeValueId, TypedLiteral, TypedLiteral),
    // _1 = _2 * _3
    Mul(Location, ScopeValueId, TypedLiteral, TypedLiteral),
    // _1 = _2 / _3
    Div(Location, ScopeValueId, TypedLiteral, TypedLiteral),
    // _1 = _2 % _3
    Mod(Location, ScopeValueId, TypedLiteral, TypedLiteral),
    // _1 = _2 & _3
    BAnd(Location, ScopeValueId, TypedLiteral, TypedLiteral),
    // _1 = _2 | _3
    BOr(Location, ScopeValueId, TypedLiteral, TypedLiteral),
    // _1 = _2 ^ _3
    BXor(Location, ScopeValueId, TypedLiteral, TypedLiteral),
    // _1 = _1 > _2
    GreaterThan(Location, ScopeValueId, TypedLiteral, TypedLiteral),
    // _1 = _1 > _2
    LessThan(Location, ScopeValueId, TypedLiteral, TypedLiteral),
    // _1 = _1 && _2
    LAnd(Location, ScopeValueId, TypedLiteral, TypedLiteral),
    // _1 = _1 || _2
    LOr(Location, ScopeValueId, TypedLiteral, TypedLiteral),
    // _1 = _1 >= _2
    GreaterThanEq(Location, ScopeValueId, TypedLiteral, TypedLiteral),
    // _1 = _1 <= _2
    LessThanEq(Location, ScopeValueId, TypedLiteral, TypedLiteral),
    // _1 = _1 == _2
    Eq(Location, ScopeValueId, TypedLiteral, TypedLiteral),
    // _1 = _1 != _2
    Neq(Location, ScopeValueId, TypedLiteral, TypedLiteral),
    // _1 = _1 << _2
    LShift(Location, ScopeValueId, TypedLiteral, TypedLiteral),
    // _1 = _1 >> _2
    RShift(Location, ScopeValueId, TypedLiteral, TypedLiteral),
    // _1 = &_1
    //                                v- guaranteed to either be `Dynamic`, `Static` or `Void`
    Reference(Location, ScopeValueId, TypedLiteral),
    // _1 = *_1
    Dereference(Location, ScopeValueId, TypedLiteral),
    // NOTE: the indexes into structs will be turned into their respective index
    // e.g. on a struct { a: i32, b: i32 }, a `.a` will be turned into 0 and a `.b` into a 1.
    // _1 = &(*_2).a[3].c.d; This is required, because we will offset the _2 pointer by the required
    // offsets
    Offset(Location, ScopeValueId, TypedLiteral, OffsetValue),
    // NOTE: the indexes into structs will be turned into their respective index
    // e.g. on a struct { a: i32, b: i32 }, a `.a` will be turned into 0 and a `.b` into a 1.
    // _1 = _2.a.b.c.d
    OffsetNonPointer(Location, ScopeValueId, TypedLiteral, usize),
    // _1 = Eq::eq(_2)
    // TODO: do this
    TraitCall(Location, ScopeValueId, TypedLiteral, TraitId, GlobalStr),
    // _1 = _2
    // used for casts, `_1 = (_2 as T)` results in `_1 = _2`.
    Alias(Location, ScopeValueId, TypedLiteral),
    // let _1 = <literal>; This **should never** contain a TypedLiteral::Dynamic as its 3rd element.
    Literal(Location, ScopeValueId, TypedLiteral),
    // let _1 = attach_metadata(_2, _3)
    // _2: &[_; _3]
    MakeUnsizedSlice(Location, ScopeValueId, TypedLiteral, usize),
    Empty(Location),
    None,
}

impl TypecheckedExpression {
    pub fn location(&self) -> &Location {
        match self {
            TypecheckedExpression::Range { location, .. }
            | TypecheckedExpression::If { loc: location, .. }
            | TypecheckedExpression::While { loc: location, .. }
            | TypecheckedExpression::IntrinsicCall(location, ..)
            | TypecheckedExpression::DirectCall(location, ..)
            | TypecheckedExpression::DirectExternCall(location, ..)
            | TypecheckedExpression::TraitCall(location, ..)
            | TypecheckedExpression::StoreAssignment(location, ..)
            | TypecheckedExpression::OffsetNonPointer(location, ..)
            | TypecheckedExpression::MakeUnsizedSlice(location, ..)
            | TypecheckedExpression::Offset(location, ..)
            | TypecheckedExpression::Literal(location, ..)
            | TypecheckedExpression::Call(location, ..)
            | TypecheckedExpression::Pos(location, ..)
            | TypecheckedExpression::Neg(location, ..)
            | TypecheckedExpression::LNot(location, ..)
            | TypecheckedExpression::BNot(location, ..)
            | TypecheckedExpression::Add(location, ..)
            | TypecheckedExpression::Sub(location, ..)
            | TypecheckedExpression::Mul(location, ..)
            | TypecheckedExpression::Div(location, ..)
            | TypecheckedExpression::Mod(location, ..)
            | TypecheckedExpression::BAnd(location, ..)
            | TypecheckedExpression::BOr(location, ..)
            | TypecheckedExpression::BXor(location, ..)
            | TypecheckedExpression::GreaterThan(location, ..)
            | TypecheckedExpression::LessThan(location, ..)
            | TypecheckedExpression::LAnd(location, ..)
            | TypecheckedExpression::LOr(location, ..)
            | TypecheckedExpression::GreaterThanEq(location, ..)
            | TypecheckedExpression::LessThanEq(location, ..)
            | TypecheckedExpression::Eq(location, ..)
            | TypecheckedExpression::Neq(location, ..)
            | TypecheckedExpression::LShift(location, ..)
            | TypecheckedExpression::RShift(location, ..)
            | TypecheckedExpression::Reference(location, ..)
            | TypecheckedExpression::Dereference(location, ..)
            | TypecheckedExpression::Alias(location, ..)
            | TypecheckedExpression::Block(location, ..)
            | TypecheckedExpression::Return(location, ..)
            | TypecheckedExpression::Empty(location) => location,
            TypecheckedExpression::None => unreachable!("none expression"),
        }
    }
}
