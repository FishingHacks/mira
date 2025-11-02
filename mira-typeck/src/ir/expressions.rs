use std::fmt::{Display, Write as _};

use mira_parser::{
    annotations::Annotations,
    module::{ExternalFunctionId, FunctionId, TraitId},
    std_annotations::intrinsic::Intrinsic,
};
use mira_spans::{Span, Symbol};

use crate::{
    Ty, TyList,
    ir::{BlockId, ValueId},
};

use super::TypedLiteral;

#[derive(Debug)]
pub enum TypedExpression<'arena> {
    Return(Span<'arena>, Option<ValueId>),
    Block(Span<'arena>, BlockId, Annotations<'arena>),
    If {
        span: Span<'arena>,
        cond: TypedLiteral<'arena>,
        if_block: BlockId,
        else_block: Option<BlockId>,
        annotations: Annotations<'arena>,
    },
    While {
        span: Span<'arena>,
        cond_block: BlockId,
        cond: TypedLiteral<'arena>,
        body: BlockId,
    },

    // _dst = _lhs..=_rhs
    // _dst = _lhs.._rhs
    Range {
        span: Span<'arena>,
        ty: Ty<'arena>,
        lhs: TypedLiteral<'arena>,
        rhs: TypedLiteral<'arena>,
        inclusive: bool,
        dst: ValueId,
    },
    // _1 = asm(...)
    Asm {
        span: Span<'arena>,
        dst: ValueId,
        inputs: Box<[ValueId]>,
        registers: String,
        volatile: bool,
        asm: String,
    },
    // *_1 = _2
    StoreAssignment(Span<'arena>, TypedLiteral<'arena>, TypedLiteral<'arena>),
    // _1 = _2(_3.1, _3.2, d, ...)
    Call(
        Span<'arena>,
        ValueId,
        TypedLiteral<'arena>,
        Box<[TypedLiteral<'arena>]>,
    ),
    // _1 = func(_3.1, _3.2, d, ...)
    DirectCall(
        Span<'arena>,
        ValueId,
        FunctionId,
        Box<[TypedLiteral<'arena>]>,
        TyList<'arena>,
    ),
    // _1 = func(_3.1, _3.2, d, ...)
    DirectExternCall(
        Span<'arena>,
        ValueId,
        ExternalFunctionId,
        Box<[TypedLiteral<'arena>]>,
    ),
    // _1 = intrinsic(_3.1, _3.2, ...)
    IntrinsicCall(
        Span<'arena>,
        ValueId,
        Intrinsic,
        Box<[TypedLiteral<'arena>]>,
        TyList<'arena>,
    ),
    // _1 = llvm_intrinsic._2(_3.1, _3.2, ...)
    LLVMIntrinsicCall(
        Span<'arena>,
        ValueId,
        Symbol<'arena>,
        Box<[TypedLiteral<'arena>]>,
    ),
    // _1 = +_2
    Pos(Span<'arena>, ValueId, TypedLiteral<'arena>),
    // _1 = -_2
    Neg(Span<'arena>, ValueId, TypedLiteral<'arena>),
    // _1 = !_2
    LNot(Span<'arena>, ValueId, TypedLiteral<'arena>),
    // _1 = ~_2
    BNot(Span<'arena>, ValueId, TypedLiteral<'arena>),
    // _1 = _2 + _3
    Add(
        Span<'arena>,
        ValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _2 - _3
    Sub(
        Span<'arena>,
        ValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _2 * _3
    Mul(
        Span<'arena>,
        ValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _2 / _3
    Div(
        Span<'arena>,
        ValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _2 % _3
    Mod(
        Span<'arena>,
        ValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _2 & _3
    BAnd(
        Span<'arena>,
        ValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _2 | _3
    BOr(
        Span<'arena>,
        ValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _2 ^ _3
    BXor(
        Span<'arena>,
        ValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _1 > _2
    GreaterThan(
        Span<'arena>,
        ValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _1 > _2
    LessThan(
        Span<'arena>,
        ValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _1 && _2
    LAnd(
        Span<'arena>,
        ValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
        BlockId,
    ),
    // _1 = _1 || _2
    LOr(
        Span<'arena>,
        ValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
        BlockId,
    ),
    // _1 = _1 >= _2
    GreaterThanEq(
        Span<'arena>,
        ValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _1 <= _2
    LessThanEq(
        Span<'arena>,
        ValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _1 == _2
    Eq(
        Span<'arena>,
        ValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _1 != _2
    Neq(
        Span<'arena>,
        ValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _1 << _2
    LShift(
        Span<'arena>,
        ValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = _1 >> _2
    RShift(
        Span<'arena>,
        ValueId,
        TypedLiteral<'arena>,
        TypedLiteral<'arena>,
    ),
    // _1 = &_1
    //                                v- guaranteed to either be `Dynamic`, `Static` or `Void`
    Reference(Span<'arena>, ValueId, TypedLiteral<'arena>),
    // _1 = *_1
    Dereference(Span<'arena>, ValueId, TypedLiteral<'arena>),
    // NOTE: the indexes into structs will be turned into their respective index
    // e.g. on a struct { a: i32, b: i32 }, a `.a` will be turned into 0 and a `.b` into a 1.
    // _1 = &(*_2).a[3].c.d; This is required, because we will offset the _2 pointer by the required
    // offsets
    Offset(Span<'arena>, ValueId, TypedLiteral<'arena>, OffsetValue),
    // Eq::val(&dyn Eq, ...)
    // The last value is the offset into the function pointer part of the vtable.
    DynCall(Span<'arena>, ValueId, Box<[TypedLiteral<'arena>]>, u32),
    // let _1 = <literal>; This **should never** contain a TypedLiteral::Dynamic as its 3rd element.
    Literal(Span<'arena>, ValueId, TypedLiteral<'arena>),
    DeclareVariable(Span<'arena>, ValueId, Ty<'arena>, Symbol<'arena>),
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
    Alias(Span<'arena>, ValueId, TypedLiteral<'arena>),
    // casting u_ to i_ (for integers of the same size)
    Bitcast(Span<'arena>, ValueId, TypedLiteral<'arena>),
    // i_ or u_ or f_ or bool to i_ or u_ or f_ or bool
    IntCast(Span<'arena>, ValueId, TypedLiteral<'arena>),
    // casts &void to usize (only valid for thin pointers)
    PtrToInt(Span<'arena>, ValueId, TypedLiteral<'arena>),
    // casts usize to &void (only valid for thin pointers)
    IntToPtr(Span<'arena>, ValueId, TypedLiteral<'arena>),
    // Strips the metadata from a fat pointer, &[T] to &T
    StripMetadata(Span<'arena>, ValueId, TypedLiteral<'arena>),
    // _2: &[_; _3]
    // let _1 = attach_metadata(_2, _3)
    MakeUnsizedSlice(Span<'arena>, ValueId, TypedLiteral<'arena>, usize),
    // _2: &<value>
    // let _1 = attach_vtable(_2, trait_1, trait_2)
    AttachVtable(
        Span<'arena>,
        ValueId,
        TypedLiteral<'arena>,
        (Ty<'arena>, Box<[TraitId]>),
    ),
    // <$ty as $trait_id>::$func($args...)
    TraitCall {
        span: Span<'arena>,
        ty: Ty<'arena>,
        trait_id: TraitId,
        func: usize,
        args: Box<[TypedLiteral<'arena>]>,
        dst: ValueId,
    },
    None,
}

impl<'arena> TypedExpression<'arena> {
    pub fn span(&self) -> Span<'arena> {
        match self {
            TypedExpression::Range { span, .. }
            | TypedExpression::Asm { span, .. }
            | TypedExpression::If { span, .. }
            | TypedExpression::While { span, .. }
            | TypedExpression::TraitCall { span, .. }
            | TypedExpression::AttachVtable(span, ..)
            | TypedExpression::DeclareVariable(span, ..)
            | TypedExpression::LLVMIntrinsicCall(span, ..)
            | TypedExpression::IntrinsicCall(span, ..)
            | TypedExpression::DirectCall(span, ..)
            | TypedExpression::DirectExternCall(span, ..)
            | TypedExpression::DynCall(span, ..)
            | TypedExpression::StoreAssignment(span, ..)
            | TypedExpression::MakeUnsizedSlice(span, ..)
            | TypedExpression::StripMetadata(span, ..)
            | TypedExpression::Bitcast(span, ..)
            | TypedExpression::IntCast(span, ..)
            | TypedExpression::PtrToInt(span, ..)
            | TypedExpression::IntToPtr(span, ..)
            | TypedExpression::Offset(span, ..)
            | TypedExpression::Literal(span, ..)
            | TypedExpression::Call(span, ..)
            | TypedExpression::Pos(span, ..)
            | TypedExpression::Neg(span, ..)
            | TypedExpression::LNot(span, ..)
            | TypedExpression::BNot(span, ..)
            | TypedExpression::Add(span, ..)
            | TypedExpression::Sub(span, ..)
            | TypedExpression::Mul(span, ..)
            | TypedExpression::Div(span, ..)
            | TypedExpression::Mod(span, ..)
            | TypedExpression::BAnd(span, ..)
            | TypedExpression::BOr(span, ..)
            | TypedExpression::BXor(span, ..)
            | TypedExpression::GreaterThan(span, ..)
            | TypedExpression::LessThan(span, ..)
            | TypedExpression::LAnd(span, ..)
            | TypedExpression::LOr(span, ..)
            | TypedExpression::GreaterThanEq(span, ..)
            | TypedExpression::LessThanEq(span, ..)
            | TypedExpression::Eq(span, ..)
            | TypedExpression::Neq(span, ..)
            | TypedExpression::LShift(span, ..)
            | TypedExpression::RShift(span, ..)
            | TypedExpression::Reference(span, ..)
            | TypedExpression::Dereference(span, ..)
            | TypedExpression::Alias(span, ..)
            | TypedExpression::Block(span, ..)
            | TypedExpression::Return(span, ..)
            | TypedExpression::Unreachable(span)
            | TypedExpression::Empty(span) => *span,
            TypedExpression::None => Span::DUMMY,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum OffsetValue {
    // its guaranteed the type of this is usize.
    Dynamic(ValueId),
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
