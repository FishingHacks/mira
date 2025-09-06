use std::ops::BitAnd;

use mira_common::store::StoreKey;
use mira_parser::std_annotations::intrinsic::Intrinsic;
use mira_spans::Symbol;

use crate::{
    Ty, TyKind, TyList, TypeckCtx, TypedExternalFunction, TypedFunction, TypedStatic, TypedStruct,
    default_types,
    ir::{Scope, ValueId},
    types::FunctionType,
};

#[derive(Debug, Clone)]
pub enum TypedLiteral<'arena> {
    Void,
    Dynamic(ValueId),
    Function(StoreKey<TypedFunction<'arena>>, TyList<'arena>),
    ExternalFunction(StoreKey<TypedExternalFunction<'arena>>),
    Static(StoreKey<TypedStatic<'arena>>),
    String(Symbol<'arena>),
    Array(Ty<'arena>, Box<[TypedLiteral<'arena>]>),
    ArrayInit(Ty<'arena>, Box<TypedLiteral<'arena>>, usize),
    Struct(StoreKey<TypedStruct<'arena>>, Box<[TypedLiteral<'arena>]>),
    Tuple(Box<[TypedLiteral<'arena>]>),
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
    Intrinsic(Intrinsic, TyList<'arena>),
    LLVMIntrinsic(Symbol<'arena>),
}

impl<'ctx> TypedLiteral<'ctx> {
    pub fn to_type(&self, scope: &Scope<'ctx>, ctx: &TypeckCtx<'ctx>) -> Ty<'ctx> {
        match self {
            TypedLiteral::Void => default_types::void,
            TypedLiteral::Dynamic(id) => scope[*id].ty,
            TypedLiteral::Function(id, _) => {
                let contract = &ctx.functions.read()[*id].0;
                let fn_type = FunctionType {
                    return_type: contract.return_type,
                    arguments: ctx
                        .ctx
                        .intern_tylist(&contract.arguments.iter().map(|v| v.1).collect::<Vec<_>>()),
                };
                ctx.ctx.intern_ty(TyKind::Function(fn_type))
            }
            TypedLiteral::ExternalFunction(id) => {
                let contract = &ctx.external_functions.read()[*id].0;
                let fn_type = FunctionType {
                    return_type: contract.return_type,
                    arguments: ctx
                        .ctx
                        .intern_tylist(&contract.arguments.iter().map(|v| v.1).collect::<Vec<_>>()),
                };
                ctx.ctx.intern_ty(TyKind::Function(fn_type))
            }
            TypedLiteral::Static(id) => ctx.statics.read()[*id].ty,
            TypedLiteral::String(_) => default_types::str_ref,
            TypedLiteral::Array(ty, elems) => ctx.ctx.intern_ty(TyKind::SizedArray {
                ty: *ty,
                number_elements: elems.len(),
            }),
            TypedLiteral::ArrayInit(ty, _, elems) => ctx.ctx.intern_ty(TyKind::SizedArray {
                ty: *ty,
                number_elements: *elems,
            }),
            TypedLiteral::Struct(struct_id, _) => ctx.ctx.intern_ty(TyKind::Struct {
                struct_id: *struct_id,
                name: ctx.structs.read()[*struct_id].name,
            }),
            TypedLiteral::Tuple(elems) => ctx.ctx.intern_ty(TyKind::Tuple(
                ctx.ctx.intern_tylist(
                    &elems
                        .iter()
                        .map(|v| v.to_type(scope, ctx))
                        .collect::<Vec<_>>(),
                ),
            )),
            TypedLiteral::F64(_) => default_types::f64,
            TypedLiteral::F32(_) => default_types::f32,
            TypedLiteral::U8(_) => default_types::u8,
            TypedLiteral::U16(_) => default_types::u16,
            TypedLiteral::U32(_) => default_types::u32,
            TypedLiteral::U64(_) => default_types::u64,
            TypedLiteral::USize(_) => default_types::usize,
            TypedLiteral::I8(_) => default_types::i8,
            TypedLiteral::I16(_) => default_types::i16,
            TypedLiteral::I32(_) => default_types::i32,
            TypedLiteral::I64(_) => default_types::i64,
            TypedLiteral::ISize(_) => default_types::isize,
            TypedLiteral::Bool(_) => default_types::bool,
            TypedLiteral::Intrinsic(..) => panic!("intrinsic cannot have a type"),
            TypedLiteral::LLVMIntrinsic(_) => panic!("llvm intrinsic cannot have a type"),
        }
    }

    pub fn to_primitive_type(
        &self,
        scope: &Scope<'ctx>,
        ctx: &TypeckCtx<'ctx>,
    ) -> Option<Ty<'ctx>> {
        match self {
            TypedLiteral::F64(_) => Some(default_types::f64),
            TypedLiteral::F32(_) => Some(default_types::f32),
            TypedLiteral::U8(_) => Some(default_types::u8),
            TypedLiteral::U16(_) => Some(default_types::u16),
            TypedLiteral::U32(_) => Some(default_types::u32),
            TypedLiteral::U64(_) => Some(default_types::u64),
            TypedLiteral::USize(_) => Some(default_types::usize),
            TypedLiteral::I8(_) => Some(default_types::i8),
            TypedLiteral::I16(_) => Some(default_types::i16),
            TypedLiteral::I32(_) => Some(default_types::i32),
            TypedLiteral::I64(_) => Some(default_types::i64),
            TypedLiteral::ISize(_) => Some(default_types::isize),
            TypedLiteral::Bool(_) => Some(default_types::bool),
            TypedLiteral::String(_) => Some(default_types::str_ref),
            TypedLiteral::Void => Some(default_types::void),
            TypedLiteral::Dynamic(id) => {
                let ty = scope[*id].ty;
                ty.is_primitive().then_some(ty)
            }
            TypedLiteral::Static(id) => {
                let ty = ctx.statics.read()[*id].ty;
                ty.is_primitive().then_some(ty)
            }
            TypedLiteral::Array(..)
            | TypedLiteral::ArrayInit(..)
            | TypedLiteral::Struct(..)
            | TypedLiteral::Tuple(..)
            | TypedLiteral::Function(..)
            | TypedLiteral::Intrinsic(..)
            | TypedLiteral::LLVMIntrinsic(_)
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
                .fold(true, BitAnd::bitand),
            TypedLiteral::ArrayInit(_, elem, amount) => *amount == 0 || elem.is_entirely_literal(),
            TypedLiteral::Static(_)
            | TypedLiteral::Dynamic(_)
            | TypedLiteral::Intrinsic(..)
            | TypedLiteral::LLVMIntrinsic(_) => false,
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
