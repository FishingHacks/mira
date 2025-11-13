use std::ops::Deref;

use inkwell::{
    basic_block::BasicBlock,
    builder::{Builder, BuilderError},
    types::BasicTypeEnum,
    values::{BasicValueEnum, PointerValue},
};
use mira_common::index::IndexMap;
use mira_parser::module::{ModuleId, StructId};
use mira_typeck::{
    Substitute, Ty, TyKind, TyList, TypeCtx, TypedStruct,
    ir::{IR, ValueId},
};
use parking_lot::RwLockReadGuard;

use crate::{
    CodegenContext,
    abi::{ArgumentType, has_special_encoding},
};
type Result<T = (), E = BuilderError> = std::result::Result<T, E>;

mod asm;
mod binop;
mod call;
mod cast;
mod comparison;
mod control_flow;
mod expr;
mod intrinsic;
mod lit;
mod ptr;
mod scope;

pub(crate) struct FunctionCodegenContext<'ctx, 'arena, 'cg, 'a, 'ir> {
    ir: &'ir IR<'arena>,
    scope: IndexMap<ValueId, BasicValueEnum<'ctx>>,
    generics: TyList<'arena>,

    pub(crate) current_block: BasicBlock<'ctx>,
    pointer_size: u64,
    module: ModuleId,

    return_ty: ArgumentType<'ctx>,
    // If return_ty is not ArgumentType::SRet, this is a null pointer.
    return_val: PointerValue<'ctx>,

    pub(crate) structs_reader: RwLockReadGuard<'a, IndexMap<StructId, TypedStruct<'arena>>>,
    pub(crate) ctx: &'cg mut CodegenContext<'ctx, 'arena, 'a>,
}

impl<'ctx> Deref for FunctionCodegenContext<'ctx, '_, '_, '_, '_> {
    type Target = Builder<'ctx>;

    fn deref(&self) -> &Self::Target {
        &self.ctx.builder
    }
}

impl<'ctx> FunctionCodegenContext<'_, 'ctx, '_, '_, '_> {
    pub(crate) fn ty_cx(&self) -> TypeCtx<'ctx> {
        self.ctx.tc_ctx.ctx
    }
}

impl<'ctx, 'arena, 'a> CodegenContext<'ctx, 'arena, 'a> {
    /// If return_ty is not ArgumentType::SRet, return_ty should be a null pointer.
    pub(crate) fn make_function_codegen_context<'me, 'ir>(
        &'me mut self,
        generics: TyList<'arena>,
        module: ModuleId,
        ir: &'ir IR<'arena>,
        current_block: BasicBlock<'ctx>,
        return_ty: ArgumentType<'ctx>,
        return_val: PointerValue<'ctx>,
    ) -> FunctionCodegenContext<'ctx, 'arena, 'me, 'a, 'ir> {
        FunctionCodegenContext {
            ir,
            scope: IndexMap::new(),
            generics,
            current_block,
            pointer_size: self.default_types.isize.get_bit_width() as u64 / 8,
            module,
            structs_reader: self.tc_ctx.structs.read(),
            ctx: self,
            return_val,
            return_ty,
        }
    }
}

impl<'ctx, 'arena> FunctionCodegenContext<'ctx, 'arena, '_, '_, '_> {
    pub(crate) fn substitute<T: Substitute<'arena>>(&self, v: T) -> T {
        self.ctx.tc_ctx.substitute(&*self.generics, v)
    }

    pub(crate) fn basic_ty(&self, ty: &TyKind<'arena>) -> BasicTypeEnum<'ctx> {
        self.ctx.basic_ty(ty)
    }

    pub(crate) fn is_stack_allocated(&self, value: ValueId) -> bool {
        self.ir.scope().get(value).stack_allocated
            || has_special_encoding(*self.substitute(self.get_ty(value)))
    }

    pub(crate) fn get_ty(&self, value: ValueId) -> Ty<'arena> {
        self.ir.get_ty(value)
    }

    pub(crate) fn goto(&mut self, bb: BasicBlock<'ctx>) {
        self.position_at_end(bb);
        self.current_block = bb;
    }

    /// uses the function to build a terminator if none was built yet
    pub(crate) fn terminate<T, E>(&self, func: impl FnOnce() -> Result<T, E>) -> Result<(), E> {
        if self.current_block.get_terminator().is_none() {
            func()?;
        }
        Ok(())
    }

    pub(crate) fn make_bb<const N: usize>(&self, names: [&str; N]) -> [BasicBlock<'ctx>; N] {
        let mut bbs = [self.current_block; N];
        for i in 0..N {
            let prev = if i == 0 {
                self.current_block
            } else {
                bbs[i - 1]
            };
            bbs[i] = self.ctx.context.insert_basic_block_after(prev, names[i]);
        }
        bbs
    }
}

// Matches on the type and then uses the float, signed, or unsigned builder method to build the
// expression.
#[macro_export]
macro_rules! float_signed_unsigned {
    ($ctx:expr, $ty: expr, $lhs: expr, $rhs: expr, $sint: ident($($sint_val:expr),*), $uint: ident($($uint_val:expr),*), $float: ident($($float_val:expr),*), $err:literal) => {
        match *$ty {
            mira_typeck::TyKind::PrimitiveI8
            | mira_typeck::TyKind::PrimitiveI16
            | mira_typeck::TyKind::PrimitiveI32
            | mira_typeck::TyKind::PrimitiveI64
            | mira_typeck::TyKind::PrimitiveISize => $ctx
                .$sint($($sint_val,)* $lhs.into_int_value(), $rhs.into_int_value(), "")?
                .into(),
            mira_typeck::TyKind::PrimitiveU8
            | mira_typeck::TyKind::PrimitiveU16
            | mira_typeck::TyKind::PrimitiveU32
            | mira_typeck::TyKind::PrimitiveU64
            | mira_typeck::TyKind::PrimitiveUSize => $ctx
                .$uint($($uint_val,)* $lhs.into_int_value(), $rhs.into_int_value(), "")?
                .into(),
            mira_typeck::TyKind::PrimitiveF32 | mira_typeck::TyKind::PrimitiveF64 => $ctx
                .$float($($float_val,)* $lhs.into_float_value(), $rhs.into_float_value(), "")?
                .into(),
            _ => unreachable!(concat!($err, "  -- Type: {}"), $ty),
        }
    };
}
