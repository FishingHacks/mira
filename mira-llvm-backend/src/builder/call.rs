use inkwell::{
    builder::BuilderError,
    types::BasicType,
    values::{BasicMetadataValueEnum, FunctionValue},
};
use mira_spans::Symbol;
use mira_typeck::{
    Substitute, SubstitutionCtx, TyKind,
    ir::{TypedLiteral, ValueId},
};

use super::{FunctionCodegenContext, Result};

impl<'ctx, 'arena> FunctionCodegenContext<'ctx, 'arena, '_, '_, '_> {
    pub(super) fn build_call(
        &mut self,
        dst: ValueId,
        func: &TypedLiteral<'arena>,
        args: &[TypedLiteral<'arena>],
    ) -> Result {
        let (fn_ty, fn_ptr) = match func {
            &TypedLiteral::Dynamic(id) => {
                (&self.ir.get_ty(id), self.get_value(id).into_pointer_value())
            }
            &TypedLiteral::Static(id) => (
                &self.ctx.tc_ctx.statics.read()[id].ty,
                self.ctx.statics[id].as_pointer_value(),
            ),
            TypedLiteral::Function(..) => {
                unreachable!("TypedLiteral::Function should have been turned into a DirectCall")
            }
            TypedLiteral::ExternalFunction(_) => unreachable!(
                "TypedLiteral::ExternalFunction should have been turned into a DirectExternCall"
            ),
            TypedLiteral::Intrinsic(..) => {
                unreachable!("TypedLiteral::Intrinsic should have been turned into a IntrinsicCall")
            }
            _ => unreachable!("{func:?} is not callable"),
        };
        let llvm_fn_ty = self.fn_type(*self.substitute(*fn_ty));
        let val = self.build_indirect_call(
            llvm_fn_ty,
            fn_ptr,
            &args
                .iter()
                .filter(|v| match v {
                    TypedLiteral::Void => false,
                    &&TypedLiteral::Dynamic(id) => !self.ir.get_ty(id).is_voidlike(),
                    _ => true,
                })
                .map(|v| self.basic_value(v).into())
                .collect::<Vec<_>>(),
            "",
        )?;
        self.push_value(
            dst,
            val.try_as_basic_value()
                .left_or_else(|_| self.ctx.default_types.empty_struct.const_zero().into()),
        );
        Ok(())
    }

    pub(super) fn build_directcall(
        &mut self,
        function: FunctionValue<'ctx>,
        dst: ValueId,
        args: &[TypedLiteral<'arena>],
    ) -> Result<(), BuilderError> {
        let val = self.build_direct_call(
            function,
            &args
                .iter()
                .filter(|v| match v {
                    TypedLiteral::Void => false,
                    &&TypedLiteral::Dynamic(id) => !self.ir.get_ty(id).is_voidlike(),
                    _ => true,
                })
                .map(|v| self.basic_value(v).into())
                .collect::<Vec<_>>(),
            "",
        )?;
        val.set_call_convention(function.get_call_conventions());
        self.push_value(
            dst,
            val.try_as_basic_value()
                .left_or_else(|_| self.ctx.default_types.empty_struct.const_zero().into()),
        );
        Ok(())
    }

    pub(super) fn build_llvm_intrinsic_call(
        &mut self,
        dst: ValueId,
        intrinsic: Symbol<'arena>,
        args: &[TypedLiteral<'arena>],
    ) -> Result<(), BuilderError> {
        let subst_ctx = SubstitutionCtx::new(self.ctx.tc_ctx, &self.generics);
        let func = self.ctx.intrinsics.get_intrinsic(
            intrinsic,
            args.iter().map(|v| {
                v.to_type(self.ir.scope(), self.ctx.tc_ctx)
                    .substitute(&subst_ctx)
            }),
            self.ir.get_ty(dst),
            &self.ctx.default_types,
            &self.ctx.structs,
            &self.ctx.module,
        );
        let val = self.build_direct_call(
            func,
            &args
                .iter()
                .map(|v| self.basic_value(v).into())
                .collect::<Vec<_>>(),
            "",
        )?;
        self.push_value(
            dst,
            val.try_as_basic_value()
                .left_or_else(|_| self.ctx.default_types.empty_struct.const_zero().into()),
        );
        Ok(())
    }

    pub(super) fn build_dyncall(
        &mut self,
        dst: ValueId,
        args: &[TypedLiteral<'arena>],
        func_offset: u32,
    ) -> Result {
        let mut arguments = args
            .iter()
            .map(|v| self.basic_value(v).into())
            .collect::<Vec<BasicMetadataValueEnum<'ctx>>>();
        let dyn_ptr = arguments[0].into_struct_value();
        let real_ptr = self.build_extract_value(dyn_ptr, 0, "")?;
        arguments[0] = real_ptr.into();
        let vtable_ptr_isize = self.build_extract_value(dyn_ptr, 1, "")?.into_int_value();
        let vtable_ptr = self.build_int_to_ptr(vtable_ptr_isize, self.ctx.default_types.ptr, "")?;
        let fn_ptr_ptr = unsafe {
            self.build_gep(
                self.ctx.default_types.ptr,
                vtable_ptr,
                &[self
                    .ctx
                    .default_types
                    .isize
                    .const_int(func_offset as u64 + 1 /* skip length */, false)],
                "",
            )
        }?;
        let fn_ptr = self
            .build_load(self.ctx.default_types.ptr, fn_ptr_ptr, "")?
            .into_pointer_value();
        let param_types = std::iter::once(self.ctx.default_types.ptr.into())
            .chain(args.iter().skip(1).map(|v| {
                let ty = self.substitute(v.to_type(self.ir.scope(), self.ctx.tc_ctx));
                self.basic_type(&ty).into()
            }))
            .collect::<Vec<_>>();
        let fn_ty = match &**self.substitute(self.ir.get_ty(dst)) {
            TyKind::PrimitiveNever | TyKind::PrimitiveVoid => {
                self.ctx.context.void_type().fn_type(&param_types, false)
            }
            v => self.basic_type(v).fn_type(&param_types, false),
        };

        let res = self.build_indirect_call(fn_ty, fn_ptr, &arguments, "")?;
        self.push_value(
            dst,
            res.try_as_basic_value()
                .left_or_else(|_| self.ctx.default_types.empty_struct.const_zero().into()),
        );
        Ok(())
    }
}
