use inkwell::{builder::BuilderError, types::BasicMetadataTypeEnum, values::FunctionValue};
use mira_parser::module::ExternalFunctionId;
use mira_spans::Symbol;
use mira_typeck::{
    Substitute, SubstitutionCtx, Ty, TyKind, TypeCtx,
    ir::{TypedLiteral, ValueId},
};

use crate::{
    CodegenContext, FnInstance,
    abi::{ArgumentType, argument, has_special_encoding, return_ty},
};

use super::{FunctionCodegenContext, Result};

pub(super) enum DirectCallTarget<'tyctx> {
    Extern(ExternalFunctionId),
    Normal(FnInstance<'tyctx>),
}

impl<'tyctx> DirectCallTarget<'tyctx> {
    pub(super) fn get_function_value<'ctx>(
        &self,
        ctx: &CodegenContext<'ctx, 'tyctx, '_>,
    ) -> FunctionValue<'ctx> {
        match self {
            &Self::Extern(v) => ctx.external_functions[v].value,
            Self::Normal(fn_instance) => ctx
                .get_fn_instance_if_generated(fn_instance)
                .expect("in order to call a function instance, it has to have been instantiated."),
        }
    }

    pub(super) fn get_return_ty<'ctx>(
        &self,
        ctx: &CodegenContext<'ctx, 'tyctx, '_>,
    ) -> ArgumentType<'ctx> {
        match self {
            &Self::Extern(id) => ctx.external_functions[id].return_ty(),
            Self::Normal(instance) => ctx.with_generated_fn_instance(instance, |f| f.return_ty()),
        }
    }

    pub(super) fn with_args<'ctx, R>(
        &self,
        ctx: &CodegenContext<'ctx, 'tyctx, '_>,
        f: impl FnOnce(&[ArgumentType<'ctx>]) -> R,
    ) -> R {
        match self {
            &Self::Extern(id) => f(ctx.external_functions[id].args()),
            Self::Normal(instance) => {
                ctx.with_generated_fn_instance(instance, |func| f(func.args()))
            }
        }
    }
}

fn classify<'ctx, 'a, 'tycx: 'a>(
    ret_ty: &TyKind<'tycx>,
    args: impl Iterator<Item = &'a TyKind<'tycx>>,
    ctx: &FunctionCodegenContext<'ctx, 'tycx, '_, '_, '_>,
) -> Box<[ArgumentType<'ctx>]> {
    let mut ret_args_types = Vec::new();
    let ty = return_ty(
        ctx.ctx.context,
        ret_ty,
        ctx.pointer_size as u8,
        &ctx.structs_reader,
        |t| ctx.basic_ty(t),
        ctx.ty_cx(),
    );
    ret_args_types.push(ty);

    for arg in args {
        let ty = argument(
            ctx.ctx.context,
            arg,
            ctx.pointer_size as u8,
            &ctx.structs_reader,
            |t| ctx.basic_ty(t),
            ctx.ty_cx(),
        );
        ret_args_types.push(ty);
    }

    ret_args_types.into_boxed_slice()
}

impl<'arena> FunctionCodegenContext<'_, 'arena, '_, '_, '_> {
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

        let TyKind::Function(v) = *self.substitute(*fn_ty) else {
            unreachable!()
        };

        let ret_args_ty = classify(&v.return_type, v.arguments.iter().map(|v| **v), self);

        let param_types = ret_args_ty[1..]
            .iter()
            .filter_map(|v| v.as_arg_ty(&self.ctx.default_types).map(Into::into))
            .collect::<Vec<_>>();

        let fn_ty = ret_args_ty[0].func_ty(
            param_types,
            false,
            &self.ctx.default_types,
            self.ctx.context,
        );

        let mut arg_vals = Vec::new();
        match &ret_args_ty[0] {
            ArgumentType::SRet(_) => {
                let ty = self.basic_ty(&self.get_ty(dst));
                let alloca = self.build_alloca(ty, "")?;
                // set the alloca as the destination.
                self.push_value_raw(dst, alloca);
                arg_vals.push(alloca.into());
            }
            ArgumentType::Regular(_) | ArgumentType::None => (),
            ArgumentType::ByVal(_) => unreachable!("byval in return"),
        }

        assert_eq!(args.len(), ret_args_ty.len() - 1);

        for (arg_ty, arg) in ret_args_ty[1..].iter().zip(args) {
            match arg_ty {
                &ArgumentType::Regular(ty) => {
                    if has_special_encoding(
                        *self.substitute(arg.to_type(self.ir.scope(), self.ctx.tc_ctx)),
                    ) {
                        match arg {
                            // can never be void, because void is a zst and as such
                            // ArgumentType::None.
                            TypedLiteral::Void => unreachable!(),
                            // For dynamics, they are guaranteed to be stack allocated, and as such
                            // can just be dereferenced
                            &TypedLiteral::Dynamic(value) => {
                                let arg = self.build_load(ty, self.get_value_ptr(value), "")?;
                                arg_vals.push(arg.into());
                            }
                            // For anything else, alloca first and then do the same as above.
                            arg => {
                                let alloca = self.build_alloca(ty, "")?;
                                self.basic_value_ptr(arg, alloca, true)?;
                                arg_vals.push(self.build_load(ty, alloca, "")?.into());
                            }
                        }
                    } else {
                        arg_vals.push(self.basic_value(arg).into())
                    }
                }
                &ArgumentType::ByVal(ty) => {
                    let alloca = self.build_alloca(ty, "")?;
                    self.basic_value_ptr(arg, alloca, true)?;
                    arg_vals.push(alloca.into());
                }
                // this argument isn't passed lol
                ArgumentType::None => {}
                ArgumentType::SRet(_) => unreachable!("sret in argument"),
            }
        }

        let val = self.build_indirect_call(fn_ty, fn_ptr, &arg_vals, "")?;

        if let ArgumentType::Regular(_) = &ret_args_ty[0] {
            let value = val
                .try_as_basic_value()
                .expect_left("regular should always return a basicvalue");
            let actual_ty = self.get_ty(dst);
            if has_special_encoding(&actual_ty) {
                assert!(self.is_stack_allocated(dst));
                let alloca = self.build_alloca(self.basic_ty(&actual_ty), "")?;
                self.build_store(alloca, value)?;
                self.push_value_raw(dst, alloca);
            } else {
                self.push_value(dst, value);
            }
        }
        Ok(())
    }

    pub(super) fn build_directcall(
        &mut self,
        target: DirectCallTarget<'arena>,
        dst: ValueId,
        args: &[TypedLiteral<'arena>],
    ) -> Result<(), BuilderError> {
        let func = target.get_function_value(self.ctx);
        let return_ty = target.get_return_ty(self.ctx);
        let mut arg_vals = Vec::new();
        match return_ty {
            ArgumentType::SRet(_) => {
                let ty = self.basic_ty(&self.get_ty(dst));
                let alloca = self.build_alloca(ty, "")?;
                // set the alloca as the destination.
                self.push_value_raw(dst, alloca);
                arg_vals.push(alloca.into());
            }
            ArgumentType::Regular(_) | ArgumentType::None => (),
            ArgumentType::ByVal(_) => unreachable!("byval in return"),
        }

        let arg_count = target.with_args(self.ctx, |arg_tys| arg_tys.len());
        assert_eq!(args.len(), arg_count);
        for (i, arg) in args.iter().enumerate() {
            let arg_ty = target.with_args(self.ctx, |arg_tys| arg_tys[i]);
            match arg_ty {
                ArgumentType::Regular(ty) => {
                    if has_special_encoding(&arg.to_type(self.ir.scope(), self.ctx.tc_ctx)) {
                        match arg {
                            // can never be void, because void is a zst and as such
                            // ArgumentType::None.
                            TypedLiteral::Void => unreachable!(),
                            // For dynamics, they are guaranteed to be stack allocated, and as such
                            // can just be dereferenced
                            &TypedLiteral::Dynamic(value) => {
                                let arg = self.build_load(ty, self.get_value_ptr(value), "")?;
                                arg_vals.push(arg.into());
                            }
                            // For anything else, alloca first and then do the same as above.
                            arg => {
                                let alloca = self.build_alloca(ty, "")?;
                                self.basic_value_ptr(arg, alloca, true)?;
                                arg_vals.push(self.build_load(ty, alloca, "")?.into());
                            }
                        }
                    } else {
                        arg_vals.push(self.basic_value(arg).into())
                    }
                }
                ArgumentType::ByVal(ty) => {
                    let alloca = self.build_alloca(ty, "")?;
                    self.basic_value_ptr(arg, alloca, true)?;
                    arg_vals.push(alloca.into());
                }
                // this argument isn't passed lol
                ArgumentType::None => {}
                ArgumentType::SRet(_) => unreachable!("sret in argument"),
            }
        }

        let val = self.build_direct_call(func, &arg_vals, "")?;
        val.set_call_convention(func.get_call_conventions());
        if let ArgumentType::Regular(_) = return_ty {
            let value = val
                .try_as_basic_value()
                .expect_left("regular should always return a basicvalue");
            let actual_ty = self.get_ty(dst);
            if has_special_encoding(&actual_ty) {
                assert!(self.is_stack_allocated(dst));
                let alloca = self.build_alloca(self.basic_ty(&actual_ty), "")?;
                self.build_store(alloca, value)?;
                self.push_value_raw(dst, alloca);
            } else {
                self.push_value(dst, value);
            }
        }
        Ok(())
    }

    pub(super) fn build_llvm_intrinsic_call(
        &mut self,
        dst: ValueId,
        intrinsic: Symbol<'arena>,
        args: &[TypedLiteral<'arena>],
    ) -> Result<(), BuilderError> {
        let subst_ctx = SubstitutionCtx::new(self.ty_cx(), &self.generics);
        let args_tys = args
            .iter()
            .map(|v| {
                v.to_type(self.ir.scope(), self.ctx.tc_ctx)
                    .substitute(&subst_ctx)
            })
            .collect::<Vec<_>>();
        let intrinsic = llvm_intrinsic_to_real_intrinsic(
            self.ty_cx(),
            intrinsic,
            &args_tys,
            self.get_ty(dst).substitute(&subst_ctx),
            self.pointer_size as u8,
        );
        let func = self
            .ctx
            .get_intrinsic(intrinsic, args_tys.into_iter(), self.ir.get_ty(dst));
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
        let dyn_ptr = self.basic_value(&args[0]).into_struct_value();
        let real_ptr = self.build_extract_value(dyn_ptr, 0, "")?;
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

        let ret_ty = self.substitute(self.get_ty(dst));

        let ret_args_ty = classify(
            *ret_ty,
            args.iter()
                .skip(1)
                .map(|v| *self.substitute(v.to_type(self.ir.scope(), self.ctx.tc_ctx))),
            self,
        );

        let mut param_types = Vec::with_capacity(ret_args_ty.len());
        // self pointer
        param_types.push(self.ctx.default_types.ptr.into());
        // the other arguments
        param_types.extend(
            ret_args_ty[1..]
                .iter()
                .filter_map(|v| v.as_arg_ty(&self.ctx.default_types))
                .map(BasicMetadataTypeEnum::from),
        );

        let fn_ty = ret_args_ty[0].func_ty(
            param_types,
            false,
            &self.ctx.default_types,
            self.ctx.context,
        );

        let mut arg_vals = Vec::new();
        match &ret_args_ty[0] {
            ArgumentType::SRet(_) => {
                let ty = self.basic_ty(&self.get_ty(dst));
                let alloca = self.build_alloca(ty, "")?;
                // set the alloca as the destination.
                self.push_value_raw(dst, alloca);
                arg_vals.push(alloca.into());
            }
            ArgumentType::Regular(_) | ArgumentType::None => (),
            ArgumentType::ByVal(_) => unreachable!("byval in return"),
        }
        // push self
        arg_vals.push(real_ptr.into());

        assert_eq!(args.len(), ret_args_ty.len() - 1);

        for (arg_ty, arg) in ret_args_ty[1..].iter().zip(args) {
            match arg_ty {
                &ArgumentType::Regular(ty) => {
                    if has_special_encoding(&arg.to_type(self.ir.scope(), self.ctx.tc_ctx)) {
                        match arg {
                            // can never be void, because void is a zst and as such
                            // ArgumentType::None.
                            TypedLiteral::Void => unreachable!(),
                            // For dynamics, they are guaranteed to be stack allocated, and as such
                            // can just be dereferenced
                            &TypedLiteral::Dynamic(value) => {
                                let arg = self.build_load(ty, self.get_value_ptr(value), "")?;
                                arg_vals.push(arg.into());
                            }
                            // For anything else, alloca first and then do the same as above.
                            arg => {
                                let alloca = self.build_alloca(ty, "")?;
                                self.basic_value_ptr(arg, alloca, true)?;
                                arg_vals.push(self.build_load(ty, alloca, "")?.into());
                            }
                        }
                    } else {
                        arg_vals.push(self.basic_value(arg).into())
                    }
                }
                &ArgumentType::ByVal(ty) => {
                    let alloca = self.build_alloca(ty, "")?;
                    self.basic_value_ptr(arg, alloca, true)?;
                    arg_vals.push(alloca.into());
                }
                // this argument isn't passed lol
                ArgumentType::None => {}
                ArgumentType::SRet(_) => unreachable!("sret in argument"),
            }
        }

        let res = self.build_indirect_call(fn_ty, fn_ptr, &arg_vals, "")?;

        if let ArgumentType::Regular(_) = &ret_args_ty[0] {
            let value = res
                .try_as_basic_value()
                .expect_left("regular should always return a basicvalue");
            let actual_ty = self.get_ty(dst);
            if has_special_encoding(&actual_ty) {
                assert!(self.is_stack_allocated(dst));
                let alloca = self.build_alloca(self.basic_ty(&actual_ty), "")?;
                self.build_store(alloca, value)?;
                self.push_value_raw(dst, alloca);
            } else {
                self.push_value(dst, value);
            }
        }

        Ok(())
    }
}

// parses $N (where n is a base-10 number) and $- for the argument name and return type
// respectively.
fn llvm_intrinsic_to_real_intrinsic<'ctx>(
    ctx: TypeCtx<'ctx>,
    name: Symbol<'ctx>,
    args: &[Ty<'_>],
    ret: Ty<'_>,
    ptrsize: u8,
) -> Symbol<'ctx> {
    if !name.contains('$') {
        return name;
    }
    let mut new_name = String::with_capacity(name.len());
    let mut is_parsing = false;
    let mut parse_buffer = String::new();
    for c in name.chars() {
        if is_parsing {
            if parse_buffer.is_empty() && c == '-' {
                is_parsing = false;
                new_name.push_str(ty_to_llvm_name(&ret, ptrsize));
            } else if c.is_ascii_digit() {
                parse_buffer.push(c);
            } else {
                is_parsing = false;
                let mut argnum = 0;
                for c in parse_buffer.drain(..) {
                    argnum = argnum * 10 + (c as u8 - b'0') as usize;
                }
                new_name.push_str(ty_to_llvm_name(&args[argnum], ptrsize));
                new_name.push(c);
            }
        } else if c == '$' {
            is_parsing = true;
        } else {
            new_name.push(c);
        }
    }
    if is_parsing {
        let mut argnum = 0;
        for c in parse_buffer.drain(..) {
            argnum = argnum * 10 + (c as u8 - b'0') as usize;
        }
        new_name.push_str(ty_to_llvm_name(&args[argnum], ptrsize));
    }
    ctx.intern_str(&new_name)
}

fn ty_to_llvm_name(ty: &TyKind<'_>, ptrsize: u8) -> &'static str {
    match ty {
        TyKind::Function(_) => "ptr",
        TyKind::PrimitiveI8 | TyKind::PrimitiveU8 => "i8",
        TyKind::PrimitiveI16 | TyKind::PrimitiveU16 => "i16",
        TyKind::PrimitiveI32 | TyKind::PrimitiveU32 => "i32",
        TyKind::PrimitiveI64 | TyKind::PrimitiveU64 => "i64",
        TyKind::PrimitiveISize | TyKind::PrimitiveUSize => match ptrsize {
            4 => "i32",
            8 => "i64",
            _ => unreachable!("Illegal ptrsize: {ptrsize}"),
        },
        TyKind::PrimitiveF32 => "f32",
        TyKind::PrimitiveF64 => "f64",
        TyKind::PrimitiveBool => "i1",
        TyKind::Ref(inner) if inner.is_sized() => "ptr",

        TyKind::UnsizedArray(_)
        | TyKind::SizedArray { .. }
        | TyKind::Tuple(_)
        | TyKind::PrimitiveVoid
        | TyKind::PrimitiveNever
        | TyKind::PrimitiveStr
        | TyKind::PrimitiveSelf
        | TyKind::Ref(_)
        | TyKind::Generic { .. }
        | TyKind::DynType(_)
        | TyKind::Struct { .. } => {
            unreachable!("only llvm primitive types are allowed in llvm intrinsics.")
        }
    }
}
