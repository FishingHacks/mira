use inkwell::{builder::BuilderError, values::BasicValue};
use mira_parser::std_annotations::intrinsic::Intrinsic;
use mira_typeck::{
    Ty, TyKind,
    ir::{TypedLiteral, ValueId},
};

use super::FunctionCodegenContext;

impl<'arena> FunctionCodegenContext<'_, 'arena, '_, '_, '_> {
    pub(crate) fn build_intrinsic_call(
        &mut self,
        dst: ValueId,
        intrinsic: Intrinsic,
        args: &[TypedLiteral<'arena>],
        generics: &[Ty<'arena>],
    ) -> Result<(), BuilderError> {
        match intrinsic {
            Intrinsic::CallMain => {
                let main_fn = self
                    .ctx
                    .tc_ctx
                    .main_function
                    .get()
                    .expect("main function has to be set to use intrinsic `call_main`.");
                let func = self.ctx.functions[main_fn];
                self.build_direct_call(func, &[], "")?;
                self.push_value(dst, self.ctx.default_types.empty_struct.const_zero().into());
            }
            Intrinsic::Unreachable => {
                self.build_unreachable()?;
                self.push_value(dst, self.ctx.default_types.empty_struct.const_zero().into());
            }
            Intrinsic::SizeOf => {
                let ty = self.substitute(generics[0]);
                let size = ty
                    .size_and_alignment(self.pointer_size, &self.ctx.tc_ctx.structs.read())
                    .0;
                self.push_value(
                    dst,
                    self.ctx.default_types.isize.const_int(size, false).into(),
                );
            }
            Intrinsic::Offset => {
                let ptr = self.basic_value(&args[0]).into_pointer_value();
                let offset = self.basic_value(&args[1]).into_int_value();
                let val = unsafe {
                    self.build_in_bounds_gep(self.ctx.default_types.i8, ptr, &[offset], "")?
                };
                self.push_value(dst, val.into());
            }
            Intrinsic::GetMetadata => {
                let ty = self.substitute(generics[0]);
                if ty.is_sized() {
                    self.push_value(dst, self.ctx.default_types.isize.const_int(0, false).into());
                } else {
                    let fat_ptr = self.basic_value(&args[0]).into_struct_value();
                    let metadata = self.build_extract_value(fat_ptr, 1, "")?;
                    self.push_value(dst, metadata);
                }
            }
            Intrinsic::WithMetadata => {
                let initial_ty = self.substitute(generics[0]);
                let target_ty = self.substitute(generics[1]);
                assert!(initial_ty.is_sized());
                assert!(!target_ty.is_sized());
                let ptr = self.basic_value(&args[0]);
                let metadata = self.basic_value(&args[1]);
                let fat_ptr = self.build_insert_value(
                    self.ctx.default_types.fat_ptr.get_poison(),
                    ptr,
                    0,
                    "",
                )?;
                let fat_ptr = self.build_insert_value(fat_ptr, metadata, 1, "")?;
                self.push_value(dst, fat_ptr.as_basic_value_enum());
            }
            Intrinsic::VolatileRead => {
                let ty = self.substitute(generics[0]);
                assert!(ty.is_sized());
                let value =
                    self.build_deref(self.basic_value(&args[0]).into_pointer_value(), ty, true)?;
                self.push_value(dst, value);
            }
            Intrinsic::VolatileWrite => {
                let ty = self.substitute(generics[0]);
                assert!(ty.is_sized());
                self.build_ptr_store(
                    self.basic_value(&args[0]).into_pointer_value(),
                    self.basic_value(&args[1]),
                    ty,
                    true,
                )?;
            }
            Intrinsic::SizeOfVal => {
                let ty = self.substitute(generics[0]);

                if ty.is_sized() {
                    let size = ty
                        .size_and_alignment(self.pointer_size, &self.ctx.tc_ctx.structs.read())
                        .0;
                    self.push_value(
                        dst,
                        self.ctx.default_types.isize.const_int(size, false).into(),
                    );
                } else {
                    match *ty {
                        TyKind::DynType { .. } => {
                            let fat_ptr = self.basic_value(&args[0]).into_struct_value();
                            let vtable_ptr_int =
                                self.build_extract_value(fat_ptr, 1, "")?.into_int_value();
                            let vtable_ptr = self.build_int_to_ptr(
                                vtable_ptr_int,
                                self.ctx.default_types.ptr,
                                "",
                            )?;
                            let size =
                                self.build_load(self.ctx.default_types.isize, vtable_ptr, "")?;
                            self.push_value(dst, size);
                        }
                        TyKind::UnsizedArray(ty) => {
                            let fat_ptr = self.basic_value(&args[0]).into_struct_value();
                            let len = self.build_extract_value(fat_ptr, 1, "")?.into_int_value();
                            let size_single = ty
                                .size_and_alignment(
                                    self.pointer_size,
                                    &self.ctx.tc_ctx.structs.read(),
                                )
                                .0;
                            let total_size = self.build_int_nuw_mul(
                                len,
                                self.ctx.default_types.isize.const_int(size_single, false),
                                "",
                            )?;
                            self.push_value(dst, total_size.as_basic_value_enum());
                        }
                        TyKind::PrimitiveStr => {
                            let fat_ptr = self.basic_value(&args[0]).into_struct_value();
                            let size = self.build_extract_value(fat_ptr, 1, "")?;
                            self.push_value(dst, size);
                        }
                        TyKind::Generic { .. } | TyKind::PrimitiveSelf => {
                            unreachable!("These should've been resolved by now.")
                        }
                        t => unreachable!("{t:?} should be sized"),
                    }
                }
            }

            Intrinsic::Read => {
                let ty = self.substitute(generics[0]);
                assert!(ty.is_sized());
                let value =
                    self.build_deref(self.basic_value(&args[0]).into_pointer_value(), ty, false)?;
                self.push_value(dst, value);
            }
            Intrinsic::Write => {
                let ty = self.substitute(generics[0]);
                assert!(ty.is_sized());
                self.build_ptr_store(
                    self.basic_value(&args[0]).into_pointer_value(),
                    self.basic_value(&args[1]),
                    ty,
                    false,
                )?;
            }

            // TODO: raii
            Intrinsic::Drop | Intrinsic::DropInPlace | Intrinsic::Forget => {
                unreachable!("these are handled by the dropck pass")
            }

            // These are being replaced by the `TypenameIntrinsicPass`
            Intrinsic::TypeName => unreachable!(),
        }
        Ok(())
    }
}
