use mira_parser::module::TraitId;
use mira_typeck::{
    Ty, TyKind, default_types,
    ir::{TypedLiteral, ValueId},
};

use super::{FunctionCodegenContext, Result};

impl<'arena> FunctionCodegenContext<'_, 'arena, '_, '_, '_> {
    pub(super) fn build_bitcast(&mut self, dst: ValueId, value: &TypedLiteral<'arena>) -> Result {
        let new_ty = self.substitute(self.get_ty(dst));
        let old_typ = self.substitute(value.to_type(self.ir.scope(), self.ctx.tc_ctx));
        assert!(!new_ty.has_refs() || new_ty.is_thin_ptr());
        assert!(!old_typ.has_refs() || old_typ.is_thin_ptr());
        let new_value = self.build_bit_cast(self.basic_value(value), self.basic_ty(&new_ty), "")?;
        self.push_value(dst, new_value);
        Ok(())
    }

    pub(super) fn build_inttoptr(&mut self, dst: ValueId, value: &TypedLiteral<'arena>) -> Result {
        let value = self.basic_value(value).into_int_value();
        let value = if value.is_constant_int() {
            value.const_to_pointer(self.ctx.default_types.ptr)
        } else {
            self.build_int_to_ptr(value, self.ctx.default_types.ptr, "")?
        };
        self.push_value(dst, value.into());
        Ok(())
    }
    pub(super) fn build_ptrtoint(&mut self, dst: ValueId, value: &TypedLiteral<'arena>) -> Result {
        let ty = self.substitute(self.get_ty(dst));
        let ty = self.basic_ty(&ty).into_int_type();
        let value = self.basic_value(value).into_pointer_value();
        if value.is_const() {
            value.const_to_int(ty)
        } else {
            self.build_ptr_to_int(value, ty, "")?
        };
        self.push_value(dst, value.into());
        Ok(())
    }

    pub(super) fn build_intcast(&mut self, dst: ValueId, value: &TypedLiteral<'arena>) -> Result {
        let src_ty = value
            .to_primitive_type(self.ir.scope(), self.ctx.tc_ctx)
            .expect("can only cast primitive types");
        let dst_ty = self.substitute(self.get_ty(dst));
        let src_value = self.basic_value(value);
        // u8 -> bool
        if src_ty == default_types::u8 && dst_ty == default_types::bool {
            let src_value = src_value.into_int_value();
            let value = if src_value.is_constant_int() {
                src_value.const_truncate(self.ctx.default_types.bool)
            } else {
                self.build_int_truncate(src_value, self.ctx.default_types.bool, "")?
            };
            self.push_value(dst, value.into());
            return Ok(());
        }
        // bool -> u8
        if src_ty == default_types::bool && dst_ty == default_types::u8 {
            let src_value = src_value.into_int_value();
            let value = self.build_int_z_extend(src_value, self.ctx.default_types.i8, "")?;
            self.push_value(dst, value.into());
            return Ok(());
        }
        // f32 -> f64
        if src_ty == default_types::f32 && dst_ty == default_types::f64 {
            let src_value = src_value.into_float_value();
            let value = self.build_float_ext(src_value, self.ctx.default_types.f64, "")?;
            self.push_value(dst, value.into());
            return Ok(());
        }
        // f64 -> f32
        if src_ty == default_types::f64 && dst_ty == default_types::f32 {
            let src_value = src_value.into_float_value();
            let value = self.build_float_trunc(src_value, self.ctx.default_types.f32, "")?;
            self.push_value(dst, value.into());
            return Ok(());
        }
        if src_ty.is_int_like() && dst_ty.is_int_like() {
            let isize_bitwidth = self.ctx.default_types.isize.get_bit_width();
            let trunc = src_ty.get_bitwidth(isize_bitwidth) > dst_ty.get_bitwidth(isize_bitwidth);
            let ty = self
                .ctx
                .context
                .custom_width_int_type(dst_ty.get_bitwidth(isize_bitwidth));
            let src_value = src_value.into_int_value();
            let value = if trunc && src_value.is_constant_int() {
                src_value.const_truncate_or_bit_cast(ty)
            } else if trunc {
                self.build_int_truncate_or_bit_cast(src_value, ty, "")?
            } else if dst_ty.is_unsigned() {
                self.build_int_z_extend_or_bit_cast(src_value, ty, "")?
            } else {
                self.build_int_s_extend_or_bit_cast(src_value, ty, "")?
            };
            self.push_value(dst, value.into());
            return Ok(());
        }
        if src_ty.is_float() {
            let isize_bitwidth = self.ctx.default_types.isize.get_bit_width();
            let ty = self
                .ctx
                .context
                .custom_width_int_type(dst_ty.get_bitwidth(isize_bitwidth));
            let value = if dst_ty.is_unsigned() {
                self.build_float_to_unsigned_int(src_value.into_float_value(), ty, "")?
            } else {
                self.build_float_to_signed_int(src_value.into_float_value(), ty, "")?
            };
            self.push_value(dst, value.into());
            Ok(())
        } else {
            let ty = match **dst_ty {
                TyKind::PrimitiveF32 => self.ctx.default_types.f32,
                TyKind::PrimitiveF64 => self.ctx.default_types.f64,
                _ => unreachable!("not a float type: {:?}", dst_ty),
            };
            let value = if src_ty.is_unsigned() {
                self.build_unsigned_int_to_float(src_value.into_int_value(), ty, "")?
            } else {
                self.build_signed_int_to_float(src_value.into_int_value(), ty, "")?
            };
            self.push_value(dst, value.into());
            Ok(())
        }
    }

    pub(super) fn build_strip_metadata(
        &mut self,
        dst: ValueId,
        value: &TypedLiteral<'arena>,
    ) -> Result {
        let (value, is_ptr) = if let &TypedLiteral::Dynamic(src) = value {
            (self.scope[src], self.is_stack_allocated(src))
        } else if let &TypedLiteral::Static(id) = value {
            (self.ctx.statics[id].as_pointer_value().into(), true)
        } else {
            (self.basic_value(value), false)
        };
        let pointer = if is_ptr {
            let pointer_pointer = self.build_struct_gep(
                self.ctx.default_types.fat_ptr,
                value.into_pointer_value(),
                0,
                "",
            )?;
            self.build_deref(pointer_pointer, self.substitute(self.get_ty(dst)), false)?
        } else {
            self.build_extract_value(value.into_struct_value(), 0, "")?
        };
        self.push_value(dst, pointer);
        Ok(())
    }

    pub(super) fn build_unsize_slice(
        &mut self,
        dst: ValueId,
        value: &TypedLiteral<'arena>,
        size: u64,
    ) -> Result {
        let defty = &self.ctx.default_types;
        let value = self.basic_value(value).into_pointer_value();
        let const_val = if value.is_const() {
            value
        } else {
            defty.ptr.get_poison()
        };
        let mut fat_ptr = defty
            .fat_ptr
            .const_named_struct(&[const_val.into(), defty.isize.const_int(size, false).into()]);
        if !value.is_const() {
            fat_ptr = self
                .build_insert_value(fat_ptr, value, 0, "")?
                .into_struct_value();
        }
        self.push_value(dst, fat_ptr.into());

        Ok(())
    }

    pub(super) fn build_attach_vtable(
        &mut self,
        dst: ValueId,
        value: &TypedLiteral<'arena>,
        vtable: &(Ty<'arena>, Box<[TraitId]>),
    ) -> Result {
        let vtable_ptr = self.ctx.get_vtable(vtable).as_pointer_value();
        let metadata = self
            .build_bit_cast(vtable_ptr, self.ctx.default_types.isize, "")?
            .into_int_value();

        let ptr = self.basic_value(value).into_pointer_value();

        let mut data = [
            self.ctx.default_types.ptr.const_zero().into(),
            self.ctx.default_types.isize.const_zero().into(),
        ];
        if ptr.is_const() {
            data[0] = ptr.into();
        }
        if metadata.is_const() {
            data[1] = metadata.into();
        }
        let mut fat_ptr = self.ctx.default_types.fat_ptr.const_named_struct(&data);
        if !ptr.is_const() {
            fat_ptr = self
                .build_insert_value(fat_ptr, ptr, 0, "")?
                .into_struct_value();
        }
        if !metadata.is_const() {
            fat_ptr = self
                .build_insert_value(fat_ptr, metadata, 1, "")?
                .into_struct_value();
        }

        self.push_value(dst, fat_ptr.into());
        Ok(())
    }
}
