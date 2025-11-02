use inkwell::{
    types::BasicType,
    values::{BasicValue, BasicValueEnum, PointerValue},
};
use mira_typeck::{
    Ty, TyKind, default_types,
    ir::{OffsetValue, TypedLiteral, ValueId},
};

use super::{FunctionCodegenContext, Result};

fn make_volatile(v: BasicValueEnum<'_>, volatile: bool) -> BasicValueEnum<'_> {
    v.as_instruction_value()
        .expect("there should be an instruction value here")
        .set_volatile(volatile)
        .expect("setting volatile should never fail");
    v
}

impl<'ctx, 'arena> FunctionCodegenContext<'ctx, 'arena, '_, '_, '_> {
    pub(crate) fn build_deref(
        &self,
        left_side: PointerValue<'ctx>,
        ty: Ty<'arena>,
        volatile: bool,
    ) -> Result<BasicValueEnum<'ctx>> {
        if ty == default_types::void || ty == default_types::never {
            return Ok(self.basic_value(&TypedLiteral::Void));
        }

        if ty.has_refs() {
            if ty.is_thin_ptr() {
                return Ok(make_volatile(
                    self.build_load(self.ctx.default_types.ptr, left_side, "")?,
                    volatile,
                ));
            } else {
                let actual_ptr = self.build_load(self.ctx.default_types.ptr, left_side, "")?;
                make_volatile(actual_ptr, volatile);
                let offset_ptr =
                    self.build_struct_gep(self.ctx.default_types.fat_ptr, left_side, 1, "")?;
                let metadata = self.build_load(self.ctx.default_types.isize, offset_ptr, "")?;
                make_volatile(metadata, volatile);
                let ptr_only_struct = self.build_insert_value(
                    self.ctx.default_types.fat_ptr.get_poison(),
                    actual_ptr,
                    0,
                    "",
                )?;
                return Ok(self
                    .build_insert_value(ptr_only_struct, metadata, 1, "")?
                    .as_basic_value_enum());
            }
        }

        match &**ty {
            TyKind::Ref(_) | TyKind::PrimitiveNever | TyKind::PrimitiveVoid => unreachable!(),
            TyKind::Generic { .. } | TyKind::PrimitiveSelf => {
                panic!("{ty:?} should be resolved by now")
            }
            TyKind::DynType { .. } | TyKind::UnsizedArray { .. } | TyKind::PrimitiveStr => {
                panic!("cannot dereference unsized type {ty:?}")
            }
            TyKind::Struct { struct_id, .. } => {
                let llvm_structure = self.basic_type(&ty).into_struct_type();
                let structure = &self.structs_reader[*struct_id];
                let mut value = llvm_structure.get_poison();
                for i in 0..structure.elements.len() {
                    let offset_val =
                        self.build_struct_gep(llvm_structure, left_side, i as u32, "")?;
                    let element_val =
                        self.build_deref(offset_val, structure.elements[i].1, volatile)?;
                    value = self
                        .build_insert_value(value, element_val, i as u32, "")?
                        .into_struct_value();
                }
                Ok(value.into())
            }
            TyKind::Tuple(elements) => {
                let llvm_structure = self.basic_type(&ty).into_struct_type();
                let mut value = llvm_structure.get_poison();
                for (i, elem) in elements.iter().enumerate() {
                    let offset_val =
                        self.build_struct_gep(llvm_structure, left_side, i as u32, "")?;
                    let element_val = self.build_deref(offset_val, *elem, volatile)?;
                    value = self
                        .build_insert_value(value, element_val, i as u32, "")?
                        .into_struct_value();
                }
                Ok(value.into())
            }
            TyKind::SizedArray {
                ty,
                number_elements,
                ..
            } => {
                let llvm_element_ty = self.basic_type(ty);
                let mut value = llvm_element_ty
                    .array_type(*number_elements as u32)
                    .get_poison();
                for i in 0..*number_elements {
                    let offset_val = unsafe {
                        self.build_in_bounds_gep(
                            llvm_element_ty,
                            left_side,
                            &[self.ctx.default_types.isize.const_int(i as u64, false)],
                            "",
                        )
                    }?;
                    let element_val = self.build_deref(offset_val, *ty, volatile)?;
                    value = self
                        .build_insert_value(value, element_val, i as u32, "")?
                        .into_array_value();
                }
                Ok(value.into())
            }
            TyKind::Function(..)
            | TyKind::PrimitiveI8
            | TyKind::PrimitiveI16
            | TyKind::PrimitiveI32
            | TyKind::PrimitiveI64
            | TyKind::PrimitiveISize
            | TyKind::PrimitiveU8
            | TyKind::PrimitiveU16
            | TyKind::PrimitiveU32
            | TyKind::PrimitiveU64
            | TyKind::PrimitiveUSize
            | TyKind::PrimitiveF32
            | TyKind::PrimitiveF64
            | TyKind::PrimitiveBool => Ok(make_volatile(
                self.build_load(self.basic_type(&ty), left_side, "")?,
                volatile,
            )),
        }
    }

    pub(crate) fn build_ptr_store(
        &self,
        left_side: PointerValue<'ctx>,
        right_side: BasicValueEnum<'ctx>,
        ty: Ty<'arena>,
        volatile: bool,
    ) -> Result {
        if ty.has_refs() {
            if ty.is_thin_ptr() {
                self.build_store(left_side, right_side)?
                    .set_volatile(volatile)
                    .expect("setting volatile should never fail");
                return Ok(());
            } else {
                let actual_ptr = self.build_extract_value(right_side.into_struct_value(), 0, "")?;
                let metadata = self.build_extract_value(right_side.into_struct_value(), 1, "")?;
                let actual_ptr_ptr = left_side;
                let metadata_ptr =
                    self.build_struct_gep(self.ctx.default_types.fat_ptr, left_side, 1, "")?;
                self.build_store(actual_ptr_ptr, actual_ptr)?
                    .set_volatile(volatile)
                    .expect("setting volatile should never fail");
                self.build_store(metadata_ptr, metadata)?
                    .set_volatile(volatile)
                    .expect("setting volatile should never fail");
                return Ok(());
            }
        }
        match &**ty {
            TyKind::Ref(_) => unreachable!(),
            TyKind::Generic { .. } | TyKind::PrimitiveSelf => {
                panic!("{ty:?} should be resolved by now")
            }
            TyKind::UnsizedArray { .. } | TyKind::DynType { .. } | TyKind::PrimitiveStr => {
                panic!("cannot store unsized type {ty:?}")
            }
            TyKind::PrimitiveNever | TyKind::PrimitiveVoid => (),
            TyKind::Struct { struct_id, .. } => {
                let structure = &self.structs_reader[*struct_id];
                let llvm_ty = self.basic_type(&ty);
                for (idx, ty) in structure.elements.iter().map(|v| &v.1).enumerate() {
                    let val =
                        self.build_extract_value(right_side.into_struct_value(), idx as u32, "")?;
                    let ptr = self.build_struct_gep(llvm_ty, left_side, idx as u32, "")?;
                    self.build_ptr_store(ptr, val, *ty, volatile)?;
                }
            }
            TyKind::Tuple(elements) => {
                let llvm_ty = self.basic_type(&ty);
                for (idx, ty) in elements.iter().enumerate() {
                    let val =
                        self.build_extract_value(right_side.into_struct_value(), idx as u32, "")?;
                    let ptr = self.build_struct_gep(llvm_ty, left_side, idx as u32, "")?;
                    self.build_ptr_store(ptr, val, *ty, volatile)?;
                }
            }
            TyKind::SizedArray {
                ty,
                number_elements,
                ..
            } => {
                let llvm_ty = self.basic_type(ty);
                for i in 0..*number_elements {
                    let val =
                        self.build_extract_value(right_side.into_array_value(), i as u32, "")?;
                    let ptr = unsafe {
                        self.build_in_bounds_gep(
                            llvm_ty,
                            left_side,
                            &[self.ctx.default_types.isize.const_int(i as u64, false)],
                            "",
                        )
                    }?;
                    self.build_ptr_store(ptr, val, *ty, volatile)?;
                }
            }
            TyKind::Function(..)
            | TyKind::PrimitiveI8
            | TyKind::PrimitiveI16
            | TyKind::PrimitiveI32
            | TyKind::PrimitiveI64
            | TyKind::PrimitiveISize
            | TyKind::PrimitiveU8
            | TyKind::PrimitiveU16
            | TyKind::PrimitiveU32
            | TyKind::PrimitiveU64
            | TyKind::PrimitiveUSize
            | TyKind::PrimitiveF32
            | TyKind::PrimitiveF64
            | TyKind::PrimitiveBool => {
                self.build_store(left_side, right_side)?
                    .set_volatile(volatile)
                    .expect("setting volatile should never fail");
            }
        }
        Ok(())
    }

    pub(crate) fn build_store_assign(
        &mut self,
        ptr: &TypedLiteral<'arena>,
        value: &TypedLiteral<'arena>,
    ) -> Result {
        match *value {
            TypedLiteral::Dynamic(id) if self.is_stack_allocated(id) => {
                let ty = self.substitute(self.get_ty(id));
                let llvm_ty = self.basic_type(*ty);
                let alignment = ty.alignment(self.pointer_size, &self.structs_reader);
                self.build_memmove(
                    self.basic_value(ptr).into_pointer_value(),
                    alignment,
                    self.get_value_ptr(id),
                    alignment,
                    llvm_ty.size_of().expect("llvm type should always be sized"),
                )?;
                return Ok(());
            }
            TypedLiteral::Static(id) => {
                let ty = self.ctx.tc_ctx.statics.read()[id].ty;
                let llvm_ty = self.basic_type(&ty);
                let alignment = ty.alignment(self.pointer_size, &self.structs_reader);
                self.build_memmove(
                    self.basic_value(ptr).into_pointer_value(),
                    alignment,
                    self.ctx.statics[id].as_pointer_value(),
                    alignment,
                    llvm_ty.size_of().expect("llvm type should always be sized"),
                )?;
                return Ok(());
            }
            _ => {}
        }
        self.build_ptr_store(
            self.basic_value(ptr).into_pointer_value(),
            self.basic_value(value),
            if let TypedLiteral::Function(..) | TypedLiteral::ExternalFunction(..) = value {
                default_types::usize
            } else {
                self.substitute(value.to_type(self.ir.scope(), self.ctx.tc_ctx))
            },
            false,
        )
    }

    pub(crate) fn build_reference(&mut self, dst: ValueId, value: &TypedLiteral<'arena>) -> Result {
        let value = match value {
            TypedLiteral::Void => self.ctx.default_types.ptr.const_zero().into(),
            &TypedLiteral::Dynamic(id) if self.get_ty(id).is_voidlike() => self
                .ctx
                .default_types
                .isize
                .const_int(1, false)
                .const_to_pointer(self.ctx.default_types.ptr)
                .into(),
            &TypedLiteral::Dynamic(id) if !self.is_stack_allocated(id) => {
                panic!("_{id} is not stack allocated (even tho it should have been)")
            }
            TypedLiteral::Dynamic(id) => self.get_value_ptr(*id).into(),
            TypedLiteral::Static(id) => self.ctx.statics[*id].as_basic_value_enum(),
            _ => panic!(
                "Cannot take a reference to {value:?} (the typechecker should have put this into a stack-allocated dynamic)"
            ),
        };
        self.push_value(dst, value);
        Ok(())
    }

    pub(crate) fn build_dereference(
        &mut self,
        dst: ValueId,
        value: &TypedLiteral<'arena>,
    ) -> Result {
        let ty = self.substitute(self.get_ty(dst));
        if self.is_stack_allocated(dst) {
            let alignment = ty.alignment(self.pointer_size, &self.structs_reader);
            let ty = self.basic_type(&ty);
            let lhs_ptr = self.build_alloca(ty, "")?;
            self.build_memmove(
                lhs_ptr, // dest (dst = *rhs), in this case *dst =
                // *rhs as lhs is stack-allocated, meaning an implicit store has to be
                // added.
                alignment,
                self.basic_value(value).into_pointer_value(),
                alignment,
                ty.size_of().expect("a type should *always* be sized"),
            )?;
            self.push_value_raw(dst, lhs_ptr);
            return Ok(());
        }
        let value = self.build_deref(
            self.basic_value(value).into_pointer_value(), // dst = *rhs
            // we take the type of lhs as it expects the type of the
            // value *after* dereferencing as any pointer in llvm is represented as the
            // same type, &i32 == &&u64 (`ptr`)
            ty,
            false,
        )?;
        self.push_value(dst, value);
        Ok(())
    }

    pub(super) fn build_offset(
        &mut self,
        dst: ValueId,
        ptr: &TypedLiteral<'arena>,
        offset: OffsetValue,
    ) -> Result {
        let ty = ptr
            .to_type(self.ir.scope(), self.ctx.tc_ctx)
            .deref()
            .expect("non-pointer values cannot be offset");
        let ty = self.substitute(ty);
        match **ty {
            TyKind::Struct { struct_id, .. } => {
                let offset = match offset {
                    OffsetValue::Dynamic(_) => unreachable!("dynamic struct offset"),
                    OffsetValue::Static(v) => self.ctx.default_types.i32.const_int(v as u64, false),
                };
                let value = unsafe {
                    self.build_in_bounds_gep(
                        self.ctx.structs[struct_id],
                        self.basic_value(ptr).into_pointer_value(),
                        &[self.ctx.default_types.isize.const_int(0, false), offset],
                        "",
                    )
                }?;
                self.push_value(dst, value.into());
                Ok(())
            }
            TyKind::Tuple { .. } => {
                let offset = match offset {
                    OffsetValue::Dynamic(_) => unreachable!("dynamic struct offset"),
                    OffsetValue::Static(v) => self.ctx.default_types.i32.const_int(v as u64, false),
                };
                let value = unsafe {
                    self.build_in_bounds_gep(
                        self.basic_type(&ty),
                        self.basic_value(ptr).into_pointer_value(),
                        &[self.ctx.default_types.isize.const_int(0, false), offset],
                        "",
                    )
                }?;
                self.push_value(dst, value.into());
                Ok(())
            }
            TyKind::SizedArray { ty, .. } => {
                let offset = match offset {
                    OffsetValue::Dynamic(id) => self.get_value(id).into_int_value(),
                    OffsetValue::Static(v) => self.ctx.default_types.i32.const_int(v as u64, false),
                };
                let value = unsafe {
                    self.build_in_bounds_gep(
                        self.basic_type(&ty),
                        self.basic_value(ptr).into_pointer_value(),
                        &[offset],
                        "",
                    )
                }?;
                self.push_value(dst, value.into());
                Ok(())
            }
            TyKind::UnsizedArray(ty) => {
                let offset = match offset {
                    OffsetValue::Dynamic(id) => self.get_value(id).into_int_value(),
                    OffsetValue::Static(v) => self.ctx.default_types.i32.const_int(v as u64, false),
                };
                let actual_ptr = self
                    .build_extract_value(self.basic_value(ptr).into_struct_value(), 0, "")?
                    .into_pointer_value();
                let value = unsafe {
                    self.build_in_bounds_gep(self.basic_type(&ty), actual_ptr, &[offset], "")
                }?;
                self.push_value(dst, value.into());
                Ok(())
            }
            _ => unreachable!("cannot take offset of {ty:?}"),
        }
    }
}
