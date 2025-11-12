use inkwell::{
    IntPredicate,
    builder::BuilderError,
    types::{AnyTypeEnum, BasicType, BasicTypeEnum},
    values::{BasicValueEnum, GlobalValue, PointerValue},
};
use mira_typeck::ir::TypedLiteral;

use crate::FnInstance;

use super::FunctionCodegenContext;

fn align_up(ptr: u64, alignment: u32) -> u64 {
    if ptr.is_multiple_of(alignment as u64) {
        ptr
    } else {
        (ptr & !(alignment as u64 - 1)) + alignment as u64
    }
}

fn global_value_ty(global: GlobalValue<'_>) -> BasicTypeEnum<'_> {
    match global.get_value_type() {
        AnyTypeEnum::ArrayType(ty) => ty.as_basic_type_enum(),
        AnyTypeEnum::FloatType(ty) => ty.as_basic_type_enum(),
        AnyTypeEnum::FunctionType(_) => unreachable!("a static isn't a function"),
        AnyTypeEnum::IntType(ty) => ty.as_basic_type_enum(),
        AnyTypeEnum::PointerType(ty) => ty.as_basic_type_enum(),
        AnyTypeEnum::StructType(ty) => ty.as_basic_type_enum(),
        AnyTypeEnum::VectorType(_) | AnyTypeEnum::ScalableVectorType(_) => {
            unreachable!("simd vectors aren't supported")
        }
        AnyTypeEnum::VoidType(_) => unreachable!("a static should never be void"),
    }
}

impl<'ctx, 'arena> FunctionCodegenContext<'ctx, 'arena, '_, '_, '_> {
    /// Stores the lit into the ptr.
    ///
    /// distinct: the ptr does not point to the same value as lit could. This means, that:
    /// - if lit is TypedLiteral::Static, ptr does not point to that static
    /// - if lit is TypedLiteral::Dynamic, and stack-allocated, ptr does not point to that alloca
    pub(crate) fn basic_value_ptr(
        &mut self,
        lit: &TypedLiteral<'arena>,
        ptr: PointerValue<'ctx>,
        distinct: bool,
    ) -> Result<(), BuilderError> {
        macro_rules! lit_store {
            ($ty:ident . $const_fn:ident($($arg:expr),* $(,)?)) => {
                self.build_store(ptr, self.ctx.default_types.$ty.$const_fn($($arg),*))
                    .map(|_| ())
            };
        }

        match lit {
            TypedLiteral::Void => Ok(()),
            &TypedLiteral::Dynamic(value) if self.is_stack_allocated(value) => {
                let src = self.get_value_ptr(value);
                let ty = self.get_ty(value);
                let (size, align) =
                    ty.size_and_alignment(self.pointer_size, &self.structs_reader, self.ty_cx());
                let size = self.ctx.default_types.isize.const_int(size, false);
                if distinct {
                    self.build_memcpy(ptr, align, src, align, size)?;
                } else {
                    self.build_memmove(ptr, align, src, align, size)?;
                }
                Ok(())
            }
            &TypedLiteral::Dynamic(value) => self
                .build_ptr_store(ptr, self.get_value(value), self.get_ty(value), false)
                .map(|_| ()),
            &TypedLiteral::Function(fn_id, generics) => {
                let value = self
                    .ctx
                    .get_fn_instance(FnInstance::new_poly(fn_id, generics))
                    .as_global_value();
                self.build_store(ptr, value).map(|_| ())
            }
            &TypedLiteral::ExternalFunction(fn_id) => {
                let value = self.ctx.external_functions[fn_id].value.as_global_value();
                self.build_store(ptr, value).map(|_| ())
            }
            &TypedLiteral::Static(static_id) => {
                let value = self.ctx.statics[static_id].as_pointer_value();
                let ty = self.ctx.tc_ctx.statics.read()[static_id].ty;
                let (size, align) =
                    ty.size_and_alignment(self.pointer_size, &self.structs_reader, self.ty_cx());
                let size = self.ctx.default_types.isize.const_int(size, false);
                if distinct {
                    self.build_memcpy(ptr, align, value, align, size)?;
                } else {
                    self.build_memmove(ptr, align, value, align, size)?;
                }
                Ok(())
            }
            &TypedLiteral::String(symbol) => {
                let value = self.ctx.get_string(symbol);
                let strlen = symbol.len() as u64;
                let strlen = self.ctx.default_types.isize.const_int(strlen, false);
                // store the pointer part of the fat pointer
                self.build_store(ptr, value)?;
                // get the metadata pointer
                let metadata_ptr =
                    self.build_struct_gep(self.ctx.default_types.fat_ptr, ptr, 1, "")?;
                self.build_store(metadata_ptr, strlen)?;
                Ok(())
            }
            // zst's don't have to be stored cuz there's nothing to store
            TypedLiteral::Array(ty, _) | TypedLiteral::ArrayInit(ty, _, _)
                if ty.is_zst(&self.structs_reader) =>
            {
                Ok(())
            }
            TypedLiteral::Array(_, lits) if lits.is_empty() => Ok(()),
            TypedLiteral::Array(ty, lits) => {
                let basic_ty = self.basic_ty(ty);
                for (idx, lit) in lits.iter().enumerate() {
                    let idx_int = self.ctx.default_types.isize.const_int(idx as u64, false);
                    let ptr = unsafe { self.build_in_bounds_gep(basic_ty, ptr, &[idx_int], "") }?;
                    self.basic_value_ptr(lit, ptr, distinct)?;
                }
                Ok(())
            }
            // [_; 0] is a zst, nothing has to be stored.
            TypedLiteral::ArrayInit(_, _, 0) => Ok(()),
            // [lit; 1] is essentially the same as just lit.
            TypedLiteral::ArrayInit(_, lit, 1) => self.basic_value_ptr(lit, ptr, distinct),
            TypedLiteral::ArrayInit(ty, lit, amount) => {
                /*
                <start>:
                    br %repeat_loop_header

                repeat_loop_header:
                    %0 = phi isize [0, %<start>], [%3, %repeat_loop_body]
                    %1 = icmp ult i64 %0, $amount ; <- this comes from the compiler.
                    br i1 %1, label %repeat_loop_body, label %repeat_loop_end
                repeat_loop_body:
                    %2 = getelementptr inbounds $ty, ptr $ptr, i64 %0
                    $self.basic_value_ptr($lit, %ptr, $distinct)
                    %3 = add nuw i64 %0, 1
                    br label %repeat_loop_body
                repeat_loop_end:
                */

                let [header, body, end] =
                    self.make_bb(["repeat_loop_header", "repeat_loop_body", "repeat_loop_end"]);
                let start = self.current_block;
                self.build_unconditional_branch(header)?;
                self.goto(header);
                // build `phi isize` and add `[0, %<start>]`
                let idx = self.build_phi(self.ctx.default_types.isize, "")?;
                let idx_int = idx.as_basic_value().into_int_value();

                let const_zero = self.ctx.default_types.isize.const_zero();
                idx.add_incoming(&[(&const_zero, start)]);

                let const_len = self
                    .ctx
                    .default_types
                    .isize
                    .const_int(*amount as u64, false);
                let is_in_loop =
                    self.build_int_compare(IntPredicate::ULT, idx_int, const_len, "")?;
                self.build_conditional_branch(is_in_loop, body, end)?;

                self.goto(body);
                let ty = self.basic_ty(ty);
                let ptr = unsafe { self.build_in_bounds_gep(ty, ptr, &[idx_int], "") }?;

                self.basic_value_ptr(lit, ptr, distinct)?;
                let const_one = self.ctx.default_types.isize.const_int(1, false);
                let new_idx = self.build_int_add(idx_int, const_one, "")?;
                idx.add_incoming(&[(&new_idx, body)]);
                self.build_unconditional_branch(header)?;

                self.goto(end);
                Ok(())
            }
            TypedLiteral::Tuple(lits) => {
                if !lits.is_empty() {
                    let mut offset = 0;
                    let ptrsize = self.pointer_size;

                    for lit in lits {
                        let (size, align) = lit
                            .to_type(self.ir.scope(), self.ctx.tc_ctx)
                            .size_and_alignment(ptrsize, &self.structs_reader, self.ty_cx());
                        offset = align_up(offset, align);
                        let idx_int = self.ctx.default_types.isize.const_int(offset, false);
                        offset += size;
                        let ptr = unsafe {
                            self.build_in_bounds_gep(self.ctx.default_types.i8, ptr, &[idx_int], "")
                        }?;
                        self.basic_value_ptr(lit, ptr, distinct)?;
                    }
                }
                Ok(())
            }
            TypedLiteral::Struct(_, _, lits) => {
                if !lits.is_empty() {
                    let mut offset = 0;
                    let ptrsize = self.pointer_size;

                    for lit in lits {
                        let (size, align) = lit
                            .to_type(self.ir.scope(), self.ctx.tc_ctx)
                            .size_and_alignment(ptrsize, &self.structs_reader, self.ty_cx());
                        offset = align_up(offset, align);
                        let idx_int = self.ctx.default_types.isize.const_int(offset, false);
                        offset += size;
                        let ptr = unsafe {
                            self.build_in_bounds_gep(self.ctx.default_types.i8, ptr, &[idx_int], "")
                        }?;
                        self.basic_value_ptr(lit, ptr, distinct)?;
                    }
                }
                Ok(())
            }
            &TypedLiteral::F64(v) => lit_store!(f32.const_float(v)),
            &TypedLiteral::F32(v) => lit_store!(f32.const_float(v as f64)),
            &TypedLiteral::U8(v) => lit_store!(i8.const_int(v as u64, true)),
            &TypedLiteral::U16(v) => lit_store!(i16.const_int(v as u64, true)),
            &TypedLiteral::U32(v) => lit_store!(i32.const_int(v as u64, true)),
            &TypedLiteral::U64(v) => lit_store!(i64.const_int(v, true)),
            &TypedLiteral::USize(v) => lit_store!(isize.const_int(v as i64 as u64, true)),
            &TypedLiteral::I8(v) => lit_store!(i8.const_int(v as i64 as u64, true)),
            &TypedLiteral::I16(v) => lit_store!(i16.const_int(v as i64 as u64, true)),
            &TypedLiteral::I32(v) => lit_store!(i32.const_int(v as i64 as u64, true)),
            &TypedLiteral::I64(v) => lit_store!(i64.const_int(v as u64, true)),
            &TypedLiteral::ISize(v) => lit_store!(isize.const_int(v as i64 as u64, true)),
            &TypedLiteral::Bool(v) => lit_store!(bool.const_int(v as u64, false)),
            TypedLiteral::Intrinsic(..) | TypedLiteral::LLVMIntrinsic(..) => {
                unreachable!("intrinsics and llvm intrinsics cannot be stored as a literal")
            }
        }
    }

    pub(crate) fn basic_value(&self, lit: &TypedLiteral<'arena>) -> BasicValueEnum<'ctx> {
        let defty = &self.ctx.default_types;
        match lit {
            TypedLiteral::Void => defty.empty_struct.const_zero().into(),
            &TypedLiteral::Dynamic(id) => self.get_value(id),
            &TypedLiteral::Function(id, generics) => {
                let func = self.ctx.get_fn_instance(FnInstance::new_poly(id, generics));
                func.as_global_value().as_pointer_value().into()
            }
            TypedLiteral::ExternalFunction(id) => self.ctx.external_functions[*id]
                .as_global_value()
                .as_pointer_value()
                .into(),
            TypedLiteral::Static(id) => {
                let static_value = self.ctx.statics[*id];
                self.build_load(
                    global_value_ty(static_value),
                    static_value.as_pointer_value(),
                    "",
                )
                .expect("the type should always match")
            }
            &TypedLiteral::String(sym) => {
                let ptr = self.ctx.get_string(sym).as_pointer_value().into();
                let size = sym.len();
                let len_const = self
                    .ctx
                    .default_types
                    .isize
                    .const_int(size as u64, false)
                    .into();
                self.ctx
                    .default_types
                    .fat_ptr
                    .const_named_struct(&[ptr, len_const])
                    .into()
            }

            TypedLiteral::ArrayInit(ty, _, 0) => self
                .basic_ty(*self.substitute(*ty))
                .array_type(0)
                .const_zero()
                .into(),
            TypedLiteral::ArrayInit(_, elem, amount) => {
                let elem = self.basic_value(elem);
                let mut array_value = elem.get_type().array_type(*amount as u32).const_zero();
                for i in 0..*amount {
                    array_value = self
                        .build_insert_value(array_value, elem, i as u32, "")
                        .expect("i should never be out of bounds")
                        .into_array_value();
                }

                array_value.into()
            }
            TypedLiteral::Array(ty, elements) => {
                if elements.is_empty() {
                    let ty = self.basic_ty(*self.substitute(*ty));
                    return ty.array_type(0).const_zero().into();
                }
                let mut insert_value_vec: Vec<(usize, BasicValueEnum<'ctx>)> = Vec::new();

                macro_rules! array_const_value {
                    ($ty:expr,$into_val_fn:ident) => {{
                        let mut const_elements = Vec::new();
                        for (i, v) in elements.iter().enumerate() {
                            let val = self.basic_value(v).$into_val_fn();
                            if val.is_const() {
                                const_elements.push(val);
                            } else {
                                insert_value_vec.push((i, val.into()));
                                const_elements.push($ty.get_poison());
                            }
                        }
                        $ty.const_array(&const_elements)
                    }};
                }
                let mut const_value = match self.basic_ty(*self.substitute(*ty)) {
                    BasicTypeEnum::ArrayType(array_type) => {
                        array_const_value!(array_type, into_array_value)
                    }
                    BasicTypeEnum::FloatType(float_type) => {
                        array_const_value!(float_type, into_float_value)
                    }
                    BasicTypeEnum::IntType(int_type) => {
                        array_const_value!(int_type, into_int_value)
                    }
                    BasicTypeEnum::PointerType(pointer_type) => {
                        array_const_value!(pointer_type, into_pointer_value)
                    }
                    BasicTypeEnum::StructType(struct_type) => {
                        array_const_value!(struct_type, into_struct_value)
                    }
                    BasicTypeEnum::VectorType(..) | BasicTypeEnum::ScalableVectorType(..) => {
                        unreachable!("vector types arent supported")
                    }
                };

                for v in insert_value_vec.drain(..) {
                    const_value = self
                        .build_insert_value(const_value, v.1, v.0 as u32, "")
                        .expect("integer should never be out of range")
                        .into_array_value();
                }
                const_value.into()
            }
            TypedLiteral::Struct(_, _, _) => {
                // TODO: UNREACHABLE >:c
                unreachable!();

                // if values.is_empty() {
                //     return self.ctx.structs[struct_id].const_named_struct(&[]).into();
                // }
                // let mut non_const_value = Vec::new();
                // let mut const_value = self.ctx.structs[*struct_id].const_named_struct(
                //     &values
                //         .iter()
                //         .enumerate()
                //         .map(|(i, v)| {
                //             let val = self.basic_value(v);
                //             if Self::is_const(&val) {
                //                 val
                //             } else {
                //                 let poison_val = Self::poison_val(val.get_type());
                //                 non_const_value.push((i, val));
                //                 poison_val
                //             }
                //         })
                //         .collect::<Vec<_>>(),
                // );
                // for v in non_const_value.drain(..) {
                //     const_value = self
                //         .build_insert_value(const_value, v.1, v.0 as u32, "")
                //         .expect("integer should never be out of range")
                //         .into_struct_value();
                // }
                // const_value.into()
            }
            TypedLiteral::Tuple(values) => {
                let mut elems = Vec::with_capacity(values.len());
                let mut elem_types = Vec::with_capacity(values.len());
                for v in values {
                    let val = self.basic_value(v);
                    elem_types.push(val.get_type());
                    elems.push(val);
                }
                let mut value = self
                    .ctx
                    .context
                    .struct_type(&elem_types, false)
                    .const_zero();
                for (i, elem) in elems.into_iter().enumerate() {
                    value = self
                        .build_insert_value(value, elem, i as u32, "")
                        .expect("integer should never be out of range")
                        .into_struct_value();
                }
                value.into()
            }
            TypedLiteral::F64(v) => defty.f64.const_float(*v).into(),
            TypedLiteral::F32(v) => defty.f32.const_float(*v as f64).into(),
            TypedLiteral::U8(v) => defty.i8.const_int(*v as u64, false).into(),
            TypedLiteral::U16(v) => defty.i16.const_int(*v as u64, false).into(),
            TypedLiteral::U32(v) => defty.i32.const_int(*v as u64, false).into(),
            TypedLiteral::U64(v) => defty.i64.const_int(*v, false).into(),
            TypedLiteral::USize(v) => defty.isize.const_int(*v as u64, false).into(),
            TypedLiteral::I8(v) => defty.i8.const_int(*v as u64, false).into(),
            TypedLiteral::I16(v) => defty.i16.const_int(*v as u64, false).into(),
            TypedLiteral::I32(v) => defty.i32.const_int(*v as u64, false).into(),
            TypedLiteral::I64(v) => defty.i64.const_int(*v as u64, false).into(),
            TypedLiteral::ISize(v) => defty.isize.const_int(*v as u64, false).into(),
            TypedLiteral::Bool(v) => defty.bool.const_int(*v as u64, false).into(),
            TypedLiteral::LLVMIntrinsic(..) | TypedLiteral::Intrinsic(..) => {
                unreachable!("intrinsics can only be used as part of intrinsic call")
            }
        }
    }
}
