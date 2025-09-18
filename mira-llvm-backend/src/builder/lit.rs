use inkwell::{
    types::{AnyTypeEnum, BasicType, BasicTypeEnum},
    values::{BasicValueEnum, GlobalValue},
};
use mira_typeck::ir::TypedLiteral;

use super::FunctionCodegenContext;

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
    pub(crate) fn basic_value(&self, lit: &TypedLiteral<'arena>) -> BasicValueEnum<'ctx> {
        let defty = &self.ctx.default_types;
        match lit {
            TypedLiteral::Void => defty.empty_struct.const_zero().into(),
            &TypedLiteral::Dynamic(id) => self.get_value(id),
            TypedLiteral::Function(id, generics) => {
                // TODO: Monomorphisation
                assert!(generics.is_empty());
                self.ctx.functions[*id]
                    .as_global_value()
                    .as_pointer_value()
                    .into()
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
            TypedLiteral::String(global_str) => {
                let ptr = self.ctx.string_map[global_str].as_pointer_value().into();
                let size = global_str.len();
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
                .basic_type(*self.substitute(*ty))
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
                    let ty = self.basic_type(*self.substitute(*ty));
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
                let mut const_value = match self.basic_type(*self.substitute(*ty)) {
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
            TypedLiteral::Struct(struct_id, values) => {
                if values.is_empty() {
                    return self.ctx.structs[*struct_id].const_named_struct(&[]).into();
                }
                let mut non_const_value = Vec::new();
                let mut const_value = self.ctx.structs[*struct_id].const_named_struct(
                    &values
                        .iter()
                        .enumerate()
                        .map(|(i, v)| {
                            let val = self.basic_value(v);
                            if Self::is_const(&val) {
                                val
                            } else {
                                let poison_val = Self::poison_val(val.get_type());
                                non_const_value.push((i, val));
                                poison_val
                            }
                        })
                        .collect::<Vec<_>>(),
                );
                for v in non_const_value.drain(..) {
                    const_value = self
                        .build_insert_value(const_value, v.1, v.0 as u32, "")
                        .expect("integer should never be out of range")
                        .into_struct_value();
                }
                const_value.into()
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
