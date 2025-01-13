use context::DefaultTypes;
use core::panic;
use std::{borrow::Cow, collections::HashMap};

use crate::{
    globals::GlobalStr,
    typechecking::{
        expression::{OffsetValue, TypecheckedExpression, TypedLiteral},
        intrinsics::Intrinsic,
        typechecking::ScopeTypeMetadata,
        Type, TypecheckingContext,
    },
};
pub use inkwell::context::Context as InkwellContext;
pub use inkwell::targets::TargetTriple;
pub mod mangling;
pub use context::CodegenContext;
pub use error::CodegenError;
pub use inkwell::support::LLVMString;
use inkwell::{
    builder::{Builder, BuilderError},
    context::Context,
    module::Module,
    types::{AnyTypeEnum, BasicType, BasicTypeEnum, StructType},
    values::{BasicValue, BasicValueEnum, FunctionValue, GlobalValue, PointerValue},
    FloatPredicate, IntPredicate,
};

mod context;
mod error;

impl<'ctx, 'me> CodegenContext<'ctx> {
    pub fn make_function_codegen_context(
        &'me self,
        tc_scope: Vec<(Type, ScopeTypeMetadata)>,
        current_fn: FunctionValue<'ctx>,
    ) -> FunctionCodegenContext<'ctx, 'me> {
        FunctionCodegenContext {
            tc_scope,
            tc_ctx: &self.tc_ctx,
            _scope: Vec::new(),
            builder: &self.builder,
            context: self.context,
            default_types: self.default_types,
            module: &self.module,
            current_fn,
            functions: &self.functions,
            external_functions: &self.external_functions,
            structs: &self.structs,
            statics: &self.statics,
            string_map: &self.string_map,
        }
    }
}

pub struct FunctionCodegenContext<'ctx, 'codegen> {
    tc_scope: Vec<(Type, ScopeTypeMetadata)>,
    tc_ctx: &'codegen TypecheckingContext,
    _scope: Vec<BasicValueEnum<'ctx>>,
    builder: &'codegen Builder<'ctx>,
    context: &'codegen Context,
    default_types: DefaultTypes<'ctx>,
    module: &'codegen Module<'ctx>,
    current_fn: FunctionValue<'ctx>,
    functions: &'codegen Vec<FunctionValue<'ctx>>,
    external_functions: &'codegen Vec<FunctionValue<'ctx>>,
    structs: &'codegen Vec<StructType<'ctx>>,
    statics: &'codegen Vec<GlobalValue<'ctx>>,
    string_map: &'codegen HashMap<GlobalStr, GlobalValue<'ctx>>,
}

impl<'ctx> FunctionCodegenContext<'ctx, '_> {
    /// In case you already made an alloca for the value, as push_value does an alloca and store if
    /// the value is stack allocated.
    pub fn push_value_raw(&mut self, id: usize, value: BasicValueEnum<'ctx>) {
        if self._scope.len() != id {
            panic!(
                "inserting values out-of-order, expected value _{}, but got value _{id}",
                self._scope.len()
            );
        }
        self._scope.push(value);
    }

    pub fn push_value(&mut self, id: usize, value: BasicValueEnum<'ctx>) {
        if self._scope.len() != id {
            panic!(
                "inserting values out-of-order, expected value _{}, but got value _{id}",
                self._scope.len()
            );
        }
        if !self.tc_scope[id].1.stack_allocated {
            self._scope.push(value);
            return;
        }
        let allocated_value = self
            .builder
            .build_alloca(
                self.tc_scope[id]
                    .0
                    .to_llvm_basic_type(&self.default_types, self.structs),
                "",
            )
            .expect("failed to build alloca for a stack allocated value");
        build_ptr_store(allocated_value, value, &self.tc_scope[id].0, self)
            .expect("failed to build store to store a basic value into a stack allocated value");
        self._scope.push(allocated_value.into());
    }

    // gets a scoped value, dereferencing it if it is stack allocated.
    pub fn get_value(&self, id: usize) -> BasicValueEnum<'ctx> {
        if self.tc_scope.len() <= id {
            panic!(
                "cannot get invalid value _{id} (len: {})",
                self.tc_scope.len()
            );
        }
        if self._scope.len() <= id {
            panic!("cannot get not-yet-defined value _{id}");
        }
        if self.tc_scope[id].1.stack_allocated {
            let ptr = self._scope[id].into_pointer_value();
            build_deref(ptr, &self.tc_scope[id].0, self)
                .expect("failed to build a dereference for a stack allocated value")
        } else {
            self._scope[id]
        }
    }

    // gets the pointer to a stack allocated value and panics if the value isn't stack allocated
    pub fn get_value_ptr(&self, id: usize) -> PointerValue<'ctx> {
        if self.tc_scope.len() <= id {
            panic!("cannot get invalid value _{id}");
        }
        if self._scope.len() <= id {
            panic!("cannot get not-yet-defined value _{id}");
        }
        if !self.tc_scope[id].1.stack_allocated {
            panic!("cannot get pointer to non-stackallocated value _{id}");
        }
        self._scope[id].into_pointer_value()
    }
}

impl<'ctx> Type {
    fn to_llvm_basic_type(
        &self,
        default_types: &DefaultTypes<'ctx>,
        structs: &Vec<StructType<'ctx>>,
    ) -> BasicTypeEnum<'ctx> {
        if self.refcount() > 0 {
            if self.is_thin_ptr() {
                return default_types.ptr.into();
            } else {
                return default_types.fat_ptr.into();
            }
        }

        match self {
            Type::Generic(..) | Type::Trait { .. } => {
                unreachable!("generics should be resolved by now")
            }
            Type::UnsizedArray { .. } => panic!("llvm types must be sized, `[_]` is not"),
            Type::PrimitiveStr(_) => panic!("llvm types must be sized, `str` is not"),
            Type::PrimitiveSelf(_) => unreachable!("Self must be resolved at this point"),
            Type::DynType { .. } => panic!("llvm types must be sized, `dyn _` is not"),
            Type::Struct { struct_id, .. } => structs[*struct_id].into(),
            Type::SizedArray {
                typ,
                number_elements,
                ..
            } => typ
                .to_llvm_basic_type(default_types, structs)
                .array_type(*number_elements as u32)
                .into(),
            // our function types are always pointers because all function types are pointers in llvm
            Type::Function(..) => default_types.ptr.into(),
            Type::PrimitiveNever | Type::PrimitiveVoid(_) => panic!(
                "void and never should be ignored as llvm types outside of function return values"
            ),
            Type::PrimitiveU8(_) | Type::PrimitiveI8(_) => default_types.i8.into(),
            Type::PrimitiveU16(_) | Type::PrimitiveI16(_) => default_types.i16.into(),
            Type::PrimitiveU32(_) | Type::PrimitiveI32(_) => default_types.i32.into(),
            Type::PrimitiveU64(_) | Type::PrimitiveI64(_) => default_types.i64.into(),
            Type::PrimitiveUSize(_) | Type::PrimitiveISize(_) => default_types.isize.into(),
            Type::PrimitiveF32(_) => default_types.f32.into(),
            Type::PrimitiveF64(_) => default_types.f64.into(),
            Type::PrimitiveBool(_) => default_types.bool.into(),
        }
    }
}

fn is_value_const(v: &BasicValueEnum<'_>) -> bool {
    match v {
        BasicValueEnum::ArrayValue(v) => v.is_const(),
        BasicValueEnum::IntValue(v) => v.is_const(),
        BasicValueEnum::FloatValue(v) => v.is_const(),
        BasicValueEnum::PointerValue(v) => v.is_const(),
        BasicValueEnum::StructValue(v) => v.is_const(),
        BasicValueEnum::VectorValue(v) => v.is_const(),
    }
}

fn poison_val<'ctx>(v: BasicTypeEnum<'ctx>) -> BasicValueEnum<'ctx> {
    match v {
        BasicTypeEnum::ArrayType(v) => v.get_poison().into(),
        BasicTypeEnum::FloatType(v) => v.get_poison().into(),
        BasicTypeEnum::IntType(v) => v.get_poison().into(),
        BasicTypeEnum::PointerType(v) => v.get_poison().into(),
        BasicTypeEnum::StructType(v) => v.get_poison().into(),
        BasicTypeEnum::VectorType(v) => v.get_poison().into(),
    }
}

fn static_to_basic_type<'ctx>(static_value: GlobalValue<'ctx>) -> BasicTypeEnum<'ctx> {
    match static_value.get_value_type() {
        AnyTypeEnum::ArrayType(ty) => ty.into(),
        AnyTypeEnum::FloatType(ty) => ty.into(),
        AnyTypeEnum::FunctionType(_) => panic!("A static should never be a function"),
        AnyTypeEnum::IntType(ty) => ty.into(),
        AnyTypeEnum::PointerType(ty) => ty.into(),
        AnyTypeEnum::StructType(ty) => ty.into(),
        AnyTypeEnum::VectorType(ty) => ty.into(),
        AnyTypeEnum::VoidType(_) => panic!("A static should never be void"),
    }
}

fn get_alignment(ty: BasicTypeEnum) -> u32 {
    match ty {
        BasicTypeEnum::FloatType(ty) => ty
            .size_of()
            .get_zero_extended_constant()
            .expect("float size has to be constant") as u32,
        BasicTypeEnum::IntType(ty) => ty.get_bit_width() / 8,
        BasicTypeEnum::PointerType(ty) => {
            return ty
                .size_of()
                .get_zero_extended_constant()
                .expect("ptr size has to be constant") as u32
        }
        BasicTypeEnum::VectorType(ty) => get_alignment(ty.get_element_type()),
        BasicTypeEnum::ArrayType(ty) => get_alignment(ty.get_element_type()),
        BasicTypeEnum::StructType(ty) => ty
            .get_field_types_iter()
            .map(|v| get_alignment(v))
            .max()
            .unwrap_or(1),
    }
}

impl<'ctx> TypedLiteral {
    fn fn_ctx_to_basic_value(
        &self,
        ctx: &FunctionCodegenContext<'ctx, '_>,
    ) -> BasicValueEnum<'ctx> {
        self.to_basic_value(
            &|id| ctx.get_value(id),
            &ctx.default_types,
            ctx.structs,
            ctx.builder,
            ctx.statics,
            ctx.functions,
            ctx.external_functions,
            ctx.string_map,
        )
    }
    fn to_basic_value(
        &self,
        scope_get_value: &dyn Fn(usize) -> BasicValueEnum<'ctx>,
        default_types: &DefaultTypes<'ctx>,
        structs: &Vec<StructType<'ctx>>,
        builder: &Builder<'ctx>,
        statics: &Vec<GlobalValue<'ctx>>,
        functions: &Vec<FunctionValue<'ctx>>,
        ext_functions: &Vec<FunctionValue<'ctx>>,
        string_map: &HashMap<GlobalStr, GlobalValue<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        match self {
            TypedLiteral::Void => default_types.empty_struct.const_zero().into(),
            TypedLiteral::Dynamic(id) => scope_get_value(*id),
            TypedLiteral::Function(id) => {
                functions[*id].as_global_value().as_pointer_value().into()
            }
            TypedLiteral::ExternalFunction(id) => ext_functions[*id]
                .as_global_value()
                .as_pointer_value()
                .into(),
            TypedLiteral::Static(id) => {
                let static_value = statics[*id];
                builder
                    .build_load(
                        static_to_basic_type(static_value),
                        static_value.as_pointer_value().into(),
                        "",
                    )
                    .expect("the type should always match")
                    .into()
            }
            TypedLiteral::String(global_str) => {
                let ptr = string_map[global_str].as_pointer_value().into();
                let size = global_str.with(|v| v.len());
                let len_const = default_types.isize.const_int(size as u64, false).into();
                default_types
                    .fat_ptr
                    .const_named_struct(&[ptr, len_const])
                    .into()
            }
            TypedLiteral::Array(ty, elements) => {
                if elements.len() == 0 {
                    return ty
                        .to_llvm_basic_type(default_types, structs)
                        .array_type(0)
                        .const_zero()
                        .into();
                }
                let mut insert_value_vec: Vec<(usize, BasicValueEnum<'ctx>)> = Vec::new();

                macro_rules! array_const_value {
                    ($ty:expr,$into_val_fn:ident) => {{
                        let mut const_elements = Vec::new();
                        for (i, v) in elements.iter().enumerate() {
                            let val = v
                                .to_basic_value(
                                    scope_get_value,
                                    default_types,
                                    structs,
                                    builder,
                                    statics,
                                    functions,
                                    ext_functions,
                                    string_map,
                                )
                                .$into_val_fn();
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
                let mut const_value = match ty.to_llvm_basic_type(default_types, structs) {
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
                    BasicTypeEnum::VectorType(vector_type) => {
                        array_const_value!(vector_type, into_vector_value)
                    }
                };

                for v in insert_value_vec.drain(..) {
                    const_value = builder
                        .build_insert_value(const_value, v.1, v.0 as u32, "")
                        .expect("integer should never be out of range")
                        .into_array_value();
                }
                const_value.into()
            }
            TypedLiteral::Struct(struct_id, vec) => {
                if vec.len() < 1 {
                    return structs[*struct_id].const_named_struct(&[]).into();
                }
                let mut non_const_value = Vec::new();
                let mut const_value = structs[*struct_id].const_named_struct(
                    &vec.iter()
                        .enumerate()
                        .map(|(i, v)| {
                            let val = v.to_basic_value(
                                scope_get_value,
                                default_types,
                                structs,
                                builder,
                                statics,
                                functions,
                                ext_functions,
                                string_map,
                            );
                            if is_value_const(&val) {
                                val
                            } else {
                                let poison_val = poison_val(val.get_type());
                                non_const_value.push((i, val));
                                poison_val
                            }
                        })
                        .collect::<Vec<_>>(),
                );
                for v in non_const_value.drain(..) {
                    const_value = builder
                        .build_insert_value(const_value, v.1, v.0 as u32, "")
                        .expect("integer should never be out of range")
                        .into_struct_value();
                }
                const_value.into()
            }
            TypedLiteral::F64(v) => default_types.f64.const_float(*v).into(),
            TypedLiteral::F32(v) => default_types.f32.const_float(*v as f64).into(),
            TypedLiteral::U8(v) => default_types.i8.const_int(*v as u64, false).into(),
            TypedLiteral::U16(v) => default_types.i16.const_int(*v as u64, false).into(),
            TypedLiteral::U32(v) => default_types.i32.const_int(*v as u64, false).into(),
            TypedLiteral::U64(v) => default_types.i64.const_int(*v, false).into(),
            TypedLiteral::USize(v) => default_types.isize.const_int(*v as u64, false).into(),
            TypedLiteral::I8(v) => default_types.i8.const_int(*v as u64, false).into(),
            TypedLiteral::I16(v) => default_types.i16.const_int(*v as u64, false).into(),
            TypedLiteral::I32(v) => default_types.i32.const_int(*v as u64, false).into(),
            TypedLiteral::I64(v) => default_types.i64.const_int(*v as u64, false).into(),
            TypedLiteral::ISize(v) => default_types.isize.const_int(*v as u64, false).into(),
            TypedLiteral::Bool(v) => default_types.bool.const_int(*v as u64, false).into(),
            TypedLiteral::Intrinsic(intrinsic) => {
                unreachable!("intrinsics should've been resolved by now")
            }
        }
    }
}

macro_rules! f_s_u {
    ($ctx:expr, $typ: expr, $lhs: expr, $rhs: expr, $sint: ident($($sint_val:expr),*), $uint: ident($($uint_val:expr),*), $float: ident($($float_val:expr),*), $err:literal) => {
        match $typ {
            Type::PrimitiveI8(0)
            | Type::PrimitiveI16(0)
            | Type::PrimitiveI32(0)
            | Type::PrimitiveI64(0)
            | Type::PrimitiveISize(0) => $ctx
                .builder
                .$sint($($sint_val,)* $lhs.into_int_value(), $rhs.into_int_value(), "")?
                .into(),
            Type::PrimitiveU8(0)
            | Type::PrimitiveU16(0)
            | Type::PrimitiveU32(0)
            | Type::PrimitiveU64(0)
            | Type::PrimitiveUSize(0) => $ctx
                .builder
                .$uint($($uint_val,)* $lhs.into_int_value(), $rhs.into_int_value(), "")?
                .into(),
            Type::PrimitiveF32(0) | Type::PrimitiveF64(0) => $ctx
                .builder
                .$float($($float_val,)* $lhs.into_float_value(), $rhs.into_float_value(), "")?
                .into(),
            _ => unreachable!($err),
        }
    };
    (with_bool $ctx:expr, $typ: expr, $lhs: expr, $rhs: expr, $sint: ident, $uint: ident, $float: ident, $bool: ident, $err:literal) => {
        match $typ {
            Type::PrimitiveI8(0)
            | Type::PrimitiveI16(0)
            | Type::PrimitiveI32(0)
            | Type::PrimitiveI64(0)
            | Type::PrimitiveISize(0) => $ctx
                .builder
                .$sint($lhs.into_int_value(), $rhs.into_int_value(), "")?
                .into(),
            Type::PrimitiveU8(0)
            | Type::PrimitiveU16(0)
            | Type::PrimitiveU32(0)
            | Type::PrimitiveU64(0)
            | Type::PrimitiveUSize(0) => $ctx
                .builder
                .$uint($lhs.into_int_value(), $rhs.into_int_value(), "")?
                .into(),
            Type::PrimitiveF32(0) | Type::PrimitiveF64(0) => $ctx
                .builder
                .$float($lhs.into_float_value(), $rhs.into_float_value(), "")?
                .into(),
            Type::PrimitiveBool(0) => $ctx
                .builder
                .$bool($lhs.into_int_value(), $rhs.into_int_value(), "")?
                .into(),
            _ => unreachable!($err),
        }
    };
}

fn build_deref<'a>(
    left_side: PointerValue<'a>,
    ty: &Type,
    ctx: &FunctionCodegenContext<'a, '_>,
) -> Result<BasicValueEnum<'a>, BuilderError> {
    if *ty == Type::PrimitiveVoid(0) || *ty == Type::PrimitiveNever {
        return Ok(TypedLiteral::Void.fn_ctx_to_basic_value(ctx));
    }

    if ty.refcount() > 0 {
        if ty.is_thin_ptr() {
            return Ok(ctx
                .builder
                .build_load(ctx.default_types.ptr, left_side, "")?);
        } else {
            let actual_ptr = ctx
                .builder
                .build_load(ctx.default_types.ptr, left_side, "")?;
            let offset_ptr =
                ctx.builder
                    .build_struct_gep(ctx.default_types.fat_ptr, left_side, 1, "")?;
            let metadata = ctx
                .builder
                .build_load(ctx.default_types.isize, offset_ptr, "")?;
            let ptr_only_struct = ctx.builder.build_insert_value(
                ctx.default_types.fat_ptr.get_poison(),
                actual_ptr,
                0,
                "",
            )?;
            return Ok(ctx
                .builder
                .build_insert_value(ptr_only_struct, metadata, 1, "")?
                .as_basic_value_enum());
        }
    }

    match ty {
        Type::Trait { .. } | Type::Generic(..) | Type::PrimitiveSelf(_) => {
            panic!("{ty:?} should be resolved by now")
        }
        Type::DynType { .. } | Type::UnsizedArray { .. } | Type::PrimitiveStr(_) => {
            panic!("cannot dereference unsized type {ty:?}")
        }
        Type::PrimitiveNever => unreachable!(),
        Type::Struct { struct_id, .. } => {
            let llvm_structure = ty
                .to_llvm_basic_type(&ctx.default_types, ctx.structs)
                .into_struct_type();
            let structure = &ctx.tc_ctx.structs.read()[*struct_id];
            let mut value = llvm_structure.get_poison();
            for i in 0..structure.elements.len() {
                let offset_val =
                    ctx.builder
                        .build_struct_gep(llvm_structure, left_side, i as u32, "")?;
                let element_val = build_deref(offset_val, &structure.elements[i].1, ctx)?;
                value = ctx
                    .builder
                    .build_insert_value(value, element_val, i as u32, "")?
                    .into_struct_value();
            }
            Ok(value.into())
        }
        Type::SizedArray {
            typ,
            number_elements,
            ..
        } => {
            let llvm_element_ty = typ.to_llvm_basic_type(&ctx.default_types, ctx.structs);
            let mut value = llvm_element_ty
                .array_type(*number_elements as u32)
                .get_poison();
            for i in 0..*number_elements {
                let offset_val = unsafe {
                    ctx.builder.build_in_bounds_gep(
                        llvm_element_ty,
                        left_side,
                        &[ctx.default_types.isize.const_int(i as u64, false)],
                        "",
                    )
                }?;
                let element_val = build_deref(offset_val, ty, ctx)?;
                value = ctx
                    .builder
                    .build_insert_value(value, element_val, i as u32, "")?
                    .into_array_value();
            }
            Ok(value.into())
        }
        Type::Function(..)
        | Type::PrimitiveVoid(_)
        | Type::PrimitiveI8(_)
        | Type::PrimitiveI16(_)
        | Type::PrimitiveI32(_)
        | Type::PrimitiveI64(_)
        | Type::PrimitiveISize(_)
        | Type::PrimitiveU8(_)
        | Type::PrimitiveU16(_)
        | Type::PrimitiveU32(_)
        | Type::PrimitiveU64(_)
        | Type::PrimitiveUSize(_)
        | Type::PrimitiveF32(_)
        | Type::PrimitiveF64(_)
        | Type::PrimitiveBool(_) => Ok(ctx.builder.build_load(
            ty.to_llvm_basic_type(&ctx.default_types, ctx.structs),
            left_side,
            "",
        )?),
    }
}

fn build_ptr_store(
    left_side: PointerValue,
    right_side: BasicValueEnum,
    ty: &Type,
    ctx: &FunctionCodegenContext,
) -> Result<(), BuilderError> {
    if ty.refcount() > 0 {
        if ty.is_thin_ptr() {
            ctx.builder.build_store(left_side, right_side)?;
            return Ok(());
        } else {
            let actual_ptr =
                ctx.builder
                    .build_extract_value(right_side.into_struct_value(), 0, "")?;
            let metadata =
                ctx.builder
                    .build_extract_value(right_side.into_struct_value(), 1, "")?;
            let actual_ptr_ptr =
                ctx.builder
                    .build_struct_gep(ctx.default_types.fat_ptr, left_side, 0, "")?;
            let metadata_ptr =
                ctx.builder
                    .build_struct_gep(ctx.default_types.fat_ptr, left_side, 0, "")?;
            ctx.builder.build_store(actual_ptr_ptr, actual_ptr)?;
            ctx.builder.build_store(metadata_ptr, metadata)?;
            return Ok(());
        }
    }
    match ty {
        Type::Trait { .. } | Type::Generic(..) | Type::PrimitiveSelf(_) => {
            panic!("{ty:?} should be resolved by now")
        }
        Type::UnsizedArray { .. } | Type::DynType { .. } | Type::PrimitiveStr(_) => {
            panic!("cannot store unsized type {ty:?}")
        }
        Type::PrimitiveNever | Type::PrimitiveVoid(_) => (),
        Type::Struct { struct_id, .. } => {
            let structure = &ctx.tc_ctx.structs.read()[*struct_id];
            let llvm_ty = ty.to_llvm_basic_type(&ctx.default_types, &ctx.structs);
            for (idx, ty) in structure.elements.iter().map(|v| &v.1).enumerate() {
                let val = ctx.builder.build_extract_value(
                    right_side.into_struct_value(),
                    idx as u32,
                    "",
                )?;
                let ptr = ctx
                    .builder
                    .build_struct_gep(llvm_ty, left_side, idx as u32, "")?;
                build_ptr_store(ptr, val, ty, ctx)?;
            }
        }
        Type::SizedArray {
            typ,
            number_elements,
            ..
        } => {
            let llvm_ty = typ.to_llvm_basic_type(&ctx.default_types, ctx.structs);
            for i in 0..*number_elements {
                let val =
                    ctx.builder
                        .build_extract_value(right_side.into_array_value(), i as u32, "")?;
                let ptr = unsafe {
                    ctx.builder.build_in_bounds_gep(
                        llvm_ty,
                        left_side,
                        &[ctx.default_types.isize.const_int(i as u64, false)],
                        "",
                    )
                }?;
                build_ptr_store(ptr, val, typ, ctx)?;
            }
        }
        Type::Function(..)
        | Type::PrimitiveI8(_)
        | Type::PrimitiveI16(_)
        | Type::PrimitiveI32(_)
        | Type::PrimitiveI64(_)
        | Type::PrimitiveISize(_)
        | Type::PrimitiveU8(_)
        | Type::PrimitiveU16(_)
        | Type::PrimitiveU32(_)
        | Type::PrimitiveU64(_)
        | Type::PrimitiveUSize(_)
        | Type::PrimitiveF32(_)
        | Type::PrimitiveF64(_)
        | Type::PrimitiveBool(_) => {
            ctx.builder.build_store(left_side, right_side)?;
        }
    }
    Ok(())
}

impl TypecheckedExpression {
    fn codegen(&self, ctx: &mut FunctionCodegenContext) -> Result<(), BuilderError> {
        match self {
            TypecheckedExpression::Return(location, typed_literal) => {
                match typed_literal {
                    TypedLiteral::Dynamic(id) if ctx.tc_scope[*id].0 == Type::PrimitiveVoid(0) => {
                        ctx.builder.build_return(None)?
                    }
                    TypedLiteral::Static(id)
                        if ctx.tc_ctx.statics.read()[*id].0 == Type::PrimitiveVoid(0) =>
                    {
                        ctx.builder.build_return(None)?
                    }
                    TypedLiteral::Void => ctx.builder.build_return(None)?,
                    TypedLiteral::Intrinsic(_) => unreachable!("intrinsic"),
                    lit => ctx
                        .builder
                        .build_return(Some(&lit.fn_ctx_to_basic_value(ctx)))?,
                };
                Ok(())
            }
            TypecheckedExpression::Block(_, child, _) => {
                for c in child {
                    c.codegen(ctx)?;
                }
                Ok(())
            }
            TypecheckedExpression::If {
                cond,
                if_block,
                else_block: None,
                ..
            } => {
                let if_basic_block = ctx.context.append_basic_block(ctx.current_fn, "then");
                let end_basic_block = ctx.context.append_basic_block(ctx.current_fn, "endif");
                ctx.builder.build_conditional_branch(
                    cond.fn_ctx_to_basic_value(ctx).into_int_value(),
                    if_basic_block,
                    end_basic_block,
                )?;
                ctx.builder.position_at_end(if_basic_block);
                for expr in if_block {
                    expr.codegen(ctx)?;
                }
                ctx.builder.build_unconditional_branch(end_basic_block)?;
                Ok(())
            }

            TypecheckedExpression::If {
                cond,
                if_block,
                else_block: Some(else_block),
                ..
            } => {
                let if_basic_block = ctx.context.append_basic_block(ctx.current_fn, "then");
                let else_basic_block = ctx.context.append_basic_block(ctx.current_fn, "else");
                let end_basic_block = ctx.context.append_basic_block(ctx.current_fn, "endif");
                ctx.builder.build_conditional_branch(
                    cond.fn_ctx_to_basic_value(ctx).into_int_value(),
                    if_basic_block,
                    else_basic_block,
                )?;
                ctx.builder.position_at_end(if_basic_block);
                for expr in if_block {
                    expr.codegen(ctx)?;
                }
                ctx.builder.build_unconditional_branch(end_basic_block)?;
                ctx.builder.position_at_end(else_basic_block);
                for expr in else_block {
                    expr.codegen(ctx)?;
                }
                ctx.builder.build_unconditional_branch(end_basic_block)?;
                Ok(())
            }
            TypecheckedExpression::While {
                cond_block,
                cond,
                body,
                ..
            } => {
                let cond_basic_block = ctx.context.append_basic_block(ctx.current_fn, "while-cond");
                let body_basic_block = ctx.context.append_basic_block(ctx.current_fn, "while-body");
                let end_basic_block = ctx.context.append_basic_block(ctx.current_fn, "while-end");
                ctx.builder.build_unconditional_branch(cond_basic_block)?;
                ctx.builder.position_at_end(cond_basic_block);
                for expr in cond_block {
                    expr.codegen(ctx)?;
                }
                ctx.builder.build_conditional_branch(
                    cond.fn_ctx_to_basic_value(ctx).into_int_value(),
                    body_basic_block,
                    end_basic_block,
                )?;
                ctx.builder.position_at_end(body_basic_block);
                for expr in body {
                    expr.codegen(ctx)?;
                }
                ctx.builder.build_unconditional_branch(cond_basic_block)?;
                ctx.builder.position_at_end(end_basic_block);
                Ok(())
            }
            TypecheckedExpression::Range { .. } => todo!(),
            TypecheckedExpression::StoreAssignment(location, dst, src) => {
                match src {
                    TypedLiteral::Dynamic(id) if ctx.tc_scope[*id].1.stack_allocated => {
                        let ty = &ctx.tc_scope[*id].0;
                        let llvm_ty = ty.to_llvm_basic_type(&ctx.default_types, ctx.structs);
                        let alignment = get_alignment(llvm_ty);
                        ctx.builder.build_memmove(
                            dst.fn_ctx_to_basic_value(ctx).into_pointer_value(),
                            alignment,
                            ctx.get_value_ptr(*id),
                            alignment,
                            llvm_ty.size_of().expect("llvm type should always be sized"),
                        )?;
                        return Ok(());
                    }
                    TypedLiteral::Static(id) => {
                        let ty = &ctx.tc_ctx.statics.read()[*id].0;
                        let llvm_ty = ty.to_llvm_basic_type(&ctx.default_types, ctx.structs);
                        let alignment = get_alignment(llvm_ty);
                        ctx.builder.build_memmove(
                            dst.fn_ctx_to_basic_value(ctx).into_pointer_value(),
                            alignment,
                            ctx.statics[*id].as_pointer_value(),
                            alignment,
                            llvm_ty.size_of().expect("llvm type should always be sized"),
                        )?;
                        return Ok(());
                    }
                    _ => {}
                }
                build_ptr_store(
                    dst.fn_ctx_to_basic_value(ctx).into_pointer_value(),
                    src.fn_ctx_to_basic_value(ctx),
                    if let TypedLiteral::Function(..) | TypedLiteral::ExternalFunction(..) = src {
                        Cow::Borrowed(&Type::PrimitiveUSize(0))
                    } else {
                        src.to_type(&ctx.tc_scope, ctx.tc_ctx)
                    }
                    .as_ref(),
                    ctx,
                )
            }
            TypecheckedExpression::Call(location, dst, fn_ptr, args) => todo!(),
            TypecheckedExpression::DirectExternCall(location, dst, func, args)
            | TypecheckedExpression::DirectCall(location, dst, func, args) => {
                let func_value = if matches!(self, TypecheckedExpression::DirectCall(..)) {
                    ctx.functions[*func]
                } else {
                    ctx.external_functions[*func]
                };
                let val = ctx.builder.build_direct_call(
                    func_value,
                    &args
                        .iter()
                        .filter(|v| match v {
                            TypedLiteral::Void => false,
                            TypedLiteral::Dynamic(id) => !matches!(
                                ctx.tc_scope[*id].0,
                                Type::PrimitiveNever | Type::PrimitiveVoid(0)
                            ),
                            _ => true,
                        })
                        .map(|v| v.fn_ctx_to_basic_value(ctx).into())
                        .collect::<Vec<_>>(),
                    "",
                )?;
                ctx.push_value(
                    *dst,
                    val.try_as_basic_value()
                        .left_or(ctx.default_types.empty_struct.const_zero().into()),
                );
                Ok(())
            }
            TypecheckedExpression::IntrinsicCall(location, dst, intrinsic, vec) => {
                match intrinsic {
                    Intrinsic::Drop => todo!(),
                    Intrinsic::DropInPlace => todo!(),
                    Intrinsic::Forget => todo!(),
                    Intrinsic::SizeOf => todo!(),
                    Intrinsic::SizeOfVal => todo!(),
                    Intrinsic::Breakpoint => todo!(),
                    Intrinsic::Location => {}
                    Intrinsic::Offset => todo!(),
                    Intrinsic::GetMetadata => todo!(),
                    Intrinsic::WithMetadata => todo!(),
                    Intrinsic::TypeName => todo!(),
                    Intrinsic::Unreachable => todo!(),
                    Intrinsic::VtableSize => todo!(),
                    Intrinsic::VtableDrop => todo!(),
                    Intrinsic::Read => todo!(),
                    Intrinsic::Write => todo!(),
                }
                Ok(())
            }
            TypecheckedExpression::Pos(location, dst, src) => {
                Ok(ctx.push_value(*dst, src.fn_ctx_to_basic_value(ctx)))
            }
            TypecheckedExpression::Neg(location, typed_literal, typed_literal1) => todo!(),
            TypecheckedExpression::LNot(location, typed_literal, typed_literal1) => todo!(),
            TypecheckedExpression::BNot(location, typed_literal, typed_literal1) => todo!(),
            TypecheckedExpression::Add(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst].0;
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_nsw_add(),
                    build_int_nuw_add(),
                    build_float_add(),
                    "tc should have errored if you try to add 2 non-int/float values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::Sub(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst].0;
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_nsw_sub(),
                    build_int_nuw_sub(),
                    build_float_sub(),
                    "tc should have errored if you try to subtract 2 non-int/float values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::Mul(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst].0;
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_nsw_mul(),
                    build_int_nuw_mul(),
                    build_float_mul(),
                    "tc should have errored if you try to multiply 2 non-int/float values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::Div(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst].0;
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_signed_div(),
                    build_int_unsigned_div(),
                    build_float_div(),
                    "tc should have errored if you try to divide 2 non-int/float values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::Mod(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst].0;
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_signed_rem(),
                    build_int_unsigned_rem(),
                    build_float_rem(),
                    "tc should have errored if you try to mod 2 non-int/float values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::BAnd(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst].0;
                if typ.is_int_like() || *typ == Type::PrimitiveBool(0) {
                    ctx.push_value(
                        *dst,
                        ctx.builder
                            .build_and(lhs.into_int_value(), rhs.into_int_value(), "")?
                            .into(),
                    );
                } else {
                    unreachable!(
                        "tc should have errored if you try to binary and 2 non-int/bool values"
                    );
                }
                Ok(())
            }
            TypecheckedExpression::BOr(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst].0;
                if typ.is_int_like() || *typ == Type::PrimitiveBool(0) {
                    ctx.push_value(
                        *dst,
                        ctx.builder
                            .build_or(lhs.into_int_value(), rhs.into_int_value(), "")?
                            .into(),
                    );
                } else {
                    unreachable!(
                        "tc should have errored if you try to binary or 2 non-int/bool values"
                    );
                }
                Ok(())
            }
            TypecheckedExpression::BXor(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst].0;
                if typ.is_int_like() || *typ == Type::PrimitiveBool(0) {
                    ctx.push_value(
                        *dst,
                        ctx.builder
                            .build_xor(lhs.into_int_value(), rhs.into_int_value(), "")?
                            .into(),
                    );
                } else {
                    unreachable!("tc should have errored if you try to xor 2 non-int/bool values");
                }
                Ok(())
            }
            TypecheckedExpression::GreaterThan(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst].0;
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_compare(IntPredicate::SGT),
                    build_int_compare(IntPredicate::UGT),
                    build_float_compare(FloatPredicate::UGT),
                    "tc should have errored if you try to compare 2 non-int/float values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::LessThan(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst].0;
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_compare(IntPredicate::SLT),
                    build_int_compare(IntPredicate::ULT),
                    build_float_compare(FloatPredicate::ULT),
                    "tc should have errored if you try to compare 2 non-int/float values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::LAnd(location, dst, lhs, rhs) => todo!(),
            TypecheckedExpression::LOr(location, dst, lhs, rhs) => todo!(),
            TypecheckedExpression::GreaterThanEq(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst].0;
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_compare(IntPredicate::SGE),
                    build_int_compare(IntPredicate::UGE),
                    build_float_compare(FloatPredicate::UGE),
                    "tc should have errored if you try to compare 2 non-int/float values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::LessThanEq(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst].0;
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_compare(IntPredicate::SLE),
                    build_int_compare(IntPredicate::ULE),
                    build_float_compare(FloatPredicate::ULE),
                    "tc should have errored if you try to compare 2 non-int/float values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::Eq(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst].0;
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_compare(IntPredicate::EQ),
                    build_int_compare(IntPredicate::EQ),
                    build_float_compare(FloatPredicate::UEQ),
                    "tc should have errored if you try to compare 2 non-int/float values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::Neq(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst].0;
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_compare(IntPredicate::NE),
                    build_int_compare(IntPredicate::NE),
                    build_float_compare(FloatPredicate::UNE),
                    "tc should have errored if you try to compare 2 non-int/float values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::LShift(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst].0;
                if typ.is_int_like() {
                    ctx.push_value(
                        *dst,
                        ctx.builder
                            .build_left_shift(lhs.into_int_value(), rhs.into_int_value(), "")?
                            .into(),
                    );
                    Ok(())
                } else {
                    unreachable!("tc should have errored if you try to left shift a non-int value");
                }
            }
            TypecheckedExpression::RShift(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst].0;
                if typ.is_int_like() {
                    ctx.push_value(
                        *dst,
                        ctx.builder
                            .build_right_shift(
                                lhs.into_int_value(),
                                rhs.into_int_value(),
                                false,
                                "",
                            )?
                            .into(),
                    );
                    Ok(())
                } else {
                    unreachable!(
                        "tc should have errored if you try to right shift a non-int value"
                    );
                }
            }
            // &void is a nullptr
            TypecheckedExpression::Reference(_, dst, TypedLiteral::Void) => {
                Ok(ctx.push_value(*dst, ctx.default_types.ptr.const_zero().into()))
            }
            TypecheckedExpression::Reference(location, dst, rhs) => {
                let value = match rhs {
                    TypedLiteral::Dynamic(id) if !ctx.tc_scope[*id].1.stack_allocated => panic!("_{id} is not stack allocated (even tho it should have been)"),
                    TypedLiteral::Dynamic(id) => ctx.get_value_ptr(*id).into(),
                    TypedLiteral::Static(id) => ctx.statics[*id].as_basic_value_enum(),
                    _ => panic!("Cannot take a reference to {rhs:?} (the typechecker should have put this into a stack-allocated dynamic)"),
                };
                ctx.push_value(*dst, value);
                Ok(())
            }
            TypecheckedExpression::MakeUnsizedSlice(_, dst, src, sz) => {
                let fat_ptr_no_ptr = ctx.default_types.fat_ptr.const_named_struct(&[
                    ctx.default_types.ptr.get_poison().into(),
                    ctx.default_types.isize.const_int(*sz as u64, false).into(),
                ]);
                let fat_ptr = ctx
                    .builder
                    .build_insert_value(fat_ptr_no_ptr, src.fn_ctx_to_basic_value(ctx), 0, "")?
                    .into_struct_value();
                ctx.push_value(*dst, fat_ptr.into());

                Ok(())
            }
            TypecheckedExpression::Dereference(location, dst, rhs) => {
                if ctx.tc_scope[*dst].1.stack_allocated {
                    let ty = ctx.tc_scope[*dst]
                        .0
                        .to_llvm_basic_type(&ctx.default_types, ctx.structs);
                    let lhs_ptr = ctx.builder.build_alloca(ty, "")?;
                    let alignment = get_alignment(ty);
                    ctx.builder.build_memmove(
                        lhs_ptr, // dest (dst = *rhs), in this case *dst =
                        // *rhs as lhs is stack-allocated, meaning an implicit store has to be
                        // added.
                        alignment,
                        rhs.fn_ctx_to_basic_value(ctx).into_pointer_value(),
                        alignment,
                        ty.size_of().expect("a type should *always* be sized"),
                    )?;
                    ctx.push_value_raw(*dst, lhs_ptr.into());
                    return Ok(());
                }
                let value = build_deref(
                    rhs.fn_ctx_to_basic_value(ctx).into_pointer_value(), // dst = *rhs
                    &ctx.tc_scope[*dst].0, // we take the type of lhs as it expects the type of the
                    // value *after* dereferencing as any pointer in llvm is represented as the
                    // same type, &i32 == &&u64 (`ptr`)
                    ctx,
                )?;
                ctx.push_value(*dst, value);
                Ok(())
            }
            // &struct, <- i64 0, i32 <idx>
            // &[a] <- extractvalue 0 and then i64 n of that
            // &[a; n] <- i64 0 n (for getelementptr [a; n]) or i64 n (for getelementptr a)
            TypecheckedExpression::Offset(location, dst, src, offset) => {
                let ty = src.to_type(&ctx.tc_scope, ctx.tc_ctx);
                match ty.as_ref() {
                    Type::Struct {
                        struct_id,
                        num_references: 1,
                        ..
                    } => {
                        let offset = match offset {
                            OffsetValue::Dynamic(id) => ctx.get_value(*id).into_int_value(),
                            OffsetValue::Static(v) => {
                                ctx.default_types.i32.const_int(*v as u64, false)
                            }
                        };
                        let value = unsafe {
                            ctx.builder.build_in_bounds_gep(
                                ctx.structs[*struct_id],
                                src.fn_ctx_to_basic_value(ctx).into_pointer_value(),
                                &[ctx.default_types.isize.const_int(0, false), offset],
                                "",
                            )
                        }?;
                        ctx.push_value(*dst, value.into());
                        Ok(())
                    }
                    Type::SizedArray {
                        typ,
                        num_references: 1,
                        ..
                    } => {
                        let offset = match offset {
                            OffsetValue::Dynamic(id) => ctx.get_value(*id).into_int_value(),
                            OffsetValue::Static(v) => {
                                ctx.default_types.i32.const_int(*v as u64, false)
                            }
                        };
                        let value = unsafe {
                            ctx.builder.build_in_bounds_gep(
                                typ.to_llvm_basic_type(&ctx.default_types, &ctx.structs),
                                src.fn_ctx_to_basic_value(ctx).into_pointer_value(),
                                &[offset],
                                "",
                            )
                        }?;
                        ctx.push_value(*dst, value.into());
                        Ok(())
                    }
                    Type::UnsizedArray {
                        typ,
                        num_references: 1,
                        ..
                    } => {
                        let offset = match offset {
                            OffsetValue::Dynamic(id) => ctx.get_value(*id).into_int_value(),
                            OffsetValue::Static(v) => {
                                ctx.default_types.i32.const_int(*v as u64, false)
                            }
                        };
                        let actual_ptr = ctx
                            .builder
                            .build_extract_value(
                                src.fn_ctx_to_basic_value(ctx).into_struct_value(),
                                0,
                                "",
                            )?
                            .into_pointer_value();
                        let value = unsafe {
                            ctx.builder.build_in_bounds_gep(
                                typ.to_llvm_basic_type(&ctx.default_types, &ctx.structs),
                                actual_ptr,
                                &[offset],
                                "",
                            )
                        }?;
                        ctx.push_value(*dst, value.into());
                        Ok(())
                    }
                    _ => unreachable!("cannot take offset of {ty:?}"),
                }
            }
            TypecheckedExpression::OffsetNonPointer(location, dst, src, offset_value) => {
                let src = src.fn_ctx_to_basic_value(ctx);
                if src.is_array_value() {
                    ctx.push_value(
                        *dst,
                        ctx.builder.build_extract_value(
                            src.into_array_value(),
                            *offset_value as u32,
                            "",
                        )?,
                    );
                } else if src.is_struct_value() {
                    ctx.push_value(
                        *dst,
                        ctx.builder.build_extract_value(
                            src.into_struct_value(),
                            *offset_value as u32,
                            "",
                        )?,
                    );
                } else {
                    panic!("offsetnonptr should never be used with a src element that is not an aggregate value")
                }
                Ok(())
            }
            TypecheckedExpression::TraitCall(
                location,
                typed_literal,
                typed_literal1,
                _,
                global_str,
            ) => todo!(),
            TypecheckedExpression::Alias(location, new, old) => {
                let new_typ = &ctx.tc_scope[*new].0;
                let old_typ = old.to_type(&ctx.tc_scope, ctx.tc_ctx);
                if *new_typ == *old_typ {
                    // this will copy the value, that is expected behavior
                    return Ok(ctx.push_value(*new, old.fn_ctx_to_basic_value(ctx)));
                }
                if new_typ.refcount() > 0 {
                    assert_eq!(new_typ.refcount(), old_typ.refcount());
                    assert!(new_typ.is_thin_ptr());
                    if !old_typ.is_thin_ptr() {
                        let val = old.fn_ctx_to_basic_value(ctx);
                        let real_ptr =
                            ctx.builder
                                .build_extract_value(val.into_struct_value(), 0, "")?;
                        ctx.push_value(*new, real_ptr);
                        return Ok(());
                    } else {
                        return Ok(ctx.push_value(*new, old.fn_ctx_to_basic_value(ctx)));
                    }
                }
                todo!()
            }
            TypecheckedExpression::Literal(location, dst, src) => {
                Ok(ctx.push_value(*dst, src.fn_ctx_to_basic_value(ctx)))
            }
            TypecheckedExpression::Empty(location) => Ok(()),
            TypecheckedExpression::None => {
                unreachable!("None-expressions are not valid and indicate an error")
            }
        }
    }
}
