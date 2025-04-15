use context::DefaultTypes;
use core::panic;
use debug_builder::DebugContext;
use intrinsics::LLVMIntrinsics;
use std::{borrow::Cow, collections::HashMap};

use crate::{
    globals::GlobalStr,
    module::{ModuleId, TraitId},
    typechecking::{
        expression::{OffsetValue, TypecheckedExpression, TypedLiteral},
        intrinsics::Intrinsic,
        typechecking::ScopeTypeMetadata,
        Type, TypecheckingContext,
    },
};
pub use inkwell::context::Context as InkwellContext;
pub mod mangling;
pub use context::{CodegenConfig, CodegenContext, Optimizations};
pub use error::CodegenError;
pub use inkwell::support::LLVMString;
use inkwell::{
    basic_block::BasicBlock,
    builder::{Builder, BuilderError},
    context::Context,
    debug_info::{AsDIScope, DIScope},
    module::Module,
    targets::TargetMachine,
    types::{AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType, StructType},
    values::{
        BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, GlobalValue, IntValue,
        PointerValue,
    },
    FloatPredicate, IntPredicate,
};

mod context;
mod debug_builder;
mod debug_constants;
mod error;
mod intrinsics;

impl<'ctx, 'me> CodegenContext<'ctx> {
    pub fn make_function_codegen_context(
        &'me mut self,
        tc_scope: Vec<(Type, ScopeTypeMetadata)>,
        current_fn: FunctionValue<'ctx>,
        bb: BasicBlock<'ctx>,
    ) -> FunctionCodegenContext<'ctx, 'me> {
        FunctionCodegenContext {
            tc_scope,
            tc_ctx: &self.tc_ctx,
            _scope: Vec::new(),
            builder: &self.builder,
            context: self.context,
            default_types: self.default_types,
            current_fn,
            functions: &self.functions,
            external_functions: &self.external_functions,
            structs: &self.structs,
            statics: &self.statics,
            string_map: &self.string_map,
            vtables: &self.vtables,
            debug_ctx: &mut self.debug_ctx,
            machine: &self.machine,
            intrinsics: &self.intrinsics,
            module: &self.module,
            retaddr: self.retaddr,
            current_block: bb,
            pointer_size: self.default_types.isize.get_bit_width() as u64 / 8,
        }
    }
}

impl<'ctx> FunctionCodegenContext<'ctx, '_> {
    pub fn goto(&mut self, bb: BasicBlock<'ctx>) {
        self.builder.position_at_end(bb);
        self.current_block = bb;
    }

    /// uses the function to build a terminator if none was built yet
    pub fn terminate<T, E>(&self, func: impl FnOnce() -> Result<T, E>) -> Result<(), E> {
        if self.current_block.get_terminator().is_none() {
            func()?;
        }
        Ok(())
    }
}

pub struct FunctionCodegenContext<'ctx, 'codegen> {
    tc_scope: Vec<(Type, ScopeTypeMetadata)>,
    tc_ctx: &'codegen TypecheckingContext,
    _scope: Vec<BasicValueEnum<'ctx>>,
    builder: &'codegen Builder<'ctx>,
    context: &'ctx Context,
    default_types: DefaultTypes<'ctx>,
    current_fn: FunctionValue<'ctx>,
    functions: &'codegen Vec<FunctionValue<'ctx>>,
    external_functions: &'codegen Vec<FunctionValue<'ctx>>,
    structs: &'codegen Vec<StructType<'ctx>>,
    statics: &'codegen Vec<GlobalValue<'ctx>>,
    string_map: &'codegen HashMap<GlobalStr, GlobalValue<'ctx>>,
    vtables: &'codegen HashMap<(Type, Vec<TraitId>), GlobalValue<'ctx>>,
    debug_ctx: &'codegen mut DebugContext<'ctx>,
    machine: &'codegen TargetMachine,
    intrinsics: &'codegen LLVMIntrinsics,
    retaddr: FunctionValue<'ctx>,
    module: &'codegen Module<'ctx>,
    current_block: BasicBlock<'ctx>,
    pointer_size: u64,
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
                self.tc_scope[id].0.to_llvm_basic_type(
                    &self.default_types,
                    self.structs,
                    self.context,
                ),
                "",
            )
            .expect("failed to build alloca for a stack allocated value");
        build_ptr_store(allocated_value, value, &self.tc_scope[id].0, self, false)
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
            build_deref(ptr, &self.tc_scope[id].0, self, false)
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
        structs: &[StructType<'ctx>],
        ctx: &'ctx Context,
    ) -> BasicTypeEnum<'ctx> {
        if self.refcount() > 0 {
            if self.is_thin_ptr() {
                return default_types.ptr.into();
            } else {
                return default_types.fat_ptr.into();
            }
        }

        match self {
            Type::Generic { .. } | Type::Trait { .. } => {
                unreachable!("generics should be resolved by now")
            }
            Type::UnsizedArray { .. } => panic!("llvm types must be sized, `[_]` is not"),
            Type::PrimitiveStr(_) => panic!("llvm types must be sized, `str` is not"),
            Type::PrimitiveSelf(_) => unreachable!("Self must be resolved at this point"),
            Type::DynType { .. } => panic!("llvm types must be sized, `dyn _` is not"),
            Type::Struct { struct_id, .. } => structs[*struct_id].into(),
            Type::Tuple { elements, .. } => ctx
                .struct_type(
                    &elements
                        .iter()
                        .map(|v| v.to_llvm_basic_type(default_types, structs, ctx))
                        .collect::<Vec<_>>(),
                    false,
                )
                .into(),
            Type::SizedArray {
                typ,
                number_elements,
                ..
            } => typ
                .to_llvm_basic_type(default_types, structs, ctx)
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

    fn to_llvm_fn_type(
        &self,
        default_types: &DefaultTypes<'ctx>,
        structs: &[StructType<'ctx>],
        ctx: &'ctx Context,
    ) -> FunctionType<'ctx> {
        let Type::Function(v, _) = self else {
            unreachable!("`self` is not a function")
        };
        let return_ty = v
            .return_type
            .to_llvm_basic_type(default_types, structs, ctx);
        let args = v
            .arguments
            .iter()
            .map(|v| v.to_llvm_basic_type(default_types, structs, ctx).into())
            .collect::<Vec<_>>();
        return_ty.fn_type(&args, false)
    }
}

fn is_value_const(v: &BasicValueEnum<'_>) -> bool {
    match v {
        BasicValueEnum::ArrayValue(v) => v.is_const(),
        BasicValueEnum::IntValue(v) => v.is_const(),
        BasicValueEnum::FloatValue(v) => v.is_const(),
        BasicValueEnum::PointerValue(v) => v.is_const(),
        BasicValueEnum::StructValue(v) => v.is_const(),
        BasicValueEnum::VectorValue(..) | BasicValueEnum::ScalableVectorValue(..) => {
            unreachable!("vector types arent supported")
        }
    }
}

fn poison_val(v: BasicTypeEnum<'_>) -> BasicValueEnum<'_> {
    match v {
        BasicTypeEnum::ArrayType(v) => v.get_poison().into(),
        BasicTypeEnum::FloatType(v) => v.get_poison().into(),
        BasicTypeEnum::IntType(v) => v.get_poison().into(),
        BasicTypeEnum::PointerType(v) => v.get_poison().into(),
        BasicTypeEnum::StructType(v) => v.get_poison().into(),
        BasicTypeEnum::VectorType(..) | BasicTypeEnum::ScalableVectorType(..) => {
            unreachable!("vector types arent supported")
        }
    }
}

fn static_to_basic_type(static_value: GlobalValue<'_>) -> BasicTypeEnum<'_> {
    match static_value.get_value_type() {
        AnyTypeEnum::ArrayType(ty) => ty.into(),
        AnyTypeEnum::FloatType(ty) => ty.into(),
        AnyTypeEnum::FunctionType(_) => panic!("A static should never be a function"),
        AnyTypeEnum::IntType(ty) => ty.into(),
        AnyTypeEnum::PointerType(ty) => ty.into(),
        AnyTypeEnum::StructType(ty) => ty.into(),
        AnyTypeEnum::VoidType(_) => panic!("A static should never be void"),
        AnyTypeEnum::VectorType(..) | AnyTypeEnum::ScalableVectorType(..) => {
            unreachable!("vector types arent supported")
        }
    }
}

fn get_alignment(ty: BasicTypeEnum) -> u32 {
    match ty {
        BasicTypeEnum::FloatType(ty) => ty
            .size_of()
            .get_zero_extended_constant()
            .expect("float size has to be constant") as u32,
        BasicTypeEnum::IntType(ty) => ty.get_bit_width() / 8,
        BasicTypeEnum::PointerType(ty) => ty
            .size_of()
            .get_zero_extended_constant()
            .expect("ptr size has to be constant") as u32,
        BasicTypeEnum::ArrayType(ty) => get_alignment(ty.get_element_type()),
        BasicTypeEnum::StructType(ty) => ty
            .get_field_types_iter()
            .map(|v| get_alignment(v))
            .max()
            .unwrap_or(1),
        BasicTypeEnum::VectorType(..) | BasicTypeEnum::ScalableVectorType(..) => {
            unreachable!("vector types aren't supported")
        }
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
            ctx.context,
        )
    }
    #[allow(clippy::too_many_arguments)]
    fn to_basic_value(
        &self,
        scope_get_value: &dyn Fn(usize) -> BasicValueEnum<'ctx>,
        default_types: &DefaultTypes<'ctx>,
        structs: &[StructType<'ctx>],
        builder: &Builder<'ctx>,
        statics: &[GlobalValue<'ctx>],
        functions: &[FunctionValue<'ctx>],
        ext_functions: &[FunctionValue<'ctx>],
        string_map: &HashMap<GlobalStr, GlobalValue<'ctx>>,
        ctx: &'ctx Context,
    ) -> BasicValueEnum<'ctx> {
        match self {
            TypedLiteral::Void => default_types.empty_struct.const_zero().into(),
            TypedLiteral::Dynamic(id) => scope_get_value(*id),
            TypedLiteral::Function(id, generics) => {
                // cannot generate a call to a function with generics
                // if a function has generics, it should've been monomorphized.
                assert!(generics.is_empty());
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
                        static_value.as_pointer_value(),
                        "",
                    )
                    .expect("the type should always match")
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

            TypedLiteral::ArrayInit(ty, _, 0) => ty
                .to_llvm_basic_type(default_types, structs, ctx)
                .array_type(0)
                .const_zero()
                .into(),
            TypedLiteral::ArrayInit(_, elem, amount) => {
                let elem = elem.to_basic_value(
                    scope_get_value,
                    default_types,
                    structs,
                    builder,
                    statics,
                    functions,
                    ext_functions,
                    string_map,
                    ctx,
                );
                let mut array_value = elem.get_type().array_type(*amount as u32).const_zero();
                for i in 0..*amount {
                    array_value = builder
                        .build_insert_value(array_value, elem, i as u32, "")
                        .expect("i should never be out of bounds")
                        .into_array_value();
                }

                array_value.into()
            }
            TypedLiteral::Array(ty, elements) => {
                if elements.is_empty() {
                    return ty
                        .to_llvm_basic_type(default_types, structs, ctx)
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
                                    ctx,
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
                let mut const_value = match ty.to_llvm_basic_type(default_types, structs, ctx) {
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
                    const_value = builder
                        .build_insert_value(const_value, v.1, v.0 as u32, "")
                        .expect("integer should never be out of range")
                        .into_array_value();
                }
                const_value.into()
            }
            TypedLiteral::Struct(struct_id, vec) => {
                if vec.is_empty() {
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
                                ctx,
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
            TypedLiteral::Tuple(values) => {
                let mut elems = Vec::with_capacity(values.len());
                let mut elem_types = Vec::with_capacity(values.len());
                for v in values {
                    let val = v.to_basic_value(
                        scope_get_value,
                        default_types,
                        structs,
                        builder,
                        statics,
                        functions,
                        ext_functions,
                        string_map,
                        ctx,
                    );
                    elem_types.push(val.get_type());
                    elems.push(val);
                }
                let mut value = ctx.struct_type(&elem_types, false).const_zero();
                for (i, elem) in elems.into_iter().enumerate() {
                    value = builder
                        .build_insert_value(value, elem, i as u32, "")
                        .expect("integer should never be out of range")
                        .into_struct_value();
                }
                value.into()
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
            TypedLiteral::Intrinsic(..) => {
                unreachable!("intrinsics can only be used as part of intrinsic call")
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
            ty => unreachable!(concat!($err, "  -- Type: {}"), ty),
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

fn make_volatile(v: BasicValueEnum<'_>, volatile: bool) -> BasicValueEnum<'_> {
    v.as_instruction_value()
        .expect("there should be an instruction value here")
        .set_volatile(volatile)
        .expect("setting volatile should never fail");
    v
}

fn build_deref<'a>(
    left_side: PointerValue<'a>,
    ty: &Type,
    ctx: &FunctionCodegenContext<'a, '_>,
    volatile: bool,
) -> Result<BasicValueEnum<'a>, BuilderError> {
    if *ty == Type::PrimitiveVoid(0) || *ty == Type::PrimitiveNever {
        return Ok(TypedLiteral::Void.fn_ctx_to_basic_value(ctx));
    }

    if ty.refcount() > 0 {
        if ty.is_thin_ptr() {
            return Ok(make_volatile(
                ctx.builder
                    .build_load(ctx.default_types.ptr, left_side, "")?,
                volatile,
            ));
        } else {
            let actual_ptr = ctx
                .builder
                .build_load(ctx.default_types.ptr, left_side, "")?;
            make_volatile(actual_ptr, volatile);
            let offset_ptr =
                ctx.builder
                    .build_struct_gep(ctx.default_types.fat_ptr, left_side, 1, "")?;
            let metadata = ctx
                .builder
                .build_load(ctx.default_types.isize, offset_ptr, "")?;
            make_volatile(metadata, volatile);
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
        Type::Trait { .. } | Type::Generic { .. } | Type::PrimitiveSelf(_) => {
            panic!("{ty:?} should be resolved by now")
        }
        Type::DynType { .. } | Type::UnsizedArray { .. } | Type::PrimitiveStr(_) => {
            panic!("cannot dereference unsized type {ty:?}")
        }
        Type::PrimitiveNever => unreachable!(),
        Type::Struct { struct_id, .. } => {
            let llvm_structure = ty
                .to_llvm_basic_type(&ctx.default_types, ctx.structs, ctx.context)
                .into_struct_type();
            let structure = &ctx.tc_ctx.structs.read()[*struct_id];
            let mut value = llvm_structure.get_poison();
            for i in 0..structure.elements.len() {
                let offset_val =
                    ctx.builder
                        .build_struct_gep(llvm_structure, left_side, i as u32, "")?;
                let element_val = build_deref(offset_val, &structure.elements[i].1, ctx, volatile)?;
                value = ctx
                    .builder
                    .build_insert_value(value, element_val, i as u32, "")?
                    .into_struct_value();
            }
            Ok(value.into())
        }
        Type::Tuple { elements, .. } => {
            let llvm_structure = ty
                .to_llvm_basic_type(&ctx.default_types, ctx.structs, ctx.context)
                .into_struct_type();
            let mut value = llvm_structure.get_poison();
            for (i, elem) in elements.iter().enumerate() {
                let offset_val =
                    ctx.builder
                        .build_struct_gep(llvm_structure, left_side, i as u32, "")?;
                let element_val = build_deref(offset_val, elem, ctx, volatile)?;
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
            let llvm_element_ty =
                typ.to_llvm_basic_type(&ctx.default_types, ctx.structs, ctx.context);
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
                let element_val = build_deref(offset_val, ty, ctx, volatile)?;
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
        | Type::PrimitiveBool(_) => Ok(make_volatile(
            ctx.builder.build_load(
                ty.to_llvm_basic_type(&ctx.default_types, ctx.structs, ctx.context),
                left_side,
                "",
            )?,
            volatile,
        )),
    }
}

fn build_ptr_store(
    left_side: PointerValue,
    right_side: BasicValueEnum,
    ty: &Type,
    ctx: &FunctionCodegenContext,
    volatile: bool,
) -> Result<(), BuilderError> {
    if ty.refcount() > 0 {
        if ty.is_thin_ptr() {
            ctx.builder
                .build_store(left_side, right_side)?
                .set_volatile(volatile)
                .expect("setting volatile should never fail");
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
                    .build_struct_gep(ctx.default_types.fat_ptr, left_side, 1, "")?;
            ctx.builder
                .build_store(actual_ptr_ptr, actual_ptr)?
                .set_volatile(volatile)
                .expect("setting volatile should never fail");
            ctx.builder
                .build_store(metadata_ptr, metadata)?
                .set_volatile(volatile)
                .expect("setting volatile should never fail");
            return Ok(());
        }
    }
    match ty {
        Type::Trait { .. } | Type::Generic { .. } | Type::PrimitiveSelf(_) => {
            panic!("{ty:?} should be resolved by now")
        }
        Type::UnsizedArray { .. } | Type::DynType { .. } | Type::PrimitiveStr(_) => {
            panic!("cannot store unsized type {ty:?}")
        }
        Type::PrimitiveNever | Type::PrimitiveVoid(_) => (),
        Type::Struct { struct_id, .. } => {
            let structure = &ctx.tc_ctx.structs.read()[*struct_id];
            let llvm_ty = ty.to_llvm_basic_type(&ctx.default_types, ctx.structs, ctx.context);
            for (idx, ty) in structure.elements.iter().map(|v| &v.1).enumerate() {
                let val = ctx.builder.build_extract_value(
                    right_side.into_struct_value(),
                    idx as u32,
                    "",
                )?;
                let ptr = ctx
                    .builder
                    .build_struct_gep(llvm_ty, left_side, idx as u32, "")?;
                build_ptr_store(ptr, val, ty, ctx, volatile)?;
            }
        }
        Type::Tuple { elements, .. } => {
            let llvm_ty = ty.to_llvm_basic_type(&ctx.default_types, ctx.structs, ctx.context);
            for (idx, ty) in elements.iter().enumerate() {
                let val = ctx.builder.build_extract_value(
                    right_side.into_struct_value(),
                    idx as u32,
                    "",
                )?;
                let ptr = ctx
                    .builder
                    .build_struct_gep(llvm_ty, left_side, idx as u32, "")?;
                build_ptr_store(ptr, val, ty, ctx, volatile)?;
            }
        }
        Type::SizedArray {
            typ,
            number_elements,
            ..
        } => {
            let llvm_ty = typ.to_llvm_basic_type(&ctx.default_types, ctx.structs, ctx.context);
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
                build_ptr_store(ptr, val, typ, ctx, volatile)?;
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
            ctx.builder
                .build_store(left_side, right_side)?
                .set_volatile(volatile)
                .expect("setting volatile should never fail");
        }
    }
    Ok(())
}

fn make_fat_ptr<'ctx>(
    builder: &Builder<'ctx>,
    default_types: &DefaultTypes<'ctx>,
    ptr: PointerValue<'ctx>,
    metadata: IntValue<'ctx>,
) -> Result<BasicValueEnum<'ctx>, BuilderError> {
    let mut data = [
        default_types.ptr.const_zero().into(),
        default_types.isize.const_zero().into(),
    ];
    if ptr.is_const() {
        data[0] = ptr.into();
    }
    if metadata.is_const() {
        data[1] = metadata.into();
    }
    let mut fat_ptr = default_types.fat_ptr.const_named_struct(&data);
    if !ptr.is_const() {
        fat_ptr = builder
            .build_insert_value(fat_ptr, ptr, 0, "")?
            .into_struct_value();
    }
    if !metadata.is_const() {
        fat_ptr = builder
            .build_insert_value(fat_ptr, metadata, 1, "")?
            .into_struct_value();
    }
    Ok(fat_ptr.into())
}

impl TypecheckedExpression {
    fn codegen<'ctx>(
        &self,
        ctx: &mut FunctionCodegenContext<'ctx, '_>,
        scope: DIScope<'ctx>,
        module_id: ModuleId,
    ) -> Result<(), BuilderError> {
        ctx.builder
            .set_current_debug_location(ctx.debug_ctx.location(scope, self.location()));
        match self {
            TypecheckedExpression::AttachVtable(_, dst, src, vtable_id) => {
                let vtable_value = ctx.vtables[vtable_id].as_pointer_value();
                let vtable_isize =
                    ctx.builder
                        .build_bit_cast(vtable_value, ctx.default_types.isize, "")?;
                let fat_ptr = make_fat_ptr(
                    ctx.builder,
                    &ctx.default_types,
                    src.fn_ctx_to_basic_value(ctx).into_pointer_value(),
                    vtable_isize.into_int_value(),
                )?;
                ctx.push_value(*dst, fat_ptr);
                Ok(())
            }
            TypecheckedExpression::DeclareVariable(loc, id, typ, name) => {
                let ptr = ctx.get_value_ptr(*id);
                ctx.debug_ctx.declare_variable(
                    ptr,
                    scope,
                    loc,
                    typ,
                    name,
                    ctx.current_block,
                    module_id,
                    &ctx.tc_ctx.structs.read(),
                );
                Ok(())
            }
            TypecheckedExpression::Return(_, typed_literal) => {
                if typed_literal.to_primitive_type(&ctx.tc_scope, ctx.tc_ctx)
                    == Some(Type::PrimitiveVoid(0))
                {
                    let bb = ctx
                        .builder
                        .get_insert_block()
                        .expect("builder needs a basic block to codegen into");
                    if bb.get_terminator().is_some() {
                        println!("[WARN]: Has a return even tho a terminating instruction was already generated");
                        return Ok(());
                    }
                    return ctx.builder.build_return(None).map(|_| ());
                }
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
                    TypedLiteral::Intrinsic(..) => unreachable!("intrinsic"),
                    lit => ctx
                        .builder
                        .build_return(Some(&lit.fn_ctx_to_basic_value(ctx)))?,
                };
                Ok(())
            }
            TypecheckedExpression::Asm {
                dst,
                inputs,
                registers,
                volatile,
                asm,
                ..
            } => {
                let input_types = inputs
                    .iter()
                    .map(|v| {
                        ctx.tc_scope[*v]
                            .0
                            .to_llvm_basic_type(&ctx.default_types, ctx.structs, ctx.context)
                            .into()
                    })
                    .collect::<Vec<_>>();
                let fn_ty = if matches!(ctx.tc_scope[*dst].0, Type::PrimitiveVoid(0)) {
                    ctx.context.void_type().fn_type(&input_types, false)
                } else {
                    ctx.tc_scope[*dst]
                        .0
                        .to_llvm_basic_type(&ctx.default_types, ctx.structs, ctx.context)
                        .fn_type(&input_types, false)
                };
                let mut constraints = registers.clone();

                // For some targets, Clang unconditionally adds some clobbers to all inline assembly.
                // While this is probably not strictly necessary, if we don't follow Clang's lead
                // here then we may risk tripping LLVM bugs since anything not used by Clang tends
                // to be buggy and regress often.
                // TODO: Add this for mips
                let cpu = ctx.machine.get_cpu();
                match cpu.to_bytes() {
                    b"x86" | b"x86_64" | b"x86-64" => {
                        if !constraints.is_empty() {
                            constraints.push(',');
                        }
                        constraints.push_str("~{dirflag},~{fpsr},~{flags}");
                    }
                    _ => (),
                }

                let asm_fn_ptr = ctx.context.create_inline_asm(
                    fn_ty,
                    asm.clone(),
                    constraints,
                    *volatile,
                    false,
                    None,
                    false,
                );
                let args = inputs
                    .iter()
                    .map(|v| ctx.get_value(*v).into())
                    .collect::<Vec<_>>();
                let val = ctx
                    .builder
                    .build_indirect_call(fn_ty, asm_fn_ptr, &args, "")?;
                ctx.push_value(
                    *dst,
                    val.try_as_basic_value()
                        .left_or_else(|_| ctx.default_types.empty_struct.const_zero().into()),
                );
                Ok(())
            }
            TypecheckedExpression::Block(loc, child, _) => {
                let block = ctx.debug_ctx.new_block(scope, loc, module_id);
                let scope = block.as_debug_info_scope();
                for c in child {
                    c.codegen(ctx, scope, module_id)?;
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
                ctx.goto(if_basic_block);
                let block = ctx.debug_ctx.new_block(scope, &if_block.1, module_id);
                let scope = block.as_debug_info_scope();
                for expr in if_block.0.iter() {
                    expr.codegen(ctx, scope, module_id)?;
                }
                ctx.terminate(|| ctx.builder.build_unconditional_branch(end_basic_block))?;
                ctx.goto(end_basic_block);
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
                ctx.goto(if_basic_block);
                let block = ctx.debug_ctx.new_block(scope, &if_block.1, module_id);
                let scope = block.as_debug_info_scope();
                for expr in if_block.0.iter() {
                    expr.codegen(ctx, scope, module_id)?;
                }

                ctx.terminate(|| ctx.builder.build_unconditional_branch(end_basic_block))?;
                ctx.goto(else_basic_block);
                let block = ctx.debug_ctx.new_block(scope, &else_block.1, module_id);
                let scope = block.as_debug_info_scope();
                for expr in else_block.0.iter() {
                    expr.codegen(ctx, scope, module_id)?;
                }
                ctx.terminate(|| ctx.builder.build_unconditional_branch(end_basic_block))?;
                ctx.goto(end_basic_block);
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
                ctx.goto(cond_basic_block);
                for expr in cond_block {
                    expr.codegen(ctx, scope, module_id)?;
                }
                ctx.terminate(|| {
                    ctx.builder.build_conditional_branch(
                        cond.fn_ctx_to_basic_value(ctx).into_int_value(),
                        body_basic_block,
                        end_basic_block,
                    )
                })?;
                ctx.goto(body_basic_block);
                let block = ctx.debug_ctx.new_block(scope, &body.1, module_id);
                let scope = block.as_debug_info_scope();
                for expr in body.0.iter() {
                    expr.codegen(ctx, scope, module_id)?;
                }
                ctx.terminate(|| ctx.builder.build_unconditional_branch(cond_basic_block))?;
                ctx.goto(end_basic_block);
                Ok(())
            }
            TypecheckedExpression::Range { .. } => todo!(),
            TypecheckedExpression::StoreAssignment(_, dst, src) => {
                match src {
                    TypedLiteral::Dynamic(id) if ctx.tc_scope[*id].1.stack_allocated => {
                        let ty = &ctx.tc_scope[*id].0;
                        let llvm_ty =
                            ty.to_llvm_basic_type(&ctx.default_types, ctx.structs, ctx.context);
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
                        let llvm_ty =
                            ty.to_llvm_basic_type(&ctx.default_types, ctx.structs, ctx.context);
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
                    false,
                )
            }
            TypecheckedExpression::Call(_, dst, fn_ptr, args) => {
                let (fn_ty, fn_ptr) = match fn_ptr {
                    TypedLiteral::Dynamic(id) => (&ctx.tc_scope[*id].0, ctx.get_value(*id).into_pointer_value()),
                    TypedLiteral::Static(id) => (&ctx.tc_ctx.statics.read()[*id].0, ctx.statics[*id].as_pointer_value()),
                    TypedLiteral::Function(..) => unreachable!("TypedLiteral::Function should have been turned into a DirectCall"),
                    TypedLiteral::ExternalFunction(_) => unreachable!("TypedLiteral::ExternalFunction should have been turned into a DirectExternCall"),
                    TypedLiteral::Intrinsic(..) => unreachable!("TypedLiteral::Intrinsic should have been turned into a IntrinsicCall"),
                    _ => unreachable!("{fn_ptr:?} is not callable"),
                };
                let llvm_fn_ty =
                    fn_ty.to_llvm_fn_type(&ctx.default_types, ctx.structs, ctx.context);
                let val = ctx.builder.build_indirect_call(
                    llvm_fn_ty,
                    fn_ptr,
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
                        .left_or_else(|_| ctx.default_types.empty_struct.const_zero().into()),
                );
                Ok(())
            }
            TypecheckedExpression::DirectCall(.., generics) if !generics.is_empty() => panic!("functions shouldn't have generics (they should've been taken care of during monomorphization)"),
            TypecheckedExpression::DirectExternCall(_, dst, func, args)
            | TypecheckedExpression::DirectCall(_, dst, func, args, _) => {
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
                val.set_call_convention(func_value.get_call_conventions());
                ctx.push_value(
                    *dst,
                    val.try_as_basic_value()
                        .left_or_else(|_| ctx.default_types.empty_struct.const_zero().into()),
                );
                Ok(())
            }
            TypecheckedExpression::IntrinsicCall(_, dst, intrinsic, args, generics) => {
                _ = &generics;
                _ = &args;
                match intrinsic {
                    Intrinsic::Unreachable => {
                        ctx.builder.build_unreachable()?;
                        ctx.push_value(*dst, ctx.default_types.empty_struct.const_zero().into());
                    }
                    Intrinsic::Trap => {
                        ctx.intrinsics
                            .trap
                            .build_call(ctx.module, ctx.builder, &[], &[])?;
                        ctx.push_value(*dst, ctx.default_types.empty_struct.const_zero().into());
                    }
                    Intrinsic::Breakpoint => {
                        ctx.intrinsics
                            .breakpoint
                            .build_call(ctx.module, ctx.builder, &[], &[])?;
                        ctx.push_value(*dst, ctx.default_types.empty_struct.const_zero().into());
                    }
                    Intrinsic::ReturnAddress => {
                        let ret = ctx.builder.build_direct_call(
                            ctx.retaddr,
                            &[ctx.default_types.i32.const_zero().into()],
                            "",
                        )?;

                        ctx.push_value(
                            *dst,
                            ret.try_as_basic_value().expect_left(
                                "returnaddress is (i32) -> ptr, and ptr is a basic value",
                            ),
                        );
                    }
                    Intrinsic::SizeOf => {
                        let size = generics[0].size_and_alignment(ctx.pointer_size, &ctx.tc_ctx.structs.read()).0;
                        ctx.push_value(*dst, ctx.default_types.isize.const_int(size, false).into());
                    }
                    Intrinsic::Offset => {
                        let ptr = args[0].fn_ctx_to_basic_value(ctx).into_pointer_value();
                        let offset = args[1].fn_ctx_to_basic_value(ctx).into_int_value();
                        let val = unsafe { ctx.builder.build_in_bounds_gep(ctx.default_types.i8, ptr, &[offset], "")? };
                        ctx.push_value(*dst, val.into());
                    }
                    Intrinsic::GetMetadata => {
                        let ty = &generics[0];
                        if ty.is_sized() {
                            ctx.push_value(*dst, ctx.default_types.isize.const_int(0, false).into());
                        } else {
                            let ptr = args[0].fn_ctx_to_basic_value(ctx).into_struct_value();
                            let metadata = ctx.builder.build_extract_value(ptr, 1, "")?;
                            ctx.push_value(*dst, metadata);
                        }
                    }
                    Intrinsic::WithMetadata => {
                        assert!(generics[0].is_sized());
                        assert!(!generics[1].is_sized());
                        let ptr = args[0].fn_ctx_to_basic_value(ctx);
                        let metadata = args[1].fn_ctx_to_basic_value(ctx);
                        let fat_ptr = ctx.builder.build_insert_value(ctx.default_types.fat_ptr.get_poison(), ptr, 0, "")?;
                        let fat_ptr = ctx.builder.build_insert_value(fat_ptr, metadata, 1, "")?;
                        ctx.push_value(*dst, fat_ptr.as_basic_value_enum());
                    }
                    Intrinsic::VolatileRead => {
                        assert!(generics[0].is_sized());
                        let value = build_deref(args[0].fn_ctx_to_basic_value(ctx).into_pointer_value(), &generics[0], ctx, true)?;
                        ctx.push_value(*dst, value);
                    }
                    Intrinsic::VolatileWrite => {
                        assert!(generics[0].is_sized());
                        build_ptr_store(args[0].fn_ctx_to_basic_value(ctx).into_pointer_value(), args[1].fn_ctx_to_basic_value(ctx), &generics[0], ctx, true)?;
                    }
                    Intrinsic::SizeOfVal => {
                        if generics[0].is_sized() {
                            let size = generics[0].size_and_alignment(ctx.pointer_size, &ctx.tc_ctx.structs.read()).0;
                            ctx.push_value(*dst, ctx.default_types.isize.const_int(size, false).into());
                        } else {
                            match &generics[0] {
                                Type::DynType { .. } => {
                                    let fat_ptr = args[0].fn_ctx_to_basic_value(ctx).into_struct_value();
                                    let vtable_ptr_int = ctx.builder.build_extract_value(fat_ptr, 1, "")?.into_int_value();
                                    let vtable_ptr = ctx.builder.build_int_to_ptr(vtable_ptr_int, ctx.default_types.ptr, "")?;
                                    let size = ctx.builder.build_load(ctx.default_types.isize, vtable_ptr, "")?;
                                    ctx.push_value(*dst, size);
                                }
                                Type::UnsizedArray { typ, .. } => {
                                    let fat_ptr = args[0].fn_ctx_to_basic_value(ctx).into_struct_value();
                                    let len = ctx.builder.build_extract_value(fat_ptr, 1, "")?.into_int_value();
                                    let size_single = typ.size_and_alignment(ctx.pointer_size, &ctx.tc_ctx.structs.read()).0;
                                    let total_size = ctx.builder.build_int_nuw_mul(len, ctx.default_types.isize.const_int(size_single, false), "")?;
                                    ctx.push_value(*dst, total_size.as_basic_value_enum());
                                }
                                Type::PrimitiveStr(_) => {
                                    let fat_ptr = args[0].fn_ctx_to_basic_value(ctx).into_struct_value();
                                    let size = ctx.builder.build_extract_value(fat_ptr, 1, "")?;
                                    ctx.push_value(*dst, size);
                                }
                                Type::Generic { .. } | Type::Trait { .. } | Type::PrimitiveSelf(_) => unreachable!("These should've been resolved by now."),
                                t => unreachable!("{t:?} should be sized"),
                            }
                        }
                    }

                    Intrinsic::Select => todo!(),
                    Intrinsic::ByteSwap => todo!(),
                    Intrinsic::BitReverse => todo!(),
                    Intrinsic::CountLeadingZeros => todo!(),
                    Intrinsic::CountTrailingZeros => todo!(),
                    Intrinsic::CountOnes => todo!(),
                    Intrinsic::AddWithOverflow => todo!(),
                    Intrinsic::SubWithOverflow => todo!(),
                    Intrinsic::MulWithOverflow => todo!(),
                    Intrinsic::WrappingAdd => todo!(),
                    Intrinsic::WrappingSub => todo!(),
                    Intrinsic::WrappingMul => todo!(),
                    Intrinsic::SaturatingAdd => todo!(),
                    Intrinsic::SaturatingSub => todo!(),
                    Intrinsic::UncheckedAdd => todo!(),
                    Intrinsic::UncheckedSub => todo!(),
                    Intrinsic::UncheckedMul => todo!(),
                    Intrinsic::UncheckedDiv => todo!(),
                    Intrinsic::UncheckedMod => todo!(),
                    Intrinsic::UncheckedShl => todo!(),
                    Intrinsic::UncheckedShr => todo!(),

                    // TODO: raii
                    Intrinsic::Drop => todo!(),
                    Intrinsic::DropInPlace => todo!(),
                    Intrinsic::Forget => todo!(),
                    // TODO: raii
                    Intrinsic::Read => todo!(),
                    Intrinsic::Write => todo!(),

                    // TODO: replace these at compile time because they
                    Intrinsic::Location => {}
                    Intrinsic::TypeName => todo!(),
                }
                Ok(())
            }
            TypecheckedExpression::Pos(_, dst, src) => {
                ctx.push_value(*dst, src.fn_ctx_to_basic_value(ctx));
                Ok(())
            }
            TypecheckedExpression::Neg(_, dst, src) => {
                let src = src.fn_ctx_to_basic_value(ctx);
                let value = match src {
                    BasicValueEnum::IntValue(int_value) => ctx
                        .builder
                        .build_int_neg(int_value, "")?
                        .as_basic_value_enum(),
                    BasicValueEnum::FloatValue(float_value) => ctx
                        .builder
                        .build_float_neg(float_value, "")?
                        .as_basic_value_enum(),
                    _ => unreachable!(),
                };
                ctx.push_value(*dst, value);
                Ok(())
            }
            TypecheckedExpression::BNot(_, dst, src) | TypecheckedExpression::LNot(_, dst, src) => {
                let src = src.fn_ctx_to_basic_value(ctx).into_int_value();
                ctx.push_value(*dst, ctx.builder.build_not(src, "")?.as_basic_value_enum());
                Ok(())
            }
            TypecheckedExpression::Add(_, dst, lhs, rhs) => {
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
                    "tc should have errored if you try to add 2 non-number values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::Sub(_, dst, lhs, rhs) => {
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
                    "tc should have errored if you try to subtract 2 non-number values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::Mul(_, dst, lhs, rhs) => {
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
                    "tc should have errored if you try to multiply 2 non-number values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::Div(_, dst, lhs, rhs) => {
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
                    "tc should have errored if you try to divide 2 non-number values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::Mod(_, dst, lhs, rhs) => {
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
                    "tc should have errored if you try to mod 2 non-number values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::BAnd(_, dst, lhs, rhs) => {
                let typ = lhs.to_type(&ctx.tc_scope, ctx.tc_ctx);
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
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
            TypecheckedExpression::BOr(_, dst, lhs, rhs) => {
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
            TypecheckedExpression::BXor(_, dst, lhs, rhs) => {
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
            TypecheckedExpression::GreaterThan(_, dst, lhs, rhs) => {
                let typ = lhs
                    .to_primitive_type(&ctx.tc_scope, ctx.tc_ctx)
                    .expect("tc should have errored if you try to compare 2 non-number values");
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_compare(IntPredicate::SGT),
                    build_int_compare(IntPredicate::UGT),
                    build_float_compare(FloatPredicate::UGT),
                    "tc should have errored if you try to compare 2 non-number values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::LessThan(_, dst, lhs, rhs) => {
                let typ = lhs
                    .to_primitive_type(&ctx.tc_scope, ctx.tc_ctx)
                    .expect("tc should have errored if you try to compare 2 non-number values");
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_compare(IntPredicate::SLT),
                    build_int_compare(IntPredicate::ULT),
                    build_float_compare(FloatPredicate::ULT),
                    "tc should have errored if you try to compare 2 non-number values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::LAnd(..) => todo!(),
            TypecheckedExpression::LOr(..) => todo!(),
            TypecheckedExpression::GreaterThanEq(_, dst, lhs, rhs) => {
                let typ = lhs
                    .to_primitive_type(&ctx.tc_scope, ctx.tc_ctx)
                    .expect("tc should have errored if you try to compare 2 non-number values");
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_compare(IntPredicate::SGE),
                    build_int_compare(IntPredicate::UGE),
                    build_float_compare(FloatPredicate::UGE),
                    "tc should have errored if you try to compare 2 non-number values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::LessThanEq(_, dst, lhs, rhs) => {
                let typ = lhs
                    .to_primitive_type(&ctx.tc_scope, ctx.tc_ctx)
                    .expect("tc should have errored if you try to compare 2 non-number values");
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_compare(IntPredicate::SLE),
                    build_int_compare(IntPredicate::ULE),
                    build_float_compare(FloatPredicate::ULE),
                    "tc should have errored if you try to compare 2 non-number values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::Eq(_, dst, lhs, rhs) => {
                let typ = lhs
                    .to_primitive_type(&ctx.tc_scope, ctx.tc_ctx)
                    .expect("tc should have errored if you try to compare 2 non-number values");
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_compare(IntPredicate::EQ),
                    build_int_compare(IntPredicate::EQ),
                    build_float_compare(FloatPredicate::UEQ),
                    "tc should have errored if you try to compare 2 non-number values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::Neq(_, dst, lhs, rhs) => {
                let typ = lhs
                    .to_primitive_type(&ctx.tc_scope, ctx.tc_ctx)
                    .expect("tc should have errored if you try to compare 2 non-number values");
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_compare(IntPredicate::NE),
                    build_int_compare(IntPredicate::NE),
                    build_float_compare(FloatPredicate::UNE),
                    "tc should have errored if you try to compare 2 non-number values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::LShift(_, dst, lhs, rhs) => {
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
            TypecheckedExpression::RShift(_, dst, lhs, rhs) => {
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
                ctx.push_value(*dst, ctx.default_types.ptr.const_zero().into());
                Ok(())
            }
            TypecheckedExpression::Reference(_, dst, rhs) => {
                let value = match rhs {
                    TypedLiteral::Dynamic(id) if ctx.tc_scope[*id].0 == Type::PrimitiveVoid(0) => ctx.default_types.ptr.const_zero().into(),
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
            TypecheckedExpression::Dereference(_, dst, rhs) => {
                if ctx.tc_scope[*dst].1.stack_allocated {
                    let ty = ctx.tc_scope[*dst].0.to_llvm_basic_type(
                        &ctx.default_types,
                        ctx.structs,
                        ctx.context,
                    );
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
                    false,
                )?;
                ctx.push_value(*dst, value);
                Ok(())
            }
            // &struct, <- i64 0, i32 <idx>
            // &[a] <- extractvalue 0 and then i64 n of that
            // &[a; n] <- i64 0 n (for getelementptr [a; n]) or i64 n (for getelementptr a)
            TypecheckedExpression::Offset(_, dst, src, offset) => {
                let ty = src.to_type(&ctx.tc_scope, ctx.tc_ctx);
                match ty.as_ref() {
                    Type::Struct {
                        struct_id,
                        num_references: 1,
                        ..
                    } => {
                        let offset = match offset {
                            OffsetValue::Dynamic(_) => unreachable!("dynamic struct offset"),
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
                    Type::Tuple {
                        num_references: 1, ..
                    } => {
                        let offset = match offset {
                            OffsetValue::Dynamic(_) => unreachable!("dynamic struct offset"),
                            OffsetValue::Static(v) => {
                                ctx.default_types.i32.const_int(*v as u64, false)
                            }
                        };
                        let value = unsafe {
                            ctx.builder.build_in_bounds_gep(
                                ty.to_llvm_basic_type(&ctx.default_types, ctx.structs, ctx.context),
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
                                typ.to_llvm_basic_type(
                                    &ctx.default_types,
                                    ctx.structs,
                                    ctx.context,
                                ),
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
                                typ.to_llvm_basic_type(
                                    &ctx.default_types,
                                    ctx.structs,
                                    ctx.context,
                                ),
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
            TypecheckedExpression::OffsetNonPointer(_, dst, src, offset_value) => {
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
            TypecheckedExpression::DynCall(_, dst, args, offset) => {
                let mut arguments = args
                    .iter()
                    .map(|v| v.fn_ctx_to_basic_value(ctx).into())
                    .collect::<Vec<BasicMetadataValueEnum<'ctx>>>();
                let dyn_ptr = arguments[0].into_struct_value();
                let real_ptr = ctx.builder.build_extract_value(dyn_ptr, 0, "")?;
                arguments[0] = real_ptr.into();
                let vtable_ptr_isize = ctx
                    .builder
                    .build_extract_value(dyn_ptr, 1, "")?
                    .into_int_value();
                let vtable_ptr =
                    ctx.builder
                        .build_int_to_ptr(vtable_ptr_isize, ctx.default_types.ptr, "")?;
                let fn_ptr_ptr = unsafe {
                    ctx.builder.build_gep(
                        ctx.default_types.ptr,
                        vtable_ptr,
                        &[ctx
                            .default_types
                            .isize
                            .const_int(*offset as u64 + 1 /* skip length */, false)],
                        "",
                    )
                }?;
                let fn_ptr = ctx
                    .builder
                    .build_load(ctx.default_types.ptr, fn_ptr_ptr, "")?
                    .into_pointer_value();
                let param_types = std::iter::once(ctx.default_types.ptr.into())
                    .chain(args.iter().skip(1).map(|v| {
                        v.to_type(&ctx.tc_scope, ctx.tc_ctx)
                            .to_llvm_basic_type(&ctx.default_types, ctx.structs, ctx.context)
                            .into()
                    }))
                    .collect::<Vec<_>>();
                let fn_ty = match &ctx.tc_scope[*dst].0 {
                    Type::PrimitiveNever | Type::PrimitiveVoid(0) => {
                        ctx.context.void_type().fn_type(&param_types, false)
                    }
                    v => v
                        .to_llvm_basic_type(&ctx.default_types, ctx.structs, ctx.context)
                        .fn_type(&param_types, false),
                };

                let res = ctx
                    .builder
                    .build_indirect_call(fn_ty, fn_ptr, &arguments, "")?;
                ctx.push_value(
                    *dst,
                    res.try_as_basic_value()
                        .left_or_else(|_| ctx.default_types.empty_struct.const_zero().into()),
                );
                Ok(())
            }
            TypecheckedExpression::Bitcast(_, new, old) => {
                let new_typ = &ctx.tc_scope[*new].0;
                let old_typ = old.to_type(&ctx.tc_scope, ctx.tc_ctx);
                assert!(new_typ.refcount() == 0 || new_typ.is_thin_ptr());
                assert!(old_typ.refcount() == 0 || old_typ.is_thin_ptr());
                let new_value = ctx.builder.build_bit_cast(
                    old.fn_ctx_to_basic_value(ctx),
                    new_typ.to_llvm_basic_type(&ctx.default_types, ctx.structs, ctx.context),
                    "",
                )?;
                ctx.push_value(*new, new_value);
                Ok(())
            }
            TypecheckedExpression::Literal(_, dst, src)
            | TypecheckedExpression::Alias(_, dst, src) => {
                if let TypedLiteral::Dynamic(id) = src {
                    if ctx.tc_scope[*dst].1.stack_allocated && ctx.tc_scope[*id].1.stack_allocated {
                        let ty = &ctx.tc_scope[*dst].0;
                        let llvm_ty =
                            ty.to_llvm_basic_type(&ctx.default_types, ctx.structs, ctx.context);
                        let alignment = get_alignment(llvm_ty);
                        let new_ptr = ctx.builder.build_alloca(llvm_ty, "")?;
                        ctx.builder.build_memmove(
                            new_ptr,
                            alignment,
                            ctx.get_value_ptr(*id),
                            alignment,
                            llvm_ty.size_of().expect("llvm type should always be sized"),
                        )?;
                        ctx.push_value_raw(*dst, new_ptr.into());
                        return Ok(());
                    }
                } else if let TypedLiteral::Static(id) = src {
                    if ctx.tc_scope[*dst].1.stack_allocated {
                        let ty = &ctx.tc_scope[*dst].0;
                        let llvm_ty =
                            ty.to_llvm_basic_type(&ctx.default_types, ctx.structs, ctx.context);
                        let alignment = get_alignment(llvm_ty);
                        let new_ptr = ctx.builder.build_alloca(llvm_ty, "")?;
                        ctx.builder.build_memmove(
                            new_ptr,
                            alignment,
                            ctx.statics[*id].as_pointer_value(),
                            alignment,
                            llvm_ty.size_of().expect("llvm type should always be sized"),
                        )?;
                        ctx.push_value_raw(*dst, new_ptr.into());
                        return Ok(());
                    }
                }
                ctx.push_value(*dst, src.fn_ctx_to_basic_value(ctx));
                Ok(())
            }
            TypecheckedExpression::PtrToInt(_, dst, src) => {
                let value = ctx.builder.build_ptr_to_int(
                    src.fn_ctx_to_basic_value(ctx).into_pointer_value(),
                    ctx.tc_scope[*dst]
                        .0
                        .to_llvm_basic_type(&ctx.default_types, ctx.structs, ctx.context)
                        .into_int_type(),
                    "",
                )?;
                ctx.push_value(*dst, value.into());
                Ok(())
            }
            TypecheckedExpression::IntToPtr(_, dst, src) => {
                let value = ctx.builder.build_int_to_ptr(
                    src.fn_ctx_to_basic_value(ctx).into_int_value(),
                    ctx.default_types.ptr,
                    "",
                )?;
                ctx.push_value(*dst, value.into());
                Ok(())
            }
            TypecheckedExpression::StripMetadata(_, dst, src) => {
                let (value, is_ptr) = if let TypedLiteral::Dynamic(src) = src {
                    (ctx._scope[*src], ctx.tc_scope[*src].1.stack_allocated)
                } else if let TypedLiteral::Static(id) = src {
                    (ctx.statics[*id].as_pointer_value().into(), true)
                } else {
                    (src.fn_ctx_to_basic_value(ctx), false)
                };
                let pointer = if is_ptr {
                    let pointer_pointer = ctx.builder.build_struct_gep(
                        ctx.default_types.fat_ptr,
                        value.into_pointer_value(),
                        0,
                        "",
                    )?;
                    build_deref(pointer_pointer, &ctx.tc_scope[*dst].0, ctx, false)?
                } else {
                    ctx.builder
                        .build_extract_value(value.into_struct_value(), 0, "")?
                };
                ctx.push_value(*dst, pointer);
                Ok(())
            }
            Self::IntCast(_, dst, src) => {
                let src_ty = src
                    .to_primitive_type(&ctx.tc_scope, ctx.tc_ctx)
                    .expect("can only cast primitive types");
                let dst_ty = &ctx.tc_scope[*dst].0;
                let src_value = src.fn_ctx_to_basic_value(ctx);
                // u8 -> bool
                if src_ty == Type::PrimitiveU8(0) && *dst_ty == Type::PrimitiveBool(0) {
                    let value = ctx.builder.build_int_truncate(
                        src_value.into_int_value(),
                        ctx.default_types.bool,
                        "",
                    )?;
                    ctx.push_value(*dst, value.into());
                    return Ok(());
                }
                // bool -> u8
                if src_ty == Type::PrimitiveBool(0) && *dst_ty == Type::PrimitiveU8(0) {
                    let value = ctx.builder.build_int_z_extend(
                        src_value.into_int_value(),
                        ctx.default_types.i8,
                        "",
                    )?;
                    ctx.push_value(*dst, value.into());
                    return Ok(());
                }
                // f32 -> f64
                if src_ty == Type::PrimitiveF32(0) && *dst_ty == Type::PrimitiveF64(0) {
                    let value = ctx.builder.build_float_ext(
                        src_value.into_float_value(),
                        ctx.default_types.f64,
                        "",
                    )?;
                    ctx.push_value(*dst, value.into());
                    return Ok(());
                }
                // f64 -> f32
                if src_ty == Type::PrimitiveBool(0) && *dst_ty == Type::PrimitiveU8(0) {
                    let value = ctx.builder.build_float_trunc(
                        src_value.into_float_value(),
                        ctx.default_types.f32,
                        "",
                    )?;
                    ctx.push_value(*dst, value.into());
                    return Ok(());
                }
                if src_ty.is_int_like() && dst_ty.is_int_like() {
                    let isize_bitwidth = ctx.default_types.isize.get_bit_width();
                    let trunc =
                        src_ty.get_bitwidth(isize_bitwidth) > dst_ty.get_bitwidth(isize_bitwidth);
                    let ty = ctx
                        .context
                        .custom_width_int_type(dst_ty.get_bitwidth(isize_bitwidth));
                    let value = if trunc {
                        ctx.builder.build_int_truncate_or_bit_cast(
                            src_value.into_int_value(),
                            ty,
                            "",
                        )?
                    } else if dst_ty.is_unsigned() {
                        ctx.builder.build_int_z_extend_or_bit_cast(
                            src_value.into_int_value(),
                            ty,
                            "",
                        )?
                    } else {
                        ctx.builder.build_int_s_extend_or_bit_cast(
                            src_value.into_int_value(),
                            ty,
                            "",
                        )?
                    };
                    ctx.push_value(*dst, value.into());
                    return Ok(());
                }
                if src_ty.is_float() {
                    let isize_bitwidth = ctx.default_types.isize.get_bit_width();
                    let ty = ctx
                        .context
                        .custom_width_int_type(dst_ty.get_bitwidth(isize_bitwidth));
                    let value = if dst_ty.is_unsigned() {
                        ctx.builder.build_float_to_unsigned_int(
                            src_value.into_float_value(),
                            ty,
                            "",
                        )?
                    } else {
                        ctx.builder.build_float_to_signed_int(
                            src_value.into_float_value(),
                            ty,
                            "",
                        )?
                    };
                    ctx.push_value(*dst, value.into());
                    Ok(())
                } else {
                    let ty = match dst_ty {
                        Type::PrimitiveF32(0) => ctx.default_types.f32,
                        Type::PrimitiveF64(0) => ctx.default_types.f64,
                        _ => unreachable!("not a float type: {:?}", dst_ty),
                    };
                    let value = if src_ty.is_unsigned() {
                        ctx.builder.build_unsigned_int_to_float(
                            src_value.into_int_value(),
                            ty,
                            "",
                        )?
                    } else {
                        ctx.builder
                            .build_signed_int_to_float(src_value.into_int_value(), ty, "")?
                    };
                    ctx.push_value(*dst, value.into());
                    Ok(())
                }
            }
            TypecheckedExpression::Unreachable(_) => Ok(_ = ctx.builder.build_unreachable()?),
            TypecheckedExpression::Empty(_) => Ok(()),
            TypecheckedExpression::None => {
                unreachable!("None-expressions are not valid and indicate an error")
            }
        }
    }
}
