use context::{DefaultTypes, ExternalFunctionsStore, FunctionsStore, StaticsStore, StructsStore};
use core::panic;
use debug_builder::DebugContext;
use intrinsics::LLVMIntrinsics;
use std::{collections::HashMap, ops::Deref};

use mira::typechecking::{
    Ty, TyKind, TypecheckedModule, TypecheckingContext, TypedTrait, default_types,
    expression::{OffsetValue, TypecheckedExpression, TypedLiteral},
    typechecking::ScopeTypeMetadata,
};
use mira_common::store::StoreKey;
use mira_parser::std_annotations::intrinsic::Intrinsic;
use mira_spans::interner::Symbol;
pub mod mangling;
pub use context::{CodegenConfig, CodegenContext, CodegenContextBuilder, Optimizations};
pub use error::CodegenError;
pub use inkwell::support::LLVMString;
use inkwell::{
    FloatPredicate, IntPredicate,
    basic_block::BasicBlock,
    builder::{Builder, BuilderError},
    context::Context,
    debug_info::{AsDIScope, DIScope},
    module::Module,
    targets::TargetMachine,
    types::{AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::{
        BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, GlobalValue, IntValue,
        PointerValue,
    },
};

mod context;
mod debug_builder;
mod error;
mod intrinsics;

impl<'ctx, 'arena> CodegenContext<'ctx, 'arena> {
    pub fn make_function_codegen_context<'me>(
        &'me mut self,
        tc_scope: Vec<(Ty<'arena>, ScopeTypeMetadata)>,
        current_fn: FunctionValue<'ctx>,
        bb: BasicBlock<'ctx>,
    ) -> FunctionCodegenContext<'ctx, 'arena, 'me> {
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

impl<'ctx> FunctionCodegenContext<'ctx, '_, '_> {
    pub fn goto(&mut self, bb: BasicBlock<'ctx>) {
        self.position_at_end(bb);
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

/// 'cg: codegen
pub struct FunctionCodegenContext<'ctx, 'arena, 'cg> {
    tc_scope: Vec<(Ty<'arena>, ScopeTypeMetadata)>,
    tc_ctx: &'cg TypecheckingContext<'arena>,
    _scope: Vec<BasicValueEnum<'ctx>>,
    builder: &'cg Builder<'ctx>,
    context: &'ctx Context,
    default_types: DefaultTypes<'ctx>,
    current_fn: FunctionValue<'ctx>,
    functions: &'cg FunctionsStore<'ctx, 'arena>,
    external_functions: &'cg ExternalFunctionsStore<'ctx, 'arena>,
    structs: &'cg StructsStore<'ctx, 'arena>,
    statics: &'cg StaticsStore<'ctx, 'arena>,
    string_map: &'cg HashMap<Symbol<'arena>, GlobalValue<'ctx>>,
    vtables: &'cg HashMap<(Ty<'arena>, Vec<StoreKey<TypedTrait<'arena>>>), GlobalValue<'ctx>>,
    debug_ctx: &'cg mut DebugContext<'ctx, 'arena>,
    machine: &'cg TargetMachine,
    intrinsics: &'cg LLVMIntrinsics,
    retaddr: FunctionValue<'ctx>,
    module: &'cg Module<'ctx>,
    current_block: BasicBlock<'ctx>,
    pointer_size: u64,
}

impl<'ctx, 'arena> FunctionCodegenContext<'ctx, 'arena, '_> {
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
            .build_alloca(
                self.tc_scope[id].0.to_llvm_basic_type(
                    &self.default_types,
                    self.structs,
                    self.context,
                ),
                "",
            )
            .expect("failed to build alloca for a stack allocated value");
        build_ptr_store(allocated_value, value, self.tc_scope[id].0, self, false)
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
            build_deref(ptr, self.tc_scope[id].0, self, false)
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

    fn lit_to_basic_value(&self, lit: &TypedLiteral<'arena>) -> BasicValueEnum<'ctx> {
        to_basic_value(
            lit,
            &|id| self.get_value(id),
            &self.default_types,
            self.structs,
            self,
            self.statics,
            self.functions,
            self.external_functions,
            self.string_map,
            self.context,
        )
    }
}

trait TyKindExt<'ctx, 'arena> {
    fn to_llvm_basic_type(
        &self,
        default_types: &DefaultTypes<'ctx>,
        structs: &StructsStore<'ctx, 'arena>,
        ctx: &'ctx Context,
    ) -> BasicTypeEnum<'ctx>;

    fn to_llvm_fn_type(
        &self,
        default_types: &DefaultTypes<'ctx>,
        structs: &StructsStore<'ctx, 'arena>,
        ctx: &'ctx Context,
    ) -> FunctionType<'ctx>;
}

impl<'ctx, 'arena> TyKindExt<'ctx, 'arena> for TyKind<'arena> {
    fn to_llvm_basic_type(
        &self,
        default_types: &DefaultTypes<'ctx>,
        structs: &StructsStore<'ctx, 'arena>,
        ctx: &'ctx Context,
    ) -> BasicTypeEnum<'ctx> {
        match self {
            TyKind::Ref(t) => {
                if t.is_sized() {
                    default_types.ptr.into()
                } else {
                    default_types.fat_ptr.into()
                }
            }
            TyKind::Generic { .. } => unreachable!("generics should be resolved by now"),
            TyKind::UnsizedArray { .. } => panic!("llvm types must be sized, `[_]` is not"),
            TyKind::PrimitiveStr => panic!("llvm types must be sized, `str` is not"),
            TyKind::PrimitiveSelf => unreachable!("Self must be resolved at this point"),
            TyKind::DynType { .. } => panic!("llvm types must be sized, `dyn _` is not"),
            TyKind::Struct { struct_id, .. } => structs[*struct_id].into(),
            TyKind::Tuple(elements) => ctx
                .struct_type(
                    &elements
                        .iter()
                        .map(|v| v.to_llvm_basic_type(default_types, structs, ctx))
                        .collect::<Vec<_>>(),
                    false,
                )
                .into(),
            TyKind::SizedArray {
                typ,
                number_elements,
                ..
            } => typ
                .to_llvm_basic_type(default_types, structs, ctx)
                .array_type(*number_elements as u32)
                .into(),
            // our function types are always pointers because all function types are pointers in llvm
            TyKind::Function(..) => default_types.ptr.into(),
            TyKind::PrimitiveNever | TyKind::PrimitiveVoid => panic!(
                "void and never should be ignored as llvm types outside of function return values"
            ),
            TyKind::PrimitiveU8 | TyKind::PrimitiveI8 => default_types.i8.into(),
            TyKind::PrimitiveU16 | TyKind::PrimitiveI16 => default_types.i16.into(),
            TyKind::PrimitiveU32 | TyKind::PrimitiveI32 => default_types.i32.into(),
            TyKind::PrimitiveU64 | TyKind::PrimitiveI64 => default_types.i64.into(),
            TyKind::PrimitiveUSize | TyKind::PrimitiveISize => default_types.isize.into(),
            TyKind::PrimitiveF32 => default_types.f32.into(),
            TyKind::PrimitiveF64 => default_types.f64.into(),
            TyKind::PrimitiveBool => default_types.bool.into(),
        }
    }

    fn to_llvm_fn_type(
        &self,
        default_types: &DefaultTypes<'ctx>,
        structs: &StructsStore<'ctx, 'arena>,
        ctx: &'ctx Context,
    ) -> FunctionType<'ctx> {
        let TyKind::Function(v) = self else {
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

#[allow(clippy::too_many_arguments)]
fn to_basic_value<'ctx, 'arena>(
    ty: &TypedLiteral<'arena>,
    scope_get_value: &dyn Fn(usize) -> BasicValueEnum<'ctx>,
    default_types: &DefaultTypes<'ctx>,
    structs: &StructsStore<'ctx, 'arena>,
    builder: &Builder<'ctx>,
    statics: &StaticsStore<'ctx, 'arena>,
    functions: &FunctionsStore<'ctx, 'arena>,
    ext_functions: &ExternalFunctionsStore<'ctx, 'arena>,
    string_map: &HashMap<Symbol, GlobalValue<'ctx>>,
    ctx: &'ctx Context,
) -> BasicValueEnum<'ctx> {
    match ty {
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
            let size = global_str.len();
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
            let elem = to_basic_value(
                elem,
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
                        let val = to_basic_value(
                            v,
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
                        let val = to_basic_value(
                            v,
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
                let val = to_basic_value(
                    v,
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

macro_rules! f_s_u {
    ($ctx:expr, $typ: expr, $lhs: expr, $rhs: expr, $sint: ident($($sint_val:expr),*), $uint: ident($($uint_val:expr),*), $float: ident($($float_val:expr),*), $err:literal) => {
        match **$typ {
            TyKind::PrimitiveI8
            | TyKind::PrimitiveI16
            | TyKind::PrimitiveI32
            | TyKind::PrimitiveI64
            | TyKind::PrimitiveISize => $ctx
                .$sint($($sint_val,)* $lhs.into_int_value(), $rhs.into_int_value(), "")?
                .into(),
            TyKind::PrimitiveU8
            | TyKind::PrimitiveU16
            | TyKind::PrimitiveU32
            | TyKind::PrimitiveU64
            | TyKind::PrimitiveUSize => $ctx
                .$uint($($uint_val,)* $lhs.into_int_value(), $rhs.into_int_value(), "")?
                .into(),
            TyKind::PrimitiveF32 | TyKind::PrimitiveF64 => $ctx
                .$float($($float_val,)* $lhs.into_float_value(), $rhs.into_float_value(), "")?
                .into(),
            _ => unreachable!(concat!($err, "  -- Type: {}"), $typ),
        }
    };
    (with_bool $ctx:expr, $typ: expr, $lhs: expr, $rhs: expr, $sint: ident, $uint: ident, $float: ident, $bool: ident, $err:literal) => {
        match **$typ {
            Type::PrimitiveI8(0)
            | Type::PrimitiveI16(0)
            | Type::PrimitiveI32(0)
            | Type::PrimitiveI64(0)
            | Type::PrimitiveISize(0) => $ctx
                .$sint($lhs.into_int_value(), $rhs.into_int_value(), "")?
                .into(),
            Type::PrimitiveU8(0)
            | Type::PrimitiveU16(0)
            | Type::PrimitiveU32(0)
            | Type::PrimitiveU64(0)
            | Type::PrimitiveUSize(0) => $ctx
                .$uint($lhs.into_int_value(), $rhs.into_int_value(), "")?
                .into(),
            Type::PrimitiveF32(0) | Type::PrimitiveF64(0) => $ctx
                .$float($lhs.into_float_value(), $rhs.into_float_value(), "")?
                .into(),
            Type::PrimitiveBool(0) => $ctx
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

fn build_deref<'ctx, 'arena>(
    left_side: PointerValue<'ctx>,
    ty: Ty<'arena>,
    ctx: &FunctionCodegenContext<'ctx, 'arena, '_>,
    volatile: bool,
) -> Result<BasicValueEnum<'ctx>, BuilderError> {
    if ty == default_types::void || ty == default_types::never {
        return Ok(ctx.lit_to_basic_value(&TypedLiteral::Void));
    }

    if ty.refcount() > 0 {
        if ty.is_thin_ptr() {
            return Ok(make_volatile(
                ctx.build_load(ctx.default_types.ptr, left_side, "")?,
                volatile,
            ));
        } else {
            let actual_ptr = ctx.build_load(ctx.default_types.ptr, left_side, "")?;
            make_volatile(actual_ptr, volatile);
            let offset_ptr = ctx.build_struct_gep(ctx.default_types.fat_ptr, left_side, 1, "")?;
            let metadata = ctx.build_load(ctx.default_types.isize, offset_ptr, "")?;
            make_volatile(metadata, volatile);
            let ptr_only_struct =
                ctx.build_insert_value(ctx.default_types.fat_ptr.get_poison(), actual_ptr, 0, "")?;
            return Ok(ctx
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
            let llvm_structure = ty
                .to_llvm_basic_type(&ctx.default_types, ctx.structs, ctx.context)
                .into_struct_type();
            let structure = &ctx.tc_ctx.structs.read()[*struct_id];
            let mut value = llvm_structure.get_poison();
            for i in 0..structure.elements.len() {
                let offset_val = ctx.build_struct_gep(llvm_structure, left_side, i as u32, "")?;
                let element_val = build_deref(offset_val, structure.elements[i].1, ctx, volatile)?;
                value = ctx
                    .build_insert_value(value, element_val, i as u32, "")?
                    .into_struct_value();
            }
            Ok(value.into())
        }
        TyKind::Tuple(elements) => {
            let llvm_structure = ty
                .to_llvm_basic_type(&ctx.default_types, ctx.structs, ctx.context)
                .into_struct_type();
            let mut value = llvm_structure.get_poison();
            for (i, elem) in elements.iter().enumerate() {
                let offset_val = ctx.build_struct_gep(llvm_structure, left_side, i as u32, "")?;
                let element_val = build_deref(offset_val, *elem, ctx, volatile)?;
                value = ctx
                    .build_insert_value(value, element_val, i as u32, "")?
                    .into_struct_value();
            }
            Ok(value.into())
        }
        TyKind::SizedArray {
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
                    ctx.build_in_bounds_gep(
                        llvm_element_ty,
                        left_side,
                        &[ctx.default_types.isize.const_int(i as u64, false)],
                        "",
                    )
                }?;
                let element_val = build_deref(offset_val, *typ, ctx, volatile)?;
                value = ctx
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
            ctx.build_load(
                ty.to_llvm_basic_type(&ctx.default_types, ctx.structs, ctx.context),
                left_side,
                "",
            )?,
            volatile,
        )),
    }
}

fn build_ptr_store<'ctx, 'arena>(
    left_side: PointerValue<'ctx>,
    right_side: BasicValueEnum<'ctx>,
    ty: Ty<'arena>,
    ctx: &FunctionCodegenContext<'ctx, 'arena, '_>,
    volatile: bool,
) -> Result<(), BuilderError> {
    if ty.refcount() > 0 {
        if ty.is_thin_ptr() {
            ctx.build_store(left_side, right_side)?
                .set_volatile(volatile)
                .expect("setting volatile should never fail");
            return Ok(());
        } else {
            let actual_ptr = ctx.build_extract_value(right_side.into_struct_value(), 0, "")?;
            let metadata = ctx.build_extract_value(right_side.into_struct_value(), 1, "")?;
            let actual_ptr_ptr =
                ctx.build_struct_gep(ctx.default_types.fat_ptr, left_side, 0, "")?;
            let metadata_ptr = ctx.build_struct_gep(ctx.default_types.fat_ptr, left_side, 1, "")?;
            ctx.build_store(actual_ptr_ptr, actual_ptr)?
                .set_volatile(volatile)
                .expect("setting volatile should never fail");
            ctx.build_store(metadata_ptr, metadata)?
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
            let structure = &ctx.tc_ctx.structs.read()[*struct_id];
            let llvm_ty = ty.to_llvm_basic_type(&ctx.default_types, ctx.structs, ctx.context);
            for (idx, ty) in structure.elements.iter().map(|v| &v.1).enumerate() {
                let val =
                    ctx.build_extract_value(right_side.into_struct_value(), idx as u32, "")?;
                let ptr = ctx.build_struct_gep(llvm_ty, left_side, idx as u32, "")?;
                build_ptr_store(ptr, val, *ty, ctx, volatile)?;
            }
        }
        TyKind::Tuple(elements) => {
            let llvm_ty = ty.to_llvm_basic_type(&ctx.default_types, ctx.structs, ctx.context);
            for (idx, ty) in elements.iter().enumerate() {
                let val =
                    ctx.build_extract_value(right_side.into_struct_value(), idx as u32, "")?;
                let ptr = ctx.build_struct_gep(llvm_ty, left_side, idx as u32, "")?;
                build_ptr_store(ptr, val, *ty, ctx, volatile)?;
            }
        }
        TyKind::SizedArray {
            typ: ty,
            number_elements,
            ..
        } => {
            let llvm_ty = ty.to_llvm_basic_type(&ctx.default_types, ctx.structs, ctx.context);
            for i in 0..*number_elements {
                let val = ctx.build_extract_value(right_side.into_array_value(), i as u32, "")?;
                let ptr = unsafe {
                    ctx.build_in_bounds_gep(
                        llvm_ty,
                        left_side,
                        &[ctx.default_types.isize.const_int(i as u64, false)],
                        "",
                    )
                }?;
                build_ptr_store(ptr, val, *ty, ctx, volatile)?;
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
            ctx.build_store(left_side, right_side)?
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

impl<'ctx, 'arena> FunctionCodegenContext<'ctx, 'arena, '_> {
    fn codegen(
        &mut self,
        expr: &TypecheckedExpression<'arena>,
        scope: DIScope<'ctx>,
        module_id: StoreKey<TypecheckedModule<'arena>>,
    ) -> Result<(), BuilderError> {
        self.set_current_debug_location(self.debug_ctx.location(scope, expr.span()));
        match expr {
            TypecheckedExpression::AttachVtable(_, dst, src, vtable_id) => {
                let vtable_value = self.vtables[vtable_id].as_pointer_value();
                let vtable_isize =
                    self.build_bit_cast(vtable_value, self.default_types.isize, "")?;
                let fat_ptr = make_fat_ptr(
                    self,
                    &self.default_types,
                    self.lit_to_basic_value(src).into_pointer_value(),
                    vtable_isize.into_int_value(),
                )?;
                self.push_value(*dst, fat_ptr);
                Ok(())
            }
            TypecheckedExpression::DeclareVariable(span, id, typ, name) => {
                let ptr = self.get_value_ptr(*id);
                self.debug_ctx.declare_variable(
                    ptr,
                    scope,
                    *span,
                    *typ,
                    *name,
                    self.current_block,
                    module_id,
                    &self.tc_ctx.structs.read(),
                );
                Ok(())
            }
            TypecheckedExpression::Return(_, typed_literal) => {
                if typed_literal.to_primitive_type(&self.tc_scope, self.tc_ctx)
                    == Some(default_types::void)
                {
                    let bb = self
                        .get_insert_block()
                        .expect("builder needs a basic block to codegen into");
                    if bb.get_terminator().is_some() {
                        println!(
                            "[WARN]: Has a return even tho a terminating instruction was already generated"
                        );
                        return Ok(());
                    }
                    return self.build_return(None).map(|_| ());
                }
                match typed_literal {
                    TypedLiteral::Dynamic(id) if self.tc_scope[*id].0 == default_types::void => {
                        self.build_return(None)?
                    }
                    TypedLiteral::Static(id)
                        if self.tc_ctx.statics.read()[*id].type_ == default_types::void =>
                    {
                        self.build_return(None)?
                    }
                    TypedLiteral::Void => self.build_return(None)?,
                    TypedLiteral::Intrinsic(..) => unreachable!("intrinsic"),
                    lit => self.build_return(Some(&self.lit_to_basic_value(lit)))?,
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
                        self.tc_scope[*v]
                            .0
                            .to_llvm_basic_type(&self.default_types, self.structs, self.context)
                            .into()
                    })
                    .collect::<Vec<_>>();
                let fn_ty = if self.tc_scope[*dst].0 == default_types::void {
                    self.context.void_type().fn_type(&input_types, false)
                } else {
                    self.tc_scope[*dst]
                        .0
                        .to_llvm_basic_type(&self.default_types, self.structs, self.context)
                        .fn_type(&input_types, false)
                };
                let mut constraints = registers.clone();

                // For some targets, Clang unconditionally adds some clobbers to all inline assembly.
                // While this is probably not strictly necessary, if we don't follow Clang's lead
                // here then we may risk tripping LLVM bugs since anything not used by Clang tends
                // to be buggy and regress often.
                let cpu = self.machine.get_cpu();
                match cpu.to_bytes() {
                    b"x86" | b"x86_64" | b"x86-64" => {
                        if !constraints.is_empty() {
                            constraints.push(',');
                        }
                        constraints.push_str("~{dirflag},~{fpsr},~{flags}");
                    }
                    cpu => unreachable!("unhandled target cpu: {}", String::from_utf8_lossy(cpu)),
                }

                let asm_fn_ptr = self.context.create_inline_asm(
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
                    .map(|v| self.get_value(*v).into())
                    .collect::<Vec<_>>();
                let val = self.build_indirect_call(fn_ty, asm_fn_ptr, &args, "")?;
                self.push_value(
                    *dst,
                    val.try_as_basic_value()
                        .left_or_else(|_| self.default_types.empty_struct.const_zero().into()),
                );
                Ok(())
            }
            TypecheckedExpression::Block(span, child, _) => {
                let block = self.debug_ctx.new_block(scope, *span, module_id);
                let scope = block.as_debug_info_scope();
                for c in child {
                    self.codegen(c, scope, module_id)?;
                }
                Ok(())
            }
            TypecheckedExpression::If {
                cond,
                if_block,
                else_block: None,
                ..
            } => {
                let if_basic_block = self.context.append_basic_block(self.current_fn, "then");
                let end_basic_block = self.context.append_basic_block(self.current_fn, "endif");
                self.build_conditional_branch(
                    self.lit_to_basic_value(cond).into_int_value(),
                    if_basic_block,
                    end_basic_block,
                )?;
                self.goto(if_basic_block);
                let block = self.debug_ctx.new_block(scope, if_block.1, module_id);
                let scope = block.as_debug_info_scope();
                for expr in if_block.0.iter() {
                    self.codegen(expr, scope, module_id)?;
                }
                self.terminate(|| self.build_unconditional_branch(end_basic_block))?;
                self.goto(end_basic_block);
                Ok(())
            }

            TypecheckedExpression::If {
                cond,
                if_block,
                else_block: Some(else_block),
                ..
            } => {
                let if_basic_block = self.context.append_basic_block(self.current_fn, "then");
                let else_basic_block = self.context.append_basic_block(self.current_fn, "else");
                let end_basic_block = self.context.append_basic_block(self.current_fn, "endif");
                self.build_conditional_branch(
                    self.lit_to_basic_value(cond).into_int_value(),
                    if_basic_block,
                    else_basic_block,
                )?;
                self.goto(if_basic_block);
                let block = self.debug_ctx.new_block(scope, if_block.1, module_id);
                let scope = block.as_debug_info_scope();
                for expr in if_block.0.iter() {
                    self.codegen(expr, scope, module_id)?;
                }

                self.terminate(|| self.build_unconditional_branch(end_basic_block))?;
                self.goto(else_basic_block);
                let block = self.debug_ctx.new_block(scope, else_block.1, module_id);
                let scope = block.as_debug_info_scope();
                for expr in else_block.0.iter() {
                    self.codegen(expr, scope, module_id)?;
                }
                self.terminate(|| self.build_unconditional_branch(end_basic_block))?;
                self.goto(end_basic_block);
                Ok(())
            }
            TypecheckedExpression::While {
                cond_block,
                cond,
                body,
                ..
            } => {
                let cond_basic_block = self
                    .context
                    .append_basic_block(self.current_fn, "while-cond");
                let body_basic_block = self
                    .context
                    .append_basic_block(self.current_fn, "while-body");
                let end_basic_block = self
                    .context
                    .append_basic_block(self.current_fn, "while-end");
                self.build_unconditional_branch(cond_basic_block)?;
                self.goto(cond_basic_block);
                for expr in cond_block {
                    self.codegen(expr, scope, module_id)?;
                }
                self.terminate(|| {
                    self.build_conditional_branch(
                        self.lit_to_basic_value(cond).into_int_value(),
                        body_basic_block,
                        end_basic_block,
                    )
                })?;
                self.goto(body_basic_block);
                let block = self.debug_ctx.new_block(scope, body.1, module_id);
                let scope = block.as_debug_info_scope();
                for expr in body.0.iter() {
                    self.codegen(expr, scope, module_id)?;
                }
                self.terminate(|| self.build_unconditional_branch(cond_basic_block))?;
                self.goto(end_basic_block);
                Ok(())
            }
            TypecheckedExpression::Range { .. } => todo!(),
            TypecheckedExpression::StoreAssignment(_, dst, src) => {
                match src {
                    TypedLiteral::Dynamic(id) if self.tc_scope[*id].1.stack_allocated => {
                        let ty = &self.tc_scope[*id].0;
                        let llvm_ty =
                            ty.to_llvm_basic_type(&self.default_types, self.structs, self.context);
                        let alignment = get_alignment(llvm_ty);
                        self.build_memmove(
                            self.lit_to_basic_value(dst).into_pointer_value(),
                            alignment,
                            self.get_value_ptr(*id),
                            alignment,
                            llvm_ty.size_of().expect("llvm type should always be sized"),
                        )?;
                        return Ok(());
                    }
                    TypedLiteral::Static(id) => {
                        let llvm_ty = self.tc_ctx.statics.read()[*id].type_.to_llvm_basic_type(
                            &self.default_types,
                            self.structs,
                            self.context,
                        );
                        let alignment = get_alignment(llvm_ty);
                        self.build_memmove(
                            self.lit_to_basic_value(dst).into_pointer_value(),
                            alignment,
                            self.statics[*id].as_pointer_value(),
                            alignment,
                            llvm_ty.size_of().expect("llvm type should always be sized"),
                        )?;
                        return Ok(());
                    }
                    _ => {}
                }
                build_ptr_store(
                    self.lit_to_basic_value(dst).into_pointer_value(),
                    self.lit_to_basic_value(src),
                    if let TypedLiteral::Function(..) | TypedLiteral::ExternalFunction(..) = src {
                        default_types::usize
                    } else {
                        src.to_type(&self.tc_scope, self.tc_ctx)
                    },
                    self,
                    false,
                )
            }
            TypecheckedExpression::Call(_, dst, fn_ptr, args) => {
                let (fn_ty, fn_ptr) = match fn_ptr {
                    TypedLiteral::Dynamic(id) => (
                        &self.tc_scope[*id].0,
                        self.get_value(*id).into_pointer_value(),
                    ),
                    TypedLiteral::Static(id) => (
                        &self.tc_ctx.statics.read()[*id].type_,
                        self.statics[*id].as_pointer_value(),
                    ),
                    TypedLiteral::Function(..) => unreachable!(
                        "TypedLiteral::Function should have been turned into a DirectCall"
                    ),
                    TypedLiteral::ExternalFunction(_) => unreachable!(
                        "TypedLiteral::ExternalFunction should have been turned into a DirectExternCall"
                    ),
                    TypedLiteral::Intrinsic(..) => unreachable!(
                        "TypedLiteral::Intrinsic should have been turned into a IntrinsicCall"
                    ),
                    _ => unreachable!("{fn_ptr:?} is not callable"),
                };
                let llvm_fn_ty =
                    fn_ty.to_llvm_fn_type(&self.default_types, self.structs, self.context);
                let val = self.build_indirect_call(
                    llvm_fn_ty,
                    fn_ptr,
                    &args
                        .iter()
                        .filter(|v| match v {
                            TypedLiteral::Void => false,
                            TypedLiteral::Dynamic(id) => {
                                self.tc_scope[*id].0 != default_types::never
                                    && self.tc_scope[*id].0 != default_types::void
                            }
                            _ => true,
                        })
                        .map(|v| self.lit_to_basic_value(v).into())
                        .collect::<Vec<_>>(),
                    "",
                )?;
                self.push_value(
                    *dst,
                    val.try_as_basic_value()
                        .left_or_else(|_| self.default_types.empty_struct.const_zero().into()),
                );
                Ok(())
            }
            TypecheckedExpression::DirectCall(.., generics) if !generics.is_empty() => panic!(
                "functions shouldn't have generics (they should've been taken care of during monomorphization)"
            ),
            TypecheckedExpression::DirectExternCall(_, dst, func, args) => {
                self.codegen_directcall(self.external_functions[*func], *dst, args)
            }
            TypecheckedExpression::DirectCall(_, dst, func, args, _) => {
                self.codegen_directcall(self.functions[*func], *dst, args)
            }
            TypecheckedExpression::IntrinsicCall(_, dst, intrinsic, args, generics) => {
                self.codegen_intrinsic(*dst, *intrinsic, args, generics)
            }
            TypecheckedExpression::Pos(_, dst, src) => {
                self.push_value(*dst, self.lit_to_basic_value(src));
                Ok(())
            }
            TypecheckedExpression::Neg(_, dst, src) => {
                let src = self.lit_to_basic_value(src);
                let value = match src {
                    BasicValueEnum::IntValue(int_value) => {
                        self.build_int_neg(int_value, "")?.as_basic_value_enum()
                    }
                    BasicValueEnum::FloatValue(float_value) => {
                        self.build_float_neg(float_value, "")?.as_basic_value_enum()
                    }
                    _ => unreachable!(),
                };
                self.push_value(*dst, value);
                Ok(())
            }
            TypecheckedExpression::BNot(_, dst, src) | TypecheckedExpression::LNot(_, dst, src) => {
                let src = self.lit_to_basic_value(src).into_int_value();
                self.push_value(*dst, self.build_not(src, "")?.as_basic_value_enum());
                Ok(())
            }
            TypecheckedExpression::Add(_, dst, lhs, rhs) => {
                let lhs = self.lit_to_basic_value(lhs);
                let rhs = self.lit_to_basic_value(rhs);
                let typ = &self.tc_scope[*dst].0;
                let v = f_s_u!(
                    self,
                    typ,
                    lhs,
                    rhs,
                    build_int_nsw_add(),
                    build_int_nuw_add(),
                    build_float_add(),
                    "tc should have errored if you try to add 2 non-number values"
                );
                self.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::Sub(_, dst, lhs, rhs) => {
                let lhs = self.lit_to_basic_value(lhs);
                let rhs = self.lit_to_basic_value(rhs);
                let typ = &self.tc_scope[*dst].0;
                let v = f_s_u!(
                    self,
                    typ,
                    lhs,
                    rhs,
                    build_int_nsw_sub(),
                    build_int_nuw_sub(),
                    build_float_sub(),
                    "tc should have errored if you try to subtract 2 non-number values"
                );
                self.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::Mul(_, dst, lhs, rhs) => {
                let lhs = self.lit_to_basic_value(lhs);
                let rhs = self.lit_to_basic_value(rhs);
                let typ = &self.tc_scope[*dst].0;
                let v = f_s_u!(
                    self,
                    typ,
                    lhs,
                    rhs,
                    build_int_nsw_mul(),
                    build_int_nuw_mul(),
                    build_float_mul(),
                    "tc should have errored if you try to multiply 2 non-number values"
                );
                self.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::Div(_, dst, lhs, rhs) => {
                let lhs = self.lit_to_basic_value(lhs);
                let rhs = self.lit_to_basic_value(rhs);
                let typ = &self.tc_scope[*dst].0;
                let v = f_s_u!(
                    self,
                    typ,
                    lhs,
                    rhs,
                    build_int_signed_div(),
                    build_int_unsigned_div(),
                    build_float_div(),
                    "tc should have errored if you try to divide 2 non-number values"
                );
                self.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::Mod(_, dst, lhs, rhs) => {
                let lhs = self.lit_to_basic_value(lhs);
                let rhs = self.lit_to_basic_value(rhs);
                let typ = &self.tc_scope[*dst].0;
                let v = f_s_u!(
                    self,
                    typ,
                    lhs,
                    rhs,
                    build_int_signed_rem(),
                    build_int_unsigned_rem(),
                    build_float_rem(),
                    "tc should have errored if you try to mod 2 non-number values"
                );
                self.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::BAnd(_, dst, lhs, rhs) => {
                let typ = lhs.to_type(&self.tc_scope, self.tc_ctx);
                let lhs = self.lit_to_basic_value(lhs);
                let rhs = self.lit_to_basic_value(rhs);
                if typ.is_int_like() || typ == default_types::bool {
                    self.push_value(
                        *dst,
                        self.build_and(lhs.into_int_value(), rhs.into_int_value(), "")?
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
                let lhs = self.lit_to_basic_value(lhs);
                let rhs = self.lit_to_basic_value(rhs);
                let typ = &self.tc_scope[*dst].0;
                if typ.is_int_like() || *typ == default_types::bool {
                    self.push_value(
                        *dst,
                        self.build_or(lhs.into_int_value(), rhs.into_int_value(), "")?
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
                let lhs = self.lit_to_basic_value(lhs);
                let rhs = self.lit_to_basic_value(rhs);
                let typ = &self.tc_scope[*dst].0;
                if typ.is_int_like() || *typ == default_types::bool {
                    self.push_value(
                        *dst,
                        self.build_xor(lhs.into_int_value(), rhs.into_int_value(), "")?
                            .into(),
                    );
                } else {
                    unreachable!("tc should have errored if you try to xor 2 non-int/bool values");
                }
                Ok(())
            }
            TypecheckedExpression::GreaterThan(_, dst, lhs, rhs) => {
                let typ = lhs
                    .to_primitive_type(&self.tc_scope, self.tc_ctx)
                    .expect("tc should have errored if you try to compare 2 non-number values");
                let lhs = self.lit_to_basic_value(lhs);
                let rhs = self.lit_to_basic_value(rhs);
                let v = f_s_u!(
                    self,
                    typ,
                    lhs,
                    rhs,
                    build_int_compare(IntPredicate::SGT),
                    build_int_compare(IntPredicate::UGT),
                    build_float_compare(FloatPredicate::UGT),
                    "tc should have errored if you try to compare 2 non-number values"
                );
                self.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::LessThan(_, dst, lhs, rhs) => {
                let typ = lhs
                    .to_primitive_type(&self.tc_scope, self.tc_ctx)
                    .expect("tc should have errored if you try to compare 2 non-number values");
                let lhs = self.lit_to_basic_value(lhs);
                let rhs = self.lit_to_basic_value(rhs);
                let v = f_s_u!(
                    self,
                    typ,
                    lhs,
                    rhs,
                    build_int_compare(IntPredicate::SLT),
                    build_int_compare(IntPredicate::ULT),
                    build_float_compare(FloatPredicate::ULT),
                    "tc should have errored if you try to compare 2 non-number values"
                );
                self.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::LAnd(..) => todo!(),
            TypecheckedExpression::LOr(..) => todo!(),
            TypecheckedExpression::GreaterThanEq(_, dst, lhs, rhs) => {
                let typ = lhs
                    .to_primitive_type(&self.tc_scope, self.tc_ctx)
                    .expect("tc should have errored if you try to compare 2 non-number values");
                let lhs = self.lit_to_basic_value(lhs);
                let rhs = self.lit_to_basic_value(rhs);
                let v = f_s_u!(
                    self,
                    typ,
                    lhs,
                    rhs,
                    build_int_compare(IntPredicate::SGE),
                    build_int_compare(IntPredicate::UGE),
                    build_float_compare(FloatPredicate::UGE),
                    "tc should have errored if you try to compare 2 non-number values"
                );
                self.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::LessThanEq(_, dst, lhs, rhs) => {
                let typ = lhs
                    .to_primitive_type(&self.tc_scope, self.tc_ctx)
                    .expect("tc should have errored if you try to compare 2 non-number values");
                let lhs = self.lit_to_basic_value(lhs);
                let rhs = self.lit_to_basic_value(rhs);
                let v = f_s_u!(
                    self,
                    typ,
                    lhs,
                    rhs,
                    build_int_compare(IntPredicate::SLE),
                    build_int_compare(IntPredicate::ULE),
                    build_float_compare(FloatPredicate::ULE),
                    "tc should have errored if you try to compare 2 non-number values"
                );
                self.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::Eq(_, dst, lhs, rhs) => {
                let typ = lhs
                    .to_primitive_type(&self.tc_scope, self.tc_ctx)
                    .expect("tc should have errored if you try to compare 2 non-number values");
                let lhs = self.lit_to_basic_value(lhs);
                let rhs = self.lit_to_basic_value(rhs);
                let v = f_s_u!(
                    self,
                    typ,
                    lhs,
                    rhs,
                    build_int_compare(IntPredicate::EQ),
                    build_int_compare(IntPredicate::EQ),
                    build_float_compare(FloatPredicate::UEQ),
                    "tc should have errored if you try to compare 2 non-number values"
                );
                self.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::Neq(_, dst, lhs, rhs) => {
                let typ = lhs
                    .to_primitive_type(&self.tc_scope, self.tc_ctx)
                    .expect("tc should have errored if you try to compare 2 non-number values");
                let lhs = self.lit_to_basic_value(lhs);
                let rhs = self.lit_to_basic_value(rhs);
                let v = f_s_u!(
                    self,
                    typ,
                    lhs,
                    rhs,
                    build_int_compare(IntPredicate::NE),
                    build_int_compare(IntPredicate::NE),
                    build_float_compare(FloatPredicate::UNE),
                    "tc should have errored if you try to compare 2 non-number values"
                );
                self.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::LShift(_, dst, lhs, rhs) => {
                let lhs = self.lit_to_basic_value(lhs);
                let rhs = self.lit_to_basic_value(rhs);
                let typ = &self.tc_scope[*dst].0;
                if typ.is_int_like() {
                    self.push_value(
                        *dst,
                        self.build_left_shift(lhs.into_int_value(), rhs.into_int_value(), "")?
                            .into(),
                    );
                    Ok(())
                } else {
                    unreachable!("tc should have errored if you try to left shift a non-int value");
                }
            }
            TypecheckedExpression::RShift(_, dst, lhs, rhs) => {
                let lhs = self.lit_to_basic_value(lhs);
                let rhs = self.lit_to_basic_value(rhs);
                let typ = &self.tc_scope[*dst].0;
                if typ.is_int_like() {
                    self.push_value(
                        *dst,
                        self.build_right_shift(
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
                self.push_value(*dst, self.default_types.ptr.const_zero().into());
                Ok(())
            }
            TypecheckedExpression::Reference(_, dst, rhs) => {
                let value = match rhs {
                    TypedLiteral::Dynamic(id) if self.tc_scope[*id].0 == default_types::void => {
                        self.default_types.ptr.const_zero().into()
                    }
                    TypedLiteral::Dynamic(id) if !self.tc_scope[*id].1.stack_allocated => {
                        panic!("_{id} is not stack allocated (even tho it should have been)")
                    }
                    TypedLiteral::Dynamic(id) => self.get_value_ptr(*id).into(),
                    TypedLiteral::Static(id) => self.statics[*id].as_basic_value_enum(),
                    _ => panic!(
                        "Cannot take a reference to {rhs:?} (the typechecker should have put this into a stack-allocated dynamic)"
                    ),
                };
                self.push_value(*dst, value);
                Ok(())
            }
            TypecheckedExpression::MakeUnsizedSlice(_, dst, src, sz) => {
                let fat_ptr_no_ptr = self.default_types.fat_ptr.const_named_struct(&[
                    self.default_types.ptr.get_poison().into(),
                    self.default_types.isize.const_int(*sz as u64, false).into(),
                ]);
                let fat_ptr = self
                    .build_insert_value(fat_ptr_no_ptr, self.lit_to_basic_value(src), 0, "")?
                    .into_struct_value();
                self.push_value(*dst, fat_ptr.into());

                Ok(())
            }
            TypecheckedExpression::Dereference(_, dst, rhs) => {
                if self.tc_scope[*dst].1.stack_allocated {
                    let ty = self.tc_scope[*dst].0.to_llvm_basic_type(
                        &self.default_types,
                        self.structs,
                        self.context,
                    );
                    let lhs_ptr = self.build_alloca(ty, "")?;
                    let alignment = get_alignment(ty);
                    self.build_memmove(
                        lhs_ptr, // dest (dst = *rhs), in this case *dst =
                        // *rhs as lhs is stack-allocated, meaning an implicit store has to be
                        // added.
                        alignment,
                        self.lit_to_basic_value(rhs).into_pointer_value(),
                        alignment,
                        ty.size_of().expect("a type should *always* be sized"),
                    )?;
                    self.push_value_raw(*dst, lhs_ptr.into());
                    return Ok(());
                }
                let value = build_deref(
                    self.lit_to_basic_value(rhs).into_pointer_value(), // dst = *rhs
                    self.tc_scope[*dst].0, // we take the type of lhs as it expects the type of the
                    // value *after* dereferencing as any pointer in llvm is represented as the
                    // same type, &i32 == &&u64 (`ptr`)
                    self,
                    false,
                )?;
                self.push_value(*dst, value);
                Ok(())
            }
            // &struct, <- i64 0, i32 <idx>
            // &[a] <- extractvalue 0 and then i64 n of that
            // &[a; n] <- i64 0 n (for getelementptr [a; n]) or i64 n (for getelementptr a)
            TypecheckedExpression::Offset(_, dst, src, offset) => {
                let ty = src
                    .to_type(&self.tc_scope, self.tc_ctx)
                    .deref()
                    .expect("non-pointer values cannot be offset");
                match **ty {
                    TyKind::Struct { struct_id, .. } => {
                        let offset = match offset {
                            OffsetValue::Dynamic(_) => unreachable!("dynamic struct offset"),
                            OffsetValue::Static(v) => {
                                self.default_types.i32.const_int(*v as u64, false)
                            }
                        };
                        let value = unsafe {
                            self.build_in_bounds_gep(
                                self.structs[struct_id],
                                self.lit_to_basic_value(src).into_pointer_value(),
                                &[self.default_types.isize.const_int(0, false), offset],
                                "",
                            )
                        }?;
                        self.push_value(*dst, value.into());
                        Ok(())
                    }
                    TyKind::Tuple { .. } => {
                        let offset = match offset {
                            OffsetValue::Dynamic(_) => unreachable!("dynamic struct offset"),
                            OffsetValue::Static(v) => {
                                self.default_types.i32.const_int(*v as u64, false)
                            }
                        };
                        let value = unsafe {
                            self.build_in_bounds_gep(
                                ty.to_llvm_basic_type(
                                    &self.default_types,
                                    self.structs,
                                    self.context,
                                ),
                                self.lit_to_basic_value(src).into_pointer_value(),
                                &[self.default_types.isize.const_int(0, false), offset],
                                "",
                            )
                        }?;
                        self.push_value(*dst, value.into());
                        Ok(())
                    }
                    TyKind::SizedArray { typ, .. } => {
                        let offset = match offset {
                            OffsetValue::Dynamic(id) => self.get_value(*id).into_int_value(),
                            OffsetValue::Static(v) => {
                                self.default_types.i32.const_int(*v as u64, false)
                            }
                        };
                        let value = unsafe {
                            self.build_in_bounds_gep(
                                typ.to_llvm_basic_type(
                                    &self.default_types,
                                    self.structs,
                                    self.context,
                                ),
                                self.lit_to_basic_value(src).into_pointer_value(),
                                &[offset],
                                "",
                            )
                        }?;
                        self.push_value(*dst, value.into());
                        Ok(())
                    }
                    TyKind::UnsizedArray(typ) => {
                        let offset = match offset {
                            OffsetValue::Dynamic(id) => self.get_value(*id).into_int_value(),
                            OffsetValue::Static(v) => {
                                self.default_types.i32.const_int(*v as u64, false)
                            }
                        };
                        let actual_ptr = self
                            .build_extract_value(
                                self.lit_to_basic_value(src).into_struct_value(),
                                0,
                                "",
                            )?
                            .into_pointer_value();
                        let value = unsafe {
                            self.build_in_bounds_gep(
                                typ.to_llvm_basic_type(
                                    &self.default_types,
                                    self.structs,
                                    self.context,
                                ),
                                actual_ptr,
                                &[offset],
                                "",
                            )
                        }?;
                        self.push_value(*dst, value.into());
                        Ok(())
                    }
                    _ => unreachable!("cannot take offset of {ty:?}"),
                }
            }
            TypecheckedExpression::OffsetNonPointer(_, dst, src, offset_value) => {
                let src = self.lit_to_basic_value(src);
                if src.is_array_value() {
                    self.push_value(
                        *dst,
                        self.build_extract_value(src.into_array_value(), *offset_value as u32, "")?,
                    );
                } else if src.is_struct_value() {
                    self.push_value(
                        *dst,
                        self.build_extract_value(
                            src.into_struct_value(),
                            *offset_value as u32,
                            "",
                        )?,
                    );
                } else {
                    panic!(
                        "offsetnonptr should never be used with a src element that is not an aggregate value"
                    )
                }
                Ok(())
            }
            TypecheckedExpression::DynCall(_, dst, args, offset) => {
                let mut arguments = args
                    .iter()
                    .map(|v| self.lit_to_basic_value(v).into())
                    .collect::<Vec<BasicMetadataValueEnum<'ctx>>>();
                let dyn_ptr = arguments[0].into_struct_value();
                let real_ptr = self.build_extract_value(dyn_ptr, 0, "")?;
                arguments[0] = real_ptr.into();
                let vtable_ptr_isize = self.build_extract_value(dyn_ptr, 1, "")?.into_int_value();
                let vtable_ptr =
                    self.build_int_to_ptr(vtable_ptr_isize, self.default_types.ptr, "")?;
                let fn_ptr_ptr = unsafe {
                    self.build_gep(
                        self.default_types.ptr,
                        vtable_ptr,
                        &[self
                            .default_types
                            .isize
                            .const_int(*offset as u64 + 1 /* skip length */, false)],
                        "",
                    )
                }?;
                let fn_ptr = self
                    .build_load(self.default_types.ptr, fn_ptr_ptr, "")?
                    .into_pointer_value();
                let param_types = std::iter::once(self.default_types.ptr.into())
                    .chain(args.iter().skip(1).map(|v| {
                        v.to_type(&self.tc_scope, self.tc_ctx)
                            .to_llvm_basic_type(&self.default_types, self.structs, self.context)
                            .into()
                    }))
                    .collect::<Vec<_>>();
                let fn_ty = match &**self.tc_scope[*dst].0 {
                    TyKind::PrimitiveNever | TyKind::PrimitiveVoid => {
                        self.context.void_type().fn_type(&param_types, false)
                    }
                    v => v
                        .to_llvm_basic_type(&self.default_types, self.structs, self.context)
                        .fn_type(&param_types, false),
                };

                let res = self.build_indirect_call(fn_ty, fn_ptr, &arguments, "")?;
                self.push_value(
                    *dst,
                    res.try_as_basic_value()
                        .left_or_else(|_| self.default_types.empty_struct.const_zero().into()),
                );
                Ok(())
            }
            TypecheckedExpression::Bitcast(_, new, old) => {
                let new_typ = &self.tc_scope[*new].0;
                let old_typ = old.to_type(&self.tc_scope, self.tc_ctx);
                assert!(new_typ.refcount() == 0 || new_typ.is_thin_ptr());
                assert!(old_typ.refcount() == 0 || old_typ.is_thin_ptr());
                let new_value = self.build_bit_cast(
                    self.lit_to_basic_value(old),
                    new_typ.to_llvm_basic_type(&self.default_types, self.structs, self.context),
                    "",
                )?;
                self.push_value(*new, new_value);
                Ok(())
            }
            TypecheckedExpression::Literal(_, dst, src)
            | TypecheckedExpression::Alias(_, dst, src) => {
                if let TypedLiteral::Dynamic(id) = src {
                    if self.tc_scope[*dst].1.stack_allocated && self.tc_scope[*id].1.stack_allocated
                    {
                        let ty = &self.tc_scope[*dst].0;
                        let llvm_ty =
                            ty.to_llvm_basic_type(&self.default_types, self.structs, self.context);
                        let alignment = get_alignment(llvm_ty);
                        let new_ptr = self.build_alloca(llvm_ty, "")?;
                        self.build_memmove(
                            new_ptr,
                            alignment,
                            self.get_value_ptr(*id),
                            alignment,
                            llvm_ty.size_of().expect("llvm type should always be sized"),
                        )?;
                        self.push_value_raw(*dst, new_ptr.into());
                        return Ok(());
                    }
                } else if let TypedLiteral::Static(id) = src {
                    if self.tc_scope[*dst].1.stack_allocated {
                        let ty = &self.tc_scope[*dst].0;
                        let llvm_ty =
                            ty.to_llvm_basic_type(&self.default_types, self.structs, self.context);
                        let alignment = get_alignment(llvm_ty);
                        let new_ptr = self.build_alloca(llvm_ty, "")?;
                        self.build_memmove(
                            new_ptr,
                            alignment,
                            self.statics[*id].as_pointer_value(),
                            alignment,
                            llvm_ty.size_of().expect("llvm type should always be sized"),
                        )?;
                        self.push_value_raw(*dst, new_ptr.into());
                        return Ok(());
                    }
                }
                self.push_value(*dst, self.lit_to_basic_value(src));
                Ok(())
            }
            TypecheckedExpression::PtrToInt(_, dst, src) => {
                let value = self.build_ptr_to_int(
                    self.lit_to_basic_value(src).into_pointer_value(),
                    self.tc_scope[*dst]
                        .0
                        .to_llvm_basic_type(&self.default_types, self.structs, self.context)
                        .into_int_type(),
                    "",
                )?;
                self.push_value(*dst, value.into());
                Ok(())
            }
            TypecheckedExpression::IntToPtr(_, dst, src) => {
                let value = self.build_int_to_ptr(
                    self.lit_to_basic_value(src).into_int_value(),
                    self.default_types.ptr,
                    "",
                )?;
                self.push_value(*dst, value.into());
                Ok(())
            }
            TypecheckedExpression::StripMetadata(_, dst, src) => {
                let (value, is_ptr) = if let TypedLiteral::Dynamic(src) = src {
                    (self._scope[*src], self.tc_scope[*src].1.stack_allocated)
                } else if let TypedLiteral::Static(id) = src {
                    (self.statics[*id].as_pointer_value().into(), true)
                } else {
                    (self.lit_to_basic_value(src), false)
                };
                let pointer = if is_ptr {
                    let pointer_pointer = self.build_struct_gep(
                        self.default_types.fat_ptr,
                        value.into_pointer_value(),
                        0,
                        "",
                    )?;
                    build_deref(pointer_pointer, self.tc_scope[*dst].0, self, false)?
                } else {
                    self.build_extract_value(value.into_struct_value(), 0, "")?
                };
                self.push_value(*dst, pointer);
                Ok(())
            }
            TypecheckedExpression::IntCast(_, dst, src) => {
                let src_ty = src
                    .to_primitive_type(&self.tc_scope, self.tc_ctx)
                    .expect("can only cast primitive types");
                let dst_ty = &self.tc_scope[*dst].0;
                let src_value = self.lit_to_basic_value(src);
                // u8 -> bool
                if src_ty == default_types::u8 && *dst_ty == default_types::bool {
                    let value = self.build_int_truncate(
                        src_value.into_int_value(),
                        self.default_types.bool,
                        "",
                    )?;
                    self.push_value(*dst, value.into());
                    return Ok(());
                }
                // bool -> u8
                if src_ty == default_types::bool && *dst_ty == default_types::u8 {
                    let value = self.build_int_z_extend(
                        src_value.into_int_value(),
                        self.default_types.i8,
                        "",
                    )?;
                    self.push_value(*dst, value.into());
                    return Ok(());
                }
                // f32 -> f64
                if src_ty == default_types::f32 && *dst_ty == default_types::f64 {
                    let value = self.build_float_ext(
                        src_value.into_float_value(),
                        self.default_types.f64,
                        "",
                    )?;
                    self.push_value(*dst, value.into());
                    return Ok(());
                }
                // f64 -> f32
                if src_ty == default_types::f64 && *dst_ty == default_types::f32 {
                    let value = self.build_float_trunc(
                        src_value.into_float_value(),
                        self.default_types.f32,
                        "",
                    )?;
                    self.push_value(*dst, value.into());
                    return Ok(());
                }
                if src_ty.is_int_like() && dst_ty.is_int_like() {
                    let isize_bitwidth = self.default_types.isize.get_bit_width();
                    let trunc =
                        src_ty.get_bitwidth(isize_bitwidth) > dst_ty.get_bitwidth(isize_bitwidth);
                    let ty = self
                        .context
                        .custom_width_int_type(dst_ty.get_bitwidth(isize_bitwidth));
                    let value = if trunc {
                        self.build_int_truncate_or_bit_cast(src_value.into_int_value(), ty, "")?
                    } else if dst_ty.is_unsigned() {
                        self.build_int_z_extend_or_bit_cast(src_value.into_int_value(), ty, "")?
                    } else {
                        self.build_int_s_extend_or_bit_cast(src_value.into_int_value(), ty, "")?
                    };
                    self.push_value(*dst, value.into());
                    return Ok(());
                }
                if src_ty.is_float() {
                    let isize_bitwidth = self.default_types.isize.get_bit_width();
                    let ty = self
                        .context
                        .custom_width_int_type(dst_ty.get_bitwidth(isize_bitwidth));
                    let value = if dst_ty.is_unsigned() {
                        self.build_float_to_unsigned_int(src_value.into_float_value(), ty, "")?
                    } else {
                        self.build_float_to_signed_int(src_value.into_float_value(), ty, "")?
                    };
                    self.push_value(*dst, value.into());
                    Ok(())
                } else {
                    let ty = match **dst_ty {
                        TyKind::PrimitiveF32 => self.default_types.f32,
                        TyKind::PrimitiveF64 => self.default_types.f64,
                        _ => unreachable!("not a float type: {:?}", dst_ty),
                    };
                    let value = if src_ty.is_unsigned() {
                        self.build_unsigned_int_to_float(src_value.into_int_value(), ty, "")?
                    } else {
                        self.build_signed_int_to_float(src_value.into_int_value(), ty, "")?
                    };
                    self.push_value(*dst, value.into());
                    Ok(())
                }
            }
            TypecheckedExpression::Unreachable(_) => Ok(_ = self.build_unreachable()?),
            TypecheckedExpression::Empty(_) => Ok(()),
            TypecheckedExpression::None => {
                unreachable!("None-expressions are not valid and indicate an error")
            }
        }
    }

    fn codegen_directcall(
        &mut self,
        function: FunctionValue<'ctx>,
        dst: usize,
        args: &[TypedLiteral<'arena>],
    ) -> Result<(), BuilderError> {
        let val = self.build_direct_call(
            function,
            &args
                .iter()
                .filter(|v| match v {
                    TypedLiteral::Void => false,
                    TypedLiteral::Dynamic(id) => {
                        self.tc_scope[*id].0 != default_types::never
                            && self.tc_scope[*id].0 != default_types::void
                    }
                    _ => true,
                })
                .map(|v| self.lit_to_basic_value(v).into())
                .collect::<Vec<_>>(),
            "",
        )?;
        val.set_call_convention(function.get_call_conventions());
        self.push_value(
            dst,
            val.try_as_basic_value()
                .left_or_else(|_| self.default_types.empty_struct.const_zero().into()),
        );
        Ok(())
    }

    fn codegen_intrinsic(
        &mut self,
        dst: usize,
        intrinsic: Intrinsic,
        args: &[TypedLiteral<'arena>],
        generics: &[Ty<'arena>],
    ) -> Result<(), BuilderError> {
        _ = &generics;
        _ = &args;
        match intrinsic {
            Intrinsic::CallMain => {
                let main_fn = self
                    .tc_ctx
                    .main_function
                    .get()
                    .expect("main function has to be set to use intrinsic `call_main`.");
                let func = self.functions[main_fn];
                self.build_call(func, &[], "")?;
                self.push_value(dst, self.default_types.empty_struct.const_zero().into());
            }
            Intrinsic::Unreachable => {
                self.build_unreachable()?;
                self.push_value(dst, self.default_types.empty_struct.const_zero().into());
            }
            Intrinsic::Trap => {
                self.intrinsics
                    .trap
                    .build_call(self.module, self, &[], &[])?;
                self.push_value(dst, self.default_types.empty_struct.const_zero().into());
            }
            Intrinsic::Breakpoint => {
                self.intrinsics
                    .breakpoint
                    .build_call(self.module, self, &[], &[])?;
                self.push_value(dst, self.default_types.empty_struct.const_zero().into());
            }
            Intrinsic::ReturnAddress => {
                let ret = self.build_direct_call(
                    self.retaddr,
                    &[self.default_types.i32.const_zero().into()],
                    "",
                )?;

                self.push_value(
                    dst,
                    ret.try_as_basic_value()
                        .expect_left("returnaddress is (i32) -> ptr, and ptr is a basic value"),
                );
            }
            Intrinsic::SizeOf => {
                let size = generics[0]
                    .size_and_alignment(self.pointer_size, &self.tc_ctx.structs.read())
                    .0;
                self.push_value(dst, self.default_types.isize.const_int(size, false).into());
            }
            Intrinsic::Offset => {
                let ptr = self.lit_to_basic_value(&args[0]).into_pointer_value();
                let offset = self.lit_to_basic_value(&args[1]).into_int_value();
                let val =
                    unsafe { self.build_in_bounds_gep(self.default_types.i8, ptr, &[offset], "")? };
                self.push_value(dst, val.into());
            }
            Intrinsic::GetMetadata => {
                let ty = &generics[0];
                if ty.is_sized() {
                    self.push_value(dst, self.default_types.isize.const_int(0, false).into());
                } else {
                    let ptr = self.lit_to_basic_value(&args[0]).into_struct_value();
                    let metadata = self.build_extract_value(ptr, 1, "")?;
                    self.push_value(dst, metadata);
                }
            }
            Intrinsic::WithMetadata => {
                assert!(generics[0].is_sized());
                assert!(!generics[1].is_sized());
                let ptr = self.lit_to_basic_value(&args[0]);
                let metadata = self.lit_to_basic_value(&args[1]);
                let fat_ptr =
                    self.build_insert_value(self.default_types.fat_ptr.get_poison(), ptr, 0, "")?;
                let fat_ptr = self.build_insert_value(fat_ptr, metadata, 1, "")?;
                self.push_value(dst, fat_ptr.as_basic_value_enum());
            }
            Intrinsic::VolatileRead => {
                assert!(generics[0].is_sized());
                let value = build_deref(
                    self.lit_to_basic_value(&args[0]).into_pointer_value(),
                    generics[0],
                    self,
                    true,
                )?;
                self.push_value(dst, value);
            }
            Intrinsic::VolatileWrite => {
                assert!(generics[0].is_sized());
                build_ptr_store(
                    self.lit_to_basic_value(&args[0]).into_pointer_value(),
                    self.lit_to_basic_value(&args[1]),
                    generics[0],
                    self,
                    true,
                )?;
            }
            Intrinsic::SizeOfVal => {
                if generics[0].is_sized() {
                    let size = generics[0]
                        .size_and_alignment(self.pointer_size, &self.tc_ctx.structs.read())
                        .0;
                    self.push_value(dst, self.default_types.isize.const_int(size, false).into());
                } else {
                    match &**generics[0] {
                        TyKind::DynType { .. } => {
                            let fat_ptr = self.lit_to_basic_value(&args[0]).into_struct_value();
                            let vtable_ptr_int =
                                self.build_extract_value(fat_ptr, 1, "")?.into_int_value();
                            let vtable_ptr =
                                self.build_int_to_ptr(vtable_ptr_int, self.default_types.ptr, "")?;
                            let size = self.build_load(self.default_types.isize, vtable_ptr, "")?;
                            self.push_value(dst, size);
                        }
                        TyKind::UnsizedArray(typ) => {
                            let fat_ptr = self.lit_to_basic_value(&args[0]).into_struct_value();
                            let len = self.build_extract_value(fat_ptr, 1, "")?.into_int_value();
                            let size_single = typ
                                .size_and_alignment(self.pointer_size, &self.tc_ctx.structs.read())
                                .0;
                            let total_size = self.build_int_nuw_mul(
                                len,
                                self.default_types.isize.const_int(size_single, false),
                                "",
                            )?;
                            self.push_value(dst, total_size.as_basic_value_enum());
                        }
                        TyKind::PrimitiveStr => {
                            let fat_ptr = self.lit_to_basic_value(&args[0]).into_struct_value();
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

            // TODO: replace these before reaching this point.
            Intrinsic::TypeName => todo!(),
        }
        Ok(())
    }
}

impl<'cg, 'ctx> Deref for FunctionCodegenContext<'ctx, '_, 'cg> {
    type Target = Builder<'ctx>;

    fn deref(&self) -> &'cg Self::Target {
        self.builder
    }
}

/// returns the llvm major and minor versions that were expected
pub fn expected_llvm_version() -> (u32, u32) {
    #[cfg(feature = "llvm20-1")]
    let v = (20, 1);
    #[cfg(feature = "llvm19-1")]
    let v = (19, 1);
    #[cfg(feature = "llvm18-0")]
    let v = (18, 0);
    #[cfg(not(any(feature = "llvm20-1", feature = "llvm19-1", feature = "llvm18-0")))]
    let v = (0, 0);
    v
}

/// returns the llvm major, minor, patch
pub fn llvm_version() -> (u32, u32, u32) {
    let mut major = 0u32;
    let mut minor = 0u32;
    let mut patch = 0u32;
    unsafe { inkwell::llvm_sys::core::LLVMGetVersion(&mut major, &mut minor, &mut patch) };
    (major, minor, patch)
}

#[cfg(not(any(feature = "llvm20-1", feature = "llvm19-1", feature = "llvm18-0")))]
compile_error!("one of llvm20-1, llvm19-1 or llvm18-0 has to be enabled");
#[cfg(any(
    all(feature = "llvm20-1", feature = "llvm19-1"),
    all(feature = "llvm20-1", feature = "llvm18-0"),
    all(feature = "llvm19-1", feature = "llvm18-0"),
))]
compile_error!("only one of llvm20-1, llvm19-1 and llvm18-0 are allowed to be active :3");
