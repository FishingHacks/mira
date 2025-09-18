use context::{DefaultTypes, ExternalFunctionsStore, FunctionsStore, StaticsStore, StructsStore};
use core::panic;
use std::collections::HashMap;

use mira_common::store::StoreKey;
use mira_spans::interner::Symbol;
use mira_typeck::TyKind;
use mira_typeck::{
    Ty, TyList, TypedFunction, TypedTrait,
    ir::{TypedLiteral, ValueId},
    queries::Providers,
};
pub mod mangling;
pub use context::{CodegenConfig, CodegenContext, CodegenContextBuilder, Optimizations};
pub use error::CodegenError;
pub use inkwell::support::LLVMString;
use inkwell::{
    builder::Builder,
    context::Context,
    types::{AnyTypeEnum, BasicType, BasicTypeEnum},
    values::{BasicValueEnum, GlobalValue},
};

mod builder;
mod context;
mod debug_builder;
mod error;
mod intrinsics;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct FnInstance<'ctx> {
    parent_fn: StoreKey<TypedFunction<'ctx>>,
    generics: TyList<'ctx>,
}

pub type VTableKey<'arena> = (Ty<'arena>, Box<[StoreKey<TypedTrait<'arena>>]>);

trait TyKindExt<'ctx, 'arena> {
    fn to_llvm_basic_type(
        &self,
        default_types: &DefaultTypes<'ctx>,
        structs: &StructsStore<'ctx, 'arena>,
        ctx: &'ctx Context,
    ) -> BasicTypeEnum<'ctx>;
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
                ty,
                number_elements,
                ..
            } => ty
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

#[allow(clippy::too_many_arguments)]
fn to_basic_value<'ctx, 'arena>(
    lit: &TypedLiteral<'arena>,
    scope_get_value: &dyn Fn(ValueId) -> BasicValueEnum<'ctx>,
    default_types: &DefaultTypes<'ctx>,
    structs: &StructsStore<'ctx, 'arena>,
    builder: &Builder<'ctx>,
    statics: &StaticsStore<'ctx, 'arena>,
    functions: &FunctionsStore<'ctx, 'arena>,
    ext_functions: &ExternalFunctionsStore<'ctx, 'arena>,
    string_map: &HashMap<Symbol<'_>, GlobalValue<'ctx>>,
    ctx: &'ctx Context,
) -> BasicValueEnum<'ctx> {
    match lit {
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
        TypedLiteral::LLVMIntrinsic(..) | TypedLiteral::Intrinsic(..) => {
            unreachable!("intrinsics can only be used as part of intrinsic call")
        }
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

pub fn provide(providers: &mut Providers<'_>) {
    mangling::provide(providers);
}
