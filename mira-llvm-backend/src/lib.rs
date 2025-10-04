use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    hash::Hash,
    io::Write,
    ops::Deref,
    path::Path,
    sync::Arc,
};

use inkwell::{
    AddressSpace, OptimizationLevel,
    attributes::{Attribute, AttributeLoc},
    builder::Builder,
    context::Context,
    debug_info::AsDIScope,
    llvm_sys::LLVMCallConv,
    memory_buffer::MemoryBuffer,
    module::{Linkage, Module},
    passes::PassBuilderOptions,
    support::LLVMString,
    targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target as LLVMTarget, TargetMachine,
        TargetTriple,
    },
    types::{AnyTypeEnum, BasicType, BasicTypeEnum, FloatType, IntType, PointerType, StructType},
    values::{BasicValueEnum, FunctionValue, GlobalValue},
};

use mira_common::index::IndexMap;
use mira_context::ErrorEmitted;
use mira_errors::FatalError;
use mira_parser::{
    module::{ExternalFunctionId, FunctionId, StaticId, StructId, TraitId},
    std_annotations::{
        alias::ExternAliasAnnotation, callconv::CallConvAnnotation, ext_vararg::ExternVarArg,
        intrinsic::IntrinsicAnnotation, llvm_intrinsic::LLVMIntrinsicAnnotation,
        noinline::Noinline, section::SectionAnnotation,
    },
};
use mira_spans::interner::Symbol;
use mira_target::{NATIVE_TARGET, Target};
use mira_typeck::{
    Substitute, SubstitutionCtx, Ty, TyKind, TyList, TypeckCtx, default_types, ir::TypedLiteral,
    queries::Providers,
};

use debug_builder::DebugContext;
pub use error::CodegenError;
use mangling::{mangle_function_instance, mangle_static, mangle_struct};

use crate::abi::{ArgumentType, argument, has_special_encoding, return_ty};

mod abi;
mod builder;
mod debug_builder;
mod error;
pub mod mangling;

fn llvm_basic_ty<'ctx>(
    ty: &TyKind<'_>,
    default_types: &DefaultTypes<'ctx>,
    structs: &StructsStore<'ctx>,
    ctx: &'ctx Context,
) -> BasicTypeEnum<'ctx> {
    match ty {
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
                    .map(|v| llvm_basic_ty(v, default_types, structs, ctx))
                    .collect::<Vec<_>>(),
                false,
            )
            .into(),
        TyKind::SizedArray {
            ty,
            number_elements,
            ..
        } => llvm_basic_ty(ty, default_types, structs, ctx)
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
        AnyTypeEnum::IntType(ty) => ty.into(),
        AnyTypeEnum::PointerType(ty) => ty.into(),
        AnyTypeEnum::StructType(ty) => ty.into(),
        AnyTypeEnum::FunctionType(_) => panic!("A static should never be a function"),
        AnyTypeEnum::VoidType(_) => panic!("A static should never be void"),
        AnyTypeEnum::VectorType(..) | AnyTypeEnum::ScalableVectorType(..) => {
            unreachable!("vector types arent supported")
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn basic_val<'ctx, 'arena>(
    lit: &TypedLiteral<'arena>,
    ctx: &mut CodegenContext<'ctx, 'arena, '_>,
) -> BasicValueEnum<'ctx> {
    match lit {
        TypedLiteral::Void => ctx.default_types.empty_struct.const_zero().into(),
        TypedLiteral::Dynamic(_) => unreachable!("TypedLiteral::Dynamic outside a function"),
        &TypedLiteral::Function(id, generics) => ctx
            .get_fn_instance(FnInstance::new_poly(id, generics))
            .as_global_value()
            .as_pointer_value()
            .into(),
        TypedLiteral::ExternalFunction(id) => ctx.external_functions[*id]
            .as_global_value()
            .as_pointer_value()
            .into(),
        TypedLiteral::Static(id) => {
            let static_value = ctx.statics[*id];
            ctx.builder
                .build_load(
                    static_to_basic_type(static_value),
                    static_value.as_pointer_value(),
                    "",
                )
                .expect("the type should always match")
        }
        TypedLiteral::String(sym) => {
            let ptr = ctx.get_string(*sym).as_pointer_value().into();
            let size = sym.len();
            let len_const = ctx.default_types.isize.const_int(size as u64, false).into();
            ctx.default_types
                .fat_ptr
                .const_named_struct(&[ptr, len_const])
                .into()
        }

        TypedLiteral::ArrayInit(ty, _, 0) => {
            llvm_basic_ty(ty, &ctx.default_types, &ctx.structs, ctx.context)
                .array_type(0)
                .const_zero()
                .into()
        }
        TypedLiteral::ArrayInit(_, elem, amount) => {
            let elem = basic_val(elem, ctx);
            let mut array_value = elem.get_type().array_type(*amount as u32).const_zero();
            for i in 0..*amount {
                array_value = ctx
                    .builder
                    .build_insert_value(array_value, elem, i as u32, "")
                    .expect("i should never be out of bounds")
                    .into_array_value();
            }

            array_value.into()
        }
        TypedLiteral::Array(ty, elements) => {
            if elements.is_empty() {
                return llvm_basic_ty(ty, &ctx.default_types, &ctx.structs, ctx.context)
                    .array_type(0)
                    .const_zero()
                    .into();
            }
            let mut insert_value_vec: Vec<(usize, BasicValueEnum<'ctx>)> = Vec::new();

            macro_rules! array_const_value {
                ($ty:expr,$into_val_fn:ident) => {{
                    let mut const_elements = Vec::new();
                    for (i, v) in elements.iter().enumerate() {
                        let val = basic_val(v, ctx).$into_val_fn();
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
            let mut const_value =
                match llvm_basic_ty(ty, &ctx.default_types, &ctx.structs, ctx.context) {
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
                const_value = ctx
                    .builder
                    .build_insert_value(const_value, v.1, v.0 as u32, "")
                    .expect("integer should never be out of range")
                    .into_array_value();
            }
            const_value.into()
        }
        TypedLiteral::Struct(struct_id, vec) => {
            if vec.is_empty() {
                return ctx.structs[*struct_id].const_named_struct(&[]).into();
            }
            let mut non_const_value = Vec::new();
            let mut const_value = ctx.structs[*struct_id].const_named_struct(
                &vec.iter()
                    .enumerate()
                    .map(|(i, v)| {
                        let val = basic_val(v, ctx);
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
                const_value = ctx
                    .builder
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
                let val = basic_val(v, ctx);
                elem_types.push(val.get_type());
                elems.push(val);
            }
            let mut value = ctx.context.struct_type(&elem_types, false).const_zero();
            for (i, elem) in elems.into_iter().enumerate() {
                value = ctx
                    .builder
                    .build_insert_value(value, elem, i as u32, "")
                    .expect("integer should never be out of range")
                    .into_struct_value();
            }
            value.into()
        }
        TypedLiteral::F64(v) => ctx.default_types.f64.const_float(*v).into(),
        TypedLiteral::F32(v) => ctx.default_types.f32.const_float(*v as f64).into(),
        TypedLiteral::U8(v) => ctx.default_types.i8.const_int(*v as u64, false).into(),
        TypedLiteral::U16(v) => ctx.default_types.i16.const_int(*v as u64, false).into(),
        TypedLiteral::U32(v) => ctx.default_types.i32.const_int(*v as u64, false).into(),
        TypedLiteral::U64(v) => ctx.default_types.i64.const_int(*v, false).into(),
        TypedLiteral::USize(v) => ctx.default_types.isize.const_int(*v as u64, false).into(),
        TypedLiteral::I8(v) => ctx.default_types.i8.const_int(*v as u64, false).into(),
        TypedLiteral::I16(v) => ctx.default_types.i16.const_int(*v as u64, false).into(),
        TypedLiteral::I32(v) => ctx.default_types.i32.const_int(*v as u64, false).into(),
        TypedLiteral::I64(v) => ctx.default_types.i64.const_int(*v as u64, false).into(),
        TypedLiteral::ISize(v) => ctx.default_types.isize.const_int(*v as u64, false).into(),
        TypedLiteral::Bool(v) => ctx.default_types.bool.const_int(*v as u64, false).into(),
        TypedLiteral::LLVMIntrinsic(..) | TypedLiteral::Intrinsic(..) => {
            unreachable!("intrinsics can only be used as part of intrinsic call")
        }
    }
}

#[derive(Clone, Copy)]
struct DefaultTypes<'ctx> {
    pub isize: IntType<'ctx>,
    pub i8: IntType<'ctx>,
    pub i16: IntType<'ctx>,
    pub i32: IntType<'ctx>,
    pub i64: IntType<'ctx>,
    pub bool: IntType<'ctx>,
    pub ptr: PointerType<'ctx>,
    // { ptr, isize }
    pub fat_ptr: StructType<'ctx>,
    pub f32: FloatType<'ctx>,
    pub f64: FloatType<'ctx>,
    // {}
    pub empty_struct: StructType<'ctx>,
}

pub(crate) struct CodegenFunction<'ctx> {
    // first value is return value
    ret_args_types: Box<[ArgumentType<'ctx>]>,
    value: FunctionValue<'ctx>,
}

impl<'ctx> CodegenFunction<'ctx> {
    pub(crate) fn return_ty(&self) -> ArgumentType<'ctx> {
        self.ret_args_types[0]
    }

    pub(crate) fn args(&self) -> &[ArgumentType<'ctx>] {
        &self.ret_args_types[1..]
    }

    pub(crate) fn as_global_value(&self) -> GlobalValue<'ctx> {
        self.value.as_global_value()
    }
}

pub(crate) type ExternalFunctionsStore<'ctx> = IndexMap<ExternalFunctionId, CodegenFunction<'ctx>>;
pub(crate) type StructsStore<'ctx> = IndexMap<StructId, StructType<'ctx>>;
pub(crate) type StaticsStore<'ctx> = IndexMap<StaticId, GlobalValue<'ctx>>;

pub struct CodegenContextBuilder(Context);

impl CodegenContextBuilder {
    pub fn new() -> Self {
        Self(Context::create())
    }

    pub fn build<'ctx, 'arena, 'a>(
        &'ctx self,
        ctx: &'a TypeckCtx<'arena>,
        module: &str,
        path: Arc<Path>,
        config: CodegenConfig<'ctx>,
    ) -> Result<CodegenContext<'ctx, 'arena, 'a>, CodegenError<'arena>> {
        CodegenContext::new(&self.0, ctx, module, path, config)
    }
}

impl Default for CodegenContextBuilder {
    fn default() -> Self {
        Self::new()
    }
}

type VTableKey<'arena> = (Ty<'arena>, Box<[TraitId]>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FnInstance<'ctx> {
    pub fn_id: FunctionId,
    pub generics: TyList<'ctx>,
}

impl<'ctx> FnInstance<'ctx> {
    pub fn new_mono(fn_id: FunctionId) -> Self {
        Self {
            fn_id,
            generics: TyList::EMPTY,
        }
    }

    pub fn new_poly(fn_id: FunctionId, generics: TyList<'ctx>) -> Self {
        Self { fn_id, generics }
    }
}

#[derive(Default)]
pub struct FnStore<'ctx, 'arena> {
    map: HashMap<FnInstance<'arena>, CodegenFunction<'ctx>>,
    missing: HashSet<FnInstance<'arena>>,
}

pub struct CodegenContext<'ctx, 'arena, 'a> {
    pub tc_ctx: &'a TypeckCtx<'arena>,
    builder: Builder<'ctx>,
    context: &'ctx Context,
    default_types: DefaultTypes<'ctx>,
    pub module: Module<'ctx>,
    pub machine: TargetMachine,
    pub triple: TargetTriple,
    external_functions: ExternalFunctionsStore<'ctx>,
    structs: StructsStore<'ctx>,
    statics: StaticsStore<'ctx>,
    debug_ctx: DebugContext<'ctx, 'arena>,
    config: CodegenConfig<'ctx>,

    intrinsics: RefCell<HashMap<Symbol<'arena>, FunctionValue<'ctx>>>,
    function_store: RefCell<FnStore<'ctx, 'arena>>,
    string_map: RefCell<HashMap<Symbol<'arena>, GlobalValue<'ctx>>>,
    vtables: RefCell<HashMap<VTableKey<'arena>, GlobalValue<'ctx>>>,
}

impl<'ctx> Deref for CodegenContext<'ctx, '_, '_> {
    type Target = Context;

    fn deref(&self) -> &'ctx Self::Target {
        self.context
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Optimizations {
    /// No Optimizations (Like clangs -O1)
    None,
    /// low optimizations (Like clangs -O1)
    Low,
    /// normal optimizations (Like clangs -O2)
    Normal,
    /// aggressive optimizations (Like clangs -O3)
    High,
    /// optimizations with respect to binary size (Like clangs -Os)
    Small,
    /// optimizations on binary size above all (Like clangs -Oz)
    ExtremelySmall,
}

#[derive(Clone, Copy, Debug)]
pub struct CodegenConfig<'a> {
    pub optimizations: Optimizations,
    pub runtime_safety: bool,
    pub target: Target,
    pub cpu_features: &'a str,
    pub reloc_mode: RelocMode,
}

macro_rules! setter {
    ($($v:ident: $ty:ty),* $(,)?) => {
        $(pub fn $v(mut self, $v: $ty) -> Self {
            self.$v = $v;
            self
        })*
    };
}

impl<'a> CodegenConfig<'a> {
    pub fn new(target: Target) -> Self {
        Self {
            target,
            optimizations: Optimizations::Normal,
            runtime_safety: true,
            cpu_features: "",
            reloc_mode: RelocMode::Default,
        }
    }

    fn new_with_opt(target: Target, optimizations: Optimizations, runtime_safety: bool) -> Self {
        Self {
            optimizations,
            runtime_safety,
            ..Self::new(target)
        }
    }

    pub fn new_debug() -> Self {
        Self::new(NATIVE_TARGET)
    }
    pub fn new_debug_unoptimized() -> Self {
        Self::new_with_opt(NATIVE_TARGET, Optimizations::None, true)
    }
    pub fn new_release_fast() -> Self {
        Self::new_with_opt(NATIVE_TARGET, Optimizations::High, false)
    }
    pub fn new_release_safe() -> Self {
        Self::new_with_opt(NATIVE_TARGET, Optimizations::High, true)
    }
    pub fn new_release_small() -> Self {
        Self::new_with_opt(NATIVE_TARGET, Optimizations::Small, false)
    }
    pub fn new_release_tiny() -> Self {
        Self::new_with_opt(NATIVE_TARGET, Optimizations::ExtremelySmall, false)
    }
    /// Sets the current optimizations and runtime safety to that of the other.
    /// This can be used with the other preset functions, e.g. with
    /// `codegen_cfg.optimizations_of(CodegenConfig::new_release_safe())`
    pub fn optimizations_of(&mut self, other: Self) {
        self.optimizations = other.optimizations;
        self.runtime_safety = other.runtime_safety;
    }

    setter!(cpu_features: &'a str, reloc_mode: RelocMode, runtime_safety: bool, optimizations: Optimizations, target: Target);
}

impl<'ctx, 'arena, 'a> CodegenContext<'ctx, 'arena, 'a> {
    fn new(
        context: &'ctx Context,
        ctx: &'a TypeckCtx<'arena>,
        module: &str,
        path: Arc<Path>,
        config: CodegenConfig<'ctx>,
    ) -> Result<Self, CodegenError<'arena>> {
        let shared_ctx = ctx.ctx;
        LLVMTarget::initialize_all(&InitializationConfig::default());
        let triple = TargetTriple::create(&config.target.to_llvm());
        let llvm_target = LLVMTarget::from_triple(&triple)?;
        let Some(machine) = llvm_target.create_target_machine(
            &triple,
            config.target.arch.to_llvm_cpu(),
            config.cpu_features,
            OptimizationLevel::Aggressive,
            RelocMode::PIC,
            CodeModel::Default,
        ) else {
            return Err(CodegenError::unknown_triple(triple));
        };
        let module = context.create_module(module);
        module.set_triple(&triple);
        let mira_version = context.metadata_string(&get_mira_version());
        if let Err(e) =
            module.add_global_metadata("llvm.ident", &context.metadata_node(&[mira_version.into()]))
        {
            ctx.emit_diag(CodegenError::LLVMNative(format!("!llvm.ident: {e}")).to_error());
            FatalError.raise();
        }

        let target_data = machine.get_target_data();
        let data_layout = target_data.get_data_layout();
        module.set_data_layout(&data_layout);
        let isize_type = context.ptr_sized_int_type(&target_data, None);
        let ptr_type = context.ptr_type(AddressSpace::default());
        let default_types = DefaultTypes {
            i8: context.i8_type(),
            i16: context.i16_type(),
            i32: context.i32_type(),
            i64: context.i64_type(),
            f32: context.f32_type(),
            f64: context.f64_type(),
            bool: context.bool_type(),
            isize: isize_type,
            ptr: ptr_type,
            fat_ptr: context.struct_type(&[ptr_type.into(), isize_type.into()], false),
            empty_struct: context.struct_type(&[], false),
        };

        let debug_ctx = DebugContext::new(
            context,
            &module,
            default_types,
            ctx,
            &path,
            config.optimizations != Optimizations::None,
            shared_ctx,
        );

        let builder = context.create_builder();

        let mut me = Self {
            context,
            module,
            builder,
            default_types,
            external_functions: IndexMap::new(),
            structs: IndexMap::new(),
            statics: IndexMap::new(),
            string_map: RefCell::new(HashMap::new()),
            machine,
            triple,
            tc_ctx: ctx,
            debug_ctx,
            intrinsics: RefCell::new(HashMap::new()),
            vtables: RefCell::new(HashMap::new()),
            config,
            function_store: RefCell::default(),
        };

        let struct_reader = ctx.structs.read();
        for k in struct_reader.keys() {
            me.structs
                .insert(k, context.opaque_struct_type(&mangle_struct(ctx, k)));
        }
        for (key, structure) in struct_reader.entries() {
            let fields = structure
                .elements
                .iter()
                .map(|(_, t, _)| {
                    if *t == default_types::void || *t == default_types::never {
                        me.default_types.i8.array_type(0).into()
                    } else {
                        llvm_basic_ty(t, &me.default_types, &me.structs, me.context)
                    }
                })
                .collect::<Vec<_>>();
            assert!(
                me.structs[key].set_body(&fields, false),
                "struct should not yet be initialized"
            );
        }

        // attributes:
        let inline =
            context.create_enum_attribute(Attribute::get_named_enum_kind_id("alwaysinline"), 0);
        let naked = context.create_enum_attribute(Attribute::get_named_enum_kind_id("naked"), 0);
        let noinline =
            context.create_enum_attribute(Attribute::get_named_enum_kind_id("noinline"), 0);

        let ptrsize = (default_types.isize.get_bit_width() / 8) as u8;
        // external functions
        let ext_function_reader = ctx.external_functions.read();
        for (key, (contract, body)) in ext_function_reader.entries() {
            let mut ret_args_types = Vec::with_capacity(contract.arguments.len() + 1);

            let ret_ty = return_ty(
                context,
                &contract.return_type,
                ptrsize,
                &struct_reader,
                |t| llvm_basic_ty(t, &default_types, &me.structs, context),
            );
            ret_args_types.push(ret_ty);

            for (_, ty) in &contract.arguments {
                let arg_ty = argument(context, ty, ptrsize, &struct_reader, |t| {
                    llvm_basic_ty(t, &default_types, &me.structs, context)
                });
                ret_args_types.push(arg_ty);
            }

            let param_types = ret_args_types[1..]
                .iter()
                .filter_map(|v| v.as_arg_ty(&default_types).map(Into::into))
                .collect::<Vec<_>>();

            let has_var_args = contract.annotations.has_annotation::<ExternVarArg>();

            let fn_ty =
                ret_args_types[0].func_ty(param_types, has_var_args, &default_types, context);

            let name = if let Some(name) = contract
                .annotations
                .get_first_annotation::<ExternAliasAnnotation>()
                .map(|v| &v.0)
            {
                name.as_str()
            } else {
                &contract
                    .name
                    .expect("external functions should always have a name")
            };

            let func = me.module.add_function(name, fn_ty, None);
            ArgumentType::add_ret_attribute_args(&ret_args_types, func, context);

            if let Some(callconv) = contract
                .annotations
                .get_first_annotation::<CallConvAnnotation>()
            {
                match callconv {
                    CallConvAnnotation::C => {
                        func.set_call_conventions(LLVMCallConv::LLVMCCallConv as u32)
                    }
                    CallConvAnnotation::Fast => {
                        func.set_call_conventions(LLVMCallConv::LLVMFastCallConv as u32)
                    }
                    CallConvAnnotation::Cold => {
                        func.set_call_conventions(LLVMCallConv::LLVMColdCallConv as u32)
                    }
                    CallConvAnnotation::Inline => {
                        func.add_attribute(AttributeLoc::Function, inline)
                    }
                    CallConvAnnotation::Naked => func.add_attribute(AttributeLoc::Function, naked),
                }
            }
            if contract.annotations.has_annotation::<Noinline>() {
                func.add_attribute(AttributeLoc::Function, noinline);
            }
            func.set_subprogram(me.debug_ctx.ext_funcs[key]);
            if let Some(section) = contract
                .annotations
                .get_first_annotation::<SectionAnnotation>()
            {
                func.set_section(Some(&section.0));
            }
            match body {
                Some(_) => func.set_linkage(Linkage::DLLExport),
                None => func.set_linkage(Linkage::DLLImport),
            }
            let func = CodegenFunction {
                ret_args_types: ret_args_types.into_boxed_slice(),
                value: func,
            };
            me.external_functions.insert(key, func);
        }
        drop(ext_function_reader);

        drop(struct_reader);

        let static_reader = ctx.statics.read();
        let mut statics = IndexMap::new();
        for (key, static_element) in static_reader.entries() {
            let global = me.module.add_global(
                llvm_basic_ty(&static_element.ty, &default_types, &me.structs, context),
                None,
                &mangle_static(ctx, key),
            );
            global.set_linkage(Linkage::Internal);
            global.set_initializer(&basic_val(&static_element.value, &mut me));
            statics.insert(key, global);
        }
        drop(static_reader);

        let module_reader = ctx.modules.read();
        for tc_module in module_reader.iter() {
            let mut inline_assembly = String::new();
            for asm in tc_module.assembly.iter() {
                if !inline_assembly.is_empty() {
                    inline_assembly.push('\n');
                }
                inline_assembly.push_str(&asm.1);
            }
            me.module.set_inline_assembly(&inline_assembly);
        }
        drop(module_reader);

        Ok(me)
    }

    fn make_fn_instance(&self, instance: FnInstance<'arena>) -> CodegenFunction<'ctx> {
        let (contract, _) = &self.tc_ctx.functions.read()[instance.fn_id];
        if contract.annotations.has_annotation::<IntrinsicAnnotation>() {
            unreachable!(
                "tried to instantiate an intrinsic. This should be impossible and have been sorted out by typechecking."
            );
        }
        assert_eq!(
            contract.generics.len(),
            instance.generics.len(),
            "Mismatching generics between contract and instantiation of function {:?}",
            contract.name
        );
        let subst_ctx = SubstitutionCtx::new(self.tc_ctx, &instance.generics);

        let mut ret_args_types = Vec::with_capacity(contract.arguments.len() + 1);
        let ptrsize = (self.default_types.isize.get_bit_width() / 8) as u8;
        let struct_reader = self.tc_ctx.structs.read();

        let ret_ty = return_ty(
            self.context,
            &contract.return_type.substitute(&subst_ctx),
            ptrsize,
            &struct_reader,
            |t| llvm_basic_ty(t, &self.default_types, &self.structs, self.context),
        );
        ret_args_types.push(ret_ty);

        for (_, ty) in &contract.arguments {
            let arg_ty = argument(
                self.context,
                &ty.substitute(&subst_ctx),
                ptrsize,
                &struct_reader,
                |t| llvm_basic_ty(t, &self.default_types, &self.structs, self.context),
            );
            ret_args_types.push(arg_ty);
        }
        drop(struct_reader);

        let param_types = ret_args_types[1..]
            .iter()
            .filter_map(|v| v.as_arg_ty(&self.default_types).map(Into::into))
            .collect::<Vec<_>>();

        let has_var_args = contract.annotations.has_annotation::<ExternVarArg>();

        let fn_ty =
            ret_args_types[0].func_ty(param_types, has_var_args, &self.default_types, self.context);

        let name = mangle_function_instance(self.tc_ctx, instance);
        let func = self
            .module
            .add_function(name.as_str(), fn_ty, Some(Linkage::Internal));

        ArgumentType::add_ret_attribute_args(&ret_args_types, func, self.context);

        let inline = self
            .context
            .create_enum_attribute(Attribute::get_named_enum_kind_id("alwaysinline"), 0);
        let naked = self
            .context
            .create_enum_attribute(Attribute::get_named_enum_kind_id("naked"), 0);
        let noinline = self
            .context
            .create_enum_attribute(Attribute::get_named_enum_kind_id("noinline"), 0);

        if let Some(callconv) = contract
            .annotations
            .get_first_annotation::<CallConvAnnotation>()
        {
            match callconv {
                CallConvAnnotation::C => {
                    func.set_call_conventions(LLVMCallConv::LLVMCCallConv as u32)
                }
                CallConvAnnotation::Fast => {
                    func.set_call_conventions(LLVMCallConv::LLVMFastCallConv as u32)
                }
                CallConvAnnotation::Cold => {
                    func.set_call_conventions(LLVMCallConv::LLVMColdCallConv as u32)
                }
                CallConvAnnotation::Inline => func.add_attribute(AttributeLoc::Function, inline),
                CallConvAnnotation::Naked => func.add_attribute(AttributeLoc::Function, naked),
            }
        }
        if contract.annotations.has_annotation::<Noinline>() {
            func.add_attribute(AttributeLoc::Function, noinline);
        }
        let (_, line) = contract
            .span
            .with_source_file(self.tc_ctx.source_map())
            .lookup_pos();
        func.set_subprogram(
            self.debug_ctx.make_subprogram(
                contract.module_id,
                contract.return_type.substitute(&subst_ctx),
                contract
                    .arguments
                    .iter()
                    .map(|(_, ty)| ty.substitute(&subst_ctx)),
                &self.tc_ctx.structs.read(),
                contract
                    .name
                    .map(|v| v.symbol().to_str())
                    .unwrap_or(mangling::ANON_FN_NAME),
                &name,
                line,
                true,
            ),
        );
        CodegenFunction {
            value: func,
            ret_args_types: ret_args_types.into_boxed_slice(),
        }
    }

    /// Panics if instance was not yet generated.
    pub(crate) fn with_generated_fn_instance<R>(
        &self,
        instance: &FnInstance<'arena>,
        f: impl FnOnce(&CodegenFunction<'ctx>) -> R,
    ) -> R {
        f(&self.function_store.borrow().map[instance])
    }

    pub fn get_fn_instance(&self, instance: FnInstance<'arena>) -> FunctionValue<'ctx> {
        if let Some(v) = self.get_fn_instance_if_generated(&instance) {
            return v;
        }
        let value = self.make_fn_instance(instance);
        let mut store = self.function_store.borrow_mut();
        let ptr = value.value;
        store.map.insert(instance, value);
        store.missing.insert(instance);
        ptr
    }

    pub fn get_fn_instance_if_generated(
        &self,
        instance: &FnInstance<'arena>,
    ) -> Option<FunctionValue<'ctx>> {
        self.function_store
            .borrow()
            .map
            .get(instance)
            .map(|v| v.value)
    }

    pub fn get_string(&self, s: Symbol<'arena>) -> GlobalValue<'ctx> {
        if let Some(&v) = self.string_map.borrow().get(&s) {
            return v;
        }
        assert!(s.len() < u32::MAX as usize);
        let mut string_map = self.string_map.borrow_mut();
        let global = self.module.add_global(
            self.default_types.i8.array_type(s.len() as u32),
            None,
            "str",
        );
        global.set_linkage(Linkage::Private);
        global.set_initializer(&self.context.const_string(s.as_bytes(), false));
        string_map.insert(s, global);
        global
    }

    pub fn get_vtable(&self, vtable: &VTableKey<'arena>) -> GlobalValue<'ctx> {
        if let Some(&v) = self.vtables.borrow().get(vtable) {
            return v;
        }
        let mut vtables = self.vtables.borrow_mut();
        let struct_reader = self.tc_ctx.structs.read();
        let ptr_width = (self.default_types.isize.get_bit_width() / 8) as u64;

        let mut typs = vec![self.default_types.isize.into()];
        typs.extend(std::iter::repeat_n(
            self.default_types.ptr.as_basic_type_enum(),
            vtable.1.len(),
        ));

        let vtable_ty = self.context.struct_type(&typs, false);
        let ty_size = vtable.0.size_and_alignment(ptr_width, &struct_reader).0;
        let mut field_values = vec![self.default_types.isize.const_int(ty_size, false).into()];

        for &trait_id in vtable.1.iter() {
            match *vtable.0 {
                &TyKind::Struct { struct_id, .. } => {
                    for fn_id in struct_reader[struct_id].trait_impl[trait_id]
                        .iter()
                        .copied()
                    {
                        let func = self.get_fn_instance(FnInstance::new_mono(fn_id));
                        field_values.push(func.as_global_value().as_pointer_value().into());
                    }
                }
                _ => unreachable!(),
            }
        }
        let global = self.module.add_global(vtable_ty, None, "vtable");
        global.set_linkage(Linkage::Private);
        global.set_initializer(&vtable_ty.const_named_struct(&field_values));
        vtables.insert(vtable.clone(), global);

        global
    }

    pub fn get_intrinsic(
        &self,
        sym: Symbol<'arena>,
        types: impl Iterator<Item = Ty<'arena>>,
        ret_ty: Ty<'arena>,
    ) -> FunctionValue<'ctx> {
        if let Some(&func) = self.intrinsics.borrow().get(&sym) {
            return func;
        }
        assert!(!has_special_encoding(*ret_ty));

        let params = types
            .inspect(|ty| assert!(!has_special_encoding(ty)))
            .map(|ty| llvm_basic_ty(*ty, &self.default_types, &self.structs, self.context).into())
            .collect::<Vec<_>>();
        let fn_ty = if ret_ty.is_voidlike() {
            self.context.void_type().fn_type(&params, false)
        } else {
            llvm_basic_ty(*ret_ty, &self.default_types, &self.structs, self.context)
                .fn_type(&params, false)
        };
        let function = self.module.add_function(sym.to_str(), fn_ty, None);
        self.intrinsics.borrow_mut().insert(sym, function);

        function
    }

    pub fn finish(&self) -> Result<(), LLVMString> {
        self.finalize_debug_info();
        self.check()?;
        self.optimize()
    }

    pub fn finalize_debug_info(&self) {
        self.debug_ctx.builder.finalize();
    }

    pub fn optimize(&self) -> Result<(), LLVMString> {
        let passes = match self.config.optimizations {
            Optimizations::None => "default<O0>",
            Optimizations::Low => "default<O1>",
            Optimizations::Normal => "default<O2>",
            Optimizations::High => "default<O3>",
            Optimizations::Small => "default<Os>",
            Optimizations::ExtremelySmall => "default<Oz>",
        };
        self.run_passes(passes)
    }

    pub fn check(&self) -> Result<(), LLVMString> {
        self.module.verify()
    }

    pub fn run_passes(&self, passes: &str) -> Result<(), LLVMString> {
        self.module
            .run_passes(passes, &self.machine, PassBuilderOptions::create())
    }

    pub fn gen_bitcode(&self) -> MemoryBuffer {
        self.module.write_bitcode_to_memory()
    }

    pub fn gen_ir(&self) -> LLVMString {
        self.module.print_to_string()
    }

    pub fn gen_assembly(&self) -> Result<MemoryBuffer, CodegenError<'arena>> {
        self.machine
            .write_to_memory_buffer(&self.module, FileType::Assembly)
            .map_err(CodegenError::from)
    }

    pub fn write_bitcode(&self, writer: &mut dyn Write) -> std::io::Result<()> {
        let buf = self.module.write_bitcode_to_memory();
        writer.write_all(buf.as_slice())
    }

    pub fn write_ir(&self, writer: &mut dyn Write) -> std::io::Result<()> {
        let string = self.module.print_to_string();
        writer.write_all(string.to_bytes())
    }

    pub fn write_assembly(&self, writer: &mut dyn Write) -> Result<(), CodegenError<'arena>> {
        let buffer = self
            .machine
            .write_to_memory_buffer(&self.module, FileType::Assembly)
            .map_err(CodegenError::from)?;
        writer
            .write_all(buffer.as_slice())
            .map_err(CodegenError::WriteAssemblyError)
    }

    pub fn write_object(&self, writer: &mut dyn Write) -> Result<(), CodegenError<'arena>> {
        let buffer = self
            .machine
            .write_to_memory_buffer(&self.module, FileType::Object)
            .map_err(CodegenError::from)?;
        writer
            .write_all(buffer.as_slice())
            .map_err(CodegenError::WriteObjectError)
    }

    pub fn compile_all_fns<
        KeyExtern,
        KeyExternFn,
        KeyNormal,
        KeyNormalFn,
        U: CompileStatusUpdate<'ctx, 'arena, 'a, KeyExtern, KeyExternFn, KeyNormal, KeyNormalFn>,
    >(
        &mut self,
        mut updater: U,
    ) -> Result<(), ErrorEmitted> {
        let mut emitted = Ok(());
        let extern_funcs = self.external_functions.keys().collect::<Vec<_>>();
        let key = updater.start_compiling_extern(self, extern_funcs.len() as u64);
        for fn_id in extern_funcs {
            let key = updater.start_compiling_extern_fn(self, fn_id);

            let res = self.internal_compile_extern(fn_id);
            if res.is_err() {
                emitted = res;
            }

            updater.finish_compiling_extern_fn(self, key);
        }
        updater.finish_compiling_extern(self, key);

        let key = updater.start_compiling_normal(self);
        loop {
            let mut function_store = self.function_store.borrow_mut();
            let Some(&instance) = function_store.missing.iter().next() else {
                break;
            };
            function_store.missing.remove(&instance);
            drop(function_store);

            let key = updater.start_compiling_normal_fn(self, instance);

            let res = self.internal_compile_fn(instance);
            if res.is_err() {
                emitted = res;
            }

            updater.finish_compiling_normal_fn(self, key);
        }
        updater.finish_compiling_normal(self, key);

        emitted
    }

    fn internal_compile_fn(&mut self, instance: FnInstance<'arena>) -> Result<(), ErrorEmitted> {
        let fn_reader = self.tc_ctx.functions.read();
        // don't do anything if the function is an intrinsic or has generics, because such
        // functions should never be directly called (intrinsics get turned into custom llvm
        // code and generic functions get monomorphised)
        let (contract, ir) = &fn_reader[instance.fn_id];
        assert!(
            !contract.annotations.has_annotation::<IntrinsicAnnotation>(),
            "fn instance with intrinsic annotation"
        );
        assert!(
            !contract
                .annotations
                .has_annotation::<LLVMIntrinsicAnnotation>(),
            "fn instance with llvm intrinsic annotations"
        );
        assert_eq!(
            contract.generics.len(),
            instance.generics.len(),
            "fn instance with llvm intrinsic annotations"
        );

        let func = self
            .get_fn_instance_if_generated(&instance)
            .expect("trying to compile a fn instance that hasn't been requested yet.");

        let body_basic_block = self.context.append_basic_block(func, "entry");

        self.builder.position_at_end(body_basic_block);
        let scope = func
            .get_subprogram()
            .expect("functions should always have an associated subprogram")
            .as_debug_info_scope();

        let mut param_idx = 0;
        let (return_ty, return_val) = self.with_generated_fn_instance(&instance, |func| {
            let return_ty = func.return_ty();
            let return_val = match return_ty {
                ArgumentType::ByVal(_) => unreachable!("byval(_) is not valid for return types."),
                ArgumentType::SRet(_) => {
                    param_idx += 1;
                    func.value.get_nth_param(0).unwrap().into_pointer_value()
                }
                ArgumentType::Regular(_) | ArgumentType::None => {
                    self.default_types.ptr.const_null()
                }
            };
            (return_ty, return_val)
        });

        let mut function_ctx = self.make_function_codegen_context(
            instance.generics,
            contract.module_id,
            ir,
            body_basic_block,
            return_ty,
            return_val,
        );
        function_ctx.goto(body_basic_block);

        function_ctx
            .set_current_debug_location(function_ctx.ctx.debug_ctx.location(scope, contract.span));

        let void_ty = function_ctx.ctx.default_types.empty_struct;
        function_ctx
            .ctx
            .with_generated_fn_instance(&instance, |func| {
                assert_eq!(func.args().len(), ir.params().len())
            });

        for (idx, item) in ir.params().iter().enumerate() {
            let ptr;
            let value = item.value;
            let arg = function_ctx
                .ctx
                .with_generated_fn_instance(&instance, |func| func.args()[idx]);

            match arg {
                ArgumentType::SRet(_) => unreachable!("sret(_) is illegal for arguments"),
                ArgumentType::Regular(_) => {
                    let arg = func.get_nth_param(param_idx).unwrap();
                    param_idx += 1;
                    function_ctx.push_value(value, arg);
                    ptr = function_ctx.get_value_ptr(value);
                }
                ArgumentType::ByVal(_) => {
                    ptr = func.get_nth_param(param_idx).unwrap().into_pointer_value();
                    param_idx += 1;
                    function_ctx.push_value_raw(value, ptr);
                }
                ArgumentType::None => ptr = function_ctx.build_alloca(void_ty, "").unwrap(),
            }

            function_ctx.ctx.debug_ctx.declare_param(
                ptr,
                scope,
                contract.span,
                ir.get_ty(value),
                item.name.symbol(),
                function_ctx.current_block,
                contract.module_id,
                &function_ctx.structs_reader,
                idx as u32,
            );
        }

        let exprs = ir.get_entry_block_exprs();
        let tracker = function_ctx.ctx.tc_ctx.ctx.track_errors();
        for expr in exprs {
            _ = function_ctx.build_expr(expr, scope).map_err(|e| {
                function_ctx
                    .ctx
                    .tc_ctx
                    .emit_diag(CodegenError::Builder(e, expr.span()).to_error())
            });
        }

        function_ctx.ctx.tc_ctx.errors_happened_res(tracker)
    }

    fn internal_compile_extern(&mut self, fn_id: ExternalFunctionId) -> Result<(), ErrorEmitted> {
        let ext_fn_reader = self.tc_ctx.external_functions.read();
        // if there's no body to an external function, don't try to generate one.
        if ext_fn_reader[fn_id].1.is_none() {
            return Ok(());
        }

        let (contract, Some(ir)) = &ext_fn_reader[fn_id] else {
            unreachable!()
        };
        let func = &self.external_functions[fn_id];

        let body_basic_block = self.context.append_basic_block(func.value, "entry");

        self.builder.position_at_end(body_basic_block);
        let scope = func
            .value
            .get_subprogram()
            .expect("functions should always have an associated subprogram")
            .as_debug_info_scope();

        let mut param_idx = 0;
        let return_ty = func.return_ty();
        let return_val = match return_ty {
            ArgumentType::ByVal(_) => unreachable!("byval(_) is not valid for return types."),
            ArgumentType::SRet(_) => {
                param_idx += 1;
                func.value.get_nth_param(0).unwrap().into_pointer_value()
            }
            ArgumentType::Regular(_) | ArgumentType::None => self.default_types.ptr.const_null(),
        };

        let mut function_ctx = self.make_function_codegen_context(
            TyList::EMPTY,
            contract.module_id,
            ir,
            body_basic_block,
            return_ty,
            return_val,
        );
        function_ctx.goto(body_basic_block);

        function_ctx
            .set_current_debug_location(function_ctx.ctx.debug_ctx.location(scope, contract.span));

        let void_ty = function_ctx.ctx.default_types.empty_struct;
        assert_eq!(
            function_ctx.ctx.external_functions[fn_id].args().len(),
            ir.params().len()
        );
        for (idx, item) in ir.params().iter().enumerate() {
            let func = &function_ctx.ctx.external_functions[fn_id];
            let arg = &func.args()[idx];

            let ptr;
            let value = item.value;
            match arg {
                ArgumentType::SRet(_) => unreachable!("sret(_) is illegal for arguments"),
                ArgumentType::Regular(_) => {
                    let arg = func.value.get_nth_param(param_idx).unwrap();
                    param_idx += 1;
                    function_ctx.push_value(value, arg);
                    ptr = function_ctx.get_value_ptr(value);
                }
                ArgumentType::ByVal(_) => {
                    ptr = func
                        .value
                        .get_nth_param(param_idx)
                        .unwrap()
                        .into_pointer_value();
                    param_idx += 1;
                    function_ctx.push_value_raw(value, ptr);
                }
                ArgumentType::None => ptr = function_ctx.build_alloca(void_ty, "").unwrap(),
            }

            function_ctx.ctx.debug_ctx.declare_param(
                ptr,
                scope,
                contract.span,
                ir.get_ty(value),
                item.name.symbol(),
                function_ctx.current_block,
                contract.module_id,
                &function_ctx.structs_reader,
                idx as u32,
            );
        }

        let exprs = ir.get_entry_block_exprs();
        let tracker = function_ctx.ctx.tc_ctx.ctx.track_errors();
        for expr in exprs {
            _ = function_ctx.build_expr(expr, scope).map_err(|e| {
                function_ctx
                    .ctx
                    .tc_ctx
                    .emit_diag(CodegenError::Builder(e, expr.span()).to_error())
            });
        }

        function_ctx.ctx.tc_ctx.errors_happened_res(tracker)
    }

    pub fn write_object_file(&self, path: impl AsRef<Path>) -> Result<(), LLVMString> {
        self.machine
            .write_to_file(&self.module, FileType::Object, path.as_ref())
    }
}

#[allow(unused_variables)]
pub trait CompileStatusUpdate<'ctx, 'arena, 'tc_ctx, KeyExtern, KeyExternFn, KeyNormal, KeyNormalFn>
{
    fn start_compiling_extern(
        &mut self,
        ctx: &CodegenContext<'ctx, 'arena, 'tc_ctx>,
        amount: u64,
    ) -> KeyExtern;
    fn finish_compiling_extern(
        &mut self,
        ctx: &CodegenContext<'ctx, 'arena, 'tc_ctx>,
        key: KeyExtern,
    ) {
    }

    fn start_compiling_normal(&mut self, ctx: &CodegenContext<'ctx, 'arena, 'tc_ctx>) -> KeyNormal;
    fn finish_compiling_normal(
        &mut self,
        ctx: &CodegenContext<'ctx, 'arena, 'tc_ctx>,
        key: KeyNormal,
    ) {
    }

    fn start_compiling_extern_fn(
        &mut self,
        ctx: &CodegenContext<'ctx, 'arena, 'tc_ctx>,
        id: ExternalFunctionId,
    ) -> KeyExternFn;
    fn finish_compiling_extern_fn(
        &mut self,
        ctx: &CodegenContext<'ctx, 'arena, 'tc_ctx>,
        key: KeyExternFn,
    ) {
    }

    fn start_compiling_normal_fn(
        &mut self,
        ctx: &CodegenContext<'ctx, 'arena, 'tc_ctx>,
        id: FnInstance<'arena>,
    ) -> KeyNormalFn;
    fn finish_compiling_normal_fn(
        &mut self,
        ctx: &CodegenContext<'ctx, 'arena, 'tc_ctx>,
        key: KeyNormalFn,
    ) {
    }
}

impl CompileStatusUpdate<'_, '_, '_, (), (), (), ()> for () {
    fn start_compiling_extern(&mut self, _: &CodegenContext<'_, '_, '_>, _: u64) {}
    fn start_compiling_normal(&mut self, _: &CodegenContext<'_, '_, '_>) {}
    fn start_compiling_extern_fn(&mut self, _: &CodegenContext<'_, '_, '_>, _: ExternalFunctionId) {
    }
    fn start_compiling_normal_fn(&mut self, _: &CodegenContext<'_, '_, '_>, _: FnInstance<'_>) {}
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

fn get_mira_version() -> String {
    const COMMIT_HASH: Option<&str> = option_env!("MIRAC_COMMIT_HASH_SHORT");
    const COMMIT_DATE: Option<&str> = option_env!("MIRAC_COMMIT_DATE");
    const VERSION: &str = const {
        match option_env!("MIRAC_PKG_OVERRIDE") {
            Some(v) => v,
            None => env!("CARGO_PKG_VERSION"),
        }
    };

    let mut s = format!("mirac version {VERSION}");
    if COMMIT_HASH.is_some() || COMMIT_DATE.is_some() {
        s.push_str(" (");
    }
    if let Some(date) = COMMIT_DATE {
        s.push_str(date);
    }
    if let Some(hash) = COMMIT_HASH {
        if COMMIT_DATE.is_some() {
            s.push(' ');
        }
        s.push_str(hash);
    }
    if COMMIT_HASH.is_some() || COMMIT_DATE.is_some() {
        s.push(')');
    }
    s
}
