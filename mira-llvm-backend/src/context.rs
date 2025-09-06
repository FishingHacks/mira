use core::panic;
use std::{
    collections::{HashMap, HashSet},
    io::Write,
    path::Path,
    sync::Arc,
};

use crate::to_basic_value;

use super::{
    TyKindExt,
    debug_builder::DebugContext,
    error::CodegenError,
    intrinsics::Intrinsics,
    mangling::{mangle_function, mangle_static, mangle_string, mangle_struct},
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
    types::{BasicType, FloatType, IntType, PointerType, StructType},
    values::{FunctionValue, GlobalValue},
};

use mira_common::store::{AssociatedStore, StoreKey};
use mira_context::ErrorEmitted;
use mira_parser::std_annotations::{
    alias::ExternAliasAnnotation, callconv::CallConvAnnotation, ext_vararg::ExternVarArg,
    intrinsic::IntrinsicAnnotation, llvm_intrinsic::LLVMIntrinsicAnnotation, noinline::Noinline,
    section::SectionAnnotation,
};
use mira_spans::interner::Symbol;
use mira_target::{NATIVE_TARGET, Target};
use mira_typeck::{
    Ty, TyKind, TypeckCtx, TypedExternalFunction, TypedFunction, TypedStatic, TypedStruct,
    TypedTrait, default_types,
    ir::{IR, TypedExpression, TypedLiteral, Visitor},
};

#[derive(Clone, Copy)]
pub(super) struct DefaultTypes<'ctx> {
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

pub(crate) type FunctionsStore<'ctx, 'arena> =
    AssociatedStore<FunctionValue<'ctx>, TypedFunction<'arena>>;
pub(crate) type ExternalFunctionsStore<'ctx, 'arena> =
    AssociatedStore<FunctionValue<'ctx>, TypedExternalFunction<'arena>>;
pub(crate) type StructsStore<'ctx, 'arena> = AssociatedStore<StructType<'ctx>, TypedStruct<'arena>>;
pub(crate) type StaticsStore<'ctx, 'arena> =
    AssociatedStore<GlobalValue<'ctx>, TypedStatic<'arena>>;

pub struct CodegenContextBuilder(Context);

impl CodegenContextBuilder {
    pub fn new() -> Self {
        Self(Context::create())
    }

    pub fn build<'ctx, 'arena>(
        &'ctx self,
        ctx: Arc<TypeckCtx<'arena>>,
        module: &str,
        path: Arc<Path>,
        config: CodegenConfig<'ctx>,
    ) -> Result<CodegenContext<'ctx, 'arena>, CodegenError<'arena>> {
        CodegenContext::new(&self.0, ctx, module, path, config)
    }
}

impl Default for CodegenContextBuilder {
    fn default() -> Self {
        Self::new()
    }
}

pub struct CodegenContext<'ctx, 'arena> {
    pub(super) tc_ctx: Arc<TypeckCtx<'arena>>,
    pub(super) builder: Builder<'ctx>,
    pub(super) context: &'ctx Context,
    pub(super) default_types: DefaultTypes<'ctx>,
    pub module: Module<'ctx>,
    pub machine: TargetMachine,
    pub triple: TargetTriple,
    pub(super) functions: FunctionsStore<'ctx, 'arena>,
    pub(super) external_functions: ExternalFunctionsStore<'ctx, 'arena>,
    pub(super) structs: StructsStore<'ctx, 'arena>,
    pub(super) statics: StaticsStore<'ctx, 'arena>,
    pub(super) string_map: HashMap<Symbol<'arena>, GlobalValue<'ctx>>,
    pub(super) debug_ctx: DebugContext<'ctx, 'arena>,
    pub(super) intrinsics: Intrinsics<'ctx, 'arena>,
    pub(super) vtables: HashMap<(Ty<'arena>, Vec<StoreKey<TypedTrait<'arena>>>), GlobalValue<'ctx>>,
    pub(super) config: CodegenConfig<'ctx>,
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

impl<'ctx, 'arena> CodegenContext<'ctx, 'arena> {
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

    fn new(
        context: &'ctx Context,
        ctx: Arc<TypeckCtx<'arena>>,
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
            &ctx,
            &path,
            config.optimizations != Optimizations::None,
            shared_ctx,
        );

        let builder = context.create_builder();
        let StringVtableCollector {
            mut strings,
            mut vtables,
        } = collect_data(&ctx);

        let mut string_map = HashMap::new();
        for strn in strings.drain() {
            let constant = context.const_string(strn.as_bytes(), false);
            let mangled_name = mangle_string(&strn);
            assert!(strn.len() <= u32::MAX as usize);

            let global = module.add_global(
                default_types.i8.array_type(strn.len() as u32),
                None,
                &mangled_name,
            );
            global.set_linkage(Linkage::Internal);
            global.set_initializer(&constant);
            string_map.insert(strn, global);
        }
        drop(strings);

        let struct_reader = ctx.structs.read();
        let mut structs = AssociatedStore::new();
        for k in struct_reader.indices() {
            structs.insert(k, context.opaque_struct_type(&mangle_struct(&ctx, k)));
        }
        for (key, structure) in struct_reader.index_value_iter() {
            let fields = structure
                .elements
                .iter()
                .map(|(_, t, _)| {
                    if *t == default_types::void || *t == default_types::never {
                        default_types.i8.array_type(0).into()
                    } else {
                        t.to_llvm_basic_type(&default_types, &structs, context)
                    }
                })
                .collect::<Vec<_>>();
            assert!(
                structs[key].set_body(&fields, false),
                "struct should not yet be initialized"
            );
        }

        // attributes:
        let inline =
            context.create_enum_attribute(Attribute::get_named_enum_kind_id("alwaysinline"), 0);
        let naked = context.create_enum_attribute(Attribute::get_named_enum_kind_id("naked"), 0);
        let noinline =
            context.create_enum_attribute(Attribute::get_named_enum_kind_id("noinline"), 0);

        // functions
        let function_reader = ctx.functions.read();
        let mut functions = AssociatedStore::new();
        for (key, (contract, _)) in function_reader.index_value_iter() {
            if contract.annotations.has_annotation::<IntrinsicAnnotation>() {
                continue;
            }
            assert!(
                contract.generics.is_empty(),
                "function should've been monomorphised by now"
            );

            let param_types = contract
                .arguments
                .iter()
                .filter(|(_, t)| *t != default_types::void && *t != default_types::never)
                .map(|(_, t)| {
                    t.to_llvm_basic_type(&default_types, &structs, context)
                        .into()
                })
                .collect::<Vec<_>>();

            let fn_typ = if contract.return_type == default_types::never
                || contract.return_type == default_types::void
            {
                context.void_type().fn_type(&param_types, false)
            } else {
                contract
                    .return_type
                    .to_llvm_basic_type(&default_types, &structs, context)
                    .fn_type(&param_types, false)
            };
            let name = mangle_function(&ctx, key);
            let func = module.add_function(name.as_str(), fn_typ, Some(Linkage::Internal));
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
            func.set_subprogram(debug_ctx.funcs[key]);
            functions.insert(key, func);
        }
        drop(function_reader);

        // external functions
        let ext_function_reader = ctx.external_functions.read();
        let mut external_functions = AssociatedStore::new();
        for (key, (contract, body)) in ext_function_reader.index_value_iter() {
            let param_types = contract
                .arguments
                .iter()
                .filter_map(|(_, t)| {
                    (*t != default_types::void && *t != default_types::never).then_some(
                        t.to_llvm_basic_type(&default_types, &structs, context)
                            .into(),
                    )
                })
                .collect::<Vec<_>>();

            if (!contract.return_type.is_primitive() && !contract.return_type.has_refs())
                || (contract.return_type.has_refs() && !contract.return_type.is_thin_ptr())
                || !contract.return_type.is_sized()
                || contract.arguments.iter().any(|(_, v)| !v.is_sized())
            {
                unreachable!("typechecking should've validated the return type and arguments.");
            }
            let has_var_args = contract.annotations.has_annotation::<ExternVarArg>();

            let fn_typ = if contract.return_type == default_types::never
                || contract.return_type == default_types::void
            {
                context.void_type().fn_type(&param_types, has_var_args)
            } else {
                contract
                    .return_type
                    .to_llvm_basic_type(&default_types, &structs, context)
                    .fn_type(&param_types, has_var_args)
            };
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

            let func = module.add_function(name, fn_typ, None);
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
            func.set_subprogram(debug_ctx.ext_funcs[key]);
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
            external_functions.insert(key, func);
        }
        drop(ext_function_reader);

        let static_reader = ctx.statics.read();
        let mut statics = AssociatedStore::new();
        for (key, static_element) in static_reader.index_value_iter() {
            let global = module.add_global(
                static_element
                    .ty
                    .to_llvm_basic_type(&default_types, &structs, context),
                None,
                &mangle_static(&ctx, key),
            );
            global.set_linkage(Linkage::Internal);
            global.set_initializer(&to_basic_value(
                &static_element.value,
                &|i| panic!("index out of bounds: the length is 0, but the index is {i}"),
                &default_types,
                &structs,
                &builder,
                &statics,
                &functions,
                &external_functions,
                &string_map,
                context,
            ));
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
            module.set_inline_assembly(&inline_assembly);
        }
        drop(module_reader);

        let mut vtable_map = HashMap::new();
        let trait_reader = ctx.traits.read();
        for (ty, traits) in vtables.drain() {
            let mut typs = vec![default_types.isize.into()];
            for trait_id in traits.iter() {
                for _ in trait_reader[*trait_id].functions.iter() {
                    // func pointer
                    typs.push(default_types.ptr.into());
                }
            }
            let vtable_ty = context.struct_type(&typs, false);
            let mut field_values = vec![
                default_types
                    .isize
                    .const_int(
                        ty.size_and_alignment(
                            (default_types.isize.get_bit_width() / 8) as u64,
                            &struct_reader,
                        )
                        .0,
                        false,
                    )
                    .into(),
            ];
            for trait_id in traits.iter() {
                match &**ty {
                    TyKind::Struct { struct_id, .. } => {
                        for fn_id in struct_reader[struct_id].trait_impl[trait_id]
                            .iter()
                            .copied()
                        {
                            field_values
                                .push(functions[fn_id].as_global_value().as_pointer_value().into());
                        }
                    }
                    _ => unreachable!(),
                }
            }
            let global = module.add_global(vtable_ty, Default::default(), "vtable");
            global.set_linkage(Linkage::LinkOnceODR);
            global.set_initializer(&vtable_ty.const_named_struct(&field_values));
            vtable_map.insert((ty, traits), global);
        }
        drop(trait_reader);
        drop(struct_reader);
        drop(vtables);

        Ok(Self {
            context,
            module,
            builder,
            default_types,
            functions,
            external_functions,
            structs,
            statics,
            string_map,
            machine,
            triple,
            tc_ctx: ctx,
            debug_ctx,
            intrinsics: Intrinsics::new(context),
            vtables: vtable_map,
            config,
        })
    }

    pub fn compile_external_fn(
        &mut self,
        fn_id: StoreKey<TypedExternalFunction<'arena>>,
    ) -> Result<(), ErrorEmitted> {
        self.internal_compile_fn(fn_id.cast(), true)
    }
    pub fn compile_fn(
        &mut self,
        fn_id: StoreKey<TypedFunction<'arena>>,
    ) -> Result<(), ErrorEmitted> {
        self.internal_compile_fn(fn_id, false)
    }
    fn internal_compile_fn(
        &mut self,
        fn_id: StoreKey<TypedFunction<'arena>>,
        is_external: bool,
    ) -> Result<(), ErrorEmitted> {
        let ext_fn_reader = self.tc_ctx.external_functions.read();
        let fn_reader = self.tc_ctx.functions.read();
        // if there's no body to an external function, don't try to generate one.
        if is_external && ext_fn_reader[fn_id.cast()].1.is_none() {
            return Ok(());
        } else if !is_external {
            // don't do anything if the function is an intrinsic or has generics, because such
            // functions should never be directly called (intrinsics get turned into custom llvm
            // code and generic functions get monomorphised)
            let contract = &fn_reader[fn_id].0;
            if !contract.generics.is_empty()
                || contract.annotations.has_annotation::<IntrinsicAnnotation>()
                || contract
                    .annotations
                    .has_annotation::<LLVMIntrinsicAnnotation>()
            {
                return Ok(());
            }
        }
        drop(ext_fn_reader);
        drop(fn_reader);

        let func = if is_external {
            self.external_functions[fn_id.cast()]
        } else {
            self.functions[fn_id]
        };

        let body_basic_block = self
            .context
            .append_basic_block(func, &format!("entry_{fn_id}"));
        let void_arg = self.default_types.empty_struct.const_zero().into();

        self.builder.position_at_end(body_basic_block);
        let scope = if is_external {
            self.debug_ctx.ext_funcs[fn_id.cast()].as_debug_info_scope()
        } else {
            self.debug_ctx.funcs[fn_id].as_debug_info_scope()
        };

        let mut function_ctx = self.make_function_codegen_context(func, body_basic_block);
        function_ctx.goto(body_basic_block);
        let function_reader = function_ctx.tc_ctx.functions.read();
        let ext_function_reader = function_ctx.tc_ctx.external_functions.read();
        let structs_reader = function_ctx.tc_ctx.structs.read();
        let contract = if is_external {
            &ext_function_reader[fn_id.cast()].0
        } else {
            &function_reader[fn_id].0
        };
        let ir = if is_external {
            let Some(v) = &ext_function_reader[fn_id.cast()].1 else {
                unreachable!()
            };
            v
        } else {
            &function_reader[fn_id].1
        };
        function_ctx.set_ir(ir);

        function_ctx
            .builder
            .set_current_debug_location(function_ctx.debug_ctx.location(scope, contract.span));

        let mut param_idx = 0;
        for (idx, param) in ir.params().iter().enumerate() {
            let ty = ir.get_ty(param.value);
            if ty == default_types::void || ty == default_types::never {
                function_ctx.push_value(param.value, void_arg);
            } else {
                function_ctx.push_value(param.value, func.get_nth_param(param_idx).unwrap());
                param_idx += 1;
            }
            let ptr = function_ctx.get_value_ptr(param.value);
            function_ctx.debug_ctx.declare_param(
                ptr,
                scope,
                contract.span,
                ty,
                param.name.symbol(),
                function_ctx.current_block,
                contract.module_id,
                &structs_reader,
                idx as u32,
            );
        }

        drop(structs_reader);

        let exprs = ir.get_entry_block_exprs();
        let tracker = function_ctx.tc_ctx.ctx.track_errors();
        for expr in exprs {
            _ = function_ctx
                .codegen(expr, scope, contract.module_id)
                .map_err(|e| {
                    function_ctx
                        .tc_ctx
                        .ctx
                        .emit_diag(CodegenError::Builder(e, expr.span()).to_error())
                });
        }
        function_ctx.tc_ctx.ctx.errors_happened_res(tracker)
    }

    pub fn write_object_file(&self, path: impl AsRef<Path>) -> Result<(), LLVMString> {
        self.machine
            .write_to_file(&self.module, FileType::Object, path.as_ref())
    }
}

#[derive(Default)]
struct StringVtableCollector<'ctx> {
    strings: HashSet<Symbol<'ctx>>,
    vtables: HashSet<(Ty<'ctx>, Vec<StoreKey<TypedTrait<'ctx>>>)>,
}

impl<'ctx> StringVtableCollector<'ctx> {
    fn visit_lit_standalone(&mut self, lit: &TypedLiteral<'ctx>) {
        match lit {
            &TypedLiteral::String(s) => _ = self.strings.insert(s),

            TypedLiteral::ArrayInit(_, lit, _) => self.visit_lit_standalone(lit),
            TypedLiteral::Array(_, lits)
            | TypedLiteral::Struct(_, lits)
            | TypedLiteral::Tuple(lits) => {
                for lit in lits {
                    self.visit_lit_standalone(lit);
                }
            }

            _ => {}
        }
    }
}

impl<'ctx> Visitor<'ctx> for StringVtableCollector<'ctx> {
    fn visit_expr(
        &mut self,
        expr: &TypedExpression<'ctx>,
        _: &IR<'ctx>,
        _: mira_typeck::TypeCtx<'ctx>,
    ) {
        if let TypedExpression::AttachVtable(_, _, _, (ty, traits)) = expr {
            self.vtables.insert((*ty, traits.clone()));
        }
    }

    fn visit_lit(&mut self, lit: &TypedLiteral<'ctx>, _: &IR<'ctx>, _: mira_typeck::TypeCtx<'ctx>) {
        if let &TypedLiteral::String(s) = lit {
            self.strings.insert(s);
        }
    }
}

#[allow(clippy::type_complexity)]
fn collect_data<'ctx>(ctx: &TypeckCtx<'ctx>) -> StringVtableCollector<'ctx> {
    let mut collector = StringVtableCollector::default();

    let function_reader = ctx.functions.read();
    let ext_function_reader = ctx.external_functions.read();
    let statics_reader = ctx.statics.read();

    for (_, body) in function_reader.iter() {
        body.visit(&mut collector, ctx.ctx);
    }
    for (_, body) in ext_function_reader.iter() {
        if let Some(body) = body {
            body.visit(&mut collector, ctx.ctx);
        }
    }
    for static_value in statics_reader.iter() {
        collector.visit_lit_standalone(&static_value.value);
    }

    collector
}
