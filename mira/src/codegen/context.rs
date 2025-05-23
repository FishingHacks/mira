use core::panic;
use std::{
    collections::{HashMap, HashSet},
    io::Write,
    path::Path,
    sync::Arc,
};

use super::{
    debug_builder::DebugContext,
    error::CodegenError,
    intrinsics::LLVMIntrinsics,
    mangling::{mangle_function, mangle_static, mangle_string, mangle_struct},
};
use inkwell::{
    attributes::{Attribute, AttributeLoc},
    builder::{Builder, BuilderError},
    context::Context,
    debug_info::AsDIScope,
    llvm_sys::LLVMCallConv,
    module::{Linkage, Module},
    passes::PassBuilderOptions,
    support::LLVMString,
    targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target as LLVMTarget, TargetMachine,
        TargetTriple,
    },
    types::{BasicType, FloatType, IntType, PointerType, StructType},
    values::{FunctionValue, GlobalValue},
    AddressSpace, OptimizationLevel,
};

use crate::{
    globals::GlobalStr,
    std_annotations::{
        alias::ExternAliasAnnotation, callconv::CallConvAnnotation, ext_vararg::ExternVarArg,
        noinline::Noinline, section::SectionAnnotation,
    },
    store::{AssociatedStore, StoreKey},
    target::{Target, NATIVE_TARGET},
    typechecking::{
        expression::{TypecheckedExpression, TypedLiteral},
        intrinsics::IntrinsicAnnotation,
        typechecking::ScopeTypeMetadata,
        Type, TypecheckingContext, TypedExternalFunction, TypedFunction, TypedStatic, TypedStruct,
        TypedTrait,
    },
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

pub type FunctionsStore<'ctx> = AssociatedStore<FunctionValue<'ctx>, TypedFunction>;
pub type ExternalFunctionsStore<'ctx> = AssociatedStore<FunctionValue<'ctx>, TypedExternalFunction>;
pub type StructsStore<'ctx> = AssociatedStore<StructType<'ctx>, TypedStruct>;
pub type StaticsStore<'ctx> = AssociatedStore<GlobalValue<'ctx>, TypedStatic>;

pub struct CodegenContext<'ctx> {
    pub(super) tc_ctx: Arc<TypecheckingContext>,
    pub(super) builder: Builder<'ctx>,
    pub(super) context: &'ctx Context,
    pub(super) default_types: DefaultTypes<'ctx>,
    pub(super) retaddr: FunctionValue<'ctx>,
    pub module: Module<'ctx>,
    pub machine: TargetMachine,
    pub triple: TargetTriple,
    pub(super) functions: FunctionsStore<'ctx>,
    pub(super) external_functions: ExternalFunctionsStore<'ctx>,
    pub(super) structs: StructsStore<'ctx>,
    pub(super) statics: StaticsStore<'ctx>,
    pub(super) string_map: HashMap<GlobalStr, GlobalValue<'ctx>>,
    pub(super) debug_ctx: DebugContext<'ctx>,
    pub(super) intrinsics: LLVMIntrinsics,
    pub(super) vtables: HashMap<(Type, Vec<StoreKey<TypedTrait>>), GlobalValue<'ctx>>,
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

impl<'a> CodegenContext<'a> {
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

    pub fn write_bitcode(&self, writer: &mut dyn Write) -> std::io::Result<()> {
        let buf = self.module.write_bitcode_to_memory();
        writer.write_all(buf.as_slice())
    }

    pub fn write_ir(&self, writer: &mut dyn Write) -> std::io::Result<()> {
        let string = self.module.print_to_string();
        writer.write_all(string.to_bytes())
    }

    pub fn write_assembly(&self, writer: &mut dyn Write) -> Result<(), CodegenError> {
        let buffer = self
            .machine
            .write_to_memory_buffer(&self.module, FileType::Assembly)
            .map_err(CodegenError::LLVMNative)?;
        writer
            .write_all(buffer.as_slice())
            .map_err(CodegenError::IO)
    }

    pub fn write_object(&self, writer: &mut dyn Write) -> Result<(), CodegenError> {
        let buffer = self
            .machine
            .write_to_memory_buffer(&self.module, FileType::Object)
            .map_err(CodegenError::LLVMNative)?;
        writer
            .write_all(buffer.as_slice())
            .map_err(CodegenError::IO)
    }

    pub fn new(
        context: &'a Context,
        ctx: Arc<TypecheckingContext>,
        module: &str,
        path: Arc<Path>,
        config: CodegenConfig<'a>,
    ) -> Result<CodegenContext<'a>, CodegenError> {
        LLVMTarget::initialize_all(&InitializationConfig::default());
        let (triple, _) = config.target.to_llvm_triple();
        let llvm_target = LLVMTarget::from_triple(&triple)?;
        let Some(machine) = llvm_target.create_target_machine(
            &triple,
            config.target.arch.to_llvm_cpu(),
            config.cpu_features,
            OptimizationLevel::Aggressive,
            RelocMode::PIC,
            CodeModel::Default,
        ) else {
            return Err(CodegenError::UnknownTriple(triple));
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
        );

        let builder = context.create_builder();
        let (mut strings, mut vtables) = collect_data(&ctx);
        let mut string_map = HashMap::new();
        for strn in strings.drain() {
            let (constant, length, mangled_name) = strn.with(|v| {
                (
                    context.const_string(v.as_bytes(), false),
                    v.len(),
                    mangle_string(v),
                )
            });
            let global = module.add_global(
                default_types.i8.array_type(length as u32),
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
                .map(|(_, t)| match t {
                    Type::PrimitiveVoid(0) | Type::PrimitiveNever => {
                        default_types.i8.array_type(0).into()
                    }
                    t => t.to_llvm_basic_type(&default_types, &structs, context),
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
                .filter_map(|(_, t)| match t {
                    Type::PrimitiveVoid(0) | Type::PrimitiveNever => None,
                    t => Some(
                        t.to_llvm_basic_type(&default_types, &structs, context)
                            .into(),
                    ),
                })
                .collect::<Vec<_>>();

            let fn_typ = if matches!(
                contract.return_type,
                Type::PrimitiveNever | Type::PrimitiveVoid(0)
            ) {
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
                .filter_map(|(_, t)| match t {
                    Type::PrimitiveVoid(0) | Type::PrimitiveNever => None,
                    t => Some(
                        t.to_llvm_basic_type(&default_types, &structs, context)
                            .into(),
                    ),
                })
                .collect::<Vec<_>>();
            if !contract.return_type.is_primitive()
                || (contract.return_type.refcount() > 0 && !contract.return_type.is_thin_ptr())
                || !contract.return_type.is_sized()
                || contract.arguments.iter().any(|(_, v)| !v.is_sized())
            {
                unreachable!("typechecking should've validated the return type and arguments.");
            }
            let has_var_args = contract.annotations.has_annotation::<ExternVarArg>();

            let fn_typ = if matches!(
                contract.return_type,
                Type::PrimitiveNever | Type::PrimitiveVoid(0)
            ) {
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
                name
            } else {
                contract
                    .name
                    .as_ref()
                    .expect("external functions should always have a name")
            };

            let func = name.with(|name| module.add_function(name, fn_typ, None));
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
                section.0.with(|name| func.set_section(Some(name)));
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
                    .0
                    .to_llvm_basic_type(&default_types, &structs, context),
                None,
                &mangle_static(&ctx, key),
            );
            global.set_linkage(Linkage::Internal);
            global.set_initializer(&static_element.1.to_basic_value(
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
            let mut field_values = vec![default_types
                .isize
                .const_int(
                    ty.size_and_alignment(
                        (default_types.isize.get_bit_width() / 8) as u64,
                        &struct_reader,
                    )
                    .0,
                    false,
                )
                .into()];
            for trait_id in traits.iter() {
                match &ty {
                    Type::Struct { struct_id, .. } => {
                        for fn_id in struct_reader[*struct_id].trait_impl[trait_id]
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

        let retaddr = module.add_function(
            "llvm.returnaddress",
            default_types
                .ptr
                .fn_type(&[default_types.i32.into()], false),
            None,
        );
        Ok(CodegenContext {
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
            intrinsics: LLVMIntrinsics::init(),
            retaddr,
            vtables: vtable_map,
            config,
        })
    }

    pub fn compile_external_fn(
        &mut self,
        fn_id: StoreKey<TypedExternalFunction>,
        tc_scope: Vec<(Type, ScopeTypeMetadata)>,
    ) -> Result<(), BuilderError> {
        self.internal_compile_fn(fn_id.cast(), tc_scope, true)
    }
    pub fn compile_fn(
        &mut self,
        fn_id: StoreKey<TypedFunction>,
        tc_scope: Vec<(Type, ScopeTypeMetadata)>,
    ) -> Result<(), BuilderError> {
        self.internal_compile_fn(fn_id, tc_scope, false)
    }
    fn internal_compile_fn(
        &mut self,
        fn_id: StoreKey<TypedFunction>,
        tc_scope: Vec<(Type, ScopeTypeMetadata)>,
        is_external: bool,
    ) -> Result<(), BuilderError> {
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
            .append_basic_block(func, &format!("entry_{}", fn_id));
        let void_arg = self.default_types.empty_struct.const_zero().into();

        self.builder.position_at_end(body_basic_block);
        let scope = if is_external {
            self.debug_ctx.ext_funcs[fn_id.cast()].as_debug_info_scope()
        } else {
            self.debug_ctx.funcs[fn_id].as_debug_info_scope()
        };

        let mut function_ctx = self.make_function_codegen_context(tc_scope, func, body_basic_block);
        function_ctx.goto(body_basic_block);
        let function_reader = function_ctx.tc_ctx.functions.read();
        let ext_function_reader = function_ctx.tc_ctx.external_functions.read();
        let structs_reader = function_ctx.tc_ctx.structs.read();
        let contract = if is_external {
            &ext_function_reader[fn_id.cast()].0
        } else {
            &function_reader[fn_id].0
        };
        let body = if is_external {
            let Some(v) = &ext_function_reader[fn_id.cast()].1 else {
                unreachable!()
            };
            &**v
        } else {
            &*function_reader[fn_id].1
        };

        function_ctx
            .builder
            .set_current_debug_location(function_ctx.debug_ctx.location(scope, &contract.location));

        let mut param_idx = 0;
        for (idx, (name, arg)) in contract.arguments.iter().enumerate() {
            if matches!(arg, Type::PrimitiveVoid(0) | Type::PrimitiveNever) {
                function_ctx.push_value(idx, void_arg);
            } else {
                function_ctx.push_value(idx, func.get_nth_param(param_idx).unwrap());
                param_idx += 1;
            }
            let ptr = function_ctx.get_value_ptr(idx);
            function_ctx.debug_ctx.declare_param(
                ptr,
                scope,
                &contract.location,
                arg,
                name,
                function_ctx.current_block,
                contract.module_id,
                &structs_reader,
                idx as u32,
            );
        }

        drop(structs_reader);

        for expr in body {
            expr.codegen(&mut function_ctx, scope, contract.module_id)?;
        }

        drop(function_reader);
        drop(ext_function_reader);
        let Some(insert_block) = function_ctx.builder.get_insert_block() else {
            unreachable!("builder does not have a basic block to insert into even though it just built a function")
        };
        assert!(
            insert_block.get_terminator().is_some(),
            "basic block does not have a terminator"
        );
        Ok(())
    }

    pub fn write_object_file(&self, path: impl AsRef<Path>) -> Result<(), LLVMString> {
        self.machine
            .write_to_file(&self.module, FileType::Object, path.as_ref())
    }
}

#[allow(clippy::type_complexity)]
fn collect_data(
    ctx: &TypecheckingContext,
) -> (
    HashSet<GlobalStr>,
    HashSet<(Type, Vec<StoreKey<TypedTrait>>)>,
) {
    let function_reader = ctx.functions.read();
    let ext_function_reader = ctx.external_functions.read();
    let statics_reader = ctx.statics.read();
    let mut strings = HashSet::new();
    let mut vtables = HashSet::new();

    for (_, body) in function_reader.iter() {
        collect_strings_for_expressions(body, &mut strings, &mut vtables);
    }
    for (_, body) in ext_function_reader.iter() {
        if let Some(body) = body {
            collect_strings_for_expressions(body, &mut strings, &mut vtables);
        }
    }
    for static_value in statics_reader.iter() {
        collect_strings_for_typed_literal(&static_value.1, &mut strings);
    }

    (strings, vtables)
}

fn collect_strings_for_expressions(
    exprs: &[TypecheckedExpression],
    strings: &mut HashSet<GlobalStr>,
    vtables: &mut HashSet<(Type, Vec<StoreKey<TypedTrait>>)>,
) {
    for expr in exprs {
        match expr {
            TypecheckedExpression::Block(_, exprs, _) => {
                collect_strings_for_expressions(exprs, strings, vtables)
            }
            TypecheckedExpression::If {
                cond,
                if_block,
                else_block,
                ..
            } => {
                collect_strings_for_typed_literal(cond, strings);
                collect_strings_for_expressions(&if_block.0, strings, vtables);
                _ = else_block
                    .as_ref()
                    .map(|v| collect_strings_for_expressions(&v.0, strings, vtables));
            }
            TypecheckedExpression::While {
                cond_block,
                cond,
                body,
                ..
            } => {
                collect_strings_for_expressions(cond_block, strings, vtables);
                collect_strings_for_expressions(&body.0, strings, vtables);
                collect_strings_for_typed_literal(cond, strings);
            }
            TypecheckedExpression::Call(_, _, lhs, args) => {
                collect_strings_for_typed_literal(lhs, strings);
                for v in args {
                    collect_strings_for_typed_literal(v, strings);
                }
            }
            TypecheckedExpression::DirectCall(.., args, _)
            | TypecheckedExpression::DynCall(.., args, _)
            | TypecheckedExpression::DirectExternCall(.., args)
            | TypecheckedExpression::IntrinsicCall(.., args, _) => {
                for v in args {
                    collect_strings_for_typed_literal(v, strings);
                }
            }
            TypecheckedExpression::Range { lhs, rhs, .. }
            | TypecheckedExpression::StoreAssignment(_, lhs, rhs)
            | TypecheckedExpression::Add(.., lhs, rhs)
            | TypecheckedExpression::Sub(.., lhs, rhs)
            | TypecheckedExpression::Mul(.., lhs, rhs)
            | TypecheckedExpression::Div(.., lhs, rhs)
            | TypecheckedExpression::Mod(.., lhs, rhs)
            | TypecheckedExpression::BAnd(.., lhs, rhs)
            | TypecheckedExpression::BOr(.., lhs, rhs)
            | TypecheckedExpression::BXor(.., lhs, rhs)
            | TypecheckedExpression::GreaterThan(.., lhs, rhs)
            | TypecheckedExpression::LessThan(.., lhs, rhs)
            | TypecheckedExpression::LAnd(.., lhs, rhs)
            | TypecheckedExpression::LOr(.., lhs, rhs)
            | TypecheckedExpression::GreaterThanEq(.., lhs, rhs)
            | TypecheckedExpression::LessThanEq(.., lhs, rhs)
            | TypecheckedExpression::Eq(.., lhs, rhs)
            | TypecheckedExpression::Neq(.., lhs, rhs)
            | TypecheckedExpression::LShift(.., lhs, rhs)
            | TypecheckedExpression::RShift(.., lhs, rhs) => {
                collect_strings_for_typed_literal(lhs, strings);
                collect_strings_for_typed_literal(rhs, strings);
            }
            TypecheckedExpression::Return(_, lit)
            | TypecheckedExpression::Reference(.., lit)
            | TypecheckedExpression::Dereference(.., lit)
            | TypecheckedExpression::Offset(.., lit, _)
            | TypecheckedExpression::OffsetNonPointer(.., lit, _)
            | TypecheckedExpression::Literal(.., lit)
            | TypecheckedExpression::MakeUnsizedSlice(.., lit, _)
            | TypecheckedExpression::Pos(.., lit)
            | TypecheckedExpression::Neg(.., lit)
            | TypecheckedExpression::LNot(.., lit)
            | TypecheckedExpression::Bitcast(.., lit)
            | TypecheckedExpression::IntCast(.., lit)
            | TypecheckedExpression::PtrToInt(.., lit)
            | TypecheckedExpression::IntToPtr(.., lit)
            | TypecheckedExpression::Alias(.., lit)
            | TypecheckedExpression::StripMetadata(.., lit)
            | TypecheckedExpression::BNot(.., lit) => {
                collect_strings_for_typed_literal(lit, strings)
            }
            TypecheckedExpression::AttachVtable(_, _, lit, (ty, traits)) => {
                collect_strings_for_typed_literal(lit, strings);
                vtables.insert((ty.clone(), traits.clone()));
            }
            TypecheckedExpression::Empty(_)
            | TypecheckedExpression::Unreachable(_)
            | TypecheckedExpression::DeclareVariable(..)
            | TypecheckedExpression::Asm { .. }
            | TypecheckedExpression::None => (),
        }
    }
}
fn collect_strings_for_typed_literal(lit: &TypedLiteral, strings: &mut HashSet<GlobalStr>) {
    match lit {
        TypedLiteral::String(global_str) => _ = strings.insert(global_str.clone()),
        TypedLiteral::Array(_, vec) | TypedLiteral::Tuple(vec) | TypedLiteral::Struct(_, vec) => {
            vec.iter()
                .for_each(|v| collect_strings_for_typed_literal(v, strings))
        }
        _ => (),
    }
}
