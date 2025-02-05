use std::{
    collections::HashMap,
    hash::{DefaultHasher, Hash, Hasher},
    path::Path,
};

use crate::{
    codegen::debug_constants::BasicTypeEncoding,
    globals::GlobalStr,
    module::ModuleId,
    tokenizer::Location,
    typechecking::{Type, TypecheckingContext, TypedStruct},
};
use inkwell::{
    basic_block::BasicBlock,
    context::Context,
    debug_info::{
        debug_metadata_version, AsDIScope, DICompileUnit, DIFile, DIFlags, DIFlagsConstants,
        DILexicalBlock, DILocation, DINamespace, DIScope, DISubprogram, DIType, DWARFEmissionKind,
        DWARFSourceLanguage, DebugInfoBuilder,
    },
    module::{FlagBehavior, Module},
    values::PointerValue,
    AddressSpace,
};

use super::{
    context::DefaultTypes,
    mangling::{mangle_external_function, mangle_function, ANON_FN_NAME},
};

pub struct DebugContext<'ctx> {
    pub(super) builder: DebugInfoBuilder<'ctx>,
    global_scope: DIScope<'ctx>,
    root_file: DIFile<'ctx>,
    compile_unit: DICompileUnit<'ctx>,
    pub(super) modules: Vec<(DINamespace<'ctx>, DIFile<'ctx>)>,
    default_types: DefaultTypes<'ctx>,
    type_store: HashMap<Type, DIType<'ctx>>,
    context: &'ctx Context,
    pub(super) funcs: Vec<DISubprogram<'ctx>>,
    pub(super) ext_funcs: Vec<DISubprogram<'ctx>>,
}

impl<'ctx> DebugContext<'ctx> {
    pub fn location(&self, scope: DIScope<'ctx>, loc: &Location) -> DILocation<'ctx> {
        self.builder
            .create_debug_location(self.context, loc.line, loc.column, scope, None)
    }

    pub fn declare_param(
        &mut self,
        ptr: PointerValue<'ctx>,
        scope: DIScope<'ctx>,
        loc: &Location,
        typ: &Type,
        name: &GlobalStr,
        bb: BasicBlock<'ctx>,
        module: ModuleId,
        structs: &[TypedStruct],
        arg: u32,
    ) -> inkwell::values::InstructionValue<'ctx> {
        let ty = self.get_type(typ, structs);
        let info = name.with(|name| {
            self.builder.create_parameter_variable(
                scope,
                name,
                arg,
                self.modules[module].1,
                loc.line,
                ty,
                true,
                DIFlags::ZERO,
            )
        });
        self.builder
            .insert_declare_at_end(ptr, Some(info), None, self.location(scope, loc), bb)
    }

    pub fn declare_variable(
        &mut self,
        ptr: PointerValue<'ctx>,
        scope: DIScope<'ctx>,
        loc: &Location,
        typ: &Type,
        name: &GlobalStr,
        bb: BasicBlock<'ctx>,
        module: ModuleId,
        structs: &[TypedStruct],
    ) -> inkwell::values::InstructionValue<'ctx> {
        let alignment = typ.alignment(
            (self.default_types.isize.get_bit_width() / 8) as u64,
            structs,
        ) * 8;
        let ty = self.get_type(typ, structs);
        let info = name.with(|name| {
            self.builder.create_auto_variable(
                scope,
                name,
                self.modules[module].1,
                loc.line,
                ty,
                true,
                DIFlags::ZERO,
                alignment,
            )
        });
        self.builder
            .insert_declare_at_end(ptr, Some(info), None, self.location(scope, loc), bb)
    }

    pub fn new_block(
        &self,
        parent_scope: DIScope<'ctx>,
        loc: &Location,
        module: ModuleId,
    ) -> DILexicalBlock<'ctx> {
        self.builder.create_lexical_block(
            parent_scope,
            self.modules[module].1,
            loc.line,
            loc.column,
        )
    }

    pub fn new(
        context: &'ctx Context,
        module: &Module<'ctx>,
        default_types: DefaultTypes<'ctx>,
        tc_ctx: &TypecheckingContext,
        root_path: &Path,
        optimizations: bool,
    ) -> Self {
        module.add_basic_value_flag(
            "Debug Info Version",
            FlagBehavior::Warning,
            default_types
                .i32
                .const_int(debug_metadata_version().into(), false),
        );

        let root_filename = root_path
            .file_name()
            .map(|v| v.to_string_lossy())
            .unwrap_or("".into());
        let root_directory = root_path
            .parent()
            .map(|v| v.to_string_lossy())
            .unwrap_or("".into());

        let (builder, compile_unit) = module.create_debug_info_builder(
            true,
            DWARFSourceLanguage::C,
            &root_filename,
            &root_directory,
            concat!("clang LLVM (mira version ", env!("CARGO_PKG_VERSION"), ")"),
            optimizations,
            "",
            0,
            "",
            DWARFEmissionKind::Full,
            0,
            false,
            false,
            "",
            "",
        );

        let module_reader = tc_ctx.modules.read();
        let struct_reader = tc_ctx.structs.read();
        let func_reader = tc_ctx.functions.read();
        let ext_func_reader = tc_ctx.external_functions.read();
        let root_file = builder.create_file(&root_filename, &root_directory);
        let mut me = Self {
            builder,
            compile_unit,
            modules: Vec::with_capacity(module_reader.len()),
            default_types,
            global_scope: compile_unit.as_debug_info_scope(),
            type_store: HashMap::new(),
            context,
            root_file,
            funcs: Vec::with_capacity(func_reader.len()),
            ext_funcs: Vec::with_capacity(ext_func_reader.len()),
        };
        for module in module_reader.iter() {
            let file = if *module.path == *root_path {
                root_file
            } else {
                me.get_file(&module.path)
            };
            let namespace = me.builder.create_namespace(
                file.as_debug_info_scope(),
                &module
                    .path
                    .file_name()
                    .map(|v| v.to_string_lossy())
                    .unwrap_or("".into()),
                false,
            );
            me.modules.push((namespace, file));
        }
        for (id, func) in func_reader.iter().enumerate() {
            let return_ty = (!matches!(
                func.0.return_type,
                Type::PrimitiveVoid(0) | Type::PrimitiveNever
            ))
            .then(|| me.get_type(&func.0.return_type, &struct_reader));
            let args = func
                .0
                .arguments
                .iter()
                .map(|(_, ty)| me.get_type(ty, &struct_reader))
                .collect::<Vec<_>>();
            let module = &me.modules[func.0.module_id];
            let flags = matches!(func.0.return_type, Type::PrimitiveNever)
                .then_some(DIFlags::NO_RETURN)
                .unwrap_or_default();
            let fn_ty = me
                .builder
                .create_subroutine_type(module.1, return_ty, &args, flags);

            let mangled_name = mangle_function(tc_ctx, id);
            let subprogram = func
                .0
                .name
                .clone()
                .unwrap_or(GlobalStr::new(ANON_FN_NAME))
                .with(|name| {
                    me.builder.create_function(
                        module.0.as_debug_info_scope(),
                        name,
                        Some(&mangled_name),
                        module.1,
                        func.0.location.line,
                        fn_ty,
                        true,
                        true,
                        func.0.location.line,
                        flags,
                        optimizations,
                    )
                });
            me.funcs.push(subprogram);
        }
        for (id, func) in ext_func_reader.iter().enumerate() {
            let return_ty = (!matches!(
                func.0.return_type,
                Type::PrimitiveVoid(0) | Type::PrimitiveNever
            ))
            .then(|| me.get_type(&func.0.return_type, &struct_reader));
            let args = func
                .0
                .arguments
                .iter()
                .map(|(_, ty)| me.get_type(ty, &struct_reader))
                .collect::<Vec<_>>();
            let module = &me.modules[func.0.module_id];
            let flags = matches!(func.0.return_type, Type::PrimitiveNever)
                .then_some(DIFlags::NO_RETURN)
                .unwrap_or_default();
            let fn_ty = me
                .builder
                .create_subroutine_type(module.1, return_ty, &args, flags);

            let mangled_name = mangle_external_function(tc_ctx, id);
            let subprogram = func
                .0
                .name
                .clone()
                .unwrap_or(GlobalStr::new(ANON_FN_NAME))
                .with(|name: &str| {
                    me.builder.create_function(
                        module.0.as_debug_info_scope(),
                        name,
                        Some(&mangled_name),
                        module.1,
                        func.0.location.line,
                        fn_ty,
                        false,
                        func.1.is_some(),
                        func.0.location.line,
                        flags,
                        optimizations,
                    )
                });
            me.ext_funcs.push(subprogram);
        }
        me
    }

    pub fn get_type(&mut self, typ: &Type, structs: &[TypedStruct]) -> DIType<'ctx> {
        if let Some(v) = self.type_store.get(typ) {
            return *v;
        }

        let di_typ = 'out: {
            let name = format!("{typ}");
            match typ {
                Type::PrimitiveI8(0)
                | Type::PrimitiveI16(0)
                | Type::PrimitiveI32(0)
                | Type::PrimitiveI64(0)
                | Type::PrimitiveISize(0) => {
                    break 'out self
                        .builder
                        .create_basic_type(
                            &name,
                            match typ {
                                Type::PrimitiveI8(0) => 8,
                                Type::PrimitiveI16(0) => 16,
                                Type::PrimitiveI32(0) => 32,
                                Type::PrimitiveI64(0) => 64,
                                Type::PrimitiveISize(0) => self.default_types.isize.get_bit_width(),
                                _ => unreachable!(),
                            } as u64,
                            BasicTypeEncoding::Signed,
                            DIFlags::PUBLIC,
                        )
                        .unwrap()
                        .as_type()
                }
                Type::PrimitiveU8(0)
                | Type::PrimitiveU16(0)
                | Type::PrimitiveU32(0)
                | Type::PrimitiveU64(0)
                | Type::PrimitiveUSize(0) => {
                    break 'out self
                        .builder
                        .create_basic_type(
                            &name,
                            match typ {
                                Type::PrimitiveU8(0) => 8,
                                Type::PrimitiveU16(0) => 16,
                                Type::PrimitiveU32(0) => 32,
                                Type::PrimitiveU64(0) => 64,
                                Type::PrimitiveUSize(0) => self.default_types.isize.get_bit_width(),
                                _ => unreachable!(),
                            } as u64,
                            BasicTypeEncoding::Unsigned,
                            DIFlags::PUBLIC,
                        )
                        .unwrap()
                        .as_type()
                }
                Type::PrimitiveF32(0) => {
                    break 'out self
                        .builder
                        .create_basic_type("f32", 0, BasicTypeEncoding::Float, DIFlags::PUBLIC)
                        .unwrap()
                        .as_type()
                }
                Type::PrimitiveF64(0) => {
                    break 'out self
                        .builder
                        .create_basic_type("f64", 0, BasicTypeEncoding::Float, DIFlags::PUBLIC)
                        .unwrap()
                        .as_type()
                }
                Type::PrimitiveBool(0) => {
                    break 'out self
                        .builder
                        .create_basic_type("bool", 0, BasicTypeEncoding::Boolean, DIFlags::PUBLIC)
                        .unwrap()
                        .as_type()
                }
                Type::PrimitiveVoid(0) | Type::PrimitiveNever => {
                    break 'out self
                        .builder
                        .create_basic_type("void", 0, BasicTypeEncoding::Unsigned, DIFlags::PUBLIC)
                        .unwrap()
                        .as_type()
                }
                _ => (),
            }
            // thin pointer
            if typ.refcount() > 0 && typ.is_thin_ptr() {
                let base_typ = self.get_type(&typ.clone().deref().unwrap(), structs);
                let typ = self.builder.create_pointer_type(
                    &name,
                    base_typ,
                    self.default_types.isize.get_bit_width() as u64,
                    self.default_types.isize.get_bit_width(),
                    AddressSpace::default(),
                );
                break 'out typ.as_type();
            // fat pointer
            } else if typ.refcount() > 0 {
                let (metadata_type, pointer_type) = match typ {
                    Type::Trait { .. } | Type::PrimitiveSelf(_) | Type::Generic(..) => {
                        unreachable!("generics and self should be resolved by now")
                    }
                    Type::DynType { .. } => (
                        self.get_type(&Type::PrimitiveVoid(1), structs),
                        self.get_type(&Type::PrimitiveVoid(1), structs),
                    ),
                    Type::UnsizedArray { typ, .. } => (
                        self.get_type(&Type::PrimitiveUSize(0), structs),
                        self.get_type(&typ.clone().take_ref(), structs),
                    ),
                    Type::PrimitiveStr(_) => (
                        self.get_type(&Type::PrimitiveUSize(0), structs),
                        self.get_type(&Type::PrimitiveU8(1), structs),
                    ),
                    v => unreachable!("Type {v} is not a fat pointer"),
                };
                break 'out self
                    .builder
                    .create_struct_type(
                        self.global_scope,
                        &name,
                        self.root_file,
                        0,
                        self.default_types.isize.get_bit_width() as u64 * 2,
                        self.default_types.isize.get_bit_width(),
                        0,
                        None,
                        &[pointer_type, metadata_type],
                        DWARFSourceLanguage::C as u32,
                        None,
                        &format!("{:x}", calculate_hash(typ)),
                    )
                    .as_type();
            }

            match typ {
                Type::Trait { .. } | Type::Generic(..) | Type::PrimitiveSelf(..) => {
                    unreachable!("generics and self should be resolved by now")
                }
                Type::PrimitiveStr(_) | Type::UnsizedArray { .. } | Type::DynType { .. } => {
                    unreachable!("cannot turn unsized type into a dwarf type")
                }
                Type::Struct { struct_id, .. } => {
                    let structure = &structs[*struct_id];
                    let elements = &structure.elements;
                    let (size, alignment) = typ.size_and_alignment(
                        (self.default_types.isize.get_bit_width() / 8) as u64,
                        structs,
                    );
                    let fields = elements
                        .iter()
                        .map(|(_, v)| self.get_type(v, structs))
                        .collect::<Vec<_>>();
                    self.builder
                        .create_struct_type(
                            self.modules[structure.module_id].0.as_debug_info_scope(),
                            &name,
                            self.modules[structure.module_id].1,
                            structure.location.line,
                            size * 8,
                            alignment * 8,
                            DIFlags::ZERO,
                            None,
                            &fields,
                            0,
                            None,
                            &format!("{:x}", calculate_hash(typ)),
                        )
                        .as_type()
                }
                Type::SizedArray {
                    typ: child,
                    number_elements,
                    ..
                } => {
                    let (size, alignment) = typ.size_and_alignment(
                        (self.default_types.isize.get_bit_width() / 8) as u64,
                        structs,
                    );
                    let inner_ty = self.get_type(child, structs);
                    self.builder
                        .create_array_type(
                            inner_ty,
                            size * 8,
                            alignment * 8,
                            &[0..*number_elements as i64],
                        )
                        .as_type()
                }
                Type::Tuple { elements, .. } => {
                    let (size, alignment) = typ.size_and_alignment(
                        (self.default_types.isize.get_bit_width() / 8) as u64,
                        structs,
                    );
                    let fields = elements
                        .iter()
                        .map(|v| self.get_type(v, structs))
                        .collect::<Vec<_>>();
                    self.builder
                        .create_struct_type(
                            self.global_scope,
                            &name,
                            self.root_file,
                            0,
                            size * 8,
                            alignment * 8,
                            DIFlags::ZERO,
                            None,
                            &fields,
                            0,
                            None,
                            &format!("{:x}", calculate_hash(typ)),
                        )
                        .as_type()
                }
                Type::Function(..) => {
                    let base_type = self.get_type(&Type::PrimitiveVoid(0), structs);
                    self.builder
                        .create_pointer_type(
                            &name,
                            base_type,
                            self.default_types.isize.get_bit_width() as u64,
                            self.default_types.isize.get_bit_width(),
                            AddressSpace::default(),
                        )
                        .as_type()
                }
                _ => unreachable!(),
            }
        };
        *self
            .type_store
            .entry(typ.clone())
            .insert_entry(di_typ)
            .get()
    }

    pub fn get_file(&self, file: &Path) -> DIFile<'ctx> {
        let file_name = file
            .file_name()
            .map(|v| v.to_string_lossy())
            .unwrap_or_else(|| "".into());
        let directory = file
            .parent()
            .map(|v| v.to_string_lossy())
            .unwrap_or_else(|| "".into());
        self.builder.create_file(&file_name, &directory)
    }
}

fn calculate_hash<T: Hash>(v: &T) -> u64 {
    let mut hasher = DefaultHasher::new();
    v.hash(&mut hasher);
    hasher.finish()
}
