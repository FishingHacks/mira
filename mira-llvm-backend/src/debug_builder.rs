use std::{
    cell::RefCell,
    collections::HashMap,
    hash::{DefaultHasher, Hash, Hasher},
    path::Path,
};

use inkwell::{
    AddressSpace,
    basic_block::BasicBlock,
    context::Context,
    debug_info::{
        AsDIScope, DIFile, DIFlags, DIFlagsConstants, DILexicalBlock, DILocation, DINamespace,
        DIScope, DISubprogram, DIType, DWARFEmissionKind, DWARFSourceLanguage, DebugInfoBuilder,
        debug_metadata_version,
    },
    module::{FlagBehavior, Module},
    values::PointerValue,
};
use mira_common::index::IndexMap;
use mira_parser::module::{ExternalFunctionId, ModuleId, StructId};
use mira_spans::{Span, interner::Symbol};
use mira_typeck::{Ty, TyKind, TypeCtx, TypeckCtx, TypedStruct, default_types};

use crate::mangling::{ANON_FN_NAME, mangle_external_function};
use crate::{DefaultTypes, get_mira_version};

#[allow(non_upper_case_globals)]
#[allow(non_snake_case)]
pub(crate) mod BasicTypeEncoding {
    pub(crate) const Boolean: u32 = 2;
    pub(crate) const Float: u32 = 4;
    pub(crate) const Signed: u32 = 5;
    pub(crate) const Unsigned: u32 = 7;
}

pub(crate) struct DebugContext<'ctx, 'arena> {
    ctx: TypeCtx<'arena>,
    pub(super) builder: DebugInfoBuilder<'ctx>,
    global_scope: DIScope<'ctx>,
    root_file: DIFile<'ctx>,
    pub(super) modules: IndexMap<ModuleId, (DINamespace<'ctx>, DIFile<'ctx>)>,
    default_types: DefaultTypes<'ctx>,
    type_store: RefCell<HashMap<Ty<'arena>, DIType<'ctx>>>,
    context: &'ctx Context,
    pub(super) ext_funcs: IndexMap<ExternalFunctionId, DISubprogram<'ctx>>,
    optimizations: bool,
}

impl<'ctx, 'arena> DebugContext<'ctx, 'arena> {
    pub(crate) fn location(&self, scope: DIScope<'ctx>, span: Span<'arena>) -> DILocation<'ctx> {
        let (line, column) = span.with_source_file(self.ctx.source_map()).lookup_pos();
        self.builder
            .create_debug_location(self.context, line, column, scope, None)
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn declare_param(
        &mut self,
        ptr: PointerValue<'ctx>,
        scope: DIScope<'ctx>,
        span: Span<'arena>,
        ty: Ty<'arena>,
        name: Symbol<'arena>,
        bb: BasicBlock<'ctx>,
        module: ModuleId,
        structs: &IndexMap<StructId, TypedStruct<'arena>>,
        arg: u32,
    ) {
        let ty = self.get_type(ty, structs);
        let info = self.builder.create_parameter_variable(
            scope,
            &name,
            arg,
            self.modules[module].1,
            span.with_source_file(self.ctx.source_map()).lookup_pos().0,
            ty,
            true,
            DIFlags::ZERO,
        );
        self.builder
            .insert_declare_at_end(ptr, Some(info), None, self.location(scope, span), bb)
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn declare_variable(
        &mut self,
        ptr: PointerValue<'ctx>,
        scope: DIScope<'ctx>,
        span: Span<'arena>,
        ty: Ty<'arena>,
        name: Symbol<'arena>,
        bb: BasicBlock<'ctx>,
        module: ModuleId,
        structs: &IndexMap<StructId, TypedStruct<'arena>>,
    ) {
        let alignment = ty.alignment(
            (self.default_types.isize.get_bit_width() / 8) as u64,
            structs,
        ) * 8;
        let ty = self.get_type(ty, structs);
        let info = self.builder.create_auto_variable(
            scope,
            &name,
            self.modules[module].1,
            span.with_source_file(self.ctx.source_map()).lookup_pos().0,
            ty,
            true,
            DIFlags::ZERO,
            alignment,
        );
        self.builder
            .insert_declare_at_end(ptr, Some(info), None, self.location(scope, span), bb)
    }

    pub(crate) fn new_block(
        &self,
        parent_scope: DIScope<'ctx>,
        span: Span<'arena>,
        module: ModuleId,
    ) -> DILexicalBlock<'ctx> {
        let (line, column) = span.with_source_file(self.ctx.source_map()).lookup_pos();
        self.builder
            .create_lexical_block(parent_scope, self.modules[module].1, line, column)
    }

    pub(crate) fn new(
        context: &'ctx Context,
        module: &Module<'ctx>,
        default_types: DefaultTypes<'ctx>,
        tc_ctx: &TypeckCtx<'arena>,
        root_path: &Path,
        optimizations: bool,
        ctx: TypeCtx<'arena>,
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
            &format!("clang LLVM ({})", get_mira_version()),
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
        let ext_func_reader = tc_ctx.external_functions.read();
        let root_file = builder.create_file(&root_filename, &root_directory);
        let mut me = Self {
            builder,
            modules: IndexMap::with_capacity(module_reader.len()),
            default_types,
            global_scope: compile_unit.as_debug_info_scope(),
            type_store: RefCell::new(HashMap::new()),
            context,
            root_file,
            ext_funcs: IndexMap::with_capacity(ext_func_reader.len()),
            ctx,
            optimizations,
        };
        for (key, module) in module_reader.entries() {
            let file = if *module.file.path == *root_path {
                root_file
            } else {
                me.get_file(&module.file.path)
            };
            let namespace = me.builder.create_namespace(
                file.as_debug_info_scope(),
                &module
                    .file
                    .path
                    .file_name()
                    .map(|v| v.to_string_lossy())
                    .unwrap_or("".into()),
                false,
            );
            me.modules.insert(key, (namespace, file));
        }

        for (key, func) in ext_func_reader.entries() {
            let mangled_name = mangle_external_function(tc_ctx, key);
            let name = func.0.name.as_deref().unwrap_or(ANON_FN_NAME);
            let line = func
                .0
                .span
                .with_source_file(me.ctx.source_map())
                .lookup_pos()
                .0;
            let subprogram = me.make_subprogram(
                func.0.module_id,
                func.0.return_type,
                func.0.arguments.iter().map(|&(_, ty)| ty),
                &struct_reader,
                name,
                &mangled_name,
                line,
                func.1.is_some(),
            );
            me.ext_funcs.insert(key, subprogram);
        }
        me
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn make_subprogram(
        &self,
        module: ModuleId,
        return_ty: Ty<'arena>,
        args: impl Iterator<Item = Ty<'arena>>,
        structs: &IndexMap<StructId, TypedStruct<'arena>>,
        name: &str,
        mangled_name: &str,
        line: u32,
        is_def: bool,
    ) -> DISubprogram<'ctx> {
        let (namespace, file) = self.modules[module];
        let scope = namespace.as_debug_info_scope();

        let flags = if return_ty == default_types::never {
            DIFlags::NO_RETURN
        } else {
            DIFlags::ZERO
        };
        let return_ty = if return_ty.is_voidlike() {
            None
        } else {
            Some(self.get_type(return_ty, structs))
        };
        let param_tys = args.map(|v| self.get_type(v, structs)).collect::<Vec<_>>();

        let subroutine_ty = self
            .builder
            .create_subroutine_type(file, return_ty, &param_tys, flags);
        self.builder.create_function(
            scope,
            name,
            Some(mangled_name),
            file,
            line,
            subroutine_ty,
            false,
            is_def,
            line,
            flags,
            self.optimizations,
        )
    }

    pub(crate) fn get_type(
        &self,
        ty: Ty<'arena>,
        structs: &IndexMap<StructId, TypedStruct<'arena>>,
    ) -> DIType<'ctx> {
        if let Some(v) = self.type_store.borrow().get(&ty) {
            return *v;
        }

        let di_typ = 'out: {
            let name = format!("{ty}");
            match **ty {
                TyKind::PrimitiveI8
                | TyKind::PrimitiveI16
                | TyKind::PrimitiveI32
                | TyKind::PrimitiveI64
                | TyKind::PrimitiveISize => {
                    break 'out self
                        .builder
                        .create_basic_type(
                            &name,
                            match **ty {
                                TyKind::PrimitiveI8 => 8,
                                TyKind::PrimitiveI16 => 16,
                                TyKind::PrimitiveI32 => 32,
                                TyKind::PrimitiveI64 => 64,
                                TyKind::PrimitiveISize => self.default_types.isize.get_bit_width(),
                                _ => unreachable!(),
                            } as u64,
                            BasicTypeEncoding::Signed,
                            DIFlags::PUBLIC,
                        )
                        .unwrap()
                        .as_type();
                }
                TyKind::PrimitiveU8
                | TyKind::PrimitiveU16
                | TyKind::PrimitiveU32
                | TyKind::PrimitiveU64
                | TyKind::PrimitiveUSize => {
                    break 'out self
                        .builder
                        .create_basic_type(
                            &name,
                            match **ty {
                                TyKind::PrimitiveU8 => 8,
                                TyKind::PrimitiveU16 => 16,
                                TyKind::PrimitiveU32 => 32,
                                TyKind::PrimitiveU64 => 64,
                                TyKind::PrimitiveUSize => self.default_types.isize.get_bit_width(),
                                _ => unreachable!(),
                            } as u64,
                            BasicTypeEncoding::Unsigned,
                            DIFlags::PUBLIC,
                        )
                        .unwrap()
                        .as_type();
                }
                TyKind::PrimitiveF32 => {
                    break 'out self
                        .builder
                        .create_basic_type("f32", 32, BasicTypeEncoding::Float, DIFlags::PUBLIC)
                        .unwrap()
                        .as_type();
                }
                TyKind::PrimitiveF64 => {
                    break 'out self
                        .builder
                        .create_basic_type("f64", 64, BasicTypeEncoding::Float, DIFlags::PUBLIC)
                        .unwrap()
                        .as_type();
                }
                TyKind::PrimitiveBool => {
                    break 'out self
                        .builder
                        .create_basic_type("bool", 8, BasicTypeEncoding::Boolean, DIFlags::PUBLIC)
                        .unwrap()
                        .as_type();
                }
                TyKind::PrimitiveVoid | TyKind::PrimitiveNever => {
                    break 'out self
                        .builder
                        .create_basic_type("void", 0, BasicTypeEncoding::Unsigned, DIFlags::PUBLIC)
                        .unwrap()
                        .as_type();
                }
                _ => (),
            }
            // thin pointer
            if ty.has_refs() && ty.is_thin_ptr() {
                let base_typ = self.get_type(ty.deref().unwrap(), structs);
                let ty = self.builder.create_pointer_type(
                    &name,
                    base_typ,
                    self.default_types.isize.get_bit_width() as u64,
                    self.default_types.isize.get_bit_width(),
                    AddressSpace::default(),
                );
                break 'out ty.as_type();
            // fat pointer
            } else if ty.has_refs() {
                // &&_ should always be a thin pointer
                assert_eq!(ty.refcount(), 1);
                let (metadata_type, pointer_type) =
                    match **ty.deref().expect("dereferencing &_ should never fail") {
                        TyKind::PrimitiveSelf | TyKind::Generic { .. } => {
                            unreachable!("generics and self should be resolved by now")
                        }
                        TyKind::DynType { .. } => (
                            self.get_type(default_types::void_ref, structs),
                            self.get_type(default_types::void_ref, structs),
                        ),
                        TyKind::UnsizedArray(ty) => (
                            self.get_type(default_types::usize, structs),
                            self.get_type(ty.take_ref(self.ctx), structs),
                        ),
                        TyKind::PrimitiveStr => (
                            self.get_type(default_types::usize, structs),
                            self.get_type(default_types::u8_ref, structs),
                        ),
                        _ => unreachable!("Type {ty} is not a fat pointer"),
                    };
                let isize_bits = self.default_types.isize.get_bit_width();
                let pointer_member_type = self.builder.create_member_type(
                    self.global_scope,
                    "ptr",
                    self.root_file,
                    0,
                    isize_bits as u64,
                    isize_bits,
                    0,
                    DIFlags::ZERO,
                    pointer_type,
                );
                let metadata_member_type = self.builder.create_member_type(
                    self.global_scope,
                    "metadata",
                    self.root_file,
                    0,
                    isize_bits as u64,
                    isize_bits,
                    isize_bits as u64,
                    DIFlags::ZERO,
                    metadata_type,
                );
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
                        &[
                            pointer_member_type.as_type(),
                            metadata_member_type.as_type(),
                        ],
                        DWARFSourceLanguage::C as u32,
                        None,
                        &format!("{:x}", calculate_hash(&ty)),
                    )
                    .as_type();
            }

            match &**ty {
                TyKind::Generic { .. } | TyKind::PrimitiveSelf => {
                    unreachable!("generics and self should be resolved by now")
                }
                TyKind::PrimitiveStr | TyKind::UnsizedArray { .. } | TyKind::DynType { .. } => {
                    unreachable!("cannot turn unsized type into a dwarf type")
                }
                TyKind::Struct { struct_id, .. } => {
                    let tmp_ty =
                        unsafe { self.builder.create_placeholder_derived_type(self.context) };
                    self.type_store.borrow_mut().insert(ty, tmp_ty.as_type());

                    let structure = &structs[*struct_id];
                    let elements = &structure.elements;
                    let ptr_size = (self.default_types.isize.get_bit_width() / 8) as u64;
                    let (size, alignment) = ty.size_and_alignment(ptr_size, structs);
                    let line = structure
                        .span
                        .with_source_file(self.ctx.source_map())
                        .lookup_pos()
                        .0;
                    let fields = elements
                        .iter()
                        .enumerate()
                        .map(|(i, (name, v, _))| {
                            let offset = ty.struct_offset(ptr_size, structs, i);
                            let (size, alignment) = v.size_and_alignment(ptr_size, structs);
                            let ty = self.get_type(*v, structs);
                            self.builder
                                .create_member_type(
                                    self.modules[structure.module_id].0.as_debug_info_scope(),
                                    name,
                                    self.modules[structure.module_id].1,
                                    line,
                                    size * 8,
                                    alignment * 8,
                                    offset * 8,
                                    DIFlags::ZERO,
                                    ty,
                                )
                                .as_type()
                        })
                        .collect::<Vec<_>>();
                    let debug_ty = self.builder.create_struct_type(
                        self.modules[structure.module_id].0.as_debug_info_scope(),
                        &name,
                        self.modules[structure.module_id].1,
                        line,
                        size * 8,
                        alignment * 8,
                        DIFlags::ZERO,
                        None,
                        &fields,
                        0,
                        None,
                        &format!("{:x}", calculate_hash(&ty)),
                    );
                    unsafe {
                        inkwell::llvm_sys::debuginfo::LLVMMetadataReplaceAllUsesWith(
                            tmp_ty.as_mut_ptr(),
                            debug_ty.as_mut_ptr(),
                        );
                    }
                    debug_ty.as_type()
                }
                TyKind::SizedArray {
                    ty: child,
                    number_elements,
                    ..
                } => {
                    let (size, alignment) = ty.size_and_alignment(
                        (self.default_types.isize.get_bit_width() / 8) as u64,
                        structs,
                    );
                    let inner_ty = self.get_type(*child, structs);
                    self.builder
                        .create_array_type(
                            inner_ty,
                            size * 8,
                            alignment * 8,
                            #[allow(clippy::single_range_in_vec_init)]
                            &[0..*number_elements as i64],
                        )
                        .as_type()
                }
                TyKind::Tuple(elements) => {
                    let (size, alignment) = ty.size_and_alignment(
                        (self.default_types.isize.get_bit_width() / 8) as u64,
                        structs,
                    );
                    let fields = elements
                        .iter()
                        .map(|v| self.get_type(*v, structs))
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
                            &format!("{:x}", calculate_hash(&ty)),
                        )
                        .as_type()
                }
                TyKind::Function(..) => {
                    let base_type = self.get_type(default_types::void, structs);
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
            .borrow_mut()
            .entry(ty)
            .insert_entry(di_typ)
            .get()
    }

    pub(crate) fn get_file(&self, file: &Path) -> DIFile<'ctx> {
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
