use mira_errors::ErrorEmitted;
use mira_macros::Display;
use parking_lot::RwLock;
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::{Hash, Hasher},
    ops::Deref,
    sync::{Arc, OnceLock},
};
use type_resolution::ResolvingState;

use ir::TypedLiteral;
use types::{resolve_primitive_type, with_refcount};

use crate::{error::TypecheckingErrorEmitterExt, ir::IR};
pub use lang_items::LangItems;
use mira_common::index::IndexMap;
use mira_context::DocComment;
use mira_parser::{
    TypeRef,
    annotations::Annotations,
    module::{
        ExternalFunctionId, FunctionContext, FunctionId, ModuleContext, ModuleId, ModuleScopeValue,
        StaticId, StructId, TraitId,
    },
};
use mira_spans::{
    ArenaList, Ident, SourceFile, Span,
    interner::{Symbol, symbols},
};

mod context;
mod lang_items;
mod monomorphisation;
pub use context::{GlobalContext, TypeCtx};
pub use monomorphisation::{Substitute, SubstitutionCtx};
mod error;
pub mod intrinsics;
pub mod ir;
pub mod ir_displayer;
pub mod queries;
mod type_resolution;
pub mod typechecking;
mod types;
pub use error::{FunctionList, TypecheckingError, TypecheckingErrorDiagnosticsExt};
pub use types::{
    EMPTY_TYLIST, FunctionType, Ty, TyKind, TyList, TypeInterner, TypeListInterner, default_types,
};

#[derive(Clone, Copy, Debug)]
pub enum CommonFunction {
    Normal(FunctionId),
    External(ExternalFunctionId),
}

impl CommonFunction {
    pub fn is_external(&self) -> bool {
        matches!(self, Self::External(_))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedGeneric<'arena> {
    pub name: Ident<'arena>,
    pub sized: bool,
    pub bounds: ArenaList<'arena, TraitId>,
}

#[derive(Debug)]
pub struct TypedFunctionContract<'arena> {
    pub name: Option<Ident<'arena>>,
    pub arguments: Vec<(Ident<'arena>, Ty<'arena>)>,
    pub return_type: Ty<'arena>,
    pub annotations: Annotations<'arena>,
    pub span: Span<'arena>,
    pub module_id: ModuleId,
    pub generics: Vec<TypedGeneric<'arena>>,
    pub comment: DocComment,
    pub context: FunctionContext,
}

/// TODO: Remove this?
impl Hash for TypedFunctionContract<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self.name {
            Some(v) => v.hash(state),
            None => "{{anon}}".hash(state),
        }
        self.module_id.hash(state);
        self.arguments.hash(state);
        self.return_type.hash(state);
        self.context.hash(state);
    }
}

#[derive(Debug)]
#[allow(clippy::type_complexity)]
pub struct TypedTrait<'arena> {
    pub name: Ident<'arena>,
    pub functions: Vec<(
        Ident<'arena>,
        Vec<(Ident<'arena>, Ty<'arena>)>,
        Ty<'arena>,
        Annotations<'arena>,
        Span<'arena>,
        DocComment,
    )>,
    pub span: Span<'arena>,
    pub module_id: ModuleId,
    pub id: TraitId,
    pub annotations: Annotations<'arena>,
    pub comment: DocComment,
}

#[derive(Debug)]
pub struct TypedStruct<'arena> {
    pub name: Ident<'arena>,
    pub elements: Vec<(Ident<'arena>, Ty<'arena>, DocComment)>,
    pub span: Span<'arena>,
    pub global_impl: HashMap<Ident<'arena>, FunctionId>,
    pub trait_impl: IndexMap<TraitId, Vec<FunctionId>>,
    pub annotations: Annotations<'arena>,
    pub module_id: ModuleId,
    pub id: StructId,
    pub generics: Vec<TypedGeneric<'arena>>,
    pub comment: DocComment,
}

impl Hash for TypedStruct<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.elements.hash(state);
        self.module_id.hash(state);
    }
}

#[derive(Debug, Clone)]
pub struct TypedStatic<'arena> {
    pub ty: Ty<'arena>,
    /// guaranteed to not be `Dynamic`, `Intrinsic` or `Static`
    pub value: TypedLiteral<'arena>,
    pub module_id: ModuleId,
    pub span: Span<'arena>,
    pub annotations: Annotations<'arena>,
    pub name: Ident<'arena>,
    pub comment: DocComment,
}

impl<'arena> TypedStatic<'arena> {
    pub fn new(
        ty: Ty<'arena>,
        literal: TypedLiteral<'arena>,
        module: ModuleId,
        span: Span<'arena>,
        annotations: Annotations<'arena>,
        name: Ident<'arena>,
        comment: DocComment,
    ) -> Self {
        Self {
            ty,
            value: literal,
            module_id: module,
            span,
            annotations,
            name,
            comment,
        }
    }
}

pub type TypedFunction<'arena> = (TypedFunctionContract<'arena>, IR<'arena>);
pub type TypedExternalFunction<'arena> = (TypedFunctionContract<'arena>, Option<IR<'arena>>);

#[allow(clippy::type_complexity)]
pub struct TypeckCtx<'arena> {
    pub modules: RwLock<IndexMap<ModuleId, TypedModule<'arena>>>,
    pub functions: RwLock<IndexMap<FunctionId, TypedFunction<'arena>>>,
    pub external_functions: RwLock<IndexMap<ExternalFunctionId, TypedExternalFunction<'arena>>>,
    pub statics: RwLock<IndexMap<StaticId, TypedStatic<'arena>>>,
    pub structs: RwLock<IndexMap<StructId, TypedStruct<'arena>>>,
    pub traits: RwLock<IndexMap<TraitId, TypedTrait<'arena>>>,
    pub lang_items: RwLock<LangItems<'arena>>,
    pub main_function: OnceLock<FunctionId>,
    pub ctx: TypeCtx<'arena>,
    pub dependencies: IndexMap<ModuleId, HashMap<Arc<str>, ModuleId>>,
}

#[derive(Debug)]
pub struct TypedModule<'ctx> {
    pub scope: HashMap<Ident<'ctx>, ResolvedValue<'ctx>>,
    pub exports: HashSet<Ident<'ctx>>,
    pub parent: Option<ModuleId>,
    pub root_module: ModuleId,
    pub module_id: ModuleId,
    pub name: Symbol<'ctx>,
    pub file: Arc<SourceFile>,
    pub assembly: Vec<(Span<'ctx>, String)>,
    pub comment: DocComment,
}

impl<'ctx> TypeckCtx<'ctx> {
    pub fn substitute<T: Deref<Target = [Ty<'ctx>]>, V: Substitute<'ctx>>(
        &self,
        tys: &T,
        v: V,
    ) -> V {
        v.substitute(&SubstitutionCtx::new(**self, tys))
    }

    pub fn validate_main_function(
        &self,
        main_pkg: ModuleId,
    ) -> Result<FunctionId, TypecheckingError<'ctx>> {
        let module = &self.modules.read()[main_pkg];
        let Some(&ResolvedValue::Function(main_fn, _)) =
            module.scope.get(&symbols::DefaultIdents::main)
        else {
            return Err(TypecheckingError::MainFuncNotFound(
                module.file.path.clone(),
            ));
        };
        let func = &self.functions.read()[main_fn].0;
        if !func.arguments.is_empty()
            || func.return_type != default_types::void
            || !func.generics.is_empty()
        {
            return Err(TypecheckingError::MainFuncWrongType {
                func_span: func.span,
            });
        }
        self.main_function
            .set(main_fn)
            .expect("validate_main_function called multiple times");
        Ok(main_fn)
    }

    pub fn new(ctx: TypeCtx<'ctx>, module_context: Arc<ModuleContext<'ctx>>) -> Arc<Self> {
        let traits_reader = module_context.traits.read();
        let structs_reader = module_context.structs.read();
        let statics_reader = module_context.statics.read();
        let functions_reader = module_context.functions.read();
        let external_functions_reader = module_context.external_functions.read();

        let mut traits = IndexMap::with_capacity(traits_reader.len());
        let mut structs = IndexMap::with_capacity(structs_reader.len());
        let mut statics = IndexMap::with_capacity(statics_reader.len());
        let mut functions = IndexMap::with_capacity(functions_reader.len());
        let mut external_functions = IndexMap::with_capacity(external_functions_reader.len());

        for (id, baked_struct) in structs_reader.entries() {
            structs.insert(
                id,
                TypedStruct {
                    name: Ident::EMPTY,
                    elements: Vec::new(),
                    span: structs_reader[id].span,
                    global_impl: HashMap::new(),
                    trait_impl: IndexMap::new(),
                    annotations: Annotations::default(),
                    module_id: baked_struct.module_id,
                    generics: Vec::new(),
                    id,
                    comment: DocComment::EMPTY,
                },
            );
        }

        for (id, static_value) in statics_reader.entries() {
            statics.insert(
                id,
                TypedStatic::new(
                    default_types::never,
                    TypedLiteral::Void,
                    static_value.module,
                    static_value.span,
                    Annotations::default(),
                    Ident::EMPTY,
                    DocComment::EMPTY,
                ),
            );
        }

        for (id, func) in functions_reader.entries() {
            functions.insert(
                id,
                (
                    TypedFunctionContract {
                        annotations: Annotations::default(),
                        name: None,
                        arguments: Vec::new(),
                        return_type: default_types::never,
                        span: func.contract.span,
                        module_id: func.parent_module,
                        generics: Vec::new(),
                        comment: DocComment::EMPTY,
                        context: func.ctx,
                    },
                    IR::new(std::iter::empty(), func.contract.span),
                ),
            );
        }

        for (id, ext_fn) in external_functions_reader.entries() {
            external_functions.insert(
                id,
                (
                    TypedFunctionContract {
                        annotations: Annotations::default(),
                        name: None,
                        arguments: Vec::new(),
                        return_type: default_types::never,
                        span: ext_fn.contract.span,
                        module_id: ext_fn.parent_module,
                        generics: Vec::new(),
                        context: FunctionContext::Freestanding,
                        comment: DocComment::EMPTY,
                    },
                    None,
                ),
            );
        }

        for (id, trait_value) in traits_reader.entries() {
            traits.insert(
                id,
                TypedTrait {
                    name: Ident::EMPTY,
                    functions: Vec::new(),
                    span: trait_value.span,
                    module_id: trait_value.module,
                    id,
                    annotations: Annotations::default(),
                    comment: DocComment::EMPTY,
                },
            );
        }

        let me = Arc::new(Self {
            ctx,
            dependencies: module_context.dependencies.clone(),
            structs: RwLock::new(structs),
            statics: RwLock::new(statics),
            functions: RwLock::new(functions),
            traits: RwLock::new(traits),
            external_functions: RwLock::new(external_functions),
            modules: Default::default(),
            lang_items: RwLock::new(LangItems::new(ctx)),
            main_function: OnceLock::new(),
        });

        let mut typechecked_module_writer = me.modules.write();
        let module_reader = module_context.modules.read();

        for (key, module) in module_reader.entries() {
            let scope = module
                .scope
                .iter()
                .map(|(&ident, &value)| (ident, value.into()))
                .collect();

            typechecked_module_writer.insert(
                key,
                TypedModule {
                    scope,
                    parent: module.parent,
                    root_module: module.package_root,
                    module_id: key,
                    name: module.name,
                    exports: module.exports.clone(),
                    file: module.file.clone(),
                    assembly: module.assembly.clone(),
                    comment: module.comment,
                },
            );
        }

        drop(module_reader);
        drop(typechecked_module_writer);

        me
    }

    pub fn resolve_imports(&self, context: Arc<ModuleContext<'ctx>>) -> Result<(), ErrorEmitted> {
        let mut typechecked_module_writer = self.modules.write();
        let module_reader = context.modules.read();
        let tracker = self.track_errors();
        for key in module_reader.keys() {
            for (name, (span, import)) in module_reader[key].imports.iter() {
                let res = resolve_import_simple(
                    &context,
                    self.ctx,
                    key,
                    import.iter(),
                    *span,
                    // a module doesn't have any generics in the context cuz it can't be generic
                    // lol
                    &[],
                    false,
                );
                match res {
                    Err(ErrorEmitted(..)) => {}
                    Ok(k) => {
                        typechecked_module_writer[key].scope.insert(*name, k);
                    }
                }
            }
        }
        self.errors_happened_res(tracker)
    }

    pub fn resolve_type(
        &self,
        module_id: ModuleId,
        ty: &TypeRef<'ctx>,
        generics: &[TypedGeneric<'ctx>],
    ) -> Result<Ty<'ctx>, ErrorEmitted> {
        if let Some(primitive) = resolve_primitive_type(self.ctx, ty) {
            return Ok(primitive);
        }

        match ty {
            TypeRef::DynReference {
                traits,
                num_references,
                span,
            } => {
                let mut trait_refs = Vec::with_capacity(traits.len());
                for trait_name in traits.iter() {
                    let v = self.resolve_import_simple(
                        module_id,
                        trait_name.iter(),
                        *span,
                        generics,
                        true,
                    )?;
                    let ResolvedValue::Trait(id) = v else {
                        return Err(self.emit_mismatching_scope_type(
                            *span,
                            ScopeKind::Trait,
                            v.into(),
                        ));
                    };
                    trait_refs.push((id, *trait_name.as_slice().last().unwrap()));
                }
                let trait_refs = ArenaList::new(self.arena(), &trait_refs);
                Ok(with_refcount(
                    self.ctx,
                    self.intern_ty(TyKind::DynType(trait_refs)),
                    *num_references,
                ))
            }
            TypeRef::Function {
                return_ty,
                args,
                num_references,
                ..
            } => {
                let return_type = self.resolve_type(module_id, return_ty, generics)?;
                let mut arguments = Vec::with_capacity(args.len());
                for arg in args {
                    arguments.push(self.resolve_type(module_id, arg, generics)?);
                }
                let arguments = self.intern_tylist(&arguments);
                let function_typ = FunctionType {
                    arguments,
                    return_type,
                };
                Ok(with_refcount(
                    self.ctx,
                    self.intern_ty(TyKind::Function(function_typ)),
                    *num_references,
                ))
            }
            TypeRef::Reference {
                num_references,
                type_name,
                span,
            } => {
                if type_name.entries.len() == 1
                    && type_name.entries[0].1.is_empty()
                    && let Some((id, generic)) = generics
                        .iter()
                        .enumerate()
                        .find(|(_, v)| v.name == type_name.entries[0].0)
                {
                    let ty = self.intern_ty(TyKind::Generic {
                        name: type_name.entries[0].0,
                        generic_id: id as u8,
                        sized: generic.sized,
                        bounds: generic.bounds,
                    });
                    return Ok(with_refcount(self.ctx, ty, *num_references));
                }

                match self.resolve_import(module_id, type_name.iter(), *span, generics, true)? {
                    ResolvedValue::Struct(struct_id, generics) => Ok(with_refcount(
                        self.ctx,
                        self.intern_ty(TyKind::Struct {
                            struct_id,
                            name: self.structs.read()[struct_id].name,
                            generics,
                        }),
                        *num_references,
                    )),
                    v => Err(self.emit_mismatching_scope_type(*span, ScopeKind::Type, v.into())),
                }
            }
            TypeRef::Void(..) | TypeRef::Never(_) => unreachable!(),
            TypeRef::UnsizedArray {
                num_references,
                child,
                span: _,
            } => Ok(with_refcount(
                self.ctx,
                self.intern_ty(TyKind::UnsizedArray(
                    self.resolve_type(module_id, child, generics)?,
                )),
                *num_references,
            )),
            TypeRef::SizedArray {
                num_references,
                child,
                number_elements,
                ..
            } => Ok(with_refcount(
                self.ctx,
                self.intern_ty(TyKind::SizedArray {
                    ty: self.resolve_type(module_id, child, generics)?,
                    number_elements: *number_elements,
                }),
                *num_references,
            )),
            TypeRef::Tuple {
                num_references,
                elements,
                ..
            } => {
                let mut typed_elements = Vec::with_capacity(elements.len());
                for elem in elements.iter() {
                    typed_elements.push(self.resolve_type(module_id, elem, generics)?);
                }
                let ty = self
                    .ctx
                    .intern_ty(TyKind::Tuple(self.intern_tylist(&typed_elements)));
                Ok(with_refcount(self.ctx, ty, *num_references))
            }
        }
    }

    /// returns if a recursive field was detected
    fn resolve_struct(
        &self,
        context: Arc<ModuleContext<'ctx>>,
        id: StructId,
        module_id: ModuleId,
        left: &mut IndexMap<StructId, ResolvingState>,
    ) -> Result<(), ErrorEmitted> {
        match left.get_mut(id) {
            None => return Ok(()),
            Some(ResolvingState::Working) => {
                unreachable!("tried resolving struct while resolving a struct")
            }
            Some(v @ ResolvingState::Pending) => *v = ResolvingState::Working,
        }
        let tracker = self.track_errors();

        let mut writer = context.structs.write();

        let global_impl = std::mem::take(&mut writer[id].global_impl);
        let annotations = std::mem::take(&mut writer[id].annotations);
        let elements = std::mem::take(&mut writer[id].elements);
        let mut generics = Vec::new();

        for generic in &writer[id].generics {
            let mut bounds = Vec::new();

            for (bound, span) in &generic.bounds {
                match resolve_import_simple(
                    &context,
                    self.ctx,
                    module_id,
                    bound.iter(),
                    *span,
                    // TODO: put the in-scope generics when adding struct generics.
                    &[],
                    true,
                ) {
                    Err(_) => {}
                    Ok(ResolvedValue::Trait(trait_id)) => bounds.push(trait_id),
                    Ok(_) => {
                        _ = self.emit_unbound_ident(
                            *span,
                            bound.as_slice()[bound.as_slice().len() - 1].symbol(),
                        );
                    }
                }
            }

            let bounds = ArenaList::new(self.arena(), &bounds);
            generics.push(TypedGeneric {
                name: generic.name,
                sized: generic.sized,
                bounds,
            });
        }

        let mut typed_struct = TypedStruct {
            name: writer[id].name,
            span: writer[id].span,
            elements: Vec::new(),
            global_impl,
            annotations,
            module_id,
            id,
            generics,
            trait_impl: IndexMap::new(),
            comment: writer[id].comment,
        };
        drop(writer);

        for element in elements {
            if let Ok(ty) = type_resolution_resolve_type(
                self.ctx,
                &element.1,
                &typed_struct.generics,
                module_id,
                &context,
            ) {
                typed_struct.elements.push((element.0, ty, element.2));
            }
        }
        self.structs.write()[id] = typed_struct;

        left.remove(id);
        self.errors_happened_res(tracker)
    }

    /// `with_generics`: If this is true, this function will error if generics are missing.
    /// This is wanted in case of uses (e.g. `type_name()`), but not wanted in the case of use
    /// statements.
    pub fn resolve_import_simple<'a>(
        &self,
        current_module: ModuleId,
        mut import: impl Iterator<Item = &'a Ident<'ctx>>,
        span: Span<'ctx>,
        generics_in_context: &[TypedGeneric<'ctx>],
        with_generics: bool,
    ) -> Result<ResolvedValue<'ctx>, ErrorEmitted>
    where
        'ctx: 'a,
    {
        let name = import
            .next()
            .expect("resolve_import called on an empty import");
        self.resolve_import_inner(
            current_module,
            #[allow(trivial_casts)]
            import.map(|v| (v, &[] as &[_])),
            (name, &[]),
            span,
            &mut HashSet::new(),
            generics_in_context,
            with_generics,
        )
    }

    /// `with_generics`: If this is true, this function will error if generics are missing.
    /// This is wanted in case of uses (e.g. `type_name()`), but not wanted in the case of use
    /// statements.
    fn resolve_import<'a>(
        &self,
        current_module: ModuleId,
        mut import: impl Iterator<Item = (&'a Ident<'ctx>, &'a [TypeRef<'ctx>])>,
        span: Span<'ctx>,
        generics_in_context: &[TypedGeneric<'ctx>],
        with_generics: bool,
    ) -> Result<ResolvedValue<'ctx>, ErrorEmitted>
    where
        'ctx: 'a,
    {
        let next = import
            .next()
            .expect("resolve_import called on an empty import");
        self.resolve_import_inner(
            current_module,
            import,
            next,
            span,
            &mut HashSet::new(),
            generics_in_context,
            with_generics,
        )
    }

    /// TODO: make this better
    #[allow(clippy::too_many_arguments)]
    fn resolve_import_inner<'a>(
        &self,
        current_module: ModuleId,
        mut import: impl Iterator<Item = (&'a Ident<'ctx>, &'a [TypeRef<'ctx>])>,
        (name, generics): (&'a Ident<'ctx>, &'a [TypeRef<'ctx>]),
        span: Span<'ctx>,
        already_included: &mut HashSet<(ModuleId, Symbol<'ctx>)>,
        generics_in_context: &[TypedGeneric<'ctx>],
        with_generics: bool,
    ) -> Result<ResolvedValue<'ctx>, ErrorEmitted>
    where
        'ctx: 'a,
    {
        if !already_included.insert((current_module, name.symbol())) {
            return Err(self.emit_cyclic_dependency(span));
        }

        let module_reader = self.modules.read();
        let cur_mod = &module_reader[current_module];
        let mut thing;
        if &**name == "pkg" {
            thing = ResolvedValue::Module(cur_mod.root_module);
        } else if &**name == "super" {
            let Some(parent) = cur_mod.parent else {
                return Err(self.emit_item_not_found(name.span(), name.symbol()));
            };
            thing = ResolvedValue::Module(parent);
        } else if let Some(&value) = cur_mod.scope.get(&name.symbol()) {
            thing = value;
        } else if let Some(&value) = cur_mod.scope.get(&name.symbol()) {
            thing = value;
        } else if let Some(package) = self.dependencies[cur_mod.root_module].get(&**name) {
            thing = ResolvedValue::Module(*package);
        } else {
            return Err(self.emit_item_not_found(span, name.symbol()));
        }

        thing.purge_generics();
        let mut generics = generics;

        loop {
            if with_generics {
                thing = self.fill_in_generics(
                    thing,
                    generics,
                    span,
                    current_module,
                    generics_in_context,
                )?;
            }
            let name;
            (name, generics) = match import.next() {
                Some(v) => v,
                None => return Ok(thing),
            };

            match thing {
                ResolvedValue::Module(module) => {
                    return self.resolve_import_inner(
                        module,
                        import,
                        (name, generics),
                        span,
                        already_included,
                        generics_in_context,
                        with_generics,
                    );
                }
                ResolvedValue::Struct(struct_id, generics) => {
                    let structs = self.structs.read();
                    let func = structs[struct_id].global_impl.get(name);
                    match func {
                        Some(&fn_id) => thing = ResolvedValue::Function(fn_id, generics),
                        None => return Err(self.emit_item_not_found(name.span(), name.symbol())),
                    }
                }

                _ => return Err(self.emit_item_not_found(name.span(), name.symbol())),
            }
        }
    }

    /// Fills in generics for the resolved value, erroring if too many/little were given. Does **NOT**
    /// check trait bounds.
    fn fill_in_generics(
        &self,
        thing: ResolvedValue<'ctx>,
        generics: &[TypeRef<'ctx>],
        span: Span<'ctx>,
        module: ModuleId,
        generics_in_context: &[TypedGeneric<'ctx>],
    ) -> Result<ResolvedValue<'ctx>, ErrorEmitted> {
        match thing {
            ResolvedValue::Function(id, ty_list) => {
                let fn_reader = self.functions.read();
                let required = &fn_reader[id].0.generics;
                if generics.len() != required.len() {
                    return Err(self.emit_mismatching_generic_count(
                        span,
                        required.len(),
                        generics.len(),
                    ));
                }

                let mut list;
                if let FunctionContext::StructFn(_) = fn_reader[id].0.context {
                    list = ty_list.to_vec();
                } else {
                    assert!(ty_list.is_empty());
                    list = Vec::new();
                }

                list.reserve(generics.len());
                for (i, generic) in generics.iter().enumerate() {
                    let ty = self.resolve_type(module, generic, generics_in_context)?;
                    if required[i].sized && !ty.is_sized() {
                        return Err(self.emit_unsized_for_sized_generic(generic.span(), ty));
                    }
                    if !ty.implements(&required[i].bounds, self) {
                        return Err(self.emit_ty_does_not_implement_bounds(
                            generic.span(),
                            required[i].name.span(),
                            ty,
                        ));
                    }
                    list.push(ty);
                }
                drop(fn_reader);
                Ok(ResolvedValue::Function(id, self.intern_tylist(&list)))
            }
            ResolvedValue::Struct(id, ty_list) => {
                assert!(ty_list.is_empty());

                let struct_reader = self.structs.read();
                let required = &struct_reader[id].generics;
                if generics.len() != required.len() {
                    return Err(self.emit_mismatching_generic_count(
                        span,
                        required.len(),
                        generics.len(),
                    ));
                }

                let mut list = Vec::with_capacity(generics.len());

                for (i, generic) in generics.iter().enumerate() {
                    let ty = self.resolve_type(module, generic, generics_in_context)?;
                    if required[i].sized && !ty.is_sized() {
                        return Err(self.emit_unsized_for_sized_generic(generic.span(), ty));
                    }
                    if !ty.implements(&required[i].bounds, self) {
                        return Err(self.emit_ty_does_not_implement_bounds(
                            generic.span(),
                            required[i].name.span(),
                            ty,
                        ));
                    }
                    list.push(ty);
                }
                drop(struct_reader);
                Ok(ResolvedValue::Struct(id, self.intern_tylist(&list)))
            }
            ResolvedValue::ExternalFunction(_)
            | ResolvedValue::Static(_)
            | ResolvedValue::Module(_)
            | ResolvedValue::Trait(_) => {
                if generics.is_empty() {
                    Ok(thing)
                } else {
                    Err(self.emit_mismatching_generic_count(span, 0, generics.len()))
                }
            }
        }
    }

    // pub fn typed_resolve_import(
    //     &self,
    //     module: ModuleId,
    //     import: &[Ident<'ctx>],
    //     span: Span<'ctx>,
    //     already_included: &mut HashSet<(ModuleId, Symbol<'ctx>)>,
    // ) -> Result<ResolvedValue<'ctx>, Diagnostic<'ctx>> {
    //     assert!(!import.is_empty());
    //     if !already_included.insert((module, import[0].symbol())) {
    //         return Err(TypecheckingError::CyclicDependency(span).to_error());
    //     }
    //
    //     let module_reader = self.modules.read();
    //     let cur_mod = &module_reader[module];
    //     let thing;
    //
    //     if &*import[0] == "pkg" {
    //         thing = ResolvedValue::Module(cur_mod.root_module);
    //     } else if &*import[0] == "super" {
    //         let Some(parent) = cur_mod.parent else {
    //             return Err(TypecheckingError::ItemNotFound {
    //                 span: import[0].span(),
    //                 name: import[0].symbol(),
    //             }
    //             .to_error());
    //         };
    //         thing = ResolvedValue::Module(parent);
    //     } else if let Some(&value) = cur_mod.scope.get(&import[0].symbol()) {
    //         thing = value;
    //     } else if let Some(package) = self.dependencies[cur_mod.root_module].get(&*import[0]) {
    //         thing = ResolvedValue::Module(*package);
    //     } else {
    //         return Err(TypecheckingError::ItemNotFound {
    //             span,
    //             name: import[0].symbol(),
    //         }
    //         .to_error());
    //     }
    //
    //     if import.len() == 1 {
    //         return Ok(thing);
    //     }
    //     match thing {
    //         ResolvedValue::Module(module) => {
    //             self.typed_resolve_import(module, &import[1..], span, already_included)
    //         }
    //
    //         _ => Err(TypecheckingError::ItemNotFound {
    //             span: import[1].span(),
    //             name: import[1].symbol(),
    //         }
    //         .to_error()),
    //     }
    // }
}

impl<'ctx> Deref for TypeckCtx<'ctx> {
    type Target = TypeCtx<'ctx>;

    fn deref(&self) -> &Self::Target {
        &self.ctx
    }
}

/// `with_generics`: If this is true, this function will error if generics are missing.
/// This is wanted in case of uses (e.g. `type_name()`), but not wanted in the case of use
/// statements.
fn resolve_import_simple<'a, 'ctx: 'a>(
    context: &ModuleContext<'ctx>,
    ctx: TypeCtx<'ctx>,
    current_module: ModuleId,
    import: impl Iterator<Item = &'a Ident<'ctx>>,
    span: Span<'ctx>,
    generics_in_context: &[TypedGeneric<'ctx>],
    with_generics: bool,
) -> Result<ResolvedValue<'ctx>, ErrorEmitted> {
    resolve_import(
        context,
        ctx,
        current_module,
        #[allow(trivial_casts)]
        import.map(|v| (v, &[] as &[_])),
        span,
        generics_in_context,
        with_generics,
    )
}

/// `with_generics`: If this is true, this function will error if generics are missing.
/// This is wanted in case of uses (e.g. `type_name()`), but not wanted in the case of use
/// statements.
fn resolve_import<'a, 'ctx: 'a>(
    context: &ModuleContext<'ctx>,
    ctx: TypeCtx<'ctx>,
    current_module: ModuleId,
    mut import: impl Iterator<Item = (&'a Ident<'ctx>, &'a [TypeRef<'ctx>])>,
    span: Span<'ctx>,
    generics_in_context: &[TypedGeneric<'ctx>],
    with_generics: bool,
) -> Result<ResolvedValue<'ctx>, ErrorEmitted> {
    let next = import
        .next()
        .expect("resolve_import called on an empty import");
    resolve_import_inner(
        context,
        ctx,
        current_module,
        import,
        next,
        span,
        &mut HashSet::new(),
        generics_in_context,
        with_generics,
    )
}

// TODO: make this better lol
#[allow(clippy::too_many_arguments)]
fn resolve_import_inner<'a, 'ctx: 'a>(
    context: &ModuleContext<'ctx>,
    ctx: TypeCtx<'ctx>,
    current_module: ModuleId,
    mut import: impl Iterator<Item = (&'a Ident<'ctx>, &'a [TypeRef<'ctx>])>,
    (name, generics): (&'a Ident<'ctx>, &'a [TypeRef<'ctx>]),
    span: Span<'ctx>,
    already_included: &mut HashSet<(ModuleId, Symbol<'ctx>)>,
    generics_in_context: &[TypedGeneric<'ctx>],
    with_generics: bool,
) -> Result<ResolvedValue<'ctx>, ErrorEmitted> {
    if !already_included.insert((current_module, name.symbol())) {
        return Err(ctx.emit_cyclic_dependency(span));
    }

    let module_reader = context.modules.read();
    let cur_mod = &module_reader[current_module];
    let mut thing;
    if &**name == "pkg" {
        thing = ResolvedValue::Module(cur_mod.package_root);
    } else if &**name == "super" {
        let Some(parent) = cur_mod.parent else {
            return Err(ctx.emit_item_not_found(name.span(), name.symbol()));
        };
        thing = ResolvedValue::Module(parent);
    } else if let Some(&value) = cur_mod.scope.get(&name.symbol()) {
        thing = value.into();
    } else if let Some((span, import)) = cur_mod.imports.get(&name.symbol()) {
        #[allow(trivial_casts)]
        let mut iter = import.iter_with_pathbuf();
        let next = iter.next().unwrap();
        thing = resolve_import_inner(
            context,
            ctx,
            current_module,
            iter,
            next,
            *span,
            already_included,
            generics_in_context,
            false,
        )?
    } else if let Some(package) = context.dependencies[cur_mod.package_root].get(&**name) {
        thing = ResolvedValue::Module(*package);
    } else {
        return Err(ctx.emit_item_not_found(span, name.symbol()));
    }

    let mut generics = generics;
    thing.purge_generics();

    loop {
        if with_generics {
            thing = fill_in_generics(
                thing,
                generics,
                span,
                context,
                ctx,
                current_module,
                generics_in_context,
            )?;
        }
        let name;
        (name, generics) = match import.next() {
            Some(v) => v,
            None => return Ok(thing),
        };

        match thing {
            ResolvedValue::Module(module) => {
                return resolve_import_inner(
                    context,
                    ctx,
                    module,
                    import,
                    (name, generics),
                    span,
                    already_included,
                    generics_in_context,
                    with_generics,
                );
            }
            ResolvedValue::Struct(struct_id, generics) => {
                let structs = context.structs.read();
                let func = structs[struct_id].global_impl.get(name);
                match func {
                    Some(&fn_id) => thing = ResolvedValue::Function(fn_id, generics),
                    None => return Err(ctx.emit_item_not_found(name.span(), name.symbol())),
                }
            }

            _ => return Err(ctx.emit_item_not_found(name.span(), name.symbol())),
        }
    }
}

/// Fills in generics for the resolved value, erroring if too many/little were given. Does **NOT**
/// check trait bounds.
fn fill_in_generics<'ctx>(
    thing: ResolvedValue<'ctx>,
    generics: &[TypeRef<'ctx>],
    span: Span<'ctx>,
    context: &ModuleContext<'ctx>,
    ctx: TypeCtx<'ctx>,
    module: ModuleId,
    generics_in_context: &[TypedGeneric<'ctx>],
) -> Result<ResolvedValue<'ctx>, ErrorEmitted> {
    match thing {
        ResolvedValue::Function(id, ty_list) => {
            let fn_reader = context.functions.read();
            let required = fn_reader[id].contract.generics.len();
            if generics.len() != required {
                return Err(ctx.emit_mismatching_generic_count(span, required, generics.len()));
            }

            let mut list;
            if let FunctionContext::StructFn(_) = fn_reader[id].ctx {
                list = ty_list.to_vec();
            } else {
                assert!(ty_list.is_empty());
                list = Vec::new();
            }
            drop(fn_reader);

            for generic in generics {
                list.push(type_resolution_resolve_type(
                    ctx,
                    generic,
                    generics_in_context,
                    module,
                    context,
                )?);
            }
            Ok(ResolvedValue::Function(id, ctx.intern_tylist(&list)))
        }
        ResolvedValue::Struct(id, ty_list) => {
            assert!(ty_list.is_empty());
            let struct_reader = context.structs.read();
            let required = struct_reader[id].generics.len();
            if generics.len() != required {
                return Err(ctx.emit_mismatching_generic_count(span, required, generics.len()));
            }

            let mut list = Vec::with_capacity(generics.len());

            for generic in generics {
                list.push(type_resolution_resolve_type(
                    ctx,
                    generic,
                    generics_in_context,
                    module,
                    context,
                )?);
            }
            Ok(ResolvedValue::Struct(id, ctx.intern_tylist(&list)))
        }
        ResolvedValue::ExternalFunction(_)
        | ResolvedValue::Static(_)
        | ResolvedValue::Module(_)
        | ResolvedValue::Trait(_) => {
            if generics.is_empty() {
                Ok(thing)
            } else {
                Err(ctx.emit_mismatching_generic_count(span, 0, generics.len()))
            }
        }
    }
}

fn type_resolution_resolve_type<'ctx>(
    ctx: TypeCtx<'ctx>,
    ty: &TypeRef<'ctx>,
    generics: &[TypedGeneric<'ctx>],
    module: ModuleId,
    context: &ModuleContext<'ctx>,
) -> Result<Ty<'ctx>, ErrorEmitted> {
    if let Some(ty) = resolve_primitive_type(ctx, ty) {
        return Ok(ty);
    }
    match ty {
        TypeRef::DynReference {
            traits,
            num_references,
            span,
        } => {
            let mut trait_refs = Vec::with_capacity(traits.len());
            for trait_name in traits.iter() {
                let v = resolve_import_simple(
                    context,
                    ctx,
                    module,
                    trait_name.as_slice().iter(),
                    *span,
                    generics,
                    true,
                );
                let id = match v {
                    Ok(ResolvedValue::Trait(id)) => id,
                    Ok(res_val) => {
                        return Err(ctx.emit_mismatching_scope_type(
                            *span,
                            ScopeKind::Trait,
                            res_val.into(),
                        ));
                    }
                    Err(e) => return Err(e),
                };
                trait_refs.push((id, *trait_name.as_slice().last().unwrap()));
            }
            let trait_refs = ArenaList::new(ctx.arena(), &trait_refs);
            Ok(with_refcount(
                ctx,
                ctx.intern_ty(TyKind::DynType(trait_refs)),
                *num_references,
            ))
        }
        TypeRef::Function {
            return_ty,
            args,
            num_references,
            ..
        } => {
            let return_type =
                type_resolution_resolve_type(ctx, return_ty, generics, module, context)?;
            let mut arguments = Vec::with_capacity(args.len());
            for arg in args {
                arguments.push(type_resolution_resolve_type(
                    ctx, arg, generics, module, context,
                )?);
            }
            let arguments = ctx.intern_tylist(&arguments);
            let ty = ctx.intern_ty(TyKind::Function(FunctionType {
                return_type,
                arguments,
            }));
            Ok(with_refcount(ctx, ty, *num_references))
        }
        TypeRef::Reference {
            num_references,
            type_name,
            span,
        } => {
            // generics can never have a generic attribute (struct Moew<T> { value: T<u32> })
            if type_name.entries.len() == 1 && type_name.entries[0].1.is_empty() {
                let name = type_name.entries[0].0;
                if let Some((generic_id, generic)) =
                    generics.iter().enumerate().find(|(_, v)| v.name == name)
                {
                    let ty = ctx.intern_ty(TyKind::Generic {
                        name: generic.name,
                        generic_id: generic_id as u8,
                        sized: generic.sized,
                        bounds: generic.bounds,
                    });
                    return Ok(with_refcount(ctx, ty, *num_references));
                }
            }

            let value = resolve_import(
                context,
                ctx,
                module,
                type_name.iter(),
                *span,
                generics,
                true,
            )?;

            let ResolvedValue::Struct(struct_id, generics) = value else {
                return Err(ctx.emit_mismatching_scope_type(*span, ScopeKind::Type, value.into()));
            };

            let name = context.structs.read()[struct_id].name;
            let ty = ctx.intern_ty(TyKind::Struct {
                struct_id,
                name,
                generics,
            });
            Ok(with_refcount(ctx, ty, *num_references))
        }
        &TypeRef::Void(_, refs) => Ok(with_refcount(ctx, default_types::void, refs)),
        TypeRef::Never(_) => Ok(default_types::never),
        TypeRef::UnsizedArray {
            num_references,
            child,
            span: _,
        } => Ok(with_refcount(
            ctx,
            ctx.intern_ty(TyKind::UnsizedArray(type_resolution_resolve_type(
                ctx, child, generics, module, context,
            )?)),
            *num_references,
        )),
        TypeRef::SizedArray {
            num_references,
            child,
            number_elements,
            span: _,
        } => Ok(with_refcount(
            ctx,
            ctx.intern_ty(TyKind::SizedArray {
                ty: type_resolution_resolve_type(ctx, child, generics, module, context)?,
                number_elements: *number_elements,
            }),
            *num_references,
        )),
        TypeRef::Tuple {
            num_references,
            elements,
            ..
        } => {
            let mut typed_elements = Vec::with_capacity(elements.len());
            for elem in elements.iter() {
                typed_elements.push(type_resolution_resolve_type(
                    ctx, elem, generics, module, context,
                )?);
            }
            let ty = ctx.intern_ty(TyKind::Tuple(ctx.intern_tylist(&typed_elements)));
            Ok(with_refcount(ctx, ty, *num_references))
        }
    }
}

impl From<ResolvedValue<'_>> for ScopeKind {
    fn from(value: ResolvedValue<'_>) -> Self {
        match value {
            ResolvedValue::Trait(_) => Self::Trait,
            ResolvedValue::Struct(_, _) => Self::Struct,
            ResolvedValue::Static(_) => Self::Static,
            ResolvedValue::Module(_) => Self::Module,
            ResolvedValue::Function(_, _) | ResolvedValue::ExternalFunction(_) => Self::Function,
        }
    }
}

impl From<ModuleScopeValue> for ResolvedValue<'static> {
    fn from(value: ModuleScopeValue) -> Self {
        use ModuleScopeValue as MSV;
        match value {
            MSV::Function(v) => Self::Function(v, EMPTY_TYLIST),
            MSV::ExternalFunction(v) => Self::ExternalFunction(v),
            MSV::Struct(v) => Self::Struct(v, EMPTY_TYLIST),
            MSV::Static(v) => Self::Static(v),
            MSV::Module(v) => Self::Module(v),
            MSV::Trait(v) => Self::Trait(v),
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum ResolvedValue<'ctx> {
    Function(FunctionId, TyList<'ctx>),
    ExternalFunction(ExternalFunctionId),
    Struct(StructId, TyList<'ctx>),
    Static(StaticId),
    Module(ModuleId),
    Trait(TraitId),
}

impl ResolvedValue<'_> {
    pub(crate) fn purge_generics(&mut self) {
        match self {
            Self::Struct(_, ty_list) | Self::Function(_, ty_list) => *ty_list = EMPTY_TYLIST,
            Self::ExternalFunction(_) | Self::Static(_) | Self::Module(_) | Self::Trait(_) => {}
        }
    }
}

#[derive(Debug, Clone, Copy, Display)]
pub enum ScopeKind {
    #[display("trait")]
    Trait,
    #[display("type")]
    Type,
    #[display("value")]
    Value,
    #[display("struct")]
    Struct,
    #[display("function")]
    Function,
    #[display("global variable")]
    Static,
    #[display("module")]
    Module,
}
