use mira_errors::Diagnostic;
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
use mira_common::store::{AssociatedStore, Store, StoreKey};
use mira_context::DocComment;
use mira_parser::{
    TypeRef,
    annotations::Annotations,
    module::{BakedStruct, Module, ModuleContext, ModuleScopeValue},
};
use mira_spans::{
    ArenaList, Ident, SourceFile, Span,
    interner::{Symbol, symbols},
};

mod context;
mod lang_items;
mod monomorphisation;
pub use monomorphisation::{Substitute, SubstitutionCtx};
pub mod optimizations;
pub use context::{GlobalContext, TypeCtx};
mod error;
pub mod intrinsics;
pub mod ir;
pub mod ir_displayer;
pub mod queries;
mod type_resolution;
pub mod typechecking;
mod types;
pub use error::{FunctionList, TypecheckingError, TypecheckingErrorDiagnosticsExt};
pub use types::{FunctionType, Ty, TyKind, TyList, TypeInterner, TypeListInterner, default_types};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedGeneric<'arena> {
    pub name: Ident<'arena>,
    pub sized: bool,
    pub bounds: ArenaList<'arena, StoreKey<TypedTrait<'arena>>>,
}

#[derive(Debug)]
pub struct TypedFunctionContract<'arena> {
    pub name: Option<Ident<'arena>>,
    pub arguments: Vec<(Ident<'arena>, Ty<'arena>)>,
    pub return_type: Ty<'arena>,
    pub annotations: Annotations<'arena>,
    pub span: Span<'arena>,
    pub module_id: StoreKey<TypedModule<'arena>>,
    pub generics: Vec<TypedGeneric<'arena>>,
    pub comment: DocComment,
}

impl Hash for TypedFunctionContract<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match &self.name {
            None => "{{anon_fn}}".hash(state),
            Some(v) => v.hash(state),
        }
        self.module_id.hash(state);
        self.arguments.hash(state);
        self.return_type.hash(state);
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
    pub module_id: StoreKey<TypedModule<'arena>>,
    pub id: StoreKey<TypedTrait<'arena>>,
    pub annotations: Annotations<'arena>,
    pub comment: DocComment,
}

#[derive(Debug)]
pub struct TypedStruct<'arena> {
    pub name: Ident<'arena>,
    pub elements: Vec<(Ident<'arena>, Ty<'arena>, DocComment)>,
    pub span: Span<'arena>,
    pub global_impl: HashMap<Ident<'arena>, StoreKey<TypedFunction<'arena>>>,
    pub trait_impl: HashMap<StoreKey<TypedTrait<'arena>>, Vec<StoreKey<TypedFunction<'arena>>>>,
    pub annotations: Annotations<'arena>,
    pub module_id: StoreKey<TypedModule<'arena>>,
    pub id: StoreKey<TypedStruct<'arena>>,
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
    pub module_id: StoreKey<TypedModule<'arena>>,
    pub span: Span<'arena>,
    pub annotations: Annotations<'arena>,
    pub name: Ident<'arena>,
    pub comment: DocComment,
}

impl<'arena> TypedStatic<'arena> {
    pub fn new(
        ty: Ty<'arena>,
        literal: TypedLiteral<'arena>,
        module: StoreKey<TypedModule<'arena>>,
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
    pub modules: RwLock<AssociatedStore<TypedModule<'arena>, Module<'arena>>>,
    pub functions: RwLock<Store<TypedFunction<'arena>>>,
    pub external_functions: RwLock<Store<TypedExternalFunction<'arena>>>,
    pub statics: RwLock<Store<TypedStatic<'arena>>>,
    pub structs: RwLock<Store<TypedStruct<'arena>>>,
    pub traits: RwLock<Store<TypedTrait<'arena>>>,
    pub lang_items: RwLock<LangItems<'arena>>,
    pub main_function: OnceLock<StoreKey<TypedFunction<'arena>>>,
    pub ctx: TypeCtx<'arena>,
    pub dependencies:
        AssociatedStore<HashMap<Arc<str>, StoreKey<TypedModule<'arena>>>, TypedModule<'arena>>,
}

#[derive(Debug)]
pub struct TypedModule<'arena> {
    pub scope: HashMap<Ident<'arena>, ModuleScopeValue<'arena>>,
    pub exports: HashSet<Ident<'arena>>,
    pub parent: StoreKey<TypedModule<'arena>>,
    pub root_module: StoreKey<TypedModule<'arena>>,
    pub name: Symbol<'arena>,
    pub file: Arc<SourceFile>,
    pub assembly: Vec<(Span<'arena>, String)>,
    pub comment: DocComment,
}

impl<'ctx> TypeckCtx<'ctx> {
    pub fn substitute<T: Deref<Target = [Ty<'ctx>]>, V: Substitute<'ctx>>(
        &self,
        tys: &T,
        v: V,
    ) -> V {
        v.substitute(&SubstitutionCtx::new(self, tys))
    }

    pub fn validate_main_function(
        &self,
        main_pkg: StoreKey<Module<'ctx>>,
    ) -> Result<StoreKey<TypedFunction<'ctx>>, TypecheckingError<'ctx>> {
        let module = &self.modules.read()[main_pkg];
        let Some(ModuleScopeValue::Function(main_fn)) =
            module.scope.get(&symbols::DefaultIdents::main)
        else {
            return Err(TypecheckingError::MainFuncNotFound(
                module.file.path.clone(),
            ));
        };
        let func = &self.functions.read()[main_fn.cast()].0;
        if !func.arguments.is_empty() || func.return_type != default_types::void {
            return Err(TypecheckingError::MainFuncWrongType {
                func_span: func.span,
            });
        }
        self.main_function
            .set(main_fn.cast())
            .expect("validate_main_function called multiple times");
        Ok(main_fn.cast())
    }

    pub fn new(ctx: TypeCtx<'ctx>, module_context: Arc<ModuleContext<'ctx>>) -> Arc<Self> {
        let traits_reader = module_context.traits.read();
        let structs_reader = module_context.structs.read();
        let statics_reader = module_context.statics.read();
        let functions_reader = module_context.functions.read();
        let external_functions_reader = module_context.external_functions.read();

        let mut traits = AssociatedStore::with_capacity(traits_reader.len());
        let mut structs = AssociatedStore::with_capacity(structs_reader.len());
        let mut statics = AssociatedStore::with_capacity(statics_reader.len());
        let mut functions = AssociatedStore::with_capacity(functions_reader.len());
        let mut external_functions =
            AssociatedStore::with_capacity(external_functions_reader.len());

        for key in structs_reader.indices() {
            structs.insert(
                key,
                TypedStruct {
                    name: Ident::EMPTY,
                    elements: Vec::new(),
                    span: structs_reader[key].span,
                    global_impl: HashMap::new(),
                    trait_impl: HashMap::new(),
                    annotations: Annotations::default(),
                    module_id: StoreKey::undefined(),
                    generics: Vec::new(),
                    id: key.cast(),
                    comment: DocComment::EMPTY,
                },
            );
        }

        for key in statics_reader.indices() {
            statics.insert(
                key,
                TypedStatic::new(
                    default_types::never,
                    TypedLiteral::Void,
                    StoreKey::undefined(),
                    statics_reader[key].span,
                    Annotations::default(),
                    Ident::EMPTY,
                    DocComment::EMPTY,
                ),
            );
        }

        for key in functions_reader.indices() {
            functions.insert(
                key,
                (
                    TypedFunctionContract {
                        annotations: Annotations::default(),
                        name: None,
                        arguments: Vec::new(),
                        return_type: default_types::never,
                        span: functions_reader[key].0.span,
                        module_id: StoreKey::undefined(),
                        generics: Vec::new(),
                        comment: DocComment::EMPTY,
                    },
                    IR::new(std::iter::empty(), functions_reader[key].0.span),
                ),
            );
        }

        for key in external_functions_reader.indices() {
            external_functions.insert(
                key,
                (
                    TypedFunctionContract {
                        annotations: Annotations::default(),
                        name: None,
                        arguments: Vec::new(),
                        return_type: default_types::never,
                        span: external_functions_reader[key].0.span,
                        module_id: StoreKey::undefined(),
                        generics: Vec::new(),
                        comment: DocComment::EMPTY,
                    },
                    None,
                ),
            );
        }

        for key in traits_reader.indices() {
            traits.insert(
                key,
                TypedTrait {
                    name: Ident::EMPTY,
                    functions: Vec::new(),
                    span: traits_reader[key].span,
                    module_id: StoreKey::undefined(),
                    id: key.cast(),
                    annotations: Annotations::default(),
                    comment: DocComment::EMPTY,
                },
            );
        }

        let mut dependencies = AssociatedStore::with_capacity(module_context.dependencies.len());
        for (k, v) in module_context.dependencies.index_value_iter() {
            let mut map = HashMap::with_capacity(v.len());
            for (k, v) in v.iter() {
                map.insert(k.clone(), v.cast());
            }
            dependencies.insert(k.cast(), map);
        }

        let me = Arc::new(Self {
            ctx,
            dependencies,
            structs: RwLock::new(structs.into()),
            statics: RwLock::new(statics.into()),
            functions: RwLock::new(functions.into()),
            traits: RwLock::new(traits.into()),
            external_functions: RwLock::new(external_functions.into()),
            modules: Default::default(),
            lang_items: RwLock::new(LangItems::new(ctx)),
            main_function: OnceLock::new(),
        });

        let mut typechecked_module_writer = me.modules.write();
        let module_reader = module_context.modules.read();

        for (key, module) in module_reader.index_value_iter() {
            let scope = module.scope.clone();

            typechecked_module_writer.insert(
                key,
                TypedModule {
                    scope,
                    parent: module.parent.cast(),
                    root_module: module.package_root.cast(),
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

    pub fn resolve_imports(&self, context: Arc<ModuleContext<'ctx>>) {
        let mut typechecked_module_writer = self.modules.write();
        let module_reader = context.modules.read();
        for key in module_reader.indices() {
            for (name, (span, import)) in module_reader[key].imports.iter() {
                let res =
                    resolve_import(&context, key, &import.entries, *span, &mut HashSet::new());
                match res {
                    Err(diag) => _ = self.ctx.emit_diag(diag),
                    Ok(k) => {
                        typechecked_module_writer[key.cast()].scope.insert(*name, k);
                    }
                }
            }
        }
    }

    pub fn resolve_type(
        &self,
        module_id: StoreKey<TypedModule<'ctx>>,
        ty: &TypeRef<'ctx>,
        generics: &[TypedGeneric<'ctx>],
    ) -> Result<Ty<'ctx>, Diagnostic<'ctx>> {
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
                    let v = self.typed_resolve_import(
                        module_id,
                        trait_name.as_slice(),
                        *span,
                        &mut HashSet::new(),
                    );
                    let Ok(ModuleScopeValue::Trait(id)) = v else {
                        return Err(
                            TypecheckingError::CannotFindTrait(*span, trait_name.clone())
                                .to_error(),
                        );
                    };
                    trait_refs.push((id.cast(), *trait_name.as_slice().last().unwrap()));
                }
                let trait_refs = ArenaList::new(self.ctx.arena(), &trait_refs);
                Ok(with_refcount(
                    self.ctx,
                    self.ctx.intern_ty(TyKind::DynType(trait_refs)),
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
                let arguments = self.ctx.intern_tylist(&arguments);
                let function_typ = FunctionType {
                    arguments,
                    return_type,
                };
                Ok(with_refcount(
                    self.ctx,
                    self.ctx.intern_ty(TyKind::Function(function_typ)),
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
                    let ty = self.ctx.intern_ty(TyKind::Generic {
                        name: type_name.entries[0].0,
                        generic_id: id as u8,
                        sized: generic.sized,
                        bounds: generic.bounds,
                    });
                    return Ok(with_refcount(self.ctx, ty, *num_references));
                }

                let path = type_name.entries.iter().map(|v| v.0).collect::<Vec<_>>();
                // NOTE: this should only have a generic at the end as this is a type
                // (std::vec::Vec, can never be std::vec::Vec<u32>::Vec.)
                for (.., generics) in type_name.entries.iter() {
                    if !generics.is_empty() {
                        return Err(
                            TypecheckingError::UnexpectedGenerics { span: *span }.to_error()
                        );
                    }
                }

                match self.typed_resolve_import(module_id, &path, *span, &mut HashSet::new())? {
                    ModuleScopeValue::Struct(id) => Ok(with_refcount(
                        self.ctx,
                        self.ctx.intern_ty(TyKind::Struct {
                            struct_id: id.cast(),
                            name: self.structs.read()[id.cast()].name,
                        }),
                        *num_references,
                    )),
                    v => Err(TypecheckingError::MismatchingScopeType {
                        span: *span,
                        expected: ScopeKind::Type,
                        found: v.into(),
                    }
                    .to_error()),
                }
            }
            TypeRef::Void(..) | TypeRef::Never(_) => unreachable!(),
            TypeRef::UnsizedArray {
                num_references,
                child,
                span: _,
            } => Ok(with_refcount(
                self.ctx,
                self.ctx.intern_ty(TyKind::UnsizedArray(
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
                self.ctx.intern_ty(TyKind::SizedArray {
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
                    .intern_ty(TyKind::Tuple(self.ctx.intern_tylist(&typed_elements)));
                Ok(with_refcount(self.ctx, ty, *num_references))
            }
        }
    }

    /// returns if a recursive field was detected
    fn resolve_struct(
        &self,
        context: Arc<ModuleContext<'ctx>>,
        id: StoreKey<BakedStruct<'ctx>>,
        module_id: StoreKey<Module<'ctx>>,
        left: &mut HashMap<StoreKey<BakedStruct<'ctx>>, ResolvingState>,
    ) -> bool {
        if !left.contains_key(&id) {
            return false;
        }
        match left.get_mut(&id) {
            None => return false,
            Some(ResolvingState::Working) => return true,
            Some(v @ ResolvingState::Pending) => *v = ResolvingState::Working,
        }

        let mut writer = context.structs.write();

        fn migrate_hashmap<K, A, B>(value: HashMap<K, StoreKey<A>>) -> HashMap<K, StoreKey<B>> {
            // SAFETY: This is safe [[i think]] because the generics are part of a phantomdata.
            unsafe { std::mem::transmute(value) }
        }

        let global_impl = migrate_hashmap(std::mem::take(&mut writer[id].global_impl));
        let annotations = std::mem::take(&mut writer[id].annotations);
        let elements = std::mem::take(&mut writer[id].elements);
        let mut generics = Vec::new();

        for generic in &writer[id].generics {
            let mut bounds = Vec::new();

            for (bound, span) in &generic.bounds {
                match resolve_import(
                    &context,
                    module_id,
                    &bound.entries,
                    *span,
                    &mut HashSet::new(),
                ) {
                    Err(e) => _ = self.ctx.emit_diag(e),
                    Ok(ModuleScopeValue::Trait(trait_id)) => bounds.push(trait_id.cast()),
                    Ok(_) => {
                        self.ctx.emit_unbound_ident(
                            *span,
                            bound.entries[bound.entries.len() - 1].symbol(),
                        );
                    }
                }
            }

            let bounds = ArenaList::new(self.ctx.arena(), &bounds);
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
            module_id: module_id.cast(),
            id: id.cast(),
            generics,
            trait_impl: HashMap::new(),
            comment: writer[id].comment,
        };
        drop(writer);

        for element in elements {
            if let Some(ty) = self.type_resolution_resolve_type(
                &element.1,
                &typed_struct.generics,
                module_id.cast(),
                context.clone(),
                left,
            ) {
                typed_struct.elements.push((element.0, ty, element.2));
            }
        }
        self.structs.write()[id.cast()] = typed_struct;

        left.remove(&id);
        false
    }

    fn type_resolution_resolve_type(
        &self,
        ty: &TypeRef<'ctx>,
        generics: &[TypedGeneric<'ctx>],
        module: StoreKey<TypedModule<'ctx>>,
        context: Arc<ModuleContext<'ctx>>,
        left: &mut HashMap<StoreKey<BakedStruct<'ctx>>, ResolvingState>,
    ) -> Option<Ty<'ctx>> {
        if let Some(ty) = resolve_primitive_type(self.ctx, ty) {
            return Some(ty);
        }
        match ty {
            TypeRef::DynReference {
                traits,
                num_references,
                span,
            } => {
                let mut trait_refs = Vec::with_capacity(traits.len());
                for trait_name in traits.iter() {
                    let v = self.typed_resolve_import(
                        module,
                        trait_name.as_slice(),
                        *span,
                        &mut HashSet::new(),
                    );
                    let Ok(ModuleScopeValue::Trait(id)) = v else {
                        return None;
                    };
                    trait_refs.push((id.cast(), *trait_name.as_slice().last().unwrap()));
                }
                let trait_refs = ArenaList::new(self.ctx.arena(), &trait_refs);
                Some(with_refcount(
                    self.ctx,
                    self.ctx.intern_ty(TyKind::DynType(trait_refs)),
                    *num_references,
                ))
            }
            TypeRef::Function {
                return_ty,
                args,
                num_references,
                ..
            } => {
                let return_type = self.type_resolution_resolve_type(
                    return_ty,
                    generics,
                    module,
                    context.clone(),
                    left,
                )?;
                let mut arguments = Vec::with_capacity(args.len());
                for arg in args {
                    arguments.push(self.type_resolution_resolve_type(
                        arg,
                        generics,
                        module,
                        context.clone(),
                        left,
                    )?);
                }
                let arguments = self.ctx.intern_tylist(&arguments);
                let ty = self.ctx.intern_ty(TyKind::Function(FunctionType {
                    return_type,
                    arguments,
                }));
                Some(with_refcount(self.ctx, ty, *num_references))
            }
            TypeRef::Reference {
                num_references,
                type_name,
                span,
            } => {
                let path = type_name.entries.iter().map(|v| v.0).collect::<Vec<_>>();
                // NOTE: this should only have a generic at the end as this is a type
                // (std::vec::Vec, can never be std::vec::Vec<u32>::Vec.)
                for (.., generics) in type_name.entries.iter() {
                    if !generics.is_empty() {
                        return None;
                    }
                }

                // generics can never have a generic attribute (struct Moew<T> { value: T<u32> })
                if type_name.entries.len() == 1 && type_name.entries[0].1.is_empty() {
                    let name = type_name.entries[0].0;
                    if let Some((generic_id, generic)) =
                        generics.iter().enumerate().find(|(_, v)| v.name == name)
                    {
                        return Some(self.ctx.intern_ty(TyKind::Generic {
                            name: generic.name,
                            generic_id: generic_id as u8,
                            sized: generic.sized,
                            bounds: generic.bounds,
                        }));
                    }
                }

                let Ok(value) =
                    resolve_import(&context, module.cast(), &path, *span, &mut HashSet::new())
                else {
                    self.ctx
                        .emit_unbound_ident(*span, path[path.len() - 1].symbol());
                    return None;
                };

                let ModuleScopeValue::Struct(id) = value else {
                    self.ctx
                        .emit_mismatching_scope_type(*span, ScopeKind::Type, value.into());
                    return None;
                };

                {
                    if !left.contains_key(&id) {
                        let typechecked_struct = &self.structs.read()[id.cast()];
                        let ty = self.ctx.intern_ty(TyKind::Struct {
                            struct_id: typechecked_struct.id,
                            name: typechecked_struct.name,
                        });
                        return Some(with_refcount(self.ctx, ty, *num_references));
                    }
                }

                let module = context.structs.read()[id].module_id;
                if self.resolve_struct(context, id, module, left) {
                    self.ctx.emit_recursive_type_detected(*span);
                    return None;
                }
                let typechecked_struct = &self.structs.read()[id.cast()];
                let ty = self.ctx.intern_ty(TyKind::Struct {
                    struct_id: typechecked_struct.id,
                    name: typechecked_struct.name,
                });
                Some(with_refcount(self.ctx, ty, *num_references))
            }
            TypeRef::Void(..) => unreachable!(),
            TypeRef::Never(_) => unreachable!(),
            TypeRef::UnsizedArray {
                num_references,
                child,
                span: _,
            } => Some(with_refcount(
                self.ctx,
                self.ctx.intern_ty(TyKind::UnsizedArray(
                    self.type_resolution_resolve_type(child, generics, module, context, left)?,
                )),
                *num_references,
            )),
            TypeRef::SizedArray {
                num_references,
                child,
                number_elements,
                span: _,
            } => Some(with_refcount(
                self.ctx,
                self.ctx.intern_ty(TyKind::SizedArray {
                    ty: self
                        .type_resolution_resolve_type(child, generics, module, context, left)?,
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
                    typed_elements.push(self.type_resolution_resolve_type(
                        elem,
                        generics,
                        module,
                        context.clone(),
                        left,
                    )?);
                }
                let ty = self
                    .ctx
                    .intern_ty(TyKind::Tuple(self.ctx.intern_tylist(&typed_elements)));
                Some(with_refcount(self.ctx, ty, *num_references))
            }
        }
    }

    pub fn typed_resolve_import(
        &self,
        module: StoreKey<TypedModule<'ctx>>,
        import: &[Ident<'ctx>],
        span: Span<'ctx>,
        already_included: &mut HashSet<(StoreKey<TypedModule<'ctx>>, Symbol<'ctx>)>,
    ) -> Result<ModuleScopeValue<'ctx>, Diagnostic<'ctx>> {
        assert!(!import.is_empty());
        if !already_included.insert((module, import[0].symbol())) {
            return Err(TypecheckingError::CyclicDependency(span).to_error());
        }

        let module_reader = self.modules.read();
        let cur_mod = &module_reader[module.cast()];
        let thing;

        if &*import[0] == "pkg" {
            thing = ModuleScopeValue::Module(cur_mod.root_module.cast());
        } else if &*import[0] == "super" {
            thing = ModuleScopeValue::Module(cur_mod.parent.cast());
        } else if let Some(value) = cur_mod.scope.get(&import[0].symbol()) {
            thing = *value;
        } else if let Some(package) = self.dependencies[cur_mod.root_module].get(&*import[0]) {
            thing = ModuleScopeValue::Module(package.cast());
        } else {
            return Err(TypecheckingError::ItemNotFound {
                span,
                name: import[0].symbol(),
            }
            .to_error());
        }

        if import.len() == 1 {
            return Ok(thing);
        }
        match thing {
            ModuleScopeValue::Module(module) => {
                self.typed_resolve_import(module.cast(), &import[1..], span, already_included)
            }

            _ => Err(TypecheckingError::ItemNotFound {
                span: import[1].span(),
                name: import[1].symbol(),
            }
            .to_error()),
        }
    }
}

impl<'ctx> Deref for TypeckCtx<'ctx> {
    type Target = TypeCtx<'ctx>;

    fn deref(&self) -> &Self::Target {
        &self.ctx
    }
}

fn resolve_import<'arena>(
    context: &ModuleContext<'arena>,
    current_module: StoreKey<Module<'arena>>,
    import: &[Ident<'arena>],
    span: Span<'arena>,
    already_included: &mut HashSet<(StoreKey<Module<'arena>>, Symbol<'arena>)>,
) -> Result<ModuleScopeValue<'arena>, Diagnostic<'arena>> {
    assert!(!import.is_empty());
    if !already_included.insert((current_module, import[0].symbol())) {
        return Err(TypecheckingError::CyclicDependency(span).to_error());
    }

    let module_reader = context.modules.read();
    let cur_mod = &module_reader[current_module];
    let thing;
    if &*import[0] == "pkg" {
        thing = ModuleScopeValue::Module(cur_mod.package_root);
    } else if &*import[0] == "super" {
        thing = ModuleScopeValue::Module(cur_mod.parent);
    } else if let Some(value) = cur_mod.scope.get(&import[0].symbol()) {
        thing = *value;
    } else if let Some((span, import)) = cur_mod.imports.get(&import[0].symbol()) {
        thing = resolve_import(
            context,
            current_module,
            &import.entries,
            *span,
            already_included,
        )?
    } else if let Some(package) = context.dependencies[cur_mod.package_root].get(&*import[0]) {
        thing = ModuleScopeValue::Module(*package);
    } else {
        return Err(TypecheckingError::ItemNotFound {
            span,
            name: import[0].symbol(),
        }
        .to_error());
    }

    if import.len() == 1 {
        return Ok(thing);
    }
    match thing {
        ModuleScopeValue::Module(module) => {
            resolve_import(context, module, &import[1..], span, already_included)
        }

        _ => Err(TypecheckingError::ItemNotFound {
            span: import[1].span(),
            name: import[1].symbol(),
        }
        .to_error()),
    }
}

impl From<ModuleScopeValue<'_>> for ScopeKind {
    fn from(value: ModuleScopeValue<'_>) -> Self {
        match value {
            ModuleScopeValue::Trait(_) => Self::Trait,
            ModuleScopeValue::Struct(_) => Self::Type,
            ModuleScopeValue::Static(_) => Self::Static,
            ModuleScopeValue::Module(_) => Self::Module,
            ModuleScopeValue::Function(_) | ModuleScopeValue::ExternalFunction(_) => Self::Function,
        }
    }
}

#[derive(Debug, Clone, Copy, Display)]
pub enum ScopeKind {
    #[display("trait")]
    Trait,
    #[display("type")]
    Type,
    #[display("function")]
    Function,
    #[display("global variable")]
    Static,
    #[display("module")]
    Module,
}
