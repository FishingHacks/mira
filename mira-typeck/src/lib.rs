use mira_errors::{Diagnostic, Diagnostics};
use mira_macros::Display;
use parking_lot::RwLock;
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::{Hash, Hasher},
    sync::{Arc, OnceLock},
};
use type_resolution::ResolvingState;

use ir::TypedLiteral;
use types::{FunctionType, resolve_primitive_type, with_refcount};

use crate::{ir::IR, lang_items::LangItems};
use mira_common::store::{AssociatedStore, Store, StoreKey, VecStore};
use mira_parser::{
    Trait, TypeRef,
    annotations::Annotations,
    module::{
        BakedStruct, ExternalFunction, Function, Module, ModuleContext, ModuleScopeValue, Static,
    },
};
use mira_spans::{
    ArenaList, Ident, SourceFile, Span,
    interner::{Symbol, symbols},
};

mod context;
mod lang_items;
pub mod optimizations;
pub use context::{GlobalContext, TypeCtx};
mod error;
pub mod intrinsics;
pub mod ir;
pub mod ir_displayer;
mod type_resolution;
pub mod typechecking;
mod types;
pub use error::{FunctionList, TypecheckingError, TypecheckingErrorDiagnosticsExt};
pub use types::{Ty, TyKind, TyList, TypeInterner, TypeListInterner, default_types};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedGeneric<'arena> {
    pub name: Ident<'arena>,
    pub sized: bool,
    pub bounds: ArenaList<'arena, StoreKey<TypedTrait<'arena>>>,
}

#[derive(Debug)]
pub struct TypecheckedFunctionContract<'arena> {
    pub name: Option<Ident<'arena>>,
    pub arguments: Vec<(Ident<'arena>, Ty<'arena>)>,
    pub return_type: Ty<'arena>,
    pub annotations: Annotations<'arena>,
    pub span: Span<'arena>,
    pub module_id: StoreKey<TypecheckedModule<'arena>>,
    pub generics: Vec<TypedGeneric<'arena>>,
}

impl Hash for TypecheckedFunctionContract<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
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
    )>,
    pub location: Span<'arena>,
    pub module_id: StoreKey<TypecheckedModule<'arena>>,
    pub id: StoreKey<TypedTrait<'arena>>,
    pub annotations: Annotations<'arena>,
}

#[derive(Debug)]
pub struct TypedStruct<'arena> {
    pub name: Ident<'arena>,
    pub elements: Vec<(Ident<'arena>, Ty<'arena>)>,
    pub span: Span<'arena>,
    pub global_impl: HashMap<Ident<'arena>, StoreKey<TypedFunction<'arena>>>,
    pub trait_impl: HashMap<StoreKey<TypedTrait<'arena>>, Vec<StoreKey<TypedFunction<'arena>>>>,
    pub annotations: Annotations<'arena>,
    pub module_id: StoreKey<TypecheckedModule<'arena>>,
    pub id: StoreKey<TypedStruct<'arena>>,
    pub generics: Vec<TypedGeneric<'arena>>,
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
    pub type_: Ty<'arena>,
    /// guaranteed to not be `Dynamic`, `Intrinsic` or `Static`
    pub value: TypedLiteral<'arena>,
    pub module_id: StoreKey<TypecheckedModule<'arena>>,
    pub loc: Span<'arena>,
    pub annotations: Annotations<'arena>,
    pub name: Ident<'arena>,
}

impl<'arena> TypedStatic<'arena> {
    pub fn new(
        type_: Ty<'arena>,
        literal: TypedLiteral<'arena>,
        module: StoreKey<TypecheckedModule<'arena>>,
        loc: Span<'arena>,
        annotations: Annotations<'arena>,
        name: Ident<'arena>,
    ) -> Self {
        Self {
            type_,
            value: literal,
            module_id: module,
            loc,
            annotations,
            name,
        }
    }
}

pub type TypedFunction<'arena> = (TypecheckedFunctionContract<'arena>, IR<'arena>);
pub type TypedExternalFunction<'arena> = (TypecheckedFunctionContract<'arena>, Option<IR<'arena>>);

#[allow(clippy::type_complexity)]
pub struct TypecheckingContext<'arena> {
    pub modules: RwLock<AssociatedStore<TypecheckedModule<'arena>, Module<'arena>>>,
    pub functions: RwLock<Store<TypedFunction<'arena>>>,
    pub external_functions: RwLock<Store<TypedExternalFunction<'arena>>>,
    pub statics: RwLock<Store<TypedStatic<'arena>>>,
    pub structs: RwLock<Store<TypedStruct<'arena>>>,
    pub traits: RwLock<Store<TypedTrait<'arena>>>,
    pub lang_items: RwLock<LangItems<'arena>>,
    pub main_function: OnceLock<StoreKey<TypedFunction<'arena>>>,
    pub ctx: TypeCtx<'arena>,
}

#[derive(Debug)]
pub struct TypecheckedModule<'arena> {
    pub scope: HashMap<Ident<'arena>, ModuleScopeValue<'arena>>,
    pub exports: HashSet<Ident<'arena>>,
    pub parent: StoreKey<TypecheckedModule<'arena>>,
    pub root_module: StoreKey<TypecheckedModule<'arena>>,
    pub name: Symbol<'arena>,
    pub file: Arc<SourceFile>,
    pub assembly: Vec<(Span<'arena>, String)>,
}

impl<'arena> TypecheckingContext<'arena> {
    pub fn validate_main_function(
        &self,
        main_pkg: StoreKey<Module<'arena>>,
    ) -> Result<StoreKey<TypedFunction<'arena>>, TypecheckingError<'arena>> {
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

    pub fn new(ctx: TypeCtx<'arena>, module_context: Arc<ModuleContext<'arena>>) -> Arc<Self> {
        let traits_reader = module_context.traits.read();
        let structs_reader = module_context.structs.read();
        let statics_reader = module_context.statics.read();
        let functions_reader = module_context.functions.read();
        let external_functions_reader = module_context.external_functions.read();

        let mut traits = AssociatedStore::<TypedTrait, Trait>::with_capacity(traits_reader.len());
        let mut structs =
            AssociatedStore::<TypedStruct, BakedStruct>::with_capacity(structs_reader.len());
        let mut statics =
            AssociatedStore::<TypedStatic, Static>::with_capacity(statics_reader.len());
        let mut functions =
            AssociatedStore::<TypedFunction, Function>::with_capacity(functions_reader.len());
        let mut external_functions =
            AssociatedStore::<TypedExternalFunction, ExternalFunction>::with_capacity(
                external_functions_reader.len(),
            );

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
                    statics_reader[key].3,
                    Annotations::default(),
                    Ident::EMPTY,
                ),
            );
        }

        for key in functions_reader.indices() {
            functions.insert(
                key,
                (
                    TypecheckedFunctionContract {
                        annotations: Annotations::default(),
                        name: None,
                        arguments: Vec::new(),
                        return_type: default_types::never,
                        span: functions_reader[key].0.span,
                        module_id: StoreKey::undefined(),
                        generics: Vec::new(),
                    },
                    IR::new(Vec::new(), VecStore::new()),
                ),
            );
        }

        for key in external_functions_reader.indices() {
            external_functions.insert(
                key,
                (
                    TypecheckedFunctionContract {
                        annotations: Annotations::default(),
                        name: None,
                        arguments: Vec::new(),
                        return_type: default_types::never,
                        span: external_functions_reader[key].0.span,
                        module_id: StoreKey::undefined(),
                        generics: Vec::new(),
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
                    location: traits_reader[key].span,
                    module_id: StoreKey::undefined(),
                    id: key.cast(),
                    annotations: Annotations::default(),
                },
            );
        }

        let me = Arc::new(Self {
            ctx,
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
                TypecheckedModule {
                    scope,
                    parent: module.parent.cast(),
                    root_module: module.package_root.cast(),
                    name: module.name,
                    exports: module.exports.clone(),
                    file: module.file.clone(),
                    assembly: module.assembly.clone(),
                },
            );
        }

        drop(module_reader);
        drop(typechecked_module_writer);

        me
    }

    pub fn resolve_imports(&self, context: Arc<ModuleContext<'arena>>) -> Diagnostics<'arena> {
        let mut errors = Diagnostics::new();
        let mut typechecked_module_writer = self.modules.write();
        let module_reader = context.modules.read();
        for key in module_reader.indices() {
            for (name, (location, import)) in module_reader[key].imports.iter() {
                let res = resolve_import(
                    &context,
                    key,
                    &import.entries,
                    *location,
                    &mut HashSet::new(),
                );
                match res {
                    Err(e) => _ = errors.add(e),
                    Ok(k) => {
                        typechecked_module_writer[key.cast()].scope.insert(*name, k);
                    }
                }
            }
        }

        errors
    }

    pub fn resolve_type(
        &self,
        module_id: StoreKey<TypecheckedModule<'arena>>,
        typ: &TypeRef<'arena>,
        generics: &[TypedGeneric<'arena>],
    ) -> Result<Ty<'arena>, Diagnostic<'arena>> {
        if let Some(primitive) = resolve_primitive_type(self.ctx, typ) {
            return Ok(primitive);
        }

        match typ {
            TypeRef::DynReference {
                traits,
                num_references,
                span: loc,
            } => {
                let mut trait_refs = Vec::with_capacity(traits.len());
                for trait_name in traits.iter() {
                    let v = typed_resolve_import(
                        self,
                        module_id,
                        trait_name.as_slice(),
                        loc,
                        &mut HashSet::new(),
                    );
                    let Ok(ModuleScopeValue::Trait(id)) = v else {
                        return Err(
                            TypecheckingError::CannotFindTrait(*loc, trait_name.clone()).to_error()
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
                span: loc,
            } => {
                if type_name.entries.len() == 1 && type_name.entries[0].1.is_empty() {
                    if let Some((id, generic)) = generics
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
                }

                let path = type_name.entries.iter().map(|v| v.0).collect::<Vec<_>>();
                // NOTE: this should only have a generic at the end as this is a type
                // (std::vec::Vec, can never be std::vec::Vec<u32>::Vec.)
                for (.., generics) in type_name.entries.iter() {
                    if !generics.is_empty() {
                        return Err(
                            TypecheckingError::UnexpectedGenerics { location: *loc }.to_error()
                        );
                    }
                }

                match typed_resolve_import(self, module_id, &path, loc, &mut HashSet::new())? {
                    ModuleScopeValue::Struct(id) => Ok(with_refcount(
                        self.ctx,
                        self.ctx.intern_ty(TyKind::Struct {
                            struct_id: id.cast(),
                            name: self.structs.read()[id.cast()].name,
                        }),
                        *num_references,
                    )),
                    v => Err(TypecheckingError::MismatchingScopeType {
                        location: *loc,
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
                    typ: self.resolve_type(module_id, child, generics)?,
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
        context: Arc<ModuleContext<'arena>>,
        id: StoreKey<BakedStruct<'arena>>,
        module_id: StoreKey<Module<'arena>>,
        errors: &mut Diagnostics<'arena>,
        left: &mut HashMap<StoreKey<BakedStruct<'arena>>, ResolvingState>,
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

            for (bound, loc) in &generic.bounds {
                match resolve_import(
                    &context,
                    module_id,
                    &bound.entries,
                    *loc,
                    &mut HashSet::new(),
                ) {
                    Err(e) => _ = errors.add(e),
                    Ok(ModuleScopeValue::Trait(trait_id)) => bounds.push(trait_id.cast()),
                    Ok(_) => {
                        errors.add_unbound_ident(
                            *loc,
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
        };
        drop(writer);

        for element in elements {
            if let Some(typ) = self.type_resolution_resolve_type(
                &element.1,
                &typed_struct.generics,
                module_id.cast(),
                context.clone(),
                errors,
                left,
            ) {
                typed_struct.elements.push((element.0, typ));
            }
        }
        self.structs.write()[id.cast()] = typed_struct;

        left.remove(&id);
        false
    }

    fn type_resolution_resolve_type(
        &self,
        typ: &TypeRef<'arena>,
        generics: &[TypedGeneric<'arena>],
        module: StoreKey<TypecheckedModule<'arena>>,
        context: Arc<ModuleContext<'arena>>,
        errors: &mut Diagnostics<'arena>,
        left: &mut HashMap<StoreKey<BakedStruct<'arena>>, ResolvingState>,
    ) -> Option<Ty<'arena>> {
        if let Some(typ) = resolve_primitive_type(self.ctx, typ) {
            return Some(typ);
        }
        match typ {
            TypeRef::DynReference {
                traits,
                num_references,
                span,
            } => {
                let mut trait_refs = Vec::with_capacity(traits.len());
                for trait_name in traits.iter() {
                    let v = typed_resolve_import(
                        self,
                        module,
                        trait_name.as_slice(),
                        span,
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
                    errors,
                    left,
                )?;
                let mut arguments = Vec::with_capacity(args.len());
                for arg in args {
                    arguments.push(self.type_resolution_resolve_type(
                        arg,
                        generics,
                        module,
                        context.clone(),
                        errors,
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
                span: loc,
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
                    resolve_import(&context, module.cast(), &path, *loc, &mut HashSet::new())
                else {
                    errors.add_unbound_ident(*loc, path[path.len() - 1].symbol());
                    return None;
                };

                let ModuleScopeValue::Struct(id) = value else {
                    errors.add_mismatching_scope_type(*loc, ScopeKind::Type, value.into());
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
                if self.resolve_struct(context, id, module, errors, left) {
                    errors.add_recursive_type_detected(*loc);
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
                self.ctx
                    .intern_ty(TyKind::UnsizedArray(self.type_resolution_resolve_type(
                        child, generics, module, context, errors, left,
                    )?)),
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
                    typ: self.type_resolution_resolve_type(
                        child, generics, module, context, errors, left,
                    )?,
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
                        errors,
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
}

fn typed_resolve_import<'arena>(
    context: &TypecheckingContext<'arena>,
    module: StoreKey<TypecheckedModule<'arena>>,
    import: &[Ident<'arena>],
    location: &Span<'arena>,
    already_included: &mut HashSet<(StoreKey<TypecheckedModule<'arena>>, Symbol<'arena>)>,
) -> Result<ModuleScopeValue<'arena>, Diagnostic<'arena>> {
    if import.is_empty() {
        return Ok(ModuleScopeValue::Module(module.cast()));
    }
    if already_included.contains(&(module, import[0].symbol())) {
        return Err(TypecheckingError::CyclicDependency(*location).to_error());
    }
    already_included.insert((module, import[0].symbol()));

    let reader = context.modules.read();
    let ident = match reader[module.cast()].exports.get(&import[0]) {
        Some(ident) => ident,
        None if already_included.len() < 2 /* this is the module it was imported from */ => &import[0],
        None => return Err(TypecheckingError::ItemNotFound {
            location: *location,
            name: import[0].symbol(),
        }.to_error()),
    };

    if let Some(value) = reader[module.cast()].scope.get(ident).copied() {
        if import.len() < 2 {
            return Ok(value);
        }
        match value {
            ModuleScopeValue::Struct(id) => {
                let reader = context.structs.read();
                if let Some(function_id) = reader[id.cast()].global_impl.get(&import[1]).copied() {
                    if import.len() < 3 {
                        return Ok(ModuleScopeValue::Function(function_id.cast()));
                    }
                    return Err(TypecheckingError::ItemNotFound {
                        location: *location,
                        name: import[2].symbol(),
                    }
                    .to_error());
                } else {
                    return Err(TypecheckingError::ItemNotFound {
                        location: reader[id.cast()].span,
                        name: import[1].symbol(),
                    }
                    .to_error());
                }
            }
            ModuleScopeValue::Module(_) => unreachable!(), // all modules must have been imports
            ModuleScopeValue::Function(_)
            | ModuleScopeValue::ExternalFunction(_)
            | ModuleScopeValue::Trait(_)
            | ModuleScopeValue::Static(_) => {
                return Err(TypecheckingError::ItemNotFound {
                    location: *location,
                    name: import[1].symbol(),
                }
                .to_error());
            }
        }
    }
    Err(TypecheckingError::ItemNotFound {
        location: *location,
        name: import[0].symbol(),
    }
    .to_error())
}

fn resolve_import<'arena>(
    context: &ModuleContext<'arena>,
    current_module: StoreKey<Module<'arena>>,
    import: &[Ident<'arena>],
    location: Span<'arena>,
    already_included: &mut HashSet<(StoreKey<Module<'arena>>, Symbol<'arena>)>,
) -> Result<ModuleScopeValue<'arena>, Diagnostic<'arena>> {
    assert!(!import.is_empty());
    if !already_included.insert((current_module, import[0].symbol())) {
        return Err(TypecheckingError::CyclicDependency(location).to_error());
    }

    let module_reader = context.modules.read();
    let cur_mod = &module_reader[current_module];
    let thing;
    if &*import[0] == "pkg" {
        thing = ModuleScopeValue::Module(cur_mod.package_root);
    } else if let Some(value) = cur_mod.scope.get(&import[0].symbol()) {
        thing = *value;
    } else if let Some((location, import)) = cur_mod.imports.get(&import[0].symbol()) {
        thing = resolve_import(
            context,
            current_module,
            &import.entries,
            *location,
            already_included,
        )?
    } else if let Some(package) = context.dependencies[cur_mod.package_root].get(&*import[0]) {
        thing = ModuleScopeValue::Module(*package);
    } else {
        return Err(TypecheckingError::ItemNotFound {
            location,
            name: import[0].symbol(),
        }
        .to_error());
    }

    if import.len() == 1 {
        return Ok(thing);
    }
    match thing {
        ModuleScopeValue::Module(module) => {
            resolve_import(context, module, &import[1..], location, already_included)
        }

        _ => Err(TypecheckingError::ItemNotFound {
            location: import[1].span(),
            name: import[1].symbol(),
        }
        .to_error()),
    }
}

impl From<ModuleScopeValue<'_>> for ScopeKind {
    fn from(value: ModuleScopeValue) -> Self {
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
