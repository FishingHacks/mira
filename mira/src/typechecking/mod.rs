use parking_lot::RwLock;
use std::{
    collections::HashMap,
    fmt::Debug,
    hash::{Hash, Hasher},
    path::Path,
    sync::Arc,
};
use type_resolution::ResolvingState;

use expression::{TypecheckedExpression, TypedLiteral};
use types::{resolve_primitive_type, FunctionType};

use crate::{
    annotations::Annotations,
    context::SharedContext,
    interner::InternedStr,
    lang_items::LangItems,
    module::{
        BakedStruct, ExternalFunction, Function, Module, ModuleContext, ModuleScopeValue, Static,
    },
    parser::{Trait, TypeRef},
    store::{AssociatedStore, Store, StoreKey},
    tokenizer::span::Span,
};

mod error;
pub mod expression;
pub mod intrinsics;
pub mod ir_displayer;
mod type_resolution;
#[allow(clippy::module_inception)]
pub mod typechecking;
mod types;
pub use error::TypecheckingError;
pub use types::Type;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedGeneric<'arena> {
    pub name: InternedStr<'arena>,
    pub sized: bool,
    pub bounds: Vec<StoreKey<TypedTrait<'arena>>>,
}

#[derive(Debug)]
pub struct TypecheckedFunctionContract<'arena> {
    pub name: Option<InternedStr<'arena>>,
    pub arguments: Vec<(InternedStr<'arena>, Type<'arena>)>,
    pub return_type: Type<'arena>,
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
    pub name: InternedStr<'arena>,
    pub functions: Vec<(
        InternedStr<'arena>,
        Vec<(InternedStr<'arena>, Type<'arena>)>,
        Type<'arena>,
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
    pub name: InternedStr<'arena>,
    pub elements: Vec<(InternedStr<'arena>, Type<'arena>)>,
    pub span: Span<'arena>,
    pub global_impl: HashMap<InternedStr<'arena>, StoreKey<TypedFunction<'arena>>>,
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
    pub type_: Type<'arena>,
    /// guaranteed to not be `Dynamic`, `Intrinsic` or `Static`
    pub value: TypedLiteral<'arena>,
    pub module: StoreKey<TypecheckedModule<'arena>>,
    pub loc: Span<'arena>,
    pub annotations: Annotations<'arena>,
}

impl<'arena> TypedStatic<'arena> {
    pub fn new(
        type_: Type<'arena>,
        literal: TypedLiteral<'arena>,
        module: StoreKey<TypecheckedModule<'arena>>,
        loc: Span<'arena>,
        annotations: Annotations<'arena>,
    ) -> Self {
        Self {
            type_,
            value: literal,
            module,
            loc,
            annotations,
        }
    }
}

pub type TypedFunction<'arena> = (
    TypecheckedFunctionContract<'arena>,
    Box<[TypecheckedExpression<'arena>]>,
);
pub type TypedExternalFunction<'arena> = (
    TypecheckedFunctionContract<'arena>,
    Option<Box<[TypecheckedExpression<'arena>]>>,
);

#[derive(Debug)]
#[allow(clippy::type_complexity)]
pub struct TypecheckingContext<'arena> {
    pub modules: RwLock<AssociatedStore<TypecheckedModule<'arena>, Module<'arena>>>,
    pub functions: RwLock<Store<TypedFunction<'arena>>>,
    pub external_functions: RwLock<Store<TypedExternalFunction<'arena>>>,
    pub statics: RwLock<Store<TypedStatic<'arena>>>,
    pub structs: RwLock<Store<TypedStruct<'arena>>>,
    pub traits: RwLock<Store<TypedTrait<'arena>>>,
    pub lang_items: RwLock<LangItems<'arena>>,
}

pub struct TypecheckedModule<'arena> {
    scope: HashMap<InternedStr<'arena>, ModuleScopeValue<'arena>>,
    exports: HashMap<InternedStr<'arena>, InternedStr<'arena>>,
    pub path: Arc<Path>,
    pub root: Arc<Path>,
    pub assembly: Vec<(Span<'arena>, String)>,
}

impl Debug for TypecheckedModule<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TypecheckedModule")
            .field("scope", &self.scope)
            .finish()
    }
}

impl<'arena> TypecheckingContext<'arena> {
    pub fn new(
        ctx: SharedContext<'arena>,
        module_context: Arc<ModuleContext<'arena>>,
    ) -> Arc<Self> {
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
                    name: InternedStr::EMPTY,
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
                    Type::PrimitiveNever,
                    TypedLiteral::Void,
                    StoreKey::undefined(),
                    statics_reader[key].3,
                    Annotations::default(),
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
                        return_type: Type::PrimitiveNever,
                        span: functions_reader[key].0.span,
                        module_id: StoreKey::undefined(),
                        generics: Vec::new(),
                    },
                    vec![].into_boxed_slice(),
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
                        return_type: Type::PrimitiveNever,
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
                    name: InternedStr::EMPTY,
                    functions: Vec::new(),
                    location: traits_reader[key].span,
                    module_id: StoreKey::undefined(),
                    id: key.cast(),
                    annotations: Annotations::default(),
                },
            );
        }

        let me = Arc::new(Self {
            structs: RwLock::new(structs.into()),
            statics: RwLock::new(statics.into()),
            functions: RwLock::new(functions.into()),
            traits: RwLock::new(traits.into()),
            external_functions: RwLock::new(external_functions.into()),
            modules: Default::default(),
            lang_items: RwLock::new(LangItems::new(ctx)),
        });

        let mut typechecked_module_writer = me.modules.write();
        let module_reader = module_context.modules.read();

        for key in module_reader.indices() {
            let scope = module_reader[key].scope.clone();

            typechecked_module_writer.insert(
                key,
                TypecheckedModule {
                    scope,
                    exports: module_reader[key].exports.clone(),
                    path: module_reader[key].path.clone(),
                    root: module_reader[key].root.clone(),
                    assembly: module_reader[key].assembly.clone(),
                },
            );
        }

        drop(module_reader);
        drop(typechecked_module_writer);

        me
    }

    pub fn resolve_imports(
        &self,
        context: Arc<ModuleContext<'arena>>,
    ) -> Vec<TypecheckingError<'arena>> {
        let mut errors = vec![];
        let mut typechecked_module_writer = self.modules.write();
        let module_reader = context.modules.read();
        for key in module_reader.indices() {
            for (name, (location, module_id, path)) in module_reader[key].imports.iter() {
                match resolve_import(
                    &context,
                    *module_id,
                    path,
                    location,
                    &mut vec![(key, InternedStr::EMPTY)],
                ) {
                    Err(e) => errors.push(e),
                    Ok(k) => {
                        typechecked_module_writer[key.cast()].scope.insert(*name, k);
                    }
                }
            }
        }

        errors
    }

    #[allow(clippy::result_large_err)]
    pub fn resolve_type(
        &self,
        module_id: StoreKey<TypecheckedModule<'arena>>,
        typ: &TypeRef<'arena>,
        generics: &[TypedGeneric<'arena>],
    ) -> Result<Type<'arena>, TypecheckingError<'arena>> {
        if let Some(primitive) = resolve_primitive_type(typ) {
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
                        &mut Vec::new(),
                    );
                    let Ok(ModuleScopeValue::Trait(id)) = v else {
                        return Err(TypecheckingError::CannotFindTrait(*loc, trait_name.clone()));
                    };
                    trait_refs.push((id.cast(), *trait_name.as_slice().last().unwrap()));
                }
                Ok(Type::DynType {
                    trait_refs,
                    num_references: *num_references,
                })
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
                let function_typ = FunctionType {
                    arguments,
                    return_type,
                };
                Ok(Type::Function(function_typ.into(), *num_references))
            }
            TypeRef::Reference {
                num_references,
                type_name,
                span: loc,
            } => {
                if type_name.entries.len() == 1 && type_name.entries[0].2.is_empty() {
                    if let Some((id, generic)) = generics
                        .iter()
                        .enumerate()
                        .find(|(_, v)| v.name == type_name.entries[0].0)
                    {
                        return Ok(Type::Generic {
                            name: type_name.entries[0].0,
                            generic_id: id as u8,
                            num_references: *num_references,
                            sized: generic.sized,
                            bounds: generic.bounds.clone(),
                        });
                    }
                }

                let path = type_name.entries.iter().map(|v| v.0).collect::<Vec<_>>();
                // NOTE: this should only have a generic at the end as this is a type
                // (std::vec::Vec, can never be std::vec::Vec<u32>::Vec.)
                for (.., generics) in type_name.entries.iter() {
                    if !generics.is_empty() {
                        return Err(TypecheckingError::UnexpectedGenerics { location: *loc });
                    }
                }

                match typed_resolve_import(self, module_id, &path, loc, &mut Vec::new())? {
                    ModuleScopeValue::Struct(id) => Ok(Type::Struct {
                        struct_id: id.cast(),
                        name: self.structs.read()[id.cast()].name,
                        num_references: *num_references,
                    }),
                    v => Err(TypecheckingError::MismatchingScopeType {
                        location: *loc,
                        expected: ScopeKind::Type,
                        found: v.into(),
                    }),
                }
            }
            TypeRef::Void(..) | TypeRef::Never(_) => unreachable!(),
            TypeRef::UnsizedArray {
                num_references,
                child,
                span: _,
            } => Ok(Type::UnsizedArray {
                typ: Box::new(self.resolve_type(module_id, child, generics)?),
                num_references: *num_references,
            }),
            TypeRef::SizedArray {
                num_references,
                child,
                number_elements,
                ..
            } => Ok(Type::SizedArray {
                typ: Box::new(self.resolve_type(module_id, child, generics)?),
                num_references: *num_references,
                number_elements: *number_elements,
            }),
            TypeRef::Tuple {
                num_references,
                elements,
                ..
            } => {
                let mut typed_elements = Vec::with_capacity(elements.len());
                for elem in elements.iter() {
                    typed_elements.push(self.resolve_type(module_id, elem, generics)?);
                }
                Ok(Type::Tuple {
                    num_references: *num_references,
                    elements: typed_elements,
                })
            }
        }
    }

    /// returns if a recursive field was detected
    fn resolve_struct(
        &self,
        context: Arc<ModuleContext<'arena>>,
        id: StoreKey<BakedStruct<'arena>>,
        module_id: StoreKey<Module<'arena>>,
        errors: &mut Vec<TypecheckingError<'arena>>,
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
                match resolve_import(&context, module_id, &bound.entries, loc, &mut Vec::new()) {
                    Err(e) => errors.push(e),
                    Ok(ModuleScopeValue::Trait(trait_id)) => bounds.push(trait_id.cast()),
                    Ok(_) => errors.push(TypecheckingError::UnboundIdent {
                        location: *loc,
                        name: bound.entries[bound.entries.len() - 1],
                    }),
                }
            }

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
        errors: &mut Vec<TypecheckingError<'arena>>,
        left: &mut HashMap<StoreKey<BakedStruct<'arena>>, ResolvingState>,
    ) -> Option<Type<'arena>> {
        if let Some(typ) = resolve_primitive_type(typ) {
            return Some(typ);
        }
        match typ {
            TypeRef::DynReference { .. } => todo!(),
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
                Some(Type::Function(
                    Arc::new(FunctionType {
                        return_type,
                        arguments,
                    }),
                    *num_references,
                ))
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
                if type_name.entries.len() == 1 && type_name.entries[0].2.is_empty() {
                    let name = &type_name.entries[0].0;
                    if let Some((generic_id, generic)) =
                        generics.iter().enumerate().find(|(_, v)| v.name == *name)
                    {
                        return Some(Type::Generic {
                            name: generic.name,
                            generic_id: generic_id as u8,
                            num_references: 0,
                            sized: generic.sized,
                            bounds: generic.bounds.clone(),
                        });
                    }
                }

                let Ok(value) =
                    resolve_import(&context, module.cast(), &path, loc, &mut Vec::new())
                else {
                    errors.push(TypecheckingError::UnboundIdent {
                        location: *loc,
                        name: path[path.len() - 1],
                    });
                    return None;
                };

                let ModuleScopeValue::Struct(id) = value else {
                    errors.push(TypecheckingError::MismatchingScopeType {
                        location: *loc,
                        expected: ScopeKind::Type,
                        found: value.into(),
                    });
                    return None;
                };

                {
                    if !left.contains_key(&id) {
                        let typechecked_struct = &self.structs.read()[id.cast()];
                        return Some(Type::Struct {
                            struct_id: typechecked_struct.id,
                            name: typechecked_struct.name,
                            num_references: *num_references,
                        });
                    }
                }

                let module = context.structs.read()[id].module_id;
                if self.resolve_struct(context, id, module, errors, left) {
                    errors.push(TypecheckingError::RecursiveTypeDetected { location: *loc });
                    return None;
                }
                let typechecked_struct = &self.structs.read()[id.cast()];
                Some(Type::Struct {
                    struct_id: typechecked_struct.id,
                    num_references: *num_references,
                    name: typechecked_struct.name,
                })
            }
            TypeRef::Void(..) => unreachable!(),
            TypeRef::Never(_) => unreachable!(),
            TypeRef::UnsizedArray {
                num_references,
                child,
                span: _,
            } => Some(Type::UnsizedArray {
                typ: Box::new(self.type_resolution_resolve_type(
                    child, generics, module, context, errors, left,
                )?),
                num_references: *num_references,
            }),
            TypeRef::SizedArray {
                num_references,
                child,
                number_elements,
                span: _,
            } => Some(Type::SizedArray {
                typ: Box::new(self.type_resolution_resolve_type(
                    child, generics, module, context, errors, left,
                )?),
                num_references: *num_references,
                number_elements: *number_elements,
            }),
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
                Some(Type::Tuple {
                    elements: typed_elements,
                    num_references: *num_references,
                })
            }
        }
    }
}

#[allow(clippy::result_large_err)]
fn typed_resolve_import<'arena>(
    context: &TypecheckingContext<'arena>,
    module: StoreKey<TypecheckedModule<'arena>>,
    import: &[InternedStr<'arena>],
    location: &Span<'arena>,
    already_included: &mut Vec<(StoreKey<TypecheckedModule<'arena>>, InternedStr<'arena>)>,
) -> Result<ModuleScopeValue<'arena>, TypecheckingError<'arena>> {
    if import.is_empty() {
        return Ok(ModuleScopeValue::Module(module.cast()));
    }
    if already_included
        .iter()
        .any(|(mod_id, imp)| module.eq(mod_id) && import[0].eq(imp))
    {
        return Err(TypecheckingError::CyclicDependency {
            location: *location,
        });
    }
    already_included.push((module, import[0]));

    let reader = context.modules.read();
    let ident = match reader[module.cast()].exports.get(&import[0]) {
        Some(ident) => ident,
        None if already_included.len() < 2 /* this is the module it was imported from */ => &import[0],
        None => return Err(TypecheckingError::ExportNotFound {
            location: *location,
            name: import[0],
        }),
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
                    return Err(TypecheckingError::ExportNotFound {
                        location: *location,
                        name: import[2],
                    });
                } else {
                    return Err(TypecheckingError::ExportNotFound {
                        location: reader[id.cast()].span,
                        name: import[1],
                    });
                }
            }
            ModuleScopeValue::Module(_) => unreachable!(), // all modules must have been imports
            ModuleScopeValue::Function(_)
            | ModuleScopeValue::ExternalFunction(_)
            | ModuleScopeValue::Trait(_)
            | ModuleScopeValue::Static(_) => {
                return Err(TypecheckingError::ExportNotFound {
                    location: *location,
                    name: import[1],
                })
            }
        }
    }
    Err(TypecheckingError::ExportNotFound {
        location: *location,
        name: import[0],
    })
}

#[allow(clippy::result_large_err)]
fn resolve_import<'arena>(
    context: &ModuleContext<'arena>,
    module: StoreKey<Module<'arena>>,
    import: &[InternedStr<'arena>],
    location: &Span<'arena>,
    already_included: &mut Vec<(StoreKey<Module<'arena>>, InternedStr<'arena>)>,
) -> Result<ModuleScopeValue<'arena>, TypecheckingError<'arena>> {
    if import.is_empty() {
        return Ok(ModuleScopeValue::Module(module));
    }
    if already_included
        .iter()
        .any(|(mod_id, imp)| module.eq(mod_id) && import[0].eq(imp))
    {
        return Err(TypecheckingError::CyclicDependency {
            location: *location,
        });
    }
    already_included.push((module, import[0]));

    let reader = context.modules.read();
    let ident = match reader[module].exports.get(&import[0]) {
        Some(ident) => ident,
        None if already_included.len() < 2 /* this is the module it was imported from */ => &import[0],
        None => return Err(TypecheckingError::ExportNotFound {
            location: *location,
            name: import[0],
        }),
    };

    if let Some((sub_location, module, path)) = reader[module].imports.get(ident) {
        let value = resolve_import(context, *module, path, sub_location, already_included)?;
        if import.len() < 2 {
            return Ok(value);
        }

        match value {
            ModuleScopeValue::Module(id) => {
                return resolve_import(context, id, &import[1..], location, already_included)
            }
            ModuleScopeValue::Struct(id) => {
                let reader = context.structs.read();
                if let Some(function_id) = reader[id].global_impl.get(&import[1]).copied() {
                    if import.len() < 3 {
                        return Ok(ModuleScopeValue::Function(function_id));
                    }
                    return Err(TypecheckingError::ExportNotFound {
                        location: context.functions.read()[function_id].0.span,
                        name: import[2],
                    });
                } else {
                    return Err(TypecheckingError::ExportNotFound {
                        location: reader[id].span,
                        name: import[1],
                    });
                }
            }
            ModuleScopeValue::Function(_)
            | ModuleScopeValue::ExternalFunction(_)
            | ModuleScopeValue::Trait(_)
            | ModuleScopeValue::Static(_) => {
                return Err(TypecheckingError::ExportNotFound {
                    location: *location,
                    name: import[1],
                })
            }
        }
    }
    if let Some(value) = reader[module].scope.get(ident).copied() {
        if import.len() < 2 {
            return Ok(value);
        }
        match value {
            ModuleScopeValue::Struct(id) => {
                let reader = context.structs.read();
                if let Some(function_id) = reader[id].global_impl.get(&import[1]).copied() {
                    if import.len() < 3 {
                        return Ok(ModuleScopeValue::Function(function_id));
                    }
                    return Err(TypecheckingError::ExportNotFound {
                        location: *location,
                        name: import[2],
                    });
                } else {
                    return Err(TypecheckingError::ExportNotFound {
                        location: reader[id].span,
                        name: import[1],
                    });
                }
            }
            ModuleScopeValue::Module(_) => unreachable!(), // all modules must have been imports
            ModuleScopeValue::Function(_)
            | ModuleScopeValue::ExternalFunction(_)
            | ModuleScopeValue::Trait(_)
            | ModuleScopeValue::Static(_) => {
                return Err(TypecheckingError::ExportNotFound {
                    location: *location,
                    name: import[1],
                })
            }
        }
    }
    Err(TypecheckingError::ExportNotFound {
        location: *location,
        name: import[0],
    })
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

#[derive(Debug, Clone, Copy)]
pub enum ScopeKind {
    Trait,
    Type,
    Function,
    Static,
    Module,
}
