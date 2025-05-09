use parking_lot::RwLock;
use std::{
    collections::HashMap,
    fmt::Debug,
    hash::{Hash, Hasher},
    path::{Path, PathBuf},
    sync::{Arc, LazyLock},
};

use expression::{TypecheckedExpression, TypedLiteral};
use types::{resolve_primitive_type, FunctionType};

use crate::{
    annotations::Annotations,
    globals::GlobalStr,
    lang_items::LangItems,
    module::{
        BakedStruct, ExternalFunction, Function, Module, ModuleContext, ModuleScopeValue, Static,
    },
    parser::{Trait, TypeRef},
    store::{AssociatedStore, Store, StoreKey},
    tokenizer::Location,
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

pub static DUMMY_LOCATION: LazyLock<Location> = LazyLock::new(|| Location {
    line: 0,
    column: 0,
    file: PathBuf::from("").into(), // a file should never be a folder :3
});

#[derive(Debug)]
pub struct TypedGeneric {
    name: GlobalStr,
    sized: bool,
    bounds: Vec<StoreKey<TypedTrait>>,
}

#[derive(Debug)]
pub struct TypecheckedFunctionContract {
    pub name: Option<GlobalStr>,
    pub arguments: Vec<(GlobalStr, Type)>,
    pub return_type: Type,
    pub annotations: Annotations,
    pub location: Location,
    pub module_id: StoreKey<TypecheckedModule>,
    pub generics: Vec<TypedGeneric>,
}

impl Hash for TypecheckedFunctionContract {
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
pub struct TypedTrait {
    pub name: GlobalStr,
    pub functions: Vec<(
        GlobalStr,
        Vec<(GlobalStr, Type)>,
        Type,
        Annotations,
        Location,
    )>,
    pub location: Location,
    pub module_id: StoreKey<TypecheckedModule>,
    pub id: StoreKey<TypedTrait>,
    pub annotations: Annotations,
}

#[derive(Debug)]
pub struct TypedStruct {
    pub name: GlobalStr,
    pub elements: Vec<(GlobalStr, Type)>,
    pub location: Location,
    pub global_impl: HashMap<GlobalStr, StoreKey<TypedFunction>>,
    pub trait_impl: HashMap<StoreKey<TypedTrait>, Vec<StoreKey<TypedFunction>>>,
    pub annotations: Annotations,
    pub module_id: StoreKey<TypecheckedModule>,
    pub id: StoreKey<TypedStruct>,
    pub generics: Vec<(GlobalStr, Vec<StoreKey<TypedTrait>>)>,
}

impl Hash for TypedStruct {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.elements.hash(state);
        self.module_id.hash(state);
    }
}

pub type TypedStatic = (
    Type,
    TypedLiteral, /* guaranteed to not be `Dynamic`, `Intrinsic` or `Static` */
    StoreKey<TypecheckedModule>,
    Location,
    Annotations,
);
pub type TypedFunction = (TypecheckedFunctionContract, Box<[TypecheckedExpression]>);
pub type TypedExternalFunction = (
    TypecheckedFunctionContract,
    Option<Box<[TypecheckedExpression]>>,
);

#[derive(Debug)]
#[allow(clippy::type_complexity)]
pub struct TypecheckingContext {
    pub modules: RwLock<AssociatedStore<TypecheckedModule, Module>>,
    pub functions: RwLock<Store<TypedFunction>>,
    pub external_functions: RwLock<Store<TypedExternalFunction>>,
    pub statics: RwLock<Store<TypedStatic>>,
    pub structs: RwLock<Store<TypedStruct>>,
    pub traits: RwLock<Store<TypedTrait>>,
    pub lang_items: RwLock<LangItems>,
}

pub struct TypecheckedModule {
    scope: HashMap<GlobalStr, ModuleScopeValue>,
    exports: HashMap<GlobalStr, GlobalStr>,
    pub path: Arc<Path>,
    pub root: Arc<Path>,
    pub assembly: Vec<(Location, String)>,
}

impl Debug for TypecheckedModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TypecheckedModule")
            .field("scope", &self.scope)
            .finish()
    }
}

impl TypecheckingContext {
    pub fn new(context: Arc<ModuleContext>) -> Arc<Self> {
        let traits_reader = context.traits.read();
        let structs_reader = context.structs.read();
        let statics_reader = context.statics.read();
        let functions_reader = context.functions.read();
        let external_functions_reader = context.external_functions.read();

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
                    name: GlobalStr::ZERO,
                    elements: Vec::new(),
                    location: DUMMY_LOCATION.clone(),
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
                (
                    Type::PrimitiveNever,
                    TypedLiteral::Void,
                    StoreKey::undefined(),
                    DUMMY_LOCATION.clone(),
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
                        location: DUMMY_LOCATION.clone(),
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
                        location: DUMMY_LOCATION.clone(),
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
                    name: GlobalStr::ZERO,
                    functions: Vec::new(),
                    location: DUMMY_LOCATION.clone(),
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
            lang_items: RwLock::new(LangItems::default()),
        });

        let mut typechecked_module_writer = me.modules.write();
        let module_reader = context.modules.read();

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

    pub fn resolve_imports(&self, context: Arc<ModuleContext>) -> Vec<TypecheckingError> {
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
                    &mut vec![(key, GlobalStr::ZERO)],
                ) {
                    Err(e) => errors.push(e),
                    Ok(k) => {
                        typechecked_module_writer[key.cast()]
                            .scope
                            .insert(name.clone(), k);
                    }
                }
            }
        }

        errors
    }

    pub fn resolve_type(
        &self,
        module_id: StoreKey<TypecheckedModule>,
        typ: &TypeRef,
        generics: &[GlobalStr],
    ) -> Result<Type, TypecheckingError> {
        if let Some(primitive) = resolve_primitive_type(typ) {
            return Ok(primitive);
        }

        match typ {
            TypeRef::DynReference {
                traits,
                num_references,
                loc,
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
                        return Err(TypecheckingError::CannotFindTrait(
                            loc.clone(),
                            trait_name.clone(),
                        ));
                    };
                    trait_refs.push((id.cast(), trait_name.as_slice().last().unwrap().clone()));
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
                loc,
            } => {
                if type_name.entries.len() == 1
                    && type_name.entries[0].1.is_empty()
                    && generics.contains(&type_name.entries[0].0)
                {
                    return Ok(Type::Generic {
                        name: type_name.entries[0].0.clone(),
                        generic_id: generics
                            .iter()
                            .position(|v| *v == type_name.entries[0].0)
                            .expect("typename should be in generics because we just checked that")
                            as u8,
                        num_references: *num_references,
                    });
                }

                let path = type_name
                    .entries
                    .iter()
                    .map(|v| v.0.clone())
                    .collect::<Vec<_>>();
                // NOTE: this should only have a generic at the end as this is a type
                // (std::vec::Vec, can never be std::vec::Vec<u32>::Vec.)
                for (_, generics) in type_name.entries.iter() {
                    if !generics.is_empty() {
                        return Err(TypecheckingError::UnexpectedGenerics {
                            location: loc.clone(),
                        });
                    }
                }

                match typed_resolve_import(self, module_id, &path, loc, &mut Vec::new())? {
                    ModuleScopeValue::Struct(id) => Ok(Type::Struct {
                        struct_id: id.cast(),
                        name: self.structs.read()[id.cast()].name.clone(),
                        num_references: *num_references,
                    }),
                    v => Err(TypecheckingError::MismatchingScopeType {
                        location: loc.clone(),
                        expected: ScopeKind::Type,
                        found: v.into(),
                    }),
                }
            }
            TypeRef::Void(..) | TypeRef::Never(_) => unreachable!(),
            TypeRef::UnsizedArray {
                num_references,
                child,
                loc: _,
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
        context: Arc<ModuleContext>,
        id: StoreKey<BakedStruct>,
        module_id: StoreKey<Module>,
        errors: &mut Vec<TypecheckingError>,
    ) -> bool {
        if DUMMY_LOCATION.ne(&self.structs.read()[id.cast()].location) {
            return false;
        }

        let mut writer = context.structs.write();
        if writer[id].location == *DUMMY_LOCATION {
            return true;
        }

        fn migrate_hashmap<K, A, B>(value: HashMap<K, StoreKey<A>>) -> HashMap<K, StoreKey<B>> {
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
                        location: loc.clone(),
                        name: bound.entries[bound.entries.len() - 1].clone(),
                    }),
                }
            }

            generics.push((generic.name.clone(), bounds));
        }

        let mut typed_struct = TypedStruct {
            name: writer[id].name.clone(),
            location: std::mem::replace(&mut writer[id].location, DUMMY_LOCATION.clone()),
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
                |generic_name| {
                    typed_struct
                        .generics
                        .iter()
                        .position(|(v, ..)| *v == *generic_name)
                        .map(|v| v as u8)
                },
                module_id.cast(),
                context.clone(),
                errors,
            ) {
                typed_struct.elements.push((element.0, typ));
            }
        }
        self.structs.write()[id.cast()] = typed_struct;

        false
    }

    fn type_resolution_resolve_type<F: Fn(&GlobalStr) -> Option<u8> + Copy>(
        &self,
        typ: &TypeRef,
        is_generic_name: F,
        module: StoreKey<TypecheckedModule>,
        context: Arc<ModuleContext>,
        errors: &mut Vec<TypecheckingError>,
    ) -> Option<Type> {
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
                    is_generic_name,
                    module,
                    context.clone(),
                    errors,
                )?;
                let mut arguments = Vec::with_capacity(args.len());
                for arg in args {
                    arguments.push(self.type_resolution_resolve_type(
                        arg,
                        is_generic_name,
                        module,
                        context.clone(),
                        errors,
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
                loc,
            } => {
                let path = type_name
                    .entries
                    .iter()
                    .map(|v| v.0.clone())
                    .collect::<Vec<_>>();
                // NOTE: this should only have a generic at the end as this is a type
                // (std::vec::Vec, can never be std::vec::Vec<u32>::Vec.)
                for (_, generics) in type_name.entries.iter() {
                    if !generics.is_empty() {
                        return None;
                    }
                }

                // generics can never have a generic attribute (struct Moew<T> { value: T<u32> })
                if type_name.entries.len() == 1 && type_name.entries[0].1.is_empty() {
                    if let Some(generic_id) = is_generic_name(&type_name.entries[0].0) {
                        return Some(Type::Generic {
                            name: type_name.entries[0].0.clone(),
                            generic_id,
                            num_references: 0,
                        });
                    }
                }

                let Ok(value) =
                    resolve_import(&context, module.cast(), &path, loc, &mut Vec::new())
                else {
                    errors.push(TypecheckingError::UnboundIdent {
                        location: loc.clone(),
                        name: path[path.len() - 1].clone(),
                    });
                    return None;
                };

                let ModuleScopeValue::Struct(id) = value else {
                    errors.push(TypecheckingError::MismatchingScopeType {
                        location: loc.clone(),
                        expected: ScopeKind::Type,
                        found: value.into(),
                    });
                    return None;
                };

                {
                    let typechecked_struct = &self.structs.read()[id.cast()];
                    if typechecked_struct.location != *DUMMY_LOCATION {
                        return Some(Type::Struct {
                            struct_id: typechecked_struct.id,
                            name: typechecked_struct.name.clone(),
                            num_references: *num_references,
                        });
                    }
                }

                let module = context.structs.read()[id].module_id;
                if self.resolve_struct(context, id, module, errors) {
                    errors.push(TypecheckingError::RecursiveTypeDetected {
                        location: loc.clone(),
                    });
                    return None;
                }
                let typechecked_struct = &self.structs.read()[id.cast()];
                if typechecked_struct.location != *DUMMY_LOCATION {
                    return Some(Type::Struct {
                        struct_id: typechecked_struct.id,
                        num_references: *num_references,
                        name: typechecked_struct.name.clone(),
                    });
                }
                unreachable!("struct should be resolved by here")
            }
            TypeRef::Void(..) => unreachable!(),
            TypeRef::Never(_) => unreachable!(),
            TypeRef::UnsizedArray {
                num_references,
                child,
                loc: _,
            } => Some(Type::UnsizedArray {
                typ: Box::new(self.type_resolution_resolve_type(
                    child,
                    is_generic_name,
                    module,
                    context,
                    errors,
                )?),
                num_references: *num_references,
            }),
            TypeRef::SizedArray {
                num_references,
                child,
                number_elements,
                loc: _,
            } => Some(Type::SizedArray {
                typ: Box::new(self.type_resolution_resolve_type(
                    child,
                    is_generic_name,
                    module,
                    context,
                    errors,
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
                        is_generic_name,
                        module,
                        context.clone(),
                        errors,
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

fn typed_resolve_import(
    context: &TypecheckingContext,
    module: StoreKey<TypecheckedModule>,
    import: &[GlobalStr],
    location: &Location,
    already_included: &mut Vec<(StoreKey<TypecheckedModule>, GlobalStr)>,
) -> Result<ModuleScopeValue, TypecheckingError> {
    if import.is_empty() {
        return Ok(ModuleScopeValue::Module(module.cast()));
    }
    if already_included
        .iter()
        .any(|(mod_id, imp)| module.eq(mod_id) && import[0].eq(imp))
    {
        return Err(TypecheckingError::CyclicDependency {
            location: location.clone(),
        });
    }
    already_included.push((module, import[0].clone()));

    let reader = context.modules.read();
    let ident = match reader[module.cast()].exports.get(&import[0]) {
        Some(ident) => ident,
        None if already_included.len() < 2 /* this is the module it was imported from */ => &import[0],
        None => return Err(TypecheckingError::ExportNotFound {
            location: location.clone(),
            name: import[0].clone(),
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
                        location: location.clone(),
                        name: import[2].clone(),
                    });
                } else {
                    return Err(TypecheckingError::ExportNotFound {
                        location: reader[id.cast()].location.clone(),
                        name: import[1].clone(),
                    });
                }
            }
            ModuleScopeValue::Module(_) => unreachable!(), // all modules must have been imports
            ModuleScopeValue::Function(_)
            | ModuleScopeValue::ExternalFunction(_)
            | ModuleScopeValue::Trait(_)
            | ModuleScopeValue::Static(_) => {
                return Err(TypecheckingError::ExportNotFound {
                    location: location.clone(),
                    name: import[1].clone(),
                })
            }
        }
    }
    Err(TypecheckingError::ExportNotFound {
        location: location.clone(),
        name: import[0].clone(),
    })
}

fn resolve_import(
    context: &ModuleContext,
    module: StoreKey<Module>,
    import: &[GlobalStr],
    location: &Location,
    already_included: &mut Vec<(StoreKey<Module>, GlobalStr)>,
) -> Result<ModuleScopeValue, TypecheckingError> {
    if import.is_empty() {
        return Ok(ModuleScopeValue::Module(module));
    }
    if already_included
        .iter()
        .any(|(mod_id, imp)| module.eq(mod_id) && import[0].eq(imp))
    {
        return Err(TypecheckingError::CyclicDependency {
            location: location.clone(),
        });
    }
    already_included.push((module, import[0].clone()));

    let reader = context.modules.read();
    let ident = match reader[module].exports.get(&import[0]) {
        Some(ident) => ident,
        None if already_included.len() < 2 /* this is the module it was imported from */ => &import[0],
        None => return Err(TypecheckingError::ExportNotFound {
            location: location.clone(),
            name: import[0].clone(),
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
                        location: context.functions.read()[function_id].0.location.clone(),
                        name: import[2].clone(),
                    });
                } else {
                    return Err(TypecheckingError::ExportNotFound {
                        location: reader[id].location.clone(),
                        name: import[1].clone(),
                    });
                }
            }
            ModuleScopeValue::Function(_)
            | ModuleScopeValue::ExternalFunction(_)
            | ModuleScopeValue::Trait(_)
            | ModuleScopeValue::Static(_) => {
                return Err(TypecheckingError::ExportNotFound {
                    location: location.clone(),
                    name: import[1].clone(),
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
                        location: location.clone(),
                        name: import[2].clone(),
                    });
                } else {
                    return Err(TypecheckingError::ExportNotFound {
                        location: reader[id].location.clone(),
                        name: import[1].clone(),
                    });
                }
            }
            ModuleScopeValue::Module(_) => unreachable!(), // all modules must have been imports
            ModuleScopeValue::Function(_)
            | ModuleScopeValue::ExternalFunction(_)
            | ModuleScopeValue::Trait(_)
            | ModuleScopeValue::Static(_) => {
                return Err(TypecheckingError::ExportNotFound {
                    location: location.clone(),
                    name: import[1].clone(),
                })
            }
        }
    }
    Err(TypecheckingError::ExportNotFound {
        location: location.clone(),
        name: import[0].clone(),
    })
}

impl From<ModuleScopeValue> for ScopeKind {
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
