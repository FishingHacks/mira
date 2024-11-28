use std::{
    collections::HashMap,
    fmt::Debug,
    path::PathBuf,
    sync::{Arc, LazyLock, RwLock},
};

use statement::TypecheckedStatement;
use types::{resolve_primitive_type, Type};

use crate::{
    globals::GlobalStr,
    module::{FunctionId, ModuleContext, ModuleId, ModuleScopeValue, StructId, TraitId},
    parser::{Annotations, LiteralValue, TypeRef},
    tokenizer::Location,
};

mod error;
mod expression;
mod statement;
pub mod typechecking;
mod types;
pub use error::TypecheckingError;

static DUMMY_LOCATION: LazyLock<Location> = LazyLock::new(|| Location {
    line: 0,
    column: 0,
    file: PathBuf::from("").into(), // a file should never be a folder :3
});

#[derive(Debug)]
pub struct TypecheckedFunctionContract {
    pub name: Option<GlobalStr>,
    pub arguments: Vec<(GlobalStr, Type)>,
    pub return_type: Type,
    pub annotations: Annotations,
    pub location: Location,
    pub module_id: ModuleId,
}

#[derive(Debug)]
pub struct TypedTrait {
    pub name: GlobalStr,
    pub functions: HashMap<
        GlobalStr,
        (
            GlobalStr,
            Vec<(GlobalStr, Type)>,
            Type,
            Annotations,
            Location,
        ),
    >,
    pub location: Location,
    pub module_id: ModuleId,
    pub id: TraitId,
    pub annotations: Annotations,
}

#[derive(Debug)]
pub struct TypedStruct {
    pub name: GlobalStr,
    pub elements: Vec<(GlobalStr, Type)>,
    pub location: Location,
    pub global_impl: HashMap<GlobalStr, FunctionId>,
    pub annotations: Annotations,
    pub module_id: ModuleId,
    pub id: StructId,
    pub generics: HashMap<GlobalStr, Vec<TraitId>>,
}

#[derive(Debug)]
pub struct TypecheckingContext {
    pub modules: RwLock<Vec<TypecheckedModule>>,
    pub functions: RwLock<Vec<(TypecheckedFunctionContract, TypecheckedStatement)>>,
    pub external_functions:
        RwLock<Vec<(TypecheckedFunctionContract, Option<TypecheckedStatement>)>>,
    pub statics: RwLock<Vec<(Type, LiteralValue, ModuleId, Location)>>,
    pub structs: RwLock<Vec<Arc<TypedStruct>>>,
    pub traits: RwLock<Vec<TypedTrait>>,
}

pub struct TypecheckedModule {
    context: Arc<TypecheckingContext>,
    scope: HashMap<GlobalStr, ModuleScopeValue>,
    exports: HashMap<GlobalStr, GlobalStr>,
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
        let modules = RwLock::new(Vec::new());
        let traits_reader = context.traits.read().expect("read-write lock is poisoned");
        let structs_reader = context.structs.read().expect("read-write lock is poisoned");
        let statics_reader = context.statics.read().expect("read-write lock is poisoned");
        let functions_reader = context
            .functions
            .read()
            .expect("read-write lock is poisoned");
        let external_functions_reader = context
            .external_functions
            .read()
            .expect("read-write lock is poisoned");
        let num_traits = traits_reader.len();
        let num_structs = structs_reader.len();
        let num_statics = statics_reader.len();
        let num_functions = functions_reader.len();
        let num_external_functions = external_functions_reader.len();

        let mut traits = Vec::with_capacity(num_traits);
        let mut structs = Vec::with_capacity(num_structs);
        let mut statics = Vec::with_capacity(num_statics);
        let mut functions = Vec::with_capacity(num_functions);
        let mut external_functions = Vec::with_capacity(num_external_functions);

        for id in 0..num_structs {
            structs.push(Arc::new(TypedStruct {
                name: GlobalStr::zeroed(),
                elements: Vec::new(),
                location: DUMMY_LOCATION.clone(),
                global_impl: HashMap::new(),
                annotations: Annotations::default(),
                module_id: 0,
                generics: HashMap::new(),
                id,
            }));
        }

        for _ in 0..num_statics {
            statics.push((
                Type::PrimitiveNever,
                LiteralValue::Void,
                0,
                DUMMY_LOCATION.clone(),
            ));
        }

        for _ in 0..num_functions {
            functions.push((
                TypecheckedFunctionContract {
                    annotations: Annotations::default(),
                    name: None,
                    arguments: Vec::new(),
                    return_type: Type::PrimitiveNever,
                    location: DUMMY_LOCATION.clone(),
                    module_id: 0,
                },
                TypecheckedStatement::None,
            ));
        }

        for _ in 0..num_external_functions {
            external_functions.push((
                TypecheckedFunctionContract {
                    annotations: Annotations::default(),
                    name: None,
                    arguments: Vec::new(),
                    return_type: Type::PrimitiveNever,
                    location: DUMMY_LOCATION.clone(),
                    module_id: 0,
                },
                None,
            ))
        }

        for _ in 0..num_traits {
            traits.push(TypedTrait {
                name: GlobalStr::zeroed(),
                functions: HashMap::new(),
                location: DUMMY_LOCATION.clone(),
                module_id: 0,
                id: 0,
                annotations: Annotations::default(),
            });
        }

        let me = Arc::new(Self {
            structs: structs.into(),
            statics: statics.into(),
            functions: functions.into(),
            traits: traits.into(),
            external_functions: external_functions.into(),
            modules,
        });

        let mut typechecked_module_writer =
            me.modules.write().expect("read-write lock is poisoned");
        let module_reader = context.modules.read().expect("read-write lock is poisoned");

        let module_id = typechecked_module_writer.len();
        let scope = module_reader[module_id].scope.clone();

        typechecked_module_writer.push(TypecheckedModule {
            context: me.clone(),
            scope,
            exports: module_reader[module_id].exports.clone(),
        });

        drop(module_reader);
        drop(typechecked_module_writer);

        me
    }

    pub fn resolve_imports(&self, context: Arc<ModuleContext>) -> Vec<TypecheckingError> {
        let mut errors = vec![];
        let mut typechecked_module_writer =
            self.modules.write().expect("read-write lock is poisoned");
        let module_reader = context.modules.read().expect("read-write lock is poisoned");
        for id in 0..typechecked_module_writer.len() {
            for (name, (location, module_id, path)) in module_reader[id].imports.iter() {
                match resolve_import(&context, *module_id, path, location, &mut Vec::new()) {
                    Err(e) => errors.push(e),
                    Ok(k) => {
                        typechecked_module_writer[id].scope.insert(name.clone(), k);
                    }
                }
            }
        }

        errors
    }

    /// Resolves the types; This should be ran *after* [Self::resolve_imports]
    ///
    /// NOTE: this could potentially deadlock as i write-lock the rwlocks in the context a bunch
    /// and don't free them until fully resolving a value. This module is designed in a way that
    /// such a deadlock should never happen but.. uhh... :3c :3
    /// meow
    pub fn resolve_types(&self, context: Arc<ModuleContext>) -> Vec<TypecheckingError> {
        let mut errors = Vec::new();

        // +---------+
        // | Structs |
        // +---------+
        let num_structs = context
            .structs
            .read()
            .expect("read-write lock is poisoned")
            .len();
        for struct_id in 0..num_structs {
            let reader = context.structs.read().expect("read-write lock is poisoned");
            let module_id = reader[struct_id].module_id;
            drop(reader);
            assert!(
                !self.resolve_struct(context.clone(), struct_id, module_id, &mut errors),
                "this came from no field, so this shouldn't be recursive"
            );
        }

        // +-----------+
        // | Functions |
        // +-----------+
        let num_functions = context
            .functions
            .read()
            .expect("read-write lock is poisoned")
            .len();
        for function_id in 0..num_functions {
            let mut writer = context
                .functions
                .write()
                .expect("read-write lock is poisoned");
            let module_id = writer[function_id].2;
            let arguments = std::mem::take(&mut writer[function_id].0.arguments);
            let return_type = std::mem::replace(
                &mut writer[function_id].0.return_type,
                TypeRef::Void(DUMMY_LOCATION.clone(), 0),
            );
            let generics = std::mem::take(&mut writer[function_id].0.generics)
                .into_iter()
                .map(|generic| generic.name)
                .collect::<Vec<_>>();
            let mut resolved_function_contract = TypecheckedFunctionContract {
                module_id,
                name: writer[function_id].0.name.clone(),
                location: writer[function_id].0.location.clone(),
                annotations: std::mem::take(&mut writer[function_id].0.annotations),
                arguments: Vec::new(),
                return_type: Type::PrimitiveNever,
            };
            drop(writer);

            let mut has_errors = false;
            match self.resolve_type(module_id, &return_type, &generics) {
                Ok(v) => resolved_function_contract.return_type = v,
                Err(e) => {
                    has_errors = true;
                    errors.push(e);
                }
            }

            for arg in arguments {
                match self.resolve_type(module_id, &arg.typ, &generics) {
                    Ok(v) => resolved_function_contract.arguments.push((arg.name, v)),
                    Err(e) => {
                        has_errors = true;
                        errors.push(e);
                    }
                }
            }

            if !has_errors {
                self.functions.write().expect("read-write lock is poisoned")[function_id] =
                    (resolved_function_contract, TypecheckedStatement::None);
            }
        }

        // +--------------------+
        // | External Functions |
        // +--------------------+
        let num_functions = context
            .external_functions
            .read()
            .expect("read-write lock is poisoned")
            .len();
        for function_id in 0..num_functions {
            let mut writer = context
                .external_functions
                .write()
                .expect("read-write lock is poisoned");
            let module_id = writer[function_id].2;
            let arguments = std::mem::take(&mut writer[function_id].0.arguments);
            let return_type = std::mem::replace(
                &mut writer[function_id].0.return_type,
                TypeRef::Void(DUMMY_LOCATION.clone(), 0),
            );
            let mut resolved_function_contract = TypecheckedFunctionContract {
                module_id,
                name: writer[function_id].0.name.clone(),
                location: writer[function_id].0.location.clone(),
                annotations: std::mem::take(&mut writer[function_id].0.annotations),
                arguments: Vec::new(),
                return_type: Type::PrimitiveNever,
            };
            drop(writer);

            let mut has_errors = false;
            match self.resolve_type(module_id, &return_type, &[]) {
                Ok(v) => resolved_function_contract.return_type = v,
                Err(e) => {
                    has_errors = true;
                    errors.push(e);
                }
            }

            for arg in arguments {
                match self.resolve_type(module_id, &arg.typ, &[]) {
                    Ok(v) => resolved_function_contract.arguments.push((arg.name, v)),
                    Err(e) => {
                        has_errors = true;
                        errors.push(e);
                    }
                }
            }

            if !has_errors {
                self.external_functions
                    .write()
                    .expect("read-write lock is poisoned")[function_id] =
                    (resolved_function_contract, None);
            }
        }

        // +---------+
        // | Statics |
        // +---------+
        let num_statics = context
            .statics
            .read()
            .expect("read-write lock is poisoned")
            .len();
        for static_id in 0..num_statics {
            let mut writer = context
                .statics
                .write()
                .expect("read-write lock is poisoned");
            let location = std::mem::replace(&mut writer[static_id].3, DUMMY_LOCATION.clone());
            let dummy_type = TypeRef::Void(writer[static_id].0.loc().clone(), 0);
            let typ = std::mem::replace(&mut writer[static_id].0, dummy_type);
            let module_id = writer[static_id].2;
            drop(writer);
            match self.resolve_type(module_id, &typ, &[]) {
                Ok(v) => {
                    self.statics.write().expect("read-write lock is poisoned")[static_id] =
                        (v, LiteralValue::Void, module_id, location);
                }
                Err(e) => errors.push(e),
            }
        }

        // +--------+
        // | Traits |
        // +--------+
        let num_traits = context
            .traits
            .read()
            .expect("read-write lock is poisoned")
            .len();
        for trait_id in 0..num_traits {
            let mut writer = context.traits.write().expect("read-write lock is poisoned");
            let location =
                std::mem::replace(&mut writer[trait_id].location, DUMMY_LOCATION.clone());
            let name = writer[trait_id].name.clone();
            let annotations = std::mem::take(&mut writer[trait_id].annotations);
            let functions = std::mem::take(&mut writer[trait_id].functions);
            let module_id = writer[trait_id].module_id;
            drop(writer);

            let mut typed_functions = HashMap::new();
            let error_count = errors.len();

            for (name, arguments, return_type, annotations, location) in functions {
                let typed_return_type = match self.resolve_type(module_id, &return_type, &[]) {
                    Ok(v) => v,
                    Err(e) => {
                        errors.push(e);
                        continue;
                    }
                };

                let mut typed_arguments = Vec::new();

                for arg in arguments {
                    match self.resolve_type(module_id, &arg.typ, &[]) {
                        Ok(v) => typed_arguments.push((arg.name, v)),
                        Err(e) => errors.push(e),
                    }
                }

                typed_functions.insert(
                    name.clone(),
                    (
                        name,
                        typed_arguments,
                        typed_return_type,
                        annotations,
                        location,
                    ),
                );
            }

            if errors.len() == error_count {
                self.traits.write().expect("read-write lock is poisoned")[trait_id] = TypedTrait {
                    name,
                    location,
                    id: trait_id,
                    module_id,
                    annotations,
                    functions: typed_functions,
                };
            }
        }

        return errors;
    }

    pub fn resolve_type(
        &self,
        module_id: ModuleId,
        typ: &TypeRef,
        generics: &[GlobalStr],
    ) -> Result<Type, TypecheckingError> {
        if let Some(primitive) = resolve_primitive_type(typ) {
            return Ok(primitive);
        }

        match typ {
            TypeRef::DynReference { .. } => todo!(),
            TypeRef::Reference {
                num_references,
                type_name,
                loc,
            } => {
                if type_name.entries.len() == 1 && type_name.entries[0].1.len() == 0 {
                    if generics.contains(&type_name.entries[0].0) {
                        return Ok(Type::Generic(
                            type_name.entries[0].0.clone(),
                            *num_references,
                        ));
                    }
                }

                let path = type_name
                    .entries
                    .iter()
                    .map(|v| v.0.clone())
                    .collect::<Vec<_>>();
                // NOTE: this should only have a generic at the end as this is a type
                // (std::vec::Vec, can never be std::vec::Vec<u32>::Vec.)
                for (_, generics) in type_name.entries.iter() {
                    if generics.len() > 0 {
                        return Err(TypecheckingError::UnexpectedGenerics {
                            location: loc.clone(),
                        });
                    }
                }

                match typed_resolve_import(self, module_id, &path, loc, &mut Vec::new())? {
                    ModuleScopeValue::Struct(id) => Ok(Type::Struct {
                        structure: self.structs.read().expect("read-write lock is poisoned")[id]
                            .clone(),
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
                typ: Box::new(self.resolve_type(module_id, &**child, generics)?),
                num_references: *num_references,
            }),
            TypeRef::SizedArray {
                num_references,
                child,
                number_elements,
                loc: _,
            } => Ok(Type::SizedArray {
                typ: Box::new(self.resolve_type(module_id, &**child, generics)?),
                num_references: *num_references,
                number_elements: *number_elements,
            }),
        }
    }

    /// returns if a recursive field was detected
    fn resolve_struct(
        &self,
        context: Arc<ModuleContext>,
        id: StructId,
        module_id: ModuleId,
        errors: &mut Vec<TypecheckingError>,
    ) -> bool {
        if DUMMY_LOCATION
            .ne(&self.structs.read().expect("read-write lock is poisoned")[id].location)
        {
            return false;
        }

        let mut writer = context
            .structs
            .write()
            .expect("read-write lock is poisoned");
        if writer[id].location == *DUMMY_LOCATION {
            return true;
        }

        let global_impl = std::mem::take(&mut writer[id].global_impl);
        let annotations = std::mem::take(&mut writer[id].annotations);
        let elements = std::mem::take(&mut writer[id].elements);
        let mut generics = HashMap::new();

        for generic in &writer[id].generics {
            let mut bounds = Vec::new();

            for (bound, loc) in &generic.bounds {
                match resolve_import(&context, module_id, &bound.entries, loc, &mut Vec::new()) {
                    Err(e) => errors.push(e),
                    Ok(ModuleScopeValue::Trait(trait_id)) => bounds.push(trait_id),
                    Ok(_) => errors.push(TypecheckingError::UnboundIdent {
                        location: loc.clone(),
                        name: bound.entries[bound.entries.len() - 1].clone(),
                    }),
                }
            }

            generics.insert(generic.name.clone(), bounds);
        }

        let mut typed_struct = TypedStruct {
            name: writer[id].name.clone(),
            location: std::mem::replace(&mut writer[id].location, DUMMY_LOCATION.clone()),
            elements: Vec::new(),
            global_impl,
            annotations,
            module_id,
            id,
            generics,
        };
        drop(writer);

        for element in elements {
            if let Some(typ) = self.type_resolution_resolve_type(
                &element.1,
                |generic_name| typed_struct.generics.contains_key(generic_name),
                module_id,
                context.clone(),
                errors,
            ) {
                let typ = match typ {
                    Type::Generic(real_name, num_references)
                        if typed_struct.generics[&real_name].len() > 0 =>
                    {
                        Type::Trait {
                            trait_refs: typed_struct.generics[&real_name].clone(),
                            num_references,
                            real_name,
                        }
                    }
                    t => t,
                };
                typed_struct.elements.push((element.0, typ));
            }
        }
        self.structs.write().expect("read-write lock is poisoned")[id] = Arc::new(typed_struct);

        false
    }

    fn type_resolution_resolve_type<F: Fn(&GlobalStr) -> bool>(
        &self,
        typ: &TypeRef,
        is_generic_name: F,
        module: ModuleId,
        context: Arc<ModuleContext>,
        errors: &mut Vec<TypecheckingError>,
    ) -> Option<Type> {
        if let Some(typ) = resolve_primitive_type(typ) {
            return Some(typ);
        }
        match typ {
            TypeRef::DynReference { .. } => todo!(),
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
                    if generics.len() > 0 {
                        return None;
                    }
                }

                // generics can never have a generic attribute (struct Moew<T> { value: T<u32> })
                if type_name.entries.len() == 1 && type_name.entries[0].1.len() == 0 {
                    if is_generic_name(&type_name.entries[0].0) {
                        return Some(Type::Generic(type_name.entries[0].0.clone(), 0));
                    }
                }

                let Ok(value) = resolve_import(&context, module, &path, loc, &mut Vec::new())
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
                    let typechecked_struct =
                        &self.structs.read().expect("read-write lock is poisoned")[id];
                    if typechecked_struct.location != *DUMMY_LOCATION {
                        return Some(Type::Struct {
                            structure: typechecked_struct.clone(),
                            num_references: *num_references,
                        });
                    }
                }

                let module =
                    context.structs.read().expect("read-write lock is poisoned")[id].module_id;
                if self.resolve_struct(context, id, module, errors) {
                    errors.push(TypecheckingError::RecursiveTypeDetected {
                        location: loc.clone(),
                    });
                    return None;
                }
                let typechecked_struct =
                    &self.structs.read().expect("read-write lock is poisoned")[id];
                if typechecked_struct.location != *DUMMY_LOCATION {
                    return Some(Type::Struct {
                        structure: typechecked_struct.clone(),
                        num_references: *num_references,
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
        }
    }
}

fn typed_resolve_import(
    context: &TypecheckingContext,
    module: ModuleId,
    import: &[GlobalStr],
    location: &Location,
    already_included: &mut Vec<(ModuleId, GlobalStr)>,
) -> Result<ModuleScopeValue, TypecheckingError> {
    if import.len() < 1 {
        return Ok(ModuleScopeValue::Module(module));
    }
    if already_included
        .iter()
        .find(|(mod_id, imp)| module.eq(mod_id) && import[0].eq(imp))
        .is_some()
    {
        return Err(TypecheckingError::CyclicDependency {
            location: location.clone(),
        });
    }
    already_included.push((module, import[0].clone()));

    let reader = context.modules.read().expect("read-write lock is poisoned");
    let ident = match reader[module].exports.get(&import[0]) {
        Some(ident) => ident,
        None if already_included.len() < 2 /* this is the module it was imported from */ => &import[0],
        None => return Err(TypecheckingError::ExportNotFound {
            location: location.clone(),
            name: import[0].clone(),
        }),
    };

    if let Some(value) = reader[module].scope.get(ident).copied() {
        if import.len() < 2 {
            return Ok(value);
        }
        match value {
            ModuleScopeValue::Struct(id) => {
                let reader = context.structs.read().expect("read-write lock is poisoned");
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
        name: import[1].clone(),
    })
}

fn resolve_import(
    context: &ModuleContext,
    module: ModuleId,
    import: &[GlobalStr],
    location: &Location,
    already_included: &mut Vec<(ModuleId, GlobalStr)>,
) -> Result<ModuleScopeValue, TypecheckingError> {
    if import.len() < 1 {
        return Ok(ModuleScopeValue::Module(module));
    }
    if already_included
        .iter()
        .find(|(mod_id, imp)| module.eq(mod_id) && import[0].eq(imp))
        .is_some()
    {
        return Err(TypecheckingError::CyclicDependency {
            location: location.clone(),
        });
    }
    already_included.push((module, import[0].clone()));

    let reader = context.modules.read().expect("read-write lock is poisoned");
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
                let reader = context.structs.read().expect("read-write lock is poisoned");
                if let Some(function_id) = reader[id].global_impl.get(&import[1]).copied() {
                    if import.len() < 3 {
                        return Ok(ModuleScopeValue::Function(function_id));
                    }
                    return Err(TypecheckingError::ExportNotFound {
                        location: context
                            .functions
                            .read()
                            .expect("read-write lock is poisoned")[function_id]
                            .0
                            .location
                            .clone(),
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
                let reader = context.structs.read().expect("read-write lock is poisoned");
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
        name: import[1].clone(),
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
