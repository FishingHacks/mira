use std::{
    collections::HashMap,
    fmt::{Debug, Write},
    path::PathBuf,
    sync::{Arc, LazyLock, RwLock},
};

use statement::TypecheckedStatement;
use types::{resolve_primitive_type, Type};

use crate::{
    globals::GlobalStr,
    module::{FunctionId, ModuleContext, ModuleId, ModuleScopeValue, StructId},
    parser::{Annotations, LiteralValue, TypeRef},
    tokenizer::Location,
};

mod expression;
mod statement;
pub mod typechecking;
mod types;

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
pub struct TypedStruct {
    pub name: GlobalStr,
    pub elements: Vec<(GlobalStr, Type)>,
    pub location: Location,
    pub global_impl: HashMap<GlobalStr, FunctionId>,
    pub annotations: Annotations,
    pub module_id: ModuleId,
    pub id: StructId,
}

#[derive(Debug)]
pub struct TypecheckingContext {
    pub modules: RwLock<Vec<TypecheckedModule>>,
    pub functions: RwLock<Vec<(TypecheckedFunctionContract, TypecheckedStatement)>>,
    pub external_functions:
        RwLock<Vec<(TypecheckedFunctionContract, Option<TypecheckedStatement>)>>,
    pub statics: RwLock<Vec<(Type, LiteralValue, ModuleId, Location)>>,
    pub structs: RwLock<Vec<Arc<TypedStruct>>>,
}

pub struct TypecheckedModule {
    context: Arc<TypecheckingContext>,
    scope: HashMap<GlobalStr, ModuleScopeValue>,
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
        let num_structs = structs_reader.len();
        let num_statics = statics_reader.len();
        let num_functions = functions_reader.len();
        let num_external_functions = external_functions_reader.len();

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

        let me = Arc::new(Self {
            structs: structs.into(),
            statics: statics.into(),
            functions: functions.into(),
            external_functions: external_functions.into(),
            modules,
        });

        let mut typechecked_module_writer =
            me.modules.write().expect("read-write lock is poisoned");
        let module_reader = context.modules.read().expect("read-write lock is poisoned");

        let scope = module_reader[typechecked_module_writer.len()].scope.clone();

        typechecked_module_writer.push(TypecheckedModule {
            context: me.clone(),
            scope,
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
                match resolve_import(context.clone(), *module_id, path, location, &mut Vec::new()) {
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
            TypeRef::Reference {
                num_references,
                type_name,
                loc,
            } => {
                if generics.contains(type_name) {
                    return Ok(Type::Generic(type_name.clone(), *num_references));
                }
                match self.modules.read().expect("read-write lock is poisoned")[module_id]
                    .scope
                    .get(type_name)
                    .copied()
                {
                    None => Err(TypecheckingError::UnboundIdent {
                        location: loc.clone(),
                        name: type_name.clone(),
                    }),
                    Some(ModuleScopeValue::Struct(id)) => Ok(Type::Struct {
                        structure: self.structs.read().expect("read-write lock is poisoned")[id]
                            .clone(),
                        num_references: *num_references,
                    }),
                    Some(v) => Err(TypecheckingError::MismatchingScopeType {
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

        let mut typed_struct = TypedStruct {
            name: writer[id].name.clone(),
            location: std::mem::replace(&mut writer[id].location, DUMMY_LOCATION.clone()),
            elements: Vec::new(),
            global_impl,
            annotations,
            module_id,
            id,
        };
        drop(writer);

        for element in elements {
            if let Some(typ) =
                self.type_resolution_resolve_type(&element.1, module_id, context.clone(), errors)
            {
                typed_struct.elements.push((element.0, typ));
            }
        }
        self.structs.write().expect("read-write lock is poisoned")[id] = Arc::new(typed_struct);

        false
    }

    fn type_resolution_resolve_type(
        &self,
        typ: &TypeRef,
        module: ModuleId,
        context: Arc<ModuleContext>,
        errors: &mut Vec<TypecheckingError>,
    ) -> Option<Type> {
        if let Some(typ) = resolve_primitive_type(typ) {
            return Some(typ);
        }
        match typ {
            TypeRef::Reference {
                num_references,
                type_name,
                loc,
            } => {
                let Some(value) = context.modules.read().expect("read-write lock is poisoned")
                    [module]
                    .scope
                    .get(type_name)
                    .copied()
                else {
                    errors.push(TypecheckingError::UnboundIdent {
                        location: loc.clone(),
                        name: type_name.clone(),
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
                typ: Box::new(self.type_resolution_resolve_type(child, module, context, errors)?),
                num_references: *num_references,
            }),
            TypeRef::SizedArray {
                num_references,
                child,
                number_elements,
                loc: _,
            } => Some(Type::SizedArray {
                typ: Box::new(self.type_resolution_resolve_type(child, module, context, errors)?),
                num_references: *num_references,
                number_elements: *number_elements,
            }),
        }
    }
}

fn resolve_import(
    context: Arc<ModuleContext>,
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
    let Some(ident) = reader[module].exports.get(&import[0]) else {
        return Err(TypecheckingError::ExportNotFound {
            location: location.clone(),
            name: import[0].clone(),
        });
    };

    if let Some((sub_location, module, path)) = reader[module].imports.get(ident) {
        let value = resolve_import(
            context.clone(),
            *module,
            path,
            sub_location,
            already_included,
        )?;
        if import.len() < 2 {
            return Ok(value);
        }

        match value {
            ModuleScopeValue::Module(id) => {
                return resolve_import(
                    context.clone(),
                    id,
                    &import[1..],
                    location,
                    already_included,
                )
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
            ModuleScopeValue::Struct(_) => Self::Type,
            ModuleScopeValue::Static(_) => Self::Static,
            ModuleScopeValue::Module(_) => Self::Module,
            ModuleScopeValue::Function(_) | ModuleScopeValue::ExternalFunction(_) => Self::Function,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ScopeKind {
    Type,
    Function,
    Static,
    Module,
}

#[derive(Clone)]
pub enum TypecheckingError {
    ExportNotFound {
        location: Location,
        name: GlobalStr,
    },
    CyclicDependency {
        location: Location,
    },
    UnboundIdent {
        location: Location,
        name: GlobalStr,
    },
    MismatchingScopeType {
        location: Location,
        expected: ScopeKind,
        found: ScopeKind,
    },
    RecursiveTypeDetected {
        location: Location,
    },
    BodyDoesNotAlwaysReturn {
        location: Location,
    },
    MismatchingType {
        expected: Type,
        found: Type,
        location: Location,
    },
    GenericFunctionPointer {
        location: Location,
    },
    IdentifierIsNotStruct {
        location: Location,
        name: GlobalStr,
    },
    NoSuchFieldFound {
        location: Location,
        name: GlobalStr,
    },
    MissingField {
        location: Location,
        name: GlobalStr,
    },
    TypeIsNotAFunction {
        location: Location,
    },
    MissingArguments {
        location: Location,
    },
    TooManyArguments {
        location: Location,
    },
}

impl Debug for TypecheckingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExportNotFound { location, name } => {
                f.write_fmt(format_args!("{location}: could not find field {name}"))
            }
            Self::CyclicDependency { location } => {
                f.write_fmt(format_args!("{location}: cyclic dependency detected"))
            }
            Self::UnboundIdent { location, name } => {
                f.write_fmt(format_args!("{location}: Unbound identifier `{name}`"))
            }
            Self::MismatchingScopeType {
                location,
                expected,
                found,
            } => f.write_fmt(format_args!(
                "{location}: Expected a {expected:?}, but foun a {found:?}"
            )),
            Self::RecursiveTypeDetected { location } => {
                f.write_fmt(format_args!("{location}: Recursive type detected"))
            }
            Self::BodyDoesNotAlwaysReturn { location } => {
                f.write_fmt(format_args!("{location}: body does not always return"))
            }
            Self::MismatchingType {
                expected,
                found,
                location,
            } => f.write_fmt(format_args!(
                "{location}: Expected {expected}, but found {found}"
            )),
            Self::GenericFunctionPointer { location } => f.write_fmt(format_args!(
                "{location}: Function pointers can't have generics"
            )),
            Self::IdentifierIsNotStruct { location, name } => {
                f.write_fmt(format_args!("{location}: {name} is not a struct type"))
            }
            Self::NoSuchFieldFound { location, name } => {
                f.write_fmt(format_args!("{location}: such field named `{name}` found!"))
            }
            Self::MissingField { location, name } => {
                f.write_fmt(format_args!("{location}: missing field `{name}`"))
            }
            Self::TypeIsNotAFunction { location } => {
                f.write_fmt(format_args!("{location}: Expected a function"))
            }
            Self::MissingArguments { location } => {
                f.write_fmt(format_args!("{location}: Function misses arguments"))
            }
            Self::TooManyArguments { location } => f.write_fmt(format_args!(
                "{location}: Function expects no more arguments"
            )),
        }
    }
}
