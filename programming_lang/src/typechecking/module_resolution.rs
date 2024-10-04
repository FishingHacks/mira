use std::{
    collections::HashMap,
    rc::Rc,
    sync::{Arc, RwLock},
};

use crate::{
    globals::GlobalStr,
    module::Module,
    parser::{Statement, TypeRef},
    tokenizer::Location,
    typechecking::{ModuleId, ScopeItem, TypecheckingContext},
};

use super::{
    type_resolution::{ResolvedStruct, Type},
    ProgrammingLangTypecheckingError, ResolvedFunctionContract, TypecheckedModule,
    TypecheckedModules,
};

fn resolve_struct(
    name: &GlobalStr,
    module_id: usize,
    modules: &mut Vec<Module>,
    typechecked_modules: TypecheckedModules,
    loc: &Location,
) -> Result<Rc<ResolvedStruct>, ProgrammingLangTypecheckingError> {
    if let Some(kind) = typechecked_modules
        .read()
        .expect("read-write lock is poisoned")
        .get(module_id)
        .and_then(|module| module.scope.get(name))
    {
        let ScopeItem::Struct(id) = kind else {
            return Err(ProgrammingLangTypecheckingError::UnboundType {
                loc: loc.clone(),
                type_name: name.clone(),
            });
        };
        if let Some(structure) = typechecked_modules
            .read()
            .expect("read-write lock is poisoned")
            .get(module_id)
            .and_then(|module| {
                module
                    .context
                    .structs
                    .read()
                    .expect("read-write lock is poisoned")
                    .get(*id)
                    .cloned()
            })
        {
            return Ok(structure);
        }
    }

    if let Some(v) = modules[module_id].imports.get(name) {
        if v.2.len() > 0 {
            todo!("paths with a length >1 are not supported");
        }
        return resolve_struct(&v.2[0].clone(), v.1, modules, typechecked_modules, loc);
    }

    if let Some(structure) = modules[module_id].structs.remove(name) {
        let mut fields = Vec::new();

        for field in structure.fields {
            fields.push((
                field.0,
                resolve_type(&field.1, module_id, modules, typechecked_modules.clone())?,
            ));
        }

        let mut typechecked_modules = typechecked_modules
            .write()
            .expect("red-write lock is poisoned");

        let resolved_struct = Rc::new(ResolvedStruct {
            module_id: ModuleId(module_id),
            id: typechecked_modules[module_id]
                .context
                .structs
                .read()
                .expect("read-write lock is poisoned")
                .len(),
            name: structure.name.clone(),
            fields: fields.into_boxed_slice(),
            global_impl: structure.global_impl,
            annotations: structure.annotations,
        });

        typechecked_modules[module_id]
            .context
            .structs
            .write()
            .expect("read-write lock is poisoned")
            .push(resolved_struct.clone());
        typechecked_modules[module_id]
            .scope
            .insert(structure.name, ScopeItem::Struct(resolved_struct.id));

        Ok(resolved_struct)
    } else {
        Err(ProgrammingLangTypecheckingError::UnboundType {
            loc: loc.clone(),
            type_name: name.clone(),
        })
    }
}

fn resolve_type(
    type_ref: &TypeRef,
    module_id: usize,
    modules: &mut Vec<Module>,
    typechecked_modules: TypecheckedModules,
) -> Result<Type, ProgrammingLangTypecheckingError> {
    if let Some(primitive) = resolve_primitive(type_ref) {
        return Ok(primitive);
    }

    match type_ref {
        TypeRef::Never(_) | TypeRef::Void(_) => unreachable!(),
        TypeRef::UnsizedArray {
            num_references,
            child,
            loc: _,
        } => Ok(Type::UnsizedArray {
            typ: Box::new(resolve_type(
                child,
                module_id,
                modules,
                typechecked_modules,
            )?),
            num_references: *num_references,
        }),
        TypeRef::SizedArray {
            num_references,
            child,
            number_elements,
            loc: _,
        } => Ok(Type::SizedArray {
            typ: Box::new(resolve_type(
                child,
                module_id,
                modules,
                typechecked_modules,
            )?),
            num_references: *num_references,
            number_elements: *number_elements,
        }),
        TypeRef::Reference {
            num_references,
            type_name,
            loc,
        } => Ok(Type::Struct {
            structure: resolve_struct(type_name, module_id, modules, typechecked_modules, loc)?,
            num_references: *num_references,
        }),
    }
}

fn resolve_primitive(type_ref: &TypeRef) -> Option<Type> {
    match type_ref {
        TypeRef::Never(_) => Some(Type::PrimitiveNever),
        TypeRef::Void(_) => Some(Type::PrimitiveVoid(0)),
        TypeRef::Reference {
            num_references: number_of_references,
            type_name,
            loc: _,
        } => type_name.with(|type_name| match type_name {
            "!" => Some(Type::PrimitiveNever),
            "void" => Some(Type::PrimitiveVoid(*number_of_references)),
            "i8" => Some(Type::PrimitiveI8(*number_of_references)),
            "i16" => Some(Type::PrimitiveI16(*number_of_references)),
            "i32" => Some(Type::PrimitiveI32(*number_of_references)),
            "i64" => Some(Type::PrimitiveI64(*number_of_references)),
            "u8" => Some(Type::PrimitiveU8(*number_of_references)),
            "u16" => Some(Type::PrimitiveU16(*number_of_references)),
            "u32" => Some(Type::PrimitiveU32(*number_of_references)),
            "u64" => Some(Type::PrimitiveU64(*number_of_references)),
            "f32" => Some(Type::PrimitiveF32(*number_of_references)),
            "f64" => Some(Type::PrimitiveF64(*number_of_references)),
            "bool" => Some(Type::PrimitiveBool(*number_of_references)),
            "str" => Some(Type::PrimitiveStr(*number_of_references)),
            "isize" => Some(Type::PrimitiveISize(*number_of_references)),
            "usize" => Some(Type::PrimitiveUSize(*number_of_references)),
            _ => None,
        }),
        TypeRef::SizedArray { .. } | TypeRef::UnsizedArray { .. } => None,
    }
}

pub fn resolve_modules(
    mut modules: Vec<Module>,
) -> Result<Arc<RwLock<Vec<TypecheckedModule>>>, Vec<ProgrammingLangTypecheckingError>> {
    let mut errors = Vec::new();
    let context = Arc::new(TypecheckingContext::default());
    let typechecked_modules = Arc::new(RwLock::new(Vec::new()));

    {
        let mut typechecked_modules_writer = typechecked_modules
            .write()
            .expect("read-write lock is poisoned");
        for _ in 0..modules.len() {
            typechecked_modules_writer.push(TypecheckedModule {
                modules: typechecked_modules.clone(),
                context: context.clone(),
                scope: HashMap::new(),
            });
        }
    }

    // +---------+
    // | Structs |
    // +---------+
    for id in 0..modules.len() {
        while let Some(key) = modules[id].structs.keys().next() {
            let loc = modules[id]
                .structs
                .get(key)
                .expect("key should exist")
                .loc
                .clone();
            if let Err(e) = resolve_struct(
                &key.clone(),
                id,
                &mut modules,
                typechecked_modules.clone(),
                &loc,
            ) {
                errors.push(e);
            }
        }
    }

    // +-----------+
    // | Functions |
    // +-----------+
    for id in 0..modules.len() {
        let mut translations =
            HashMap::<usize, usize>::with_capacity(modules[id].function_registry.len());

        for fn_id in 0..modules[id].function_registry.len() {
            let generics = modules[id].function_registry[fn_id]
                .0
                .generics
                .iter()
                .map(|v| v.name.clone())
                .collect::<Vec<_>>();
            let annotations =
                std::mem::take(&mut modules[id].function_registry[fn_id].0.annotations);
            let name = modules[id].function_registry[fn_id].0.name.clone();
            let location = modules[id].function_registry[fn_id].0.location.clone();

            let mut has_errs = false;
            let mut arguments = Vec::new();
            for arg in std::mem::take(&mut modules[id].function_registry[fn_id].0.arguments) {
                match resolve_type(&arg.typ, id, &mut modules, typechecked_modules.clone()) {
                    Ok(v) => arguments.push((arg.name, v)),
                    Err(e) => {
                        has_errs = true;
                        errors.push(e);
                    }
                }
            }

            let return_type = modules[id].function_registry[fn_id].0.return_type.clone();
            let return_type =
                match resolve_type(&return_type, id, &mut modules, typechecked_modules.clone()) {
                    Ok(v) => v,
                    Err(e) => {
                        errors.push(e);
                        continue;
                    }
                };
            if has_errs {
                continue;
            }

            let resolved_fn_contract = ResolvedFunctionContract {
                name,
                location: location.clone(),
                annotations,
                generics: generics.into_boxed_slice(),
                module_id: ModuleId(id),
                return_type,
                arguments: arguments.into_boxed_slice(),
            };

            let statement = std::mem::replace(
                &mut modules[id].function_registry[fn_id].1,
                Statement::BakedFunction(usize::MAX, location),
            );

            let module_writer = typechecked_modules
                .read()
                .expect("read-write lock is poisoned");
            let mut context_writer = module_writer[id]
                .context
                .functions
                .write()
                .expect("read-write lock is poisoned");
            let function_id = context_writer.len();
            context_writer.push((resolved_fn_contract, statement.into()));
            drop(context_writer);
            translations.insert(fn_id, function_id);
        }

        let mut module_writer = typechecked_modules
            .write()
            .expect("read-write lock is poisoned");
        for (key, fn_id) in std::mem::take(&mut modules[id].functions) {
            let translated_id = translations
                .get(&fn_id)
                .expect("function without matching function id");
            module_writer[id]
                .scope
                .insert(key, ScopeItem::Function(*translated_id));
        }
    }

    // +-------------------+
    // | External Function |
    // +-------------------+
    for id in 0..modules.len() {
        while let Some(key) = modules[id].external_functions.keys().next().cloned() {
            let value = modules[id]
                .external_functions
                .remove(&key)
                .expect("key should always exist");

            let mut has_errs = false;
            let mut arguments = Vec::new();
            for arg in value.arguments {
                match resolve_type(&arg.typ, id, &mut modules, typechecked_modules.clone()) {
                    Ok(v) => arguments.push((arg.name, v)),
                    Err(e) => {
                        has_errs = true;
                        errors.push(e);
                    }
                }
            }

            let return_type = match resolve_type(
                &value.return_type,
                id,
                &mut modules,
                typechecked_modules.clone(),
            ) {
                Ok(v) => v,
                Err(e) => {
                    errors.push(e);
                    continue;
                }
            };
            if has_errs {
                continue;
            }

            let resolved_function_contract = ResolvedFunctionContract {
                name: value.name,
                location: value.location,
                module_id: ModuleId(id),
                annotations: value.annotations,
                generics: value
                    .generics
                    .into_iter()
                    .map(|v| v.name)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                arguments: arguments.into_boxed_slice(),
                return_type,
            };

            let mut module_writer = typechecked_modules
                .write()
                .expect("read-write lock is poisoned");
            let mut external_function_writer = module_writer[id]
                .context
                .external_functions
                .write()
                .expect("read-write lock is poisoned");
            let fn_id = external_function_writer.len();
            external_function_writer.push(resolved_function_contract);
            drop(external_function_writer);

            module_writer[id]
                .scope
                .insert(key, ScopeItem::ExternalFunction(fn_id));
            drop(module_writer);
        }
    }

    // +---------------+
    // | Static Values |
    // +---------------+
    for id in 0..modules.len() {
        while let Some(key) = modules[id].static_values.keys().next().cloned() {
            let value = modules[id]
                .static_values
                .remove(&key)
                .expect("key should exist");
            let value_type =
                match resolve_type(&value.0, id, &mut modules, typechecked_modules.clone()) {
                    Ok(v) => v,
                    Err(e) => {
                        errors.push(e);
                        continue;
                    }
                };

            let mut module_writer = typechecked_modules
                .write()
                .expect("read-write lock is poisoned");
            let mut external_function_writer = module_writer[id]
                .context
                .statics
                .write()
                .expect("read-write lock is poisoned");
            let static_id = external_function_writer.len();
            external_function_writer.push((value_type, value.1, ModuleId(id)));
            drop(external_function_writer);

            module_writer[id]
                .scope
                .insert(key, ScopeItem::StaticValue(static_id));
            drop(module_writer);
        }
    }

    // +---------+
    // | Imports |
    // +---------+
    for id in 0..modules.len() {
        for import in modules[id].imports.keys() {
            let item = match resolve_import(
                &modules,
                typechecked_modules.clone(),
                id,
                import,
                &modules[id].imports[import].0,
            ) {
                Ok(Some(v)) => v,
                Ok(None) => continue,
                Err(e) => {
                    errors.push(e);
                    continue;
                }
            };

            typechecked_modules
                .write()
                .expect("read-write lock is poisoned")[id]
                .scope
                .insert(import.clone(), item);
        }
    }

    if errors.len() > 0 {
        return Err(errors);
    }
    Ok(typechecked_modules)
}

pub fn resolve_import(
    modules: &[Module],
    typechecked_modules: TypecheckedModules,
    module_id: usize,
    import: &GlobalStr,
    loc: &Location,
) -> Result<Option<ScopeItem>, ProgrammingLangTypecheckingError> {
    let (location, import_module_id, path) = modules[module_id]
        .imports
        .get(import)
        .expect("import should always exist");
    if path.len() > 1 {
        todo!("paths with a length > 1 are not supported")
    }
    if modules[*import_module_id].imports.contains_key(&path[0]) {
        return resolve_import(
            modules,
            typechecked_modules,
            *import_module_id,
            &path[0],
            location,
        );
    }

    let Some(key) = modules[*import_module_id].exports.get(&path[0]) else {
        return Err(ProgrammingLangTypecheckingError::UnboundExport {
            loc: loc.clone(),
            name: path[0].clone(),
        });
    };
    Ok(typechecked_modules
        .read()
        .expect("read-write lock is poisoned")[*import_module_id]
        .scope
        .get(key)
        .copied())
}
