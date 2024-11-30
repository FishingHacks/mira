use std::{
    collections::HashMap,
    fmt::Debug,
    sync::{Arc, RwLock},
};

use crate::{
    annotations::Annotations,
    error::ProgramFormingError,
    globals::GlobalStr,
    parser::{Expression, FunctionContract, Generic, LiteralValue, Statement, Trait, TypeRef},
    tokenizer::Location,
};

pub type ModuleId = usize;
pub type StructId = usize;
pub type TraitId = usize;
pub type FunctionId = usize;
pub type StaticId = usize;
pub type ExternalFunctionId = usize;

#[derive(Debug, Copy, Clone)]
pub enum ModuleScopeValue {
    Function(FunctionId),
    ExternalFunction(ExternalFunctionId),
    Struct(StructId),
    Static(StaticId),
    Module(ModuleId),
    Trait(TraitId),
}

#[derive(Debug)]
pub struct BakedStruct {
    pub name: GlobalStr,
    pub elements: Vec<(GlobalStr, TypeRef)>,
    pub location: Location,
    pub global_impl: HashMap<GlobalStr, FunctionId>,
    pub impls: Vec<(GlobalStr, HashMap<GlobalStr, FunctionId>, Location)>,
    pub annotations: Annotations,
    pub module_id: ModuleId,
    pub generics: Vec<Generic>,
}

#[derive(Default)]
pub struct ModuleContext {
    pub modules: RwLock<Vec<Module>>,
    pub functions: RwLock<Vec<(FunctionContract, Statement, ModuleId)>>,
    pub external_functions: RwLock<Vec<(FunctionContract, Option<Statement>, ModuleId)>>,
    pub statics: RwLock<Vec<(TypeRef, LiteralValue, ModuleId, Location, Annotations)>>, // TODO: const-eval for statics
    pub structs: RwLock<Vec<BakedStruct>>,
    pub traits: RwLock<Vec<Trait>>,
}

impl Debug for ModuleContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ModuleContext")
            .field(
                "functions",
                &self.functions.read().expect("read-write lock is poisoned"),
            )
            .field(
                "external_functions",
                &self
                    .external_functions
                    .read()
                    .expect("read-write lock is poisoned"),
            )
            .field(
                "statics",
                &self.statics.read().expect("read-write lock is poisoned"),
            )
            .field(
                "structs",
                &self.structs.read().expect("read-write lock is poisoned"),
            )
            .finish()
    }
}

pub struct Module {
    pub context: Arc<ModuleContext>,
    pub scope: HashMap<GlobalStr, ModuleScopeValue>,
    pub imports: HashMap<GlobalStr, (Location, usize, Vec<GlobalStr>)>,
    pub exports: HashMap<GlobalStr, GlobalStr>,
}

impl Debug for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Module")
            .field("scope", &self.scope)
            .field("imports", &self.imports)
            .field("exports", &self.exports)
            .finish()
    }
}

impl Module {
    pub fn new(
        context: Arc<ModuleContext>,
        imports: HashMap<GlobalStr, (Location, usize, Vec<GlobalStr>)>,
    ) -> Self {
        Self {
            context,
            imports,
            scope: HashMap::new(),
            exports: HashMap::new(),
        }
    }

    pub fn push_fn(
        &mut self,
        contract: FunctionContract,
        body: Statement,
        module: ModuleId,
    ) -> FunctionId {
        let mut writer = self
            .context
            .functions
            .write()
            .expect("read-write lock is poisoned");
        writer.push((contract, body, module));
        writer.len() - 1
    }

    pub fn push_all(
        &mut self,
        statements: Vec<Statement>,
        module_id: ModuleId,
    ) -> Result<(), Vec<ProgramFormingError>> {
        let errors = statements
            .into_iter()
            .map(|statement| self.push_statement(statement, module_id))
            .filter_map(|el| el.err())
            .collect::<Vec<_>>();

        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(())
        }
    }

    pub fn push_statement(
        &mut self,
        statement: Statement,
        module_id: ModuleId,
    ) -> Result<(), ProgramFormingError> {
        match statement {
            Statement::Function(contract, mut body) => {
                let Some(name) = contract.name.clone() else {
                    return Err(ProgramFormingError::AnonymousFunctionAtGlobalLevel(
                        contract.location.clone(),
                    ));
                };

                if self.scope.contains_key(&name) || self.imports.contains_key(&name) {
                    return Err(ProgramFormingError::IdentAlreadyDefined(
                        contract.location.clone(),
                        name,
                    ));
                }

                body.bake_functions(self, module_id);
                let fn_id = self.push_fn(contract, *body, module_id);
                self.scope.insert(name, ModuleScopeValue::Function(fn_id));
            }
            Statement::Trait(mut r#trait) => {
                r#trait.module_id = module_id;
                if self.scope.contains_key(&r#trait.name)
                    || self.imports.contains_key(&r#trait.name)
                {
                    return Err(ProgramFormingError::IdentAlreadyDefined(
                        r#trait.location.clone(),
                        r#trait.name.clone(),
                    ));
                }

                let mut writer = self.context.traits.write().expect("failed to lock rw lock");
                let name = r#trait.name.clone();
                writer.push(r#trait);
                self.scope
                    .insert(name, ModuleScopeValue::Trait(writer.len() - 1));
            }
            Statement::Struct {
                name,
                elements,
                location,
                global_impl,
                impls,
                annotations,
                generics,
            } => {
                if self.scope.contains_key(&name) || self.imports.contains_key(&name) {
                    return Err(ProgramFormingError::IdentAlreadyDefined(
                        location.clone(),
                        name,
                    ));
                }

                let mut baked_global_impl = HashMap::new();
                let mut baked_impls = Vec::new();

                for (name, (contract, body)) in global_impl {
                    baked_global_impl.insert(name, self.push_fn(contract, body, module_id));
                }

                for (trait_name, implementation, loc) in impls {
                    let mut baked_impl = HashMap::new();
                    for (name, (contract, body)) in implementation {
                        baked_impl.insert(name, self.push_fn(contract, body, module_id));
                    }
                    baked_impls.push((trait_name, baked_impl, loc));
                }

                let baked_struct = BakedStruct {
                    name: name.clone(),
                    elements,
                    annotations,
                    location,
                    global_impl: baked_global_impl,
                    impls: baked_impls,
                    module_id,
                    generics,
                };

                let mut writer = self
                    .context
                    .structs
                    .write()
                    .expect("read-write lock is poisoned");
                writer.push(baked_struct);
                self.scope
                    .insert(name, ModuleScopeValue::Struct(writer.len() - 1));
            }
            Statement::Var(_, _, None, location, _) => {
                return Err(ProgramFormingError::GlobalValueNoType(location.clone()))
            }
            Statement::Var(name, expr, Some(typ), location, annotations) => {
                if self.scope.contains_key(&name) || self.imports.contains_key(&name) {
                    return Err(ProgramFormingError::IdentAlreadyDefined(
                        location.clone(),
                        name,
                    ));
                }

                let Expression::Literal(value, _) = expr else {
                    return Err(ProgramFormingError::GlobalValueNoLiteral(
                        expr.loc().clone(),
                    ));
                };
                let mut writer = self
                    .context
                    .statics
                    .write()
                    .expect("read-write lock is poisoned");
                writer.push((typ, value, module_id, location, annotations));
                self.scope
                    .insert(name, ModuleScopeValue::Static(writer.len() - 1));
            }
            Statement::ExternalFunction(contract, mut body) => {
                let Some(name) = contract.name.clone() else {
                    return Err(ProgramFormingError::AnonymousFunctionAtGlobalLevel(
                        contract.location.clone(),
                    ));
                };

                if self.scope.contains_key(&name) || self.imports.contains_key(&name) {
                    return Err(ProgramFormingError::IdentAlreadyDefined(
                        contract.location.clone(),
                        name,
                    ));
                }

                if let Some(ref mut body) = body {
                    body.bake_functions(self, module_id);
                }
                let mut writer = self
                    .context
                    .external_functions
                    .write()
                    .expect("read-write lock is poisoned");
                writer.push((contract, body.map(|v| *v), module_id));
                self.scope
                    .insert(name, ModuleScopeValue::ExternalFunction(writer.len() - 1));
            }
            Statement::Export(key, exported_key, loc) => {
                if !self.scope.contains_key(&key) && !self.imports.contains_key(&key) {
                    return Err(ProgramFormingError::IdentNotDefined(loc, key));
                }
                self.exports.insert(exported_key, key);
            }
            _ => {
                return Err(ProgramFormingError::NoCodeOutsideOfFunctions(
                    statement.loc().clone(),
                ))
            }
        }

        Ok(())
    }
}
