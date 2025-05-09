use parking_lot::RwLock;
use std::{collections::HashMap, fmt::Debug, path::Path, sync::Arc};

use crate::{
    annotations::Annotations,
    error::ProgramFormingError,
    globals::GlobalStr,
    parser::{Expression, FunctionContract, Generic, LiteralValue, Statement, Trait, TypeRef},
    store::{Store, StoreKey},
    tokenizer::Location,
};

#[derive(Debug, Copy, Clone)]
pub enum ModuleScopeValue {
    Function(StoreKey<Function>),
    ExternalFunction(StoreKey<ExternalFunction>),
    Struct(StoreKey<BakedStruct>),
    Static(StoreKey<Static>),
    Module(StoreKey<Module>),
    Trait(StoreKey<Trait>),
}

#[derive(Debug)]
pub struct BakedStruct {
    pub name: GlobalStr,
    pub elements: Vec<(GlobalStr, TypeRef)>,
    pub location: Location,
    pub global_impl: HashMap<GlobalStr, StoreKey<Function>>,
    pub impls: Vec<(GlobalStr, HashMap<GlobalStr, StoreKey<Function>>, Location)>,
    pub annotations: Annotations,
    pub module_id: StoreKey<Module>,
    pub generics: Vec<Generic>,
}

pub type Function = (FunctionContract, Statement, StoreKey<Module>);
pub type ExternalFunction = (FunctionContract, Option<Statement>, StoreKey<Module>);
pub type Static = (
    TypeRef,
    LiteralValue,
    StoreKey<Module>,
    Location,
    Annotations,
);

#[derive(Default)]
pub struct ModuleContext {
    pub modules: RwLock<Store<Module>>,
    pub functions: RwLock<Store<Function>>,
    pub external_functions: RwLock<Store<ExternalFunction>>,
    pub statics: RwLock<Store<Static>>, // TODO: const-eval for statics
    pub structs: RwLock<Store<BakedStruct>>,
    pub traits: RwLock<Store<Trait>>,
}

impl Debug for ModuleContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ModuleContext")
            .field("functions", &self.functions.read())
            .field("external_functions", &self.external_functions.read())
            .field("statics", &self.statics.read())
            .field("structs", &self.structs.read())
            .finish()
    }
}

pub struct Module {
    pub scope: HashMap<GlobalStr, ModuleScopeValue>,
    pub imports: HashMap<GlobalStr, (Location, StoreKey<Module>, Vec<GlobalStr>)>,
    pub exports: HashMap<GlobalStr, GlobalStr>,
    pub path: Arc<Path>,
    pub root: Arc<Path>,
    pub assembly: Vec<(Location, String)>,
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
        imports: HashMap<GlobalStr, (Location, StoreKey<Module>, Vec<GlobalStr>)>,
        path: Arc<Path>,
        root: Arc<Path>,
    ) -> Self {
        Self {
            imports,
            scope: HashMap::new(),
            exports: HashMap::new(),
            path,
            root,
            assembly: Vec::new(),
        }
    }

    pub fn push_fn(
        &mut self,
        contract: FunctionContract,
        mut body: Statement,
        module: StoreKey<Module>,
        context: &ModuleContext,
    ) -> StoreKey<Function> {
        let key = {
            let mut writer = context.functions.write();
            let loc = contract.location.clone();
            writer.insert((
                contract,
                Statement::Return(None, loc), /* "zeroed" statement */
                module,
            ))
        };
        // we have to bake the body *after* pushing the function to ensure the function is
        // typechecked before any of its child elements, such as closures, as we need to evaluate
        // the function body before the closure to fill in the closures types.
        body.bake_functions(self, module, context);
        context.functions.write().get_mut(&key).unwrap().1 = body;

        key
    }

    pub fn push_all(
        &mut self,
        statements: Vec<Statement>,
        module_id: StoreKey<Module>,
        context: &ModuleContext,
    ) -> Result<(), Vec<ProgramFormingError>> {
        let errors = statements
            .into_iter()
            .map(|statement| self.push_statement(statement, module_id, context))
            .filter_map(|el| el.err())
            .collect::<Vec<_>>();

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    pub fn push_statement(
        &mut self,
        statement: Statement,
        module_id: StoreKey<Module>,
        context: &ModuleContext,
    ) -> Result<(), ProgramFormingError> {
        match statement {
            Statement::Function(contract, body) => {
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

                let fn_id = self.push_fn(contract, *body, module_id, context);
                self.scope.insert(name, ModuleScopeValue::Function(fn_id));
            }
            Statement::Trait(mut r#trait) => {
                r#trait.module = module_id;
                if self.scope.contains_key(&r#trait.name)
                    || self.imports.contains_key(&r#trait.name)
                {
                    return Err(ProgramFormingError::IdentAlreadyDefined(
                        r#trait.location.clone(),
                        r#trait.name.clone(),
                    ));
                }

                let mut writer = context.traits.write();
                let name = r#trait.name.clone();
                let key = writer.insert(r#trait);
                self.scope.insert(name, ModuleScopeValue::Trait(key));
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
                    baked_global_impl
                        .insert(name, self.push_fn(contract, body, module_id, context));
                }

                for (trait_name, implementation, loc) in impls {
                    let mut baked_impl = HashMap::new();
                    for (name, (contract, body)) in implementation {
                        baked_impl.insert(name, self.push_fn(contract, body, module_id, context));
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

                let mut writer = context.structs.write();
                let key = writer.insert(baked_struct);
                self.scope.insert(name, ModuleScopeValue::Struct(key));
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
                let mut writer = context.statics.write();
                let key = writer.insert((typ, value, module_id, location, annotations));
                self.scope.insert(name, ModuleScopeValue::Static(key));
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
                    body.bake_functions(self, module_id, context);
                }
                let mut writer = context.external_functions.write();
                let key = writer.insert((contract, body.map(|v| *v), module_id));
                self.scope
                    .insert(name, ModuleScopeValue::ExternalFunction(key));
            }
            Statement::Export(key, exported_key, loc) => {
                if !self.scope.contains_key(&key) && !self.imports.contains_key(&key) {
                    return Err(ProgramFormingError::IdentNotDefined(loc, key));
                }
                self.exports.insert(exported_key, key);
            }
            Statement::ModuleAsm(loc, strn) => self.assembly.push((loc, strn)),
            _ => {
                return Err(ProgramFormingError::NoCodeOutsideOfFunctions(
                    statement.loc().clone(),
                ))
            }
        }

        Ok(())
    }
}
