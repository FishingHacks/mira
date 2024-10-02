use std::collections::HashMap;

use crate::{
    error::ProgrammingLangProgramFormingError,
    globals::GlobalStr,
    parser::{
        Expression, FunctionContract, Implementation, LiteralValue, Statement, Struct, TypeRef,
    },
};

pub type FunctionId = usize;

#[derive(Debug, Default)]
pub struct Scope {
    pub structs: HashMap<GlobalStr, Struct>,
    pub functions: HashMap<GlobalStr, FunctionId>,
    pub external_functions: HashMap<GlobalStr, FunctionContract>,
    pub static_values: HashMap<GlobalStr, (TypeRef, LiteralValue)>,
}

#[derive(Debug)]
pub enum ExportItem {
    Function(usize),
    ExternalFunction(GlobalStr),
    StaticValue(GlobalStr),
    Struct(GlobalStr),
    Import(GlobalStr),
}

#[derive(Debug)]
pub struct Module {
    pub global_scope: Scope,
    pub functions: Vec<(FunctionContract, Statement)>,
    pub imports: HashMap<GlobalStr, (usize, Vec<GlobalStr>)>,
    pub exports: HashMap<GlobalStr, ExportItem>,
}

impl Module {
    pub fn new(imports: HashMap<GlobalStr, (usize, Vec<GlobalStr>)>) -> Self {
        Self {
            global_scope: Scope::default(),
            functions: Vec::new(),
            imports,
            exports: HashMap::new(),
        }
    }

    pub fn push_fn(&mut self, contract: FunctionContract, statement: Statement) -> FunctionId {
        self.functions.push((contract, statement));
        return (self.functions.len() - 1) as FunctionId;
    }

    pub fn get_fn(&self, id: FunctionId) -> Option<&(FunctionContract, Statement)> {
        return self.functions.get(id);
    }

    pub fn push_all(
        &mut self,
        statements: Vec<Statement>,
    ) -> Result<(), Vec<ProgrammingLangProgramFormingError>> {
        let errors = statements
            .into_iter()
            .map(|statement| self.push_statement(statement))
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
    ) -> Result<(), ProgrammingLangProgramFormingError> {
        let loc = statement.loc().clone();
        match statement {
            Statement::Function(contract, mut statement) => {
                let Some(name) = contract.name.clone() else {
                    return Err(
                        ProgrammingLangProgramFormingError::AnonymousFunctionAtGlobalLevel(loc),
                    );
                };

                statement.bake_functions(self);
                let fn_id = self.push_fn(contract, *statement);
                self.global_scope.functions.insert(name, fn_id);
            }
            Statement::Struct {
                name,
                elements: fields,
                location,
                global_impl,
                impls,
                annotations,
            } => {
                if self.is_defined(&name) {
                    return Err(ProgrammingLangProgramFormingError::IdentAlreadyDefined(
                        location, name,
                    ));
                }

                let mut struct_global_impl: Implementation = HashMap::new();

                for (function_name, mut function) in global_impl.into_iter() {
                    function.bake(self);
                    struct_global_impl.insert(function_name, function.get_baked_id());
                }

                let mut struct_impls: Vec<(GlobalStr, Implementation)> = Vec::new();

                for (trait_name, trait_impl) in impls.into_iter() {
                    let mut cur_impl: Implementation = HashMap::new();

                    for (function_name, mut function) in trait_impl.into_iter() {
                        function.bake(self);
                        cur_impl.insert(function_name, function.get_baked_id());
                    }

                    struct_impls.push((trait_name, cur_impl));
                }

                let typ = Struct {
                    name: name.clone(),
                    fields,
                    global_impl: struct_global_impl,
                    trait_impls: struct_impls,
                    annotations,
                };
                self.global_scope.structs.insert(name, typ);
            }
            Statement::Var(_, expr, None, _) => {
                return Err(ProgrammingLangProgramFormingError::GlobalValueNoType(
                    expr.loc().clone(),
                ))
            }
            Statement::Var(name, expr, Some(typ), location) => {
                if self.is_defined(&name) {
                    return Err(ProgrammingLangProgramFormingError::IdentAlreadyDefined(
                        location, name,
                    ));
                }
                if let Expression::Literal(val, _) = expr {
                    self.global_scope.static_values.insert(name, (typ, val));
                } else {
                    return Err(ProgrammingLangProgramFormingError::GlobalValueNoLiteral(
                        expr.loc().clone(),
                    ));
                }
            }
            Statement::ExternalFunction(contract) => {
                if let Some(name) = contract.name.clone() {
                    if self.is_defined(&name) {
                        return Err(ProgrammingLangProgramFormingError::IdentAlreadyDefined(
                            contract.location.clone(),
                            name,
                        ));
                    }
                    self.global_scope
                        .external_functions
                        .insert(name.clone(), contract);
                } else {
                    return Err(
                        ProgrammingLangProgramFormingError::AnonymousFunctionAtGlobalLevel(
                            contract.location,
                        ),
                    );
                }
            }
            Statement::Export(key, loc) => {
                if let Some(func) = self.global_scope.functions.get(&key) {
                    self.exports.insert(key, ExportItem::Function(*func));
                } else if let Some(external_func) = self.global_scope.external_functions.get(&key) {
                    self.exports.insert(
                        key,
                        ExportItem::ExternalFunction(
                            external_func
                                .name
                                .clone()
                                .expect("external function without a name"),
                        ),
                    );
                } else if self.global_scope.structs.contains_key(&key) {
                    self.exports.insert(key.clone(), ExportItem::Struct(key));
                } else if self.global_scope.static_values.contains_key(&key) {
                    self.exports
                        .insert(key.clone(), ExportItem::StaticValue(key));
                } else if self.exports.contains_key(&key) {
                    self.exports.insert(key.clone(), ExportItem::Import(key));
                } else {
                    return Err(ProgrammingLangProgramFormingError::IdentNotDefined(
                        loc, key,
                    ));
                }
            }
            _ => return Err(ProgrammingLangProgramFormingError::NoCodeOutsideOfFunctions(loc)),
        }

        Ok(())
    }

    fn is_defined(&self, key: &GlobalStr) -> bool {
        self.imports.contains_key(key)
            || self.global_scope.functions.contains_key(key)
            || self.global_scope.structs.contains_key(key)
            || self.global_scope.static_values.contains_key(key)
            || self.global_scope.external_functions.contains_key(key)
    }
}
