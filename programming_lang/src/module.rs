use std::{collections::HashMap, rc::Rc};

use crate::{
    error::ProgrammingLangProgramFormingError,
    parser::{
        Expression, FunctionContract, Implementation, LiteralValue, Statement, Struct, Type,
        TypeRef,
    },
};

pub type FunctionId = usize;

#[derive(Debug, Default)]
pub struct Scope {
    types: HashMap<Rc<str>, Type>,
    functions: HashMap<Rc<str>, FunctionId>,
    external_functions: HashMap<Rc<str>, FunctionContract>,
    static_values: HashMap<Rc<str>, (TypeRef, LiteralValue)>,
}

#[derive(Debug)]
pub struct Module {
    global_scope: Scope,
    functions: Vec<(FunctionContract, Statement)>,
}

impl Module {
    pub fn new() -> Self {
        Self {
            global_scope: Scope::default(),
            functions: Vec::new(),
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
                location: _,
                global_impl,
                impls,
                annotations,
            } => {
                let mut struct_global_impl: Implementation = HashMap::new();

                for (function_name, mut function) in global_impl.into_iter() {
                    function.bake(self);
                    struct_global_impl.insert(function_name, function.get_baked_id());
                }

                let mut struct_impls: Vec<(Rc<str>, Implementation)> = Vec::new();

                for (trait_name, trait_impl) in impls.into_iter() {
                    let mut cur_impl: Implementation = HashMap::new();

                    for (function_name, mut function) in trait_impl.into_iter() {
                        function.bake(self);
                        cur_impl.insert(function_name, function.get_baked_id());
                    }

                    struct_impls.push((trait_name, cur_impl));
                }

                let typ = Type::Struct(Box::new(Struct {
                    name: name.clone(),
                    fields,
                    global_impl: struct_global_impl,
                    trait_impls: struct_impls,
                    annotations,
                }));
                self.global_scope.types.insert(name, typ);
            }
            Statement::Var(_, expr, None, _) => {
                return Err(ProgrammingLangProgramFormingError::GlobalValueNoType(
                    expr.loc().clone(),
                ))
            }
            Statement::Var(name, expr, Some(typ), _) => {
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
                    self.global_scope.external_functions.insert(name, contract);
                } else {
                    return Err(
                        ProgrammingLangProgramFormingError::AnonymousFunctionAtGlobalLevel(
                            contract.location,
                        ),
                    );
                }
            }
            _ => return Err(ProgrammingLangProgramFormingError::NoCodeOutsideOfFunctions(loc)),
        }

        Ok(())
    }
}
