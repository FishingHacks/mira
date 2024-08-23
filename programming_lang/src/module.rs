use std::collections::HashMap;

use crate::{
    error::ProgrammingLangProgramFormingError,
    globals::GlobalString,
    parser::{Expression, FunctionContract, LiteralValue, Statement, Type, TypeRef},
};

pub type FunctionId = usize;

#[derive(Debug, Default)]
pub struct Scope {
    types: HashMap<GlobalString, Type>,
    functions: HashMap<GlobalString, FunctionId>,
    external_functions: HashMap<GlobalString, FunctionContract>,
    static_values: HashMap<GlobalString, (TypeRef, LiteralValue)>,
    constant_values: HashMap<GlobalString, (TypeRef, LiteralValue)>,
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
        let loc = statement.loc();
        match statement {
            Statement::Function(contract, mut statement) => {
                let Some(name) = contract.name else {
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
                elements,
                location: _,
            } => {
                self.global_scope
                    .types
                    .insert(name, Type::Struct(name, elements));
            }
            Statement::Var(_, expr, None, _) | Statement::Const(_, expr, None, _) => {
                return Err(ProgrammingLangProgramFormingError::GlobalValueNoType(
                    expr.loc(),
                ))
            }
            Statement::Var(name, expr, Some(typ), _) => {
                if let Expression::Literal(val, _) = expr {
                    self.global_scope.static_values.insert(name, (typ, val));
                } else {
                    return Err(ProgrammingLangProgramFormingError::GlobalValueNoLiteral(
                        expr.loc(),
                    ));
                }
            }
            Statement::Const(name, expr, Some(typ), _) => {
                if let Expression::Literal(val, _) = expr {
                    self.global_scope.constant_values.insert(name, (typ, val));
                } else {
                    return Err(ProgrammingLangProgramFormingError::GlobalValueNoLiteral(
                        expr.loc(),
                    ));
                }
            }
            Statement::ExternalFunction(contract) => {
                if let Some(name) = contract.name {
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
