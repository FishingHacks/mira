use std::collections::HashMap;

use crate::{
    error::ProgrammingLangProgramFormingError,
    globals::GlobalString,
    parser::{Expression, FunctionContract, Statement, Type, TypeRef},
};

#[derive(Debug)]
pub struct Module {
    types: HashMap<GlobalString, Type>,
    functions: HashMap<GlobalString, (FunctionContract, Box<Statement>)>,
    static_values: HashMap<GlobalString, (Option<TypeRef>, Expression)>,
    constant_values: HashMap<GlobalString, (Option<TypeRef>, Expression)>,
}

impl Module {
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
            functions: HashMap::new(),
            static_values: HashMap::new(),
            constant_values: HashMap::new(),
        }
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
            Statement::Function(contract, statements) => {
                let Some(name) = contract.name else {
                    return Err(
                        ProgrammingLangProgramFormingError::AnonymousFunctionAtGlobalLevel(loc),
                    );
                };

                self.functions.insert(name, (contract, statements));
            }
            Statement::Struct { name, elements, location: _ } => {
                self.types.insert(name, Type::Struct(name, elements));
            }
            Statement::Var(name, expr, typ, _) => {
                self.static_values.insert(name, (typ, expr));
            }
            Statement::Const(name, expr, typ, _) => {
                self.constant_values.insert(name, (typ, expr));
            }
            _ => return Err(ProgrammingLangProgramFormingError::NoCodeOutsideOfFunctions(loc)),
        }
        
        Ok(())
    }
}
