use std::{collections::HashMap, rc::Rc};

use error::ProgrammingLangTypecheckingError;
use type_resolution::resolve_type;
pub use type_resolution::{Type, TypecheckedModule};

use crate::{
    module::Module,
    parser::{FunctionContract, Statement},
};
pub mod error;
mod type_resolution;

#[derive(Debug)]
struct TypecheckingScope {
    parent: Option<Box<TypecheckingScope>>,
    values: HashMap<Rc<str>, Type>,
}

impl TypecheckingScope {
    /// Inserts a new variable and its type into the scope.
    /// Returns `true` if a variable with such a name exists already
    pub fn insert(&mut self, key: Rc<str>, typ: Type) -> bool {
        if self.values.contains_key(&key) {
            return true;
        }
        self.values.insert(key, typ);
        false
    }
}

pub fn typecheck_statement(module: &Module, statement: &Statement, return_type: Type) {}

pub fn typecheck_function(
    module: &mut Module,
    typechecked_module: &mut TypecheckedModule,
    contract: &FunctionContract,
    body: &Statement,
) -> Result<(), ProgrammingLangTypecheckingError> {
    let resolved_type = resolve_type(&contract.return_type, module, typechecked_module)?;
    let mut scope = TypecheckingScope {
        parent: None,
        values: HashMap::with_capacity(contract.arguments.len()),
    };

    for argument in contract.arguments.iter() {
        let resolved_type = resolve_type(&argument.typ, module, typechecked_module)?;
        assert!(
            !scope.insert(argument.name.clone(), resolved_type),
            "there should be no variables named like the arguments in the scope"
        );
    }

    println!("Scope: {scope:?}");

    typecheck_statement(module, body, resolved_type);
    Ok(())
}

pub fn typecheck_module(
    mut module: Module,
    mut typechecked_module: TypecheckedModule,
) -> Result<
    (TypecheckedModule, Module),
    (ProgrammingLangTypecheckingError, TypecheckedModule, Module),
> {
    while let Some((contract, body)) = module.functions.pop() {
        if let Err(e) = typecheck_function(&mut module, &mut typechecked_module, &contract, &body) {
            return Err((e, typechecked_module, module));
        }
    }

    Ok((typechecked_module, module))
}
