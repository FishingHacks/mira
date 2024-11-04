use core::panic;
use std::collections::HashMap;

use crate::{
    globals::GlobalStr,
    parser::{Expression, Statement},
    typechecking::ScopeItem,
};

use super::{
    type_resolution::Type, ProgrammingLangTypecheckingError, ResolvedFunctionContract,
    TypecheckedModule, TypecheckingContext,
};

#[derive(Clone)]
enum ScopeEntry {
    FunctionPtr(usize),
    ExternalFunctionPtr(usize),
    Type(Type),
    Value(Type),
}

struct Scopes(Vec<HashMap<GlobalStr, ScopeEntry>>);

impl Scopes {
    pub fn from_module_root(module: &TypecheckedModule, context: &TypecheckingContext) -> Self {
        let mut scopes = Self(vec![HashMap::with_capacity(module.scope.capacity())]);

        for (k, v) in module.scope.iter() {
            assert!(scopes.insert(
                k.clone(),
                match *v {
                    ScopeItem::Struct(id) => ScopeEntry::Type(Type::Struct {
                        structure: context.structs.read().expect("read-write lock is poisoned")[id]
                            .clone(),
                        num_references: 0
                    }),
                    ScopeItem::Function(id) => ScopeEntry::FunctionPtr(id),
                    ScopeItem::ExternalFunction(id) => ScopeEntry::ExternalFunctionPtr(id),
                    ScopeItem::StaticValue(id) => ScopeEntry::Value(
                        context.statics.read().expect("read-write lock is poisoned")[id]
                            .0
                            .clone()
                    ),
                }
            ));
        }

        scopes
    }

    pub fn new() -> Self {
        Self(vec![HashMap::new()])
    }

    pub fn push_scope(&mut self) {
        self.0.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) -> HashMap<GlobalStr, ScopeEntry> {
        if self.0.len() <= 1 {
            panic!("trying to pop the root scope")
        }
        self.0.pop().expect("we should always have a scope to pop")
    }

    /// returns a bool indicating if we could insert the key.
    pub fn insert(&mut self, key: GlobalStr, value: ScopeEntry) -> bool {
        let last = self.0.len() - 1;
        if self.0[last].contains_key(&key) {
            return false;
        }
        assert!(self.0[last].insert(key, value).is_none());
        true
    }

    pub fn insert_override(&mut self, key: GlobalStr, value: ScopeEntry) {
        let last = self.0.len() - 1;
        self.0[last].insert(key, value);
    }

    pub fn get(&self, key: &GlobalStr) -> Option<&ScopeEntry> {
        let len = self.0.len();
        for i in 1..len {
            if let Some(v) = self.0[len - i].get(key) {
                return Some(v);
            }
        }
        None
    }

    pub fn get_root(&self, key: &GlobalStr) -> Option<&ScopeEntry> {
        self.0[0].get(key)
    }
}

pub fn typecheck_function(
    module: &TypecheckedModule,
    context: &TypecheckingContext,
    function_contract: &ResolvedFunctionContract,
    statement: &Statement,
) -> Result<(), ProgrammingLangTypecheckingError> {
    let mut scopes = Scopes::from_module_root(module, context);

    for name in function_contract.generics.iter() {
        scopes.insert_override(
            name.clone(),
            ScopeEntry::Type(Type::Generic(name.clone(), 0)),
        );
    }

    for (name, typ) in function_contract.arguments.iter() {
        scopes.insert_override(name.clone(), ScopeEntry::Value(typ.clone()));
    }

    typecheck_statement(
        module,
        context,
        &mut scopes,
        &function_contract.return_type,
        statement,
    )
}

pub fn typecheck_statement(
    module: &TypecheckedModule,
    context: &TypecheckingContext,
    scopes: &mut Scopes,
    return_type: &Type,
    statement: &Statement,
) -> Result<(), ProgrammingLangTypecheckingError> {
    match statement {
        Statement::If {
            condition,
            if_stmt,
            else_stmt,
            location,
            annotations,
        } => todo!(),
        Statement::While {
            condition,
            child,
            location,
            annotations,
        } => todo!(),
        Statement::For {
            iterator,
            var_name,
            child,
            location,
            annotations,
        } => todo!(),
        Statement::Return(expression, location) => todo!(),
        Statement::Block(statements, _, _) => {
            scopes.push_scope();
            for statement in statements.iter() {
                typecheck_statement(module, context, scopes, return_type, statement)?;
            }
            scopes.pop_scope();
        }
        Statement::Var(global_str, expression, type_ref, location) => {}
        Statement::Expression(expression) => {
            _ = typecheck_expression(module, context, scopes, expression)
        }
        Statement::BakedFunction(..)
        | Statement::Function(..)
        | Statement::ExternalFunction(..) => {
            panic!("unbaked function in statements")
        }
        Statement::Struct { .. } => panic!("struct in statement"),
        Statement::Export(..) => panic!("struct in statement"),
    }
    Ok(())
}

pub fn typecheck_expression(
    module: &TypecheckedModule,
    context: &TypecheckingContext,
    scopes: &mut Scopes,
    expr: &Expression,
) -> Result<Type, ProgrammingLangTypecheckingError> {
    todo!()
}
