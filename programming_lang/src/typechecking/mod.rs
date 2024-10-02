use std::collections::HashMap;

use error::ProgrammingLangTypecheckingError;
use type_resolution::resolve_type;
pub use type_resolution::{Type, TypecheckedModule};

use crate::{
    globals::GlobalStr,
    module::Module,
    parser::{Expression, FunctionContract, Statement},
};
pub mod error;
mod type_resolution;

#[derive(Debug)]
struct Scopes(Vec<HashMap<GlobalStr, Type>>);

impl Scopes {
    pub fn new() -> Self {
        Self(vec![HashMap::new()])
    }

    pub fn push_scope(&mut self) {
        self.0.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        if self.0.len() > 1 {
            _ = self.0.pop();
        } else {
            panic!("tried to pop root scope");
        }
    }

    /// tries to insert a new key into the current scope.
    /// returns false if such a key already exists.
    pub fn insert(&mut self, name: GlobalStr, value: Type) -> bool {
        let len = self.0.len();
        let scope = &mut self.0[len - 1];
        if scope.contains_key(&name) {
            false
        } else {
            scope.insert(name, value);
            true
        }
    }

    pub fn get(&self, name: &GlobalStr) -> Option<&Type> {
        for i in 1..=self.0.len() {
            if let Some(v) = self.0[self.0.len() - i].get(name) {
                return Some(v);
            }
        }
        None
    }
}

struct TypecheckingContext {
    pub module: Module,
    pub typechecked_module: TypecheckedModule,
    pub scopes: Scopes,
}

fn typecheck_expr(
    ctx: &mut TypecheckingContext,
    expr: &Expression,
) -> Result<Type, ProgrammingLangTypecheckingError> {
    Ok(match expr {
        Expression::Literal(literal_value, _) => match literal_value {
            crate::parser::LiteralValue::String(_) => Type::PrimitiveStr(0),
            crate::parser::LiteralValue::Array(vec) => todo!(),
            crate::parser::LiteralValue::Struct(_, path) => todo!(),
            crate::parser::LiteralValue::Float(_) => Type::PrimitiveF32(0),
            crate::parser::LiteralValue::SInt(_) => Type::PrimitiveI32(0),
            crate::parser::LiteralValue::UInt(_) => Type::PrimitiveU32(0),
            crate::parser::LiteralValue::Bool(_) => Type::PrimitiveBool(0),
            crate::parser::LiteralValue::Dynamic(path) => todo!(),
            crate::parser::LiteralValue::AnonymousFunction(..) => {
                unreachable!("should have baked the anonymous function")
            }
            crate::parser::LiteralValue::BakedAnonymousFunction(_) => todo!(),
        },
        Expression::Unary {
            operator,
            right_side,
        } => todo!(),
        Expression::Binary {
            operator,
            right_side,
            left_side,
        } => todo!(),
        Expression::FunctionCall {
            identifier,
            arguments,
        } => todo!(),
        Expression::Indexing {
            left_side,
            right_side,
        } => todo!(),
        Expression::MemberAccess {
            left_side,
            right_side,
            loc,
        } => todo!(),
        Expression::Assignment {
            left_side,
            right_side,
            loc,
        } => todo!(),
        Expression::Range {
            left_side,
            right_side,
            inclusive,
            loc,
        } => todo!(),
        Expression::TypeCast {
            left_side,
            new_type,
            loc,
        } => todo!(),
    })
}

fn typecheck_statement<'a>(
    ctx: &mut TypecheckingContext,
    statement: &Statement,
    return_type: &Type,
) -> Result<(), ProgrammingLangTypecheckingError> {
    match statement {
        Statement::If {
            condition,
            if_stmt,
            else_stmt,
            location: _,
            annotations: _,
        } => {
            typecheck_expr(ctx, condition)?;
            ctx.scopes.push_scope();
            typecheck_statement(ctx, if_stmt, return_type)?;
            ctx.scopes.pop_scope();
            if let Some(else_stmt) = else_stmt {
                ctx.scopes.push_scope();
                typecheck_statement(ctx, &else_stmt, return_type)?;
                ctx.scopes.pop_scope();
            }
        }
        Statement::While {
            condition,
            child,
            location: _,
            annotations: _,
        } => {
            typecheck_expr(ctx, condition)?;
            ctx.scopes.push_scope();
            typecheck_statement(ctx, child, return_type)?;
            ctx.scopes.pop_scope();
        }
        Statement::For {
            iterator: _,
            var_name: _,
            child: _,
            location: _,
            annotations: _,
        } => {
            todo!("find a way to get the type of the iterator");
            //let typ = typecheck_expr(module, scopes, iterator)?;
            //scopes.push_scope();
            //scopes.insert(var_name.clone(), typ);
            //typecheck_statement(module, scopes, child, return_type)?;
            //scopes.pop_scope();
        }
        Statement::Return(expression, location) => {
            let typ = if let Some(expression) = expression {
                typecheck_expr(ctx, expression)?
            } else {
                Type::PrimitiveVoid(0)
            };
            if typ != *return_type {
                return Err(ProgrammingLangTypecheckingError::UnexpectedType {
                    loc: expression
                        .as_ref()
                        .map(Expression::loc)
                        .unwrap_or(location)
                        .clone(),
                    expected: return_type.clone(),
                    found: typ,
                });
            }
        }
        Statement::Block(statements, _, _) => {
            for statement in statements.iter() {
                typecheck_statement(ctx, statement, return_type)?;
            }
        }
        Statement::Var(global_str, expression, type_ref, location) => {
            let evalutated_type = typecheck_expr(ctx, expression)?;
            if let Some(type_ref) = type_ref {
                let typ = resolve_type(type_ref, &mut ctx.module, &mut ctx.typechecked_module)?;
                if typ != evalutated_type {
                    return Err(ProgrammingLangTypecheckingError::UnexpectedType {
                        loc: location.clone(),
                        expected: typ,
                        found: evalutated_type,
                    });
                }
            }
            if !ctx.scopes.insert(global_str.clone(), evalutated_type) {
                return Err(ProgrammingLangTypecheckingError::VariableAlreadyDeclared {
                    loc: location.clone(),
                    name: global_str.clone(),
                });
            }
        }
        Statement::Expression(expression) => {
            typecheck_expr(ctx, expression)?;
        }
        Statement::ExternalFunction(_)
        | Statement::BakedFunction(_, _)
        | Statement::Function(_, _) => {
            unreachable!("function in statement")
        }
        Statement::Export(..) => unreachable!("export in statement"),
        Statement::Struct { .. } => unreachable!("struct in statement"),
    }
    Ok(())
}

fn typecheck_function(
    ctx: &mut TypecheckingContext,
    contract: &FunctionContract,
    body: &Statement,
) -> Result<(), ProgrammingLangTypecheckingError> {
    let resolved_type = resolve_type(
        &contract.return_type,
        &mut ctx.module,
        &mut ctx.typechecked_module,
    )?;
    let mut scopes = Scopes::new();

    for argument in contract.arguments.iter() {
        let resolved_type =
            resolve_type(&argument.typ, &mut ctx.module, &mut ctx.typechecked_module)?;
        assert!(
            !scopes.insert(argument.name.clone(), resolved_type),
            "there should be no variables named like the arguments in the scope"
        );
    }

    println!("Scope: {scopes:?}");

    typecheck_statement(ctx, body, &resolved_type)
}

pub fn typecheck_module(
    module: Module,
    typechecked_module: TypecheckedModule,
) -> Result<
    (TypecheckedModule, Module),
    (ProgrammingLangTypecheckingError, TypecheckedModule, Module),
> {
    let mut ctx = TypecheckingContext {
        scopes: Scopes::new(),
        typechecked_module,
        module,
    };
    while let Some((contract, body)) = ctx.module.functions.pop() {
        if let Err(e) = typecheck_function(&mut ctx, &contract, &body) {
            return Err((e, ctx.typechecked_module, ctx.module));
        }
    }

    Ok((ctx.typechecked_module, ctx.module))
}
