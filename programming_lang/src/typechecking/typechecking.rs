use std::{collections::HashMap, sync::Arc};

use crate::{
    annotations::Annotations,
    globals::GlobalStr,
    module::{
        ExternalFunctionId, FunctionId, ModuleContext, ModuleId, ModuleScopeValue, StructId,
        TraitId,
    },
    parser::{Expression, LiteralValue, Statement},
    tokenizer::Location,
};

use super::{
    expression::TypecheckedExpression,
    types::{FunctionType, Type},
    TypecheckedModule, TypecheckedStatement, TypecheckingContext, TypecheckingError,
};

pub enum ScopeValue {
    Module(ModuleId),
    Variable(Type),
    Struct(StructId),
    Function(FunctionId),
    Trait(TraitId),
    ExternalFunction(ExternalFunctionId),
}

pub struct Scope {
    values: HashMap<GlobalStr, ScopeValue>,
}

pub struct Scopes {
    entries: Vec<Scope>,
}

impl Scopes {
    pub fn new(
        module: &TypecheckedModule,
        statics: &[(Type, LiteralValue, usize, Location, Annotations)],
    ) -> Self {
        let mut values = HashMap::new();

        for (k, v) in module.scope.iter() {
            let v = match *v {
                ModuleScopeValue::Function(id) => ScopeValue::Function(id),
                ModuleScopeValue::ExternalFunction(id) => ScopeValue::ExternalFunction(id),
                ModuleScopeValue::Struct(id) => ScopeValue::Struct(id),
                ModuleScopeValue::Static(id) => ScopeValue::Variable(statics[id].0.clone()),
                ModuleScopeValue::Module(id) => ScopeValue::Module(id),
                ModuleScopeValue::Trait(id) => ScopeValue::Trait(id),
            };
            values.insert(k.clone(), v);
        }

        Self {
            entries: vec![Scope { values }],
        }
    }

    pub fn get(&self, key: &GlobalStr) -> Option<&ScopeValue> {
        let len = self.entries.len();
        for i in 1..=len {
            if let Some(v) = self.entries[len - i].values.get(key) {
                return Some(v);
            }
        }
        None
    }

    pub fn insert(&mut self, key: GlobalStr, value: ScopeValue) -> Option<ScopeValue> {
        if self.entries.len() < 1 {
            self.push_scope();
        }
        let idx = self.entries.len() - 1;
        self.entries[idx].values.insert(key, value)
    }

    pub fn push_scope(&mut self) {
        self.entries.push(Scope {
            values: HashMap::new(),
        });
    }

    pub fn pop_scope(&mut self) -> Option<Scope> {
        if self.entries.len() == 1 {
            return None;
        }
        self.entries.pop()
    }
}

pub fn typecheck_function(
    context: &TypecheckingContext,
    module_context: &ModuleContext,
    function_id: usize,
) -> Vec<TypecheckingError> {
    let (_, ref statement, module_id) = module_context
        .functions
        .read()
        .expect("read-write lock is poisoned")[function_id];

    let mut scope = Scopes::new(
        &context.modules.read().expect("read-write lock is poisoned")[module_id],
        &context.statics.read().expect("read-write lock is poisoned"),
    );

    let return_type = {
        let contract = &context
            .functions
            .read()
            .expect("read-write lock is poisoned")[function_id]
            .0;

        for arg in contract.arguments.iter().cloned() {
            scope.insert(arg.0, ScopeValue::Variable(arg.1));
        }

        contract.return_type.clone()
    };

    match typecheck_statement(context, &mut scope, statement, module_id, &return_type) {
        Ok((typechecked_statement, always_returns)) => {
            if !matches!(return_type, Type::PrimitiveVoid(0)) && !always_returns {
                return vec![TypecheckingError::BodyDoesNotAlwaysReturn {
                    location: statement.loc().clone(),
                }];
            }
            context
                .functions
                .write()
                .expect("read-write lock is poisoned")[function_id]
                .1 = typechecked_statement;
            Vec::new()
        }
        Err(e) => e,
    }
}

/// Returns if the statement and if it always returns
fn typecheck_statement(
    context: &TypecheckingContext,
    scope: &mut Scopes,
    statement: &Statement,
    module: ModuleId,
    return_type: &Type,
) -> Result<(TypecheckedStatement, bool), Vec<TypecheckingError>> {
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
        Statement::Return(None, location) => {
            if matches!(return_type, Type::PrimitiveVoid(0)) {
                Ok((
                    TypecheckedStatement::Return {
                        location: location.clone(),
                        typ: Type::PrimitiveVoid(0),
                        expr: TypecheckedExpression::None,
                    },
                    true,
                ))
            } else {
                Err(vec![TypecheckingError::MismatchingType {
                    expected: return_type.clone(),
                    found: Type::PrimitiveVoid(0),
                    location: location.clone(),
                }])
            }
        }
        Statement::Return(Some(expression), location) => {
            let (typ, typed_expression) =
                typecheck_expression(context, scope, expression).map_err(|e| vec![e])?;
            if typ != *return_type {
                return Err(vec![TypecheckingError::MismatchingType {
                    expected: return_type.clone(),
                    found: typ,
                    location: expression.loc().clone(),
                }]);
            }
            Ok((
                TypecheckedStatement::Return {
                    location: location.clone(),
                    typ,
                    expr: typed_expression,
                },
                true,
            ))
        }
        Statement::Block(statements, location, annotations) => {
            let mut errs = Vec::new();
            let mut typed_statements = Vec::with_capacity(statements.len());
            let mut last_always_returns = false;
            scope.push_scope();

            for statement in statements.iter() {
                match typecheck_statement(context, scope, statement, module, return_type) {
                    Ok((statement, statement_always_returns)) => {
                        typed_statements.push(statement);
                        last_always_returns = statement_always_returns;
                    }
                    Err(e) => {
                        last_always_returns = false;
                        typed_statements.push(TypecheckedStatement::None);
                        errs.extend(e);
                    }
                }
            }

            scope.pop_scope();

            if errs.len() > 0 {
                Err(errs)
            } else {
                Ok((
                    TypecheckedStatement::Block {
                        location: location.clone(),
                        statements: typed_statements.into_boxed_slice(),
                        annotations: annotations.clone(),
                    },
                    last_always_returns,
                ))
            }
        }
        Statement::Var(global_str, expression, type_ref, location, _) => todo!(),
        Statement::Expression(expression) => typecheck_expression(context, scope, expression)
            .map_err(|v| vec![v])
            .map(|(typ, expr)| {
                (
                    TypecheckedStatement::Expression(expr),
                    matches!(typ, Type::PrimitiveNever),
                )
            }),
        Statement::BakedFunction(_, location) => todo!(),
        Statement::Function(..)
        | Statement::ExternalFunction(..)
        | Statement::BakedStruct(..)
        | Statement::BakedStatic(..)
        | Statement::Struct { .. }
        | Statement::Export(..)
        | Statement::Trait(_)
        | Statement::BakedTrait(..)
        | Statement::BakedExternalFunction(..) => {
            unreachable!()
        }
    }
}

fn typecheck_expression(
    context: &TypecheckingContext,
    scope: &mut Scopes,
    expression: &Expression,
) -> Result<(Type, TypecheckedExpression), TypecheckingError> {
    match expression {
        Expression::Literal(literal_value, location) => {
            let typ = match literal_value {
                LiteralValue::String(_) => Type::PrimitiveStr(1),
                LiteralValue::Array(elements) => {
                    if elements.len() > 1 {
                        unreachable!("elements need at least 1 element");
                    }
                    let (typ, _) = typecheck_expression(context, scope, &elements[0])?;

                    for el in elements[1..].iter() {
                        let (el_typ, _) = typecheck_expression(context, scope, el)?;
                        if typ != el_typ {
                            return Err(TypecheckingError::MismatchingType {
                                expected: typ,
                                found: el_typ,
                                location: el.loc().clone(),
                            });
                        }
                    }

                    Type::SizedArray {
                        typ: Box::new(typ),
                        num_references: 0,
                        number_elements: elements.len(),
                    }
                }
                LiteralValue::Struct(entries, path) => {
                    if path.entries.len() > 1 {
                        unimplemented!();
                    }
                    let (name, generics) = &path.entries[0];
                    if generics.len() > 0 {
                        unimplemented!("generics on paths");
                    }
                    let Some(val) = scope.get(name) else {
                        return Err(TypecheckingError::UnboundIdent {
                            location: location.clone(),
                            name: name.clone(),
                        });
                    };
                    let ScopeValue::Struct(id) = val else {
                        return Err(TypecheckingError::IdentifierIsNotStruct {
                            location: location.clone(),
                            name: name.clone(),
                        });
                    };
                    let structure =
                        &context.structs.read().expect("read-write lock is poisoned")[*id];
                    for (k, (loc, _)) in entries.iter() {
                        if structure
                            .elements
                            .iter()
                            .find(|(name, _)| k == name)
                            .is_none()
                        {
                            return Err(TypecheckingError::NoSuchFieldFound {
                                location: loc.clone(),
                                name: k.clone(),
                            });
                        }
                    }
                    for (name, typ) in structure.elements.iter() {
                        let Some((_, expr)) = entries.get(name) else {
                            return Err(TypecheckingError::MissingField {
                                location: location.clone(),
                                name: name.clone(),
                            });
                        };
                        let (expr_typ, _) = typecheck_expression(context, scope, expr)?;
                        if expr_typ != *typ {
                            return Err(TypecheckingError::MismatchingType {
                                expected: typ.clone(),
                                found: expr_typ,
                                location: expr.loc().clone(),
                            });
                        }
                    }
                    Type::Struct {
                        struct_id: structure.id,
                        name: structure.name.clone(),
                        num_references: 0,
                    }
                }
                LiteralValue::Float(_, typ) => {
                    Type::from_numtype(*typ).unwrap_or(Type::PrimitiveF32(0))
                }
                LiteralValue::SInt(_, typ) => {
                    Type::from_numtype(*typ).unwrap_or(Type::PrimitiveI32(0))
                }
                LiteralValue::UInt(_, typ) => {
                    Type::from_numtype(*typ).unwrap_or(Type::PrimitiveI32(0))
                }
                LiteralValue::Bool(_) => Type::PrimitiveBool(0),
                LiteralValue::Dynamic(path) => {
                    if path.entries.len() > 1 {
                        unimplemented!();
                    }
                    let (name, generics) = &path.entries[0];
                    if generics.len() > 0 {
                        unimplemented!("generics on paths");
                    }
                    let Some(typ) = scope.get(name) else {
                        return Err(TypecheckingError::UnboundIdent {
                            location: location.clone(),
                            name: name.clone(),
                        });
                    };

                    match typ {
                        ScopeValue::Module(_) | ScopeValue::Struct(_) | ScopeValue::Trait(_) => {
                            return Err(TypecheckingError::UnboundIdent {
                                location: location.clone(),
                                name: name.clone(),
                            })
                        }
                        ScopeValue::Variable(typ) => typ.clone(),
                        ScopeValue::Function(func_id) => {
                            let contract = &context
                                .functions
                                .read()
                                .expect("read-write lock is poisoned")[*func_id]
                                .0;
                            Type::Function(
                                Arc::new(FunctionType {
                                    arguments: contract
                                        .arguments
                                        .iter()
                                        .map(|(_, typ)| typ.clone())
                                        .collect(),
                                    return_type: contract.return_type.clone(),
                                }),
                                0,
                            )
                        }
                        ScopeValue::ExternalFunction(func_id) => {
                            let contract = &context
                                .external_functions
                                .read()
                                .expect("read-write lock is poisoned")[*func_id]
                                .0;
                            Type::Function(
                                Arc::new(FunctionType {
                                    arguments: contract
                                        .arguments
                                        .iter()
                                        .map(|(_, typ)| typ.clone())
                                        .collect(),
                                    return_type: contract.return_type.clone(),
                                }),
                                0,
                            )
                        }
                    }
                }
                LiteralValue::BakedAnonymousFunction(id) => {
                    let contract = &context
                        .functions
                        .read()
                        .expect("read-write lock is poisoned")[*id]
                        .0;
                    if matches!(contract.return_type, Type::Generic(..)) {
                        return Err(TypecheckingError::GenericFunctionPointer {
                            location: location.clone(),
                        });
                    }
                    let mut arguments = Vec::with_capacity(contract.arguments.len());
                    for arg in contract.arguments.iter() {
                        if matches!(arg.1, Type::Generic(..)) {
                            return Err(TypecheckingError::GenericFunctionPointer {
                                location: location.clone(),
                            });
                        }
                        arguments.push(arg.1.clone());
                    }
                    Type::Function(
                        Arc::new(FunctionType {
                            arguments,
                            return_type: contract.return_type.clone(),
                        }),
                        0,
                    )
                }
                LiteralValue::Void => Type::PrimitiveVoid(0),
                LiteralValue::AnonymousFunction(..) => unreachable!(),
            };

            Ok((
                typ.clone(),
                TypecheckedExpression::Literal(location.clone(), typ, literal_value.clone()),
            ))
        }
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
        } => {
            let (typ, function_expr) = typecheck_expression(context, scope, identifier)?;
            let Type::Function(function_type, _) = typ else {
                return Err(TypecheckingError::TypeIsNotAFunction {
                    location: identifier.loc().clone(),
                });
            };
            let mut typed_arguments = Vec::with_capacity(function_type.arguments.len());
            if arguments.len() < function_type.arguments.len() {
                return Err(TypecheckingError::MissingArguments {
                    location: identifier.loc().clone(),
                });
            }
            if arguments.len() > function_type.arguments.len() {
                return Err(TypecheckingError::TooManyArguments {
                    location: arguments[function_type.arguments.len() - 1].loc().clone(),
                });
            }
            for i in 0..function_type.arguments.len() {
                let (typ, expr) = typecheck_expression(context, scope, &arguments[i])?;
                if typ != function_type.arguments[i] {
                    return Err(TypecheckingError::MismatchingType {
                        expected: function_type.arguments[i].clone(),
                        found: typ,
                        location: arguments[i].loc().clone(),
                    });
                }
                typed_arguments.push(expr);
            }

            Ok((
                function_type.return_type.clone(),
                TypecheckedExpression::Call(
                    identifier.loc().clone(),
                    Box::new(function_expr),
                    typed_arguments,
                ),
            ))
        }
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
        } => {
            let (typ, lhs) = typecheck_expression(context, scope, left_side)?;
            let (typ_rhs, rhs) = typecheck_expression(context, scope, right_side)?;
            if typ != typ_rhs {
                return Err(TypecheckingError::MismatchingType {
                    expected: typ.clone(),
                    found: typ_rhs.clone(),
                    location: right_side.loc().clone(),
                });
            }

            Ok((
                typ.clone(),
                TypecheckedExpression::Assignment(loc.clone(), Box::new(lhs), Box::new(rhs)),
            ))
        }
        Expression::Range {
            left_side,
            right_side,
            inclusive,
            loc,
        } => {
            let (typ, _) = typecheck_expression(context, scope, left_side)?;
            let (typ_rhs, _) = typecheck_expression(context, scope, right_side)?;
            if typ != typ_rhs {
                return Err(TypecheckingError::MismatchingType {
                    expected: typ.clone(),
                    found: typ_rhs.clone(),
                    location: right_side.loc().clone(),
                });
            }

            unimplemented!("lang-items");
        }
        Expression::TypeCast {
            left_side,
            new_type,
            loc,
        } => todo!(),
    }
}
