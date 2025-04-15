use std::{collections::HashMap, sync::Arc};

use crate::{
    globals::GlobalStr,
    module::{ModuleContext, ModuleId, ModuleScopeValue, StaticId},
    parser::{ArrayLiteral, BinaryOp, Expression, LiteralValue, Path, Statement, UnaryOp},
    std_annotations::ext_vararg::ExternVarArg,
    tokenizer::{Location, NumberType},
    typechecking::typed_resolve_import,
};

use super::{
    expression::{OffsetValue, TypecheckedExpression, TypedLiteral},
    intrinsics::IntrinsicAnnotation,
    types::{FunctionType, Type, TypeSuggestion},
    TypecheckingContext, TypecheckingError,
};

pub type ScopeValueId = usize;

pub struct ScopeTypeMetadata {
    pub stack_allocated: bool,
}

#[derive(Default)]
pub struct Scopes {
    entries: Vec<HashMap<GlobalStr, ScopeValueId>>,
    values: Vec<(Type, ScopeTypeMetadata)>,
}

impl Scopes {
    pub fn get(&self, key: &GlobalStr) -> Option<(&(Type, ScopeTypeMetadata), ScopeValueId)> {
        let len = self.entries.len();
        for i in 1..=len {
            if let Some(v) = self.entries[len - i].get(key) {
                return Some((&self.values[*v], *v));
            }
        }
        None
    }

    pub fn make_stack_allocated(&mut self, id: usize) {
        self.values[id].1.stack_allocated = true;
    }

    pub fn insert_value(
        &mut self,
        key: GlobalStr,
        value: Type,
    ) -> (ScopeValueId, Option<ScopeValueId>) {
        let id = self.push(value);
        (id, self.insert(key, id))
    }

    pub fn insert(&mut self, key: GlobalStr, value: ScopeValueId) -> Option<ScopeValueId> {
        if self.entries.is_empty() {
            self.push_scope();
        }
        let idx = self.entries.len() - 1;
        self.entries[idx].insert(key, value)
    }

    pub fn push(&mut self, value: Type) -> ScopeValueId {
        if !value.is_sized() {
            panic!("unsized type: {value:?}");
        }
        if self.entries.is_empty() {
            self.push_scope();
        }
        self.values.push((
            value,
            ScopeTypeMetadata {
                stack_allocated: false,
            },
        ));
        self.values.len() - 1
    }

    pub fn push_scope(&mut self) {
        self.entries.push(HashMap::new());
    }

    /// Returns if it could pop a scope or not.
    pub fn pop_scope(&mut self) -> bool {
        if self.entries.len() == 1 {
            return false;
        }
        assert!(self.entries.pop().is_some(), "a scope should always exist");
        true
    }
}

pub fn typecheck_static(
    context: &TypecheckingContext,
    module_context: &ModuleContext,
    static_id: StaticId,
    errs: &mut Vec<TypecheckingError>,
) -> bool {
    let tc_module_reader = context.statics.read();
    let typ = &tc_module_reader[static_id].0;
    let expr = {
        std::mem::replace(
            &mut module_context.statics.write()[static_id].1,
            LiteralValue::Void,
        )
    };

    match typecheck_expression(
        context,
        tc_module_reader[static_id].2,
        &mut Scopes::default(),
        &Expression::Literal(expr, tc_module_reader[static_id].3.clone()),
        &mut Vec::new(),
        TypeSuggestion::from_type(typ),
    ) {
        Err(e) => {
            errs.push(e);
            return false;
        }
        Ok((expr_typ, expr)) => {
            if *typ != expr_typ {
                errs.push(TypecheckingError::MismatchingType {
                    expected: typ.clone(),
                    found: expr_typ,
                    location: tc_module_reader[static_id].3.clone(),
                });
                return false;
            }
            if !expr.is_entirely_literal() {
                errs.push(TypecheckingError::StaticsNeedToBeLiteral(
                    tc_module_reader[static_id].3.clone(),
                ));
            }
            drop(tc_module_reader);
            context.statics.write()[static_id].1 = expr;
        }
    }
    true
}

pub fn typecheck_function(
    context: &TypecheckingContext,
    module_context: &ModuleContext,
    function_id: usize,
    is_external: bool,
) -> Result<Vec<(Type, ScopeTypeMetadata)>, Vec<TypecheckingError>> {
    let ext_fn_reader = module_context.external_functions.read();
    let fn_reader = module_context.functions.read();
    let (statement, module_id) = if is_external {
        let (_, ref statement, module_id) = ext_fn_reader[function_id];
        if let Some(statement) = statement {
            (statement, module_id)
        } else {
            return Ok(Vec::new());
        }
    } else {
        let (_, ref statement, module_id) = fn_reader[function_id];
        (statement, module_id)
    };

    let mut scope = Scopes::default();

    let (return_type, args, loc) = if is_external {
        let contract = &context.external_functions.read()[function_id].0;
        (
            contract.return_type.clone(),
            contract.arguments.clone(),
            contract.location.clone(),
        )
    } else {
        let reader = context.functions.read();
        let contract = &reader[function_id].0;
        if contract
            .annotations
            .get_first_annotation::<IntrinsicAnnotation>()
            .is_some()
        {
            let loc = contract.location.clone();
            drop(reader);
            context.functions.write()[function_id].1 =
                Box::new([TypecheckedExpression::Unreachable(loc)]);
            return Ok(Vec::new());
        }
        (
            contract.return_type.clone(),
            contract.arguments.clone(),
            contract.location.clone(),
        )
    };

    let mut errs = vec![];
    if is_external
        && (!return_type.is_primitive()
            || (return_type.refcount() > 0 && !return_type.is_thin_ptr()))
    {
        errs.push(TypecheckingError::InvalidExternReturnType(loc.clone()));
    }
    if !return_type.is_sized() {
        errs.push(TypecheckingError::UnsizedReturnType(
            loc.clone(),
            return_type.clone(),
        ));
    }
    for (_, arg) in args.iter().filter(|v| !v.1.is_sized()) {
        errs.push(TypecheckingError::UnsizedArgument(loc.clone(), arg.clone()));
    }
    (errs.is_empty()).then_some(()).ok_or(errs)?;

    let mut exprs = vec![];
    for arg in args {
        let (id, _) = scope.insert_value(arg.0, arg.1);
        scope.make_stack_allocated(id);
    }

    match typecheck_statement(
        context,
        &mut scope,
        statement,
        module_id,
        &return_type,
        &mut exprs,
    ) {
        Ok(always_returns) => {
            if !matches!(return_type, Type::PrimitiveVoid(0)) && !always_returns {
                return Err(vec![TypecheckingError::BodyDoesNotAlwaysReturn {
                    location: statement.loc().clone(),
                }]);
            }
            if !always_returns {
                typecheck_statement(
                    context,
                    &mut scope,
                    &Statement::Return(None, statement.loc().clone()),
                    module_id,
                    &return_type,
                    &mut exprs,
                )?;
            }
            if is_external {
                let mut exprs = Some(exprs.into_boxed_slice());
                std::mem::swap(
                    &mut exprs,
                    &mut context.external_functions.write()[function_id].1,
                );
                assert!(exprs.is_none());
            } else {
                let mut boxed_slice = exprs.into_boxed_slice();
                std::mem::swap(
                    &mut boxed_slice,
                    &mut context.functions.write()[function_id].1,
                );
                assert_eq!(boxed_slice.len(), 0);
            }
        }
        Err(e) => return Err(e),
    }
    Ok(scope.values)
}

/// Returns if the statement and if it always returns
fn typecheck_statement(
    context: &TypecheckingContext,
    scope: &mut Scopes,
    statement: &Statement,
    module: ModuleId,
    return_type: &Type,
    exprs: &mut Vec<TypecheckedExpression>,
) -> Result<bool, Vec<TypecheckingError>> {
    match statement {
        Statement::If {
            condition,
            if_stmt,
            else_stmt,
            location,
            annotations,
        } => {
            let mut errs = Vec::new();
            let expr_result = typecheck_expression(
                context,
                module,
                scope,
                condition,
                exprs,
                TypeSuggestion::Bool,
            )
            .map_err(|v| errs.push(v));

            let mut if_exprs = Vec::new();
            let mut else_exprs = Vec::new();
            let if_stmt_result =
                typecheck_statement(context, scope, if_stmt, module, return_type, &mut if_exprs)
                    .map_err(|v| errs.extend(v));
            let else_stmt_result = if let Some(else_stmt) = else_stmt {
                typecheck_statement(
                    context,
                    scope,
                    else_stmt,
                    module,
                    return_type,
                    &mut else_exprs,
                )
                .map_err(|v| errs.extend(v))
            } else {
                Ok(false)
            };
            let Ok((condition_ty, cond)) = expr_result else {
                return Err(errs);
            };
            if condition_ty != Type::PrimitiveBool(0) {
                errs.push(TypecheckingError::MismatchingType {
                    expected: Type::PrimitiveBool(0),
                    found: condition_ty,
                    location: location.clone(),
                });
            }
            let Ok(if_stmt_exits) = if_stmt_result else {
                return Err(errs);
            };
            let Ok(else_stmt_exits) = else_stmt_result else {
                return Err(errs);
            };
            if !errs.is_empty() {
                return Err(errs);
            }
            exprs.push(TypecheckedExpression::If {
                loc: location.clone(),
                cond,
                if_block: (if_exprs.into_boxed_slice(), if_stmt.loc().clone()),
                else_block: else_stmt
                    .as_ref()
                    .map(move |stmt| (else_exprs.into_boxed_slice(), stmt.loc().clone())),
                annotations: annotations.clone(),
            });
            Ok(if_stmt_exits && else_stmt_exits)
        }
        Statement::While {
            condition,
            child,
            location,
            ..
        } => {
            let mut errs = Vec::new();
            let mut condition_block = Vec::new();
            let mut body = Vec::new();

            let condition_result = typecheck_expression(
                context,
                module,
                scope,
                condition,
                &mut condition_block,
                TypeSuggestion::Bool,
            )
            .map_err(|v| errs.push(v));
            let body_result =
                typecheck_statement(context, scope, child, module, return_type, &mut body)
                    .map_err(|v| errs.extend(v));
            let Ok((condition_ty, cond)) = condition_result else {
                return Err(errs);
            };
            if condition_ty != Type::PrimitiveBool(0) {
                errs.push(TypecheckingError::MismatchingType {
                    expected: Type::PrimitiveBool(0),
                    found: condition_ty,
                    location: location.clone(),
                });
            }
            let Ok(mut always_exits) = body_result else {
                return Err(errs);
            };
            if !errs.is_empty() {
                return Err(errs);
            }
            // while (true) {} also never exits
            always_exits = always_exits || matches!(cond, TypedLiteral::Bool(true));
            exprs.push(TypecheckedExpression::While {
                loc: location.clone(),
                cond_block: condition_block.into_boxed_slice(),
                cond,
                body: (body.into_boxed_slice(), child.loc().clone()),
            });

            Ok(always_exits)
        }
        Statement::For { .. } => todo!("iterator (requires generics)"),
        Statement::Return(None, location) => {
            if matches!(return_type, Type::PrimitiveVoid(0)) {
                exprs.push(TypecheckedExpression::Return(
                    location.clone(),
                    TypedLiteral::Void,
                ));
                Ok(true)
            } else {
                Err(vec![TypecheckingError::MismatchingType {
                    expected: return_type.clone(),
                    found: Type::PrimitiveVoid(0),
                    location: location.clone(),
                }])
            }
        }
        Statement::Return(Some(expression), location) => {
            let (typ, typed_expression) = typecheck_expression(
                context,
                module,
                scope,
                expression,
                exprs,
                TypeSuggestion::from_type(return_type),
            )
            .map_err(|e| vec![e])?;
            if typ != *return_type {
                return Err(vec![TypecheckingError::MismatchingType {
                    expected: return_type.clone(),
                    found: typ,
                    location: expression.loc().clone(),
                }]);
            }
            exprs.push(TypecheckedExpression::Return(
                location.clone(),
                typed_expression,
            ));
            Ok(true)
        }
        Statement::Block(statements, location, annotations) => {
            let mut errs = Vec::new();
            let mut block_exprs = Vec::with_capacity(statements.len());
            let mut always_returns = false;
            scope.push_scope();

            for statement in statements.iter() {
                match typecheck_statement(
                    context,
                    scope,
                    statement,
                    module,
                    return_type,
                    &mut block_exprs,
                ) {
                    Ok(true) => {
                        always_returns = true;
                        if !matches!(statement, Statement::Return(..)) {
                            block_exprs
                                .push(TypecheckedExpression::Unreachable(statement.loc().clone()));
                        }
                        break;
                    }
                    Ok(_) => {}
                    Err(e) => {
                        errs.extend(e);
                    }
                }
            }

            scope.pop_scope();

            if !errs.is_empty() {
                Err(errs)
            } else {
                exprs.push(TypecheckedExpression::Block(
                    location.clone(),
                    block_exprs.into_boxed_slice(),
                    annotations.clone(),
                ));
                Ok(always_returns)
            }
        }
        Statement::Var(name, expression, type_ref, location, _) => {
            let expected_typ = type_ref
                .as_ref()
                .map(|v| context.resolve_type(module, v, &[]))
                .transpose()
                .map_err(|v| vec![v])?;

            let (typ, expr) = typecheck_expression(
                context,
                module,
                scope,
                expression,
                exprs,
                expected_typ
                    .as_ref()
                    .map(TypeSuggestion::from_type)
                    .unwrap_or_default(),
            )
            .map_err(|e| vec![e])?;

            if let Some(expected_typ) = expected_typ {
                if expected_typ != typ {
                    return Err(vec![TypecheckingError::MismatchingType {
                        expected: expected_typ,
                        found: typ,
                        location: location.clone(),
                    }]);
                }
            }

            let id = match expr {
                TypedLiteral::Dynamic(id) => id,
                _ => {
                    let id = scope.push(typ.clone());
                    exprs.push(TypecheckedExpression::Literal(
                        expression.loc().clone(),
                        id,
                        expr,
                    ));
                    id
                }
            };
            scope.insert(name.clone(), id);
            exprs.push(TypecheckedExpression::DeclareVariable(
                location.clone(),
                id,
                typ,
                name.clone(),
            ));
            scope.make_stack_allocated(id);
            Ok(false)
        }
        Statement::Expression(expression) => typecheck_expression(
            context,
            module,
            scope,
            expression,
            exprs,
            Default::default(),
        )
        .map_err(|v| vec![v])
        .map(|(typ, _)| matches!(typ, Type::PrimitiveNever)),
        Statement::BakedFunction(..)
        | Statement::Function(..)
        | Statement::ExternalFunction(..)
        | Statement::BakedStruct(..)
        | Statement::BakedStatic(..)
        | Statement::Struct { .. }
        | Statement::Export(..)
        | Statement::ModuleAsm(..)
        | Statement::Trait(_)
        | Statement::BakedTrait(..)
        | Statement::BakedExternalFunction(..) => {
            unreachable!()
        }
    }
}

macro_rules! tc_res {
    (unary $scope:expr, $exprs:expr; $name:ident ($loc:expr, $right_side:expr, $typ:expr)) => {{
        let typ = $typ;
        let id = $scope.push(typ.clone());
        $exprs.push(TypecheckedExpression::$name($loc, id, $right_side));
        Ok((typ, TypedLiteral::Dynamic(id)))
    }};

    (binary $scope:expr, $exprs:expr; $name:ident ($loc:expr, $left_side:expr, $right_side:expr, $typ:expr)) => {{
        let typ = $typ;
        let id = $scope.push(typ.clone());
        $exprs.push(TypecheckedExpression::$name(
            $loc,
            id,
            $left_side,
            $right_side,
        ));
        Ok((typ, TypedLiteral::Dynamic(id)))
    }};
}

fn signed_number_to_literal(
    v: i64,
    number_type: NumberType,
    expected: TypeSuggestion,
) -> (Type, TypedLiteral) {
    match number_type {
        NumberType::I8 => (Type::PrimitiveI8(0), TypedLiteral::I8(v as i8)),
        NumberType::I16 => (Type::PrimitiveI16(0), TypedLiteral::I16(v as i16)),
        NumberType::I32 => (Type::PrimitiveI32(0), TypedLiteral::I32(v as i32)),
        NumberType::I64 => (Type::PrimitiveI64(0), TypedLiteral::I64(v)),
        NumberType::Isize => (Type::PrimitiveISize(0), TypedLiteral::ISize(v as isize)),
        NumberType::None => match expected {
            TypeSuggestion::Number(
                number_typ @ (NumberType::I8
                | NumberType::I16
                | NumberType::I32
                | NumberType::I64
                | NumberType::Isize),
            ) => signed_number_to_literal(v, number_typ, TypeSuggestion::Unknown),
            _ => (Type::PrimitiveI32(0), TypedLiteral::I32(v as i32)),
        },
        _ => unreachable!("this should never be a float or unsigned number"),
    }
}

fn unsigned_number_to_literal(
    v: u64,
    number_type: NumberType,
    expected: TypeSuggestion,
) -> (Type, TypedLiteral) {
    match number_type {
        NumberType::U8 => (Type::PrimitiveU8(0), TypedLiteral::U8(v as u8)),
        NumberType::U16 => (Type::PrimitiveU16(0), TypedLiteral::U16(v as u16)),
        NumberType::U32 => (Type::PrimitiveU32(0), TypedLiteral::U32(v as u32)),
        NumberType::U64 => (Type::PrimitiveU64(0), TypedLiteral::U64(v)),
        NumberType::Usize => (Type::PrimitiveUSize(0), TypedLiteral::USize(v as usize)),
        NumberType::I8 => (Type::PrimitiveI8(0), TypedLiteral::I8(v as i8)),
        NumberType::I16 => (Type::PrimitiveI16(0), TypedLiteral::I16(v as i16)),
        NumberType::I32 => (Type::PrimitiveI32(0), TypedLiteral::I32(v as i32)),
        NumberType::I64 => (Type::PrimitiveI64(0), TypedLiteral::I64(v as i64)),
        NumberType::Isize => (Type::PrimitiveISize(0), TypedLiteral::ISize(v as isize)),
        NumberType::None => match expected {
            TypeSuggestion::Number(NumberType::F32 | NumberType::F64) => {
                (Type::PrimitiveI32(0), TypedLiteral::I32(v as i32))
            }
            TypeSuggestion::Number(number_typ) => {
                unsigned_number_to_literal(v, number_typ, TypeSuggestion::Unknown)
            }
            _ => (Type::PrimitiveI32(0), TypedLiteral::I32(v as i32)),
        },
        _ => unreachable!("this should never be a float or signed number"),
    }
}

fn float_number_to_literal(
    v: f64,
    number_type: NumberType,
    expected: TypeSuggestion,
) -> (Type, TypedLiteral) {
    match number_type {
        NumberType::F32 => (Type::PrimitiveF32(0), TypedLiteral::F32(v as f32)),
        NumberType::F64 => (Type::PrimitiveF64(0), TypedLiteral::F64(v)),
        NumberType::None => match expected {
            TypeSuggestion::Number(number_typ @ (NumberType::F32 | NumberType::F64)) => {
                float_number_to_literal(v, number_typ, TypeSuggestion::Unknown)
            }
            _ => (Type::PrimitiveF32(0), TypedLiteral::F32(v as f32)),
        },
        _ => unreachable!("this should never be a signed or unsigned number"),
    }
}

fn typecheck_expression(
    context: &TypecheckingContext,
    module: ModuleId,
    scope: &mut Scopes,
    expression: &Expression,
    exprs: &mut Vec<TypecheckedExpression>,
    type_suggestion: TypeSuggestion,
) -> Result<(Type, TypedLiteral), TypecheckingError> {
    match expression {
        Expression::Literal(literal_value, location) => match literal_value {
            LiteralValue::String(global_str) => Ok((
                Type::PrimitiveStr(1),
                TypedLiteral::String(global_str.clone()),
            )),
            LiteralValue::Array(ArrayLiteral::Values(vec)) if vec.is_empty() => {
                let typ = match type_suggestion {
                    TypeSuggestion::UnsizedArray(_) | TypeSuggestion::Array(_) => type_suggestion
                        .to_type(context)
                        .ok_or_else(|| TypecheckingError::CannotInferArrayType(location.clone()))?,
                    _ => return Err(TypecheckingError::CannotInferArrayType(location.clone())),
                };
                Ok((typ.clone(), TypedLiteral::Array(typ, Vec::new())))
            }
            LiteralValue::Array(ArrayLiteral::CopyInitialized(value, amount)) => {
                let suggested_typ = match type_suggestion {
                    TypeSuggestion::UnsizedArray(v) | TypeSuggestion::Array(v) => *v,
                    _ => TypeSuggestion::Unknown,
                };
                let (typ, lit) =
                    typecheck_expression(context, module, scope, value, exprs, suggested_typ)?;
                Ok((
                    Type::SizedArray {
                        typ: Box::new(typ.clone()),
                        num_references: 0,
                        number_elements: *amount,
                    },
                    TypedLiteral::ArrayInit(typ, Box::new(lit), *amount),
                ))
            }
            LiteralValue::Array(ArrayLiteral::Values(vec)) => {
                let suggested_typ = match type_suggestion {
                    TypeSuggestion::UnsizedArray(v) | TypeSuggestion::Array(v) => *v,
                    _ => TypeSuggestion::Unknown,
                };
                let (typ, mut elements) =
                    typecheck_expression(context, module, scope, &vec[0], exprs, suggested_typ)
                        .map(|(typ, lit)| (typ, vec![lit]))?;
                let suggested_typ = TypeSuggestion::from_type(&typ);
                for expr in vec.iter().skip(1) {
                    let (el_typ, el_lit) = typecheck_expression(
                        context,
                        module,
                        scope,
                        expr,
                        exprs,
                        suggested_typ.clone(),
                    )?;
                    if el_typ != typ {
                        return Err(TypecheckingError::MismatchingType {
                            expected: typ,
                            found: el_typ,
                            location: expr.loc().clone(),
                        });
                    }
                    elements.push(el_lit);
                }
                let arr_typ = Type::SizedArray {
                    typ: Box::new(typ.clone()),
                    num_references: 0,
                    number_elements: vec.len(),
                };
                Ok((arr_typ, TypedLiteral::Array(typ, elements)))
            }
            LiteralValue::Tuple(values) => {
                let mut elements = Vec::with_capacity(values.len());
                let mut element_types = Vec::with_capacity(values.len());
                let mut suggested_element_types = &Vec::new();
                if let TypeSuggestion::Tuple(vec) = &type_suggestion {
                    suggested_element_types = vec;
                }

                for (i, (_, value)) in values.iter().enumerate() {
                    let (typ, val) = typecheck_expression(
                        context,
                        module,
                        scope,
                        value,
                        exprs,
                        suggested_element_types.get(i).cloned().unwrap_or_default(),
                    )?;
                    elements.push(val);
                    element_types.push(typ);
                }
                Ok((
                    Type::Tuple {
                        elements: element_types,
                        num_references: 0,
                    },
                    TypedLiteral::Tuple(elements),
                ))
            }
            LiteralValue::AnonymousStruct(values) => {
                let struct_id = if let TypeSuggestion::Struct(id) = type_suggestion {
                    id
                } else {
                    return Err(TypecheckingError::CannotInferAnonStructType(
                        location.clone(),
                    ));
                };

                let structure = &context.structs.read()[struct_id];
                // ensure there are no excessive values in the struct initialization
                for k in values.keys() {
                    if !structure.elements.iter().any(|v| v.0 == *k) {
                        return Err(TypecheckingError::NoSuchFieldFound {
                            location: values[k].0.clone(),
                            name: k.clone(),
                        });
                    }
                }

                let mut elements = Vec::with_capacity(structure.elements.len());
                for (key, typ) in structure.elements.iter() {
                    let Some((loc, expr)) = values.get(key) else {
                        return Err(TypecheckingError::MissingField {
                            location: location.clone(),
                            name: key.clone(),
                        });
                    };
                    let (expr_typ, expr_lit) = typecheck_expression(
                        context,
                        module,
                        scope,
                        expr,
                        exprs,
                        TypeSuggestion::from_type(typ),
                    )?;
                    if *typ != expr_typ {
                        return Err(TypecheckingError::MismatchingType {
                            expected: typ.clone(),
                            found: expr_typ,
                            location: loc.clone(),
                        });
                    }
                    elements.push(expr_lit);
                }
                Ok((
                    Type::Struct {
                        struct_id,
                        name: structure.name.clone(),
                        num_references: 0,
                    },
                    TypedLiteral::Struct(struct_id, elements),
                ))
            }
            LiteralValue::Struct(values, path) => {
                let value = typed_resolve_import(
                    context,
                    module,
                    &path
                        .entries
                        .iter()
                        .map(|(v, _)| v.clone())
                        .collect::<Vec<_>>(),
                    location,
                    &mut Vec::new(),
                )
                .map_err(|_| TypecheckingError::CannotFindValue(location.clone(), path.clone()))?;
                let ModuleScopeValue::Struct(struct_id) = value else {
                    return Err(TypecheckingError::CannotFindValue(
                        location.clone(),
                        path.clone(),
                    ));
                };
                let structure = &context.structs.read()[struct_id];
                // ensure there are no excessive values in the struct initialization
                for k in values.keys() {
                    if !structure.elements.iter().any(|v| v.0 == *k) {
                        return Err(TypecheckingError::NoSuchFieldFound {
                            location: values[k].0.clone(),
                            name: k.clone(),
                        });
                    }
                }

                let mut elements = Vec::with_capacity(structure.elements.len());
                for (key, typ) in structure.elements.iter() {
                    let Some((loc, expr)) = values.get(key) else {
                        return Err(TypecheckingError::MissingField {
                            location: location.clone(),
                            name: key.clone(),
                        });
                    };
                    let (expr_typ, expr_lit) = typecheck_expression(
                        context,
                        module,
                        scope,
                        expr,
                        exprs,
                        TypeSuggestion::from_type(typ),
                    )?;
                    if *typ != expr_typ {
                        return Err(TypecheckingError::MismatchingType {
                            expected: typ.clone(),
                            found: expr_typ,
                            location: loc.clone(),
                        });
                    }
                    elements.push(expr_lit);
                }
                Ok((
                    Type::Struct {
                        struct_id,
                        name: structure.name.clone(),
                        num_references: 0,
                    },
                    TypedLiteral::Struct(struct_id, elements),
                ))
            }
            LiteralValue::Float(v, number_type) => {
                Ok(float_number_to_literal(*v, *number_type, type_suggestion))
            }
            LiteralValue::SInt(v, number_type) => {
                Ok(signed_number_to_literal(*v, *number_type, type_suggestion))
            }
            LiteralValue::UInt(v, number_type) => Ok(unsigned_number_to_literal(
                *v,
                *number_type,
                type_suggestion,
            )),
            LiteralValue::Bool(v) => Ok((Type::PrimitiveBool(0), TypedLiteral::Bool(*v))),
            LiteralValue::Dynamic(path) => {
                if path.entries.len() == 1 && path.entries[0].1.is_empty() {
                    if let Some(((typ, _), id)) = scope.get(&path.entries[0].0) {
                        return Ok((typ.clone(), TypedLiteral::Dynamic(id)));
                    }
                }
                let mut generics = Vec::new();
                for (_, generic_value) in path.entries.iter() {
                    generics.extend_from_slice(generic_value);
                }

                let value = typed_resolve_import(
                    context,
                    module,
                    &path
                        .entries
                        .iter()
                        .map(|(v, _)| v.clone())
                        .collect::<Vec<_>>(),
                    location,
                    &mut Vec::new(),
                )
                .map_err(|_| TypecheckingError::CannotFindValue(location.clone(), path.clone()))?;
                match value {
                    ModuleScopeValue::Function(id) => {
                        let reader = &context.functions.read()[id];
                        let mut function_typ = FunctionType {
                            return_type: reader.0.return_type.clone(),
                            arguments: reader.0.arguments.iter().map(|v| v.1.clone()).collect(),
                        };
                        if reader.0.generics.len() != generics.len() {
                            return Err(TypecheckingError::MismatchingGenericCount(
                                location.clone(),
                                reader.0.generics.len(),
                                generics.len(),
                            ));
                        }
                        // TODO: Bounds check
                        let mut generic_types = Vec::with_capacity(generics.len());
                        for (i, typ) in generics.iter().enumerate() {
                            let ty = context.resolve_type(module, typ, &[])?;
                            if reader.0.generics[i].sized && !ty.is_sized() {
                                return Err(TypecheckingError::UnsizedForSizedGeneric(
                                    location.clone(),
                                    ty,
                                ));
                            }
                            generic_types.push(ty);
                        }
                        for ty in function_typ
                            .arguments
                            .iter_mut()
                            .chain(std::iter::once(&mut function_typ.return_type))
                        {
                            if let Type::Generic {
                                generic_id,
                                num_references,
                                ..
                            } = ty
                            {
                                let generic_typ_refcount =
                                    generic_types[*generic_id as usize].refcount();
                                *ty = generic_types[*generic_id as usize]
                                    .clone()
                                    .with_num_refs(*num_references + generic_typ_refcount);
                            }
                        }

                        let literal = match reader
                            .0
                            .annotations
                            .get_first_annotation::<IntrinsicAnnotation>()
                            .map(IntrinsicAnnotation::get)
                        {
                            Some(intrinsic) => TypedLiteral::Intrinsic(intrinsic, generic_types),
                            None => TypedLiteral::Function(id, generic_types),
                        };
                        Ok((Type::Function(Arc::new(function_typ), 0), literal))
                    }
                    ModuleScopeValue::ExternalFunction(id) => {
                        let reader = &context.external_functions.read()[id];
                        let function_typ = FunctionType {
                            return_type: reader.0.return_type.clone(),
                            arguments: reader.0.arguments.iter().map(|v| v.1.clone()).collect(),
                        };
                        Ok((
                            Type::Function(Arc::new(function_typ), 0),
                            TypedLiteral::ExternalFunction(id),
                        ))
                    }
                    ModuleScopeValue::Static(id) => {
                        if !generics.is_empty() {
                            return Err(TypecheckingError::UnexpectedGenerics {
                                location: location.clone(),
                            });
                        }
                        Ok((
                            context.statics.read()[id].0.clone(),
                            TypedLiteral::Static(id),
                        ))
                    }
                    _ => Err(TypecheckingError::CannotFindValue(
                        location.clone(),
                        path.clone(),
                    )),
                }
            }
            LiteralValue::BakedAnonymousFunction(fn_id) => {
                let func = &context.functions.read()[*fn_id].0;
                let fn_typ = FunctionType {
                    return_type: func.return_type.clone(),
                    arguments: func.arguments.iter().map(|(_, v)| v.clone()).collect(),
                };
                Ok((
                    Type::Function(Arc::new(fn_typ), 0),
                    TypedLiteral::Function(*fn_id, vec![]),
                ))
            }
            LiteralValue::AnonymousFunction(..) => unreachable!("unbaked function"),
            LiteralValue::Void => Ok((Type::PrimitiveVoid(0), TypedLiteral::Void)),
        },
        Expression::Asm {
            loc,
            asm,
            volatile,
            output,
            registers,
            inputs,
        } => {
            let name = match output {
                crate::parser::TypeRef::Reference { type_name, .. } => {
                    Some(type_name.entries[0].0.clone())
                }
                _ => None,
            };
            // this should never fail unless this is an incompatible type (non-primitive)
            let output = context
                .resolve_type(module, output, &[])
                .ok()
                .filter(|v| {
                    // we have to do this match because output is either TypeRef::Reference or
                    // TypeRef::Void(0) in the case no output was specified. This means that name
                    // can only be None if the the type is `TypeRef::Void(0)`, but the user
                    // could've specified `void` as their type which is why we have to check for
                    // both. However, when this returns false, it's as such ensured that name is
                    // some.
                    (matches!(v, Type::PrimitiveVoid(0)) && name.is_none()) || v.is_asm_primitive()
                })
                .ok_or_else(|| {
                    TypecheckingError::AsmNonNumericType(
                        loc.clone(),
                        name.unwrap(), /* see comment above as to why this is fine */
                    )
                })?;
            let mut typed_inputs = Vec::with_capacity(inputs.len());
            for (loc, name) in inputs {
                let Some(((entry_ty, _), id)) = scope.get(name) else {
                    return Err(TypecheckingError::CannotFindValue(
                        loc.clone(),
                        Path::new(name.clone(), Vec::new()),
                    ));
                };
                if !entry_ty.is_asm_primitive() {
                    return Err(TypecheckingError::AsmNonNumericTypeResolved(
                        loc.clone(),
                        entry_ty.clone(),
                    ));
                }
                typed_inputs.push(id);
            }
            let id = scope.push(output.clone());
            exprs.push(TypecheckedExpression::Asm {
                location: loc.clone(),
                dst: id,
                inputs: typed_inputs,
                registers: registers.clone(),
                volatile: *volatile,
                asm: asm.clone(),
            });
            if matches!(output, Type::PrimitiveVoid(0)) {
                Ok((output, TypedLiteral::Void))
            } else {
                Ok((output, TypedLiteral::Dynamic(id)))
            }
        }
        Expression::Unary {
            operator,
            right_side,
            ..
        } if *operator == UnaryOp::Reference => {
            typecheck_take_ref(context, module, scope, right_side, exprs, type_suggestion)
        }
        Expression::Unary {
            operator,
            right_side,
            loc,
        } => {
            let (typ, right_side) =
                typecheck_expression(context, module, scope, right_side, exprs, type_suggestion)?;
            match operator {
                UnaryOp::Plus if typ.is_int_like() => {
                    tc_res!(unary scope, exprs; Pos(loc.clone(), right_side, typ))
                }
                UnaryOp::Plus => Err(TypecheckingError::CannotPos(loc.clone(), typ)),
                UnaryOp::Minus if (typ.is_int_like() && !typ.is_unsigned()) || typ.is_float() => {
                    tc_res!(unary scope, exprs; Neg(loc.clone(), right_side, typ))
                }
                UnaryOp::Minus => Err(TypecheckingError::CannotNeg(loc.clone(), typ)),
                UnaryOp::LogicalNot if matches!(typ, Type::PrimitiveBool(0)) => {
                    tc_res!(unary scope, exprs; LNot(loc.clone(), right_side, typ))
                }
                UnaryOp::LogicalNot => Err(TypecheckingError::CannotLNot(loc.clone(), typ)),
                UnaryOp::BitwiseNot
                    if typ.is_int_like() || matches!(typ, Type::PrimitiveBool(0)) =>
                {
                    tc_res!(unary scope, exprs; BNot(loc.clone(), right_side, typ))
                }
                UnaryOp::BitwiseNot => Err(TypecheckingError::CannotBNot(loc.clone(), typ)),
                UnaryOp::Dereference => match typ.deref() {
                    Ok(typ) => {
                        tc_res!(unary scope, exprs; Dereference(loc.clone(), right_side, typ))
                    }
                    Err(typ) => Err(TypecheckingError::CannotDeref(loc.clone(), typ)),
                },
                UnaryOp::Reference => unreachable!(),
            }
        }
        Expression::Binary {
            operator,
            right_side,
            left_side,
            loc,
        } => {
            if matches!(operator, BinaryOp::LShift | BinaryOp::RShift) {
                let (typ, left_side) = typecheck_expression(
                    context,
                    module,
                    scope,
                    left_side,
                    exprs,
                    type_suggestion,
                )?;
                let (typ_right, right_side) = typecheck_expression(
                    context,
                    module,
                    scope,
                    right_side,
                    exprs,
                    TypeSuggestion::Number(NumberType::Usize),
                )?;
                if !typ_right.is_int_like() || !typ_right.is_unsigned() {
                    return Err(TypecheckingError::CannotShiftByNonUInt(
                        loc.clone(),
                        typ_right,
                    ));
                }

                return match operator {
                    BinaryOp::LShift if typ.is_int_like() => {
                        tc_res!(binary scope, exprs; LShift(loc.clone(), left_side, right_side, typ))
                    }
                    BinaryOp::RShift if typ.is_int_like() => {
                        tc_res!(binary scope, exprs; RShift(loc.clone(), left_side, right_side, typ))
                    }

                    BinaryOp::LShift => Err(TypecheckingError::CannotShl(loc.clone(), typ)),
                    BinaryOp::RShift => Err(TypecheckingError::CannotShr(loc.clone(), typ)),
                    _ => unreachable!(),
                };
            }
            let (typ_left, left_side) =
                typecheck_expression(context, module, scope, left_side, exprs, type_suggestion)?;
            let (typ_right, right_side) = typecheck_expression(
                context,
                module,
                scope,
                right_side,
                exprs,
                TypeSuggestion::from_type(&typ_left),
            )?;
            if typ_left != typ_right {
                return Err(TypecheckingError::LhsNotRhs(
                    loc.clone(),
                    typ_left,
                    typ_right,
                ));
            }
            let typ = typ_left;
            let loc = loc.clone();
            match operator {
                BinaryOp::Plus if typ.is_int_like() => {
                    tc_res!(binary scope, exprs; Add(loc, left_side, right_side, typ))
                }
                BinaryOp::Minus if typ.is_int_like() => {
                    tc_res!(binary scope, exprs; Sub(loc, left_side, right_side, typ))
                }
                BinaryOp::Multiply if typ.is_int_like() => {
                    tc_res!(binary scope, exprs; Mul(loc, left_side, right_side, typ))
                }
                BinaryOp::Divide if typ.is_int_like() => {
                    tc_res!(binary scope, exprs; Div(loc, left_side, right_side, typ))
                }
                BinaryOp::Modulo if typ.is_int_like() => {
                    tc_res!(binary scope, exprs; Mod(loc, left_side, right_side, typ))
                }
                BinaryOp::BitwiseAnd if typ.is_int_like() || typ.is_bool() => {
                    tc_res!(binary scope, exprs; BAnd(loc, left_side, right_side, typ))
                }
                BinaryOp::BitwiseOr if typ.is_int_like() || typ.is_bool() => {
                    tc_res!(binary scope, exprs; BOr(loc, left_side, right_side, typ))
                }
                BinaryOp::BitwiseXor if typ.is_int_like() || typ.is_bool() => {
                    tc_res!(binary scope, exprs; BXor(loc, left_side, right_side, typ))
                }
                BinaryOp::LogicalOr if typ.is_bool() => {
                    tc_res!(binary scope, exprs; LOr(loc, left_side, right_side, typ))
                }
                BinaryOp::LogicalAnd if typ.is_bool() => {
                    tc_res!(binary scope, exprs; LAnd(loc, left_side, right_side, typ))
                }
                BinaryOp::GreaterThan if typ.is_int_like() => {
                    tc_res!(binary scope, exprs; GreaterThan(loc, left_side, right_side, Type::PrimitiveBool(0)))
                }
                BinaryOp::GreaterThanEq if typ.is_int_like() => {
                    tc_res!(binary scope, exprs; GreaterThanEq(loc, left_side, right_side, Type::PrimitiveBool(0)))
                }
                BinaryOp::LessThan if typ.is_int_like() => {
                    tc_res!(binary scope, exprs; LessThan(loc, left_side, right_side, Type::PrimitiveBool(0)))
                }
                BinaryOp::LessThanEq if typ.is_int_like() => {
                    tc_res!(binary scope, exprs; LessThanEq(loc, left_side, right_side, Type::PrimitiveBool(0)))
                }
                BinaryOp::Equals if typ.is_int_like() || typ.is_bool() => {
                    tc_res!(binary scope, exprs; Eq(loc, left_side, right_side, Type::PrimitiveBool(0)))
                }
                BinaryOp::NotEquals if typ.is_int_like() || typ.is_bool() => {
                    tc_res!(binary scope, exprs; Neq(loc, left_side, right_side, Type::PrimitiveBool(0)))
                }

                BinaryOp::Plus => Err(TypecheckingError::CannotAdd(loc, typ)),
                BinaryOp::Minus => Err(TypecheckingError::CannotSub(loc, typ)),
                BinaryOp::Multiply => Err(TypecheckingError::CannotMul(loc, typ)),
                BinaryOp::Divide => Err(TypecheckingError::CannotDiv(loc, typ)),
                BinaryOp::Modulo => Err(TypecheckingError::CannotMod(loc, typ)),
                BinaryOp::BitwiseAnd => Err(TypecheckingError::CannotBAnd(loc, typ)),
                BinaryOp::BitwiseOr => Err(TypecheckingError::CannotBOr(loc, typ)),
                BinaryOp::BitwiseXor => Err(TypecheckingError::CannotBXor(loc, typ)),
                BinaryOp::LogicalOr => Err(TypecheckingError::CannotLOr(loc, typ)),
                BinaryOp::LogicalAnd => Err(TypecheckingError::CannotLAnd(loc, typ)),
                BinaryOp::GreaterThan
                | BinaryOp::GreaterThanEq
                | BinaryOp::LessThan
                | BinaryOp::LessThanEq => Err(TypecheckingError::CannotCompare(loc, typ)),
                BinaryOp::Equals | BinaryOp::NotEquals => {
                    Err(TypecheckingError::CannotEq(loc, typ))
                }
                BinaryOp::RShift | BinaryOp::LShift => unreachable!(),
            }
        }
        Expression::FunctionCall {
            identifier,
            arguments,
        } => {
            let (typ, function_expr) = typecheck_expression(
                context,
                module,
                scope,
                identifier,
                exprs,
                TypeSuggestion::Unknown,
            )?;
            let has_vararg = if let TypedLiteral::ExternalFunction(id) = function_expr {
                context.external_functions.read()[id]
                    .0
                    .annotations
                    .get_first_annotation::<ExternVarArg>()
                    .is_some()
            } else {
                false
            };
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
            if arguments.len() > function_type.arguments.len() && !has_vararg {
                return Err(TypecheckingError::TooManyArguments {
                    location: arguments[function_type.arguments.len().saturating_sub(1)]
                        .loc()
                        .clone(),
                });
            }
            for (i, arg) in arguments.iter().enumerate() {
                let (typ, expr) = typecheck_expression(
                    context,
                    module,
                    scope,
                    arg,
                    exprs,
                    function_type
                        .arguments
                        .get(i)
                        .map(TypeSuggestion::from_type)
                        .unwrap_or_default(),
                )?;
                // ignore for i => function_type.arguments.len() because we can't possibly know the
                // type of varargs. This is of course incredibly unsafe and as such safety
                // precautions have to be taken when calling a function with varargs, but the
                // compiler cannot help with those.
                if i < function_type.arguments.len() && typ != function_type.arguments[i] {
                    return Err(TypecheckingError::MismatchingType {
                        expected: function_type.arguments[i].clone(),
                        found: typ,
                        location: arguments[i].loc().clone(),
                    });
                }
                typed_arguments.push(expr);
            }

            if let TypedLiteral::Intrinsic(intrinsic, generics) = function_expr {
                let typ = function_type.return_type.clone();
                let id = scope.push(typ.clone());
                exprs.push(TypecheckedExpression::IntrinsicCall(
                    identifier.loc().clone(),
                    id,
                    intrinsic,
                    typed_arguments,
                    generics,
                ));
                Ok((typ, TypedLiteral::Dynamic(id)))
            } else if let TypedLiteral::Function(fn_id, generics) = function_expr {
                let typ = function_type.return_type.clone();
                let id = scope.push(typ.clone());
                exprs.push(TypecheckedExpression::DirectCall(
                    identifier.loc().clone(),
                    id,
                    fn_id,
                    typed_arguments,
                    generics,
                ));
                Ok((typ, TypedLiteral::Dynamic(id)))
            } else if let TypedLiteral::ExternalFunction(fn_id) = function_expr {
                tc_res!(binary scope, exprs; DirectExternCall(identifier.loc().clone(), fn_id, typed_arguments, function_type.return_type.clone()))
            } else {
                tc_res!(binary scope, exprs; Call(identifier.loc().clone(), function_expr, typed_arguments, function_type.return_type.clone()))
            }
        }
        Expression::Indexing { .. } | Expression::MemberAccess { .. } => {
            copy_resolve_indexing(context, module, scope, expression, exprs, type_suggestion)
        }
        Expression::MemberCall {
            identifier,
            lhs,
            arguments,
        } => typecheck_membercall(context, module, scope, exprs, lhs, identifier, arguments),
        Expression::Assignment {
            left_side,
            right_side,
            loc,
        } => {
            let (typ_lhs, lhs) = match &**left_side {
                Expression::Unary {
                    operator: UnaryOp::Dereference,
                    right_side,
                    ..
                } => {
                    let (typ, lhs) = typecheck_expression(
                        context,
                        module,
                        scope,
                        right_side,
                        exprs,
                        TypeSuggestion::Unknown,
                    )?;
                    let typ = typ.deref().map_err(|typ| {
                        TypecheckingError::CannotDeref(right_side.loc().clone(), typ)
                    })?;

                    (typ, lhs)
                }
                _ => {
                    let (typ, lhs) = typecheck_expression(
                        context,
                        module,
                        scope,
                        &Expression::Unary {
                            operator: UnaryOp::Reference,
                            loc: loc.clone(),
                            right_side: left_side.clone(),
                        },
                        exprs,
                        TypeSuggestion::Unknown,
                    )?;
                    (
                        typ.deref().expect("&_ should never fail to dereference"),
                        lhs,
                    )
                }
            };
            let (typ_rhs, rhs) = typecheck_expression(
                context,
                module,
                scope,
                right_side,
                exprs,
                TypeSuggestion::from_type(&typ_lhs),
            )?;

            if typ_lhs != typ_rhs {
                return Err(TypecheckingError::MismatchingType {
                    expected: typ_lhs,
                    found: typ_rhs,
                    location: loc.clone(),
                });
            }
            exprs.push(TypecheckedExpression::StoreAssignment(
                loc.clone(),
                lhs,
                rhs,
            ));
            Ok((Type::PrimitiveVoid(0), TypedLiteral::Void))
        }
        Expression::Range {
            left_side,
            right_side,
            ..
        } => {
            let (typ, _) =
                typecheck_expression(context, module, scope, left_side, exprs, type_suggestion)?;
            let (typ_rhs, _) = typecheck_expression(
                context,
                module,
                scope,
                right_side,
                exprs,
                TypeSuggestion::from_type(&typ),
            )?;
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
        } => {
            let (typ, lhs) =
                typecheck_expression(context, module, scope, left_side, exprs, type_suggestion)?;
            let new_type = context.resolve_type(module, new_type, &[])?;
            typecheck_cast(scope, exprs, typ, new_type, lhs, loc.clone(), context)
        }
    }
}

fn typecheck_cast(
    scope: &mut Scopes,
    exprs: &mut Vec<TypecheckedExpression>,
    typ: Type,
    new_typ: Type,
    lhs: TypedLiteral,
    loc: Location,
    context: &TypecheckingContext,
) -> Result<(Type, TypedLiteral), TypecheckingError> {
    if typ == new_typ {
        return Ok((new_typ, lhs));
    }

    match (&typ, &new_typ) {
        // &str -> &[u8]
        (
            Type::PrimitiveStr(ref_self),
            Type::UnsizedArray {
                typ,
                num_references: ref_other,
            },
        ) if ref_self == ref_other && **typ == Type::PrimitiveU8(0) => {
            let id = scope.push(new_typ.clone());
            exprs.push(TypecheckedExpression::Alias(loc, id, lhs));
            Ok((new_typ, TypedLiteral::Dynamic(id)))
        }
        // &str -> &u8
        (Type::PrimitiveStr(1), Type::PrimitiveU8(1)) => {
            let id = scope.push(new_typ.clone());
            exprs.push(TypecheckedExpression::StripMetadata(loc, id, lhs));
            Ok((new_typ, TypedLiteral::Dynamic(id)))
        }
        // &T to &[T; 1]
        (
            _,
            Type::SizedArray {
                typ: typ_other,
                num_references: ref_other,
                number_elements: 1,
            },
        ) if *ref_other > 0
            && typ.refcount() >= *ref_other
            && typ.clone().with_num_refs(typ.refcount() - *ref_other) == **typ_other =>
        {
            let id = scope.push(new_typ.clone());
            exprs.push(TypecheckedExpression::Alias(loc, id, lhs));
            Ok((new_typ, TypedLiteral::Dynamic(id)))
        }
        // &T -> &dyn _
        (
            v,
            Type::DynType {
                trait_refs,
                num_references: 1,
            },
        ) if v.is_thin_ptr() && v.refcount() > 0 => {
            let traits = trait_refs.iter().map(|v| v.0).collect::<Vec<_>>();
            let ty = v.clone().deref().expect("v should have a refcount of > 0");
            if !ty.implements(&traits, context) {
                let trait_reader = context.traits.read();
                return Err(TypecheckingError::MismatchingTraits(
                    loc,
                    typ,
                    traits
                        .iter()
                        .map(|v| trait_reader[*v].name.clone())
                        .collect(),
                ));
            }
            let id = scope.push(new_typ.clone());
            exprs.push(TypecheckedExpression::AttachVtable(
                loc,
                id,
                lhs,
                (ty, traits),
            ));
            Ok((new_typ, TypedLiteral::Dynamic(id)))
        }
        // &[T; N] -> &[T]
        (
            Type::SizedArray {
                typ,
                num_references: 1,
                number_elements,
            },
            Type::UnsizedArray {
                typ: typ_other,
                num_references: 1,
            },
        ) if typ == typ_other => {
            let id = scope.push(new_typ.clone());
            exprs.push(TypecheckedExpression::MakeUnsizedSlice(
                loc,
                id,
                lhs,
                *number_elements,
            ));
            Ok((new_typ, TypedLiteral::Dynamic(id)))
        }
        // (&[T] -> &T)
        (
            Type::UnsizedArray {
                typ,
                num_references: 1,
            },
            _,
        ) if new_typ.refcount() == 1 && new_typ == typ.clone().take_ref() => {
            let id = scope.push(new_typ.clone());
            exprs.push(TypecheckedExpression::StripMetadata(loc, id, lhs));
            Ok((new_typ, TypedLiteral::Dynamic(id)))
        }
        // &&void -> &void
        (Type::PrimitiveVoid(ref_self), Type::PrimitiveVoid(1)) if *ref_self > 0 => {
            let id = scope.push(new_typ.clone());
            exprs.push(TypecheckedExpression::Bitcast(loc, id, lhs));
            Ok((new_typ, TypedLiteral::Dynamic(id)))
        }
        // &void -> usize
        (Type::PrimitiveVoid(1), Type::PrimitiveUSize(0)) => {
            let id = scope.push(new_typ.clone());
            exprs.push(TypecheckedExpression::PtrToInt(loc, id, lhs));
            Ok((new_typ, TypedLiteral::Dynamic(id)))
        }
        // usize -> &void
        (Type::PrimitiveUSize(0), Type::PrimitiveVoid(1)) => {
            let id = scope.push(new_typ.clone());
            exprs.push(TypecheckedExpression::IntToPtr(loc, id, lhs));
            Ok((new_typ, TypedLiteral::Dynamic(id)))
        }
        // fn to &void
        (Type::Function(_, 0), Type::PrimitiveVoid(1)) => {
            let id = scope.push(new_typ.clone());
            exprs.push(TypecheckedExpression::Bitcast(loc, id, lhs));
            Ok((new_typ, TypedLiteral::Dynamic(id)))
        }
        // &T to &void
        (_, Type::PrimitiveVoid(ref_other))
            if *ref_other > 0 && typ.refcount() > 0 && typ.is_thin_ptr() =>
        {
            let id = scope.push(new_typ.clone());
            exprs.push(TypecheckedExpression::Bitcast(loc, id, lhs));
            Ok((new_typ, TypedLiteral::Dynamic(id)))
        }
        // &void to &T
        (Type::PrimitiveVoid(ref_self), _) if *ref_self > 0 && new_typ.is_thin_ptr() => {
            let id = scope.push(new_typ.clone());
            exprs.push(TypecheckedExpression::Bitcast(loc, id, lhs));
            Ok((new_typ, TypedLiteral::Dynamic(id)))
        }
        (
            Type::PrimitiveU8(0) | Type::PrimitiveBool(0),
            Type::PrimitiveU8(0) | Type::PrimitiveBool(0),
        )
        | (
            Type::PrimitiveU8(0)
            | Type::PrimitiveU16(0)
            | Type::PrimitiveU32(0)
            | Type::PrimitiveU64(0)
            | Type::PrimitiveUSize(0)
            | Type::PrimitiveI8(0)
            | Type::PrimitiveI16(0)
            | Type::PrimitiveI32(0)
            | Type::PrimitiveI64(0)
            | Type::PrimitiveISize(0)
            | Type::PrimitiveF32(0)
            | Type::PrimitiveF64(0),
            Type::PrimitiveU8(0)
            | Type::PrimitiveU16(0)
            | Type::PrimitiveU32(0)
            | Type::PrimitiveU64(0)
            | Type::PrimitiveUSize(0)
            | Type::PrimitiveI8(0)
            | Type::PrimitiveI16(0)
            | Type::PrimitiveI32(0)
            | Type::PrimitiveI64(0)
            | Type::PrimitiveISize(0)
            | Type::PrimitiveF32(0)
            | Type::PrimitiveF64(0),
        ) => {
            let id = scope.push(new_typ.clone());
            exprs.push(TypecheckedExpression::IntCast(loc, id, lhs));
            Ok((new_typ, TypedLiteral::Dynamic(id)))
        }
        _ => Err(TypecheckingError::DisallowedCast(loc, typ, new_typ)),
    }
}

#[allow(clippy::too_many_arguments)]
fn typecheck_dyn_membercall(
    context: &TypecheckingContext,
    scope: &mut Scopes,
    module: ModuleId,
    exprs: &mut Vec<TypecheckedExpression>,
    ident: &GlobalStr,
    args: &[Expression],
    lhs: TypedLiteral,
    lhs_loc: Location,
    trait_refs: Vec<(usize, GlobalStr)>,
    num_references: u8,
) -> Result<(Type, TypedLiteral), TypecheckingError> {
    let trait_reader = context.traits.read();
    let mut offset = 0;
    let (mut arg_typs, return_ty, trait_name) = 'out: {
        for trait_id in trait_refs.iter().map(|v| v.0) {
            for func in trait_reader[trait_id].functions.iter() {
                if func.0 == *ident {
                    break 'out (
                        func.1.clone(),
                        func.2.clone(),
                        trait_reader[trait_id].name.clone(),
                    );
                }
                offset += 1;
            }
        }

        return Err(TypecheckingError::CannotFindFunctionOnType(
            lhs_loc,
            ident.clone(),
            Type::DynType {
                trait_refs,
                num_references,
            },
        ));
    };
    drop(trait_reader);

    match &arg_typs[0].1 {
        Type::PrimitiveSelf(_) => {}
        _ => {
            return Err(TypecheckingError::InvalidDynTypeFunc(
                lhs_loc,
                ident.clone(),
                trait_name,
            ))
        }
    }
    if matches!(return_ty, Type::PrimitiveSelf(_))
        || arg_typs
            .iter()
            .skip(1)
            .any(|v| matches!(v.1, Type::PrimitiveSelf(_)))
    {
        return Err(TypecheckingError::InvalidDynTypeFunc(
            lhs_loc,
            ident.clone(),
            trait_name,
        ));
    }

    let mut typ = Type::DynType {
        trait_refs,
        num_references,
    };
    let mut lhs = lhs;
    while typ.refcount() > 1 {
        typ = typ.deref().expect("dereferencing &_ should never fail");
        let id = scope.push(typ.clone());
        exprs.push(TypecheckedExpression::Dereference(lhs_loc.clone(), id, lhs));
        lhs = TypedLiteral::Dynamic(id);
    }

    let mut typed_args = Vec::with_capacity(args.len() + 1);
    typed_args.push(lhs);

    if args.len() < arg_typs.len() - 1 {
        return Err(TypecheckingError::MissingArguments { location: lhs_loc });
    }
    if args.len() > arg_typs.len() - 1 {
        return Err(TypecheckingError::TooManyArguments {
            location: args[arg_typs.len() - 1].loc().clone(),
        });
    }

    for i in 0..args.len() {
        let (ty, lit) = typecheck_expression(
            context,
            module,
            scope,
            &args[i],
            exprs,
            TypeSuggestion::from_type(&arg_typs[i + 1].1),
        )?;
        if ty != arg_typs[i + 1].1 {
            return Err(TypecheckingError::MismatchingType {
                expected: arg_typs.remove(i + 1).1,
                found: typ,
                location: args[i].loc().clone(),
            });
        }
        typed_args.push(lit);
    }

    let is_void = matches!(return_ty, Type::PrimitiveNever | Type::PrimitiveVoid(0));
    let id = scope.push(return_ty.clone());
    exprs.push(TypecheckedExpression::DynCall(
        lhs_loc, id, typed_args, offset,
    ));
    if is_void {
        return Ok((return_ty, TypedLiteral::Void));
    }
    Ok((return_ty, TypedLiteral::Dynamic(id)))
}

fn typecheck_membercall(
    context: &TypecheckingContext,
    module: ModuleId,
    scope: &mut Scopes,
    exprs: &mut Vec<TypecheckedExpression>,
    lhs: &Expression,
    ident: &GlobalStr,
    args: &[Expression],
) -> Result<(Type, TypedLiteral), TypecheckingError> {
    let (mut typ_lhs, mut typed_literal_lhs) =
        typecheck_take_ref(context, module, scope, lhs, exprs, TypeSuggestion::Unknown)?;
    match typ_lhs {
        Type::DynType {
            trait_refs,
            num_references,
        } if num_references > 0 => {
            return typecheck_dyn_membercall(
                context,
                scope,
                module,
                exprs,
                ident,
                args,
                typed_literal_lhs,
                lhs.loc().clone(),
                trait_refs,
                num_references,
            );
        }
        _ => (),
    }

    let function_reader = context.functions.read();
    let langitem_reader = context.lang_items.read();
    let struct_reader = context.structs.read();

    let struct_id = match &typ_lhs {
        Type::UnsizedArray { .. }
        | Type::SizedArray { .. }
        | Type::Tuple { .. }
        | Type::Trait { .. }
        | Type::DynType { .. }
        | Type::Generic { .. }
        | Type::Function(..)
        | Type::PrimitiveVoid(..)
        | Type::PrimitiveSelf(..)
        | Type::PrimitiveNever => None,
        Type::Struct { struct_id, .. } => Some(*struct_id),
        Type::PrimitiveI8(_) => langitem_reader.i8,
        Type::PrimitiveI16(_) => langitem_reader.i16,
        Type::PrimitiveI32(_) => langitem_reader.i32,
        Type::PrimitiveI64(_) => langitem_reader.i64,
        Type::PrimitiveISize(_) => langitem_reader.isize,
        Type::PrimitiveU8(_) => langitem_reader.u8,
        Type::PrimitiveU16(_) => langitem_reader.u16,
        Type::PrimitiveU32(_) => langitem_reader.u32,
        Type::PrimitiveU64(_) => langitem_reader.u64,
        Type::PrimitiveUSize(_) => langitem_reader.usize,
        Type::PrimitiveF32(_) => langitem_reader.f32,
        Type::PrimitiveF64(_) => langitem_reader.f64,
        Type::PrimitiveStr(_) => langitem_reader.str,
        Type::PrimitiveBool(_) => langitem_reader.bool,
    };
    drop(langitem_reader);
    let structure = struct_id.map(|v| &struct_reader[v]);
    let function_id = structure
        .and_then(|v| v.global_impl.get(ident))
        .copied()
        .or_else(|| {
            structure?
                .trait_impl
                .iter()
                .filter_map(|v| {
                    v.1.iter().find(|v| {
                        if let Some(name) = &function_reader[**v].0.name {
                            name == ident
                        } else {
                            false
                        }
                    })
                })
                .next()
                .copied()
        });
    drop(struct_reader);
    let Some(function_id) = function_id else {
        return Err(TypecheckingError::CannotFindFunctionOnType(
            lhs.loc().clone(),
            ident.clone(),
            typ_lhs,
        ));
    };

    let function = &function_reader[function_id];
    if function
        .0
        .arguments
        .first()
        .map(|v| v.1.clone().without_ref())
        != Some(typ_lhs.clone().without_ref())
    {
        return Err(TypecheckingError::NonMemberFunction(
            function.0.location.clone(),
            ident.clone(),
            typ_lhs,
        ));
    }
    let arg_refcount = function.0.arguments[0].1.refcount();
    while typ_lhs.refcount() != arg_refcount {
        if typ_lhs.refcount() > arg_refcount {
            typ_lhs = typ_lhs
                .deref()
                .expect("you should always be able to deref &_");
            let new_id = scope.push(typ_lhs.clone());
            exprs.push(TypecheckedExpression::Dereference(
                lhs.loc().clone(),
                new_id,
                typed_literal_lhs,
            ));
            typed_literal_lhs = TypedLiteral::Dynamic(new_id);
        } else {
            typed_literal_lhs = make_reference(
                scope,
                exprs,
                typ_lhs.clone(),
                typed_literal_lhs,
                lhs.loc().clone(),
            );
            typ_lhs = typ_lhs.take_ref();
        }
    }

    let mut typed_arguments = Vec::with_capacity(function.0.arguments.len() + 1);
    typed_arguments.push(typed_literal_lhs);
    if args.len() < function.0.arguments.len() - 1 {
        return Err(TypecheckingError::MissingArguments {
            location: lhs.loc().clone(),
        });
    }
    if args.len() > function.0.arguments.len() - 1 {
        return Err(TypecheckingError::TooManyArguments {
            location: args[function.0.arguments.len() - 1].loc().clone(),
        });
    }
    for (i, (_, arg)) in function.0.arguments.iter().skip(1).enumerate() {
        let (typ, expr) = typecheck_expression(
            context,
            module,
            scope,
            &args[i + 1], // skip one argument for the `self` argument
            exprs,
            TypeSuggestion::from_type(arg),
        )?;
        if typ != function.0.arguments[i + 1].1 {
            return Err(TypecheckingError::MismatchingType {
                expected: function.0.arguments[i + 1].1.clone(),
                found: typ,
                location: args[i].loc().clone(),
            });
        }
        typed_arguments.push(expr);
    }

    let call_id = scope.push(function.0.return_type.clone());
    exprs.push(TypecheckedExpression::DirectCall(
        lhs.loc().clone(),
        call_id,
        function_id,
        typed_arguments,
        Vec::new(),
    ));

    Ok((
        function.0.return_type.clone(),
        TypedLiteral::Dynamic(call_id),
    ))
}

fn typecheck_take_ref(
    context: &TypecheckingContext,
    module: ModuleId,
    scope: &mut Scopes,
    expression: &Expression,
    exprs: &mut Vec<TypecheckedExpression>,
    type_suggestion: TypeSuggestion,
) -> Result<(Type, TypedLiteral), TypecheckingError> {
    match expression {
        //&*_1 => _1
        Expression::Unary {
            operator,
            right_side,
            ..
        } if *operator == UnaryOp::Dereference => {
            typecheck_expression(context, module, scope, right_side, exprs, type_suggestion)
        }
        _ => {
            let (typ, expr) = ref_resolve_indexing(
                context,
                module,
                scope,
                expression,
                exprs,
                type_suggestion,
                true,
            )?;
            let typ = typ.take_ref();
            Ok((typ, expr))
        }
    }
}

// NOTE: the actual type of the value is a reference on top of the returned type, but we don't do
// that as that would require additional computations. The reference is as such implicit but has to
// be added when pushing the type onto the scope (e.g. during auto deref)
fn ref_resolve_indexing(
    context: &TypecheckingContext,
    module: ModuleId,
    scope: &mut Scopes,
    expression: &Expression,
    exprs: &mut Vec<TypecheckedExpression>,
    type_suggestion: TypeSuggestion,
    increase_ref: bool,
) -> Result<(Type, TypedLiteral), TypecheckingError> {
    match expression {
        Expression::MemberAccess {
            left_side,
            index,
            loc,
        } => {
            let (mut typ_lhs, mut typed_literal_lhs) = ref_resolve_indexing(
                context,
                module,
                scope,
                left_side,
                exprs,
                TypeSuggestion::Unknown,
                false,
            )?;
            for element_name in index {
                while typ_lhs.refcount() > 0 {
                    typ_lhs = typ_lhs
                        .deref()
                        .expect("&_ should never fail to dereference");
                    let id = scope.push(typ_lhs.clone().take_ref());
                    exprs.push(TypecheckedExpression::Dereference(
                        expression.loc().clone(),
                        id,
                        typed_literal_lhs,
                    ));
                    typed_literal_lhs = TypedLiteral::Dynamic(id);
                }
                assert_eq!(typ_lhs.refcount(), 0, "non-zero refcount after auto-deref");
                match typ_lhs {
                    Type::Struct { struct_id, .. } => {
                        let structure = &context.structs.read()[struct_id];
                        match structure
                            .elements
                            .iter()
                            .enumerate()
                            .find(|(_, (v, _))| v == element_name)
                            .map(|(idx, (_, typ))| (idx, typ))
                        {
                            Some((idx, typ)) => {
                                typ_lhs = typ.clone();
                                let new_val = scope.push(typ_lhs.clone().take_ref());
                                exprs.push(TypecheckedExpression::Offset(
                                    loc.clone(),
                                    new_val,
                                    typed_literal_lhs,
                                    OffsetValue::Static(idx),
                                ));
                                typed_literal_lhs = TypedLiteral::Dynamic(new_val);
                            }
                            None => {
                                return Err(TypecheckingError::FieldNotFound(
                                    expression.loc().clone(),
                                    typ_lhs,
                                    element_name.clone(),
                                ))
                            }
                        }
                    }
                    _ => {
                        return Err(TypecheckingError::AccessNonStructValue(
                            loc.clone(),
                            typ_lhs,
                        ))
                    }
                };
            }
            Ok((typ_lhs, typed_literal_lhs))
        }
        Expression::Indexing {
            left_side,
            right_side,
        } => {
            let (mut typ_lhs, mut typed_literal_lhs) = ref_resolve_indexing(
                context,
                module,
                scope,
                left_side,
                exprs,
                TypeSuggestion::Array(Box::new(type_suggestion)),
                false,
            )?;
            while typ_lhs.refcount() > 0 {
                typ_lhs = typ_lhs
                    .deref()
                    .expect("&_ should never fail to dereference");
                let id = scope.push(typ_lhs.clone().take_ref());
                exprs.push(TypecheckedExpression::Dereference(
                    expression.loc().clone(),
                    id,
                    typed_literal_lhs,
                ));
                typed_literal_lhs = TypedLiteral::Dynamic(id);
            }
            assert_eq!(typ_lhs.refcount(), 0, "non-zero refcount after auto-deref");
            let offset = indexing_resolve_rhs(context, module, scope, right_side, exprs)?;
            let typ = match typ_lhs {
                Type::SizedArray { typ, .. } => *typ,
                Type::UnsizedArray { typ, .. } => *typ,
                Type::Tuple { elements, .. } => match offset {
                    OffsetValue::Dynamic(_) => {
                        return Err(TypecheckingError::TupleDynamicIndex(
                            expression.loc().clone(),
                        ))
                    }
                    OffsetValue::Static(idx) if idx >= elements.len() => {
                        return Err(TypecheckingError::TupleIndexOutOfBounds(
                            expression.loc().clone(),
                            elements.len(),
                            idx,
                        ))
                    }
                    OffsetValue::Static(idx) => elements[idx].clone(),
                },
                _ => {
                    return Err(TypecheckingError::IndexNonArrayElem(
                        expression.loc().clone(),
                        typ_lhs,
                    ))
                }
            };

            let id = scope.push(typ.clone().take_ref());
            exprs.push(TypecheckedExpression::Offset(
                expression.loc().clone(),
                id,
                typed_literal_lhs,
                offset,
            ));
            Ok((typ, TypedLiteral::Dynamic(id)))
        }
        _ => {
            let (typ, typed_literal) = typecheck_expression(
                context,
                module,
                scope,
                expression,
                exprs,
                type_suggestion.clone(),
            )?;

            if let TypeSuggestion::UnsizedArray(_) = type_suggestion {
                if let Type::SizedArray {
                    num_references: 1,
                    number_elements,
                    typ,
                } = &typ
                {
                    let typ = Type::UnsizedArray {
                        typ: typ.clone(),
                        num_references: 0,
                    };
                    let id = scope.push(typ.clone().take_ref());
                    exprs.push(TypecheckedExpression::MakeUnsizedSlice(
                        expression.loc().clone(),
                        id,
                        typed_literal,
                        *number_elements,
                    ));
                    return Ok((typ, TypedLiteral::Dynamic(id)));
                }
            }

            if typ.refcount() > 0 && !increase_ref {
                return Ok((
                    typ.deref()
                        .expect("&_ should never fail to be dereferenced"),
                    typed_literal,
                ));
            }

            match (type_suggestion, typ) {
                (
                    TypeSuggestion::UnsizedArray(_),
                    Type::SizedArray {
                        typ,
                        num_references: 0,
                        number_elements,
                    },
                ) => {
                    let typed_literal_sized_array_ref = make_reference(
                        scope,
                        exprs,
                        Type::SizedArray {
                            typ: typ.clone(),
                            num_references: 0,
                            number_elements,
                        },
                        typed_literal,
                        expression.loc().clone(),
                    );
                    let typ = Type::UnsizedArray {
                        typ,
                        num_references: 0,
                    };
                    let id = scope.push(typ.clone().take_ref());
                    exprs.push(TypecheckedExpression::MakeUnsizedSlice(
                        expression.loc().clone(),
                        id,
                        typed_literal_sized_array_ref,
                        number_elements,
                    ));
                    Ok((typ, TypedLiteral::Dynamic(id)))
                }
                (_, typ) => Ok((
                    typ.clone(),
                    make_reference(scope, exprs, typ, typed_literal, expression.loc().clone()),
                )),
            }
        }
    }
}

fn indexing_resolve_rhs(
    context: &TypecheckingContext,
    module: ModuleId,
    scope: &mut Scopes,
    expression: &Expression,
    exprs: &mut Vec<TypecheckedExpression>,
) -> Result<OffsetValue, TypecheckingError> {
    let (typ, rhs) = typecheck_expression(
        context,
        module,
        scope,
        expression,
        exprs,
        TypeSuggestion::Number(NumberType::Usize),
    )?;
    if !matches!(typ, Type::PrimitiveUSize(0)) {
        return Err(TypecheckingError::MismatchingType {
            expected: Type::PrimitiveUSize(0),
            found: typ,
            location: expression.loc().clone(),
        });
    }
    match rhs {
        TypedLiteral::Dynamic(v) => Ok(OffsetValue::Dynamic(v)),
        TypedLiteral::USize(v) => Ok(OffsetValue::Static(v)),
        _ => unreachable!(),
    }
}

/// typ - the type before the reference (as in, typ is the type of the typed_literal, the type of
/// the returned type literal is typ.take_ref())
fn make_reference(
    scope: &mut Scopes,
    expressions: &mut Vec<TypecheckedExpression>,
    typ: Type,
    mut typed_literal: TypedLiteral,
    loc: Location,
) -> TypedLiteral {
    match typed_literal {
        TypedLiteral::Void | TypedLiteral::Static(_) => {}
        TypedLiteral::Dynamic(v) => scope.make_stack_allocated(v),
        _ => {
            let lit_id = scope.push(typ.clone());
            scope.make_stack_allocated(lit_id);
            expressions.push(TypecheckedExpression::Literal(
                loc.clone(),
                lit_id,
                typed_literal,
            ));
            typed_literal = TypedLiteral::Dynamic(lit_id);
        }
    }

    let new_id = scope.push(typ.take_ref());
    expressions.push(TypecheckedExpression::Reference(loc, new_id, typed_literal));
    TypedLiteral::Dynamic(new_id)
}

fn copy_resolve_indexing(
    context: &TypecheckingContext,
    module: ModuleId,
    scope: &mut Scopes,
    expression: &Expression,
    exprs: &mut Vec<TypecheckedExpression>,
    type_suggestion: TypeSuggestion,
) -> Result<(Type, TypedLiteral), TypecheckingError> {
    match expression {
        Expression::Indexing {
            left_side,
            right_side,
        } => {
            let offset = indexing_resolve_rhs(context, module, scope, right_side, exprs)?;
            let (typ, lhs) = copy_resolve_indexing(
                context,
                module,
                scope,
                left_side,
                exprs,
                TypeSuggestion::Array(Box::new(type_suggestion)),
            )?;
            if !matches!(
                typ,
                Type::UnsizedArray { .. } | Type::SizedArray { .. } | Type::Tuple { .. }
            ) {
                return Err(TypecheckingError::IndexNonArrayElem(
                    expression.loc().clone(),
                    typ,
                ));
            }
            if typ.refcount() == 0 {
                let new_typ = match typ.clone() {
                    Type::SizedArray { typ, .. } => *typ,
                    Type::Tuple { elements, .. } => match offset {
                        OffsetValue::Dynamic(_) => {
                            return Err(TypecheckingError::TupleDynamicIndex(
                                expression.loc().clone(),
                            ))
                        }
                        OffsetValue::Static(v) if v >= elements.len() => {
                            return Err(TypecheckingError::TupleIndexOutOfBounds(
                                expression.loc().clone(),
                                elements.len(),
                                v,
                            ))
                        }
                        OffsetValue::Static(v) => elements[v].clone(),
                    },
                    Type::UnsizedArray { .. } => panic!("unsized element without references"),
                    _ => unreachable!(),
                };
                let new_id = match offset {
                    OffsetValue::Static(offset) => {
                        let new_id = scope.push(new_typ.clone());
                        exprs.push(TypecheckedExpression::OffsetNonPointer(
                            expression.loc().clone(),
                            new_id,
                            lhs,
                            offset,
                        ));
                        new_id
                    }
                    off @ OffsetValue::Dynamic(_) => {
                        if let TypedLiteral::Dynamic(lhs) = lhs {
                            scope.make_stack_allocated(lhs);
                        }
                        let typed_lit_ref =
                            make_reference(scope, exprs, typ, lhs, expression.loc().clone());
                        let offset_id = scope.push(new_typ.clone().take_ref());
                        let new_id = scope.push(new_typ.clone());
                        exprs.push(TypecheckedExpression::Offset(
                            expression.loc().clone(),
                            offset_id,
                            typed_lit_ref,
                            off,
                        ));
                        exprs.push(TypecheckedExpression::Dereference(
                            expression.loc().clone(),
                            new_id,
                            TypedLiteral::Dynamic(offset_id),
                        ));
                        new_id
                    }
                };
                Ok((new_typ, TypedLiteral::Dynamic(new_id)))
            } else {
                let mut typ = typ;
                let mut lhs = lhs;
                while typ.refcount() > 1 {
                    typ = typ.deref().expect("dereferencing &_ should never fail");
                    let new_id = scope.push(typ.clone());
                    exprs.push(TypecheckedExpression::Dereference(
                        expression.loc().clone(),
                        new_id,
                        lhs,
                    ));
                    lhs = TypedLiteral::Dynamic(new_id);
                }
                let typ = match typ {
                    Type::UnsizedArray { typ, .. } | Type::SizedArray { typ, .. } => *typ,
                    _ => unreachable!(),
                };
                let offset_id = scope.push(typ.clone().take_ref());
                let value_id = scope.push(typ.clone());
                exprs.push(TypecheckedExpression::Offset(
                    expression.loc().clone(),
                    offset_id,
                    lhs,
                    offset,
                ));
                exprs.push(TypecheckedExpression::Dereference(
                    expression.loc().clone(),
                    value_id,
                    TypedLiteral::Dynamic(offset_id),
                ));
                Ok((typ, TypedLiteral::Dynamic(value_id)))
            }
        }
        Expression::MemberAccess {
            index,
            left_side,
            loc,
        } => {
            let (mut typ_lhs, mut typed_literal_lhs) = copy_resolve_indexing(
                context,
                module,
                scope,
                left_side,
                exprs,
                TypeSuggestion::Unknown,
            )?;
            for element_name in index {
                let needs_deref = typ_lhs.refcount() > 0;
                while typ_lhs.refcount() > 1 {
                    typ_lhs = typ_lhs.deref().expect("dereferencing &_ should never fail");
                    let new_id = scope.push(typ_lhs.clone());
                    exprs.push(TypecheckedExpression::Dereference(
                        expression.loc().clone(),
                        new_id,
                        typed_literal_lhs,
                    ));
                    typed_literal_lhs = TypedLiteral::Dynamic(new_id);
                }
                let offset = match typ_lhs {
                    Type::Struct { struct_id, .. } => {
                        let structure = &context.structs.read()[struct_id];
                        match structure
                            .elements
                            .iter()
                            .enumerate()
                            .find(|(_, (v, _))| v == element_name)
                            .map(|(idx, (_, typ))| (idx, typ))
                        {
                            Some((idx, typ)) => {
                                typ_lhs = typ.clone();
                                idx
                            }
                            None => {
                                return Err(TypecheckingError::FieldNotFound(
                                    expression.loc().clone(),
                                    typ_lhs.without_ref(),
                                    element_name.clone(),
                                ))
                            }
                        }
                    }
                    _ => {
                        return Err(TypecheckingError::AccessNonStructValue(
                            loc.clone(),
                            typ_lhs,
                        ))
                    }
                };
                if !needs_deref {
                    let new_id = scope.push(typ_lhs.clone());
                    if let TypedLiteral::Dynamic(id) = typed_literal_lhs {
                        scope.make_stack_allocated(id);
                    }
                    exprs.push(TypecheckedExpression::OffsetNonPointer(
                        loc.clone(),
                        new_id,
                        typed_literal_lhs,
                        offset,
                    ));
                    typed_literal_lhs = TypedLiteral::Dynamic(new_id);
                } else {
                    let offset_id = scope.push(typ_lhs.clone().take_ref());
                    let deref_id = scope.push(typ_lhs.clone());
                    exprs.push(TypecheckedExpression::Offset(
                        loc.clone(),
                        offset_id,
                        typed_literal_lhs,
                        OffsetValue::Static(offset),
                    ));
                    exprs.push(TypecheckedExpression::Dereference(
                        loc.clone(),
                        deref_id,
                        TypedLiteral::Dynamic(offset_id),
                    ));
                    typed_literal_lhs = TypedLiteral::Dynamic(deref_id);
                }
            }
            Ok((typ_lhs, typed_literal_lhs))
        }
        _ => typecheck_expression(context, module, scope, expression, exprs, type_suggestion),
    }
}
