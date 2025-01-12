use std::{collections::HashMap, sync::Arc};

use crate::{
    globals::GlobalStr,
    module::{ModuleContext, ModuleId, ModuleScopeValue, StaticId},
    parser::{Expression, LiteralValue, Statement},
    tokenizer::{Location, NumberType, TokenType},
    typechecking::typed_resolve_import,
};

use super::{
    expression::{OffsetValue, TypecheckedExpression, TypedLiteral},
    types::{FunctionType, Type, TypeSuggestion},
    TypecheckingContext, TypecheckingError,
};

pub type ScopeValueId = usize;

pub struct ScopeTypeMetadata {
    stack_allocated: bool,
}

pub struct Scopes {
    entries: Vec<HashMap<GlobalStr, ScopeValueId>>,
    values: Vec<(Type, ScopeTypeMetadata)>,
}

impl Scopes {
    pub fn new() -> Self {
        Self {
            entries: vec![HashMap::new()],
            values: Vec::new(),
        }
    }

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
        if self.entries.len() < 1 {
            self.push_scope();
        }
        let idx = self.entries.len() - 1;
        self.entries[idx].insert(key, value)
    }

    pub fn push(&mut self, value: Type) -> ScopeValueId {
        if !value.is_sized() {
            panic!("unsized type: {value:?}");
        }
        if self.entries.len() < 1 {
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
        &mut Scopes::new(),
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

    let mut scope = Scopes::new();

    let (return_type, args) = if is_external {
        let contract = &context.external_functions.read()[function_id].0;
        (contract.return_type.clone(), contract.arguments.clone())
    } else {
        let contract = &context.functions.read()[function_id].0;
        (contract.return_type.clone(), contract.arguments.clone())
    };
    for arg in args {
        scope.insert_value(arg.0, arg.1);
    }

    let mut exprs = vec![];
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
                if let Err(e) = typecheck_statement(
                    context,
                    &mut scope,
                    &Statement::Return(None, statement.loc().clone()),
                    module_id,
                    &return_type,
                    &mut exprs,
                ) {
                    return Err(e);
                }
            }
            context.functions.write()[function_id].1 = exprs.into_boxed_slice();
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
        } => todo!("trait generics"),
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
            let mut last_always_returns = false;
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
                    Ok(statement_always_returns) => last_always_returns = statement_always_returns,
                    Err(e) => {
                        last_always_returns = false;
                        errs.extend(e);
                    }
                }
            }

            scope.pop_scope();

            if errs.len() > 0 {
                Err(errs)
            } else {
                exprs.push(TypecheckedExpression::Block(
                    location.clone(),
                    block_exprs.into_boxed_slice(),
                    annotations.clone(),
                ));
                Ok(last_always_returns)
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
        .map(|(typ, expr)| matches!(typ, Type::PrimitiveNever)),
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
            LiteralValue::Array(vec) if vec.len() == 0 => {
                let typ = match type_suggestion {
                    TypeSuggestion::UnsizedArray(_) | TypeSuggestion::Array(_) => type_suggestion
                        .to_type(context)
                        .ok_or_else(|| TypecheckingError::CannotInferArrayType(location.clone()))?,
                    _ => return Err(TypecheckingError::CannotInferArrayType(location.clone())),
                };
                Ok((typ.clone(), TypedLiteral::Array(typ, Vec::new())))
            }
            LiteralValue::Array(vec) => {
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
                let typ = Type::SizedArray {
                    typ: Box::new(typ),
                    num_references: 0,
                    number_elements: vec.len(),
                };
                Ok((typ.clone(), TypedLiteral::Array(typ, elements)))
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
                    if structure.elements.iter().find(|v| v.0 == *k).is_none() {
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
                if path.entries.len() == 1 && path.entries[0].1.len() == 0 {
                    if let Some(((typ, _), id)) = scope.get(&path.entries[0].0) {
                        return Ok((typ.clone(), TypedLiteral::Dynamic(id)));
                    }
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
                        let function_typ = FunctionType {
                            return_type: reader.0.return_type.clone(),
                            arguments: reader.0.arguments.iter().map(|v| v.1.clone()).collect(),
                        };
                        Ok((
                            Type::Function(Arc::new(function_typ), 0),
                            TypedLiteral::Function(id),
                        ))
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
                    ModuleScopeValue::Static(id) => Ok((
                        context.statics.read()[id].0.clone(),
                        TypedLiteral::Static(id),
                    )),
                    _ => {
                        return Err(TypecheckingError::CannotFindValue(
                            location.clone(),
                            path.clone(),
                        ))
                    }
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
                    TypedLiteral::Function(*fn_id),
                ))
            }
            LiteralValue::AnonymousFunction(function_contract, statement) => {
                unreachable!("unbaked function")
            }
            LiteralValue::Void => Ok((Type::PrimitiveVoid(0), TypedLiteral::Void)),
        },
        Expression::Unary {
            operator,
            right_side,
        } if operator.typ == TokenType::Ampersand => typecheck_take_ref(
            context,
            module,
            scope,
            right_side,
            exprs,
            expression.loc(),
            type_suggestion,
        ),
        Expression::Unary {
            operator,
            right_side,
        } => {
            let (typ, right_side) =
                typecheck_expression(context, module, scope, right_side, exprs, type_suggestion)?;
            match operator.typ {
                TokenType::Plus if typ.is_int_like() => {
                    tc_res!(unary scope, exprs; Pos(operator.location.clone(), right_side, typ))
                }
                TokenType::Plus => {
                    Err(TypecheckingError::CannotPos(operator.location.clone(), typ))
                }
                TokenType::Minus if typ.is_int_like() && !typ.is_unsigned() => {
                    tc_res!(unary scope, exprs; Neg(operator.location.clone(), right_side, typ))
                }
                TokenType::Minus => {
                    Err(TypecheckingError::CannotNeg(operator.location.clone(), typ))
                }
                TokenType::LogicalNot if matches!(typ, Type::PrimitiveBool(0)) => {
                    tc_res!(unary scope, exprs; LNot(operator.location.clone(), right_side, typ))
                }
                TokenType::LogicalNot => Err(TypecheckingError::CannotLNot(
                    operator.location.clone(),
                    typ,
                )),
                TokenType::BitwiseNot
                    if typ.is_int_like() || matches!(typ, Type::PrimitiveBool(0)) =>
                {
                    tc_res!(unary scope, exprs; BNot(operator.location.clone(), right_side, typ))
                }
                TokenType::BitwiseNot => Err(TypecheckingError::CannotBNot(
                    operator.location.clone(),
                    typ,
                )),
                TokenType::Ampersand => unreachable!(),
                TokenType::Asterix => match typ.deref() {
                    Ok(typ) => {
                        tc_res!(unary scope, exprs; Dereference(operator.location.clone(), right_side, typ))
                    }
                    Err(typ) => Err(TypecheckingError::CannotDeref(
                        operator.location.clone(),
                        typ,
                    )),
                },
                v => unreachable!("invalid unary operator {v:?}"),
            }
        }
        Expression::Binary {
            operator,
            right_side,
            left_side,
        } => {
            if matches!(operator.typ, TokenType::LShift | TokenType::RShift) {
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
                        operator.location.clone(),
                        typ_right,
                    ));
                }

                return match operator.typ {
                    TokenType::LShift if typ.is_int_like() => {
                        tc_res!(binary scope, exprs; LShift(operator.location.clone(), left_side, right_side, typ))
                    }
                    TokenType::RShift if typ.is_int_like() => {
                        tc_res!(binary scope, exprs; RShift(operator.location.clone(), left_side, right_side, typ))
                    }

                    TokenType::LShift => {
                        Err(TypecheckingError::CannotShl(operator.location.clone(), typ))
                    }
                    TokenType::RShift => {
                        Err(TypecheckingError::CannotShr(operator.location.clone(), typ))
                    }
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
                    operator.location.clone(),
                    typ_left,
                    typ_right,
                ));
            }
            let loc = operator.location.clone();
            let typ = typ_left;
            match operator.typ {
                TokenType::Plus if typ.is_int_like() => {
                    tc_res!(binary scope, exprs; Add(operator.location.clone(), left_side, right_side, typ))
                }
                TokenType::Minus if typ.is_int_like() => {
                    tc_res!(binary scope, exprs; Sub(operator.location.clone(), left_side, right_side, typ))
                }
                TokenType::Asterix if typ.is_int_like() => {
                    tc_res!(binary scope, exprs; Mul(operator.location.clone(), left_side, right_side, typ))
                }
                TokenType::Divide if typ.is_int_like() => {
                    tc_res!(binary scope, exprs; Div(operator.location.clone(), left_side, right_side, typ))
                }
                TokenType::Modulo if typ.is_int_like() => {
                    tc_res!(binary scope, exprs; Mod(operator.location.clone(), left_side, right_side, typ))
                }
                TokenType::Ampersand if typ.is_int_like() || typ.is_bool() => {
                    tc_res!(binary scope, exprs; BAnd(operator.location.clone(), left_side, right_side, typ))
                }
                TokenType::BitwiseOr if typ.is_int_like() || typ.is_bool() => {
                    tc_res!(binary scope, exprs; BOr(operator.location.clone(), left_side, right_side, typ))
                }
                TokenType::BitwiseXor if typ.is_int_like() || typ.is_bool() => {
                    tc_res!(binary scope, exprs; BXor(operator.location.clone(), left_side, right_side, typ))
                }
                TokenType::LogicalOr if typ.is_bool() => {
                    tc_res!(binary scope, exprs; LOr(operator.location.clone(), left_side, right_side, typ))
                }
                TokenType::LogicalAnd if typ.is_bool() => {
                    tc_res!(binary scope, exprs; LAnd(operator.location.clone(), left_side, right_side, typ))
                }
                TokenType::GreaterThan if typ.is_int_like() => {
                    tc_res!(binary scope, exprs; GreaterThan(operator.location.clone(), left_side, right_side, Type::PrimitiveBool(0)))
                }
                TokenType::GreaterThanEquals if typ.is_int_like() => {
                    tc_res!(binary scope, exprs; GreaterThanEq(operator.location.clone(), left_side, right_side, Type::PrimitiveBool(0)))
                }
                TokenType::LessThan if typ.is_int_like() => {
                    tc_res!(binary scope, exprs; LessThan(operator.location.clone(), left_side, right_side, Type::PrimitiveBool(0)))
                }
                TokenType::LessThanEquals if typ.is_int_like() => {
                    tc_res!(binary scope, exprs; LessThanEq(operator.location.clone(), left_side, right_side, Type::PrimitiveBool(0)))
                }
                TokenType::EqualEqual if typ.is_int_like() || typ.is_bool() => {
                    tc_res!(binary scope, exprs; Eq(operator.location.clone(), left_side, right_side, Type::PrimitiveBool(0)))
                }
                TokenType::NotEquals if typ.is_int_like() || typ.is_bool() => {
                    tc_res!(binary scope, exprs; Neq(operator.location.clone(), left_side, right_side, Type::PrimitiveBool(0)))
                }

                TokenType::Plus => Err(TypecheckingError::CannotAdd(loc, typ)),
                TokenType::Minus => Err(TypecheckingError::CannotSub(loc, typ)),
                TokenType::Asterix => Err(TypecheckingError::CannotMul(loc, typ)),
                TokenType::Divide => Err(TypecheckingError::CannotDiv(loc, typ)),
                TokenType::Modulo => Err(TypecheckingError::CannotMod(loc, typ)),
                TokenType::Ampersand => Err(TypecheckingError::CannotBAnd(loc, typ)),
                TokenType::BitwiseOr => Err(TypecheckingError::CannotBOr(loc, typ)),
                TokenType::BitwiseXor => Err(TypecheckingError::CannotBXor(loc, typ)),
                TokenType::LogicalOr => Err(TypecheckingError::CannotLOr(loc, typ)),
                TokenType::LogicalAnd => Err(TypecheckingError::CannotLAnd(loc, typ)),
                TokenType::GreaterThan
                | TokenType::GreaterThanEquals
                | TokenType::LessThan
                | TokenType::LessThanEquals => Err(TypecheckingError::CannotCompare(loc, typ)),
                TokenType::EqualEqual | TokenType::NotEquals => {
                    Err(TypecheckingError::CannotEq(loc, typ))
                }
                v => unreachable!("invalid unary operator {v:?}"),
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
                let (typ, expr) = typecheck_expression(
                    context,
                    module,
                    scope,
                    &arguments[i],
                    exprs,
                    TypeSuggestion::from_type(&function_type.arguments[i]),
                )?;
                if typ != function_type.arguments[i] {
                    return Err(TypecheckingError::MismatchingType {
                        expected: function_type.arguments[i].clone(),
                        found: typ,
                        location: arguments[i].loc().clone(),
                    });
                }
                typed_arguments.push(expr);
            }

            if let TypedLiteral::Intrinsic(intrinsic) = function_expr {
                tc_res!(binary scope, exprs; IntrinsicCall(identifier.loc().clone(), intrinsic, typed_arguments, function_type.return_type.clone()))
            } else if let TypedLiteral::Function(fn_id) = function_expr {
                tc_res!(binary scope, exprs; DirectCall(identifier.loc().clone(), fn_id, typed_arguments, function_type.return_type.clone()))
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
                    operator,
                    right_side,
                } if operator.typ == TokenType::Asterix => {
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
                        left_side,
                        exprs,
                        TypeSuggestion::Unknown,
                    )?;
                    let dst_typed_lit =
                        make_reference(scope, exprs, typ.clone(), lhs, left_side.loc().clone());
                    (typ, dst_typed_lit)
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
            inclusive,
            loc,
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
            let new_type = context.resolve_type(module, new_type, &[])?;
            let (typ_lhs, lhs) = typecheck_expression(
                context,
                module,
                scope,
                left_side,
                exprs,
                TypeSuggestion::from_type(&new_type),
            )?;
            if typ_lhs.refcount() == 0 && new_type.refcount() == 0 {
                // TODO: ensure both types are primitives
            }

            // only allow casting between types with the same reference count and if either of them
            // are &{&}void.
            if typ_lhs.refcount() != new_type.refcount()
                || !(matches!(typ_lhs, Type::PrimitiveVoid(_))
                    || matches!(new_type, Type::PrimitiveVoid(_)))
            {
                return Err(TypecheckingError::DisallowedPointerCast(
                    loc.clone(),
                    typ_lhs,
                    new_type,
                ));
            } else {
                if let TypedLiteral::Dynamic(lhs_id) = lhs {
                    let id = scope.push(new_type.clone());
                    exprs.push(TypecheckedExpression::Alias(loc.clone(), id, lhs_id));
                    Ok((new_type, TypedLiteral::Dynamic(id)))
                } else {
                    Ok((new_type, lhs))
                }
            }
        }
    }
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
    let (mut typ_lhs, mut typed_literal_lhs) = typecheck_take_ref(
        context,
        module,
        scope,
        lhs,
        exprs,
        lhs.loc(),
        TypeSuggestion::Unknown,
    )?;
    let function_reader = context.functions.read();

    let Some(function_id) = (match typ_lhs {
        Type::Struct {
            struct_id,
            num_references,
            ..
        } => {
            let structure = &context.structs.read()[struct_id];
            structure.global_impl.get(ident).copied().or_else(|| {
                structure
                    .trait_impl
                    .iter()
                    .map(|v| {
                        v.1.iter().find(|v| {
                            if let Some(name) = &function_reader[**v].0.name {
                                name == ident
                            } else {
                                false
                            }
                        })
                    })
                    .filter_map(|v| v)
                    .next()
                    .copied()
            })
        }
        _ => todo!(),
    }) else {
        return Err(TypecheckingError::CannotFindFunctionOnType(
            lhs.loc().clone(),
            ident.clone(),
            typ_lhs,
        ));
    };

    let function = &function_reader[function_id];
    if !matches!(
        function.0.arguments.get(0),
        Some((_, Type::PrimitiveSelf(_)))
    ) {
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
            location: args[function.0.arguments.len() - 2].loc().clone(),
        });
    }
    for i in 0..function.0.arguments.len() - 1 {
        let (typ, expr) = typecheck_expression(
            context,
            module,
            scope,
            &args[i],
            exprs,
            TypeSuggestion::from_type(&function.0.arguments[i + 1].1),
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
    location: &Location,
    type_suggestion: TypeSuggestion,
) -> Result<(Type, TypedLiteral), TypecheckingError> {
    match expression {
        //&*_1 => _1
        Expression::Unary {
            operator,
            right_side,
        } if operator.typ == TokenType::Asterix => {
            typecheck_expression(context, module, scope, right_side, exprs, type_suggestion)
        }
        _ => {
            let (typ, expr) =
                ref_resolve_indexing(context, module, scope, expression, exprs, type_suggestion)?;
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
                                let mut new_val = scope.push(typ_lhs.clone().take_ref());
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
            let typ = match typ_lhs {
                Type::SizedArray { typ, .. } => *typ,
                Type::UnsizedArray { typ, .. } => *typ,
                _ => {
                    return Err(TypecheckingError::IndexNonArrayElem(
                        expression.loc().clone(),
                        typ_lhs,
                    ))
                }
            };

            let offset = indexing_resolve_rhs(context, module, scope, right_side, exprs)?;
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

            if typ.refcount() > 0 {
                return Ok((typ.deref().unwrap(), typed_literal));
            }

            match type_suggestion {
                TypeSuggestion::UnsizedArray(_) => match typ {
                    Type::SizedArray {
                        typ,
                        num_references: 0,
                        number_elements,
                    } => {
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
                        return Ok((typ, TypedLiteral::Dynamic(id)));
                    }
                    _ => (),
                },
                _ => (),
            }
            Ok((
                typ.clone(),
                make_reference(scope, exprs, typ, typed_literal, expression.loc().clone()),
            ))
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
            if !matches!(typ, Type::UnsizedArray { .. } | Type::SizedArray { .. }) {
                return Err(TypecheckingError::IndexNonArrayElem(
                    expression.loc().clone(),
                    typ,
                ));
            }
            if typ.refcount() == 0 {
                let new_typ = match typ.clone() {
                    Type::SizedArray { typ, .. } => *typ,
                    Type::UnsizedArray { typ, .. } => panic!("unsized element without references"),
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
                    offset_id,
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
