use std::{collections::HashMap, sync::Arc};

use crate::{
    module::{ModuleContext, ModuleScopeValue},
    parser::{ArrayLiteral, BinaryOp, Expression, LiteralValue, Path, Statement, UnaryOp},
    std_annotations::ext_vararg::ExternVarArg,
    store::StoreKey,
    tokenizer::NumberType,
    typechecking::typed_resolve_import,
};
use mira_spans::{interner::InternedStr, Span};

use super::{
    expression::{OffsetValue, TypecheckedExpression, TypedLiteral},
    intrinsics::IntrinsicAnnotation,
    types::{FunctionType, Type, TypeSuggestion},
    TypecheckedModule, TypecheckingContext, TypecheckingError, TypedExternalFunction,
    TypedFunction, TypedStatic, TypedTrait,
};

pub type ScopeValueId = usize;

pub struct ScopeTypeMetadata {
    pub stack_allocated: bool,
}

#[derive(Default)]
pub struct Scopes<'arena> {
    entries: Vec<HashMap<InternedStr<'arena>, ScopeValueId>>,
    values: Vec<(Type<'arena>, ScopeTypeMetadata)>,
}

impl<'arena> Scopes<'arena> {
    pub fn get(
        &self,
        key: InternedStr<'arena>,
    ) -> Option<(&(Type<'arena>, ScopeTypeMetadata), ScopeValueId)> {
        let len = self.entries.len();
        for i in 1..=len {
            if let Some(v) = self.entries[len - i].get(&key) {
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
        key: InternedStr<'arena>,
        value: Type<'arena>,
    ) -> (ScopeValueId, Option<ScopeValueId>) {
        let id = self.push(value);
        (id, self.insert(key, id))
    }

    pub fn insert(
        &mut self,
        key: InternedStr<'arena>,
        value: ScopeValueId,
    ) -> Option<ScopeValueId> {
        if self.entries.is_empty() {
            self.push_scope();
        }
        let idx = self.entries.len() - 1;
        self.entries[idx].insert(key, value)
    }

    pub fn push(&mut self, value: Type<'arena>) -> ScopeValueId {
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

pub fn typecheck_static<'arena>(
    context: &TypecheckingContext<'arena>,
    module_context: &ModuleContext<'arena>,
    static_id: StoreKey<TypedStatic<'arena>>,
    errs: &mut Vec<TypecheckingError<'arena>>,
) -> bool {
    let tc_module_reader = context.statics.read();
    let typ = &tc_module_reader[static_id].type_;
    let expr = {
        std::mem::replace(
            &mut module_context.statics.write()[static_id.cast()].1,
            LiteralValue::Void,
        )
    };

    match typecheck_expression(
        context,
        tc_module_reader[static_id].module,
        &mut Scopes::default(),
        &Expression::Literal(expr, tc_module_reader[static_id].loc),
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
                    location: tc_module_reader[static_id].loc,
                });
                return false;
            }
            if !expr.is_entirely_literal() {
                errs.push(TypecheckingError::StaticsNeedToBeLiteral(
                    tc_module_reader[static_id].loc,
                ));
            }
            drop(tc_module_reader);
            context.statics.write()[static_id].value = expr;
        }
    }
    true
}

pub fn typecheck_external_function<'arena>(
    context: &TypecheckingContext<'arena>,
    module_context: &ModuleContext<'arena>,
    function_id: StoreKey<TypedExternalFunction<'arena>>,
) -> Result<Vec<(Type<'arena>, ScopeTypeMetadata)>, Vec<TypecheckingError<'arena>>> {
    inner_typecheck_function(context, module_context, function_id.cast(), true)
}

pub fn typecheck_function<'arena>(
    context: &TypecheckingContext<'arena>,
    module_context: &ModuleContext<'arena>,
    function_id: StoreKey<TypedFunction<'arena>>,
) -> Result<Vec<(Type<'arena>, ScopeTypeMetadata)>, Vec<TypecheckingError<'arena>>> {
    inner_typecheck_function(context, module_context, function_id, false)
}

// NOTE: function_id has to be StoreKey<TypedExternalFunction> if is_external is set to true.
fn inner_typecheck_function<'arena>(
    context: &TypecheckingContext<'arena>,
    module_context: &ModuleContext<'arena>,
    function_id: StoreKey<TypedFunction<'arena>>,
    is_external: bool,
) -> Result<Vec<(Type<'arena>, ScopeTypeMetadata)>, Vec<TypecheckingError<'arena>>> {
    let ext_fn_reader = module_context.external_functions.read();
    let fn_reader = module_context.functions.read();
    let (statement, module_id) = if is_external {
        let (_, ref statement, module_id) = ext_fn_reader[function_id.cast()];
        if let Some(statement) = statement {
            (statement, module_id)
        } else {
            return Ok(Vec::new());
        }
    } else {
        let (_, ref statement, module_id) = fn_reader[function_id.cast()];
        (statement, module_id)
    };

    let mut scope = Scopes::default();

    let (return_type, args, loc) = if is_external {
        let contract = &context.external_functions.read()[function_id.cast()].0;
        (
            contract.return_type.clone(),
            contract.arguments.clone(),
            contract.span,
        )
    } else {
        let reader = context.functions.read();
        let contract = &reader[function_id].0;
        if contract.annotations.has_annotation::<IntrinsicAnnotation>() {
            let loc = contract.span;
            drop(reader);
            context.functions.write()[function_id].1 =
                Box::new([TypecheckedExpression::Unreachable(loc)]);
            return Ok(Vec::new());
        }
        (
            contract.return_type.clone(),
            contract.arguments.clone(),
            contract.span,
        )
    };

    let mut errs = vec![];
    if is_external
        && (!return_type.is_primitive()
            || (return_type.refcount() > 0 && !return_type.is_thin_ptr()))
    {
        errs.push(TypecheckingError::InvalidExternReturnType(loc));
    }
    if !return_type.is_sized() {
        errs.push(TypecheckingError::UnsizedReturnType(
            loc,
            return_type.clone(),
        ));
    }
    for (_, arg) in args.iter().filter(|v| !v.1.is_sized()) {
        errs.push(TypecheckingError::UnsizedArgument(loc, arg.clone()));
    }
    (errs.is_empty()).then_some(()).ok_or(errs)?;

    let mut exprs = vec![];
    for arg in args {
        let (id, _) = scope.insert_value(arg.0, arg.1);
        scope.make_stack_allocated(id);
    }

    let always_returns = typecheck_statement(
        context,
        &mut scope,
        statement,
        module_id.cast(),
        &return_type,
        &mut exprs,
    )?;
    if !matches!(return_type, Type::PrimitiveVoid(0)) && !always_returns {
        return Err(vec![TypecheckingError::BodyDoesNotAlwaysReturn {
            location: statement.span(),
        }]);
    }
    if !always_returns {
        typecheck_statement(
            context,
            &mut scope,
            &Statement::Return(None, statement.span()),
            module_id.cast(),
            &return_type,
            &mut exprs,
        )?;
    }
    if is_external {
        let mut exprs = Some(exprs.into_boxed_slice());
        std::mem::swap(
            &mut exprs,
            &mut context.external_functions.write()[function_id.cast()].1,
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
    Ok(scope.values)
}

/// Returns if the statement and if it always returns
fn typecheck_statement<'arena>(
    context: &TypecheckingContext<'arena>,
    scope: &mut Scopes<'arena>,
    statement: &Statement<'arena>,
    module: StoreKey<TypecheckedModule<'arena>>,
    return_type: &Type<'arena>,
    exprs: &mut Vec<TypecheckedExpression<'arena>>,
) -> Result<bool, Vec<TypecheckingError<'arena>>> {
    match statement {
        Statement::If {
            condition,
            if_stmt,
            else_stmt,
            span,
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
                    location: *span,
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
                span: *span,
                cond,
                if_block: (if_exprs.into_boxed_slice(), if_stmt.span()),
                else_block: else_stmt
                    .as_ref()
                    .map(move |stmt| (else_exprs.into_boxed_slice(), stmt.span())),
                annotations: annotations.clone(),
            });
            Ok(if_stmt_exits && else_stmt_exits)
        }
        Statement::While {
            condition,
            child,
            span,
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
                    location: *span,
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
                span: *span,
                cond_block: condition_block.into_boxed_slice(),
                cond,
                body: (body.into_boxed_slice(), child.span()),
            });

            Ok(always_exits)
        }
        Statement::For { .. } => todo!("iterator (requires generics)"),
        Statement::Return(None, span) => {
            if matches!(return_type, Type::PrimitiveVoid(0)) {
                exprs.push(TypecheckedExpression::Return(*span, TypedLiteral::Void));
                Ok(true)
            } else {
                Err(vec![TypecheckingError::MismatchingType {
                    expected: return_type.clone(),
                    found: Type::PrimitiveVoid(0),
                    location: *span,
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
                    location: expression.span(),
                }]);
            }
            exprs.push(TypecheckedExpression::Return(*location, typed_expression));
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
                            block_exprs.push(TypecheckedExpression::Unreachable(statement.span()));
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
                    *location,
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
                        location: *location,
                    }]);
                }
            }

            let id = match expr {
                TypedLiteral::Dynamic(id) => id,
                _ => {
                    let id = scope.push(typ.clone());
                    exprs.push(TypecheckedExpression::Literal(expression.span(), id, expr));
                    id
                }
            };
            scope.insert(*name, id);
            exprs.push(TypecheckedExpression::DeclareVariable(
                *location, id, typ, *name,
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

fn signed_number_to_literal<'arena>(
    v: i64,
    number_type: NumberType,
    expected: TypeSuggestion<'arena>,
) -> (Type<'arena>, TypedLiteral<'arena>) {
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

fn unsigned_number_to_literal<'arena>(
    v: u64,
    number_type: NumberType,
    expected: TypeSuggestion<'arena>,
) -> (Type<'arena>, TypedLiteral<'arena>) {
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

fn float_number_to_literal<'arena>(
    v: f64,
    number_type: NumberType,
    expected: TypeSuggestion<'arena>,
) -> (Type<'arena>, TypedLiteral<'arena>) {
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

#[allow(clippy::result_large_err)]
fn typecheck_expression<'arena>(
    context: &TypecheckingContext<'arena>,
    module: StoreKey<TypecheckedModule<'arena>>,
    scope: &mut Scopes<'arena>,
    expression: &Expression<'arena>,
    exprs: &mut Vec<TypecheckedExpression<'arena>>,
    type_suggestion: TypeSuggestion<'arena>,
) -> Result<(Type<'arena>, TypedLiteral<'arena>), TypecheckingError<'arena>> {
    match expression {
        Expression::Literal(literal_value, location) => match literal_value {
            LiteralValue::String(global_str) => {
                Ok((Type::PrimitiveStr(1), TypedLiteral::String(*global_str)))
            }
            LiteralValue::Array(ArrayLiteral::Values(vec)) if vec.is_empty() => {
                let typ = match type_suggestion {
                    TypeSuggestion::UnsizedArray(_) | TypeSuggestion::Array(_) => type_suggestion
                        .to_type(context)
                        .ok_or(TypecheckingError::CannotInferArrayType(*location))?,
                    _ => return Err(TypecheckingError::CannotInferArrayType(*location)),
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
                            location: expr.span(),
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
                    return Err(TypecheckingError::CannotInferAnonStructType(*location));
                };

                let structure = &context.structs.read()[struct_id];
                // ensure there are no excessive values in the struct initialization
                for k in values.keys() {
                    if !structure.elements.iter().any(|v| v.0 == *k) {
                        return Err(TypecheckingError::NoSuchFieldFound {
                            location: values[k].0,
                            name: *k,
                        });
                    }
                }

                let mut elements = Vec::with_capacity(structure.elements.len());
                for (key, typ) in structure.elements.iter() {
                    let Some((loc, expr)) = values.get(key) else {
                        return Err(TypecheckingError::MissingField {
                            location: *location,
                            name: *key,
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
                            location: *loc,
                        });
                    }
                    elements.push(expr_lit);
                }
                Ok((
                    Type::Struct {
                        struct_id,
                        name: structure.name,
                        num_references: 0,
                    },
                    TypedLiteral::Struct(struct_id, elements),
                ))
            }
            LiteralValue::Struct(values, path) => {
                let value = typed_resolve_import(
                    context,
                    module,
                    &path.entries.iter().map(|v| v.0).collect::<Vec<_>>(),
                    location,
                    &mut Vec::new(),
                )
                .map_err(|_| TypecheckingError::CannotFindValue(*location, path.clone()))?;
                let ModuleScopeValue::Struct(struct_id) = value else {
                    return Err(TypecheckingError::CannotFindValue(*location, path.clone()));
                };
                let structure = &context.structs.read()[struct_id.cast()];
                // ensure there are no excessive values in the struct initialization
                for k in values.keys() {
                    if !structure.elements.iter().any(|v| v.0 == *k) {
                        return Err(TypecheckingError::NoSuchFieldFound {
                            location: values[k].0,
                            name: *k,
                        });
                    }
                }

                let mut elements = Vec::with_capacity(structure.elements.len());
                for (key, typ) in structure.elements.iter() {
                    let Some((loc, expr)) = values.get(key) else {
                        return Err(TypecheckingError::MissingField {
                            location: *location,
                            name: *key,
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
                            location: *loc,
                        });
                    }
                    elements.push(expr_lit);
                }
                Ok((
                    Type::Struct {
                        struct_id: struct_id.cast(),
                        name: structure.name,
                        num_references: 0,
                    },
                    TypedLiteral::Struct(struct_id.cast(), elements),
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
                if path.entries.len() == 1 && path.entries[0].2.is_empty() {
                    if let Some(((typ, _), id)) = scope.get(path.entries[0].0) {
                        return Ok((typ.clone(), TypedLiteral::Dynamic(id)));
                    }
                }
                let mut generics = Vec::new();
                for (.., generic_value) in path.entries.iter() {
                    generics.extend_from_slice(generic_value);
                }

                let value = typed_resolve_import(
                    context,
                    module,
                    &path.entries.iter().map(|v| v.0).collect::<Vec<_>>(),
                    location,
                    &mut Vec::new(),
                )
                .map_err(|_| TypecheckingError::CannotFindValue(*location, path.clone()))?;
                match value {
                    ModuleScopeValue::Function(id) => {
                        let reader = &context.functions.read()[id.cast()];
                        let mut function_typ = FunctionType {
                            return_type: reader.0.return_type.clone(),
                            arguments: reader.0.arguments.iter().map(|v| v.1.clone()).collect(),
                        };
                        if reader.0.generics.len() != generics.len() {
                            return Err(TypecheckingError::MismatchingGenericCount(
                                *location,
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
                                    *location, ty,
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
                            None => TypedLiteral::Function(id.cast(), generic_types),
                        };
                        Ok((Type::Function(Arc::new(function_typ), 0), literal))
                    }
                    ModuleScopeValue::ExternalFunction(id) => {
                        let reader = &context.external_functions.read()[id.cast()];
                        let function_typ = FunctionType {
                            return_type: reader.0.return_type.clone(),
                            arguments: reader.0.arguments.iter().map(|v| v.1.clone()).collect(),
                        };
                        Ok((
                            Type::Function(Arc::new(function_typ), 0),
                            TypedLiteral::ExternalFunction(id.cast()),
                        ))
                    }
                    ModuleScopeValue::Static(id) => {
                        if !generics.is_empty() {
                            return Err(TypecheckingError::UnexpectedGenerics {
                                location: *location,
                            });
                        }
                        Ok((
                            context.statics.read()[id.cast()].type_.clone(),
                            TypedLiteral::Static(id.cast()),
                        ))
                    }
                    _ => Err(TypecheckingError::CannotFindValue(*location, path.clone())),
                }
            }
            LiteralValue::BakedAnonymousFunction(fn_id) => {
                let func = &context.functions.read()[fn_id.cast()].0;
                let fn_typ = FunctionType {
                    return_type: func.return_type.clone(),
                    arguments: func.arguments.iter().map(|(_, v)| v.clone()).collect(),
                };
                Ok((
                    Type::Function(Arc::new(fn_typ), 0),
                    TypedLiteral::Function(fn_id.cast(), vec![]),
                ))
            }
            LiteralValue::AnonymousFunction(..) => unreachable!("unbaked function"),
            LiteralValue::Void => Ok((Type::PrimitiveVoid(0), TypedLiteral::Void)),
        },
        Expression::Asm {
            span: loc,
            asm,
            volatile,
            output,
            registers,
            inputs,
        } => {
            let name = match output {
                crate::parser::TypeRef::Reference { type_name, .. } => Some(type_name.entries[0].0),
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
                        *loc,
                        name.unwrap(), /* see comment above as to why this is fine */
                    )
                })?;
            let mut typed_inputs = Vec::with_capacity(inputs.len());
            for (span, name) in inputs {
                let Some(((entry_ty, _), id)) = scope.get(*name) else {
                    return Err(TypecheckingError::CannotFindValue(
                        *span,
                        Path::new(*name, *span, Vec::new()),
                    ));
                };
                if !entry_ty.is_asm_primitive() {
                    return Err(TypecheckingError::AsmNonNumericTypeResolved(
                        *span,
                        entry_ty.clone(),
                    ));
                }
                typed_inputs.push(id);
            }
            let id = scope.push(output.clone());
            exprs.push(TypecheckedExpression::Asm {
                span: *loc,
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
            span: loc,
        } => {
            let (typ, right_side) =
                typecheck_expression(context, module, scope, right_side, exprs, type_suggestion)?;
            match operator {
                UnaryOp::Plus if typ.is_int_like() => {
                    tc_res!(unary scope, exprs; Pos(*loc, right_side, typ))
                }
                UnaryOp::Plus => Err(TypecheckingError::CannotPos(*loc, typ)),
                UnaryOp::Minus if (typ.is_int_like() && !typ.is_unsigned()) || typ.is_float() => {
                    tc_res!(unary scope, exprs; Neg(*loc, right_side, typ))
                }
                UnaryOp::Minus => Err(TypecheckingError::CannotNeg(*loc, typ)),
                UnaryOp::LogicalNot if matches!(typ, Type::PrimitiveBool(0)) => {
                    tc_res!(unary scope, exprs; LNot(*loc, right_side, typ))
                }
                UnaryOp::LogicalNot => Err(TypecheckingError::CannotLNot(*loc, typ)),
                UnaryOp::BitwiseNot
                    if typ.is_int_like() || matches!(typ, Type::PrimitiveBool(0)) =>
                {
                    tc_res!(unary scope, exprs; BNot(*loc, right_side, typ))
                }
                UnaryOp::BitwiseNot => Err(TypecheckingError::CannotBNot(*loc, typ)),
                UnaryOp::Dereference => match typ.deref() {
                    Ok(typ) => {
                        tc_res!(unary scope, exprs; Dereference(*loc, right_side, typ))
                    }
                    Err(typ) => Err(TypecheckingError::CannotDeref(*loc, typ)),
                },
                UnaryOp::Reference => unreachable!(),
            }
        }
        Expression::Binary {
            operator,
            right_side,
            left_side,
            span,
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
                    return Err(TypecheckingError::CannotShiftByNonUInt(*span, typ_right));
                }

                return match operator {
                    BinaryOp::LShift if typ.is_int_like() => {
                        tc_res!(binary scope, exprs; LShift(*span, left_side, right_side, typ))
                    }
                    BinaryOp::RShift if typ.is_int_like() => {
                        tc_res!(binary scope, exprs; RShift(*span, left_side, right_side, typ))
                    }

                    BinaryOp::LShift => Err(TypecheckingError::CannotShl(*span, typ)),
                    BinaryOp::RShift => Err(TypecheckingError::CannotShr(*span, typ)),
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
                return Err(TypecheckingError::LhsNotRhs(*span, typ_left, typ_right));
            }
            let typ = typ_left;
            let loc = *span;
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
            ..
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
                    .has_annotation::<ExternVarArg>()
            } else {
                false
            };
            let Type::Function(function_type, _) = typ else {
                return Err(TypecheckingError::TypeIsNotAFunction {
                    location: identifier.span(),
                });
            };
            let mut typed_arguments = Vec::with_capacity(function_type.arguments.len());
            if arguments.len() < function_type.arguments.len() {
                return Err(TypecheckingError::MissingArguments {
                    location: identifier.span(),
                });
            }
            if arguments.len() > function_type.arguments.len() && !has_vararg {
                return Err(TypecheckingError::TooManyArguments {
                    location: arguments[function_type.arguments.len().saturating_sub(1)].span(),
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
                        location: arguments[i].span(),
                    });
                }
                typed_arguments.push(expr);
            }

            if let TypedLiteral::Intrinsic(intrinsic, generics) = function_expr {
                let typ = function_type.return_type.clone();
                let id = scope.push(typ.clone());
                exprs.push(TypecheckedExpression::IntrinsicCall(
                    identifier.span(),
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
                    identifier.span(),
                    id,
                    fn_id,
                    typed_arguments,
                    generics,
                ));
                Ok((typ, TypedLiteral::Dynamic(id)))
            } else if let TypedLiteral::ExternalFunction(fn_id) = function_expr {
                tc_res!(binary scope, exprs; DirectExternCall(identifier.span(), fn_id, typed_arguments, function_type.return_type.clone()))
            } else {
                tc_res!(binary scope, exprs; Call(identifier.span(), function_expr, typed_arguments, function_type.return_type.clone()))
            }
        }
        Expression::Indexing { .. } | Expression::MemberAccess { .. } => {
            copy_resolve_indexing(context, module, scope, expression, exprs, type_suggestion)
        }
        Expression::MemberCall {
            identifier,
            lhs,
            arguments,
            ..
        } => typecheck_membercall(context, module, scope, exprs, lhs, identifier, arguments),
        Expression::Assignment {
            left_side,
            right_side,
            span,
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
                    let typ = typ
                        .deref()
                        .map_err(|typ| TypecheckingError::CannotDeref(right_side.span(), typ))?;

                    (typ, lhs)
                }
                _ => {
                    let (typ, lhs) = typecheck_expression(
                        context,
                        module,
                        scope,
                        &Expression::Unary {
                            operator: UnaryOp::Reference,
                            span: *span,
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
                    location: *span,
                });
            }
            exprs.push(TypecheckedExpression::StoreAssignment(*span, lhs, rhs));
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
                    location: right_side.span(),
                });
            }

            unimplemented!("lang-items");
        }
        Expression::TypeCast {
            left_side,
            new_type,
            span,
        } => {
            let (typ, lhs) =
                typecheck_expression(context, module, scope, left_side, exprs, type_suggestion)?;
            let new_type = context.resolve_type(module, new_type, &[])?;
            typecheck_cast(scope, exprs, typ, new_type, lhs, *span, context)
        }
    }
}

#[allow(clippy::result_large_err)]
fn typecheck_cast<'arena>(
    scope: &mut Scopes<'arena>,
    exprs: &mut Vec<TypecheckedExpression<'arena>>,
    typ: Type<'arena>,
    new_typ: Type<'arena>,
    lhs: TypedLiteral<'arena>,
    loc: Span<'arena>,
    context: &TypecheckingContext<'arena>,
) -> Result<(Type<'arena>, TypedLiteral<'arena>), TypecheckingError<'arena>> {
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
                    traits.iter().map(|v| trait_reader[*v].name).collect(),
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

#[allow(clippy::too_many_arguments, clippy::result_large_err)]
fn typecheck_dyn_membercall<'arena>(
    context: &TypecheckingContext<'arena>,
    scope: &mut Scopes<'arena>,
    module: StoreKey<TypecheckedModule<'arena>>,
    exprs: &mut Vec<TypecheckedExpression<'arena>>,
    ident: &InternedStr<'arena>,
    args: &[Expression<'arena>],
    lhs: TypedLiteral<'arena>,
    lhs_loc: Span<'arena>,
    trait_refs: Vec<(StoreKey<TypedTrait<'arena>>, InternedStr<'arena>)>,
    num_references: u8,
) -> Result<(Type<'arena>, TypedLiteral<'arena>), TypecheckingError<'arena>> {
    let trait_reader = context.traits.read();
    let mut offset = 0;
    let (mut arg_typs, return_ty, trait_name) = 'out: {
        for trait_id in trait_refs.iter().map(|v| v.0) {
            for func in trait_reader[trait_id].functions.iter() {
                if func.0 == *ident {
                    break 'out (func.1.clone(), func.2.clone(), trait_reader[trait_id].name);
                }
                offset += 1;
            }
        }

        return Err(TypecheckingError::CannotFindFunctionOnType(
            lhs_loc,
            *ident,
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
                lhs_loc, *ident, trait_name,
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
            lhs_loc, *ident, trait_name,
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
        exprs.push(TypecheckedExpression::Dereference(lhs_loc, id, lhs));
        lhs = TypedLiteral::Dynamic(id);
    }

    let mut typed_args = Vec::with_capacity(args.len() + 1);
    typed_args.push(lhs);

    if args.len() < arg_typs.len() - 1 {
        return Err(TypecheckingError::MissingArguments { location: lhs_loc });
    }
    if args.len() > arg_typs.len() - 1 {
        return Err(TypecheckingError::TooManyArguments {
            location: args[arg_typs.len() - 1].span(),
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
                location: args[i].span(),
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

#[allow(clippy::result_large_err)]
fn typecheck_membercall<'arena>(
    context: &TypecheckingContext<'arena>,
    module: StoreKey<TypecheckedModule<'arena>>,
    scope: &mut Scopes<'arena>,
    exprs: &mut Vec<TypecheckedExpression<'arena>>,
    lhs: &Expression<'arena>,
    ident: &InternedStr<'arena>,
    args: &[Expression<'arena>],
) -> Result<(Type<'arena>, TypedLiteral<'arena>), TypecheckingError<'arena>> {
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
                lhs.span(),
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
    let structure = struct_id.map(|v| &struct_reader[v.cast()]);
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
            lhs.span(),
            *ident,
            typ_lhs,
        ));
    };

    let function: &(_, _) = &function_reader[function_id];
    if function
        .0
        .arguments
        .first()
        .map(|v| v.1.clone().without_ref())
        != Some(typ_lhs.clone().without_ref())
    {
        return Err(TypecheckingError::NonMemberFunction(
            function.0.span,
            *ident,
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
                lhs.span(),
                new_id,
                typed_literal_lhs,
            ));
            typed_literal_lhs = TypedLiteral::Dynamic(new_id);
        } else {
            typed_literal_lhs =
                make_reference(scope, exprs, typ_lhs.clone(), typed_literal_lhs, lhs.span());
            typ_lhs = typ_lhs.take_ref();
        }
    }

    let mut typed_arguments = Vec::with_capacity(function.0.arguments.len() + 1);
    typed_arguments.push(typed_literal_lhs);
    if args.len() < function.0.arguments.len() - 1 {
        return Err(TypecheckingError::MissingArguments {
            location: lhs.span(),
        });
    }
    if args.len() > function.0.arguments.len() - 1 {
        return Err(TypecheckingError::TooManyArguments {
            location: args[function.0.arguments.len() - 1].span(),
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
                location: args[i].span(),
            });
        }
        typed_arguments.push(expr);
    }

    let call_id = scope.push(function.0.return_type.clone());
    exprs.push(TypecheckedExpression::DirectCall(
        lhs.span(),
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

#[allow(clippy::result_large_err)]
fn typecheck_take_ref<'arena>(
    context: &TypecheckingContext<'arena>,
    module: StoreKey<TypecheckedModule<'arena>>,
    scope: &mut Scopes<'arena>,
    expression: &Expression<'arena>,
    exprs: &mut Vec<TypecheckedExpression<'arena>>,
    type_suggestion: TypeSuggestion<'arena>,
) -> Result<(Type<'arena>, TypedLiteral<'arena>), TypecheckingError<'arena>> {
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
#[allow(clippy::result_large_err)]
fn ref_resolve_indexing<'arena>(
    context: &TypecheckingContext<'arena>,
    module: StoreKey<TypecheckedModule<'arena>>,
    scope: &mut Scopes<'arena>,
    expression: &Expression<'arena>,
    exprs: &mut Vec<TypecheckedExpression<'arena>>,
    type_suggestion: TypeSuggestion<'arena>,
    increase_ref: bool,
) -> Result<(Type<'arena>, TypedLiteral<'arena>), TypecheckingError<'arena>> {
    match expression {
        Expression::MemberAccess {
            left_side,
            index,
            span,
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
                        expression.span(),
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
                                    *span,
                                    new_val,
                                    typed_literal_lhs,
                                    OffsetValue::Static(idx),
                                ));
                                typed_literal_lhs = TypedLiteral::Dynamic(new_val);
                            }
                            None => {
                                return Err(TypecheckingError::FieldNotFound(
                                    expression.span(),
                                    typ_lhs,
                                    *element_name,
                                ))
                            }
                        }
                    }
                    _ => return Err(TypecheckingError::AccessNonStructValue(*span, typ_lhs)),
                };
            }
            Ok((typ_lhs, typed_literal_lhs))
        }
        Expression::Indexing {
            left_side,
            right_side,
            ..
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
                    expression.span(),
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
                        return Err(TypecheckingError::TupleDynamicIndex(expression.span()))
                    }
                    OffsetValue::Static(idx) if idx >= elements.len() => {
                        return Err(TypecheckingError::TupleIndexOutOfBounds(
                            expression.span(),
                            elements.len(),
                            idx,
                        ))
                    }
                    OffsetValue::Static(idx) => elements[idx].clone(),
                },
                _ => {
                    return Err(TypecheckingError::IndexNonArrayElem(
                        expression.span(),
                        typ_lhs,
                    ))
                }
            };

            let id = scope.push(typ.clone().take_ref());
            exprs.push(TypecheckedExpression::Offset(
                expression.span(),
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
                        expression.span(),
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
                        expression.span(),
                    );
                    let typ = Type::UnsizedArray {
                        typ,
                        num_references: 0,
                    };
                    let id = scope.push(typ.clone().take_ref());
                    exprs.push(TypecheckedExpression::MakeUnsizedSlice(
                        expression.span(),
                        id,
                        typed_literal_sized_array_ref,
                        number_elements,
                    ));
                    Ok((typ, TypedLiteral::Dynamic(id)))
                }
                (_, typ) => Ok((
                    typ.clone(),
                    make_reference(scope, exprs, typ, typed_literal, expression.span()),
                )),
            }
        }
    }
}

#[allow(clippy::result_large_err)]
fn indexing_resolve_rhs<'arena>(
    context: &TypecheckingContext<'arena>,
    module: StoreKey<TypecheckedModule<'arena>>,
    scope: &mut Scopes<'arena>,
    expression: &Expression<'arena>,
    exprs: &mut Vec<TypecheckedExpression<'arena>>,
) -> Result<OffsetValue, TypecheckingError<'arena>> {
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
            location: expression.span(),
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
fn make_reference<'arena>(
    scope: &mut Scopes<'arena>,
    expressions: &mut Vec<TypecheckedExpression<'arena>>,
    typ: Type<'arena>,
    mut typed_literal: TypedLiteral<'arena>,
    loc: Span<'arena>,
) -> TypedLiteral<'arena> {
    match typed_literal {
        TypedLiteral::Void | TypedLiteral::Static(_) => {}
        TypedLiteral::Dynamic(v) => scope.make_stack_allocated(v),
        _ => {
            let lit_id = scope.push(typ.clone());
            scope.make_stack_allocated(lit_id);
            expressions.push(TypecheckedExpression::Literal(loc, lit_id, typed_literal));
            typed_literal = TypedLiteral::Dynamic(lit_id);
        }
    }

    let new_id = scope.push(typ.take_ref());
    expressions.push(TypecheckedExpression::Reference(loc, new_id, typed_literal));
    TypedLiteral::Dynamic(new_id)
}

#[allow(clippy::result_large_err)]
fn copy_resolve_indexing<'arena>(
    context: &TypecheckingContext<'arena>,
    module: StoreKey<TypecheckedModule<'arena>>,
    scope: &mut Scopes<'arena>,
    expression: &Expression<'arena>,
    exprs: &mut Vec<TypecheckedExpression<'arena>>,
    type_suggestion: TypeSuggestion<'arena>,
) -> Result<(Type<'arena>, TypedLiteral<'arena>), TypecheckingError<'arena>> {
    match expression {
        Expression::Indexing {
            left_side,
            right_side,
            ..
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
                return Err(TypecheckingError::IndexNonArrayElem(expression.span(), typ));
            }
            if typ.refcount() == 0 {
                let new_typ = match typ.clone() {
                    Type::SizedArray { typ, .. } => *typ,
                    Type::Tuple { elements, .. } => match offset {
                        OffsetValue::Dynamic(_) => {
                            return Err(TypecheckingError::TupleDynamicIndex(expression.span()))
                        }
                        OffsetValue::Static(v) if v >= elements.len() => {
                            return Err(TypecheckingError::TupleIndexOutOfBounds(
                                expression.span(),
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
                            expression.span(),
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
                            make_reference(scope, exprs, typ, lhs, expression.span());
                        let offset_id = scope.push(new_typ.clone().take_ref());
                        let new_id = scope.push(new_typ.clone());
                        exprs.push(TypecheckedExpression::Offset(
                            expression.span(),
                            offset_id,
                            typed_lit_ref,
                            off,
                        ));
                        exprs.push(TypecheckedExpression::Dereference(
                            expression.span(),
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
                        expression.span(),
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
                    expression.span(),
                    offset_id,
                    lhs,
                    offset,
                ));
                exprs.push(TypecheckedExpression::Dereference(
                    expression.span(),
                    value_id,
                    TypedLiteral::Dynamic(offset_id),
                ));
                Ok((typ, TypedLiteral::Dynamic(value_id)))
            }
        }
        Expression::MemberAccess {
            index,
            left_side,
            span,
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
                        expression.span(),
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
                                    expression.span(),
                                    typ_lhs.without_ref(),
                                    *element_name,
                                ))
                            }
                        }
                    }
                    _ => return Err(TypecheckingError::AccessNonStructValue(*span, typ_lhs)),
                };
                if !needs_deref {
                    let new_id = scope.push(typ_lhs.clone());
                    if let TypedLiteral::Dynamic(id) = typed_literal_lhs {
                        scope.make_stack_allocated(id);
                    }
                    exprs.push(TypecheckedExpression::OffsetNonPointer(
                        *span,
                        new_id,
                        typed_literal_lhs,
                        offset,
                    ));
                    typed_literal_lhs = TypedLiteral::Dynamic(new_id);
                } else {
                    let offset_id = scope.push(typ_lhs.clone().take_ref());
                    let deref_id = scope.push(typ_lhs.clone());
                    exprs.push(TypecheckedExpression::Offset(
                        *span,
                        offset_id,
                        typed_literal_lhs,
                        OffsetValue::Static(offset),
                    ));
                    exprs.push(TypecheckedExpression::Dereference(
                        *span,
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
