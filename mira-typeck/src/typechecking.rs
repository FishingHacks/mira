use std::collections::{HashMap, HashSet};

use crate::{
    context::TypeCtx,
    ir::{IR, ScopeEntry},
    typed_resolve_import,
    types::EMPTY_TYLIST,
};
use mira_common::store::{StoreKey, VecStore};
use mira_errors::{Diagnostic, Diagnostics};
use mira_lexer::NumberType;
use mira_parser::{
    ArrayLiteral, BinaryOp, Expression, LiteralValue, Path, Statement, UnaryOp,
    module::{ModuleContext, ModuleScopeValue},
    std_annotations::{
        ext_vararg::ExternVarArg, intrinsic::IntrinsicAnnotation,
        llvm_intrinsic::LLVMIntrinsicAnnotation,
    },
};
use mira_spans::{ArenaList, Ident, Span};

use super::{
    TypecheckedModule, TypecheckingContext, TypecheckingError, TypecheckingErrorDiagnosticsExt,
    TypedExternalFunction, TypedFunction, TypedStatic, TypedTrait, default_types,
    ir::{OffsetValue, TypedExpression, TypedLiteral},
    types::{FunctionType, Ty, TyKind, TypeSuggestion, with_refcount},
};

pub type ScopeValueId<'arena> = StoreKey<ScopeEntry<'arena>>;

pub struct Scopes<'arena> {
    entries: Vec<HashMap<Ident<'arena>, ScopeValueId<'arena>>>,
    values: VecStore<ScopeEntry<'arena>>,
}

impl<'arena> Scopes<'arena> {
    pub fn new() -> Self {
        Self {
            entries: vec![HashMap::new()],
            values: VecStore::new(),
        }
    }

    pub fn get(&self, key: Ident<'arena>) -> Option<(ScopeEntry<'arena>, ScopeValueId<'arena>)> {
        let len = self.entries.len();
        for i in 1..=len {
            if let Some(v) = self.entries[len - i].get(&key) {
                return Some((self.values[*v], *v));
            }
        }
        None
    }

    pub fn make_stack_allocated(&mut self, id: ScopeValueId<'arena>) {
        self.values[id].stack_allocated = true;
    }

    pub fn insert_value(&mut self, key: Ident<'arena>, value: Ty<'arena>) -> ScopeValueId<'arena> {
        let id = self.push(value);
        self.insert(key, id);
        id
    }

    pub fn insert(&mut self, key: Ident<'arena>, value: ScopeValueId<'arena>) {
        debug_assert!(!self.entries.is_empty());
        let idx = self.entries.len() - 1;
        self.entries[idx].insert(key, value);
    }

    pub fn push(&mut self, ty: Ty<'arena>) -> ScopeValueId<'arena> {
        if !ty.is_sized() {
            panic!("unsized type: {ty:?}");
        }
        self.values.insert(ScopeEntry {
            ty,
            stack_allocated: false,
        })
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

impl<'arena> Default for Scopes<'arena> {
    fn default() -> Self {
        Self::new()
    }
}

pub fn typecheck_static<'arena>(
    context: &TypecheckingContext<'arena>,
    module_context: &ModuleContext<'arena>,
    static_id: StoreKey<TypedStatic<'arena>>,
    errs: &mut Diagnostics<'arena>,
) -> bool {
    let tc_module_reader = context.statics.read();
    let ty = tc_module_reader[static_id].type_;
    let expr = {
        std::mem::replace(
            &mut module_context.statics.write()[static_id.cast()].1,
            LiteralValue::Void,
        )
    };

    match typecheck_expression(
        context,
        tc_module_reader[static_id].module,
        &mut Scopes::new(),
        &Expression::Literal(expr, tc_module_reader[static_id].loc),
        &mut Vec::new(),
        TypeSuggestion::from_type(ty),
    ) {
        Err(e) => {
            errs.add(e);
            return false;
        }
        Ok((expr_typ, expr)) => {
            if ty != expr_typ {
                errs.add_mismatching_type(tc_module_reader[static_id].loc, ty, expr_typ);
                return false;
            }
            if !expr.is_entirely_literal() {
                errs.add_statics_need_to_be_literal(tc_module_reader[static_id].loc);
            }
            drop(tc_module_reader);
            context.statics.write()[static_id].value = expr;
        }
    }
    true
}

#[allow(clippy::result_unit_err)]
pub fn typecheck_external_function<'arena>(
    context: &TypecheckingContext<'arena>,
    module_context: &ModuleContext<'arena>,
    function_id: StoreKey<TypedExternalFunction<'arena>>,
    diagnostics: &mut Diagnostics<'arena>,
) -> Result<(), ()> {
    inner_typecheck_function(
        context,
        module_context,
        function_id.cast(),
        true,
        diagnostics,
    )
}

#[allow(clippy::result_unit_err)]
pub fn typecheck_function<'arena>(
    context: &TypecheckingContext<'arena>,
    module_context: &ModuleContext<'arena>,
    function_id: StoreKey<TypedFunction<'arena>>,
    diagnostics: &mut Diagnostics<'arena>,
) -> Result<(), ()> {
    inner_typecheck_function(context, module_context, function_id, false, diagnostics)
}

// NOTE: function_id has to be StoreKey<TypedExternalFunction> if is_external is set to true.
fn inner_typecheck_function<'arena>(
    context: &TypecheckingContext<'arena>,
    module_context: &ModuleContext<'arena>,
    function_id: StoreKey<TypedFunction<'arena>>,
    is_external: bool,
    errs: &mut Diagnostics<'arena>,
) -> Result<(), ()> {
    let errs_start_len = errs.len();

    let ext_fn_reader = module_context.external_functions.read();
    let fn_reader = module_context.functions.read();
    let (statement, module_id) = if is_external {
        let (_, ref statement, module_id) = ext_fn_reader[function_id.cast()];
        if let Some(statement) = statement {
            (statement, module_id)
        } else {
            return Ok(());
        }
    } else {
        let (_, ref statement, module_id) = fn_reader[function_id.cast()];
        (statement, module_id)
    };

    let mut scope = Scopes::new();

    let (return_type, args, loc) = if is_external {
        let contract = &context.external_functions.read()[function_id.cast()].0;
        (
            contract.return_type,
            contract.arguments.clone(),
            contract.span,
        )
    } else {
        let reader = context.functions.read();
        let contract = &reader[function_id].0;
        if contract.annotations.has_annotation::<IntrinsicAnnotation>()
            || contract
                .annotations
                .has_annotation::<LLVMIntrinsicAnnotation>()
        {
            let span = contract.span;
            drop(reader);
            context.functions.write()[function_id]
                .1
                .add_expr(TypedExpression::Unreachable(span));
            return Ok(());
        }
        (
            contract.return_type,
            contract.arguments.clone(),
            contract.span,
        )
    };

    if is_external
        && (!return_type.is_primitive() || (return_type.has_refs() && !return_type.is_thin_ptr()))
    {
        errs.add_invalid_extern_return_type(loc);
    }
    if !return_type.is_sized() {
        errs.add_unsized_return_type(loc, return_type);
    }
    for (_, arg) in args.iter().filter(|v| !v.1.is_sized()) {
        errs.add_unsized_argument(loc, *arg);
    }
    (errs.len() == errs_start_len).then_some(()).ok_or(())?;

    let mut exprs = vec![];
    for arg in args {
        let id = scope.insert_value(arg.0, arg.1);
        scope.make_stack_allocated(id);
    }

    let always_returns = typecheck_statement(
        context,
        &mut scope,
        statement,
        module_id.cast(),
        return_type,
        &mut exprs,
        errs,
    )?;
    if return_type != default_types::void && !always_returns {
        errs.add_body_does_not_always_return(statement.span());
        return Err(());
    }
    // add an implicit `return;` at the end of the function
    if !always_returns {
        typecheck_statement(
            context,
            &mut scope,
            &Statement::Return(None, statement.span()),
            module_id.cast(),
            return_type,
            &mut exprs,
            errs,
        )?;
    }
    if is_external {
        let mut ir = Some(IR::new(exprs, scope.values));
        std::mem::swap(
            &mut ir,
            &mut context.external_functions.write()[function_id.cast()].1,
        );
        assert!(ir.is_none())
    } else {
        let mut ir = IR::new(exprs, scope.values);
        std::mem::swap(&mut ir, &mut context.functions.write()[function_id].1);
        assert!(ir.exprs.is_empty());
        assert!(ir.values.is_empty());
    }
    Ok(())
}

/// Returns if the statement and if it always returns
fn typecheck_statement<'arena>(
    context: &TypecheckingContext<'arena>,
    scope: &mut Scopes<'arena>,
    statement: &Statement<'arena>,
    module: StoreKey<TypecheckedModule<'arena>>,
    return_type: Ty<'arena>,
    exprs: &mut Vec<TypedExpression<'arena>>,
    errs: &mut Diagnostics<'arena>,
) -> Result<bool, ()> {
    let errs_at_start = errs.len();
    match statement {
        Statement::If {
            condition,
            if_stmt,
            else_stmt,
            span,
            annotations,
        } => {
            let expr_result = typecheck_expression(
                context,
                module,
                scope,
                condition,
                exprs,
                TypeSuggestion::Bool,
            )
            .map_err(|v| _ = errs.add(v));

            let mut if_exprs = Vec::new();
            let mut else_exprs = Vec::new();
            let if_stmt_result = typecheck_statement(
                context,
                scope,
                if_stmt,
                module,
                return_type,
                &mut if_exprs,
                errs,
            );
            let else_stmt_result = if let Some(else_stmt) = else_stmt {
                typecheck_statement(
                    context,
                    scope,
                    else_stmt,
                    module,
                    return_type,
                    &mut else_exprs,
                    errs,
                )
            } else {
                Ok(false)
            };
            let Ok((condition_ty, cond)) = expr_result else {
                return Err(());
            };
            if condition_ty != default_types::bool {
                errs.add_mismatching_type(*span, default_types::bool, condition_ty);
            }
            let if_stmt_exits = if_stmt_result?;
            let else_stmt_exits = else_stmt_result?;
            if errs.len() != errs_at_start {
                return Err(());
            }
            exprs.push(TypedExpression::If {
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
            .map_err(|v| _ = errs.add(v));
            let body_result =
                typecheck_statement(context, scope, child, module, return_type, &mut body, errs);
            let Ok((condition_ty, cond)) = condition_result else {
                return Err(());
            };
            if condition_ty != default_types::bool {
                errs.add_mismatching_type(*span, default_types::bool, condition_ty);
            }
            let mut always_exits = body_result?;
            if errs.len() != errs_at_start {
                return Err(());
            }
            // while (true) {} also never exits
            always_exits = always_exits || matches!(cond, TypedLiteral::Bool(true));
            exprs.push(TypedExpression::While {
                span: *span,
                cond_block: condition_block.into_boxed_slice(),
                cond,
                body: (body.into_boxed_slice(), child.span()),
            });

            Ok(always_exits)
        }
        Statement::For { .. } => todo!("iterator (requires generics)"),
        Statement::Return(None, span) => {
            if return_type == default_types::void {
                exprs.push(TypedExpression::Return(*span, TypedLiteral::Void));
                Ok(true)
            } else {
                errs.add_mismatching_type(*span, return_type, default_types::void);
                Err(())
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
            .map_err(|e| _ = errs.add(e))?;
            if typ != return_type {
                errs.add_mismatching_type(expression.span(), return_type, typ);
                return Err(());
            }
            exprs.push(TypedExpression::Return(*location, typed_expression));
            Ok(true)
        }
        Statement::Block(statements, location, annotations) => {
            let mut block_exprs = Vec::with_capacity(statements.len());
            let mut always_returns = false;
            scope.push_scope();

            for statement in statements.iter() {
                if let Ok(true) = typecheck_statement(
                    context,
                    scope,
                    statement,
                    module,
                    return_type,
                    &mut block_exprs,
                    errs,
                ) {
                    always_returns = true;
                    if !matches!(statement, Statement::Return(..)) {
                        block_exprs.push(TypedExpression::Unreachable(statement.span()));
                    }
                    break;
                }
            }

            scope.pop_scope();

            if errs.len() != errs_at_start {
                Err(())
            } else {
                exprs.push(TypedExpression::Block(
                    *location,
                    block_exprs.into_boxed_slice(),
                    annotations.clone(),
                ));
                Ok(always_returns)
            }
        }
        Statement::Var(var) | Statement::Static { var, .. } => {
            let expected_typ = var
                .ty
                .as_ref()
                .map(|v| context.resolve_type(module, v, &[]))
                .transpose()
                .map_err(|v| _ = errs.add(v))?;

            let (typ, expr) = typecheck_expression(
                context,
                module,
                scope,
                &var.value,
                exprs,
                expected_typ
                    .as_ref()
                    .copied()
                    .map(TypeSuggestion::from_type)
                    .unwrap_or_default(),
            )
            .map_err(|e| _ = errs.add(e))?;

            if let Some(expected_typ) = expected_typ {
                if expected_typ != typ {
                    errs.add_mismatching_type(var.span, expected_typ, typ);
                    return Err(());
                }
            }

            let id = match expr {
                TypedLiteral::Dynamic(id) => id,
                _ => {
                    let id = scope.push(typ);
                    exprs.push(TypedExpression::Literal(var.value.span(), id, expr));
                    id
                }
            };
            scope.insert(var.name, id);
            exprs.push(TypedExpression::DeclareVariable(
                var.span,
                id,
                typ,
                var.name.symbol(),
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
        .map_err(|e| _ = errs.add(e))
        .map(|(typ, _)| typ == default_types::never),
        Statement::Use { .. }
        | Statement::Mod { .. }
        | Statement::BakedFunction(..)
        | Statement::Function { .. }
        | Statement::ExternalFunction { .. }
        | Statement::BakedStruct(..)
        | Statement::BakedStatic(..)
        | Statement::Struct { .. }
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
        let id = $scope.push(typ);
        $exprs.push(TypedExpression::$name($loc, id, $right_side));
        Ok((typ, TypedLiteral::Dynamic(id)))
    }};

    (binary $scope:expr, $exprs:expr; $name:ident ($loc:expr, $left_side:expr, $right_side:expr, $typ:expr)) => {{
        let typ = $typ;
        let id = $scope.push(typ);
        $exprs.push(TypedExpression::$name($loc, id, $left_side, $right_side));
        Ok((typ, TypedLiteral::Dynamic(id)))
    }};
}

fn signed_number_to_literal<'arena>(
    v: i64,
    number_type: NumberType,
    expected: TypeSuggestion<'arena>,
) -> (Ty<'arena>, TypedLiteral<'arena>) {
    match number_type {
        NumberType::I8 => (default_types::i8, TypedLiteral::I8(v as i8)),
        NumberType::I16 => (default_types::i16, TypedLiteral::I16(v as i16)),
        NumberType::I32 => (default_types::i32, TypedLiteral::I32(v as i32)),
        NumberType::I64 => (default_types::i64, TypedLiteral::I64(v)),
        NumberType::Isize => (default_types::isize, TypedLiteral::ISize(v as isize)),
        NumberType::F32 => (default_types::f32, TypedLiteral::F32(v as f32)),
        NumberType::F64 => (default_types::f64, TypedLiteral::F64(v as f64)),
        NumberType::None => match expected {
            TypeSuggestion::Number(
                number_typ @ (NumberType::I8
                | NumberType::I16
                | NumberType::I32
                | NumberType::I64
                | NumberType::Isize),
            ) => signed_number_to_literal(v, number_typ, TypeSuggestion::Unknown),
            _ => (default_types::i32, TypedLiteral::I32(v as i32)),
        },
        _ => unreachable!("this should never be an unsigned number"),
    }
}

fn unsigned_number_to_literal<'arena>(
    v: u64,
    number_type: NumberType,
    expected: TypeSuggestion<'arena>,
) -> (Ty<'arena>, TypedLiteral<'arena>) {
    match number_type {
        NumberType::U8 => (default_types::u8, TypedLiteral::U8(v as u8)),
        NumberType::U16 => (default_types::u16, TypedLiteral::U16(v as u16)),
        NumberType::U32 => (default_types::u32, TypedLiteral::U32(v as u32)),
        NumberType::U64 => (default_types::u64, TypedLiteral::U64(v)),
        NumberType::Usize => (default_types::usize, TypedLiteral::USize(v as usize)),
        NumberType::I8 => (default_types::i8, TypedLiteral::I8(v as i8)),
        NumberType::I16 => (default_types::i16, TypedLiteral::I16(v as i16)),
        NumberType::I32 => (default_types::i32, TypedLiteral::I32(v as i32)),
        NumberType::I64 => (default_types::i64, TypedLiteral::I64(v as i64)),
        NumberType::Isize => (default_types::isize, TypedLiteral::ISize(v as isize)),
        NumberType::F32 => (default_types::f32, TypedLiteral::F32(v as f32)),
        NumberType::F64 => (default_types::f64, TypedLiteral::F64(v as f64)),
        NumberType::None => match expected {
            TypeSuggestion::Number(NumberType::F32 | NumberType::F64) => {
                (default_types::i32, TypedLiteral::I32(v as i32))
            }
            TypeSuggestion::Number(number_typ) => {
                unsigned_number_to_literal(v, number_typ, TypeSuggestion::Unknown)
            }
            _ => (default_types::i32, TypedLiteral::I32(v as i32)),
        },
    }
}

fn float_number_to_literal<'arena>(
    v: f64,
    number_type: NumberType,
    expected: TypeSuggestion<'arena>,
) -> (Ty<'arena>, TypedLiteral<'arena>) {
    match number_type {
        NumberType::F32 => (default_types::f32, TypedLiteral::F32(v as f32)),
        NumberType::F64 => (default_types::f64, TypedLiteral::F64(v)),
        NumberType::None => match expected {
            TypeSuggestion::Number(number_typ @ (NumberType::F32 | NumberType::F64)) => {
                float_number_to_literal(v, number_typ, TypeSuggestion::Unknown)
            }
            _ => (default_types::f32, TypedLiteral::F32(v as f32)),
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
    exprs: &mut Vec<TypedExpression<'arena>>,
    type_suggestion: TypeSuggestion<'arena>,
) -> Result<(Ty<'arena>, TypedLiteral<'arena>), Diagnostic<'arena>> {
    let ctx = context.ctx;
    match expression {
        Expression::Literal(literal_value, location) => match literal_value {
            LiteralValue::String(global_str) => {
                Ok((default_types::str_ref, TypedLiteral::String(*global_str)))
            }
            LiteralValue::Array(ArrayLiteral::Values(vec)) if vec.is_empty() => {
                let typ = match type_suggestion {
                    TypeSuggestion::UnsizedArray(_) | TypeSuggestion::Array(_) => {
                        type_suggestion.to_type(context).ok_or_else(|| {
                            TypecheckingError::CannotInferArrayType(*location).to_error()
                        })?
                    }
                    _ => return Err(TypecheckingError::CannotInferArrayType(*location).to_error()),
                };
                Ok((typ, TypedLiteral::Array(typ, [].into())))
            }
            LiteralValue::Array(ArrayLiteral::CopyInitialized(value, amount)) => {
                let suggested_typ = match type_suggestion {
                    TypeSuggestion::UnsizedArray(v) | TypeSuggestion::Array(v) => *v,
                    _ => TypeSuggestion::Unknown,
                };
                let (typ, lit) =
                    typecheck_expression(context, module, scope, value, exprs, suggested_typ)?;
                Ok((
                    ctx.intern_ty(TyKind::SizedArray {
                        typ,
                        number_elements: *amount,
                    }),
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
                let suggested_typ = TypeSuggestion::from_type(typ);
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
                        }
                        .to_error());
                    }
                    elements.push(el_lit);
                }
                let arr_typ = ctx.intern_ty(TyKind::SizedArray {
                    typ,
                    number_elements: vec.len(),
                });
                Ok((
                    arr_typ,
                    TypedLiteral::Array(typ, elements.into_boxed_slice()),
                ))
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
                    ctx.intern_ty(TyKind::Tuple(ctx.intern_tylist(&element_types))),
                    TypedLiteral::Tuple(elements.into_boxed_slice()),
                ))
            }
            LiteralValue::AnonymousStruct(values) => {
                let struct_id = if let TypeSuggestion::Struct(id) = type_suggestion {
                    id
                } else {
                    return Err(TypecheckingError::CannotInferAnonStructType(*location).to_error());
                };

                let structure = &context.structs.read()[struct_id];
                // ensure there are no excessive values in the struct initialization
                for k in values.keys() {
                    if !structure.elements.iter().any(|v| v.0 == *k) {
                        return Err(TypecheckingError::NoSuchFieldFound {
                            location: values[k].0,
                            name: k.symbol(),
                        }
                        .to_error());
                    }
                }

                let mut elements = Vec::with_capacity(structure.elements.len());
                for (key, typ) in structure.elements.iter() {
                    let Some((loc, expr)) = values.get(key) else {
                        return Err(TypecheckingError::MissingField {
                            location: *location,
                            name: key.symbol(),
                        }
                        .to_error());
                    };
                    let (expr_typ, expr_lit) = typecheck_expression(
                        context,
                        module,
                        scope,
                        expr,
                        exprs,
                        TypeSuggestion::from_type(*typ),
                    )?;
                    if *typ != expr_typ {
                        return Err(TypecheckingError::MismatchingType {
                            expected: *typ,
                            found: expr_typ,
                            location: *loc,
                        }
                        .to_error());
                    }
                    elements.push(expr_lit);
                }
                Ok((
                    ctx.intern_ty(TyKind::Struct {
                        struct_id,
                        name: structure.name,
                    }),
                    TypedLiteral::Struct(struct_id, elements.into_boxed_slice()),
                ))
            }
            LiteralValue::Struct(values, path) => {
                let value = typed_resolve_import(
                    context,
                    module,
                    &path.entries.iter().map(|v| v.0).collect::<Vec<_>>(),
                    location,
                    &mut HashSet::new(),
                )
                .map_err(|_| {
                    TypecheckingError::CannotFindValue(*location, path.clone()).to_error()
                })?;
                let ModuleScopeValue::Struct(struct_id) = value else {
                    return Err(
                        TypecheckingError::CannotFindValue(*location, path.clone()).to_error()
                    );
                };
                let structure = &context.structs.read()[struct_id.cast()];
                // ensure there are no excessive values in the struct initialization
                for k in values.keys() {
                    if !structure.elements.iter().any(|v| v.0 == *k) {
                        return Err(TypecheckingError::NoSuchFieldFound {
                            location: values[k].0,
                            name: k.symbol(),
                        }
                        .to_error());
                    }
                }

                let mut elements = Vec::with_capacity(structure.elements.len());
                for (key, typ) in structure.elements.iter() {
                    let Some((loc, expr)) = values.get(key) else {
                        return Err(TypecheckingError::MissingField {
                            location: *location,
                            name: key.symbol(),
                        }
                        .to_error());
                    };
                    let (expr_typ, expr_lit) = typecheck_expression(
                        context,
                        module,
                        scope,
                        expr,
                        exprs,
                        TypeSuggestion::from_type(*typ),
                    )?;
                    if *typ != expr_typ {
                        return Err(TypecheckingError::MismatchingType {
                            expected: *typ,
                            found: expr_typ,
                            location: *loc,
                        }
                        .to_error());
                    }
                    elements.push(expr_lit);
                }
                Ok((
                    ctx.intern_ty(TyKind::Struct {
                        struct_id: struct_id.cast(),
                        name: structure.name,
                    }),
                    TypedLiteral::Struct(struct_id.cast(), elements.into_boxed_slice()),
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
            LiteralValue::Bool(v) => Ok((default_types::bool, TypedLiteral::Bool(*v))),
            LiteralValue::Dynamic(path) => {
                if path.entries.len() == 1 && path.entries[0].1.is_empty() {
                    if let Some((entry, id)) = scope.get(path.entries[0].0) {
                        return Ok((entry.ty, TypedLiteral::Dynamic(id)));
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
                    &mut HashSet::new(),
                )
                .map_err(|_| {
                    TypecheckingError::CannotFindValue(*location, path.clone()).to_error()
                })?;
                match value {
                    ModuleScopeValue::Function(id) => {
                        let reader = &context.functions.read()[id.cast()];
                        let mut return_type = reader.0.return_type;
                        let mut arguments =
                            reader.0.arguments.iter().map(|v| v.1).collect::<Vec<_>>();
                        if reader.0.generics.len() != generics.len() {
                            return Err(TypecheckingError::MismatchingGenericCount(
                                *location,
                                reader.0.generics.len(),
                                generics.len(),
                            )
                            .to_error());
                        }
                        let mut generic_types = Vec::with_capacity(generics.len());
                        for (i, typ) in generics.iter().enumerate() {
                            let ty = context.resolve_type(module, typ, &[])?;
                            if reader.0.generics[i].sized && !ty.is_sized() {
                                return Err(TypecheckingError::UnsizedForSizedGeneric(
                                    *location, ty,
                                )
                                .to_error());
                            }
                            generic_types.push(ty);
                        }
                        for argty in arguments
                            .iter_mut()
                            .chain(std::iter::once(&mut return_type))
                        {
                            let (refcount, ty) = argty.remove_refs();
                            if let TyKind::Generic { generic_id, .. } = **ty {
                                *argty = with_refcount(
                                    ctx,
                                    generic_types[generic_id as usize],
                                    refcount,
                                );
                            }
                        }
                        let function_ty = FunctionType {
                            arguments: ctx.intern_tylist(&arguments),
                            return_type,
                        };

                        let literal = if let Some(intrinsic) = reader
                            .0
                            .annotations
                            .get_first_annotation::<IntrinsicAnnotation>()
                            .map(IntrinsicAnnotation::get)
                        {
                            TypedLiteral::Intrinsic(intrinsic, ctx.intern_tylist(&generic_types))
                        } else if let Some(llvm_intrinsic) = reader
                            .0
                            .annotations
                            .get_first_annotation::<LLVMIntrinsicAnnotation>()
                            .map(LLVMIntrinsicAnnotation::get)
                        {
                            let sym = ctx.intern_str(llvm_intrinsic);
                            TypedLiteral::LLVMIntrinsic(sym)
                        } else {
                            TypedLiteral::Function(id.cast(), ctx.intern_tylist(&generic_types))
                        };
                        Ok((ctx.intern_ty(TyKind::Function(function_ty)), literal))
                    }
                    ModuleScopeValue::ExternalFunction(id) => {
                        let reader = &context.external_functions.read()[id.cast()];
                        let function_typ = FunctionType {
                            return_type: reader.0.return_type,
                            arguments: ctx.intern_tylist(
                                &reader.0.arguments.iter().map(|v| v.1).collect::<Vec<_>>(),
                            ),
                        };
                        Ok((
                            ctx.intern_ty(TyKind::Function(function_typ)),
                            TypedLiteral::ExternalFunction(id.cast()),
                        ))
                    }
                    ModuleScopeValue::Static(id) => {
                        if !generics.is_empty() {
                            return Err(TypecheckingError::UnexpectedGenerics {
                                location: *location,
                            }
                            .to_error());
                        }
                        Ok((
                            context.statics.read()[id.cast()].type_,
                            TypedLiteral::Static(id.cast()),
                        ))
                    }
                    _ => {
                        Err(TypecheckingError::CannotFindValue(*location, path.clone()).to_error())
                    }
                }
            }
            LiteralValue::BakedAnonymousFunction(fn_id) => {
                let func = &context.functions.read()[fn_id.cast()].0;
                let fn_typ = FunctionType {
                    return_type: func.return_type,
                    arguments: ctx
                        .intern_tylist(&func.arguments.iter().map(|(_, v)| *v).collect::<Vec<_>>()),
                };
                Ok((
                    ctx.intern_ty(TyKind::Function(fn_typ)),
                    TypedLiteral::Function(fn_id.cast(), EMPTY_TYLIST),
                ))
            }
            LiteralValue::AnonymousFunction(..) => unreachable!("unbaked function"),
            LiteralValue::Void => Ok((default_types::void, TypedLiteral::Void)),
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
                mira_parser::TypeRef::Reference { type_name, .. } => Some(type_name.entries[0].0),
                _ => None,
            };
            // this should never fail unless this is an incompatible type (non-primitive)
            let output = context
                .resolve_type(module, output, &[])
                .ok()
                .filter(|v| (*v == default_types::void && name.is_none()) || v.is_asm_primitive())
                .ok_or_else(|| {
                    TypecheckingError::AsmNonNumericType(*loc, name.unwrap().symbol()).to_error()
                })?;
            let mut typed_inputs = Vec::with_capacity(inputs.len());
            for (span, name) in inputs {
                let Some((entry, id)) = scope.get(*name) else {
                    return Err(TypecheckingError::CannotFindValue(
                        *span,
                        Path::new(*name, Vec::new()),
                    )
                    .to_error());
                };
                if !entry.ty.is_asm_primitive() {
                    return Err(
                        TypecheckingError::AsmNonNumericTypeResolved(*span, entry.ty).to_error(),
                    );
                }
                typed_inputs.push(id);
            }
            let id = scope.push(output);
            exprs.push(TypedExpression::Asm {
                span: *loc,
                dst: id,
                inputs: typed_inputs,
                registers: registers.clone(),
                volatile: *volatile,
                asm: asm.clone(),
            });
            if output == default_types::void {
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
                UnaryOp::Plus => Err(TypecheckingError::CannotPos(*loc, typ).to_error()),
                UnaryOp::Minus if (typ.is_int_like() && !typ.is_unsigned()) || typ.is_float() => {
                    tc_res!(unary scope, exprs; Neg(*loc, right_side, typ))
                }
                UnaryOp::Minus => Err(TypecheckingError::CannotNeg(*loc, typ).to_error()),
                UnaryOp::LogicalNot if typ == default_types::bool => {
                    tc_res!(unary scope, exprs; LNot(*loc, right_side, typ))
                }
                UnaryOp::LogicalNot => Err(TypecheckingError::CannotLNot(*loc, typ).to_error()),
                UnaryOp::BitwiseNot if typ.is_int_like() || typ == default_types::bool => {
                    tc_res!(unary scope, exprs; BNot(*loc, right_side, typ))
                }
                UnaryOp::BitwiseNot => Err(TypecheckingError::CannotBNot(*loc, typ).to_error()),
                UnaryOp::Dereference => match typ.deref() {
                    Some(ty) => tc_res!(unary scope, exprs; Dereference(*loc, right_side, ty)),
                    None => Err(TypecheckingError::CannotDeref(*loc, typ).to_error()),
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
                    return Err(
                        TypecheckingError::CannotShiftByNonUInt(*span, typ_right).to_error()
                    );
                }

                return match operator {
                    BinaryOp::LShift if typ.is_int_like() => {
                        tc_res!(binary scope, exprs; LShift(*span, left_side, right_side, typ))
                    }
                    BinaryOp::RShift if typ.is_int_like() => {
                        tc_res!(binary scope, exprs; RShift(*span, left_side, right_side, typ))
                    }

                    BinaryOp::LShift => Err(TypecheckingError::CannotShl(*span, typ).to_error()),
                    BinaryOp::RShift => Err(TypecheckingError::CannotShr(*span, typ).to_error()),
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
                TypeSuggestion::from_type(typ_left),
            )?;
            if typ_left != typ_right {
                return Err(TypecheckingError::LhsNotRhs(*span, typ_left, typ_right).to_error());
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
                    tc_res!(binary scope, exprs; GreaterThan(loc, left_side, right_side, default_types::bool))
                }
                BinaryOp::GreaterThanEq if typ.is_int_like() => {
                    tc_res!(binary scope, exprs; GreaterThanEq(loc, left_side, right_side, default_types::bool))
                }
                BinaryOp::LessThan if typ.is_int_like() => {
                    tc_res!(binary scope, exprs; LessThan(loc, left_side, right_side, default_types::bool))
                }
                BinaryOp::LessThanEq if typ.is_int_like() => {
                    tc_res!(binary scope, exprs; LessThanEq(loc, left_side, right_side, default_types::bool))
                }
                BinaryOp::Equals if typ.is_int_like() || typ.is_bool() => {
                    tc_res!(binary scope, exprs; Eq(loc, left_side, right_side, default_types::bool))
                }
                BinaryOp::NotEquals if typ.is_int_like() || typ.is_bool() => {
                    tc_res!(binary scope, exprs; Neq(loc, left_side, right_side, default_types::bool))
                }

                BinaryOp::Plus => Err(TypecheckingError::CannotAdd(loc, typ).to_error()),
                BinaryOp::Minus => Err(TypecheckingError::CannotSub(loc, typ).to_error()),
                BinaryOp::Multiply => Err(TypecheckingError::CannotMul(loc, typ).to_error()),
                BinaryOp::Divide => Err(TypecheckingError::CannotDiv(loc, typ).to_error()),
                BinaryOp::Modulo => Err(TypecheckingError::CannotMod(loc, typ).to_error()),
                BinaryOp::BitwiseAnd => Err(TypecheckingError::CannotBAnd(loc, typ).to_error()),
                BinaryOp::BitwiseOr => Err(TypecheckingError::CannotBOr(loc, typ).to_error()),
                BinaryOp::BitwiseXor => Err(TypecheckingError::CannotBXor(loc, typ).to_error()),
                BinaryOp::LogicalOr => Err(TypecheckingError::CannotLOr(loc, typ).to_error()),
                BinaryOp::LogicalAnd => Err(TypecheckingError::CannotLAnd(loc, typ).to_error()),
                BinaryOp::GreaterThan
                | BinaryOp::GreaterThanEq
                | BinaryOp::LessThan
                | BinaryOp::LessThanEq => {
                    Err(TypecheckingError::CannotCompare(loc, typ).to_error())
                }
                BinaryOp::Equals | BinaryOp::NotEquals => {
                    Err(TypecheckingError::CannotEq(loc, typ).to_error())
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
            let TyKind::Function(function_type) = &**typ.without_ref() else {
                return Err(TypecheckingError::TypeIsNotAFunction {
                    location: identifier.span(),
                }
                .to_error());
            };
            let mut typed_arguments = Vec::with_capacity(function_type.arguments.len());
            if arguments.len() < function_type.arguments.len() {
                return Err(TypecheckingError::MissingArguments {
                    location: identifier.span(),
                }
                .to_error());
            }
            if arguments.len() > function_type.arguments.len() && !has_vararg {
                return Err(TypecheckingError::TooManyArguments {
                    location: arguments[function_type.arguments.len().saturating_sub(1)].span(),
                }
                .to_error());
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
                        .copied()
                        .map(TypeSuggestion::from_type)
                        .unwrap_or_default(),
                )?;
                // ignore for i => function_type.arguments.len() because we can't possibly know the
                // type of varargs. This is of course incredibly unsafe and as such safety
                // precautions have to be taken when calling a function with varargs, but the
                // compiler cannot help with those.
                if i < function_type.arguments.len() && typ != function_type.arguments[i] {
                    return Err(TypecheckingError::MismatchingType {
                        expected: function_type.arguments[i],
                        found: typ,
                        location: arguments[i].span(),
                    }
                    .to_error());
                }
                typed_arguments.push(expr);
            }

            if let TypedLiteral::Intrinsic(intrinsic, generics) = function_expr {
                let typ = function_type.return_type;
                let id = scope.push(typ);
                exprs.push(TypedExpression::IntrinsicCall(
                    identifier.span(),
                    id,
                    intrinsic,
                    typed_arguments,
                    generics,
                ));
                Ok((typ, TypedLiteral::Dynamic(id)))
            } else if let TypedLiteral::LLVMIntrinsic(intrinsic) = function_expr {
                let typ = function_type.return_type;
                let id = scope.push(typ);
                exprs.push(TypedExpression::LLVMIntrinsicCall(
                    identifier.span(),
                    id,
                    intrinsic,
                    typed_arguments,
                ));
                Ok((typ, TypedLiteral::Dynamic(id)))
            } else if let TypedLiteral::Function(fn_id, generics) = function_expr {
                let typ = function_type.return_type;
                let id = scope.push(typ);
                exprs.push(TypedExpression::DirectCall(
                    identifier.span(),
                    id,
                    fn_id,
                    typed_arguments,
                    generics,
                ));
                Ok((typ, TypedLiteral::Dynamic(id)))
            } else if let TypedLiteral::ExternalFunction(fn_id) = function_expr {
                tc_res!(binary scope, exprs; DirectExternCall(identifier.span(), fn_id, typed_arguments, function_type.return_type))
            } else {
                tc_res!(binary scope, exprs; Call(identifier.span(), function_expr, typed_arguments, function_type.return_type))
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
                    let typ = typ.deref().ok_or_else(|| {
                        TypecheckingError::CannotDeref(right_side.span(), typ).to_error()
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
                TypeSuggestion::from_type(typ_lhs),
            )?;

            if typ_lhs != typ_rhs {
                return Err(TypecheckingError::MismatchingType {
                    expected: typ_lhs,
                    found: typ_rhs,
                    location: *span,
                }
                .to_error());
            }
            exprs.push(TypedExpression::StoreAssignment(*span, lhs, rhs));
            Ok((default_types::void, TypedLiteral::Void))
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
                TypeSuggestion::from_type(typ),
            )?;
            if typ != typ_rhs {
                return Err(TypecheckingError::MismatchingType {
                    expected: typ,
                    found: typ_rhs,
                    location: right_side.span(),
                }
                .to_error());
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
    exprs: &mut Vec<TypedExpression<'arena>>,
    typ: Ty<'arena>,
    new_typ: Ty<'arena>,
    lhs: TypedLiteral<'arena>,
    loc: Span<'arena>,
    context: &TypecheckingContext<'arena>,
) -> Result<(Ty<'arena>, TypedLiteral<'arena>), Diagnostic<'arena>> {
    if typ == new_typ {
        return Ok((new_typ, lhs));
    }
    let ctx = context.ctx;

    let (ref_self, ty) = typ.remove_refs();
    let (ref_other, new_ty) = new_typ.remove_refs();
    match (&**ty, &**new_ty) {
        // &str -> &[u8]
        (TyKind::PrimitiveStr, TyKind::UnsizedArray(typ))
            if ref_self == ref_other && ref_self > 0 && *typ == default_types::u8 =>
        {
            let id = scope.push(new_typ);
            exprs.push(TypedExpression::Alias(loc, id, lhs));
            Ok((new_typ, TypedLiteral::Dynamic(id)))
        }
        // &str -> &u8
        (TyKind::PrimitiveStr, TyKind::PrimitiveU8) if ref_self == 1 && ref_other == 1 => {
            let id = scope.push(new_typ);
            exprs.push(TypedExpression::StripMetadata(loc, id, lhs));
            Ok((new_typ, TypedLiteral::Dynamic(id)))
        }
        // &T to &[T; 1]
        (
            _,
            TyKind::SizedArray {
                typ: typ_other,
                number_elements: 1,
            },
        ) if ref_other > 0
            && ref_self >= ref_other
            && *typ_other == typ.with_num_refs(ref_self - ref_other, ctx) =>
        {
            let id = scope.push(new_typ);
            exprs.push(TypedExpression::Alias(loc, id, lhs));
            Ok((new_typ, TypedLiteral::Dynamic(id)))
        }
        // &T -> &dyn _
        (_, TyKind::DynType(trait_refs)) if ref_self > 0 && ref_other == 1 && typ.is_thin_ptr() => {
            let traits = trait_refs.iter().map(|v| v.0).collect::<Vec<_>>();
            let ty = typ.deref().expect("v should have a refcount of > 0");
            if !ty.implements(&traits, context) {
                let trait_reader = context.traits.read();
                return Err(TypecheckingError::MismatchingTraits(
                    loc,
                    typ,
                    traits
                        .iter()
                        .map(|v| trait_reader[*v].name.symbol())
                        .collect(),
                )
                .to_error());
            }
            let id = scope.push(new_typ);
            exprs.push(TypedExpression::AttachVtable(loc, id, lhs, (ty, traits)));
            Ok((new_typ, TypedLiteral::Dynamic(id)))
        }
        // &[T; N] -> &[T]
        (
            TyKind::SizedArray {
                typ,
                number_elements,
            },
            TyKind::UnsizedArray(typ_other),
        ) if typ == typ_other && ref_self == 1 && ref_other == 1 => {
            let id = scope.push(new_typ);
            exprs.push(TypedExpression::MakeUnsizedSlice(
                loc,
                id,
                lhs,
                *number_elements,
            ));
            Ok((new_typ, TypedLiteral::Dynamic(id)))
        }
        // (&[T] -> &T)
        (TyKind::UnsizedArray(typ), _)
            if ref_self == 1 && ref_other == 1 && **new_typ == TyKind::Ref(*typ) =>
        {
            let id = scope.push(new_typ);
            exprs.push(TypedExpression::StripMetadata(loc, id, lhs));
            Ok((new_typ, TypedLiteral::Dynamic(id)))
        }
        // &&void -> &void
        (TyKind::PrimitiveVoid, TyKind::PrimitiveVoid) if ref_self > 0 && ref_other == 1 => {
            let id = scope.push(new_typ);
            exprs.push(TypedExpression::Bitcast(loc, id, lhs));
            Ok((new_typ, TypedLiteral::Dynamic(id)))
        }
        // &void -> usize
        (TyKind::PrimitiveVoid, TyKind::PrimitiveUSize) if ref_self == 1 && ref_other == 0 => {
            let id = scope.push(new_typ);
            exprs.push(TypedExpression::PtrToInt(loc, id, lhs));
            Ok((new_typ, TypedLiteral::Dynamic(id)))
        }
        // usize -> &void
        (TyKind::PrimitiveUSize, TyKind::PrimitiveVoid) if ref_self == 0 && ref_other == 1 => {
            let id = scope.push(new_typ);
            exprs.push(TypedExpression::IntToPtr(loc, id, lhs));
            Ok((new_typ, TypedLiteral::Dynamic(id)))
        }
        // fn to &void
        (TyKind::Function(_), TyKind::PrimitiveVoid) if ref_self == 0 && ref_other == 1 => {
            let id = scope.push(new_typ);
            exprs.push(TypedExpression::Bitcast(loc, id, lhs));
            Ok((new_typ, TypedLiteral::Dynamic(id)))
        }
        // &T to &void
        (_, TyKind::PrimitiveVoid) if ref_other > 0 && ref_self > 0 && typ.is_thin_ptr() => {
            let id = scope.push(new_typ);
            exprs.push(TypedExpression::Bitcast(loc, id, lhs));
            Ok((new_typ, TypedLiteral::Dynamic(id)))
        }
        // &void to &T
        (TyKind::PrimitiveVoid, _) if ref_self > 0 && ref_other > 0 && new_typ.is_thin_ptr() => {
            let id = scope.push(new_typ);
            exprs.push(TypedExpression::Bitcast(loc, id, lhs));
            Ok((new_typ, TypedLiteral::Dynamic(id)))
        }
        (
            TyKind::PrimitiveU8 | TyKind::PrimitiveBool,
            TyKind::PrimitiveU8 | TyKind::PrimitiveBool,
        )
        | (
            TyKind::PrimitiveU8
            | TyKind::PrimitiveU16
            | TyKind::PrimitiveU32
            | TyKind::PrimitiveU64
            | TyKind::PrimitiveUSize
            | TyKind::PrimitiveI8
            | TyKind::PrimitiveI16
            | TyKind::PrimitiveI32
            | TyKind::PrimitiveI64
            | TyKind::PrimitiveISize
            | TyKind::PrimitiveF32
            | TyKind::PrimitiveF64,
            TyKind::PrimitiveU8
            | TyKind::PrimitiveU16
            | TyKind::PrimitiveU32
            | TyKind::PrimitiveU64
            | TyKind::PrimitiveUSize
            | TyKind::PrimitiveI8
            | TyKind::PrimitiveI16
            | TyKind::PrimitiveI32
            | TyKind::PrimitiveI64
            | TyKind::PrimitiveISize
            | TyKind::PrimitiveF32
            | TyKind::PrimitiveF64,
        ) if ref_self == 0 && ref_other == 0 => {
            let id = scope.push(new_typ);
            exprs.push(TypedExpression::IntCast(loc, id, lhs));
            Ok((new_typ, TypedLiteral::Dynamic(id)))
        }
        _ => Err(TypecheckingError::DisallowedCast(loc, typ, new_typ).to_error()),
    }
}

#[allow(clippy::too_many_arguments, clippy::result_large_err)]
fn typecheck_dyn_membercall<'arena>(
    context: &TypecheckingContext<'arena>,
    scope: &mut Scopes<'arena>,
    module: StoreKey<TypecheckedModule<'arena>>,
    exprs: &mut Vec<TypedExpression<'arena>>,
    ident: &Ident<'arena>,
    args: &[Expression<'arena>],
    lhs: TypedLiteral<'arena>,
    lhs_loc: Span<'arena>,
    trait_refs: ArenaList<'arena, (StoreKey<TypedTrait<'arena>>, Ident<'arena>)>,
    num_references: u8,
) -> Result<(Ty<'arena>, TypedLiteral<'arena>), Diagnostic<'arena>> {
    let ctx = context.ctx;
    let trait_reader = context.traits.read();
    let mut offset = 0;
    let (mut arg_typs, return_ty, trait_name) = 'out: {
        for trait_id in trait_refs.iter().map(|v| v.0) {
            for func in trait_reader[trait_id].functions.iter() {
                if func.0 == *ident {
                    break 'out (func.1.clone(), func.2, trait_reader[trait_id].name);
                }
                offset += 1;
            }
        }

        return Err(TypecheckingError::CannotFindFunctionOnType(
            lhs_loc,
            ident.symbol(),
            ctx.intern_ty(TyKind::DynType(trait_refs)),
        )
        .to_error());
    };
    drop(trait_reader);

    if arg_typs[0].1.without_ref() != default_types::self_ {
        return Err(TypecheckingError::InvalidDynTypeFunc(
            lhs_loc,
            ident.symbol(),
            trait_name.symbol(),
        )
        .to_error());
    }
    if return_ty.without_ref() == default_types::self_
        || arg_typs
            .iter()
            .skip(1)
            .any(|v| v.1.without_ref() == default_types::self_)
    {
        return Err(TypecheckingError::InvalidDynTypeFunc(
            lhs_loc,
            ident.symbol(),
            trait_name.symbol(),
        )
        .to_error());
    }

    let ty = ctx.intern_ty(TyKind::DynType(trait_refs));
    let mut ty = with_refcount(ctx, ty, num_references);
    let mut lhs = lhs;
    while ty.has_double_refs() {
        ty = ty.deref().expect("dereferencing &_ should never fail");
        let id = scope.push(ctx.intern_ty_ref(&ty));
        exprs.push(TypedExpression::Dereference(lhs_loc, id, lhs));
        lhs = TypedLiteral::Dynamic(id);
    }

    let mut typed_args = Vec::with_capacity(args.len() + 1);
    typed_args.push(lhs);

    if args.len() < arg_typs.len() - 1 {
        return Err(TypecheckingError::MissingArguments { location: lhs_loc }.to_error());
    }
    if args.len() > arg_typs.len() - 1 {
        return Err(TypecheckingError::TooManyArguments {
            location: args[arg_typs.len() - 1].span(),
        }
        .to_error());
    }

    for i in 0..args.len() {
        let (ty, lit) = typecheck_expression(
            context,
            module,
            scope,
            &args[i],
            exprs,
            TypeSuggestion::from_type(arg_typs[i + 1].1),
        )?;
        if ty != arg_typs[i + 1].1 {
            return Err(TypecheckingError::MismatchingType {
                expected: arg_typs.remove(i + 1).1,
                found: ty,
                location: args[i].span(),
            }
            .to_error());
        }
        typed_args.push(lit);
    }

    let is_void = return_ty == default_types::void || return_ty == default_types::never;
    let id = scope.push(return_ty);
    exprs.push(TypedExpression::DynCall(lhs_loc, id, typed_args, offset));
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
    exprs: &mut Vec<TypedExpression<'arena>>,
    lhs: &Expression<'arena>,
    ident: &Ident<'arena>,
    args: &[Expression<'arena>],
) -> Result<(Ty<'arena>, TypedLiteral<'arena>), Diagnostic<'arena>> {
    let (mut typ_lhs, mut typed_literal_lhs) =
        typecheck_take_ref(context, module, scope, lhs, exprs, TypeSuggestion::Unknown)?;
    let lhs_refcount = typ_lhs.refcount();
    match **typ_lhs.without_ref() {
        TyKind::DynType(trait_refs) if lhs_refcount > 0 => {
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
                lhs_refcount,
            );
        }
        _ => (),
    }

    let function_reader = context.functions.read();
    let langitem_reader = context.lang_items.read();
    let struct_reader = context.structs.read();

    let struct_id = match **typ_lhs.without_ref() {
        TyKind::Ref(_) | TyKind::Generic { .. } | TyKind::PrimitiveSelf => unreachable!(),
        TyKind::UnsizedArray { .. }
        | TyKind::SizedArray { .. }
        | TyKind::Tuple { .. }
        | TyKind::DynType { .. }
        | TyKind::Function(..)
        | TyKind::PrimitiveVoid
        | TyKind::PrimitiveNever => None,
        TyKind::Struct { struct_id, .. } => Some(struct_id),
        TyKind::PrimitiveI8 => langitem_reader.i8,
        TyKind::PrimitiveI16 => langitem_reader.i16,
        TyKind::PrimitiveI32 => langitem_reader.i32,
        TyKind::PrimitiveI64 => langitem_reader.i64,
        TyKind::PrimitiveISize => langitem_reader.isize,
        TyKind::PrimitiveU8 => langitem_reader.u8,
        TyKind::PrimitiveU16 => langitem_reader.u16,
        TyKind::PrimitiveU32 => langitem_reader.u32,
        TyKind::PrimitiveU64 => langitem_reader.u64,
        TyKind::PrimitiveUSize => langitem_reader.usize,
        TyKind::PrimitiveF32 => langitem_reader.f32,
        TyKind::PrimitiveF64 => langitem_reader.f64,
        TyKind::PrimitiveStr => langitem_reader.str,
        TyKind::PrimitiveBool => langitem_reader.bool,
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
            ident.symbol(),
            typ_lhs,
        )
        .to_error());
    };

    let function: &(_, _) = &function_reader[function_id];
    if function.0.arguments.first().map(|v| v.1.without_ref()) != Some(typ_lhs.without_ref()) {
        return Err(
            TypecheckingError::NonMemberFunction(function.0.span, ident.symbol(), typ_lhs)
                .to_error(),
        );
    }
    let arg_refcount = function.0.arguments[0].1.refcount();
    while typ_lhs.refcount() != arg_refcount {
        if typ_lhs.refcount() > arg_refcount {
            typ_lhs = typ_lhs
                .deref()
                .expect("you should always be able to deref &_");
            let new_id = scope.push(typ_lhs);
            exprs.push(TypedExpression::Dereference(
                lhs.span(),
                new_id,
                typed_literal_lhs,
            ));
            typed_literal_lhs = TypedLiteral::Dynamic(new_id);
        } else {
            typed_literal_lhs = make_reference(
                scope,
                exprs,
                typ_lhs,
                typed_literal_lhs,
                lhs.span(),
                context.ctx,
            );
            typ_lhs = typ_lhs.take_ref(context.ctx);
        }
    }

    let mut typed_arguments = Vec::with_capacity(function.0.arguments.len() + 1);
    typed_arguments.push(typed_literal_lhs);
    if args.len() < function.0.arguments.len() - 1 {
        return Err(TypecheckingError::MissingArguments {
            location: lhs.span(),
        }
        .to_error());
    }
    if args.len() > function.0.arguments.len() - 1 {
        return Err(TypecheckingError::TooManyArguments {
            location: args[function.0.arguments.len() - 1].span(),
        }
        .to_error());
    }
    for (i, (_, arg)) in function.0.arguments.iter().skip(1).enumerate() {
        let (typ, expr) = typecheck_expression(
            context,
            module,
            scope,
            &args[i + 1], // skip one argument for the `self` argument
            exprs,
            TypeSuggestion::from_type(*arg),
        )?;
        if typ != function.0.arguments[i + 1].1 {
            return Err(TypecheckingError::MismatchingType {
                expected: function.0.arguments[i + 1].1,
                found: typ,
                location: args[i].span(),
            }
            .to_error());
        }
        typed_arguments.push(expr);
    }

    let call_id = scope.push(function.0.return_type);
    exprs.push(TypedExpression::DirectCall(
        lhs.span(),
        call_id,
        function_id,
        typed_arguments,
        EMPTY_TYLIST,
    ));

    Ok((function.0.return_type, TypedLiteral::Dynamic(call_id)))
}

#[allow(clippy::result_large_err)]
fn typecheck_take_ref<'arena>(
    context: &TypecheckingContext<'arena>,
    module: StoreKey<TypecheckedModule<'arena>>,
    scope: &mut Scopes<'arena>,
    expression: &Expression<'arena>,
    exprs: &mut Vec<TypedExpression<'arena>>,
    type_suggestion: TypeSuggestion<'arena>,
) -> Result<(Ty<'arena>, TypedLiteral<'arena>), Diagnostic<'arena>> {
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
            Ok((typ.take_ref(context.ctx), expr))
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
    exprs: &mut Vec<TypedExpression<'arena>>,
    type_suggestion: TypeSuggestion<'arena>,
    increase_ref: bool,
) -> Result<(Ty<'arena>, TypedLiteral<'arena>), Diagnostic<'arena>> {
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
                while typ_lhs.has_refs() {
                    let old_typ_lhs = typ_lhs;
                    typ_lhs = typ_lhs
                        .deref()
                        .expect("&_ should never fail to dereference");
                    let id = scope.push(old_typ_lhs);
                    exprs.push(TypedExpression::Dereference(
                        expression.span(),
                        id,
                        typed_literal_lhs,
                    ));
                    typed_literal_lhs = TypedLiteral::Dynamic(id);
                }
                assert!(!typ_lhs.has_refs(), "non-zero refcount after auto-deref");
                match **typ_lhs {
                    TyKind::Struct { struct_id, .. } => {
                        let structure = &context.structs.read()[struct_id];
                        match structure
                            .elements
                            .iter()
                            .enumerate()
                            .find(|(_, (v, _))| v == element_name)
                            .map(|(idx, (_, typ))| (idx, typ))
                        {
                            Some((idx, typ)) => {
                                typ_lhs = *typ;
                                let new_val = scope.push(typ_lhs.take_ref(context.ctx));
                                exprs.push(TypedExpression::Offset(
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
                                    element_name.symbol(),
                                )
                                .to_error());
                            }
                        }
                    }
                    _ => {
                        return Err(
                            TypecheckingError::AccessNonStructValue(*span, typ_lhs).to_error()
                        );
                    }
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
            while typ_lhs.has_refs() {
                typ_lhs = typ_lhs
                    .deref()
                    .expect("&_ should never fail to dereference");
                let id = scope.push(typ_lhs);
                exprs.push(TypedExpression::Dereference(
                    expression.span(),
                    id,
                    typed_literal_lhs,
                ));
                typed_literal_lhs = TypedLiteral::Dynamic(id);
            }
            assert!(!typ_lhs.has_refs(), "non-zero refcount after auto-deref");
            let offset = indexing_resolve_rhs(context, module, scope, right_side, exprs)?;
            let typ = match &**typ_lhs {
                TyKind::SizedArray { typ, .. } | TyKind::UnsizedArray(typ) => *typ,
                TyKind::Tuple(elements) => match offset {
                    OffsetValue::Dynamic(_) => {
                        return Err(
                            TypecheckingError::TupleDynamicIndex(expression.span()).to_error()
                        );
                    }
                    OffsetValue::Static(idx) if idx >= elements.len() => {
                        return Err(TypecheckingError::TupleIndexOutOfBounds(
                            expression.span(),
                            elements.len(),
                            idx,
                        )
                        .to_error());
                    }
                    OffsetValue::Static(idx) => elements[idx],
                },
                _ => {
                    return Err(
                        TypecheckingError::IndexNonArrayElem(expression.span(), typ_lhs).to_error(),
                    );
                }
            };

            let id = scope.push(typ.take_ref(context.ctx));
            exprs.push(TypedExpression::Offset(
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
                if let TyKind::Ref(ty) = **typ
                    && let TyKind::SizedArray {
                        number_elements,
                        typ,
                    } = **ty
                {
                    let typ = context.ctx.intern_ty(TyKind::UnsizedArray(typ));
                    let id = scope.push(typ.take_ref(context.ctx));
                    exprs.push(TypedExpression::MakeUnsizedSlice(
                        expression.span(),
                        id,
                        typed_literal,
                        number_elements,
                    ));
                    return Ok((typ, TypedLiteral::Dynamic(id)));
                }
            }

            if typ.has_refs() && !increase_ref {
                return Ok((
                    typ.deref()
                        .expect("&_ should never fail to be dereferenced"),
                    typed_literal,
                ));
            }

            match (type_suggestion, &**typ) {
                (
                    TypeSuggestion::UnsizedArray(_),
                    TyKind::SizedArray {
                        typ,
                        number_elements,
                    },
                ) => {
                    let typed_literal_sized_array_ref = make_reference(
                        scope,
                        exprs,
                        context.ctx.intern_ty(TyKind::SizedArray {
                            typ: *typ,
                            number_elements: *number_elements,
                        }),
                        typed_literal,
                        expression.span(),
                        context.ctx,
                    );
                    let typ = context.ctx.intern_ty(TyKind::UnsizedArray(*typ));
                    let id = scope.push(typ.take_ref(context.ctx));
                    exprs.push(TypedExpression::MakeUnsizedSlice(
                        expression.span(),
                        id,
                        typed_literal_sized_array_ref,
                        *number_elements,
                    ));
                    Ok((typ, TypedLiteral::Dynamic(id)))
                }
                _ => Ok((
                    typ,
                    make_reference(
                        scope,
                        exprs,
                        typ,
                        typed_literal,
                        expression.span(),
                        context.ctx,
                    ),
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
    exprs: &mut Vec<TypedExpression<'arena>>,
) -> Result<OffsetValue<'arena>, Diagnostic<'arena>> {
    let (typ, rhs) = typecheck_expression(
        context,
        module,
        scope,
        expression,
        exprs,
        TypeSuggestion::Number(NumberType::Usize),
    )?;
    if typ != default_types::usize {
        return Err(TypecheckingError::MismatchingType {
            expected: default_types::usize,
            found: typ,
            location: expression.span(),
        }
        .to_error());
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
    expressions: &mut Vec<TypedExpression<'arena>>,
    typ: Ty<'arena>,
    mut typed_literal: TypedLiteral<'arena>,
    loc: Span<'arena>,
    ctx: TypeCtx<'arena>,
) -> TypedLiteral<'arena> {
    match typed_literal {
        TypedLiteral::Void | TypedLiteral::Static(_) => {}
        TypedLiteral::Dynamic(v) => scope.make_stack_allocated(v),
        _ => {
            let lit_id = scope.push(typ);
            scope.make_stack_allocated(lit_id);
            expressions.push(TypedExpression::Literal(loc, lit_id, typed_literal));
            typed_literal = TypedLiteral::Dynamic(lit_id);
        }
    }

    let new_id = scope.push(typ.take_ref(ctx));
    expressions.push(TypedExpression::Reference(loc, new_id, typed_literal));
    TypedLiteral::Dynamic(new_id)
}

#[allow(clippy::result_large_err)]
fn copy_resolve_indexing<'arena>(
    context: &TypecheckingContext<'arena>,
    module: StoreKey<TypecheckedModule<'arena>>,
    scope: &mut Scopes<'arena>,
    expression: &Expression<'arena>,
    exprs: &mut Vec<TypedExpression<'arena>>,
    type_suggestion: TypeSuggestion<'arena>,
) -> Result<(Ty<'arena>, TypedLiteral<'arena>), Diagnostic<'arena>> {
    let ctx = context.ctx;
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
                **typ,
                TyKind::UnsizedArray { .. } | TyKind::SizedArray { .. } | TyKind::Tuple { .. }
            ) {
                return Err(TypecheckingError::IndexNonArrayElem(expression.span(), typ).to_error());
            }
            if !typ.has_refs() {
                let new_typ = match &**typ {
                    TyKind::SizedArray { typ, .. } => *typ,
                    TyKind::Tuple(elements) => match offset {
                        OffsetValue::Dynamic(_) => {
                            return Err(
                                TypecheckingError::TupleDynamicIndex(expression.span()).to_error()
                            );
                        }
                        OffsetValue::Static(v) if v >= elements.len() => {
                            return Err(TypecheckingError::TupleIndexOutOfBounds(
                                expression.span(),
                                elements.len(),
                                v,
                            )
                            .to_error());
                        }
                        OffsetValue::Static(v) => elements[v],
                    },
                    TyKind::UnsizedArray { .. } => panic!("unsized element without references"),
                    _ => unreachable!(),
                };
                let new_id = match offset {
                    OffsetValue::Static(offset) => {
                        let new_id = scope.push(new_typ);
                        exprs.push(TypedExpression::OffsetNonPointer(
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
                            make_reference(scope, exprs, typ, lhs, expression.span(), ctx);
                        let offset_id = scope.push(new_typ.take_ref(ctx));
                        let new_id = scope.push(new_typ);
                        exprs.push(TypedExpression::Offset(
                            expression.span(),
                            offset_id,
                            typed_lit_ref,
                            off,
                        ));
                        exprs.push(TypedExpression::Dereference(
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
                while typ.has_double_refs() {
                    typ = typ.deref().expect("dereferencing &_ should never fail");
                    let new_id = scope.push(typ);
                    exprs.push(TypedExpression::Dereference(expression.span(), new_id, lhs));
                    lhs = TypedLiteral::Dynamic(new_id);
                }
                let ty = match **typ {
                    TyKind::UnsizedArray(typ) | TyKind::SizedArray { typ, .. } => typ,
                    _ => unreachable!(),
                };
                let offset_id = scope.push(ty.take_ref(ctx));
                let value_id = scope.push(ty);
                exprs.push(TypedExpression::Offset(
                    expression.span(),
                    offset_id,
                    lhs,
                    offset,
                ));
                exprs.push(TypedExpression::Dereference(
                    expression.span(),
                    value_id,
                    TypedLiteral::Dynamic(offset_id),
                ));
                Ok((ty, TypedLiteral::Dynamic(value_id)))
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
                let needs_deref = typ_lhs.has_refs();
                while typ_lhs.has_double_refs() {
                    typ_lhs = typ_lhs.deref().expect("dereferencing &_ should never fail");
                    let new_id = scope.push(typ_lhs);
                    exprs.push(TypedExpression::Dereference(
                        expression.span(),
                        new_id,
                        typed_literal_lhs,
                    ));
                    typed_literal_lhs = TypedLiteral::Dynamic(new_id);
                }
                let offset = match **typ_lhs {
                    TyKind::Struct { struct_id, .. } => {
                        let structure = &context.structs.read()[struct_id];
                        match structure
                            .elements
                            .iter()
                            .enumerate()
                            .find(|(_, (v, _))| v == element_name)
                            .map(|(idx, (_, typ))| (idx, typ))
                        {
                            Some((idx, typ)) => {
                                typ_lhs = *typ;
                                idx
                            }
                            None => {
                                return Err(TypecheckingError::FieldNotFound(
                                    expression.span(),
                                    typ_lhs.without_ref(),
                                    element_name.symbol(),
                                )
                                .to_error());
                            }
                        }
                    }
                    _ => {
                        return Err(
                            TypecheckingError::AccessNonStructValue(*span, typ_lhs).to_error()
                        );
                    }
                };
                if !needs_deref {
                    let new_id = scope.push(typ_lhs);
                    if let TypedLiteral::Dynamic(id) = typed_literal_lhs {
                        scope.make_stack_allocated(id);
                    }
                    exprs.push(TypedExpression::OffsetNonPointer(
                        *span,
                        new_id,
                        typed_literal_lhs,
                        offset,
                    ));
                    typed_literal_lhs = TypedLiteral::Dynamic(new_id);
                } else {
                    let offset_id = scope.push(typ_lhs.take_ref(ctx));
                    let deref_id = scope.push(typ_lhs);
                    exprs.push(TypedExpression::Offset(
                        *span,
                        offset_id,
                        typed_literal_lhs,
                        OffsetValue::Static(offset),
                    ));
                    exprs.push(TypedExpression::Dereference(
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
