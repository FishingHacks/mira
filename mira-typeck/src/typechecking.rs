use std::{collections::HashSet, ops::Deref};

use crate::{
    TypedFunctionContract, TypedModule,
    context::TypeCtx,
    ir::{BlockId, ScopedIR},
    types::EMPTY_TYLIST,
};
use mira_common::store::StoreKey;
use mira_context::ErrorEmitted;
use mira_errors::Diagnostic;
use mira_lexer::NumberType;
use mira_parser::{
    ArrayLiteral, BinaryOp, Expression, LiteralValue, Path, Statement, TypeRef, UnaryOp,
    annotations::Annotations,
    module::{ModuleContext, ModuleScopeValue},
    std_annotations::{
        ext_vararg::ExternVarArg, intrinsic::IntrinsicAnnotation,
        llvm_intrinsic::LLVMIntrinsicAnnotation,
    },
};
use mira_spans::{ArenaList, Ident, Span};

use super::{
    TypecheckingError, TypeckCtx, TypedExternalFunction, TypedFunction, TypedStatic, TypedTrait,
    default_types,
    error::TypecheckingErrorEmitterExt,
    ir::{OffsetValue, TypedExpression, TypedLiteral},
    types::{FunctionType, Ty, TyKind, TypeSuggestion, with_refcount},
};

pub fn typecheck_static<'arena>(
    ctx: &TypeckCtx<'arena>,
    module_context: &ModuleContext<'arena>,
    static_id: StoreKey<TypedStatic<'arena>>,
) -> Result<(), ErrorEmitted> {
    let tc_module_reader = ctx.statics.read();
    let ty = tc_module_reader[static_id].ty;
    let span = tc_module_reader[static_id].span;
    let expr = {
        std::mem::replace(
            &mut module_context.statics.write()[static_id.cast()].value,
            LiteralValue::Void,
        )
    };

    let contract = TypedFunctionContract {
        name: Some(tc_module_reader[static_id].name),
        arguments: Vec::new(),
        return_type: default_types::never,
        annotations: Annotations::new(),
        span,
        module_id: tc_module_reader[static_id].module_id,
        generics: Vec::new(),
        comment: tc_module_reader[static_id].comment,
    };

    // TODO: Rename
    let fn_ctx = FnContext {
        tcx: ctx,
        contract: &contract,
    };

    match typecheck_expression(
        fn_ctx,
        &mut ScopedIR::new(std::iter::empty(), span),
        &Expression::Literal(expr, span),
        TypeSuggestion::from_type(ty),
    ) {
        Err(e) => return Err(ctx.emit_diag(e)),
        Ok((expr_typ, expr)) => {
            let tracker = ctx.track_errors();
            if !expr.is_entirely_literal() {
                ctx.emit_statics_need_to_be_literal(tc_module_reader[static_id].span);
            }
            if ty != expr_typ {
                ctx.emit_mismatching_type(tc_module_reader[static_id].span, ty, expr_typ);
            }
            ctx.errors_happened_res(tracker)?;
            drop(tc_module_reader);
            ctx.statics.write()[static_id].value = expr;
        }
    }
    Ok(())
}

#[allow(clippy::result_unit_err)]
pub fn typecheck_external_function<'arena>(
    ctx: &TypeckCtx<'arena>,
    module_context: &ModuleContext<'arena>,
    function_id: StoreKey<TypedExternalFunction<'arena>>,
) -> Result<(), ErrorEmitted> {
    inner_typecheck_function(ctx, module_context, function_id.cast(), true)
}

#[derive(Clone, Copy)]
struct FnContext<'ctx, 'tcx> {
    tcx: &'tcx TypeckCtx<'ctx>,
    contract: &'tcx TypedFunctionContract<'ctx>,
}

impl<'ctx> FnContext<'ctx, '_> {
    fn resolve_type(
        &self,
        module_id: StoreKey<TypedModule<'ctx>>,
        ty: &TypeRef<'ctx>,
    ) -> Result<Ty<'ctx>, Diagnostic<'ctx>> {
        self.tcx
            .resolve_type(module_id, ty, &self.contract.generics)
    }
}

impl<'ctx, 'tcx> Deref for FnContext<'ctx, 'tcx> {
    type Target = TypeckCtx<'ctx>;

    fn deref(&self) -> &'tcx Self::Target {
        self.tcx
    }
}

#[allow(clippy::result_unit_err)]
pub fn typecheck_function<'arena>(
    ctx: &TypeckCtx<'arena>,
    module_context: &ModuleContext<'arena>,
    function_id: StoreKey<TypedFunction<'arena>>,
) -> Result<(), ErrorEmitted> {
    inner_typecheck_function(ctx, module_context, function_id, false)
}

fn is_invalid_extern_return_ty(ty: &TyKind<'_>) -> bool {
    // unsized types are always invalid.
    if !ty.is_sized() {
        return true;
    }
    // primitive types are always valid.
    if ty.is_primitive() {
        return false;
    }
    // thin pointers are always vaoid
    if ty.has_refs() && ty.is_thin_ptr() {
        return false;
    }
    // everything else is invalid.
    true
}

// NOTE: function_id has to be StoreKey<TypedExternalFunction> if is_external is set to true.
fn inner_typecheck_function<'arena>(
    tc_ctx: &TypeckCtx<'arena>,
    module_context: &ModuleContext<'arena>,
    function_id: StoreKey<TypedFunction<'arena>>,
    is_external: bool,
) -> Result<(), ErrorEmitted> {
    let ext_fn_reader = module_context.external_functions.read();
    let fn_reader = module_context.functions.read();

    let typ_ext_fn_reader = tc_ctx.external_functions.read();
    let typ_fn_reader = tc_ctx.functions.read();

    let statement = if is_external {
        let Some(statement) = &ext_fn_reader[function_id.cast()].1 else {
            return Ok(());
        };
        statement
    } else {
        &fn_reader[function_id.cast()].1
    };

    let contract = if is_external {
        &typ_ext_fn_reader[function_id.cast()].0
    } else {
        let contract = &typ_fn_reader[function_id].0;
        if contract.annotations.has_annotation::<IntrinsicAnnotation>()
            || contract
                .annotations
                .has_annotation::<LLVMIntrinsicAnnotation>()
        {
            let span = contract.span;
            drop(typ_fn_reader);
            drop(typ_ext_fn_reader);
            tc_ctx.functions.write()[function_id]
                .1
                .append(TypedExpression::Unreachable(span));
            return Ok(());
        }
        contract
    };

    let ctx = FnContext {
        tcx: tc_ctx,
        contract,
    };

    let tracker = ctx.track_errors();
    if !contract.return_type.is_sized() {
        ctx.ctx
            .emit_unsized_return_type(contract.span, contract.return_type);
    }
    if is_external && is_invalid_extern_return_ty(&contract.return_type) {
        ctx.emit_invalid_extern_return_type(contract.span);
    }
    for (_, arg) in contract.arguments.iter().filter(|v| !v.1.is_sized()) {
        ctx.emit_unsized_argument(contract.span, *arg);
    }
    ctx.errors_happened_res(tracker)?;

    let mut ir = ScopedIR::new(
        contract
            .arguments
            .iter()
            .map(|&(name, ty)| (name, name.span(), ty)),
        ctx.combine_spans([contract.span, statement.span()]),
    );
    let always_returns = typecheck_block_inline(ctx, &mut ir, statement)?;
    if contract.return_type != default_types::void && !always_returns {
        ctx.ctx.emit_body_does_not_always_return(statement.span());
        return Err(ErrorEmitted);
    }
    // add an implicit `return;` at the end of the function
    if !always_returns {
        typecheck_statement(ctx, &mut ir, &Statement::Return(None, statement.span()))?;
    }

    drop(fn_reader);
    drop(ext_fn_reader);

    drop(typ_fn_reader);
    drop(typ_ext_fn_reader);

    if is_external {
        let mut ir = Some(ir.to_ir());
        std::mem::swap(
            &mut ir,
            &mut tc_ctx.external_functions.write()[function_id.cast()].1,
        );
        assert!(ir.is_none())
    } else {
        let mut ir = ir.to_ir();
        std::mem::swap(&mut ir, &mut tc_ctx.functions.write()[function_id].1);
        assert!(ir.is_empty());
    }
    Ok(())
}

fn typecheck_block_inline<'ctx>(
    ctx: FnContext<'ctx, '_>,
    ir: &mut ScopedIR<'ctx>,
    statement: &Statement<'ctx>,
) -> Result<bool, ErrorEmitted> {
    if let Statement::Block(stmts, _, _) = statement {
        for stmt in stmts {
            if let Ok(true) = typecheck_statement(ctx, ir, stmt) {
                if !matches!(statement, Statement::Return(..)) {
                    ir.append(TypedExpression::Unreachable(statement.span()));
                }
                return Ok(true);
            }
        }
        return Ok(false);
    }
    typecheck_statement(ctx, ir, statement)
}

/// Typechecks a statement that *acts* like a block, or a block. This means that
/// - `if(a) b = c;` is `if _0 { _1 = _2; }`
/// - `if(a) { b = c; }` is `if _0 { _1 = _2; }`, and not `if _0 { { _1 = _2 } }`
fn typecheck_quasi_block<'ctx>(
    ctx: FnContext<'ctx, '_>,
    ir: &mut ScopedIR<'ctx>,
    statement: &Statement<'ctx>,
) -> Result<(BlockId, bool), ErrorEmitted> {
    if let Statement::Block(statements, span, _) = statement {
        let tracker = ctx.track_errors();
        let (block, always_returns) = ir.with_new_block(*span, |ir| {
            let mut always_returns = false;
            for statement in statements.iter() {
                if let Ok(true) = typecheck_statement(ctx, ir, statement) {
                    always_returns = true;
                    if !matches!(statement, Statement::Return(..)) {
                        ir.append(TypedExpression::Unreachable(statement.span()));
                    }
                    break;
                }
            }
            always_returns
        });

        ctx.errors_happened_res(tracker)?;
        return Ok((block, always_returns));
    }
    let (block, res) = ir.with_new_block(statement.span(), |ir| {
        typecheck_statement(ctx, ir, statement)
    });
    Ok((block, res?))
}

/// Returns if the statement and if it always returns
fn typecheck_statement<'ctx>(
    ctx: FnContext<'ctx, '_>,
    ir: &mut ScopedIR<'ctx>,
    statement: &Statement<'ctx>,
) -> Result<bool, ErrorEmitted> {
    let tracker = ctx.track_errors();
    match statement {
        Statement::If {
            condition,
            if_stmt,
            else_stmt,
            span,
            annotations,
        } => {
            let expr_result = typecheck_expression(ctx, ir, condition, TypeSuggestion::Bool)
                .map_err(|v| ctx.emit_diag(v));

            let if_stmt_result = typecheck_quasi_block(ctx, ir, if_stmt);
            let else_stmt_result = if let Some(else_stmt) = else_stmt {
                typecheck_quasi_block(ctx, ir, else_stmt).map(|(blk, exits)| (Some(blk), exits))
            } else {
                Ok((None, false))
            };
            let (condition_ty, cond) = expr_result?;
            if condition_ty != default_types::bool {
                ctx.ctx
                    .emit_mismatching_type(*span, default_types::bool, condition_ty);
            }
            let (if_block, if_stmt_exits) = if_stmt_result?;
            let (else_block, else_stmt_exits) = else_stmt_result?;
            ctx.errors_happened_res(tracker)?;
            ir.append(TypedExpression::If {
                span: *span,
                cond,
                if_block,
                else_block,
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
            let (condition_body, condition_result) = ir.with_new_block(condition.span(), |ir| {
                typecheck_expression(ctx, ir, condition, TypeSuggestion::Bool)
            });
            let condition_result = condition_result.map_err(|diag| ctx.emit_diag(diag));

            let body_result = typecheck_quasi_block(ctx, ir, child);
            let (condition_ty, cond) = condition_result?;
            if condition_ty != default_types::bool {
                ctx.ctx
                    .emit_mismatching_type(*span, default_types::bool, condition_ty);
            }
            let (body, mut always_exits) = body_result?;
            ctx.errors_happened_res(tracker)?;

            // while (true) {} also never exits
            always_exits = always_exits || matches!(cond, TypedLiteral::Bool(true));
            ir.append(TypedExpression::While {
                span: *span,
                cond_block: condition_body,
                cond,
                body,
            });

            Ok(always_exits)
        }
        Statement::For { .. } => todo!("iterator (requires generics)"),
        Statement::Return(None, span) => {
            if ctx.contract.return_type == default_types::void {
                ir.append(TypedExpression::Return(*span, TypedLiteral::Void));
                Ok(true)
            } else {
                ctx.ctx
                    .emit_mismatching_type(*span, ctx.contract.return_type, default_types::void);
                Err(ErrorEmitted)
            }
        }
        Statement::Return(Some(expression), span) => {
            let (ty, typed_expression) = typecheck_expression(
                ctx,
                ir,
                expression,
                TypeSuggestion::from_type(ctx.contract.return_type),
            )
            .map_err(|diag| ctx.emit_diag(diag))?;
            if ty != ctx.contract.return_type {
                ctx.ctx
                    .emit_mismatching_type(expression.span(), ctx.contract.return_type, ty);
                return Err(ErrorEmitted);
            }
            ir.append(TypedExpression::Return(*span, typed_expression));
            Ok(true)
        }
        Statement::Block(_, span, annotations) => {
            let (block, always_returns) = typecheck_quasi_block(ctx, ir, statement)?;
            ir.append(TypedExpression::Block(*span, block, annotations.clone()));
            Ok(always_returns)
        }
        Statement::Var(var) => {
            let expected_typ = var
                .ty
                .as_ref()
                .map(|v| ctx.resolve_type(ctx.contract.module_id, v))
                .transpose()
                .map_err(|diag| ctx.emit_diag(diag))?;

            let (ty, expr) = typecheck_expression(
                ctx,
                ir,
                &var.value,
                expected_typ
                    .as_ref()
                    .copied()
                    .map(TypeSuggestion::from_type)
                    .unwrap_or_default(),
            )
            .map_err(|diag| ctx.emit_diag(diag))?;

            if let Some(expected_typ) = expected_typ
                && expected_typ != ty
            {
                ctx.ctx.emit_mismatching_type(var.span, expected_typ, ty);
                return Err(ErrorEmitted);
            }

            let value = ir.add_value(ty);
            ir.append(TypedExpression::Literal(var.value.span(), value, expr));

            ir.scope_value(var.name, value);
            ir.append(TypedExpression::DeclareVariable(
                var.span,
                value,
                ty,
                var.name.symbol(),
            ));
            ir.make_stack_allocated(value);
            Ok(false)
        }
        Statement::Expression(expression) => {
            typecheck_expression(ctx, ir, expression, Default::default())
                .map_err(|diag| ctx.emit_diag(diag))
                .map(|(ty, _)| ty == default_types::never)
        }
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
        | Statement::Static { .. }
        | Statement::BakedExternalFunction(..) => {
            unreachable!()
        }
    }
}

macro_rules! tc_res {
    (unary $ir:expr; $name:ident ($span:expr, $right_side:expr, $ty:expr)) => {{
        let ty = $ty;
        let id = $ir.add_value(ty);
        $ir.append(TypedExpression::$name($span, id, $right_side));
        Ok((ty, TypedLiteral::Dynamic(id)))
    }};

    (binary $ir:expr; $name:ident ($span:expr, $left_side:expr, $right_side:expr, $ty:expr)) => {{
        let ty = $ty;
        let id = $ir.add_value(ty);
        $ir.append(TypedExpression::$name($span, id, $left_side, $right_side));
        Ok((ty, TypedLiteral::Dynamic(id)))
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

fn typecheck_expression<'ctx>(
    ctx: FnContext<'ctx, '_>,
    ir: &mut ScopedIR<'ctx>,
    expression: &Expression<'ctx>,
    type_suggestion: TypeSuggestion<'ctx>,
) -> Result<(Ty<'ctx>, TypedLiteral<'ctx>), Diagnostic<'ctx>> {
    match expression {
        &Expression::Literal(ref literal_value, span) => match literal_value {
            LiteralValue::String(global_str) => {
                Ok((default_types::str_ref, TypedLiteral::String(*global_str)))
            }
            LiteralValue::Array(ArrayLiteral::Values(vec)) if vec.is_empty() => {
                let ty = match type_suggestion {
                    TypeSuggestion::UnsizedArray(_) | TypeSuggestion::Array(_) => type_suggestion
                        .to_type(&ctx)
                        .ok_or_else(|| TypecheckingError::CannotInferArrayType(span).to_error())?,
                    _ => return Err(TypecheckingError::CannotInferArrayType(span).to_error()),
                };
                Ok((ty, TypedLiteral::Array(ty, [].into())))
            }
            LiteralValue::Array(ArrayLiteral::CopyInitialized(value, amount)) => {
                let suggested_typ = match type_suggestion {
                    TypeSuggestion::UnsizedArray(v) | TypeSuggestion::Array(v) => *v,
                    _ => TypeSuggestion::Unknown,
                };
                let (ty, lit) = typecheck_expression(ctx, ir, value, suggested_typ)?;
                Ok((
                    ctx.intern_ty(TyKind::SizedArray {
                        ty,
                        number_elements: *amount,
                    }),
                    TypedLiteral::ArrayInit(ty, Box::new(lit), *amount),
                ))
            }
            LiteralValue::Array(ArrayLiteral::Values(vec)) => {
                let suggested_typ = match type_suggestion {
                    TypeSuggestion::UnsizedArray(v) | TypeSuggestion::Array(v) => *v,
                    _ => TypeSuggestion::Unknown,
                };
                let (ty, mut elements) = typecheck_expression(ctx, ir, &vec[0], suggested_typ)
                    .map(|(ty, lit)| (ty, vec![lit]))?;
                let suggested_typ = TypeSuggestion::from_type(ty);
                for expr in vec.iter().skip(1) {
                    let (el_typ, el_lit) =
                        typecheck_expression(ctx, ir, expr, suggested_typ.clone())?;
                    if el_typ != ty {
                        return Err(TypecheckingError::MismatchingType {
                            expected: ty,
                            found: el_typ,
                            span: expr.span(),
                        }
                        .to_error());
                    }
                    elements.push(el_lit);
                }
                let arr_typ = ctx.intern_ty(TyKind::SizedArray {
                    ty,
                    number_elements: vec.len(),
                });
                Ok((
                    arr_typ,
                    TypedLiteral::Array(ty, elements.into_boxed_slice()),
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
                    let (ty, val) = typecheck_expression(
                        ctx,
                        ir,
                        value,
                        suggested_element_types.get(i).cloned().unwrap_or_default(),
                    )?;
                    elements.push(val);
                    element_types.push(ty);
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
                    return Err(TypecheckingError::CannotInferAnonStructType(span).to_error());
                };

                let structure = &ctx.structs.read()[struct_id];
                // ensure there are no excessive values in the struct initialization
                for k in values.keys() {
                    if !structure.elements.iter().any(|v| v.0 == *k) {
                        return Err(TypecheckingError::NoSuchFieldFound {
                            span: values[k].0,
                            name: k.symbol(),
                        }
                        .to_error());
                    }
                }

                let mut elements = Vec::with_capacity(structure.elements.len());
                for (key, ty, _) in structure.elements.iter() {
                    let Some(&(span, ref expr)) = values.get(key) else {
                        return Err(TypecheckingError::MissingField {
                            span,
                            name: key.symbol(),
                        }
                        .to_error());
                    };
                    let (expr_typ, expr_lit) =
                        typecheck_expression(ctx, ir, expr, TypeSuggestion::from_type(*ty))?;
                    if *ty != expr_typ {
                        return Err(TypecheckingError::MismatchingType {
                            expected: *ty,
                            found: expr_typ,
                            span,
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
                let value = ctx
                    .typed_resolve_import(
                        ctx.contract.module_id,
                        &path.entries.iter().map(|v| v.0).collect::<Vec<_>>(),
                        span,
                        &mut HashSet::new(),
                    )
                    .map_err(|_| {
                        TypecheckingError::CannotFindValue(span, path.clone()).to_error()
                    })?;
                let ModuleScopeValue::Struct(struct_id) = value else {
                    return Err(TypecheckingError::CannotFindValue(span, path.clone()).to_error());
                };
                let structure = &ctx.structs.read()[struct_id.cast()];
                // ensure there are no excessive values in the struct initialization
                for k in values.keys() {
                    if !structure.elements.iter().any(|v| v.0 == *k) {
                        return Err(TypecheckingError::NoSuchFieldFound {
                            span: values[k].0,
                            name: k.symbol(),
                        }
                        .to_error());
                    }
                }

                let mut elements = Vec::with_capacity(structure.elements.len());
                for (key, ty, _) in structure.elements.iter() {
                    let Some(&(span, ref expr)) = values.get(key) else {
                        return Err(TypecheckingError::MissingField {
                            span,
                            name: key.symbol(),
                        }
                        .to_error());
                    };
                    let (expr_typ, expr_lit) =
                        typecheck_expression(ctx, ir, expr, TypeSuggestion::from_type(*ty))?;
                    if *ty != expr_typ {
                        return Err(TypecheckingError::MismatchingType {
                            expected: *ty,
                            found: expr_typ,
                            span,
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
                if path.entries.len() == 1
                    && path.entries[0].1.is_empty()
                    && let Some(id) = ir.get_scoped(&path.entries[0].0)
                {
                    return Ok((ir.get_ty(id), TypedLiteral::Dynamic(id)));
                }
                let mut generics = Vec::new();
                for (.., generic_value) in path.entries.iter() {
                    generics.extend_from_slice(generic_value);
                }

                let value = ctx
                    .typed_resolve_import(
                        ctx.contract.module_id,
                        &path.entries.iter().map(|v| v.0).collect::<Vec<_>>(),
                        span,
                        &mut HashSet::new(),
                    )
                    .map_err(|e| {
                        e.dismiss();
                        TypecheckingError::CannotFindValue(span, path.clone()).to_error()
                    })?;
                match value {
                    ModuleScopeValue::Function(id) => {
                        let reader = &ctx.functions.read()[id.cast()];
                        let return_type = reader.0.return_type;
                        let arguments = reader.0.arguments.iter().map(|v| v.1).collect::<Vec<_>>();
                        if reader.0.generics.len() != generics.len() {
                            return Err(TypecheckingError::MismatchingGenericCount(
                                span,
                                reader.0.generics.len(),
                                generics.len(),
                            )
                            .to_error());
                        }
                        let mut generic_types = Vec::with_capacity(generics.len());
                        for (i, ty) in generics.iter().enumerate() {
                            let ty = ctx.resolve_type(ctx.contract.module_id, ty)?;
                            if reader.0.generics[i].sized && !ty.is_sized() {
                                return Err(
                                    TypecheckingError::UnsizedForSizedGeneric(span, ty).to_error()
                                );
                            }
                            generic_types.push(ty);
                        }

                        let arguments = ctx.substitute(&generic_types, arguments);
                        let return_type = ctx.substitute(&generic_types, return_type);

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
                        let reader = &ctx.external_functions.read()[id.cast()];
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
                            return Err(TypecheckingError::UnexpectedGenerics { span }.to_error());
                        }
                        Ok((
                            ctx.statics.read()[id.cast()].ty,
                            TypedLiteral::Static(id.cast()),
                        ))
                    }
                    _ => Err(TypecheckingError::CannotFindValue(span, path.clone()).to_error()),
                }
            }
            LiteralValue::BakedAnonymousFunction(fn_id) => {
                let func = &ctx.functions.read()[fn_id.cast()].0;
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
            span,
            asm,
            volatile,
            output,
            registers,
            inputs,
        } => {
            let name = match output {
                TypeRef::Reference { type_name, .. } => Some(type_name.entries[0].0),
                _ => None,
            };
            // this should never fail unless this is an incompatible type (non-primitive)
            let output = ctx
                .resolve_type(ctx.contract.module_id, output)
                .ok()
                .filter(|v| (*v == default_types::void && name.is_none()) || v.is_asm_primitive())
                .ok_or_else(|| {
                    TypecheckingError::AsmNonNumericType(*span, name.unwrap().symbol()).to_error()
                })?;
            let mut typed_inputs = Vec::with_capacity(inputs.len());
            for (span, name) in inputs {
                let Some(id) = ir.get_scoped(name) else {
                    return Err(TypecheckingError::CannotFindValue(
                        *span,
                        Path::new(*name, Vec::new()),
                    )
                    .to_error());
                };
                let ty = ir.get_ty(id);
                if !ty.is_asm_primitive() {
                    return Err(TypecheckingError::AsmNonNumericTypeResolved(*span, ty).to_error());
                }
                typed_inputs.push(id);
            }
            let id = ir.add_value(output);
            ir.append(TypedExpression::Asm {
                span: *span,
                dst: id,
                inputs: typed_inputs.into_boxed_slice(),
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
            typecheck_take_ref(ctx, ir, right_side, type_suggestion)
        }
        Expression::Unary {
            operator,
            right_side,
            span,
        } => {
            let (ty, right_side) = typecheck_expression(ctx, ir, right_side, type_suggestion)?;
            match operator {
                UnaryOp::Plus if ty.is_int_like() => {
                    tc_res!(unary ir; Pos(*span, right_side, ty))
                }
                UnaryOp::Plus => Err(TypecheckingError::CannotPos(*span, ty).to_error()),
                UnaryOp::Minus if (ty.is_int_like() && !ty.is_unsigned()) || ty.is_float() => {
                    tc_res!(unary ir; Neg(*span, right_side, ty))
                }
                UnaryOp::Minus => Err(TypecheckingError::CannotNeg(*span, ty).to_error()),
                UnaryOp::LogicalNot if ty == default_types::bool => {
                    tc_res!(unary ir; LNot(*span, right_side, ty))
                }
                UnaryOp::LogicalNot => Err(TypecheckingError::CannotLNot(*span, ty).to_error()),
                UnaryOp::BitwiseNot if ty.is_int_like() || ty == default_types::bool => {
                    tc_res!(unary ir; BNot(*span, right_side, ty))
                }
                UnaryOp::BitwiseNot => Err(TypecheckingError::CannotBNot(*span, ty).to_error()),
                UnaryOp::Dereference => match ty.deref() {
                    Some(ty) => tc_res!(unary ir; Dereference(*span, right_side, ty)),
                    None => Err(TypecheckingError::CannotDeref(*span, ty).to_error()),
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
                let (ty, left_side) = typecheck_expression(ctx, ir, left_side, type_suggestion)?;
                let (typ_right, right_side) = typecheck_expression(
                    ctx,
                    ir,
                    right_side,
                    TypeSuggestion::Number(NumberType::Usize),
                )?;
                if !typ_right.is_int_like() || !typ_right.is_unsigned() {
                    return Err(
                        TypecheckingError::CannotShiftByNonUInt(*span, typ_right).to_error()
                    );
                }

                return match operator {
                    BinaryOp::LShift if ty.is_int_like() => {
                        tc_res!(binary ir; LShift(*span, left_side, right_side, ty))
                    }
                    BinaryOp::RShift if ty.is_int_like() => {
                        tc_res!(binary ir; RShift(*span, left_side, right_side, ty))
                    }

                    BinaryOp::LShift => Err(TypecheckingError::CannotShl(*span, ty).to_error()),
                    BinaryOp::RShift => Err(TypecheckingError::CannotShr(*span, ty).to_error()),
                    _ => unreachable!(),
                };
            }
            if matches!(operator, BinaryOp::LogicalAnd | BinaryOp::LogicalOr) {
                let span_left = left_side.span();
                let span_right = right_side.span();
                let (typ_left, left_side) =
                    typecheck_expression(ctx, ir, left_side, TypeSuggestion::Bool)?;
                if typ_left != default_types::bool {
                    return Err(TypecheckingError::MismatchingType {
                        span: span_left,
                        expected: default_types::bool,
                        found: typ_left,
                    }
                    .to_error());
                }
                let (rhs_block, right_res) = ir.with_new_block(right_side.span(), |ir| {
                    typecheck_expression(ctx, ir, right_side, TypeSuggestion::Bool)
                });
                let (typ_right, right_side) = right_res?;
                if typ_right != default_types::bool {
                    return Err(TypecheckingError::MismatchingType {
                        span: span_right,
                        expected: default_types::bool,
                        found: typ_right,
                    }
                    .to_error());
                }
                let span = span_left.combine_with([span_right], ctx.span_interner());

                let id = ir.add_value(default_types::bool);
                match operator {
                    BinaryOp::LogicalAnd => ir.append(TypedExpression::LAnd(
                        span, id, left_side, right_side, rhs_block,
                    )),
                    BinaryOp::LogicalOr => ir.append(TypedExpression::LOr(
                        span, id, left_side, right_side, rhs_block,
                    )),
                    _ => unreachable!(),
                }
                return Ok((default_types::bool, TypedLiteral::Dynamic(id)));
            }
            let (typ_left, left_side) = typecheck_expression(ctx, ir, left_side, type_suggestion)?;
            let (typ_right, right_side) =
                typecheck_expression(ctx, ir, right_side, TypeSuggestion::from_type(typ_left))?;
            if typ_left != typ_right {
                return Err(TypecheckingError::LhsNotRhs(*span, typ_left, typ_right).to_error());
            }
            let ty = typ_left;
            let span = *span;
            match operator {
                BinaryOp::Plus if ty.is_int_like() => {
                    tc_res!(binary ir; Add(span, left_side, right_side, ty))
                }
                BinaryOp::Minus if ty.is_int_like() => {
                    tc_res!(binary ir; Sub(span, left_side, right_side, ty))
                }
                BinaryOp::Multiply if ty.is_int_like() => {
                    tc_res!(binary ir; Mul(span, left_side, right_side, ty))
                }
                BinaryOp::Divide if ty.is_int_like() => {
                    tc_res!(binary ir; Div(span, left_side, right_side, ty))
                }
                BinaryOp::Modulo if ty.is_int_like() => {
                    tc_res!(binary ir; Mod(span, left_side, right_side, ty))
                }
                BinaryOp::BitwiseAnd if ty.is_int_like() || ty.is_bool() => {
                    tc_res!(binary ir; BAnd(span, left_side, right_side, ty))
                }
                BinaryOp::BitwiseOr if ty.is_int_like() || ty.is_bool() => {
                    tc_res!(binary ir; BOr(span, left_side, right_side, ty))
                }
                BinaryOp::BitwiseXor if ty.is_int_like() || ty.is_bool() => {
                    tc_res!(binary ir; BXor(span, left_side, right_side, ty))
                }
                BinaryOp::GreaterThan if ty.is_int_like() => {
                    tc_res!(binary ir; GreaterThan(span, left_side, right_side, default_types::bool))
                }
                BinaryOp::GreaterThanEq if ty.is_int_like() => {
                    tc_res!(binary ir; GreaterThanEq(span, left_side, right_side, default_types::bool))
                }
                BinaryOp::LessThan if ty.is_int_like() => {
                    tc_res!(binary ir; LessThan(span, left_side, right_side, default_types::bool))
                }
                BinaryOp::LessThanEq if ty.is_int_like() => {
                    tc_res!(binary ir; LessThanEq(span, left_side, right_side, default_types::bool))
                }
                BinaryOp::Equals if ty.is_int_like() || ty.is_bool() => {
                    tc_res!(binary ir; Eq(span, left_side, right_side, default_types::bool))
                }
                BinaryOp::NotEquals if ty.is_int_like() || ty.is_bool() => {
                    tc_res!(binary ir; Neq(span, left_side, right_side, default_types::bool))
                }

                BinaryOp::Plus => Err(TypecheckingError::CannotAdd(span, ty).to_error()),
                BinaryOp::Minus => Err(TypecheckingError::CannotSub(span, ty).to_error()),
                BinaryOp::Multiply => Err(TypecheckingError::CannotMul(span, ty).to_error()),
                BinaryOp::Divide => Err(TypecheckingError::CannotDiv(span, ty).to_error()),
                BinaryOp::Modulo => Err(TypecheckingError::CannotMod(span, ty).to_error()),
                BinaryOp::BitwiseAnd => Err(TypecheckingError::CannotBAnd(span, ty).to_error()),
                BinaryOp::BitwiseOr => Err(TypecheckingError::CannotBOr(span, ty).to_error()),
                BinaryOp::BitwiseXor => Err(TypecheckingError::CannotBXor(span, ty).to_error()),
                BinaryOp::GreaterThan
                | BinaryOp::GreaterThanEq
                | BinaryOp::LessThan
                | BinaryOp::LessThanEq => {
                    Err(TypecheckingError::CannotCompare(span, ty).to_error())
                }
                BinaryOp::Equals | BinaryOp::NotEquals => {
                    Err(TypecheckingError::CannotEq(span, ty).to_error())
                }
                BinaryOp::LogicalOr
                | BinaryOp::LogicalAnd
                | BinaryOp::RShift
                | BinaryOp::LShift => unreachable!(),
            }
        }
        Expression::FunctionCall {
            func, arguments, ..
        } => {
            let (ty, function_expr) = typecheck_expression(ctx, ir, func, TypeSuggestion::Unknown)?;
            let has_vararg = if let TypedLiteral::ExternalFunction(id) = function_expr {
                ctx.external_functions.read()[id]
                    .0
                    .annotations
                    .has_annotation::<ExternVarArg>()
            } else {
                false
            };
            let TyKind::Function(function_type) = &**ty.without_ref() else {
                return Err(TypecheckingError::TypeIsNotAFunction { span: func.span() }.to_error());
            };
            let mut typed_arguments = Vec::with_capacity(function_type.arguments.len());
            if arguments.len() < function_type.arguments.len() {
                return Err(TypecheckingError::MissingArguments { span: func.span() }.to_error());
            }
            if arguments.len() > function_type.arguments.len() && !has_vararg {
                return Err(TypecheckingError::TooManyArguments {
                    span: arguments[function_type.arguments.len().saturating_sub(1)].span(),
                }
                .to_error());
            }
            for (i, arg) in arguments.iter().enumerate() {
                let (ty, expr) = typecheck_expression(
                    ctx,
                    ir,
                    arg,
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
                if i < function_type.arguments.len() && ty != function_type.arguments[i] {
                    return Err(TypecheckingError::MismatchingType {
                        expected: function_type.arguments[i],
                        found: ty,
                        span: arguments[i].span(),
                    }
                    .to_error());
                }
                typed_arguments.push(expr);
            }

            if let TypedLiteral::Intrinsic(intrinsic, generics) = function_expr {
                let ty = function_type.return_type;
                let id = ir.add_value(ty);
                ir.append(TypedExpression::IntrinsicCall(
                    func.span(),
                    id,
                    intrinsic,
                    typed_arguments.into_boxed_slice(),
                    generics,
                ));
                Ok((ty, TypedLiteral::Dynamic(id)))
            } else if let TypedLiteral::LLVMIntrinsic(intrinsic) = function_expr {
                let ty = function_type.return_type;
                let id = ir.add_value(ty);
                ir.append(TypedExpression::LLVMIntrinsicCall(
                    func.span(),
                    id,
                    intrinsic,
                    typed_arguments.into_boxed_slice(),
                ));
                Ok((ty, TypedLiteral::Dynamic(id)))
            } else if let TypedLiteral::Function(fn_id, generics) = function_expr {
                let ty = function_type.return_type;
                let id = ir.add_value(ty);
                ir.append(TypedExpression::DirectCall(
                    func.span(),
                    id,
                    fn_id,
                    typed_arguments.into_boxed_slice(),
                    generics,
                ));
                Ok((ty, TypedLiteral::Dynamic(id)))
            } else if let TypedLiteral::ExternalFunction(fn_id) = function_expr {
                tc_res!(binary ir; DirectExternCall(func.span(), fn_id, typed_arguments.into_boxed_slice(), function_type.return_type))
            } else {
                tc_res!(binary ir; Call(func.span(), function_expr, typed_arguments.into_boxed_slice(), function_type.return_type))
            }
        }
        Expression::Indexing { .. } | Expression::MemberAccess { .. } => {
            copy_resolve_indexing(ctx, ir, expression, type_suggestion)
        }
        Expression::MemberCall {
            fn_name: identifier,
            lhs,
            arguments,
            ..
        } => typecheck_membercall(ctx, ir, lhs, identifier, arguments),
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
                    let (ty, lhs) =
                        typecheck_expression(ctx, ir, right_side, TypeSuggestion::Unknown)?;
                    let ty = ty.deref().ok_or_else(|| {
                        TypecheckingError::CannotDeref(right_side.span(), ty).to_error()
                    })?;

                    (ty, lhs)
                }
                _ => {
                    let (ty, lhs) = typecheck_expression(
                        ctx,
                        ir,
                        &Expression::Unary {
                            operator: UnaryOp::Reference,
                            span: *span,
                            right_side: left_side.clone(),
                        },
                        TypeSuggestion::Unknown,
                    )?;
                    (
                        ty.deref().expect("&_ should never fail to dereference"),
                        lhs,
                    )
                }
            };
            let (typ_rhs, rhs) =
                typecheck_expression(ctx, ir, right_side, TypeSuggestion::from_type(typ_lhs))?;

            if typ_lhs != typ_rhs {
                return Err(TypecheckingError::MismatchingType {
                    expected: typ_lhs,
                    found: typ_rhs,
                    span: *span,
                }
                .to_error());
            }
            ir.append(TypedExpression::StoreAssignment(*span, lhs, rhs));
            Ok((default_types::void, TypedLiteral::Void))
        }
        Expression::Range {
            left_side,
            right_side,
            ..
        } => {
            let (ty, _) = typecheck_expression(ctx, ir, left_side, type_suggestion)?;
            let (typ_rhs, _) =
                typecheck_expression(ctx, ir, right_side, TypeSuggestion::from_type(ty))?;
            if ty != typ_rhs {
                return Err(TypecheckingError::MismatchingType {
                    expected: ty,
                    found: typ_rhs,
                    span: right_side.span(),
                }
                .to_error());
            }

            unimplemented!("lang-items");
        }
        Expression::TypeCast {
            left_side,
            new_ty,
            span,
        } => {
            let (ty, lhs) = typecheck_expression(ctx, ir, left_side, type_suggestion)?;
            let new_ty = ctx.resolve_type(ctx.contract.module_id, new_ty)?;
            typecheck_cast(ir, ty, new_ty, lhs, *span, ctx)
        }
        Expression::TraitFunctionCall {
            ty,
            trait_path,
            fn_name,
            arguments,
            span,
        } => {
            let ty = ctx.resolve_type(ctx.contract.module_id, ty)?;
            let import = ctx.typed_resolve_import(
                ctx.contract.module_id,
                trait_path.as_slice(),
                trait_path.span,
                &mut HashSet::new(),
            )?;
            let ModuleScopeValue::Trait(trait_id) = import else {
                return Err(
                    TypecheckingError::CannotFindValue(*span, trait_path.to_normal_path())
                        .to_error(),
                );
            };
            let trait_id = trait_id.cast::<TypedTrait<'ctx>>();
            let trait_value = &ctx.traits.read()[trait_id];

            if !ty.implements(&[trait_id], &ctx) {
                return Err(TypecheckingError::MismatchingTraits(
                    *span,
                    ty,
                    vec![trait_value.name.symbol()],
                )
                .to_error());
            }

            let func = trait_value
                .functions
                .iter()
                .position(|(id, ..)| id == fn_name);
            let Some(func_id) = func else {
                return Err(TypecheckingError::CannotFindFunctionOnTrait {
                    span: fn_name.span(),
                    func_name: fn_name.symbol(),
                    trait_name: trait_value.name.symbol(),
                }
                .to_error());
            };
            let &(_, ref args, ret_ty, _, fn_span, _) = &trait_value.functions[func_id];

            let mut typed_args = Vec::with_capacity(arguments.len());

            if arguments.len() < args.len() {
                return Err(TypecheckingError::MissingArguments { span: fn_span }.to_error());
            }
            if arguments.len() > args.len() {
                let span = if arguments.is_empty() {
                    *span
                } else {
                    arguments[arguments.len() - 1].span()
                };
                return Err(TypecheckingError::TooManyArguments { span }.to_error());
            }

            for (i, arg) in arguments.iter().enumerate() {
                let (ty, expr) =
                    typecheck_expression(ctx, ir, arg, TypeSuggestion::from_type(args[i].1))?;
                if ty != args[i].1 {
                    return Err(TypecheckingError::MismatchingType {
                        span: arg.span(),
                        expected: args[i].1,
                        found: ty,
                    }
                    .to_error());
                }
                typed_args.push(expr);
            }

            let value_id = ir.add_value(ret_ty);
            ir.append(TypedExpression::TraitCall {
                span: *span,
                ty,
                trait_id,
                func: func_id,
                args: typed_args.into_boxed_slice(),
                dst: value_id,
            });
            Ok((ret_ty, TypedLiteral::Dynamic(value_id)))
        }
    }
}

fn typecheck_cast<'ctx>(
    ir: &mut ScopedIR<'ctx>,
    ty: Ty<'ctx>,
    new_ty: Ty<'ctx>,
    lhs: TypedLiteral<'ctx>,
    span: Span<'ctx>,
    ctx: FnContext<'ctx, '_>,
) -> Result<(Ty<'ctx>, TypedLiteral<'ctx>), Diagnostic<'ctx>> {
    if ty == new_ty {
        return Ok((new_ty, lhs));
    }

    let (ref_self, refless_ty) = ty.remove_refs();
    let (ref_other, refless_new_ty) = new_ty.remove_refs();
    match (&**refless_ty, &**refless_new_ty) {
        // &str -> &[u8]
        (TyKind::PrimitiveStr, TyKind::UnsizedArray(ty))
            if ref_self == ref_other && ref_self > 0 && *ty == default_types::u8 =>
        {
            let id = ir.add_value(new_ty);
            ir.append(TypedExpression::Alias(span, id, lhs));
            Ok((new_ty, TypedLiteral::Dynamic(id)))
        }
        // &str -> &u8
        (TyKind::PrimitiveStr, TyKind::PrimitiveU8) if ref_self == 1 && ref_other == 1 => {
            let id = ir.add_value(new_ty);
            ir.append(TypedExpression::StripMetadata(span, id, lhs));
            Ok((new_ty, TypedLiteral::Dynamic(id)))
        }
        // &T to &[T; 1]
        (
            _,
            TyKind::SizedArray {
                ty: ty_other,
                number_elements: 1,
            },
        ) if ref_other > 0
            && ref_self >= ref_other
            && *ty_other == ty.with_num_refs(ref_self - ref_other, **ctx) =>
        {
            let id = ir.add_value(new_ty);
            ir.append(TypedExpression::Alias(span, id, lhs));
            Ok((new_ty, TypedLiteral::Dynamic(id)))
        }
        // &T -> &dyn _
        (_, TyKind::DynType(trait_refs)) if ref_self > 0 && ref_other == 1 && ty.is_thin_ptr() => {
            let traits = trait_refs.iter().map(|v| v.0).collect::<Vec<_>>();
            let ty = ty.deref().expect("v should have a refcount of > 0");
            if !ty.implements(&traits, &ctx) {
                let trait_reader = ctx.traits.read();
                return Err(TypecheckingError::MismatchingTraits(
                    span,
                    ty,
                    traits
                        .iter()
                        .map(|v| trait_reader[*v].name.symbol())
                        .collect(),
                )
                .to_error());
            }
            let id = ir.add_value(new_ty);
            ir.append(TypedExpression::AttachVtable(
                span,
                id,
                lhs,
                (ty, traits.into_boxed_slice()),
            ));
            Ok((new_ty, TypedLiteral::Dynamic(id)))
        }
        // &[T; N] -> &[T]
        (
            TyKind::SizedArray {
                ty,
                number_elements,
            },
            TyKind::UnsizedArray(typ_other),
        ) if ty == typ_other && ref_self == 1 && ref_other == 1 => {
            let id = ir.add_value(new_ty);
            ir.append(TypedExpression::MakeUnsizedSlice(
                span,
                id,
                lhs,
                *number_elements,
            ));
            Ok((new_ty, TypedLiteral::Dynamic(id)))
        }
        // (&[T] -> &T)
        (TyKind::UnsizedArray(ty), _)
            if ref_self == 1 && ref_other == 1 && **new_ty == TyKind::Ref(*ty) =>
        {
            let id = ir.add_value(new_ty);
            ir.append(TypedExpression::StripMetadata(span, id, lhs));
            Ok((new_ty, TypedLiteral::Dynamic(id)))
        }
        // &&void -> &void
        (TyKind::PrimitiveVoid, TyKind::PrimitiveVoid) if ref_self > 0 && ref_other == 1 => {
            let id = ir.add_value(new_ty);
            ir.append(TypedExpression::Bitcast(span, id, lhs));
            Ok((new_ty, TypedLiteral::Dynamic(id)))
        }
        // &void -> usize
        (TyKind::PrimitiveVoid, TyKind::PrimitiveUSize) if ref_self == 1 && ref_other == 0 => {
            let id = ir.add_value(new_ty);
            ir.append(TypedExpression::PtrToInt(span, id, lhs));
            Ok((new_ty, TypedLiteral::Dynamic(id)))
        }
        // usize -> &void
        (TyKind::PrimitiveUSize, TyKind::PrimitiveVoid) if ref_self == 0 && ref_other == 1 => {
            let id = ir.add_value(new_ty);
            ir.append(TypedExpression::IntToPtr(span, id, lhs));
            Ok((new_ty, TypedLiteral::Dynamic(id)))
        }
        // fn to &void
        (TyKind::Function(_), TyKind::PrimitiveVoid) if ref_self == 0 && ref_other == 1 => {
            let id = ir.add_value(new_ty);
            ir.append(TypedExpression::Bitcast(span, id, lhs));
            Ok((new_ty, TypedLiteral::Dynamic(id)))
        }
        // &T to &void
        (_, TyKind::PrimitiveVoid) if ref_other > 0 && ref_self > 0 && ty.is_thin_ptr() => {
            let id = ir.add_value(new_ty);
            ir.append(TypedExpression::Bitcast(span, id, lhs));
            Ok((new_ty, TypedLiteral::Dynamic(id)))
        }
        // &void to &T
        (TyKind::PrimitiveVoid, _) if ref_self > 0 && ref_other > 0 && new_ty.is_thin_ptr() => {
            let id = ir.add_value(new_ty);
            ir.append(TypedExpression::Bitcast(span, id, lhs));
            Ok((new_ty, TypedLiteral::Dynamic(id)))
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
            let id = ir.add_value(new_ty);
            ir.append(TypedExpression::IntCast(span, id, lhs));
            Ok((new_ty, TypedLiteral::Dynamic(id)))
        }
        _ => Err(TypecheckingError::DisallowedCast(span, ty, new_ty).to_error()),
    }
}

#[allow(clippy::too_many_arguments)]
fn typecheck_dyn_membercall<'ctx>(
    ctx: FnContext<'ctx, '_>,
    ir: &mut ScopedIR<'ctx>,
    ident: &Ident<'ctx>,
    args: &[Expression<'ctx>],
    lhs: TypedLiteral<'ctx>,
    lhs_span: Span<'ctx>,
    trait_refs: ArenaList<'ctx, (StoreKey<TypedTrait<'ctx>>, Ident<'ctx>)>,
    num_references: u8,
) -> Result<(Ty<'ctx>, TypedLiteral<'ctx>), Diagnostic<'ctx>> {
    let trait_reader = ctx.traits.read();
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
            lhs_span,
            ident.symbol(),
            ctx.intern_ty(TyKind::DynType(trait_refs)),
        )
        .to_error());
    };
    drop(trait_reader);

    if arg_typs[0].1.without_ref() != default_types::self_ {
        return Err(TypecheckingError::InvalidDynTypeFunc(
            lhs_span,
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
            lhs_span,
            ident.symbol(),
            trait_name.symbol(),
        )
        .to_error());
    }

    let ty = ctx.intern_ty(TyKind::DynType(trait_refs));
    let mut ty = with_refcount(**ctx, ty, num_references);
    let mut lhs = lhs;
    while ty.has_double_refs() {
        ty = ty.deref().expect("dereferencing &_ should never fail");
        let id = ir.add_value(ctx.intern_ty_ref(&ty));
        ir.append(TypedExpression::Dereference(lhs_span, id, lhs));
        lhs = TypedLiteral::Dynamic(id);
    }

    let mut typed_args = Vec::with_capacity(args.len() + 1);
    typed_args.push(lhs);

    if args.len() < arg_typs.len() - 1 {
        return Err(TypecheckingError::MissingArguments { span: lhs_span }.to_error());
    }
    if args.len() > arg_typs.len() - 1 {
        return Err(TypecheckingError::TooManyArguments {
            span: args[arg_typs.len() - 1].span(),
        }
        .to_error());
    }

    for i in 0..args.len() {
        let (ty, lit) = typecheck_expression(
            ctx,
            ir,
            &args[i],
            TypeSuggestion::from_type(arg_typs[i + 1].1),
        )?;
        if ty != arg_typs[i + 1].1 {
            return Err(TypecheckingError::MismatchingType {
                expected: arg_typs.remove(i + 1).1,
                found: ty,
                span: args[i].span(),
            }
            .to_error());
        }
        typed_args.push(lit);
    }

    let is_void = return_ty == default_types::void || return_ty == default_types::never;
    let id = ir.add_value(return_ty);
    ir.append(TypedExpression::DynCall(
        lhs_span,
        id,
        typed_args.into_boxed_slice(),
        offset,
    ));
    if is_void {
        return Ok((return_ty, TypedLiteral::Void));
    }
    Ok((return_ty, TypedLiteral::Dynamic(id)))
}

fn typecheck_membercall<'ctx>(
    ctx: FnContext<'ctx, '_>,
    ir: &mut ScopedIR<'ctx>,
    lhs: &Expression<'ctx>,
    ident: &Ident<'ctx>,
    args: &[Expression<'ctx>],
) -> Result<(Ty<'ctx>, TypedLiteral<'ctx>), Diagnostic<'ctx>> {
    let (mut typ_lhs, mut typed_literal_lhs) =
        typecheck_take_ref(ctx, ir, lhs, TypeSuggestion::Unknown)?;
    let lhs_refcount = typ_lhs.refcount();
    match **typ_lhs.without_ref() {
        TyKind::DynType(trait_refs) if lhs_refcount > 0 => {
            return typecheck_dyn_membercall(
                ctx,
                ir,
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

    let function_reader = ctx.functions.read();
    let langitem_reader = ctx.lang_items.read();
    let struct_reader = ctx.structs.read();

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
            let new_id = ir.add_value(typ_lhs);
            ir.append(TypedExpression::Dereference(
                lhs.span(),
                new_id,
                typed_literal_lhs,
            ));
            typed_literal_lhs = TypedLiteral::Dynamic(new_id);
        } else {
            typed_literal_lhs = make_reference(ir, typ_lhs, typed_literal_lhs, lhs.span(), ctx.ctx);
            typ_lhs = typ_lhs.take_ref(ctx.ctx);
        }
    }

    let mut typed_arguments = Vec::with_capacity(function.0.arguments.len() + 1);
    typed_arguments.push(typed_literal_lhs);
    if args.len() < function.0.arguments.len() - 1 {
        return Err(TypecheckingError::MissingArguments { span: lhs.span() }.to_error());
    }
    if args.len() > function.0.arguments.len() - 1 {
        return Err(TypecheckingError::TooManyArguments {
            span: args[function.0.arguments.len() - 1].span(),
        }
        .to_error());
    }
    for (i, (_, arg)) in function.0.arguments.iter().skip(1).enumerate() {
        let (ty, expr) = typecheck_expression(
            ctx,
            ir,
            &args[i + 1], // skip one argument for the `self` argument
            TypeSuggestion::from_type(*arg),
        )?;
        if ty != function.0.arguments[i + 1].1 {
            return Err(TypecheckingError::MismatchingType {
                expected: function.0.arguments[i + 1].1,
                found: ty,
                span: args[i].span(),
            }
            .to_error());
        }
        typed_arguments.push(expr);
    }

    let call_id = ir.add_value(function.0.return_type);
    ir.append(TypedExpression::DirectCall(
        lhs.span(),
        call_id,
        function_id,
        typed_arguments.into_boxed_slice(),
        EMPTY_TYLIST,
    ));

    Ok((function.0.return_type, TypedLiteral::Dynamic(call_id)))
}

fn typecheck_take_ref<'ctx>(
    ctx: FnContext<'ctx, '_>,
    ir: &mut ScopedIR<'ctx>,
    expression: &Expression<'ctx>,
    type_suggestion: TypeSuggestion<'ctx>,
) -> Result<(Ty<'ctx>, TypedLiteral<'ctx>), Diagnostic<'ctx>> {
    match expression {
        //&*_1 => _1
        Expression::Unary {
            operator,
            right_side,
            ..
        } if *operator == UnaryOp::Dereference => {
            typecheck_expression(ctx, ir, right_side, type_suggestion)
        }
        _ => {
            let (ty, expr) = ref_resolve_indexing(ctx, ir, expression, type_suggestion, true)?;
            Ok((ty.take_ref(ctx.ctx), expr))
        }
    }
}

// NOTE: the actual type of the value is a reference on top of the returned type, but we don't do
// that as that would require additional computations. The reference is as such implicit but has to
// be added when pushing the type onto the scope (e.g. during auto deref)
fn ref_resolve_indexing<'ctx>(
    ctx: FnContext<'ctx, '_>,
    ir: &mut ScopedIR<'ctx>,
    expression: &Expression<'ctx>,
    type_suggestion: TypeSuggestion<'ctx>,
    increase_ref: bool,
) -> Result<(Ty<'ctx>, TypedLiteral<'ctx>), Diagnostic<'ctx>> {
    match expression {
        Expression::MemberAccess {
            left_side,
            index,
            span,
        } => {
            let (mut typ_lhs, mut typed_literal_lhs) =
                ref_resolve_indexing(ctx, ir, left_side, TypeSuggestion::Unknown, false)?;
            for element_name in index {
                while typ_lhs.has_refs() {
                    let old_typ_lhs = typ_lhs;
                    typ_lhs = typ_lhs
                        .deref()
                        .expect("&_ should never fail to dereference");
                    let id = ir.add_value(old_typ_lhs);
                    ir.append(TypedExpression::Dereference(
                        expression.span(),
                        id,
                        typed_literal_lhs,
                    ));
                    typed_literal_lhs = TypedLiteral::Dynamic(id);
                }
                assert!(!typ_lhs.has_refs(), "non-zero refcount after auto-deref");
                match **typ_lhs {
                    TyKind::Struct { struct_id, .. } => {
                        let structure = &ctx.structs.read()[struct_id];
                        match structure
                            .elements
                            .iter()
                            .enumerate()
                            .find(|(_, (v, _, _))| v == element_name)
                            .map(|(idx, (_, ty, _))| (idx, ty))
                        {
                            Some((idx, ty)) => {
                                typ_lhs = *ty;
                                let new_val = ir.add_value(typ_lhs.take_ref(ctx.ctx));
                                ir.append(TypedExpression::Offset(
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
                ctx,
                ir,
                left_side,
                TypeSuggestion::Array(Box::new(type_suggestion)),
                false,
            )?;
            while typ_lhs.has_refs() {
                typ_lhs = typ_lhs
                    .deref()
                    .expect("&_ should never fail to dereference");
                let id = ir.add_value(typ_lhs);
                ir.append(TypedExpression::Dereference(
                    expression.span(),
                    id,
                    typed_literal_lhs,
                ));
                typed_literal_lhs = TypedLiteral::Dynamic(id);
            }
            assert!(!typ_lhs.has_refs(), "non-zero refcount after auto-deref");
            let offset = indexing_resolve_rhs(ctx, ir, right_side)?;
            let ty = match &**typ_lhs {
                TyKind::SizedArray { ty, .. } | TyKind::UnsizedArray(ty) => *ty,
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

            let id = ir.add_value(ty.take_ref(ctx.ctx));
            ir.append(TypedExpression::Offset(
                expression.span(),
                id,
                typed_literal_lhs,
                offset,
            ));
            Ok((ty, TypedLiteral::Dynamic(id)))
        }
        _ => {
            let (ty, typed_literal) =
                typecheck_expression(ctx, ir, expression, type_suggestion.clone())?;

            if let TypeSuggestion::UnsizedArray(_) = type_suggestion
                && let TyKind::Ref(ty) = **ty
                && let TyKind::SizedArray {
                    number_elements,
                    ty,
                } = **ty
            {
                let ty = ctx.ctx.intern_ty(TyKind::UnsizedArray(ty));
                let id = ir.add_value(ty.take_ref(ctx.ctx));
                ir.append(TypedExpression::MakeUnsizedSlice(
                    expression.span(),
                    id,
                    typed_literal,
                    number_elements,
                ));
                return Ok((ty, TypedLiteral::Dynamic(id)));
            }

            if ty.has_refs() && !increase_ref {
                return Ok((
                    ty.deref().expect("&_ should never fail to be dereferenced"),
                    typed_literal,
                ));
            }

            match (type_suggestion, &**ty) {
                (
                    TypeSuggestion::UnsizedArray(_),
                    &TyKind::SizedArray {
                        ty,
                        number_elements,
                    },
                ) => {
                    let typed_literal_sized_array_ref = make_reference(
                        ir,
                        ctx.ctx.intern_ty(TyKind::SizedArray {
                            ty,
                            number_elements,
                        }),
                        typed_literal,
                        expression.span(),
                        ctx.ctx,
                    );
                    let ty = ctx.ctx.intern_ty(TyKind::UnsizedArray(ty));
                    let id = ir.add_value(ty.take_ref(ctx.ctx));
                    ir.append(TypedExpression::MakeUnsizedSlice(
                        expression.span(),
                        id,
                        typed_literal_sized_array_ref,
                        number_elements,
                    ));
                    Ok((ty, TypedLiteral::Dynamic(id)))
                }
                _ => Ok((
                    ty,
                    make_reference(ir, ty, typed_literal, expression.span(), ctx.ctx),
                )),
            }
        }
    }
}

fn indexing_resolve_rhs<'ctx>(
    ctx: FnContext<'ctx, '_>,
    ir: &mut ScopedIR<'ctx>,
    expression: &Expression<'ctx>,
) -> Result<OffsetValue, Diagnostic<'ctx>> {
    let (ty, rhs) = typecheck_expression(
        ctx,
        ir,
        expression,
        TypeSuggestion::Number(NumberType::Usize),
    )?;
    if ty != default_types::usize {
        return Err(TypecheckingError::MismatchingType {
            expected: default_types::usize,
            found: ty,
            span: expression.span(),
        }
        .to_error());
    }
    match rhs {
        TypedLiteral::Dynamic(v) => Ok(OffsetValue::Dynamic(v)),
        TypedLiteral::USize(v) => Ok(OffsetValue::Static(v)),
        _ => unreachable!(),
    }
}

/// ty - the type before the reference (as in, ty is the type of the typed_literal, the type of
/// the returned type literal is ty.take_ref())
fn make_reference<'arena>(
    ir: &mut ScopedIR<'arena>,
    ty: Ty<'arena>,
    mut typed_literal: TypedLiteral<'arena>,
    span: Span<'arena>,
    ctx: TypeCtx<'arena>,
) -> TypedLiteral<'arena> {
    match typed_literal {
        TypedLiteral::Void | TypedLiteral::Static(_) => {}
        TypedLiteral::Dynamic(v) => ir.make_stack_allocated(v),
        _ => {
            let lit_id = ir.add_value(ty);
            ir.make_stack_allocated(lit_id);
            ir.append(TypedExpression::Literal(span, lit_id, typed_literal));
            typed_literal = TypedLiteral::Dynamic(lit_id);
        }
    }

    let new_id = ir.add_value(ty.take_ref(ctx));
    ir.append(TypedExpression::Reference(span, new_id, typed_literal));
    TypedLiteral::Dynamic(new_id)
}

fn copy_resolve_indexing<'ctx>(
    ctx: FnContext<'ctx, '_>,
    ir: &mut ScopedIR<'ctx>,
    expression: &Expression<'ctx>,
    type_suggestion: TypeSuggestion<'ctx>,
) -> Result<(Ty<'ctx>, TypedLiteral<'ctx>), Diagnostic<'ctx>> {
    match expression {
        Expression::Indexing {
            left_side,
            right_side,
            ..
        } => {
            let offset = indexing_resolve_rhs(ctx, ir, right_side)?;
            let (ty, lhs) = copy_resolve_indexing(
                ctx,
                ir,
                left_side,
                TypeSuggestion::Array(Box::new(type_suggestion)),
            )?;
            if !matches!(
                **ty,
                TyKind::UnsizedArray { .. } | TyKind::SizedArray { .. } | TyKind::Tuple { .. }
            ) {
                return Err(TypecheckingError::IndexNonArrayElem(expression.span(), ty).to_error());
            }
            if !ty.has_refs() {
                let new_ty = match &**ty {
                    TyKind::SizedArray { ty, .. } => *ty,
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
                        let new_id = ir.add_value(new_ty);
                        ir.append(TypedExpression::OffsetNonPointer(
                            expression.span(),
                            new_id,
                            lhs,
                            offset,
                        ));
                        new_id
                    }
                    off @ OffsetValue::Dynamic(_) => {
                        if let TypedLiteral::Dynamic(lhs) = lhs {
                            ir.make_stack_allocated(lhs);
                        }
                        let typed_lit_ref = make_reference(ir, ty, lhs, expression.span(), **ctx);
                        let offset_id = ir.add_value(new_ty.take_ref(**ctx));
                        let new_id = ir.add_value(new_ty);
                        ir.append(TypedExpression::Offset(
                            expression.span(),
                            offset_id,
                            typed_lit_ref,
                            off,
                        ));
                        ir.append(TypedExpression::Dereference(
                            expression.span(),
                            new_id,
                            TypedLiteral::Dynamic(offset_id),
                        ));
                        new_id
                    }
                };
                Ok((new_ty, TypedLiteral::Dynamic(new_id)))
            } else {
                let mut ty = ty;
                let mut lhs = lhs;
                while ty.has_double_refs() {
                    ty = ty.deref().expect("dereferencing &_ should never fail");
                    let new_id = ir.add_value(ty);
                    ir.append(TypedExpression::Dereference(expression.span(), new_id, lhs));
                    lhs = TypedLiteral::Dynamic(new_id);
                }
                let ty = match **ty {
                    TyKind::UnsizedArray(ty) | TyKind::SizedArray { ty, .. } => ty,
                    _ => unreachable!(),
                };
                let offset_id = ir.add_value(ty.take_ref(**ctx));
                let value_id = ir.add_value(ty);
                ir.append(TypedExpression::Offset(
                    expression.span(),
                    offset_id,
                    lhs,
                    offset,
                ));
                ir.append(TypedExpression::Dereference(
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
            let (mut typ_lhs, mut typed_literal_lhs) =
                copy_resolve_indexing(ctx, ir, left_side, TypeSuggestion::Unknown)?;
            for element_name in index {
                let needs_deref = typ_lhs.has_refs();
                while typ_lhs.has_double_refs() {
                    typ_lhs = typ_lhs.deref().expect("dereferencing &_ should never fail");
                    let new_id = ir.add_value(typ_lhs);
                    ir.append(TypedExpression::Dereference(
                        expression.span(),
                        new_id,
                        typed_literal_lhs,
                    ));
                    typed_literal_lhs = TypedLiteral::Dynamic(new_id);
                }
                let offset = match **typ_lhs {
                    TyKind::Struct { struct_id, .. } => {
                        let structure = &ctx.structs.read()[struct_id];
                        match structure
                            .elements
                            .iter()
                            .enumerate()
                            .find(|(_, (v, _, _))| v == element_name)
                            .map(|(idx, (_, ty, _))| (idx, ty))
                        {
                            Some((idx, ty)) => {
                                typ_lhs = *ty;
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
                    let new_id = ir.add_value(typ_lhs);
                    if let TypedLiteral::Dynamic(id) = typed_literal_lhs {
                        ir.make_stack_allocated(id);
                    }
                    ir.append(TypedExpression::OffsetNonPointer(
                        *span,
                        new_id,
                        typed_literal_lhs,
                        offset,
                    ));
                    typed_literal_lhs = TypedLiteral::Dynamic(new_id);
                } else {
                    let offset_id = ir.add_value(typ_lhs.take_ref(**ctx));
                    let deref_id = ir.add_value(typ_lhs);
                    ir.append(TypedExpression::Offset(
                        *span,
                        offset_id,
                        typed_literal_lhs,
                        OffsetValue::Static(offset),
                    ));
                    ir.append(TypedExpression::Dereference(
                        *span,
                        deref_id,
                        TypedLiteral::Dynamic(offset_id),
                    ));
                    typed_literal_lhs = TypedLiteral::Dynamic(deref_id);
                }
            }
            Ok((typ_lhs, typed_literal_lhs))
        }
        _ => typecheck_expression(ctx, ir, expression, type_suggestion),
    }
}
