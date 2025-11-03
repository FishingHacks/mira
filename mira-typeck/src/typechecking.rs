use std::{collections::HashSet, ops::Deref};

use crate::{
    CommonFunction, TypedFunctionContract,
    ir::{BlockId, ScopedIR},
    types::EMPTY_TYLIST,
};
use mira_context::ErrorEmitted;
use mira_errors::Diagnostic;
use mira_lexer::NumberType;
use mira_parser::{
    ArrayLiteral, BinaryOp, Expression, If, LiteralValue, Path, Statement, TypeRef, UnaryOp, While,
    annotations::Annotations,
    module::{
        ExternalFunctionId, FunctionId, ModuleContext, ModuleId, ModuleScopeValue, StaticId,
        TraitId,
    },
    std_annotations::{
        ext_vararg::ExternVarArg, intrinsic::IntrinsicAnnotation,
        llvm_intrinsic::LLVMIntrinsicAnnotation,
    },
};
use mira_spans::{ArenaList, Ident, Span};

use super::{
    TypecheckingError, TypeckCtx, default_types,
    error::TypecheckingErrorEmitterExt,
    ir::{OffsetValue, TypedExpression, TypedLiteral},
    types::{FunctionType, Ty, TyKind, TypeSuggestion, with_refcount},
};

mod private {
    use super::*;
    use crate::ir::ValueId;

    pub(super) struct TcCtx<'ctx, 'tcx> {
        ir: ScopedIR<'ctx>,
        /// a list of reversed deferred statements
        pub rev_deferred_exprs: Vec<TypedExpression<'ctx>>,
        deferred_always_return: bool,
        is_deferred: bool,
        pub fn_ctx: FnContext<'ctx, 'tcx>,
    }

    impl<'ctx, 'tcx> TcCtx<'ctx, 'tcx> {
        pub(super) fn new(ir: ScopedIR<'ctx>, fn_ctx: FnContext<'ctx, 'tcx>) -> Self {
            Self {
                ir,
                rev_deferred_exprs: Vec::new(),
                fn_ctx,
                is_deferred: false,
                deferred_always_return: false,
            }
        }
    }

    impl<'ctx> TcCtx<'ctx, '_> {
        pub(super) fn into_ir(self) -> ScopedIR<'ctx> {
            self.ir
        }

        pub(super) fn make_stack_allocated(&mut self, value: ValueId) {
            self.ir.make_stack_allocated(value);
        }

        pub(super) fn add_value(&mut self, ty: Ty<'ctx>) -> ValueId {
            self.ir.add_value(ty)
        }

        pub(super) fn get_ty(&self, value: ValueId) -> Ty<'ctx> {
            self.ir.get_ty(value)
        }

        /// creates a block and sets the current block to it, calls `f`, and restores the current block after f exits.
        pub(super) fn with_new_block<R>(
            &mut self,
            span: Span<'ctx>,
            f: impl FnOnce(&mut Self) -> R,
        ) -> (BlockId, R) {
            let old = self.ir.current_block;
            let block = self.ir.create_block(span);
            self.goto(block);
            let v = f(self);
            self.goto(old);
            (block, v)
        }

        pub(super) fn goto(&mut self, block: BlockId) {
            self.ir.goto(block)
        }

        pub(super) fn append(&mut self, expr: TypedExpression<'ctx>) {
            if self.is_deferred {
                self.rev_deferred_exprs.push(expr);
            } else {
                self.ir.append(expr);
            }
        }

        /// Puts all deferred statements like before a return and returns true if one of the
        /// deferred statements always returns.
        pub(super) fn put_all_deferred(&mut self) -> bool {
            assert!(!self.is_deferred);
            for expr in self.rev_deferred_exprs.iter().rev().cloned() {
                self.ir.append(expr);
            }
            self.deferred_always_return
        }

        /// returns if the statement always returns.
        pub(super) fn append_deferred(
            &mut self,
            stmt: &Statement<'ctx>,
        ) -> Result<(), ErrorEmitted> {
            assert!(!self.is_deferred);
            let current_deferred = self.rev_deferred_exprs.len();
            self.is_deferred = true;

            let res = match stmt {
                Statement::DeferredBlock(_, span, annotations) => {
                    match typecheck_quasi_block(self, stmt) {
                        Ok((block, always_returns)) => {
                            self.append(TypedExpression::Block(*span, block, annotations.clone()));
                            Ok(always_returns)
                        }
                        Err(e) => Err(e),
                    }
                }
                Statement::DeferredExpr(expr) => {
                    match typecheck_expression(self, expr, Default::default()) {
                        Err(e) => Err(self.emit_diag(e)),
                        Ok((ty, _)) if ty == default_types::never => {
                            self.append(TypedExpression::Unreachable(expr.span()));
                            Ok(true)
                        }
                        Ok(_) => Ok(false),
                    }
                }
                Statement::DeferredIf(v) => typecheck_if(self, v),
                Statement::DeferredWhile(v) => typecheck_while(self, v),
                Statement::DeferredFor(_) => todo!("for loops (iterators)"),
                _ => unreachable!("deferred only"),
            };

            if self.deferred_always_return {
                // if the deferred always return (== have an expression evaluating to !), truncate
                // the newly generated exprs, because they'll never run anyway (and the backend
                // expects the last expression of a block to always be an always returning
                // expression).
                self.rev_deferred_exprs.truncate(current_deferred);
            } else {
                // reverse the expressions of this statement, because they're gonna be placed  in
                // opposite order. This achieves that the expressions of one statement are all in
                // order, but the groups are in opposite order from the way they were deferred.
                //
                // ```
                // defer 4;
                // rev_deferred:
                // _0 = 4;
                //
                // deferred:
                // _0 = 4;
                //
                // defer 1 + 2 + 3;
                //
                // rev_deferred (without this reverse):
                // _0 = 4;
                // _1 = 1 + 2
                // _2 = _1 + 3
                //
                // deferred (without this reverse):
                // _2 = _1 + 3
                // _1 = 1 + 2
                // _0 = 4;
                //
                // rev_deferred (with this reverse):
                // _0 = 4;
                // _2 = _1 + 3
                // _1 = 1 + 2
                //
                // deferred (with this reverse):
                // _1 = 1 + 2
                // _2 = _1 + 3
                // _0 = 4;
                // ```
                self.rev_deferred_exprs[current_deferred..].reverse();
            }
            self.is_deferred = false;
            if res? {
                self.deferred_always_return = true;
            }
            Ok(())
        }

        pub(super) fn scope_value(&mut self, ident: Ident<'ctx>, value: ValueId) {
            self.ir.scope_value(ident, value)
        }

        pub(super) fn get_scoped(&self, i: &Ident<'ctx>) -> Option<ValueId> {
            self.ir.get_scoped(i)
        }

        // does the same as `with_scope_block`, except handling defers.
        // This is used when we're in a defer block already, where defers *never* happen.
        fn with_scope_block_no_defer<E>(
            &mut self,
            f: impl FnOnce(&mut Self) -> Result<bool, E>,
            span: Span<'ctx>,
        ) -> Result<(BlockId, bool), E> {
            assert!(self.is_deferred);

            let old = self.ir.current_block;
            let block = self.ir.create_block(span);
            self.goto(block);

            self.ir.push_scope();

            let res = f(self);
            self.ir.pop_scope();
            self.goto(old);
            Ok((block, res?))
        }

        /// pushes a scope and creates a new block, calls f, runs all deferred things inside of the
        /// block, pops the scope, goes to the previous block, and returns the block and if the
        /// deferred things always return.
        ///
        /// If f returns true, always returns is always true and the deferred expressions won't be
        /// put at the end of the block. This means f's return value should be if the block's
        /// contents always return or not.
        pub(super) fn with_scope_block<E>(
            &mut self,
            f: impl FnOnce(&mut Self) -> Result<bool, E>,
            span: Span<'ctx>,
        ) -> Result<(BlockId, bool), E> {
            if self.is_deferred {
                return self.with_scope_block_no_defer(f, span);
            }
            assert!(!self.is_deferred);

            let old = self.ir.current_block;
            let block = self.ir.create_block(span);
            self.goto(block);

            self.ir.push_scope();

            let current_deferred = self.rev_deferred_exprs.len();
            let previous_deferred_always_return = self.deferred_always_return;
            let res = f(self);

            match res {
                Err(e) => {
                    // truncate to remove this scope's deferred values, reset the
                    // deferred_always_return value, pop the scope and go back to the previous block.
                    self.rev_deferred_exprs.truncate(current_deferred);
                    self.deferred_always_return = previous_deferred_always_return;
                    self.ir.pop_scope();
                    self.goto(old);

                    Err(e)
                }
                Ok(true) => {
                    // truncate to remove this scope's deferred values, reset the
                    // deferred_always_return value, pop the scope and go back to the previous block.
                    self.rev_deferred_exprs.truncate(current_deferred);
                    self.deferred_always_return = previous_deferred_always_return;
                    self.ir.pop_scope();
                    self.goto(old);

                    Ok((block, true))
                }
                Ok(false) => {
                    for expr in self.rev_deferred_exprs[current_deferred..].iter().rev() {
                        // clone is fine here, because these exprs should only ever be reachable by one
                        // codepath.
                        self.ir.append(expr.clone());
                    }
                    self.rev_deferred_exprs.truncate(current_deferred);
                    let deferred_always_return = self.deferred_always_return;
                    self.deferred_always_return = previous_deferred_always_return;
                    self.ir.pop_scope();
                    self.goto(old);

                    Ok((block, deferred_always_return))
                }
            }
        }
    }

    impl<'ctx, 'tcx> Deref for TcCtx<'ctx, 'tcx> {
        type Target = FnContext<'ctx, 'tcx>;

        fn deref(&self) -> &Self::Target {
            &self.fn_ctx
        }
    }
}
use private::TcCtx;

pub fn typecheck_static<'arena>(
    ctx: &TypeckCtx<'arena>,
    module_context: &ModuleContext<'arena>,
    static_id: StaticId,
) -> Result<(), ErrorEmitted> {
    let tc_module_reader = ctx.statics.read();
    let ty = tc_module_reader[static_id].ty;
    let span = tc_module_reader[static_id].span;
    let expr = {
        std::mem::replace(
            &mut module_context.statics.write()[static_id].value,
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

    let mut ctx = TcCtx::new(ScopedIR::new(std::iter::empty(), span), fn_ctx);

    match typecheck_expression(
        &mut ctx,
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
    function_id: ExternalFunctionId,
) -> Result<(), ErrorEmitted> {
    inner_typecheck_function(ctx, module_context, CommonFunction::External(function_id))
}

#[derive(Clone, Copy)]
struct FnContext<'ctx, 'tcx> {
    tcx: &'tcx TypeckCtx<'ctx>,
    contract: &'tcx TypedFunctionContract<'ctx>,
}

impl<'ctx> FnContext<'ctx, '_> {
    fn resolve_type(
        &self,
        module_id: ModuleId,
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
    function_id: FunctionId,
) -> Result<(), ErrorEmitted> {
    inner_typecheck_function(ctx, module_context, CommonFunction::Normal(function_id))
}

fn inner_typecheck_function<'arena>(
    tc_ctx: &TypeckCtx<'arena>,
    module_context: &ModuleContext<'arena>,
    function: CommonFunction,
) -> Result<(), ErrorEmitted> {
    let ext_fn_reader = module_context.external_functions.read();
    let fn_reader = module_context.functions.read();

    let typ_ext_fn_reader = tc_ctx.external_functions.read();
    let typ_fn_reader = tc_ctx.functions.read();

    let statement = match function {
        CommonFunction::Normal(id) => &fn_reader[id].1,
        CommonFunction::External(id) => {
            let Some(statement) = &ext_fn_reader[id].1 else {
                return Ok(());
            };
            statement
        }
    };
    let contract = match function {
        CommonFunction::External(id) => &typ_ext_fn_reader[id].0,
        CommonFunction::Normal(id) => {
            let contract = &typ_fn_reader[id].0;
            if contract.annotations.has_annotation::<IntrinsicAnnotation>()
                || contract
                    .annotations
                    .has_annotation::<LLVMIntrinsicAnnotation>()
            {
                let span = contract.span;
                drop(typ_fn_reader);
                drop(typ_ext_fn_reader);
                drop(fn_reader);
                drop(ext_fn_reader);

                tc_ctx.functions.write()[id]
                    .1
                    .append(TypedExpression::Unreachable(span));
                return Ok(());
            }
            contract
        }
    };

    let fn_ctx = FnContext {
        tcx: tc_ctx,
        contract,
    };
    let tracker = fn_ctx.track_errors();
    if !contract.return_type.is_sized() {
        fn_ctx
            .ctx
            .emit_unsized_return_type(contract.span, contract.return_type);
    }
    for (_, arg) in contract.arguments.iter().filter(|v| !v.1.is_sized()) {
        fn_ctx.emit_unsized_argument(contract.span, *arg);
    }
    fn_ctx.errors_happened_res(tracker)?;

    let ir = ScopedIR::new(
        contract
            .arguments
            .iter()
            .map(|&(name, ty)| (name, name.span(), ty)),
        fn_ctx.combine_spans([contract.span, statement.span()]),
    );
    let mut ctx = TcCtx::new(ir, fn_ctx);

    let always_returns = typecheck_block_inline(&mut ctx, statement)?;
    if contract.return_type != default_types::void && !always_returns {
        fn_ctx
            .ctx
            .emit_body_does_not_always_return(statement.span());
        return Err(ErrorEmitted);
    }
    // add an implicit `return;` at the end of the function
    if !always_returns {
        typecheck_statement(&mut ctx, &Statement::Return(None, statement.span()))?;
    }
    let mut ir = ctx.into_ir().into_ir();

    drop(fn_reader);
    drop(ext_fn_reader);

    drop(typ_fn_reader);
    drop(typ_ext_fn_reader);

    match function {
        CommonFunction::External(id) => {
            let mut ir = Some(ir);
            std::mem::swap(&mut ir, &mut tc_ctx.external_functions.write()[id].1);
            assert!(ir.is_none())
        }
        CommonFunction::Normal(id) => {
            std::mem::swap(&mut ir, &mut tc_ctx.functions.write()[id].1);
            assert!(ir.is_empty());
        }
    }

    Ok(())
}

fn typecheck_block_inline<'ctx>(
    ctx: &mut TcCtx<'ctx, '_>,
    statement: &Statement<'ctx>,
) -> Result<bool, ErrorEmitted> {
    if let Statement::Block(stmts, _, _) = statement {
        for stmt in stmts {
            if let Ok(true) = typecheck_statement(ctx, stmt) {
                return Ok(true);
            }
        }
        return Ok(false);
    }
    typecheck_statement(ctx, statement)
}

/// Typechecks a statement that *acts* like a block, or a block. This means that
/// - `if(a) b = c;` is `if _0 { _1 = _2; }`
/// - `if(a) { b = c; }` is `if _0 { _1 = _2; }`, and not `if _0 { { _1 = _2 } }`
fn typecheck_quasi_block<'ctx>(
    ctx: &mut TcCtx<'ctx, '_>,
    statement: &Statement<'ctx>,
) -> Result<(BlockId, bool), ErrorEmitted> {
    ctx.with_scope_block(
        |ctx| {
            if let Statement::Block(statements, _, _) | Statement::DeferredBlock(statements, _, _) =
                statement
            {
                let tracker = ctx.track_errors();
                let mut always_returns = false;
                for statement in statements.iter() {
                    if let Ok(true) = typecheck_statement(ctx, statement) {
                        always_returns = true;
                        break;
                    }
                }

                ctx.errors_happened_res(tracker)?;
                return Ok(always_returns);
            }
            typecheck_statement(ctx, statement)
        },
        statement.span(),
    )
}

/// Returns if the statement and if it always returns
fn typecheck_if<'ctx>(
    ctx: &mut TcCtx<'ctx, '_>,
    If {
        condition,
        if_stmt,
        else_stmt,
        span,
        annotations,
    }: &If<'ctx>,
) -> Result<bool, ErrorEmitted> {
    let tracker = ctx.track_errors();
    let expr_result =
        typecheck_expression(ctx, condition, TypeSuggestion::Bool).map_err(|v| ctx.emit_diag(v));

    let if_stmt_result = typecheck_quasi_block(ctx, if_stmt);
    let else_stmt_result = if let Some(else_stmt) = else_stmt {
        typecheck_quasi_block(ctx, else_stmt).map(|(blk, exits)| (Some(blk), exits))
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
    ctx.append(TypedExpression::If {
        span: *span,
        cond,
        if_block,
        else_block,
        annotations: annotations.clone(),
    });
    Ok(if_stmt_exits && else_stmt_exits)
}

/// Returns if the statement and if it always returns
fn typecheck_while<'ctx>(
    ctx: &mut TcCtx<'ctx, '_>,
    While {
        condition,
        child,
        span,
        ..
    }: &While<'ctx>,
) -> Result<bool, ErrorEmitted> {
    let tracker = ctx.track_errors();

    let (condition_body, condition_result) = ctx.with_new_block(condition.span(), |ctx| {
        typecheck_expression(ctx, condition, TypeSuggestion::Bool)
    });
    let condition_result = condition_result.map_err(|diag| ctx.emit_diag(diag));

    let body_result = typecheck_quasi_block(ctx, child);
    let (condition_ty, cond) = condition_result?;
    if condition_ty != default_types::bool {
        ctx.ctx
            .emit_mismatching_type(*span, default_types::bool, condition_ty);
    }
    let (body, mut always_exits) = body_result?;
    ctx.errors_happened_res(tracker)?;

    // while (true) {} also never exits
    always_exits = always_exits || matches!(cond, TypedLiteral::Bool(true));
    ctx.append(TypedExpression::While {
        span: *span,
        cond_block: condition_body,
        cond,
        body,
    });

    Ok(always_exits)
}

/// Returns if the statement always returns
fn typecheck_statement<'ctx>(
    ctx: &mut TcCtx<'ctx, '_>,
    statement: &Statement<'ctx>,
) -> Result<bool, ErrorEmitted> {
    match statement {
        Statement::DeferredExpr(_)
        | Statement::DeferredIf(_)
        | Statement::DeferredWhile(_)
        | Statement::DeferredBlock(_, _, _)
        | Statement::DeferredFor { .. } => ctx.append_deferred(statement).map(|()| false),

        Statement::If(v) => typecheck_if(ctx, v),
        Statement::While(v) => typecheck_while(ctx, v),
        Statement::For { .. } => todo!("iterator (requires generics)"),
        Statement::Return(None, span) => match ctx.put_all_deferred() {
            true => Ok(true),
            false if ctx.contract.return_type == default_types::void => {
                ctx.append(TypedExpression::Return(*span, None));
                Ok(true)
            }
            false => {
                ctx.ctx
                    .emit_mismatching_type(*span, ctx.contract.return_type, default_types::void);
                Err(ErrorEmitted)
            }
        },
        Statement::Return(Some(expression), span) => {
            let (ty, typed_expression) = typecheck_expression(
                ctx,
                expression,
                TypeSuggestion::from_type(ctx.contract.return_type),
            )
            .map_err(|diag| ctx.emit_diag(diag))?;

            if ty == default_types::never {
                ctx.append(TypedExpression::Unreachable(*span));
                return Ok(true);
            }

            if ty != ctx.contract.return_type {
                ctx.ctx
                    .emit_mismatching_type(expression.span(), ctx.contract.return_type, ty);
                return Err(ErrorEmitted);
            }

            if ctx.put_all_deferred() {
                return Ok(true);
            }

            let ret_val = match typed_expression {
                _ if ty.is_voidlike() => None,
                // using the id here is fine because we only return a ValueId because we want the
                // return value to *always* be alloca'd, not because it represents a new value.
                TypedLiteral::Dynamic(id) => {
                    ctx.make_stack_allocated(id);
                    Some(id)
                }
                TypedLiteral::Void => None,
                lit => {
                    let id = ctx.add_value(ty);
                    ctx.make_stack_allocated(id);
                    ctx.append(TypedExpression::Literal(expression.span(), id, lit));
                    Some(id)
                }
            };
            ctx.append(TypedExpression::Return(*span, ret_val));

            Ok(true)
        }
        Statement::Block(_, span, annotations) => {
            let (block, always_returns) = typecheck_quasi_block(ctx, statement)?;
            ctx.append(TypedExpression::Block(*span, block, annotations.clone()));
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

            let value = ctx.add_value(ty);
            ctx.append(TypedExpression::Literal(var.value.span(), value, expr));

            ctx.scope_value(var.name, value);
            ctx.append(TypedExpression::DeclareVariable(
                var.span,
                value,
                ty,
                var.name.symbol(),
            ));
            ctx.make_stack_allocated(value);
            Ok(false)
        }
        Statement::Expr(expr) => match typecheck_expression(ctx, expr, Default::default()) {
            Err(e) => Err(ctx.emit_diag(e)),
            Ok((ty, _)) if ty == default_types::never => {
                ctx.append(TypedExpression::Unreachable(expr.span()));
                Ok(true)
            }
            Ok(_) => Ok(false),
        },

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
        Statement::None => unreachable!("none statement"),
    }
}

macro_rules! tc_res {
    (unary $ctx:expr; $name:ident ($span:expr, $right_side:expr, $ty:expr)) => {{
        let ty = $ty;
        let id = $ctx.add_value(ty);
        $ctx.append(TypedExpression::$name($span, id, $right_side));
        Ok((ty, TypedLiteral::Dynamic(id)))
    }};

    (binary $ctx:expr; $name:ident ($span:expr, $left_side:expr, $right_side:expr, $ty:expr)) => {{
        let ty = $ty;
        let id = $ctx.add_value(ty);
        $ctx.append(TypedExpression::$name($span, id, $left_side, $right_side));
        Ok((ty, TypedLiteral::Dynamic(id)))
    }};
}

fn signed_number_to_literal<'arena>(
    v: i64,
    number_type: NumberType,
    expected: TypeSuggestion,
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
    expected: TypeSuggestion,
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
    expected: TypeSuggestion,
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
    ctx: &mut TcCtx<'ctx, '_>,
    expression: &Expression<'ctx>,
    type_suggestion: TypeSuggestion,
) -> Result<(Ty<'ctx>, TypedLiteral<'ctx>), Diagnostic<'ctx>> {
    match expression {
        &Expression::Literal(ref literal_value, span) => match literal_value {
            LiteralValue::String(global_str) => {
                Ok((default_types::str_ref, TypedLiteral::String(*global_str)))
            }
            LiteralValue::Array(ArrayLiteral::Values(vec)) if vec.is_empty() => {
                let ty = match type_suggestion {
                    TypeSuggestion::UnsizedArray(_) | TypeSuggestion::Array(_) => type_suggestion
                        .to_type(ctx)
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
                let (ty, lit) = typecheck_expression(ctx, value, suggested_typ)?;
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
                let (ty, mut elements) = typecheck_expression(ctx, &vec[0], suggested_typ)
                    .map(|(ty, lit)| (ty, vec![lit]))?;
                let suggested_typ = TypeSuggestion::from_type(ty);
                for expr in vec.iter().skip(1) {
                    let (el_typ, el_lit) = typecheck_expression(ctx, expr, suggested_typ.clone())?;
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

                let structure = &ctx.fn_ctx.tcx.structs.read()[struct_id];
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
                        typecheck_expression(ctx, expr, TypeSuggestion::from_type(*ty))?;
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
                let structure = &ctx.fn_ctx.tcx.structs.read()[struct_id];
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
                        typecheck_expression(ctx, expr, TypeSuggestion::from_type(*ty))?;
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
                    && let Some(id) = ctx.get_scoped(&path.entries[0].0)
                {
                    return Ok((ctx.get_ty(id), TypedLiteral::Dynamic(id)));
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
                        let reader = &ctx.functions.read()[id];
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
                            TypedLiteral::Function(id, ctx.intern_tylist(&generic_types))
                        };
                        Ok((ctx.intern_ty(TyKind::Function(function_ty)), literal))
                    }
                    ModuleScopeValue::ExternalFunction(id) => {
                        let reader = &ctx.external_functions.read()[id];
                        let function_typ = FunctionType {
                            return_type: reader.0.return_type,
                            arguments: ctx.intern_tylist(
                                &reader.0.arguments.iter().map(|v| v.1).collect::<Vec<_>>(),
                            ),
                        };
                        Ok((
                            ctx.intern_ty(TyKind::Function(function_typ)),
                            TypedLiteral::ExternalFunction(id),
                        ))
                    }
                    ModuleScopeValue::Static(id) => {
                        if !generics.is_empty() {
                            return Err(TypecheckingError::UnexpectedGenerics { span }.to_error());
                        }
                        Ok((ctx.statics.read()[id].ty, TypedLiteral::Static(id)))
                    }
                    _ => Err(TypecheckingError::CannotFindValue(span, path.clone()).to_error()),
                }
            }
            &LiteralValue::BakedAnonymousFunction(fn_id) => {
                let func = &ctx.functions.read()[fn_id].0;
                let fn_typ = FunctionType {
                    return_type: func.return_type,
                    arguments: ctx
                        .intern_tylist(&func.arguments.iter().map(|(_, v)| *v).collect::<Vec<_>>()),
                };
                Ok((
                    ctx.intern_ty(TyKind::Function(fn_typ)),
                    TypedLiteral::Function(fn_id, EMPTY_TYLIST),
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
                let Some(id) = ctx.get_scoped(name) else {
                    return Err(TypecheckingError::CannotFindValue(
                        *span,
                        Path::new(*name, Vec::new()),
                    )
                    .to_error());
                };
                let ty = ctx.get_ty(id);
                if !ty.is_asm_primitive() {
                    return Err(TypecheckingError::AsmNonNumericTypeResolved(*span, ty).to_error());
                }
                typed_inputs.push(id);
            }
            let id = ctx.add_value(output);
            ctx.append(TypedExpression::Asm {
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
            span,
        } if *operator == UnaryOp::Reference => {
            typecheck_take_ref(ctx, right_side, type_suggestion, *span)
        }
        Expression::Unary {
            operator,
            right_side,
            span,
        } => {
            let (ty, right_side) = typecheck_expression(ctx, right_side, type_suggestion)?;
            match operator {
                UnaryOp::Plus if ty.is_int_like() => {
                    tc_res!(unary ctx; Pos(*span, right_side, ty))
                }
                UnaryOp::Plus => Err(TypecheckingError::CannotPos(*span, ty).to_error()),
                UnaryOp::Minus if (ty.is_int_like() && !ty.is_unsigned()) || ty.is_float() => {
                    tc_res!(unary ctx; Neg(*span, right_side, ty))
                }
                UnaryOp::Minus => Err(TypecheckingError::CannotNeg(*span, ty).to_error()),
                UnaryOp::LogicalNot if ty == default_types::bool => {
                    tc_res!(unary ctx; LNot(*span, right_side, ty))
                }
                UnaryOp::LogicalNot => Err(TypecheckingError::CannotLNot(*span, ty).to_error()),
                UnaryOp::BitwiseNot if ty.is_int_like() || ty == default_types::bool => {
                    tc_res!(unary ctx; BNot(*span, right_side, ty))
                }
                UnaryOp::BitwiseNot => Err(TypecheckingError::CannotBNot(*span, ty).to_error()),
                UnaryOp::Dereference => match ty.deref() {
                    Some(ty) => tc_res!(unary ctx; Dereference(*span, right_side, ty)),
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
                let (ty, left_side) = typecheck_expression(ctx, left_side, type_suggestion)?;
                let (typ_right, right_side) = typecheck_expression(
                    ctx,
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
                        tc_res!(binary ctx; LShift(*span, left_side, right_side, ty))
                    }
                    BinaryOp::RShift if ty.is_int_like() => {
                        tc_res!(binary ctx; RShift(*span, left_side, right_side, ty))
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
                    typecheck_expression(ctx, left_side, TypeSuggestion::Bool)?;
                if typ_left != default_types::bool {
                    return Err(TypecheckingError::MismatchingType {
                        span: span_left,
                        expected: default_types::bool,
                        found: typ_left,
                    }
                    .to_error());
                }
                let (rhs_block, right_res) = ctx.with_new_block(right_side.span(), |ctx| {
                    typecheck_expression(ctx, right_side, TypeSuggestion::Bool)
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

                let id = ctx.add_value(default_types::bool);
                match operator {
                    BinaryOp::LogicalAnd => ctx.append(TypedExpression::LAnd(
                        span, id, left_side, right_side, rhs_block,
                    )),
                    BinaryOp::LogicalOr => ctx.append(TypedExpression::LOr(
                        span, id, left_side, right_side, rhs_block,
                    )),
                    _ => unreachable!(),
                }
                return Ok((default_types::bool, TypedLiteral::Dynamic(id)));
            }
            let (typ_left, left_side) = typecheck_expression(ctx, left_side, type_suggestion)?;
            let (typ_right, right_side) =
                typecheck_expression(ctx, right_side, TypeSuggestion::from_type(typ_left))?;
            if typ_left != typ_right {
                return Err(TypecheckingError::LhsNotRhs(*span, typ_left, typ_right).to_error());
            }
            let ty = typ_left;
            let span = *span;
            match operator {
                BinaryOp::Plus if ty.is_int_like() => {
                    tc_res!(binary ctx; Add(span, left_side, right_side, ty))
                }
                BinaryOp::Minus if ty.is_int_like() => {
                    tc_res!(binary ctx; Sub(span, left_side, right_side, ty))
                }
                BinaryOp::Multiply if ty.is_int_like() => {
                    tc_res!(binary ctx; Mul(span, left_side, right_side, ty))
                }
                BinaryOp::Divide if ty.is_int_like() => {
                    tc_res!(binary ctx; Div(span, left_side, right_side, ty))
                }
                BinaryOp::Modulo if ty.is_int_like() => {
                    tc_res!(binary ctx; Mod(span, left_side, right_side, ty))
                }
                BinaryOp::BitwiseAnd if ty.is_int_like() || ty.is_bool() => {
                    tc_res!(binary ctx; BAnd(span, left_side, right_side, ty))
                }
                BinaryOp::BitwiseOr if ty.is_int_like() || ty.is_bool() => {
                    tc_res!(binary ctx; BOr(span, left_side, right_side, ty))
                }
                BinaryOp::BitwiseXor if ty.is_int_like() || ty.is_bool() => {
                    tc_res!(binary ctx; BXor(span, left_side, right_side, ty))
                }
                BinaryOp::GreaterThan if ty.is_int_like() => {
                    tc_res!(binary ctx; GreaterThan(span, left_side, right_side, default_types::bool))
                }
                BinaryOp::GreaterThanEq if ty.is_int_like() => {
                    tc_res!(binary ctx; GreaterThanEq(span, left_side, right_side, default_types::bool))
                }
                BinaryOp::LessThan if ty.is_int_like() => {
                    tc_res!(binary ctx; LessThan(span, left_side, right_side, default_types::bool))
                }
                BinaryOp::LessThanEq if ty.is_int_like() => {
                    tc_res!(binary ctx; LessThanEq(span, left_side, right_side, default_types::bool))
                }
                BinaryOp::Equals if ty.is_int_like() || ty.is_bool() => {
                    tc_res!(binary ctx; Eq(span, left_side, right_side, default_types::bool))
                }
                BinaryOp::NotEquals if ty.is_int_like() || ty.is_bool() => {
                    tc_res!(binary ctx; Neq(span, left_side, right_side, default_types::bool))
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
            let (ty, function_expr) = typecheck_expression(ctx, func, TypeSuggestion::Unknown)?;
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
                let id = ctx.add_value(ty);
                ctx.append(TypedExpression::IntrinsicCall(
                    func.span(),
                    id,
                    intrinsic,
                    typed_arguments.into_boxed_slice(),
                    generics,
                ));
                Ok((ty, TypedLiteral::Dynamic(id)))
            } else if let TypedLiteral::LLVMIntrinsic(intrinsic) = function_expr {
                let ty = function_type.return_type;
                let id = ctx.add_value(ty);
                ctx.append(TypedExpression::LLVMIntrinsicCall(
                    func.span(),
                    id,
                    intrinsic,
                    typed_arguments.into_boxed_slice(),
                ));
                Ok((ty, TypedLiteral::Dynamic(id)))
            } else if let TypedLiteral::Function(fn_id, generics) = function_expr {
                let ty = function_type.return_type;
                let id = ctx.add_value(ty);
                ctx.append(TypedExpression::DirectCall(
                    func.span(),
                    id,
                    fn_id,
                    typed_arguments.into_boxed_slice(),
                    generics,
                ));
                Ok((ty, TypedLiteral::Dynamic(id)))
            } else if let TypedLiteral::ExternalFunction(fn_id) = function_expr {
                tc_res!(binary ctx; DirectExternCall(func.span(), fn_id, typed_arguments.into_boxed_slice(), function_type.return_type))
            } else {
                tc_res!(binary ctx; Call(func.span(), function_expr, typed_arguments.into_boxed_slice(), function_type.return_type))
            }
        }
        Expression::Indexing { .. } | Expression::MemberAccess { .. } => {
            let (ty, lit) = new_index(ctx, expression, type_suggestion)?;
            let ty = ty.deref().unwrap();
            let dst = ctx.add_value(ty);
            ctx.append(TypedExpression::Dereference(expression.span(), dst, lit));
            Ok((ty, TypedLiteral::Dynamic(dst)))
        }
        Expression::MemberCall {
            fn_name: identifier,
            lhs,
            arguments,
            ..
        } => typecheck_membercall(ctx, lhs, identifier, arguments),
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
                    let (ty, lhs) = typecheck_expression(ctx, right_side, TypeSuggestion::Unknown)?;
                    let ty = ty.deref().ok_or_else(|| {
                        TypecheckingError::CannotDeref(right_side.span(), ty).to_error()
                    })?;

                    (ty, lhs)
                }
                _ => {
                    let (ty, lhs) = typecheck_expression(
                        ctx,
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
                typecheck_expression(ctx, right_side, TypeSuggestion::from_type(typ_lhs))?;

            if typ_lhs != typ_rhs {
                return Err(TypecheckingError::MismatchingType {
                    expected: typ_lhs,
                    found: typ_rhs,
                    span: *span,
                }
                .to_error());
            }
            ctx.append(TypedExpression::StoreAssignment(*span, lhs, rhs));
            Ok((default_types::void, TypedLiteral::Void))
        }
        Expression::Range {
            left_side,
            right_side,
            ..
        } => {
            let (ty, _) = typecheck_expression(ctx, left_side, type_suggestion)?;
            let (typ_rhs, _) =
                typecheck_expression(ctx, right_side, TypeSuggestion::from_type(ty))?;
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
            let (ty, lhs) = typecheck_expression(ctx, left_side, type_suggestion)?;
            let new_ty = ctx.resolve_type(ctx.contract.module_id, new_ty)?;
            typecheck_cast(ctx, ty, new_ty, lhs, *span)
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
            let trait_value = &ctx.fn_ctx.tcx.traits.read()[trait_id];

            if !ty.implements(&[trait_id], ctx) {
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
                    typecheck_expression(ctx, arg, TypeSuggestion::from_type(args[i].1))?;
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

            let value_id = ctx.add_value(ret_ty);
            ctx.append(TypedExpression::TraitCall {
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
    ctx: &mut TcCtx<'ctx, '_>,
    ty: Ty<'ctx>,
    new_ty: Ty<'ctx>,
    lhs: TypedLiteral<'ctx>,
    span: Span<'ctx>,
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
            let id = ctx.add_value(new_ty);
            ctx.append(TypedExpression::Alias(span, id, lhs));
            Ok((new_ty, TypedLiteral::Dynamic(id)))
        }
        // &str -> &u8
        (TyKind::PrimitiveStr, TyKind::PrimitiveU8) if ref_self == 1 && ref_other == 1 => {
            let id = ctx.add_value(new_ty);
            ctx.append(TypedExpression::StripMetadata(span, id, lhs));
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
            && *ty_other == ty.with_num_refs(ref_self - ref_other, ctx.ctx) =>
        {
            let id = ctx.add_value(new_ty);
            ctx.append(TypedExpression::Alias(span, id, lhs));
            Ok((new_ty, TypedLiteral::Dynamic(id)))
        }
        // &T -> &dyn _
        (_, TyKind::DynType(trait_refs)) if ref_self > 0 && ref_other == 1 && ty.is_thin_ptr() => {
            let traits = trait_refs.iter().map(|v| v.0).collect::<Vec<_>>();
            let ty = ty.deref().expect("v should have a refcount of > 0");
            if !ty.implements(&traits, ctx) {
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
            let id = ctx.add_value(new_ty);
            ctx.append(TypedExpression::AttachVtable(
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
            let id = ctx.add_value(new_ty);
            ctx.append(TypedExpression::MakeUnsizedSlice(
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
            let id = ctx.add_value(new_ty);
            ctx.append(TypedExpression::StripMetadata(span, id, lhs));
            Ok((new_ty, TypedLiteral::Dynamic(id)))
        }
        // &&void -> &void
        (TyKind::PrimitiveVoid, TyKind::PrimitiveVoid) if ref_self > 0 && ref_other == 1 => {
            let id = ctx.add_value(new_ty);
            ctx.append(TypedExpression::Bitcast(span, id, lhs));
            Ok((new_ty, TypedLiteral::Dynamic(id)))
        }
        // &void -> usize
        (TyKind::PrimitiveVoid, TyKind::PrimitiveUSize) if ref_self == 1 && ref_other == 0 => {
            let id = ctx.add_value(new_ty);
            ctx.append(TypedExpression::PtrToInt(span, id, lhs));
            Ok((new_ty, TypedLiteral::Dynamic(id)))
        }
        // usize -> &void
        (TyKind::PrimitiveUSize, TyKind::PrimitiveVoid) if ref_self == 0 && ref_other == 1 => {
            let id = ctx.add_value(new_ty);
            ctx.append(TypedExpression::IntToPtr(span, id, lhs));
            Ok((new_ty, TypedLiteral::Dynamic(id)))
        }
        // fn to &void
        (TyKind::Function(_), TyKind::PrimitiveVoid) if ref_self == 0 && ref_other == 1 => {
            let id = ctx.add_value(new_ty);
            ctx.append(TypedExpression::Bitcast(span, id, lhs));
            Ok((new_ty, TypedLiteral::Dynamic(id)))
        }
        // &T to &void
        (_, TyKind::PrimitiveVoid) if ref_other > 0 && ref_self > 0 && ty.is_thin_ptr() => {
            let id = ctx.add_value(new_ty);
            ctx.append(TypedExpression::Bitcast(span, id, lhs));
            Ok((new_ty, TypedLiteral::Dynamic(id)))
        }
        // &void to &T
        (TyKind::PrimitiveVoid, _) if ref_self > 0 && ref_other > 0 && new_ty.is_thin_ptr() => {
            let id = ctx.add_value(new_ty);
            ctx.append(TypedExpression::Bitcast(span, id, lhs));
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
            let id = ctx.add_value(new_ty);
            ctx.append(TypedExpression::IntCast(span, id, lhs));
            Ok((new_ty, TypedLiteral::Dynamic(id)))
        }
        _ => Err(TypecheckingError::DisallowedCast(span, ty, new_ty).to_error()),
    }
}

#[allow(clippy::too_many_arguments)]
fn typecheck_dyn_membercall<'ctx>(
    ctx: &mut TcCtx<'ctx, '_>,
    ident: &Ident<'ctx>,
    args: &[Expression<'ctx>],
    lhs: TypedLiteral<'ctx>,
    lhs_span: Span<'ctx>,
    trait_refs: ArenaList<'ctx, (TraitId, Ident<'ctx>)>,
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
    let lhs = make_single_ref(
        ctx,
        with_refcount(ctx.ctx, ty, num_references),
        lhs,
        lhs_span,
    );

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
        let (ty, lit) =
            typecheck_expression(ctx, &args[i], TypeSuggestion::from_type(arg_typs[i + 1].1))?;
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
    let id = ctx.add_value(return_ty);
    ctx.append(TypedExpression::DynCall(
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
    ctx: &mut TcCtx<'ctx, '_>,
    lhs: &Expression<'ctx>,
    ident: &Ident<'ctx>,
    args: &[Expression<'ctx>],
) -> Result<(Ty<'ctx>, TypedLiteral<'ctx>), Diagnostic<'ctx>> {
    let (mut typ_lhs, mut typed_literal_lhs) = new_index(ctx, lhs, TypeSuggestion::Unknown)?;

    let lhs_refcount = typ_lhs.refcount();
    match **typ_lhs.without_ref() {
        TyKind::DynType(trait_refs) if lhs_refcount > 0 => {
            return typecheck_dyn_membercall(
                ctx,
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

    let function_reader = ctx.fn_ctx.tcx.functions.read();
    let langitem_reader = ctx.fn_ctx.tcx.lang_items.read();
    let struct_reader = ctx.fn_ctx.tcx.structs.read();

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
    let structure = struct_id.map(|v| &struct_reader[v]);
    let function_id = structure
        .and_then(|v| v.global_impl.get(ident))
        .copied()
        .or_else(|| {
            structure?
                .trait_impl
                .iter()
                .filter_map(|v| {
                    v.iter().find(|v| {
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
            let new_id = ctx.add_value(typ_lhs);
            ctx.append(TypedExpression::Dereference(
                lhs.span(),
                new_id,
                typed_literal_lhs,
            ));
            typed_literal_lhs = TypedLiteral::Dynamic(new_id);
        } else {
            typed_literal_lhs = make_reference(ctx, typ_lhs, typed_literal_lhs, lhs.span());
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

    let call_id = ctx.add_value(function.0.return_type);
    ctx.append(TypedExpression::DirectCall(
        lhs.span(),
        call_id,
        function_id,
        typed_arguments.into_boxed_slice(),
        EMPTY_TYLIST,
    ));

    Ok((function.0.return_type, TypedLiteral::Dynamic(call_id)))
}

fn typecheck_take_ref<'ctx>(
    ctx: &mut TcCtx<'ctx, '_>,
    expression: &Expression<'ctx>,
    type_suggestion: TypeSuggestion,
    span: Span<'ctx>,
) -> Result<(Ty<'ctx>, TypedLiteral<'ctx>), Diagnostic<'ctx>> {
    match expression {
        //&*_1 => _1
        Expression::Unary {
            operator,
            right_side,
            ..
        } if *operator == UnaryOp::Dereference => {
            typecheck_expression(ctx, right_side, type_suggestion)
        }
        Expression::Indexing { .. } | Expression::MemberAccess { .. } => {
            new_index(ctx, expression, type_suggestion)
        }

        _ => {
            let (ty, lit) = typecheck_expression(ctx, expression, type_suggestion)?;
            let new_lit = make_reference(ctx, ty, lit, span);
            Ok((ty.take_ref(ctx.ctx), new_lit))
        }
    }
}

fn indexing_resolve_rhs<'ctx>(
    ctx: &mut TcCtx<'ctx, '_>,
    expression: &Expression<'ctx>,
) -> Result<OffsetValue, Diagnostic<'ctx>> {
    let (ty, rhs) =
        typecheck_expression(ctx, expression, TypeSuggestion::Number(NumberType::Usize))?;
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
fn make_reference<'ctx>(
    ctx: &mut TcCtx<'ctx, '_>,
    ty: Ty<'ctx>,
    mut typed_literal: TypedLiteral<'ctx>,
    span: Span<'ctx>,
) -> TypedLiteral<'ctx> {
    match typed_literal {
        TypedLiteral::Void | TypedLiteral::Static(_) => {}
        TypedLiteral::Dynamic(v) => ctx.make_stack_allocated(v),
        _ => {
            let lit_id = ctx.add_value(ty);
            ctx.make_stack_allocated(lit_id);
            ctx.append(TypedExpression::Literal(span, lit_id, typed_literal));
            typed_literal = TypedLiteral::Dynamic(lit_id);
        }
    }

    let ty = ty.take_ref(ctx.ctx);
    let new_id = ctx.add_value(ty);
    ctx.append(TypedExpression::Reference(span, new_id, typed_literal));
    TypedLiteral::Dynamic(new_id)
}

fn new_index<'ctx>(
    ctx: &mut TcCtx<'ctx, '_>,
    expression: &Expression<'ctx>,
    type_suggestion: TypeSuggestion,
) -> Result<(Ty<'ctx>, TypedLiteral<'ctx>), Diagnostic<'ctx>> {
    match expression {
        Expression::Indexing {
            left_side,
            right_side,
            span,
        } => {
            let offset = indexing_resolve_rhs(ctx, right_side)?;
            let (ty, lhs) = new_index(
                ctx,
                left_side,
                TypeSuggestion::Array(Box::new(type_suggestion)),
            )?;
            let single_ref_lit = make_single_ref(ctx, ty, lhs, *span);
            if !ty.is_indexable() {
                return Err(TypecheckingError::IndexNonArrayElem(expression.span(), ty).to_error());
            }
            let elem_ty = match **ty.without_ref() {
                TyKind::SizedArray {
                    ty,
                    number_elements,
                } => {
                    if let OffsetValue::Static(v) = offset
                        && v >= number_elements
                    {
                        return Err(TypecheckingError::ArrayIndexOutOfBounds(
                            expression.span(),
                            v,
                            number_elements,
                        )
                        .to_error());
                    }
                    ty
                }
                TyKind::UnsizedArray(ty) => ty,
                TyKind::Tuple(elems) => match offset {
                    OffsetValue::Dynamic(_) => {
                        return Err(
                            TypecheckingError::TupleDynamicIndex(expression.span()).to_error()
                        );
                    }

                    OffsetValue::Static(v) if v >= elems.len() => {
                        return Err(TypecheckingError::TupleIndexOutOfBounds(
                            expression.span(),
                            elems.len(),
                            v,
                        )
                        .to_error());
                    }
                    OffsetValue::Static(v) => elems[v],
                },
                _ => unreachable!(),
            };
            let new_ty = elem_ty.take_ref(ctx.ctx);
            let dst = ctx.add_value(new_ty);
            ctx.append(TypedExpression::Offset(*span, dst, single_ref_lit, offset));

            Ok((new_ty, TypedLiteral::Dynamic(dst)))
        }
        Expression::MemberAccess {
            left_side,
            index,
            span,
        } => {
            let (mut ty_lhs, mut lit) = new_index(ctx, left_side, TypeSuggestion::Unknown)?;
            for field_name in index {
                let (offset, new_ty) = match **ty_lhs.without_ref() {
                    TyKind::Struct { struct_id, .. } => {
                        let structure = &ctx.structs.read()[struct_id];
                        match structure
                            .elements
                            .iter()
                            .enumerate()
                            .find(|(_, (v, _, _))| v == field_name)
                            .map(|(idx, (_, ty, _))| (idx, ty))
                        {
                            Some((idx, ty)) => (idx, ty.take_ref(ctx.ctx)),
                            None => {
                                return Err(TypecheckingError::FieldNotFound(
                                    expression.span(),
                                    ty_lhs.without_ref(),
                                    field_name.symbol(),
                                )
                                .to_error());
                            }
                        }
                    }
                    _ => {
                        return Err(
                            TypecheckingError::AccessNonStructValue(*span, ty_lhs).to_error()
                        );
                    }
                };
                lit = make_single_ref(ctx, ty_lhs, lit, *span);
                let dst = ctx.add_value(new_ty);
                ctx.append(TypedExpression::Offset(
                    *span,
                    dst,
                    lit,
                    OffsetValue::Static(offset),
                ));
                lit = TypedLiteral::Dynamic(dst);
                ty_lhs = new_ty;
            }

            Ok((ty_lhs, lit))
        }
        _ => typecheck_expression(ctx, expression, type_suggestion),
    }
}

fn make_single_ref<'ctx>(
    ctx: &mut TcCtx<'ctx, '_>,
    mut current: Ty<'ctx>,
    mut value: TypedLiteral<'ctx>,
    span: Span<'ctx>,
) -> TypedLiteral<'ctx> {
    if !current.has_refs() {
        // make ref
        value = make_reference(ctx, current, value, span);
    } else {
        current = current.deref().unwrap();
        while current.has_refs() {
            let new_id = ctx.add_value(current);
            current = current.deref().unwrap();
            ctx.append(TypedExpression::Dereference(span, new_id, value));
            value = TypedLiteral::Dynamic(new_id);
        }
    }
    value
}
