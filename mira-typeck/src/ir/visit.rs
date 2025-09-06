use crate::{
    TypeCtx,
    ir::{BlockId, IR, Scope, TypedExpression, TypedLiteral},
};

pub trait Visitor<'ctx> {
    #[allow(unused_variables)]
    fn visit_block(&mut self, block: BlockId, ir: &IR<'ctx>, tcx: TypeCtx<'ctx>) {}
    #[allow(unused_variables)]
    fn visit_expr(&mut self, expr: &TypedExpression<'ctx>, ir: &IR<'ctx>, tcx: TypeCtx<'ctx>) {}
    #[allow(unused_variables)]
    fn visit_lit(&mut self, lit: &TypedLiteral<'ctx>, ir: &IR<'ctx>, tcx: TypeCtx<'ctx>) {}

    fn visit_block_super(&mut self, block: BlockId, ir: &IR<'ctx>, tcx: TypeCtx<'ctx>) {
        self.visit_block(block, ir, tcx);
        walk_block(block, ir, self, tcx);
    }
    fn visit_expr_super(
        &mut self,
        expr: &TypedExpression<'ctx>,
        ir: &IR<'ctx>,
        tcx: TypeCtx<'ctx>,
    ) {
        self.visit_expr(expr, ir, tcx);
        walk_expr(expr, ir, self, tcx);
    }
    fn visit_lit_super(&mut self, lit: &TypedLiteral<'ctx>, ir: &IR<'ctx>, tcx: TypeCtx<'ctx>) {
        self.visit_lit(lit, ir, tcx);
        walk_lit(lit, ir, self, tcx);
    }
}

pub fn walk_block<'ctx, W: Visitor<'ctx> + ?Sized>(
    block: BlockId,
    ir: &IR<'ctx>,
    visitor: &mut W,
    tcx: TypeCtx<'ctx>,
) {
    for expr in ir.get_block_exprs(block) {
        visitor.visit_expr_super(expr, ir, tcx);
    }
}

pub fn walk_expr<'ctx, W: Visitor<'ctx> + ?Sized>(
    expr: &TypedExpression<'ctx>,
    ir: &IR<'ctx>,
    visitor: &mut W,
    tcx: TypeCtx<'ctx>,
) {
    match expr {
        &TypedExpression::Block(_, blk, _) => visitor.visit_block_super(blk, ir, tcx),
        TypedExpression::If {
            cond,
            if_block,
            else_block,
            ..
        } => {
            visitor.visit_lit_super(cond, ir, tcx);
            visitor.visit_block_super(*if_block, ir, tcx);
            if let &Some(else_block) = else_block {
                visitor.visit_block_super(else_block, ir, tcx);
            }
        }
        TypedExpression::While {
            cond_block,
            cond,
            body,
            ..
        } => {
            visitor.visit_block_super(*cond_block, ir, tcx);
            visitor.visit_lit_super(cond, ir, tcx);
            visitor.visit_block_super(*body, ir, tcx);
        }
        TypedExpression::Return(_, lit)
        | TypedExpression::AttachVtable(_, _, lit, _)
        | TypedExpression::Pos(_, _, lit)
        | TypedExpression::Neg(_, _, lit)
        | TypedExpression::LNot(_, _, lit)
        | TypedExpression::BNot(_, _, lit)
        | TypedExpression::Reference(_, _, lit)
        | TypedExpression::Dereference(_, _, lit)
        | TypedExpression::OffsetNonPointer(_, _, lit, _)
        | TypedExpression::Literal(_, _, lit)
        | TypedExpression::Alias(_, _, lit)
        | TypedExpression::Bitcast(_, _, lit)
        | TypedExpression::IntCast(_, _, lit)
        | TypedExpression::PtrToInt(_, _, lit)
        | TypedExpression::IntToPtr(_, _, lit)
        | TypedExpression::Offset(_, _, lit, _)
        | TypedExpression::StripMetadata(_, _, lit)
        | TypedExpression::MakeUnsizedSlice(_, _, lit, _) => visitor.visit_lit_super(lit, ir, tcx),

        TypedExpression::Range { lhs, rhs, .. }
        | TypedExpression::StoreAssignment(_, lhs, rhs)
        | TypedExpression::Add(_, _, lhs, rhs)
        | TypedExpression::Sub(_, _, lhs, rhs)
        | TypedExpression::Mul(_, _, lhs, rhs)
        | TypedExpression::Div(_, _, lhs, rhs)
        | TypedExpression::Mod(_, _, lhs, rhs)
        | TypedExpression::BAnd(_, _, lhs, rhs)
        | TypedExpression::BOr(_, _, lhs, rhs)
        | TypedExpression::BXor(_, _, lhs, rhs)
        | TypedExpression::GreaterThan(_, _, lhs, rhs)
        | TypedExpression::LessThan(_, _, lhs, rhs)
        | TypedExpression::GreaterThanEq(_, _, lhs, rhs)
        | TypedExpression::LessThanEq(_, _, lhs, rhs)
        | TypedExpression::Eq(_, _, lhs, rhs)
        | TypedExpression::Neq(_, _, lhs, rhs)
        | TypedExpression::LShift(_, _, lhs, rhs)
        | TypedExpression::RShift(_, _, lhs, rhs) => {
            visitor.visit_lit_super(lhs, ir, tcx);
            visitor.visit_lit_super(rhs, ir, tcx);
        }

        TypedExpression::LAnd(_, _, lhs, rhs, rhs_blk)
        | TypedExpression::LOr(_, _, lhs, rhs, rhs_blk) => {
            visitor.visit_lit_super(lhs, ir, tcx);
            visitor.visit_block_super(*rhs_blk, ir, tcx);
            visitor.visit_lit_super(rhs, ir, tcx);
        }

        TypedExpression::DynCall(_, _, lits, _)
        | TypedExpression::DirectCall(_, _, _, lits, _)
        | TypedExpression::DirectExternCall(_, _, _, lits)
        | TypedExpression::IntrinsicCall(_, _, _, lits, _)
        | TypedExpression::LLVMIntrinsicCall(_, _, _, lits) => {
            for lit in lits {
                visitor.visit_lit_super(lit, ir, tcx);
            }
        }

        TypedExpression::Call(_, _, lit, lits) => {
            visitor.visit_lit_super(lit, ir, tcx);
            for lit in lits {
                visitor.visit_lit_super(lit, ir, tcx);
            }
        }

        TypedExpression::Empty(_)
        | TypedExpression::Unreachable(_)
        | TypedExpression::DeclareVariable(_, _, _, _)
        | TypedExpression::DropIf(_, _, _)
        | TypedExpression::Drop(_, _)
        | TypedExpression::None
        | TypedExpression::Asm { .. } => {}
    }
}

pub fn walk_lit<'ctx, W: Visitor<'ctx> + ?Sized>(
    lit: &TypedLiteral<'ctx>,
    ir: &IR<'ctx>,
    visitor: &mut W,
    tcx: TypeCtx<'ctx>,
) {
    match lit {
        TypedLiteral::ArrayInit(_, lit, _) => visitor.visit_lit_super(lit, ir, tcx),
        TypedLiteral::Array(_, lits)
        | TypedLiteral::Struct(_, lits)
        | TypedLiteral::Tuple(lits) => {
            for lit in lits {
                visitor.visit_lit_super(lit, ir, tcx);
            }
        }
        _ => {}
    }
}

#[allow(unused_variables)]
pub trait MutVisitor<'ctx> {
    #[allow(unused_variables)]
    fn visit_block(&mut self, block: BlockId, ir: &mut IR<'ctx>, tcx: TypeCtx<'ctx>) {}
    #[allow(unused_variables)]
    fn visit_expr_block(
        &mut self,
        block: BlockId,
        expr: usize,
        ir: &mut IR<'ctx>,
        tcx: TypeCtx<'ctx>,
    ) {
    }

    #[allow(unused_variables)]
    fn visit_expr(
        &mut self,
        expr: &mut TypedExpression<'ctx>,
        scope: &mut Scope<'ctx>,
        tcx: TypeCtx<'ctx>,
    ) {
    }
    #[allow(unused_variables)]
    fn visit_lit(
        &mut self,
        lit: &mut TypedLiteral<'ctx>,
        scope: &mut Scope<'ctx>,
        tcx: TypeCtx<'ctx>,
    ) {
    }

    fn visit_block_super(&mut self, block: BlockId, ir: &mut IR<'ctx>, tcx: TypeCtx<'ctx>) {
        self.visit_block(block, ir, tcx);
        walk_mut_block(block, ir, self, tcx);
    }
    fn visit_expr_block_super(
        &mut self,
        block: BlockId,
        expr: usize,
        ir: &mut IR<'ctx>,
        tcx: TypeCtx<'ctx>,
    ) {
        self.visit_expr_block(block, expr, ir, tcx);
        let (exprs, scope) = ir.get_block_exprs_mut(block);
        self.visit_expr(&mut exprs[expr], scope, tcx);
        walk_mut_expr_block(block, expr, ir, self, tcx);
    }
    fn visit_lit_super(
        &mut self,
        lit: &mut TypedLiteral<'ctx>,
        scope: &mut Scope<'ctx>,
        tcx: TypeCtx<'ctx>,
    ) {
        self.visit_lit(lit, scope, tcx);
        walk_mut_lit(lit, scope, self, tcx);
    }
}

pub fn walk_mut_block<'ctx, W: MutVisitor<'ctx> + ?Sized>(
    block: BlockId,
    ir: &mut IR<'ctx>,
    visitor: &mut W,
    tcx: TypeCtx<'ctx>,
) {
    let mut idx = 0;
    while idx < ir.get_block_exprs(block).len() {
        visitor.visit_expr_block_super(block, idx, ir, tcx);
        idx += 1;
    }
}

pub fn walk_mut_expr_block<'ctx, W: MutVisitor<'ctx> + ?Sized>(
    block: BlockId,
    expr: usize,
    ir: &mut IR<'ctx>,
    visitor: &mut W,
    tcx: TypeCtx<'ctx>,
) {
    let (exprs, scope) = ir.get_block_exprs_mut(block);
    match &mut exprs[expr] {
        &mut TypedExpression::Block(_, blk, _) => visitor.visit_block_super(blk, ir, tcx),
        &mut TypedExpression::If {
            if_block,
            else_block,
            ..
        } => {
            let TypedExpression::If { cond, .. } = &mut exprs[expr] else {
                unreachable!()
            };
            visitor.visit_lit_super(cond, scope, tcx);
            visitor.visit_block_super(if_block, ir, tcx);
            if let Some(else_block) = else_block {
                visitor.visit_block_super(else_block, ir, tcx);
            }
        }
        &mut TypedExpression::While {
            cond_block, body, ..
        } => {
            visitor.visit_block_super(cond_block, ir, tcx);
            let (exprs, scope) = ir.get_block_exprs_mut(block);
            let TypedExpression::While { cond, .. } = &mut exprs[expr] else {
                unreachable!()
            };
            visitor.visit_lit_super(cond, scope, tcx);
            visitor.visit_block_super(body, ir, tcx);
        }
        TypedExpression::Return(_, lit)
        | TypedExpression::AttachVtable(_, _, lit, _)
        | TypedExpression::Pos(_, _, lit)
        | TypedExpression::Neg(_, _, lit)
        | TypedExpression::LNot(_, _, lit)
        | TypedExpression::BNot(_, _, lit)
        | TypedExpression::Reference(_, _, lit)
        | TypedExpression::Dereference(_, _, lit)
        | TypedExpression::OffsetNonPointer(_, _, lit, _)
        | TypedExpression::Literal(_, _, lit)
        | TypedExpression::Alias(_, _, lit)
        | TypedExpression::Bitcast(_, _, lit)
        | TypedExpression::IntCast(_, _, lit)
        | TypedExpression::PtrToInt(_, _, lit)
        | TypedExpression::IntToPtr(_, _, lit)
        | TypedExpression::Offset(_, _, lit, _)
        | TypedExpression::StripMetadata(_, _, lit)
        | TypedExpression::MakeUnsizedSlice(_, _, lit, _) => {
            visitor.visit_lit_super(lit, scope, tcx)
        }

        TypedExpression::Range { lhs, rhs, .. }
        | TypedExpression::StoreAssignment(_, lhs, rhs)
        | TypedExpression::Add(_, _, lhs, rhs)
        | TypedExpression::Sub(_, _, lhs, rhs)
        | TypedExpression::Mul(_, _, lhs, rhs)
        | TypedExpression::Div(_, _, lhs, rhs)
        | TypedExpression::Mod(_, _, lhs, rhs)
        | TypedExpression::BAnd(_, _, lhs, rhs)
        | TypedExpression::BOr(_, _, lhs, rhs)
        | TypedExpression::BXor(_, _, lhs, rhs)
        | TypedExpression::GreaterThan(_, _, lhs, rhs)
        | TypedExpression::LessThan(_, _, lhs, rhs)
        | TypedExpression::GreaterThanEq(_, _, lhs, rhs)
        | TypedExpression::LessThanEq(_, _, lhs, rhs)
        | TypedExpression::Eq(_, _, lhs, rhs)
        | TypedExpression::Neq(_, _, lhs, rhs)
        | TypedExpression::LShift(_, _, lhs, rhs)
        | TypedExpression::RShift(_, _, lhs, rhs) => {
            visitor.visit_lit_super(lhs, scope, tcx);
            visitor.visit_lit_super(rhs, scope, tcx);
        }

        &mut TypedExpression::LAnd(_, _, _, _, rhs_blk)
        | &mut TypedExpression::LOr(_, _, _, _, rhs_blk) => {
            let (TypedExpression::LAnd(_, _, lhs, _, _) | TypedExpression::LOr(_, _, lhs, _, _)) =
                &mut exprs[expr]
            else {
                unreachable!()
            };
            visitor.visit_lit_super(lhs, scope, tcx);
            visitor.visit_block_super(rhs_blk, ir, tcx);
            let (exprs, scope) = ir.get_block_exprs_mut(block);
            let (TypedExpression::LAnd(_, _, _, rhs, _) | TypedExpression::LOr(_, _, _, rhs, _)) =
                &mut exprs[expr]
            else {
                unreachable!()
            };
            visitor.visit_lit_super(rhs, scope, tcx);
        }

        TypedExpression::DynCall(_, _, lits, _)
        | TypedExpression::DirectCall(_, _, _, lits, _)
        | TypedExpression::DirectExternCall(_, _, _, lits)
        | TypedExpression::IntrinsicCall(_, _, _, lits, _)
        | TypedExpression::LLVMIntrinsicCall(_, _, _, lits) => {
            for lit in lits {
                visitor.visit_lit_super(lit, scope, tcx);
            }
        }

        TypedExpression::Call(_, _, lit, lits) => {
            visitor.visit_lit_super(lit, scope, tcx);
            for lit in lits {
                visitor.visit_lit_super(lit, scope, tcx);
            }
        }

        TypedExpression::Empty(_)
        | TypedExpression::Unreachable(_)
        | TypedExpression::DeclareVariable(_, _, _, _)
        | TypedExpression::DropIf(_, _, _)
        | TypedExpression::Drop(_, _)
        | TypedExpression::None
        | TypedExpression::Asm { .. } => {}
    }
}

pub fn walk_mut_lit<'ctx, W: MutVisitor<'ctx> + ?Sized>(
    lit: &mut TypedLiteral<'ctx>,
    scope: &mut Scope<'ctx>,
    visitor: &mut W,
    tcx: TypeCtx<'ctx>,
) {
    match lit {
        TypedLiteral::ArrayInit(_, lit, _) => visitor.visit_lit_super(lit, scope, tcx),
        TypedLiteral::Array(_, lits)
        | TypedLiteral::Struct(_, lits)
        | TypedLiteral::Tuple(lits) => {
            for lit in lits {
                visitor.visit_lit_super(lit, scope, tcx);
            }
        }
        _ => {}
    }
}
