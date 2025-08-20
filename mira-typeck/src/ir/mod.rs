mod expressions;
mod literal;
pub mod passes;
pub use expressions::{OffsetValue, TypedExpression};
pub use literal::TypedLiteral;
use mira_common::store::{StoreKey, VecStore};

use crate::{Ty, TypeCtx};

pub type ScopeValueId<'arena> = StoreKey<ScopeEntry<'arena>>;

#[derive(Clone, Copy)]
pub struct ScopeEntry<'arena> {
    pub stack_allocated: bool,
    pub ty: Ty<'arena>,
}

pub struct IR<'arena> {
    pub exprs: Vec<TypedExpression<'arena>>,
    pub values: VecStore<ScopeEntry<'arena>>,
}

pub trait IRVisitor<'arena> {
    fn visit_expr(
        &mut self,
        expr: &mut TypedExpression<'arena>,
        values: &mut VecStore<ScopeEntry<'arena>>,
        tcx: TypeCtx<'arena>,
    );
}

impl<
    'arena,
    T: FnMut(&mut TypedExpression<'arena>, &mut VecStore<ScopeEntry<'arena>>, TypeCtx<'arena>)
        + 'arena,
> IRVisitor<'arena> for T
{
    fn visit_expr(
        &mut self,
        expr: &mut TypedExpression<'arena>,
        values: &mut VecStore<ScopeEntry<'arena>>,
        tcx: TypeCtx<'arena>,
    ) {
        self(expr, values, tcx)
    }
}

impl<'arena> IR<'arena> {
    pub const fn new(
        exprs: Vec<TypedExpression<'arena>>,
        values: VecStore<ScopeEntry<'arena>>,
    ) -> Self {
        Self { exprs, values }
    }

    pub fn add_expr(&mut self, expr: TypedExpression<'arena>) {
        self.exprs.push(expr);
    }

    pub fn visit<T: IRVisitor<'arena>>(&mut self, visitor: &mut T, tcx: TypeCtx<'arena>) {
        for expr in self.exprs.iter_mut() {
            visit_expr(expr, &mut self.values, visitor, tcx);
        }
    }
}

fn visit_expr<'arena>(
    expr: &mut TypedExpression<'arena>,
    values: &mut VecStore<ScopeEntry<'arena>>,
    visitor: &mut impl IRVisitor<'arena>,
    tcx: TypeCtx<'arena>,
) {
    if !matches!(expr, TypedExpression::While { .. }) {
        visitor.visit_expr(expr, values, tcx);
    }
    match expr {
        TypedExpression::Return(..)
        | TypedExpression::Range { .. }
        | TypedExpression::Asm { .. }
        | TypedExpression::StoreAssignment(..)
        | TypedExpression::Call(..)
        | TypedExpression::DirectCall(..)
        | TypedExpression::DirectExternCall(..)
        | TypedExpression::IntrinsicCall(..)
        | TypedExpression::LLVMIntrinsicCall(..)
        | TypedExpression::Pos(..)
        | TypedExpression::Neg(..)
        | TypedExpression::LNot(..)
        | TypedExpression::BNot(..)
        | TypedExpression::Add(..)
        | TypedExpression::Sub(..)
        | TypedExpression::Mul(..)
        | TypedExpression::Div(..)
        | TypedExpression::Mod(..)
        | TypedExpression::BAnd(..)
        | TypedExpression::BOr(..)
        | TypedExpression::BXor(..)
        | TypedExpression::GreaterThan(..)
        | TypedExpression::LessThan(..)
        | TypedExpression::LAnd(..)
        | TypedExpression::LOr(..)
        | TypedExpression::GreaterThanEq(..)
        | TypedExpression::LessThanEq(..)
        | TypedExpression::Eq(..)
        | TypedExpression::Neq(..)
        | TypedExpression::LShift(..)
        | TypedExpression::RShift(..)
        | TypedExpression::Reference(..)
        | TypedExpression::Dereference(..)
        | TypedExpression::Offset(..)
        | TypedExpression::OffsetNonPointer(..)
        | TypedExpression::DynCall(..)
        | TypedExpression::Literal(..)
        | TypedExpression::DeclareVariable(..)
        | TypedExpression::Empty(_)
        | TypedExpression::Unreachable(_)
        | TypedExpression::Alias(..)
        | TypedExpression::Bitcast(..)
        | TypedExpression::IntCast(..)
        | TypedExpression::PtrToInt(..)
        | TypedExpression::IntToPtr(..)
        | TypedExpression::StripMetadata(..)
        | TypedExpression::MakeUnsizedSlice(..)
        | TypedExpression::AttachVtable(..)
        | TypedExpression::DropIf(..)
        | TypedExpression::Drop(..)
        | TypedExpression::None => {}

        TypedExpression::Block(_, exprs, _) => {
            for expr in exprs {
                visit_expr(expr, values, visitor, tcx)
            }
        }
        TypedExpression::If {
            if_block,
            else_block,
            ..
        } => {
            for expr in if_block.0.iter_mut() {
                visit_expr(expr, values, visitor, tcx)
            }
            if let Some(else_block) = else_block {
                for expr in else_block.0.iter_mut() {
                    visit_expr(expr, values, visitor, tcx)
                }
            }
        }
        TypedExpression::While { cond_block, .. } => {
            for expr in cond_block {
                visit_expr(expr, values, visitor, tcx)
            }
            visitor.visit_expr(expr, values, tcx);
            let TypedExpression::While { body, .. } = expr else {
                unreachable!()
            };
            for expr in body.0.iter_mut() {
                visit_expr(expr, values, visitor, tcx)
            }
        }
    }
}
