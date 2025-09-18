mod expressions;
mod literal;
pub mod passes;
mod visit;

pub use visit::*;

use std::{
    collections::HashMap,
    ops::{Deref, DerefMut, Index},
};

pub use expressions::{OffsetValue, TypedExpression};
pub use literal::TypedLiteral;
use mira_common::{index::IndexVec, newty};
use mira_spans::{Ident, Span};

use crate::{Ty, TypeCtx, monomorphisation::Substitute};

newty! {
    #[display("Block(#{})")]
    pub struct BlockId {}
    #[display("_{}")]
    pub struct ValueId {}
}

pub struct Block<'ctx> {
    pub exprs: Vec<TypedExpression<'ctx>>,
    pub span: Span<'ctx>,
}

#[derive(Clone, Copy)]
pub struct ScopeEntry<'ctx> {
    pub stack_allocated: bool,
    pub ty: Ty<'ctx>,
}

pub struct Param<'ctx> {
    pub span: Span<'ctx>,
    pub name: Ident<'ctx>,
    pub value: ValueId,
}

pub struct Scope<'ctx>(IndexVec<ValueId, ScopeEntry<'ctx>>);

impl<'ctx> Scope<'ctx> {
    pub fn add(&mut self, ty: Ty<'ctx>) -> ValueId {
        if !ty.is_sized() {
            panic!("unsized type: {ty:?}");
        }
        self.0.add(ScopeEntry {
            ty,
            stack_allocated: false,
        })
    }

    pub fn make_stack_allocated(&mut self, value: ValueId) {
        self.0[value].stack_allocated = true;
    }

    pub fn get_ty(&self, value: ValueId) -> Ty<'ctx> {
        self.0[value].ty
    }

    pub fn is_stack_allocated(&self, value: ValueId) -> bool {
        self.0[value].stack_allocated
    }

    pub fn get(&self, value: ValueId) -> ScopeEntry<'ctx> {
        self.0[value]
    }

    pub const fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<'ctx> Index<ValueId> for Scope<'ctx> {
    type Output = ScopeEntry<'ctx>;

    fn index(&self, index: ValueId) -> &Self::Output {
        &self.0[index]
    }
}

pub struct IR<'ctx> {
    pub(crate) scope: Scope<'ctx>,
    pub(crate) blocks: IndexVec<BlockId, Block<'ctx>>,
    pub(crate) current_block: BlockId,
    pub(crate) params: Vec<Param<'ctx>>,
}

impl<'ctx> IR<'ctx> {
    pub const fn dummy() -> Self {
        Self {
            scope: Scope(IndexVec::new()),
            blocks: IndexVec::new(),
            current_block: BlockId::ZERO,
            params: Vec::new(),
        }
    }

    /// Creates a new ir, where ValueId 0..params.count() are the parameters.
    pub fn new(
        params: impl Iterator<Item = (Ident<'ctx>, Span<'ctx>, Ty<'ctx>)>,
        span: Span<'ctx>,
    ) -> Self {
        let mut scope = Scope(IndexVec::new());
        let mut params_vec = Vec::new();
        for (name, span, ty) in params {
            let value = scope.add(ty);
            params_vec.push(Param { span, name, value });
            // stack allocate params for llvm's `#declare_param`
            scope.make_stack_allocated(value);
        }
        let mut blocks = IndexVec::new();
        let current_block = blocks.add(Block {
            exprs: Vec::new(),
            span,
        });
        Self {
            scope,
            blocks,
            current_block,
            params: params_vec,
        }
    }

    pub fn params(&self) -> &[Param<'ctx>] {
        &self.params
    }

    pub const fn current_block(&self) -> BlockId {
        self.current_block
    }

    pub fn goto_entry_block(&mut self) {
        self.goto(self.get_entry_block());
    }

    pub const fn get_entry_block(&self) -> BlockId {
        BlockId::ZERO
    }

    pub fn get_entry_block_exprs(&self) -> &[TypedExpression<'ctx>] {
        self.get_block_exprs(self.get_entry_block())
    }

    pub fn get_block_exprs(&self, block: BlockId) -> &[TypedExpression<'ctx>] {
        &self.blocks[block].exprs
    }

    pub fn get_block_exprs_mut(
        &mut self,
        block: BlockId,
    ) -> (&mut Vec<TypedExpression<'ctx>>, &mut Scope<'ctx>) {
        (&mut self.blocks[block].exprs, &mut self.scope)
    }

    pub fn get_block_span(&self, block: BlockId) -> Span<'ctx> {
        self.blocks[block].span
    }

    pub fn is_empty(&self) -> bool {
        // this should never happen, as `Self::new` adds a block with id zero (the entry block).
        debug_assert!(!self.blocks.is_empty());
        self.scope.is_empty()
            && self.params.is_empty()
            && self.blocks.len() == 1
            && self.blocks[self.get_entry_block()].exprs.is_empty()
    }

    pub fn goto(&mut self, block: BlockId) {
        self.current_block = block;
    }

    pub fn create_block(&mut self, span: Span<'ctx>) -> BlockId {
        self.blocks.add(Block {
            exprs: Vec::new(),
            span,
        })
    }

    /// creates a block and sets the current block to it, calls `f`, and restores the current block after f exits.
    pub fn with_block<R>(&mut self, block: BlockId, f: impl FnOnce(&mut Self) -> R) -> R {
        let old = self.current_block;
        self.goto(block);
        let v = f(self);
        self.goto(old);
        v
    }

    /// creates a block and sets the current block to it, calls `f`, and restores the current block after f exits.
    pub fn with_new_block<R>(
        &mut self,
        span: Span<'ctx>,
        f: impl FnOnce(&mut Self) -> R,
    ) -> (BlockId, R) {
        let old = self.current_block;
        let block = self.create_block(span);
        let v = f(self);
        self.goto(old);
        (block, v)
    }

    pub fn append(&mut self, expr: TypedExpression<'ctx>) {
        self.blocks[self.current_block].exprs.push(expr);
    }
    pub fn add_value(&mut self, ty: Ty<'ctx>) -> ValueId {
        self.scope.add(ty)
    }

    pub fn make_stack_allocated(&mut self, value: ValueId) {
        self.scope.make_stack_allocated(value);
    }

    pub fn get_ty(&self, value: ValueId) -> Ty<'ctx> {
        self.scope.get_ty(value)
    }

    pub fn get_value(&self, value: ValueId) -> ScopeEntry<'ctx> {
        self.scope.get(value)
    }

    pub fn is_stack_allocated(&self, value: ValueId) -> bool {
        self.scope.is_stack_allocated(value)
    }

    pub fn scope(&self) -> &Scope<'ctx> {
        &self.scope
    }

    pub fn visit_mut<V: MutVisitor<'ctx>>(&mut self, visitor: &mut V, tcx: TypeCtx<'ctx>) {
        walk_mut_block(self.get_entry_block(), self, visitor, tcx);
    }

    pub fn visit<V: Visitor<'ctx>>(&self, visitor: &mut V, tcx: TypeCtx<'ctx>) {
        walk_block(self.get_entry_block(), self, visitor, tcx);
    }
}

pub struct ScopedIR<'ctx> {
    ir: IR<'ctx>,
    scopes: Vec<HashMap<Ident<'ctx>, ValueId>>,
}

impl<'ctx> ScopedIR<'ctx> {
    /// Creates a new ir, where ValueId 0..params.count() are the parameters, and they're scoped by
    /// their names.
    pub fn new(
        params: impl Iterator<Item = (Ident<'ctx>, Span<'ctx>, Ty<'ctx>)>,
        span: Span<'ctx>,
    ) -> Self {
        let mut bound_scope = HashMap::new();
        let mut scope = Scope(IndexVec::new());
        let mut params_vec = Vec::new();
        for (name, span, ty) in params {
            let value = scope.add(ty);
            params_vec.push(Param { span, name, value });
            bound_scope.insert(name, value);
            // stack allocate params for llvm's `#declare_param`
            scope.make_stack_allocated(value);
        }
        let mut blocks = IndexVec::new();
        let current_block = blocks.add(Block {
            exprs: Vec::new(),
            span,
        });
        Self {
            ir: IR {
                scope,
                blocks,
                current_block,
                params: params_vec,
            },
            scopes: vec![bound_scope],
        }
    }

    pub fn to_ir(self) -> IR<'ctx> {
        self.ir
    }

    pub fn add_scoped_value(&mut self, ident: Ident<'ctx>, ty: Ty<'ctx>) -> ValueId {
        let value = self.ir.add_value(ty);
        self.scope_value(ident, value);
        value
    }

    pub fn scope_value(&mut self, ident: Ident<'ctx>, value: ValueId) {
        self.scopes.last_mut().unwrap().insert(ident, value);
    }

    pub fn get_scoped(&self, i: &Ident<'ctx>) -> Option<ValueId> {
        self.scopes.last().unwrap().get(i).copied()
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        if self.scopes.len() == 1 {
            unreachable!("tried to pop the outermost scope");
        }
        self.scopes.pop();
    }

    /// pushes a scope, calls f, and pops it.
    pub fn with_scope<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        self.push_scope();
        let v = f(self);
        self.pop_scope();
        v
    }

    /// creates a block and sets the current block to it, calls `f`, and restores the current block after f exits.
    pub fn with_block<R>(&mut self, block: BlockId, f: impl FnOnce(&mut Self) -> R) -> R {
        let old = self.current_block;
        self.goto(block);
        let v = f(self);
        self.goto(old);
        v
    }

    /// creates a block and sets the current block to it, calls `f`, and restores the current block after f exits.
    pub fn with_new_block<R>(
        &mut self,
        span: Span<'ctx>,
        f: impl FnOnce(&mut Self) -> R,
    ) -> (BlockId, R) {
        let old = self.current_block;
        let block = self.create_block(span);
        self.goto(block);
        let v = f(self);
        self.goto(old);
        (block, v)
    }
}

impl<'ctx> Deref for ScopedIR<'ctx> {
    type Target = IR<'ctx>;

    fn deref(&self) -> &Self::Target {
        &self.ir
    }
}

impl DerefMut for ScopedIR<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.ir
    }
}

impl<'ctx> Substitute<'ctx> for Scope<'ctx> {
    fn substitute(mut self, ctx: &crate::monomorphisation::SubstitutionCtx<'ctx, '_, '_>) -> Self {
        self.0.iter_mut().for_each(|v| v.ty = v.ty.substitute(ctx));
        self
    }

    fn would_substitute(&self) -> bool {
        self.0.iter().any(|v| v.ty.would_substitute())
    }
}
