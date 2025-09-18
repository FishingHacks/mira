use inkwell::debug_info::{AsDIScope, DIScope};
use mira_typeck::ir::{BlockId, TypedLiteral};

use super::{FunctionCodegenContext, Result};

impl<'ctx, 'arena> FunctionCodegenContext<'ctx, 'arena, '_, '_, '_> {
    pub(crate) fn build_block(
        &mut self,
        block: BlockId,
        mut current_scope: DIScope<'ctx>,
        new_scope: bool,
    ) -> Result {
        if new_scope {
            let block = self.ctx.debug_ctx.new_block(
                current_scope,
                self.ir.get_block_span(block),
                self.module,
            );
            current_scope = block.as_debug_info_scope();
        }
        for c in self.ir.get_block_exprs(block) {
            self.build_expr(c, current_scope)?;
        }
        Ok(())
    }

    fn build_if_else(
        &mut self,
        cond: &TypedLiteral<'arena>,
        if_block: BlockId,
        else_block: BlockId,
        scope: DIScope<'ctx>,
    ) -> Result {
        let if_basic_block = self.ctx.context.append_basic_block(self.current_fn, "then");
        let else_basic_block = self.ctx.context.append_basic_block(self.current_fn, "else");
        let end_basic_block = self
            .ctx
            .context
            .append_basic_block(self.current_fn, "endif");
        self.build_conditional_branch(
            self.basic_value(cond).into_int_value(),
            if_basic_block,
            else_basic_block,
        )?;

        self.goto(if_basic_block);
        self.build_block(if_block, scope, true)?;
        self.terminate(|| self.build_unconditional_branch(end_basic_block))?;

        self.goto(else_basic_block);
        self.build_block(else_block, scope, true)?;
        self.terminate(|| self.build_unconditional_branch(end_basic_block))?;

        self.goto(end_basic_block);
        Ok(())
    }

    pub(super) fn build_if(
        &mut self,
        cond: &TypedLiteral<'arena>,
        if_block: BlockId,
        else_block: Option<BlockId>,
        scope: DIScope<'ctx>,
    ) -> Result {
        if let Some(else_block) = else_block {
            return self.build_if_else(cond, if_block, else_block, scope);
        }
        let if_basic_block = self.ctx.context.append_basic_block(self.current_fn, "then");
        let end_basic_block = self
            .ctx
            .context
            .append_basic_block(self.current_fn, "endif");
        self.build_conditional_branch(
            self.basic_value(cond).into_int_value(),
            if_basic_block,
            end_basic_block,
        )?;

        self.goto(if_basic_block);
        self.build_block(if_block, scope, true)?;
        self.terminate(|| self.build_unconditional_branch(end_basic_block))?;

        self.goto(end_basic_block);
        Ok(())
    }

    pub(super) fn build_while(
        &mut self,
        cond: &TypedLiteral<'arena>,
        cond_block: BlockId,
        body: BlockId,
        scope: DIScope<'ctx>,
    ) -> Result {
        let cond_basic_block = self
            .ctx
            .context
            .append_basic_block(self.current_fn, "while-cond");
        let body_basic_block = self
            .ctx
            .context
            .append_basic_block(self.current_fn, "while-body");
        let end_basic_block = self
            .ctx
            .context
            .append_basic_block(self.current_fn, "while-end");

        self.build_unconditional_branch(cond_basic_block)?;

        self.goto(cond_basic_block);
        self.build_block(cond_block, scope, false)?;
        self.terminate(|| {
            self.build_conditional_branch(
                self.basic_value(cond).into_int_value(),
                body_basic_block,
                end_basic_block,
            )
        })?;

        self.goto(body_basic_block);
        self.build_block(body, scope, true)?;
        self.terminate(|| self.build_unconditional_branch(cond_basic_block))?;

        self.goto(end_basic_block);
        Ok(())
    }
}
