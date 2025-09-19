use super::Result;
use inkwell::{FloatPredicate, IntPredicate, debug_info::DIScope};
use mira_typeck::ir::{BlockId, TypedLiteral, ValueId};

use super::FunctionCodegenContext;
use crate::float_signed_unsigned;

impl<'ctx, 'arena> FunctionCodegenContext<'ctx, 'arena, '_, '_, '_> {
    pub(super) fn build_lt(
        &mut self,
        dst: ValueId,
        lhs: &TypedLiteral<'arena>,
        rhs: &TypedLiteral<'arena>,
    ) -> Result {
        let ty = lhs
            .to_primitive_type(self.ir.scope(), self.ctx.tc_ctx)
            .expect("tc should have errored if you try to compare 2 non-number values");
        let ty = self.substitute(ty);

        let lhs = self.basic_value(lhs);
        let rhs = self.basic_value(rhs);

        let v = float_signed_unsigned!(
            self,
            ty,
            lhs,
            rhs,
            build_int_compare(IntPredicate::SLT),
            build_int_compare(IntPredicate::ULT),
            build_float_compare(FloatPredicate::ULT),
            "tc should have errored if you try to compare 2 non-number values"
        );
        self.push_value(dst, v);
        Ok(())
    }

    pub(super) fn build_gt(
        &mut self,
        dst: ValueId,
        lhs: &TypedLiteral<'arena>,
        rhs: &TypedLiteral<'arena>,
    ) -> Result {
        let ty = lhs
            .to_primitive_type(self.ir.scope(), self.ctx.tc_ctx)
            .expect("tc should have errored if you try to compare 2 non-number values");
        let ty = self.substitute(ty);

        let lhs = self.basic_value(lhs);
        let rhs = self.basic_value(rhs);

        let v = float_signed_unsigned!(
            self,
            ty,
            lhs,
            rhs,
            build_int_compare(IntPredicate::SGT),
            build_int_compare(IntPredicate::UGT),
            build_float_compare(FloatPredicate::UGT),
            "tc should have errored if you try to compare 2 non-number values"
        );
        self.push_value(dst, v);
        Ok(())
    }

    pub(super) fn build_lte(
        &mut self,
        dst: ValueId,
        lhs: &TypedLiteral<'arena>,
        rhs: &TypedLiteral<'arena>,
    ) -> Result {
        let ty = lhs
            .to_primitive_type(self.ir.scope(), self.ctx.tc_ctx)
            .expect("tc should have errored if you try to compare 2 non-number values");
        let ty = self.substitute(ty);

        let lhs = self.basic_value(lhs);
        let rhs = self.basic_value(rhs);

        let v = float_signed_unsigned!(
            self,
            ty,
            lhs,
            rhs,
            build_int_compare(IntPredicate::SLE),
            build_int_compare(IntPredicate::ULE),
            build_float_compare(FloatPredicate::ULE),
            "tc should have errored if you try to compare 2 non-number values"
        );
        self.push_value(dst, v);
        Ok(())
    }

    pub(super) fn build_gte(
        &mut self,
        dst: ValueId,
        lhs: &TypedLiteral<'arena>,
        rhs: &TypedLiteral<'arena>,
    ) -> Result {
        let ty = lhs
            .to_primitive_type(self.ir.scope(), self.ctx.tc_ctx)
            .expect("tc should have errored if you try to compare 2 non-number values");
        let ty = self.substitute(ty);

        let lhs = self.basic_value(lhs);
        let rhs = self.basic_value(rhs);

        let v = float_signed_unsigned!(
            self,
            ty,
            lhs,
            rhs,
            build_int_compare(IntPredicate::SGE),
            build_int_compare(IntPredicate::UGE),
            build_float_compare(FloatPredicate::UGE),
            "tc should have errored if you try to compare 2 non-number values"
        );
        self.push_value(dst, v);
        Ok(())
    }

    pub(super) fn build_eq(
        &mut self,
        dst: ValueId,
        lhs: &TypedLiteral<'arena>,
        rhs: &TypedLiteral<'arena>,
    ) -> Result {
        let ty = lhs
            .to_primitive_type(self.ir.scope(), self.ctx.tc_ctx)
            .expect("tc should have errored if you try to compare 2 non-number values");
        let ty = self.substitute(ty);

        let lhs = self.basic_value(lhs);
        let rhs = self.basic_value(rhs);

        let v = float_signed_unsigned!(
            self,
            ty,
            lhs,
            rhs,
            build_int_compare(IntPredicate::EQ),
            build_int_compare(IntPredicate::EQ),
            build_float_compare(FloatPredicate::UEQ),
            "tc should have errored if you try to compare 2 non-number values"
        );
        self.push_value(dst, v);
        Ok(())
    }

    pub(super) fn build_neq(
        &mut self,
        dst: ValueId,
        lhs: &TypedLiteral<'arena>,
        rhs: &TypedLiteral<'arena>,
    ) -> Result {
        let ty = lhs
            .to_primitive_type(self.ir.scope(), self.ctx.tc_ctx)
            .expect("tc should have errored if you try to compare 2 non-number values");
        let ty = self.substitute(ty);

        let lhs = self.basic_value(lhs);
        let rhs = self.basic_value(rhs);

        let v = float_signed_unsigned!(
            self,
            ty,
            lhs,
            rhs,
            build_int_compare(IntPredicate::NE),
            build_int_compare(IntPredicate::NE),
            build_float_compare(FloatPredicate::UNE),
            "tc should have errored if you try to compare 2 non-number values"
        );
        self.push_value(dst, v);
        Ok(())
    }

    pub(super) fn build_logical_and(
        &mut self,
        dst: ValueId,
        left: &TypedLiteral<'arena>,
        right: &TypedLiteral<'arena>,
        rhs_block: BlockId,
        scope: DIScope<'ctx>,
    ) -> Result {
        let [rhs_bb, end_bb] = self.make_bb(["log_and_right", "log_and_end"]);

        let left_value = self.basic_value(left);
        self.build_conditional_branch(left_value.into_int_value(), rhs_bb, end_bb)?;
        let cur_blk = self.current_block;
        self.goto(rhs_bb);

        self.build_block(rhs_block, scope, false)?;
        let rhs_value = self.basic_value(right);
        self.build_unconditional_branch(end_bb)?;

        self.goto(end_bb);
        let res = self.build_phi(self.ctx.default_types.bool, "")?;
        res.add_incoming(&[
            (&self.ctx.default_types.bool.const_zero(), cur_blk),
            (&rhs_value, rhs_bb),
        ]);
        self.push_value(dst, res.as_basic_value());
        Ok(())
    }

    pub(super) fn build_logical_or(
        &mut self,
        dst: ValueId,
        left: &TypedLiteral<'arena>,
        right: &TypedLiteral<'arena>,
        rhs_block: BlockId,
        scope: DIScope<'ctx>,
    ) -> Result {
        let [rhs_bb, end_bb] = self.make_bb(["log_or_right", "log_or_end"]);

        let left_value = self.basic_value(left);
        self.build_conditional_branch(left_value.into_int_value(), end_bb, rhs_bb)?;
        let cur_blk = self.current_block;
        self.goto(rhs_bb);

        self.build_block(rhs_block, scope, false)?;
        let rhs_value = self.basic_value(right);
        self.build_unconditional_branch(end_bb)?;

        self.goto(end_bb);
        let res = self.build_phi(self.ctx.default_types.bool, "")?;
        res.add_incoming(&[
            (&self.ctx.default_types.bool.const_int(1, false), cur_blk),
            (&rhs_value, rhs_bb),
        ]);
        self.push_value(dst, res.as_basic_value());
        Ok(())
    }
}
