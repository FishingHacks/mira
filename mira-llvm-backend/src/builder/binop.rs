use crate::float_signed_unsigned;
use mira_typeck::{
    default_types,
    ir::{TypedLiteral, ValueId},
};

use super::{FunctionCodegenContext, Result};

macro_rules! meow {
    ($fnname:ident $sint:ident $uint:ident $float:ident $op:literal) => {
        pub(crate) fn $fnname(
            &mut self,
            dst: ValueId,
            lhs: &TypedLiteral<'arena>,
            rhs: &TypedLiteral<'arena>,
        ) -> Result {
            let lhs = self.basic_value(lhs);
            let rhs = self.basic_value(rhs);
            let ty = self.ir.get_ty(dst);
            let v = float_signed_unsigned!(self, ty, lhs, rhs, $sint(), $uint(), $float(), $op);
            self.push_value(dst, v);
            Ok(())
        }
    };
}

impl<'arena> FunctionCodegenContext<'_, 'arena, '_, '_, '_> {
    meow!(build_add build_int_nsw_add build_int_nuw_add build_float_add "tc should have errored if you try to add 2 non-number values.");
    meow!(build_sub build_int_nsw_sub build_int_nuw_sub build_float_sub "tc should have errored if you try to subtract 2 non-number values.");
    meow!(build_mul build_int_nsw_mul build_int_nuw_mul build_float_mul "tc should have errored if you try to multiply 2 non-number values.");
    meow!(build_div build_int_signed_div build_int_unsigned_div build_float_div "tc should have errored if you try to divide 2 non-number values.");
    meow!(build_rem build_int_signed_rem build_int_unsigned_rem build_float_rem "tc should have errored if you try to take the remainder of 2 non-number values.");

    pub(crate) fn build_binary_and(
        &mut self,
        dst: ValueId,
        lhs: &TypedLiteral<'arena>,
        rhs: &TypedLiteral<'arena>,
    ) -> Result {
        let ty = lhs.to_type(self.ir.scope(), self.ctx.tc_ctx);
        let lhs = self.basic_value(lhs);
        let rhs = self.basic_value(rhs);
        if ty.is_int_like() || ty == default_types::bool {
            self.push_value(
                dst,
                self.build_and(lhs.into_int_value(), rhs.into_int_value(), "")?
                    .into(),
            );
        } else {
            unreachable!("tc should have errored if you try to band 2 non-int/bool values");
        }
        Ok(())
    }

    pub(crate) fn build_binary_or(
        &mut self,
        dst: ValueId,
        lhs: &TypedLiteral<'arena>,
        rhs: &TypedLiteral<'arena>,
    ) -> Result {
        let ty = lhs.to_type(self.ir.scope(), self.ctx.tc_ctx);
        let lhs = self.basic_value(lhs);
        let rhs = self.basic_value(rhs);
        if ty.is_int_like() || ty == default_types::bool {
            self.push_value(
                dst,
                self.build_or(lhs.into_int_value(), rhs.into_int_value(), "")?
                    .into(),
            );
        } else {
            unreachable!("tc should have errored if you try to bor 2 non-int/bool values");
        }
        Ok(())
    }

    pub(crate) fn build_binary_xor(
        &mut self,
        dst: ValueId,
        lhs: &TypedLiteral<'arena>,
        rhs: &TypedLiteral<'arena>,
    ) -> Result {
        let ty = lhs.to_type(self.ir.scope(), self.ctx.tc_ctx);
        let lhs = self.basic_value(lhs);
        let rhs = self.basic_value(rhs);
        if ty.is_int_like() || ty == default_types::bool {
            self.push_value(
                dst,
                self.build_xor(lhs.into_int_value(), rhs.into_int_value(), "")?
                    .into(),
            );
        } else {
            unreachable!("tc should have errored if you try to bxor 2 non-int/bool values");
        }
        Ok(())
    }

    pub(crate) fn build_lshift(
        &mut self,
        dst: ValueId,
        lhs: &TypedLiteral<'arena>,
        rhs: &TypedLiteral<'arena>,
    ) -> Result {
        let lhs = self.basic_value(lhs);
        let rhs = self.basic_value(rhs);
        let ty = &self.ir.get_ty(dst);
        if ty.is_int_like() {
            self.push_value(
                dst,
                self.build_left_shift(lhs.into_int_value(), rhs.into_int_value(), "")?
                    .into(),
            );
            Ok(())
        } else {
            unreachable!("tc should have errored if you try to left shift a non-int value");
        }
    }

    pub(crate) fn build_rshift(
        &mut self,
        dst: ValueId,
        lhs: &TypedLiteral<'arena>,
        rhs: &TypedLiteral<'arena>,
    ) -> Result {
        let lhs = self.basic_value(lhs);
        let rhs = self.basic_value(rhs);
        let ty = &self.ir.get_ty(dst);
        if ty.is_int_like() {
            self.push_value(
                dst,
                self.build_right_shift(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    ty.is_signed(),
                    "",
                )?
                .into(),
            );
            Ok(())
        } else {
            unreachable!("tc should have errored if you try to right shift a non-int value");
        }
    }
}
