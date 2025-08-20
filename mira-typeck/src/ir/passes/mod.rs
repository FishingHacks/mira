use crate::{TypeCtx, ir::IR};

mod typename_intrinsic;

pub fn run_passes<'arena>(ir: &mut IR<'arena>, tcx: TypeCtx<'arena>) {
    ir.visit(&mut typename_intrinsic::TypenameIntrinsicPass, tcx);
}
