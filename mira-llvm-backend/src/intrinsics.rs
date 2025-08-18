use std::collections::HashMap;

use inkwell::{context::Context, module::Module, types::BasicType, values::FunctionValue};
use mira_spans::Symbol;
use mira_typeck::{Ty, default_types};

use crate::{DefaultTypes, TyKindExt, context::StructsStore};

pub(super) struct Intrinsics<'ctx, 'arena> {
    values: HashMap<Symbol<'arena>, FunctionValue<'ctx>>,
    ctx: &'ctx Context,
}

impl<'ctx, 'tctx> Intrinsics<'ctx, 'tctx> {
    pub fn new(ctx: &'ctx Context) -> Self {
        Self {
            values: HashMap::new(),
            ctx,
        }
    }

    pub fn get_intrinsic(
        &mut self,
        sym: Symbol<'tctx>,
        types: impl Iterator<Item = Ty<'tctx>>,
        ret_ty: Ty<'tctx>,
        default_types: &DefaultTypes<'ctx>,
        structs: &StructsStore<'ctx, 'tctx>,
        module: &Module<'ctx>,
    ) -> FunctionValue<'ctx> {
        if let Some(v) = self.values.get(&sym) {
            return *v;
        }
        let params = types
            .map(|v| {
                v.to_llvm_basic_type(default_types, structs, self.ctx)
                    .into()
            })
            .collect::<Vec<_>>();
        let ty = if ret_ty == default_types::void || ret_ty == default_types::never {
            self.ctx.void_type().fn_type(&params, false)
        } else {
            ret_ty
                .to_llvm_basic_type(default_types, structs, self.ctx)
                .fn_type(&params, false)
        };
        let v = module.add_function(sym.to_str(), ty, None);
        self.values.insert(sym, v);
        v
    }
}
