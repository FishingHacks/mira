mod plumbing;
use crate::TypedModule;
use crate::define_system;
use mira_common::store::StoreKey;
use mira_macros::queries;
pub use plumbing::{DefaultCache, QueryCache, QueryKey, SingleCache};

queries! {
    #[manually_allocated(u8)]
    mangle_module(StoreKey<TypedModule<'arena>>) -> &'arena str;

    #[manually_allocated(u8)]
    get_module_path(StoreKey<TypedModule<'arena>>) -> &'arena str;

    a(()) -> u8;
    b(()) -> u8;
}

#[allow(clippy::module_inception)]
pub mod queries {
    #[allow(unused_imports)]
    use super::*;
    use crate::define_modules;

    all_queries! { define_modules }
}
all_queries! { names define_system }

#[cfg(test)]
mod test {
    use std::{sync::Arc, time::Duration};

    use crate::{GlobalContext, TypeckCtx};

    use mira_common::store::{AssociatedStore, Store};
    use mira_context::DiagEmitter;
    use mira_errors::{AsciiPrinter, Styles};
    use mira_parser::module::ModuleContext;
    use mira_spans::Arena;

    #[test]
    #[should_panic = r#"Error:
error: Cycle detected when a
  = note: ...which then immediately requires a, completing the cycle.
"#]
    fn cyclic_condition1() {
        let arena = Arena::new();
        let gcx = GlobalContext::new(
            &arena,
            DiagEmitter::NoFail,
            Box::new(AsciiPrinter::new()),
            Styles::NO_COLORS,
            |providers| {
                providers.a = |ctx, ()| ctx.a(());
            },
        );
        let tcx = gcx.ty_ctx();
        let mod_ctx = Arc::new(ModuleContext::new(
            Store::new(),
            gcx.ctx(),
            AssociatedStore::new(),
        ));
        let tc_ctx = TypeckCtx::new(tcx, mod_ctx);
        tc_ctx.a(());
    }

    #[test]
    #[should_panic = r#"Error:
error: Cycle detected when a
  = note: ...which requires b
  = note: ...which then again requires a, completing the cycle.

"#]
    fn cyclic_condition2() {
        let arena = Arena::new();
        let gcx = GlobalContext::new(
            &arena,
            DiagEmitter::NoFail,
            Box::new(AsciiPrinter::new()),
            Styles::NO_COLORS,
            |providers| {
                providers.a = |ctx, ()| ctx.b(());
                providers.b = |ctx, ()| ctx.a(());
            },
        );
        let tcx = gcx.ty_ctx();
        let mod_ctx = Arc::new(ModuleContext::new(
            Store::new(),
            gcx.ctx(),
            AssociatedStore::new(),
        ));
        let tc_ctx = TypeckCtx::new(tcx, mod_ctx);
        tc_ctx.a(());
    }

    #[test]
    #[should_panic = r#"Error:
error: Cycle detected when b
  = note: ...which then immediately requires b, completing the cycle.
"#]
    fn cyclic_condition3() {
        let arena = Arena::new();
        let gcx = GlobalContext::new(
            &arena,
            DiagEmitter::NoFail,
            Box::new(AsciiPrinter::new()),
            Styles::NO_COLORS,
            |providers| {
                providers.a = |ctx, ()| ctx.b(());
                providers.b = |ctx, ()| ctx.b(());
            },
        );
        let tcx = gcx.ty_ctx();
        let mod_ctx = Arc::new(ModuleContext::new(
            Store::new(),
            gcx.ctx(),
            AssociatedStore::new(),
        ));
        let tc_ctx = TypeckCtx::new(tcx, mod_ctx);
        tc_ctx.a(());
    }

    #[test]
    fn nopanic() {
        let arena = Arena::new();
        let gcx = GlobalContext::new(
            &arena,
            DiagEmitter::NoFail,
            Box::new(AsciiPrinter::new()),
            Styles::NO_COLORS,
            |providers| {
                providers.a = |_, ()| {
                    println!("starting a");
                    std::thread::sleep(Duration::from_millis(500));
                    println!("finishing a");
                    0
                }
            },
        );
        let tcx = gcx.ty_ctx();
        let mod_ctx = Arc::new(ModuleContext::new(
            Store::new(),
            gcx.ctx(),
            AssociatedStore::new(),
        ));
        let tc_ctx = TypeckCtx::new(tcx, mod_ctx);

        std::thread::scope(|s| {
            s.spawn(|| {
                tc_ctx.a(());
            });

            tc_ctx.a(());
        });
    }
}
