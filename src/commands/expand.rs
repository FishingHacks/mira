use mira_argparse::ExpandArgs;
use mira_driver::{Context, ProgressBarStyle};
use mira_spans::Arena;
use mira_typeck::GlobalContext;

use super::compile::{emit_diagnostics, to_emit};

pub fn expand_main(args: ExpandArgs) {
    let arena = Arena::new();
    let ctx = GlobalContext::new(&arena);
    let mut dcx = Context::new(ctx.ty_ctx(), ProgressBarStyle::Normal);
    let Ok(tokens) = dcx
        .expand_macros(args.file.into())
        .map_err(|v| emit_diagnostics(&dcx, v.into_iter()))
    else {
        return;
    };
    let Some(emit_method) = to_emit(Some(args.output)) else {
        return;
    };
    if let Err(e) = dcx.emit_tokens(&tokens, emit_method) {
        emit_diagnostics(&dcx, e.into_iter());
    }
    drop(dcx);
}
