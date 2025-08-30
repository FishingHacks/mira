use mira_argparse::ExpandArgs;
use mira_driver::{ContextData, DiagEmitter};
use mira_spans::Arena;

use super::compile::to_emit;

pub fn expand_main(args: ExpandArgs) {
    let arena = Arena::new();
    let (ctx, data) = ContextData::new(&arena, None, DiagEmitter::Stdout);
    let mut context = data.to_context(ctx.ty_ctx());
    let Ok(tokens) = context.expand_macros(args.file.into()) else {
        return;
    };
    let Some(emit_method) = to_emit(Some(args.output)) else {
        return;
    };
    _ = context.emit_tokens(&tokens, emit_method);
}
