use std::path::PathBuf;

use clap::Args;
use mira::{context::GlobalContext, Arena};
use mira::{Output, UnicodePrinter};

use super::compile::{to_emit, PathOrStdout};

#[derive(Debug, Args)]
pub struct ExpandArgs {
    /// the file to expand
    file: PathBuf,
    /// File to emit the ir to (or specify `-` for stdout)
    output: Option<PathOrStdout>,
}

pub fn expand_main(args: ExpandArgs) {
    let arena = Arena::new();
    let ctx = GlobalContext::new(&arena);
    let s_ctx = ctx.share();
    let res = mira_driver::expand_macros(s_ctx, args.file.into(), to_emit(args.output));
    if let Err(e) = res {
        let mut formatter = s_ctx.make_diagnostic_formatter(UnicodePrinter::new(), Output::Stderr);
        for err in e {
            formatter
                .display_diagnostic(err)
                .expect("failed to display diagnostics");
        }
    }
}
