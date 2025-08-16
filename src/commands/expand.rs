use std::path::PathBuf;

use clap::Args;
use mira_driver::{Arena, Output, UnicodePrinter};
use mira_typeck::GlobalContext;

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
    let res = mira_driver::expand_macros(ctx.ctx(), args.file.into(), to_emit(args.output));
    if let Err(e) = res {
        let mut formatter = ctx.make_diagnostic_formatter(UnicodePrinter::new(), Output::Stderr);
        for err in e {
            formatter
                .display_diagnostic(err)
                .expect("failed to display diagnostics");
        }
    }
}
