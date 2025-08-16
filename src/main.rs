mod commands;
mod editor;
mod libfinder;
mod repl;
use commands::{about::print_about, compile::compile_main, expand::expand_main, repl::repl_main};
use mira_argparse::{parse_args, print_help, Action};
use mira_target::{Target, NATIVE_TARGET};
use std::error::Error;

const VERSION: &str = const {
    match option_env!("MIRAC_PKG_OVERRIDE") {
        Some(v) => v,
        None => env!("CARGO_PKG_VERSION"),
    }
};
const COMMIT: Option<&str> = option_env!("MIRAC_COMMIT_HASH");
const COMMIT_SHORT: Option<&str> = option_env!("MIRAC_COMMIT_HASH_SHORT");
const COMMIT_DATE: Option<&str> = option_env!("MIRAC_COMMIT_DATE");

fn main() -> Result<(), Box<dyn Error>> {
    let args = parse_args();
    if args.about || args.version {
        print_about();
        return Ok(());
    }
    let Some(action) = args.cmd else {
        print_help()?;
        return Ok(());
    };
    match action {
        Action::Version | Action::About => {
            print_about();
            Ok(())
        }
        Action::Expand(args) => {
            expand_main(args);
            Ok(())
        }
        Action::Targets => {
            println!("Native Target: {NATIVE_TARGET}");
            println!("Supported Targets:");
            for target in Target::targets() {
                println!("  {target}");
            }
            Ok(())
        }
        Action::Compile(args) => compile_main(args),
        Action::Repl(args) => repl_main(args),
    }
}
