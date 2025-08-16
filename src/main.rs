mod commands;
mod editor;
mod libfinder;
mod repl;
use clap::{CommandFactory, Parser, Subcommand};
use commands::{
    about::print_about,
    compile::{compile_main, CompileArgs},
    expand::{expand_main, ExpandArgs},
    repl::{repl_main, ReplArgs},
};
use mira::{
    target::{Target, NATIVE_TARGET},
    VERSION as VER,
};
use std::error::Error;

const MIRAC_VERSION: &str = const {
    match option_env!("MIRAC_PKG_OVERRIDE") {
        Some(v) => v,
        None => env!("CARGO_PKG_VERSION"),
    }
};
const COMMIT: Option<&str> = option_env!("MIRAC_COMMIT_HASH");
const COMMIT_SHORT: Option<&str> = option_env!("MIRAC_COMMIT_HASH_SHORT");
const COMMIT_DATE: Option<&str> = option_env!("MIRAC_COMMIT_DATE");

#[derive(Parser, Debug)]
#[command(arg_required_else_help(true))]
struct Args {
    #[arg(short, long)]
    version: bool,
    #[arg(long)]
    about: bool,
    #[command(subcommand)]
    cmd: Option<Action>,
}

#[derive(Subcommand, Debug)]
#[allow(clippy::large_enum_variant)]
enum Action {
    /// Prints the current mira and mirac version
    Version,
    /// Displays some information about mira and mirac
    About,
    /// Compile a mira program
    Compile(CompileArgs),
    /// Print supported targets
    Targets,
    /// Launches a repl to edit, compile and run your code in.
    Repl(ReplArgs),
    /// Print out all the tokens after running macro expansion on a file
    Expand(ExpandArgs),
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    if args.about || args.version {
        print_about();
        return Ok(());
    }
    let Some(action) = args.cmd else {
        <Args as CommandFactory>::command().print_help()?;
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
