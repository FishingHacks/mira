mod commands;
mod editor;
mod libfinder;
mod repl;
use clap::{CommandFactory, Parser, Subcommand};
use commands::{
    about::print_about,
    compile::{compile_main, CompileArgs},
    repl::{repl_main, ReplArgs},
};
use mira::{
    target::{Target, NATIVE_TARGET},
    AUTHORS as MIRA_AUTHORS, VERSION as VER,
};
use std::{collections::HashSet, error::Error, sync::LazyLock};

const MIRAC_VERSION: &str = env!("CARGO_PKG_VERSION");
const MIRAC_AUTHORS: &str = env!("CARGO_PKG_AUTHORS");
static AUTHORS: LazyLock<Vec<&'static str>> = LazyLock::new(|| {
    let hashset = MIRA_AUTHORS
        .split(':')
        .chain(MIRAC_AUTHORS.split(':'))
        .collect::<HashSet<_>>();
    hashset.into_iter().collect()
});

#[derive(Parser, Debug)]
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
