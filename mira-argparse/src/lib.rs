use std::{
    ffi::OsString,
    fmt::Debug,
    path::{Path, PathBuf},
};

use clap::{Args, CommandFactory, Parser, Subcommand, ValueEnum};
use mira_target::{NATIVE_TARGET, Target};

#[derive(Parser, Debug)]
#[command(arg_required_else_help(true))]
pub struct MiraArgs {
    #[arg(short, long)]
    pub version: bool,
    #[arg(long)]
    pub about: bool,
    #[command(subcommand)]
    pub cmd: Option<Action>,
}

pub fn print_help() -> std::io::Result<()> {
    <MiraArgs as CommandFactory>::command().print_help()
}

pub fn parse_args() -> MiraArgs {
    MiraArgs::parse()
}

#[derive(Subcommand, Debug)]
#[allow(clippy::large_enum_variant)]
pub enum Action {
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
#[value(rename_all = "PascalCase")]
pub enum OptimizationMode {
    Debugger,
    Debug,
    ReleaseSafe,
    ReleaseFast,
    ReleaseSmall,
    ReleaseTiny,
}

impl std::fmt::Display for OptimizationMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

//┌─┐└┘│├┬┴┼┤
// ┌┬┐ ┌─┐
// ├┼┤ │ │
// └┴┘ └─┘
//
//┌──────────────┬───────────┬────────────────┐
//│ Name         │ Opt level │ Runtime Checks │
//├──────────────┼───────────┼────────────────┤
//│ Debugger     │    -O0    │      true      │
//│ Debug        │    -O2    │      true      │
//│ ReleaseSafe  │    -O3    │      true      │
//│ ReleaseFast  │    -O3    │      false     │
//│ ReleaseSmall │    -Os    │      false     │
//│ ReleaseTiny  │    -Oz    │      false     │
//└──────────────┴───────────┴────────────────┘
//┌──────────────┬───────────┬────────────────┐
//│ Name         │ Opt level │ Runtime Checks │
//┝━━━━━━━━━━━━━━┿━━━━━━━━━━━┿━━━━━━━━━━━━━━━━┥
//│ Debugger     │    -O0    │      true      │
//│ Debug        │    -O2    │      true      │
//│ ReleaseSafe  │    -O3    │      true      │
//│ ReleaseFast  │    -O3    │      false     │
//│ ReleaseSmall │    -Os    │      false     │
//│ ReleaseTiny  │    -Oz    │      false     │
//└──────────────┴───────────┴────────────────┘
//┌──────────────┬───────────┬────────────────┐
//│ Name         │ Opt level │ Runtime Checks │
//╞══════════════╪═══════════╪════════════════╡
//│ Debugger     │    -O0    │      true      │
//│ Debug        │    -O2    │      true      │
//│ ReleaseSafe  │    -O3    │      true      │
//│ ReleaseFast  │    -O3    │      false     │
//│ ReleaseSmall │    -Os    │      false     │
//│ ReleaseTiny  │    -Oz    │      false     │
//└──────────────┴───────────┴────────────────┘

#[derive(Clone, Debug)]
pub enum PathOrStdout {
    Stdout,
    Path(Box<Path>),
}

impl From<OsString> for PathOrStdout {
    fn from(value: OsString) -> Self {
        if value == "-" {
            Self::Stdout
        } else {
            Self::Path(PathBuf::from(value).into_boxed_path())
        }
    }
}

#[derive(Debug, Args)]
pub struct CompileArgs {
    /// The target to compile to
    #[arg(short, long, default_value_t = NATIVE_TARGET)]
    pub target: Target,
    /// File to emit the ir to (or specify `-` for stdout)
    #[arg(long)]
    pub emit_ir: Option<PathOrStdout>,
    /// File to emit the llvm ir to (or specify `-` for stdout)
    #[arg(long)]
    pub emit_llvm_ir: Option<PathOrStdout>,
    /// File to emit the llvm bitcode to (or specify `-` for stdout)
    #[arg(long)]
    pub emit_llvm_bc: Option<PathOrStdout>,
    /// File to emit the assembly to (or specify `-` for stdout)
    #[arg(long)]
    pub emit_asm: Option<PathOrStdout>,
    /// The file to emit the object (.o) file to
    #[arg(long)]
    pub emit_obj: Option<PathBuf>,
    /// The path to the linker script used by the linker
    #[arg(long, short = 'l')]
    pub linker_script: Option<PathBuf>,
    /// The Optimization level of the compilation.
    ///
    /// ┌──────────────┬───────────┬────────────────┐
    /// │ Name         │ Opt level │ Runtime Checks │
    /// ┝━━━━━━━━━━━━━━┿━━━━━━━━━━━┿━━━━━━━━━━━━━━━━┥
    /// │ Debugger     │    -O0    │      true      │
    /// │ Debug        │    -O2    │      true      │
    /// │ ReleaseSafe  │    -O3    │      true      │
    /// │ ReleaseFast  │    -O3    │      false     │
    /// │ ReleaseSmall │    -Os    │      false     │
    /// │ ReleaseTiny  │    -Oz    │      false     │
    /// └──────────────┴───────────┴────────────────┘
    #[arg(long, short = 'o', default_value_t = OptimizationMode::ReleaseSafe, verbatim_doc_comment)]
    pub opt: OptimizationMode,
    /// A String representing the cpu features you want to enable
    #[arg(long)]
    pub cpu_features: Option<String>,
    /// Additional arguments to pass to the linker
    #[arg(long, short = 'L')]
    pub linker_args: Vec<String>,
    /// Prints out a lot of info about the compilation
    #[arg(long)]
    pub verbose: bool,
    #[arg(long, short)]
    pub run: bool,
    /// Specify this flag if you don't want mirac to append the appropriate extension to your
    /// binary file
    #[arg(long)]
    pub without_extension: bool,
    /// Specify if you want to generate a shared file (.dll / .so / .dylib)
    #[arg(long, short)]
    pub shared: bool,
    /// The mira file to compile
    pub src: PathBuf,
    /// The output file. Don't specify if you don't want to get a binary
    pub out_file: Option<PathBuf>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum EditorMode {
    Cli,
    Editor,
}

#[derive(Debug, Args)]
pub struct ReplArgs {
    #[arg(value_enum, short = 'm', long = "mode")]
    /// The mode to start the repl in, editor causes your favorite cli-based editor to open.
    pub mode: Option<EditorMode>,
    /// Specify the file to load, if any
    pub file: Option<PathBuf>,
}

#[derive(Debug, Args)]
pub struct ExpandArgs {
    /// the file to expand
    pub file: PathBuf,
    /// File to emit the ir to (or specify `-` for stdout)
    pub output: Option<PathOrStdout>,
}
