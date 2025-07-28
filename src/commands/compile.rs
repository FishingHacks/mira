use std::{
    error::Error,
    fmt::Debug,
    path::{Path, PathBuf},
    process::Command,
    sync::Arc,
    time::{SystemTime, UNIX_EPOCH},
};

use clap::{Args, ValueEnum};
use mira::{
    codegen::CodegenConfig,
    context::GlobalContext,
    linking::{run_full_compilation_pipeline, FullCompilationOptions},
    module_resolution::{
        AbsoluteResolver, BasicModuleResolver, RelativeResolver, SingleFileModuleResolver,
        SingleModuleResolver,
    },
    target::{Target, NATIVE_TARGET},
    Arena, ModuleResolver,
};

use crate::libfinder;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
#[value(rename_all = "PascalCase")]
enum OptimizationMode {
    Debugger,
    Debug,
    ReleaseSafe,
    ReleaseFast,
    ReleaseSmall,
    ReleaseTiny,
}

impl OptimizationMode {
    pub fn to_codegen_ops(self, target: Target, cpu_features: Option<&str>) -> CodegenConfig {
        match self {
            OptimizationMode::Debugger => CodegenConfig::new_debug_unoptimized(),
            OptimizationMode::Debug => CodegenConfig::new_debug(),
            OptimizationMode::ReleaseSafe => CodegenConfig::new_release_safe(),
            OptimizationMode::ReleaseFast => CodegenConfig::new_release_fast(),
            OptimizationMode::ReleaseSmall => CodegenConfig::new_release_small(),
            OptimizationMode::ReleaseTiny => CodegenConfig::new_release_tiny(),
        }
        .cpu_features(cpu_features.unwrap_or(""))
        .target(target)
    }
}

impl std::fmt::Display for OptimizationMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

//┌─┐└┘│├┬┴┼┴┤
const LONG_HELP_OPTMODE: &str = r#"The Optimization level of the compilation.

┌──────────────┬───────────┬────────────────┐
│ Name         │ Opt level │ Runtime Checks │
├──────────────┼───────────┼────────────────┤
│ Debugger     │    -O0    │      true      │
│ Debug        │    -O2    │      true      │
│ ReleaseSafe  │    -O3    │      true      │
│ ReleaseFast  │    -O3    │      false     │
│ ReleaseSmall │    -Os    │      false     │
│ ReleaseTiny  │    -Oz    │      false     │
└──────────────┴───────────┴────────────────┘"#;

#[derive(Debug, Args)]
pub struct CompileArgs {
    /// The target to compile to
    #[arg(short, long, default_value_t = NATIVE_TARGET)]
    target: Target,
    #[arg(long)]
    emit_ir: Option<PathBuf>,
    #[arg(long)]
    emit_llvm_ir: Option<PathBuf>,
    #[arg(long)]
    emit_llvm_bc: Option<PathBuf>,
    #[arg(long)]
    emit_asm: Option<PathBuf>,
    /// The file to emit the object (.o) file to
    #[arg(long)]
    emit_obj: Option<PathBuf>,
    #[arg(long = "package", short)]
    /// Specify paths to directories you want to be able to have available as modules that you can
    /// access without having to specify a relative path. Directories in `.mira/modules` are
    /// automatically included.
    packages: Vec<PathBuf>,
    /// The path to the linker script used by the linker
    #[arg(long, short = 'l')]
    linker_script: Option<PathBuf>,
    /// The Optimization level of the compilation.
    ///
    /// | Name         | Opt level | Runtime Checks |
    /// | Debugger     |    -O0    |      true      |
    /// | Debug        |    -O2    |      true      |
    /// | ReleaseSafe  |    -O3    |      true      |
    /// | ReleaseFast  |    -O3    |      false     |
    /// | ReleaseSmall |    -Os    |      false     |
    /// | ReleaseTiny  |    -Oz    |      false     |
    #[arg(long, short = 'o', default_value_t = OptimizationMode::ReleaseSafe, long_help = LONG_HELP_OPTMODE)]
    opt: OptimizationMode,
    /// A String representing the cpu features you want to enable
    #[arg(long)]
    cpu_features: Option<String>,
    /// Additional arguments to pass to the linker
    #[arg(long, short = 'L')]
    linker_args: Vec<String>,
    /// Prints out a lot of info about the compilation
    #[arg(long, short)]
    verbose: bool,
    #[arg(long, short)]
    run: bool,
    /// Specify this flag if you don't want mirac to append the appropriate extension to your
    /// binary file
    #[arg(long)]
    without_extension: bool,
    /// Specify if you want to generate a shared file (.dll / .so / .dylib)
    #[arg(long, short)]
    shared: bool,
    /// The mira file to compile
    src: PathBuf,
    /// The output file. Don't specify if you don't want to get a binary
    out_file: Option<PathBuf>,
}

pub fn compile_main(mut args: CompileArgs) -> Result<(), Box<dyn Error>> {
    let Some(libmirastd) = libfinder::find_library("mirastd") else {
        println!("Failed to find mirastd");
        return Ok(());
    };
    let Ok(root_directory) = std::env::current_dir() else {
        println!("Failed to get the current directory");
        return Ok(());
    };
    let file: Arc<Path> = args.src.into();
    if args.run && args.out_file.is_none() {
        args.out_file = Some(file.with_extension(""));
        args.without_extension = false;
    }

    let mut resolvers: Vec<Box<dyn ModuleResolver>> = vec![
        Box::new(RelativeResolver),
        Box::new(AbsoluteResolver),
        Box::new(SingleModuleResolver("std".into(), libmirastd.into())),
        Box::new(SingleFileModuleResolver::new(
            "\0__root",
            file.clone(),
            None,
        )),
        Box::new(BasicModuleResolver(vec![
            ".mira/modules/$name",
            ".mira/modules/$name/src",
        ])),
    ];

    for package in args.packages {
        let Some(filename) = package.file_name() else {
            println!(
                "Failed to get the directory name for package {}",
                package.display()
            );
            return Ok(());
        };
        resolvers.push(Box::new(SingleModuleResolver(
            filename.to_string_lossy().into_owned(),
            package.into(),
        )));
    }

    let mut opts = FullCompilationOptions::new(file, root_directory.into(), resolvers.into());
    opts.set_target(args.target)
        .set_codegen_opts(
            args.opt
                .to_codegen_ops(args.target, args.cpu_features.as_deref()),
        )
        .set_linker_script(args.linker_script.as_deref())
        .set_verbose_printing(args.verbose)
        .shared_object(args.shared)
        .set_additional_linker_args(&args.linker_args)
        .always_include_file(Some("std/prelude.mr".into()))
        .add_extension_to_exe = !args.without_extension;
    opts.exec_path = args.out_file;
    if let Some(path) = &args.emit_ir {
        opts.ir_writer = Some(Box::new(
            std::fs::File::options()
                .create(true)
                .write(true)
                .truncate(true)
                .open(path)?,
        ));
    }
    if let Some(path) = &args.emit_llvm_ir {
        opts.llvm_ir_writer = Some(Box::new(
            std::fs::File::options()
                .create(true)
                .write(true)
                .truncate(true)
                .open(path)?,
        ));
    }
    if let Some(path) = &args.emit_llvm_bc {
        opts.llvm_bc_writer = Some(Box::new(
            std::fs::File::options()
                .create(true)
                .write(true)
                .truncate(true)
                .open(path)?,
        ));
    }
    if let Some(path) = &args.emit_asm {
        opts.asm_writer = Some(Box::new(
            std::fs::File::options()
                .create(true)
                .write(true)
                .truncate(true)
                .open(path)?,
        ));
    }
    let mut tmp_obj_path = None;
    opts.obj_path = args.emit_obj.or_else(|| {
        opts.exec_path.is_some().then(|| {
            let path: PathBuf = format!(
                "/tmp/__mira_output_{}.o",
                SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .expect("time has gone backwards")
                    .as_secs()
            )
            .into();
            tmp_obj_path = Some(path.clone());
            path
        })
    });

    let arena = Arena::new();
    let ctx = GlobalContext::new(&arena);
    let ctx = ctx.share();
    let exec_path = match run_full_compilation_pipeline(ctx, opts) {
        Err(e) => {
            println!("Failed to compile:");
            for e in e.iter() {
                println!("{e}");
            }
            drop(e);
            if let Some(v) = tmp_obj_path {
                _ = std::fs::remove_file(v);
            }
            return Ok(());
        }
        _ if !args.run => return Ok(()),
        Ok(None) => {
            println!("could not find executable despite run being specified.. this is a bug, please report it");
            return Ok(());
        }
        Ok(Some(exec_path)) => exec_path,
    };

    let mut cmd = Command::new(&exec_path);
    if args.verbose {
        println!("[INFO] Running {cmd:?}");
        println!("----[ run output ]----");
    }
    match cmd.spawn().and_then(|mut v| v.wait()) {
        Err(e) => println!("-> failed to run the program: {e:?}"),
        Ok(v) if !v.success() => println!("\n\n  process exited with error: {v:?}"),
        Ok(_) => println!("\n"),
    }

    Ok(())
}
