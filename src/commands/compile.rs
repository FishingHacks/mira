use std::{
    error::Error,
    path::{Path, PathBuf},
    process::Command,
    sync::Arc,
    time::{SystemTime, UNIX_EPOCH},
};

use mira_argparse::{CompileArgs, OptimizationMode, PathOrStdout};
use mira_driver::{
    run_full_compilation_pipeline, Arena, EmitMethod, FullCompilationOptions, LibraryTree, Output,
    UnicodePrinter,
};
use mira_llvm_backend::CodegenConfig;
use mira_target::Target;
use mira_typeck::GlobalContext;

use crate::libfinder;

pub fn opt_mode_to_codegen_cfg(
    mode: OptimizationMode,
    target: Target,
    cpu_features: Option<&str>,
) -> CodegenConfig {
    match mode {
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

pub(super) fn to_emit(value: Option<PathOrStdout>) -> EmitMethod {
    match value {
        Some(PathOrStdout::Stdout) => EmitMethod::Stdout,
        Some(PathOrStdout::Path(p)) => EmitMethod::file(p),
        None => EmitMethod::None,
    }
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

    let mut libtree = LibraryTree::new();
    let stdlib_main_file = libmirastd.join("lib.mr").into();
    let libstd = libtree
        .build_library(libmirastd.into(), stdlib_main_file)
        .build();
    let mainlib = libtree
        .build_library(root_directory.into(), file)
        .with_dependency("std", libstd)
        .build();
    libtree.main_library(mainlib);
    let mut opts = FullCompilationOptions::new(libtree);
    opts.set_target(args.target)
        .set_codegen_opts(opt_mode_to_codegen_cfg(
            args.opt,
            args.target,
            args.cpu_features.as_deref(),
        ))
        .set_linker_script(args.linker_script.as_deref())
        .set_verbose_printing(args.verbose)
        .shared_object(args.shared)
        .set_additional_linker_args(&args.linker_args)
        .ir_writer(to_emit(args.emit_ir))
        .llvm_bc_writer(to_emit(args.emit_llvm_bc))
        .llvm_ir_writer(to_emit(args.emit_llvm_ir))
        .asm_writer(to_emit(args.emit_asm))
        .add_extension_to_exe = !args.without_extension;
    opts.exec_path = args.out_file;
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

    let exec_path = {
        let arena = Arena::new();
        let ctx = GlobalContext::new(&arena);
        let s_ctx = ctx.ty_ctx();
        let mut res = run_full_compilation_pipeline(s_ctx, opts);
        let path = match &mut res {
            Err(e) => {
                println!("Failed to compile:");
                let mut fmt =
                    s_ctx.make_diagnostic_formatter(UnicodePrinter::new(), Output::Stderr);
                for e in std::mem::take(e) {
                    fmt.display_diagnostic(e)
                        .expect("failed to display diagnostic");
                }
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
            Ok(Some(exec_path)) => std::mem::take(exec_path),
        };
        drop(res);
        path
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
