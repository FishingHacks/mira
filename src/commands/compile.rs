use std::{
    error::Error,
    path::{Path, PathBuf},
    process::Command,
    sync::Arc,
    time::{SystemTime, UNIX_EPOCH},
};

use mira_argparse::{CompileArgs, OptimizationMode, PathOrStdout};
use mira_driver::{
    find_library, ContextData, DiagEmitter, EmitMethod, ErrorEmitted, LibraryTree, LinkOpts,
    ProgressBarStyle,
};
use mira_llvm_backend::{CodegenConfig, CodegenContextBuilder};
use mira_spans::Arena;
use mira_target::Target;

pub fn opt_mode_to_codegen_cfg(
    mode: OptimizationMode,
    target: Target,
    cpu_features: Option<&'_ str>,
) -> CodegenConfig<'_> {
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

pub(super) fn to_emit(value: Option<PathOrStdout>) -> Option<EmitMethod> {
    match value {
        Some(PathOrStdout::Stdout) => Some(EmitMethod::Stdout),
        Some(PathOrStdout::Path(p)) => Some(EmitMethod::file(p)),
        None => None,
    }
}

pub fn compile_main(mut args: CompileArgs) -> Result<(), Box<dyn Error>> {
    let Some(libmirastd) = find_library("mirastd") else {
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
        .build_library(libmirastd.into(), stdlib_main_file, "std")
        .build();
    let module_name = root_directory
        .file_stem()
        .map(|v| v.to_string_lossy().into_owned())
        .unwrap_or_else(|| "root".to_string());
    let mainlib = libtree
        .build_library(root_directory.into(), file, module_name)
        .with_dependency("std", libstd)
        .build();
    libtree.main_library(mainlib);
    let mut tmp_obj_path = None;
    let obj_path = args.emit_obj.or_else(|| {
        args.out_file.is_some().then(|| {
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

    let res = compile(
        &libtree,
        opt_mode_to_codegen_cfg(args.opt, args.target, args.cpu_features.as_deref()),
        to_emit(args.emit_llvm_ir),
        to_emit(args.emit_llvm_bc),
        to_emit(args.emit_asm),
        to_emit(args.emit_ir),
        obj_path,
        args.out_file.as_deref(),
        &args.linker_args,
        args.verbose,
    );

    if let Some(v) = tmp_obj_path {
        _ = std::fs::remove_file(v);
    }
    if res.is_err() {
        return Ok(());
    }

    let Some(exec_path) = args.out_file else {
        return Ok(());
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

#[allow(clippy::too_many_arguments)]
pub(crate) fn compile(
    libtree: &LibraryTree,
    codegen_opts: CodegenConfig,
    llvm_ir_writer: Option<EmitMethod>,
    llvm_bc_writer: Option<EmitMethod>,
    asm_writer: Option<EmitMethod>,
    ir_writer: Option<EmitMethod>,
    obj_file: Option<PathBuf>,
    exec_file: Option<&Path>,
    additional_linker_args: &[String],
    verbose: bool,
) -> Result<(), ErrorEmitted> {
    let arena = Arena::new();
    let (ctx, data) = ContextData::new(&arena, Some(ProgressBarStyle::Normal), DiagEmitter::Stdout);
    let s_ctx = ctx.ty_ctx();
    let mut ctx = data.to_context(s_ctx);

    let (module_ctx, main_module) = ctx.parse_all_files(libtree)?;
    let typechecking_item = ctx.add_typechecking_item();
    let tc_ctx = ctx.resolve_types(&module_ctx, typechecking_item)?;
    ctx.typecheck_items(&module_ctx, &tc_ctx, typechecking_item)?;
    drop(module_ctx);
    ctx.validate_main_fn(&tc_ctx, main_module)?;
    ctx.run_dead_code_elimination(&tc_ctx);
    if let Some(ir_writer) = ir_writer {
        ctx.emit_ir(&tc_ctx, ir_writer)?;
    }
    ctx.remove_progress_item(typechecking_item);

    let codegen_ctx_builder = CodegenContextBuilder::new();
    let codegen_item = ctx.add_codegen_item();
    let (codegen_ctx, err) = ctx.codegen(
        &tc_ctx,
        main_module,
        codegen_opts,
        &codegen_ctx_builder,
        codegen_item,
    );
    drop(tc_ctx);
    err?;
    ctx.optimize(&codegen_ctx, codegen_item)?;
    if let Some(llvm_ir_writer) = llvm_ir_writer {
        ctx.emit_llvm_ir(&codegen_ctx, llvm_ir_writer)?;
    }
    if let Some(llvm_bc_writer) = llvm_bc_writer {
        ctx.emit_llvm_bc(&codegen_ctx, llvm_bc_writer)?;
    }
    if let Some(asm_writer) = asm_writer {
        ctx.emit_asm(&codegen_ctx, asm_writer)?;
    }
    ctx.remove_progress_item(codegen_item);

    let Some(obj_file) = obj_file else {
        return Ok(());
    };

    ctx.emit_object(&codegen_ctx, &obj_file)?;
    drop(codegen_ctx);

    let Some(exec_file) = exec_file else {
        return Ok(());
    };
    ctx.link(LinkOpts {
        additional_linker_searchdir: &[],
        additional_args: additional_linker_args,
        debug_info: true,
        verbose,
        link_script: None,
        shared_obj: false,
        target: codegen_opts.target,
        output_path: exec_file,
        obj_path: &[obj_file],
        input: vec![],
    })?;
    Ok(())
}
