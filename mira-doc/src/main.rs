use std::{path::PathBuf, process::ExitCode, rc::Rc};

use html::err_fs;
use mira_common::index::Idx;
use mira_driver::{ContextData, DiagEmitter, EmitResult, LibraryTree, find_library};
use mira_errors::IoWriteError;
use mira_parser::module::ModuleId;
use mira_spans::Arena;
use mira_typeck::ResolvedValue;

mod default_ty_links;
mod html;
mod markdown;

fn main() -> ExitCode {
    match _main() {
        Ok(_) => ExitCode::SUCCESS,
        Err(_) => ExitCode::FAILURE,
    }
}

fn _main() -> EmitResult<()> {
    let mut args = std::env::args();
    let exec_path = args.next();
    let file = args.next();
    let output = args.next();

    let (file, output) = match (file, output) {
        (Some(a), Some(b)) => (a, b),
        _ => {
            println!(
                "Usage: {} [file] [output directory]",
                exec_path.as_deref().unwrap_or("mira-rustdoc")
            );
            println!(
                "Generate the documentation for the mira standard library and the root file into the output directory."
            );
            return Ok(());
        }
    };

    let file = if file == "--mira-std-only" {
        None
    } else {
        Some(PathBuf::from(file))
    };
    let output = PathBuf::from(output);

    let arena = Arena::new();
    let input = file;
    let (ctx, data) = ContextData::new(&arena, None, DiagEmitter::Stdout);
    let mut context = data.to_context(ctx.ty_ctx());

    let std = find_library("mirastd").expect("failed to find mira std library");
    let std_lib_file = std.join("lib.mr");

    let mut libtree = LibraryTree::new();
    let std_id = libtree
        .build_library(std.into(), std_lib_file.into(), "std")
        .build();
    let main_lib = input.map(|path| {
        let module_name = path
            .parent()
            .unwrap()
            .file_stem()
            .map(|v| v.to_string_lossy().into_owned())
            .unwrap_or_else(|| "root".to_string());
        libtree
            .build_library(path.parent().unwrap().into(), path.into(), module_name)
            .with_dependency("std", std_id)
            .build()
    });
    libtree.main_library(main_lib.unwrap_or(std_id));

    let (module_ctx, main_module) = context.parse_all_files(&libtree)?;
    let typechecking_item = context.add_typechecking_item();
    let typeck_ctx = context.resolve_types(&module_ctx, typechecking_item)?;
    context.remove_progress_item(typechecking_item);
    // drop(context);
    macro_rules! tri {
        ($e:expr) => {
            match $e {
                Ok(v) => v,
                Err(diag) => return Err(typeck_ctx.ctx.emit_diag(diag)),
            }
        };
    }
    let stdlib_module = ModuleId::from_usize(std_id.to_usize());
    let index_file = output.join("index.html");
    let mut context = tri!(html::HTMLGenerateContext::new(
        output,
        typeck_ctx.clone(),
        if main_lib.is_some() {
            vec![main_module, stdlib_module]
        } else {
            vec![stdlib_module]
        }
    ));
    while !context.queued.borrow().is_empty() {
        let k = *context.queued.borrow().keys().next().unwrap();
        let v = context.queued.borrow_mut().remove(&k).unwrap();
        context.generated.borrow_mut().insert(k, Rc::clone(&v));
        match k {
            ResolvedValue::Function(key, _) => {
                let s = context.generate_function(&v, key);
                tri!(err_fs::create_dir_all(v.0.parent().unwrap().to_path_buf()));
                tri!(
                    std::fs::write(&v.0, s)
                        .map_err(|e| IoWriteError(v.0.to_path_buf(), e).to_error())
                );
            }
            ResolvedValue::ExternalFunction(key) => {
                let s = context.generate_external_function(&v, key);
                tri!(err_fs::create_dir_all(v.0.parent().unwrap().to_path_buf()));
                tri!(
                    std::fs::write(&v.0, s)
                        .map_err(|e| IoWriteError(v.0.to_path_buf(), e).to_error())
                );
            }
            ResolvedValue::Static(key) => {
                let s = context.generate_static(&v, key);
                tri!(err_fs::create_dir_all(v.0.parent().unwrap().to_path_buf()));
                tri!(
                    std::fs::write(&v.0, s)
                        .map_err(|e| IoWriteError(v.0.to_path_buf(), e).to_error())
                );
            }
            ResolvedValue::Trait(key) => {
                let s = context.generate_trait(&v, key);
                tri!(err_fs::create_dir_all(v.0.parent().unwrap().to_path_buf()));
                tri!(
                    std::fs::write(&v.0, s)
                        .map_err(|e| IoWriteError(v.0.to_path_buf(), e).to_error())
                );
            }
            ResolvedValue::Struct(key, _) => {
                let s = context.generate_struct(&v, key);
                tri!(err_fs::create_dir_all(v.0.parent().unwrap().to_path_buf()));
                tri!(
                    std::fs::write(&v.0, s)
                        .map_err(|e| IoWriteError(v.0.to_path_buf(), e).to_error())
                );
            }
            ResolvedValue::Module(key) => {
                let s = context.generate_module(&v, key);
                tri!(err_fs::create_dir_all(v.0.parent().unwrap().to_path_buf()));
                tri!(
                    std::fs::write(&v.0, s)
                        .map_err(|e| IoWriteError(v.0.to_path_buf(), e).to_error())
                );
            }
        }
    }

    let search_idx = context.generate_search_index();
    tri!(err_fs::write_file(
        context.path.join("searchidx.js"),
        &search_idx
    ));

    // generate the module file of the main lib in the root directory under index.html

    let qualified_paths =
        Rc::clone(&context.generated.borrow()[&ResolvedValue::Module(main_module)]);
    let qualified_paths = (
        index_file,
        qualified_paths.1.clone(),
        qualified_paths.2.clone(),
    );
    let s = context.generate_module(&qualified_paths, main_module);
    tri!(
        std::fs::write(&qualified_paths.0, s)
            .map_err(|e| IoWriteError(qualified_paths.0, e).to_error())
    );

    Ok(())
}
