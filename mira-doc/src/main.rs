use std::{path::PathBuf, rc::Rc};

use html::err_fs;
use mira_common::store::StoreKey;
use mira_driver::{Context, LibraryTree, ProgressBarStyle, find_library};
use mira_errors::{DiagnosticFormatter, Diagnostics, IoWriteError, Output};
use mira_parser::module::ModuleScopeValue;
use mira_spans::Arena;
use mira_typeck::{GlobalContext, TypeCtx};

mod html;

fn main() {
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
            return;
        }
    };

    let file = if file == "--mira-std-only" {
        None
    } else {
        Some(PathBuf::from(file))
    };
    let output = PathBuf::from(output);

    let arena = Arena::new();
    let ctx = GlobalContext::new(&arena);
    if let Some((mut fmt, errs)) = _main(ctx.ty_ctx(), file, output) {
        for err in errs {
            fmt.display_diagnostic(err)
                .expect("failed to display diagnostics");
        }
    }
}

fn _main(
    ctx: TypeCtx,
    input: Option<PathBuf>,
    output: PathBuf,
) -> Option<(DiagnosticFormatter, Diagnostics)> {
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

    let mut context = Context::new(ctx, ProgressBarStyle::Normal);
    macro_rules! tri {
        ($e:expr) => {
            match $e {
                Ok(v) => v,
                Err(e) => return Some((context.diagnostic_formatter(Output::Stderr), e)),
            }
        };
    }
    let (module_ctx, main_module) = tri!(context.parse_all_files(&libtree));
    let typechecking_item = context.add_typechecking_item();
    let typeck_ctx = tri!(context.resolve_types(&module_ctx, typechecking_item));
    context.remove_progress_item(typechecking_item);
    let formatter = context.diagnostic_formatter(Output::Stderr);
    drop(context);
    macro_rules! tri {
        ($e:expr) => {
            match $e {
                Ok(v) => v,
                Err(e) => return Some((formatter, e.into())),
            }
        };
    }
    let stdlib_module = StoreKey::from_usize(std_id.to_usize());
    let mut context = tri!(html::HTMLGenerateContext::new(
        output,
        typeck_ctx.clone(),
        if main_lib.is_some() {
            vec![main_module.cast(), stdlib_module]
        } else {
            vec![stdlib_module]
        }
    ));
    while !context.queued.borrow().is_empty() {
        let k = *context.queued.borrow().keys().next().unwrap();
        let v = context.queued.borrow_mut().remove(&k).unwrap();
        context.generated.borrow_mut().insert(k, Rc::clone(&v));
        match k {
            ModuleScopeValue::Function(key) => {
                let s = context.generate_function(&v, key.cast());
                tri!(err_fs::create_dir_all(v.0.parent().unwrap().to_path_buf()));
                tri!(
                    std::fs::write(&v.0, s)
                        .map_err(|e| IoWriteError(v.0.to_path_buf(), e).to_error())
                );
            }
            ModuleScopeValue::ExternalFunction(key) => {
                let s = context.generate_external_function(&v, key.cast());
                tri!(err_fs::create_dir_all(v.0.parent().unwrap().to_path_buf()));
                tri!(
                    std::fs::write(&v.0, s)
                        .map_err(|e| IoWriteError(v.0.to_path_buf(), e).to_error())
                );
            }
            ModuleScopeValue::Static(key) => {
                let s = context.generate_static(&v, key.cast());
                tri!(err_fs::create_dir_all(v.0.parent().unwrap().to_path_buf()));
                tri!(
                    std::fs::write(&v.0, s)
                        .map_err(|e| IoWriteError(v.0.to_path_buf(), e).to_error())
                );
            }
            ModuleScopeValue::Trait(key) => {
                let s = context.generate_trait(&v, key.cast());
                tri!(err_fs::create_dir_all(v.0.parent().unwrap().to_path_buf()));
                tri!(
                    std::fs::write(&v.0, s)
                        .map_err(|e| IoWriteError(v.0.to_path_buf(), e).to_error())
                );
            }
            ModuleScopeValue::Struct(key) => {
                let s = context.generate_struct(&v, key.cast());
                tri!(err_fs::create_dir_all(v.0.parent().unwrap().to_path_buf()));
                tri!(
                    std::fs::write(&v.0, s)
                        .map_err(|e| IoWriteError(v.0.to_path_buf(), e).to_error())
                );
            }
            ModuleScopeValue::Module(key) => {
                let s = context.generate_module(&v, key.cast());
                tri!(err_fs::create_dir_all(v.0.parent().unwrap().to_path_buf()));
                tri!(
                    std::fs::write(&v.0, s)
                        .map_err(|e| IoWriteError(v.0.to_path_buf(), e).to_error())
                );
            }
        }
    }

    None
}
