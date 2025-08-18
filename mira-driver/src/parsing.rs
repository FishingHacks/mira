use std::{collections::HashMap, fmt::Write, path::Path, sync::Arc};

use crossbeam_channel::{Sender, bounded};
use mira_errors::Diagnostics;
use mira_progress_bar::{ProgressItemRef, print_thread::ProgressBarThread};
use parking_lot::RwLock;

use mira_common::store::{AssociatedStore, Store, StoreKey};
use mira_common::threadpool::ThreadPool;
use mira_errors::IoReadError;
use mira_lexer::{Lexer, LexingError};
use mira_parser::{
    Parser, ParsingError, ProgramFormingError,
    module::{Module, ModuleContext, ParserQueueEntry},
};
use mira_spans::{SharedCtx, SourceFile};

use crate::{EmitMethod, LibraryId, LibraryInput, LibraryTree};

#[allow(clippy::too_many_arguments)]
fn parse_single<'arena>(
    file: Arc<SourceFile>,
    mut progress_bar: ProgressBarThread,
    parsing_item: ProgressItemRef,
    finish_sender: Sender<()>,
    errors: Arc<RwLock<Diagnostics<'arena>>>,
    parsing_queue: Arc<RwLock<Vec<ParserQueueEntry<'arena>>>>,
    cur_entry: ParserQueueEntry<'arena>,
    module_context: Arc<ModuleContext<'arena>>,
) {
    // ┌──────────────┐
    // │ Tokenization │
    // └──────────────┘
    let mut item = progress_bar.add_child(
        parsing_item,
        format!("Tokenizing {}", file.path.display()).into_boxed_str(),
    );
    let mut lexer = Lexer::new(module_context.ctx, file.clone());

    if let Err(errs) = lexer.scan_tokens() {
        errors
            .write()
            .extend(errs.into_iter().map(LexingError::to_error));
    }

    progress_bar.remove(item);
    item = progress_bar.add_child(
        parsing_item,
        format!("Parsing {}", file.path.display()).into_boxed_str(),
    );

    let file = lexer.file.clone();
    let tokens = lexer.into_tokens();
    let tokens = {
        let mut diagnostics = Diagnostics::new();
        match mira_parser::expand_tokens(module_context.ctx, file.clone(), tokens, &mut diagnostics)
        {
            Some(v) => v,
            None => {
                errors.write().extend(diagnostics);
                vec![]
            }
        }
    };
    // ┌─────────┐
    // │ Parsing │
    // └─────────┘
    let mut current_parser = Parser::from_tokens(
        module_context.ctx,
        &tokens,
        file.clone(),
        cur_entry.module_key,
    );

    let (statements, parsing_errors) = current_parser.parse_all();
    errors
        .write()
        .extend(parsing_errors.into_iter().map(ParsingError::to_error));

    // ┌─────────────────┐
    // │ Program Forming │
    // └─────────────────┘
    progress_bar.remove(item);
    item = progress_bar.add_child(
        parsing_item,
        format!("Processing {}", file.path.display()).into_boxed_str(),
    );

    let mut module = Module::new(
        current_parser.file,
        cur_entry.package_root,
        cur_entry.parent,
    );
    _ = current_parser;
    if let Err(errs) = module.push_all(
        statements,
        cur_entry.module_key,
        &module_context,
        &parsing_queue,
        &module_context.modules,
    ) {
        errors
            .write()
            .extend(errs.into_iter().map(ProgramFormingError::to_error));
    }
    module_context
        .modules
        .write()
        .insert_reserved(cur_entry.module_key, module);
    progress_bar.remove(item);

    _ = finish_sender.send(())
}

/// Parses a string of text into a module
///
/// `file` - The file the source came from. Used to evaluate relative imports
/// `root_directory` - The path the import `@root/` points to
/// `source` - The source that will be parsed
///
/// Returns the parsed module context as well as the main module.
#[allow(clippy::too_many_arguments)]
pub fn parse_all<'arena>(
    ctx: SharedCtx<'arena>,
    progress_bar: ProgressBarThread,
    parsing_item: ProgressItemRef,
    libtree: &LibraryTree,
) -> Result<(Arc<ModuleContext<'arena>>, StoreKey<Module<'arena>>), Diagnostics<'arena>> {
    let errors = Arc::new(RwLock::new(Diagnostics::new()));
    let mut module_store = Store::new();
    let mut parsing_queue = Vec::with_capacity(libtree.libraries.len());
    let mut dependencies = AssociatedStore::new();
    let mut lib_id_to_module = HashMap::new();
    for (lib_id, lib) in libtree
        .libraries
        .iter()
        .enumerate()
        .map(|(k, v)| (LibraryId(k), v))
    {
        let modid = module_store.reserve_key();
        lib_id_to_module.insert(lib_id, modid);
        dependencies.insert(modid, HashMap::with_capacity(lib.dependencies.len()));
        let deps = &mut dependencies[modid];
        for (k, v) in lib.dependencies.iter() {
            deps.insert(k.clone(), lib_id_to_module[v]);
        }

        let loaded_file = match &lib.input {
            LibraryInput::Path => None,
            LibraryInput::String(s) => Some(
                ctx.source_map
                    .new_file(lib.root_file_path.clone(), lib.root.clone(), s.clone())
                    .id,
            ),
        };
        parsing_queue.push(ParserQueueEntry {
            file: lib.root_file_path.clone(),
            file_root: lib.root.clone(),
            loaded_file,
            package_root: modid,
            parent: modid,
            module_key: modid,
        });
    }
    let main_mod = libtree
        .main
        .expect("error: configured no package as the `main` package on the library tree.");
    let main_mod = lib_id_to_module
        .get(&main_mod)
        .expect("the `main` package appears to not be in the library tree");
    let module_context = Arc::new(ModuleContext::new(module_store, ctx, dependencies));

    let parsing_queue = Arc::new(RwLock::new(parsing_queue));
    let (finish_sender, finish_receiver) = bounded::<()>(1);

    let mut modules_left = 0;

    let mut thread_pool = ThreadPool::new_auto();

    thread_pool.enter(|handle| {
        'outer_loop: loop {
            if parsing_queue.read().is_empty() {
                break;
            }
            let entry = parsing_queue.write().remove(0);

            let source = match entry.loaded_file {
                Some(fid) => ctx.source_map.get_file(fid).unwrap(),
                None => match ctx
                    .source_map
                    .load_file(entry.file.clone(), entry.file_root.clone())
                {
                    Ok(v) => v,
                    Err(e) => {
                        errors
                            .write()
                            .add_err(IoReadError(entry.file.to_path_buf(), e));
                        break 'outer_loop;
                    }
                },
            };

            let progress_bar = progress_bar.clone();
            let errors = errors.clone();
            let finish_sender = finish_sender.clone();
            let _parsing_queue = parsing_queue.clone();
            let _module_context = module_context.clone();
            modules_left += 1;
            handle.spawn(move || {
                parse_single(
                    source,
                    progress_bar,
                    parsing_item,
                    finish_sender,
                    errors,
                    _parsing_queue,
                    entry,
                    _module_context,
                );
            });

            if !parsing_queue.read().is_empty() {
                continue 'outer_loop;
            }

            loop {
                _ = finish_receiver.recv();
                modules_left -= 1;
                if !parsing_queue.read().is_empty() {
                    continue 'outer_loop;
                }
                if modules_left == 0 {
                    break 'outer_loop;
                }
            }
        }
    });

    if errors.read().is_empty() {
        Ok((module_context, *main_mod))
    } else {
        Err(Arc::into_inner(errors)
            .expect("more than one reference to errors")
            .into_inner())
    }
}

pub fn expand_macros<'a>(
    ctx: SharedCtx<'a>,
    file: Arc<Path>,
    mut output: EmitMethod,
) -> Result<(), Diagnostics<'a>> {
    let mut diags = Diagnostics::new();
    let f = match ctx
        .source_map
        .load_file(file.clone(), file.parent().unwrap().into())
    {
        Ok(v) => v,
        Err(e) => {
            diags.add_err(IoReadError(file.to_path_buf(), e));
            return Err(diags);
        }
    };
    let mut lexer = Lexer::new(ctx, f);
    _ = lexer
        .scan_tokens()
        .map_err(|e| diags.extend(e.into_iter().map(LexingError::to_error)));

    let file = lexer.file.clone();
    let tokens = lexer.into_tokens();
    let tokens = match mira_parser::expand_tokens(ctx, file.clone(), tokens, &mut diags) {
        Some(v) => v,
        None => return Err(diags),
    };
    let mut s = String::with_capacity(10);
    for token in tokens {
        s.write_fmt(format_args!("{token} ")).unwrap();
        if !output.emit_diags(s.as_bytes(), &mut diags) {
            return Err(diags);
        }
        s.clear();
    }

    Ok(())
}
