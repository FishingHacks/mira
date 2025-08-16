use std::{collections::HashMap, fmt::Write, path::Path, sync::Arc};

use crossbeam_channel::{Sender, bounded};
use mira_errors::Diagnostics;
use mira_progress_bar::{ProgressItemRef, print_thread::ProgressBarThread};
use parking_lot::RwLock;

use mira::{
    context::SharedContext,
    error::{ParsingError, ProgramFormingError},
    module::{Module, ModuleContext},
    parser::{Parser, ParserQueueEntry},
    store::{Store, StoreKey},
    threadpool::ThreadPool,
};
use mira_errors::IoReadError;
use mira_lexer::{Lexer, LexingError};
use mira_spans::SourceFile;

use crate::EmitMethod;

#[allow(clippy::too_many_arguments)]
fn parse_single<'arena>(
    file: Arc<SourceFile>,
    mut progress_bar: ProgressBarThread,
    parsing_item: ProgressItemRef,
    finish_sender: Sender<()>,
    errors: Arc<RwLock<Diagnostics<'arena>>>,
    parsing_queue: Arc<RwLock<Vec<ParserQueueEntry<'arena>>>>,
    module_context: Arc<ModuleContext<'arena>>,
    key: StoreKey<Module<'arena>>,
) {
    // ┌──────────────┐
    // │ Tokenization │
    // └──────────────┘
    let mut item = progress_bar.add_child(
        parsing_item,
        format!("Tokenizing {}", file.path.display()).into_boxed_str(),
    );
    let lexing_ctx = module_context.ctx.into();
    let mut lexer = Lexer::new(lexing_ctx, file.clone());

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
        match mira::parser::expand_tokens(
            module_context.ctx,
            file.clone(),
            tokens,
            &mut diagnostics,
        ) {
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
        parsing_queue.clone(),
        &module_context.modules,
        file.clone(),
        key,
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

    let mut module = Module::new(current_parser.imports, current_parser.file);
    _ = current_parser;
    if let Err(errs) = module.push_all(statements, key, &module_context) {
        errors
            .write()
            .extend(errs.into_iter().map(ProgramFormingError::to_error));
    }
    module_context.modules.write().insert_reserved(key, module);
    progress_bar.remove(item);

    _ = finish_sender.send(())
}

/// Parses a string of text into a module
///
/// `file` - The file the source came from. Used to evaluate relative imports
/// `root_directory` - The path the import `@root/` points to
/// `source` - The source that will be parsed
#[allow(clippy::too_many_arguments)]
pub fn parse_all<'arena>(
    ctx: SharedContext<'arena>,
    progress_bar: ProgressBarThread,
    parsing_item: ProgressItemRef,
) -> Result<Arc<ModuleContext<'arena>>, Diagnostics<'arena>> {
    let errors = Arc::new(RwLock::new(Diagnostics::new()));
    let mut module_store = Store::new();
    let files = ctx.source_map().files();
    let parsing_queue = ctx
        .source_map()
        .packages()
        .iter()
        .map(|p| {
            let file = &files[p.root_file.to_inner() as usize];
            assert_eq!(file.package, p.id);
            ParserQueueEntry {
                file: file.path.clone(),
                package: p.id,
                reserved_key: module_store.reserve_key(),
                loaded_file: Some(p.root_file),
            }
        })
        .collect::<Vec<_>>();
    drop(files);
    let packages = parsing_queue
        .iter()
        .map(|e| (e.package, e.reserved_key))
        .collect();
    let module_context = Arc::new(ModuleContext::new(packages, module_store, ctx));
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
            let key = entry.reserved_key;
            let file = entry.file;
            let package = entry.package;

            let source = match entry.loaded_file {
                Some(fid) => ctx.source_map().get_file(fid).unwrap(),
                None => match ctx.source_map().load_file(file.clone(), package) {
                    Ok(v) => v,
                    Err(e) => {
                        errors.write().add_err(IoReadError(file.to_path_buf(), e));
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
                    _module_context,
                    key,
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
        Ok(module_context)
    } else {
        Err(Arc::into_inner(errors)
            .expect("more than one reference to errors")
            .into_inner())
    }
}

pub fn expand_macros<'a>(
    ctx: SharedContext<'a>,
    file: Arc<Path>,
    mut output: EmitMethod,
) -> Result<(), Diagnostics<'a>> {
    let mut diags = Diagnostics::new();
    let f = match ctx.source_map().add_package_load(
        file.parent().unwrap().into(),
        file.clone(),
        HashMap::new(),
    ) {
        Ok(v) => v.1,
        Err(e) => {
            diags.add_err(IoReadError(file.to_path_buf(), e));
            return Err(diags);
        }
    };
    let mut lexer = Lexer::new(ctx.into(), f);
    _ = lexer
        .scan_tokens()
        .map_err(|e| diags.extend(e.into_iter().map(LexingError::to_error)));

    let file = lexer.file.clone();
    let tokens = lexer.into_tokens();
    let tokens = match mira::parser::expand_tokens(ctx, file.clone(), tokens, &mut diags) {
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
