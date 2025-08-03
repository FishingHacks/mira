use std::sync::Arc;

use crossbeam::channel::{bounded, Sender};
use mira_errors::Diagnostics;
use parking_lot::RwLock;

use crate::{
    context::SharedContext,
    error::{IoReadError, ParsingError, ProgramFormingError, TokenizationError},
    module::{Module, ModuleContext},
    parser::ParserQueueEntry,
    progress_bar::{ProgressBar, ProgressItemRef},
    store::{Store, StoreKey},
    threadpool::ThreadPool,
    tokenizer::Tokenizer,
};
use mira_spans::{PackageId, SourceFile};

use super::print_progress_bar;

#[allow(clippy::too_many_arguments)]
fn parse_single<'arena>(
    file: Arc<SourceFile>,
    progress_bar: Arc<RwLock<ProgressBar>>,
    parsing_item: ProgressItemRef,
    finish_sender: Sender<()>,
    errors: Arc<RwLock<Diagnostics<'arena>>>,
    parsing_queue: Arc<RwLock<Vec<ParserQueueEntry<'arena>>>>,
    module_context: Arc<ModuleContext<'arena>>,
    key: StoreKey<Module<'arena>>,
    package: PackageId,
) {
    // ┌──────────────┐
    // │ Tokenization │
    // └──────────────┘
    let mut item = progress_bar.write().add_child_item(
        parsing_item,
        format!("Tokenizing {}", file.path.display()).into_boxed_str(),
    );
    print_progress_bar(&progress_bar);
    let mut tokenizer = Tokenizer::new(module_context.ctx, file.clone());

    if let Err(errs) = tokenizer.scan_tokens() {
        errors
            .write()
            .extend(errs.into_iter().map(TokenizationError::to_error));
    }

    {
        let mut writer = progress_bar.write();
        writer.remove_item(item);
        item = writer.add_child_item(
            parsing_item,
            format!("Parsing {}", file.path.display()).into_boxed_str(),
        );
    }
    print_progress_bar(&progress_bar);

    // ┌─────────┐
    // │ Parsing │
    // └─────────┘
    let mut current_parser =
        tokenizer.to_parser(parsing_queue.clone(), &module_context.modules, key);
    current_parser.file = file.clone();

    let (statements, parsing_errors) = current_parser.parse_all();
    errors
        .write()
        .extend(parsing_errors.into_iter().map(ParsingError::to_error));

    // ┌─────────────────┐
    // │ Program Forming │
    // └─────────────────┘
    {
        let mut writer = progress_bar.write();
        writer.remove_item(item);
        item = writer.add_child_item(
            parsing_item,
            format!("Processing {}", file.path.display()).into_boxed_str(),
        );
    }
    print_progress_bar(&progress_bar);

    let mut module = Module::new(
        current_parser.imports,
        current_parser.exports,
        package,
        current_parser.file.path.clone(),
        current_parser.file.package_root.clone(),
    );
    if let Err(errs) = module.push_all(statements, key, &module_context) {
        errors
            .write()
            .extend(errs.into_iter().map(ProgramFormingError::to_error));
    }
    module_context.modules.write().insert_reserved(key, module);
    progress_bar.write().remove_item(item);
    print_progress_bar(&progress_bar);

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
    progress_bar: Arc<RwLock<ProgressBar>>,
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

    thread_pool.enter(|handle| 'outer_loop: loop {
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
                package,
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
    });

    if errors.read().is_empty() {
        Ok(module_context)
    } else {
        Err(Arc::into_inner(errors)
            .expect("more than one reference to errors")
            .into_inner())
    }
}
