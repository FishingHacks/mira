use std::{collections::HashMap, path::Path, sync::Arc};

use crossbeam::channel::{bounded, Sender};
use parking_lot::RwLock;

use crate::{
    context::SharedContext,
    error::{MiraError, ParsingError, ProgramFormingError, TokenizationError},
    module::{Module, ModuleContext},
    module_resolution::ModuleResolver,
    parser::ParserQueueEntry,
    progress_bar::{ProgressBar, ProgressItemRef},
    store::StoreKey,
    threadpool::ThreadPool,
    tokenizer::{Literal, Location, Token, TokenType, Tokenizer},
};

use super::print_progress_bar;

enum ParsingErrors<'arena> {
    Tokenization(TokenizationError),
    Parsing(ParsingError<'arena>),
    ProgramForming(ProgramFormingError<'arena>),
    IO(std::io::Error),
}

#[allow(clippy::too_many_arguments)]
fn parse_single<'arena>(
    ctx: SharedContext<'arena>,
    file: Arc<Path>,
    root_directory: Arc<Path>,
    debug_file: Arc<Path>,
    source: &str,
    always_include: Option<String>,
    progress_bar: Arc<RwLock<ProgressBar>>,
    parsing_item: ProgressItemRef,
    finish_sender: Sender<()>,
    errors: Arc<RwLock<Vec<ParsingErrors<'arena>>>>,
    parsing_queue: Arc<RwLock<Vec<ParserQueueEntry<'arena>>>>,
    resolvers: Arc<[Box<dyn ModuleResolver>]>,
    path_exists: Arc<dyn Fn(&std::path::Path) -> bool>,
    path_is_dir: Arc<dyn Fn(&std::path::Path) -> bool>,
    module_context: Arc<ModuleContext<'arena>>,
    key: StoreKey<Module<'arena>>,
) {
    // +--------------+
    // | Tokenization |
    // +--------------+
    let mut item = progress_bar.write().add_child_item(
        parsing_item,
        format!("Tokenizing {}", file.display()).into_boxed_str(),
    );
    print_progress_bar(&progress_bar);
    let mut tokenizer = Tokenizer::new(ctx.clone(), source, debug_file.clone());

    // default includes ("prelude")
    if let Some(path) = always_include {
        let loc = Location::new(debug_file.clone(), 0, 0);
        tokenizer.push_token(Token {
            typ: TokenType::Use,
            literal: None,
            location: loc.clone(),
        });
        tokenizer.push_token(Token {
            typ: TokenType::StringLiteral,
            literal: Some(Literal::String(ctx.intern_str(&path))),
            location: loc.clone(),
        });
        tokenizer.push_token(Token {
            typ: TokenType::Semicolon,
            literal: None,
            location: loc.clone(),
        });
    }

    if let Err(errs) = tokenizer.scan_tokens() {
        errors
            .write()
            .extend(errs.into_iter().map(ParsingErrors::Tokenization));
    }

    {
        let mut writer = progress_bar.write();
        writer.remove_item(item);
        item = writer.add_child_item(
            parsing_item,
            format!("Parsing {}", file.display()).into_boxed_str(),
        );
    }
    print_progress_bar(&progress_bar);

    // +---------+
    // | Parsing |
    // +---------+
    let mut current_parser = tokenizer.to_parser(
        parsing_queue.clone(),
        &module_context.modules,
        root_directory,
        resolvers,
        path_exists,
        path_is_dir,
        key,
    );
    current_parser.file = file.clone();

    let (statements, parsing_errors) = current_parser.parse_all();
    errors
        .write()
        .extend(parsing_errors.into_iter().map(ParsingErrors::Parsing));

    // +-----------------+
    // | Program Forming |
    // +-----------------+
    {
        let mut writer = progress_bar.write();
        writer.remove_item(item);
        item = writer.add_child_item(
            parsing_item,
            format!("Parsing {}", file.display()).into_boxed_str(),
        );
    }
    print_progress_bar(&progress_bar);

    let mut module = Module::new(
        current_parser.imports,
        current_parser.file,
        current_parser.root_directory,
    );
    if let Err(errs) = module.push_all(statements, key, &module_context) {
        errors
            .write()
            .extend(errs.into_iter().map(ParsingErrors::ProgramForming));
    }
    module_context.modules.write()[key] = module;
    progress_bar.write().remove_item(item);
    print_progress_bar(&progress_bar);

    _ = finish_sender.send(())
}

/// Parses a string of text into a module
///
/// `file` - The file the source came from. Used to evaluate relative imports
/// `root_directory` - The path the import `@root/` points to
/// `debug_file` - The file that will appear in locations and debug info
/// `source` - The source that will be parsed
#[allow(clippy::too_many_arguments)]
pub fn parse_all<'arena>(
    ctx: SharedContext<'arena>,
    file: Arc<Path>,
    root_directory: Arc<Path>,
    debug_file: Arc<Path>,
    source: &str,
    always_include: Option<String>,
    resolvers: Arc<[Box<dyn ModuleResolver>]>,
    path_exists: Arc<dyn Fn(&std::path::Path) -> bool + Send + Sync>,
    path_is_dir: Arc<dyn Fn(&std::path::Path) -> bool + Send + Sync>,
    progress_bar: Arc<RwLock<ProgressBar>>,
    parsing_item: ProgressItemRef,
) -> Result<Arc<ModuleContext<'arena>>, Vec<MiraError<'arena>>> {
    let errors = Arc::new(RwLock::new(Vec::new()));
    let module_context = Arc::new(ModuleContext::default());
    let parsing_queue = Arc::new(RwLock::new(Vec::new()));
    let (finish_sender, finish_receiver) = bounded::<()>(1);
    let root_key = module_context.modules.write().insert(Module::new(
        HashMap::new(),
        file.clone(),
        root_directory.clone(),
    ));

    parse_single(
        ctx.clone(),
        file,
        root_directory,
        debug_file,
        source,
        always_include,
        progress_bar.clone(),
        parsing_item,
        finish_sender.clone(),
        errors.clone(),
        parsing_queue.clone(),
        resolvers.clone(),
        path_exists.clone(),
        path_is_dir.clone(),
        module_context.clone(),
        root_key,
    );
    finish_receiver.recv().expect("a sender still exists");

    let mut modules_left = 0;

    let mut thread_pool = ThreadPool::new_auto();

    thread_pool.enter(|handle| 'outer_loop: loop {
        if parsing_queue.read().is_empty() {
            break;
        }
        let entry = parsing_queue.write().remove(0);
        let key = entry.reserved_key;
        let file = entry.file;
        let root = entry.root_dir;
        module_context
            .modules
            .write()
            .insert_reserved(key, Module::new(HashMap::new(), file.clone(), root.clone()));

        let source = match std::fs::read_to_string(&file) {
            Ok(v) => v,
            Err(e) => {
                errors.write().push(ParsingErrors::IO(e));
                break 'outer_loop;
            }
        };

        let progress_bar = progress_bar.clone();
        let resolvers = resolvers.clone();
        let path_exists = path_exists.clone();
        let path_is_dir = path_is_dir.clone();
        let errors = errors.clone();
        let finish_sender = finish_sender.clone();
        let _parsing_queue = parsing_queue.clone();
        let _module_context = module_context.clone();
        modules_left += 1;
        let thread_ctx = ctx.clone();
        // TODO: Make sure this *does not* compile
        handle.spawn(move || {
            parse_single(
                thread_ctx,
                file.clone(),
                root,
                file,
                &source,
                None,
                progress_bar,
                parsing_item,
                finish_sender,
                errors,
                _parsing_queue,
                resolvers,
                path_exists,
                path_is_dir,
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
    });

    if errors.read().is_empty() {
        Ok(module_context)
    } else {
        Err(Arc::into_inner(errors)
            .expect("more than one reference to errors")
            .into_inner()
            .into_iter()
            .map(|v| match v {
                ParsingErrors::Tokenization(inner) => MiraError::Tokenization { inner },
                ParsingErrors::Parsing(inner) => MiraError::Parsing { inner },
                ParsingErrors::ProgramForming(inner) => MiraError::ProgramForming { inner },
                ParsingErrors::IO(inner) => MiraError::IO { inner },
            })
            .collect())
    }
}
