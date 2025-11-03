use std::{
    collections::HashMap,
    sync::Arc,
    sync::mpsc::{Sender, channel},
};

use mira_common::{
    index::{IndexMap, IndexStore},
    threadpool::ThreadPool,
};
use mira_context::{DocComment, SharedCtx};
use mira_errors::Diagnostics;
use mira_parser::module::ModuleId;
use mira_progress_bar::{ProgressItemRef, print_thread::ProgressBarThread};
use parking_lot::RwLock;

use mira_errors::IoReadError;
use mira_lexer::{Lexer, LexingError, Literal, TokenType};
use mira_parser::{
    Parser, ParsingError, ProgramFormingError,
    module::{Module, ModuleContext, ParserQueueEntry},
};
use mira_spans::SourceFile;

use crate::{LibraryId, LibraryInput, LibraryTree};

#[allow(clippy::too_many_arguments)]
fn parse_single<'arena>(
    file: Arc<SourceFile>,
    progress_bar: ProgressBarThread,
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
    {
        item = progress_bar.add_child(
            parsing_item,
            format!("Parsing {}", file.path.display()).into_boxed_str(),
        );
    }

    let file = lexer.file.clone();
    let mut tokens = lexer.into_tokens();
    let mut comment = None;

    if let Some(tok) = tokens.first()
        && tok.ty == TokenType::ModuleDocComment
    {
        let Some(Literal::DocComment(v)) = tok.literal else {
            unreachable!()
        };
        comment = Some(v);
    }
    if comment.is_some() {
        tokens.remove(0);
    }
    if let Some(c) = cur_entry.comment {
        match comment {
            Some(v) => {
                module_context.ctx.merge_doc_comments(v, c);
                module_context.ctx.clear_doc_comment(c);
            }
            None => comment = Some(c),
        }
    }

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
        cur_entry.module_id,
    );

    let (statements, parsing_errors) = current_parser.parse_all();
    errors
        .write()
        .extend(parsing_errors.into_iter().map(ParsingError::to_error));

    // ┌─────────────────┐
    // │ Program Forming │
    // └─────────────────┘
    progress_bar.remove(item);
    {
        item = progress_bar.add_child(
            parsing_item,
            format!("Processing {}", file.path.display()).into_boxed_str(),
        );
    }

    let mut module = Module::new(
        current_parser.file,
        cur_entry.package_root,
        cur_entry.parent,
        cur_entry.name,
        comment.unwrap_or(DocComment::EMPTY),
    );
    // `current_parser` is partially moved and can't be dropped with `drop(..)`
    #[allow(let_underscore_drop)]
    let _ = current_parser;
    if let Err(errs) = module.push_all(
        statements,
        cur_entry.module_id,
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
        .insert_reserved(module, cur_entry.module_id);
    progress_bar.remove(item);

    _ = finish_sender.send(());
}

/// Parses a string of text into a module
///
/// `file` - The file the source came from. Used to evaluate relative imports
/// `root_directory` - The path the import `@root/` points to
/// `source` - The source that will be parsed
///
/// Returns the parsed module context as well as the main module.
pub(crate) fn parse_all<'arena>(
    ctx: SharedCtx<'arena>,
    progress_bar: ProgressBarThread,
    parsing_item: ProgressItemRef,
    libtree: &LibraryTree,
) -> Result<(Arc<ModuleContext<'arena>>, ModuleId), Diagnostics<'arena>> {
    let errors = Arc::new(RwLock::new(Diagnostics::new()));
    let mut module_store = IndexStore::new();
    let mut parsing_queue = Vec::with_capacity(libtree.libraries.len());
    let mut dependencies = IndexMap::new();
    let mut lib_id_to_module = HashMap::new();
    for (lib_id, lib) in libtree
        .libraries
        .iter()
        .enumerate()
        .map(|(k, v)| (LibraryId(k), v))
    {
        let modid: ModuleId = module_store.reserve_id();
        assert_eq!(lib_id.to_usize(), modid.to_usize());
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
            name: ctx.intern_str(&lib.name),
            file_root: lib.root.clone(),
            loaded_file,
            package_root: modid,
            parent: None,
            module_id: modid,
            comment: None,
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
    let (finish_sender, finish_receiver) = channel();

    let mut modules_left = 0;

    ThreadPool::new_auto().enter(|handle| {
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
