use std::{fmt::Debug, ops::Index, sync::Arc};

use mira_errors::{Diagnostic, StyledPrinter, Styles, default_printer, default_styles};
use mira_spans::{
    Arena, SourceMap, Span, SpanData, Symbol,
    interner::{SpanInterner, SymbolInterner},
};
use parking_lot::Mutex;

mod diagnostics_emitter;
pub use diagnostics_emitter::*;

#[derive(Clone, Copy)]
pub struct ErrorTracker(usize);

pub struct GlobalCtx<'arena> {
    string_interner: Mutex<SymbolInterner<'arena>>,
    doc_comment_store: Mutex<DocCommentStore>,
    pub span_interner: SpanInterner<'arena>,
    pub source_map: Arc<SourceMap>,
    diag_ctx: Mutex<DiagCtx>,
}

impl<'arena> GlobalCtx<'arena> {
    pub fn new(
        arena: &'arena Arena,
        emitter: DiagEmitter,
        printer: Box<dyn StyledPrinter>,
        styles: Styles,
    ) -> Self {
        let source_map = Arc::new(SourceMap::new());
        Self {
            string_interner: SymbolInterner::new(arena).into(),
            doc_comment_store: DocCommentStore::new().into(),
            span_interner: SpanInterner::new(arena),
            diag_ctx: DiagCtx::new(emitter, source_map.clone(), printer, styles).into(),
            source_map,
        }
    }

    pub fn new_test_noemit(arena: &'arena Arena) -> Self {
        Self::new(
            arena,
            DiagEmitter::NoFail,
            default_printer(),
            default_styles(),
        )
    }

    pub fn share(&'arena self) -> SharedCtx<'arena> {
        SharedCtx::new(
            &self.string_interner,
            &self.doc_comment_store,
            &self.span_interner,
            &self.source_map,
            &self.diag_ctx,
        )
    }
}

#[derive(Clone, Copy)]
pub struct SharedCtx<'arena> {
    string_interner: &'arena Mutex<SymbolInterner<'arena>>,
    doc_comment_store: &'arena Mutex<DocCommentStore>,
    dctx: &'arena Mutex<DiagCtx>,
    pub span_interner: &'arena SpanInterner<'arena>,
    pub source_map: &'arena SourceMap,
}

impl<'arena> SharedCtx<'arena> {
    pub fn new(
        string_interner: &'arena Mutex<SymbolInterner<'arena>>,
        doc_comment_store: &'arena Mutex<DocCommentStore>,
        span_interner: &'arena SpanInterner<'arena>,
        source_map: &'arena SourceMap,
        dctx: &'arena Mutex<DiagCtx>,
    ) -> Self {
        Self {
            string_interner,
            doc_comment_store,
            span_interner,
            source_map,
            dctx,
        }
    }

    pub fn intern_str(self, s: &str) -> Symbol<'arena> {
        self.string_interner.lock().intern(s)
    }

    pub fn intern_span(self, data: SpanData) -> Span<'arena> {
        Span::new(data, self.span_interner)
    }

    pub fn add_doc_comment(&self, comment: impl Into<Box<str>>) -> DocComment {
        self.doc_comment_store
            .lock()
            .add_doc_comment(comment.into())
    }

    pub fn with_doc_comment<T>(&self, comment: DocComment, f: impl FnOnce(&str) -> T) -> T {
        f(&self.doc_comment_store.lock()[comment])
    }

    /// merges 2 doc comments, appending the second one to the first one.
    pub fn merge_doc_comments(&self, a: DocComment, b: DocComment) {
        self.doc_comment_store.lock().merge_doc_comments(a, b);
    }

    /// removes a doc comment.
    pub fn clear_doc_comment(&self, v: DocComment) {
        self.doc_comment_store.lock().clear_doc_comment(v);
    }

    pub fn emit_diags(&self, diags: impl IntoIterator<Item = Diagnostic<'arena>>) {
        let mut dctx = self.dctx.lock();
        for diag in diags {
            dctx.emit_diag(diag);
        }
    }

    pub fn emit_diag(&self, diag: Diagnostic<'arena>) {
        self.dctx.lock().emit_diag(diag);
    }

    pub fn err_count(&self) -> usize {
        self.dctx.lock().err_count()
    }

    pub fn track_errors(&self) -> ErrorTracker {
        self.dctx.lock().track_errors()
    }

    pub fn errors_happened(&self, tracker: ErrorTracker) -> bool {
        self.dctx.lock().errors_happened(tracker)
    }
}

impl<'ctx> mira_errors::DiagEmitter<'ctx> for SharedCtx<'ctx> {
    fn emit_diagnostic(&self, diag: Diagnostic<'ctx>) {
        SharedCtx::emit_diag(self, diag);
    }
}

/// storage for doc comments, so that they don't 'trash' the arena. No deduplication is happening,
/// because most doc comments will more likely than not be unique from eachother.
pub struct DocCommentStore(Vec<Box<str>>);

impl DocCommentStore {
    pub fn new() -> Self {
        Self(vec![String::new().into_boxed_str()])
    }

    pub fn add_doc_comment(&mut self, comment: Box<str>) -> DocComment {
        self.0.push(comment);
        DocComment(self.0.len() - 1)
    }

    pub fn merge_doc_comments(&mut self, a: DocComment, b: DocComment) {
        let mut v = std::mem::take(&mut self.0[a.0]).into_string();
        v.push('\n');
        v.push_str(&self.0[b.0]);
        self.0[a.0] = v.into_boxed_str();
    }

    pub fn clear_doc_comment(&mut self, v: DocComment) {
        self.0[v.0] = String::new().into_boxed_str();
    }
}

impl Index<DocComment> for DocCommentStore {
    type Output = str;

    fn index(&self, index: DocComment) -> &Self::Output {
        &self.0[index.0]
    }
}

impl Default for DocCommentStore {
    fn default() -> Self {
        Self::new()
    }
}

/// A doc comment index
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct DocComment(usize);

impl DocComment {
    pub const EMPTY: DocComment = DocComment(0);

    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }
}

impl Debug for DocComment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("<doc comment#{}>", self.0))
    }
}
