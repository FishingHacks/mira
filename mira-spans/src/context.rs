use std::{fmt::Debug, ops::Index};

use crate::{
    Arena, SourceMap, Span, SpanData, Symbol,
    interner::{SpanInterner, SymbolInterner},
};
use parking_lot::Mutex;

pub struct GlobalCtx<'arena> {
    string_interner: Mutex<SymbolInterner<'arena>>,
    doc_comment_store: Mutex<DocCommentStore>,
    pub span_interner: SpanInterner<'arena>,
    pub source_map: SourceMap,
}

impl<'arena> GlobalCtx<'arena> {
    pub fn new(arena: &'arena Arena) -> Self {
        Self {
            string_interner: SymbolInterner::new(arena).into(),
            doc_comment_store: DocCommentStore::new().into(),
            span_interner: SpanInterner::new(arena),
            source_map: SourceMap::new(),
        }
    }

    pub fn share(&'arena self) -> SharedCtx<'arena> {
        SharedCtx::new(
            &self.string_interner,
            &self.doc_comment_store,
            &self.span_interner,
            &self.source_map,
        )
    }
}

#[derive(Clone, Copy)]
pub struct SharedCtx<'arena> {
    string_interner: &'arena Mutex<SymbolInterner<'arena>>,
    doc_comment_store: &'arena Mutex<DocCommentStore>,
    pub span_interner: &'arena SpanInterner<'arena>,
    pub source_map: &'arena SourceMap,
}

impl<'arena> SharedCtx<'arena> {
    pub fn new(
        string_interner: &'arena Mutex<SymbolInterner<'arena>>,
        doc_comment_store: &'arena Mutex<DocCommentStore>,
        span_interner: &'arena SpanInterner<'arena>,
        source_map: &'arena SourceMap,
    ) -> Self {
        Self {
            string_interner,
            doc_comment_store,
            span_interner,
            source_map,
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
    pub fn merge_doc_comments(&mut self, a: DocComment, b: DocComment) {
        self.doc_comment_store.lock().merge_doc_comments(a, b);
    }

    /// removes a doc comment.
    pub fn clear_doc_comment(&mut self, v: DocComment) {
        self.doc_comment_store.lock().clear_doc_comment(v);
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
}

impl Debug for DocComment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("<doc comment#{}>", self.0))
    }
}
