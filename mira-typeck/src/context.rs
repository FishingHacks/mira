use std::{fmt::Debug, sync::Arc};

use mira_context::{DiagCtx, DiagEmitter, DocComment, DocCommentStore, ErrorTracker, SharedCtx};
use mira_errors::{Diagnostic, ErrorEmitted, StyledPrinter, Styles};
use parking_lot::Mutex;

use mira_spans::{
    Arena, SourceMap, Span, SpanData, Symbol,
    interner::{SpanInterner, SymbolInterner},
};

use crate::{
    Ty, TyKind, TyList, TypeInterner, TypeListInterner,
    queries::{Providers, QuerySystem},
};

pub struct GlobalContext<'arena> {
    arena: &'arena Arena,
    string_interner: Mutex<SymbolInterner<'arena>>,
    doc_comment_store: Mutex<DocCommentStore>,
    span_interner: SpanInterner<'arena>,
    type_interner: Mutex<TypeInterner<'arena>>,
    type_list_interner: Mutex<TypeListInterner<'arena>>,
    source_map: Arc<SourceMap>,
    diag_ctx: Mutex<DiagCtx>,
    query_system: QuerySystem<'arena>,
}

impl Debug for GlobalContext<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GlobalContext").finish()
    }
}

impl<'arena> GlobalContext<'arena> {
    pub fn new(
        arena: &'arena Arena,
        emitter: DiagEmitter,
        printer: Box<dyn StyledPrinter>,
        styles: Styles,
        provider: impl FnOnce(&mut Providers<'arena>),
    ) -> Self {
        let source_map = Arc::new(SourceMap::new());
        let mut query_system = QuerySystem::new();
        provider(query_system.get_providers());
        Self {
            string_interner: SymbolInterner::new(arena).into(),
            doc_comment_store: DocCommentStore::new().into(),
            span_interner: SpanInterner::new(arena),
            type_interner: TypeInterner::new(arena).into(),
            type_list_interner: TypeListInterner::new(arena).into(),
            diag_ctx: DiagCtx::new(emitter, source_map.clone(), printer, styles).into(),
            source_map,
            arena,
            query_system,
        }
    }

    pub fn ctx(&'arena self) -> SharedCtx<'arena> {
        SharedCtx::new(
            &self.string_interner,
            &self.doc_comment_store,
            &self.span_interner,
            &self.source_map,
            &self.diag_ctx,
        )
    }

    pub fn ty_ctx(&'arena self) -> TypeCtx<'arena> {
        TypeCtx(self)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct TypeCtx<'arena>(&'arena GlobalContext<'arena>);

impl<'arena> TypeCtx<'arena> {
    pub fn arena(self) -> &'arena Arena {
        self.0.arena
    }

    pub fn intern_str(self, s: &str) -> Symbol<'arena> {
        self.0.string_interner.lock().intern(s)
    }

    pub fn intern_span(self, data: SpanData) -> Span<'arena> {
        Span::new(data, &self.0.span_interner)
    }

    pub fn intern_ty_ref(self, ty: &TyKind<'arena>) -> Ty<'arena> {
        self.0.type_interner.lock().intern(ty)
    }

    pub fn intern_ty(self, ty: TyKind<'arena>) -> Ty<'arena> {
        self.0.type_interner.lock().intern_owned(ty)
    }

    pub fn intern_tylist(self, types: &[Ty<'arena>]) -> TyList<'arena> {
        self.0.type_list_interner.lock().intern(types)
    }

    pub fn span_interner(&self) -> &SpanInterner<'arena> {
        &self.0.span_interner
    }

    pub fn combine_spans(&self, spans: impl IntoIterator<Item = Span<'arena>>) -> Span<'arena> {
        let mut spans = spans.into_iter();
        let Some(first) = spans.next() else {
            return Span::DUMMY;
        };
        first.combine_with(spans, self.span_interner())
    }

    pub fn source_map(&self) -> &SourceMap {
        &self.0.source_map
    }

    pub fn emit_diags(&self, diags: impl IntoIterator<Item = Diagnostic<'arena>>) -> ErrorEmitted {
        let mut dctx = self.0.diag_ctx.lock();
        let mut iter = diags.into_iter();
        let first = iter.next().expect("emit_diags: no diagnostics?");
        let emitted = dctx.emit_diag(first);
        for diag in iter {
            _ = dctx.emit_diag(diag);
        }
        emitted
    }

    pub fn emit_diag(&self, diag: Diagnostic<'arena>) -> ErrorEmitted {
        self.0.diag_ctx.lock().emit_diag(diag)
    }

    pub fn err_count(&self) -> usize {
        self.0.diag_ctx.lock().err_count()
    }

    pub fn track_errors(&self) -> ErrorTracker {
        self.0.diag_ctx.lock().track_errors()
    }

    #[deprecated = "use error_happened_res"]
    pub fn errors_happened(&self, tracker: ErrorTracker) -> bool {
        self.0.diag_ctx.lock().errors_happened(tracker)
    }

    pub fn errors_happened_res(&self, tracker: ErrorTracker) -> Result<(), ErrorEmitted> {
        if self.0.diag_ctx.lock().errors_happened(tracker) {
            // This is fine, because we *know* errors were emitted.
            #[allow(deprecated)]
            Err(ErrorEmitted::new())
        } else {
            Ok(())
        }
    }

    pub fn add_doc_comment(&mut self, comment: impl Into<Box<str>>) -> DocComment {
        self.0
            .doc_comment_store
            .lock()
            .add_doc_comment(comment.into())
    }

    pub fn with_doc_comment<T>(&self, comment: DocComment, f: impl FnOnce(&str) -> T) -> T {
        f(&self.0.doc_comment_store.lock()[comment])
    }

    pub fn to_shared(self) -> SharedCtx<'arena> {
        self.into()
    }

    pub fn query_system(&self) -> &'arena QuerySystem<'arena> {
        &self.0.query_system
    }
}

impl<'ctx> mira_errors::DiagEmitter<'ctx> for TypeCtx<'ctx> {
    fn emit_diagnostic(&self, diag: Diagnostic<'ctx>) -> ErrorEmitted {
        self.emit_diag(diag)
    }
}

impl<'arena> From<TypeCtx<'arena>> for SharedCtx<'arena> {
    fn from(value: TypeCtx<'arena>) -> Self {
        SharedCtx::new(
            &value.0.string_interner,
            &value.0.doc_comment_store,
            &value.0.span_interner,
            &value.0.source_map,
            &value.0.diag_ctx,
        )
    }
}

impl<'ctx> mira_lexer::LexingErrorEmitterExt<'ctx> for TypeCtx<'ctx> {}
impl<'ctx> crate::error::TypecheckingErrorEmitterExt<'ctx> for TypeCtx<'ctx> {}
impl<'ctx> crate::lang_items::LangItemErrorEmitterExt<'ctx> for TypeCtx<'ctx> {}
