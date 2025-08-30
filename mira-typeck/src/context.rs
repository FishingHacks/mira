use std::{fmt::Debug, sync::Arc};

use mira_context::{DiagCtx, DiagEmitter, DocComment, DocCommentStore, ErrorTracker, SharedCtx};
use mira_errors::{Diagnostic, StyledPrinter, Styles};
use parking_lot::Mutex;

use mira_spans::{
    Arena, SourceMap, Span, SpanData, Symbol,
    interner::{SpanInterner, SymbolInterner},
};

use crate::{Ty, TyKind, TyList, TypeInterner, TypeListInterner};

pub struct GlobalContext<'arena> {
    arena: &'arena Arena,
    string_interner: Mutex<SymbolInterner<'arena>>,
    doc_comment_store: Mutex<DocCommentStore>,
    span_interner: SpanInterner<'arena>,
    type_interner: Mutex<TypeInterner<'arena>>,
    type_list_interner: Mutex<TypeListInterner<'arena>>,
    source_map: Arc<SourceMap>,
    diag_ctx: Mutex<DiagCtx>,
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
    ) -> Self {
        let source_map = Arc::new(SourceMap::new());
        Self {
            string_interner: SymbolInterner::new(arena).into(),
            doc_comment_store: DocCommentStore::new().into(),
            span_interner: SpanInterner::new(arena),
            type_interner: TypeInterner::new(arena).into(),
            type_list_interner: TypeListInterner::new(arena).into(),
            diag_ctx: DiagCtx::new(emitter, source_map.clone(), printer, styles).into(),
            source_map,
            arena,
        }
    }

    // pub fn diag_ctx(&'arena self, emitter: DiagEmitter) -> Mutex<DiagCtx<'arena>> {
    //     let styles = match std::env::var("MIRA_COLOR").ok().as_deref() {
    //         Some("0" | "none" | "no") => Styles::NO_COLORS,
    //         Some("1" | "yes") => Styles::DEFAULT,
    //         _ if std::io::stdout().is_terminal() => Styles::DEFAULT,
    //         _ => Styles::NO_COLORS,
    //     };
    //     let printer: Box<dyn StyledPrinter> = match std::env::var("MIRA_DIAG_STYLE").ok().as_deref()
    //     {
    //         Some("ascii" | "text") => Box::new(AsciiPrinter::new()),
    //         _ => Box::new(UnicodePrinter::new()),
    //     };
    //     Mutex::new(DiagCtx::new(
    //         emitter,
    //         self.source_map.clone(),
    //         printer,
    //         styles,
    //     ))
    // }

    // pub fn make_diagnostic_formatter<P: StyledPrinter + 'static>(
    //     &self,
    //     printer: P,
    //     output: Output,
    // ) -> DiagnosticFormatter<'_> {
    //     let styles = match std::env::var("MIRA_COLOR").ok().as_deref() {
    //         Some("0" | "none" | "no") => Styles::NO_COLORS,
    //         Some("1" | "yes") => Styles::DEFAULT,
    //         _ if std::io::stdout().is_terminal() => Styles::DEFAULT,
    //         _ => Styles::NO_COLORS,
    //     };
    //     DiagnosticFormatter::new(&self.source_map, output, Box::new(printer), styles)
    // }

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

    pub fn source_map(&self) -> &SourceMap {
        &self.0.source_map
    }

    // pub fn make_diagnostic_formatter<P: StyledPrinter + 'static>(
    //     &self,
    //     printer: P,
    //     output: Output,
    // ) -> DiagnosticFormatter<'_> {
    //     self.0.make_diagnostic_formatter(printer, output)
    // }

    pub fn emit_diags(&self, diags: impl IntoIterator<Item = Diagnostic<'arena>>) {
        let mut dctx = self.0.diag_ctx.lock();
        for diag in diags {
            dctx.emit_diag(diag);
        }
    }

    pub fn emit_diag(&self, diag: Diagnostic<'arena>) {
        self.0.diag_ctx.lock().emit_diag(diag);
    }

    pub fn err_count(&self) -> usize {
        self.0.diag_ctx.lock().err_count()
    }

    pub fn track_errors(&self) -> ErrorTracker {
        self.0.diag_ctx.lock().track_errors()
    }

    pub fn errors_happened(&self, tracker: ErrorTracker) -> bool {
        self.0.diag_ctx.lock().errors_happened(tracker)
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
}

impl<'ctx> mira_errors::DiagEmitter<'ctx> for TypeCtx<'ctx> {
    fn emit_diagnostic(&self, diag: Diagnostic<'ctx>) {
        TypeCtx::emit_diag(self, diag);
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
