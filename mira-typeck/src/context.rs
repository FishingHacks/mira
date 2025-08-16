use std::{fmt::Debug, io::IsTerminal};

use mira_errors::{DiagnosticFormatter, Output, StyledPrinter, Styles};
use mira_spans::SharedCtx;
use parking_lot::Mutex;

use mira_spans::{
    Arena, SourceMap, Span, SpanData, Symbol,
    interner::{SpanInterner, SymbolInterner},
};

use crate::{Ty, TyKind, TyList, TypeInterner, TypeListInterner};

pub struct GlobalContext<'arena> {
    arena: &'arena Arena,
    string_interner: Mutex<SymbolInterner<'arena>>,
    span_interner: SpanInterner<'arena>,
    type_interner: Mutex<TypeInterner<'arena>>,
    type_list_interner: Mutex<TypeListInterner<'arena>>,
    source_map: SourceMap,
}

impl Debug for GlobalContext<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GlobalContext").finish()
    }
}

impl<'arena> GlobalContext<'arena> {
    pub fn new(arena: &'arena Arena) -> Self {
        Self {
            string_interner: SymbolInterner::new(arena).into(),
            span_interner: SpanInterner::new(arena),
            type_interner: TypeInterner::new(arena).into(),
            type_list_interner: TypeListInterner::new(arena).into(),
            source_map: SourceMap::new(),
            arena,
        }
    }

    pub fn make_diagnostic_formatter<P: StyledPrinter>(
        &self,
        printer: P,
        output: Output,
    ) -> DiagnosticFormatter<'_, P> {
        let styles = match std::env::var("MIRA_COLOR").ok().as_deref() {
            Some("0" | "none" | "no") => Styles::NO_COLORS,
            Some("1" | "yes") => Styles::DEFAULT,
            _ if std::io::stdout().is_terminal() => Styles::DEFAULT,
            _ => Styles::NO_COLORS,
        };
        DiagnosticFormatter::new(&self.source_map, output, printer, styles)
    }

    pub fn ctx(&'arena self) -> SharedCtx<'arena> {
        SharedCtx::new(&self.string_interner, &self.span_interner, &self.source_map)
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

    pub fn make_diagnostic_formatter<P: StyledPrinter>(
        &self,
        printer: P,
        output: Output,
    ) -> DiagnosticFormatter<'_, P> {
        let styles = match std::env::var("MIRA_COLOR").ok().as_deref() {
            Some("0" | "none" | "no") => Styles::NO_COLORS,
            Some("1" | "yes") => Styles::DEFAULT,
            _ if std::io::stdout().is_terminal() => Styles::DEFAULT,
            _ => Styles::NO_COLORS,
        };
        DiagnosticFormatter::new(self.source_map(), output, printer, styles)
    }
}

impl<'arena> From<TypeCtx<'arena>> for SharedCtx<'arena> {
    fn from(value: TypeCtx<'arena>) -> Self {
        SharedCtx::new(
            &value.0.string_interner,
            &value.0.span_interner,
            &value.0.source_map,
        )
    }
}
