use std::{fmt::Debug, io::IsTerminal};

use mira_errors::{DiagnosticFormatter, Output, StyledPrinter, Styles};
use parking_lot::Mutex;

use mira_spans::{
    interner::{SpanInterner, SymbolInterner},
    Arena, SourceMap, Span, SpanData, Symbol,
};

pub struct GlobalContext<'arena> {
    string_interner: Mutex<SymbolInterner<'arena>>,
    span_interner: SpanInterner<'arena>,
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
            source_map: SourceMap::new(),
        }
    }

    pub fn share(&'arena self) -> SharedContext<'arena> {
        SharedContext(self)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct SharedContext<'arena>(&'arena GlobalContext<'arena>);

impl<'arena> SharedContext<'arena> {
    pub fn intern_str(self, s: &str) -> Symbol<'arena> {
        self.0.string_interner.lock().intern(s)
    }

    pub fn intern_span(self, data: SpanData) -> Span<'arena> {
        Span::new(data, &self.0.span_interner)
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
