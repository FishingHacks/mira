use std::{fmt::Debug, io::IsTerminal, sync::OnceLock};

use mira_errors::{DiagnosticFormatter, Output, StyledPrinter, Styles};
use parking_lot::Mutex;

use mira_spans::{
    interner::{InternedStr, SpanInterner, StringInterner},
    Arena, SourceMap, Span, SpanData,
};

pub struct GlobalContext<'arena> {
    string_interner: Mutex<StringInterner<'arena>>,
    span_interner: SpanInterner<'arena>,
    source_map: OnceLock<SourceMap>,
}

impl Debug for GlobalContext<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GlobalContext").finish()
    }
}

impl<'arena> GlobalContext<'arena> {
    pub fn new(arena: &'arena Arena) -> Self {
        Self {
            string_interner: StringInterner::new(arena).into(),
            span_interner: SpanInterner::new(arena),
            source_map: OnceLock::new(),
        }
    }

    pub fn init_source_map(&self, source_map: SourceMap) {
        self.source_map
            .set(source_map)
            .map_err(|_| ())
            .expect("source map should not yet be initialized")
    }

    pub fn share(&'arena self) -> SharedContext<'arena> {
        SharedContext(self)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct SharedContext<'arena>(&'arena GlobalContext<'arena>);

impl<'arena> SharedContext<'arena> {
    pub fn intern_str(self, s: &str) -> InternedStr<'arena> {
        self.0.string_interner.lock().intern(s)
    }

    pub fn intern_span(self, data: SpanData) -> Span<'arena> {
        Span::new(data, &self.0.span_interner)
    }

    pub fn span_interner(&self) -> &SpanInterner<'arena> {
        &self.0.span_interner
    }

    pub fn source_map(&self) -> &SourceMap {
        self.0
            .source_map
            .get()
            .expect("source map should have been initialized")
    }

    pub fn init_source_map(self, source_map: SourceMap) {
        self.0
            .source_map
            .set(source_map)
            .map_err(|_| ())
            .expect("source map should not yet be initialized")
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
