use mira_spans::{
    SourceMap, Span, SpanData, Symbol,
    interner::{SpanInterner, SymbolInterner},
};
use parking_lot::Mutex;

#[derive(Clone, Copy)]
pub struct LexingContext<'arena> {
    string_interner: &'arena Mutex<SymbolInterner<'arena>>,
    span_interner: &'arena SpanInterner<'arena>,
    pub source_map: &'arena SourceMap,
}

impl<'arena> LexingContext<'arena> {
    pub fn new(
        string_interner: &'arena Mutex<SymbolInterner<'arena>>,
        span_interner: &'arena SpanInterner<'arena>,
        source_map: &'arena SourceMap,
    ) -> Self {
        Self {
            string_interner,
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
}
