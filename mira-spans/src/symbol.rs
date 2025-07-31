use std::{
    borrow::Borrow,
    fmt::{Debug, Display},
    hash::Hash,
    ops::Deref,
};

pub use crate::interner::Symbol;
use crate::span::Span;

#[derive(Debug, Clone, Copy, Eq)]
pub struct Ident<'arena> {
    symbol: Symbol<'arena>,
    span: Span<'arena>,
}

impl PartialEq for Ident<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.symbol().eq(&other.symbol())
    }
}

impl Hash for Ident<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.symbol().hash(state);
    }
}

impl<'arena> Borrow<Symbol<'arena>> for Ident<'arena> {
    fn borrow(&self) -> &Symbol<'arena> {
        &self.symbol
    }
}

impl Display for Ident<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(*self.symbol, f)
    }
}

impl<'arena> Ident<'arena> {
    pub const EMPTY: Ident<'arena> = Ident::new(Symbol::EMPTY, Span::DUMMY);

    pub const fn new(symbol: Symbol<'arena>, span: Span<'arena>) -> Self {
        Self { symbol, span }
    }

    pub fn symbol(&self) -> Symbol<'arena> {
        self.symbol
    }

    pub fn span(&self) -> Span<'arena> {
        self.span
    }
}

impl<'arena> Deref for Ident<'arena> {
    type Target = str;

    fn deref(&self) -> &'arena Self::Target {
        &self.symbol
    }
}
