use std::fmt::{Display, Write};

use mira_lexer::TokenType;
use mira_spans::{Ident, Span, interner::SpanInterner};

use crate::{Parser, ParsingError, TypeRef};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Path<'ctx> {
    pub entries: Vec<(Ident<'ctx>, Vec<TypeRef<'ctx>>)>,
    pub span: Span<'ctx>,
}

impl Display for Path<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in 0..self.entries.len() {
            if i != 0 {
                f.write_str("::")?;
            }

            Display::fmt(&self.entries[i].0, f)?;
            if !self.entries[i].1.is_empty() {
                f.write_char('<')?;
                for type_idx in 0..self.entries[i].1.len() {
                    if type_idx != 0 {
                        f.write_str(", ")?;
                    }
                    Display::fmt(&self.entries[i].1[type_idx], f)?;
                }
                f.write_char('>')?;
            }
        }
        Ok(())
    }
}

impl<'ctx> Path<'ctx> {
    pub fn iter(&self) -> impl Iterator<Item = (&Ident<'ctx>, &[TypeRef<'ctx>])> {
        self.entries
            .iter()
            .map(|(name, generics)| (name, generics.as_slice()))
    }
    pub fn push(&mut self, name: Ident<'ctx>, generics: Vec<TypeRef<'ctx>>) {
        self.entries.push((name, generics));
    }
    pub fn pop(&mut self) -> Option<(Ident<'ctx>, Vec<TypeRef<'ctx>>)> {
        // ensure this is at least 1 element
        if self.entries.len() > 1 {
            self.entries.pop()
        } else {
            None
        }
    }
    pub fn new(entry: Ident<'ctx>, generics: Vec<TypeRef<'ctx>>) -> Self {
        Self {
            entries: vec![(entry, generics)],
            span: entry.span(),
        }
    }

    fn parse_generics(
        parser: &mut Parser<'_, 'ctx>,
    ) -> Result<Vec<TypeRef<'ctx>>, ParsingError<'ctx>> {
        let mut types = vec![];

        while !parser.match_tok_dismiss(TokenType::GreaterThan) {
            if !types.is_empty() {
                parser.expect(TokenType::Comma)?;
            }

            types.push(TypeRef::parse(parser)?);
        }
        Ok(types)
    }
    pub fn readjust_self_span(&mut self, span_interner: &SpanInterner<'ctx>) {
        self.span = self.entries[0]
            .0
            .span()
            .combine_with(self.entries[1..].iter().map(|v| v.0.span()), span_interner);
    }

    /// Parses a path where the generics are delimited with < ... >
    pub fn parse_ty(parser: &mut Parser<'_, 'ctx>) -> Result<Self, ParsingError<'ctx>> {
        let name = parser.expect_identifier()?;
        let mut path = if parser.match_tok_dismiss(TokenType::LessThan) {
            Self::new(name, Self::parse_generics(parser)?)
        } else {
            Self::new(name, Vec::new())
        };

        loop {
            if !parser.match_tok_dismiss(TokenType::NamespaceAccess) {
                break;
            }
            let subpath = parser.expect_identifier()?;
            if parser.match_tok_dismiss(TokenType::LessThan) {
                path.push(subpath, Self::parse_generics(parser)?);
            } else {
                path.push(subpath, Vec::new());
            }
        }
        path.readjust_self_span(parser.ctx().span_interner);

        Ok(path)
    }

    /// Parses a path where the generics are delimited with ::< ... >
    pub fn parse(parser: &mut Parser<'_, 'ctx>) -> Result<Self, ParsingError<'ctx>> {
        let name = parser.expect_identifier()?;
        if !parser.match_tok_dismiss(TokenType::NamespaceAccess) {
            return Ok(Self::new(name, Vec::new()));
        }
        let mut path;
        if parser.match_tok_dismiss(TokenType::LessThan) {
            path = Self::new(name, Self::parse_generics(parser)?);
            if !parser.match_tok_dismiss(TokenType::NamespaceAccess) {
                return Ok(path);
            }
        } else {
            path = Self::new(name, Vec::new());
        };

        loop {
            let subpath = parser.expect_identifier()?;
            if !parser.match_tok_dismiss(TokenType::NamespaceAccess) {
                path.push(subpath, Vec::new());
                break;
            }
            if parser.match_tok_dismiss(TokenType::LessThan) {
                path.push(subpath, Self::parse_generics(parser)?);
                if !parser.match_tok_dismiss(TokenType::NamespaceAccess) {
                    break;
                }
            } else {
                path.push(subpath, Vec::new());
            }
        }
        path.readjust_self_span(parser.ctx().span_interner);

        Ok(path)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PathWithoutGenerics<'ctx> {
    pub entries: Vec<Ident<'ctx>>,
    pub span: Span<'ctx>,
}

impl<'ctx> PathWithoutGenerics<'ctx> {
    pub fn iter(&self) -> std::slice::Iter<'_, Ident<'ctx>> {
        self.entries.iter()
    }

    pub fn iter_with_pathbuf(&self) -> impl Iterator<Item = (&Ident<'ctx>, &[TypeRef<'ctx>])> {
        #[allow(trivial_casts)]
        self.entries.iter().map(|v| (v, &[] as &[_]))
    }

    pub fn to_normal_path(&self) -> Path<'ctx> {
        Path {
            entries: self
                .entries
                .iter()
                .map(|&ident| (ident, Vec::new()))
                .collect(),
            span: self.span,
        }
    }

    pub fn push(&mut self, name: Ident<'ctx>) {
        self.entries.push(name);
    }
    pub fn pop(&mut self) -> Option<Ident<'ctx>> {
        // ensure this is at least 1 element
        if self.entries.len() > 1 {
            self.entries.pop()
        } else {
            None
        }
    }
    pub fn new(entry: Ident<'ctx>) -> Self {
        Self {
            span: entry.span(),
            entries: vec![entry],
        }
    }
    pub fn readjust_self_span(&mut self, span_interner: &SpanInterner<'ctx>) {
        self.span = self.entries[0]
            .span()
            .combine_with(self.entries[1..].iter().map(Ident::span), span_interner);
    }
    pub fn parse(parser: &mut Parser<'_, 'ctx>) -> Result<Self, ParsingError<'ctx>> {
        let mut path = Self::new(parser.expect_identifier()?);

        while parser.match_tok_dismiss(TokenType::NamespaceAccess) {
            let name = parser.expect_identifier()?;
            path.push(name);
        }

        path.readjust_self_span(parser.ctx().span_interner);
        Ok(path)
    }

    pub fn as_slice(&self) -> &[Ident<'ctx>] {
        &self.entries
    }
}

impl Display for PathWithoutGenerics<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in 0..self.entries.len() {
            if i != 0 {
                f.write_str("::")?;
            }
            Display::fmt(&self.entries[i], f)?;
        }
        Ok(())
    }
}
