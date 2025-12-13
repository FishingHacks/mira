use mira_lexer::{Delimiter, TokenType};
use mira_spans::{Span, interner::symbols};

use crate::{Parser, ParsingError};

impl<'ctx> Parser<'_, 'ctx> {
    pub fn parse_annotation(&mut self, span: Span<'ctx>) -> Result<(), ParsingError<'ctx>> {
        self.dismiss();

        let name = match self.peek_tok().map(|v| v.ty) {
            Some(TokenType::If) => symbols::Keywords::If,
            Some(TokenType::While) => symbols::Keywords::While,
            Some(TokenType::For) => symbols::Keywords::For,
            Some(TokenType::Pub) => symbols::Keywords::Pub,
            Some(TokenType::As) => symbols::Keywords::As,
            Some(TokenType::Else) => symbols::Keywords::Else,
            Some(TokenType::Asm) => symbols::Keywords::Asm,
            Some(TokenType::Volatile) => symbols::Keywords::Volatile,
            Some(TokenType::Impl) => symbols::Keywords::Impl,
            Some(TokenType::Fn) => symbols::Keywords::Fn,
            Some(TokenType::In) => symbols::Keywords::In,
            Some(TokenType::Unsized) => symbols::Keywords::Unsized,
            Some(TokenType::Struct) => symbols::Keywords::Struct,
            Some(TokenType::Trait) => symbols::Keywords::Trait,
            Some(TokenType::IdentifierLiteral) => self.peek_tok().unwrap().string_literal(),

            _ => {
                return Err(ParsingError::Expected {
                    span: self.peek_span(),
                    expected: TokenType::IdentifierLiteral,
                    found: self.peek_tok(),
                });
            }
        };
        let mut end_span = self.eat().unwrap().span();

        let children = self
            .match_delim(Delimiter::Parenthesis)
            .inspect(|v| end_span = v.close_span)
            .map(|v| &*v.children)
            .unwrap_or(&[]);

        self.data.current_annotations.borrow_mut().push_annotation(
            *name,
            children,
            span.combine_with([end_span], self.ctx().span_interner),
            end_span,
            *self.ctx(),
        )
    }
}
