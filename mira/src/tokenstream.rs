use crate::{
    error::ParsingError,
    tokenizer::{Literal, NumberType, Token, TokenType},
};
use mira_spans::{interner::InternedStr, Span};

enum LiteralType {
    String,
    SInt,
    UInt,
    Float,
    Bool,
}

// this is represented by a vector of tokens. So it's not really a "stream", but i dont know any
// better name so this will do for now.
// TODO: Change name to something more sensical
pub struct TokenStream<'arena>(Vec<Token<'arena>>, Span<'arena>);

impl<'arena> TokenStream<'arena> {
    /// pushes the token to the end of the tokenstream, unless the last token is `TokenType::Eof`
    pub fn push_token(&mut self, tok: Token<'arena>) {
        self.0.push(tok);
        let len = self.0.len();
        if len > 1 && self.0[len - 2].typ == TokenType::Eof {
            self.0.swap(len - 1, len - 2);
        }
    }

    pub fn new(tokens: Vec<Token<'arena>>, end: Span<'arena>) -> Self {
        Self(tokens, end)
    }

    /// returns true if the inner vec is empty or there's only an Eof token left.
    pub fn is_at_end(&self) -> bool {
        self.0.is_empty() || (self.0.len() == 1 && self.0[0].typ == TokenType::Eof)
    }

    pub fn eof_span(&self) -> Span<'arena> {
        self.1
    }

    pub fn expect_token(
        &self,
        expected: TokenType,
    ) -> Result<&Token<'arena>, ParsingError<'arena>> {
        self.0
            .first()
            .filter(|v| v.typ == expected)
            .ok_or(ParsingError::ExpectedArbitrary {
                loc: self.1,
                expected,
                found: TokenType::Eof,
            })
            .map(|_| &self.0[0])
    }

    /// Expects and removes a token. Only removes if the token matches the expected token.
    pub fn expect_remove_token(
        &mut self,
        expected: TokenType,
    ) -> Result<Token<'arena>, ParsingError<'arena>> {
        self.expect_token(expected)?;
        Ok(self.0.remove(0))
    }

    /// dismisses (removes) a token
    pub fn dismiss(&mut self) {
        self.0.remove(0);
    }

    fn expect_literal(
        &mut self,
        expected_type: TokenType,
        expected_literal_type: LiteralType,
    ) -> Result<&Token<'arena>, ParsingError<'arena>> {
        let tok = self.expect_token(expected_type)?;
        match expected_literal_type {
            LiteralType::String => _ = tok.string_literal()?,
            LiteralType::SInt => _ = tok.sint_literal()?,
            LiteralType::UInt => _ = tok.uint_literal()?,
            LiteralType::Float => _ = tok.float_literal()?,
            LiteralType::Bool => _ = tok.bool_literal()?,
        }
        Ok(tok)
    }

    fn expect_remove_literal(
        &mut self,
        expected_type: TokenType,
        expected_literal_type: LiteralType,
    ) -> Result<Token<'arena>, ParsingError<'arena>> {
        self.expect_literal(expected_type, expected_literal_type)?;
        Ok(self.0.remove(0))
    }

    pub fn expect_identifier(
        &mut self,
    ) -> Result<(InternedStr<'arena>, Span<'_>), ParsingError<'arena>> {
        self.expect_literal(TokenType::IdentifierLiteral, LiteralType::String)
            .map(|v| match &v.literal {
                Some(Literal::String(lit)) => (*lit, v.span),
                _ => unreachable!(),
            })
    }

    /// Expects and removes a token. Only removes if the token matches the expected token.
    pub fn expect_remove_identifier(
        &mut self,
    ) -> Result<(InternedStr<'arena>, Span<'arena>), ParsingError<'arena>> {
        self.expect_remove_literal(TokenType::IdentifierLiteral, LiteralType::String)
            .map(|v| match v.literal {
                Some(Literal::String(lit)) => (lit, v.span),
                _ => unreachable!(),
            })
    }

    pub fn expect_string(
        &mut self,
    ) -> Result<(InternedStr<'arena>, Span<'arena>), ParsingError<'arena>> {
        self.expect_literal(TokenType::StringLiteral, LiteralType::String)
            .map(|v| match &v.literal {
                Some(Literal::String(lit)) => (*lit, v.span),
                _ => unreachable!(),
            })
    }

    /// Expects and removes a token. Only removes if the token matches the expected token.
    pub fn expect_remove_string(
        &mut self,
    ) -> Result<(InternedStr<'arena>, Span<'arena>), ParsingError<'arena>> {
        self.expect_remove_literal(TokenType::StringLiteral, LiteralType::String)
            .map(|v| match v.literal {
                Some(Literal::String(lit)) => (lit, v.span),
                _ => unreachable!(),
            })
    }

    pub fn expect_float(
        &mut self,
    ) -> Result<(f64, NumberType, Span<'arena>), ParsingError<'arena>> {
        self.expect_literal(TokenType::FloatLiteral, LiteralType::Float)
            .map(|v| match &v.literal {
                Some(Literal::Float(lit, typ)) => (*lit, *typ, v.span),
                _ => unreachable!(),
            })
    }

    /// Expects and removes a token. Only removes if the token matches the expected token.
    pub fn expect_remove_float(
        &mut self,
    ) -> Result<(f64, NumberType, Span<'arena>), ParsingError<'arena>> {
        let v = self.expect_float()?;
        self.0.remove(0);
        Ok(v)
    }

    pub fn expect_uint(&mut self) -> Result<(u64, NumberType, Span<'arena>), ParsingError<'arena>> {
        self.expect_literal(TokenType::UIntLiteral, LiteralType::UInt)
            .map(|v| match &v.literal {
                Some(Literal::UInt(lit, typ)) => (*lit, *typ, v.span),
                _ => unreachable!(),
            })
    }

    /// Expects and removes a token. Only removes if the token matches the expected token.
    pub fn expect_remove_uint(
        &mut self,
    ) -> Result<(u64, NumberType, Span<'arena>), ParsingError<'arena>> {
        let v = self.expect_uint()?;
        self.0.remove(0);
        Ok(v)
    }

    pub fn expect_sint(&mut self) -> Result<(i64, NumberType, Span<'arena>), ParsingError<'arena>> {
        self.expect_literal(TokenType::SIntLiteral, LiteralType::SInt)
            .map(|v| match &v.literal {
                Some(Literal::SInt(lit, typ)) => (*lit, *typ, v.span),
                _ => unreachable!(),
            })
    }

    /// Expects and removes a token. Only removes if the token matches the expected token.
    pub fn expect_remove_sint(
        &mut self,
    ) -> Result<(i64, NumberType, Span<'arena>), ParsingError<'arena>> {
        let v = self.expect_sint()?;
        self.0.remove(0);
        Ok(v)
    }

    pub fn expect_bool(&mut self) -> Result<(bool, Span<'arena>), ParsingError<'arena>> {
        self.expect_literal(TokenType::BooleanLiteral, LiteralType::Bool)
            .map(|v| match &v.literal {
                Some(Literal::Bool(lit)) => (*lit, v.span),
                _ => unreachable!(),
            })
    }

    /// Expects and removes a token. Only removes if the token matches the expected token.
    pub fn expect_remove_bool(&mut self) -> Result<(bool, Span<'arena>), ParsingError<'arena>> {
        let v = self.expect_bool()?;
        self.0.remove(0);
        Ok(v)
    }

    /// Finishes parsing. Will error if there are any tokens remaining, unless there's only a
    /// single `Eof` token.
    pub fn finish(&mut self) -> Result<(), ParsingError<'arena>> {
        if (self.0.len() == 1 && self.0[0].typ == TokenType::Eof) || self.0.is_empty() {
            Ok(())
        } else {
            Err(ParsingError::ExpectedArbitrary {
                loc: self.0[0].span,
                expected: TokenType::Eof,
                found: self.0[0].typ,
            })
        }
    }
}
