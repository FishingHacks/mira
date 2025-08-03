use crate::{
    error::ParsingError,
    tokenizer::{Token, TokenType},
};
use mira_spans::{interner::Symbol, Ident, Span};

pub struct TokenStream<'arena> {
    tokens: Vec<Token<'arena>>,
    eof_span: Span<'arena>,
    pos: usize,
}

impl<'arena> TokenStream<'arena> {
    /// pushes the token to the end of the tokenstream, maintaining the last token as eof.
    pub fn push_token(&mut self, tok: Token<'arena>) {
        assert_ne!(tok.typ, TokenType::Eof);
        self.tokens.push(tok);
        let len = self.tokens.len();
        if len > 1 && self.tokens[len - 2].typ == TokenType::Eof {
            self.tokens.swap(len - 1, len - 2);
        }
    }

    pub fn add_tokens<I: IntoIterator<Item = Token<'arena>>>(&mut self, tokens: I) {
        self.tokens.extend(tokens);
        assert!(!self.tokens[0..self.tokens.len().saturating_sub(1)]
            .iter()
            .any(|v| v.typ == TokenType::Eof));
    }

    pub fn new(mut tokens: Vec<Token<'arena>>, eof_span: Span<'arena>) -> Self {
        assert!(!tokens[0..tokens.len().saturating_sub(1)]
            .iter()
            .any(|v| v.typ == TokenType::Eof));
        match tokens.last() {
            Some(Token {
                typ: TokenType::Eof,
                ..
            }) => {}
            _ => tokens.push(Token::new(TokenType::Eof, None, eof_span)),
        }
        Self {
            tokens,
            eof_span,
            pos: 0,
        }
    }

    /// returns all tokens from the current position of the stream
    pub fn tokens(&self) -> &[Token<'arena>] {
        &self.tokens[self.pos..]
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn set_pos(&mut self, pos: usize) {
        self.pos = pos;
    }

    /// returns true if the inner vec is empty or there's only an Eof token left.
    pub fn is_at_end(&self) -> bool {
        let tokens = self.tokens();
        tokens.is_empty() || (tokens.len() == 1 && tokens[0].typ == TokenType::Eof)
    }

    pub fn eof_span(&self) -> Span<'arena> {
        self.eof_span
    }

    pub fn eof_token(&self) -> Token<'arena> {
        Token::new(TokenType::Eof, None, self.eof_span)
    }

    /// dismisses (removes) a token
    pub fn dismiss(&mut self) {
        self.pos += 1;
    }

    pub fn eat(&mut self) -> Token<'arena> {
        self.pos += 1;
        self.tokens[self.pos - 1]
    }

    /// returns the "current" token, aka the last token dismissed or returned by eat
    pub fn current(&self) -> Token<'arena> {
        assert!(self.pos > 0);
        self.tokens[self.pos - 1]
    }

    /// returns the token that would be returned by the next eat() call
    pub fn peek(&self) -> Token<'arena> {
        self.tokens
            .get(self.pos)
            .copied()
            .unwrap_or_else(|| self.eof_token())
    }

    // the token returned by the eat call before the current one
    pub fn last(&self) -> Token<'arena> {
        assert!(self.pos > 1);
        self.tokens[self.pos - 2]
    }

    /// returns the token that would be returned by the next eat() call after a prior eat/dismiss
    /// call
    pub fn peek2(&self) -> Token<'arena> {
        self.tokens
            .get(self.pos + 1)
            .copied()
            .unwrap_or_else(|| self.eof_token())
    }

    /// Expects and removes a token. Only removes if the token matches the expected token.
    pub fn expect(&mut self, expected: TokenType) -> Result<Token<'arena>, ParsingError<'arena>> {
        if self.is_at_end() {
            return Err(ParsingError::Expected {
                loc: self.eof_span,
                expected,
                found: self.eof_token(),
            });
        }
        let tok = self.peek();
        if tok.typ != expected {
            return Err(ParsingError::Expected {
                loc: tok.span,
                expected,
                found: tok,
            });
        }
        Ok(self.eat())
    }

    pub fn expect_one_of(
        &mut self,
        expected: &'static [TokenType],
    ) -> Result<Token<'arena>, ParsingError<'arena>> {
        if self.is_at_end() {
            return Err(ParsingError::ExpectedOneOf {
                loc: self.eof_span,
                valid: expected,
                found: self.eof_token(),
            });
        }
        let tok = self.peek();
        if !expected.contains(&tok.typ) {
            return Err(ParsingError::ExpectedOneOf {
                loc: tok.span,
                valid: expected,
                found: tok,
            });
        }
        Ok(self.eat())
    }

    /// Expects and removes a token. Only removes if the token matches the expected token.
    pub fn expect_identifier(&mut self) -> Result<Ident<'arena>, ParsingError<'arena>> {
        let tok = self.expect(TokenType::IdentifierLiteral)?;
        Ok(Ident::new(tok.string_literal()?, tok.span))
    }

    pub fn match_tok(&mut self, typ: TokenType) -> bool {
        let res = self.peek().typ == typ;
        if res {
            self.dismiss();
        }
        res
    }

    pub fn matches(&mut self, typ: &[TokenType]) -> bool {
        let res = typ.contains(&self.peek().typ);
        if res {
            self.dismiss();
        }
        res
    }

    /// Expects and removes a token. Only removes if the token matches the expected token.
    pub fn expect_string(
        &mut self,
    ) -> Result<(Symbol<'arena>, Span<'arena>), ParsingError<'arena>> {
        let tok = self.expect(TokenType::StringLiteral)?;
        Ok((tok.string_literal()?, tok.span))
    }

    /// Finishes parsing. Will error if there are any tokens remaining, unless there's only a
    /// single `Eof` token.
    pub fn finish(&mut self) -> Result<(), ParsingError<'arena>> {
        self.is_at_end()
            .then_some(())
            .ok_or(ParsingError::Expected {
                loc: self.tokens[0].span,
                expected: TokenType::Eof,
                found: self.tokens[0],
            })
    }
}
