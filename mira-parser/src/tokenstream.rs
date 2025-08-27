use std::slice::SliceIndex;

use crate::error::ParsingError;
use mira_lexer::{Literal, Token, TokenType};
use mira_spans::{Ident, Span, context::DocComment, interner::Symbol};

pub trait TokenHolder<'arena>: AsRef<[Token<'arena>]> {
    fn len(&self) -> usize {
        self.as_ref().len()
    }
    fn is_empty(&self) -> bool {
        self.as_ref().is_empty()
    }
    fn last(&self) -> Option<Token<'arena>> {
        self.as_ref()
            .get(self.as_ref().len().saturating_sub(1))
            .copied()
    }
    fn get<'a, I: SliceIndex<[Token<'arena>]>>(&'a self, value: I) -> Option<&'a I::Output>
    where
        'arena: 'a,
    {
        self.as_ref().get(value)
    }
}

pub trait MutableTokenHolder<'arena>: TokenHolder<'arena> {
    fn swap(&mut self, a: usize, b: usize);
    fn push(&mut self, token: Token<'arena>);
    fn extend(&mut self, iter: impl IntoIterator<Item = Token<'arena>>) {
        for token in iter {
            self.push(token)
        }
    }
}

impl<'arena> TokenHolder<'arena> for &[Token<'arena>] {}

impl<'arena> TokenHolder<'arena> for Vec<Token<'arena>> {}

impl<'arena> MutableTokenHolder<'arena> for Vec<Token<'arena>> {
    fn swap(&mut self, a: usize, b: usize) {
        <[_]>::swap(self, a, b)
    }

    fn push(&mut self, token: Token<'arena>) {
        Vec::push(self, token);
    }

    fn extend(&mut self, iter: impl IntoIterator<Item = Token<'arena>>) {
        Extend::extend(self, iter);
    }
}

pub struct TokenStream<'arena, Holder: TokenHolder<'arena> = Vec<Token<'arena>>> {
    tokens: Holder,
    eof_span: Span<'arena>,
    pos: usize,
}

pub type OwnedTokenStream<'arena> = TokenStream<'arena>;
pub type BorrowedTokenStream<'arena, 'a> = TokenStream<'arena, &'a [Token<'arena>]>;

impl<'arena, Holder: TokenHolder<'arena>> TokenStream<'arena, Holder> {
    pub fn new(tokens: Holder, eof_span: Span<'arena>) -> Self {
        let token_slice = tokens.as_ref();
        assert!(
            !token_slice[0..token_slice.len().saturating_sub(1)]
                .iter()
                .any(|v| v.typ == TokenType::Eof)
        );
        Self {
            tokens,
            eof_span,
            pos: 0,
        }
    }

    /// returns all tokens from the current position of the stream, without doc comments
    pub fn tokens(&self) -> impl Iterator<Item = Token<'arena>> {
        self.tokens.as_ref()[self.pos..]
            .iter()
            .filter(|v| v.typ != TokenType::DocComment)
            .copied()
    }

    /// returns all previous tokens
    fn previous_tokens(&self) -> impl Iterator<Item = Token<'arena>> {
        self.tokens.as_ref()[..self.pos]
            .iter()
            .rev()
            .filter(|v| v.typ != TokenType::DocComment)
            .copied()
    }

    pub fn token_holder(&self) -> &Holder {
        &self.tokens
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn set_pos(&mut self, pos: usize) {
        self.pos = pos;
    }

    /// returns true if the inner vec is empty or there's only an Eof token left.
    pub fn is_at_end(&self) -> bool {
        let mut toks = self.tokens();
        let token = toks.next();
        match token {
            _ if toks.next().is_some() => false,
            Some(v) if v.typ != TokenType::Eof => false,
            _ => true,
        }
    }

    pub fn eof_span(&self) -> Span<'arena> {
        self.eof_span
    }

    pub fn eof_token(&self) -> Token<'arena> {
        Token::new(TokenType::Eof, None, self.eof_span)
    }

    /// dismisses (removes) a token
    pub fn dismiss(&mut self) {
        self.eat();
    }

    pub fn eat(&mut self) -> Token<'arena> {
        loop {
            match self.tokens.get(self.pos).copied() {
                Some(tok) if tok.typ == TokenType::DocComment => self.pos += 1,
                Some(tok) => {
                    self.pos += 1;
                    return tok;
                }
                None => return self.eof_token(),
            }
        }
    }

    /// returns the "current" token, aka the last token dismissed or returned by eat
    pub fn current(&self) -> Token<'arena> {
        self.previous_tokens()
            .next()
            .expect("there should always have been a token")
    }

    pub fn eat_doc_comment(&mut self) -> Option<DocComment> {
        let tok = self.tokens.get(self.pos)?;
        if tok.typ != TokenType::DocComment {
            return None;
        }
        self.pos += 1;
        let Some(Literal::DocComment(v)) = tok.literal else {
            unreachable!()
        };
        assert_ne!(
            self.tokens.get(self.pos).map(|v| v.typ),
            Some(TokenType::DocComment),
        );
        Some(v)
    }

    /// returns the token that would be returned by the next eat() call
    pub fn peek(&self) -> Token<'arena> {
        self.tokens().next().unwrap_or_else(|| self.eof_token())
    }

    // the token returned by the eat call before the current one
    pub fn last(&self) -> Token<'arena> {
        self.previous_tokens()
            .nth(1)
            .expect("there should always have been a token")
    }

    /// returns the token that would be returned by the next eat() call after a prior eat/dismiss
    /// call
    pub fn peek2(&self) -> Token<'arena> {
        self.tokens().nth(1).unwrap_or_else(|| self.eof_token())
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
        Ok(Ident::new(tok.string_literal(), tok.span))
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
        Ok((tok.string_literal(), tok.span))
    }

    /// Finishes parsing. Will error if there are any tokens remaining, unless there's only a
    /// single `Eof` token.
    pub fn finish(&mut self) -> Result<(), ParsingError<'arena>> {
        self.is_at_end()
            .then_some(())
            .ok_or(ParsingError::Expected {
                loc: self.tokens.get(0).unwrap().span,
                expected: TokenType::Eof,
                found: *self.tokens.get(0).unwrap(),
            })
    }
}

impl<'arena, Holder: MutableTokenHolder<'arena>> TokenStream<'arena, Holder> {
    /// pushes the token to the end of the tokenstream, maintaining the last token as eof.
    pub fn push_token(&mut self, tok: Token<'arena>) {
        assert_ne!(tok.typ, TokenType::Eof);
        self.tokens.push(tok);
        let len = self.tokens.len();
        if len > 1 && self.tokens.get(self.tokens.len() - 2).unwrap().typ == TokenType::Eof {
            self.tokens.swap(len - 1, len - 2);
        }
    }

    pub fn add_tokens<I: IntoIterator<Item = Token<'arena>>>(&mut self, tokens: I) {
        self.tokens.extend(tokens);
        assert!(
            !self
                .tokens
                .get(0..self.tokens.len().saturating_sub(1))
                .unwrap()
                .iter()
                .any(|v| v.typ == TokenType::Eof)
        );
    }
}
