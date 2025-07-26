use parking_lot::RwLock;
use std::{collections::HashMap, sync::Arc};

use crate::{
    annotations::Annotations,
    context::SharedContext,
    error::ParsingError,
    interner::InternedStr,
    module::Module,
    store::{Store, StoreKey},
    tokenizer::{
        span::{SourceFile, SourceMap, Span},
        Token, TokenType,
    },
};
pub use expression::{
    ArrayLiteral, BinaryOp, Expression, LiteralValue, Path, PathWithoutGenerics, UnaryOp,
};
pub use statement::{Argument, BakableFunction, FunctionContract, Statement, Trait};
pub use types::{Generic, Implementation, Struct, TypeRef, RESERVED_TYPE_NAMES};
mod expression;
pub mod module_resolution;
mod statement;
mod types;

#[derive(Clone, Debug)]
pub struct ParserQueueEntry<'arena> {
    pub file: Arc<std::path::Path>,
    pub root_dir: Arc<std::path::Path>,
    pub reserved_key: StoreKey<Module<'arena>>,
}

pub struct Parser<'a, 'arena> {
    pub ctx: SharedContext<'arena>,
    pub file: Arc<SourceFile>,

    pub tokens: Vec<Token<'arena>>,
    pub current: usize,
    current_annotations: Annotations<'arena>,
    pub parser_queue: Arc<RwLock<Vec<ParserQueueEntry<'arena>>>>,
    pub modules: &'a RwLock<Store<Module<'arena>>>,
    /// a map of idents => imports. if the size of the vec is 0, the identifier refers to the
    /// module itself. otherwise, it refers to something in it.
    pub imports: HashMap<
        InternedStr<'arena>,
        (
            Span<'arena>,
            StoreKey<Module<'arena>>,
            Vec<InternedStr<'arena>>,
        ),
    >,
    source_map: &'a SourceMap,
    pub key: StoreKey<Module<'arena>>,
}

impl std::fmt::Debug for Parser<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Parser")
            .field("file", &self.file.path)
            .field("root_directory", &self.file.package_root)
            .field("tokens", &self.tokens)
            .field("current", &self.current)
            .field("current_annotations", &self.current_annotations)
            .field("modules", &self.parser_queue)
            .field("imports", &self.imports)
            .finish()
    }
}

impl<'a, 'arena> Parser<'a, 'arena> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        ctx: SharedContext<'arena>,
        tokens: Vec<Token<'arena>>,
        parser_queue: Arc<RwLock<Vec<ParserQueueEntry<'arena>>>>,
        modules: &'a RwLock<Store<Module<'arena>>>,
        file: Arc<SourceFile>,
        source_map: &'a SourceMap,
        key: StoreKey<Module<'arena>>,
    ) -> Self {
        Self {
            ctx,
            tokens,
            current: 0,
            current_annotations: Default::default(),
            imports: HashMap::new(),
            parser_queue,
            modules,
            file,
            source_map,
            key,
        }
    }

    pub fn add_tokens<I: IntoIterator<Item = Token<'arena>>>(&mut self, tokens: I) {
        self.tokens.extend(tokens);
    }

    pub fn is_at_end(&self) -> bool {
        if self.current >= self.tokens.len() - 1 {
            return true;
        }
        assert_ne!(
            self.tokens[self.current].typ,
            TokenType::Eof,
            "TokenType::Eof inside file"
        );
        false
    }

    fn peek(&self) -> &Token<'arena> {
        if self.is_at_end() {
            return &self.tokens[self.tokens.len() - 1]; // eof
        }

        &self.tokens[self.current]
    }

    fn peekpeek(&self) -> &Token<'arena> {
        self.tokens
            .get(self.current + 1)
            .unwrap_or(&self.tokens[self.tokens.len() - 1])
    }

    fn check(&self, typ: TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek().typ == typ
        }
    }

    fn advance(&mut self) -> &Token<'arena> {
        if !self.is_at_end() {
            self.current += 1;
        }
        &self.tokens[self.current - 1]
    }

    fn last(&self) -> &Token<'arena> {
        if self.current < 2 {
            &self.tokens[0]
        } else {
            &self.tokens[self.current - 2]
        }
    }

    fn current(&self) -> &Token<'arena> {
        if self.current < 1 {
            &self.tokens[0]
        } else {
            &self.tokens[self.current - 1]
        }
    }

    fn matches(&mut self, types: &[TokenType]) -> bool {
        if self.is_at_end() {
            return false;
        }
        for typ in types {
            if self.check(*typ) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn expect_tok(
        &mut self,
        token_type: TokenType,
    ) -> Result<&Token<'arena>, ParsingError<'arena>> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            Err(ParsingError::ExpectedArbitrary {
                loc: self.peek().span,
                expected: token_type,
                found: self.peek().typ,
            })
        }
    }

    fn match_tok(&mut self, token_type: TokenType) -> bool {
        if self.check(token_type) {
            self.advance();
            return true;
        }

        false
    }

    // gets to the next sensical expression/type boundary
    pub fn bail(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.current().typ == TokenType::Semicolon {
                break;
            }

            match self.peek().typ {
                TokenType::Struct
                | TokenType::Fn
                | TokenType::If
                | TokenType::While
                | TokenType::For
                | TokenType::Trait
                | TokenType::Let
                | TokenType::Return => break,
                _ => (),
            }

            self.advance();
        }
    }
}
