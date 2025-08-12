use parking_lot::RwLock;
use std::{
    collections::{HashMap, HashSet},
    ops::{Deref, DerefMut},
    sync::Arc,
};

use crate::{
    annotations::Annotations,
    context::SharedContext,
    error::ParsingError,
    module::{Import, Module},
    store::{Store, StoreKey},
    tokenstream::TokenStream,
};
pub use expression::{
    ArrayLiteral, BinaryOp, Expression, LiteralValue, Path, PathWithoutGenerics, UnaryOp,
};
use mira_lexer::{Lexer, TokenType};
use mira_spans::{BytePos, FileId, Ident, PackageId, SourceFile, Span, SpanData};
pub use statement::{Argument, BakableFunction, FunctionContract, Statement, Trait};
pub use types::{Generic, Implementation, RESERVED_TYPE_NAMES, Struct, TypeRef};
mod expression;
pub mod module_resolution;
mod statement;
mod types;

#[derive(Clone, Debug)]
pub struct ParserQueueEntry<'arena> {
    pub file: Arc<std::path::Path>,
    pub loaded_file: Option<FileId>,
    pub package: PackageId,
    pub reserved_key: StoreKey<Module<'arena>>,
}

pub struct Parser<'a, 'arena> {
    pub ctx: SharedContext<'arena>,
    pub file: Arc<SourceFile>,

    // pub tokens: Vec<Token<'arena>>,
    stream: TokenStream<'arena>,
    // pub current: usize,
    current_annotations: Annotations<'arena>,
    pub parser_queue: Arc<RwLock<Vec<ParserQueueEntry<'arena>>>>,
    pub modules: &'a RwLock<Store<Module<'arena>>>,
    /// all imports
    pub imports: HashMap<Ident<'arena>, (Span<'arena>, Import<'arena>)>,
    /// all the `pub _` exports
    pub exports: HashSet<Ident<'arena>>,
    pub key: StoreKey<Module<'arena>>,
}

impl std::fmt::Debug for Parser<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Parser")
            .field("file", &self.file.path)
            .field("root_directory", &self.file.package_root)
            // .field("tokens", &self.tokens)
            // .field("current", &self.current)
            .field("current_annotations", &self.current_annotations)
            .field("modules", &self.parser_queue)
            .field("imports", &self.imports)
            .finish()
    }
}

impl<'a, 'arena> Parser<'a, 'arena> {
    #[allow(clippy::too_many_arguments)]
    pub fn from_lexer(
        lexer: Lexer<'arena>,
        parser_queue: Arc<RwLock<Vec<ParserQueueEntry<'arena>>>>,
        modules: &'a RwLock<Store<Module<'arena>>>,
        key: StoreKey<Module<'arena>>,
        ctx: SharedContext<'arena>,
    ) -> Parser<'a, 'arena> {
        let eof_span = ctx.intern_span(SpanData::new(
            BytePos::from_u32(lexer.file.len()),
            1,
            lexer.file.id,
        ));
        let file = lexer.file.clone();
        Parser::new(
            ctx,
            TokenStream::new(lexer.into_tokens(), eof_span),
            parser_queue,
            modules,
            file,
            key,
        )
    }

    #[allow(clippy::too_many_arguments)]
    pub fn new(
        ctx: SharedContext<'arena>,
        // tokens: Vec<Token<'arena>>,
        stream: TokenStream<'arena>,
        parser_queue: Arc<RwLock<Vec<ParserQueueEntry<'arena>>>>,
        modules: &'a RwLock<Store<Module<'arena>>>,
        file: Arc<SourceFile>,
        key: StoreKey<Module<'arena>>,
    ) -> Self {
        Self {
            ctx,
            stream,
            // tokens,
            // current: 0,
            current_annotations: Default::default(),
            parser_queue,
            modules,
            file,
            key,
            imports: HashMap::new(),
            exports: HashSet::new(),
        }
    }

    fn add_export(&mut self, ident: Ident<'arena>) -> Result<(), ParsingError<'arena>> {
        if !self.exports.insert(ident) {
            return Err(ParsingError::ItemAlreadyDefined {
                loc: ident.span(),
                name: ident.symbol(),
                first: self.exports.get(&ident).unwrap().span(),
            });
        }
        Ok(())
    }

    fn add_import(
        &mut self,
        ident: Ident<'arena>,
        span: Span<'arena>,
        import: Import<'arena>,
    ) -> Result<(), ParsingError<'arena>> {
        if let Some((first, _)) = self.imports.get(&ident) {
            return Err(ParsingError::ItemAlreadyDefined {
                loc: ident.span(),
                name: ident.symbol(),
                first: *first,
            });
        }
        self.imports.insert(ident, (span, import));
        Ok(())
    }

    // gets to the next sensical expression/type boundary
    pub fn bail(&mut self) {
        self.dismiss();

        while !self.is_at_end() {
            if self.current().typ == TokenType::CurlyRight {
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

            self.dismiss();
        }
    }
}

impl<'arena> Deref for Parser<'_, 'arena> {
    type Target = TokenStream<'arena>;

    fn deref(&self) -> &Self::Target {
        &self.stream
    }
}

impl<'arena> DerefMut for Parser<'_, 'arena> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.stream
    }
}
