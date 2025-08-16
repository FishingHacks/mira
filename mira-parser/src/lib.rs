use std::{
    ops::{Deref, DerefMut},
    sync::Arc,
};

use annotations::Annotations;
pub use error::{ParsingError, ProgramFormingError};
pub use expand::expand_tokens;
pub use expression::{
    ArrayLiteral, BinaryOp, Expression, LiteralValue, Path, PathWithoutGenerics, UnaryOp,
};
use mira_common::store::StoreKey;
use mira_lexer::{Token, TokenType};
use mira_spans::{BytePos, SharedCtx, SourceFile, SpanData};
use module::Module;
pub use statement::{Argument, BakableFunction, FunctionContract, Statement, Trait};
use tokenstream::{BorrowedTokenStream, TokenStream};
pub use types::{Generic, Implementation, RESERVED_TYPE_NAMES, TypeRef};

pub mod annotations;
mod error;
mod expand;
mod expression;
pub mod module;
mod statement;
pub mod std_annotations;
pub mod tokenstream;
mod types;

pub struct Parser<'a, 'arena> {
    pub ctx: SharedCtx<'arena>,
    pub file: Arc<SourceFile>,

    stream: BorrowedTokenStream<'arena, 'a>,
    current_annotations: Annotations<'arena>,
    pub key: StoreKey<Module<'arena>>,
}

impl std::fmt::Debug for Parser<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Parser")
            .field("file", &self.file.path)
            .field("root_directory", &self.file.package_root)
            .field("current_annotations", &self.current_annotations)
            .finish()
    }
}

impl<'a, 'arena> Parser<'a, 'arena> {
    pub fn from_tokens(
        ctx: SharedCtx<'arena>,
        tokens: &'a [Token<'arena>],
        file: Arc<SourceFile>,
        key: StoreKey<Module<'arena>>,
    ) -> Self {
        let eof_span = ctx.intern_span(SpanData::new(BytePos::from_u32(file.len()), 1, file.id));
        let file = file.clone();
        Parser::new(ctx, TokenStream::new(tokens, eof_span), file, key)
    }

    pub fn new(
        ctx: SharedCtx<'arena>,
        stream: BorrowedTokenStream<'arena, 'a>,
        file: Arc<SourceFile>,
        key: StoreKey<Module<'arena>>,
    ) -> Self {
        Self {
            ctx,
            stream,
            // tokens,
            // current: 0,
            current_annotations: Default::default(),
            file,
            key,
            // imports: HashMap::new(),
        }
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

impl<'a, 'arena> Deref for Parser<'a, 'arena> {
    type Target = BorrowedTokenStream<'arena, 'a>;

    fn deref(&self) -> &Self::Target {
        &self.stream
    }
}

impl<'arena> DerefMut for Parser<'_, 'arena> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.stream
    }
}
