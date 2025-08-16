use std::{
    ops::{Deref, DerefMut},
    sync::Arc,
};

use crate::{
    annotations::Annotations,
    context::SharedContext,
    module::Module,
    store::StoreKey,
    tokenstream::{BorrowedTokenStream, TokenStream},
};
pub use expand::expand_tokens;
pub use expression::{
    ArrayLiteral, BinaryOp, Expression, LiteralValue, Path, PathWithoutGenerics, UnaryOp,
};
use mira_lexer::{Token, TokenType};
use mira_spans::{BytePos, SourceFile, SpanData};
pub use statement::{Argument, BakableFunction, FunctionContract, Statement, Trait};
pub use types::{Generic, Implementation, RESERVED_TYPE_NAMES, Struct, TypeRef};

mod expand;
mod expression;
mod statement;
mod types;

pub struct Parser<'a, 'arena> {
    pub ctx: SharedContext<'arena>,
    pub file: Arc<SourceFile>,

    stream: BorrowedTokenStream<'arena, 'a>,
    current_annotations: Annotations<'arena>,
    /// all imports
    // pub imports: HashMap<Ident<'arena>, (Span<'arena>, Import<'arena>)>,
    pub key: StoreKey<Module<'arena>>,
}

impl std::fmt::Debug for Parser<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Parser")
            .field("file", &self.file.path)
            .field("root_directory", &self.file.package_root)
            .field("current_annotations", &self.current_annotations)
            // .field("imports", &self.imports)
            .finish()
    }
}

impl<'a, 'arena> Parser<'a, 'arena> {
    pub fn from_tokens(
        ctx: SharedContext<'arena>,
        tokens: &'a [Token<'arena>],
        file: Arc<SourceFile>,
        key: StoreKey<Module<'arena>>,
    ) -> Self {
        let eof_span = ctx.intern_span(SpanData::new(BytePos::from_u32(file.len()), 1, file.id));
        let file = file.clone();
        Parser::new(ctx, TokenStream::new(tokens, eof_span), file, key)
    }

    pub fn new(
        ctx: SharedContext<'arena>,
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

    // fn add_import(
    //     &mut self,
    //     ident: Ident<'arena>,
    //     span: Span<'arena>,
    //     import: Import<'arena>,
    // ) -> Result<(), ParsingError<'arena>> {
    //     if let Some((first, _)) = self.imports.get(&ident) {
    //         return Err(ParsingError::ItemAlreadyDefined {
    //             loc: ident.span(),
    //             name: ident.symbol(),
    //             first: *first,
    //         });
    //     }
    //     self.imports.insert(ident, (span, import));
    //     Ok(())
    // }

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
