use std::{
    cell::{Cell, RefCell},
    ops::{Deref, DerefMut},
    rc::Rc,
    sync::Arc,
};

use annotations::Annotations;
pub use error::{ParsingError, ProgramFormingError};
pub use expand::expand_tokens;
pub use expression::{ArrayLiteral, BinaryOp, Expression, LiteralValue, UnaryOp};
use mira_context::{DocComment, SharedCtx};
use mira_lexer::TokenTree;
use mira_spans::{BytePos, SourceFile, SpanData};
pub use statements::{
    Argument, BakableFunction, For, FunctionContract, If, Statement, Trait, While,
};
pub use statements::{TraitFunction, Variable};
pub use tokenstream::TokenStream;
pub use types::{Generic, RESERVED_TYPE_NAMES, TypeRef};

use crate::module::ModuleId;

pub mod annotations;
mod error;
mod expand;
mod expression;
mod path;
pub use path::{Path, PathWithoutGenerics};
pub mod module;
// mod statement;
mod statements;
pub mod std_annotations;
pub mod tokenstream;
mod types;

pub struct ParserData<'ctx> {
    pub ctx: SharedCtx<'ctx>,
    pub file: Arc<SourceFile>,
    pub key: ModuleId,

    current_annotations: RefCell<Annotations<'ctx>>,
    current_doc_comment: Cell<Option<DocComment>>,
}

pub struct Parser<'a, 'ctx> {
    data: Rc<ParserData<'ctx>>,
    stream: TokenStream<'a, 'ctx>,
}

impl std::fmt::Debug for Parser<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Parser")
            .field("file", &self.data.file.path)
            .field("root_directory", &self.data.file.package_root)
            .field("current_annotations", &self.data.current_annotations)
            .finish()
    }
}

impl<'a, 'ctx> Parser<'a, 'ctx> {
    pub fn from_token_tree(
        ctx: SharedCtx<'ctx>,
        tts: &'a [TokenTree<'ctx>],
        file: Arc<SourceFile>,
        key: ModuleId,
    ) -> Self {
        let eof_span = ctx.intern_span(SpanData::new(BytePos::from_u32(file.len()), 1, file.id));
        let file = file.clone();
        Parser::new(ctx, TokenStream::new(tts, eof_span), file, key)
    }

    pub fn new(
        ctx: SharedCtx<'ctx>,
        stream: TokenStream<'a, 'ctx>,
        file: Arc<SourceFile>,
        key: ModuleId,
    ) -> Self {
        let data = Rc::new(ParserData {
            ctx,
            file,
            key,
            current_annotations: Default::default(),
            current_doc_comment: Cell::new(None),
        });
        Self { stream, data }
    }

    pub fn subparser(&self, stream: TokenStream<'a, 'ctx>) -> Self {
        let data = self.data.clone();
        Self { data, stream }
    }

    pub fn take_doc_comment(&mut self) -> DocComment {
        let mut v = self.data.current_doc_comment.get();
        let ret = v.take().unwrap_or(DocComment::EMPTY);
        self.data.current_doc_comment.set(v);
        ret
    }

    pub fn ctx(&self) -> &SharedCtx<'ctx> {
        &self.data.ctx
    }

    pub fn file(&self) -> &Arc<SourceFile> {
        &self.data.file
    }

    pub fn key(&self) -> ModuleId {
        self.data.key
    }
}

impl<'a, 'ctx> Deref for Parser<'a, 'ctx> {
    type Target = TokenStream<'a, 'ctx>;

    fn deref(&self) -> &Self::Target {
        &self.stream
    }
}

impl DerefMut for Parser<'_, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.stream
    }
}
