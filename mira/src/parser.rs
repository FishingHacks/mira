use module_resolution::ModuleResolver;
use parking_lot::RwLock;
use std::{collections::HashMap, sync::Arc};

use crate::{
    annotations::Annotations,
    error::ParsingError,
    globals::GlobalStr,
    tokenizer::{Location, Token, TokenType},
};
pub use expression::{BinaryOp, Expression, LiteralValue, Path, PathWithoutGenerics, UnaryOp};
pub use statement::{Argument, BakableFunction, FunctionContract, Statement, Trait};
pub use types::{Generic, Implementation, Struct, TypeRef, RESERVED_TYPE_NAMES};
mod expression;
pub mod module_resolution;
mod statement;
mod types;

/*
#[derive(Default, Debug, Clone)]
pub struct Annotations(pub Vec<Annotation>);

impl Display for Annotations {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for annotation in self.0.iter() {
            f.write_char('@')?;
            Display::fmt(&annotation.name, f)?;
            f.write_char('(')?;
            for i in 0..annotation.args.len() {
                if i != 0 {
                    f.write_char(' ')?;
                }
                Display::fmt(&annotation.args[i], f)?;
            }
            f.write_str(")\n")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Annotation {
    loc: Location,
    name: GlobalStr,
    args: Vec<Token>,
}

impl Display for Annotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('@')?;
        Display::fmt(&self.name, f)?;
        f.write_char('(')?;
        for arg in self.args.iter() {
            Display::fmt(arg, f)?;
        }
        f.write_char(')')
    }
}
*/

#[derive(Clone, Debug)]
pub struct ParserQueueEntry {
    pub file: Arc<std::path::Path>,
    pub root_dir: Arc<std::path::Path>,
}

pub struct Parser {
    pub file: Arc<std::path::Path>,
    root_directory: Arc<std::path::Path>,

    pub tokens: Vec<Token>,
    pub current: usize,
    current_annotations: Annotations,
    /// The modules, the index is their id. The second boolean dictates if they already started
    /// parsing. there's no reference to whether or not they finished. it is assumed they all
    /// finished when typechecking.
    pub modules: Arc<RwLock<Vec<ParserQueueEntry>>>,
    /// a map of idents => imports. if the size of the vec is 0, the identifier refers to the
    /// module itself. otherwise, it refers to something in it.
    pub imports: HashMap<GlobalStr, (Location, usize, Vec<GlobalStr>)>,
    resolvers: Arc<[Box<dyn ModuleResolver>]>,
    path_exists: Arc<dyn Fn(&std::path::Path) -> bool>,
    path_is_dir: Arc<dyn Fn(&std::path::Path) -> bool>,
}

impl std::fmt::Debug for Parser {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Parser")
            .field("file", &self.file)
            .field("root_directory", &self.root_directory)
            .field("tokens", &self.tokens)
            .field("current", &self.current)
            .field("current_annotations", &self.current_annotations)
            .field("modules", &self.modules)
            .field("imports", &self.imports)
            .finish()
    }
}

impl Parser {
    pub fn new(
        tokens: Vec<Token>,
        modules: Arc<RwLock<Vec<ParserQueueEntry>>>,
        file: Arc<std::path::Path>,
        root_directory: Arc<std::path::Path>,
        resolvers: Arc<[Box<dyn ModuleResolver>]>,
        path_exists: Arc<dyn Fn(&std::path::Path) -> bool>,
        path_is_dir: Arc<dyn Fn(&std::path::Path) -> bool>,
    ) -> Self {
        Self {
            tokens,
            current: 0,
            current_annotations: Default::default(),
            imports: HashMap::new(),
            modules,
            file,
            root_directory,
            resolvers,
            path_exists,
            path_is_dir,
        }
    }

    pub fn add_tokens<I: IntoIterator<Item = Token>>(&mut self, tokens: I) {
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

    fn peek(&self) -> &Token {
        if self.is_at_end() {
            return &self.tokens[self.tokens.len() - 1]; // eof
        }

        &self.tokens[self.current]
    }

    fn peekpeek(&self) -> &Token {
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

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        &self.tokens[self.current - 1]
    }

    fn current(&self) -> &Token {
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

    fn expect_tok(&mut self, token_type: TokenType) -> Result<&Token, ParsingError> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            Err(ParsingError::ExpectedArbitrary {
                loc: self.peek().location.clone(),
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
