use std::{
    collections::HashMap,
    fmt::{Display, Write},
    sync::{Arc, RwLock},
};

use crate::{
    globals::GlobalStr,
    tokenizer::{Location, Token, TokenType},
};
pub use expression::{Expression, LiteralValue, Path, PathWithoutGenerics};
pub use statement::{Argument, BakableFunction, FunctionContract, Statement, Trait};
pub use types::{Generic, Implementation, Struct, TypeRef, RESERVED_TYPE_NAMES};
mod expression;
mod module_resolution;
mod statement;
mod types;

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

#[derive(Clone, Debug)]
pub struct ParserQueueEntry {
    pub file: Arc<std::path::Path>,
    pub root: Arc<std::path::Path>,
}

#[derive(Debug)]
pub struct Parser {
    file: Arc<std::path::Path>,
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
}

impl Parser {
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

    fn check(&self, typ: TokenType) -> bool {
        if self.is_at_end() {
            return false;
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

    fn previous(&self) -> &Token {
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

    fn match_tok(&mut self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
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
            if self.previous().typ == TokenType::Semicolon {
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
