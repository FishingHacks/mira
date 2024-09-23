use std::{fmt::{Display, Write}, rc::Rc};

use crate::tokenizer::{Location, Token, TokenType};
pub use expression::{Expression, LiteralValue, Path};
use statement::Annotations;
pub use statement::{Statement, FunctionContract, Argument, BakableFunction};
pub use types::{RESERVED_TYPE_NAMES, Type, TypeRef, Struct, Implementation};
mod expression;
mod statement;
mod types;

#[derive(Debug, Clone)]
pub struct Annotation {
    loc: Location,
    name: Rc<str>,
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

pub struct Parser {
    pub tokens: Vec<Token>,
    pub current: usize,
    current_annotations: Annotations,
}

impl Parser {
    pub fn add_tokens<I: IntoIterator<Item = Token>>(&mut self, tokens: I) {
        self.tokens.extend(tokens);
    }

    pub fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
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
