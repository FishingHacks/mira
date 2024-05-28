use crate::tokenizer::{Token, TokenType};
pub use expression::{Expression, LiteralValue};
pub use statement::Statement;
mod expression;
mod statement;

pub struct Parser {
    pub tokens: Vec<Token>,
    pub current: usize,
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
                | TokenType::Impl
                | TokenType::Trait
                | TokenType::Let
                | TokenType::Return
                | TokenType::Const => break,
                _ => (),
            }

            self.advance();
        }
    }
}
