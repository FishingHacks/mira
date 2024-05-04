#[derive(Debug, Clone, Copy)]
pub struct Loc {
    pub file: GlobalString,
    pub line: usize,
    pub column: usize,
    pub length: usize,
}

macro_rules! loc {
    ($file:expr;$line:expr;$column:expr;$length:expr) => {
        Loc { file: $file, line: $line, column: $column, length: $length }
    };
    ($file:expr;$line:expr;$column:expr) => {
        $crate::tokens::Loc { file: $file, line: $line, column: $column, length: 0 }
    };
    ($line:expr;$column:expr;) => {
        Loc { file: "<built-in>".into(), line: $line, column: $column, length: 0 }
    };
}

impl Loc {
    pub fn new(file: GlobalString, line: usize, column: usize, length: usize) -> Self {
        Self { file, line, column, length }
    }
}

impl Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.file, f)?;
        f.write_fmt(format_args!(":{}:{}", self.line, self.column))
    }
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenType,
    pub loc: Loc,
}

impl Token {
    pub const fn new(kind: TokenType, loc: Loc) -> Self {
        Self { kind, loc }
    }
}

#[derive(Debug)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    SemiColon,
    Bang,
    Equal,
    Greater,
    Less,

    BangEqual,
    EqualEqual,
    GreaterEqual,
    LessEqual,

    Identifier(String),
    String(String),
    Integer(i32),
    UnsignedInteger(u32),
    Float(f32),

    If,
    Else,
    Fn,
    For,
    Null,
    False,
    True,
    Return,
    While,
    Let,
    Const,
    Eof,
}

use std::fmt::Display;

pub(crate) use loc;

use crate::globals::GlobalString;

#[derive(Debug, Clone, Copy)]
pub enum Number {
    I32(i32),
    U32(u32),
    F32(f32),
}
