use parking_lot::RwLock;
use std::{
    fmt::{Display, Write},
    path::Path,
    str::FromStr,
    sync::Arc,
};

use crate::{
    error::TokenizationError,
    globals::GlobalStr,
    parser::{LiteralValue, Parser, ParserQueueEntry},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenType {
    Let,                  // done, done
    EqualEqual,           // done, done
    NotEquals,            // done, done
    LessThan,             // done, done
    GreaterThan,          // done, done
    LessThanEquals,       // done, done
    GreaterThanEquals,    // done, done
    LogicalNot,           // done, done
    LogicalAnd,           // done, done
    LogicalOr,            // done, done
    StringLiteral,        // done, done
    FloatLiteral,         // done, done
    SIntLiteral,          // done, done
    UIntLiteral,          // done, done
    BooleanLiteral,       // done, done
    VoidLiteral,          // done, done
    IdentifierLiteral,    // done, done
    Equal,                // done, done
    Colon,                // done, done
    Semicolon,            // done, done
    ParenLeft,            // done, done
    ParenRight,           // done, done
    CurlyLeft,            // done, done
    CurlyRight,           // done, done
    BracketLeft,          // done, done
    BracketRight,         // done, done
    Plus,                 // done, done
    Minus,                // done, done
    Asterix,              // done, done
    Divide,               // done, done
    Modulo,               // done, done
    ModuloAssign,         // done, done
    BitwiseNot,           // done, done
    Ampersand,            // done, done
    BitwiseOr,            // done, done
    BitwiseXor,           // done, done
    LShift,               // done, done
    RShift,               // done, done
    PipeOperator,         // done, done
    Return,               // done, done
    Fn,                   // done, done
    Extern,               // done, done
    Use,                  // done, done
    Export,               // done, done
    If,                   // done, done
    Else,                 // done, done
    While,                // done, done
    For,                  // done, done
    In,                   // done, done
    Range,                // done, done
    RangeInclusive,       // done, done
    ReturnType,           // done, done
    Struct,               // done, done
    Trait,                // done, done
    Impl,                 // done, done
    Comma,                // done, done
    PlusAssign,           // done, done
    MinusAssign,          // done, done
    DivideAssign,         // done, done
    MultiplyAssign,       // done, done
    BitwiseAndAssign,     // done, done
    BitwiseOrAssign,      // done, done
    BitwiseXorAssign,     // done, done
    BitwiseLShiftAssign,  // done, done
    BitwiseRShiftAssign,  // done, done
    Dot,                  // done, done
    As,                   // done, done
    QuestionMark,         // done, done
    AnnotationIntroducer, // done, done
    NamespaceAccess,      // done, done
    Eof,                  // done, done
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumberType {
    F32,
    F64,
    I8,
    I16,
    I32,
    I64,
    Isize,
    U8,
    U16,
    U32,
    U64,
    Usize,
    None,
}

impl Display for NumberType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::F32 => f.write_str("f32"),
            Self::F64 => f.write_str("f64"),
            Self::I8 => f.write_str("i8"),
            Self::I16 => f.write_str("i16"),
            Self::I32 => f.write_str("i32"),
            Self::I64 => f.write_str("i64"),
            Self::U8 => f.write_str("u8"),
            Self::U16 => f.write_str("u16"),
            Self::U32 => f.write_str("u32"),
            Self::U64 => f.write_str("u64"),
            Self::Usize => f.write_str("usize"),
            Self::Isize => f.write_str("isize"),
            Self::None => Ok(()),
        }
    }
}

impl FromStr for NumberType {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "f32" => Ok(Self::F32),
            "f64" => Ok(Self::F64),
            "i8" => Ok(Self::I8),
            "i16" => Ok(Self::I16),
            "i32" => Ok(Self::I32),
            "i64" => Ok(Self::I64),
            "u8" => Ok(Self::U8),
            "u16" => Ok(Self::U16),
            "u32" => Ok(Self::U32),
            "u64" => Ok(Self::U64),
            "usize" => Ok(Self::Usize),
            "isize" => Ok(Self::Isize),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Literal {
    Float(f64, NumberType),
    SInt(i64, NumberType),
    UInt(u64, NumberType),
    String(GlobalStr),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location {
    pub line: u32,
    pub column: u32,
    pub file: Arc<Path>,
}

impl Location {
    pub fn new(file: Arc<Path>, line: u32, column: u32) -> Self {
        Self { column, file, line }
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.file.display(), f)?;
        f.write_char(':')?;
        Display::fmt(&self.line, f)?;
        f.write_char(':')?;
        Display::fmt(&self.column, f)
    }
}

#[macro_export]
macro_rules! loc {
    ($file:expr;$line:expr) => {
        $crate::tokenizer::Location::new($file.clone(), $line, 0)
    };
    ($file:expr;$line:expr;$column:expr) => {
        $crate::tokenizer::Location::new($file.clone(), $line, $column)
    };
}
pub(crate) use loc;

#[derive(Clone, Debug)]
pub struct Token {
    pub typ: TokenType,
    pub literal: Option<Literal>,
    pub location: Location,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.typ {
            TokenType::As => f.write_str("as"),
            TokenType::Colon => f.write_str(":"),
            TokenType::Equal => f.write_str("="),
            TokenType::BitwiseAndAssign => f.write_str("&="),
            TokenType::Ampersand => f.write_str("&"),
            TokenType::BitwiseNot => f.write_str("~"),
            TokenType::BitwiseOr => f.write_str("|"),
            TokenType::BitwiseOrAssign => f.write_str("|="),
            TokenType::BitwiseXor => f.write_str("^"),
            TokenType::BitwiseXorAssign => f.write_str("^="),
            TokenType::LShift => f.write_str("<<"),
            TokenType::BitwiseLShiftAssign => f.write_str("<<="),
            TokenType::RShift => f.write_str(">>"),
            TokenType::BitwiseRShiftAssign => f.write_str(">>="),
            TokenType::VoidLiteral => f.write_str("void"),
            TokenType::BooleanLiteral => match &self.literal {
                Some(Literal::Bool(v)) => f.write_fmt(format_args!("bool({v})")),
                _ => f.write_str("bool(malformed data)"),
            },
            TokenType::BracketLeft => f.write_str("["),
            TokenType::BracketRight => f.write_str("]"),
            TokenType::Comma => f.write_str(","),
            TokenType::CurlyLeft => f.write_str("{"),
            TokenType::CurlyRight => f.write_str("}"),
            TokenType::Divide => f.write_str("/"),
            TokenType::DivideAssign => f.write_str("/="),
            TokenType::Dot => f.write_str("."),
            TokenType::Else => f.write_str("else"),
            TokenType::Eof => f.write_str("<EOF>"),
            TokenType::EqualEqual => f.write_str("=="),
            TokenType::Extern => f.write_str("extern"),
            TokenType::Fn => f.write_str("fn"),
            TokenType::For => f.write_str("for"),
            TokenType::GreaterThan => f.write_str(">"),
            TokenType::GreaterThanEquals => f.write_str(">="),
            TokenType::IdentifierLiteral => match &self.literal {
                Some(Literal::String(v)) => f.write_fmt(format_args!("identifier({v})")),
                _ => f.write_str("identifier(malformed data)"),
            },
            TokenType::Use => f.write_str("use"),
            TokenType::Export => f.write_str("export"),
            TokenType::If => f.write_str("if"),
            TokenType::Impl => f.write_str("impl"),
            TokenType::In => f.write_str("in"),
            TokenType::LessThan => f.write_str("<"),
            TokenType::LessThanEquals => f.write_str("<="),
            TokenType::Let => f.write_str("let"),
            TokenType::LogicalAnd => f.write_str("&&"),
            TokenType::LogicalNot => f.write_str("!"),
            TokenType::LogicalOr => f.write_str("||"),
            TokenType::Minus => f.write_str("-"),
            TokenType::MinusAssign => f.write_str("-="),
            TokenType::Modulo => f.write_str("%"),
            TokenType::ModuloAssign => f.write_str("%="),
            TokenType::MultiplyAssign => f.write_str("*="),
            TokenType::Asterix => f.write_str("*"),
            TokenType::NotEquals => f.write_str("!="),
            TokenType::AnnotationIntroducer => f.write_str("@"),
            TokenType::NamespaceAccess => f.write_str("::"),
            TokenType::FloatLiteral => match self.literal {
                Some(Literal::Float(v, typ)) => f.write_fmt(format_args!("float({v}{typ})")),
                _ => f.write_str("float(malformed data)"),
            },
            TokenType::SIntLiteral => match self.literal {
                Some(Literal::SInt(v, typ)) => f.write_fmt(format_args!("int({v}{typ})")),
                _ => f.write_str("int(malformed data)"),
            },
            TokenType::UIntLiteral => match self.literal {
                Some(Literal::UInt(v, typ)) => f.write_fmt(format_args!("uint({v}{typ})")),
                _ => f.write_str("uint(malformed data)"),
            },
            TokenType::ParenLeft => f.write_str("("),
            TokenType::ParenRight => f.write_str(")"),
            TokenType::PipeOperator => f.write_str("|>"),
            TokenType::Plus => f.write_str("+"),
            TokenType::PlusAssign => f.write_str("+="),
            TokenType::QuestionMark => f.write_str("?"),
            TokenType::Range => f.write_str(".."),
            TokenType::RangeInclusive => f.write_str("..="),
            TokenType::Return => f.write_str("return"),
            TokenType::ReturnType => f.write_str("->"),
            TokenType::Semicolon => f.write_str(";"),
            TokenType::StringLiteral => match &self.literal {
                Some(Literal::String(v)) => f.write_fmt(format_args!("string({v:?})")),
                _ => f.write_str("string(malformed data)"),
            },
            TokenType::Struct => f.write_str("struct"),
            TokenType::Trait => f.write_str("trait"),
            TokenType::While => f.write_str("while"),
        }
        //f.write_str(" at ")?;
        //Display::fmt(&self.location, f)
    }
}

impl Token {
    pub fn new(
        typ: TokenType,
        literal: Option<Literal>,
        line_number: u32,
        column: u32,
        file: Arc<Path>,
    ) -> Self {
        Self {
            typ,
            location: loc!(file;line_number;column),
            literal,
        }
    }

    pub fn to_literal_value(&self) -> Option<LiteralValue> {
        match self.typ {
            TokenType::StringLiteral
            | TokenType::BooleanLiteral
            | TokenType::FloatLiteral
            | TokenType::UIntLiteral
            | TokenType::SIntLiteral => self.literal.as_ref().map(|v| match v {
                Literal::Bool(boolean) => LiteralValue::Bool(*boolean),
                Literal::Float(float, typ) => LiteralValue::Float(*float, *typ),
                Literal::SInt(int, typ) => LiteralValue::SInt(*int, *typ),
                Literal::UInt(uint, typ) => LiteralValue::UInt(*uint, *typ),
                Literal::String(string) => LiteralValue::String(string.clone()),
            }),
            TokenType::VoidLiteral => Some(LiteralValue::Void),
            TokenType::IdentifierLiteral => {
                if let Some(lit) = &self.literal {
                    return match &lit {
                        Literal::String(v) => Some(LiteralValue::Dynamic(
                            crate::parser::Path::new(v.clone(), Vec::new()),
                        )),
                        _ => None,
                    };
                } else {
                    return None;
                }
            }
            _ => None,
        }
    }
}

pub struct Tokenizer {
    source: Vec<char>,
    pub file: Arc<Path>,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: u32,
    column: u32,
}

impl Tokenizer {
    pub fn new(source: &str, file: Arc<Path>) -> Self {
        Self {
            source: source.chars().collect(),
            file: file.into(),
            start: 0,
            current: 0,
            tokens: vec![],
            line: 0,
            column: 0,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<(), Vec<TokenizationError>> {
        let mut errors = vec![];
        while !self.is_at_end() {
            self.start = self.current;
            if let Err(e) = self.scan_token() {
                errors.push(e);
            }
        }

        self.tokens.push(Token {
            typ: TokenType::Eof,
            literal: None,
            location: loc!(self.file;self.line + 1),
        });

        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(())
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        self.column += 1;
        self.current += 1;
        if self.current > 1 && self.source[self.current - 2] == '\n' {
            self.line += 1;
            self.column = 0;
        }
        self.source[self.current - 1]
    }

    fn cur_char(&self) -> char {
        self.source[self.current.saturating_sub(1)]
    }

    fn peek(&self) -> char {
        self.source.get(self.current).copied().unwrap_or('\0')
    }

    fn if_char_advance(&mut self, character: char) -> bool {
        if self.peek() != character {
            return false;
        }

        self.advance();
        true
    }

    fn scan_token(&mut self) -> Result<(), TokenizationError> {
        let tok = self.int_scan_token()?;
        let Some(tok) = tok else { return Ok(()) };
        match tok.typ {
            TokenType::IdentifierLiteral if self.if_char_advance('!') => match &tok.literal {
                Some(Literal::String(str)) => {
                    let mut tokens = self.do_macro(&tok.location, str)?;
                    self.tokens.append(&mut tokens);
                }
                _ => unreachable!(
                    "Token::IdentifierLiteral should always have a string literal value"
                ),
            },
            _ => self.tokens.push(tok),
        }
        Ok(())
    }

    fn int_scan_token(&mut self) -> Result<Option<Token>, TokenizationError> {
        let c = self.advance();

        macro_rules! token {
            ($token: ident) => {
                Ok(self.get_token(TokenType::$token))
            };

            ($tokena: ident, $tokenb: ident, $char: expr) => {{
                if self.if_char_advance($char) {
                    Ok(self.get_token(TokenType::$tokenb))
                } else {
                    Ok(self.get_token(TokenType::$tokena))
                }
            }};
        }

        match c {
            '?' => token!(QuestionMark),
            '(' => token!(ParenLeft),
            ')' => token!(ParenRight),
            '{' => token!(CurlyLeft),
            '}' => token!(CurlyRight),
            '[' => token!(BracketLeft),
            ']' => token!(BracketRight),
            ',' => token!(Comma),
            '.' if self.if_char_advance('.') => token!(Range, RangeInclusive, '.'),
            '.' if self.peek().is_ascii_digit() => self.parse_number('.'),
            '.' => token!(Dot),
            '+' => token!(Plus, PlusAssign, '='),
            '-' if self.peek().is_ascii_digit() || self.peek() == '.' => self.parse_number('-'),
            '-' if self.if_char_advance('=') => token!(MinusAssign),
            '-' if self.if_char_advance('>') => token!(ReturnType),
            '-' => token!(Minus),
            '/' if self.peek() != '/' => token!(Divide, DivideAssign, '='),
            '%' => token!(Modulo, ModuloAssign, '='),
            '*' => token!(Asterix, MultiplyAssign, '='),
            '=' => token!(Equal, EqualEqual, '='),
            '<' if self.peek() != '<' => token!(LessThan, LessThanEquals, '='),
            '<' if self.if_char_advance('<') => token!(LShift, BitwiseLShiftAssign, '='),
            '>' if self.peek() != '>' => token!(GreaterThan, GreaterThanEquals, '='),
            '>' if self.if_char_advance('>') => token!(RShift, BitwiseRShiftAssign, '='),
            ':' => token!(Colon, NamespaceAccess, ':'),
            ';' => token!(Semicolon),
            '!' => token!(LogicalNot, NotEquals, '='),
            '~' => token!(BitwiseNot),
            '&' if self.if_char_advance('=') => token!(BitwiseAndAssign),
            '&' if self.if_char_advance('&') => token!(LogicalAnd),
            '&' => token!(Ampersand),
            '|' if self.if_char_advance('=') => token!(BitwiseOrAssign),
            '|' if self.if_char_advance('|') => token!(LogicalOr),
            '|' if self.if_char_advance('>') => token!(PipeOperator),
            '|' => token!(BitwiseOr),
            '^' => token!(BitwiseXor, BitwiseXorAssign, '='),
            ' ' | '\n' | '\r' | '\t' => {
                while matches!(self.peek(), ' ' | '\n' | '\r' | '\t') {
                    self.advance();
                }
                return Ok(None);
            }
            '/' if self.if_char_advance('/') => {
                while !self.is_at_end() && self.advance() != '\n' {}
                return Ok(None);
            }
            '@' => token!(AnnotationIntroducer),
            ('0'..='9') => self.parse_number(c),
            '"' => self.parse_string('"'),
            '`' => {
                let mut tok = self.parse_string('`')?;
                tok.typ = TokenType::IdentifierLiteral;
                Ok(tok)
            }
            _ if Self::is_valid_identifier_char(c) && !matches!(c, ('0'..='9')) => {
                self.parse_identifier(c)
            }
            _ => {
                return Err(TokenizationError::unknown_token(
                    loc!(self.file;self.line),
                    c,
                ))
            }
        }
        .map(Some)
    }

    #[inline(always)]
    fn get_token(&self, token: TokenType) -> Token {
        Token::new(token, None, self.line, self.column, self.file.clone())
    }

    fn skip_to_after_number(&mut self) {
        loop {
            match self.peek() {
                '0'..='9' => _ = self.advance(),
                '.' if matches!(
                    self.source.get(self.current + 1).copied().unwrap_or('\0'),
                    ('0'..='9')
                ) =>
                {
                    _ = self.advance()
                }
                '.' => break,
                _ => break,
            }
        }
    }

    fn parse_numtype(
        &mut self,
        loc: Location,
        first_char: char,
        value: u64,
        is_negative: bool,
        allow_float: bool,
    ) -> Result<Token, TokenizationError> {
        let mut typ = String::from(first_char);

        loop {
            match self.peek() {
                'a'..='z' | '0'..='9' => typ.push(self.advance()),
                _ => {
                    let err = TokenizationError::InvalidNumberType(
                        loc!(self.file;self.line;self.column - typ.len() as u32),
                    );
                    let Ok(number_type) = NumberType::from_str(&typ) else {
                        return Err(err);
                    };
                    return match number_type {
                        NumberType::F32 | NumberType::F64 if !allow_float => Err(err),
                        NumberType::F32 | NumberType::F64 if is_negative => Ok(self
                            .get_token_lit_loc(
                                TokenType::FloatLiteral,
                                Literal::Float(-(value as f64), number_type),
                                loc,
                            )),
                        NumberType::F32 | NumberType::F64 => Ok(self.get_token_lit_loc(
                            TokenType::FloatLiteral,
                            Literal::Float(value as f64, number_type),
                            loc,
                        )),
                        NumberType::U8
                        | NumberType::U16
                        | NumberType::U32
                        | NumberType::U64
                        | NumberType::Usize
                            if is_negative =>
                        {
                            Err(err)
                        }
                        _ if is_negative => Ok(self.get_token_lit_loc(
                            TokenType::SIntLiteral,
                            Literal::SInt(-(value as i64), number_type),
                            loc,
                        )),
                        _ => Ok(self.get_token_lit_loc(
                            TokenType::SIntLiteral,
                            Literal::UInt(value, number_type),
                            loc,
                        )),
                    };
                }
            }
        }
    }

    fn parse_hex(
        &mut self,
        location: Location,
        is_negative: bool,
    ) -> Result<Token, TokenizationError> {
        let mut value: u64 = 0;

        loop {
            match self.peek() {
                '0'..='9' => value = (value << 4) | self.advance() as u64 - '0' as u64,
                'a'..='f' => value = (value << 4) | self.advance() as u64 - 'a' as u64 + 0xa,
                'A'..='F' => value = (value << 4) | self.advance() as u64 - 'A' as u64 + 0xa,
                '.' if matches!(
                    self.source
                        .get(self.current + 1)
                        .map(|c| *c)
                        .unwrap_or('\0'),
                    '0'..='9' | 'a'..='f' | 'A'..='F'
                ) =>
                {
                    self.advance();
                    loop {
                        match self.peek() {
                            '0'..='9' => _ = self.advance(),
                            '.' => break,
                            _ => break,
                        }
                    }
                    return Err(TokenizationError::InvalidNumberError {
                        loc: loc!(self.file;self.line;self.column),
                    });
                }
                c @ ('u' | 'i') => {
                    return self.parse_numtype(location, c, value, is_negative, false)
                }
                _ if is_negative => {
                    return Ok(self.get_token_lit_loc(
                        TokenType::SIntLiteral,
                        Literal::SInt(-(value as i64), NumberType::None),
                        location,
                    ))
                }
                _ => {
                    return Ok(self.get_token_lit_loc(
                        TokenType::UIntLiteral,
                        Literal::UInt(value, NumberType::None),
                        location,
                    ))
                }
            }
        }
    }

    fn parse_bin(
        &mut self,
        location: Location,
        is_negative: bool,
    ) -> Result<Token, TokenizationError> {
        let mut value: u64 = 0;

        loop {
            match self.peek() {
                '0' | '1' => value = (value << 1) | self.advance() as u64 - '0' as u64,
                '2'..='9' => {
                    self.advance();
                    return Err(TokenizationError::InvalidNumberError {
                        loc: loc!(self.file;self.line;self.column),
                    });
                }
                '.' if matches!(
                    self.source
                        .get(self.current + 1)
                        .map(|c| *c)
                        .unwrap_or('\0'),
                    ('0'..='9')
                ) =>
                {
                    self.advance();
                    self.skip_to_after_number();
                    return Err(TokenizationError::InvalidNumberError {
                        loc: loc!(self.file;self.line;self.column),
                    });
                }
                c @ ('u' | 'i') => {
                    return self.parse_numtype(location, c, value, is_negative, false)
                }
                _ if is_negative => {
                    return Ok(self.get_token_lit_loc(
                        TokenType::SIntLiteral,
                        Literal::SInt(-(value as i64), NumberType::None),
                        location,
                    ))
                }
                _ => {
                    return Ok(self.get_token_lit_loc(
                        TokenType::UIntLiteral,
                        Literal::UInt(value, NumberType::None),
                        location,
                    ))
                }
            }
        }
    }

    fn parse_oct(
        &mut self,
        location: Location,
        is_negative: bool,
    ) -> Result<Token, TokenizationError> {
        let mut value: u64 = 0;

        loop {
            match self.peek() {
                '0'..='7' => value = (value << 3) | self.advance() as u64 - '0' as u64,
                '8' | '9' => {
                    self.advance();
                    return Err(TokenizationError::InvalidNumberError {
                        loc: loc!(self.file;self.line;self.column),
                    });
                }
                '.' if matches!(
                    self.source
                        .get(self.current + 1)
                        .map(|c| *c)
                        .unwrap_or('\0'),
                    ('0'..='9')
                ) =>
                {
                    self.advance();
                    self.skip_to_after_number();
                    return Err(TokenizationError::InvalidNumberError {
                        loc: loc!(self.file;self.line;self.column),
                    });
                }
                c @ ('i' | 'u') => {
                    return self.parse_numtype(location, c, value, is_negative, false)
                }
                _ if is_negative => {
                    return Ok(self.get_token_lit_loc(
                        TokenType::SIntLiteral,
                        Literal::SInt(-(value as i64), NumberType::None),
                        location,
                    ))
                }
                _ => {
                    return Ok(self.get_token_lit_loc(
                        TokenType::UIntLiteral,
                        Literal::UInt(value, NumberType::None),
                        location,
                    ))
                }
            }
        }
    }

    fn parse_dec(str: &str) -> u64 {
        let mut value = 0u64;

        for char in str.chars() {
            match char {
                '0'..='9' => {
                    value *= 10;
                    value += char as u64 - '0' as u64;
                }
                _ => unreachable!(),
            }
        }

        value
    }

    fn parse_number(&mut self, mut first_char: char) -> Result<Token, TokenizationError> {
        let loc = loc!(self.file;self.line;self.column);
        let is_negative = first_char == '-';
        let mut is_float = false;
        if is_negative {
            first_char = self.advance();
        }

        if first_char == '0' && self.peek() == 'x' {
            self.advance();
            if !matches!(self.peek(), '0'..='9' | 'a'..='f' | 'A'..='F') {
                self.advance();
                return Err(TokenizationError::InvalidNumberError {
                    loc: loc!(self.file;self.line;self.column),
                });
            }
            return self.parse_hex(loc, is_negative);
        } else if first_char == '0' && self.peek() == 'b' {
            self.advance();
            if !matches!(self.peek(), '0' | '1') {
                self.advance();
                return Err(TokenizationError::InvalidNumberError {
                    loc: loc!(self.file;self.line;self.column),
                });
            }
            return self.parse_bin(loc, is_negative);
        } else if first_char == '0' && self.peek() == 'o' {
            self.advance();
            if !matches!(self.peek(), '0'..='7') {
                self.advance();
                return Err(TokenizationError::InvalidNumberError {
                    loc: loc!(self.file;self.line;self.column),
                });
            }
            return self.parse_oct(loc, is_negative);
        }

        let mut str = String::new();
        let mut typ = String::new();

        if is_negative {
            str.push('-');
        }
        if first_char == '.' {
            str.push('0');
            is_float = true;
        }
        str.push(first_char);

        while !self.is_at_end() {
            if typ.len() > 0 {
                if matches!(self.peek(), 'a'..='z' | '0'..='9') {
                    typ.push(self.advance());
                    continue;
                }
                break;
            }

            if matches!(self.peek(), ('0'..='9')) {
                str.push(self.advance())
            } else if self.peek() == '.'
                && matches!(
                    self.source
                        .get(self.current + 1)
                        .map(|c| *c)
                        .unwrap_or('\0'),
                    ('0'..='9')
                )
            {
                if is_float {
                    self.skip_to_after_number();
                    return Err(TokenizationError::InvalidNumberError {
                        loc: loc!(self.file;self.line;self.column),
                    });
                }
                is_float = true;
                str.push(self.advance());
            } else if matches!(self.peek(), 'i' | 'u' | 'f') {
                typ.push(self.advance());
            } else {
                break;
            }
        }
        let number_type = match NumberType::from_str(&typ) {
            Ok(v @ (NumberType::F32 | NumberType::F64)) => v,
            Ok(v @ (NumberType::U8 | NumberType::U16 | NumberType::U32 | NumberType::U64))
                if !is_negative && !is_float =>
            {
                v
            }
            Ok(v) if !is_float => v,
            Err(_) if typ.len() < 1 => NumberType::None,
            _ => {
                return Err(TokenizationError::InvalidNumberType(
                    loc!(self.file;self.line;self.column - typ.len() as u32),
                ))
            }
        };

        let (lit, tok) = if is_float {
            let num = match str.parse::<f64>() {
                Ok(num) => num,
                Err(..) => {
                    return Err(TokenizationError::invalid_number(loc!(self.file;self.line)))
                }
            };
            (Literal::Float(num, number_type), TokenType::FloatLiteral)
        } else {
            if is_negative {
                (
                    Literal::SInt(-(Self::parse_dec(&str[1..]) as i64), number_type),
                    TokenType::SIntLiteral,
                )
            } else {
                (
                    Literal::UInt(Self::parse_dec(&str), number_type),
                    TokenType::UIntLiteral,
                )
            }
        };

        Ok(self.get_token_lit_loc(tok, lit, loc))
    }

    fn parse_string(&mut self, string_char: char) -> Result<Token, TokenizationError> {
        let mut is_backslash = false;
        let mut str = String::new();
        let loc = loc!(self.file;self.line;self.column);

        while !self.is_at_end() {
            let c = self.advance();

            if is_backslash {
                is_backslash = false;
                str.push(Self::escape_char_to_real_char(c));
            } else if c == '\\' {
                is_backslash = true;
            } else if c == string_char {
                break;
            } else {
                str.push(c);
            }
        }
        if self.cur_char() != string_char || self.source[self.current - 2] == '\\' {
            return Err(TokenizationError::unclosed_string(
                loc!(self.file;self.line+1),
            ));
        }

        Ok(self.get_token_lit_loc(
            TokenType::StringLiteral,
            Literal::String(GlobalStr::new_boxed(str.into_boxed_str())),
            loc,
        ))
    }

    fn escape_char_to_real_char(character: char) -> char {
        match character {
            'n' => '\n',
            '0' => '\0',
            'r' => '\r',
            't' => '\t',
            _ => character,
        }
    }

    fn parse_identifier(&mut self, starting_char: char) -> Result<Token, TokenizationError> {
        let mut identifier = String::new();
        identifier.push(starting_char);
        let loc = loc!(self.file;self.line;self.column);

        while !self.is_at_end() {
            if !Self::is_valid_identifier_char(self.peek()) {
                break;
            }
            identifier.push(self.advance());
        }
        match identifier.as_str() {
            "true" => {
                return Ok(self.get_token_lit_loc(
                    TokenType::BooleanLiteral,
                    Literal::Bool(true),
                    loc,
                ))
            }
            "false" => {
                return Ok(self.get_token_lit_loc(
                    TokenType::BooleanLiteral,
                    Literal::Bool(false),
                    loc,
                ))
            }
            "void" => return Ok(self.get_token(TokenType::VoidLiteral)),
            _ => (),
        }
        Ok(Self::try_token_from_keyword(&identifier)
            .map(|v| self.get_token(v))
            .unwrap_or_else(|| {
                self.get_token_lit_loc(
                    TokenType::IdentifierLiteral,
                    Literal::String(GlobalStr::new_boxed(identifier.into_boxed_str())),
                    loc,
                )
            }))
    }

    fn do_macro(
        &mut self,
        loc: &Location,
        name: &GlobalStr,
    ) -> Result<Vec<Token>, TokenizationError> {
        let closing_bracket_type = match self.peek() {
            '[' => ']',
            '(' => ')',
            '{' => '}',
            _ => {
                return Err(TokenizationError::MacroExpectedBracket {
                    loc: loc!(self.file.clone();self.line;self.column + 1),
                    character: self.peek(),
                })
            }
        };
        let opening_bracket_type = self.advance();
        let mut depth = 0usize;
        let mut tokens = Vec::new();
        loop {
            if self.peek() == closing_bracket_type && depth > 0 {
                depth -= 1;
            } else if self.peek() == closing_bracket_type {
                self.advance();
                break;
            } else if self.peek() == opening_bracket_type {
                depth += 1;
            } else if self.peek() == '\0' || self.is_at_end() {
                return Err(TokenizationError::UnclosedMacro {
                    loc: loc!(self.file;self.line;self.column),
                    bracket: closing_bracket_type,
                });
            }
            let Some(tok) = self.int_scan_token()? else {
                continue;
            };
            if tok.typ == TokenType::IdentifierLiteral && self.if_char_advance('!') {
                let Some(Literal::String(ref name)) = tok.literal else {
                    unreachable!(
                        "TokenType::IdentifierLiteral should always have a string literal value"
                    )
                };
                tokens.append(&mut self.do_macro(&tok.location, name)?);
            } else {
                tokens.push(tok);
            }
        }
        let tokens =
            if let Some(macro_fn) = name.with(|v| crate::builtin_macros::get_builtin_macro(v)) {
                macro_fn(loc, &tokens)
            } else {
                // TODO: implement macros
                unimplemented!("custom macros");
            };
        Ok(tokens)
    }

    fn try_token_from_keyword(word: &str) -> Option<TokenType> {
        match word {
            "let" => Some(TokenType::Let),
            "as" => Some(TokenType::As),
            "fn" => Some(TokenType::Fn),
            "extern" => Some(TokenType::Extern),
            "return" => Some(TokenType::Return),
            "if" => Some(TokenType::If),
            "else" => Some(TokenType::Else),
            "while" => Some(TokenType::While),
            "for" => Some(TokenType::For),
            "in" => Some(TokenType::In),
            "struct" => Some(TokenType::Struct),
            "impl" => Some(TokenType::Impl),
            "trait" => Some(TokenType::Trait),
            "use" => Some(TokenType::Use),
            "export" => Some(TokenType::Export),
            _ => None,
        }
    }

    fn get_token_lit_loc(&self, token: TokenType, literal: Literal, location: Location) -> Token {
        Token::new(
            token,
            Some(literal),
            location.line,
            location.column,
            location.file,
        )
    }

    /// valid characters:
    /// abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$#
    fn is_valid_identifier_char(character: char) -> bool {
        matches!(
            character,
            '_' | '$' | '#' | ('a'..='z') | ('A'..='Z') | ('0'..='9')
        )
    }

    pub fn get_tokens(&self) -> &[Token] {
        &self.tokens
    }

    pub fn to_parser(self, modules: Arc<RwLock<Vec<ParserQueueEntry>>>, root: Arc<Path>) -> Parser {
        Parser::new(self.tokens, modules, self.file, root)
    }
}

#[cfg(test)]
mod test {
    use std::path::PathBuf;

    use super::*;

    #[test]
    fn test_numbers() {
        let mut tokenizer = Tokenizer::new(
            r"
12;
-23;
23.9;
-29.3;

0x1;
-0x1;
0x1.2;
-0x1.2;

0b1101;
-0b101;
0b10.2;
-0b10.1;

0o5;
-0o42;
0o6.23;
-0o5.76;
",
            PathBuf::from("test").into(),
        );

        let errs = tokenizer.scan_tokens().err().unwrap_or(vec![]);
        let tokens = tokenizer.get_tokens();

        insta::assert_debug_snapshot!((errs, tokens));
    }
}
