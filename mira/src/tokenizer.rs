use mira_spans::{BytePos, Ident, SourceFile, Span, SpanData};
use parking_lot::RwLock;
use std::{
    fmt::{Debug, Display},
    str::FromStr,
    sync::Arc,
};

use crate::{
    context::SharedContext,
    error::{ParsingError, TokenizationError},
    module::Module,
    parser::{LiteralValue, Parser, ParserQueueEntry},
    store::{Store, StoreKey},
};
use mira_spans::interner::Symbol;

macro_rules! token_type {
    ($($key:ident $(=$value:literal)?),* $(,)?) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub enum TokenType {
            $($key),*
        }

        impl TokenType {
            pub fn char_len(self) -> Option<u32> {
                self.as_str().map(|s| s.len() as u32)
            }

            pub fn as_str(self) -> Option<&'static str> {
                match self {
                    $(Self::$key => token_type!(!internal $($value)?),)*
                }
            }
        }
    };
    (!internal) => {
        None
    };
    (!internal $value:literal) => {
        Some($value)
    };
}

// #[derive(Clone, Copy, Debug, PartialEq, Eq)]
// pub enum TokenType {
token_type! {
    Let = "let",
    EqualEqual = "==",
    NotEquals = "!=",
    LessThan = "<",
    GreaterThan = ">",
    LogicalNot = "!",
    LogicalAnd = "&&",
    LogicalOr = "||",
    StringLiteral,
    FloatLiteral,
    SIntLiteral,
    UIntLiteral,
    BooleanLiteral,
    VoidLiteral = "void",
    IdentifierLiteral,
    Equal = "=",
    Colon = ":",
    Semicolon = ";",
    ParenLeft = "(",
    ParenRight = ")",
    CurlyLeft = "{",
    CurlyRight = "}",
    BracketLeft = "[",
    BracketRight = "]",
    Plus = "+",
    Minus = "-",
    Asterix = "*",
    Divide = "/",
    Modulo = "%",
    BitwiseNot = "~",
    Ampersand = "&",
    BitwiseOr = "|",
    BitwiseXor = "^",
    PipeOperator = "|>",
    Return = "return",
    Fn = "fn",
    Extern = "extern",
    Use = "use",
    Mod = "mod",
    Export = "export",
    If = "if",
    Else = "else",
    Asm = "asm",
    Volatile = "volatile",
    While = "while",
    For = "for",
    Pub = "pub",
    In = "in",
    Unsized = "unsized",
    Range = "..",
    RangeInclusive = "..=",
    ReturnType = "->",
    Struct = "struct",
    Trait = "trait",
    Impl = "impl",
    Comma = ",",
    PlusAssign = "+=",
    MinusAssign = "-=",
    Dot = ".",
    As = "as",
    AnnotationIntroducer = "@",
    NamespaceAccess = "::",
    Eof = "",
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Literal<'arena> {
    Float(f64, NumberType),
    SInt(i64, NumberType),
    UInt(u64, NumberType),
    String(Symbol<'arena>),
    Bool(bool),
}

#[derive(Clone, Copy, Debug)]
pub struct Token<'arena> {
    pub typ: TokenType,
    pub literal: Option<Literal<'arena>>,
    pub span: Span<'arena>,
}

impl<'arena> From<Token<'arena>> for Ident<'arena> {
    fn from(value: Token<'arena>) -> Self {
        assert_eq!(value.typ, TokenType::IdentifierLiteral);
        let Some(Literal::String(s)) = value.literal else {
            unreachable!("expected value.literal to be Some(Literal::String(_))")
        };
        Self::new(s, value.span)
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(s) = self.as_str() {
            return f.write_str(s);
        }
        match self {
            TokenType::IdentifierLiteral => f.write_str("identifier"),
            TokenType::SIntLiteral => f.write_str("signed number"),
            TokenType::UIntLiteral => f.write_str("unsigned number"),
            TokenType::VoidLiteral => f.write_str("void"),
            TokenType::FloatLiteral => f.write_str("decimal number"),
            TokenType::StringLiteral => f.write_str("string"),
            TokenType::BooleanLiteral => f.write_str("boolean"),
            _ => unreachable!(),
        }
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(s) = self.typ.as_str() {
            return f.write_str(s);
        }
        match self.typ {
            TokenType::BooleanLiteral => match &self.literal {
                Some(Literal::Bool(v)) => Display::fmt(v, f),
                _ => f.write_str("bool(malformed data)"),
            },
            TokenType::IdentifierLiteral => match &self.literal {
                Some(Literal::String(v)) => Display::fmt(v, f),
                _ => f.write_str("identifier(malformed data)"),
            },
            TokenType::FloatLiteral => match self.literal {
                Some(Literal::Float(v, typ)) => f.write_fmt(format_args!("{v}{typ}")),
                _ => f.write_str("float(malformed data)"),
            },
            TokenType::SIntLiteral => match self.literal {
                Some(Literal::SInt(v, typ)) => f.write_fmt(format_args!("{v}{typ}")),
                _ => f.write_str("int(malformed data)"),
            },
            TokenType::UIntLiteral => match self.literal {
                Some(Literal::UInt(v, typ)) => f.write_fmt(format_args!("{v}{typ}")),
                _ => f.write_str("uint(malformed data)"),
            },
            TokenType::StringLiteral => match &self.literal {
                Some(Literal::String(v)) => Debug::fmt(v, f),
                _ => f.write_str("string(malformed data)"),
            },
            _ => unreachable!(),
        }
    }
}

impl<'arena> Token<'arena> {
    pub fn new(typ: TokenType, literal: Option<Literal<'arena>>, span: Span<'arena>) -> Self {
        Self { typ, span, literal }
    }

    pub fn to_literal_value(&self) -> Option<LiteralValue<'arena>> {
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
                Literal::String(string) => LiteralValue::String(*string),
            }),
            TokenType::VoidLiteral => Some(LiteralValue::Void),
            TokenType::IdentifierLiteral => match self.literal {
                Some(Literal::String(ref v)) => Some(LiteralValue::Dynamic(
                    crate::parser::Path::new(Ident::new(*v, self.span), Vec::new()),
                )),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn void_literal(&self) -> Result<(), ParsingError<'arena>> {
        match &self.literal {
            None => Ok(()),
            _ => Err(ParsingError::InvalidTokenization(self.span)),
        }
    }

    pub fn string_literal(&self) -> Result<Symbol<'arena>, ParsingError<'arena>> {
        match &self.literal {
            Some(Literal::String(v)) => Ok(*v),
            _ => Err(ParsingError::InvalidTokenization(self.span)),
        }
    }

    pub fn bool_literal(&self) -> Result<bool, ParsingError<'arena>> {
        match &self.literal {
            Some(Literal::Bool(v)) => Ok(*v),
            _ => Err(ParsingError::InvalidTokenization(self.span)),
        }
    }

    pub fn float_literal(&self) -> Result<(f64, NumberType), ParsingError<'arena>> {
        match &self.literal {
            Some(Literal::Float(v, numty)) => Ok((*v, *numty)),
            _ => Err(ParsingError::InvalidTokenization(self.span)),
        }
    }

    pub fn sint_literal(&self) -> Result<(i64, NumberType), ParsingError<'arena>> {
        match &self.literal {
            Some(Literal::SInt(v, numty)) => Ok((*v, *numty)),
            _ => Err(ParsingError::InvalidTokenization(self.span)),
        }
    }

    pub fn uint_literal(&self) -> Result<(u64, NumberType), ParsingError<'arena>> {
        match &self.literal {
            Some(Literal::UInt(v, numty)) => Ok((*v, *numty)),
            _ => Err(ParsingError::InvalidTokenization(self.span)),
        }
    }
}

pub struct Tokenizer<'arena> {
    source: Vec<char>,
    pub file: Arc<SourceFile>,
    tokens: Vec<Token<'arena>>,
    start: usize,
    current: usize,
    ctx: SharedContext<'arena>,
}

impl<'arena> Tokenizer<'arena> {
    pub fn new(ctx: SharedContext<'arena>, file: Arc<SourceFile>) -> Self {
        Self {
            source: file.source.chars().collect(),
            file,
            start: 0,
            current: 0,
            tokens: vec![],
            ctx,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<(), Vec<TokenizationError<'arena>>> {
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
            span: self
                .ctx
                .intern_span(SpanData::new(self.file.source_len + 1, 1, self.file.id)),
        });

        if !errors.is_empty() {
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
        self.current += 1;
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

    pub fn push_token(&mut self, tok: Token<'arena>) {
        self.tokens.push(tok);
    }

    fn scan_token(&mut self) -> Result<(), TokenizationError<'arena>> {
        let tok = self.int_scan_token()?;
        let Some(tok) = tok else { return Ok(()) };
        match tok.typ {
            TokenType::IdentifierLiteral if self.if_char_advance('!') => match &tok.literal {
                Some(Literal::String(str)) => {
                    let mut tokens = self.do_macro(tok.span, str)?;
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

    fn int_scan_token(&mut self) -> Result<Option<Token<'arena>>, TokenizationError<'arena>> {
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
            '(' => token!(ParenLeft),
            ')' => token!(ParenRight),
            '{' => token!(CurlyLeft),
            '}' => token!(CurlyRight),
            '[' => token!(BracketLeft),
            ']' => token!(BracketRight),
            ',' => token!(Comma),
            '.' if self.if_char_advance('.') => token!(Range, RangeInclusive, '='),
            '.' if self.peek().is_ascii_digit() => self.parse_number('.'),
            '.' => token!(Dot),
            '+' => token!(Plus, PlusAssign, '='),
            '-' if self.peek().is_ascii_digit() || self.peek() == '.' => self.parse_number('-'),
            '-' if self.if_char_advance('=') => token!(MinusAssign),
            '-' if self.if_char_advance('>') => token!(ReturnType),
            '-' => token!(Minus),
            '/' if self.peek() != '/' && self.peek() != '*' => token!(Divide),
            '%' => token!(Modulo),
            '*' => token!(Asterix),
            '=' => token!(Equal, EqualEqual, '='),
            '<' => token!(LessThan),
            '>' => token!(GreaterThan),
            ':' => token!(Colon, NamespaceAccess, ':'),
            ';' => token!(Semicolon),
            '!' => token!(LogicalNot, NotEquals, '='),
            '~' => token!(BitwiseNot),
            '&' => token!(Ampersand, LogicalAnd, '&'),
            '|' if self.if_char_advance('|') => token!(LogicalOr),
            '|' if self.if_char_advance('>') => token!(PipeOperator),
            '|' => token!(BitwiseOr),
            '^' => token!(BitwiseXor),
            ' ' | '\n' | '\r' | '\t' => {
                while matches!(self.peek(), ' ' | '\n' | '\r' | '\t') {
                    self.advance();
                }
                return Ok(None);
            }
            '/' if self.if_char_advance('*') => {
                loop {
                    if self.advance() == '*' && self.if_char_advance('/') {
                        break;
                    }
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
            _ if Self::is_valid_identifier_char(c) && !c.is_ascii_digit() => {
                self.parse_identifier(c)
            }
            _ => return Err(TokenizationError::unknown_token(self.current_span(), c)),
        }
        .map(Some)
    }

    #[inline(always)]
    fn get_token(&self, token: TokenType) -> Token<'arena> {
        Token::new(
            token,
            None,
            self.span_from(self.current - token.char_len().unwrap() as usize),
        )
    }

    fn skip_to_after_number(&mut self) {
        loop {
            match self.peek() {
                '0'..='9' => _ = self.advance(),
                '.' if self
                    .source
                    .get(self.current + 1)
                    .copied()
                    .unwrap_or('\0')
                    .is_ascii_digit() =>
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
        start_bytepos: usize,
        first_char: char,
        value: u64,
        is_negative: bool,
        allow_float: bool,
    ) -> Result<Token<'arena>, TokenizationError<'arena>> {
        let mut typ = String::from(first_char);
        let first_type_char = self.current - 1;

        loop {
            match self.peek() {
                'a'..='z' | '0'..='9' => typ.push(self.advance()),
                _ => {
                    let err = TokenizationError::InvalidNumberType(self.span_from(first_type_char));
                    let Ok(number_type) = NumberType::from_str(&typ) else {
                        return Err(err);
                    };
                    return match number_type {
                        NumberType::F32 | NumberType::F64 if !allow_float => Err(err),
                        NumberType::F32 | NumberType::F64 if is_negative => Ok(Token::new(
                            TokenType::FloatLiteral,
                            Some(Literal::Float(-(value as f64), number_type)),
                            self.span_from(start_bytepos),
                        )),
                        NumberType::F32 | NumberType::F64 => Ok(Token::new(
                            TokenType::FloatLiteral,
                            Some(Literal::Float(value as f64, number_type)),
                            self.span_from(start_bytepos),
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
                        _ if is_negative => Ok(Token::new(
                            TokenType::SIntLiteral,
                            Some(Literal::SInt(-(value as i64), number_type)),
                            self.span_from(start_bytepos),
                        )),
                        _ => Ok(Token::new(
                            TokenType::SIntLiteral,
                            Some(Literal::UInt(value, number_type)),
                            self.span_from(start_bytepos),
                        )),
                    };
                }
            }
        }
    }

    fn parse_hex(
        &mut self,
        start_bytepos: usize,
        is_negative: bool,
    ) -> Result<Token<'arena>, TokenizationError<'arena>> {
        let mut value: u64 = 0;

        loop {
            match self.peek() {
                '0'..='9' => value = (value << 4) | (self.advance() as u64 - '0' as u64),
                'a'..='f' => value = (value << 4) | (self.advance() as u64 - 'a' as u64 + 0xa),
                'A'..='F' => value = (value << 4) | (self.advance() as u64 - 'A' as u64 + 0xa),
                c if Self::is_valid_identifier_char(c) => {
                    return self.parse_numtype(start_bytepos, c, value, is_negative, false)
                }
                _ if is_negative => {
                    return Ok(Token::new(
                        TokenType::SIntLiteral,
                        Some(Literal::SInt(-(value as i64), NumberType::None)),
                        self.span_from(start_bytepos),
                    ))
                }
                _ => {
                    return Ok(Token::new(
                        TokenType::UIntLiteral,
                        Some(Literal::UInt(value, NumberType::None)),
                        self.span_from(start_bytepos),
                    ))
                }
            }
        }
    }

    fn parse_bin(
        &mut self,
        start_bytepos: usize,
        is_negative: bool,
    ) -> Result<Token<'arena>, TokenizationError<'arena>> {
        let mut value: u64 = 0;

        loop {
            match self.peek() {
                '0' | '1' => value = (value << 1) | (self.advance() as u64 - '0' as u64),
                '2'..='9' => {
                    self.advance();
                    return Err(TokenizationError::invalid_number(
                        self.span_from(start_bytepos),
                    ));
                }
                c if Self::is_valid_identifier_char(c) => {
                    return self.parse_numtype(start_bytepos, c, value, is_negative, false)
                }
                _ if is_negative => {
                    return Ok(Token::new(
                        TokenType::SIntLiteral,
                        Some(Literal::SInt(-(value as i64), NumberType::None)),
                        self.span_from(start_bytepos),
                    ))
                }
                _ => {
                    return Ok(Token::new(
                        TokenType::UIntLiteral,
                        Some(Literal::UInt(value, NumberType::None)),
                        self.span_from(start_bytepos),
                    ))
                }
            }
        }
    }

    fn parse_oct(
        &mut self,
        start_bytepos: usize,
        is_negative: bool,
    ) -> Result<Token<'arena>, TokenizationError<'arena>> {
        let mut value: u64 = 0;

        loop {
            match self.peek() {
                '0'..='7' => value = (value << 3) | (self.advance() as u64 - '0' as u64),
                '8' | '9' => {
                    self.advance();
                    return Err(TokenizationError::invalid_number(
                        self.span_from(start_bytepos),
                    ));
                }
                c if Self::is_valid_identifier_char(c) => {
                    return self.parse_numtype(start_bytepos, c, value, is_negative, false)
                }
                _ if is_negative => {
                    return Ok(Token::new(
                        TokenType::SIntLiteral,
                        Some(Literal::SInt(-(value as i64), NumberType::None)),
                        self.span_from(start_bytepos),
                    ))
                }
                _ => {
                    return Ok(Token::new(
                        TokenType::UIntLiteral,
                        Some(Literal::UInt(value, NumberType::None)),
                        self.span_from(start_bytepos),
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

    fn parse_number(
        &mut self,
        mut first_char: char,
    ) -> Result<Token<'arena>, TokenizationError<'arena>> {
        let start_byte = self.current;
        let is_negative = first_char == '-';
        let mut is_float = false;
        if is_negative {
            first_char = self.advance();
        }

        if first_char == '0' && self.peek() == 'x' {
            self.advance();
            if !self.peek().is_ascii_hexdigit() {
                self.advance();
                return Err(TokenizationError::invalid_number(
                    self.span_from(start_byte),
                ));
            }
            return self.parse_hex(start_byte, is_negative);
        } else if first_char == '0' && self.peek() == 'b' {
            self.advance();
            if !matches!(self.peek(), '0' | '1') {
                self.advance();
                return Err(TokenizationError::invalid_number(
                    self.span_from(start_byte),
                ));
            }
            return self.parse_bin(start_byte, is_negative);
        } else if first_char == '0' && self.peek() == 'o' {
            self.advance();
            if !matches!(self.peek(), '0'..='7') {
                self.advance();
                return Err(TokenizationError::invalid_number(
                    self.span_from(start_byte),
                ));
            }
            return self.parse_oct(start_byte, is_negative);
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
            if !typ.is_empty() {
                if self.peek().is_ascii_alphanumeric() {
                    typ.push(self.advance());
                    continue;
                }
                break;
            }

            if self.peek().is_ascii_digit() {
                str.push(self.advance())
            } else if self.peek() == '.'
                && self
                    .source
                    .get(self.current + 1)
                    .copied()
                    .unwrap_or('\0')
                    .is_ascii_digit()
            {
                if is_float {
                    self.skip_to_after_number();
                    return Err(TokenizationError::invalid_number(
                        self.span_from(start_byte),
                    ));
                }
                is_float = true;
                str.push(self.advance());
            } else if Self::is_valid_identifier_char(self.peek()) {
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
            Err(_) if typ.is_empty() => NumberType::None,
            _ => {
                return Err(TokenizationError::InvalidNumberType(
                    self.span_from(start_byte),
                ))
            }
        };

        let (lit, tok) = if is_float {
            let num = match str.parse::<f64>() {
                Ok(num) => num,
                Err(..) => {
                    return Err(TokenizationError::invalid_number(
                        self.span_from(start_byte),
                    ))
                }
            };
            (Literal::Float(num, number_type), TokenType::FloatLiteral)
        } else if is_negative {
            (
                Literal::SInt(-(Self::parse_dec(&str[1..]) as i64), number_type),
                TokenType::SIntLiteral,
            )
        } else {
            (
                Literal::UInt(Self::parse_dec(&str), number_type),
                TokenType::UIntLiteral,
            )
        };

        Ok(Token::new(tok, Some(lit), self.span_from(start_byte)))
    }

    fn parse_string(
        &mut self,
        string_char: char,
    ) -> Result<Token<'arena>, TokenizationError<'arena>> {
        let mut is_backslash = false;
        let mut s = String::new();
        let start = self.current;

        while !self.is_at_end() {
            let c = self.advance();

            if is_backslash {
                is_backslash = false;
                s.push(Self::escape_char_to_real_char(c));
            } else if c == '\\' {
                is_backslash = true;
            } else if c == string_char || c == '\n' {
                break;
            } else {
                s.push(c);
            }
        }
        if self.cur_char() != string_char || self.source[self.current - 2] == '\\' {
            return Err(TokenizationError::unclosed_string(self.current_span()));
        }

        Ok(Token::new(
            TokenType::StringLiteral,
            Some(Literal::String(self.ctx.intern_str(&s))),
            self.span_from(start),
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

    fn parse_identifier(
        &mut self,
        starting_char: char,
    ) -> Result<Token<'arena>, TokenizationError<'arena>> {
        let mut identifier = String::new();
        identifier.push(starting_char);
        let start = self.current;

        while !self.is_at_end() {
            if !Self::is_valid_identifier_char(self.peek()) {
                break;
            }
            identifier.push(self.advance());
        }
        match identifier.as_str() {
            "true" => {
                return Ok(Token::new(
                    TokenType::BooleanLiteral,
                    Some(Literal::Bool(true)),
                    self.span_from(start),
                ))
            }
            "false" => {
                return Ok(Token::new(
                    TokenType::BooleanLiteral,
                    Some(Literal::Bool(false)),
                    self.span_from(start),
                ))
            }
            "void" => return Ok(self.get_token(TokenType::VoidLiteral)),
            _ => (),
        }
        Ok(Self::try_token_from_keyword(&identifier)
            .map(|v| self.get_token(v))
            .unwrap_or_else(|| {
                Token::new(
                    TokenType::IdentifierLiteral,
                    Some(Literal::String(self.ctx.intern_str(&identifier))),
                    self.span_from(start),
                )
            }))
    }

    fn span_from(&self, from: usize) -> Span<'arena> {
        self.span((self.current - from) as u32)
    }

    fn span(&self, len: u32) -> Span<'arena> {
        self.ctx.intern_span(SpanData::new(
            BytePos::from_u32((self.current as u32).saturating_sub(len)),
            len,
            self.file.id,
        ))
    }

    fn current_span(&self) -> Span<'arena> {
        self.ctx.intern_span(SpanData::new(
            BytePos::from_u32((self.current as u32).saturating_sub(1)),
            1,
            self.file.id,
        ))
    }

    fn do_macro(
        &mut self,
        loc: Span<'arena>,
        name: &Symbol<'arena>,
    ) -> Result<Vec<Token<'arena>>, TokenizationError<'arena>> {
        let start = loc.get_span_data().pos.to_usize();

        let bracket_type = match self.peek() {
            '[' => ']',
            '(' => ')',
            '{' => '}',
            _ => {
                return Err(TokenizationError::MacroExpectedBracket {
                    loc: self.current_span(),
                    character: self.peek(),
                })
            }
        };
        let mut depth = 0usize;
        let mut tokens = Vec::new();
        loop {
            if self.peek() == bracket_type && depth > 0 {
                depth -= 1;
            } else if self.peek() == bracket_type {
                self.advance();
                break;
            } else if self.peek() == bracket_type {
                depth += 1;
            } else if self.peek() == '\0' || self.is_at_end() {
                return Err(TokenizationError::UnclosedMacro {
                    loc: self.current_span(),
                    bracket: bracket_type,
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
                tokens.append(&mut self.do_macro(tok.span, name)?);
            } else {
                tokens.push(tok);
            }
        }
        let tokens = if let Some(macro_fn) = crate::builtin_macros::get_builtin_macro(name) {
            macro_fn(self.ctx, self.span_from(start), &tokens)
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
            "asm" => Some(TokenType::Asm),
            "volatile" => Some(TokenType::Volatile),
            "while" => Some(TokenType::While),
            "for" => Some(TokenType::For),
            "pub" => Some(TokenType::Pub),
            "in" => Some(TokenType::In),
            "unsized" => Some(TokenType::Unsized),
            "struct" => Some(TokenType::Struct),
            "impl" => Some(TokenType::Impl),
            "trait" => Some(TokenType::Trait),
            "use" => Some(TokenType::Use),
            "mod" => Some(TokenType::Mod),
            "export" => Some(TokenType::Export),
            _ => None,
        }
    }

    /// valid characters:
    /// abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$#
    fn is_valid_identifier_char(character: char) -> bool {
        matches!(
            character,
            '_' | '$' | '#' | ('a'..='z') | ('A'..='Z') | ('0'..='9')
        )
    }

    pub fn get_tokens(&self) -> &[Token<'arena>] {
        &self.tokens
    }

    #[allow(clippy::too_many_arguments)]
    pub fn to_parser<'a>(
        self,
        parser_queue: Arc<RwLock<Vec<ParserQueueEntry<'arena>>>>,
        modules: &'a RwLock<Store<Module<'arena>>>,
        key: StoreKey<Module<'arena>>,
    ) -> Parser<'a, 'arena> {
        Parser::new(self.ctx, self.tokens, parser_queue, modules, self.file, key)
    }
}

#[cfg(test)]
mod test {
    use std::{collections::HashMap, path::Path};

    use crate::context::GlobalContext;
    use mira_spans::{interner::SymbolInterner, Arena};

    use super::*;

    fn check_tokens(tokens: &[Token]) {
        for token in tokens {
            match (&token.typ, &token.literal) {
                (TokenType::IdentifierLiteral, Some(Literal::String(..)))
                | (TokenType::StringLiteral, Some(Literal::String(..)))
                | (TokenType::SIntLiteral, Some(Literal::SInt(..)))
                | (TokenType::UIntLiteral, Some(Literal::UInt(..)))
                | (TokenType::FloatLiteral, Some(Literal::Float(..)))
                | (TokenType::BooleanLiteral, Some(Literal::Bool(..)))
                | (TokenType::VoidLiteral, None) => (),
                (TokenType::IdentifierLiteral, _)
                | (TokenType::StringLiteral, _)
                | (TokenType::SIntLiteral, _)
                | (TokenType::UIntLiteral, _)
                | (TokenType::FloatLiteral, _)
                | (TokenType::BooleanLiteral, _)
                | (TokenType::VoidLiteral, _) => {
                    panic!("invalid literal {:?} for {:?}", token.literal, token.typ)
                }
                _ => (),
            }
        }
    }

    fn get_tokens<'arena>(
        ctx: SharedContext<'arena>,
        src: &str,
    ) -> (Vec<Token<'arena>>, Vec<TokenizationError<'arena>>) {
        let mut tokenizer = Tokenizer::new(
            ctx,
            ctx.source_map()
                .add_package(
                    Path::new("root").into(),
                    Path::new("root/file.mr").into(),
                    src.into(),
                    HashMap::new(),
                )
                .1,
        );
        let errs = tokenizer.scan_tokens().err().unwrap_or_default();
        check_tokens(tokenizer.get_tokens());
        (tokenizer.tokens, errs)
    }

    fn assert_token_eq(src: &str, expected_tokens: &[(TokenType, Option<Literal>)]) {
        let arena = Arena::new();
        let ctx = GlobalContext::new(&arena);
        let eof_token = (TokenType::Eof, None);
        let (tokens, errs) = get_tokens(ctx.share(), src);
        println!("{tokens:?}");
        assert_eq!(errs.len(), 0, "unexpected errors: {errs:?}");
        assert_eq!(tokens.len(), expected_tokens.len() + 1 /* eof token */);
        for (tok, expected) in tokens
            .iter()
            .zip(expected_tokens.iter().chain(std::iter::once(&eof_token)))
        {
            if tok.typ != expected.0 || tok.literal != expected.1 {
                panic!("mismatching tokens\n  left: {tokens:?}\n  right: {expected_tokens:?}\n\n{tok:?} - {expected:?}");
            }
        }
    }

    macro_rules! match_errs {
        ($str: expr; $($pat:pat),* $(,)?) => {
            let mut i = 0;
            let arena = Arena::new();
            let ctx = GlobalContext::new(&arena);
            let (_, errs) = get_tokens(ctx.share(), $str);
            $(
                if i >= errs.len() {
                    panic!("Expected error matching {:?} ({i})", stringify!($pat));
                }
                if !matches!(errs[i], $pat) {
                    panic!("Mismatching error, err: {:?}, Expected: {} ({i})", errs[i], stringify!($pat));
                }
                i += 1;
            )*
            if i < errs.len() {
                panic!("Mismatching error, err: {:?}, expected nothing", errs[i]);
            }
        };
    }

    macro_rules! tok {
        ($interner:expr, IdentifierLiteral, $lit:ident) => {
            (
                TokenType::IdentifierLiteral,
                Some(Literal::String($interner.intern(stringify!($lit)))),
            )
        };
        ($ty:ident) => {
            (TokenType::$ty, None)
        };
        ($interner:expr, $ty: ident, $lit:ident($val:expr)) => {
            (TokenType::$ty, Some(Literal::$lit($interner.intern($val))))
        };
        ($ty: ident, $lit:ident($val:expr, _)) => {
            (TokenType::$ty, Some(Literal::$lit($val, NumberType::None)))
        };
        ($ty: ident, $lit:ident($val:expr, $numty:ident)) => {
            (
                TokenType::$ty,
                Some(Literal::$lit($val, NumberType::$numty)),
            )
        };
    }

    #[test]
    fn test_strings() {
        let arena = Arena::new();
        let mut interner = SymbolInterner::new(&arena);
        assert_token_eq(
            r#"
"a b c";
"a\n\n\\t";
"a\t\3";
            "#,
            &[
                tok!(interner, StringLiteral, String("a b c")),
                tok!(Semicolon),
                tok!(interner, StringLiteral, String("a\n\n\\t")),
                tok!(Semicolon),
                tok!(interner, StringLiteral, String("a\t3")),
                tok!(Semicolon),
            ],
        );

        match_errs!("\"a\nb\nc\";"; TokenizationError::UnclosedString { loc: _ }, TokenizationError::UnclosedString { loc: _ });
    }

    #[test]
    fn test_idents() {
        let arena = Arena::new();
        let mut interner = SymbolInterner::new(&arena);
        assert_token_eq(
            "jkhdfgkjhdf",
            &[tok!(interner, IdentifierLiteral, jkhdfgkjhdf)],
        );
        assert_token_eq("_Zn3Meow", &[tok!(interner, IdentifierLiteral, _Zn3Meow)]);
        assert_token_eq(
            "_3$5#12_mow",
            &[tok!(interner, IdentifierLiteral, String("_3$5#12_mow"))],
        );
        match_errs!("1289hjdsjhfgdfg_meow"; TokenizationError::InvalidNumberType(_));
    }

    #[test]
    fn test_numbers() {
        assert_token_eq(
            "12; -23; 23.9; -29.3; 0x1; -0x1;",
            &[
                tok!(UIntLiteral, UInt(12, _)),
                tok!(Semicolon),
                tok!(SIntLiteral, SInt(-23, _)),
                tok!(Semicolon),
                tok!(FloatLiteral, Float(23.9, _)),
                tok!(Semicolon),
                tok!(FloatLiteral, Float(-29.3, _)),
                tok!(Semicolon),
                tok!(UIntLiteral, UInt(0x1, _)),
                tok!(Semicolon),
                tok!(SIntLiteral, SInt(-0x1, _)),
                tok!(Semicolon),
            ],
        );

        assert_token_eq(
            "0x1.2; -0x1.2",
            &[
                tok!(UIntLiteral, UInt(1, _)),
                tok!(FloatLiteral, Float(0.2, _)),
                tok!(Semicolon),
                tok!(SIntLiteral, SInt(-1, _)),
                tok!(FloatLiteral, Float(0.2, _)),
            ],
        );

        assert_token_eq(
            "0b1101; -0b101",
            &[
                tok!(UIntLiteral, UInt(0b1101, _)),
                tok!(Semicolon),
                tok!(SIntLiteral, SInt(-(0b101), _)),
            ],
        );

        assert_token_eq(
            "0b10.2; -0b10.1",
            &[
                tok!(UIntLiteral, UInt(0b10, _)),
                tok!(FloatLiteral, Float(0.2, _)),
                tok!(Semicolon),
                tok!(SIntLiteral, SInt(-0b10, _)),
                tok!(FloatLiteral, Float(0.1, _)),
            ],
        );

        assert_token_eq(
            "0o5; -0o42",
            &[
                tok!(UIntLiteral, UInt(0o5, _)),
                tok!(Semicolon),
                tok!(SIntLiteral, SInt(-0o42, _)),
            ],
        );

        assert_token_eq(
            "0o6.23; -0o5.76",
            &[
                tok!(UIntLiteral, UInt(0o6, _)),
                tok!(FloatLiteral, Float(0.23, _)),
                tok!(Semicolon),
                tok!(SIntLiteral, SInt(-0o5, _)),
                tok!(FloatLiteral, Float(0.76, _)),
            ],
        );
    }
}
