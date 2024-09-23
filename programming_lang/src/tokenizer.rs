use std::{
    fmt::{Display, Write},
    path::Path,
    rc::Rc,
};

use crate::{
    error::ProgrammingLangTokenizationError,
    parser::{LiteralValue, Parser},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenType {
    // Tokenization, Parsing ; *: no types
    Let,                  // done, done
    EqualEqual,               // done, done
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
    IdentifierLiteral,    // done, done
    Equal,          // done, done
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
    BitwiseLShift,        // done, done
    BitwiseRShift,        // done, done
    PipeOperator,         // done, done
    Return,               // done, done
    Fn,                   // done, done
    Extern,               // done, done
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

#[derive(Clone, Debug)]
pub enum Literal {
    Float(f64),
    SInt(i64),
    UInt(u64),
    String(Rc<str>),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub struct Location {
    pub line: usize,
    pub column: usize,
    pub file: Rc<Path>,
}

impl Location {
    pub fn new(file: Rc<Path>, line: usize, column: usize) -> Self {
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
            TokenType::BitwiseLShift => f.write_str("<<"),
            TokenType::BitwiseLShiftAssign => f.write_str("<<="),
            TokenType::BitwiseRShift => f.write_str(">>"),
            TokenType::BitwiseRShiftAssign => f.write_str(">>="),
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
                Some(Literal::Float(v)) => f.write_fmt(format_args!("float({v})")),
                _ => f.write_str("float(malformed data)"),
            },
            TokenType::SIntLiteral => match self.literal {
                Some(Literal::SInt(v)) => f.write_fmt(format_args!("int({v})")),
                _ => f.write_str("int(malformed data)"),
            },
            TokenType::UIntLiteral => match self.literal {
                Some(Literal::UInt(v)) => f.write_fmt(format_args!("uint({v})")),
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
        }?;
        f.write_str(" @ ")?;
        Display::fmt(&self.location, f)?;
        f.write_char(' ')
    }
}

impl Token {
    pub fn new(
        typ: TokenType,
        literal: Option<Literal>,
        line_number: usize,
        column: usize,
        file: Rc<Path>,
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
                Literal::Float(float) => LiteralValue::Float(*float),
                Literal::SInt(int) => LiteralValue::SInt(*int),
                Literal::UInt(uint) => LiteralValue::UInt(*uint),
                Literal::String(string) => LiteralValue::String(string.clone()),
            }),
            TokenType::IdentifierLiteral => {
                if let Some(lit) = &self.literal {
                    return match &lit {
                        Literal::String(v) => {
                            Some(LiteralValue::Dynamic(crate::parser::Path::new(v.clone(), Vec::new())))
                        }
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
    file: Rc<Path>,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    column: usize,
}

impl Tokenizer {
    pub fn new(source: &str, file: Rc<Path>) -> Self {
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

    pub fn scan_tokens(&mut self) -> Result<(), Vec<ProgrammingLangTokenizationError>> {
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
        if self.current < 1 {
            self.source[0] // before advance() was called, this shouldnt happen
        } else {
            self.source[self.current - 1]
        }
    }

    fn peek(&self) -> char {
        self.source.get(self.current).map(|c| *c).unwrap_or('\0')
    }

    fn if_char_advance(&mut self, character: char) -> bool {
        if self.peek() == character {
            self.advance();
            true
        } else {
            false
        }
    }

    fn scan_token(&mut self) -> Result<(), ProgrammingLangTokenizationError> {
        let c = self.advance();

        macro_rules! token {
            ($token: ident) => {
                self.add_token(TokenType::$token)
            };

            ($tokena: ident, $tokenb: ident, $char: expr) => {{
                if self.if_char_advance($char) {
                    self.add_token(TokenType::$tokenb)
                } else {
                    self.add_token(TokenType::$tokena)
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
            '.' => {
                if self.if_char_advance('.') {
                    if self.if_char_advance('=') {
                        token!(RangeInclusive); // ..=
                    } else {
                        token!(Range); // ..
                    }
                } else {
                    if ('0'..='9').contains(&self.peek()) {
                        return self.parse_number('.');
                    } else {
                        token!(Dot) // .
                    }
                }
            }
            '+' => token!(Plus, PlusAssign, '='),
            '-' => {
                if matches!(self.peek(), ('0'..='9') | '.') {
                    self.parse_number('-')?;
                } else {
                    if self.if_char_advance('=') {
                        token!(MinusAssign);
                    } else if self.if_char_advance('>') {
                        token!(ReturnType);
                    } else {
                        token!(Minus);
                    }
                }
            }
            '/' if self.peek() != '/' => token!(Divide, DivideAssign, '='),
            '%' => token!(Modulo, ModuloAssign, '='),
            '*' => token!(Asterix, MultiplyAssign, '='),
            '=' => token!(Equal, EqualEqual, '='),
            '<' if self.peek() != '<' => token!(LessThan, LessThanEquals, '='),
            '<' if self.if_char_advance('<') => token!(BitwiseLShift, BitwiseLShiftAssign, '='),
            '>' if self.peek() != '>' => token!(GreaterThan, GreaterThanEquals, '='),
            '>' if self.if_char_advance('>') => token!(BitwiseRShift, BitwiseRShiftAssign, '='),
            ':' => token!(Colon, NamespaceAccess, ':'),
            ';' => token!(Semicolon),
            '!' => token!(LogicalNot, NotEquals, '='),
            '~' => token!(BitwiseNot),
            '&' => {
                if self.if_char_advance('=') {
                    token!(BitwiseAndAssign);
                } else if self.if_char_advance('&') {
                    token!(LogicalAnd);
                } else {
                    token!(Ampersand);
                }
            }
            '|' => {
                if self.if_char_advance('=') {
                    token!(BitwiseOrAssign);
                } else if self.if_char_advance('|') {
                    token!(LogicalOr);
                } else if self.if_char_advance('>') {
                    token!(PipeOperator);
                } else {
                    token!(BitwiseOr);
                }
            }
            '^' => token!(BitwiseXor, BitwiseXorAssign, '='),
            ' ' | '\n' | '\r' | '\t' => (),
            '/' if self.if_char_advance('/') => {
                while !self.is_at_end() && self.advance() != '\n' {}
            }
            '@' => token!(AnnotationIntroducer),
            ('0'..='9') => return self.parse_number(c),
            '"' => return self.parse_string(),
            _ if Self::is_valid_identifier_char(c) && !matches!(c, ('0'..='9')) => {
                self.parse_identifier(c)
            }
            _ => {
                return Err(ProgrammingLangTokenizationError::unknown_token(
                    loc!(self.file;self.line),
                    c,
                ))
            }
        }
        Ok(())
    }

    fn add_token(&mut self, token: TokenType) {
        self.tokens.push(Token::new(
            token,
            None,
            self.line,
            self.column,
            self.file.clone(),
        ));
    }

    fn skip_to_after_number(&mut self) {
        loop {
            match self.peek() {
                '0'..='9' => _ = self.advance(),
                '.' if matches!(
                    self.source
                        .get(self.current + 1)
                        .map(|c| *c)
                        .unwrap_or('\0'),
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

    fn parse_hex(
        &mut self,
        location: Location,
        is_negative: bool,
    ) -> Result<(), ProgrammingLangTokenizationError> {
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
                    return Err(ProgrammingLangTokenizationError::InvalidNumberError {
                        loc: loc!(self.file;self.line;self.column),
                    });
                }
                _ if is_negative => {
                    return Ok(self.add_token_lit_loc(
                        TokenType::SIntLiteral,
                        Literal::SInt(-(value as i64)),
                        location,
                    ))
                }
                _ => {
                    return Ok(self.add_token_lit_loc(
                        TokenType::UIntLiteral,
                        Literal::UInt(value),
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
    ) -> Result<(), ProgrammingLangTokenizationError> {
        let mut value: u64 = 0;

        loop {
            match self.peek() {
                '0' | '1' => value = (value << 1) | self.advance() as u64 - '0' as u64,
                '2'..='9' => {
                    self.advance();
                    return Err(ProgrammingLangTokenizationError::InvalidNumberError {
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
                    return Err(ProgrammingLangTokenizationError::InvalidNumberError {
                        loc: loc!(self.file;self.line;self.column),
                    });
                }
                _ if is_negative => {
                    return Ok(self.add_token_lit_loc(
                        TokenType::SIntLiteral,
                        Literal::SInt(-(value as i64)),
                        location,
                    ))
                }
                _ => {
                    return Ok(self.add_token_lit_loc(
                        TokenType::UIntLiteral,
                        Literal::UInt(value),
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
    ) -> Result<(), ProgrammingLangTokenizationError> {
        let mut value: u64 = 0;

        loop {
            match self.peek() {
                '0'..='7' => value = (value << 3) | self.advance() as u64 - '0' as u64,
                '8' | '9' => {
                    self.advance();
                    return Err(ProgrammingLangTokenizationError::InvalidNumberError {
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
                    return Err(ProgrammingLangTokenizationError::InvalidNumberError {
                        loc: loc!(self.file;self.line;self.column),
                    });
                }
                _ if is_negative => {
                    return Ok(self.add_token_lit_loc(
                        TokenType::SIntLiteral,
                        Literal::SInt(-(value as i64)),
                        location,
                    ))
                }
                _ => {
                    return Ok(self.add_token_lit_loc(
                        TokenType::UIntLiteral,
                        Literal::UInt(value),
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

    fn parse_number(
        &mut self,
        mut first_char: char,
    ) -> Result<(), ProgrammingLangTokenizationError> {
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
                return Err(ProgrammingLangTokenizationError::InvalidNumberError {
                    loc: loc!(self.file;self.line;self.column),
                });
            }
            return self.parse_hex(loc, is_negative);
        } else if first_char == '0' && self.peek() == 'b' {
            self.advance();
            if !matches!(self.peek(), '0' | '1') {
                self.advance();
                return Err(ProgrammingLangTokenizationError::InvalidNumberError {
                    loc: loc!(self.file;self.line;self.column),
                });
            }
            return self.parse_bin(loc, is_negative);
        } else if first_char == '0' && self.peek() == 'o' {
            self.advance();
            if !matches!(self.peek(), '0'..='7') {
                self.advance();
                return Err(ProgrammingLangTokenizationError::InvalidNumberError {
                    loc: loc!(self.file;self.line;self.column),
                });
            }
            return self.parse_oct(loc, is_negative);
        }

        let mut str = String::with_capacity(7);
        if is_negative {
            str.push('-');
        }
        if first_char == '.' {
            str.push('0');
            is_float = true;
        }
        str.push(first_char);

        while !self.is_at_end() {
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
                    return Err(ProgrammingLangTokenizationError::InvalidNumberError {
                        loc: loc!(self.file;self.line;self.column),
                    });
                }
                is_float = true;
                str.push(self.advance());
            } else {
                break;
            }
        }

        let (lit, tok) = if is_float {
            let num = match str.parse::<f64>() {
                Ok(num) => num,
                Err(..) => {
                    return Err(ProgrammingLangTokenizationError::invalid_number(
                        loc!(self.file;self.line),
                    ))
                }
            };
            if num.floor() == num {
                if num < 0.0 {
                    (Literal::SInt(num as i64), TokenType::SIntLiteral)
                } else {
                    (Literal::UInt(num as u64), TokenType::UIntLiteral)
                }
            } else {
                (Literal::Float(num), TokenType::FloatLiteral)
            }
        } else {
            if is_negative {
                (
                    Literal::SInt(-(Self::parse_dec(&str[1..]) as i64)),
                    TokenType::SIntLiteral,
                )
            } else {
                (Literal::UInt(Self::parse_dec(&str)), TokenType::UIntLiteral)
            }
        };

        self.add_token_lit_loc(tok, lit, loc);

        Ok(())
    }

    fn parse_string(&mut self) -> Result<(), ProgrammingLangTokenizationError> {
        let mut is_backslash = false;
        let mut str = String::with_capacity(30);
        let loc = loc!(self.file;self.line;self.column);

        while !self.is_at_end() {
            let c = self.advance();

            if is_backslash {
                is_backslash = false;
                str.push(Self::backslash_char_to_real_char(c));
            } else if c == '\\' {
                is_backslash = true;
            } else if c == '"' {
                break;
            } else {
                str.push(c);
            }
        }
        if self.cur_char() != '"' || self.source[self.current - 2] == '\\' {
            return Err(ProgrammingLangTokenizationError::unclosed_string(
                loc!(self.file;self.line+1),
            ));
        }

        self.add_token_lit_loc(TokenType::StringLiteral, Literal::String(str.into()), loc);
        return Ok(());
    }

    fn backslash_char_to_real_char(character: char) -> char {
        match character {
            'n' => '\n',
            '0' => '\0',
            'r' => '\r',
            't' => '\t',
            _ => character,
        }
    }

    fn parse_identifier(&mut self, starting_char: char) {
        let mut identifier = String::with_capacity(10);
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
                return self.add_token_lit_loc(TokenType::BooleanLiteral, Literal::Bool(true), loc)
            }
            "false" => {
                return self.add_token_lit_loc(TokenType::BooleanLiteral, Literal::Bool(false), loc)
            }
            _ => (),
        }
        if let Some(typ) = Self::try_token_from_keyword(&identifier) {
            return self.add_token(typ);
        }
        self.add_token_lit_loc(
            TokenType::IdentifierLiteral,
            Literal::String(identifier.into()),
            loc,
        );
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
            _ => None,
        }
    }

    fn add_token_lit_loc(&mut self, token: TokenType, literal: Literal, location: Location) {
        self.tokens.push(Token::new(
            token,
            Some(literal),
            location.line,
            location.column,
            location.file,
        ));
    }

    /// valid characters:
    /// abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$#
    fn is_valid_identifier_char(character: char) -> bool {
        matches!(
            character,
            '_' | '$' | '#' | ('a'..='z') | ('A'..='Z') | ('0'..='9') // TODO: generics / types: make TypeName::FunctionName use some custom token as :: instead of having that part of the type
        )
    }

    pub fn get_tokens(&self) -> &Vec<Token> {
        &self.tokens
    }

    pub fn to_parser(self) -> Parser {
        Parser::new(self.tokens)
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
