use std::fmt::{Display, Write};

use crate::{
    error::ProgrammingLangTokenizationError,
    globals::GlobalString,
    parser::{LiteralValue, Parser},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenType {
    // Tokenization, Parsing ; *: no types
    Let,                     // done, done
    Const,                   // done, done
    Equals,                  // done, done
    NotEquals,               // done, done
    LessThan,                // done, done
    GreaterThan,             // done, done
    LessThanEquals,          // done, done
    GreaterThanEquals,       // done, done
    LogicalNot,              // done, done
    LogicalAnd,              // done, done
    LogicalOr,               // done, done
    StringLiteral,           // done, done
    NumberLiteral,           // done, done
    BooleanLiteral,          // done, done
    NullLiteral,             // done, done
    IdentifierLiteral,       // done, done
    AssignValue,             // done, done
    AssignTypeOrStructValue, // done, done*
    Semicolon,               // done, done
    ParenLeft,               // done, done
    ParenRight,              // done, done
    CurlyLeft,               // done, done
    CurlyRight,              // done, done
    BracketLeft,             // done, done
    BracketRight,            // done, done
    Plus,                    // done, done
    Minus,                   // done, done
    MultiplyOrDeref,         // done, done
    Divide,                  // done, done
    Modulo,                  // done, done
    ModuloAssign,            // done, done
    IntegerDivide,           // done, done
    BitwiseNot,              // done, done
    BitwiseAndOrReference,   // done, done
    BitwiseOr,               // done, done
    BitwiseXor,              // done, done
    Return,                  // done, done
    Fn,                      // done, done
    Extern,
    If,                      // done, done
    Else,                    // done, done
    While,                   // done, done
    For,                     // done, done
    In,                      // done, done
    Range,                   // done, done
    RangeInclusive,          // done, done
    ReturnType,              // done, *
    Struct,                  // done,
    Trait,                   // done, *
    Impl,                    // done,
    Comma,                   // done, done
    PlusAssign,              // done, done
    MinusAssign,             // done, done
    DivideAssign,            // done, done
    MultiplyAssign,          // done, done
    BitwiseAndAssign,        // done, done
    BitwiseOrAssign,         // done, done
    BitwiseXorAssign,        // done, done
    Dot,                     // done, done
    As,                      // done, *
    QuestionMark,            // done, *
    Eof,                     // done, done
}

#[derive(Clone, Debug)]
pub enum Literal {
    Number(f64),
    String(Box<str>),
    Null,
    Bool(bool),
}

#[derive(Debug, Clone, Copy)]
pub struct Location {
    pub line: usize,
    pub file: GlobalString,
    pub column: usize,
}

impl Location {
    pub fn new(file: GlobalString, line: usize, column: usize) -> Self {
        Self { column, file, line }
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.file, f)?;
        f.write_char(':')?;
        Display::fmt(&self.line, f)?;
        f.write_char(':')?;
        Display::fmt(&self.column, f)
    }
}

#[macro_export]
macro_rules! loc {
    ($file:expr;$line:expr) => {
        $crate::tokenizer::Location::new($file, $line, 0)
    };
    ($file:expr;$line:expr;$column:expr) => {
        $crate::tokenizer::Location::new($file, $line, $column)
    };
}
pub(crate) use loc;

#[derive(Clone, Debug)]
pub struct Token {
    pub typ: TokenType,
    pub literal: Option<Literal>,
    pub location: Location,
}

impl Token {
    pub fn new(
        typ: TokenType,
        literal: Option<Literal>,
        line_number: usize,
        file: GlobalString,
    ) -> Self {
        Self {
            typ,
            location: loc!(file;line_number),
            literal,
        }
    }

    pub fn to_literal_value(&self) -> Option<LiteralValue> {
        match self.typ {
            TokenType::StringLiteral
            | TokenType::NullLiteral
            | TokenType::BooleanLiteral
            | TokenType::NumberLiteral => self.literal.as_ref().map(|v| match v {
                Literal::Null => LiteralValue::Null,
                Literal::Bool(b) => LiteralValue::Bool(*b),
                Literal::Number(n) => LiteralValue::Number(*n),
                Literal::String(s) => LiteralValue::String(s.clone()),
            }),
            TokenType::IdentifierLiteral => {
                if let Some(lit) = &self.literal {
                    return match &lit {
                        Literal::String(v) => Some(LiteralValue::Dynamic(v.clone())),
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
    file: GlobalString,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

impl Tokenizer {
    pub fn new<I: Into<GlobalString>>(source: &str, file: I) -> Self {
        Self {
            source: source.chars().collect(),
            file: file.into(),
            start: 0,
            current: 0,
            tokens: vec![],
            line: 0,
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
        self.current += 1;
        if self.current > 1 && self.source[self.current - 2] == '\n' {
            self.line += 1;
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
                if self.if_char_advance('=') {
                    token!(MinusAssign);
                } else if self.if_char_advance('>') {
                    token!(ReturnType);
                } else {
                    token!(Minus);
                }
            }
            '/' if self.peek() != '/' => token!(Divide, DivideAssign, '='),
            '%' => token!(Modulo, ModuloAssign, '='),
            '*' => token!(MultiplyOrDeref, MultiplyAssign, '='),
            '=' => token!(AssignValue, Equals, '='),
            '<' => token!(LessThan, LessThanEquals, '='),
            '>' => token!(GreaterThan, GreaterThanEquals, '='),
            ':' => token!(AssignTypeOrStructValue),
            ';' => token!(Semicolon),
            '!' => token!(LogicalNot, NotEquals, '='),
            '~' => token!(BitwiseNot),
            '&' => {
                if self.if_char_advance('=') {
                    token!(BitwiseAndAssign);
                } else if self.if_char_advance('&') {
                    token!(LogicalAnd);
                } else {
                    token!(BitwiseAndOrReference);
                }
            }
            '|' => {
                if self.if_char_advance('=') {
                    token!(BitwiseOrAssign);
                } else if self.if_char_advance('|') {
                    token!(LogicalOr);
                } else {
                    token!(BitwiseOr);
                }
            }
            '^' => token!(BitwiseXor, BitwiseXorAssign, '='),
            ' ' | '\n' | '\r' | '\t' => (),
            '/' if self.if_char_advance('/') => {
                while !self.is_at_end() && self.advance() != '\n' {}
            }
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
        self.tokens
            .push(Token::new(token, None, self.line, self.file));
    }

    fn parse_number(&mut self, first_char: char) -> Result<(), ProgrammingLangTokenizationError> {
        let mut str = String::with_capacity(5);
        if first_char == '.' {
            str.push('0');
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
                str.push(self.advance());
            } else {
                break;
            }
        }

        let num = match str.parse::<f64>() {
            Ok(num) => num,
            Err(..) => {
                return Err(ProgrammingLangTokenizationError::invalid_number(
                    loc!(self.file;self.line),
                ))
            }
        };
        self.add_token_lit(TokenType::NumberLiteral, Literal::Number(num));

        Ok(())
    }

    fn parse_string(&mut self) -> Result<(), ProgrammingLangTokenizationError> {
        let mut is_backslash = false;
        let mut str = String::with_capacity(30);

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

        self.add_token_lit(
            TokenType::StringLiteral,
            Literal::String(str.into_boxed_str()),
        );
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

        while !self.is_at_end() {
            if Self::is_valid_identifier_char(self.peek()) {
                identifier.push(self.advance());
            } else {
                let next_char = self.peek();
                let char_after_next = self.source.get(self.current + 1).map(|c| *c).unwrap_or('\0');
                if next_char == ':' && char_after_next == ':' {
                    identifier.push_str("::");
                    self.advance();
                    self.advance();
                    continue;
                }
                break;
            }
        }
        match identifier.as_str() {
            "null" => return self.add_token(TokenType::NullLiteral),
            "true" => return self.add_token_lit(TokenType::BooleanLiteral, Literal::Bool(true)),
            "false" => return self.add_token_lit(TokenType::BooleanLiteral, Literal::Bool(false)),
            _ => (),
        }
        if let Some(typ) = Self::try_token_from_keyword(&identifier) {
            return self.add_token(typ);
        }
        self.add_token_lit(
            TokenType::IdentifierLiteral,
            Literal::String(identifier.into()),
        );
    }

    fn try_token_from_keyword(word: &str) -> Option<TokenType> {
        match word {
            "let" => Some(TokenType::Let),
            "const" => Some(TokenType::Const),
            "as" => Some(TokenType::As),
            "fn" => Some(TokenType::Fn),
            "extern" => Some(TokenType::Extern),
            "return" => Some(TokenType::Return),
            "if" => Some(TokenType::If),
            "else" => Some(TokenType::Else),
            "while" => Some(TokenType::While),
            "for" => Some(TokenType::For),
            "in" => Some(TokenType::In),
            "idiv" => Some(TokenType::IntegerDivide),
            "struct" => Some(TokenType::Struct),
            "impl" => Some(TokenType::Impl),
            "trait" => Some(TokenType::Trait),
            _ => None,
        }
    }

    fn add_token_lit(&mut self, token: TokenType, literal: Literal) {
        self.tokens
            .push(Token::new(token, Some(literal), self.line, self.file));
    }

    // valid characters:
    // abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$#
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
