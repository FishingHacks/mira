use crate::globals::GlobalString;

#[derive(Clone, Copy, Debug)]
pub enum TokenType {
    Let,                     // done
    Const,                   // done
    Equals,                  // done
    NotEquals,               // done
    LessThan,                // done
    GreaterThan,             // done
    LessThanEquals,          // done
    GreaterThanEquals,       // done
    LogicalAnd,              // done
    LogicalOr,               // done
    StringLiteral,           // done
    NumberLiteral,           // done
    IdentifierLiteral,       // done
    AssignValue,             // done
    AssignTypeOrStructValue, // done
    Semicolon,               // done
    ParenLeft,               // done
    ParenRight,              // done
    CurlyLeft,               // done
    CurlyRight,              // done
    BracketLeft,             // done
    BracketRight,            // done
    Plus,                    // done
    Minus,                   // done
    Multiply,                // done
    Divide,                  // done
    Modulo,                  // done
    IntegerDivide,           // done
    BitwiseNot,              // done
    BitwiseAnd,              // done
    BitwiseOr,               // done
    BitwiseXor,              // done
    Return,                  // done
    If,                      // done
    While,                   // done
    For,                     // done
    In,                      // done
    Range,                   // done
    RangeInclusive,          // done
    Spread,                  // done
    Typeof,                  // done
    ReturnType,              // done
    Trait,                   // done
    Impl,                    // done
    Comma,                   // done
    Copy,                    // done
    PlusAssign,              // done
    MinusAssign,             // done
    DivideAssign,            // done
    MultiplyAssign,          // done
    BitwiseAndAssign,        // done
    BitwiseOrAssign,         // done
    BitwiseXorAssign,        // done
    Dot,                     // done
    Eof,                     // done
}

#[derive(Clone, Debug)]
pub enum Literal {
    Number(f64),
    String(Box<str>),
}

#[derive(Clone, Debug)]
pub struct Token {
    pub typ: TokenType,
    pub literal: Option<Literal>,
    pub line_number: usize,
    pub file: GlobalString,
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
            file,
            line_number,
            literal,
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

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>, Vec<String>> {
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
            line_number: self.line,
            file: self.file,
        });

        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(self.tokens.clone())
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

    fn scan_token(&mut self) -> Result<(), String> {
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
                    } else if self.if_char_advance('.') {
                        token!(Spread); // ...
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
            '%' => token!(Modulo),
            '*' => token!(Multiply, MultiplyAssign, '='),
            '=' => token!(AssignValue, Equals, '='),
            '<' => token!(LessThan, LessThanEquals, '='),
            '>' => token!(GreaterThan, GreaterThanEquals, '='),
            ':' => token!(AssignTypeOrStructValue),
            ';' => token!(Semicolon),
            '!' => token!(BitwiseNot, NotEquals, '='),
            '&' => {
                if self.if_char_advance('=') {
                    token!(BitwiseAndAssign);
                } else if self.if_char_advance('&') {
                    token!(LogicalAnd);
                } else {
                    token!(BitwiseAnd);
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
            c @ ('0'..='9') => return self.parse_number(c),
            '"' => return self.parse_string(),
            c @ _ if Self::is_valid_identifier_char(c) => self.parse_identifier(c),
            _ => return Err(format!("Unknown Token: {c}")),
        }
        Ok(())
    }

    fn add_token(&mut self, token: TokenType) {
        self.tokens
            .push(Token::new(token, None, self.line, self.file));
    }

    fn parse_number(&mut self, first_char: char) -> Result<(), String> {
        let mut str = String::with_capacity(5);
        if first_char == '.' {
            str.push('0');
        }
        str.push(first_char);

        while !self.is_at_end() {
            if matches!(self.peek(), '.' | ('0'..='9')) {
                str.push(self.advance())
            } else {
                break;
            }
        }

        let num = match str.parse::<f64>() {
            Ok(num) => num,
            Err(e) => return Err(format!("{e:?}")),
        };
        self.add_token_lit(TokenType::NumberLiteral, Literal::Number(num));

        Ok(())
    }

    fn parse_string(&mut self) -> Result<(), String> {
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
            return Err(format!(
                "{}:{}: Expected \", but found nothing",
                self.file, self.line,
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
                if let Some(typ) = Self::try_token_from_keyword(&identifier) {
                    self.add_token(typ);
                } else {
                    self.add_token_lit(
                        TokenType::IdentifierLiteral,
                        Literal::String(identifier.into()),
                    );
                }
                break;
            }
        }
    }

    fn try_token_from_keyword(word: &str) -> Option<TokenType> {
        match word {
            "let" => Some(TokenType::Let),
            "const" => Some(TokenType::Const),
            "idiv" => Some(TokenType::IntegerDivide),
            "return" => Some(TokenType::Return),
            "if" => Some(TokenType::If),
            "while" => Some(TokenType::While),
            "for" => Some(TokenType::For),
            "in" => Some(TokenType::In),
            "typeof" => Some(TokenType::Typeof),
            "trait" => Some(TokenType::Trait),
            "impl" => Some(TokenType::Impl),
            "copy" => Some(TokenType::Copy),
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
        matches!(character, '_' | '$' | '#' | ('a'..='z') | ('A'..='Z'))
    }
}
