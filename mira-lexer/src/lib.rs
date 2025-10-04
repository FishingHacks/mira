use mira_context::SharedCtx;
use mira_spans::{BytePos, SourceFile, Span, SpanData};
use std::str::FromStr;
use std::sync::Arc;

mod error;
pub mod token;
pub use error::{LexingError, LexingErrorDiagnosticsExt, LexingErrorEmitterExt};
pub use token::{Literal, NumberType, Token, TokenType};
#[macro_use]
mod quote;

#[doc(hidden)]
pub fn dummy_token(tokens: &mut Vec<Token<'static>>, ty: TokenType) {
    tokens.push(Token::new(ty, None, Span::DUMMY));
}

pub struct Lexer<'arena> {
    source: Vec<char>,
    pub file: Arc<SourceFile>,
    tokens: Vec<Token<'arena>>,
    start: usize,
    current: usize,
    ctx: SharedCtx<'arena>,
}

impl<'arena> Lexer<'arena> {
    pub fn new(ctx: SharedCtx<'arena>, file: Arc<SourceFile>) -> Self {
        Self {
            source: file.source.chars().collect(),
            file,
            start: 0,
            current: 0,
            tokens: vec![],
            ctx,
        }
    }

    pub fn into_tokens(self) -> Vec<Token<'arena>> {
        self.tokens
    }

    pub fn scan_tokens(&mut self) -> Result<(), Vec<LexingError<'arena>>> {
        let mut errors = vec![];
        while !self.is_at_end() {
            self.start = self.current;
            if let Err(e) = self.scan_token() {
                errors.push(e);
            }
        }

        self.tokens.push(Token {
            ty: TokenType::Eof,
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

    fn peek2(&self) -> char {
        self.source.get(self.current + 1).copied().unwrap_or('\0')
    }

    fn peek3(&self) -> char {
        self.source.get(self.current + 2).copied().unwrap_or('\0')
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

    fn scan_token(&mut self) -> Result<(), LexingError<'arena>> {
        let tok = self.int_scan_token()?;
        let Some(tok) = tok else { return Ok(()) };
        match tok.ty {
            TokenType::IdentifierLiteral if self.if_char_advance('!') => match &tok.literal {
                Some(Literal::String(sym)) => {
                    let span = self
                        .current_span()
                        .combine_with([tok.span], self.ctx.span_interner);
                    if **sym == "macro" {
                        self.tokens
                            .push(Token::new(TokenType::MacroDef, None, span));
                    } else {
                        self.tokens
                            .push(Token::new(TokenType::MacroInvocation, tok.literal, span));
                    }
                }
                _ => unreachable!(
                    "Token::IdentifierLiteral should always have a string literal value"
                ),
            },
            _ => self.tokens.push(tok),
        }
        Ok(())
    }

    fn int_scan_token(&mut self) -> Result<Option<Token<'arena>>, LexingError<'arena>> {
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
            '$' => token!(Dollar),
            '?' => token!(QuestionMark),
            '.' if self.if_char_advance('.') => token!(Range, RangeInclusive, '='),
            '.' if self.peek().is_ascii_digit() => self.parse_number('.'),
            '.' => token!(Dot),
            '+' => token!(Plus, PlusAssign, '='),
            '-' if self.peek().is_ascii_digit() || self.peek() == '.' => self.parse_number('-'),
            '-' if self.if_char_advance('=') => token!(MinusAssign),
            '-' if self.if_char_advance('>') => token!(ReturnType),
            '-' => token!(Minus),
            // doc comment (///) or doc block comment (/**). note that we have to filter out /(^ *\* *|)/, so that
            // /**
            //  * meow
            //  * meow
            //  */
            // gets parsed as "meow\nmeow"
            '/' if self.peek() == self.peek2() && matches!(self.peek(), '/' | '*') => {
                let now = self.current;
                let single_line = self.advance() == '/';
                self.advance();

                let mut s = String::new();
                self.parse_doc_comments(&mut s, single_line, false);
                let span = self.span_from(now);
                let comment = self.ctx.add_doc_comment(s.into_boxed_str());
                Ok(Token::new(
                    TokenType::DocComment,
                    Some(Literal::DocComment(comment)),
                    span,
                ))
            }
            // module doc comment is only allowed as the first token.
            // module doc comment (//!) or module doc block comment (/*!). note that we have to filter out /(^ *\* *|)/, so that
            // /*!
            //  * meow
            //  * meow
            //  */
            // gets parsed as "meow\nmeow"
            '/' if matches!(self.peek(), '/' | '*') && self.peek2() == '!' => {
                if self.tokens.len() > 1
                    || (!self.tokens.is_empty() && self.tokens[0].ty != TokenType::Eof)
                {
                    let now = self.current;
                    let single_line = self.advance() == '/';
                    self.advance();
                    self.parse_doc_comments(&mut String::new(), single_line, true);

                    return Err(LexingError::InvalidModuleDocComment(self.span_from(now)));
                }
                let single_line = self.advance() == '/';
                self.advance();

                let now = self.current;
                let mut s = String::new();
                self.parse_doc_comments(&mut s, single_line, true);
                let span = self.span_from(now);
                let comment = self.ctx.add_doc_comment(s.into_boxed_str());
                Ok(Token::new(
                    TokenType::ModuleDocComment,
                    Some(Literal::DocComment(comment)),
                    span,
                ))
            }
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
                self.skip_multi_line_comment();
                return Ok(None);
            }
            '/' if self.if_char_advance('/') => {
                self.skip_single_line_comment();
                return Ok(None);
            }
            '@' => token!(AnnotationIntroducer),
            ('0'..='9') => self.parse_number(c),
            '"' => self.parse_string('"'),
            '`' => {
                let mut tok = self.parse_string('`')?;
                tok.ty = TokenType::IdentifierLiteral;
                Ok(tok)
            }
            _ if Self::is_valid_identifier_char(c) && !c.is_ascii_digit() => {
                self.parse_identifier(c)
            }
            _ => {
                return Err(LexingError::UnknownTokenError {
                    span: self.current_span(),
                    char: c,
                });
            }
        }
        .map(Some)
    }

    fn skip_single_line_comment(&mut self) {
        while !self.is_at_end() && self.advance() != '\n' {}
    }

    fn skip_multi_line_comment(&mut self) {
        loop {
            if self.advance() == '*' && self.advance() == '/' {
                break;
            }
        }
    }

    fn skip_spaces(&mut self) -> usize {
        let mut i = 0;
        while matches!(self.peek(), ' ' | '\t' | '\n' | '\r') {
            i += 1;
            self.advance();
        }
        i
    }

    fn skip_spaces_max(&mut self, mut i: usize) {
        while i > 0 && matches!(self.peek(), ' ' | '\t') {
            i -= 1;
            self.advance();
        }
    }

    fn parse_multi_line_doc_comment(&mut self, s: &mut String) {
        self.skip_spaces();
        // skip the line if it's only `/**`.
        if self.peek() == '\n' {
            self.advance();
        }
        let mut num_spaces = self.skip_spaces();
        let has_star = self.peek() == '*' && self.peek2() != '/';
        if has_star {
            self.advance();
            num_spaces = self.skip_spaces();
        }
        loop {
            if has_star {
                self.skip_spaces();
                if self.peek() == '*' && self.peek2() != '/' {
                    self.advance();
                    self.skip_spaces_max(num_spaces);
                }
            }
            loop {
                match self.peek() {
                    '\n' => {
                        self.advance();
                        s.push('\n');
                        break;
                    }
                    '*' if self.peek2() == '/' => {
                        self.advance();
                        self.advance();
                        // /**
                        //  * meow
                        //  */
                        // should be parsed as "meow\n".
                        if !s.ends_with('\n') {
                            s.push('\n');
                        }
                        return;
                    }
                    _ if self.is_at_end() => return,
                    _ => s.push(self.advance()),
                }
            }
        }
    }

    fn parse_single_line_doc_comment(&mut self, s: &mut String, max_spaces: usize) {
        self.skip_spaces_max(max_spaces);
        while !self.is_at_end() && !self.if_char_advance('\n') {
            s.push(self.advance());
        }
        if self.is_at_end() {
            return;
        }
        s.push('\n');
    }

    fn parse_doc_comments(&mut self, s: &mut String, mut single_line: bool, module: bool) {
        let mut max_spaces = None;
        loop {
            if single_line {
                let max_spaces = match max_spaces {
                    Some(v) => v,
                    None => {
                        let spaces = self.skip_spaces();
                        max_spaces = Some(spaces);
                        spaces
                    }
                };
                self.parse_single_line_doc_comment(s, max_spaces);
            } else {
                self.parse_multi_line_doc_comment(s);
            }
            if self.is_at_end() {
                return;
            }
            // look for /// or /**
            loop {
                self.skip_spaces();
                if self.peek() == '/' && self.peek2() == '/' {
                    if module {
                        if self.peek3() == '/' {
                            return;
                        }
                    } else if self.peek3() == '!' {
                        return;
                    }
                    self.advance();
                    self.advance();
                    // /// was found, continue to the start of the loop
                    if self.peek() == '/' || self.peek() == '!' {
                        self.advance();
                        single_line = true;
                        break;
                    }
                    // normal comment, skip it
                    self.skip_single_line_comment();
                    continue;
                }
                // /* was found, skip normal comment
                if self.peek() == '/' && self.peek2() == '*' {
                    if module {
                        if self.peek3() == '*' {
                            return;
                        }
                    } else if self.peek3() == '!' {
                        return;
                    }
                    self.advance();
                    self.advance();

                    // /** was found, invoke the multiline parsing procedure
                    if self.peek() == '*' || self.peek() == '!' {
                        self.advance();
                        single_line = false;
                        break;
                    }
                    // normal comment, skip it
                    self.skip_multi_line_comment();
                    continue;
                }
                // if no /// was found, return
                return;
            }
        }
    }

    #[inline(always)]
    fn get_token(&self, token: TokenType) -> Token<'arena> {
        Token::new(token, None, self.span(token.char_len().unwrap()))
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
    ) -> Result<Token<'arena>, LexingError<'arena>> {
        let mut ty = String::from(first_char);
        let first_type_char = self.current - 1;

        loop {
            match self.peek() {
                'a'..='z' | '0'..='9' => ty.push(self.advance()),
                _ => {
                    let err = LexingError::InvalidNumberType(self.span_from(first_type_char));
                    let Ok(number_type) = NumberType::from_str(&ty) else {
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
    ) -> Result<Token<'arena>, LexingError<'arena>> {
        let mut value: u64 = 0;

        loop {
            match self.peek() {
                '0'..='9' => value = (value << 4) | (self.advance() as u64 - '0' as u64),
                'a'..='f' => value = (value << 4) | (self.advance() as u64 - 'a' as u64 + 0xa),
                'A'..='F' => value = (value << 4) | (self.advance() as u64 - 'A' as u64 + 0xa),
                '_' => _ = self.advance(),
                c if Self::is_valid_identifier_char(c) => {
                    self.advance();
                    return self.parse_numtype(start_bytepos, c, value, is_negative, false);
                }
                _ if is_negative => {
                    return Ok(Token::new(
                        TokenType::SIntLiteral,
                        Some(Literal::SInt(-(value as i64), NumberType::None)),
                        self.span_from(start_bytepos),
                    ));
                }
                _ => {
                    return Ok(Token::new(
                        TokenType::UIntLiteral,
                        Some(Literal::UInt(value, NumberType::None)),
                        self.span_from(start_bytepos),
                    ));
                }
            }
        }
    }

    fn parse_bin(
        &mut self,
        start_bytepos: usize,
        is_negative: bool,
    ) -> Result<Token<'arena>, LexingError<'arena>> {
        let mut value: u64 = 0;

        loop {
            match self.peek() {
                '0' | '1' => value = (value << 1) | (self.advance() as u64 - '0' as u64),
                '2'..='9' => {
                    self.advance();
                    return Err(LexingError::InvalidNumberError(
                        self.span_from(start_bytepos),
                    ));
                }
                '_' => _ = self.advance(),
                c if Self::is_valid_identifier_char(c) => {
                    self.advance();
                    return self.parse_numtype(start_bytepos, c, value, is_negative, false);
                }
                _ if is_negative => {
                    return Ok(Token::new(
                        TokenType::SIntLiteral,
                        Some(Literal::SInt(-(value as i64), NumberType::None)),
                        self.span_from(start_bytepos),
                    ));
                }
                _ => {
                    return Ok(Token::new(
                        TokenType::UIntLiteral,
                        Some(Literal::UInt(value, NumberType::None)),
                        self.span_from(start_bytepos),
                    ));
                }
            }
        }
    }

    fn parse_oct(
        &mut self,
        start_bytepos: usize,
        is_negative: bool,
    ) -> Result<Token<'arena>, LexingError<'arena>> {
        let mut value: u64 = 0;

        loop {
            match self.peek() {
                '0'..='7' => value = (value << 3) | (self.advance() as u64 - '0' as u64),
                '8' | '9' => {
                    self.advance();
                    return Err(LexingError::InvalidNumberError(
                        self.span_from(start_bytepos),
                    ));
                }
                '_' => _ = self.advance(),
                c if Self::is_valid_identifier_char(c) => {
                    self.advance();
                    return self.parse_numtype(start_bytepos, c, value, is_negative, false);
                }
                _ if is_negative => {
                    return Ok(Token::new(
                        TokenType::SIntLiteral,
                        Some(Literal::SInt(-(value as i64), NumberType::None)),
                        self.span_from(start_bytepos),
                    ));
                }
                _ => {
                    return Ok(Token::new(
                        TokenType::UIntLiteral,
                        Some(Literal::UInt(value, NumberType::None)),
                        self.span_from(start_bytepos),
                    ));
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

    fn parse_number(&mut self, mut first_char: char) -> Result<Token<'arena>, LexingError<'arena>> {
        let start_byte = self.current - 1;
        let is_negative = first_char == '-';
        let mut is_float = false;
        if is_negative {
            first_char = self.advance();
        }

        if first_char == '0' && self.peek() == 'x' {
            self.advance();
            if !self.peek().is_ascii_hexdigit() {
                self.advance();
                return Err(LexingError::InvalidNumberError(self.span_from(start_byte)));
            }
            return self.parse_hex(start_byte, is_negative);
        } else if first_char == '0' && self.peek() == 'b' {
            self.advance();
            if !matches!(self.peek(), '0' | '1') {
                self.advance();
                return Err(LexingError::InvalidNumberError(self.span_from(start_byte)));
            }
            return self.parse_bin(start_byte, is_negative);
        } else if first_char == '0' && self.peek() == 'o' {
            self.advance();
            if !matches!(self.peek(), '0'..='7') {
                self.advance();
                return Err(LexingError::InvalidNumberError(self.span_from(start_byte)));
            }
            return self.parse_oct(start_byte, is_negative);
        }

        let mut str = String::new();
        let mut ty = String::new();

        if is_negative {
            str.push('-');
        }
        if first_char == '.' {
            str.push('0');
            is_float = true;
        }
        str.push(first_char);

        while !self.is_at_end() {
            if !ty.is_empty() {
                if self.peek().is_ascii_alphanumeric() {
                    ty.push(self.advance());
                    continue;
                }
                break;
            }

            match self.peek() {
                '0'..='9' => str.push(self.advance()),
                '_' => _ = self.advance(),
                '.' if self.peek2().is_ascii_digit() => {
                    if is_float {
                        self.skip_to_after_number();
                        return Err(LexingError::InvalidNumberError(self.span_from(start_byte)));
                    }
                    is_float = true;
                    str.push(self.advance());
                }
                c if Self::is_valid_identifier_char(c) => ty.push(self.advance()),
                _ => break,
            }
        }
        let number_type = match NumberType::from_str(&ty) {
            Ok(v @ (NumberType::F32 | NumberType::F64)) => v,
            Ok(v @ (NumberType::U8 | NumberType::U16 | NumberType::U32 | NumberType::U64))
                if !is_negative && !is_float =>
            {
                v
            }
            Ok(v) if !is_float => v,
            Err(_) if ty.is_empty() => NumberType::None,
            _ => {
                return Err(LexingError::InvalidNumberType(self.span_from(start_byte)));
            }
        };

        let (lit, tok) = if is_float {
            let num = match str.parse::<f64>() {
                Ok(num) => num,
                Err(..) => {
                    return Err(LexingError::InvalidNumberError(self.span_from(start_byte)));
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

    // \xHH
    fn parse_xhh_escape(&mut self, string_char: char) -> Result<char, LexingError<'arena>> {
        let part1 = match self.advance() {
            '\0' => return Err(LexingError::UnclosedString(self.current_span())),
            c @ '0'..='9' => c as u8 - b'0',
            c @ 'a'..='f' => c as u8 - b'a' + 0xa,
            c @ 'A'..='F' => c as u8 - b'A' + 0xa,
            c if c == string_char => {
                return Err(LexingError::UnterminatedNumericEscape(self.span(3)));
            }
            c => {
                return Err(LexingError::InvalidNumericEscapeChar(
                    self.current_span(),
                    c,
                ));
            }
        };
        let part2 = match self.advance() {
            '\0' => return Err(LexingError::UnclosedString(self.current_span())),
            c @ '0'..='9' => c as u8 - b'0',
            c @ 'a'..='f' => c as u8 - b'a' + 0xa,
            c @ 'A'..='F' => c as u8 - b'A' + 0xa,
            c if c == string_char => {
                return Err(LexingError::UnterminatedNumericEscape(self.span(3)));
            }
            c => {
                return Err(LexingError::InvalidNumericEscapeChar(
                    self.current_span(),
                    c,
                ));
            }
        };
        match char::from_u32(((part1 << 4) | part2) as u32) {
            Some(c) if c.is_ascii() => Ok(c),
            _ => Err(LexingError::OutOfRangeEscape(self.span(4), 0..=0x7f)),
        }
    }

    fn parse_unicode_escape(&mut self, string_char: char) -> Result<char, LexingError<'arena>> {
        // the start of the sequence (current character - 1) to include the '\'
        let start = self.current - 1;

        match self.advance() {
            '\0' => return Err(LexingError::UnclosedString(self.current_span())),
            '{' => {}
            _ => return Err(LexingError::InvalidUnicodeEscape(self.span_from(start))),
        }
        // to track the amount of characters inside { ... }, which cannot be more than 10.
        let mut char_count = 0;
        let mut num = 0;
        loop {
            match self.advance() {
                '}' => break,
                '\0' => return Err(LexingError::UnclosedString(self.current_span())),
                c @ '0'..='9' => num = (num << 4) | (c as u8 - b'0') as u32,
                c @ 'a'..='f' => num = (num << 4) | (c as u8 - b'a' + 0xa) as u32,
                c @ 'A'..='F' => num = (num << 4) | (c as u8 - b'A' + 0xa) as u32,
                c if c == string_char => {
                    return Err(LexingError::UnterminatedUnicodeEscape(
                        self.span_from(start),
                    ));
                }
                _ => return Err(LexingError::InvalidUnicodeEscapeChar(self.current_span())),
            }
            char_count += 1;
        }
        if char_count == 0 {
            return Err(LexingError::EmptyUnicodeEscapeSequence(
                self.span_from(start),
            ));
        }
        if char_count > 6 {
            return Err(LexingError::TooLongUnicodeEscapeSequence(
                self.span_from(start),
            ));
        }
        match char::from_u32(num) {
            Some(c) => Ok(c),
            None => Err(LexingError::OutOfRangeUnicodeEscapeSequence(
                self.span_from(start),
            )),
        }
    }

    fn parse_string(&mut self, string_char: char) -> Result<Token<'arena>, LexingError<'arena>> {
        let mut is_backslash = false;
        let mut s = String::new();
        let start = self.current - 1;

        while !self.is_at_end() {
            let c = self.advance();

            if c == '\n' || c == '\r' {
                return Err(LexingError::UnclosedString(self.current_span()));
            } else if is_backslash {
                is_backslash = false;
                match c {
                    'n' => s.push('\n'),
                    '0' => s.push('\0'),
                    'r' => s.push('\r'),
                    't' => s.push('\t'),
                    'e' => s.push('\x1b'),
                    'b' => s.push('\x08'),
                    '\\' => s.push('\\'),
                    '\'' => s.push('\''),
                    '"' => s.push('"'),
                    c if string_char == c => s.push(c),
                    'x' => s.push(self.parse_xhh_escape(string_char)?),
                    'u' => s.push(self.parse_unicode_escape(string_char)?),
                    '\n' => loop {
                        match self.peek() {
                            '\0' => return Err(LexingError::UnclosedString(self.current_span())),
                            // skip all whitespace characters
                            ' ' | '\n' | '\r' | '\x09' => _ = self.advance(),
                            _ => break,
                        }
                    },
                    _ => return Err(LexingError::InvalidEscape(self.current_span(), c)),
                }
            } else if c == '\\' {
                is_backslash = true;
            } else if c == string_char || c == '\n' {
                break;
            } else {
                s.push(c);
            }
        }
        if self.cur_char() != string_char || self.source[self.current - 2] == '\\' {
            return Err(LexingError::UnclosedString(self.current_span()));
        }

        Ok(Token::new(
            TokenType::StringLiteral,
            Some(Literal::String(self.ctx.intern_str(&s))),
            self.span_from(start),
        ))
    }

    fn parse_identifier(
        &mut self,
        starting_char: char,
    ) -> Result<Token<'arena>, LexingError<'arena>> {
        let mut identifier = String::new();
        identifier.push(starting_char);
        let start = self.current - 1;

        while !self.is_at_end() {
            if !Self::is_valid_identifier_char(self.peek()) {
                break;
            }
            identifier.push(self.advance());
        }
        Ok(match identifier.as_str() {
            "true" => Token::new(
                TokenType::BooleanLiteral,
                Some(Literal::Bool(true)),
                self.span_from(start),
            ),
            "false" => Token::new(
                TokenType::BooleanLiteral,
                Some(Literal::Bool(false)),
                self.span_from(start),
            ),
            "void" => self.get_token(TokenType::VoidLiteral),
            ident => Self::try_token_from_keyword(ident)
                .map(|v| self.get_token(v))
                .unwrap_or_else(|| {
                    Token::new(
                        TokenType::IdentifierLiteral,
                        Some(Literal::String(self.ctx.intern_str(ident))),
                        self.span_from(start),
                    )
                }),
        })
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
}

#[cfg(test)]
mod test {
    use mira_context::GlobalCtx;
    use mira_spans::Arena;

    use super::*;

    fn check_tokens(tokens: &[Token<'_>]) {
        for token in tokens {
            match (&token.ty, &token.literal) {
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
                    panic!("invalid literal {:?} for {:?}", token.literal, token.ty)
                }
                _ => (),
            }
        }
    }

    fn get_tokens<'arena>(
        ctx: SharedCtx<'arena>,
        src: &str,
    ) -> (Vec<Token<'arena>>, Vec<LexingError<'arena>>) {
        let mut tokenizer = Lexer::new(ctx, ctx.source_map.testing_new_file(src.into()));
        let errs = tokenizer.scan_tokens().err().unwrap_or_default();
        check_tokens(tokenizer.get_tokens());
        (tokenizer.tokens, errs)
    }

    fn assert_token_eq(
        src: &str,
        expected_tokens: &[(TokenType, Option<Literal<'_>>)],
        ctx: SharedCtx<'_>,
    ) {
        let eof_token = (TokenType::Eof, None);
        let (tokens, errs) = get_tokens(ctx, src);
        println!("{tokens:?}");
        assert_eq!(errs.len(), 0, "unexpected errors: {errs:?}");
        assert_eq!(tokens.len(), expected_tokens.len() + 1 /* eof token */);
        for (tok, expected) in tokens
            .iter()
            .zip(expected_tokens.iter().chain(std::iter::once(&eof_token)))
        {
            let literals_eq = (tok.literal.is_none() && expected.1.is_none())
                || (tok
                    .literal
                    .unwrap()
                    .doc_comment_as_symbol_eq(expected.1.as_ref().unwrap(), ctx));
            if tok.ty != expected.0 || !literals_eq {
                if let Some(Literal::DocComment(left)) = tok.literal {
                    ctx.with_doc_comment(left, |s| println!("Left: {s:?}"));
                }
                if let Some(Literal::DocComment(right)) = expected.1 {
                    ctx.with_doc_comment(right, |s| println!("Right: {s:?}"));
                }
                panic!(
                    "mismatching tokens\n  left: {tokens:?}\n  right: {expected_tokens:?}\n\n{tok:?} - {expected:?}"
                );
            }
        }
    }

    macro_rules! match_errs {
        ($str: expr; $($pat:pat),* $(,)?) => {
            let mut i = 0;
            let arena = Arena::new();
            let gtx = GlobalCtx::new_test_noemit(&arena);
            let ctx = gtx.share();
            let (_, errs) = get_tokens(ctx, $str);
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
        ($ctx:expr, IdentifierLiteral, $lit:ident) => {
            (
                TokenType::IdentifierLiteral,
                Some(Literal::String($ctx.intern_str(stringify!($lit)))),
            )
        };
        ($ty:ident) => {
            (TokenType::$ty, None)
        };
        ($ctx:expr, $ty: ident, $lit:ident($val:expr)) => {
            (TokenType::$ty, Some(Literal::$lit($ctx.intern_str($val))))
        };
        ($ty: ident, $lit:ident($val:expr, _)) => {
            (TokenType::$ty, Some(Literal::$lit($val, NumberType::None)))
        };
    }

    #[test]
    fn test_strings() {
        let arena = Arena::new();
        let gtx = GlobalCtx::new_test_noemit(&arena);
        let ctx = gtx.share();
        assert_token_eq(
            r#"
"a b c";
"a\n\n\\t";
"a\t3";
            "#,
            &[
                tok!(ctx, StringLiteral, String("a b c")),
                tok!(Semicolon),
                tok!(ctx, StringLiteral, String("a\n\n\\t")),
                tok!(Semicolon),
                tok!(ctx, StringLiteral, String("a\t3")),
                tok!(Semicolon),
            ],
            ctx,
        );

        match_errs!("\"a\nb\nc\";"; LexingError::UnclosedString(_), LexingError::UnclosedString(_));
    }

    #[test]
    fn test_idents() {
        let arena = Arena::new();
        let gtx = GlobalCtx::new_test_noemit(&arena);
        let ctx = gtx.share();
        assert_token_eq(
            "jkhdfgkjhdf",
            &[tok!(ctx, IdentifierLiteral, jkhdfgkjhdf)],
            ctx,
        );
        assert_token_eq("_Zn3Meow", &[tok!(ctx, IdentifierLiteral, _Zn3Meow)], ctx);
        assert_token_eq(
            "_3$5#12_mow",
            &[tok!(ctx, IdentifierLiteral, String("_3$5#12_mow"))],
            ctx,
        );
        match_errs!("1289hjdsjhfgdfg_meow"; LexingError::InvalidNumberType(_));
    }

    #[test]
    fn test_numbers() {
        let arena = Arena::new();
        let gtx = GlobalCtx::new_test_noemit(&arena);
        let ctx = gtx.share();
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
            ctx,
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
            ctx,
        );

        assert_token_eq(
            "0b1101; -0b101",
            &[
                tok!(UIntLiteral, UInt(0b1101, _)),
                tok!(Semicolon),
                tok!(SIntLiteral, SInt(-(0b101), _)),
            ],
            ctx,
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
            ctx,
        );

        assert_token_eq(
            "0o5; -0o42",
            &[
                tok!(UIntLiteral, UInt(0o5, _)),
                tok!(Semicolon),
                tok!(SIntLiteral, SInt(-0o42, _)),
            ],
            ctx,
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
            ctx,
        );
    }

    #[test]
    fn test_doc_comments() {
        let source = r#"
/**
 *  This is a multiline comment
    With spaces
*   and other stuff
*/
/// Nya
/**
 * Meow
**/
meow
        "#;

        let arena = Arena::new();
        let gtx = GlobalCtx::new_test_noemit(&arena);
        let ctx = gtx.share();
        assert_token_eq(
            source,
            &[
                tok!(
                    ctx,
                    DocComment,
                    String(
                        "This is a multiline comment\nWith spaces\n and other stuff\nNya\nMeow\n"
                    )
                ),
                tok!(ctx, IdentifierLiteral, String("meow")),
            ],
            ctx,
        );
    }
}
