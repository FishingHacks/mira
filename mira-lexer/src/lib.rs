use mira_context::SharedCtx;
use mira_spans::{BytePos, SourceFile, Span, SpanData};
use std::str::{Chars, FromStr};
use std::sync::Arc;

mod error;
pub mod token;
pub use error::{LexingError, LexingErrorDiagnosticsExt, LexingErrorEmitterExt};
pub use token::{Literal, NumberType, Token, TokenType};

use crate::token::{Delimiter, TokenTree};
#[macro_use]
mod quote;

#[doc(hidden)]
pub fn dummy_token(tokens: &mut Vec<Token<'static>>, ty: TokenType) {
    tokens.push(Token::new(ty, None, Span::DUMMY));
}

pub struct Cursor<'src> {
    chars: Chars<'src>,
    peeks: [Option<char>; 3],
    pos: usize,
}

impl<'src> Cursor<'src> {
    fn amount_peeks(&self) -> u8 {
        match self.peeks {
            [Some(_), Some(_), Some(_)] => 3,
            [Some(_), Some(_), None] => 2,
            [Some(_), None, _] => 1,
            [None, _, _] => 0,
        }
    }
    /// Returns the return value of amount_peeks after this function ran.
    fn _peek(&mut self, amount: u8) -> Option<u8> {
        assert!(amount <= 3);
        let mut amount_peeks = self.amount_peeks();
        if amount_peeks >= amount {
            return Some(amount_peeks);
        }
        while amount_peeks < amount {
            let c = self.chars.next()?;
            self.peeks[amount_peeks as usize] = Some(c);
            amount_peeks += 1;
        }
        Some(amount_peeks)
    }
    pub fn new(chars: Chars<'src>) -> Self {
        Self {
            chars,
            peeks: [None; 3],
            pos: 0,
        }
    }
    pub fn peek(&mut self) -> Option<char> {
        let amount_peeks = self._peek(1)?;
        assert!(amount_peeks >= 1);
        self.peeks[0]
    }
    pub fn peek2(&mut self) -> Option<char> {
        let amount_peeks = self._peek(2)?;
        assert!(amount_peeks >= 2);
        self.peeks[1]
    }
    pub fn peek3(&mut self) -> Option<char> {
        let amount_peeks = self._peek(3)?;
        assert_eq!(amount_peeks, 3);
        self.peeks[2]
    }
    pub fn next_char(&mut self) -> Option<char> {
        let amount_peeks = self.amount_peeks();
        let c = if amount_peeks > 0 {
            let c = self.peeks[0].take();
            self.peeks.swap(0, 1);
            self.peeks.swap(1, 2);
            c
        } else {
            self.chars.next()
        };
        if let Some(c) = c {
            self.pos += c.len_utf8();
        }
        c
    }
    /// Returns the position of the next character
    pub fn next_pos(&self) -> usize {
        self.pos
    }
}

pub struct Lexer<'arena, 'src> {
    pub file: Arc<SourceFile>,
    cursor: Cursor<'src>,
    ctx: SharedCtx<'arena>,
    delimiters: usize,
    is_mod_doc_comment_allowed: bool,
}

impl<'ctx, 'src> Lexer<'ctx, 'src> {
    pub fn new(ctx: SharedCtx<'ctx>, file: &'src Arc<SourceFile>) -> Self {
        Self {
            cursor: Cursor::new(file.source.chars()),
            file: file.clone(),
            ctx,
            delimiters: 0,
            is_mod_doc_comment_allowed: true,
        }
    }

    fn is_at_end(&mut self) -> bool {
        self.cursor.peek().is_none()
    }

    fn advance(&mut self) -> Option<char> {
        self.cursor.next_char()
    }

    fn dismiss(&mut self) {
        self.cursor.next_char();
    }

    /// Returns the position of the next character
    fn next_pos(&self) -> usize {
        self.cursor.next_pos()
    }

    fn span(&self, pos: usize, len: usize) -> Span<'ctx> {
        assert!(len < u32::MAX as usize);
        assert!(pos < u32::MAX as usize);
        self.ctx.intern_span(SpanData {
            pos: BytePos::from_u32(pos as u32),
            len: len as u32,
            file: self.file.id,
        })
    }

    fn peek(&mut self) -> Option<char> {
        self.cursor.peek()
    }

    fn peek2(&mut self) -> Option<char> {
        self.cursor.peek2()
    }

    fn peek3(&mut self) -> Option<char> {
        self.cursor.peek3()
    }

    fn if_char_advance(&mut self, character: char) -> bool {
        if self.peek() != Some(character) {
            return false;
        }

        self.dismiss();
        true
    }

    pub fn scan_tokens(&mut self) -> Result<Box<[TokenTree<'ctx>]>, Vec<LexingError<'ctx>>> {
        let mut errs = Vec::new();
        match self.scan_tt(&mut errs) {
            Ok(v) if errs.is_empty() => Ok(v),
            _ => Err(errs),
        }
    }

    fn scan_tt(&mut self, errs: &mut Vec<LexingError<'ctx>>) -> Result<Box<[TokenTree<'ctx>]>, ()> {
        let mut toks = Vec::new();
        while let Some(c) = self.peek() {
            if matches!(c, ')' | ']' | '}') {
                if self.delimiters == 0 {
                    let span = self.span(self.next_pos(), 1);
                    self.dismiss();
                    toks = Vec::new();
                    errs.push(LexingError::UnopenedDelimiter { span });
                    continue;
                }
                break;
            } else if matches!(c, '(' | '[' | '{') {
                let delimiter = match c {
                    '(' => Delimiter::Parenthesis,
                    '[' => Delimiter::Brackets,
                    '{' => Delimiter::Curlies,
                    _ => unreachable!(),
                };
                let open_span = self.span(self.next_pos(), 1);
                self.delimiters += 1;
                self.dismiss();
                let children = self.scan_tt(errs).unwrap_or_default();
                let close_span = self.span(self.next_pos(), 1);
                match (self.advance(), delimiter) {
                    (Some(')'), Delimiter::Parenthesis)
                    | (Some(']'), Delimiter::Brackets)
                    | (Some('}'), Delimiter::Curlies) => {}
                    _ => errs.push(LexingError::UnclosedDelimiter {
                        open_span,
                        close_span,
                    }),
                }
                if !errs.is_empty() {
                    toks = Vec::new();
                } else {
                    toks.push(TokenTree::Delimited {
                        open_span,
                        close_span,
                        delimiter,
                        children,
                    });
                }
            } else {
                match self.int_scan_token() {
                    Ok(Some(v)) if errs.is_empty() => toks.push(TokenTree::Token(v)),
                    Ok(_) => {}
                    Err(e) => {
                        errs.push(e);
                        toks = Vec::new();
                    }
                }
            }
        }
        Ok(toks.into_boxed_slice())
    }

    fn int_scan_token(&mut self) -> Result<Option<Token<'ctx>>, LexingError<'ctx>> {
        let pos_start = self.next_pos();
        let c = self.advance().unwrap();

        macro_rules! token {
            ($token: ident) => {
                Ok(self.get_token(TokenType::$token, pos_start))
            };

            ($tokena: ident, $tokenb: ident, $char: expr) => {{
                if self.if_char_advance($char) {
                    Ok(self.get_token(TokenType::$tokenb, pos_start))
                } else {
                    Ok(self.get_token(TokenType::$tokena, pos_start))
                }
            }};
        }

        let p = self.peek().unwrap_or('\0');
        match c {
            ',' => token!(Comma),
            '$' => token!(Dollar),
            '?' => token!(QuestionMark),
            '.' if self.if_char_advance('.') => token!(Range, RangeInclusive, '='),
            '.' if p.is_ascii_digit() => self.parse_number('.', pos_start),
            '.' => token!(Dot),
            '+' => token!(Plus, PlusAssign, '='),
            '-' if p.is_ascii_digit() || p == '.' => self.parse_number('-', pos_start),
            '-' if self.if_char_advance('=') => token!(MinusAssign),
            '-' if self.if_char_advance('>') => token!(ReturnType),
            '-' => token!(Minus),
            // doc comment (///) or doc block comment (/**). note that we have to filter out /(^ *\* *|)/, so that
            // /**
            //  * meow
            //  * meow
            //  */
            // gets parsed as "meow\nmeow"
            '/' if self.peek() == self.peek2() && matches!(self.peek(), Some('/' | '*')) => {
                let single_line = self.advance() == Some('/');
                self.dismiss();

                let mut s = String::new();
                self.parse_doc_comments(&mut s, single_line, false);
                let span = self.span_from(pos_start);
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
            '/' if matches!(self.peek(), Some('/' | '*')) && self.peek2() == Some('!') => {
                let single_line = self.advance() == Some('/');
                self.dismiss();

                if !self.is_mod_doc_comment_allowed {
                    self.parse_doc_comments(&mut String::new(), single_line, true);

                    return Err(LexingError::InvalidModuleDocComment(
                        self.span_from(pos_start),
                    ));
                }

                let mut s = String::new();
                self.parse_doc_comments(&mut s, single_line, true);
                let span = self.span_from(pos_start);
                let comment = self.ctx.add_doc_comment(s.into_boxed_str());
                Ok(Token::new(
                    TokenType::ModuleDocComment,
                    Some(Literal::DocComment(comment)),
                    span,
                ))
            }
            '/' if p != '/' && p != '*' => token!(Divide),
            '%' => token!(Modulo),
            '*' => token!(Asterix),
            '=' => token!(Equal, EqualEqual, '='),
            '<' => token!(LessThan),
            '>' => token!(GreaterThan),
            ':' => token!(Colon, NamespaceAccess, ':'),
            ';' => token!(Semicolon),
            '!' => token!(LogicalNot, NotEquals, '='),
            '&' => token!(Ampersand, LogicalAnd, '&'),
            '|' if self.if_char_advance('|') => token!(LogicalOr),
            '|' if self.if_char_advance('>') => token!(PipeOperator),
            '|' => token!(BitwiseOr),
            '^' => token!(BitwiseXor),
            ' ' | '\n' | '\r' | '\t' => {
                while matches!(self.peek(), Some(' ' | '\n' | '\r' | '\t')) {
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
            ('0'..='9') => self.parse_number(c, pos_start),
            '"' => self.parse_string('"', pos_start),
            '`' => {
                let mut tok = self.parse_string('`', pos_start)?;
                tok.ty = TokenType::IdentifierLiteral;
                Ok(tok)
            }
            _ if Self::is_valid_identifier_char(c) && !c.is_ascii_digit() => {
                self.parse_identifier(c, pos_start)
            }
            _ => {
                return Err(LexingError::UnknownTokenError {
                    span: self.span(pos_start, 1),
                    char: c,
                });
            }
        }
        .map(Some)
    }

    fn skip_single_line_comment(&mut self) {
        while !matches!(self.advance(), Some('\n') | None) {}
    }

    fn skip_multi_line_comment(&mut self) {
        loop {
            if matches!(self.advance(), Some('*') | None)
                && matches!(self.advance(), Some('/') | None)
            {
                break;
            }
        }
    }

    fn skip_spaces(&mut self) -> usize {
        let mut i = 0;
        while matches!(self.peek(), Some(' ' | '\t' | '\n' | '\r')) {
            i += 1;
            self.dismiss();
        }
        i
    }

    /// returns the position of the last space
    fn skip_spaces_max(&mut self, mut i: usize) -> usize {
        while i > 0 && matches!(self.peek(), Some(' ' | '\t')) {
            i -= 1;
            self.dismiss();
        }
        // ' '.len_utf8() and '\t'.len_utf8() == 1
        self.next_pos() - 1
    }

    /// Returns the position of the last character of the doc comment.
    fn parse_multi_line_doc_comment(&mut self, s: &mut String) {
        self.skip_spaces();
        // skip the line if it's only `/**`.
        self.if_char_advance('\n');
        let mut num_spaces = self.skip_spaces();
        let has_star = self.peek() == Some('*') && self.peek2() != Some('/');
        if has_star {
            self.advance();
            num_spaces = self.skip_spaces();
        }
        loop {
            if has_star {
                self.skip_spaces();
                if self.peek() == Some('*') && self.peek2() != Some('/') {
                    self.advance();
                    self.skip_spaces_max(num_spaces);
                }
            }
            loop {
                match self.peek() {
                    Some('\n') => {
                        self.advance();
                        s.push('\n');
                        break;
                    }
                    Some('*') if self.peek2() == Some('/') => {
                        self.dismiss();
                        self.dismiss();
                        // /**
                        //  * meow
                        //  */
                        // should be parsed as "meow\n".
                        if !s.ends_with('\n') {
                            s.push('\n');
                        }
                        return;
                    }
                    None => return,
                    Some(c) => {
                        s.push(c);
                        self.dismiss();
                    }
                }
            }
        }
    }

    fn parse_single_line_doc_comment(&mut self, s: &mut String, max_spaces: usize) {
        self.skip_spaces_max(max_spaces);
        loop {
            match self.advance() {
                None => return,
                Some('\n') => {
                    s.push('\n');
                    return;
                }
                Some(v) => s.push(v),
            }
        }
    }

    fn parse_doc_comments(&mut self, s: &mut String, mut single_line: bool, module: bool) {
        let mut max_spaces = None;
        println!("parsing doc comment");
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
                if self.peek() == Some('/') && self.peek2() == Some('/') {
                    if module {
                        if self.peek3() == Some('/') {
                            return;
                        }
                    } else if self.peek3() == Some('!') {
                        return;
                    }
                    self.dismiss();
                    self.dismiss();
                    // /// was found, continue to the start of the loop
                    if self.peek() == Some('/') || self.peek() == Some('!') {
                        self.dismiss();
                        single_line = true;
                        break;
                    }
                    // normal comment, skip it
                    self.skip_single_line_comment();
                    continue;
                }
                // /* was found, skip normal comment
                else if self.peek() == Some('/') && self.peek2() == Some('*') {
                    if module {
                        if self.peek3() == Some('*') {
                            return;
                        }
                    } else if self.peek3() == Some('!') {
                        return;
                    }
                    self.dismiss();
                    self.dismiss();

                    // /** was found, invoke the multiline parsing procedure
                    if self.peek() == Some('*') || self.peek() == Some('!') {
                        self.advance();
                        single_line = false;
                        break;
                    }
                    // normal comment, skip it
                    self.skip_multi_line_comment();
                    continue;
                }
                // if no /// was found, return
                else {
                    return;
                }
            }
        }
    }

    #[inline(always)]
    fn get_token(&self, token: TokenType, start: usize) -> Token<'ctx> {
        Token::new(
            token,
            None,
            token
                .char_len()
                .map(|len| self.span(start, len as usize))
                .unwrap_or_else(|| self.span_from(start)),
        )
    }

    fn skip_to_after_number(&mut self) {
        loop {
            match self.peek() {
                Some('0'..='9') => self.dismiss(),
                Some('.') if self.peek2().unwrap_or('\0').is_ascii_digit() => self.dismiss(),
                _ => break,
            }
        }
    }

    fn parse_numtype(
        &mut self,
        num_start_bytepos: usize,
        numtype_start_bytepos: usize,
        first_char: char,
        value: u64,
        is_negative: bool,
        allow_float: bool,
    ) -> Result<Token<'ctx>, LexingError<'ctx>> {
        let mut ty = String::from(first_char);

        loop {
            match self.peek() {
                Some(c @ ('a'..='z' | '0'..='9')) => {
                    ty.push(c);
                    self.dismiss();
                }
                _ => {
                    let err = LexingError::InvalidNumberType(self.span_from(numtype_start_bytepos));
                    let Ok(number_type) = NumberType::from_str(&ty) else {
                        return Err(err);
                    };
                    return match number_type {
                        NumberType::F32 | NumberType::F64 if !allow_float => Err(err),
                        NumberType::F32 | NumberType::F64 if is_negative => Ok(Token::new(
                            TokenType::FloatLiteral,
                            Some(Literal::Float(-(value as f64), number_type)),
                            self.span_from(num_start_bytepos),
                        )),
                        NumberType::F32 | NumberType::F64 => Ok(Token::new(
                            TokenType::FloatLiteral,
                            Some(Literal::Float(value as f64, number_type)),
                            self.span_from(num_start_bytepos),
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
                            self.span_from(num_start_bytepos),
                        )),
                        _ => Ok(Token::new(
                            TokenType::SIntLiteral,
                            Some(Literal::UInt(value, number_type)),
                            self.span_from(num_start_bytepos),
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
    ) -> Result<Token<'ctx>, LexingError<'ctx>> {
        let mut value: u64 = 0;

        loop {
            match self.peek() {
                Some(c @ '0'..='9') => {
                    self.dismiss();
                    value = (value << 4) | (c as u64 - '0' as u64)
                }
                Some(c @ 'a'..='f') => {
                    self.dismiss();
                    value = (value << 4) | (c as u64 - 'a' as u64 + 0xa)
                }
                Some(c @ 'A'..='F') => {
                    self.dismiss();
                    value = (value << 4) | (c as u64 - 'A' as u64 + 0xa)
                }
                Some('_') => self.dismiss(),
                Some(c) if Self::is_valid_identifier_char(c) => {
                    let pos = self.next_pos();
                    self.dismiss();
                    return self.parse_numtype(start_bytepos, pos, c, value, is_negative, false);
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
    ) -> Result<Token<'ctx>, LexingError<'ctx>> {
        let mut value: u64 = 0;

        loop {
            match self.peek() {
                Some(c @ ('0' | '1')) => {
                    self.dismiss();
                    value = (value << 1) | (c as u64 - '0' as u64)
                }
                Some('2'..='9') => {
                    self.advance();
                    return Err(LexingError::InvalidNumberError(
                        self.span_from(start_bytepos),
                    ));
                }
                Some('_') => self.dismiss(),
                Some(c) if Self::is_valid_identifier_char(c) => {
                    let pos = self.next_pos();
                    self.dismiss();
                    return self.parse_numtype(start_bytepos, pos, c, value, is_negative, false);
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
    ) -> Result<Token<'ctx>, LexingError<'ctx>> {
        let mut value: u64 = 0;

        loop {
            match self.peek() {
                Some(c @ ('0'..='7')) => {
                    self.dismiss();
                    value = (value << 3) | (c as u64 - '0' as u64)
                }
                Some('8' | '9') => {
                    self.dismiss();
                    return Err(LexingError::InvalidNumberError(
                        self.span_from(start_bytepos),
                    ));
                }
                Some('_') => self.dismiss(),
                Some(c) if Self::is_valid_identifier_char(c) => {
                    let pos = self.next_pos();
                    self.dismiss();
                    return self.parse_numtype(start_bytepos, pos, c, value, is_negative, false);
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

    fn parse_number(
        &mut self,
        mut first_char: char,
        start_byte: usize,
    ) -> Result<Token<'ctx>, LexingError<'ctx>> {
        let is_negative = first_char == '-';
        let mut is_float = false;
        if is_negative {
            first_char = self
                .advance()
                .expect("`-` always needs another character following it.");
        }

        if first_char == '0' && self.peek() == Some('x') {
            self.dismiss();
            if !self.peek().unwrap_or('\0').is_ascii_hexdigit() {
                let pos = self.next_pos();
                self.dismiss();
                return Err(LexingError::ExpectedHexDigit(self.span(pos, 1)));
            }
            return self.parse_hex(start_byte, is_negative);
        } else if first_char == '0' && self.peek() == Some('b') {
            self.dismiss();
            if !matches!(self.peek(), Some('0' | '1')) {
                let pos = self.next_pos();
                self.dismiss();
                return Err(LexingError::ExpectedBinDigit(self.span(pos, 1)));
            }
            return self.parse_bin(start_byte, is_negative);
        } else if first_char == '0' && self.peek() == Some('o') {
            self.dismiss();
            if !matches!(self.peek(), Some('0'..='7')) {
                let pos = self.next_pos();
                self.dismiss();
                return Err(LexingError::ExpectedOctDigit(self.span(pos, 1)));
            }
            return self.parse_oct(start_byte, is_negative);
        }

        let mut str = String::new();

        if is_negative {
            str.push('-');
        }
        if first_char == '.' {
            str.push('0');
            is_float = true;
        }
        str.push(first_char);

        if first_char == '.' && !matches!(self.peek(), Some('0'..='9')) {
            let pos = self.next_pos();
            self.dismiss();
            return Err(LexingError::ExpectedDecDigit(self.span(pos, 1)));
        }

        let mut ty = String::new();
        while let Some(p) = self.peek() {
            if !ty.is_empty() {
                if p.is_ascii_alphanumeric() {
                    self.dismiss();
                    ty.push(p);
                    continue;
                }
                break;
            }

            match p {
                '0'..='9' => {
                    self.dismiss();
                    str.push(p)
                }
                '_' => self.dismiss(),
                '.' if self.peek2().unwrap_or('\0').is_ascii_digit() => {
                    if is_float {
                        let pos = self.next_pos();
                        self.skip_to_after_number();
                        return Err(LexingError::MultipleDecimalPoints(self.span(pos, 1)));
                    }
                    is_float = true;
                    str.push(p);
                    self.dismiss();
                }
                _ if Self::is_valid_identifier_char(p) => {
                    self.dismiss();
                    ty.push(p)
                }
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
    fn parse_xhh_escape(
        &mut self,
        string_char: char,
        backslash_byte: usize,
    ) -> Result<char, LexingError<'ctx>> {
        let pos = self.next_pos();
        let part1 = match self.advance() {
            None => return Err(LexingError::UnclosedString(self.span(pos, 1))),
            Some(c @ '0'..='9') => c as u8 - b'0',
            Some(c @ 'a'..='f') => c as u8 - b'a' + 0xa,
            Some(c @ 'A'..='F') => c as u8 - b'A' + 0xa,
            Some(c) if c == string_char => {
                return Err(LexingError::UnterminatedNumericEscape(
                    self.span(backslash_byte, 2),
                ));
            }
            Some(c) => {
                return Err(LexingError::InvalidNumericEscapeChar(self.span(pos, 1), c));
            }
        };
        let pos = self.next_pos();
        let part2 = match self.advance() {
            None => return Err(LexingError::UnclosedString(self.span(pos, 1))),
            Some(c @ '0'..='9') => c as u8 - b'0',
            Some(c @ 'a'..='f') => c as u8 - b'a' + 0xa,
            Some(c @ 'A'..='F') => c as u8 - b'A' + 0xa,
            Some(c) if c == string_char => {
                return Err(LexingError::UnterminatedNumericEscape(
                    self.span(backslash_byte, 3),
                ));
            }
            Some(c) => {
                return Err(LexingError::InvalidNumericEscapeChar(self.span(pos, 1), c));
            }
        };
        match char::from_u32(((part1 << 4) | part2) as u32) {
            Some(c) if c.is_ascii() => Ok(c),
            _ => Err(LexingError::OutOfRangeEscape(
                self.span(backslash_byte, 4),
                0..=0x7f,
            )),
        }
    }

    fn parse_unicode_escape(
        &mut self,
        string_char: char,
        backslash_byte: usize,
    ) -> Result<char, LexingError<'ctx>> {
        let pos = self.next_pos();
        match self.advance() {
            None => return Err(LexingError::UnclosedString(self.span(pos, 1))),
            Some('{') => {}
            _ => {
                return Err(LexingError::InvalidUnicodeEscape(
                    self.span_from(backslash_byte),
                ));
            }
        }
        // to track the amount of characters inside { ... }, which cannot be more than 10.
        let mut char_count = 0;
        let mut num = 0;
        loop {
            let pos = self.next_pos();
            match self.advance() {
                None => return Err(LexingError::UnclosedString(self.span(pos, 1))),
                Some('}') => break,
                Some(c @ '0'..='9') => num = (num << 4) | (c as u8 - b'0') as u32,
                Some(c @ 'a'..='f') => num = (num << 4) | (c as u8 - b'a' + 0xa) as u32,
                Some(c @ 'A'..='F') => num = (num << 4) | (c as u8 - b'A' + 0xa) as u32,
                Some(c) if c == string_char => {
                    return Err(LexingError::UnterminatedUnicodeEscape(
                        self.span_from(backslash_byte),
                    ));
                }
                _ => return Err(LexingError::InvalidUnicodeEscapeChar(self.span(pos, 1))),
            }
            char_count += 1;
        }
        if char_count == 0 {
            return Err(LexingError::EmptyUnicodeEscapeSequence(
                self.span_from(backslash_byte),
            ));
        }
        if char_count > 6 {
            return Err(LexingError::TooLongUnicodeEscapeSequence(
                self.span_from(backslash_byte),
            ));
        }
        match char::from_u32(num) {
            Some(c) => Ok(c),
            None => Err(LexingError::OutOfRangeUnicodeEscapeSequence(
                self.span_from(backslash_byte),
            )),
        }
    }

    fn parse_string(
        &mut self,
        string_char: char,
        start: usize,
    ) -> Result<Token<'ctx>, LexingError<'ctx>> {
        let mut is_backslash = false;
        let mut s = String::new();

        loop {
            let Some(c) = self.peek() else {
                return Err(LexingError::UnclosedString(self.span(self.next_pos(), 1)));
            };
            let pos = self.next_pos();
            self.dismiss();
            if c == '\n' || c == '\r' {
                return Err(LexingError::UnclosedString(self.span(pos, 1)));
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
                    // 'x'.len_utf8() == 1; 'u'.len_utf8() == 1
                    'x' => s.push(self.parse_xhh_escape(string_char, pos - 1)?),
                    'u' => s.push(self.parse_unicode_escape(string_char, pos - 1)?),
                    '\n' => loop {
                        match self.peek() {
                            None => {
                                let pos = self.next_pos();
                                return Err(LexingError::UnclosedString(self.span(pos, 1)));
                            }
                            // skip all whitespace characters
                            Some(' ' | '\n' | '\r' | '\x09') => _ = self.advance(),
                            _ => break,
                        }
                    },
                    _ => return Err(LexingError::InvalidEscape(self.span(pos, 1), c)),
                }
            } else if c == '\\' {
                is_backslash = true;
            } else if c == string_char {
                break;
            } else if c == '\n' {
                return Err(LexingError::UnclosedString(self.span(pos, 1)));
            } else {
                s.push(c);
            }
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
        start: usize,
    ) -> Result<Token<'ctx>, LexingError<'ctx>> {
        let mut identifier = String::new();
        identifier.push(starting_char);

        while let Some(c) = self.peek() {
            if !Self::is_valid_identifier_char(c) {
                break;
            }
            self.dismiss();
            identifier.push(c);
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
            "void" => self.get_token(TokenType::VoidLiteral, start),
            ident => Self::try_token_from_keyword(ident)
                .map(|v| self.get_token(v, start))
                .unwrap_or_else(|| {
                    Token::new(
                        TokenType::IdentifierLiteral,
                        Some(Literal::String(self.ctx.intern_str(ident))),
                        self.span_from(start),
                    )
                }),
        })
    }

    fn span_from(&self, from: usize) -> Span<'ctx> {
        self.span(from, self.next_pos() - from)
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
            "defer" => Some(TokenType::Defer),
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

    /// Get the tokens if there's no token tree (no `( ... )`, `[ ... ]` and `{ ... }`)
    fn get_tokens<'arena>(
        ctx: SharedCtx<'arena>,
        src: &str,
    ) -> Result<Vec<Token<'arena>>, Vec<LexingError<'arena>>> {
        let f = ctx.source_map.testing_new_file(src.into());
        let mut tokenizer = Lexer::new(ctx, &f);
        let tts = tokenizer.scan_tokens()?;
        let mut tokens = Vec::with_capacity(tts.len());
        for tt in tts {
            match tt {
                TokenTree::Token(t) => tokens.push(t),
                TokenTree::Delimited { .. } => panic!("delimited tokens found"),
            }
        }
        check_tokens(&tokens);
        Ok(tokens)
    }

    fn assert_token_eq(
        src: &str,
        expected_tokens: &[(TokenType, Option<Literal<'_>>)],
        ctx: SharedCtx<'_>,
    ) {
        let eof_token = (TokenType::Eof, None);
        let tokens = get_tokens(ctx, src).expect("unexpected errors");
        println!("Doc Comments:");
        ctx.with_all_doc_comments(|i, s| println!("#{i}: {s:?}"));
        if tokens.len() < expected_tokens.len() {
            println!("Missing tokens. Expected ({}):", expected_tokens.len());
            for t in expected_tokens {
                println!("{t:?}");
            }
            println!("Got ({}):", tokens.len());
            for t in &tokens {
                println!("{t:?}");
            }
            panic!("Missing tokens")
        }
        if tokens.len() > expected_tokens.len() {
            println!("Excessive tokens. Expected ({}):", expected_tokens.len());
            for t in expected_tokens {
                println!("{t:?}");
            }
            println!("Got ({}):", tokens.len());
            for t in &tokens {
                println!("{t:?}");
            }
            panic!("Excessive tokens")
        }
        assert_eq!(tokens.len(), expected_tokens.len());
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
            let Err(errs) = get_tokens(ctx, $str) else { unreachable!("everything parsed correctly") };
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
// ignored
/// Nya
// still ignored
/**
 * Meow
*/
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
