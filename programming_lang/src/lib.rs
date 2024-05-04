use error::LangErr;
use globals::GlobalString;
use tokens::{loc, Loc, Number, Token, TokenType};

pub mod error;
pub(crate) mod globals;
pub(crate) mod tokens;

pub struct Lexer {
    input: Vec<char>,
    cur_pos: usize,
    file_name: GlobalString,
    cur_line: usize,
    cur_col: usize,
}

impl Lexer {
    pub fn new(source: &str, file_name: &str) -> Self {
        Self {
            input: source.chars().collect(),
            file_name: file_name.into(),
            cur_pos: 0,
            cur_col: 0,
            cur_line: 0,
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        if self.cur_pos == self.input.len() {
            return Some(Token::new(TokenType::Eof, loc!(self.file_name;0;0)));
        } else if self.cur_pos > self.input.len() {
            return None;
        }
        None
    }

    pub fn parse_number(&mut self) -> Result<Number, LangErr> {
        let mut str = String::new();

    }

    fn expected_strings<T>(&self, strs: Box<[&'static str]>) -> Result<T, LangErr> {
        Err(LangErr::new(
            error::LangErrType::ExpectedStrs(strs),
            self.get_loc(),
        ))
    }

    fn expected_smth<T>(&self) -> Result<T, LangErr> {
        Err(LangErr::new(
            error::LangErrType::ExpectedSomething,
            self.get_loc(),
        ))
    }

    fn get_loc(&self) -> Loc {
        loc!(self.file_name;self.cur_col;self.cur_line)
    }

    fn get_loc_span(&self, length: usize) -> Loc {
        loc!(self.file_name;self.cur_col;self.cur_pos;length)
    }

    fn peek_next_char(&self) -> Option<char> {
        self.input.get(self.cur_pos).copied()
    }

    fn next_char_limiting(
        &mut self,
        is_valid: &dyn Fn(char) -> bool,
        strs: &dyn Fn() -> Box<[&'static str]>,
    ) -> Result<char, LangErr> {
        match self.next_char() {
            Some(c) if is_valid(c) => Ok(c),
            _ => self.expected_strings(strs()),
        }
    }

    fn next_char(&mut self) -> Option<char> {
        if self.cur_pos != 0 {
            self.cur_col += 1;
        }
        let character = self.input.get(self.cur_pos);
        self.cur_pos += 1;
        match character {
            Some(c) if *c == '\n' && self.cur_pos != 1 => {
                self.cur_col = 0;
                self.cur_line += 1;
                Some(*c)
            }
            Some(c) => Some(*c),
            None => {
                self.cur_col -= 1;
                None
            }
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}
