use mira_spans::{Ident, Span, Symbol};
use std::fmt::{Debug, Display, Write};
use std::str::FromStr;

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
    MacroInvocation,
    MacroDef = "macro!",
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
    Dollar = "$",
    As = "as",
    AnnotationIntroducer = "@",
    NamespaceAccess = "::",
    QuestionMark = "?",
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

#[derive(Clone, Copy, Debug, PartialEq)]
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

fn display_ident(f: &mut std::fmt::Formatter, ident: Symbol) -> std::fmt::Result {
    // only idents that are >= 1 character and only alphanumeric + #, $, _ and don't start with a
    // number can be printed without a string
    let needs_str = matches!(ident.chars().next(), Some('0'..='9') | None)
        || ident
            .chars()
            .any(|c| !matches!(c, 'a'..='z'|'A'..='Z'|'0'..='9'|'#'|'$'|'_'));
    if needs_str {
        f.write_char('`')?;
        for c in ident.escape_debug() {
            f.write_char(c)?;
        }
        f.write_char('`')
    } else {
        f.write_str(&ident)
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
                Some(Literal::String(v)) => display_ident(f, *v),
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
            TokenType::MacroInvocation => match &self.literal {
                Some(Literal::String(v)) => {
                    display_ident(f, *v)?;
                    f.write_char('!')
                }
                _ => f.write_str("macro_invocation(malformed data)!"),
            },
            _ => unreachable!(),
        }
    }
}

impl<'arena> Token<'arena> {
    pub fn new(typ: TokenType, literal: Option<Literal<'arena>>, span: Span<'arena>) -> Self {
        Self { typ, span, literal }
    }

    pub fn void_literal(&self) {
        match &self.literal {
            None => (),
            _ => unreachable!("{self} should only ever contain a void literal"),
        }
    }

    pub fn string_literal(&self) -> Symbol<'arena> {
        match &self.literal {
            Some(Literal::String(v)) => *v,
            _ => unreachable!("{self} should only ever contain a string literal"),
        }
    }

    pub fn bool_literal(&self) -> bool {
        match &self.literal {
            Some(Literal::Bool(v)) => *v,
            _ => unreachable!("{self} should only ever contain a boolean literal"),
        }
    }

    pub fn float_literal(&self) -> (f64, NumberType) {
        match &self.literal {
            Some(Literal::Float(v, numty)) => (*v, *numty),
            _ => unreachable!("{self} should only ever contain a void literal"),
        }
    }

    pub fn sint_literal(&self) -> (i64, NumberType) {
        match &self.literal {
            Some(Literal::SInt(v, numty)) => (*v, *numty),
            _ => unreachable!("{self} should only ever contain a void literal"),
        }
    }

    pub fn uint_literal(&self) -> (u64, NumberType) {
        match &self.literal {
            Some(Literal::UInt(v, numty)) => (*v, *numty),
            _ => unreachable!("{self} should only ever contain a void literal"),
        }
    }
}
