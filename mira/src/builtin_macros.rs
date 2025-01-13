use crate::tokenizer::{Literal, Location, NumberType, Token, TokenType};
use std::fmt::Write;

type MacroFn = fn(&Location, &[Token]) -> Vec<Token>;

pub fn get_builtin_macro(name: &str) -> Option<MacroFn> {
    match name {
        "concat" => Some(macro_concat),
        "concat_idents" => Some(macro_concat_idents),
        "line" => Some(macro_line),
        "column" => Some(macro_column),
        "file" => Some(macro_file),
        "compile_error" => Some(macro_compile_error),
        "stringify" => Some(macro_stringify),
        _ => None,
    }
}

fn macro_concat(loc: &Location, args: &[Token]) -> Vec<Token> {
    let mut concat_str = String::new();
    for arg in args {
        match arg.typ {
            TokenType::IdentifierLiteral
            | TokenType::StringLiteral
            | TokenType::SIntLiteral
            | TokenType::UIntLiteral
            | TokenType::FloatLiteral
            | TokenType::Comma
            | TokenType::BooleanLiteral => (),
            _ => panic!("{}: concat! only accepts literals", arg.location),
        }
        match arg.literal {
            None => (),
            Some(Literal::SInt(v, _)) => {
                write!(concat_str, "{v}").expect("writing to a string should never fail")
            }
            Some(Literal::UInt(v, _)) => {
                write!(concat_str, "{v}").expect("writing to a string should never fail")
            }
            Some(Literal::Float(v, _)) => {
                write!(concat_str, "{v}").expect("writing to a string should never fail")
            }
            Some(Literal::Bool(v)) if v => concat_str.push_str("true"),
            Some(Literal::Bool(_)) => concat_str.push_str("false"),
            Some(Literal::String(ref v)) => v.with(|v| concat_str.push_str(v)),
        }
    }
    vec![Token {
        typ: TokenType::StringLiteral,
        literal: Some(Literal::String(concat_str.into())),
        location: loc.clone(),
    }]
}

fn macro_concat_idents(loc: &Location, args: &[Token]) -> Vec<Token> {
    let mut concat_str = String::new();
    for arg in args {
        match arg.typ {
            TokenType::IdentifierLiteral | TokenType::Comma => (),
            _ => panic!("{}: concat! only accepts identifiers", arg.location),
        }
        match arg.literal {
            None => (),
            Some(Literal::String(ref v)) => v.with(|v| concat_str.push_str(v)),
            _ => unreachable!(
                "identifier literals should never have any literal value other than string"
            ),
        }
    }
    vec![Token {
        typ: TokenType::IdentifierLiteral,
        literal: Some(Literal::String(concat_str.into())),
        location: loc.clone(),
    }]
}

fn macro_line(loc: &Location, args: &[Token]) -> Vec<Token> {
    if args.len() > 0 {
        panic!("{loc}: did not expect any arguments")
    }
    vec![Token {
        location: loc.clone(),
        literal: Some(Literal::UInt(loc.line as u64, NumberType::U32)),
        typ: TokenType::UIntLiteral,
    }]
}

fn macro_column(loc: &Location, args: &[Token]) -> Vec<Token> {
    if args.len() > 0 {
        panic!("{loc}: did not expect any arguments")
    }
    vec![Token {
        location: loc.clone(),
        literal: Some(Literal::UInt(loc.column as u64, NumberType::U32)),
        typ: TokenType::UIntLiteral,
    }]
}

fn macro_file(loc: &Location, args: &[Token]) -> Vec<Token> {
    if args.len() > 0 {
        panic!("{loc}: did not expect any arguments")
    }
    vec![Token {
        location: loc.clone(),
        literal: Some(Literal::String(loc.file.display().to_string().into())),
        typ: TokenType::StringLiteral,
    }]
}

fn macro_compile_error(loc: &Location, args: &[Token]) -> Vec<Token> {
    if let Some(lit) = args.get(0).as_ref().and_then(|v| v.literal.as_ref()) {
        match lit {
            Literal::Float(v, _) => panic!("{loc}: error: {v}"),
            Literal::SInt(v, _) => panic!("{loc}: error: {v}"),
            Literal::UInt(v, _) => panic!("{loc}: error: {v}"),
            Literal::String(v) => panic!("{loc}: error: {v}"),
            Literal::Bool(v) => panic!("{loc}: error: {v}"),
        }
    } else {
        panic!("{loc}: Unknown Compile-time Error")
    }
}

fn macro_stringify(loc: &Location, args: &[Token]) -> Vec<Token> {
    let mut strn = String::new();
    for arg in args {
        write!(strn, "{arg}").expect("writing to a string should never fail");
    }
    vec![Token {
        location: loc.clone(),
        literal: Some(Literal::String(strn.into())),
        typ: TokenType::StringLiteral,
    }]
}
