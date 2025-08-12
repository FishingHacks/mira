use crate::{LexingContext, Literal, NumberType, Token, TokenType};
use mira_spans::Span;
use std::fmt::Write;

type MacroFn<'arena> =
    fn(LexingContext<'arena>, Span<'arena>, &[Token<'arena>]) -> Vec<Token<'arena>>;

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

fn macro_concat<'arena>(
    ctx: LexingContext<'arena>,
    span: Span<'arena>,
    args: &[Token<'arena>],
) -> Vec<Token<'arena>> {
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
            _ => panic!(
                "{}: concat! only accepts literals",
                arg.span.with_source_file(ctx.source_map)
            ),
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
            Some(Literal::String(ref v)) => concat_str.push_str(v),
        }
    }
    vec![Token {
        typ: TokenType::StringLiteral,
        literal: Some(Literal::String(ctx.intern_str(&concat_str))),
        span,
    }]
}

fn macro_concat_idents<'arena>(
    ctx: LexingContext<'arena>,
    span: Span<'arena>,
    args: &[Token<'arena>],
) -> Vec<Token<'arena>> {
    let mut concat_str = String::new();
    for arg in args {
        match arg.typ {
            TokenType::IdentifierLiteral | TokenType::Comma => (),
            _ => panic!(
                "{}: concat! only accepts identifiers",
                arg.span.with_source_file(ctx.source_map)
            ),
        }
        match arg.literal {
            None => (),
            Some(Literal::String(ref v)) => concat_str.push_str(v),
            _ => unreachable!(
                "identifier literals should never have any literal value other than string"
            ),
        }
    }
    vec![Token {
        typ: TokenType::IdentifierLiteral,
        literal: Some(Literal::String(ctx.intern_str(&concat_str))),
        span,
    }]
}

fn macro_line<'arena>(
    ctx: LexingContext<'arena>,
    span: Span<'arena>,
    args: &[Token<'arena>],
) -> Vec<Token<'arena>> {
    if !args.is_empty() {
        panic!(
            "{}: did not expect any arguments",
            span.with_source_file(ctx.source_map)
        );
    }
    let data = span.get_span_data();
    let line = ctx
        .source_map
        .lookup_line(data.file, data.pos)
        .map(|v| v + 1)
        .unwrap_or_default();
    vec![Token {
        span,
        literal: Some(Literal::UInt(line as u64, NumberType::U32)),
        typ: TokenType::UIntLiteral,
    }]
}

fn macro_column<'arena>(
    ctx: LexingContext<'arena>,
    span: Span<'arena>,
    args: &[Token<'arena>],
) -> Vec<Token<'arena>> {
    if !args.is_empty() {
        panic!(
            "{}: did not expect any arguments",
            span.with_source_file(ctx.source_map)
        )
    }
    let data = span.get_span_data();
    let column = ctx.source_map.lookup_file_pos(data.file, data.pos).1;
    vec![Token {
        span,
        literal: Some(Literal::UInt(column as u64, NumberType::U32)),
        typ: TokenType::UIntLiteral,
    }]
}

fn macro_file<'arena>(
    ctx: LexingContext<'arena>,
    span: Span<'arena>,
    args: &[Token<'arena>],
) -> Vec<Token<'arena>> {
    if !args.is_empty() {
        panic!(
            "{}: did not expect any arguments",
            span.with_source_file(ctx.source_map)
        )
    }
    vec![Token {
        span,
        literal: Some(Literal::String(
            ctx.intern_str(
                &ctx.source_map
                    .get_file(span.get_span_data().file)
                    .unwrap()
                    .path
                    .display()
                    .to_string(),
            ),
        )),
        typ: TokenType::StringLiteral,
    }]
}

fn macro_compile_error<'arena>(
    ctx: LexingContext<'arena>,
    span: Span<'arena>,
    args: &[Token<'arena>],
) -> Vec<Token<'arena>> {
    let span = span.with_source_file(ctx.source_map);
    if let Some(lit) = args.first().as_ref().and_then(|v| v.literal.as_ref()) {
        match lit {
            Literal::Float(v, _) => panic!("{span}: error: {v}"),
            Literal::SInt(v, _) => panic!("{span}: error: {v}"),
            Literal::UInt(v, _) => panic!("{span}: error: {v}"),
            Literal::String(v) => panic!("{span}: error: {v}"),
            Literal::Bool(v) => panic!("{span}: error: {v}"),
        }
    } else {
        panic!("{span}: Unknown Compile-time Error")
    }
}

fn macro_stringify<'arena>(
    ctx: LexingContext<'arena>,
    span: Span<'arena>,
    args: &[Token<'arena>],
) -> Vec<Token<'arena>> {
    let mut strn = String::new();
    for arg in args {
        write!(strn, "{arg}").expect("writing to a string should never fail");
    }
    vec![Token {
        span,
        literal: Some(Literal::String(ctx.intern_str(&strn))),
        typ: TokenType::StringLiteral,
    }]
}
