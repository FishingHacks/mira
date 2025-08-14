use mira_errors::{Diagnostics, ErrorData};
use mira_lexer::{Literal, NumberType, Token, TokenType, token::IdentDisplay};
use mira_spans::{Span, SpanData, Symbol};
use std::fmt::Write;

use crate::{context::SharedContext, tokenstream::BorrowedTokenStream};

type MacroFn<'arena> = fn(
    SharedContext<'arena>,
    Span<'arena>,
    &[Token<'arena>],
    diagnostics: &mut Diagnostics<'arena>,
) -> Result<Vec<Token<'arena>>, ()>;

pub fn get_builtin_macro(name: &str) -> Option<MacroFn> {
    match name {
        "concat" => Some(macro_concat),
        "concat_idents" => Some(macro_concat_idents),
        "line" => Some(macro_line),
        "column" => Some(macro_column),
        "file" => Some(macro_file),
        "compile_error" => Some(macro_compile_error),
        "stringify" => Some(macro_stringify),
        "env" => Some(macro_env),
        _ => None,
    }
}

#[derive(Clone, Copy, ErrorData)]
#[error("concat! only accepts literals and `,`")]
struct ConcatOnlyExpectsLiterals<'arena>(
    #[primary_label("found {_1}")] Span<'arena>,
    Token<'arena>,
);

#[derive(Clone, Copy, ErrorData)]
#[error("concat_idents! only accepts identifiers and `,`")]
struct ConcatIdentsOnlyExpectsLiterals<'arena>(
    #[primary_label("found {_1}")] Span<'arena>,
    Token<'arena>,
);

#[derive(Clone, Copy, ErrorData)]
#[error("{_0}! doesn't accept arguments")]
struct NoArgMacro<'arena>(&'static str, #[primary_label("")] Span<'arena>);

#[derive(Clone, ErrorData)]
#[error("Environment variable {} is not defined", IdentDisplay(*_0))]
struct UndefinedEnvVar<'arena>(Symbol<'arena>, #[primary_label("")] Span<'arena>);

fn eof_span<'arena>(ctx: SharedContext<'arena>, span: Span<'arena>) -> Span<'arena> {
    let data = span.get_span_data();
    ctx.intern_span(SpanData::new(data.pos + data.len - 1, 1, data.file))
}

macro_rules! err {
    ($diags: ident, $err:expr) => {
        $err.map_err(|v| _ = $diags.add_err(v))
    };
}

fn macro_env<'arena>(
    ctx: SharedContext<'arena>,
    span: Span<'arena>,
    args: &[Token<'arena>],
    diagnostics: &mut Diagnostics<'arena>,
) -> Result<Vec<Token<'arena>>, ()> {
    let mut stream = BorrowedTokenStream::new(args, eof_span(ctx, span));
    let name = err!(diagnostics, stream.expect_string())?.0;
    let err;
    if stream.match_tok(TokenType::Comma) {
        if stream.is_at_end() {
            err = None;
        } else {
            err = Some(err!(diagnostics, stream.expect_string())?.0);
            stream.match_tok(TokenType::Comma);
        }
    } else {
        err = None;
    }
    err!(diagnostics, stream.finish())?;
    match std::env::var(name.to_str()) {
        Ok(v) => Ok(vec![Token::new(
            TokenType::StringLiteral,
            Some(Literal::String(ctx.intern_str(&v))),
            span,
        )]),
        Err(_) => {
            match err {
                Some(v) => _ = diagnostics.add_compile_error(v.to_string(), span),
                None => _ = diagnostics.add_undefined_env_var(name, span),
            }
            Err(())
        }
    }
}

fn macro_concat<'arena>(
    ctx: SharedContext<'arena>,
    span: Span<'arena>,
    args: &[Token<'arena>],
    diagnostics: &mut Diagnostics<'arena>,
) -> Result<Vec<Token<'arena>>, ()> {
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
            _ => {
                diagnostics.add_concat_only_expects_literals(arg.span, *arg);
                return Err(());
            }
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
    Ok(vec![Token {
        typ: TokenType::StringLiteral,
        literal: Some(Literal::String(ctx.intern_str(&concat_str))),
        span,
    }])
}

fn macro_concat_idents<'arena>(
    ctx: SharedContext<'arena>,
    span: Span<'arena>,
    args: &[Token<'arena>],
    diagnostics: &mut Diagnostics<'arena>,
) -> Result<Vec<Token<'arena>>, ()> {
    let mut concat_str = String::new();
    for arg in args {
        match arg.typ {
            TokenType::IdentifierLiteral | TokenType::Comma => (),
            _ => {
                diagnostics.add_concat_idents_only_expects_literals(arg.span, *arg);
                return Err(());
            }
        }
        match arg.literal {
            None => (),
            Some(Literal::String(ref v)) => concat_str.push_str(v),
            _ => unreachable!(
                "identifier literals should never have any literal value other than string"
            ),
        }
    }
    Ok(vec![Token {
        typ: TokenType::IdentifierLiteral,
        literal: Some(Literal::String(ctx.intern_str(&concat_str))),
        span,
    }])
}

fn macro_line<'arena>(
    ctx: SharedContext<'arena>,
    span: Span<'arena>,
    args: &[Token<'arena>],
    diagnostics: &mut Diagnostics<'arena>,
) -> Result<Vec<Token<'arena>>, ()> {
    if !args.is_empty() {
        diagnostics.add_no_arg_macro("line", span);
        return Err(());
    }
    let data = span.get_span_data();
    let line = ctx
        .source_map()
        .lookup_line(data.file, data.pos)
        .map(|v| v + 1)
        .unwrap_or_default();
    Ok(vec![Token {
        span,
        literal: Some(Literal::UInt(line as u64, NumberType::U32)),
        typ: TokenType::UIntLiteral,
    }])
}

fn macro_column<'arena>(
    ctx: SharedContext<'arena>,
    span: Span<'arena>,
    args: &[Token<'arena>],
    diagnostics: &mut Diagnostics<'arena>,
) -> Result<Vec<Token<'arena>>, ()> {
    if !args.is_empty() {
        diagnostics.add_no_arg_macro("line", span);
        return Err(());
    }
    let data = span.get_span_data();
    let column = ctx.source_map().lookup_file_pos(data.file, data.pos).1;
    Ok(vec![Token {
        span,
        literal: Some(Literal::UInt(column as u64, NumberType::U32)),
        typ: TokenType::UIntLiteral,
    }])
}

fn macro_file<'arena>(
    ctx: SharedContext<'arena>,
    span: Span<'arena>,
    args: &[Token<'arena>],
    diagnostics: &mut Diagnostics<'arena>,
) -> Result<Vec<Token<'arena>>, ()> {
    if !args.is_empty() {
        diagnostics.add_no_arg_macro("line", span);
        return Err(());
    }
    Ok(vec![Token {
        span,
        literal: Some(Literal::String(
            ctx.intern_str(
                &ctx.source_map()
                    .get_file(span.get_span_data().file)
                    .unwrap()
                    .path
                    .display()
                    .to_string(),
            ),
        )),
        typ: TokenType::StringLiteral,
    }])
}

fn macro_compile_error<'arena>(
    _: SharedContext<'arena>,
    span: Span<'arena>,
    args: &[Token<'arena>],
    diagnostics: &mut Diagnostics<'arena>,
) -> Result<Vec<Token<'arena>>, ()> {
    if let Some(lit) = args.first().as_ref().and_then(|v| v.literal.as_ref()) {
        match lit {
            Literal::Float(v, _) => _ = diagnostics.add_compile_error(format!("{v}"), span),
            Literal::SInt(v, _) => _ = diagnostics.add_compile_error(format!("{v}"), span),
            Literal::UInt(v, _) => _ = diagnostics.add_compile_error(format!("{v}"), span),
            Literal::String(v) => _ = diagnostics.add_compile_error(format!("{v}"), span),
            Literal::Bool(v) => _ = diagnostics.add_compile_error(format!("{v}"), span),
        }
    } else {
        diagnostics.add_compile_error("Unknown Compile-time Error".into(), span);
    }
    Err(())
}

#[derive(Clone, ErrorData)]
#[error("{_0}")]
struct CompileError<'arena>(String, #[primary_label("")] Span<'arena>);

fn macro_stringify<'arena>(
    ctx: SharedContext<'arena>,
    span: Span<'arena>,
    args: &[Token<'arena>],
    _: &mut Diagnostics<'arena>,
) -> Result<Vec<Token<'arena>>, ()> {
    let mut strn = String::new();
    for arg in args {
        write!(strn, "{arg}").expect("writing to a string should never fail");
    }
    Ok(vec![Token {
        span,
        literal: Some(Literal::String(ctx.intern_str(&strn))),
        typ: TokenType::StringLiteral,
    }])
}
