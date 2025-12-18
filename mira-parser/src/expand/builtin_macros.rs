use mira_context::SharedCtx;
use mira_errors::{Diagnostics, ErrorData};
use mira_lexer::{Literal, NumberType, Token, TokenTree, TokenType, token::IdentDisplay};
use mira_spans::{Span, Symbol};
use std::fmt::Write;

use crate::tokenstream::TokenStream;

type MacroFn<'ctx> = fn(
    ctx: SharedCtx<'ctx>,
    span: Span<'ctx>,
    stream: TokenStream<'_, 'ctx>,
    diagnostics: &mut Diagnostics<'ctx>,
) -> Result<Vec<TokenTree<'ctx>>, ()>;

pub(super) fn get_builtin_macro(name: &str) -> Option<MacroFn<'_>> {
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

macro_rules! err {
    ($diags: ident, $err:expr) => {
        $err.map_err(|v| _ = $diags.add_err(v))
    };
}

fn t<'ctx>(ty: TokenType, literal: Option<Literal<'ctx>>, span: Span<'ctx>) -> TokenTree<'ctx> {
    TokenTree::Token(Token::new(ty, literal, span))
}

fn macro_env<'ctx>(
    ctx: SharedCtx<'ctx>,
    span: Span<'ctx>,
    mut stream: TokenStream<'_, 'ctx>,
    diagnostics: &mut Diagnostics<'ctx>,
) -> Result<Vec<TokenTree<'ctx>>, ()> {
    let (name, name_span) = err!(diagnostics, stream.expect_string())?;
    let err;
    if stream.match_tok_dismiss(TokenType::Comma) {
        if stream.is_at_end() {
            err = None;
        } else {
            err = Some(err!(diagnostics, stream.expect_string())?.0);
            stream.match_tok_dismiss(TokenType::Comma);
        }
    } else {
        err = None;
    }
    err!(diagnostics, stream.finish())?;
    match std::env::var(name.to_str()) {
        Ok(v) => Ok(vec![t(
            TokenType::StringLiteral,
            Some(Literal::String(ctx.intern_str(&v))),
            span,
        )]),
        Err(_) => {
            match err {
                Some(v) => _ = diagnostics.add_compile_error(v.to_string(), name_span),
                None => _ = diagnostics.add_undefined_env_var(name, name_span),
            }
            Err(())
        }
    }
}

fn macro_concat<'ctx>(
    ctx: SharedCtx<'ctx>,
    span: Span<'ctx>,
    stream: TokenStream<'_, 'ctx>,
    diagnostics: &mut Diagnostics<'ctx>,
) -> Result<Vec<TokenTree<'ctx>>, ()> {
    let mut concat_str = String::new();
    for arg in stream.inner() {
        let arg = match arg {
            TokenTree::Token(arg) => arg,
            TokenTree::Delimited(ttdelim) => {
                diagnostics.add_concat_only_expects_literals(ttdelim.open_span, ttdelim.open_tok());
                return Err(());
            }
        };
        match arg.ty {
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
            Some(Literal::ByteString(ref v)) => {
                for c in v.utf8_chunks() {
                    concat_str.push_str(c.valid());
                    if !c.invalid().is_empty() {
                        concat_str.push(std::char::REPLACEMENT_CHARACTER);
                    }
                }
            }
            Some(Literal::DocComment(v)) => ctx.with_doc_comment(v, |v| concat_str.push_str(v)),
        }
    }
    Ok(vec![t(
        TokenType::StringLiteral,
        Some(Literal::String(ctx.intern_str(&concat_str))),
        span,
    )])
}

fn macro_concat_idents<'ctx>(
    ctx: SharedCtx<'ctx>,
    span: Span<'ctx>,
    stream: TokenStream<'_, 'ctx>,
    diagnostics: &mut Diagnostics<'ctx>,
) -> Result<Vec<TokenTree<'ctx>>, ()> {
    let mut concat_str = String::new();
    for arg in stream.inner() {
        let arg = match arg {
            TokenTree::Token(t) => t,
            TokenTree::Delimited(ttdelim) => {
                diagnostics
                    .add_concat_idents_only_expects_literals(ttdelim.open_span, ttdelim.open_tok());
                return Err(());
            }
        };
        match arg.ty {
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
    Ok(vec![t(
        TokenType::IdentifierLiteral,
        Some(Literal::String(ctx.intern_str(&concat_str))),
        span,
    )])
}

fn macro_line<'ctx>(
    ctx: SharedCtx<'ctx>,
    span: Span<'ctx>,
    stream: TokenStream<'_, 'ctx>,
    diagnostics: &mut Diagnostics<'ctx>,
) -> Result<Vec<TokenTree<'ctx>>, ()> {
    if !stream.is_at_end() {
        diagnostics.add_no_arg_macro("line", span);
        return Err(());
    }
    let data = span.get_span_data();
    let line = ctx
        .source_map
        .lookup_line(data.file, data.pos)
        .map(|v| v + 1)
        .unwrap_or_default();
    Ok(vec![t(
        TokenType::UIntLiteral,
        Some(Literal::UInt(line as u64, NumberType::U32)),
        span,
    )])
}

fn macro_column<'ctx>(
    ctx: SharedCtx<'ctx>,
    span: Span<'ctx>,
    stream: TokenStream<'_, 'ctx>,
    diagnostics: &mut Diagnostics<'ctx>,
) -> Result<Vec<TokenTree<'ctx>>, ()> {
    if !stream.is_at_end() {
        diagnostics.add_no_arg_macro("line", span);
        return Err(());
    }
    let data = span.get_span_data();
    let column = ctx.source_map.lookup_file_pos(data.file, data.pos).1;
    Ok(vec![t(
        TokenType::UIntLiteral,
        Some(Literal::UInt(column as u64, NumberType::U32)),
        span,
    )])
}

fn macro_file<'ctx>(
    ctx: SharedCtx<'ctx>,
    span: Span<'ctx>,
    stream: TokenStream<'_, 'ctx>,
    diagnostics: &mut Diagnostics<'ctx>,
) -> Result<Vec<TokenTree<'ctx>>, ()> {
    if !stream.is_at_end() {
        diagnostics.add_no_arg_macro("line", span);
        return Err(());
    }
    Ok(vec![t(
        TokenType::StringLiteral,
        Some(Literal::String(
            ctx.intern_str(
                &ctx.source_map
                    .get_file(span.get_span_data().file)
                    .unwrap()
                    .path
                    .display()
                    .to_string(),
            ),
        )),
        span,
    )])
}

fn macro_compile_error<'ctx>(
    ctx: SharedCtx<'ctx>,
    span: Span<'ctx>,
    stream: TokenStream<'_, 'ctx>,
    diagnostics: &mut Diagnostics<'ctx>,
) -> Result<Vec<TokenTree<'ctx>>, ()> {
    let lit = match stream.peek() {
        Some(&TokenTree::Token(Token { literal, .. })) => literal,
        _ => None,
    };
    if let Some(lit) = lit {
        match lit {
            Literal::DocComment(v) => {
                _ = diagnostics.add_compile_error(ctx.with_doc_comment(v, str::to_string), span)
            }
            Literal::Float(v, _) => _ = diagnostics.add_compile_error(format!("{v}"), span),
            Literal::SInt(v, _) => _ = diagnostics.add_compile_error(format!("{v}"), span),
            Literal::UInt(v, _) => _ = diagnostics.add_compile_error(format!("{v}"), span),
            Literal::String(v) => _ = diagnostics.add_compile_error(format!("{v}"), span),
            Literal::ByteString(v) => _ = diagnostics.add_compile_error(format!("{v}"), span),
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

fn macro_stringify<'ctx>(
    ctx: SharedCtx<'ctx>,
    span: Span<'ctx>,
    stream: TokenStream<'_, 'ctx>,
    _: &mut Diagnostics<'ctx>,
) -> Result<Vec<TokenTree<'ctx>>, ()> {
    let mut strn = String::new();
    fn stringify(s: &mut String, tt: &TokenTree<'_>) {
        match tt {
            TokenTree::Token(token) => {
                if token.ty == TokenType::StringLiteral {
                    s.push_str(token.string_literal().to_str());
                } else {
                    write!(s, "{token}").unwrap();
                }
            }
            TokenTree::Delimited(ttdelim) => {
                s.push('(');
                ttdelim.children.iter().for_each(|v| stringify(s, v));
                s.push(')');
            }
        }
    }
    stream.inner().iter().for_each(|v| stringify(&mut strn, v));
    Ok(vec![t(
        TokenType::StringLiteral,
        Some(Literal::String(ctx.intern_str(&strn))),
        span,
    )])
}
