use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    sync::Arc,
};

use mira_context::SharedCtx;
use mira_errors::{Diagnostic, Diagnostics, pluralize};
use mira_lexer::{
    Delimiter, Literal, TTDelim, Token, TokenTree as TT, TokenType, token::IdentDisplay,
};
use mira_macros::ErrorData;
use mira_spans::{BytePos, Ident, SourceFile, Span, SpanData, Symbol, interner::SpanInterner};

use crate::{TokenStream, expand::pat_parser::parse_stream};

use super::{
    ExpandContext, Macro, MacroError, MacroErrorDiagnosticsExt, MacroParser, MatcherLoc,
    NamedMatch, ParseResult, SingleMatch, TokenTree, builtin_macros, compute_locs,
};

/// returns None if any errors occurred, which will be put into `diagnostics`.
pub fn expand_tokens<'ctx>(
    ctx: SharedCtx<'ctx>,
    file: Arc<SourceFile>,
    tokens: Vec<TT<'ctx>>,
    diagnostics: &mut Diagnostics<'ctx>,
) -> Option<Vec<TT<'ctx>>> {
    let eof_span = ctx.intern_span(SpanData::new(BytePos::from_u32(file.len()), 1, file.id));
    let mut expander = MacroExpander::new(ctx, file);
    match expander.expand_macros(&tokens, eof_span, diagnostics, true) {
        Ok(Cow::Owned(v)) => Some(v),
        Ok(Cow::Borrowed(_)) => Some(tokens),
        Err(()) => None,
    }
}

struct MacroExpander<'arena> {
    macros: HashMap<Symbol<'arena>, Macro<'arena>>,
    ctx: ExpandContext<'arena>,
}

impl<'ctx> MacroExpander<'ctx> {
    fn new(ctx: SharedCtx<'ctx>, file: Arc<SourceFile>) -> Self {
        Self {
            macros: HashMap::new(),
            ctx: ExpandContext::new(ctx, file),
        }
    }

    pub fn does_expand(tokens: &[TT<'ctx>]) -> bool {
        tokens.iter().any(|v| match v {
            TT::Delimited(v) => Self::does_expand(&v.children),
            TT::Token(t) => matches!(t.ty, TokenType::MacroInvocation | TokenType::MacroDef),
        })
    }

    fn expand_macros<'a>(
        &mut self,
        tokens: &'a [TT<'ctx>],
        eof_span: Span<'ctx>,
        diagnostics: &mut Diagnostics<'ctx>,
        can_define_macros: bool,
    ) -> Result<Cow<'a, [TT<'ctx>]>, ()> {
        macro_rules! err {
            ($v:expr) => {
                $v.map_err(|v| _ = diagnostics.add_err(v))
            };
        }
        if !Self::does_expand(tokens) {
            return Ok(Cow::Borrowed(tokens));
        }
        let mut has_err = false;

        let mut tokens = TokenStream::new(tokens, eof_span);
        let mut toks = Vec::with_capacity(tokens.pos());

        while let Some(p) = tokens.eat_with_doc_comments() {
            let t = match p {
                TT::Delimited(delim) => {
                    match self.expand_macros(
                        &delim.children,
                        delim.close_span,
                        diagnostics,
                        can_define_macros,
                    ) {
                        Err(()) => has_err = true,
                        Ok(v) => {
                            // TODO: this should be able to be done without allocating in the case
                            // of Cow::Borrowed by reusing the data it got (and, in turn, taking
                            // ownership of the tt). This however is not possible without drain
                            // iterator, which would make macro definitions and macro invocations
                            // kinda painful, remove(0), which is slow, or a custom unsafe data
                            // structure.
                            toks.push(TT::Delimited(TTDelim {
                                children: v.into_owned().into_boxed_slice(),
                                ..*delim
                            }));
                        }
                    }
                    continue;
                }
                &TT::Token(token) => match token.ty {
                    TokenType::MacroDef if can_define_macros => token,
                    TokenType::MacroInvocation => token,
                    _ => {
                        toks.push(TT::Token(token));
                        continue;
                    }
                },
            };
            if t.ty == TokenType::MacroDef {
                let name = err!(tokens.expect(TokenType::IdentifierLiteral));
                let content = err!(tokens.expect_delim(Delimiter::Curlies));
                let Ok(name) = name else { continue };
                let Ok(content) = content else { continue };
                match Self::parse_macro_def(
                    Ident::new(name.string_literal(), name.span),
                    content.into(),
                    diagnostics,
                    self.ctx.ctx.span_interner,
                ) {
                    Ok(v) => drop(self.macros.insert(name.string_literal(), v)),
                    Err(()) => has_err = true,
                }
            } else {
                let name = t.string_literal();
                let p = match tokens.eat() {
                    Some(TT::Delimited(v)) => v,
                    _ => {
                        err!(tokens.expect_one_of(&[
                            TokenType::ParenOpen,
                            TokenType::BracketOpen,
                            TokenType::CurlyOpen
                        ]));
                        unreachable!();
                    }
                };

                if builtin_macros::get_builtin_macro(&name).is_none()
                    && !self.macros.contains_key(&name)
                {
                    diagnostics.add_cannot_find_macro(t.span, name);
                    has_err = true;
                }
                let Ok(content) = self.expand_macros(&p.children, p.close_span, diagnostics, false)
                else {
                    has_err = true;
                    continue;
                };

                let output = if let Some(macro_fn) = builtin_macros::get_builtin_macro(&name) {
                    let span = t
                        .span
                        .combine_with([p.close_span], self.ctx.ctx.span_interner);
                    let res = macro_fn(self.ctx.ctx, span, p.into(), diagnostics);
                    match res {
                        Ok(v) => v,
                        Err(()) => {
                            has_err = true;
                            vec![]
                        }
                    }
                } else if let Some(macro_) = self.macros.get(&name) {
                    let mut output = Vec::new();
                    if let Err(e) =
                        expand_macro(macro_, &content, p.close_span, &mut output, &self.ctx)
                    {
                        diagnostics.add(e);
                        has_err = true;
                    }
                    output
                } else {
                    vec![]
                };
                // TODO: change this to the span of the closing curly of the applied macro branch
                // or to a dummy span in the case of builtin macros.
                match self.expand_macros(&output, p.close_span, diagnostics, true) {
                    Ok(Cow::Owned(v)) => toks.extend(v),
                    Ok(Cow::Borrowed(v)) => toks.extend_from_slice(v),
                    Err(()) => has_err = true,
                }
            }
        }

        if has_err {
            return Err(());
        }
        Ok(Cow::Owned(toks))
    }

    fn parse_macro_def(
        name: Ident<'ctx>,
        // (...pat...) => { ...content... }; repeating
        mut tokens: TokenStream<'_, 'ctx>,
        diagnostics: &mut Diagnostics<'ctx>,
        span_interner: &SpanInterner<'ctx>,
    ) -> Result<Macro<'ctx>, ()> {
        let s = <&TTDelim<'ctx> as Into<TokenStream<'_, 'ctx>>>::into;
        macro_rules! err {
            ($v:expr) => {
                $v.map_err(|v| _ = diagnostics.add_err(v))?
            };
        }

        if tokens.is_at_end() {
            diagnostics.add_empty_macro(name.span());
            return Err(());
        }

        let mut has_err = false;
        let mut cases = Vec::new();
        while !tokens.is_at_end() {
            let param_delim = err!(tokens.expect_delim(Delimiter::Parenthesis));

            let def = parse_stream(&mut s(param_delim), true, diagnostics, span_interner)?;
            let def = compute_locs(&def);
            let mut defined_meta_vars = HashSet::new();
            for loc in &def {
                if let MatcherLoc::MetaVarDecl { bind, .. } = loc {
                    match defined_meta_vars.get(bind) {
                        None => _ = defined_meta_vars.insert(*bind),
                        Some(first) => {
                            has_err = true;
                            diagnostics.add_multiple_meta_vars(
                                bind.symbol(),
                                bind.span(),
                                first.span(),
                            );
                        }
                    }
                }
            }
            err!(tokens.expect(TokenType::Equal));
            err!(tokens.expect(TokenType::GreaterThan));

            let body_delim = err!(tokens.expect_delim(Delimiter::Curlies));

            let body = parse_stream(&mut s(body_delim), false, diagnostics, span_interner)?;
            err!(tokens.expect(TokenType::Semicolon));
            cases.push((def.into_boxed_slice(), body.into_boxed_slice()));
        }
        (!has_err).then_some(Macro { name, cases }).ok_or(())
    }
}

fn expand_macro<'arena>(
    r#macro: &Macro<'arena>,
    input: &[TT<'arena>],
    end_span: Span<'arena>,
    output: &mut Vec<TT<'arena>>,
    ctx: &ExpandContext<'arena>,
) -> Result<(), Diagnostic<'arena>> {
    let mut err = None;
    let mut res = None;
    let mut parser = MacroParser::new();
    for (i, (pat, body)) in r#macro.cases.iter().enumerate() {
        match parser.parse(input, end_span, pat, ctx) {
            ParseResult::Success(values) => {
                res = Some((values, body));
                break;
            }
            ParseResult::Failure(diagnostic, approx_pos) => match err {
                Some((_, _, cur_pos)) if cur_pos > approx_pos => diagnostic.dismiss(),
                _ => {
                    if let Some((diag, ..)) = err.replace((diagnostic, i, approx_pos)) {
                        diag.dismiss();
                    }
                }
            },
            ParseResult::Err(diagnostic) => {
                return Err(diagnostic.with_note(format!(
                    "While parsing macro `{}`, in rule #{}",
                    r#macro.name, i
                )));
            }
        }
    }
    let Some((values, body)) = res else {
        let (err, rule, _) = err.unwrap();
        return Err(err.with_note(format!(
            "While parsing macro `{}`, in rule #{}",
            r#macro.name, rule
        )));
    };
    if let Some((diag, ..)) = err {
        diag.dismiss()
    }
    expand_body(body, &values, output, &mut Vec::new())
}

fn expand_body<'arena>(
    body: &[TokenTree<'arena>],
    matches: &HashMap<Ident<'arena>, NamedMatch<'arena>>,
    output: &mut Vec<TT<'arena>>,
    indices: &mut Vec<usize>,
) -> Result<(), Diagnostic<'arena>> {
    for part in body {
        match part {
            TokenTree::Delimited {
                children,
                delim,
                open,
                close,
            } => {
                let mut new = Vec::with_capacity(children.len());
                expand_body(children, matches, &mut new, indices);
                let v = TTDelim {
                    open_span: *open,
                    close_span: *close,
                    delimiter: *delim,
                    children: new.into_boxed_slice(),
                };
                output.push(TT::Delimited(v));
            }
            TokenTree::Token(token) => output.push(TT::Token(*token)),
            TokenTree::Tokens(tokens) => output.extend(tokens.iter().map(|v| TT::Token(*v))),
            TokenTree::Sequence(repetition) => {
                let mut amount_of_values = None;
                part.meta_vars(|var2| {
                    let matched = &matches[&var2];
                    match (matched.repetitions_at_idx(indices), amount_of_values) {
                        (Some(v), None) => amount_of_values = Some((v, var2)),
                        (Some(cur), Some((v, var1))) if cur != v => {
                            return Err(VariableRepetitionMismatch {
                                span: repetition.span,
                                var1: var1.symbol(),
                                var1_rep: v,
                                var2: var2.symbol(),
                                var2_rep: cur,
                            }
                            .to_error());
                        }
                        _ => {}
                    }
                    Ok(())
                })?;
                let Some((amount_of_values, _)) = amount_of_values else {
                    return Err(MacroError::NoVariableRepetition(repetition.span).to_error());
                };
                for i in 0..amount_of_values {
                    if i != 0
                        && let Some(tok) = repetition.separator
                    {
                        output.push(TT::Token(tok));
                    }
                    indices.push(i);
                    let res = expand_body(&repetition.content, matches, output, indices);
                    indices.pop();
                    res?
                }
            }
            TokenTree::MetaVar(ident) => {
                let mut matched = &matches[ident];
                for &index in indices.iter() {
                    match matched {
                        NamedMatch::MatchedSeq(matches) => matched = &matches[index],
                        NamedMatch::ParsedSingle(_) => break,
                    }
                }
                match matched {
                    NamedMatch::MatchedSeq(_) => {
                        return Err(MacroError::VariableStillRepeating(
                            ident.span(),
                            ident.symbol(),
                        )
                        .to_error());
                    }
                    NamedMatch::ParsedSingle(v) => match v {
                        SingleMatch::TokenTree(tt) => output.push(tt.clone()),
                        SingleMatch::Ident(i) => output.push(TT::Token(Token::new(
                            TokenType::IdentifierLiteral,
                            Some(Literal::String(i.symbol())),
                            i.span(),
                        ))),
                    },
                }
            }
            TokenTree::MetaVarDecl(..) => unreachable!(),
        }
    }
    Ok(())
}

#[derive(ErrorData)]
#[error("variable {} repeats {var1_rep} time{s1}, but variable {} repeats {var2_rep} time{s2}", IdentDisplay(*var1), IdentDisplay(*var2), s1 = pluralize!(*var1_rep), s2 = pluralize!(*var2_rep))]
struct VariableRepetitionMismatch<'arena> {
    #[primary_label("")]
    span: Span<'arena>,
    var1: Symbol<'arena>,
    var1_rep: usize,
    var2: Symbol<'arena>,
    var2_rep: usize,
}
