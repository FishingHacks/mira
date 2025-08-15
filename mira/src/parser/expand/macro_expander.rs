use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    sync::Arc,
};

use mira_errors::{Diagnostic, Diagnostics, pluralize};
use mira_lexer::{Token, TokenType, token::IdentDisplay};
use mira_macros::ErrorData;
use mira_spans::{BytePos, Ident, SourceFile, Span, SpanData, Symbol, interner::SpanInterner};

use crate::{
    context::SharedContext,
    parser::expand::{
        MacroErrorDiagnosticsExt, MacroParser, MatcherLoc, ParseResult, builtin_macros,
        compute_locs, pat_parser::parse_token_tree,
    },
    tokenstream::BorrowedTokenStream,
};

use super::{
    ExpandContext, Macro, MacroError, NamedMatch, SingleMatch, TokenTree,
    pat_parser::{ParenType, match_paren},
};

/// returns None if any errors occurred, which will be put into `diagnostics`.
pub fn expand_tokens<'arena>(
    ctx: SharedContext<'arena>,
    file: Arc<SourceFile>,
    tokens: Vec<Token<'arena>>,
    diagnostics: &mut Diagnostics<'arena>,
) -> Option<Vec<Token<'arena>>> {
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

impl<'arena> MacroExpander<'arena> {
    fn new(ctx: SharedContext<'arena>, file: Arc<SourceFile>) -> Self {
        Self {
            macros: HashMap::new(),
            ctx: ExpandContext::new(ctx, file),
        }
    }

    fn expand_macros<'a>(
        &mut self,
        tokens: &'a [Token<'arena>],
        eof_span: Span<'arena>,
        diagnostics: &mut Diagnostics<'arena>,
        can_define_macros: bool,
    ) -> Result<Cow<'a, [Token<'arena>]>, ()> {
        macro_rules! err {
            ($v:expr) => {
                $v.map_err(|v| _ = diagnostics.add_err(v))?
            };
        }
        let mut has_err = false;

        let mut tokens = BorrowedTokenStream::new(tokens, eof_span);
        let mut is_macro = false;
        while !tokens.is_at_end() {
            match tokens.eat().typ {
                TokenType::MacroDef if !can_define_macros => {
                    diagnostics.add_unexpected_token(tokens.current().span);
                    has_err = true;
                    is_macro = true;
                    break;
                }
                TokenType::MacroInvocation | TokenType::MacroDef => {
                    is_macro = true;
                    break;
                }
                _ => {}
            }
        }
        if !is_macro {
            return Ok(Cow::Borrowed(tokens.token_holder()));
        }
        let mut toks = Vec::with_capacity(tokens.pos());

        // push all the tokens up until the macro into the newly created tokens list
        toks.extend_from_slice(&tokens.token_holder()[..tokens.pos() - 1]);

        while !tokens.is_at_end() {
            if tokens.current().typ == TokenType::MacroDef {
                let name = err!(tokens.expect(TokenType::IdentifierLiteral)).string_literal();
                let name_span = tokens.current().span;
                let start_span = err!(tokens.expect(TokenType::CurlyLeft)).span;
                let content = err!(match_paren(&mut tokens, ParenType::Curly).ok_or(
                    MacroError::UnmatchedParen(start_span, tokens.current().span)
                ));
                match Self::parse_macro_def(
                    Ident::new(name, name_span),
                    BorrowedTokenStream::new(content, tokens.current().span),
                    diagnostics,
                    self.ctx.ctx.span_interner(),
                ) {
                    Ok(v) if can_define_macros => _ = self.macros.insert(name, v),
                    Ok(_) => {}
                    Err(()) => has_err = true,
                }
            } else if tokens.current().typ == TokenType::MacroInvocation {
                let name = tokens.current().string_literal();
                let name_span = tokens.current().span;
                let tok = err!(tokens.expect_one_of(&[
                    TokenType::ParenLeft,
                    TokenType::BracketLeft,
                    TokenType::CurlyLeft
                ]));
                let paren = ParenType::from_tt(tok.typ).unwrap();
                if builtin_macros::get_builtin_macro(&name).is_none()
                    && !self.macros.contains_key(&name)
                {
                    diagnostics.add_cannot_find_macro(name_span, name);
                    has_err = true;
                }
                let content = err!(
                    match_paren(&mut tokens, paren)
                        .ok_or(MacroError::UnmatchedParen(tok.span, tokens.current().span))
                );
                if let Ok(content) =
                    self.expand_macros(content, tokens.current().span, diagnostics, false)
                {
                    let output = if let Some(macro_fn) = builtin_macros::get_builtin_macro(&name) {
                        let res = macro_fn(
                            self.ctx.ctx,
                            tokens
                                .current()
                                .span
                                .combine_with([name_span], self.ctx.ctx.span_interner()),
                            &content,
                            diagnostics,
                        );
                        match res {
                            Ok(v) => v,
                            Err(()) => {
                                has_err = true;
                                vec![]
                            }
                        }
                    } else if let Some(macro_) = self.macros.get(&name) {
                        let mut output = Vec::new();
                        if let Err(e) = expand_macro(
                            macro_,
                            &content,
                            tokens.current().span,
                            &mut output,
                            &self.ctx,
                        ) {
                            diagnostics.add(e);
                            has_err = true;
                        }
                        output
                    } else {
                        vec![]
                    };
                    match self.expand_macros(&output, tokens.current().span, diagnostics, true) {
                        Ok(Cow::Owned(v)) => toks.extend(v),
                        Ok(Cow::Borrowed(v)) => toks.extend_from_slice(v),
                        Err(()) => has_err = true,
                    }
                }
            } else {
                unreachable!();
            }

            while !tokens.is_at_end() {
                match tokens.eat().typ {
                    TokenType::MacroDef if !can_define_macros => {
                        diagnostics.add_unexpected_token(tokens.current().span);
                        has_err = true;
                        break;
                    }
                    TokenType::MacroInvocation | TokenType::MacroDef => break,
                    _ => toks.push(tokens.current()),
                }
            }
        }

        if has_err {
            return Err(());
        }
        Ok(Cow::Owned(toks))
    }

    fn parse_macro_def(
        name: Ident<'arena>,
        // (...pat...) => { ...content... }; repeating
        mut tokens: BorrowedTokenStream<'arena, '_>,
        diagnostics: &mut Diagnostics<'arena>,
        span_interner: &SpanInterner<'arena>,
    ) -> Result<Macro<'arena>, ()> {
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
            err!(tokens.expect(TokenType::ParenLeft));
            let paren_start_span = tokens.current().span;
            let def = err!(match_paren(&mut tokens, ParenType::Paren).ok_or(
                MacroError::UnmatchedParen(paren_start_span, tokens.eof_span())
            ));
            let def = parse_token_tree(
                BorrowedTokenStream::new(def, tokens.current().span),
                true,
                diagnostics,
                span_interner,
            )?;
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
            err!(tokens.expect(TokenType::CurlyLeft));
            let body = err!(match_paren(&mut tokens, ParenType::Curly).ok_or(
                MacroError::UnmatchedParen(paren_start_span, tokens.eof_span())
            ));
            let body = parse_token_tree(
                BorrowedTokenStream::new(body, tokens.current().span),
                false,
                diagnostics,
                span_interner,
            )?;
            err!(tokens.expect(TokenType::Semicolon));
            cases.push((def.into_boxed_slice(), body.into_boxed_slice()));
        }
        (!has_err).then_some(Macro { name, cases }).ok_or(())
    }
}

fn expand_macro<'arena>(
    r#macro: &Macro<'arena>,
    input: &[Token<'arena>],
    end_span: Span<'arena>,
    output: &mut Vec<Token<'arena>>,
    ctx: &ExpandContext<'arena>,
) -> Result<(), Diagnostic<'arena>> {
    let mut err = None;
    let mut res = None;
    let mut parser = MacroParser::new();
    for (i, (pat, body)) in r#macro.cases.iter().enumerate() {
        match parser.parse(BorrowedTokenStream::new(input, end_span), pat, ctx) {
            ParseResult::Success(values) => {
                res = Some((values, body));
                break;
            }
            ParseResult::Failure(diagnostic, approx_pos) => match err {
                Some((_, _, cur_pos)) if cur_pos > approx_pos => {}
                _ => err = Some((diagnostic, i, approx_pos)),
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
    output: &mut Vec<Token<'arena>>,
    indices: &mut Vec<usize>,
) -> Result<(), Diagnostic<'arena>> {
    for part in body {
        match part {
            TokenTree::Token(token) => output.push(*token),
            TokenTree::Tokens(tokens) => output.extend(tokens),
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
                        output.push(tok);
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
                    NamedMatch::ParsedSingle(SingleMatch::Token(tok)) => output.push(*tok),
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
