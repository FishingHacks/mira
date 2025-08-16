use mira_errors::Diagnostics;
use mira_lexer::{Token, TokenType};
use mira_spans::{Ident, interner::SpanInterner};

use crate::{error::ParsingErrorDiagnosticsExt as _, tokenstream::BorrowedTokenStream};

use super::{KleeneOp, MacroErrorDiagnosticsExt, MetaVarType, SequenceRepetition, TokenTree};

pub fn parse_token_tree<'arena>(
    mut input: BorrowedTokenStream<'arena, '_>,
    // if this is the body or the pattern of the macro
    parse_def: bool,
    diagnostics: &mut Diagnostics<'arena>,
    span_interner: &SpanInterner<'arena>,
) -> Result<Vec<TokenTree<'arena>>, ()> {
    let mut res = Vec::new();
    let mut success = true;
    while !input.is_at_end() {
        match parse_tree(&mut input, parse_def, diagnostics, span_interner) {
            Ok(v) => res.push(v),
            Err(()) => success = false,
        }
    }
    success.then_some(res).ok_or(())
}

fn count_metavar_decls(matcher: &[TokenTree]) -> usize {
    matcher
        .iter()
        .map(|tt| match tt {
            TokenTree::Tokens(_) => 0,
            TokenTree::Token(_) => 0,
            TokenTree::Sequence(seq) => seq.num_captures,
            TokenTree::MetaVar(_) => 1,
            TokenTree::MetaVarDecl(..) => 1,
        })
        .sum()
}

fn parse_tree<'arena>(
    tokens: &mut BorrowedTokenStream<'arena, '_>,
    // if this is the body or the pattern of the macro
    parse_def: bool,
    diagnostics: &mut Diagnostics<'arena>,
    span_interner: &SpanInterner<'arena>,
) -> Result<TokenTree<'arena>, ()> {
    let res = match tokens.eat().typ {
        TokenType::Dollar => {
            let next = tokens.eat();
            match next.typ {
                TokenType::ParenLeft => {
                    let span_start = next.span;
                    let Some(toks) = match_paren(tokens, ParenType::Paren) else {
                        crate::error::ParsingErrorDiagnosticsExt::add_unmatched_paren(
                            diagnostics,
                            next.span,
                            tokens.eof_span(),
                        );
                        return Err(());
                    };
                    let res = parse_token_tree(
                        BorrowedTokenStream::new(toks, tokens.current().span),
                        parse_def,
                        diagnostics,
                        span_interner,
                    );
                    // match <tok>?<kleene>
                    let separator = match tokens.peek().typ {
                        TokenType::Asterix | TokenType::Plus | TokenType::QuestionMark => None,
                        _ => Some(tokens.eat()),
                    };
                    let kleene = match tokens.eat().typ {
                        TokenType::Asterix => KleeneOp::ZeroOrMore,
                        TokenType::Plus => KleeneOp::OneOrMore,
                        TokenType::QuestionMark if separator.is_some() => {
                            diagnostics.add_unexpected_token(separator.unwrap().span);
                            return Err(());
                        }
                        TokenType::QuestionMark => KleeneOp::ZeroOrOne,
                        _ => {
                            diagnostics.add_expected_one_of(
                                tokens.current().span,
                                &[TokenType::Asterix, TokenType::Plus, TokenType::QuestionMark],
                                tokens.current(),
                            );
                            return Err(());
                        }
                    };
                    let content = res?;
                    Ok(TokenTree::Sequence(SequenceRepetition {
                        num_captures: count_metavar_decls(&content),
                        content,
                        separator,
                        kleene,
                        span: span_start.combine_with([tokens.current().span], span_interner),
                    }))
                }
                TokenType::IdentifierLiteral => {
                    let name = Ident::new(next.string_literal(), next.span);
                    // check if there is a :type following in case we're parsing a definition
                    if parse_def {
                        if tokens.eat().typ != TokenType::Colon {
                            diagnostics.add_expected(
                                tokens.current().span,
                                TokenType::Colon,
                                tokens.current(),
                            );
                            return Err(());
                        }
                        if tokens.eat().typ != TokenType::IdentifierLiteral {
                            diagnostics.add_expected(
                                tokens.current().span,
                                TokenType::IdentifierLiteral,
                                tokens.current(),
                            );
                            return Err(());
                        }
                        let ident = tokens.current();
                        let meta_var_type =
                            match MetaVarType::from_str(ident.string_literal().to_str()) {
                                Some(v) => v,
                                None => {
                                    diagnostics.add_invalid_meta_var_type(
                                        ident.span,
                                        ident.string_literal(),
                                    );
                                    return Err(());
                                }
                            };
                        Ok(TokenTree::MetaVarDecl(name, meta_var_type))
                    } else {
                        Ok(TokenTree::MetaVar(name))
                    }
                }
                // escaped $
                TokenType::Dollar => Ok(TokenTree::Token(tokens.current())),
                TokenType::Eof => Ok(TokenTree::Token(tokens.last())),
                _ => {
                    tokens.set_pos(tokens.pos() - 1);
                    match tokens.expect_one_of(&[
                        TokenType::Dollar,
                        TokenType::IdentifierLiteral,
                        TokenType::ParenLeft,
                    ]) {
                        Ok(_) => unreachable!(),
                        Err(e) => {
                            tokens.eat();
                            diagnostics.add_err(e);
                            Err(())
                        }
                    }
                }
            }
        }
        _ => Ok(TokenTree::Token(tokens.current())),
    }?;
    // clump multiple tokens in the body together
    if !parse_def {
        if let TokenTree::Token(tok) = res {
            if !tokens.is_at_end() && tokens.peek().typ == TokenType::Dollar {
                return Ok(res);
            }
            let mut toks = vec![tok];
            while !tokens.is_at_end() && tokens.peek().typ != TokenType::Dollar {
                toks.push(tokens.eat());
            }
            return Ok(TokenTree::Tokens(toks.into_boxed_slice()));
        }
    }
    Ok(res)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ParenType {
    Paren,
    Bracket,
    Curly,
}

impl ParenType {
    pub fn to_tt(self) -> (TokenType, TokenType) {
        match self {
            ParenType::Paren => (TokenType::ParenLeft, TokenType::ParenRight),
            ParenType::Bracket => (TokenType::BracketLeft, TokenType::BracketRight),
            ParenType::Curly => (TokenType::CurlyLeft, TokenType::CurlyRight),
        }
    }

    pub fn from_tt(tt: TokenType) -> Option<Self> {
        match tt {
            TokenType::ParenLeft => Some(ParenType::Paren),
            TokenType::BracketLeft => Some(ParenType::Bracket),
            TokenType::CurlyLeft => Some(ParenType::Curly),
            _ => None,
        }
    }
}

pub(super) fn match_paren<'arena, 'tok>(
    tokens: &mut BorrowedTokenStream<'arena, 'tok>,
    parens: ParenType,
) -> Option<&'tok [Token<'arena>]> {
    let mut paren_count = 0;
    let start = tokens.pos();
    let (left, right) = parens.to_tt();
    loop {
        let tok = tokens.eat().typ;
        if tok == left {
            paren_count += 1;
        } else if tok == right {
            match paren_count {
                0 => break,
                _ => paren_count -= 1,
            }
        } else if tok == TokenType::Eof {
            tokens.set_pos(start);
            return None;
        }
    }
    let inside_tokens = &tokens.token_holder()[start..tokens.pos() - 1];
    Some(inside_tokens)
}
