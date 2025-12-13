use mira_errors::Diagnostics;
use mira_lexer::{Literal, Token, TokenTree as TT, TokenType};
use mira_spans::{Ident, interner::SpanInterner};

use crate::{error::ParsingErrorDiagnosticsExt as _, tokenstream::TokenStream};

use super::{KleeneOp, MetaVarType, SequenceRepetition, TokenTree};

// pub(super) fn parse_token_tree<'arena>(
//     mut input: TokenStream<'arena, '_>,
//     // if this is the body or the pattern of the macro
//     parse_def: bool,
//     diagnostics: &mut Diagnostics<'arena>,
//     span_interner: &SpanInterner<'arena>,
// ) -> Result<Vec<TokenTree<'arena>>, ()> {
//     let mut res = Vec::new();
//     let mut success = true;
//     while !input.is_at_end() {
//         match parse_tree(&mut input, parse_def, diagnostics, span_interner) {
//             Ok(v) => res.push(v),
//             Err(()) => success = false,
//         }
//     }
//     success.then_some(res).ok_or(())
// }

fn count_metavar_decls(matcher: &[TokenTree<'_>]) -> usize {
    matcher
        .iter()
        .map(|tt| match tt {
            TokenTree::Tokens(_) => 0,
            TokenTree::Token(_) => 0,
            TokenTree::Sequence(seq) => seq.num_captures,
            TokenTree::MetaVar(_) => 1,
            TokenTree::MetaVarDecl(..) => 1,
            TokenTree::Delimited { children, .. } => count_metavar_decls(children),
        })
        .sum()
}

pub(super) fn parse_stream<'arena>(
    input: &mut TokenStream<'_, 'arena>,
    // if this is the body or the pattern of the macro
    parse_def: bool,
    diagnostics: &mut Diagnostics<'arena>,
    span_interner: &SpanInterner<'arena>,
) -> Result<Vec<TokenTree<'arena>>, ()> {
    let mut res = Vec::new();
    let mut success = false;
    while !input.is_at_end() {
        match parse_tree(input, parse_def, diagnostics, span_interner) {
            Ok(v) => res.push(v),
            Err(()) => success = false,
        }
    }
    success.then_some(res).ok_or(())
}

fn parse_tree<'arena>(
    tokens: &mut TokenStream<'_, 'arena>,
    // if this is the body or the pattern of the macro
    parse_def: bool,
    diagnostics: &mut Diagnostics<'arena>,
    span_interner: &SpanInterner<'arena>,
) -> Result<TokenTree<'arena>, ()> {
    let v = match tokens.eat().expect("tt should never be at the end") {
        TT::Token(t) if t.ty == TokenType::Dollar => {
            let Some(next) = tokens.eat() else {
                diagnostics.add_expected_one_of(
                    tokens.eof_span(),
                    &[
                        TokenType::IdentifierLiteral,
                        TokenType::ParenOpen,
                        TokenType::Dollar,
                    ],
                    None,
                );
                return Err(());
            };
            match next {
                TT::Delimited(v) => {
                    let inner = parse_stream(
                        &mut TokenStream::new(&v.children, v.close_span),
                        parse_def,
                        diagnostics,
                        span_interner,
                    );
                    let span;
                    let kleene;
                    let separator;
                    match tokens.eat() {
                        Some(&TT::Token(t)) => {
                            let kleene_op = match t.ty {
                                TokenType::Asterix => Some(KleeneOp::ZeroOrMore),
                                TokenType::QuestionMark => Some(KleeneOp::ZeroOrOne),
                                TokenType::Plus => Some(KleeneOp::OneOrMore),
                                _ => None,
                            };
                            if let Some(kleene_op) = kleene_op {
                                kleene = kleene_op;
                                separator = None;
                                span = v
                                    .open_span
                                    .combine_with([v.close_span, t.span], span_interner);
                            } else {
                                separator = Some(t);
                                let k = tokens
                                    .expect_one_of(&[
                                        TokenType::Asterix,
                                        TokenType::Plus,
                                        TokenType::QuestionMark,
                                    ])
                                    .map_err(|v| _ = diagnostics.add_err(v))?;
                                span = v
                                    .open_span
                                    .combine_with([v.close_span, t.span, k.span], span_interner);
                                match k.ty {
                                    TokenType::Asterix => kleene = KleeneOp::ZeroOrMore,
                                    TokenType::QuestionMark => kleene = KleeneOp::ZeroOrOne,
                                    TokenType::Plus => kleene = KleeneOp::OneOrMore,
                                    _ => unreachable!(),
                                }
                            }
                        }
                        Some(TT::Delimited(v)) => {
                            diagnostics.add_expected_kleene_op_or_sep(v.open_span);
                            return Err(());
                        }
                        None => {
                            diagnostics.add_expected_kleene_op_or_sep(tokens.eof_span());
                            return Err(());
                        }
                    }
                    let content = inner?;
                    Ok(TokenTree::Sequence(SequenceRepetition {
                        num_captures: count_metavar_decls(&content),
                        content,
                        separator,
                        kleene,
                        span,
                    }))
                }
                &TT::Token(Token {
                    ty: TokenType::IdentifierLiteral,
                    literal,
                    span,
                }) => {
                    let Some(Literal::String(v)) = literal else {
                        unreachable!()
                    };
                    let name = Ident::new(v, span);
                    if parse_def {
                        if let Err(e) = tokens.expect(TokenType::Colon) {
                            diagnostics.add_err(e);
                            return Err(());
                        }
                        let ident = tokens
                            .expect(TokenType::IdentifierLiteral)
                            .map_err(|e| _ = diagnostics.add_err(e))?;

                        let &Some(Literal::String(ty)) = &ident.literal else {
                            unreachable!()
                        };

                        match MetaVarType::from_str(*ty) {
                            Some(ty) => Ok(TokenTree::MetaVarDecl(name, ty)),
                            None => {
                                diagnostics.add_invalid_meta_var_type(ident.span, ty);
                                Err(())
                            }
                        }
                    } else {
                        Ok(TokenTree::MetaVar(name))
                    }
                }
                // $$ matches a single $
                TT::Token(Token {
                    ty: TokenType::Dollar,
                    span,
                    ..
                }) => Ok(TokenTree::Token(Token {
                    ty: TokenType::Dollar,
                    literal: None,
                    span: span.combine_with([t.span], span_interner),
                })),
                _ => {
                    diagnostics.add_expected_one_of(
                        tokens.eof_span(),
                        &[
                            TokenType::IdentifierLiteral,
                            TokenType::ParenOpen,
                            TokenType::Dollar,
                        ],
                        None,
                    );
                    return Err(());
                }
            }
        }
        TT::Token(token) => Ok(TokenTree::Token(*token)),
        TT::Delimited(v) => {
            let children = parse_stream(
                &mut TokenStream::new(&v.children, v.close_span),
                parse_def,
                diagnostics,
                span_interner,
            )?;

            Ok(TokenTree::Delimited {
                children: children.into_boxed_slice(),
                delim: v.delimiter,
                open: v.open_span,
                close: v.close_span,
            })
        }
    }?;

    if !parse_def && let TokenTree::Token(tok) = v {
        match tokens.peek() {
            None
            | Some(TT::Delimited { .. })
            | Some(TT::Token(Token {
                ty: TokenType::Dollar,
                ..
            })) => return Ok(v),
            _ => {}
        }
        let mut toks = vec![tok];
        while let Some(&TT::Token(t)) = tokens.peek() {
            if t.ty == TokenType::Dollar {
                break;
            }
            tokens.dismiss();
            toks.push(t);
        }
        return Ok(TokenTree::Tokens(toks.into_boxed_slice()));
    }
    Ok(v)
}

// #[derive(Clone, Copy, Debug, PartialEq, Eq)]
// pub(super) enum ParenType {
//     Paren,
//     Bracket,
//     Curly,
// }
//
// impl ParenType {
//     pub(super) fn to_tt(self) -> (TokenType, TokenType) {
//         match self {
//             ParenType::Paren => (TokenType::ParenLeft, TokenType::ParenRight),
//             ParenType::Bracket => (TokenType::BracketLeft, TokenType::BracketRight),
//             ParenType::Curly => (TokenType::CurlyLeft, TokenType::CurlyRight),
//         }
//     }
//
//     pub(super) fn from_tt(tt: TokenType) -> Option<Self> {
//         match tt {
//             TokenType::ParenLeft => Some(ParenType::Paren),
//             TokenType::BracketLeft => Some(ParenType::Bracket),
//             TokenType::CurlyLeft => Some(ParenType::Curly),
//             _ => None,
//         }
//     }
// }
//
// pub(super) fn match_paren<'arena, 'tok>(
//     tokens: &mut BorrowedTokenStream<'arena, 'tok>,
//     parens: ParenType,
// ) -> Option<&'tok [Token<'arena>]> {
//     let mut paren_count = 0;
//     let start = tokens.pos();
//     let (left, right) = parens.to_tt();
//     loop {
//         let tok = tokens.eat().ty;
//         if tok == left {
//             paren_count += 1;
//         } else if tok == right {
//             match paren_count {
//                 0 => break,
//                 _ => paren_count -= 1,
//             }
//         } else if tok == TokenType::Eof {
//             tokens.set_pos(start);
//             return None;
//         }
//     }
//     let inside_tokens = &tokens.token_holder()[start..tokens.pos() - 1];
//     Some(inside_tokens)
// }
