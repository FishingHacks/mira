use mira_errors::Diagnostics;
use mira_lexer::{Token, TokenType};
use mira_spans::Ident;

use crate::{error::ParsingErrorDiagnosticsExt as _, tokenstream::BorrowedTokenStream};

use super::{KleeneOp, MacroMatchErrorDiagnosticsExt, MetaVarType, SequenceRepetition, TokenTree};

pub fn parse_token_tree<'arena>(
    mut input: BorrowedTokenStream<'arena, '_>,
    // if this is the body or the pattern of the macro
    parse_def: bool,
    diagnostics: &mut Diagnostics<'arena>,
) -> Result<Vec<TokenTree<'arena>>, ()> {
    let mut res = Vec::new();
    let mut success = true;
    while !input.is_at_end() {
        match parse_tree(&mut input, parse_def, diagnostics) {
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
            TokenTree::Token(_) => 0,
            TokenTree::Sequence(seq) => seq.num_captures,
            TokenTree::MetaVar(_) => unreachable!(),
            TokenTree::MetaVarDecl(..) => 1,
        })
        .sum()
}

fn parse_tree<'arena>(
    tokens: &mut BorrowedTokenStream<'arena, '_>,
    // if this is the body or the pattern of the macro
    parse_def: bool,
    diagnostics: &mut Diagnostics<'arena>,
) -> Result<TokenTree<'arena>, ()> {
    match tokens.eat().typ {
        TokenType::Dollar => {
            let next = tokens.eat();
            match next.typ {
                TokenType::ParenLeft => {
                    let Some(toks) = match_paren(tokens) else {
                        diagnostics.add_unmatched_paren(next.span, tokens.eof_span());
                        return Err(());
                    };
                    let res = parse_token_tree(
                        BorrowedTokenStream::new(toks, tokens.current().span),
                        parse_def,
                        diagnostics,
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
    }
}

fn match_paren<'arena, 'tok>(
    tokens: &mut BorrowedTokenStream<'arena, 'tok>,
) -> Option<&'tok [Token<'arena>]> {
    let mut paren_count = 0;
    let start = tokens.pos();
    loop {
        match tokens.eat().typ {
            TokenType::ParenLeft => paren_count += 1,
            TokenType::ParenRight if paren_count > 0 => paren_count -= 1,
            TokenType::ParenRight => break,
            TokenType::Eof => {
                tokens.set_pos(start);
                return None;
            }
            _ => {}
        }
    }
    let inside_tokens = &tokens.token_holder()[start..tokens.pos() - 1];
    Some(inside_tokens)
}
