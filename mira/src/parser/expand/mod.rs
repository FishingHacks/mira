use std::{
    collections::{HashMap, hash_map::Entry},
    fmt::{Display, Write},
    rc::Rc,
    sync::Arc,
};

use mira_errors::Diagnostic;
use mira_lexer::{Token, TokenType, token::IdentDisplay};
use mira_macros::{Display, ErrorData};
use mira_spans::{Ident, SourceFile, Span, Symbol};
use parking_lot::RwLock;
mod builtin_macros;
mod macro_expander;
mod pat_parser;

pub use macro_expander::expand_tokens;

use crate::{
    context::SharedContext,
    error::ParsingError,
    module::Module,
    store::{Store, StoreKey},
    tokenstream::BorrowedTokenStream,
};

use super::{Parser, ParserQueueEntry};

enum FullyMatchedParsers<'arena> {
    None,
    One(MatcherPos<'arena>),
    Multiple,
}

impl<'arena> FullyMatchedParsers<'arena> {
    fn add_position(&mut self, position: MatcherPos<'arena>) {
        match self {
            FullyMatchedParsers::None => *self = Self::One(position),
            FullyMatchedParsers::One(_) => *self = Self::Multiple,
            FullyMatchedParsers::Multiple => {}
        }
    }
}

struct MacroParser<'arena> {
    // the positions for the current step
    cur_pos: Vec<MatcherPos<'arena>>,
    // the positions for the next step to refill cur_pos once it's empty
    next_pos: Vec<MatcherPos<'arena>>,
    // the positions waiting for the parser to parse a meta variable
    waiting_positions: Vec<MatcherPos<'arena>>,

    // pre-allocated empty matches to cheaply clone for macros with many rules
    empty_matches: Rc<Vec<NamedMatch<'arena>>>,
}

#[derive(ErrorData)]
enum MacroError<'arena> {
    #[error("Macro is empty")]
    EmptyMacro(#[primary_label("empty macro")] Span<'arena>),
    #[error("more input is required")]
    MissingTokens(#[primary_label("more input required")] Span<'arena>),
    #[error("ambiguity: multiple successful parses")]
    MultipleFinished(#[primary_label("")] Span<'arena>),
    #[error("More than one meta variable defined as {}", IdentDisplay(*_0))]
    MultipleMetaVars(
        Symbol<'arena>,
        #[primary_label("this meta variable was already defined")] Span<'arena>,
        #[secondary_label("first definition here")] Span<'arena>,
    ),
    #[error("No rules expected this token")]
    UnexpectedToken(#[primary_label("unexpected token")] Span<'arena>),
    #[error("{_0}")]
    Ambiguity(String, #[primary_label("")] Span<'arena>),
    #[error("Unclosed Delimiter")]
    UnmatchedParen(
        #[primary_label("unclosed delimiter")] Span<'arena>,
        #[secondary_label("")] Span<'arena>,
    ),
    #[error("Cannot find macro {}", IdentDisplay(*_1))]
    CannotFindMacro(#[primary_label("")] Span<'arena>, Symbol<'arena>),
    #[error("variable `{}` is still repeating at this depth", IdentDisplay(*_1))]
    VariableStillRepeating(#[primary_label("")] Span<'arena>, Symbol<'arena>),
    #[error(
        "attempted to repeat an expression containing no syntax variables matched as repeating at this depth"
    )]
    NoVariableRepetition(#[primary_label("")] Span<'arena>),
}

type NamedParseResult<'arena> = HashMap<Ident<'arena>, NamedMatch<'arena>>;

enum ParseResult<'arena> {
    Success(NamedParseResult<'arena>),
    /// failed to match the input
    /// the failure diagnostic and the approximate buffer position
    Failure(Diagnostic<'arena>, usize),
    Err(Diagnostic<'arena>),
}

macro_rules! parse_res {
    ($res:expr, $pos:expr) => {
        match $res {
            Ok(v) => v,
            Err(e) => return ParseResult::Failure(e, $pos),
        }
    };
    (@err $res:expr) => {
        match $res {
            Ok(v) => v,
            Err(e) => return ParseResult::Err(e),
        }
    };
}

/// a context with dummy values, the shared context and the source file to construct a parser,
struct ExpandContext<'arena> {
    ctx: SharedContext<'arena>,
    file: Arc<SourceFile>,
    parser_queue: Arc<RwLock<Vec<ParserQueueEntry<'arena>>>>,
    modules: RwLock<Store<Module<'arena>>>,
}

impl<'arena> ExpandContext<'arena> {
    fn new(ctx: SharedContext<'arena>, file: Arc<SourceFile>) -> Self {
        Self {
            ctx,
            file,
            parser_queue: Arc::default(),
            modules: RwLock::default(),
        }
    }
}

impl<'arena> MacroParser<'arena> {
    fn parse_inner(
        &mut self,
        matcher: &[MatcherLoc<'arena>],
        token: &Token<'arena>,
        approx_pos: usize,
    ) -> Option<ParseResult<'arena>> {
        let mut fully_matched = FullyMatchedParsers::None;

        while let Some(mut pos) = self.cur_pos.pop() {
            let loc = &matcher[pos.index];

            match loc {
                MatcherLoc::Token(expected) => {
                    // if the token matches, we can move on
                    if expected.typ == token.typ && expected.literal == token.literal {
                        pos.index += 1;
                        self.next_pos.push(pos);
                    }
                }
                MatcherLoc::Sequence {
                    op,
                    seq_depth,
                    num_metavar_decls,
                    idx_first_after,
                    next_metavar,
                } => {
                    // add a sequence for each meta variable
                    for metavar_idx in *next_metavar..*next_metavar + *num_metavar_decls {
                        pos.push_match(metavar_idx, *seq_depth, NamedMatch::MatchedSeq(Vec::new()));
                    }

                    if matches!(op, KleeneOp::ZeroOrOne | KleeneOp::ZeroOrMore) {
                        // try zero matches by skipping over it
                        self.cur_pos.push(MatcherPos {
                            index: *idx_first_after,
                            matches: Rc::clone(&pos.matches),
                        });
                    }

                    pos.index += 1;
                    self.cur_pos.push(pos);
                }
                MatcherLoc::SequenceKleeneOpNoSep { op, first_index } => {
                    // We are past the end of a sequence with no separator. Try ending the
                    // sequence. If that's not possible, this position will fail quietly when it is
                    // processed next time around the loop.
                    self.cur_pos.push(MatcherPos {
                        index: pos.index + 1,
                        matches: Rc::clone(&pos.matches),
                    });

                    if *op != KleeneOp::ZeroOrOne {
                        // Try another repetition
                        pos.index = *first_index;
                        self.cur_pos.push(pos);
                    }
                }
                MatcherLoc::SequenceSep { separator } => {
                    // We are past the end of a sequence with a separator but we haven't seen the
                    // separator yet. Try ending the sequence. If that's not possible, this
                    // position will fail quietly when it is processed next time around the loop.
                    self.cur_pos.push(MatcherPos {
                        index: pos.index + 2,
                        matches: Rc::clone(&pos.matches),
                    });

                    // if the token matches, go to the next location
                    if token.typ == separator.typ && token.literal == separator.literal {
                        pos.index += 1;
                        self.next_pos.push(pos);
                    }
                }
                MatcherLoc::SequenceKleeneOpAfterSep { idx_first } => {
                    // We are past the sequence separator. This can't be a `?` Kleene op, because
                    // they don't permit separators. Try another repetition.
                    pos.index = *idx_first;
                    self.cur_pos.push(pos);
                }
                MatcherLoc::MetaVarDecl { .. } => self.waiting_positions.push(pos),
                MatcherLoc::Eof => {
                    assert_eq!(pos.index, matcher.len() - 1);
                    // the parser has successfully parsed all the tokens, if the next token is `eof`
                    if token.typ == TokenType::Eof {
                        fully_matched.add_position(pos);
                    }
                }
            }
        }

        // If we reached the end of input, check that there is EXACTLY ONE possible matcher.
        // Otherwise, either the parse is ambiguous (which is an error) or there is a syntax error.
        if token.typ == TokenType::Eof {
            Some(match fully_matched {
                FullyMatchedParsers::None => ParseResult::Failure(
                    MacroError::MissingTokens(token.span).to_error(),
                    approx_pos,
                ),
                FullyMatchedParsers::One(mut pos) => {
                    Rc::make_mut(&mut pos.matches);
                    let mut matches = Rc::try_unwrap(pos.matches).unwrap().into_iter();
                    let mut result = HashMap::new();
                    for loc in matcher {
                        if let MatcherLoc::MetaVarDecl { bind, .. } = loc {
                            match result.entry(*bind) {
                                Entry::Occupied(e) => {
                                    return Some(ParseResult::Err(
                                        MacroError::MultipleMetaVars(
                                            bind.symbol(),
                                            bind.span(),
                                            e.key().span(),
                                        )
                                        .to_error(),
                                    ));
                                }
                                Entry::Vacant(e) => _ = e.insert(matches.next().unwrap()),
                            }
                        }
                    }
                    ParseResult::Success(result)
                }
                FullyMatchedParsers::Multiple => {
                    ParseResult::Err(MacroError::MultipleFinished(token.span).to_error())
                }
            })
        } else {
            None
        }
    }

    fn new() -> Self {
        Self {
            cur_pos: Vec::new(),
            next_pos: Vec::new(),
            waiting_positions: Vec::new(),
            empty_matches: Default::default(),
        }
    }

    fn parse(
        &mut self,
        tokens: BorrowedTokenStream<'arena, '_>,
        matcher: &[MatcherLoc<'arena>],
        ctx: &ExpandContext<'arena>,
    ) -> ParseResult<'arena> {
        let mut parser = Parser::new(
            ctx.ctx,
            tokens,
            ctx.parser_queue.clone(),
            &ctx.modules,
            ctx.file.clone(),
            StoreKey::undefined(),
        );

        self.cur_pos.clear();
        self.cur_pos.push(MatcherPos {
            index: 0,
            matches: Rc::clone(&self.empty_matches),
        });

        loop {
            self.next_pos.clear();
            self.waiting_positions.clear();

            let approx_pos = parser.pos();
            if let Some(res) = self.parse_inner(matcher, &parser.peek(), approx_pos) {
                return res;
            }

            match (self.next_pos.len(), self.waiting_positions.len()) {
                // unexpected token
                (0, 0) => {
                    return ParseResult::Failure(
                        MacroError::UnexpectedToken(parser.current().span).to_error(),
                        approx_pos,
                    );
                }
                (_, 0) => {
                    self.cur_pos.append(&mut self.next_pos);
                    parser.dismiss();
                }
                (0, 1) => {
                    // parse a meta var
                    let mut pos = self.waiting_positions.pop().unwrap();
                    let MatcherLoc::MetaVarDecl {
                        bind,
                        kind,
                        next_metavar,
                        seq_depth,
                    } = &matcher[pos.index]
                    else {
                        unreachable!()
                    };

                    let r#match = match kind {
                        MetaVarType::Token => SingleMatch::Token(parser.eat()),
                        MetaVarType::Ident => SingleMatch::Token(parse_res!(
                            parser
                                .expect(TokenType::IdentifierLiteral)
                                .map_err(ParsingError::to_error)
                                .map_err(|v| {
                                    v.with_secondary_label(
                                        bind.span(),
                                        "While trying to parse this meta variable",
                                    )
                                }),
                            approx_pos
                        )),
                    };

                    pos.index += 1;
                    pos.push_match(*next_metavar, *seq_depth, NamedMatch::ParsedSingle(r#match));
                    self.cur_pos.push(pos);
                }
                _ => {
                    let mut iter =
                        self.waiting_positions
                            .iter()
                            .map(|pos| match &matcher[pos.index] {
                                MatcherLoc::MetaVarDecl { bind, kind, .. } => {
                                    format!("${bind} ({kind})")
                                }
                                _ => unreachable!(),
                            });
                    let mut err = iter.next().unwrap();
                    for entry in iter {
                        err.push_str(" or ");
                        err.push_str(&entry);
                    }
                    if !self.next_pos.is_empty() {
                        err.push_str(" or ");
                        err.write_fmt(format_args!(
                            " or {} other option{s}.",
                            self.next_pos.len(),
                            s = if self.next_pos.len() == 1 { "" } else { "s" }
                        ))
                        .unwrap();
                    }
                    let s = format!("local ambiguity: multiple parsing options: {err}",);
                    return ParseResult::Err(
                        MacroError::Ambiguity(s, parser.peek().span).to_error(),
                    );
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
enum SingleMatch<'arena> {
    Token(Token<'arena>),
}

#[derive(Debug, Clone)]
enum NamedMatch<'arena> {
    MatchedSeq(Vec<NamedMatch<'arena>>),
    ParsedSingle(SingleMatch<'arena>),
}

impl NamedMatch<'_> {
    fn repetitions_at_idx(&self, depth: &[usize]) -> Option<usize> {
        let mut me = self;
        for i in depth.iter().copied() {
            match me {
                NamedMatch::MatchedSeq(named_matchs) => me = &named_matchs[i],
                NamedMatch::ParsedSingle(_) => return None,
            }
        }
        match me {
            NamedMatch::MatchedSeq(named_matchs) => Some(named_matchs.len()),
            NamedMatch::ParsedSingle(_) => None,
        }
    }
}

#[derive(Debug)]
struct MatcherPos<'arena> {
    index: usize,
    matches: Rc<Vec<NamedMatch<'arena>>>,
}

impl<'arena> MatcherPos<'arena> {
    #[inline(always)]
    fn push_match(&mut self, metavar_idx: usize, seq_depth: usize, r#match: NamedMatch<'arena>) {
        let matches = Rc::make_mut(&mut self.matches);
        match seq_depth {
            0 => {
                // matches contains an index for each metavar value. if we're not in a sequence,
                // this metavar couldn't have been pushed yet.
                assert_eq!(metavar_idx, matches.len());
                matches.push(r#match);
            }
            _ => {
                // while appending to a sequence, we have to find the correct matchedseq for the
                // depth

                // a metavar entry for this match must exist
                let mut curr = &mut matches[metavar_idx];
                // we iterate from 1, because seq_depth 1 is just one sequence in,
                // meaning `curr` is already the correct match.
                for _ in 1..seq_depth {
                    // a sequence can only have a matchedseq
                    match curr {
                        NamedMatch::MatchedSeq(v) => curr = v.last_mut().unwrap(),
                        _ => unreachable!(),
                    }
                }
                // a sequence can only have a matchedseq
                match curr {
                    NamedMatch::MatchedSeq(v) => v.push(r#match),
                    _ => unreachable!(),
                }
            }
        }
    }
}

type MacroPattern<'arena> = Box<[MatcherLoc<'arena>]>;
type MacroBody<'arena> = Box<[TokenTree<'arena>]>;
struct Macro<'arena> {
    name: Ident<'arena>,
    cases: Vec<(MacroPattern<'arena>, MacroBody<'arena>)>,
}

#[derive(Clone, Debug)]
enum MatcherLoc<'arena> {
    Token(Token<'arena>),
    Sequence {
        op: KleeneOp,
        seq_depth: usize,
        num_metavar_decls: usize,
        idx_first_after: usize,
        next_metavar: usize,
    },
    SequenceKleeneOpNoSep {
        op: KleeneOp,
        first_index: usize,
    },
    SequenceSep {
        separator: Token<'arena>,
    },
    SequenceKleeneOpAfterSep {
        idx_first: usize,
    },
    MetaVarDecl {
        bind: Ident<'arena>,
        kind: MetaVarType,
        next_metavar: usize,
        seq_depth: usize,
    },
    Eof,
}

impl Display for MatcherLoc<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MatcherLoc::SequenceSep { separator: token } | MatcherLoc::Token(token) => {
                f.write_fmt(format_args!("'{token}'"))
            }
            MatcherLoc::Sequence { .. } => f.write_str("sequence start"),
            MatcherLoc::SequenceKleeneOpAfterSep { .. }
            | MatcherLoc::SequenceKleeneOpNoSep { .. } => f.write_str("sequence end"),
            MatcherLoc::MetaVarDecl { bind, kind, .. } => {
                f.write_fmt(format_args!("meta-variable `${bind}:{kind}`"))
            }
            MatcherLoc::Eof => f.write_str("end of macro"),
        }
    }
}

fn compute_locs<'arena>(matcher: &[TokenTree<'arena>]) -> Vec<MatcherLoc<'arena>> {
    fn inner<'arena>(
        tts: &[TokenTree<'arena>],
        locs: &mut Vec<MatcherLoc<'arena>>,
        next_metavar: &mut usize,
        seq_depth: usize,
    ) {
        for tt in tts {
            match tt {
                TokenTree::Tokens(_) => unreachable!(),
                TokenTree::Token(token) => {
                    locs.push(MatcherLoc::Token(*token));
                }
                TokenTree::Sequence(seq) => {
                    // We can't determine `idx_first_after` and construct the final
                    // `MatcherLoc::Sequence` until after `inner()` is called and the sequence end
                    // pieces are processed. So we push a dummy value (`Eof` is cheapest to
                    // construct) now, and overwrite it with the proper value below.
                    let dummy = MatcherLoc::Eof;
                    locs.push(dummy);

                    let next_metavar_orig = *next_metavar;
                    let op = seq.kleene;
                    let idx_first = locs.len();
                    let idx_seq = idx_first - 1;
                    inner(&seq.content, locs, next_metavar, seq_depth + 1);

                    if let Some(separator) = &seq.separator {
                        locs.push(MatcherLoc::SequenceSep {
                            separator: *separator,
                        });
                        locs.push(MatcherLoc::SequenceKleeneOpAfterSep { idx_first });
                    } else {
                        locs.push(MatcherLoc::SequenceKleeneOpNoSep {
                            op,
                            first_index: idx_first,
                        });
                    }

                    // Overwrite the dummy value pushed above with the proper value.
                    locs[idx_seq] = MatcherLoc::Sequence {
                        op,
                        num_metavar_decls: seq.num_captures,
                        idx_first_after: locs.len(),
                        next_metavar: next_metavar_orig,
                        seq_depth,
                    };
                }
                TokenTree::MetaVarDecl(bind, kind) => {
                    locs.push(MatcherLoc::MetaVarDecl {
                        bind: *bind,
                        kind: *kind,
                        next_metavar: *next_metavar,
                        seq_depth,
                    });
                    *next_metavar += 1;
                }
                TokenTree::MetaVar(..) => unreachable!(),
            }
        }
    }

    let mut locs = vec![];
    let mut next_metavar = 0;
    inner(matcher, &mut locs, &mut next_metavar, 0);

    // A final entry is needed for eof.
    locs.push(MatcherLoc::Eof);

    locs
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum KleeneOp {
    /// Kleene Start (`*`)
    ZeroOrMore,
    /// Kleene Plus (`+`)
    OneOrMore,
    /// Kleene Optional (`?`)
    ZeroOrOne,
}

#[derive(Clone, PartialEq, Debug)]
struct SequenceRepetition<'arena> {
    content: Vec<TokenTree<'arena>>,
    /// the optional seperator:
    /// $($item:ident),+
    ///               ^- this bit
    separator: Option<Token<'arena>>,
    kleene: KleeneOp,
    num_captures: usize,
    span: Span<'arena>,
}

#[derive(Clone, Display, Copy, PartialEq, Eq, Debug)]
enum MetaVarType {
    #[display("token")]
    Token,
    #[display("ident")]
    Ident,
}

impl MetaVarType {
    fn from_str(s: &str) -> Option<Self> {
        match s {
            "tok" => Some(Self::Token),
            "token" => Some(Self::Token),
            "ident" => Some(Self::Ident),
            _ => None,
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
enum TokenTree<'arena> {
    Token(Token<'arena>),
    Tokens(Box<[Token<'arena>]>),
    Sequence(SequenceRepetition<'arena>),
    MetaVar(Ident<'arena>),
    MetaVarDecl(Ident<'arena>, MetaVarType),
}

impl<'arena> TokenTree<'arena> {
    /// calls the function `f` on all meta var children with their name, returning how many there
    /// were in total.
    fn meta_vars<E>(&self, mut f: impl FnMut(Ident<'arena>) -> Result<(), E>) -> Result<(), E> {
        fn inner<'arena, E>(
            me: &TokenTree<'arena>,
            f: &mut impl FnMut(Ident<'arena>) -> Result<(), E>,
        ) -> Result<(), E> {
            match me {
                TokenTree::Token(_) | TokenTree::Tokens(_) | TokenTree::MetaVarDecl(..) => Ok(()),
                TokenTree::Sequence(sequence_repetition) => {
                    for v in sequence_repetition.content.iter() {
                        inner(v, f)?;
                    }
                    Ok(())
                }
                TokenTree::MetaVar(ident) => f(*ident),
            }
        }
        inner(self, &mut f)
    }
}
