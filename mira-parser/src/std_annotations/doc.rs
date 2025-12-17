use std::fmt::Debug;

use mira_context::{DocComment, SharedCtx};

use super::*;

#[derive(Clone, Copy, Debug)]
pub enum DocAnnotation {
    /// @doc("...")
    DocComment(DocComment),
    /// @doc(hidden)
    HideItem,
}

annotation!(DocAnnotation(self) = "doc" {
    is_valid_for: |_a, _b| true,
    parse: |tokens, ctx| {
        if let Some(t) = tokens.match_tok(TokenType::DocComment) {
            tokens.finish()?;
            return Ok(DocAnnotation::DocComment(t.doc_literal()))
        }
        if let Some(t) = tokens.match_tok(TokenType::StringLiteral) {
            let v = ctx.add_doc_comment(t.string_literal().to_str());
            tokens.finish()?;
            Ok(DocAnnotation::DocComment(v))
        } else if let Some(t) = tokens.match_tok(TokenType::IdentifierLiteral) {
            match t.string_literal().to_str() {
                "hidden" => {
                    tokens.finish()?;
                    Ok(DocAnnotation::HideItem)
                }
                s => Err(ParsingError::ArbitraryError { error: format!("Invalid doc modifier: `{}`", s.escape_debug()), label: String::new(), label_span: t.span }),
            }
        } else {
            tokens.expect_one_of(&[TokenType::StringLiteral, TokenType::IdentifierLiteral])?;
            unreachable!();
        }
    },
    display: |f| match self {
        Self::DocComment(doc_comment) => Debug::fmt(doc_comment, f),
        Self::HideItem => f.write_str("hidden"),
    },
});
