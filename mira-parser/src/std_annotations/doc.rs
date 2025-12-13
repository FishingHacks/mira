use std::fmt::Debug;

use mira_context::{DocComment, SharedCtx};
use mira_lexer::Literal;
use mira_spans::interner::symbols;

use super::*;

#[derive(Clone, Copy, Debug)]
pub enum DocAnnotation {
    /// @doc("...")
    DocComment(DocComment),
    /// @doc(hidden)
    HideItem,
}

impl Annotation for DocAnnotation {
    fn get_name(&self) -> &'static str {
        "doc"
    }
}

impl Display for DocAnnotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('@')?;
        f.write_str(self.get_name())?;
        f.write_char('(')?;
        match self {
            DocAnnotation::DocComment(doc_comment) => Debug::fmt(doc_comment, f)?,
            DocAnnotation::HideItem => f.write_str("hidden")?,
        }
        f.write_char(')')
    }
}

pub fn parse<'arena>(
    mut tokens: TokenStream<'arena>,
    ctx: SharedCtx<'arena>,
) -> Result<DocAnnotation, ParsingError<'arena>> {
    if tokens.peek().ty == TokenType::DocComment {
        let Some(Literal::DocComment(v)) = tokens.eat().literal else {
            unreachable!()
        };
        tokens.finish()?;
        return Ok(DocAnnotation::DocComment(v));
    }
    tokens.expect_one_of(&[TokenType::StringLiteral, TokenType::IdentifierLiteral])?;
    if tokens.match_tok_dismiss(TokenType::StringLiteral) {
        let v = ctx.add_doc_comment(tokens.current().string_literal().to_str());
        tokens.finish()?;
        Ok(DocAnnotation::DocComment(v))
    } else if tokens.peek().ty == TokenType::IdentifierLiteral
        && tokens.peek().string_literal() == symbols::DefaultIdents::hidden
    {
        tokens.eat();
        tokens.finish()?;
        Ok(DocAnnotation::HideItem)
    } else if tokens.match_tok_dismiss(TokenType::IdentifierLiteral) {
        Err(ParsingError::ArbitraryError {
            error: "Invalid doc modifier: Expected `hidden`".to_string(),
            label: String::new(),
            label_span: tokens.current().span,
        })
    } else {
        unreachable!()
    }
}
