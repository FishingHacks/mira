use mira_context::DocComment;
use mira_lexer::{Delimiter, TokenType};
use mira_spans::{Ident, Span};

use crate::{
    Argument, Parser, ParsingError, Statement, TypeRef,
    annotations::{AnnotationReceiver, Annotations},
    module::ModuleId,
};

#[derive(Clone, Debug)]
pub struct TraitFunction<'arena> {
    pub name: Ident<'arena>,
    pub args: Vec<Argument<'arena>>,
    pub return_ty: TypeRef<'arena>,
    pub annotations: Annotations<'arena>,
    pub span: Span<'arena>,
    pub comment: DocComment,
}

#[derive(Clone, Debug)]
pub struct Trait<'arena> {
    pub name: Ident<'arena>,
    pub functions: Vec<TraitFunction<'arena>>,
    pub span: Span<'arena>,
    pub annotations: Annotations<'arena>,
    pub module: ModuleId,
    pub public: bool,
    pub comment: DocComment,
}

impl<'ctx> Parser<'_, 'ctx> {
    pub(super) fn parse_trait(
        &mut self,
        public: bool,
        span: Span<'ctx>,
    ) -> Result<Statement<'ctx>, ParsingError<'ctx>> {
        self.dismiss();

        let annotations = std::mem::take(&mut *self.data.current_annotations.borrow_mut());
        annotations.are_annotations_valid_for(AnnotationReceiver::Trait)?;

        let comment = self.take_doc_comment();
        let name = self.expect_identifier()?;

        let delim = self.expect_delim(Delimiter::Curlies)?;
        let mut p = self.subparser(delim.into());

        let mut functions = Vec::new();

        let mut func_comment = None;
        while !p.is_at_end() {
            if let Some(comment) = p.eat_doc_comment() {
                match func_comment {
                    Some(v) => {
                        self.ctx().merge_doc_comments(v, comment);
                        self.ctx().clear_doc_comment(comment);
                    }
                    None => func_comment = Some(comment),
                }
            }
            if p.peek_tok().map(|v| v.ty) == Some(TokenType::AnnotationIntroducer) {
                p.parse_annotation(p.peek_span())?;
                continue;
            }

            let span = p.expect(TokenType::Fn)?.span;
            let func = p.parse_trait_fn(func_comment.take().unwrap_or(DocComment::EMPTY), span)?;
            functions.push(func);
        }

        Ok(Statement::Trait(Trait {
            name,
            functions,
            span: span.combine_with([delim.close_span], self.ctx().span_interner),
            annotations,
            module: self.key(),
            public,
            comment,
        }))
    }

    fn parse_trait_fn(
        &mut self,
        comment: DocComment,
        span: Span<'ctx>,
    ) -> Result<TraitFunction<'ctx>, ParsingError<'ctx>> {
        let name = self.expect_identifier()?;
        let annotations = std::mem::take(&mut *self.data.current_annotations.borrow_mut());
        let mut args = vec![];

        let arg_delim = self.expect_delim(Delimiter::Parenthesis)?;
        let mut argp = self.subparser(arg_delim.into());

        while !argp.is_at_end() {
            if !args.is_empty() {
                if !argp.match_tok_dismiss(TokenType::Comma) {
                    return Err(ParsingError::ExpectedFunctionArgument {
                        span: argp.peek_span(),
                        found: argp.peek_tok(),
                    });
                }

                // for trailing comma
                if argp.is_at_end() {
                    break;
                }
            }

            let name = argp.expect_identifier()?;
            argp.expect(TokenType::Colon)?;

            args.push(Argument::new(TypeRef::parse(&mut argp)?, name))
        }

        let return_ty = if self.match_tok_dismiss(TokenType::ReturnType) {
            TypeRef::parse(self)?
        } else {
            TypeRef::Void(self.peek_span(), 0)
        };

        let close_span = self.expect(TokenType::Semicolon)?.span;

        Ok(TraitFunction {
            name,
            args,
            return_ty,
            annotations,
            span: span.combine_with([close_span], self.ctx().span_interner),
            comment,
        })
    }
}
