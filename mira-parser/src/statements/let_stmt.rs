use mira_context::DocComment;
use mira_lexer::TokenType;
use mira_spans::{Ident, Span};

use crate::{
    Expression, Parser, ParsingError, Statement, TypeRef,
    annotations::{AnnotationReceiver, Annotations},
};

#[derive(Clone, Debug)]
pub struct Variable<'ctx> {
    pub name: Ident<'ctx>,
    pub value: Expression<'ctx>,
    pub ty: Option<TypeRef<'ctx>>,
    pub span: Span<'ctx>,
    pub annotations: Annotations<'ctx>,
}

impl<'ctx> Parser<'_, 'ctx> {
    pub(super) fn parse_let(
        &mut self,
        is_static: bool,
        public: bool,
        span: Span<'ctx>,
    ) -> Result<Statement<'ctx>, ParsingError<'ctx>> {
        // dismiss `let`
        self.dismiss();

        // pub only when in global scope
        // [pub] let <identifier>;
        // [pub] let <identifier> = <expr>;
        if public {
            assert!(is_static);
        }
        let comment = if is_static {
            self.take_doc_comment()
        } else {
            DocComment::EMPTY
        };

        let annotations = std::mem::take(&mut *self.data.current_annotations.borrow_mut());
        annotations.are_annotations_valid_for(if is_static {
            AnnotationReceiver::Static
        } else {
            AnnotationReceiver::Variable
        })?;

        let name = self.expect_identifier()?;

        let ty = if self.match_tok_dismiss(TokenType::Colon) {
            Some(TypeRef::parse(self)?)
        } else if is_static {
            self.expect(TokenType::Colon)?;
            None
        } else {
            None
        };

        self.expect(TokenType::Equal)?;

        let value = self.parse_expression()?;
        let end_span = self.expect(TokenType::Semicolon)?.span;
        let var = Variable {
            name,
            value,
            ty,
            span: end_span.combine_with([span], self.ctx().span_interner),
            annotations,
        };
        match is_static {
            true => Ok(Statement::Static {
                var,
                public,
                comment,
            }),
            false => Ok(Statement::Var(var)),
        }
    }
}
