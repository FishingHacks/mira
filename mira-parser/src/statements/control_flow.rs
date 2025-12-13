use mira_lexer::{Delimiter, TokenType};
use mira_spans::{Ident, Span};

use crate::{
    Expression, Parser, ParsingError, Statement,
    annotations::{AnnotationReceiver, Annotations},
};

#[derive(Clone, Debug)]
pub struct If<'ctx> {
    pub condition: Expression<'ctx>,
    pub if_stmt: Box<Statement<'ctx>>,
    pub else_stmt: Option<Box<Statement<'ctx>>>,
    pub span: Span<'ctx>,
    pub annotations: Annotations<'ctx>,
}

#[derive(Clone, Debug)]
pub struct While<'ctx> {
    pub condition: Expression<'ctx>,
    pub child: Box<Statement<'ctx>>,
    pub span: Span<'ctx>,
    pub annotations: Annotations<'ctx>,
}

#[derive(Clone, Debug)]
pub struct For<'ctx> {
    pub iterator: Expression<'ctx>,
    pub var_name: Ident<'ctx>,
    pub child: Box<Statement<'ctx>>,
    pub span: Span<'ctx>,
    pub annotations: Annotations<'ctx>,
}

impl<'ctx> Parser<'_, 'ctx> {
    pub(super) fn parse_if(&mut self, span: Span<'ctx>) -> Result<If<'ctx>, ParsingError<'ctx>> {
        let annotations = std::mem::take(&mut *self.data.current_annotations.borrow_mut());
        annotations.are_annotations_valid_for(AnnotationReceiver::If)?;

        // if (<expr>) <stmt>
        // if (<expr>) <stmt> else <stmt>
        // we have else if support, because else <stmt>, the stmt can be another if expression!
        self.dismiss();

        let cond = self.expect_delim(Delimiter::Parenthesis)?;
        let mut condp = self.subparser(cond.into());

        let condition = condp.parse_expression()?;
        condp.finish()?;

        let if_stmt = self.parse_statement(false)?;
        if self.match_tok_dismiss(TokenType::Else) {
            let else_stmt = self.parse_statement(false)?;
            return Ok(If {
                condition,
                if_stmt: Box::new(if_stmt),
                span: span.combine_with(
                    [cond.close_span, else_stmt.span()],
                    self.ctx().span_interner,
                ),
                else_stmt: Some(Box::new(else_stmt)),
                annotations,
            });
        }
        Ok(If {
            condition,
            if_stmt: Box::new(if_stmt),
            else_stmt: None,
            span: span.combine_with([cond.close_span], self.ctx().span_interner),
            annotations,
        })
    }
    pub(super) fn parse_while(
        &mut self,
        span: Span<'ctx>,
    ) -> Result<While<'ctx>, ParsingError<'ctx>> {
        let annotations = std::mem::take(&mut *self.data.current_annotations.borrow_mut());
        annotations.are_annotations_valid_for(AnnotationReceiver::While)?;

        // while (<expr>) <stmt>
        // we have else if support, because else <stmt>, the stmt can be another if expression!
        self.dismiss();

        let cond = self.expect_delim(Delimiter::Parenthesis)?;
        let mut condp = self.subparser(cond.into());

        let condition = condp.parse_expression()?;
        condp.finish()?;

        let child = self.parse_statement(false)?;

        Ok(While {
            condition,
            span: span.combine_with([child.span(), cond.close_span], self.ctx().span_interner),
            child: Box::new(child),
            annotations,
        })
    }
    pub(super) fn parse_for(&mut self, span: Span<'ctx>) -> Result<For<'ctx>, ParsingError<'ctx>> {
        let annotations = std::mem::take(&mut *self.data.current_annotations.borrow_mut());
        annotations.are_annotations_valid_for(AnnotationReceiver::For)?;

        // for (<identifier> in <expr>) <stmt>
        self.dismiss();

        let cond = self.expect_delim(Delimiter::Parenthesis)?;
        let mut condp = self.subparser(cond.into());

        let var_name = condp.expect_identifier()?;
        condp.expect(TokenType::In)?;
        let iterator = condp.parse_expression()?;
        condp.finish()?;

        let child = Box::new(self.parse_statement(false)?);
        Ok(For {
            span: span.combine_with(
                [child.span(), iterator.span(), cond.close_span],
                self.ctx().span_interner,
            ),
            iterator,
            var_name,
            child,
            annotations,
        })
    }
}
