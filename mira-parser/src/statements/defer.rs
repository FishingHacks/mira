use mira_lexer::{Delimiter, TokenType};
use mira_spans::Span;

use crate::{For, If, Parser, ParsingError, Statement, While, annotations::AnnotationReceiver};

impl<'ctx> Parser<'_, 'ctx> {
    pub(super) fn parse_defer(
        &mut self,
        span: Span<'ctx>,
    ) -> Result<Statement<'ctx>, ParsingError<'ctx>> {
        // skip `defer`
        self.dismiss();

        let res = match self.peek_tok().map(|v| v.ty) {
            Some(TokenType::For) => self.parse_if(span).map(Statement::DeferredIf),
            Some(TokenType::While) => self.parse_while(span).map(Statement::DeferredWhile),
            Some(TokenType::If) => self.parse_for(span).map(Statement::DeferredFor),
            Some(TokenType::CurlyOpen) => {
                let annotations = std::mem::take(&mut *self.data.current_annotations.borrow_mut());
                annotations.are_annotations_valid_for(AnnotationReceiver::Block)?;

                let delim = self.expect_delim(Delimiter::Curlies)?;
                let mut statements = vec![];
                let mut p = self.subparser(delim.into());

                while !p.is_at_end() {
                    statements.push(p.parse_statement(false)?);
                }

                Ok(Statement::Block(
                    statements.into_boxed_slice(),
                    span.combine_with([delim.close_span], self.ctx().span_interner),
                    annotations,
                ))
            }
            _ => {
                let expr = self.parse_expression()?;
                self.expect(TokenType::Semicolon)?;
                Ok(Statement::DeferredExpr(expr))
            }
        }?;

        // ensure there are no defers or returns.
        let mut left = Vec::new();

        match &res {
            Statement::DeferredFor(For { child, .. })
            | Statement::DeferredWhile(While { child, .. }) => left.push(&**child),
            Statement::DeferredBlock(stmts, _, _) => left.extend(stmts),
            Statement::DeferredExpr(_) => {}
            Statement::DeferredIf(v) => {
                left.push(&v.if_stmt);
                if let Some(else_stmt) = &v.else_stmt {
                    left.push(else_stmt);
                }
            }
            _ => unreachable!(),
        }

        while let Some(v) = left.pop() {
            match v {
                Statement::If(If {
                    else_stmt: None,
                    if_stmt,
                    ..
                }) => left.push(if_stmt),
                Statement::If(If {
                    else_stmt: Some(else_stmt),
                    if_stmt,
                    ..
                }) => left.extend([&**else_stmt, &**if_stmt]),
                Statement::While(While { child, .. }) => left.push(child),
                Statement::For(For { child, .. }) => left.push(child),

                Statement::Block(stmts, ..) => left.extend(stmts),

                Statement::DeferredExpr(_)
                | Statement::DeferredBlock(..)
                | Statement::DeferredWhile(_)
                | Statement::DeferredFor(_)
                | Statement::DeferredIf(_)
                | Statement::Return(..) => {
                    return Err(ParsingError::DeferOrReturnInDefer(v.span()));
                }

                Statement::None | Statement::Var(_) | Statement::Expr(_) => {}
                Statement::Function { .. }
                | Statement::ExternalFunction { .. }
                | Statement::Struct { .. }
                | Statement::Trait(_)
                | Statement::ModuleAsm(..)
                | Statement::Static { .. }
                | Statement::Use { .. }
                | Statement::Mod { .. }
                | Statement::BakedFunction(..)
                | Statement::BakedExternalFunction(..)
                | Statement::BakedStruct(..)
                | Statement::BakedStatic(..)
                | Statement::BakedTrait(..) => unreachable!(),
            }
        }

        Ok(res)
    }
}
