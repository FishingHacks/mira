use std::collections::HashMap;

use mira_context::DocComment;
use mira_lexer::{Delimiter, TokenType};
use mira_spans::{Ident, Span};

use crate::{
    FunctionContract, Generic, Parser, ParsingError, Statement, TypeRef,
    annotations::AnnotationReceiver,
};

impl<'ctx> Parser<'_, 'ctx> {
    pub(super) fn parse_struct(
        &mut self,
        public: bool,
        span: Span<'ctx>,
    ) -> Result<Statement<'ctx>, ParsingError<'ctx>> {
        let annotations = std::mem::take(&mut *self.data.current_annotations.borrow_mut());
        let comment = self.take_doc_comment();
        annotations.are_annotations_valid_for(AnnotationReceiver::Struct)?;
        self.dismiss();

        // struct Name { ... fields ...; implementation area }
        // fields: field: type,[...]
        // implementation area: fn implementation area | impl TraitName { implementation area no trait } implementation area | ""
        // implementation area no trait: fn implementation area no trait | ""
        let name = self.expect_identifier()?;

        let mut generics = vec![];
        if self.match_tok_dismiss(TokenType::LessThan) {
            while !self.match_tok_dismiss(TokenType::GreaterThan) {
                if generics.len() > 1 {
                    self.expect_one_of(&[TokenType::Comma, TokenType::GreaterThan])?;

                    if self.match_tok_dismiss(TokenType::GreaterThan) {
                        break;
                    }
                }

                generics.push(Generic::parse(self)?);
            }
        }

        let delim = self.expect_delim(Delimiter::Curlies)?;
        let mut p = self.subparser(delim.into());
        let mut elements = vec![];

        while !p.is_at_end() && !p.match_tok_dismiss(TokenType::Semicolon) {
            let comment = p.eat_doc_comment().unwrap_or(DocComment::EMPTY);
            if !elements.is_empty() {
                // needs comma
                p.expect(TokenType::Comma)?;
                // for trailing commas
                if p.is_at_end() || p.match_tok_dismiss(TokenType::Semicolon) {
                    break;
                }
            }
            let name = p.expect_identifier()?;
            p.expect(TokenType::Colon)?;
            let ty = TypeRef::parse(&mut p)?;
            elements.push((name, ty, comment));
        }

        let mut global_impl =
            HashMap::<Ident<'ctx>, (FunctionContract<'ctx>, Statement<'ctx>)>::new();
        let mut impls = Vec::new();

        // implementation area. has a list of functions
        // or has impl <TraitName> { <list of functions for the trait> }

        while !p.is_at_end() {
            match p.peek_tok().map(|v| v.ty) {
                Some(TokenType::Fn) => {
                    if let Some(doc_comment) = p.eat_doc_comment() {
                        // overriding here is fine because we don't parse annotations,
                        // meaning there can only ever be one doc comment.
                        p.data.current_doc_comment.set(Some(doc_comment));
                    }
                    let func = p.parse_callable(false, false, p.peek_span());
                    p.data.current_doc_comment.set(None);
                    let func = func?;
                    let name = func
                        .0
                        .contract
                        .name
                        .as_ref()
                        .cloned()
                        .expect("non-anonymous function without name");
                    if let Some(other_func) = global_impl.get(&name) {
                        return Err(ParsingError::FunctionAlreadyDefined {
                            span: func.0.span,
                            name: name.symbol(),
                            first_func_span: other_func.0.span,
                        });
                    }
                    global_impl.insert(name, (func.0.contract, func.1));
                }
                Some(TokenType::Impl) => {
                    let span = p.eat().unwrap().span();
                    let trait_name = p.expect_identifier()?;
                    let mut current_impl =
                        HashMap::<Ident<'ctx>, (FunctionContract<'ctx>, Statement<'ctx>)>::new();

                    let impl_delimit = p.expect_delim(Delimiter::Curlies)?;
                    let mut implp = p.subparser(impl_delimit.into());

                    while !implp.is_at_end() {
                        if let Some(doc_comment) = implp.eat_doc_comment() {
                            // overriding here is fine because we don't parse annotations,
                            // meaning there can only ever be one doc comment.
                            implp.data.current_doc_comment.set(Some(doc_comment));
                        }

                        if implp.peek_tok().map(|v| v.ty) != Some(TokenType::Fn) {
                            implp.data.current_doc_comment.set(None);
                            return Err(ParsingError::StructImplRegionExpect {
                                span: implp.peek_span(),
                                found: implp.peek_tok(),
                                is_trait_impl: true,
                            });
                        }

                        let func = implp.parse_callable(false, false, implp.peek_span());
                        implp.data.current_doc_comment.set(None);
                        let func = func?;
                        let name = func
                            .0
                            .contract
                            .name
                            .as_ref()
                            .cloned()
                            .expect("non-anonymous function without name");
                        if let Some(other_func) = current_impl.get(&name) {
                            return Err(ParsingError::FunctionAlreadyDefined {
                                span: func.0.span,
                                name: name.symbol(),
                                first_func_span: other_func.0.span,
                            });
                        }
                        current_impl.insert(name, (func.0.contract, func.1));
                    }

                    let span = span.combine_with([impl_delimit.close_span], p.ctx().span_interner);
                    impls.push((trait_name, current_impl, span));
                }
                _ => {
                    return Err(ParsingError::StructImplRegionExpect {
                        span: p.peek_span(),
                        found: p.peek_tok(),
                        is_trait_impl: false,
                    });
                }
            }
        }

        let span = span.combine_with([delim.close_span], self.ctx().span_interner);
        Ok(Statement::Struct {
            name,
            elements,
            span,
            global_impl,
            impls,
            annotations,
            generics,
            public,
            comment,
        })
    }
}
