use std::collections::HashMap;

use mira_context::DocComment;
use mira_lexer::{Delimiter, TokenType};
use mira_spans::{Ident, Span};

mod annotation;
mod control_flow;
mod defer;
mod let_stmt;
mod struct_def;
mod trait_def;
pub use control_flow::{For, If, While};
pub(crate) use display::display_contract;
pub use let_stmt::Variable;
pub use trait_def::{Trait, TraitFunction};

use crate::{
    Expression, Generic, LiteralValue, Parser, ParsingError, PathWithoutGenerics, TypeRef,
    annotations::{AnnotationReceiver, Annotations},
    module::{
        ExternalFunctionId, FunctionId, Module, ModuleContext, ModuleId, StaticId, StructId,
        TraitId,
    },
    tokenstream::BailType,
};

mod display;

#[derive(Clone, Debug)]
pub enum BakableFunction<'arena> {
    Function(Box<(FunctionContract<'arena>, Statement<'arena>)>),
    BakedFunction(FunctionId),
}

#[derive(Clone, Debug)]
pub enum Statement<'ctx> {
    If(If<'ctx>),
    While(While<'ctx>),
    For(For<'ctx>),
    Return(Option<Expression<'ctx>>, Span<'ctx>),
    Block(Box<[Statement<'ctx>]>, Span<'ctx>, Annotations<'ctx>),
    Var(Variable<'ctx>),
    Expr(Expression<'ctx>),
    Function {
        contract: FunctionContract<'ctx>,
        body: Box<Statement<'ctx>>,
        span: Span<'ctx>,
        public: bool,
    },
    ExternalFunction {
        contract: FunctionContract<'ctx>,
        body: Option<Box<Statement<'ctx>>>,
        span: Span<'ctx>,
        public: bool,
    },
    Struct {
        name: Ident<'ctx>,
        elements: Vec<(Ident<'ctx>, TypeRef<'ctx>, DocComment)>,
        span: Span<'ctx>,
        global_impl: HashMap<Ident<'ctx>, (FunctionContract<'ctx>, Statement<'ctx>)>,
        #[allow(clippy::type_complexity)]
        impls: Vec<(
            Ident<'ctx>,
            HashMap<Ident<'ctx>, (FunctionContract<'ctx>, Statement<'ctx>)>,
            Span<'ctx>,
        )>,
        generics: Vec<Generic<'ctx>>,
        annotations: Annotations<'ctx>,
        public: bool,
        comment: DocComment,
    },
    Trait(Trait<'ctx>),
    ModuleAsm(Span<'ctx>, String),
    Static {
        var: Variable<'ctx>,
        public: bool,
        comment: DocComment,
    },

    DeferredExpr(Expression<'ctx>),
    DeferredBlock(Box<[Statement<'ctx>]>, Span<'ctx>, Annotations<'ctx>),
    DeferredWhile(While<'ctx>),
    DeferredFor(For<'ctx>),
    DeferredIf(If<'ctx>),

    Use {
        span: Span<'ctx>,
        path: PathWithoutGenerics<'ctx>,
        alias: Option<Ident<'ctx>>,
        public: bool,
    },
    Mod {
        span: Span<'ctx>,
        name: Ident<'ctx>,
        public: bool,
        comment: Option<DocComment>,
    },
    None,

    BakedFunction(FunctionId, Span<'ctx>),
    BakedExternalFunction(ExternalFunctionId, Span<'ctx>),
    BakedStruct(StructId, Span<'ctx>),
    BakedStatic(StaticId, Span<'ctx>),
    BakedTrait(TraitId, Span<'ctx>),
}

impl<'arena> Statement<'arena> {
    pub fn span(&self) -> Span<'arena> {
        match self {
            Self::DeferredExpr(expr) | Self::Expr(expr) => expr.span(),
            Self::Static { var, .. } => var.span,
            Self::ExternalFunction { span, .. }
            | Self::Function { span, .. }
            | Self::DeferredBlock(_, span, _)
            | Self::Block(_, span, _)
            | Self::ModuleAsm(span, _)
            | Self::Return(_, span)
            | Self::Struct { span, .. }
            | Self::Var(Variable { span, .. })
            | Self::BakedFunction(_, span)
            | Self::BakedExternalFunction(_, span)
            | Self::BakedStruct(_, span)
            | Self::BakedStatic(_, span)
            | Self::BakedTrait(_, span)
            | Self::Trait(Trait { span, .. })
            | Self::Use { span, .. }
            | Self::Mod { span, .. } => *span,
            Self::For(v) | Self::DeferredFor(v) => v.span,
            Self::If(v) | Self::DeferredIf(v) => v.span,
            Self::While(v) | Self::DeferredWhile(v) => v.span,
            Self::None => unreachable!(),
        }
    }

    pub fn bake_functions(
        &mut self,
        module: &mut Module<'arena>,
        module_key: ModuleId,
        context: &ModuleContext<'arena>,
    ) {
        match self {
            Self::BakedFunction(..)
            | Self::BakedExternalFunction(..)
            | Self::BakedTrait(..)
            | Self::BakedStatic(..)
            | Self::BakedStruct(..)
            | Self::Return(None, ..)
            | Self::ModuleAsm(..)
            | Self::None
            | Self::Trait { .. } => (),
            Self::ExternalFunction { .. } => {
                unreachable!("external function in a non-top-level scope")
            }
            Self::Use { .. } => unreachable!("use in a non-top-level scope"),
            Self::Mod { .. } => unreachable!("mod in a non-top-level scope"),
            Self::Struct { .. } => unreachable!("struct in a non-top-level scope"),
            Self::Function { .. } => unreachable!("function in a non-top-level scope"),
            Self::Block(statements, ..) | Self::DeferredBlock(statements, ..) => statements
                .iter_mut()
                .for_each(|stmt| stmt.bake_functions(module, module_key, context)),
            Self::Var(Variable { value, .. }) => value.bake_functions(module, module_key, context),
            Self::Static { var, .. } => var.value.bake_functions(module, module_key, context),
            Self::Expr(expr) | Self::DeferredExpr(expr) => {
                expr.bake_functions(module, module_key, context)
            }
            Self::For(v) | Self::DeferredFor(v) => {
                v.iterator.bake_functions(module, module_key, context);
                v.child.bake_functions(module, module_key, context);
            }
            Self::While(v) | Self::DeferredWhile(v) => {
                v.condition.bake_functions(module, module_key, context);
                v.child.bake_functions(module, module_key, context);
            }
            Self::If(v) | Self::DeferredIf(v) => {
                v.condition.bake_functions(module, module_key, context);
                v.if_stmt.bake_functions(module, module_key, context);
                if let Some(stmt) = &mut v.else_stmt {
                    stmt.bake_functions(module, module_key, context);
                }
            }
            Self::Return(Some(val), ..) => val.bake_functions(module, module_key, context),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Argument<'arena> {
    pub name: Ident<'arena>,
    pub ty: TypeRef<'arena>,
}

impl<'arena> Argument<'arena> {
    pub fn new(ty: TypeRef<'arena>, name: Ident<'arena>) -> Self {
        Self { name, ty }
    }
}

#[derive(Clone, Debug)]
pub struct FunctionContract<'arena> {
    pub comment: DocComment,
    pub name: Option<Ident<'arena>>,
    pub arguments: Vec<Argument<'arena>>,
    pub return_type: TypeRef<'arena>,
    pub span: Span<'arena>,
    pub annotations: Annotations<'arena>,
    pub generics: Vec<Generic<'arena>>,
}

/*
Callables (aka. functions):

Anonymous: means no name

fn <name or nothing if anonymous>(arguments) body

name => Identifier
body => Statement
arguments => list of argument seperated by comma, trailing comma allowed, the last argument may have the spread parameter (...) in front (iex: ...rest), which means all the arguments from here on will get put into a list and supplied to that argument
=> also arguments can have `copy` in front to indicate we'll copy them into a new variable before calling the function
*/

pub(super) struct Callable<'arena> {
    pub contract: FunctionContract<'arena>,
    pub span: Span<'arena>,
    /// never set if parse_callable is called with `anonymous` set to true
    pub public: bool,
}

static TOP_LEVEL_BAIL: [BailType; 10] = [
    BailType::Before(TokenType::Pub),
    BailType::Before(TokenType::Struct),
    BailType::Before(TokenType::Extern),
    BailType::Before(TokenType::Fn),
    BailType::Before(TokenType::Trait),
    BailType::Before(TokenType::Let),
    BailType::Before(TokenType::Use),
    BailType::Before(TokenType::Mod),
    BailType::Before(TokenType::Asm),
    BailType::Before(TokenType::AnnotationIntroducer),
];

impl<'ctx> Parser<'_, 'ctx> {
    pub fn parse_all(&mut self) -> (Vec<Statement<'ctx>>, Vec<ParsingError<'ctx>>) {
        let mut statements = vec![];
        let mut errors = vec![];

        while !self.is_at_end() {
            match self.parse_statement_part(true) {
                Err(error) => {
                    errors.push(error);
                    self.bail(&TOP_LEVEL_BAIL);
                }
                Ok(Some(statement)) => statements.push(statement),
                Ok(None) => {}
            }
        }

        if !self.data.current_annotations.borrow().is_empty() {
            errors.push(ParsingError::ExpectedStatement(self.peek_span()));
        }

        (statements, errors)
    }

    pub fn parse_statement(
        &mut self,
        is_global: bool,
    ) -> Result<Statement<'ctx>, ParsingError<'ctx>> {
        while !self.is_at_end() {
            if let Some(statement) = self.parse_statement_part(is_global)? {
                return Ok(statement);
            }
        }
        Err(ParsingError::ExpectedStatement(self.peek_span()))
    }

    pub fn parse_statement_part(
        &mut self,
        is_global: bool,
    ) -> Result<Option<Statement<'ctx>>, ParsingError<'ctx>> {
        macro_rules! invalid_kw {
            ($kw: literal) => {
                return Err(ParsingError::InvalidKeyword {
                    keyword: $kw,
                    span: self.peek_span(),
                })
            };
        }

        let Some(next) = self.peek_tok() else {
            return Err(ParsingError::ExpectedStatement(self.peek_span()));
        };

        if !self.data.current_annotations.borrow().is_empty()
            && !matches!(
                next.ty,
                TokenType::AnnotationIntroducer
                    | TokenType::Extern
                    | TokenType::Fn
                    | TokenType::CurlyOpen
                    | TokenType::Struct
                    | TokenType::For
                    | TokenType::While
                    | TokenType::Let
                    | TokenType::Trait
                    | TokenType::Pub
                    | TokenType::If
            )
        {
            return Err(ParsingError::ExpectedAnnotationStatement(self.peek_span()));
        }

        if let Some(doc_comment) = self.eat_doc_comment() {
            match self.data.current_doc_comment.get() {
                Some(v) => {
                    self.ctx().merge_doc_comments(v, doc_comment);
                    self.ctx().clear_doc_comment(doc_comment);
                }
                None => self.data.current_doc_comment.set(Some(doc_comment)),
            }
        }
        let consumes_doc_comment = !matches!(
            next.ty,
            TokenType::AnnotationIntroducer | TokenType::DocComment | TokenType::ModuleDocComment
        );

        let maybe_statement = match next.ty {
            TokenType::Extern if !is_global => invalid_kw!("external value/function"),
            TokenType::Struct if !is_global => invalid_kw!("struct definition"),
            TokenType::Use if !is_global => invalid_kw!("use"),
            TokenType::Mod if !is_global => invalid_kw!("mod"),
            TokenType::Trait if !is_global => invalid_kw!("trait"),
            TokenType::Pub if !is_global => invalid_kw!("pub"),

            TokenType::Return if is_global => invalid_kw!("return"),
            TokenType::CurlyOpen if is_global => invalid_kw!("code block"),
            TokenType::If if is_global => invalid_kw!("if statement"),
            TokenType::While if is_global => invalid_kw!("while loop"),
            TokenType::For if is_global => invalid_kw!("for loop"),
            TokenType::Defer if is_global => invalid_kw!("defer"),

            TokenType::Asm if is_global => self.parse_global_asm(next.span).map(Some),
            TokenType::Trait => self.parse_trait(false, next.span).map(Some),
            TokenType::Let => self.parse_let(is_global, false, next.span).map(Some),
            TokenType::CurlyOpen => self.parse_block().map(Some),
            TokenType::Return => self.parse_return(next.span).map(Some),
            TokenType::Defer => self.parse_defer(next.span).map(Some),
            TokenType::If => self.parse_if(next.span).map(Statement::If).map(Some),
            TokenType::While => self.parse_while(next.span).map(Statement::While).map(Some),
            TokenType::For => self.parse_for(next.span).map(Statement::For).map(Some),
            TokenType::Struct => self.parse_struct(false, next.span).map(Some),
            TokenType::Fn if !is_global => {
                let (Callable { contract, span, .. }, body) =
                    self.parse_callable(false, false, next.span)?;
                contract
                    .annotations
                    .are_annotations_valid_for(AnnotationReceiver::Function)?;
                let name = contract.name.unwrap();
                let func = LiteralValue::AnonymousFunction(contract, Box::new(body));
                let value = Expression::Literal(func, span);
                let var = Variable {
                    name,
                    value,
                    ty: None,
                    span,
                    annotations: Annotations::new(),
                };
                Ok(Some(Statement::Var(var)))
            }
            TokenType::Fn => self
                .parse_callable(false, false, next.span)
                .and_then(|(callable, body)| {
                    callable
                        .contract
                        .annotations
                        .are_annotations_valid_for(AnnotationReceiver::Function)?;
                    Ok(Statement::Function {
                        contract: callable.contract,
                        body: Box::new(body),
                        span: callable.span,
                        public: callable.public,
                    })
                })
                .map(Some),
            TokenType::AnnotationIntroducer => {
                self.parse_annotation(next.span)?;
                Ok(None)
            }
            TokenType::Extern => self.parse_external(false, next.span).map(Some),
            TokenType::Use => self.parse_use(false, next.span).map(Some),
            TokenType::Mod => self.parse_mod(false, next.span).map(Some),
            TokenType::Pub => self.parse_pub(next.span).map(Some),
            _ if is_global => {
                return Err(ParsingError::ExpressionAtTopLevel(next.span));
            }
            _ => self.parse_expression_stmt().map(Some),
        };

        if consumes_doc_comment {
            self.data.current_doc_comment.set(None);
        }

        let maybe_statement = maybe_statement?;
        if maybe_statement.is_some() {
            assert_eq!(
                self.data.current_annotations.borrow().len(),
                0,
                "more than 0 annotations left after parsing a statement"
            );
        }

        Ok(maybe_statement)
    }

    fn parse_expression_stmt(&mut self) -> Result<Statement<'ctx>, ParsingError<'ctx>> {
        let expr = self.parse_expression()?;
        self.expect(TokenType::Semicolon)?;
        Ok(Statement::Expr(expr))
    }

    fn parse_pub(&mut self, span: Span<'ctx>) -> Result<Statement<'ctx>, ParsingError<'ctx>> {
        self.dismiss();
        match self.peek_tok().map(|v| v.ty) {
            Some(TokenType::Fn) => {
                self.parse_callable(false, true, span)
                    .and_then(|(callable, body)| {
                        assert!(callable.public);
                        callable
                            .contract
                            .annotations
                            .are_annotations_valid_for(AnnotationReceiver::Function)?;
                        Ok(Statement::Function {
                            contract: callable.contract,
                            body: Box::new(body),
                            span: callable.span,
                            public: true,
                        })
                    })
            }
            Some(TokenType::Let) => self.parse_let(true, true, span),
            Some(TokenType::Struct) => self.parse_struct(true, span),
            Some(TokenType::Extern) => self.parse_external(true, span),
            Some(TokenType::Trait) => self.parse_trait(true, span),
            Some(TokenType::Use) => self.parse_use(true, span),
            Some(TokenType::Mod) => self.parse_mod(true, span),
            _ => Err(ParsingError::ExpectedElementForPub {
                span: self.peek_span(),
                found: self.peek_tok(),
            }),
        }
    }

    fn parse_use(
        &mut self,
        public: bool,
        span: Span<'ctx>,
    ) -> Result<Statement<'ctx>, ParsingError<'ctx>> {
        // dismiss the `use` token.
        self.dismiss();
        let path = PathWithoutGenerics::parse(self)?;
        if let Some(t) = self.match_tok(TokenType::Semicolon) {
            let span = t.span.combine_with([span], self.ctx().span_interner);
            return Ok(Statement::Use {
                span,
                path,
                alias: None,
                public,
            });
        }
        self.expect_one_of(&[TokenType::As, TokenType::Semicolon])?;
        let alias = self.expect_identifier()?;
        let span = self
            .expect(TokenType::Semicolon)?
            .span
            .combine_with([span], self.ctx().span_interner);
        Ok(Statement::Use {
            alias: Some(alias),
            path,
            public,
            span,
        })
    }

    fn parse_mod(
        &mut self,
        public: bool,
        span: Span<'ctx>,
    ) -> Result<Statement<'ctx>, ParsingError<'ctx>> {
        self.dismiss();
        let comment = self.data.current_doc_comment.replace(None);
        let name = self.expect_identifier()?;
        let semicolon_span = self.expect(TokenType::Semicolon)?.span;

        let span = span.combine_with([semicolon_span, name.span()], self.ctx().span_interner);
        Ok(Statement::Mod {
            span,
            name,
            public,
            comment,
        })
    }

    fn parse_global_asm(
        &mut self,
        span: Span<'ctx>,
    ) -> Result<Statement<'ctx>, ParsingError<'ctx>> {
        self.dismiss();

        let delim = self.expect_delim(Delimiter::Parenthesis)?;
        let mut p = self.subparser(delim.into());
        let mut strn = String::new();

        while !p.is_at_end() {
            if !strn.is_empty() {
                p.expect_one_of(&[TokenType::Comma, TokenType::ParenClose])?;
                if p.is_at_end() {
                    break;
                }
            }
            let (s, _) = p.expect_string()?;
            if !strn.is_empty() {
                strn.push('\n');
            }
            strn.push_str(*s);
        }

        Ok(Statement::ModuleAsm(
            span.combine_with(
                [delim.open_span, delim.close_span],
                self.ctx().span_interner,
            ),
            strn,
        ))
    }

    fn parse_block(&mut self) -> Result<Statement<'ctx>, ParsingError<'ctx>> {
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
            delim.full_span(p.ctx().span_interner),
            annotations,
        ))
    }

    fn parse_return(&mut self, span: Span<'ctx>) -> Result<Statement<'ctx>, ParsingError<'ctx>> {
        // return;
        // return <expr>;
        self.dismiss();
        if let Some(t) = self.match_tok(TokenType::Semicolon) {
            return Ok(Statement::Return(
                None,
                span.combine_with([t.span], self.ctx().span_interner),
            ));
        }
        let expr = self.parse_expression()?;
        let s = self.expect(TokenType::Semicolon)?.span;
        Ok(Statement::Return(
            Some(expr),
            span.combine_with([s], self.ctx().span_interner),
        ))
    }
}

impl<'ctx> Parser<'_, 'ctx> {
    pub fn parse_external(
        &mut self,
        public: bool,
        span: Span<'ctx>,
    ) -> Result<Statement<'ctx>, ParsingError<'ctx>> {
        // dismiss extern
        self.dismiss();
        self.parse_any_callable(false, false, false, public, span)
            .and_then(|(callable, body)| {
                callable
                    .contract
                    .annotations
                    .are_annotations_valid_for(AnnotationReceiver::ExternalFunction)?;
                Ok(Statement::ExternalFunction {
                    contract: callable.contract,
                    body: body.map(Box::new),
                    span: callable.span,
                    public: callable.public,
                })
            })
    }

    pub(super) fn parse_callable(
        &mut self,
        anonymous: bool,
        public: bool,
        span: Span<'ctx>,
    ) -> Result<(Callable<'ctx>, Statement<'ctx>), ParsingError<'ctx>> {
        self.parse_any_callable(anonymous, true, true, public, span)
            .map(|(callable, body)| (callable, body.expect("there should always exist a body")))
    }

    fn parse_any_callable(
        &mut self,
        anonymous: bool,
        needs_body: bool,
        can_have_generics: bool,
        public: bool,
        span: Span<'ctx>,
    ) -> Result<(Callable<'ctx>, Option<Statement<'ctx>>), ParsingError<'ctx>> {
        let annotations = std::mem::take(&mut *self.data.current_annotations.borrow_mut());
        let comment = self.take_doc_comment();

        self.expect(TokenType::Fn)?;

        let name = if anonymous {
            self.match_tok(TokenType::IdentifierLiteral)
                .map(|v| Ident::new(v.string_literal(), v.span))
        } else {
            Some(self.expect_identifier()?)
        };

        let generics = if can_have_generics && self.match_tok_dismiss(TokenType::LessThan) {
            self.parse_function_generics()?
        } else {
            vec![]
        };

        let arg_delim = self.expect_delim(Delimiter::Parenthesis)?;
        let mut arguments = vec![];
        let mut argp = self.subparser(arg_delim.into());

        while !argp.is_at_end() {
            if !arguments.is_empty() {
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

            arguments.push(Argument::new(TypeRef::parse(&mut argp)?, name));
        }

        let return_type = if self.match_tok_dismiss(TokenType::ReturnType) {
            TypeRef::parse(self)?
        } else {
            TypeRef::Void(self.peek_span(), 0)
        };

        let mut end_span = self.peek_span();
        let body = if !needs_body && self.match_tok_dismiss(TokenType::Semicolon) {
            None
        } else {
            Some(match self.peek_tok().map(|v| v.ty) {
                Some(TokenType::CurlyOpen) => self.parse_block()?,
                Some(TokenType::Equal) => {
                    self.dismiss();
                    let expr = self.parse_expression()?;
                    let return_span = expr.span();
                    if !anonymous {
                        // needs a semicolon if this isnt anonymous
                        self.expect(TokenType::Semicolon)?;
                    }
                    Statement::Return(Some(expr), return_span)
                }
                _ => {
                    return Err(ParsingError::ExpectedFunctionBody {
                        span: self.peek_span(),
                        found: self.peek_tok(),
                    });
                }
            })
        };
        if let Some(body) = &body {
            end_span = body.span();
        }

        let span = span.combine_with([end_span], self.ctx().span_interner);
        let contract = FunctionContract {
            name,
            arguments,
            return_type,
            span,
            annotations,
            generics,
            comment,
        };
        Ok((
            Callable {
                contract,
                span,
                public,
            },
            body,
        ))
    }

    pub fn parse_function_generics(&mut self) -> Result<Vec<Generic<'ctx>>, ParsingError<'ctx>> {
        let mut generics = vec![];

        while !self.match_tok_dismiss(TokenType::GreaterThan) {
            if !generics.is_empty() {
                self.expect(TokenType::Comma)?;

                // trailing comma
                if self.match_tok_dismiss(TokenType::GreaterThan) {
                    break;
                }
            }

            generics.push(Generic::parse(self)?);
        }

        Ok(generics)
    }
}
