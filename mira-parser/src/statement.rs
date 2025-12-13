use std::{
    collections::HashMap,
    fmt::{Debug, Display, Write},
};

use crate::{
    LiteralValue,
    annotations::{AnnotationReceiver, Annotations},
    error::ParsingError,
    module::{
        ExternalFunctionId, FunctionId, Module, ModuleContext, ModuleId, StaticId, StructId,
        TraitId,
    },
};
use mira_context::DocComment;
use mira_lexer::{Token, TokenType};
use mira_spans::interner::symbols;

use super::{
    Expression, Parser, PathWithoutGenerics,
    types::{Generic, TypeRef},
};
use mira_spans::{Ident, Span, SpanData};

#[derive(Clone, Debug)]
pub enum BakableFunction<'arena> {
    Function(Box<(FunctionContract<'arena>, Statement<'arena>)>),
    BakedFunction(FunctionId),
}

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

impl Display for Trait<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.annotations, f)?;

        f.write_str("trait ")?;
        Display::fmt(&self.name, f)?;
        f.write_str("{\n")?;

        for func in self.functions.iter() {
            Display::fmt(&func.annotations, f)?;
            f.write_str("    fn ")?;
            Display::fmt(&func.name, f)?;
            f.write_char('(')?;
            for (i, v) in func.args.iter().enumerate() {
                if i != 0 {
                    f.write_str(", ")?;
                }
                Display::fmt(v, f)?;
            }
            f.write_char(')')?;
            if !matches!(func.return_ty, TypeRef::Void(_, 0)) {
                f.write_str(" -> ")?;
                Display::fmt(&func.return_ty, f)?;
            }
            f.write_str(";\n")?;
        }

        f.write_char('}')
    }
}

#[derive(Clone, Debug)]
pub struct Variable<'ctx> {
    pub name: Ident<'ctx>,
    pub value: Expression<'ctx>,
    pub ty: Option<TypeRef<'ctx>>,
    pub span: Span<'ctx>,
    pub annotations: Annotations<'ctx>,
}

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

impl Display for Statement<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::None => unreachable!(),
            Self::BakedFunction(id, _) => {
                f.write_fmt(format_args!("(module-fn {})", id.to_usize()))
            }
            Self::BakedExternalFunction(id, _) => {
                f.write_fmt(format_args!("(module-external-fn {})", id.to_usize()))
            }
            Self::BakedStruct(id, _) => {
                f.write_fmt(format_args!("(module-struct {})", id.to_usize()))
            }
            Self::BakedStatic(id, _) => {
                f.write_fmt(format_args!("(module-static {})", id.to_usize()))
            }
            Self::BakedTrait(id, _) => {
                f.write_fmt(format_args!("(module-trait {})", id.to_usize()))
            }

            Self::Trait(r#trait) => Display::fmt(&r#trait, f),
            Self::Var(Variable {
                name,
                value,
                ty: None,
                ..
            }) => f.write_fmt(format_args!("(var-assign {name} {value})")),
            Self::Var(Variable {
                name,
                value,
                ty: Some(ty),
                ..
            }) => f.write_fmt(format_args!("(var-assign {name} {ty} {value})")),
            Self::Static { var, public, .. } => {
                f.write_char('(')?;
                if *public {
                    f.write_str("pub ")?;
                }
                f.write_fmt(format_args!("static {} ", var.name))?;
                match &var.ty {
                    Some(v) => Display::fmt(v, f)?,
                    None => f.write_str("missing-ty")?,
                }
                f.write_fmt(format_args!(" {})", var.value))
            }
            Self::Block(stmts, _, annotations) => {
                Display::fmt(annotations, f)?;

                f.write_char('{')?;
                for stmt in &**stmts {
                    f.write_char('\n')?;
                    Display::fmt(stmt, f)?;
                }
                if !stmts.is_empty() {
                    // {} for empty block, for filled block:
                    // {
                    // statement1
                    // statement2
                    // }
                    f.write_char('\n')?;
                }
                f.write_char('}')
            }
            Self::DeferredBlock(stmts, _, annotations) => {
                Display::fmt(annotations, f)?;

                f.write_str("defer {")?;
                for stmt in &**stmts {
                    f.write_char('\n')?;
                    Display::fmt(stmt, f)?;
                }
                if !stmts.is_empty() {
                    // {} for empty block, for filled block:
                    // {
                    // statement1
                    // statement2
                    // }
                    f.write_char('\n')?;
                }
                f.write_char('}')
            }
            Self::Expr(v) => Display::fmt(v, f),
            Self::DeferredExpr(v) => f.write_fmt(format_args!("(defer {v})")),
            Self::Return(Some(v), _) => f.write_fmt(format_args!("(return {v})")),
            Self::Return(None, _) => f.write_str("(return null)"),
            Self::If(If {
                condition,
                if_stmt,
                else_stmt: Some(else_stmt),
                span: _,
                annotations,
            }) => f.write_fmt(format_args!(
                "{annotations}(if {condition} {if_stmt} {else_stmt})"
            )),
            Self::If(If {
                condition,
                if_stmt,
                else_stmt: None,
                span: _,
                annotations,
            }) => f.write_fmt(format_args!("{annotations}(if {condition} {if_stmt})")),
            Self::DeferredIf(If {
                condition,
                if_stmt,
                else_stmt: Some(else_stmt),
                span: _,
                annotations,
            }) => f.write_fmt(format_args!(
                "{annotations}(deferred_if {condition} {if_stmt} {else_stmt})"
            )),
            Self::DeferredIf(If {
                condition,
                if_stmt,
                else_stmt: None,
                span: _,
                annotations,
            }) => f.write_fmt(format_args!(
                "{annotations}(deferred_if {condition} {if_stmt})"
            )),
            Self::For(For {
                iterator,
                var_name,
                child,
                span: _,
                annotations,
            }) => f.write_fmt(format_args!(
                "{annotations}(for {var_name} {iterator} {child})"
            )),
            Self::DeferredFor(For {
                iterator,
                var_name,
                child,
                span: _,
                annotations,
            }) => f.write_fmt(format_args!(
                "{annotations}(deferred_for {var_name} {iterator} {child})"
            )),
            Self::While(While {
                condition,
                child,
                span: _,
                annotations,
            }) => f.write_fmt(format_args!("{annotations}(while {condition} {child})")),
            Self::DeferredWhile(While {
                condition,
                child,
                span: _,
                annotations,
            }) => f.write_fmt(format_args!(
                "{annotations}(deferred_while {condition} {child})"
            )),
            Self::Struct {
                name,
                elements,
                span: _,
                generics,
                global_impl,
                impls,
                ..
            } => {
                f.write_str("struct ")?;
                Display::fmt(name, f)?;
                if !generics.is_empty() {
                    f.write_char('<')?;
                    for (i, generic) in generics.iter().enumerate() {
                        if i != 0 {
                            f.write_str(", ")?;
                        }
                        Display::fmt(&generic.name, f)?;
                        if !generic.bounds.is_empty() {
                            for i in 0..generic.bounds.len() {
                                if i != 0 {
                                    f.write_str(" + ")?;
                                }
                                Display::fmt(&generic.bounds[i].0, f)?;
                            }
                        }
                    }
                    f.write_char('>')?;
                }
                f.write_str(" {\n")?;

                for arg_name in elements {
                    f.write_str("    ")?;
                    Display::fmt(&arg_name.0, f)?;
                    f.write_str(": ")?;
                    Display::fmt(&arg_name.1, f)?;
                    f.write_str(",\n")?;
                }

                if !global_impl.is_empty() || !impls.is_empty() {
                    f.write_str("\n")?;
                    for (contract, body) in global_impl.values() {
                        f.write_str("    ")?;
                        display_contract(f, contract, false)?;
                        f.write_char(' ')?;
                        Display::fmt(body, f)?;
                        f.write_str(")\n")?;
                    }

                    for (trait_name, impls, _) in impls {
                        f.write_str("    impl ")?;
                        f.write_str(trait_name)?;
                        f.write_str(" {\n")?;
                        for (contract, body) in impls.values() {
                            f.write_str("        ")?;
                            display_contract(f, contract, false)?;
                            f.write_char(' ')?;
                            Display::fmt(body, f)?;
                            f.write_str(")\n")?;
                        }
                        f.write_str("    }\n")?;
                    }
                }

                f.write_str("}")
            }
            Self::Function {
                contract,
                body,
                public,
                ..
            } => {
                f.write_str(public.then_some("pub ").unwrap_or_default())?;
                display_contract(f, contract, false)?;
                f.write_char(' ')?;
                Display::fmt(body, f)?;
                f.write_char(')')
            }
            Self::ExternalFunction {
                contract,
                body: None,
                public,
                ..
            } => {
                f.write_str(public.then_some("pub ").unwrap_or_default())?;
                display_contract(f, contract, true)?;
                f.write_char(')')
            }
            Self::ExternalFunction {
                contract,
                body: Some(body),
                public,
                ..
            } => {
                f.write_str(public.then_some("pub ").unwrap_or_default())?;
                display_contract(f, contract, true)?;
                f.write_char(' ')?;
                Display::fmt(body, f)?;
                f.write_char(')')
            }
            Self::ModuleAsm(_, asm) => {
                f.write_str("asm (")?;
                for asm in asm.split('\n') {
                    Debug::fmt(asm, f)?;
                    f.write_char('\n')?;
                }
                f.write_str(");")
            }
            Self::Use {
                path,
                alias,
                public,
                ..
            } => {
                if *public {
                    f.write_str("pub ")?;
                }
                f.write_str("use ")?;
                Display::fmt(path, f)?;
                if let Some(alias) = alias {
                    f.write_str(" as ")?;
                    f.write_str(alias)?;
                }
                f.write_char(';')
            }
            Self::Mod { name, public, .. } => {
                if *public {
                    f.write_str("pub ")?;
                }
                f.write_str("mod ")?;
                f.write_str(name)?;
                f.write_char(';')
            }
        }
    }
}

pub(crate) fn display_contract(
    f: &mut std::fmt::Formatter<'_>,
    contract: &FunctionContract<'_>,
    is_external: bool,
) -> std::fmt::Result {
    Display::fmt(&contract.annotations, f)?;

    if is_external {
        f.write_str("(external callable ")?;
    } else {
        f.write_str("(callable ")?;
    }

    if !contract.generics.is_empty() {
        f.write_char('<')?;
        for i in 0..contract.generics.len() {
            let generic = &contract.generics[i];
            if i != 0 {
                f.write_str(", ")?;
            }
            Display::fmt(&generic.name, f)?;
            if !generic.bounds.is_empty() {
                f.write_str(": ")?;
                for i in 0..generic.bounds.len() {
                    if i != 0 {
                        f.write_str(" + ")?;
                    }
                    Display::fmt(&generic.bounds[i].0, f)?;
                }
            }
        }
        f.write_str("> ")?;
    }

    if let Some(name) = &contract.name {
        Display::fmt(name, f)?;
        f.write_char(' ')?;
    }
    f.write_char('(')?;
    for i in 0..contract.arguments.len() {
        if i != 0 {
            f.write_char(' ')?;
        }
        Display::fmt(&contract.arguments[i], f)?;
    }
    f.write_char(')')?;
    if let TypeRef::Void(_, 0) = &contract.return_type {
    } else {
        f.write_str(" returns ")?;
        Display::fmt(&contract.return_type, f)?;
    }
    Ok(())
}

impl<'ctx> Parser<'_, 'ctx> {
    fn consume_semicolon(&mut self) -> Result<(), ParsingError<'ctx>> {
        self.expect(TokenType::Semicolon)?;
        while self.match_tok_dismiss(TokenType::Semicolon) {}
        Ok(())
    }

    pub fn parse_all(&mut self) -> (Vec<Statement<'ctx>>, Vec<ParsingError<'ctx>>) {
        let mut statements = vec![];
        let mut errors = vec![];

        while !self.is_at_end() {
            match self.parse_statement_part(true) {
                Err(error) => {
                    errors.push(error);
                    self.bail();
                }
                Ok(Some(statement)) => statements.push(statement),
                Ok(None) => {}
            }
        }

        if !self.current_annotations.is_empty() {
            errors.push(ParsingError::ExpectedStatement(self.peek().span));
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
        Err(ParsingError::ExpectedStatement(self.peek().span))
    }

    pub fn parse_statement_part(
        &mut self,
        is_global: bool,
    ) -> Result<Option<Statement<'ctx>>, ParsingError<'ctx>> {
        macro_rules! invalid_kw {
            ($kw: literal) => {
                return Err(ParsingError::InvalidKeyword {
                    keyword: $kw,
                    span: self.peek().span,
                })
            };
        }

        if !self.current_annotations.is_empty()
            && !matches!(
                self.peek().ty,
                TokenType::AnnotationIntroducer
                    | TokenType::Extern
                    | TokenType::Fn
                    | TokenType::CurlyLeft
                    | TokenType::Struct
                    | TokenType::For
                    | TokenType::While
                    | TokenType::Let
                    | TokenType::Trait
                    | TokenType::Pub
                    | TokenType::If
            )
        {
            self.bail();
            return Err(ParsingError::ExpectedAnnotationStatement(self.peek().span));
        }

        if let Some(doc_comment) = self.eat_doc_comment() {
            match self.current_doc_comment {
                Some(v) => {
                    self.ctx.merge_doc_comments(v, doc_comment);
                    self.ctx.clear_doc_comment(doc_comment);
                }
                None => self.current_doc_comment = Some(doc_comment),
            }
        }
        let consumes_doc_comment = self.peek().ty != TokenType::AnnotationIntroducer;

        let maybe_statement = match self.peek().ty {
            TokenType::Extern if !is_global => invalid_kw!("external value/function"),
            TokenType::Struct if !is_global => invalid_kw!("struct definition"),
            TokenType::Use if !is_global => invalid_kw!("use"),
            TokenType::Mod if !is_global => invalid_kw!("mod"),
            TokenType::Trait if !is_global => invalid_kw!("trait"),
            TokenType::Pub if !is_global => invalid_kw!("pub"),

            TokenType::Return if is_global => invalid_kw!("return"),
            TokenType::CurlyLeft if is_global => invalid_kw!("code block"),
            TokenType::If if is_global => invalid_kw!("if statement"),
            TokenType::While if is_global => invalid_kw!("while loop"),
            TokenType::For if is_global => invalid_kw!("for loop"),
            TokenType::Defer if is_global => invalid_kw!("defer"),

            TokenType::Asm if is_global => self.parse_global_asm().map(Some),
            TokenType::Trait => self.parse_trait(false).map(Some),
            TokenType::Let => self.parse_let(is_global, false).map(Some),
            TokenType::CurlyLeft => self.parse_block().map(Some),
            TokenType::Return => self.parse_return().map(Some),
            TokenType::Defer => self.parse_defer().map(Some),
            TokenType::If => self.parse_if().map(Statement::If).map(Some),
            TokenType::While => self.parse_while().map(Statement::While).map(Some),
            TokenType::For => self.parse_for().map(Statement::For).map(Some),
            TokenType::Struct => self.parse_struct(false).map(Some),
            TokenType::Fn if !is_global => {
                let (Callable { contract, span, .. }, body) = self.parse_callable(false, false)?;
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
                .parse_callable(false, false)
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
                self.parse_annotation()?;
                Ok(None)
            }
            TokenType::Extern => self.parse_external(false).map(Some),
            TokenType::Eof => {
                return Err(ParsingError::ExpectedStatement(self.peek().span));
            }
            TokenType::Use => self.parse_use(false).map(Some),
            TokenType::Mod => self.parse_mod(false).map(Some),
            TokenType::Pub => self.parse_pub().map(Some),
            _ if is_global => {
                return Err(ParsingError::ExpressionAtTopLevel(self.peek().span));
            }
            _ => self.parse_expression_stmt().map(Some),
        };

        if consumes_doc_comment {
            self.current_doc_comment = None;
        }

        let maybe_statement = maybe_statement?;
        if maybe_statement.is_some() {
            assert_eq!(
                self.current_annotations.len(),
                0,
                "more than 0 annotations left after parsing a statement"
            );
        }

        Ok(maybe_statement)
    }

    fn parse_pub(&mut self) -> Result<Statement<'ctx>, ParsingError<'ctx>> {
        self.dismiss();
        match self.peek().ty {
            TokenType::Fn => self
                .parse_callable(false, true)
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
                }),
            TokenType::Let => self.parse_let(true, true),
            TokenType::Struct => self.parse_struct(true),
            TokenType::Extern => self.parse_external(true),
            TokenType::Trait => self.parse_trait(true),
            TokenType::Use => self.parse_use(true),
            TokenType::Mod => self.parse_mod(true),
            _ => Err(ParsingError::ExpectedElementForPub {
                span: self.peek().span,
                tok: self.peek(),
            }),
        }
    }

    pub fn join_spans(&self, left: Span<'ctx>, right: Span<'ctx>) -> Span<'ctx> {
        left.combine_with([right], self.ctx.span_interner)
    }

    pub fn span_from(&self, previous: Span<'ctx>) -> Span<'ctx> {
        self.join_spans(self.current().span, previous)
    }

    fn parse_global_asm(&mut self) -> Result<Statement<'ctx>, ParsingError<'ctx>> {
        let span = self.eat().span;
        self.expect(TokenType::ParenLeft)?;
        let mut strn = String::new();
        while !self.match_tok_dismiss(TokenType::ParenRight) {
            if !strn.is_empty() {
                self.expect_one_of(&[TokenType::Comma, TokenType::ParenRight])?;
                if self.match_tok_dismiss(TokenType::ParenRight) {
                    break;
                }
            }
            let (s, _) = self.expect_string()?;
            if !strn.is_empty() {
                strn.push('\n');
            }
            strn.push_str(*s);
        }
        Ok(Statement::ModuleAsm(self.span_from(span), strn))
    }

    fn parse_mod(&mut self, public: bool) -> Result<Statement<'ctx>, ParsingError<'ctx>> {
        let comment = self.current_doc_comment.take();
        let span = self.eat().span;
        let name = self.expect_identifier()?;
        let semicolon_span = self.expect(TokenType::Semicolon)?.span;

        let span = span.combine_with([semicolon_span, name.span()], self.ctx.span_interner);
        Ok(Statement::Mod {
            span,
            name,
            public,
            comment,
        })
    }

    fn parse_use(&mut self, public: bool) -> Result<Statement<'ctx>, ParsingError<'ctx>> {
        let span = self.eat().span;
        let path = PathWithoutGenerics::parse(self)?;
        if self.match_tok_dismiss(TokenType::Semicolon) {
            let span = self
                .current()
                .span
                .combine_with([span], self.ctx.span_interner);
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
            .combine_with([span], self.ctx.span_interner);
        Ok(Statement::Use {
            alias: Some(alias),
            path,
            public,
            span,
        })
    }

    fn parse_trait_fn(
        &mut self,
        comment: DocComment,
    ) -> Result<TraitFunction<'ctx>, ParsingError<'ctx>> {
        let span = self.current().span;
        let name = self.expect_identifier()?;
        let annotations = std::mem::take(&mut self.current_annotations);
        let mut args = vec![];

        self.expect(TokenType::ParenLeft)?;

        while !self.match_tok_dismiss(TokenType::ParenRight) {
            if !args.is_empty() {
                if !self.match_tok_dismiss(TokenType::Comma) {
                    return Err(ParsingError::ExpectedFunctionArgument {
                        span: self.peek().span,
                        found: self.peek(),
                    });
                }

                // for trailing comma
                if self.match_tok_dismiss(TokenType::ParenRight) {
                    break;
                }
            }

            let name = self.expect_identifier()?;
            self.expect(TokenType::Colon)?;

            args.push(Argument::new(TypeRef::parse(self)?, name))
        }

        let return_ty = if self.match_tok_dismiss(TokenType::ReturnType) {
            TypeRef::parse(self)?
        } else {
            TypeRef::Void(self.peek().span, 0)
        };

        self.expect(TokenType::Semicolon)?;

        Ok(TraitFunction {
            name,
            args,
            return_ty,
            annotations,
            span,
            comment,
        })
    }

    fn parse_trait(&mut self, public: bool) -> Result<Statement<'ctx>, ParsingError<'ctx>> {
        let span = if public {
            self.current().span
        } else {
            self.peek().span
        };
        self.expect(TokenType::Trait)?;
        let comment = self.take_doc_comment();
        let name = self.expect_identifier()?;

        self.expect(TokenType::CurlyLeft)?;

        let annotations = std::mem::take(&mut self.current_annotations);
        annotations.are_annotations_valid_for(AnnotationReceiver::Trait)?;
        let mut functions = Vec::new();

        let mut func_comment = None;
        while !self.match_tok_dismiss(TokenType::CurlyRight) {
            if let Some(comment) = self.eat_doc_comment() {
                match func_comment {
                    Some(v) => {
                        self.ctx.merge_doc_comments(v, comment);
                        self.ctx.clear_doc_comment(comment);
                    }
                    None => func_comment = Some(comment),
                }
            }
            if self.match_tok_dismiss(TokenType::AnnotationIntroducer) {
                self.parse_annotation()?;
                continue;
            }

            self.expect(TokenType::Fn)?;
            let func = self.parse_trait_fn(func_comment.take().unwrap_or(DocComment::EMPTY))?;
            functions.push(func);
        }

        Ok(Statement::Trait(Trait {
            name,
            functions,
            span: self.span_from(span),
            annotations,
            module: self.key,
            public,
            comment,
        }))
    }

    fn parse_expression_stmt(&mut self) -> Result<Statement<'ctx>, ParsingError<'ctx>> {
        let expr = self.parse_expression()?;
        self.consume_semicolon()?;
        Ok(Statement::Expr(expr))
    }

    fn parse_let(
        &mut self,
        is_static: bool,
        public: bool,
    ) -> Result<Statement<'ctx>, ParsingError<'ctx>> {
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
        let span = if public {
            self.current().span
        } else {
            self.peek().span
        };
        self.expect(TokenType::Let)?;

        let annotations = std::mem::take(&mut self.current_annotations);
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
        self.consume_semicolon()?;
        let var = Variable {
            name,
            value,
            ty,
            span: self.span_from(span),
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
    fn parse_block(&mut self) -> Result<Statement<'ctx>, ParsingError<'ctx>> {
        let annotations = std::mem::take(&mut self.current_annotations);
        annotations.are_annotations_valid_for(AnnotationReceiver::Block)?;

        // { <...statements...> }
        let span = self.eat().span; // skip `{`
        let mut statements = vec![];

        while !self.match_tok_dismiss(TokenType::CurlyRight) {
            statements.push(self.parse_statement(false)?);
        }

        Ok(Statement::Block(
            statements.into_boxed_slice(),
            self.span_from(span),
            annotations,
        ))
    }
    fn parse_return(&mut self) -> Result<Statement<'ctx>, ParsingError<'ctx>> {
        // return;
        // return <expr>;
        let span = self.eat().span; // skip `return`
        if self.match_tok_dismiss(TokenType::Semicolon) {
            return Ok(Statement::Return(None, self.span_from(span)));
        }
        let expr = self.parse_expression()?;
        self.consume_semicolon()?;
        Ok(Statement::Return(Some(expr), self.span_from(span)))
    }
    fn parse_defer(&mut self) -> Result<Statement<'ctx>, ParsingError<'ctx>> {
        // skip `defer`
        self.dismiss();

        let res = match self.peek().ty {
            TokenType::For => self.parse_if().map(Statement::DeferredIf),
            TokenType::While => self.parse_while().map(Statement::DeferredWhile),
            TokenType::If => self.parse_for().map(Statement::DeferredFor),
            TokenType::CurlyLeft => {
                let annotations = std::mem::take(&mut self.current_annotations);
                annotations.are_annotations_valid_for(AnnotationReceiver::Block)?;

                // { <...statements...> }
                let span = self.eat().span; // skip `{`
                let mut statements = vec![];

                while !self.match_tok_dismiss(TokenType::CurlyRight) {
                    statements.push(self.parse_statement(false)?);
                }

                Ok(Statement::Block(
                    statements.into_boxed_slice(),
                    self.span_from(span),
                    annotations,
                ))
            }
            _ => {
                let expr = self.parse_expression()?;
                self.consume_semicolon()?;
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
    fn parse_if(&mut self) -> Result<If<'ctx>, ParsingError<'ctx>> {
        let annotations = std::mem::take(&mut self.current_annotations);
        annotations.are_annotations_valid_for(AnnotationReceiver::If)?;

        // if (<expr>) <stmt>
        // if (<expr>) <stmt> else <stmt>
        // we have else if support, because else <stmt>, the stmt can be another if expression!
        let span = self.eat().span; // skip `if`
        self.expect(TokenType::ParenLeft)?;

        let condition = self.parse_expression()?;

        self.expect(TokenType::ParenRight)?;
        let if_stmt = self.parse_statement(false)?;
        if self.match_tok_dismiss(TokenType::Else) {
            return Ok(If {
                condition,
                if_stmt: Box::new(if_stmt),
                else_stmt: Some(Box::new(self.parse_statement(false)?)),
                span: self.span_from(span),
                annotations: std::mem::take(&mut self.current_annotations),
            });
        }
        Ok(If {
            condition,
            if_stmt: Box::new(if_stmt),
            else_stmt: None,
            span: self.span_from(span),
            annotations,
        })
    }
    fn parse_while(&mut self) -> Result<While<'ctx>, ParsingError<'ctx>> {
        let annotations = std::mem::take(&mut self.current_annotations);
        annotations.are_annotations_valid_for(AnnotationReceiver::While)?;

        // while (<expr>) <stmt>
        // we have else if support, because else <stmt>, the stmt can be another if expression!
        let span = self.eat().span; // skip `while`

        self.expect(TokenType::ParenLeft)?;
        let condition = self.parse_expression()?;
        self.expect(TokenType::ParenRight)?;

        Ok(While {
            condition,
            child: Box::new(self.parse_statement(false)?),
            span: self.span_from(span),
            annotations,
        })
    }
    fn parse_for(&mut self) -> Result<For<'ctx>, ParsingError<'ctx>> {
        let annotations = std::mem::take(&mut self.current_annotations);
        annotations.are_annotations_valid_for(AnnotationReceiver::For)?;

        // for (<identifier> in <expr>) <stmt>
        let span = self.eat().span; // skip over `for`

        self.expect(TokenType::ParenLeft)?;

        let var_name = self.expect_identifier()?;
        self.expect(TokenType::In)?;

        let iterator = self.parse_expression()?;

        self.expect(TokenType::ParenRight)?;

        let child = Box::new(self.parse_statement(false)?);
        Ok(For {
            iterator,
            var_name,
            child,
            span: self.span_from(span),
            annotations,
        })
    }
    fn parse_struct(&mut self, public: bool) -> Result<Statement<'ctx>, ParsingError<'ctx>> {
        let annotations = std::mem::take(&mut self.current_annotations);
        let comment = self.take_doc_comment();
        annotations.are_annotations_valid_for(AnnotationReceiver::Struct)?;

        // struct Name { ... fields ...; implementation area }
        // fields: field: type,[...]
        // implementation area: fn implementation area | impl TraitName { implementation area no trait } implementation area | ""
        // implementation area no trait: fn implementation area no trait | ""
        let span = if public {
            self.current().span
        } else {
            self.peek().span
        };
        self.expect(TokenType::Struct)?;
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

        let mut elements = vec![];

        self.expect(TokenType::CurlyLeft)?;

        while !self.matches_dismiss(&[TokenType::CurlyRight, TokenType::Semicolon]) {
            let comment = self.eat_doc_comment().unwrap_or(DocComment::EMPTY);
            if !elements.is_empty() {
                // needs comma
                self.expect_one_of(&[
                    TokenType::Comma,
                    TokenType::CurlyRight,
                    TokenType::Semicolon,
                ])?;
                // for trailing commas
                if self.match_tok_dismiss(TokenType::CurlyRight) {
                    break;
                }
            }
            let name = self.expect_identifier()?;
            self.expect(TokenType::Colon)?;
            let ty = TypeRef::parse(self)?;
            elements.push((name, ty, comment));
        }

        let mut global_impl =
            HashMap::<Ident<'ctx>, (FunctionContract<'ctx>, Statement<'ctx>)>::new();
        let mut impls = Vec::new();

        if self.current().ty == TokenType::Semicolon {
            // implementation area. has a list of functions
            // or has impl <TraitName> { <list of functions for the trait> }

            while !self.match_tok_dismiss(TokenType::CurlyRight) {
                match self.peek().ty {
                    TokenType::Fn => {
                        if let Some(doc_comment) = self.eat_doc_comment() {
                            // overriding here is fine because we don't parse annotations,
                            // meaning there can only ever be one doc comment.
                            self.current_doc_comment = Some(doc_comment);
                        }
                        let func = self.parse_callable(false, false);
                        self.current_doc_comment = None;
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
                    TokenType::Impl => {
                        let span = self.eat().span;
                        let trait_name = self.expect_identifier()?;
                        let mut current_impl =
                            HashMap::<Ident<'ctx>, (FunctionContract<'ctx>, Statement<'ctx>)>::new(
                            );

                        self.expect(TokenType::CurlyLeft)?;
                        while !self.match_tok_dismiss(TokenType::CurlyRight) {
                            if let Some(doc_comment) = self.eat_doc_comment() {
                                // overriding here is fine because we don't parse annotations,
                                // meaning there can only ever be one doc comment.
                                self.current_doc_comment = Some(doc_comment);
                            }

                            if self.peek().ty != TokenType::Fn {
                                self.current_doc_comment = None;
                                return Err(ParsingError::StructImplRegionExpect {
                                    span: self.peek().span,
                                    found: self.peek().ty,
                                    is_trait_impl: true,
                                });
                            }
                            let func = self.parse_callable(false, false);
                            self.current_doc_comment = None;
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
                        impls.push((trait_name, current_impl, span));
                    }
                    token => {
                        return Err(ParsingError::StructImplRegionExpect {
                            span: self.peek().span,
                            found: token,
                            is_trait_impl: false,
                        });
                    }
                }
            }
        }

        Ok(Statement::Struct {
            name,
            elements,
            span: self.span_from(span),
            global_impl,
            impls,
            annotations,
            generics,
            public,
            comment,
        })
    }

    pub fn expect_identifier(&mut self) -> Result<Ident<'ctx>, ParsingError<'ctx>> {
        if !self.match_tok_dismiss(TokenType::IdentifierLiteral) {
            return Err(ParsingError::ExpectedIdentifier {
                span: self.peek().span,
                found: self.peek().ty,
            });
        }
        Ok(self.current().into())
    }
    pub fn parse_annotation(&mut self) -> Result<(), ParsingError<'ctx>> {
        let span = self.peek().span;
        assert_eq!(self.eat().ty, TokenType::AnnotationIntroducer);

        let name = match self.peek().ty {
            TokenType::If => symbols::Keywords::If,
            TokenType::While => symbols::Keywords::While,
            TokenType::For => symbols::Keywords::For,
            TokenType::Pub => symbols::Keywords::Pub,
            TokenType::As => symbols::Keywords::As,
            TokenType::Else => symbols::Keywords::Else,
            TokenType::Asm => symbols::Keywords::Asm,
            TokenType::Volatile => symbols::Keywords::Volatile,
            TokenType::Impl => symbols::Keywords::Impl,
            TokenType::Fn => symbols::Keywords::Fn,
            TokenType::In => symbols::Keywords::In,
            TokenType::Unsized => symbols::Keywords::Unsized,
            TokenType::Struct => symbols::Keywords::Struct,
            TokenType::Trait => symbols::Keywords::Trait,
            TokenType::IdentifierLiteral => self.peek().string_literal(),

            _ => {
                return Err(ParsingError::ExpectedIdentifier {
                    span: self.peek().span,
                    found: self.peek().ty,
                });
            }
        };
        self.dismiss();

        self.expect(TokenType::ParenLeft)?;
        let mut deepness = 0;
        let mut args = vec![];

        loop {
            match self.peek().ty {
                TokenType::Eof => {
                    return Err(ParsingError::Expected {
                        span: self.peek().span,
                        expected: TokenType::ParenRight,
                        found: Token::new(
                            TokenType::Eof,
                            None,
                            self.ctx.intern_span(SpanData::new(
                                self.file.end_pos(),
                                1,
                                self.file.id,
                            )),
                        ),
                    });
                }
                TokenType::ParenRight if deepness == 0 => {
                    self.dismiss();
                    break;
                }
                TokenType::ParenRight => deepness -= 1,
                TokenType::ParenLeft => deepness += 1,
                _ => (),
            }
            args.push(self.eat());
        }

        self.current_annotations
            .push_annotation(&name, args, self.span_from(span), self.ctx)
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

impl Display for Argument<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.name, f)?;
        f.write_str(": ")?;
        Display::fmt(&self.ty, f)
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

impl<'arena> Parser<'_, 'arena> {
    pub fn parse_external(
        &mut self,
        public: bool,
    ) -> Result<Statement<'arena>, ParsingError<'arena>> {
        let span = self.eat().span;
        self.parse_any_callable(false, false, false, public)
            .and_then(|(mut callable, body)| {
                callable
                    .contract
                    .annotations
                    .are_annotations_valid_for(AnnotationReceiver::ExternalFunction)?;
                callable.contract.span = span;
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
    ) -> Result<(Callable<'arena>, Statement<'arena>), ParsingError<'arena>> {
        self.parse_any_callable(anonymous, true, true, public)
            .map(|(callable, body)| (callable, body.expect("there should always exist a body")))
    }

    fn parse_any_callable(
        &mut self,
        anonymous: bool,
        needs_body: bool,
        can_have_generics: bool,
        public: bool,
    ) -> Result<(Callable<'arena>, Option<Statement<'arena>>), ParsingError<'arena>> {
        let annotations = std::mem::take(&mut self.current_annotations);
        let comment = self.take_doc_comment();

        let span = if public {
            self.current().span
        } else {
            self.peek().span
        };
        self.expect(TokenType::Fn)?;

        let name = if anonymous {
            self.expect_identifier().ok()
        } else {
            Some(self.expect_identifier()?)
        };

        let generics = if can_have_generics && self.match_tok_dismiss(TokenType::LessThan) {
            self.parse_function_generics()?
        } else {
            vec![]
        };

        let mut arguments = vec![];

        self.expect(TokenType::ParenLeft)?;

        while !self.match_tok_dismiss(TokenType::ParenRight) {
            if !arguments.is_empty() {
                if !self.match_tok_dismiss(TokenType::Comma) {
                    return Err(ParsingError::ExpectedFunctionArgument {
                        span: self.peek().span,
                        found: self.peek(),
                    });
                }

                // for trailing comma
                if self.match_tok_dismiss(TokenType::ParenRight) {
                    break;
                }
            }

            let name = self.expect_identifier()?;
            self.expect(TokenType::Colon)?;

            arguments.push(Argument::new(TypeRef::parse(self)?, name));
        }

        let return_type = if self.match_tok_dismiss(TokenType::ReturnType) {
            TypeRef::parse(self)?
        } else {
            TypeRef::Void(self.peek().span, 0)
        };

        let body = if !needs_body && self.match_tok_dismiss(TokenType::Semicolon) {
            None
        } else {
            Some(match self.peek().ty {
                TokenType::CurlyLeft => self.parse_block()?,
                TokenType::Equal => {
                    self.dismiss();
                    let expr = self.parse_expression()?;
                    let return_span = expr.span();
                    if !anonymous {
                        // needs a semicolon if this isnt anonymous
                        self.consume_semicolon()?;
                    }
                    Statement::Return(Some(expr), return_span)
                }
                _ => {
                    return Err(ParsingError::ExpectedFunctionBody {
                        span: self.peek().span,
                        found: self.peek().ty,
                    });
                }
            })
        };

        let span = self.span_from(span);
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

    pub fn parse_function_generics(
        &mut self,
    ) -> Result<Vec<Generic<'arena>>, ParsingError<'arena>> {
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
