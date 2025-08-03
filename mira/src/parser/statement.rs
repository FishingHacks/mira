use std::{
    collections::HashMap,
    fmt::{Debug, Display, Write},
};

use crate::{
    annotations::{AnnotationReceiver, Annotations},
    error::ParsingError,
    module::{
        BakedStruct, ExternalFunction, Function, Import, Module, ModuleContext, ModuleScopeValue,
        Static,
    },
    parser::ParserQueueEntry,
    store::StoreKey,
    symbols,
    tokenizer::{Token, TokenType},
};

use super::{
    module_resolution,
    types::{Generic, TypeRef},
    Expression, Parser, PathWithoutGenerics,
};
use mira_spans::{Ident, Span, SpanData};

#[derive(Clone, Debug)]
pub enum BakableFunction<'arena> {
    Function(Box<(FunctionContract<'arena>, Statement<'arena>)>),
    BakedFunction(StoreKey<Function<'arena>>),
}

#[derive(Clone, Debug)]
pub struct Trait<'arena> {
    pub name: Ident<'arena>,
    pub functions: Vec<(
        Ident<'arena>,
        Vec<Argument<'arena>>,
        TypeRef<'arena>,
        Annotations<'arena>,
        Span<'arena>,
    )>,
    pub span: Span<'arena>,
    pub annotations: Annotations<'arena>,
    pub module: StoreKey<Module<'arena>>,
}

impl Display for Trait<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.annotations, f)?;

        f.write_str("trait ")?;
        Display::fmt(&self.name, f)?;
        f.write_str("{\n")?;

        for (name, args, return_type, annotations, _) in self.functions.iter() {
            Display::fmt(annotations, f)?;
            f.write_str("    fn ")?;
            Display::fmt(name, f)?;
            f.write_char('(')?;
            for (i, v) in args.iter().enumerate() {
                if i != 0 {
                    f.write_str(", ")?;
                }
                Display::fmt(v, f)?;
            }
            f.write_char(')')?;
            if !matches!(return_type, TypeRef::Void(_, 0)) {
                f.write_str(" -> ")?;
                Display::fmt(return_type, f)?;
            }
            f.write_str(";\n")?;
        }

        f.write_char('}')
    }
}

#[derive(Clone, Debug)]
pub enum Statement<'arena> {
    If {
        condition: Expression<'arena>,
        if_stmt: Box<Statement<'arena>>,
        else_stmt: Option<Box<Statement<'arena>>>,
        span: Span<'arena>,
        annotations: Annotations<'arena>,
    },
    While {
        condition: Expression<'arena>,
        child: Box<Statement<'arena>>,
        span: Span<'arena>,
        annotations: Annotations<'arena>,
    },
    For {
        iterator: Expression<'arena>,
        var_name: Ident<'arena>,
        child: Box<Statement<'arena>>,
        span: Span<'arena>,
        annotations: Annotations<'arena>,
    },
    Return(Option<Expression<'arena>>, Span<'arena>),
    Block(Box<[Statement<'arena>]>, Span<'arena>, Annotations<'arena>),
    Var(
        Ident<'arena>,
        Expression<'arena>,
        Option<TypeRef<'arena>>,
        Span<'arena>,
        Annotations<'arena>,
    ),
    Expression(Expression<'arena>),
    Function(
        FunctionContract<'arena>,
        Box<Statement<'arena>>,
        Span<'arena>,
    ),
    ExternalFunction(
        FunctionContract<'arena>,
        Option<Box<Statement<'arena>>>,
        Span<'arena>,
    ),
    Struct {
        name: Ident<'arena>,
        elements: Vec<(Ident<'arena>, TypeRef<'arena>)>,
        span: Span<'arena>,
        global_impl: HashMap<Ident<'arena>, (FunctionContract<'arena>, Statement<'arena>)>,
        #[allow(clippy::type_complexity)]
        impls: Vec<(
            Ident<'arena>,
            HashMap<Ident<'arena>, (FunctionContract<'arena>, Statement<'arena>)>,
            Span<'arena>,
        )>,
        generics: Vec<Generic<'arena>>,
        annotations: Annotations<'arena>,
    },
    Trait(Trait<'arena>),
    /// key (the name of the thing in the module), export key (the name during import), location
    Export(Ident<'arena>, Ident<'arena>, Span<'arena>),
    ModuleAsm(Span<'arena>, String),

    Use {
        span: Span<'arena>,
        path: PathWithoutGenerics<'arena>,
        alias: Option<Ident<'arena>>,
        public: bool,
    },
    Mod {
        span: Span<'arena>,
        name: Ident<'arena>,
        public: bool,
    },

    BakedFunction(StoreKey<Function<'arena>>, Span<'arena>),
    BakedExternalFunction(StoreKey<ExternalFunction<'arena>>, Span<'arena>),
    BakedStruct(StoreKey<BakedStruct<'arena>>, Span<'arena>),
    BakedStatic(StoreKey<Static<'arena>>, Span<'arena>),
    BakedTrait(StoreKey<Trait<'arena>>, Span<'arena>),
}

impl<'arena> Statement<'arena> {
    pub fn span(&self) -> Span<'arena> {
        match self {
            Self::Expression(expr) => expr.span(),
            Self::ExternalFunction(.., span)
            | Self::Function(.., span)
            | Self::Export(_, _, span)
            | Self::Block(_, span, _)
            | Self::ModuleAsm(span, _)
            | Self::For { span, .. }
            | Self::If { span, .. }
            | Self::Return(_, span)
            | Self::Struct { span, .. }
            | Self::Var(_, _, _, span, _)
            | Self::BakedFunction(_, span)
            | Self::BakedExternalFunction(_, span)
            | Self::BakedStruct(_, span)
            | Self::BakedStatic(_, span)
            | Self::BakedTrait(_, span)
            | Self::Trait(Trait { span, .. })
            | Self::Use { span, .. }
            | Self::Mod { span, .. }
            | Self::While { span, .. } => *span,
        }
    }

    pub fn bake_functions(
        &mut self,
        module: &mut Module<'arena>,
        module_key: StoreKey<Module<'arena>>,
        context: &ModuleContext<'arena>,
    ) {
        match self {
            Self::BakedFunction(..)
            | Self::BakedExternalFunction(..)
            | Self::BakedTrait(..)
            | Self::BakedStatic(..)
            | Self::BakedStruct(..)
            | Self::Return(None, ..)
            | Self::Export(..)
            | Self::ModuleAsm(..)
            | Self::Trait { .. } => (),
            Self::ExternalFunction(..) => {
                unreachable!("external function in a non-top-level scope")
            }
            Self::Use { .. } => unreachable!("use in a non-top-level scope"),
            Self::Mod { .. } => unreachable!("mod in a non-top-level scope"),
            Self::Struct { .. } => unreachable!("struct in a non-top-level scope"),
            Self::Function(..) => unreachable!("function in a non-top-level scope"),
            Self::Block(statements, ..) => statements
                .iter_mut()
                .for_each(|stmt| stmt.bake_functions(module, module_key, context)),
            Self::Var(_, stmt, ..) => stmt.bake_functions(module, module_key, context),
            Self::Expression(expr) => expr.bake_functions(module, module_key, context),
            Self::For {
                iterator, child, ..
            } => {
                iterator.bake_functions(module, module_key, context);
                child.bake_functions(module, module_key, context);
            }
            Self::While {
                condition, child, ..
            } => {
                condition.bake_functions(module, module_key, context);
                child.bake_functions(module, module_key, context);
            }
            Self::If {
                condition,
                if_stmt,
                else_stmt,
                ..
            } => {
                condition.bake_functions(module, module_key, context);
                if_stmt.bake_functions(module, module_key, context);
                if let Some(stmt) = else_stmt {
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
            Self::BakedFunction(id, _) => f.write_fmt(format_args!("(module-fn {id})")),
            Self::BakedExternalFunction(id, _) => {
                f.write_fmt(format_args!("(module-external-fn {id})"))
            }
            Self::BakedStruct(id, _) => f.write_fmt(format_args!("(module-struct {id})")),
            Self::BakedStatic(id, _) => f.write_fmt(format_args!("(module-static {id})")),
            Self::BakedTrait(id, _) => f.write_fmt(format_args!("(module-trait {id})")),

            Self::Trait(r#trait) => Display::fmt(&r#trait, f),
            Self::Var(left_hand, right_hand, None, ..) => {
                f.write_fmt(format_args!("(var-assign {left_hand} {right_hand})"))
            }
            Self::Var(left_hand, right_hand, Some(typ), ..) => {
                f.write_fmt(format_args!("(var-assign {left_hand} {typ} {right_hand})"))
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
            Self::Expression(v) => Display::fmt(v, f),
            Self::Return(Some(v), _) => f.write_fmt(format_args!("(return {v})")),
            Self::Return(None, _) => f.write_str("(return null)"),
            Self::If {
                condition,
                if_stmt,
                else_stmt: Some(else_stmt),
                span: _,
                annotations,
            } => f.write_fmt(format_args!(
                "{annotations}(if {condition} {if_stmt} {else_stmt})"
            )),
            Self::If {
                condition,
                if_stmt,
                else_stmt: None,
                span: _,
                annotations,
            } => f.write_fmt(format_args!("{annotations}(if {condition} {if_stmt})")),
            Self::For {
                iterator,
                var_name,
                child,
                span: _,
                annotations,
            } => f.write_fmt(format_args!(
                "{annotations}(for {var_name} {iterator} {child})"
            )),
            Self::While {
                condition,
                child,
                span: _,
                annotations,
            } => f.write_fmt(format_args!("{annotations}(while {condition} {child})")),
            Self::Struct {
                name,
                elements: arguments,
                span: _,
                generics,
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

                for arg_name in arguments {
                    f.write_str("    ")?;
                    Display::fmt(&arg_name.0, f)?;
                    f.write_str(": ")?;
                    Display::fmt(&arg_name.1, f)?;
                    f.write_str(",\n")?;
                }

                f.write_str("}")
            }
            Self::Function(contract, body, _) => {
                display_contract(f, contract, false)?;
                f.write_char(' ')?;
                Display::fmt(body, f)?;
                f.write_char(')')
            }
            Self::ExternalFunction(contract, None, _) => {
                display_contract(f, contract, true)?;
                f.write_char(')')
            }
            Self::ExternalFunction(contract, Some(body), _) => {
                display_contract(f, contract, true)?;
                f.write_char(' ')?;
                Display::fmt(body, f)?;
                f.write_char(')')
            }
            Self::Export(name, exported_name, _) => {
                f.write_str("(export ")?;
                Display::fmt(name, f)?;
                if name != exported_name {
                    f.write_str(" as ")?;
                    Display::fmt(exported_name, f)?;
                }
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

pub fn display_contract(
    f: &mut std::fmt::Formatter<'_>,
    contract: &FunctionContract,
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

impl<'arena> Parser<'_, 'arena> {
    fn consume_semicolon(&mut self) -> Result<(), ParsingError<'arena>> {
        self.expect(TokenType::Semicolon)?;
        while self.match_tok(TokenType::Semicolon) {}
        Ok(())
    }

    pub fn parse_all(&mut self) -> (Vec<Statement<'arena>>, Vec<ParsingError<'arena>>) {
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
    ) -> Result<Statement<'arena>, ParsingError<'arena>> {
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
    ) -> Result<Option<Statement<'arena>>, ParsingError<'arena>> {
        macro_rules! invalid_kw {
            ($kw: literal) => {
                return Err(ParsingError::InvalidKeyword {
                    keyword: $kw,
                    loc: self.peek().span,
                })
            };
        }

        if !self.current_annotations.is_empty()
            && !matches!(
                self.peek().typ,
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

        let maybe_statement = match self.peek().typ {
            TokenType::Extern if !is_global => invalid_kw!("external value/function"),
            TokenType::Fn if !is_global => invalid_kw!("function"),
            TokenType::Struct if !is_global => invalid_kw!("struct definition"),
            TokenType::Use if !is_global => invalid_kw!("use"),
            TokenType::Mod if !is_global => invalid_kw!("mod"),
            TokenType::Export if !is_global => invalid_kw!("export"),
            TokenType::Trait if !is_global => invalid_kw!("trait"),
            TokenType::Pub if !is_global => invalid_kw!("pub"),

            TokenType::Return if is_global => invalid_kw!("return"),
            TokenType::CurlyLeft if is_global => invalid_kw!("code block"),
            TokenType::If if is_global => invalid_kw!("if statement"),
            TokenType::While if is_global => invalid_kw!("while loop"),
            TokenType::For if is_global => invalid_kw!("for loop"),

            TokenType::Asm if is_global => self.parse_global_asm().map(Some),
            TokenType::Trait => self.parse_trait().map(Some),
            TokenType::Let => self.parse_let_stmt(is_global).map(Some),
            TokenType::CurlyLeft => self.parse_block_stmt().map(Some),
            TokenType::Return => self.parse_return_stmt().map(Some),
            TokenType::If => self.parse_if_stmt().map(Some),
            TokenType::While => self.parse_while_stmt().map(Some),
            TokenType::For => self.parse_for_stmt().map(Some),
            TokenType::Struct => self.parse_struct().map(Some),
            TokenType::Fn => self
                .parse_callable(false)
                .and_then(|(contract, body, span)| {
                    contract
                        .annotations
                        .are_annotations_valid_for(AnnotationReceiver::Function)?;
                    Ok(Statement::Function(contract, Box::new(body), span))
                })
                .map(Some),
            TokenType::AnnotationIntroducer => {
                self.parse_annotation()?;
                Ok(None)
            }
            TokenType::Extern => self.parse_external().map(Some),
            TokenType::Eof => {
                return Err(ParsingError::ExpectedStatement(self.peek().span));
            }
            TokenType::Use => self.parse_use(false).map(Some),
            TokenType::Mod => self.parse_mod(false).map(Some),
            TokenType::Export => self.parse_export().map(Some),
            TokenType::Pub => self.parse_pub().map(Some),
            _ if is_global => {
                return Err(ParsingError::ExpressionAtTopLevel(self.peek().span));
            }
            _ => self.parse_expression_stmt().map(Some),
        }?;
        if maybe_statement.is_some() {
            assert_eq!(
                self.current_annotations.len(),
                0,
                "more than 0 annotations left after parsing a statement"
            );
        }

        Ok(maybe_statement)
    }

    fn parse_pub(&mut self) -> Result<Statement<'arena>, ParsingError<'arena>> {
        self.dismiss();
        let stmt = match self.peek().typ {
            TokenType::Fn => self
                .parse_callable(false)
                .and_then(|(contract, body, span)| {
                    contract
                        .annotations
                        .are_annotations_valid_for(AnnotationReceiver::Function)?;
                    Ok(Statement::Function(contract, Box::new(body), span))
                })?,

            TokenType::Let => self.parse_let_stmt(true)?,
            TokenType::Struct => self.parse_struct()?,
            TokenType::Extern => self.parse_external()?,
            TokenType::Trait => self.parse_trait()?,
            TokenType::Use => return self.parse_use(true),
            TokenType::Mod => return self.parse_mod(true),
            _ => {
                return Err(ParsingError::ExpectedElementForPub {
                    loc: self.peek().span,
                    typ: self.peek().typ,
                })
            }
        };
        let ident = match &stmt {
            Statement::Function(c, ..) | Statement::ExternalFunction(c, ..) => {
                c.name.expect("global functions should always have a name")
            }
            Statement::Trait(Trait { name, .. })
            | Statement::Var(name, ..)
            | Statement::Struct { name, .. } => *name,
            _ => unreachable!(),
        };
        self.add_export(ident)?;

        Ok(stmt)
    }

    pub fn join_spans(&self, left: Span<'arena>, right: Span<'arena>) -> Span<'arena> {
        left.combine_with([right], self.ctx.span_interner())
    }

    pub fn span_from(&self, previous: Span<'arena>) -> Span<'arena> {
        self.join_spans(self.current().span, previous)
    }

    fn parse_global_asm(&mut self) -> Result<Statement<'arena>, ParsingError<'arena>> {
        let span = self.eat().span;
        self.expect(TokenType::ParenLeft)?;
        let mut strn = String::new();
        while !self.match_tok(TokenType::ParenRight) {
            if !strn.is_empty() {
                self.expect_one_of(&[TokenType::Comma, TokenType::ParenRight])?;
                if self.match_tok(TokenType::ParenRight) {
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

    fn parse_export(&mut self) -> Result<Statement<'arena>, ParsingError<'arena>> {
        let span = self.eat().span;
        let name = self.expect_identifier()?;
        let exported_name = if self.match_tok(TokenType::As) {
            self.expect_identifier()?
        } else {
            name
        };
        self.consume_semicolon()?;
        Ok(Statement::Export(name, exported_name, self.span_from(span)))
    }

    fn parse_mod(&mut self, public: bool) -> Result<Statement<'arena>, ParsingError<'arena>> {
        let span = self.eat().span;
        let name = self.expect_identifier()?;
        let semicolon_span = self.expect(TokenType::Semicolon)?.span;

        let file = module_resolution::resolve_module(
            self.ctx.span_interner(),
            name,
            &self.file,
            self.ctx.source_map(),
            span,
            semicolon_span,
        )?;
        let reserved_key = self.modules.write().reserve_key();
        self.parser_queue.write().push(ParserQueueEntry {
            file,
            package: self.file.package,
            reserved_key,
            loaded_file: None,
        });
        if public {
            self.add_export(name)?
        }
        let span = span.combine_with([semicolon_span, name.span()], self.ctx.span_interner());
        self.add_import(
            name,
            span,
            Import::Resolved(ModuleScopeValue::Module(reserved_key)),
        )?;
        Ok(Statement::Mod { span, name, public })
    }

    fn parse_use(&mut self, public: bool) -> Result<Statement<'arena>, ParsingError<'arena>> {
        let span = self.eat().span;
        let path = PathWithoutGenerics::parse(self)?;
        if self.match_tok(TokenType::Semicolon) {
            let span = self
                .current()
                .span
                .combine_with([span], self.ctx.span_interner());
            let alias = path.entries[path.entries.len() - 1];
            if public {
                self.add_export(alias)?;
            }
            self.add_import(alias, span, Import::Unresolved(path.clone()))?;
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
            .combine_with([span], self.ctx.span_interner());
        if public {
            self.add_export(alias)?;
        }
        self.add_import(alias, span, Import::Unresolved(path.clone()))?;
        Ok(Statement::Use {
            alias: Some(alias),
            path,
            public,
            span,
        })
    }

    fn parse_trait_fn(
        &mut self,
    ) -> Result<
        (
            Ident<'arena>,
            Vec<Argument<'arena>>,
            TypeRef<'arena>,
            Annotations<'arena>,
            Span<'arena>,
        ),
        ParsingError<'arena>,
    > {
        let span = self.current().span;
        let name = self.expect_identifier()?;

        let mut arguments = vec![];

        self.expect(TokenType::ParenLeft)?;

        while !self.match_tok(TokenType::ParenRight) {
            if !arguments.is_empty() {
                if !self.match_tok(TokenType::Comma) {
                    return Err(ParsingError::ExpectedFunctionArgument {
                        loc: self.peek().span,
                        found: self.peek(),
                    });
                }

                // for trailing comma
                if self.match_tok(TokenType::ParenRight) {
                    break;
                }
            }

            let name = self.expect_identifier()?;
            self.expect(TokenType::Colon)?;

            arguments.push(Argument::new(TypeRef::parse(self)?, name))
        }

        let return_type = if self.match_tok(TokenType::ReturnType) {
            TypeRef::parse(self)?
        } else {
            TypeRef::Void(self.peek().span, 0)
        };

        self.expect(TokenType::Semicolon)?;

        Ok((
            name,
            arguments,
            return_type,
            std::mem::take(&mut self.current_annotations),
            self.span_from(span),
        ))
    }

    fn parse_trait(&mut self) -> Result<Statement<'arena>, ParsingError<'arena>> {
        let span = self.eat().span; // skip `trait`
        let name = self.expect_identifier()?;

        self.expect(TokenType::CurlyLeft)?;

        let annotations = std::mem::take(&mut self.current_annotations);
        annotations.are_annotations_valid_for(crate::annotations::AnnotationReceiver::Trait)?;
        let mut functions = Vec::new();

        while !self.match_tok(TokenType::CurlyRight) {
            if self.match_tok(TokenType::AnnotationIntroducer) {
                self.parse_annotation()?;
                continue;
            }

            self.expect(TokenType::Fn)?;
            let func = self.parse_trait_fn()?;
            functions.push(func);
        }

        Ok(Statement::Trait(Trait {
            name,
            functions,
            span: self.span_from(span),
            annotations,
            module: self.key,
        }))
    }

    fn parse_expression_stmt(&mut self) -> Result<Statement<'arena>, ParsingError<'arena>> {
        let expr = self.parse_expression()?;
        self.consume_semicolon()?;
        Ok(Statement::Expression(expr))
    }

    fn parse_let_stmt(
        &mut self,
        is_static: bool,
    ) -> Result<Statement<'arena>, ParsingError<'arena>> {
        // let <identifier>;
        // let <identifier> = <expr>;
        let span = self.eat().span; // skip `let`

        let annotations = std::mem::take(&mut self.current_annotations);
        annotations.are_annotations_valid_for(if is_static {
            AnnotationReceiver::Static
        } else {
            AnnotationReceiver::Variable
        })?;

        let name = self.expect_identifier()?;

        let typ = if self.match_tok(TokenType::Colon) {
            Some(TypeRef::parse(self)?)
        } else {
            None
        };

        self.expect(TokenType::Equal)?;

        let expr = self.parse_expression()?;
        self.consume_semicolon()?;
        Ok(Statement::Var(
            name,
            expr,
            typ,
            self.span_from(span),
            annotations,
        ))
    }
    fn parse_block_stmt(&mut self) -> Result<Statement<'arena>, ParsingError<'arena>> {
        let annotations = std::mem::take(&mut self.current_annotations);
        annotations.are_annotations_valid_for(AnnotationReceiver::Block)?;

        // { <...statements...> }
        let span = self.eat().span; // skip `{`
        let mut statements = vec![];

        while !self.match_tok(TokenType::CurlyRight) {
            statements.push(self.parse_statement(false)?);
        }

        Ok(Statement::Block(
            statements.into_boxed_slice(),
            self.span_from(span),
            annotations,
        ))
    }
    fn parse_return_stmt(&mut self) -> Result<Statement<'arena>, ParsingError<'arena>> {
        // return;
        // return <expr>;
        let span = self.eat().span; // skip `return`
        if self.match_tok(TokenType::Semicolon) {
            return Ok(Statement::Return(None, self.span_from(span)));
        }
        let expr = self.parse_expression()?;
        self.consume_semicolon()?;
        Ok(Statement::Return(Some(expr), self.span_from(span)))
    }
    fn parse_if_stmt(&mut self) -> Result<Statement<'arena>, ParsingError<'arena>> {
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
        if self.match_tok(TokenType::Else) {
            return Ok(Statement::If {
                condition,
                if_stmt: Box::new(if_stmt),
                else_stmt: Some(Box::new(self.parse_statement(false)?)),
                span: self.span_from(span),
                annotations: std::mem::take(&mut self.current_annotations),
            });
        }
        Ok(Statement::If {
            condition,
            if_stmt: Box::new(if_stmt),
            else_stmt: None,
            span: self.span_from(span),
            annotations,
        })
    }
    fn parse_while_stmt(&mut self) -> Result<Statement<'arena>, ParsingError<'arena>> {
        let annotations = std::mem::take(&mut self.current_annotations);
        annotations.are_annotations_valid_for(AnnotationReceiver::While)?;

        // while (<expr>) <stmt>
        // we have else if support, because else <stmt>, the stmt can be another if expression!
        let span = self.eat().span; // skip `while`

        self.expect(TokenType::ParenLeft)?;
        let condition = self.parse_expression()?;
        self.expect(TokenType::ParenRight)?;

        Ok(Statement::While {
            condition,
            child: Box::new(self.parse_statement(false)?),
            span: self.span_from(span),
            annotations,
        })
    }
    fn parse_for_stmt(&mut self) -> Result<Statement<'arena>, ParsingError<'arena>> {
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
        Ok(Statement::For {
            iterator,
            var_name,
            child,
            span: self.span_from(span),
            annotations,
        })
    }
    fn parse_struct(&mut self) -> Result<Statement<'arena>, ParsingError<'arena>> {
        let annotations = std::mem::take(&mut self.current_annotations);
        annotations.are_annotations_valid_for(AnnotationReceiver::Struct)?;

        // struct Name { ... fields ...; implementation area }
        // fields: field: type,[...]
        // implementation area: fn implementation area | impl TraitName { implementation area no trait } implementation area | ""
        // implementation area no trait: fn implementation area no trait | ""
        let span = self.eat().span; // skip over `struct`
        let name = self.expect_identifier()?;

        let mut generics = vec![];
        if self.match_tok(TokenType::LessThan) {
            while !self.match_tok(TokenType::GreaterThan) {
                if generics.len() > 1 {
                    self.expect_one_of(&[TokenType::Comma, TokenType::GreaterThan])?;

                    if self.match_tok(TokenType::GreaterThan) {
                        break;
                    }
                }

                generics.push(Generic::parse(self)?);
            }
        }

        let mut elements = vec![];

        self.expect(TokenType::CurlyLeft)?;

        while !self.matches(&[TokenType::CurlyRight, TokenType::Semicolon]) {
            if !elements.is_empty() {
                // needs comma
                self.expect_one_of(&[
                    TokenType::Comma,
                    TokenType::CurlyRight,
                    TokenType::Semicolon,
                ])?;
                // for trailing commas
                if self.match_tok(TokenType::CurlyRight) {
                    break;
                }
            }
            let name = self.expect_identifier()?;
            self.expect(TokenType::Colon)?;
            let typ = TypeRef::parse(self)?;
            elements.push((name, typ));
        }

        let mut global_impl =
            HashMap::<Ident<'arena>, (FunctionContract<'arena>, Statement<'arena>)>::new();
        let mut impls = Vec::new();

        if self.current().typ == TokenType::Semicolon {
            // implementation area. has a list of functions
            // or has impl <TraitName> { <list of functions for the trait> }

            while !self.match_tok(TokenType::CurlyRight) {
                match self.peek().typ {
                    TokenType::Fn => {
                        let func = self.parse_callable(false)?;
                        let name = func
                            .0
                            .name
                            .as_ref()
                            .cloned()
                            .expect("non-anonymous function without name");
                        if let Some(other_func) = global_impl.get(&name) {
                            return Err(ParsingError::FunctionAlreadyDefined {
                                loc: func.0.span,
                                name: name.symbol(),
                                first_func_loc: other_func.0.span,
                            });
                        }
                        global_impl.insert(name, (func.0, func.1));
                    }
                    TokenType::Impl => {
                        let loc = self.eat().span;
                        let trait_name = self.expect_identifier()?;
                        let mut current_impl = HashMap::<
                            Ident<'arena>,
                            (FunctionContract<'arena>, Statement<'arena>),
                        >::new();

                        self.expect(TokenType::CurlyLeft)?;
                        while !self.match_tok(TokenType::CurlyRight) {
                            if self.peek().typ != TokenType::Fn {
                                return Err(ParsingError::StructImplRegionExpect {
                                    loc: self.peek().span,
                                    found: self.peek().typ,
                                    is_trait_impl: true,
                                });
                            }
                            let func = self.parse_callable(false)?;
                            let name = func
                                .0
                                .name
                                .as_ref()
                                .cloned()
                                .expect("non-anonymous function without name");
                            if let Some(other_func) = current_impl.get(&name) {
                                return Err(ParsingError::FunctionAlreadyDefined {
                                    loc: func.0.span,
                                    name: name.symbol(),
                                    first_func_loc: other_func.0.span,
                                });
                            }
                            current_impl.insert(name, (func.0, func.1));
                        }
                        impls.push((trait_name, current_impl, loc));
                    }
                    token => {
                        return Err(ParsingError::StructImplRegionExpect {
                            loc: self.peek().span,
                            found: token,
                            is_trait_impl: false,
                        })
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
        })
    }

    pub fn expect_identifier(&mut self) -> Result<Ident<'arena>, ParsingError<'arena>> {
        if !self.match_tok(TokenType::IdentifierLiteral) {
            return Err(ParsingError::ExpectedIdentifier {
                loc: self.peek().span,
                found: self.peek().typ,
            });
        }
        Ok(self.current().into())
    }
    pub fn parse_annotation(&mut self) -> Result<(), ParsingError<'arena>> {
        let span = self.peek().span;
        assert_eq!(self.eat().typ, TokenType::AnnotationIntroducer);

        let name = match self.peek().typ {
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
            TokenType::IdentifierLiteral => self.peek().string_literal()?,

            _ => {
                return Err(ParsingError::ExpectedIdentifier {
                    loc: self.peek().span,
                    found: self.peek().typ,
                })
            }
        };
        self.dismiss();

        self.expect(TokenType::ParenLeft)?;
        let mut deepness = 0;
        let mut args = vec![];

        loop {
            match self.peek().typ {
                TokenType::Eof => {
                    return Err(ParsingError::Expected {
                        loc: self.peek().span,
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
                    })
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
            .push_annotation(&name, args, self.span_from(span))
    }
}

#[derive(Debug, Clone)]
pub struct Argument<'arena> {
    pub name: Ident<'arena>,
    pub typ: TypeRef<'arena>,
}

impl<'arena> Argument<'arena> {
    pub fn new(typ: TypeRef<'arena>, name: Ident<'arena>) -> Self {
        Self { name, typ }
    }
}

impl Display for Argument<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.name, f)?;
        f.write_str(": ")?;
        Display::fmt(&self.typ, f)
    }
}

#[derive(Clone, Debug)]
pub struct FunctionContract<'arena> {
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

impl<'arena> Parser<'_, 'arena> {
    pub fn parse_external(&mut self) -> Result<Statement<'arena>, ParsingError<'arena>> {
        let location = self.eat().span;
        self.parse_any_callable(false, false, false)
            .and_then(|(mut contract, body, span)| {
                contract
                    .annotations
                    .are_annotations_valid_for(AnnotationReceiver::ExternalFunction)?;
                contract.span = location;
                Ok(Statement::ExternalFunction(
                    contract,
                    body.map(Box::new),
                    span,
                ))
            })
    }

    pub fn parse_callable(
        &mut self,
        anonymous: bool,
    ) -> Result<(FunctionContract<'arena>, Statement<'arena>, Span<'arena>), ParsingError<'arena>>
    {
        self.parse_any_callable(anonymous, true, true)
            .map(|(contract, body, span)| {
                (
                    contract,
                    body.expect("there should always exist a body"),
                    span,
                )
            })
    }

    pub fn parse_any_callable(
        &mut self,
        anonymous: bool,
        needs_body: bool,
        can_have_generics: bool,
    ) -> Result<
        (
            FunctionContract<'arena>,
            Option<Statement<'arena>>,
            Span<'arena>,
        ),
        ParsingError<'arena>,
    > {
        let annotations = std::mem::take(&mut self.current_annotations);

        let span = self.expect(TokenType::Fn)?.span;

        let name = if anonymous {
            self.expect_identifier().ok()
        } else {
            Some(self.expect_identifier()?)
        };

        let generics = if can_have_generics && self.match_tok(TokenType::LessThan) {
            self.parse_function_generics()?
        } else {
            vec![]
        };

        let mut arguments = vec![];

        self.expect(TokenType::ParenLeft)?;

        while !self.match_tok(TokenType::ParenRight) {
            if !arguments.is_empty() {
                if !self.match_tok(TokenType::Comma) {
                    return Err(ParsingError::ExpectedFunctionArgument {
                        loc: self.peek().span,
                        found: self.peek(),
                    });
                }

                // for trailing comma
                if self.match_tok(TokenType::ParenRight) {
                    break;
                }
            }

            let name = self.expect_identifier()?;
            self.expect(TokenType::Colon)?;

            arguments.push(Argument::new(TypeRef::parse(self)?, name));
        }

        let return_type = if self.match_tok(TokenType::ReturnType) {
            TypeRef::parse(self)?
        } else {
            TypeRef::Void(self.peek().span, 0)
        };

        let body = if !needs_body && self.match_tok(TokenType::Semicolon) {
            None
        } else {
            Some(match self.peek().typ {
                TokenType::CurlyLeft => self.parse_block_stmt()?,
                TokenType::Equal => {
                    self.dismiss();
                    let expr = self.parse_expression()?;
                    let return_location = expr.span();
                    if !anonymous {
                        // needs a semicolon if this isnt anonymous
                        self.consume_semicolon()?;
                    }
                    Statement::Return(Some(expr), return_location)
                }
                _ => {
                    return Err(ParsingError::ExpectedFunctionBody {
                        loc: self.peek().span,
                        found: self.peek().typ,
                    })
                }
            })
        };

        let span = self.span_from(span);
        Ok((
            FunctionContract {
                name,
                arguments,
                return_type,
                span,
                annotations,
                generics,
            },
            body,
            span,
        ))
    }

    pub fn parse_function_generics(
        &mut self,
    ) -> Result<Vec<Generic<'arena>>, ParsingError<'arena>> {
        let mut generics = vec![];

        while !self.match_tok(TokenType::GreaterThan) {
            if !generics.is_empty() {
                self.expect(TokenType::Comma)?;

                // trailing comma
                if self.match_tok(TokenType::GreaterThan) {
                    break;
                }
            }

            generics.push(Generic::parse(self)?);
        }

        Ok(generics)
    }
}
