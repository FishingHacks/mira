use std::{
    collections::HashMap,
    fmt::{Debug, Display, Write},
};

use crate::{
    annotations::{AnnotationReceiver, Annotations},
    error::ParsingError,
    globals::GlobalStr,
    module::{BakedStruct, ExternalFunction, Function, Module, ModuleContext, Static},
    parser::{module_resolution::resolve_module, ParserQueueEntry},
    store::StoreKey,
    tokenizer::{Literal, Location, Token, TokenType},
};

use super::{
    module_resolution::ResolvedPath,
    types::{Generic, TypeRef},
    Expression, Parser,
};

#[derive(Clone, Debug)]
pub enum BakableFunction {
    Function(Box<(FunctionContract, Statement)>),
    BakedFunction(StoreKey<Function>),
}

#[derive(Clone, Debug)]
pub struct Trait {
    pub name: GlobalStr,
    pub functions: Vec<(GlobalStr, Vec<Argument>, TypeRef, Annotations, Location)>,
    pub location: Location,
    pub annotations: Annotations,
    pub module: StoreKey<Module>,
}

impl Display for Trait {
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
pub enum Statement {
    If {
        condition: Expression,
        if_stmt: Box<Statement>,
        else_stmt: Option<Box<Statement>>,
        location: Location,
        annotations: Annotations,
    },
    While {
        condition: Expression,
        child: Box<Statement>,
        location: Location,
        annotations: Annotations,
    },
    For {
        iterator: Expression,
        var_name: GlobalStr,
        child: Box<Statement>,
        location: Location,
        annotations: Annotations,
    },
    Return(Option<Expression>, Location),
    Block(Box<[Statement]>, Location, Annotations),
    Var(
        GlobalStr,
        Expression,
        Option<TypeRef>,
        Location,
        Annotations,
    ),
    Expression(Expression),
    Function(FunctionContract, Box<Statement>),
    ExternalFunction(FunctionContract, Option<Box<Statement>>),
    Struct {
        name: GlobalStr,
        elements: Vec<(GlobalStr, TypeRef)>,
        location: Location,
        global_impl: HashMap<GlobalStr, (FunctionContract, Statement)>,
        #[allow(clippy::type_complexity)]
        impls: Vec<(
            GlobalStr,
            HashMap<GlobalStr, (FunctionContract, Statement)>,
            Location,
        )>,
        generics: Vec<Generic>,
        annotations: Annotations,
    },
    Trait(Trait),
    /// key (the name of the thing in the module), export key (the name during import), location
    Export(GlobalStr, GlobalStr, Location),
    ModuleAsm(Location, String),

    BakedFunction(StoreKey<Function>, Location),
    BakedExternalFunction(StoreKey<ExternalFunction>, Location),
    BakedStruct(StoreKey<BakedStruct>, Location),
    BakedStatic(StoreKey<Static>, Location),
    BakedTrait(StoreKey<Trait>, Location),
}

impl Statement {
    pub fn loc(&self) -> &Location {
        match self {
            Self::Expression(expr) => expr.loc(),
            Self::ExternalFunction(c, _) | Self::Function(c, _) => &c.location,

            Self::Export(_, _, location)
            | Self::Block(_, location, _)
            | Self::ModuleAsm(location, _)
            | Self::For { location, .. }
            | Self::If { location, .. }
            | Self::Return(_, location)
            | Self::Struct { location, .. }
            | Self::Var(_, _, _, location, _)
            | Self::BakedFunction(_, location)
            | Self::BakedExternalFunction(_, location)
            | Self::BakedStruct(_, location)
            | Self::BakedStatic(_, location)
            | Self::BakedTrait(_, location)
            | Self::Trait(Trait { location, .. })
            | Self::While { location, .. } => location,
        }
    }

    pub fn bake_functions(
        &mut self,
        module: &mut Module,
        module_key: StoreKey<Module>,
        context: &ModuleContext,
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
            Self::ExternalFunction(..) => unreachable!("function in a non-top-level scope"),
            Self::Struct { location, .. } => {
                panic!("{location}: use Module::push_statement to bake a struct")
            }
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

impl Display for Statement {
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
                if stmts.len() > 0 {
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
                location: _,
                annotations,
            } => f.write_fmt(format_args!(
                "{annotations}(if {condition} {if_stmt} {else_stmt})"
            )),
            Self::If {
                condition,
                if_stmt,
                else_stmt: None,
                location: _,
                annotations,
            } => f.write_fmt(format_args!("{annotations}(if {condition} {if_stmt})")),
            Self::For {
                iterator,
                var_name,
                child,
                location: _,
                annotations,
            } => f.write_fmt(format_args!(
                "{annotations}(for {var_name} {iterator} {child})"
            )),
            Self::While {
                condition,
                child,
                location: _,
                annotations,
            } => f.write_fmt(format_args!("{annotations}(while {condition} {child})")),
            Self::Struct {
                name,
                elements: arguments,
                location: _,
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
            Self::Function(contract, body) => {
                display_contract(f, contract, false)?;
                f.write_char(' ')?;
                Display::fmt(body, f)?;
                f.write_char(')')
            }
            Self::ExternalFunction(contract, None) => {
                display_contract(f, contract, true)?;
                f.write_char(')')
            }
            Self::ExternalFunction(contract, Some(body)) => {
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

impl Parser<'_> {
    fn consume_semicolon(&mut self) -> Result<(), ParsingError> {
        self.expect_tok(TokenType::Semicolon)?;
        while self.match_tok(TokenType::Semicolon) {}
        Ok(())
    }

    pub fn parse_all(&mut self) -> (Vec<Statement>, Vec<ParsingError>) {
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
            errors.push(ParsingError::ExpectedStatement {
                loc: self.peek().location.clone(),
            });
        }

        (statements, errors)
    }

    pub fn parse_statement(&mut self, is_global: bool) -> Result<Statement, ParsingError> {
        while !self.is_at_end() {
            if let Some(statement) = self.parse_statement_part(is_global)? {
                return Ok(statement);
            }
        }
        Err(ParsingError::ExpectedStatement {
            loc: self.peek().location.clone(),
        })
    }

    pub fn parse_statement_part(
        &mut self,
        is_global: bool,
    ) -> Result<Option<Statement>, ParsingError> {
        macro_rules! invalid_kw {
            ($kw: literal) => {
                return Err(ParsingError::InvalidKeyword {
                    keyword: $kw,
                    loc: self.peek().location.clone(),
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
            return Err(ParsingError::ExpectedAnnotationStatement {
                loc: self.peek().location.clone(),
            });
        }

        let maybe_statement = match self.peek().typ {
            TokenType::Extern if !is_global => invalid_kw!("external value/function"),
            TokenType::Fn if !is_global => invalid_kw!("function"),
            TokenType::Struct if !is_global => invalid_kw!("struct definition"),
            TokenType::Use if !is_global => invalid_kw!("use"),
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
                .and_then(|(contract, body)| {
                    contract
                        .annotations
                        .are_annotations_valid_for(AnnotationReceiver::Function)?;
                    Ok(Statement::Function(contract, Box::new(body)))
                })
                .map(Some),
            TokenType::AnnotationIntroducer => {
                self.parse_annotation()?;
                Ok(None)
            }
            TokenType::Extern => self.parse_external().map(Some),
            TokenType::Eof => {
                return Err(ParsingError::ExpectedStatement {
                    loc: self.peek().location.clone(),
                });
            }
            TokenType::Use => self.parse_use().map(|_| None),
            TokenType::Export => self.parse_export().map(Some),
            TokenType::Pub => self.parse_pub().map(Some),
            _ if is_global => {
                return Err(ParsingError::ExpressionAtTopLevel {
                    loc: self.peek().location.clone(),
                });
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

    fn parse_pub(&mut self) -> Result<Statement, ParsingError> {
        let loc = self.advance().location.clone();
        let stmt = match self.peek().typ {
            TokenType::Fn => self.parse_callable(false).and_then(|(contract, body)| {
                contract
                    .annotations
                    .are_annotations_valid_for(AnnotationReceiver::Function)?;
                Ok(Statement::Function(contract, Box::new(body)))
            })?,

            TokenType::Let => self.parse_let_stmt(true)?,
            TokenType::Struct => self.parse_struct()?,
            TokenType::Extern => self.parse_external()?,
            TokenType::Trait => self.parse_trait()?,
            _ => {
                return Err(ParsingError::ExpectedElementForPub {
                    loc,
                    typ: self.peek().typ,
                })
            }
        };
        let (symbol, loc) = match &stmt {
            Statement::Function(c, _) | Statement::ExternalFunction(c, _) => (
                c.name
                    .as_ref()
                    .cloned()
                    .expect("global functions should always have a name"),
                c.location.clone(),
            ),
            Statement::Trait(Trait { name, location, .. })
            | Statement::Var(name, .., location, _)
            | Statement::Struct { name, location, .. } => (name.clone(), location.clone()),
            _ => unreachable!(),
        };
        self.tokens.insert(
            self.current,
            Token {
                typ: TokenType::Export,
                literal: None,
                location: loc.clone(),
            },
        );
        self.tokens.insert(
            self.current + 1,
            Token {
                typ: TokenType::IdentifierLiteral,
                literal: Some(Literal::String(symbol)),
                location: loc.clone(),
            },
        );
        self.tokens.insert(
            self.current + 2,
            Token {
                typ: TokenType::Semicolon,
                literal: None,
                location: loc,
            },
        );

        Ok(stmt)
    }

    fn parse_global_asm(&mut self) -> Result<Statement, ParsingError> {
        let loc = self.advance().location.clone();
        self.expect_tok(TokenType::ParenLeft)?;
        let mut strn = String::new();
        while !self.match_tok(TokenType::ParenRight) {
            if !strn.is_empty() {
                self.expect_tok(TokenType::Comma)?;
                if self.match_tok(TokenType::ParenRight) {
                    break;
                }
            }
            let tok = self.expect_tok(TokenType::StringLiteral)?;
            tok.string_literal()?.with(|v| {
                if !strn.is_empty() {
                    strn.push('\n');
                }
                strn.push_str(v);
            });
        }
        Ok(Statement::ModuleAsm(loc, strn))
    }

    fn parse_export(&mut self) -> Result<Statement, ParsingError> {
        let loc = self.advance().location.clone();
        let name = self.expect_identifier()?;
        let exported_name = if self.match_tok(TokenType::As) {
            self.expect_identifier()?
        } else {
            name.clone()
        };
        self.consume_semicolon()?;
        Ok(Statement::Export(name, exported_name, loc))
    }

    fn parse_use(&mut self) -> Result<(), ParsingError> {
        let loc = self.advance().location.clone();
        let name = self
            .expect_tok(TokenType::StringLiteral)?
            .string_literal()?
            .clone();
        let ResolvedPath { root_dir, file } = name
            .with(|name| {
                resolve_module(
                    name,
                    self.file
                        .parent()
                        .expect("file should have a parent directory"),
                    self.root_directory.clone(),
                    &loc,
                    &self.resolvers,
                    &*self.path_exists,
                    &*self.path_is_dir,
                )
            })
            .ok_or_else(|| ParsingError::CannotResolveModule {
                loc: loc.clone(),
                name,
            })?;

        let module_key = self
            .modules
            .read()
            .index_value_iter()
            .find(|v| v.1.path == file)
            .map(|v| v.0);
        let module_key = match module_key {
            Some(v) => v,
            None => {
                let reserved_key = self.modules.write().reserve_key();
                self.parser_queue.write().push(ParserQueueEntry {
                    file,
                    root_dir,
                    reserved_key,
                });
                reserved_key
            }
        };

        if self.match_tok(TokenType::Semicolon) {
            _ = self.consume_semicolon(); // it doesn't matter if this fails because we already got
                                          // at least a single semicolon
            return Ok(());
        }

        if self.match_tok(TokenType::As) {
            let name = self.expect_identifier()?;
            self.imports.insert(name, (loc, module_key, Vec::new()));
            self.consume_semicolon()?;
            return Ok(());
        }

        self.expect_tok(TokenType::NamespaceAccess)?;

        if self.match_tok(TokenType::CurlyLeft) {
            let mut is_first = true;
            while !self.match_tok(TokenType::CurlyRight) {
                if !is_first {
                    self.expect_tok(TokenType::Comma)?;

                    if self.match_tok(TokenType::CurlyRight) {
                        break;
                    }
                }
                is_first = false;

                let import_name = self.parse_path_no_generics()?;

                if self.match_tok(TokenType::As) {
                    let alias_name = self.expect_identifier()?;
                    self.imports
                        .insert(alias_name, (loc.clone(), module_key, import_name));
                } else {
                    self.imports.insert(
                        import_name[import_name.len() - 1].clone(),
                        (loc.clone(), module_key, import_name),
                    );
                }
            }
            self.consume_semicolon()?;
            return Ok(());
        }

        let import_name = self.parse_path_no_generics()?;

        if self.match_tok(TokenType::As) {
            let alias_name = self.expect_identifier()?;
            self.imports
                .insert(alias_name, (loc, module_key, import_name));
            self.consume_semicolon()?;
            return Ok(());
        }

        self.imports.insert(
            import_name[import_name.len() - 1].clone(),
            (loc, module_key, import_name),
        );
        self.consume_semicolon()?;
        Ok(())
    }

    fn parse_trait_fn(
        &mut self,
    ) -> Result<(GlobalStr, Vec<Argument>, TypeRef, Annotations, Location), ParsingError> {
        let location = self.tokens[self.current].location.clone();
        let name = self.expect_identifier()?;

        let mut arguments = vec![];

        self.expect_tok(TokenType::ParenLeft)?;

        while !self.match_tok(TokenType::ParenRight) {
            if !arguments.is_empty() {
                if !self.match_tok(TokenType::Comma) {
                    return Err(ParsingError::ExpectedFunctionArgument {
                        loc: self.peek().location.clone(),
                        found: self.peek().typ,
                    });
                }

                // for trailing comma
                if self.match_tok(TokenType::ParenRight) {
                    break;
                }
            }

            let name = self.expect_identifier()?.clone();
            self.expect_tok(TokenType::Colon)?;

            arguments.push(Argument::new(TypeRef::parse(self)?, name))
        }

        let return_type = if self.match_tok(TokenType::ReturnType) {
            TypeRef::parse(self)?
        } else {
            TypeRef::Void(self.peek().location.clone(), 0)
        };

        self.expect_tok(TokenType::Semicolon)?;

        Ok((
            name,
            arguments,
            return_type,
            std::mem::take(&mut self.current_annotations),
            location,
        ))
    }

    fn parse_trait(&mut self) -> Result<Statement, ParsingError> {
        let location = self.advance().location.clone(); // skip `trait`
        let name = self.expect_identifier()?;

        self.expect_tok(TokenType::CurlyLeft)?;

        let annotations = std::mem::take(&mut self.current_annotations);
        annotations.are_annotations_valid_for(crate::annotations::AnnotationReceiver::Trait)?;
        let mut functions = Vec::new();

        while !self.match_tok(TokenType::CurlyRight) {
            if self.match_tok(TokenType::AnnotationIntroducer) {
                self.parse_annotation()?;
                continue;
            }

            self.expect_tok(TokenType::Fn)?;
            let func = self.parse_trait_fn()?;
            functions.push(func);
        }

        Ok(Statement::Trait(Trait {
            name,
            functions,
            location,
            annotations,
            module: self.key,
        }))
    }

    fn parse_path_no_generics(&mut self) -> Result<Vec<GlobalStr>, ParsingError> {
        let mut path = vec![self.expect_identifier()?];

        while self.match_tok(TokenType::NamespaceAccess) {
            path.push(self.expect_identifier()?);
        }

        Ok(path)
    }

    fn parse_expression_stmt(&mut self) -> Result<Statement, ParsingError> {
        let expr = self.parse_expression()?;
        self.consume_semicolon()?;
        Ok(Statement::Expression(expr))
    }

    fn parse_let_stmt(&mut self, is_static: bool) -> Result<Statement, ParsingError> {
        // let <identifier>;
        // let <identifier> = <expr>;
        let location = self.advance().location.clone(); // skip `let`

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

        self.expect_tok(TokenType::Equal)?;

        let expr = self.parse_expression()?;
        self.consume_semicolon()?;
        Ok(Statement::Var(name, expr, typ, location, annotations))
    }
    fn parse_block_stmt(&mut self) -> Result<Statement, ParsingError> {
        let annotations = std::mem::take(&mut self.current_annotations);
        annotations.are_annotations_valid_for(AnnotationReceiver::Block)?;

        // { <...statements...> }
        let location = self.advance().location.clone(); // skip `{`
        let mut statements = vec![];

        while !self.match_tok(TokenType::CurlyRight) {
            statements.push(self.parse_statement(false)?);
        }

        Ok(Statement::Block(
            statements.into_boxed_slice(),
            location,
            annotations,
        ))
    }
    fn parse_return_stmt(&mut self) -> Result<Statement, ParsingError> {
        // return;
        // return <expr>;
        let location = self.advance().location.clone(); // skip `return`
        if self.match_tok(TokenType::Semicolon) {
            return Ok(Statement::Return(None, location));
        }
        let expr = self.parse_expression()?;
        self.consume_semicolon()?;
        Ok(Statement::Return(Some(expr), location))
    }
    fn parse_if_stmt(&mut self) -> Result<Statement, ParsingError> {
        let annotations = std::mem::take(&mut self.current_annotations);
        annotations.are_annotations_valid_for(AnnotationReceiver::If)?;

        // if (<expr>) <stmt>
        // if (<expr>) <stmt> else <stmt>
        // we have else if support, because else <stmt>, the stmt can be another if expression!
        let location = self.advance().location.clone(); // skip `if`
        self.expect_tok(TokenType::ParenLeft)?;

        let condition = self.parse_expression()?;

        self.expect_tok(TokenType::ParenRight)?;
        let if_stmt = self.parse_statement(false)?;
        if self.match_tok(TokenType::Else) {
            return Ok(Statement::If {
                condition,
                if_stmt: Box::new(if_stmt),
                else_stmt: Some(Box::new(self.parse_statement(false)?)),
                location,
                annotations: std::mem::take(&mut self.current_annotations),
            });
        }
        Ok(Statement::If {
            condition,
            if_stmt: Box::new(if_stmt),
            else_stmt: None,
            location,
            annotations,
        })
    }
    fn parse_while_stmt(&mut self) -> Result<Statement, ParsingError> {
        let annotations = std::mem::take(&mut self.current_annotations);
        annotations.are_annotations_valid_for(AnnotationReceiver::While)?;

        // while (<expr>) <stmt>
        // we have else if support, because else <stmt>, the stmt can be another if expression!
        let location = self.advance().location.clone(); // skip `while`

        self.expect_tok(TokenType::ParenLeft)?;
        let condition = self.parse_expression()?;
        self.expect_tok(TokenType::ParenRight)?;

        Ok(Statement::While {
            condition,
            child: Box::new(self.parse_statement(false)?),
            location,
            annotations,
        })
    }
    fn parse_for_stmt(&mut self) -> Result<Statement, ParsingError> {
        let annotations = std::mem::take(&mut self.current_annotations);
        annotations.are_annotations_valid_for(AnnotationReceiver::For)?;

        // for (<identifier> in <expr>) <stmt>
        let location = self.advance().location.clone(); // skip over `for`

        self.expect_tok(TokenType::ParenLeft)?;

        let var_name = self.expect_identifier()?;
        self.expect_tok(TokenType::In)?;

        let iterator = self.parse_expression()?;

        self.expect_tok(TokenType::ParenRight)?;

        let child = Box::new(self.parse_statement(false)?);
        Ok(Statement::For {
            iterator,
            var_name,
            child,
            location,
            annotations,
        })
    }
    fn parse_struct(&mut self) -> Result<Statement, ParsingError> {
        let annotations = std::mem::take(&mut self.current_annotations);
        annotations.are_annotations_valid_for(AnnotationReceiver::Struct)?;

        // struct Name { ... fields ...; implementation area }
        // fields: field: type,[...]
        // implementation area: fn implementation area | impl TraitName { implementation area no trait } implementation area | ""
        // implementation area no trait: fn implementation area no trait | ""
        let location = self.advance().location.clone(); // skip over `struct`
        let name = self.expect_identifier()?;

        let mut generics = vec![];
        if self.match_tok(TokenType::LessThan) {
            while !self.match_tok(TokenType::GreaterThan) {
                if generics.len() > 1 {
                    self.expect_tok(TokenType::Comma)?;

                    if self.match_tok(TokenType::GreaterThan) {
                        break;
                    }
                }

                generics.push(Generic::parse(self)?);
            }
        }

        let mut elements = vec![];

        self.expect_tok(TokenType::CurlyLeft)?;

        while !self.matches(&[TokenType::CurlyRight, TokenType::Semicolon]) {
            if !elements.is_empty() {
                // needs comma
                if !self.match_tok(TokenType::Comma) {
                    return Err(ParsingError::ExpectedObjectElement {
                        loc: self.peek().location.clone(),
                        found: self.peek().typ,
                    });
                }
                // for trailing commas
                if self.match_tok(TokenType::CurlyRight) {
                    break;
                }
            }
            let name = self.expect_identifier()?;
            if !self.match_tok(TokenType::Colon) {
                return Err(ParsingError::ExpectedType {
                    loc: self.peek().location.clone(),
                    found: self.peek().typ,
                });
            }
            let typ = TypeRef::parse(self)?;
            elements.push((name, typ));
        }

        let mut global_impl = HashMap::<GlobalStr, (FunctionContract, Statement)>::new();
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
                                loc: func.0.location.clone(),
                                name,
                                first_func_loc: other_func.0.location.clone(),
                            });
                        }
                        global_impl.insert(name, func);
                    }
                    TokenType::Impl => {
                        let loc = self.advance().location.clone();
                        let trait_name: GlobalStr = self.expect_identifier()?;
                        let mut current_impl =
                            HashMap::<GlobalStr, (FunctionContract, Statement)>::new();

                        self.expect_tok(TokenType::CurlyLeft)?;
                        while !self.match_tok(TokenType::CurlyRight) {
                            if self.peek().typ != TokenType::Fn {
                                return Err(ParsingError::StructImplRegionExpect {
                                    loc: self.peek().location.clone(),
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
                                    loc: func.0.location,
                                    name,
                                    first_func_loc: other_func.0.location.clone(),
                                });
                            }
                            current_impl.insert(name, func);
                        }
                        impls.push((trait_name, current_impl, loc));
                    }
                    token => {
                        return Err(ParsingError::StructImplRegionExpect {
                            loc: self.peek().location.clone(),
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
            location,
            global_impl,
            impls,
            annotations,
            generics,
        })
    }

    pub fn expect_identifier(&mut self) -> Result<GlobalStr, ParsingError> {
        if !self.match_tok(TokenType::IdentifierLiteral) {
            return Err(ParsingError::ExpectedIdentifier {
                loc: self.peek().location.clone(),
                found: self.peek().typ,
            });
        }
        self.current().string_literal().cloned()
    }
    pub fn parse_annotation(&mut self) -> Result<(), ParsingError> {
        let loc = self.peek().location.clone();
        assert_eq!(self.advance().typ, TokenType::AnnotationIntroducer);

        let name = match self.peek().typ {
            TokenType::If => GlobalStr::new("if"),
            TokenType::While => GlobalStr::new("while"),
            TokenType::For => GlobalStr::new("for"),
            TokenType::Pub => GlobalStr::new("pub"),
            TokenType::As => GlobalStr::new("as"),
            TokenType::Else => GlobalStr::new("else"),
            TokenType::Asm => GlobalStr::new("asm"),
            TokenType::Volatile => GlobalStr::new("volatile"),
            TokenType::Impl => GlobalStr::new("impl"),
            TokenType::Fn => GlobalStr::new("fn"),
            TokenType::In => GlobalStr::new("in"),
            TokenType::Unsized => GlobalStr::new("unsized"),
            TokenType::Struct => GlobalStr::new("struct"),
            TokenType::Trait => GlobalStr::new("trait"),
            TokenType::IdentifierLiteral => self.peek().string_literal()?.clone(),

            _ => {
                return Err(ParsingError::ExpectedIdentifier {
                    loc: self.peek().location.clone(),
                    found: self.peek().typ,
                })
            }
        };
        self.advance();

        self.expect_tok(TokenType::ParenLeft)?;
        let mut deepness = 0;
        let mut args = vec![];

        loop {
            match self.peek().typ {
                TokenType::Eof => {
                    return Err(ParsingError::ExpectedArbitrary {
                        loc: self.peek().location.clone(),
                        expected: TokenType::ParenRight,
                        found: TokenType::Eof,
                    })
                }
                TokenType::ParenRight if deepness == 0 => {
                    self.advance();
                    break;
                }
                TokenType::ParenRight => deepness -= 1,
                TokenType::ParenLeft => deepness += 1,
                _ => (),
            }
            args.push(self.advance().clone());
        }

        let name = name.with(|v| v.to_string());
        self.current_annotations.push_annotation(&name, args, loc)
    }
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub name: GlobalStr,
    pub typ: TypeRef,
}

impl Argument {
    pub fn new(typ: TypeRef, name: GlobalStr) -> Self {
        Self { name, typ }
    }
}

impl Display for Argument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.name, f)?;
        f.write_str(": ")?;
        Display::fmt(&self.typ, f)
    }
}

#[derive(Clone, Debug)]
pub struct FunctionContract {
    pub name: Option<GlobalStr>,
    pub arguments: Vec<Argument>,
    pub return_type: TypeRef,
    pub location: Location,
    pub annotations: Annotations,
    pub generics: Vec<Generic>,
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

impl Parser<'_> {
    pub fn parse_external(&mut self) -> Result<Statement, ParsingError> {
        let location = self.advance().location.clone();
        self.parse_any_callable(false, false, false)
            .and_then(|(mut contract, body)| {
                contract
                    .annotations
                    .are_annotations_valid_for(AnnotationReceiver::ExternalFunction)?;
                contract.location = location;
                Ok(Statement::ExternalFunction(contract, body.map(Box::new)))
            })
    }

    pub fn parse_callable(
        &mut self,
        anonymous: bool,
    ) -> Result<(FunctionContract, Statement), ParsingError> {
        self.parse_any_callable(anonymous, true, true)
            .map(|(contract, body)| (contract, body.expect("there should always exist a body")))
    }

    pub fn parse_any_callable(
        &mut self,
        anonymous: bool,
        needs_body: bool,
        can_have_generics: bool,
    ) -> Result<(FunctionContract, Option<Statement>), ParsingError> {
        let annotations = std::mem::take(&mut self.current_annotations);

        let location = self.peek().location.clone();
        self.expect_tok(TokenType::Fn)?;

        let name = if anonymous {
            self.expect_identifier().ok()
        } else {
            Some(self.expect_identifier()?.clone())
        };

        let generics = if can_have_generics && self.match_tok(TokenType::LessThan) {
            self.parse_function_generics()?
        } else {
            vec![]
        };

        let mut arguments = vec![];

        self.expect_tok(TokenType::ParenLeft)?;

        while !self.match_tok(TokenType::ParenRight) {
            if !arguments.is_empty() {
                if !self.match_tok(TokenType::Comma) {
                    return Err(ParsingError::ExpectedFunctionArgument {
                        loc: self.peek().location.clone(),
                        found: self.peek().typ,
                    });
                }

                // for trailing comma
                if self.match_tok(TokenType::ParenRight) {
                    break;
                }
            }

            let name = self.expect_identifier()?.clone();
            self.expect_tok(TokenType::Colon)?;

            arguments.push(Argument::new(TypeRef::parse(self)?, name));
        }

        let return_type = if self.match_tok(TokenType::ReturnType) {
            TypeRef::parse(self)?
        } else {
            TypeRef::Void(self.peek().location.clone(), 0)
        };

        let body = if !needs_body && self.match_tok(TokenType::Semicolon) {
            None
        } else {
            Some(match self.peek().typ {
                TokenType::CurlyLeft => self.parse_block_stmt()?,
                TokenType::Equal => {
                    self.advance();
                    let expr = self.parse_expression()?;
                    let return_location = expr.loc().clone();
                    if !anonymous {
                        // needs a semicolon if this isnt anonymous
                        self.consume_semicolon()?;
                    }
                    Statement::Return(Some(expr), return_location)
                }
                _ => {
                    return Err(ParsingError::ExpectedFunctionBody {
                        loc: self.peek().location.clone(),
                        found: self.peek().typ,
                    })
                }
            })
        };

        Ok((
            FunctionContract {
                name,
                arguments,
                return_type,
                location,
                annotations,
                generics,
            },
            body,
        ))
    }

    pub fn parse_function_generics(&mut self) -> Result<Vec<Generic>, ParsingError> {
        let mut generics = vec![];

        while !self.match_tok(TokenType::GreaterThan) {
            if !generics.is_empty() {
                self.expect_tok(TokenType::Comma)?;

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
