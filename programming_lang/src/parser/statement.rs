use std::{
    collections::HashMap,
    fmt::{Debug, Display, Write},
};

use crate::{
    error::ProgrammingLangParsingError,
    globals::GlobalStr,
    module::{FunctionId, Module},
    parser::{module_resolution::resolve_module, Annotation, ParserQueueEntry},
    tokenizer::{Literal, Location, TokenType},
};

use super::{
    types::{Generic, TypeRef},
    Annotations, Expression, Parser, Path,
};

#[derive(Clone, Debug)]
pub enum BakableFunction {
    Function(Box<(FunctionContract, Statement)>),
    BakedFunction(FunctionId),
}

impl BakableFunction {
    pub fn bake(&mut self, module: &mut Module) {
        match self {
            Self::BakedFunction(..) => (),
            Self::Function(..) => {
                let mut old_self = Self::BakedFunction(0);
                std::mem::swap(self, &mut old_self);
                let val = match old_self {
                    Self::BakedFunction(..) => unreachable!(),
                    Self::Function(func) => *func,
                };
                *self = Self::BakedFunction(module.push_fn(val.0, val.1));
            }
        }
    }

    pub fn get_baked_id(&self) -> FunctionId {
        match self {
            Self::Function(..) => unreachable!(),
            Self::BakedFunction(id) => *id,
        }
    }

    pub fn get_contract_unbaked(&self) -> &FunctionContract {
        match self {
            Self::Function(func) => &func.0,
            Self::BakedFunction(..) => unreachable!(),
        }
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
    Var(GlobalStr, Expression, Option<TypeRef>, Location),
    Expression(Expression),
    Function(FunctionContract, Box<Statement>),
    BakedFunction(FunctionId, Location),
    ExternalFunction(FunctionContract),
    Struct {
        name: GlobalStr,
        elements: Vec<(GlobalStr, TypeRef)>,
        location: Location,
        global_impl: HashMap<GlobalStr, BakableFunction>,
        impls: Vec<(GlobalStr, HashMap<GlobalStr, BakableFunction>)>,
        annotations: Annotations,
    },
    Export(GlobalStr, Location),
}

impl Statement {
    pub fn loc(&self) -> &Location {
        match self {
            Self::Expression(expr) => expr.loc(),
            Self::ExternalFunction(c) | Self::Function(c, _) => &c.location,

            Self::Export(_, location)
            | Self::BakedFunction(_, location)
            | Self::Block(_, location, _)
            | Self::For { location, .. }
            | Self::If { location, .. }
            | Self::Return(_, location)
            | Self::Struct { location, .. }
            | Self::Var(_, _, _, location)
            | Self::While { location, .. } => location,
        }
    }

    pub fn bake_functions(&mut self, module: &mut Module) {
        match self {
            Self::BakedFunction(..)
            | Self::ExternalFunction(..)
            | Self::Return(None, ..)
            | Self::Export(..) => (),
            Self::Struct { location, .. } => {
                panic!("{location}: use Module::push_statement to bake a struct")
            }
            Self::Function(contract, statement) => {
                let location = contract.location.clone();

                let dummy_contract: FunctionContract = FunctionContract {
                    annotations: Default::default(),
                    arguments: Default::default(),
                    generics: Default::default(),
                    location: location.clone(),
                    name: None,
                    return_type: TypeRef::Void(location.clone()),
                };
                let dummy_statement: Statement =
                    Statement::ExternalFunction(dummy_contract.clone());

                let id = module.push_fn(
                    std::mem::replace(contract, dummy_contract),
                    std::mem::replace(statement, dummy_statement),
                );

                *self = Self::BakedFunction(id, location);
            }
            Self::Block(statements, ..) => statements
                .iter_mut()
                .for_each(|stmt| stmt.bake_functions(module)),
            Self::Var(_, stmt, ..) => stmt.bake_functions(module),
            Self::Expression(expr) => expr.bake_functions(module),
            Self::For {
                iterator, child, ..
            } => {
                iterator.bake_functions(module);
                child.bake_functions(module);
            }
            Self::While {
                condition, child, ..
            } => {
                condition.bake_functions(module);
                child.bake_functions(module);
            }
            Self::If {
                condition,
                if_stmt,
                else_stmt,
                ..
            } => {
                condition.bake_functions(module);
                if_stmt.bake_functions(module);
                if let Some(stmt) = else_stmt {
                    stmt.bake_functions(module);
                }
            }
            Self::Return(Some(val), ..) => val.bake_functions(module),
        }
    }

    pub fn display_module(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        module: &Module,
    ) -> std::fmt::Result {
        match self {
            Self::BakedFunction(id, _) => {
                if let Some((contract, body)) = module.get_fn(*id) {
                    display_contract(f, contract, false)?;
                    Display::fmt(body, f)?;
                    f.write_char(')')
                } else {
                    f.write_str("(module-fn ")?;
                    Debug::fmt(id, f)?;
                    f.write_char(')')
                }
            }
            Self::Var(left_hand, right_hand, None, _) => {
                f.write_fmt(format_args!("(var-assign {left_hand} {right_hand})"))
            }
            Self::Var(left_hand, right_hand, Some(typ), _) => {
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
                ..
            } => {
                Display::fmt(name, f)?;
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
                Display::fmt(body, f)?;
                f.write_char(')')
            }
            Self::ExternalFunction(contract) => display_contract(f, contract, true),
            Self::Export(name, _) => {
                f.write_str("(export ")?;
                Display::fmt(name, f)?;
                f.write_char(')')
            }
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BakedFunction(id, _) => f.write_fmt(format_args!("(module-fn {id:08x})")),
            Self::Var(left_hand, right_hand, None, _) => {
                f.write_fmt(format_args!("(var-assign {left_hand} {right_hand})"))
            }
            Self::Var(left_hand, right_hand, Some(typ), _) => {
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
                ..
            } => {
                Display::fmt(name, f)?;
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
                Display::fmt(body, f)?;
                f.write_char(')')
            }
            Self::ExternalFunction(contract) => display_contract(f, contract, true),
            Self::Export(name, _) => {
                f.write_str("(export ")?;
                Display::fmt(name, f)?;
                f.write_char(')')
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

    if contract.generics.len() > 0 {
        f.write_char('<')?;
        for i in 0..contract.generics.len() {
            let generic = &contract.generics[i];
            if i != 0 {
                f.write_str(", ")?;
            }
            Display::fmt(&generic.name, f)?;
            if generic.bounds.len() > 0 {
                f.write_str(": ")?;
                for i in 0..generic.bounds.len() {
                    if i != 0 {
                        f.write_str(" + ")?;
                    }
                    Display::fmt(&generic.bounds[i], f)?;
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
    if let TypeRef::Void(_) = &contract.return_type {
    } else {
        f.write_str(" returns ")?;
        Display::fmt(&contract.return_type, f)?;
    }
    if is_external {
        f.write_char(')')
    } else {
        f.write_char(' ')
    }
}

impl Parser {
    fn consume_semicolon(&mut self) -> Result<(), ProgrammingLangParsingError> {
        if !self.match_tok(TokenType::Semicolon) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location.clone(),
                expected: TokenType::Semicolon,
                found: self.peek().typ,
            });
        }
        while self.match_tok(TokenType::Semicolon) {}
        Ok(())
    }

    pub fn parse_all(&mut self) -> (Vec<Statement>, Vec<ProgrammingLangParsingError>) {
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

        if !self.current_annotations.0.is_empty() {
            errors.push(ProgrammingLangParsingError::ExpectedStatement {
                loc: self.peek().location.clone(),
            });
        }

        (statements, errors)
    }

    pub fn parse_statement(
        &mut self,
        is_global: bool,
    ) -> Result<Statement, ProgrammingLangParsingError> {
        while !self.is_at_end() {
            match self.parse_statement_part(is_global)? {
                Some(statement) => return Ok(statement),
                None => {}
            }
        }
        return Err(ProgrammingLangParsingError::ExpectedStatement {
            loc: self.peek().location.clone(),
        });
    }

    pub fn parse_statement_part(
        &mut self,
        is_global: bool,
    ) -> Result<Option<Statement>, ProgrammingLangParsingError> {
        macro_rules! invalid_kw {
            ($kw: literal) => {
                return Err(ProgrammingLangParsingError::InvalidKeyword {
                    keyword: $kw,
                    loc: self.peek().location.clone(),
                })
            };
        }

        if self.current_annotations.0.len() > 0
            && match self.peek().typ {
                TokenType::AnnotationIntroducer
                | TokenType::Extern
                | TokenType::Fn
                | TokenType::CurlyLeft
                | TokenType::Struct
                | TokenType::For
                | TokenType::While
                | TokenType::If => false,

                _ => true,
            }
        {
            self.bail();
            return Err(ProgrammingLangParsingError::ExpectedAnnotationStatement {
                loc: self.peek().location.clone(),
            });
        }

        let maybe_statement = match self.peek().typ {
            TokenType::Extern if !is_global => invalid_kw!("external value/function"),
            TokenType::Struct if !is_global => invalid_kw!("struct definition"),
            TokenType::Use if !is_global => invalid_kw!("use"),
            TokenType::Export if !is_global => invalid_kw!("export"),

            TokenType::Return if is_global => invalid_kw!("return"),
            TokenType::CurlyLeft if is_global => invalid_kw!("code block"),
            TokenType::If if is_global => invalid_kw!("if statement"),
            TokenType::While if is_global => invalid_kw!("while loop"),
            TokenType::For if is_global => invalid_kw!("for loop"),

            TokenType::Let => self.parse_let_stmt().map(Some),
            TokenType::CurlyLeft => self.parse_block_stmt().map(Some),
            TokenType::Return => self.parse_return_stmt().map(Some),
            TokenType::If => self.parse_if_stmt().map(Some),
            TokenType::While => self.parse_while_stmt().map(Some),
            TokenType::For => self.parse_for_stmt().map(Some),
            TokenType::Struct => self.parse_struct().map(Some),
            TokenType::Fn => self
                .parse_callable(false)
                .map(|(contract, body)| Statement::Function(contract, body))
                .map(Some),
            TokenType::AnnotationIntroducer => {
                self.parse_annotation()?;
                Ok(None)
            }
            TokenType::Extern => self.parse_external().map(Some),
            TokenType::Eof => {
                return Err(ProgrammingLangParsingError::ExpectedStatement {
                    loc: self.peek().location.clone(),
                });
            }
            TokenType::Use => self.parse_use().map(|_| None),
            TokenType::Export => self.parse_export().map(Some),
            _ if is_global => {
                return Err(ProgrammingLangParsingError::ExpressionAtTopLevel {
                    loc: self.peek().location.clone(),
                });
            }
            _ => self.parse_expression_stmt().map(Some),
        }?;
        if maybe_statement.is_some() {
            assert_eq!(
                self.current_annotations.0.len(),
                0,
                "more than 0 annotations left after parsing a statement"
            );
        }

        Ok(maybe_statement)
    }

    fn parse_export(&mut self) -> Result<Statement, ProgrammingLangParsingError> {
        let loc = self.advance().location.clone();
        let statement = self
            .expect_identifier()
            .map(|v| Statement::Export(v, loc))?;
        self.consume_semicolon()?;
        Ok(statement)
    }

    fn parse_use(&mut self) -> Result<(), ProgrammingLangParsingError> {
        let location = self.advance().location.clone();
        let name = self.advance();
        if name.typ != TokenType::StringLiteral {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: name.location.clone(),
                expected: TokenType::StringLiteral,
                found: name.typ,
            });
        }
        let Some(Literal::String(ref name)) = name.literal else {
            return Err(ProgrammingLangParsingError::InvalidTokenization {
                loc: name.location.clone(),
            });
        };
        let (module_file, module_root) = name
            .clone()
            .with(|name| {
                resolve_module(
                    name,
                    self.file
                        .parent()
                        .expect("file should have a parent directory"),
                    &self.root_directory,
                    &location,
                )
            })
            .map_err(ProgrammingLangParsingError::ModuleResolution)?;

        let module_id = self
            .modules
            .read()
            .expect("read-write Lock was poisoned")
            .iter()
            .position(|v| (*v.file).eq(&module_file));
        let module_id = match module_id {
            Some(v) => v,
            None => {
                let mut vec = self.modules.write().expect("read-write lock was poisoned");
                vec.push(ParserQueueEntry {
                    root: module_root
                        .map(Into::into)
                        .unwrap_or_else(|| self.root_directory.clone()),
                    file: module_file.into(),
                });
                vec.len() - 1
            }
        };

        if self.match_tok(TokenType::Semicolon) {
            _ = self.consume_semicolon(); // it doesn't matter if this fails because we already got
                                          // at least a single semicolon
            return Ok(());
        }

        if self.match_tok(TokenType::As) {
            let name = self.expect_identifier()?;
            self.imports.insert(name, (module_id, Vec::new()));
            self.consume_semicolon()?;
            return Ok(());
        }

        if !self.match_tok(TokenType::NamespaceAccess) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location.clone(),
                expected: TokenType::NamespaceAccess,
                found: self.peek().typ,
            });
        }

        if self.match_tok(TokenType::CurlyLeft) {
            let mut is_first = true;
            while !self.match_tok(TokenType::CurlyRight) {
                if !is_first {
                    // expect a comma
                    if !self.match_tok(TokenType::Comma) {
                        return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                            loc: self.peek().location.clone(),
                            expected: TokenType::Comma,
                            found: self.peek().typ,
                        });
                    }
                    if self.match_tok(TokenType::CurlyRight) {
                        break;
                    }
                }
                is_first = false;

                let import_name = self.parse_path_no_generics()?;

                if self.match_tok(TokenType::As) {
                    let alias_name = self.expect_identifier()?;
                    self.imports.insert(alias_name, (module_id, import_name));
                } else {
                    self.imports.insert(
                        import_name[import_name.len() - 1].clone(),
                        (module_id, import_name),
                    );
                }
            }
            self.consume_semicolon()?;
            return Ok(());
        }

        let import_name = self.parse_path_no_generics()?;

        if self.match_tok(TokenType::As) {
            let alias_name = self.expect_identifier()?;
            self.imports.insert(alias_name, (module_id, import_name));
            self.consume_semicolon()?;
            return Ok(());
        }

        self.imports.insert(
            import_name[import_name.len() - 1].clone(),
            (module_id, import_name),
        );
        self.consume_semicolon()?;
        Ok(())
    }

    fn parse_path_no_generics(&mut self) -> Result<Vec<GlobalStr>, ProgrammingLangParsingError> {
        let mut path = vec![self.expect_identifier()?];

        while self.match_tok(TokenType::NamespaceAccess) {
            path.push(self.expect_identifier()?);
        }

        Ok(path)
    }

    fn parse_expression_stmt(&mut self) -> Result<Statement, ProgrammingLangParsingError> {
        let expr = self.parse_expression()?;
        self.consume_semicolon()?;
        Ok(Statement::Expression(expr))
    }

    fn parse_let_stmt(&mut self) -> Result<Statement, ProgrammingLangParsingError> {
        // let <identifier>;
        // let <identifier> = <expr>;
        let location = self.advance().location.clone(); // skip `let`
        let name = self.expect_identifier()?;

        let typ = if self.match_tok(TokenType::Colon) {
            Some(TypeRef::parse(self)?)
        } else {
            None
        };

        if !self.match_tok(TokenType::Equal) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location.clone(),
                expected: TokenType::Equal,
                found: self.peek().typ,
            });
        }
        let expr = self.parse_expression()?;
        self.consume_semicolon()?;
        Ok(Statement::Var(name, expr, typ, location))
    }
    fn parse_block_stmt(&mut self) -> Result<Statement, ProgrammingLangParsingError> {
        let annotations = std::mem::take(&mut self.current_annotations);

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
    fn parse_return_stmt(&mut self) -> Result<Statement, ProgrammingLangParsingError> {
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
    fn parse_if_stmt(&mut self) -> Result<Statement, ProgrammingLangParsingError> {
        let annotations = std::mem::take(&mut self.current_annotations);

        // if (<expr>) <stmt>
        // if (<expr>) <stmt> else <stmt>
        // we have else if support, because else <stmt>, the stmt can be another if expression!
        let location = self.advance().location.clone(); // skip `if`
        if !self.match_tok(TokenType::ParenLeft) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location.clone(),
                found: self.peek().typ,
                expected: TokenType::ParenLeft,
            });
        }
        let condition = self.parse_expression()?;
        if !self.match_tok(TokenType::ParenRight) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location.clone(),
                found: self.peek().typ,
                expected: TokenType::ParenRight,
            });
        }
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
    fn parse_while_stmt(&mut self) -> Result<Statement, ProgrammingLangParsingError> {
        let annotations = std::mem::take(&mut self.current_annotations);

        // while (<expr>) <stmt>
        // we have else if support, because else <stmt>, the stmt can be another if expression!
        let location = self.advance().location.clone(); // skip `while`
        if !self.match_tok(TokenType::ParenLeft) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location.clone(),
                found: self.peek().typ,
                expected: TokenType::ParenLeft,
            });
        }
        let condition = self.parse_expression()?;
        if !self.match_tok(TokenType::ParenRight) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location.clone(),
                found: self.peek().typ,
                expected: TokenType::ParenRight,
            });
        }
        Ok(Statement::While {
            condition,
            child: Box::new(self.parse_statement(false)?),
            location,
            annotations,
        })
    }
    fn parse_for_stmt(&mut self) -> Result<Statement, ProgrammingLangParsingError> {
        let annotations = std::mem::take(&mut self.current_annotations);

        // for (<identifier> in <expr>) <stmt>
        let location = self.advance().location.clone(); // skip over `for`

        if !self.match_tok(TokenType::ParenLeft) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location.clone(),
                found: self.peek().typ,
                expected: TokenType::ParenLeft,
            });
        }

        let var_name = self.expect_identifier()?;
        if !self.match_tok(TokenType::In) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location.clone(),
                expected: TokenType::In,
                found: self.peek().typ,
            });
        }
        let iterator = self.parse_expression()?;
        if !self.match_tok(TokenType::ParenRight) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location.clone(),
                found: self.peek().typ,
                expected: TokenType::ParenRight,
            });
        }

        let child = Box::new(self.parse_statement(false)?);
        Ok(Statement::For {
            iterator,
            var_name,
            child,
            location,
            annotations,
        })
    }
    fn parse_struct(&mut self) -> Result<Statement, ProgrammingLangParsingError> {
        let annotations = std::mem::take(&mut self.current_annotations);

        // struct Name { ... fields ...; implementation area }
        // fields: field: type,[...]
        // implementation area: fn implementation area | impl TraitName { implementation area no trait } implementation area | ""
        // implementation area no trait: fn implementation area no trait | ""
        let location = self.advance().location.clone(); // skip over `struct`
        let name = self.expect_identifier()?;
        let mut elements = vec![];

        if !self.match_tok(TokenType::CurlyLeft) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location.clone(),
                expected: TokenType::CurlyLeft,
                found: self.peek().typ,
            });
        }

        while !self.matches(&[TokenType::CurlyRight, TokenType::Semicolon]) {
            if elements.len() > 0 {
                // needs comma
                if !self.match_tok(TokenType::Comma) {
                    return Err(ProgrammingLangParsingError::ExpectedObjectElement {
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
                return Err(ProgrammingLangParsingError::ExpectedType {
                    loc: self.peek().location.clone(),
                    found: self.peek().typ,
                });
            }
            let typ = TypeRef::parse(self)?;
            elements.push((name, typ));
        }

        let mut global_impl = HashMap::<GlobalStr, BakableFunction>::new();
        let mut impls = Vec::new();

        if self.previous().typ == TokenType::Semicolon {
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
                            return Err(ProgrammingLangParsingError::FunctionAlreadyDefined {
                                loc: func.0.location.clone(),
                                name: name,
                                first_func_loc: other_func.get_contract_unbaked().location.clone(),
                            });
                        }
                        global_impl.insert(
                            name,
                            BakableFunction::Function(Box::new((func.0.clone(), *func.1))),
                        );
                    }
                    TokenType::Impl => {
                        self.advance();
                        let trait_name: GlobalStr = self.expect_identifier()?;
                        let mut current_impl = HashMap::<GlobalStr, BakableFunction>::new();

                        if !self.match_tok(TokenType::CurlyLeft) {
                            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                                loc: self.peek().location.clone(),
                                expected: TokenType::CurlyLeft,
                                found: self.peek().typ,
                            });
                        }
                        while !self.match_tok(TokenType::CurlyRight) {
                            if self.peek().typ != TokenType::Fn {
                                return Err(ProgrammingLangParsingError::StructImplRegionExpect {
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
                                return Err(ProgrammingLangParsingError::FunctionAlreadyDefined {
                                    loc: func.0.location,
                                    name,
                                    first_func_loc: other_func
                                        .get_contract_unbaked()
                                        .location
                                        .clone(),
                                });
                            }
                            current_impl.insert(
                                name,
                                BakableFunction::Function(Box::new((func.0, *func.1))),
                            );
                        }
                        impls.push((trait_name, current_impl));
                    }
                    token @ _ => {
                        return Err(ProgrammingLangParsingError::StructImplRegionExpect {
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
        })
    }

    pub fn expect_identifier(&mut self) -> Result<GlobalStr, ProgrammingLangParsingError> {
        if !self.match_tok(TokenType::IdentifierLiteral) {
            return Err(ProgrammingLangParsingError::ExpectedIdentifier {
                loc: self.peek().location.clone(),
                found: self.peek().typ,
            });
        }
        let Some(Literal::String(ref str)) = self.previous().literal else {
            return Err(ProgrammingLangParsingError::InvalidTokenization {
                loc: self.previous().location.clone(),
            });
        };
        Ok(str.clone())
    }
    pub fn parse_annotation(&mut self) -> Result<(), ProgrammingLangParsingError> {
        let loc = self.peek().location.clone();
        assert_eq!(self.advance().typ, TokenType::AnnotationIntroducer);

        let name = match self.peek().typ {
            TokenType::If => GlobalStr::new("if"),
            TokenType::While => GlobalStr::new("while"),
            TokenType::For => GlobalStr::new("for"),
            TokenType::As => GlobalStr::new("as"),
            TokenType::Else => GlobalStr::new("else"),
            TokenType::Impl => GlobalStr::new("impl"),
            TokenType::Fn => GlobalStr::new("fn"),
            TokenType::In => GlobalStr::new("in"),
            TokenType::Struct => GlobalStr::new("struct"),
            TokenType::Trait => GlobalStr::new("trait"),

            TokenType::IdentifierLiteral => match &self.peek().literal {
                Some(Literal::String(v)) => v.clone(),
                _ => {
                    return Err(ProgrammingLangParsingError::InvalidTokenization {
                        loc: self.peek().location.clone(),
                    })
                }
            },

            _ => {
                return Err(ProgrammingLangParsingError::ExpectedIdentifier {
                    loc: self.peek().location.clone(),
                    found: self.peek().typ,
                })
            }
        };
        self.advance();

        if !self.match_tok(TokenType::ParenLeft) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location.clone(),
                expected: TokenType::ParenLeft,
                found: self.peek().typ,
            });
        }
        let mut deepness = 0;
        let mut args = vec![];

        loop {
            match self.peek().typ {
                TokenType::Eof => {
                    return Err(ProgrammingLangParsingError::ExpectedArbitrary {
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

        self.current_annotations
            .0
            .push(Annotation { args, loc, name });

        Ok(())
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

impl Parser {
    pub fn parse_external(&mut self) -> Result<Statement, ProgrammingLangParsingError> {
        let location = self.peek().location.clone();
        if !self.match_tok(TokenType::Extern) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location.clone(),
                expected: TokenType::Extern,
                found: self.peek().typ,
            });
        }
        if !self.match_tok(TokenType::Fn) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location.clone(),
                expected: TokenType::Fn,
                found: self.peek().typ,
            });
        }

        let name = {
            if !self.match_tok(TokenType::IdentifierLiteral) {
                return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                    loc: self.peek().location.clone(),
                    expected: TokenType::IdentifierLiteral,
                    found: self.peek().typ,
                });
            }

            let Some(Literal::String(ref name)) = self.previous().literal else {
                return Err(ProgrammingLangParsingError::InvalidTokenization {
                    loc: self.previous().location.clone(),
                });
            };

            name.clone()
        };

        let mut arguments = vec![];

        if !self.match_tok(TokenType::ParenLeft) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location.clone(),
                found: self.peek().typ,
                expected: TokenType::ParenLeft,
            });
        }

        while !self.match_tok(TokenType::ParenRight) {
            if arguments.len() > 0 {
                if !self.match_tok(TokenType::Comma) {
                    return Err(ProgrammingLangParsingError::ExpectedFunctionArgument {
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
            if !self.match_tok(TokenType::Colon) {
                return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                    loc: self.previous().location.clone(),
                    expected: TokenType::Colon,
                    found: self.previous().typ,
                });
            }

            arguments.push(Argument::new(TypeRef::parse(self)?, name));
        }

        let return_type = if self.match_tok(TokenType::ReturnType) {
            TypeRef::parse(self)?
        } else {
            TypeRef::Void(self.peek().location.clone())
        };

        if !self.match_tok(TokenType::Semicolon) {
            Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location.clone(),
                expected: TokenType::Semicolon,
                found: self.peek().typ,
            })
        } else {
            Ok(Statement::ExternalFunction(FunctionContract {
                arguments,
                name: Some(name),
                return_type,
                location,
                annotations: std::mem::take(&mut self.current_annotations),
                generics: vec![],
            }))
        }
    }

    pub fn parse_callable(
        &mut self,
        anonymous: bool,
    ) -> Result<(FunctionContract, Box<Statement>), ProgrammingLangParsingError> {
        let annotations = std::mem::take(&mut self.current_annotations);

        let location = self.peek().location.clone();
        if !self.match_tok(TokenType::Fn) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location.clone(),
                expected: TokenType::Fn,
                found: self.peek().typ,
            });
        }

        let name = if anonymous {
            if self.match_tok(TokenType::IdentifierLiteral) {
                let Some(Literal::String(ref name)) = self.previous().literal else {
                    return Err(ProgrammingLangParsingError::InvalidTokenization {
                        loc: self.previous().location.clone(),
                    });
                };
                Some(name.clone())
            } else {
                None
            }
        } else {
            if !self.match_tok(TokenType::IdentifierLiteral) {
                return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                    loc: self.peek().location.clone(),
                    expected: TokenType::IdentifierLiteral,
                    found: self.peek().typ,
                });
            }

            let Some(Literal::String(ref name)) = self.previous().literal else {
                return Err(ProgrammingLangParsingError::InvalidTokenization {
                    loc: self.previous().location.clone(),
                });
            };
            Some(name.clone())
        };

        let generics = if self.match_tok(TokenType::LessThan) {
            self.parse_function_generics()?
        } else {
            vec![]
        };

        let mut arguments = vec![];

        if !self.match_tok(TokenType::ParenLeft) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location.clone(),
                found: self.peek().typ,
                expected: TokenType::ParenLeft,
            });
        }

        while !self.match_tok(TokenType::ParenRight) {
            if arguments.len() > 0 {
                if !self.match_tok(TokenType::Comma) {
                    return Err(ProgrammingLangParsingError::ExpectedFunctionArgument {
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
            if !self.match_tok(TokenType::Colon) {
                return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                    loc: self.previous().location.clone(),
                    expected: TokenType::Colon,
                    found: self.previous().typ,
                });
            }

            arguments.push(Argument::new(TypeRef::parse(self)?, name));
        }

        let return_type = if self.match_tok(TokenType::ReturnType) {
            TypeRef::parse(self)?
        } else {
            TypeRef::Void(self.peek().location.clone())
        };

        // body:
        // - a block
        // - equal and then an expression
        let body = match self.peek().typ {
            TokenType::CurlyLeft => self.parse_block_stmt().map(Box::new)?,
            TokenType::Equal => {
                self.advance();
                let expr = self.parse_expression()?;
                let return_location = expr.loc().clone();
                if !anonymous {
                    // needs a semicolon if this isnt anonymous
                    self.consume_semicolon()?;
                }
                Box::new(Statement::Return(Some(expr), return_location))
            }
            _ => {
                return Err(ProgrammingLangParsingError::ExpectedFunctionBody {
                    loc: self.peek().location.clone(),
                    found: self.peek().typ,
                })
            }
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

    pub fn parse_function_generics(&mut self) -> Result<Vec<Generic>, ProgrammingLangParsingError> {
        let mut generics = vec![];

        while !self.match_tok(TokenType::GreaterThan) {
            if generics.len() > 0 {
                if !self.match_tok(TokenType::Comma) {
                    return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                        loc: self.peek().location.clone(),
                        expected: TokenType::Comma,
                        found: self.peek().typ,
                    });
                }

                // trailing comma
                if self.match_tok(TokenType::GreaterThan) {
                    break;
                }
            }

            let name = self.expect_identifier()?;

            if self.match_tok(TokenType::Colon) {
                // trait bounds
                let mut bounds = vec![];

                loop {
                    if matches!(self.peek().typ, TokenType::Comma | TokenType::GreaterThan) {
                        break;
                    }

                    // expect `+` between bounds
                    if bounds.len() > 0 && !self.match_tok(TokenType::Plus) {
                        return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                            loc: self.peek().location.clone(),
                            found: self.peek().typ,
                            expected: TokenType::Plus,
                        });
                    }

                    bounds.push(Path::parse(self)?);
                }
                generics.push(Generic { name, bounds });
            } else {
                generics.push(Generic {
                    name,
                    bounds: vec![],
                });
            }
        }

        Ok(generics)
    }
}
