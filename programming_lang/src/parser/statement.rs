use std::fmt::{Debug, Display, Write};

use crate::{
    error::ProgrammingLangParsingError,
    globals::GlobalString,
    module::{FunctionId, Module},
    tokenizer::{Literal, Location, TokenType},
};

use super::{types::TypeRef, Expression, LiteralValue, Parser};

#[derive(Clone, Debug)]
pub enum Statement {
    If {
        condition: Expression,
        if_stmt: Box<Statement>,
        else_stmt: Option<Box<Statement>>,
        location: Location,
    },
    While {
        condition: Expression,
        child: Box<Statement>,
        location: Location,
    },
    For {
        iterator: Expression,
        var_name: GlobalString,
        child: Box<Statement>,
        location: Location,
    },
    Return(Option<Expression>, Location),
    Block(Box<[Statement]>, Location),
    Const(GlobalString, Expression, Option<TypeRef>, Location),
    Var(GlobalString, Expression, Option<TypeRef>, Location),
    Expression(Expression),
    Function(FunctionContract, Box<Statement>),
    BakedFunction(FunctionId, Location),
    ExternalFunction(FunctionContract),
    Struct {
        name: GlobalString,
        elements: Vec<(GlobalString, TypeRef)>,
        location: Location,
    },
    Implementation {
        struct_name: GlobalString,
        functions: Vec<(FunctionContract, Box<Statement>)>,
        location: Location,
    },
}

impl Statement {
    pub fn loc(&self) -> Location {
        match self {
            Self::Expression(expr) => expr.loc(),
            Self::ExternalFunction(c) | Self::Function(c, _) => c.location,

            Self::BakedFunction(_, location)
            | Self::Block(_, location)
            | Self::Const(_, _, _, location)
            | Self::For { location, .. }
            | Self::If { location, .. }
            | Self::Implementation { location, .. }
            | Self::Return(_, location)
            | Self::Struct { location, .. }
            | Self::Var(_, _, _, location)
            | Self::While { location, .. } => *location,
        }
    }

    pub fn bake_functions(&mut self, module: &mut Module) {
        match self {
            Self::Implementation { .. } => unreachable!(),
            Self::BakedFunction(..)
            | Self::Struct { .. }
            | Self::ExternalFunction(..)
            | Self::Return(None, ..) => (),
            Self::Function(contract, statement) => {
                let location = contract.location;
                let id = module.push_fn(contract.clone(), *(statement.clone()));
                *self = Self::BakedFunction(id, location);
            }
            Self::Block(statements, ..) => statements
                .iter_mut()
                .for_each(|stmt| stmt.bake_functions(module)),
            Self::Const(_, stmt, ..) | Self::Var(_, stmt, ..) => stmt.bake_functions(module),
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

    pub fn mod_fmt(&self, f: &mut std::fmt::Formatter<'_>, module: &Module) -> std::fmt::Result {
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
            Self::Const(left_hand, right_hand, None, _) => {
                f.write_fmt(format_args!("(constant {left_hand} {right_hand})"))
            }
            Self::Const(left_hand, right_hand, Some(typ), _) => {
                f.write_fmt(format_args!("(constant {left_hand} {typ} {right_hand})"))
            }
            Self::Block(stmts, _) => {
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
            } => f.write_fmt(format_args!("(if {condition} {if_stmt} {else_stmt})")),
            Self::If {
                condition,
                if_stmt,
                else_stmt: None,
                location: _,
            } => f.write_fmt(format_args!("(if {condition} {if_stmt})")),
            Self::For {
                iterator,
                var_name,
                child,
                location: _,
            } => f.write_fmt(format_args!("(for {var_name} {iterator} {child})")),
            Self::While {
                condition,
                child,
                location: _,
            } => f.write_fmt(format_args!("(while {condition} {child})")),
            Self::Struct {
                name,
                elements: arguments,
                location: _,
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
            Self::Implementation {
                struct_name,
                functions,
                location: _,
            } => {
                f.write_str("impl ")?;
                Display::fmt(struct_name, f)?;
                f.write_str(" {\n")?;

                for (contract, body) in functions {
                    f.write_str("    ")?;
                    display_contract(f, contract, false)?;
                    Display::fmt(body, f)?;
                    f.write_str(")\n")?;
                }

                f.write_str("}")
            }
            Self::Function(contract, body) => {
                display_contract(f, contract, false)?;
                Display::fmt(body, f)?;
                f.write_char(')')
            }
            Self::ExternalFunction(contract) => display_contract(f, contract, true),
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
            Self::Const(left_hand, right_hand, None, _) => {
                f.write_fmt(format_args!("(constant {left_hand} {right_hand})"))
            }
            Self::Const(left_hand, right_hand, Some(typ), _) => {
                f.write_fmt(format_args!("(constant {left_hand} {typ} {right_hand})"))
            }
            Self::Block(stmts, _) => {
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
            } => f.write_fmt(format_args!("(if {condition} {if_stmt} {else_stmt})")),
            Self::If {
                condition,
                if_stmt,
                else_stmt: None,
                location: _,
            } => f.write_fmt(format_args!("(if {condition} {if_stmt})")),
            Self::For {
                iterator,
                var_name,
                child,
                location: _,
            } => f.write_fmt(format_args!("(for {var_name} {iterator} {child})")),
            Self::While {
                condition,
                child,
                location: _,
            } => f.write_fmt(format_args!("(while {condition} {child})")),
            Self::Struct {
                name,
                elements: arguments,
                location: _,
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
            Self::Implementation {
                struct_name,
                functions,
                location: _,
            } => {
                f.write_str("impl ")?;
                Display::fmt(struct_name, f)?;
                f.write_str(" {\n")?;

                for (contract, body) in functions {
                    f.write_str("    ")?;
                    display_contract(f, contract, false)?;
                    Display::fmt(body, f)?;
                    f.write_str(")\n")?;
                }

                f.write_str("}")
            }
            Self::Function(contract, body) => {
                display_contract(f, contract, false)?;
                Display::fmt(body, f)?;
                f.write_char(')')
            }
            Self::ExternalFunction(contract) => display_contract(f, contract, true),
        }
    }
}

pub fn display_contract(
    f: &mut std::fmt::Formatter<'_>,
    contract: &FunctionContract,
    is_external: bool,
) -> std::fmt::Result {
    if is_external {
        f.write_str("(external callable ")?;
    } else {
        f.write_str("(callable ")?;
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
    if let Some(return_type) = &contract.return_type {
        f.write_str(" returns ")?;
        Display::fmt(return_type, f)?;
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
                loc: self.peek().location,
                expected: TokenType::Semicolon,
                found: self.peek().typ,
            });
        }
        while self.match_tok(TokenType::Semicolon) {}
        Ok(())
    }

    pub fn is_statement(typ: TokenType) -> bool {
        matches!(
            typ,
            TokenType::Let
                | TokenType::Const
                | TokenType::CurlyLeft
                | TokenType::Return
                | TokenType::If
                | TokenType::While
                | TokenType::For
                | TokenType::Fn
        )
    }

    pub fn parse_all(&mut self) -> Result<Vec<Statement>, Vec<ProgrammingLangParsingError>> {
        let mut statements = vec![];
        let mut errors = vec![];

        while self.current < self.tokens.len() - 1 {
            match self.parse_statement(true) {
                Err(error) => {
                    errors.push(error);
                    self.bail();
                }
                Ok(statement) => statements.push(statement),
            }
        }

        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(statements)
        }
    }

    pub fn parse_statement(
        &mut self,
        is_global: bool,
    ) -> Result<Statement, ProgrammingLangParsingError> {
        macro_rules! invalid_kw {
            ($kw: literal) => {
                return Err(ProgrammingLangParsingError::InvalidKeyword {
                    keyword: $kw,
                    loc: self.peek().location,
                })
            };
        }

        while self.match_tok(TokenType::Semicolon) {}
        match self.peek().typ {
            TokenType::Impl if !is_global => invalid_kw!("impl block"),
            TokenType::Extern if !is_global => invalid_kw!("external value/function"),
            TokenType::Struct if !is_global => invalid_kw!("struct definition"),

            TokenType::Return if is_global => invalid_kw!("return"),
            TokenType::CurlyLeft if is_global => invalid_kw!("code block"),
            TokenType::If if is_global => invalid_kw!("if statement"),
            TokenType::While if is_global => invalid_kw!("while loop"),
            TokenType::For if is_global => invalid_kw!("for loop"),


            TokenType::Let => self.parse_let_stmt(),
            TokenType::Const => self.parse_const_stmt(),
            TokenType::CurlyLeft => self.parse_block_stmt(),
            TokenType::Return => self.parse_return_stmt(),
            TokenType::If => self.parse_if_stmt(),
            TokenType::While => self.parse_while_stmt(),
            TokenType::For => self.parse_for_stmt(),
            TokenType::Struct => self.parse_struct(),
            TokenType::Impl => self.parse_impl(),
            TokenType::Fn => self
                .parse_callable(false)
                .map(|(contract, body)| Statement::Function(contract, body)),
            TokenType::Extern => self.parse_external(),
            _ => self.parse_expression_stmt(),
        }
    }

    fn parse_expression_stmt(&mut self) -> Result<Statement, ProgrammingLangParsingError> {
        let expr = self.parse_expression()?;
        self.consume_semicolon()?;
        Ok(Statement::Expression(expr))
    }

    fn parse_let_stmt(&mut self) -> Result<Statement, ProgrammingLangParsingError> {
        // let <identifier>;
        // let <identifier> = <expr>;
        let location = self.advance().location; // skip `let`
        let name = GlobalString::from(&**self.expect_identifier()?);

        let typ = if self.match_tok(TokenType::AssignTypeOrStructValue) {
            Some(TypeRef::parse(self)?)
        } else {
            None
        };

        let expr = if self.match_tok(TokenType::AssignValue) {
            self.parse_expression()?
        } else {
            Expression::Literal(LiteralValue::Null, self.previous().location)
        };
        self.consume_semicolon()?;
        Ok(Statement::Var(name, expr, typ, location))
    }
    fn parse_const_stmt(&mut self) -> Result<Statement, ProgrammingLangParsingError> {
        // const <identifier> = <expr>;
        let location = self.advance().location; // skip `const`
        let name = GlobalString::from(&**self.expect_identifier()?);

        let typ = if self.match_tok(TokenType::AssignTypeOrStructValue) {
            Some(TypeRef::parse(self)?)
        } else {
            None
        };

        if !self.match_tok(TokenType::AssignValue) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location,
                expected: TokenType::AssignValue,
                found: self.peek().typ,
            });
        }
        let expr = self.parse_expression()?;
        self.consume_semicolon()?;
        Ok(Statement::Const(name, expr, typ, location))
    }
    fn parse_block_stmt(&mut self) -> Result<Statement, ProgrammingLangParsingError> {
        // { <...statements...> }
        let location = self.advance().location; // skip `{`
        let mut statements = vec![];

        while !self.match_tok(TokenType::CurlyRight) {
            statements.push(self.parse_statement(false)?);
        }

        Ok(Statement::Block(statements.into_boxed_slice(), location))
    }
    fn parse_return_stmt(&mut self) -> Result<Statement, ProgrammingLangParsingError> {
        // return;
        // return <expr>;
        let location = self.advance().location; // skip `return`
        if self.match_tok(TokenType::Semicolon) {
            return Ok(Statement::Return(None, location));
        }
        let expr = self.parse_expression()?;
        self.consume_semicolon()?;
        Ok(Statement::Return(Some(expr), location))
    }
    fn parse_if_stmt(&mut self) -> Result<Statement, ProgrammingLangParsingError> {
        // if (<expr>) <stmt>
        // if (<expr>) <stmt> else <stmt>
        // we have else if support, because else <stmt>, the stmt can be another if expression!
        let location = self.advance().location; // skip `if`
        if !self.match_tok(TokenType::ParenLeft) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location,
                found: self.peek().typ,
                expected: TokenType::ParenLeft,
            });
        }
        let condition = self.parse_expression()?;
        if !self.match_tok(TokenType::ParenRight) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location,
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
            });
        }
        Ok(Statement::If {
            condition,
            if_stmt: Box::new(if_stmt),
            else_stmt: None,
            location,
        })
    }
    fn parse_while_stmt(&mut self) -> Result<Statement, ProgrammingLangParsingError> {
        // if (<expr>) <stmt>
        // if (<expr>) <stmt> else <stmt>
        // we have else if support, because else <stmt>, the stmt can be another if expression!
        let location = self.advance().location; // skip `if`
        if !self.match_tok(TokenType::ParenLeft) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location,
                found: self.peek().typ,
                expected: TokenType::ParenLeft,
            });
        }
        let condition = self.parse_expression()?;
        if !self.match_tok(TokenType::ParenRight) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location,
                found: self.peek().typ,
                expected: TokenType::ParenRight,
            });
        }
        Ok(Statement::While {
            condition,
            child: Box::new(self.parse_statement(false)?),
            location,
        })
    }
    fn parse_for_stmt(&mut self) -> Result<Statement, ProgrammingLangParsingError> {
        // for (<identifier> in <expr>) <stmt>
        let location = self.advance().location; // skip over `for`

        if !self.match_tok(TokenType::ParenLeft) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location,
                found: self.peek().typ,
                expected: TokenType::ParenLeft,
            });
        }

        let var_name = GlobalString::from(&**self.expect_identifier()?);
        if !self.match_tok(TokenType::In) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location,
                expected: TokenType::In,
                found: self.peek().typ,
            });
        }
        let iterator = self.parse_expression()?;
        if !self.match_tok(TokenType::ParenRight) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location,
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
        })
    }
    fn parse_struct(&mut self) -> Result<Statement, ProgrammingLangParsingError> {
        // struct Name { ... fields ... }
        // fields: field: type,[...]
        let location = self.advance().location; // skip over `struct`
        let name = GlobalString::from(&**self.expect_identifier()?);
        let mut elements = vec![];

        if !self.match_tok(TokenType::CurlyLeft) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location,
                expected: TokenType::CurlyLeft,
                found: self.peek().typ,
            });
        }

        while !self.match_tok(TokenType::CurlyRight) {
            if elements.len() > 0 {
                // needs comma
                if !self.match_tok(TokenType::Comma) {
                    return Err(ProgrammingLangParsingError::ExpectedObjectElement {
                        loc: self.peek().location,
                        found: self.peek().typ,
                    });
                }
                // for trailing commas
                if self.match_tok(TokenType::CurlyRight) {
                    break;
                }
            }
            let name = GlobalString::from(&**self.expect_identifier()?);
            if !self.match_tok(TokenType::AssignTypeOrStructValue) {
                return Err(ProgrammingLangParsingError::ExpectedType {
                    loc: self.peek().location,
                    found: self.peek().typ,
                });
            }
            let typ = TypeRef::parse(self)?;
            elements.push((name, typ));
        }

        Ok(Statement::Struct {
            name,
            elements,
            location,
        })
    }
    fn parse_impl(&mut self) -> Result<Statement, ProgrammingLangParsingError> {
        // impl <name> { ... }
        // body: bunch of functions
        // skip over `impl`
        let location = self.advance().location;
        let struct_name = GlobalString::from(&**self.expect_identifier()?);

        if !self.match_tok(TokenType::CurlyLeft) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location,
                expected: TokenType::CurlyLeft,
                found: self.peek().typ,
            });
        }

        let mut functions = vec![];

        while !self.match_tok(TokenType::CurlyRight) {
            functions.push(self.parse_callable(false)?);
        }

        Ok(Statement::Implementation {
            struct_name,
            functions,
            location,
        })
    }

    pub fn expect_identifier(&mut self) -> Result<&Box<str>, ProgrammingLangParsingError> {
        if !self.match_tok(TokenType::IdentifierLiteral) {
            return Err(ProgrammingLangParsingError::ExpectedIdentifier {
                loc: self.peek().location,
                found: self.peek().typ,
            });
        }
        let Some(Literal::String(ref str)) = self.previous().literal else {
            return Err(ProgrammingLangParsingError::InvalidTokenization {
                loc: self.previous().location,
            });
        };
        Ok(str)
    }
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub name: Box<str>,
    pub typ: TypeRef,
}

impl Argument {
    pub fn new(typ: TypeRef, name: Box<str>) -> Self {
        Self { name, typ }
    }
}

impl Display for Argument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&*self.name)?;
        f.write_str(": ")?;
        Display::fmt(&self.typ, f)
    }
}

#[derive(Clone, Debug)]
pub struct FunctionContract {
    pub name: Option<GlobalString>,
    pub arguments: Vec<Argument>,
    pub return_type: Option<TypeRef>,
    pub location: Location,
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
        let location = self.peek().location;
        if !self.match_tok(TokenType::Extern) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location,
                expected: TokenType::Extern,
                found: self.peek().typ,
            });
        }
        if !self.match_tok(TokenType::Fn) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location,
                expected: TokenType::Fn,
                found: self.peek().typ,
            });
        }

        let name = {
            if !self.match_tok(TokenType::IdentifierLiteral) {
                return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                    loc: self.peek().location,
                    expected: TokenType::IdentifierLiteral,
                    found: self.peek().typ,
                });
            }

            let Some(Literal::String(ref name)) = self.previous().literal else {
                return Err(ProgrammingLangParsingError::InvalidTokenization {
                    loc: self.previous().location,
                });
            };

            GlobalString::from(&**name)
        };

        let mut arguments = vec![];

        if !self.match_tok(TokenType::ParenLeft) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location,
                found: self.peek().typ,
                expected: TokenType::ParenLeft,
            });
        }

        while !self.match_tok(TokenType::ParenRight) {
            if arguments.len() > 0 {
                if !self.match_tok(TokenType::Comma) {
                    return Err(ProgrammingLangParsingError::ExpectedFunctionArgument {
                        loc: self.peek().location,
                        found: self.peek().typ,
                    });
                }

                // for trailing comma
                if self.match_tok(TokenType::ParenRight) {
                    break;
                }
            }

            let name = self.expect_identifier()?.clone();
            if !self.match_tok(TokenType::AssignTypeOrStructValue) {
                return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                    loc: self.previous().location,
                    expected: TokenType::AssignTypeOrStructValue,
                    found: self.previous().typ,
                });
            }

            arguments.push(Argument::new(TypeRef::parse(self)?, name));
        }

        let return_type = if self.match_tok(TokenType::ReturnType) {
            Some(TypeRef::parse(self)?)
        } else {
            None
        };

        if !self.match_tok(TokenType::Semicolon) {
            Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location,
                expected: TokenType::Semicolon,
                found: self.peek().typ,
            })
        } else {
            Ok(Statement::ExternalFunction(FunctionContract {
                arguments,
                name: Some(name),
                return_type,
                location,
            }))
        }
    }

    pub fn parse_callable(
        &mut self,
        anonymous: bool,
    ) -> Result<(FunctionContract, Box<Statement>), ProgrammingLangParsingError> {
        let location = self.peek().location;
        if !self.match_tok(TokenType::Fn) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location,
                expected: TokenType::Fn,
                found: self.peek().typ,
            });
        }

        let name = if anonymous {
            if self.match_tok(TokenType::IdentifierLiteral) {
                let Some(Literal::String(ref name)) = self.previous().literal else {
                    return Err(ProgrammingLangParsingError::InvalidTokenization {
                        loc: self.previous().location,
                    });
                };
                Some(GlobalString::from(&**name))
            } else {
                None
            }
        } else {
            if !self.match_tok(TokenType::IdentifierLiteral) {
                return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                    loc: self.peek().location,
                    expected: TokenType::IdentifierLiteral,
                    found: self.peek().typ,
                });
            }

            let Some(Literal::String(ref name)) = self.previous().literal else {
                return Err(ProgrammingLangParsingError::InvalidTokenization {
                    loc: self.previous().location,
                });
            };
            Some(GlobalString::from(&**name))
        };

        let mut arguments = vec![];

        if !self.match_tok(TokenType::ParenLeft) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location,
                found: self.peek().typ,
                expected: TokenType::ParenLeft,
            });
        }

        while !self.match_tok(TokenType::ParenRight) {
            if arguments.len() > 0 {
                if !self.match_tok(TokenType::Comma) {
                    return Err(ProgrammingLangParsingError::ExpectedFunctionArgument {
                        loc: self.peek().location,
                        found: self.peek().typ,
                    });
                }

                // for trailing comma
                if self.match_tok(TokenType::ParenRight) {
                    break;
                }
            }

            let name = self.expect_identifier()?.clone();
            if !self.match_tok(TokenType::AssignTypeOrStructValue) {
                return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                    loc: self.previous().location,
                    expected: TokenType::AssignTypeOrStructValue,
                    found: self.previous().typ,
                });
            }

            arguments.push(Argument::new(TypeRef::parse(self)?, name));
        }

        let return_type = if self.match_tok(TokenType::ReturnType) {
            Some(TypeRef::parse(self)?)
        } else {
            None
        };

        // body:
        // - an expression
        // - a statement
        // examples: `
        let body = if Self::is_statement(self.peek().typ) {
            Box::new(self.parse_statement(false)?)
        } else {
            let expr = self.parse_expression()?;
            let return_location = expr.loc();
            if !anonymous {
                // needs a semicolon if this isnt anonymous
                self.consume_semicolon()?;
            }
            Box::new(Statement::Return(Some(expr), return_location))
        };

        Ok((
            FunctionContract {
                name,
                arguments,
                return_type,
                location,
            },
            body,
        ))
    }
}
