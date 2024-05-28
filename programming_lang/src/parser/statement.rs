use std::fmt::{Debug, Display, Write};

use crate::{
    error::ProgrammingLangParsingError,
    tokenizer::{Literal, TokenType},
};

use super::{Expression, LiteralValue, Parser};

#[derive(Clone)]
pub enum Statement {
    If {
        condition: Expression,
        if_stmt: Box<Statement>,
        else_stmt: Option<Box<Statement>>,
    },
    While {
        condition: Expression,
        child: Box<Statement>,
    },
    For {
        iterator: Expression,
        var_name: Box<str>,
        child: Box<Statement>,
    },
    Return(Option<Expression>),
    Block(Box<[Statement]>),
    Const(Box<str>, Expression),
    Var(Box<str>, Expression),
    Expression(Expression),
    Function(Callable),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Var(left_hand, right_hand) => {
                f.write_fmt(format_args!("(var-assign {left_hand} {right_hand})"))
            }
            Self::Const(left_hand, right_hand) => {
                f.write_fmt(format_args!("(constant {left_hand} {right_hand})"))
            }
            Self::Block(stmts) => {
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
            Self::Return(Some(v)) => f.write_fmt(format_args!("(return {v})")),
            Self::Return(None) => f.write_str("(return null)"),
            Self::If {
                condition,
                if_stmt,
                else_stmt: Some(else_stmt),
            } => f.write_fmt(format_args!("(if {condition} {if_stmt} {else_stmt})")),
            Self::If {
                condition,
                if_stmt,
                else_stmt: None,
            } => f.write_fmt(format_args!("(if {condition} {if_stmt})")),
            Self::For {
                iterator,
                var_name,
                child,
            } => f.write_fmt(format_args!("(for {var_name} {iterator} {child})")),
            Self::While { condition, child } => {
                f.write_fmt(format_args!("(while {condition} {child})"))
            }
            Self::Function(callable) => Display::fmt(callable, f),
        }
    }
}

impl Parser {
    fn consume_semicolon(&mut self) -> Result<(), ProgrammingLangParsingError> {
        if !self.matches(&[TokenType::Semicolon]) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location,
                expected: TokenType::Semicolon,
                found: self.peek().typ,
            });
        }
        while self.matches(&[TokenType::Semicolon]) {}
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

    pub fn parse_statement(&mut self) -> Result<Statement, ProgrammingLangParsingError> {
        while self.matches(&[TokenType::Semicolon]) {}
        match self.peek().typ {
            TokenType::Let => self.parse_let_stmt(),
            TokenType::Const => self.parse_const_stmt(),
            TokenType::CurlyLeft => self.parse_block_stmt(),
            TokenType::Return => self.parse_return_stmt(),
            TokenType::If => self.parse_if_stmt(),
            TokenType::While => self.parse_while_stmt(),
            TokenType::For => self.parse_for_stmt(),
            TokenType::Fn => Ok(Statement::Function(self.parse_callable(false)?)),
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
        self.advance(); // skip `let`
        let name = self.expect_identifier()?.clone();
        let expr = if self.matches(&[TokenType::AssignValue]) {
            self.parse_expression()?
        } else {
            Expression::Literal(LiteralValue::Null, self.previous().location)
        };
        self.consume_semicolon()?;
        Ok(Statement::Var(name, expr))
    }
    fn parse_const_stmt(&mut self) -> Result<Statement, ProgrammingLangParsingError> {
        // const <identifier> = <expr>;
        self.advance(); // skip `const`
        let name = self.expect_identifier()?.clone();
        if !self.matches(&[TokenType::AssignValue]) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location,
                expected: TokenType::AssignValue,
                found: self.peek().typ,
            });
        }
        let expr = self.parse_expression()?;
        self.consume_semicolon()?;
        Ok(Statement::Const(name, expr))
    }
    fn parse_block_stmt(&mut self) -> Result<Statement, ProgrammingLangParsingError> {
        // { <...statements...> }
        self.advance(); // skip `{`
        let mut statements = vec![];

        while !self.matches(&[TokenType::CurlyRight]) {
            statements.push(self.parse_statement()?);
        }

        Ok(Statement::Block(statements.into_boxed_slice()))
    }
    fn parse_return_stmt(&mut self) -> Result<Statement, ProgrammingLangParsingError> {
        // return;
        // return <expr>;
        self.advance(); // skip `return`
        if self.matches(&[TokenType::Semicolon]) {
            return Ok(Statement::Return(None));
        }
        let expr = self.parse_expression()?;
        self.consume_semicolon()?;
        Ok(Statement::Return(Some(expr)))
    }
    fn parse_if_stmt(&mut self) -> Result<Statement, ProgrammingLangParsingError> {
        // if (<expr>) <stmt>
        // if (<expr>) <stmt> else <stmt>
        // we have else if support, because else <stmt>, the stmt can be another if expression!
        self.advance(); // skip `if`
        if !self.matches(&[TokenType::ParenLeft]) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location,
                found: self.peek().typ,
                expected: TokenType::ParenLeft,
            });
        }
        let condition = self.parse_expression()?;
        if !self.matches(&[TokenType::ParenRight]) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location,
                found: self.peek().typ,
                expected: TokenType::ParenRight,
            });
        }
        let if_stmt = self.parse_statement()?;
        if self.matches(&[TokenType::Else]) {
            return Ok(Statement::If {
                condition,
                if_stmt: Box::new(if_stmt),
                else_stmt: Some(Box::new(self.parse_statement()?)),
            });
        }
        Ok(Statement::If {
            condition,
            if_stmt: Box::new(if_stmt),
            else_stmt: None,
        })
    }

    fn parse_while_stmt(&mut self) -> Result<Statement, ProgrammingLangParsingError> {
        // if (<expr>) <stmt>
        // if (<expr>) <stmt> else <stmt>
        // we have else if support, because else <stmt>, the stmt can be another if expression!
        self.advance(); // skip `if`
        if !self.matches(&[TokenType::ParenLeft]) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location,
                found: self.peek().typ,
                expected: TokenType::ParenLeft,
            });
        }
        let condition = self.parse_expression()?;
        if !self.matches(&[TokenType::ParenRight]) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location,
                found: self.peek().typ,
                expected: TokenType::ParenRight,
            });
        }
        Ok(Statement::While {
            condition,
            child: Box::new(self.parse_statement()?),
        })
    }

    fn parse_for_stmt(&mut self) -> Result<Statement, ProgrammingLangParsingError> {
        // for (<identifier> in <expr>) <stmt>
        self.advance(); // skip over `for`

        if !self.matches(&[TokenType::ParenLeft]) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location,
                found: self.peek().typ,
                expected: TokenType::ParenLeft,
            });
        }

        let var_name = self.expect_identifier()?.clone();
        if !self.matches(&[TokenType::In]) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location,
                expected: TokenType::In,
                found: self.peek().typ,
            });
        }
        let iterator = self.parse_expression()?;
        if !self.matches(&[TokenType::ParenRight]) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location,
                found: self.peek().typ,
                expected: TokenType::ParenRight,
            });
        }

        let child = Box::new(self.parse_statement()?);
        Ok(Statement::For {
            iterator,
            var_name,
            child,
        })
    }

    fn expect_identifier(&mut self) -> Result<&Box<str>, ProgrammingLangParsingError> {
        if !self.matches(&[TokenType::IdentifierLiteral]) {
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

impl Debug for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let body = format!("{}", self.body);

        f.debug_struct("Callable")
            .field("name", &self.name)
            .field("arguments", &self.arguments)
            .field("last_one_spreads", &self.last_one_spreads)
            .field("body", &body)
            .finish()
    }
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub copy: bool,
    pub name: Box<str>,
}

impl Argument {
    pub fn new(copy: bool, name: Box<str>) -> Self { Self { copy, name } }
}

impl Display for Argument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.copy {
            f.write_str("copy ")?;
        }
        f.write_str(&*self.name)
    }
}

#[derive(Clone)]
pub struct Callable {
    pub name: Option<Box<str>>,
    pub arguments: Vec<Argument>,
    pub last_one_spreads: bool,
    pub body: Box<Statement>,
}

impl Display for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("(callable ")?;
        if let Some(ref name) = self.name {
            f.write_str(&**name)?;
            f.write_char(' ')?;
        }

        f.write_char('(')?;

        for i in 0..self.arguments.len() {
            if i != 0 {
                f.write_str(", ")?;
            }
            if i == self.arguments.len() - 1 && self.last_one_spreads {
                f.write_str("...")?;
            }
            Display::fmt(&self.arguments[i], f)?;
        }

        f.write_str(") ")?;

        Display::fmt(&*self.body, f)?;

        f.write_char(')')
    }
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
    pub fn parse_callable(
        &mut self,
        anonymous: bool,
    ) -> Result<Callable, ProgrammingLangParsingError> {
        if !self.matches(&[TokenType::Fn]) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location,
                expected: TokenType::Fn,
                found: self.peek().typ,
            });
        }

        let name = if anonymous {
            if self.matches(&[TokenType::IdentifierLiteral]) {
                let Some(Literal::String(ref v)) = self.previous().literal else {
                    return Err(ProgrammingLangParsingError::InvalidTokenization {
                        loc: self.previous().location,
                    });
                };
                Some(v.clone())
            } else {
                None
            }
        } else {
            if !self.matches(&[TokenType::IdentifierLiteral]) {
                return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                    loc: self.peek().location,
                    expected: TokenType::IdentifierLiteral,
                    found: self.peek().typ,
                });
            }

            let Some(Literal::String(ref v)) = self.previous().literal else {
                return Err(ProgrammingLangParsingError::InvalidTokenization {
                    loc: self.previous().location,
                });
            };
            Some(v.clone())
        };

        let mut arguments = vec![];
        let mut last_one_spreads = false;

        // parse arguments

        if !self.matches(&[TokenType::ParenLeft]) {
            return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                loc: self.peek().location,
                found: self.peek().typ,
                expected: TokenType::ParenLeft,
            });
        }

        while !self.matches(&[TokenType::ParenRight]) {
            if arguments.len() > 0 {
                if !self.matches(&[TokenType::Comma]) {
                    return Err(ProgrammingLangParsingError::ExpectedFunctionArgument {
                        loc: self.peek().location,
                        found: self.peek().typ,
                    });
                }

                // for trailing comma
                if self.matches(&[TokenType::ParenRight]) {
                    break;
                }
            }

            if self.matches(&[TokenType::Spread]) {
                // last argument
                let copy = self.matches(&[TokenType::Copy]);
                arguments.push(Argument::new(copy, self.expect_identifier()?.clone()));
                last_one_spreads = true;

                // consume optional comma
                self.matches(&[TokenType::Comma]);

                if !self.matches(&[TokenType::ParenRight]) {
                    return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                        loc: self.peek().location,
                        found: self.peek().typ,
                        expected: TokenType::ParenRight,
                    });
                }
                break;
            }

            let copy = self.matches(&[TokenType::Copy]);
            arguments.push(Argument::new(copy, self.expect_identifier()?.clone()));
        }

        // body:
        // - an expression
        // - a statement
        // examples: `
        let body = if Self::is_statement(self.peek().typ) {
            Box::new(self.parse_statement()?)
        } else {
            let expr = self.parse_expression()?;
            if !anonymous {
                // needs a semicolon if this isnt anonymous
                self.consume_semicolon()?;
            }
            Box::new(Statement::Return(Some(expr)))
        };

        Ok(Callable {
            name,
            arguments,
            last_one_spreads,
            body,
        })
    }
}
