use std::{
    collections::HashMap,
    fmt::{Debug, Display, Write},
};

use crate::{
    annotations::AnnotationReceiver,
    error::ParsingError,
    module::{Function, Module, ModuleContext},
};
use mira_common::store::StoreKey;
use mira_lexer::{Literal, NumberType, Token, TokenType};
use mira_spans::{
    Ident, Span,
    interner::{SpanInterner, Symbol},
};

use super::{
    Parser, Statement,
    statement::{FunctionContract, display_contract},
    types::TypeRef,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Path<'arena> {
    pub entries: Vec<(Ident<'arena>, Vec<TypeRef<'arena>>)>,
    pub span: Span<'arena>,
}

impl Display for Path<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in 0..self.entries.len() {
            if i != 0 {
                f.write_str("::")?;
            }

            Display::fmt(&self.entries[i].0, f)?;
            if !self.entries[i].1.is_empty() {
                f.write_char('<')?;
                for type_idx in 0..self.entries[i].1.len() {
                    if type_idx != 0 {
                        f.write_str(", ")?;
                    }
                    Display::fmt(&self.entries[i].1[type_idx], f)?;
                }
                f.write_char('>')?;
            }
        }
        Ok(())
    }
}

impl<'arena> Path<'arena> {
    pub fn push(&mut self, name: Ident<'arena>, generics: Vec<TypeRef<'arena>>) {
        self.entries.push((name, generics));
    }
    pub fn pop(&mut self) -> Option<(Ident<'arena>, Vec<TypeRef<'arena>>)> {
        // ensure this is at least 1 element
        if self.entries.len() > 1 {
            self.entries.pop()
        } else {
            None
        }
    }
    pub fn new(entry: Ident<'arena>, generics: Vec<TypeRef<'arena>>) -> Self {
        Self {
            entries: vec![(entry, generics)],
            span: entry.span(),
        }
    }

    fn parse_generics(
        parser: &mut Parser<'_, 'arena>,
    ) -> Result<Vec<TypeRef<'arena>>, ParsingError<'arena>> {
        let mut types = vec![];

        while !parser.match_tok(TokenType::GreaterThan) {
            if !types.is_empty() {
                parser.expect(TokenType::Comma)?;
            }

            types.push(TypeRef::parse(parser)?);
        }
        Ok(types)
    }
    pub fn readjust_self_span(&mut self, span_interner: &SpanInterner<'arena>) {
        self.span = self.entries[0]
            .0
            .span()
            .combine_with(self.entries[1..].iter().map(|v| v.0.span()), span_interner);
    }

    pub fn parse<'a>(parser: &mut Parser<'a, 'arena>) -> Result<Self, ParsingError<'arena>> {
        let name = parser.expect_identifier()?;
        let generics = if parser.match_tok(TokenType::LessThan) {
            Self::parse_generics(parser)?
        } else {
            Default::default()
        };

        let mut path = Self::new(name, generics);

        while parser.match_tok(TokenType::NamespaceAccess) {
            let subpath = parser.expect_identifier()?;
            let generics = if parser.match_tok(TokenType::LessThan) {
                Self::parse_generics(parser)?
            } else {
                Default::default()
            };

            path.push(subpath, generics);
        }
        path.readjust_self_span(parser.ctx.span_interner);

        Ok(path)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PathWithoutGenerics<'arena> {
    pub entries: Vec<Ident<'arena>>,
    pub span: Span<'arena>,
}

impl<'arena> PathWithoutGenerics<'arena> {
    pub fn push(&mut self, name: Ident<'arena>) {
        self.entries.push(name);
    }
    pub fn pop(&mut self) -> Option<Ident<'arena>> {
        // ensure this is at least 1 element
        if self.entries.len() > 1 {
            self.entries.pop()
        } else {
            None
        }
    }
    pub fn new(entry: Ident<'arena>) -> Self {
        Self {
            span: entry.span(),
            entries: vec![entry],
        }
    }
    pub fn readjust_self_span(&mut self, span_interner: &SpanInterner<'arena>) {
        self.span = self.entries[0]
            .span()
            .combine_with(self.entries[1..].iter().map(Ident::span), span_interner);
    }
    pub fn parse(parser: &mut Parser<'_, 'arena>) -> Result<Self, ParsingError<'arena>> {
        let mut path = Self::new(parser.expect_identifier()?);

        while parser.match_tok(TokenType::NamespaceAccess) {
            let name = parser.expect_identifier()?;
            path.push(name);
        }

        path.readjust_self_span(parser.ctx.span_interner);
        Ok(path)
    }

    pub fn as_slice(&self) -> &[Ident<'arena>] {
        &self.entries
    }
}

impl Display for PathWithoutGenerics<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in 0..self.entries.len() {
            if i != 0 {
                f.write_str("::")?;
            }
            Display::fmt(&self.entries[i], f)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum ArrayLiteral<'arena> {
    Values(Vec<Expression<'arena>>),
    CopyInitialized(Box<Expression<'arena>>, usize),
}

#[derive(Debug, Clone)]
pub enum LiteralValue<'arena> {
    String(Symbol<'arena>),
    Array(ArrayLiteral<'arena>),
    Struct(
        HashMap<Ident<'arena>, (Span<'arena>, Expression<'arena>)>,
        Path<'arena>,
    ),
    AnonymousStruct(HashMap<Ident<'arena>, (Span<'arena>, Expression<'arena>)>),
    Tuple(Vec<(Span<'arena>, Expression<'arena>)>),
    Float(f64, NumberType),
    SInt(i64, NumberType),
    UInt(u64, NumberType),
    Bool(bool),
    Dynamic(Path<'arena>),
    AnonymousFunction(FunctionContract<'arena>, Box<Statement<'arena>>),
    BakedAnonymousFunction(StoreKey<Function<'arena>>),
    Void,
}

impl Display for LiteralValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralValue::BakedAnonymousFunction(id) => {
                f.write_fmt(format_args!("(module-fn {id})"))
            }
            LiteralValue::Bool(b) => f.write_str(if *b { "true" } else { "false" }),
            LiteralValue::Dynamic(d) => Display::fmt(d, f),
            LiteralValue::UInt(v, typ) => f.write_fmt(format_args!("{}{}", *v, *typ)),
            LiteralValue::SInt(v, typ) => f.write_fmt(format_args!("{}{}", *v, *typ)),
            LiteralValue::Float(v, typ) => f.write_fmt(format_args!("{}{}", *v, *typ)),
            LiteralValue::String(v) => Debug::fmt(v, f),
            LiteralValue::Array(v) => {
                f.write_char('[')?;
                match v {
                    ArrayLiteral::Values(values) => {
                        for (i, v) in values.iter().enumerate() {
                            if i != 0 {
                                f.write_str(", ")?;
                            }
                            Display::fmt(v, f)?;
                        }
                    }
                    ArrayLiteral::CopyInitialized(expression, amount) => {
                        Display::fmt(expression, f)?;
                        f.write_str("; ")?;
                        Display::fmt(amount, f)?;
                    }
                }
                f.write_char(']')
            }
            LiteralValue::Struct(v, name) => {
                Display::fmt(name, f)?;
                f.write_str(" {")?;

                for (k, (_, v)) in v {
                    f.write_char(' ')?;
                    Display::fmt(k, f)?;
                    f.write_str(": ")?;
                    Display::fmt(v, f)?;
                    f.write_char(',')?;
                }

                f.write_str(" }")
            }
            LiteralValue::AnonymousStruct(v) => {
                f.write_str(".{")?;

                for (k, (_, v)) in v {
                    f.write_char(' ')?;
                    Display::fmt(k, f)?;
                    f.write_str(": ")?;
                    Display::fmt(v, f)?;
                    f.write_char(',')?;
                }

                f.write_str(" }")
            }
            LiteralValue::Tuple(v) => {
                f.write_str(".(")?;

                for (i, v) in v.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?
                    }
                    Display::fmt(&v.1, f)?;
                }

                f.write_char(')')
            }
            LiteralValue::AnonymousFunction(v, body) => {
                display_contract(f, v, false)?;
                Display::fmt(body, f)?;
                f.write_char(')')
            }
            LiteralValue::Void => f.write_str("void"),
        }
    }
}

impl<'arena> LiteralValue<'arena> {
    // NOTE: **ONLY** FOR ERROR MESSAGES!!!!!
    pub fn type_name(&self) -> &'static str {
        match self {
            Self::Bool(..) => "boolean",
            LiteralValue::Dynamic(..) => "{unknown}",
            LiteralValue::Float(..) | LiteralValue::SInt(..) | LiteralValue::UInt(..) => "number",
            LiteralValue::String(..) => "string",
            LiteralValue::Array(..) => "array",
            LiteralValue::Struct(..) => "struct",
            LiteralValue::AnonymousStruct(..) => "anonymous struct",
            LiteralValue::Tuple(..) => "tuple",
            LiteralValue::Void => "void",
            LiteralValue::AnonymousFunction(..) | Self::BakedAnonymousFunction(..) => "function",
        }
    }

    fn from_token(value: Token<'arena>) -> Option<Self> {
        match value.typ {
            TokenType::StringLiteral
            | TokenType::BooleanLiteral
            | TokenType::FloatLiteral
            | TokenType::UIntLiteral
            | TokenType::SIntLiteral => value.literal.as_ref().map(|v| match v {
                Literal::Bool(boolean) => LiteralValue::Bool(*boolean),
                Literal::Float(float, typ) => LiteralValue::Float(*float, *typ),
                Literal::SInt(int, typ) => LiteralValue::SInt(*int, *typ),
                Literal::UInt(uint, typ) => LiteralValue::UInt(*uint, *typ),
                Literal::String(string) => LiteralValue::String(*string),
            }),
            TokenType::VoidLiteral => Some(LiteralValue::Void),
            TokenType::IdentifierLiteral => match value.literal {
                Some(Literal::String(ref v)) => Some(LiteralValue::Dynamic(crate::Path::new(
                    Ident::new(*v, value.span),
                    Vec::new(),
                ))),
                _ => None,
            },
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Plus,
    Minus,
    Divide,
    Multiply,
    Modulo,
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    LShift,
    RShift,
    LogicalAnd,
    LogicalOr,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Equals,
    NotEquals,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Plus,
    Minus,
    BitwiseNot,
    LogicalNot,
    Reference,
    Dereference,
}

#[derive(Debug, Clone)]
pub enum Expression<'arena> {
    Literal(LiteralValue<'arena>, Span<'arena>),
    Unary {
        operator: UnaryOp,
        span: Span<'arena>,
        right_side: Box<Expression<'arena>>,
    },
    Binary {
        operator: BinaryOp,
        span: Span<'arena>,
        right_side: Box<Expression<'arena>>,
        left_side: Box<Expression<'arena>>,
    },
    FunctionCall {
        identifier: Box<Expression<'arena>>,
        arguments: Vec<Expression<'arena>>,
        span: Span<'arena>,
    },
    MemberCall {
        identifier: Ident<'arena>,
        lhs: Box<Expression<'arena>>,
        arguments: Vec<Expression<'arena>>,
        span: Span<'arena>,
    },
    Indexing {
        left_side: Box<Expression<'arena>>,
        right_side: Box<Expression<'arena>>,
        span: Span<'arena>,
    },
    MemberAccess {
        left_side: Box<Expression<'arena>>,
        index: Vec<Ident<'arena>>,
        span: Span<'arena>,
    },
    Assignment {
        left_side: Box<Expression<'arena>>,
        right_side: Box<Expression<'arena>>,
        span: Span<'arena>,
    },
    Range {
        left_side: Box<Expression<'arena>>,
        right_side: Box<Expression<'arena>>,
        inclusive: bool,
        span: Span<'arena>,
    },
    TypeCast {
        left_side: Box<Expression<'arena>>,
        new_type: TypeRef<'arena>,
        span: Span<'arena>,
    },
    Asm {
        span: Span<'arena>,
        asm: String,
        volatile: bool,
        output: TypeRef<'arena>,
        registers: String, // input + output + clobber
        inputs: Vec<(Span<'arena>, Ident<'arena>)>,
    },
}

impl Display for Expression<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Literal(v, ..) => Display::fmt(v, f),
            Expression::Binary {
                operator,
                right_side,
                left_side,
                ..
            } => {
                f.write_char('(')?;
                operator.fmt(f)?;
                f.write_char(' ')?;
                Display::fmt(left_side, f)?;
                f.write_char(' ')?;
                Display::fmt(right_side, f)?;
                f.write_char(')')
            }
            Expression::Unary {
                operator,
                right_side,
                ..
            } => {
                f.write_char('(')?;
                operator.fmt(f)?;
                f.write_char(' ')?;
                Display::fmt(right_side, f)?;
                f.write_char(')')
            }
            Expression::FunctionCall {
                identifier,
                arguments,
                ..
            } => {
                f.write_str("(call ")?;
                Display::fmt(identifier, f)?;
                for v in arguments.iter() {
                    f.write_char(' ')?;
                    Display::fmt(v, f)?;
                }
                f.write_char(')')
            }
            Expression::Indexing {
                left_side,
                right_side,
                ..
            } => {
                f.write_str("(index ")?;
                Display::fmt(left_side, f)?;
                f.write_char(' ')?;
                Display::fmt(right_side, f)?;
                f.write_char(')')
            }
            Expression::MemberAccess {
                left_side, index, ..
            } => {
                f.write_str("(member ")?;
                Display::fmt(left_side, f)?;
                for value in index {
                    f.write_char(' ')?;
                    Display::fmt(value, f)?;
                }

                f.write_char(')')
            }
            Expression::Assignment {
                left_side,
                right_side,
                ..
            } => {
                f.write_str("(assignment ")?;
                Display::fmt(left_side, f)?;
                f.write_char(' ')?;
                Display::fmt(right_side, f)?;
                f.write_char(')')
            }
            Expression::Range {
                left_side,
                right_side,
                inclusive,
                span: _,
            } => {
                f.write_str("(range ")?;
                if *inclusive {
                    f.write_str("inclusive ")?;
                }
                Display::fmt(left_side, f)?;
                f.write_char(' ')?;
                Display::fmt(right_side, f)?;
                f.write_char(')')
            }
            Expression::TypeCast {
                left_side,
                new_type,
                span: _,
            } => {
                f.write_str("(type-cast ")?;
                Display::fmt(left_side, f)?;
                f.write_str(" -> ")?;
                Display::fmt(new_type, f)?;
                f.write_char(')')
            }
            Expression::MemberCall {
                identifier,
                lhs,
                arguments,
                ..
            } => {
                f.write_str("(member-call ")?;
                Display::fmt(lhs, f)?;
                f.write_char(' ')?;
                Display::fmt(identifier, f)?;
                for arg in arguments {
                    f.write_char(' ')?;
                    Display::fmt(arg, f)?;
                }
                f.write_char(')')
            }
            Self::Asm {
                asm,
                volatile,
                output,
                registers,
                inputs,
                ..
            } => {
                f.write_str("asm ")?;
                if *volatile {
                    f.write_str("volatile ")?;
                }
                f.write_char('(')?;
                for asm in asm.split('\n') {
                    Debug::fmt(asm, f)?;
                    f.write_char('\n')?;
                }
                f.write_str(": ")?;
                Display::fmt(output, f)?;
                f.write_str("\n: ")?;
                for (i, v) in inputs.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }
                    Display::fmt(&v.1, f)?;
                }
                f.write_str("\n: ")?;
                Debug::fmt(registers, f)?;
                f.write_str(");")
            }
        }
    }
}

impl<'arena> Expression<'arena> {
    pub fn bool(value: bool, loc: Span<'arena>) -> Self {
        Self::Literal(LiteralValue::Bool(value), loc)
    }

    pub fn string(value: Symbol<'arena>, span: Span<'arena>) -> Self {
        Self::Literal(LiteralValue::String(value), span)
    }

    pub fn unary(operator: UnaryOp, span: Span<'arena>, right: Expression<'arena>) -> Self {
        Self::Unary {
            operator,
            span,
            right_side: Box::new(right),
        }
    }

    pub fn binary(
        operator: BinaryOp,
        span: Span<'arena>,
        left: Expression<'arena>,
        right: Expression<'arena>,
    ) -> Self {
        Self::Binary {
            operator,
            span,
            right_side: Box::new(right),
            left_side: Box::new(left),
        }
    }

    pub fn span(&self) -> Span<'arena> {
        match self {
            Self::FunctionCall { span, .. }
            | Self::MemberAccess { span, .. }
            | Self::MemberCall { span, .. }
            | Self::Indexing { span, .. }
            | Self::Literal(_, span)
            | Self::Unary { span, .. }
            | Self::Asm { span, .. }
            | Self::Binary { span, .. }
            | Self::Assignment { span, .. }
            | Self::Range { span, .. }
            | Self::TypeCast { span, .. } => *span,
        }
    }

    pub fn bake_functions(
        &mut self,
        module: &mut Module<'arena>,
        module_key: StoreKey<Module<'arena>>,
        context: &ModuleContext<'arena>,
    ) {
        match self {
            Self::Asm { .. } => (),
            Self::Literal(val, ..) => {
                if let LiteralValue::AnonymousFunction(contract, statements) = val {
                    let id = module.push_fn(
                        FunctionContract {
                            name: contract.name.take(),
                            arguments: std::mem::take(&mut contract.arguments),
                            return_type: contract.return_type.clone(),
                            annotations: std::mem::take(&mut contract.annotations),
                            span: contract.span,
                            generics: std::mem::take(&mut contract.generics),
                        },
                        std::mem::replace(
                            &mut **statements,
                            Statement::BakedTrait(StoreKey::undefined(), contract.span),
                        ),
                        module_key,
                        context,
                    );
                    *val = LiteralValue::BakedAnonymousFunction(id)
                }
            }
            Self::Binary {
                right_side,
                left_side,
                ..
            }
            | Self::Assignment {
                left_side,
                right_side,
                ..
            }
            | Self::Indexing {
                left_side,
                right_side,
                ..
            }
            | Self::Range {
                left_side,
                right_side,
                ..
            } => {
                left_side.bake_functions(module, module_key, context);
                right_side.bake_functions(module, module_key, context);
            }
            Self::FunctionCall {
                identifier,
                arguments,
                ..
            } => {
                identifier.bake_functions(module, module_key, context);
                arguments
                    .iter_mut()
                    .for_each(|el| el.bake_functions(module, module_key, context));
            }
            Self::MemberCall { lhs, arguments, .. } => {
                lhs.bake_functions(module, module_key, context);
                arguments
                    .iter_mut()
                    .for_each(|el| el.bake_functions(module, module_key, context));
            }
            Self::MemberAccess { left_side, .. }
            | Self::TypeCast { left_side, .. }
            | Self::Unary {
                right_side: left_side,
                ..
            } => left_side.bake_functions(module, module_key, context),
        }
    }
}

macro_rules! assign_set {
    ($expr: expr, $right: expr, $operator: expr, $span: expr) => {
        let span = $span;
        $expr = Expression::Assignment {
            left_side: Box::new($expr.clone()),
            right_side: Box::new(Expression::binary($operator, span, $expr, $right)),
            span,
        }
    };
}

struct AsmBinding<'arena> {
    name: Symbol<'arena>,
    bound: Symbol<'arena>,
    type_or_name: Ident<'arena>,
}

// asm expression
impl<'arena> Parser<'_, 'arena> {
    /// Parses [<.0>] "<.1>" (<.2>)
    fn parse_asm_binding(&mut self) -> Result<AsmBinding<'arena>, ParsingError<'arena>> {
        self.expect(TokenType::BracketLeft)?;
        let name = self.expect_identifier()?.symbol();
        self.expect(TokenType::BracketRight)?;
        let bound = self.expect(TokenType::StringLiteral)?.string_literal();
        self.expect(TokenType::ParenLeft)?;
        let type_ = self.expect_identifier()?;
        self.expect(TokenType::ParenRight)?;
        Ok(AsmBinding {
            name,
            bound,
            type_or_name: type_,
        })
    }

    fn parse_asm(&mut self) -> Result<Expression<'arena>, ParsingError<'arena>> {
        let span1 = self.eat().span;
        let volatile = self.match_tok(TokenType::Volatile);
        self.expect(TokenType::ParenLeft)?;

        let mut replacers: Vec<(String, String)> = Vec::new();
        let mut registers = String::new();
        let mut inputs = Vec::new();
        let mut output = TypeRef::Void(span1, 0);
        let mut asm = String::new();
        while !self.matches(&[TokenType::ParenRight, TokenType::Colon]) {
            if !asm.is_empty() {
                self.expect(TokenType::Comma)?;
                if self.matches(&[TokenType::ParenRight, TokenType::Colon]) {
                    break;
                }
            }
            let s = self.expect(TokenType::StringLiteral)?.string_literal();
            if !asm.is_empty() {
                asm.push('\n');
            }
            asm.push_str(&s);
        }

        let span2 = 'out: {
            if self.current().typ != TokenType::Colon {
                break 'out self.last().span;
            }

            // outputs
            'outputs: {
                if self.peek().typ == TokenType::Colon {
                    break 'outputs;
                }
                // [v] "=r" (ty)
                let loc = self.peek().span;
                // let (replacer, register, type_name, span) = self.parse_asm_binding()?;
                let asm_binding = self.parse_asm_binding()?;
                let replacer_string = format!("%[{}]", asm_binding.name);
                if replacers.iter().any(|v| v.0 == replacer_string) {
                    return Err(ParsingError::DuplicateAsmReplacer {
                        loc,
                        name: asm_binding.name,
                    });
                }
                if !asm_binding.bound.starts_with('=') {
                    return Err(ParsingError::OutputNotStartingWithEqual {
                        loc,
                        output: asm_binding.bound,
                    });
                }
                replacers.push((replacer_string, format!("${{{}}}", replacers.len())));
                registers.push_str(&asm_binding.bound);
                output = TypeRef::Reference {
                    num_references: 0,
                    type_name: Path::new(asm_binding.type_or_name, Vec::new()),
                    span: loc,
                }
            };

            if !self.match_tok(TokenType::Colon) {
                break 'out self.expect(TokenType::ParenRight)?.span;
            }

            // inputs
            while !self.matches(&[TokenType::ParenRight, TokenType::Colon]) {
                if !inputs.is_empty() {
                    self.expect(TokenType::Comma)?;
                    if self.matches(&[TokenType::ParenRight, TokenType::Colon]) {
                        break;
                    }
                }

                let loc = self.peek().span;
                let asm_binding = self.parse_asm_binding()?;
                let replacer_string = format!("%[{}]", asm_binding.name);
                if replacers.iter().any(|v| v.0 == replacer_string) {
                    return Err(ParsingError::DuplicateAsmReplacer {
                        loc,
                        name: asm_binding.name,
                    });
                }
                if asm_binding.bound.starts_with('=') || asm_binding.bound.starts_with('~') {
                    return Err(ParsingError::InputStartingWithInvalidChar {
                        loc,
                        input: asm_binding.bound,
                    });
                }
                replacers.push((replacer_string, format!("${{{}}}", replacers.len())));
                if !registers.is_empty() {
                    registers.push(',');
                }
                registers.push_str(&asm_binding.bound);
                inputs.push((loc, asm_binding.type_or_name));
            }

            if self.current().typ != TokenType::Colon {
                break 'out self.expect(TokenType::ParenRight)?.span;
            }
            if self.match_tok(TokenType::ParenRight) {
                break 'out self.last().span;
            }
            if !registers.is_empty() {
                registers.push(',');
            }
            registers.push_str("~{");
            let register = self.expect(TokenType::StringLiteral)?.string_literal();
            registers.push_str(&register);
            registers.push('}');

            loop {
                if self.peek().typ == TokenType::ParenRight {
                    break self.eat().span;
                }
                self.expect(TokenType::Comma)?;
                registers.push_str(",~{");
                let register = self.expect(TokenType::StringLiteral)?.string_literal();
                registers.push_str(&register);
                registers.push('}');
            }
        };

        while let Some((to_replace, value)) = replacers.last() {
            let Some(occurence) = asm.find(to_replace) else {
                replacers.pop();
                continue;
            };
            asm.replace_range(occurence..occurence + to_replace.len(), value);
        }
        Ok(Expression::Asm {
            span: span1.combine_with([span2], self.ctx.span_interner),
            asm,
            volatile,
            output,
            registers,
            inputs,
        })
    }

    pub fn parse_expression(&mut self) -> Result<Expression<'arena>, ParsingError<'arena>> {
        if self.peek().typ == TokenType::Asm {
            return self.parse_asm();
        }
        self.comparison()
    }

    fn comparison(&mut self) -> Result<Expression<'arena>, ParsingError<'arena>> {
        let mut expr = self.pipe_operator()?;

        loop {
            let op = match self.peek().typ {
                TokenType::EqualEqual => BinaryOp::Equals,
                TokenType::NotEquals => BinaryOp::NotEquals,
                TokenType::LogicalAnd => BinaryOp::LogicalAnd,
                TokenType::LogicalOr => BinaryOp::LogicalOr,
                TokenType::LessThan if self.peek2().typ == TokenType::Equal => BinaryOp::LessThanEq,
                TokenType::LessThan => BinaryOp::LessThan,
                TokenType::GreaterThan if self.peek2().typ == TokenType::Equal => {
                    BinaryOp::GreaterThanEq
                }
                TokenType::GreaterThan => BinaryOp::GreaterThan,
                _ => break,
            };
            self.dismiss();
            let right = self.pipe_operator()?;
            expr = Expression::binary(
                op,
                expr.span()
                    .combine_with([right.span()], self.ctx.span_interner),
                expr,
                right,
            );
        }

        Ok(expr)
    }

    fn pipe_operator(&mut self) -> Result<Expression<'arena>, ParsingError<'arena>> {
        let mut expr = self.range()?;

        while self.match_tok(TokenType::PipeOperator) {
            let call = self.indexed()?;
            match call {
                Expression::FunctionCall {
                    identifier,
                    mut arguments,
                    span,
                } => {
                    let expr_span = expr.span();
                    arguments.insert(0, expr);
                    expr = Expression::FunctionCall {
                        identifier,
                        arguments,
                        span: span.combine_with([expr_span], self.ctx.span_interner),
                    };
                }
                e => return Err(ParsingError::ExpectedFunctionCall { loc: e.span() }),
            }
        }

        Ok(expr)
    }

    fn range(&mut self) -> Result<Expression<'arena>, ParsingError<'arena>> {
        let mut expr = self.term()?;

        while self.matches(&[TokenType::Range, TokenType::RangeInclusive]) {
            let inclusive = self.current().typ == TokenType::RangeInclusive;
            let loc = self.current().span;
            let right_side = Box::new(self.parse_expression()?);
            expr = Expression::Range {
                left_side: Box::new(expr),
                right_side,
                inclusive,
                span: loc,
            };
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expression<'arena>, ParsingError<'arena>> {
        let mut expr = self.factor()?;

        while self.matches(&[
            TokenType::Minus,
            TokenType::Plus,
            TokenType::PlusAssign,
            TokenType::MinusAssign,
        ]) {
            let mut operator = self.current();
            let right = self.factor()?;
            let span = expr
                .span()
                .combine_with([right.span()], self.ctx.span_interner);

            match operator.typ {
                TokenType::PlusAssign => {
                    operator.typ = TokenType::Plus;
                    assign_set!(expr, right, BinaryOp::Plus, span);
                }
                TokenType::MinusAssign => {
                    operator.typ = TokenType::Minus;
                    assign_set!(expr, right, BinaryOp::Minus, span);
                }
                TokenType::Plus => expr = Expression::binary(BinaryOp::Plus, span, expr, right),
                TokenType::Minus => expr = Expression::binary(BinaryOp::Minus, span, expr, right),
                _ => unreachable!(),
            }
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expression<'arena>, ParsingError<'arena>> {
        let mut expr = self.unary()?;

        loop {
            let op = match (self.peek().typ, self.peek2().typ) {
                (TokenType::Divide, _) => BinaryOp::Divide,
                (TokenType::Modulo, _) => BinaryOp::Modulo,
                (TokenType::Asterix, _) => BinaryOp::Multiply,
                (TokenType::Ampersand, _) => BinaryOp::BitwiseAnd,
                (TokenType::BitwiseOr, _) => BinaryOp::BitwiseOr,
                (TokenType::BitwiseXor, _) => BinaryOp::BitwiseXor,
                (TokenType::LessThan, TokenType::LessThan) => BinaryOp::LShift,
                (TokenType::GreaterThan, TokenType::GreaterThan) => BinaryOp::RShift,
                _ => break,
            };
            self.dismiss();
            if op == BinaryOp::LShift || op == BinaryOp::RShift {
                self.dismiss();
            }
            let right = self.unary()?;
            let span = expr
                .span()
                .combine_with([right.span()], self.ctx.span_interner);
            if self.match_tok(TokenType::Equal) {
                assign_set!(expr, right, op, span);
            } else {
                expr = Expression::binary(op, span, expr, right);
            }
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expression<'arena>, ParsingError<'arena>> {
        if self.matches(&[
            TokenType::Plus,
            TokenType::Minus,
            TokenType::BitwiseNot,
            TokenType::LogicalNot,
        ]) {
            let operator = match self.current().typ {
                TokenType::Plus => UnaryOp::Plus,
                TokenType::Minus => UnaryOp::Minus,
                TokenType::BitwiseNot => UnaryOp::BitwiseNot,
                TokenType::LogicalNot => UnaryOp::LogicalNot,
                _ => unreachable!(),
            };
            let span = self.current().span;
            let next = self.unary()?;
            let span = span.combine_with([next.span()], self.ctx.span_interner);
            return Ok(Expression::unary(operator, span, next));
        }

        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expression<'arena>, ParsingError<'arena>> {
        let expr = self.type_cast()?;
        if self.match_tok(TokenType::Equal) {
            let value = self.parse_expression()?;
            let span = expr
                .span()
                .combine_with([value.span()], self.ctx.span_interner);
            return Ok(Expression::Assignment {
                left_side: Box::new(expr),
                right_side: Box::new(value),
                span,
            });
        }
        Ok(expr)
    }

    fn type_cast(&mut self) -> Result<Expression<'arena>, ParsingError<'arena>> {
        let mut expr = self.references()?;

        while self.match_tok(TokenType::As) {
            let new_type = TypeRef::parse(self)?;
            let span = expr
                .span()
                .combine_with([new_type.span()], self.ctx.span_interner);
            expr = Expression::TypeCast {
                left_side: Box::new(expr),
                new_type,
                span,
            };
        }

        Ok(expr)
    }

    fn references(&mut self) -> Result<Expression<'arena>, ParsingError<'arena>> {
        if self.match_tok(TokenType::Ampersand) {
            let right = self.references()?;
            let span = right
                .span()
                .combine_with([self.current().span], self.ctx.span_interner);
            Ok(Expression::Unary {
                operator: UnaryOp::Reference,
                span,
                right_side: Box::new(right),
            })
        } else if self.match_tok(TokenType::LogicalAnd) {
            let right = self.references()?;
            let span = right
                .span()
                .combine_with([self.current().span], self.ctx.span_interner);
            let expr = Expression::Unary {
                operator: UnaryOp::Reference,
                span,
                right_side: Box::new(right),
            };
            Ok(Expression::Unary {
                operator: UnaryOp::Reference,
                span,
                right_side: Box::new(expr),
            })
        } else if self.match_tok(TokenType::Asterix) {
            let right = self.references()?;
            let span = right
                .span()
                .combine_with([self.current().span], self.ctx.span_interner);
            Ok(Expression::Unary {
                operator: UnaryOp::Dereference,
                span,
                right_side: Box::new(right),
            })
        } else {
            self.indexed()
        }
    }

    fn indexed(&mut self) -> Result<Expression<'arena>, ParsingError<'arena>> {
        let mut expr = self.primary()?;

        while self.matches(&[TokenType::BracketLeft, TokenType::ParenLeft, TokenType::Dot]) {
            if self.current().typ == TokenType::ParenLeft {
                // function call
                let mut arguments: Vec<Expression> = vec![];
                loop {
                    if self.peek().typ == TokenType::ParenRight {
                        self.dismiss();
                        break;
                    } else if !arguments.is_empty() {
                        // , or ), but ) is alr handled by the thing above
                        if self.eat().typ != TokenType::Comma {
                            return Err(ParsingError::ExpectedFunctionArgument {
                                loc: self.current().span,
                                found: self.current(),
                            });
                        }
                    }

                    let next_loc = self.peek().span;
                    let next_typ = self.peek().typ;
                    if let Ok(expr) = self.parse_expression() {
                        arguments.push(expr);
                    } else {
                        return Err(ParsingError::ExpectedFunctionArgumentExpression {
                            loc: next_loc,
                            found: next_typ,
                        });
                    }
                }

                if self.last().typ == TokenType::ParenRight {
                    expr = Expression::FunctionCall {
                        span: self
                            .current()
                            .span
                            .combine_with([expr.span()], self.ctx.span_interner),
                        identifier: Box::new(expr),
                        arguments,
                    };
                } else if let Expression::MemberAccess {
                    left_side,
                    mut index,
                    span,
                } = expr
                {
                    expr = Expression::MemberCall {
                        arguments,
                        span: self
                            .current()
                            .span
                            .combine_with([span], self.ctx.span_interner),
                        identifier: index
                            .pop()
                            .expect("member access did not access any members"),
                        lhs: Box::new(Expression::MemberAccess {
                            left_side,
                            index,
                            span,
                        }),
                    }
                } else {
                    expr = Expression::FunctionCall {
                        span: self
                            .current()
                            .span
                            .combine_with([expr.span()], self.ctx.span_interner),
                        identifier: Box::new(expr),
                        arguments,
                    };
                }
            } else if self.current().typ == TokenType::BracketLeft {
                let indexing_expr = self.parse_expression()?;

                self.expect(TokenType::BracketRight)?;
                expr = Expression::Indexing {
                    span: self
                        .current()
                        .span
                        .combine_with([expr.span()], self.ctx.span_interner),
                    left_side: Box::new(expr),
                    right_side: Box::new(indexing_expr),
                };
            } else if self.current().typ == TokenType::Dot {
                let name = self.expect_identifier()?;
                if let Expression::MemberAccess { index, .. } = &mut expr {
                    index.push(name);
                    continue;
                }
                expr = Expression::MemberAccess {
                    span: self
                        .current()
                        .span
                        .combine_with([expr.span()], self.ctx.span_interner),
                    left_side: Box::new(expr),
                    index: vec![name],
                };
            } else {
                unreachable!();
            }
        }

        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expression<'arena>, ParsingError<'arena>> {
        let span1 = self.peek().span;
        if self.match_tok(TokenType::Dot) {
            // .{} / .[] / .()
            let typ = self.peek().typ;
            if typ == TokenType::ParenLeft || typ == TokenType::BracketLeft {
                self.eat();
                let matching_tok = if typ == TokenType::ParenLeft {
                    TokenType::ParenRight
                } else {
                    TokenType::BracketRight
                };
                let mut elements = Vec::new();
                while !self.match_tok(matching_tok) {
                    if !elements.is_empty() {
                        self.expect(TokenType::Comma)?;

                        if self.match_tok(matching_tok) {
                            break;
                        }
                    }

                    elements.push((self.peek().span, self.parse_expression()?));
                }
                let span = self
                    .current()
                    .span
                    .combine_with([span1], self.ctx.span_interner);
                return Ok(Expression::Literal(LiteralValue::Tuple(elements), span));
            } else if typ == TokenType::CurlyLeft {
                return self
                    .try_object()
                    .expect("this should never return None as we ensured the next token is of type CurlyLeft.")
                    .map(LiteralValue::AnonymousStruct)
                    .map(move |v| Expression::Literal(v, self
                        .current()
                        .span
                        .combine_with([span1], self.ctx.span_interner)));
            } else {
                unreachable!()
            }
        }

        // arrays
        if let Some(v) = self.try_array().transpose() {
            return v;
        }

        // functions/callables
        if self.peek().typ == TokenType::Fn {
            let loc = self.peek().span;
            return Ok(Expression::Literal(
                self.parse_callable(true, false)
                    .and_then(|(callable, body)| {
                        callable
                            .contract
                            .annotations
                            .are_annotations_valid_for(AnnotationReceiver::Function)?;
                        Ok(LiteralValue::AnonymousFunction(
                            callable.contract,
                            Box::new(body),
                        ))
                    })?,
                loc,
            ));
        }

        if matches!(self.peek().typ, TokenType::IdentifierLiteral) {
            let current = self.pos();
            if let Ok(path) = Path::parse(self) {
                // StructName { ... };
                let loc = self.peek().span;
                if let Some(obj) = self.try_object() {
                    return match obj {
                        Ok(v) => Ok(Expression::Literal(LiteralValue::Struct(v, path), loc)),
                        Err(e) => Err(e),
                    };
                } else {
                    return Ok(Expression::Literal(LiteralValue::Dynamic(path), loc));
                }
            } else {
                self.set_pos(current);
            }
        }

        if let Some(lit) = LiteralValue::from_token(self.peek()) {
            return Ok(Expression::Literal(lit, self.eat().span));
        }

        if self.match_tok(TokenType::ParenLeft) {
            let expr = self.parse_expression()?;
            self.expect(TokenType::ParenRight)?;
            return Ok(expr);
        }

        let found_token = self.peek();
        Err(ParsingError::ExpectedExpression {
            loc: self.peek().span,
            found: found_token.typ,
        })
    }

    fn try_array(&mut self) -> Result<Option<Expression<'arena>>, ParsingError<'arena>> {
        let span = self.peek().span;
        if self.match_tok(TokenType::BracketLeft) {
            let loc = self.current().span;
            let mut arr = vec![];
            while !self.match_tok(TokenType::BracketRight) {
                if arr.len() == 1 && self.match_tok(TokenType::Semicolon) {
                    let amount = self.expect(TokenType::UIntLiteral)?.uint_literal().0 as usize;
                    self.expect(TokenType::BracketRight)?;
                    return Ok(Some(Expression::Literal(
                        LiteralValue::Array(ArrayLiteral::CopyInitialized(
                            Box::new(arr.remove(0)),
                            amount,
                        )),
                        loc,
                    )));
                }
                if !arr.is_empty() {
                    // expect a comma
                    if !self.match_tok(TokenType::Comma) {
                        return Err(ParsingError::ExpectedArrayElement {
                            loc: self.peek().span,
                            found: self.peek().typ,
                        });
                    }
                    // in order to allow stuff like [1, 2, 3, ]
                    if self.match_tok(TokenType::BracketRight) {
                        break;
                    }
                }

                arr.push(self.parse_expression()?);
            }

            let span = self
                .peek()
                .span
                .combine_with([span], self.ctx.span_interner);
            Ok(Some(Expression::Literal(
                LiteralValue::Array(ArrayLiteral::Values(arr)),
                span,
            )))
        } else {
            Ok(None)
        }
    }

    // { key : value, }
    #[allow(clippy::type_complexity)]
    fn try_object(
        &mut self,
    ) -> Option<
        Result<HashMap<Ident<'arena>, (Span<'arena>, Expression<'arena>)>, ParsingError<'arena>>,
    > {
        if self.match_tok(TokenType::CurlyLeft) {
            let mut obj = HashMap::new();
            while !self.match_tok(TokenType::CurlyRight) {
                if !obj.is_empty() {
                    // expect a comma
                    if let Err(e) = self.expect_one_of(&[TokenType::CurlyRight, TokenType::Comma]) {
                        return Some(Err(e));
                    }
                    // in order to allow stuff like [1, 2, 3, ]
                    if self.match_tok(TokenType::CurlyRight) {
                        break;
                    }
                }

                // parse key : value
                let key = match self.expect_identifier() {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
                let location = self.current().span;

                if let Err(e) = self.expect(TokenType::Colon) {
                    return Some(Err(e));
                }

                match self.parse_expression() {
                    Ok(expr) => obj.insert(key, (location, expr)),
                    Err(e) => return Some(Err(e)),
                };
            }

            Some(Ok(obj))
        } else {
            None
        }
    }
}
