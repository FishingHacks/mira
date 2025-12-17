use std::{
    collections::HashMap,
    fmt::{Debug, Display, Write},
};

use crate::{
    Path, PathWithoutGenerics,
    annotations::AnnotationReceiver,
    error::ParsingError,
    module::{FunctionId, Module, ModuleContext, ModuleId},
};
use mira_lexer::{Delimiter, Literal, NumberType, Token, TokenTree, TokenType};
use mira_spans::{Ident, Span, interner::Symbol};

use super::{FunctionContract, Parser, Statement, statements::display_contract, types::TypeRef};

type StdResult<T, E> = std::result::Result<T, E>;
type Result<'ctx, V = Expression<'ctx>> = StdResult<V, ParsingError<'ctx>>;

#[derive(Debug, Clone)]
pub enum ArrayLiteral<'ctx> {
    Values(Vec<Expression<'ctx>>),
    CopyInitialized(Box<Expression<'ctx>>, usize),
}

#[derive(Debug, Clone)]
pub enum LiteralValue<'ctx> {
    String(Symbol<'ctx>),
    Array(ArrayLiteral<'ctx>),
    Struct(
        HashMap<Ident<'ctx>, (Span<'ctx>, Expression<'ctx>)>,
        Path<'ctx>,
    ),
    AnonymousStruct(HashMap<Ident<'ctx>, (Span<'ctx>, Expression<'ctx>)>),
    Tuple(Vec<Expression<'ctx>>),
    Float(f64, NumberType),
    SInt(i64, NumberType),
    UInt(u64, NumberType),
    Bool(bool),
    Dynamic(Path<'ctx>),
    AnonymousFunction(FunctionContract<'ctx>, Box<Statement<'ctx>>),
    BakedAnonymousFunction(FunctionId),
    Void,
}

impl Display for LiteralValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralValue::BakedAnonymousFunction(id) => {
                f.write_fmt(format_args!("(module-fn {})", id.to_usize()))
            }
            LiteralValue::Bool(b) => f.write_str(if *b { "true" } else { "false" }),
            LiteralValue::Dynamic(d) => Display::fmt(d, f),
            LiteralValue::UInt(v, ty) => f.write_fmt(format_args!("{}{}", *v, *ty)),
            LiteralValue::SInt(v, ty) => f.write_fmt(format_args!("{}{}", *v, *ty)),
            LiteralValue::Float(v, ty) => f.write_fmt(format_args!("{}{}", *v, *ty)),
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
                    Display::fmt(v, f)?;
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

impl<'ctx> LiteralValue<'ctx> {
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

    fn from_token(value: Token<'ctx>) -> Option<Self> {
        match value.ty {
            TokenType::StringLiteral
            | TokenType::BooleanLiteral
            | TokenType::FloatLiteral
            | TokenType::UIntLiteral
            | TokenType::SIntLiteral => value.literal.as_ref().map(|v| match v {
                Literal::Bool(boolean) => LiteralValue::Bool(*boolean),
                Literal::Float(float, ty) => LiteralValue::Float(*float, *ty),
                Literal::SInt(int, ty) => LiteralValue::SInt(*int, *ty),
                Literal::UInt(uint, ty) => LiteralValue::UInt(*uint, *ty),
                Literal::String(string) => LiteralValue::String(*string),
                Literal::DocComment(_) => unreachable!(),
            }),
            TokenType::VoidLiteral => Some(LiteralValue::Void),
            TokenType::IdentifierLiteral => match value.literal {
                Some(Literal::String(ref v)) => Some(LiteralValue::Dynamic(Path::new(
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
    Minus,
    Not,
    Reference,
    Dereference,
}

#[derive(Debug, Clone)]
pub enum Expression<'ctx> {
    Literal(LiteralValue<'ctx>, Span<'ctx>),
    Unary {
        operator: UnaryOp,
        span: Span<'ctx>,
        rhs: Box<Expression<'ctx>>,
    },
    Binary {
        operator: BinaryOp,
        span: Span<'ctx>,
        rhs: Box<Expression<'ctx>>,
        lhs: Box<Expression<'ctx>>,
    },
    FunctionCall {
        func: Box<Expression<'ctx>>,
        arguments: Vec<Expression<'ctx>>,
        span: Span<'ctx>,
    },
    // <$typ as $trait>::$fn_name(..$arguments),
    TraitFunctionCall {
        ty: TypeRef<'ctx>,
        trait_path: PathWithoutGenerics<'ctx>,
        fn_name: Ident<'ctx>,
        arguments: Vec<Expression<'ctx>>,
        span: Span<'ctx>,
    },
    MemberCall {
        fn_name: Ident<'ctx>,
        lhs: Box<Expression<'ctx>>,
        generics: Vec<TypeRef<'ctx>>,
        arguments: Vec<Expression<'ctx>>,
        span: Span<'ctx>,
    },
    Indexing {
        lhs: Box<Expression<'ctx>>,
        rhs: Box<Expression<'ctx>>,
        span: Span<'ctx>,
    },
    MemberAccess {
        lhs: Box<Expression<'ctx>>,
        index: Vec<Ident<'ctx>>,
        span: Span<'ctx>,
    },
    Assignment {
        lhs: Box<Expression<'ctx>>,
        rhs: Box<Expression<'ctx>>,
        span: Span<'ctx>,
    },
    Range {
        lhs: Box<Expression<'ctx>>,
        rhs: Box<Expression<'ctx>>,
        inclusive: bool,
        span: Span<'ctx>,
    },
    TypeCast {
        lhs: Box<Expression<'ctx>>,
        new_ty: TypeRef<'ctx>,
        span: Span<'ctx>,
    },
    Asm {
        span: Span<'ctx>,
        asm: String,
        volatile: bool,
        output: TypeRef<'ctx>,
        registers: String, // input + output + clobber
        inputs: Vec<(Span<'ctx>, Ident<'ctx>)>,
    },
}

impl Display for Expression<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Literal(v, ..) => Display::fmt(v, f),
            Expression::Binary {
                operator, rhs, lhs, ..
            } => {
                f.write_char('(')?;
                operator.fmt(f)?;
                f.write_char(' ')?;
                Display::fmt(lhs, f)?;
                f.write_char(' ')?;
                Display::fmt(rhs, f)?;
                f.write_char(')')
            }
            Expression::Unary { operator, rhs, .. } => {
                f.write_char('(')?;
                operator.fmt(f)?;
                f.write_char(' ')?;
                Display::fmt(rhs, f)?;
                f.write_char(')')
            }
            Expression::FunctionCall {
                func,
                arguments,
                span: _,
            } => {
                f.write_str("(call ")?;
                Display::fmt(func, f)?;
                for v in arguments.iter() {
                    f.write_char(' ')?;
                    Display::fmt(v, f)?;
                }
                f.write_char(')')
            }
            Self::TraitFunctionCall {
                ty,
                trait_path,
                fn_name,
                arguments,
                span: _,
            } => {
                f.write_str("(generic-call ")?;
                Display::fmt(trait_path, f)?;
                f.write_char(' ')?;
                Display::fmt(fn_name, f)?;
                f.write_char(' ')?;
                Display::fmt(ty, f)?;

                for v in arguments.iter() {
                    f.write_char(' ')?;
                    Display::fmt(v, f)?;
                }
                f.write_char(')')
            }
            Expression::Indexing { lhs, rhs, span: _ } => {
                f.write_str("(index ")?;
                Display::fmt(lhs, f)?;
                f.write_char(' ')?;
                Display::fmt(rhs, f)?;
                f.write_char(')')
            }
            Expression::MemberAccess {
                lhs,
                index,
                span: _,
            } => {
                f.write_str("(member ")?;
                Display::fmt(lhs, f)?;
                for value in index {
                    f.write_char(' ')?;
                    Display::fmt(value, f)?;
                }

                f.write_char(')')
            }
            Expression::Assignment { lhs, rhs, span: _ } => {
                f.write_str("(assignment ")?;
                Display::fmt(lhs, f)?;
                f.write_char(' ')?;
                Display::fmt(rhs, f)?;
                f.write_char(')')
            }
            Expression::Range {
                lhs,
                rhs,
                inclusive,
                span: _,
            } => {
                f.write_str("(range ")?;
                if *inclusive {
                    f.write_str("inclusive ")?;
                }
                Display::fmt(lhs, f)?;
                f.write_char(' ')?;
                Display::fmt(rhs, f)?;
                f.write_char(')')
            }
            Expression::TypeCast {
                lhs,
                new_ty: new_type,
                span: _,
            } => {
                f.write_str("(type-cast ")?;
                Display::fmt(lhs, f)?;
                f.write_str(" -> ")?;
                Display::fmt(new_type, f)?;
                f.write_char(')')
            }
            Expression::MemberCall {
                fn_name: identifier,
                lhs,
                arguments,
                generics,
                span: _,
            } => {
                f.write_str("(member-call ")?;
                Display::fmt(lhs, f)?;
                f.write_char(' ')?;
                Display::fmt(identifier, f)?;
                if !generics.is_empty() {
                    f.write_str("::<")?;
                    for (i, g) in generics.iter().enumerate() {
                        if i != 0 {
                            f.write_char(' ')?;
                        }
                        Display::fmt(g, f)?;
                    }
                    f.write_char('>')?;
                }
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
                span: _,
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

impl<'ctx> Expression<'ctx> {
    pub fn bool(value: bool, span: Span<'ctx>) -> Self {
        Self::Literal(LiteralValue::Bool(value), span)
    }

    pub fn string(value: Symbol<'ctx>, span: Span<'ctx>) -> Self {
        Self::Literal(LiteralValue::String(value), span)
    }

    pub fn unary(operator: UnaryOp, span: Span<'ctx>, right: Expression<'ctx>) -> Self {
        Self::Unary {
            operator,
            span,
            rhs: Box::new(right),
        }
    }

    pub fn binary(
        operator: BinaryOp,
        span: Span<'ctx>,
        left: Expression<'ctx>,
        right: Expression<'ctx>,
    ) -> Self {
        Self::Binary {
            operator,
            span,
            rhs: Box::new(right),
            lhs: Box::new(left),
        }
    }

    pub fn span(&self) -> Span<'ctx> {
        match self {
            Self::FunctionCall { span, .. }
            | Self::TraitFunctionCall { span, .. }
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
        module: &mut Module<'ctx>,
        module_key: ModuleId,
        context: &ModuleContext<'ctx>,
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
                            comment: contract.comment,
                        },
                        std::mem::replace(&mut **statements, Statement::None),
                        module_key,
                        context,
                    );
                    *val = LiteralValue::BakedAnonymousFunction(id)
                }
            }
            Self::Binary { rhs, lhs, .. }
            | Self::Assignment { lhs, rhs, .. }
            | Self::Indexing { lhs, rhs, .. }
            | Self::Range { lhs, rhs, .. } => {
                lhs.bake_functions(module, module_key, context);
                rhs.bake_functions(module, module_key, context);
            }
            Self::TraitFunctionCall { arguments, .. } => arguments
                .iter_mut()
                .for_each(|el| el.bake_functions(module, module_key, context)),
            Self::FunctionCall {
                func, arguments, ..
            } => {
                func.bake_functions(module, module_key, context);
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
            Self::MemberAccess { lhs, .. }
            | Self::TypeCast { lhs, .. }
            | Self::Unary { rhs: lhs, .. } => lhs.bake_functions(module, module_key, context),
        }
    }
}

struct AsmBinding<'ctx> {
    name: Symbol<'ctx>,
    bound: Symbol<'ctx>,
    type_or_name: Ident<'ctx>,
}

#[derive(Debug, Clone, Copy)]
enum InfixOp {
    Plus,
    Minus,
    Divide,
    Multiply,
    Modulo,
    LShift,
    RShift,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    PlusAssign,
    MinusAssign,
    DivideAssign,
    MultiplyAssign,
    ModuloAssign,
    LShiftAssign,
    RShiftAssign,
    Assign,
    BitwiseAndAssign,
    BitwiseOrAssign,
    BitwiseXorAssign,
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    Eq,
    Neq,
    LogicalAnd,
    LogicalOr,
    Range,
    RangeInclusive,
}

fn infix_binding_power(op: InfixOp) -> (u8, u8) {
    use InfixOp as O;

    match op {
        O::PlusAssign
        | O::MinusAssign
        | O::DivideAssign
        | O::MultiplyAssign
        | O::ModuloAssign
        | O::LShiftAssign
        | O::RShiftAssign
        | O::BitwiseAndAssign
        | O::BitwiseOrAssign
        | O::BitwiseXorAssign
        | O::Assign => (2, 1),
        O::Range | O::RangeInclusive => (3, 4),
        O::LogicalAnd | O::LogicalOr => (5, 6),
        O::BitwiseAnd | O::BitwiseOr | O::BitwiseXor => (7, 8),
        O::Eq | O::Neq => (9, 10),
        O::LessThan | O::LessThanEq | O::GreaterThan | O::GreaterThanEq => (11, 12),
        O::LShift | O::RShift => (13, 14),
        O::Plus | O::Minus => (15, 16),
        O::Divide | O::Multiply | O::Modulo => (17, 18),
    }
}

impl<'ctx> Parser<'_, 'ctx> {
    /// Parses [<.0>] "<.1>" (<.2>)
    fn parse_asm_binding(&mut self) -> Result<'ctx, AsmBinding<'ctx>> {
        let name_delim = self.expect_delim_stream(Delimiter::Brackets)?;
        let mut namep = self.subparser(name_delim);
        let name = namep.expect_identifier()?.symbol();
        namep.finish()?;

        let bound = self.expect(TokenType::StringLiteral)?.string_literal();

        let ty_delim = self.expect_delim_stream(Delimiter::Parenthesis)?;
        let mut typ = self.subparser(ty_delim);
        let ty = typ.expect_identifier()?;
        typ.finish()?;

        Ok(AsmBinding {
            name,
            bound,
            type_or_name: ty,
        })
    }

    // TODO: replace with rust-style asm!() macro i think?
    fn parse_asm(&mut self, span: Span<'ctx>) -> Result<'ctx> {
        let volatile = self.match_tok_dismiss(TokenType::Volatile);

        let delim = self.expect_delim(Delimiter::Parenthesis)?;
        let mut p = self.subparser(delim.into());
        let span = span.combine_with([delim.close_span], self.ctx().span_interner);

        let mut replacers: Vec<(String, String)> = Vec::new();
        let mut registers = String::new();
        let mut inputs = Vec::new();
        let mut output = TypeRef::Void(span, 0);
        let mut asm = String::new();
        while !p.is_at_end() && !p.match_tok_dismiss(TokenType::Colon) {
            if !asm.is_empty() {
                p.expect(TokenType::Comma)?;
                if !p.is_at_end() && !p.match_tok_dismiss(TokenType::Colon) {
                    break;
                }
            }
            let s = p.expect(TokenType::StringLiteral)?.string_literal();
            if !asm.is_empty() {
                asm.push('\n');
            }
            asm.push_str(&s);
        }

        'out: {
            if p.is_at_end() {
                break 'out;
            }

            // outputs
            'outputs: {
                if p.peek_tok().unwrap().ty == TokenType::Colon {
                    break 'outputs;
                }
                // [v] "=r" (ty)
                let span = p.peek().unwrap().span();
                let asm_binding = p.parse_asm_binding()?;
                let replacer_string = format!("%[{}]", asm_binding.name);
                if replacers.iter().any(|v| v.0 == replacer_string) {
                    return Err(ParsingError::DuplicateAsmReplacer {
                        span,
                        name: asm_binding.name,
                    });
                }
                if !asm_binding.bound.starts_with('=') {
                    return Err(ParsingError::OutputNotStartingWithEqual {
                        span,
                        output: asm_binding.bound,
                    });
                }
                replacers.push((replacer_string, format!("${{{}}}", replacers.len())));
                registers.push_str(&asm_binding.bound);
                output = TypeRef::Reference {
                    num_references: 0,
                    type_name: Path::new(asm_binding.type_or_name, Vec::new()),
                    span,
                }
            };

            if p.is_at_end() {
                break 'out;
            }
            p.expect(TokenType::Colon)?;

            // inputs
            while !p.is_at_end() && !p.match_tok_dismiss(TokenType::Colon) {
                if !inputs.is_empty() {
                    p.expect(TokenType::Comma)?;
                    if p.is_at_end() || p.match_tok_dismiss(TokenType::Colon) {
                        break;
                    }
                }

                let span = p.peek().unwrap().span();
                let asm_binding = p.parse_asm_binding()?;
                let replacer_string = format!("%[{}]", asm_binding.name);
                if replacers.iter().any(|v| v.0 == replacer_string) {
                    return Err(ParsingError::DuplicateAsmReplacer {
                        span,
                        name: asm_binding.name,
                    });
                }
                if asm_binding.bound.starts_with('=') || asm_binding.bound.starts_with('~') {
                    return Err(ParsingError::InputStartingWithInvalidChar {
                        span,
                        input: asm_binding.bound,
                    });
                }
                replacers.push((replacer_string, format!("${{{}}}", replacers.len())));
                if !registers.is_empty() {
                    registers.push(',');
                }
                registers.push_str(&asm_binding.bound);
                inputs.push((span, asm_binding.type_or_name));
            }

            if p.is_at_end() {
                break 'out;
            }
            if !registers.is_empty() {
                registers.push(',');
            }
            registers.push_str("~{");
            let register = self.expect(TokenType::StringLiteral)?.string_literal();
            registers.push_str(&register);
            registers.push('}');

            while !p.is_at_end() {
                p.expect(TokenType::Comma)?;
                registers.push_str(",~{");
                let register = p.expect(TokenType::StringLiteral)?.string_literal();
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
            span,
            asm,
            volatile,
            output,
            registers,
            inputs,
        })
    }

    fn peek_infix_op(&mut self) -> Option<(InfixOp, u8)> {
        use InfixOp as O;
        use Some as S;
        use TokenType as T;

        let peek1 = self.peek().and_then(TokenTree::token)?;
        let peek2 = self.peek2().and_then(TokenTree::token);
        let peek3 = self.peek3().and_then(TokenTree::token);

        let ty1 = peek1.ty;
        let ty2 = peek2.map(|v| v.ty);
        let ty3 = peek3.map(|v| v.ty);

        // Range,
        // RangeInclusive,
        let v = match ty1 {
            T::Plus => (O::Plus, 1),
            T::PlusAssign => (O::PlusAssign, 1),
            T::Minus => (O::Minus, 1),
            T::MinusAssign => (O::MinusAssign, 1),
            T::Divide if ty2 == S(T::Equal) => (O::DivideAssign, 2),
            T::Divide => (O::Divide, 1),
            T::Asterix if ty2 == S(T::Equal) => (O::MultiplyAssign, 2),
            T::Asterix => (O::Multiply, 1),
            T::Modulo if ty2 == S(T::Equal) => (O::ModuloAssign, 2),
            T::Modulo => (O::Modulo, 1),
            T::LessThan if ty2 == S(T::LessThan) && ty3 == S(T::Equal) => (O::LShiftAssign, 3),
            T::LessThan if ty2 == S(T::LessThan) => (O::LShift, 2),
            T::GreaterThan if ty2 == S(T::GreaterThan) && ty3 == S(T::Equal) => {
                (O::RShiftAssign, 3)
            }
            T::GreaterThan if ty2 == S(T::GreaterThan) => (O::RShift, 2),
            T::LessThan if ty2 == S(T::Equal) => (O::LessThanEq, 2),
            T::LessThan => (O::LessThan, 1),
            T::GreaterThan if ty2 == S(T::Equal) => (O::GreaterThanEq, 2),
            T::GreaterThan => (O::GreaterThan, 1),
            T::EqualEqual => (O::Eq, 1),
            T::Equal => (O::Assign, 1),
            T::NotEquals => (O::Neq, 1),
            T::LogicalAnd => (O::LogicalAnd, 1),
            T::LogicalOr => (O::LogicalOr, 1),
            T::Ampersand if ty2 == S(T::Equal) => (O::BitwiseAndAssign, 2),
            T::BitwiseOr if ty2 == S(T::Equal) => (O::BitwiseOrAssign, 2),
            T::BitwiseXor if ty2 == S(T::Equal) => (O::BitwiseXorAssign, 2),
            T::Ampersand => (O::BitwiseAnd, 1),
            T::BitwiseXor => (O::BitwiseXor, 1),
            T::BitwiseOr => (O::BitwiseOr, 1),
            T::Range => (O::Range, 1),
            T::RangeInclusive => (O::RangeInclusive, 1),
            _ => return None,
        };
        Some(v)
    }

    pub fn parse_expression(&mut self) -> Result<'ctx> {
        if let Some(t) = self.match_tok(TokenType::Asm) {
            return self.parse_asm(t.span);
        }

        self.expr_bp(0)
    }

    fn infix_op_to_expr(
        op: InfixOp,
        lhs: Expression<'ctx>,
        rhs: Expression<'ctx>,
        span: Span<'ctx>,
    ) -> Expression<'ctx> {
        use BinaryOp as B;
        use Expression as E;
        fn bin<'ctx>(op: B, lhs: Box<E<'ctx>>, rhs: Box<E<'ctx>>, span: Span<'ctx>) -> E<'ctx> {
            E::Binary {
                operator: op,
                span,
                rhs,
                lhs,
            }
        }
        fn binass<'ctx>(op: B, lhs: Box<E<'ctx>>, rhs: Box<E<'ctx>>, span: Span<'ctx>) -> E<'ctx> {
            E::Assignment {
                rhs: Box::new(bin(op, lhs.clone(), rhs, span)),
                lhs,
                span,
            }
        }

        let lhs = Box::new(lhs);
        let rhs = Box::new(rhs);
        match op {
            InfixOp::Plus => bin(BinaryOp::Plus, lhs, rhs, span),
            InfixOp::Minus => bin(BinaryOp::Minus, lhs, rhs, span),
            InfixOp::Divide => bin(BinaryOp::Divide, lhs, rhs, span),
            InfixOp::Multiply => bin(BinaryOp::Multiply, lhs, rhs, span),
            InfixOp::Modulo => bin(BinaryOp::Modulo, lhs, rhs, span),
            InfixOp::LShift => bin(BinaryOp::LShift, lhs, rhs, span),
            InfixOp::RShift => bin(BinaryOp::RShift, lhs, rhs, span),
            InfixOp::BitwiseAnd => bin(BinaryOp::BitwiseAnd, lhs, rhs, span),
            InfixOp::BitwiseOr => bin(BinaryOp::BitwiseOr, lhs, rhs, span),
            InfixOp::BitwiseXor => bin(BinaryOp::BitwiseXor, lhs, rhs, span),
            InfixOp::LessThan => bin(BinaryOp::LessThan, lhs, rhs, span),
            InfixOp::LessThanEq => bin(BinaryOp::LessThanEq, lhs, rhs, span),
            InfixOp::GreaterThan => bin(BinaryOp::GreaterThan, lhs, rhs, span),
            InfixOp::GreaterThanEq => bin(BinaryOp::GreaterThanEq, lhs, rhs, span),
            InfixOp::Eq => bin(BinaryOp::Equals, lhs, rhs, span),
            InfixOp::Neq => bin(BinaryOp::NotEquals, lhs, rhs, span),
            InfixOp::LogicalAnd => bin(BinaryOp::LogicalAnd, lhs, rhs, span),
            InfixOp::LogicalOr => bin(BinaryOp::LogicalOr, lhs, rhs, span),
            InfixOp::Range => E::Range {
                lhs,
                rhs,
                inclusive: false,
                span,
            },
            InfixOp::RangeInclusive => E::Range {
                lhs,
                rhs,
                inclusive: true,
                span,
            },

            InfixOp::PlusAssign => binass(BinaryOp::Plus, lhs, rhs, span),
            InfixOp::MinusAssign => binass(BinaryOp::Minus, lhs, rhs, span),
            InfixOp::DivideAssign => binass(BinaryOp::Divide, lhs, rhs, span),
            InfixOp::MultiplyAssign => binass(BinaryOp::Multiply, lhs, rhs, span),
            InfixOp::ModuloAssign => binass(BinaryOp::Modulo, lhs, rhs, span),
            InfixOp::LShiftAssign => binass(BinaryOp::LShift, lhs, rhs, span),
            InfixOp::RShiftAssign => binass(BinaryOp::RShift, lhs, rhs, span),
            InfixOp::BitwiseAndAssign => binass(BinaryOp::BitwiseAnd, lhs, rhs, span),
            InfixOp::BitwiseOrAssign => binass(BinaryOp::BitwiseOr, lhs, rhs, span),
            InfixOp::BitwiseXorAssign => binass(BinaryOp::BitwiseXor, lhs, rhs, span),
            InfixOp::Assign => E::Assignment { lhs, rhs, span },
        }
    }

    fn expr_bp(&mut self, min_bp: u8) -> Result<'ctx> {
        let mut lhs = self.parse_typecast()?;

        loop {
            if self.is_at_end() {
                break;
            }
            let Some((op, toks)) = self.peek_infix_op() else {
                break;
            };
            let (l_bp, r_bp) = infix_binding_power(op);
            if l_bp < min_bp {
                break;
            }
            for _ in 0..toks {
                self.dismiss();
            }

            let rhs = self.expr_bp(r_bp)?;
            let span = lhs
                .span()
                .combine_with([rhs.span()], self.ctx().span_interner);
            lhs = Self::infix_op_to_expr(op, lhs, rhs, span);
        }

        Ok(lhs)
    }

    fn parse_typecast(&mut self) -> Result<'ctx> {
        let mut expr = self.parse_unary()?;

        while self.match_tok_dismiss(TokenType::As) {
            let ty = TypeRef::parse(self)?;
            let span = expr
                .span()
                .combine_with([ty.span()], self.ctx().span_interner);
            expr = Expression::TypeCast {
                lhs: Box::new(expr),
                new_ty: ty,
                span,
            };
        }
        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<'ctx> {
        match self.peek_tok().map(|v| v.ty) {
            Some(TokenType::Minus) => {
                let span = self.peek_span();
                self.dismiss();
                let e = self.parse_unary()?;
                let span = span.combine_with([e.span()], self.ctx().span_interner);
                Ok(Expression::Unary {
                    operator: UnaryOp::Minus,
                    span,
                    rhs: Box::new(e),
                })
            }
            Some(TokenType::Not) => {
                let span = self.peek_span();
                self.dismiss();
                let e = self.parse_unary()?;
                let span = span.combine_with([e.span()], self.ctx().span_interner);
                Ok(Expression::Unary {
                    operator: UnaryOp::Not,
                    span,
                    rhs: Box::new(e),
                })
            }
            Some(TokenType::Ampersand) => {
                let span = self.peek_span();
                self.dismiss();
                let e = self.parse_unary()?;
                let span = span.combine_with([e.span()], self.ctx().span_interner);
                Ok(Expression::Unary {
                    operator: UnaryOp::Reference,
                    span,
                    rhs: Box::new(e),
                })
            }
            Some(TokenType::LogicalAnd) => {
                let span = self.peek_span();
                self.dismiss();
                let e = self.parse_unary()?;
                let span = span.combine_with([e.span()], self.ctx().span_interner);
                let e = Expression::Unary {
                    operator: UnaryOp::Reference,
                    span,
                    rhs: Box::new(e),
                };
                Ok(Expression::Unary {
                    operator: UnaryOp::Reference,
                    span,
                    rhs: Box::new(e),
                })
            }
            Some(TokenType::Asterix) => {
                let span = self.peek_span();
                self.dismiss();
                let e = self.parse_unary()?;
                let span = span.combine_with([e.span()], self.ctx().span_interner);
                Ok(Expression::Unary {
                    operator: UnaryOp::Dereference,
                    span,
                    rhs: Box::new(e),
                })
            }
            _ => self.parse_index_fncall(),
        }
    }

    fn parse_index_fncall(&mut self) -> Result<'ctx> {
        let mut expr = self.parse_literal()?;

        loop {
            if self.match_tok_dismiss(TokenType::Dot) {
                let field = self.expect_identifier()?;
                // check for :: or ( ... ), as that means this is a member call (a.b::<i32>(), or
                // a.b())
                if let Some(t) = self.peek_tok()
                    && matches!(t.ty, TokenType::NamespaceAccess | TokenType::ParenOpen)
                {
                    expr = self.parse_membercall(expr, field)?;
                } else if let Expression::MemberAccess { index, span, .. } = &mut expr {
                    index.push(field);
                    *span = span.combine_with([field.span()], self.ctx().span_interner);
                } else {
                    let span = expr
                        .span()
                        .combine_with([field.span()], self.ctx().span_interner);
                    expr = Expression::MemberAccess {
                        lhs: Box::new(expr),
                        index: vec![field],
                        span,
                    };
                }
            } else if let Some(delim) = self.match_delim(Delimiter::Brackets) {
                let mut argp = self.subparser(delim.into());
                let idx = argp.parse_expression()?;
                argp.finish()?;
                let span = expr
                    .span()
                    .combine_with([delim.close_span], self.ctx().span_interner);
                expr = Expression::Indexing {
                    lhs: Box::new(expr),
                    rhs: Box::new(idx),
                    span,
                };
            } else if let Some(delim) = self.match_delim(Delimiter::Parenthesis) {
                // a(), (a.b)(), a()(), ...; member calls, a.b(), are handled by the `.` case.
                let arguments = self.subparser(delim.into()).parse_args()?;
                let span = expr
                    .span()
                    .combine_with([delim.close_span], self.ctx().span_interner);
                expr = Expression::FunctionCall {
                    func: Box::new(expr),
                    arguments,
                    span,
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_membercall(&mut self, lhs: Expression<'ctx>, fn_name: Ident<'ctx>) -> Result<'ctx> {
        let mut generics = Vec::new();
        if self.match_tok_dismiss(TokenType::NamespaceAccess) {
            self.expect(TokenType::LessThan)?;
            while !self.match_tok_dismiss(TokenType::GreaterThan) {
                generics.push(TypeRef::parse(self)?);
            }
        }
        let delim = self.expect_delim(Delimiter::Parenthesis)?;
        let arguments = self.subparser(delim.into()).parse_args()?;
        let span = lhs
            .span()
            .combine_with([fn_name.span(), delim.close_span], self.ctx().span_interner);
        Ok(Expression::MemberCall {
            fn_name,
            lhs: Box::new(lhs),
            generics,
            arguments,
            span,
        })
    }

    /// Parses a literal or anything with the same precedence (e.g. (a + b), 12, "awa", ...)
    fn parse_literal(&mut self) -> Result<'ctx> {
        if let Some(s) = self.match_delim_stream(Delimiter::Parenthesis) {
            let mut p = self.subparser(s);
            let v = p.parse_expression()?;
            p.finish()?;
            Ok(v)
        } else if self.match_tok_dismiss(TokenType::Dot) {
            // .{} / .[]
            if let Some(delim) = self.match_delim(Delimiter::Brackets) {
                // tuple (.[a, b, c, d])
                let mut p = self.subparser(delim.into());
                let elems = p.parse_args()?;
                let span = delim.full_span(self.ctx().span_interner);
                Ok(Expression::Literal(LiteralValue::Tuple(elems), span))
            } else if let Some(delim) = self.match_delim(Delimiter::Curlies) {
                // anon struct (.{ a: b, c, d })
                let mut p = self.subparser(delim.into());
                let body = p.parse_struct_body()?;
                let span = delim.full_span(self.ctx().span_interner);
                Ok(Expression::Literal(
                    LiteralValue::AnonymousStruct(body),
                    span,
                ))
            } else {
                Err(ParsingError::ExpectedOneOf {
                    span: self.peek_span(),
                    found: self.peek_tok(),
                    valid: &[TokenType::BracketOpen, TokenType::CurlyOpen],
                })
            }
        } else if let Some(delim) = self.match_delim(Delimiter::Brackets) {
            // [a, b, c, ...] or [expr; amount]
            let mut p = self.subparser(delim.into());
            let span = delim.full_span(self.ctx().span_interner);
            if p.is_at_end() {
                // []
                Ok(Expression::Literal(
                    LiteralValue::Array(ArrayLiteral::Values(vec![])),
                    span,
                ))
            } else {
                let expr = p.parse_expression()?;
                if p.match_tok_dismiss(TokenType::Semicolon) {
                    let t = p.expect(TokenType::UIntLiteral)?;
                    let lit = LiteralValue::Array(ArrayLiteral::CopyInitialized(
                        Box::new(expr),
                        t.uint_literal().0 as usize,
                    ));
                    Ok(Expression::Literal(lit, span))
                } else {
                    let elems = p.parse_args_partial(vec![expr])?;
                    let lit = LiteralValue::Array(ArrayLiteral::Values(elems));
                    Ok(Expression::Literal(lit, span))
                }
            }
        } else if self.peek_tok().map(|t| t.ty) == Some(TokenType::Fn) {
            // fnlit

            let (callable, body) = self
                .parse_callable(true, false, self.peek_span())
                .and_then(|(callable, body)| {
                    callable
                        .contract
                        .annotations
                        .are_annotations_valid_for(AnnotationReceiver::Function)?;
                    Ok((callable, body))
                })?;
            let span = callable
                .contract
                .span
                .combine_with([body.span()], self.ctx().span_interner);
            let lit = LiteralValue::AnonymousFunction(callable.contract, Box::new(body));
            Ok(Expression::Literal(lit, span))
        } else if self.peek_tok().map(|t| t.ty) == Some(TokenType::IdentifierLiteral) {
            // path or struct def
            let p = Path::parse(self)?;
            if let Some(delim) = self.match_delim(Delimiter::Curlies) {
                let mut bodyp = self.subparser(delim.into());
                let body = bodyp.parse_struct_body()?;
                let span = p
                    .span
                    .combine_with([delim.close_span], bodyp.ctx().span_interner);
                Ok(Expression::Literal(LiteralValue::Struct(body, p), span))
            } else {
                let span = p.span;
                Ok(Expression::Literal(LiteralValue::Dynamic(p), span))
            }
        } else if self.match_tok_dismiss(TokenType::LessThan) {
            // <type as trait>::fn()
            let ty = TypeRef::parse(self)?;
            self.expect(TokenType::As)?;
            let trait_path = PathWithoutGenerics::parse(self)?;
            self.expect(TokenType::GreaterThan)?;
            self.expect(TokenType::NamespaceAccess)?;
            let fn_name = self.expect_identifier()?;
            let delim = self.expect_delim(Delimiter::Parenthesis)?;
            let arguments = self.subparser(delim.into()).parse_args()?;
            let span = ty.span().combine_with(
                [trait_path.span, delim.close_span, fn_name.span()],
                self.ctx().span_interner,
            );
            Ok(Expression::TraitFunctionCall {
                ty,
                trait_path,
                fn_name,
                arguments,
                span,
            })
        } else if let Some(v) = self.peek_tok().and_then(LiteralValue::from_token) {
            let span = self.peek_span();
            self.dismiss();
            Ok(Expression::Literal(v, span))
        } else {
            Err(ParsingError::ExpectedOneOf {
                span: self.peek_span(),
                found: self.peek_tok(),
                valid: &[
                    TokenType::BracketOpen,
                    TokenType::Dot,
                    TokenType::IdentifierLiteral,
                    TokenType::Fn,
                    TokenType::ParenOpen,
                    TokenType::FloatLiteral,
                    TokenType::StringLiteral,
                    TokenType::VoidLiteral,
                    TokenType::BooleanLiteral,
                    TokenType::SIntLiteral,
                    TokenType::UIntLiteral,
                    TokenType::LessThan,
                ],
            })
        }
    }

    fn parse_args(&mut self) -> Result<'ctx, Vec<Expression<'ctx>>> {
        self.parse_args_partial(Vec::new())
    }

    /// Parses the insides of a delimited list of expressions (consumes this entire parser).
    fn parse_args_partial(
        &mut self,
        mut args: Vec<Expression<'ctx>>,
    ) -> Result<'ctx, Vec<Expression<'ctx>>> {
        while !self.is_at_end() {
            if !args.is_empty() {
                self.expect(TokenType::Comma)?;
                if self.is_at_end() {
                    break;
                }
            }
            args.push(self.parse_expression()?);
        }
        Ok(args)
    }

    /// Parses the insides of the curlies of a struct (consumes this entire parser).
    fn parse_struct_body(
        &mut self,
    ) -> Result<'ctx, HashMap<Ident<'ctx>, (Span<'ctx>, Expression<'ctx>)>> {
        let mut fields = HashMap::new();
        while !self.is_at_end() {
            if !fields.is_empty() {
                self.expect(TokenType::Comma)?;
                if self.is_at_end() {
                    break;
                }
            }

            // either `name: expr` or `name`.
            let name = self.expect_identifier()?;
            if self.is_at_end()
                || self
                    .peek_tok()
                    .map(|v| v.ty == TokenType::Comma)
                    .unwrap_or(false)
            {
                let lit = LiteralValue::Dynamic(Path::new(name, vec![]));
                let expr = Expression::Literal(lit, name.span());
                fields.insert(name, (name.span(), expr));
                continue;
            }
            // can only be colon, because comma was just checked and the control flow is aborted,
            // and curlyclose would be captured by the parent parser. They're only there for the
            // error.
            self.expect_one_of(&[TokenType::Comma, TokenType::Colon, TokenType::CurlyClose])?;
            let expr = self.parse_expression()?;
            let span = name
                .span()
                .combine_with([expr.span()], self.ctx().span_interner);
            fields.insert(name, (span, expr));
        }
        Ok(fields)
    }
}
