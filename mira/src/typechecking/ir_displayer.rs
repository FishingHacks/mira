use crate::globals::GlobalStr;

use super::{
    expression::{TypecheckedExpression, TypedLiteral},
    Type, TypecheckedFunctionContract, TypecheckedModule, TypecheckingContext,
};
use std::fmt::{Debug, Display, Formatter, Result, Write};

macro_rules! display_to_debug {
    ($t:ty) => {
        impl Debug for $t {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result {
                Display::fmt(self, f)
            }
        }
    };
}

pub struct TCContextDisplay<'a>(pub &'a TypecheckingContext);
// TypedLiteralDisplay
#[repr(transparent)]
struct Tld<'a>(&'a TypedLiteral);
#[repr(transparent)]
struct ExpressionDisplay<'a>(&'a TypecheckedExpression);
#[repr(transparent)]
struct ModuleDisplay<'a>(&'a TypecheckedModule);
#[repr(transparent)]
struct FuncDisplay<'a>(&'a TypecheckedFunctionContract);
#[repr(transparent)]
struct ArgList<'a>(&'a [(GlobalStr, Type)]);
//struct Display<'a>(&'a TypecheckingContext);

macro_rules! format_tlds {
    ($f:ident $fmt:tt, $first: expr, $($value:expr),* $(,)?) => {
        $f.write_fmt(format_args!($fmt, $first$(, Tld($value))*))
    };
}

impl Display for TCContextDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for module in self.0.modules.read().iter() {
            Display::fmt(&ModuleDisplay(module), f)?;
            f.write_str("\n\n")?;
        }
        f.write_str("\n\n")?;
        for func in self.0.functions.read().iter() {
            Display::fmt(&FuncDisplay(&func.0), f)?;
            f.debug_list()
                .entries(func.1.iter().map(ExpressionDisplay))
                .finish()?;
            f.write_str("\n\n")?;
        }
        f.write_str("\n\n")?;
        for func in self.0.external_functions.read().iter() {
            f.write_str("External")?;
            Display::fmt(&FuncDisplay(&func.0), f)?;
            if let Some(ref exprs) = func.1 {
                f.debug_list()
                    .entries(exprs.iter().map(ExpressionDisplay))
                    .finish()?;
            }
            f.write_str("\n\n")?;
        }
        f.write_str("\n\n")?;
        for structure in self.0.structs.read().iter() {
            f.debug_struct("struct")
                .field("name", &structure.name)
                .field("annotations", &structure.annotations)
                .field("global_impl", &structure.global_impl)
                .field("trait_impl", &structure.trait_impl)
                .field("elements", &structure.elements)
                .field("module_id", &structure.module_id)
                .finish()?;
            f.write_str("\n\n")?;
        }
        f.write_str("\n\n")?;

        Ok(())
    }
}

impl Display for FuncDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let mut dbg_struct = f.debug_struct("FunctionContract");
        if let Some(ref v) = self.0.name {
            dbg_struct.field("name", v);
        }
        dbg_struct.field("returns", &DbgDsp(&self.0.return_type));
        dbg_struct.field("arguments", &ArgList(&self.0.arguments));
        dbg_struct.field("annotations", &self.0.annotations);
        dbg_struct.field("module_id", &self.0.module_id);

        dbg_struct.finish()
    }
}

impl Display for ArgList<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_char('(')?;
        for i in 0..self.0.len() {
            if i != 0 {
                f.write_str(", ")?;
            }
            f.write_char('_')?;
            Display::fmt(&i, f)?;
            f.write_str(": ")?;
            Display::fmt(&self.0[i].1, f)?;
        }
        f.write_char(')')
    }
}

impl Display for ModuleDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let mut dbg_struct = f.debug_struct("mod ");
        dbg_struct.field("%exports", &self.0.exports);
        for (k, v) in self.0.scope.iter() {
            k.with(|k| _ = dbg_struct.field(k, v));
        }
        dbg_struct.finish()
    }
}

impl Display for ExpressionDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self.0 {
            TypecheckedExpression::Unreachable(_) => f.write_str("unreachable"),
            TypecheckedExpression::DeclareVariable(_, id, typ, name) => {
                // let <name>: <ty> = _<id>
                f.write_str("let ")?;
                Display::fmt(name, f)?;
                f.write_str(": ")?;
                Display::fmt(typ, f)?;
                f.write_str(" = _")?;
                Display::fmt(id, f)
            }
            TypecheckedExpression::Asm {
                dst,
                inputs,
                registers,
                volatile,
                asm,
                ..
            } => {
                f.write_char('_')?;
                Display::fmt(dst, f)?;
                f.write_str(" = asm ")?;
                if *volatile {
                    f.write_str("volatile ")?;
                }
                f.write_char('(')?;
                for line in asm.split('\n') {
                    Debug::fmt(line, f)?;
                    f.write_char('\n')?;
                }
                f.write_str(": ")?;
                Debug::fmt(registers, f)?;
                f.write_str(": (")?;
                for (i, input) in inputs.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?
                    }
                    f.write_char('_')?;
                    Display::fmt(input, f)?;
                }
                f.write_str(")\n)")
            }
            TypecheckedExpression::Return(_, typed_literal) => {
                f.write_fmt(format_args!("return {}", Tld(typed_literal)))
            }
            TypecheckedExpression::Block(_, exprs, annotations) => {
                Display::fmt(annotations, f)?;
                f.write_str("block ")?;
                f.debug_list()
                    .entries(exprs.iter().map(ExpressionDisplay))
                    .finish()
            }
            TypecheckedExpression::If {
                cond,
                if_block,
                else_block,
                ..
            } => {
                f.write_str("if ")?;
                Display::fmt(&Tld(cond), f)?;
                f.debug_list()
                    .entries(if_block.0.iter().map(ExpressionDisplay))
                    .finish()?;
                if let Some(else_block) = else_block {
                    f.write_str("\nelse ")?;
                    f.debug_list()
                        .entries(else_block.0.iter().map(ExpressionDisplay))
                        .finish()?;
                }
                Ok(())
            }
            TypecheckedExpression::While {
                cond_block,
                cond,
                body,
                ..
            } => {
                f.write_str("while_cond ")?;
                f.debug_list()
                    .entries(cond_block.iter().map(ExpressionDisplay))
                    .finish()?;
                f.write_str("while ")?;
                Display::fmt(&Tld(cond), f)?;
                f.debug_list()
                    .entries(body.0.iter().map(ExpressionDisplay))
                    .finish()?;
                Ok(())
            }
            TypecheckedExpression::Range {
                lhs,
                rhs,
                inclusive,
                dst,
                ..
            } => {
                f.write_char('_')?;
                Display::fmt(dst, f)?;
                f.write_str(" = ")?;
                Display::fmt(&Tld(lhs), f)?;
                f.write_str("..")?;
                if *inclusive {
                    f.write_char('=')?;
                }
                Display::fmt(&Tld(rhs), f)
            }
            TypecheckedExpression::StoreAssignment(_, lhs, rhs) => {
                format_tlds!(f "{}*{} = {}", "", lhs, rhs)
            }
            TypecheckedExpression::Call(_, lhs, rhs, vec) => {
                f.write_fmt(format_args!("_{} = {}(", lhs, Tld(rhs)))?;
                for arg in vec {
                    Display::fmt(&Tld(arg), f)?;
                    f.write_char(',')?;
                }
                f.write_char(')')
            }
            TypecheckedExpression::DirectCall(_, typed_literal, func, vec) => {
                f.write_fmt(format_args!(
                    "_{} = {}(",
                    typed_literal,
                    Tld(&TypedLiteral::Function(*func))
                ))?;
                for arg in vec {
                    Display::fmt(&Tld(arg), f)?;
                    f.write_char(',')?;
                }
                f.write_char(')')
            }
            TypecheckedExpression::DirectExternCall(_, typed_literal, func, vec) => {
                f.write_fmt(format_args!(
                    "_{} = {}(",
                    typed_literal,
                    Tld(&TypedLiteral::ExternalFunction(*func))
                ))?;
                for arg in vec {
                    Display::fmt(&Tld(arg), f)?;
                    f.write_char(',')?;
                }
                f.write_char(')')
            }
            TypecheckedExpression::IntrinsicCall(_, typed_literal, intrinsic, vec) => {
                f.write_fmt(format_args!(
                    "_{} = {}(",
                    typed_literal,
                    Tld(&TypedLiteral::Intrinsic(*intrinsic))
                ))?;
                for arg in vec {
                    Display::fmt(&Tld(arg), f)?;
                    f.write_char(',')?;
                }
                f.write_char(')')
            }
            TypecheckedExpression::Pos(_, lhs, rhs) => format_tlds!(f "{} = +{}", lhs, rhs),
            TypecheckedExpression::Neg(_, lhs, rhs) => format_tlds!(f "{} = -{}", lhs, rhs),
            TypecheckedExpression::LNot(_, lhs, rhs) => format_tlds!(f "{} = !{}", lhs, rhs),
            TypecheckedExpression::BNot(_, lhs, rhs) => format_tlds!(f "{} = ~{}", lhs, rhs),
            TypecheckedExpression::Add(_, dst, lhs, rhs) => {
                format_tlds!(f "{} = {} + {}", dst, lhs, rhs)
            }
            TypecheckedExpression::Sub(_, dst, lhs, rhs) => {
                format_tlds!(f "{} = {} - {}", dst, lhs, rhs)
            }
            TypecheckedExpression::Mul(_, dst, lhs, rhs) => {
                format_tlds!(f "{} = {} * {}", dst, lhs, rhs)
            }
            TypecheckedExpression::Div(_, dst, lhs, rhs) => {
                format_tlds!(f "{} = {} / {}", dst, lhs, rhs)
            }
            TypecheckedExpression::Mod(_, dst, lhs, rhs) => {
                format_tlds!(f "{} = {} % {}", dst, lhs, rhs)
            }
            TypecheckedExpression::BAnd(_, dst, lhs, rhs) => {
                format_tlds!(f "{} = {} & {}", dst, lhs, rhs)
            }
            TypecheckedExpression::BOr(_, dst, lhs, rhs) => {
                format_tlds!(f "{} = {} | {}", dst, lhs, rhs)
            }
            TypecheckedExpression::BXor(_, dst, lhs, rhs) => {
                format_tlds!(f "{} = {} ^ {}", dst, lhs, rhs)
            }
            TypecheckedExpression::GreaterThan(_, dst, lhs, rhs) => {
                format_tlds!(f "{} = {} > {}", dst, lhs, rhs)
            }
            TypecheckedExpression::LessThan(_, dst, lhs, rhs) => {
                format_tlds!(f "{} = {} < {}", dst, lhs, rhs)
            }
            TypecheckedExpression::LAnd(_, dst, lhs, rhs) => {
                format_tlds!(f "{} = {} && {}", dst, lhs, rhs)
            }
            TypecheckedExpression::LOr(_, dst, lhs, rhs) => {
                format_tlds!(f "{} = {} || {}", dst, lhs, rhs)
            }
            TypecheckedExpression::GreaterThanEq(_, dst, lhs, rhs) => {
                format_tlds!(f "{} = {} >= {}", dst, lhs, rhs)
            }
            TypecheckedExpression::LessThanEq(_, dst, lhs, rhs) => {
                format_tlds!(f "{} = {} <= {}", dst, lhs, rhs)
            }
            TypecheckedExpression::Eq(_, dst, lhs, rhs) => {
                format_tlds!(f "{} = {} == {}", dst, lhs, rhs)
            }
            TypecheckedExpression::Neq(_, dst, lhs, rhs) => {
                format_tlds!(f "{} = {} != {}", dst, lhs, rhs)
            }
            TypecheckedExpression::LShift(_, dst, lhs, rhs) => {
                format_tlds!(f "{} = {} << {}", dst, lhs, rhs)
            }
            TypecheckedExpression::RShift(_, dst, lhs, rhs) => {
                format_tlds!(f "{} = {} >> {}", dst, lhs, rhs)
            }
            TypecheckedExpression::Reference(_, lhs, rhs) => format_tlds!(f "_{} = &{}", lhs, rhs),
            TypecheckedExpression::Dereference(_, lhs, rhs) => {
                f.write_fmt(format_args!("_{} = *{}", lhs, Tld(rhs)))
            }
            TypecheckedExpression::Offset(_, lhs, rhs, offsets) => {
                f.write_fmt(format_args!("_{} = offset({}, {offsets:?})", lhs, Tld(rhs),))
            }
            TypecheckedExpression::OffsetNonPointer(_, lhs, rhs, offset_value) => f.write_fmt(
                format_args!("_{} = offset_non_ptr({}, {offset_value})", lhs, Tld(rhs)),
            ),
            TypecheckedExpression::DynCall(_, dst, args, offset) => {
                f.write_char('_')?;
                Display::fmt(dst, f)?;
                f.write_str(" = dyncall(")?;
                Display::fmt(offset, f)?;
                for arg in args {
                    f.write_str(", ")?;
                    Display::fmt(&Tld(arg), f)?;
                }
                f.write_char(')')
            }
            TypecheckedExpression::Bitcast(_, lhs, rhs)
            | TypecheckedExpression::PtrToInt(_, lhs, rhs)
            | TypecheckedExpression::IntToPtr(_, lhs, rhs)
            | TypecheckedExpression::Alias(_, lhs, rhs)
            | TypecheckedExpression::StripMetadata(_, lhs, rhs)
            | TypecheckedExpression::IntCast(_, lhs, rhs) => {
                f.write_fmt(format_args!("_{lhs} = {} as <unknown type T>", Tld(rhs)))
            }
            TypecheckedExpression::Literal(_, lhs, rhs) => {
                f.write_fmt(format_args!("let _{} = {}", lhs, Tld(rhs)))
            }
            TypecheckedExpression::AttachVtable(_, lhs, rhs, (_, traits)) => f.write_fmt(
                format_args!("_{lhs} = attach_vtable({}, {traits:?})", Tld(rhs)),
            ),
            TypecheckedExpression::MakeUnsizedSlice(_, lhs, rhs, size) => f.write_fmt(
                format_args!("_{lhs} = attach_size_metadata({}, {size})", Tld(rhs)),
            ),
            TypecheckedExpression::Empty(_) => f.write_str("<removed>"),
            TypecheckedExpression::None => f.write_str("<none>"),
        }
    }
}

impl Display for Tld<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self.0 {
            TypedLiteral::Void => f.write_str("void"),
            TypedLiteral::Dynamic(id) => f.write_fmt(format_args!("_{id}")),
            TypedLiteral::Function(id) => f.write_fmt(format_args!("f_{id}")),
            TypedLiteral::ExternalFunction(id) => f.write_fmt(format_args!("extf_{id}")),
            TypedLiteral::Static(id) => f.write_fmt(format_args!("%{id}")),
            TypedLiteral::String(global_str) => Debug::fmt(global_str, f),
            TypedLiteral::Array(_, vec) => f.debug_list().entries(vec.iter().map(Tld)).finish(),
            TypedLiteral::ArrayInit(_, elem, amount) => {
                f.write_char('[')?;
                Display::fmt(&Tld(elem), f)?;
                f.write_str("; ")?;
                Display::fmt(amount, f)?;
                f.write_char(']')
            }
            TypedLiteral::Struct(id, vec) => {
                f.write_str("s_")?;
                Display::fmt(id, f)?;
                f.write_char(' ')?;
                f.debug_list().entries(vec.iter().map(Tld)).finish()
            }
            TypedLiteral::Tuple(vec) => {
                f.write_str("tuple ")?;
                f.debug_list().entries(vec.iter().map(Tld)).finish()
            }
            TypedLiteral::F64(v) => Display::fmt(v, f),
            TypedLiteral::F32(v) => Display::fmt(v, f),
            TypedLiteral::U8(v) => Display::fmt(v, f),
            TypedLiteral::U16(v) => Display::fmt(v, f),
            TypedLiteral::U32(v) => Display::fmt(v, f),
            TypedLiteral::U64(v) => Display::fmt(v, f),
            TypedLiteral::USize(v) => Display::fmt(v, f),
            TypedLiteral::I8(v) => Display::fmt(v, f),
            TypedLiteral::I16(v) => Display::fmt(v, f),
            TypedLiteral::I32(v) => Display::fmt(v, f),
            TypedLiteral::I64(v) => Display::fmt(v, f),
            TypedLiteral::ISize(v) => Display::fmt(v, f),
            TypedLiteral::Bool(v) => Display::fmt(v, f),
            TypedLiteral::Intrinsic(intrinsic) => {
                f.write_fmt(format_args!("Intrinsic::{intrinsic}"))
            }
        }
    }
}

display_to_debug!(Tld<'_>);
display_to_debug!(ExpressionDisplay<'_>);
display_to_debug!(ArgList<'_>);

struct DbgDsp<'a, T: Display>(&'a T);
impl<T: Display> Debug for DbgDsp<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        Display::fmt(self.0, f)
    }
}
