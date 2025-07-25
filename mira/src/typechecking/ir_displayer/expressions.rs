use crate::typechecking::expression::{TypecheckedExpression, TypedLiteral};

use super::{formatter::Formatter, typed_literal::Tld};

#[repr(transparent)]
pub struct ExpressionDisplay<'a>(pub &'a TypecheckedExpression<'a>);

macro_rules! expr {
    (binary $f:ident, $dst:expr, $lhs:expr, $rhs:expr, $operator:literal) => {{
        let _: &usize = $dst;
        let _: &TypedLiteral = $lhs;
        let _: &TypedLiteral = $rhs;
        $f.write_char('_')?;
        $f.write_value($dst)?;
        $f.write_str(" = ")?;
        Tld($lhs).fmt($f)?;
        $f.write_str(concat!(" ", $operator, " "))?;
        Tld($rhs).fmt($f)
    }};

    (unary $f:ident, $dst:expr, $lhs:expr, $operator:literal) => {{
        let _: &usize = $dst;
        let _: &TypedLiteral = $lhs;
        $f.write_char('_')?;
        $f.write_value($dst)?;
        $f.write_str(concat!(" = ", $operator))?;
        Tld($lhs).fmt($f)
    }};
}

impl ExpressionDisplay<'_> {
    pub fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self.0 {
            TypecheckedExpression::Unreachable(_) => f.write_str("unreachable"),
            TypecheckedExpression::DeclareVariable(_, id, typ, name) => {
                // let <name>: <ty> = _<id>
                f.write_str("declare ")?;
                f.write_value(name)?;
                f.write_str(": ")?;
                f.write_value(typ)?;
                f.write_str(" = _")?;
                f.write_value(id)
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
                f.write_value(dst)?;
                f.write_str(" = asm")?;
                if *volatile {
                    f.write_str(" volatile")?;
                }
                f.write_char('(')?;
                f.push_indent();
                for line in asm.split('\n') {
                    f.write_char('\n')?;
                    f.write_debug(&line)?;
                }
                // if the registers are empty, inputs must also be empty
                if !registers.is_empty() {
                    f.write_str("\n: ")?;
                    f.write_debug(registers)?;
                }
                if !inputs.is_empty() {
                    f.write_str("\n: (")?;
                    for (i, input) in inputs.iter().enumerate() {
                        if i != 0 {
                            f.write_str(", ")?
                        }
                        f.write_char('_')?;
                        f.write_value(input)?;
                    }
                    f.write_char(')')?;
                }
                f.pop_indent();
                f.write_str("\n)")
            }
            TypecheckedExpression::Return(_, typed_literal) => {
                f.write_str("return ")?;
                Tld(typed_literal).fmt(f)
            }
            TypecheckedExpression::Block(_, exprs, annotations) => {
                f.write_value(annotations)?;
                write_block(f, exprs)
            }
            TypecheckedExpression::If {
                cond,
                if_block,
                else_block,
                ..
            } => {
                f.write_str("if ")?;
                Tld(cond).fmt(f)?;
                f.write_char(' ')?;
                write_implicit_block(f, &if_block.0)?;
                if let Some(else_block) = else_block {
                    f.write_str(" else ")?;
                    write_implicit_block(f, &else_block.0)?;
                }
                Ok(())
            }
            TypecheckedExpression::While {
                cond_block,
                cond,
                body,
                ..
            } => {
                f.write_str("while_cond_block ")?;
                write_implicit_block(f, cond_block)?;
                f.write_str("\nwhile ")?;
                Tld(cond).fmt(f)?;
                f.write_char(' ')?;
                write_implicit_block(f, &body.0)
            }
            TypecheckedExpression::Range {
                lhs,
                rhs,
                inclusive,
                dst,
                ..
            } => {
                f.write_char('_')?;
                f.write_value(dst)?;
                f.write_str(" = ")?;
                Tld(lhs).fmt(f)?;
                f.write_str("..")?;
                if *inclusive {
                    f.write_char('=')?;
                }
                Tld(rhs).fmt(f)
            }
            TypecheckedExpression::StoreAssignment(_, lhs, rhs) => {
                f.write_char('*')?;
                Tld(lhs).fmt(f)?;
                f.write_str(" = ")?;
                Tld(rhs).fmt(f)
            }
            TypecheckedExpression::Call(_, lhs, rhs, vec) => {
                f.write_char('_')?;
                f.write_value(lhs)?;
                f.write_str(" = ")?;
                Tld(rhs).fmt(f)?;
                for (idx, arg) in vec.iter().enumerate() {
                    if idx != 0 {
                        f.write_str(", ")?;
                    }
                    Tld(arg).fmt(f)?;
                }
                f.write_char(')')
            }
            TypecheckedExpression::DirectCall(_, dst, func, vec, _) => {
                f.write_char('_')?;
                f.write_value(dst)?;
                f.write_str(" = ")?;
                Tld(&TypedLiteral::Function(*func, Vec::new())).fmt(f)?;
                f.write_char('(')?;
                for (idx, arg) in vec.iter().enumerate() {
                    if idx != 0 {
                        f.write_str(", ")?;
                    }
                    Tld(arg).fmt(f)?;
                }
                f.write_char(')')
            }
            TypecheckedExpression::DirectExternCall(_, dst, func, vec) => {
                f.write_char('_')?;
                f.write_value(dst)?;
                f.write_str(" = ")?;
                Tld(&TypedLiteral::ExternalFunction(*func)).fmt(f)?;
                f.write_char('(')?;
                for (idx, arg) in vec.iter().enumerate() {
                    if idx != 0 {
                        f.write_str(", ")?;
                    }
                    Tld(arg).fmt(f)?;
                }
                f.write_char(')')
            }
            TypecheckedExpression::IntrinsicCall(_, dst, intrinsic, vec, _) => {
                f.write_char('_')?;
                f.write_value(dst)?;
                f.write_str(" = ")?;
                Tld(&TypedLiteral::Intrinsic(*intrinsic, Vec::new())).fmt(f)?;
                f.write_char('(')?;
                for (idx, arg) in vec.iter().enumerate() {
                    if idx != 0 {
                        f.write_str(", ")?;
                    }
                    Tld(arg).fmt(f)?;
                }
                f.write_char(')')
            }
            TypecheckedExpression::Pos(_, lhs, rhs) => expr!(unary f, lhs, rhs, "+"),
            TypecheckedExpression::Neg(_, lhs, rhs) => expr!(unary f, lhs, rhs, "-"),
            TypecheckedExpression::LNot(_, lhs, rhs) => expr!(unary f, lhs, rhs, "!"),
            TypecheckedExpression::BNot(_, lhs, rhs) => expr!(unary f, lhs, rhs, "~"),
            TypecheckedExpression::Add(_, dst, lhs, rhs) => expr!(binary f, dst, lhs, rhs, "+"),
            TypecheckedExpression::Sub(_, dst, lhs, rhs) => expr!(binary f, dst, lhs, rhs, "-"),
            TypecheckedExpression::Mul(_, dst, lhs, rhs) => expr!(binary f, dst, lhs, rhs, "*"),
            TypecheckedExpression::Div(_, dst, lhs, rhs) => expr!(binary f, dst, lhs, rhs, "/"),
            TypecheckedExpression::Mod(_, dst, lhs, rhs) => expr!(binary f, dst, lhs, rhs, "%"),
            TypecheckedExpression::BAnd(_, dst, lhs, rhs) => expr!(binary f, dst, lhs, rhs, "&"),
            TypecheckedExpression::BOr(_, dst, lhs, rhs) => expr!(binary f, dst, lhs, rhs, "|"),
            TypecheckedExpression::BXor(_, dst, lhs, rhs) => expr!(binary f, dst, lhs, rhs, "^"),
            TypecheckedExpression::GreaterThan(_, dst, lhs, rhs) => {
                expr!(binary f, dst, lhs, rhs, ">")
            }
            TypecheckedExpression::LessThan(_, dst, lhs, rhs) => {
                expr!(binary f, dst, lhs, rhs, "<")
            }
            TypecheckedExpression::LAnd(_, dst, lhs, rhs) => expr!(binary f, dst, lhs, rhs, "&&"),
            TypecheckedExpression::LOr(_, dst, lhs, rhs) => expr!(binary f, dst, lhs, rhs, "||"),
            TypecheckedExpression::GreaterThanEq(_, dst, lhs, rhs) => {
                expr!(binary f, dst, lhs, rhs, ">=")
            }
            TypecheckedExpression::LessThanEq(_, dst, lhs, rhs) => {
                expr!(binary f, dst, lhs, rhs, "<=")
            }
            TypecheckedExpression::Eq(_, dst, lhs, rhs) => expr!(binary f, dst, lhs, rhs, "=="),
            TypecheckedExpression::Neq(_, dst, lhs, rhs) => expr!(binary f, dst, lhs, rhs, "!="),
            TypecheckedExpression::LShift(_, dst, lhs, rhs) => expr!(binary f, dst, lhs, rhs, "<<"),
            TypecheckedExpression::RShift(_, dst, lhs, rhs) => expr!(binary f, dst, lhs, rhs, ">>"),
            TypecheckedExpression::Reference(_, lhs, rhs) => expr!(unary f, lhs, rhs, "&"),
            TypecheckedExpression::Dereference(_, lhs, rhs) => expr!(unary f, lhs, rhs, "*"),
            TypecheckedExpression::Offset(_, lhs, rhs, offsets) => {
                f.write_char('_')?;
                f.write_value(lhs)?;
                f.write_str(" = offset(")?;
                Tld(rhs).fmt(f)?;
                f.write_str(", ")?;
                f.write_debug(offsets)?;
                f.write_char(')')
            }
            TypecheckedExpression::OffsetNonPointer(_, lhs, rhs, offset_value) => {
                f.write_char('_')?;
                f.write_value(lhs)?;
                f.write_str(" = offset_non_ptr(")?;
                Tld(rhs).fmt(f)?;
                f.write_str(", ")?;
                f.write_value(offset_value)?;
                f.write_char(')')
            }
            TypecheckedExpression::DynCall(_, dst, args, offset) => {
                f.write_char('_')?;
                f.write_value(dst)?;
                f.write_str(" = dyncall(")?;
                f.write_value(offset)?;
                for arg in args {
                    f.write_str(", ")?;
                    Tld(arg).fmt(f)?;
                }
                f.write_char(')')
            }
            TypecheckedExpression::Bitcast(_, lhs, rhs)
            | TypecheckedExpression::PtrToInt(_, lhs, rhs)
            | TypecheckedExpression::IntToPtr(_, lhs, rhs)
            | TypecheckedExpression::Alias(_, lhs, rhs)
            | TypecheckedExpression::StripMetadata(_, lhs, rhs)
            | TypecheckedExpression::IntCast(_, lhs, rhs) => {
                f.write_char('_')?;
                f.write_value(lhs)?;
                f.write_str(" = ")?;
                Tld(rhs).fmt(f)?;
                f.write_str(" as <unknown type T>")
            }
            TypecheckedExpression::Literal(_, lhs, rhs) => {
                f.write_str("_")?;
                f.write_value(lhs)?;
                f.write_str(" = ")?;
                Tld(rhs).fmt(f)
            }
            TypecheckedExpression::AttachVtable(_, lhs, rhs, (_, traits)) => {
                f.write_char('_')?;
                f.write_value(lhs)?;
                f.write_str(" = attach_vtable(")?;
                Tld(rhs).fmt(f)?;
                f.write_str(", ")?;
                f.write_debug(traits)?;
                f.write_char(')')
            }
            TypecheckedExpression::MakeUnsizedSlice(_, lhs, rhs, size) => {
                f.write_char('_')?;
                f.write_value(lhs)?;
                f.write_str(" = attach_size_metadata(")?;
                Tld(rhs).fmt(f)?;
                f.write_str(", ")?;
                f.write_value(size)?;
                f.write_char(')')
            }
            TypecheckedExpression::Empty(_) => f.write_str("<removed>"),
            TypecheckedExpression::None => f.write_str("<none>"),
        }
    }
}

pub fn write_block(f: &mut Formatter, exprs: &[TypecheckedExpression]) -> std::fmt::Result {
    f.write_char('{')?;
    f.push_indent();
    for expr in exprs {
        f.write_char('\n')?;
        ExpressionDisplay(expr).fmt(f)?;
    }
    f.pop_indent();
    f.write_str("\n}")
}

// writes a block to `f` unless the block is a single expression of type
// `TypecheckedExpression::Block`, in which case it will print the block.
pub fn write_implicit_block(
    f: &mut Formatter,
    exprs: &[TypecheckedExpression],
) -> std::fmt::Result {
    if exprs.len() == 1 && matches!(exprs[0], TypecheckedExpression::Block(..)) {
        ExpressionDisplay(&exprs[0]).fmt(f)
    } else {
        write_block(f, exprs)
    }
}
