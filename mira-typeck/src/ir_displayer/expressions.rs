use crate::ir::{BlockId, IR, ValueId};
use crate::ir::{TypedExpression, TypedLiteral};
use crate::types::EMPTY_TYLIST;

use super::{formatter::Formatter, typed_literal::Tld};

#[repr(transparent)]
pub(super) struct ExpressionDisplay<'a>(pub &'a TypedExpression<'a>);

macro_rules! expr {
    (binary $f:ident, $dst:expr, $lhs:expr, $rhs:expr, $operator:literal) => {{
        let _: &ValueId = $dst;
        let _: &TypedLiteral<'_> = $lhs;
        let _: &TypedLiteral<'_> = $rhs;
        $f.write_value($dst)?;
        $f.write_str(" = ")?;
        Tld($lhs).fmt($f)?;
        $f.write_str(concat!(" ", $operator, " "))?;
        Tld($rhs).fmt($f)
    }};

    (unary $f:ident, $dst:expr, $lhs:expr, $operator:literal) => {{
        let _: &ValueId = $dst;
        let _: &TypedLiteral<'_> = $lhs;
        $f.write_value($dst)?;
        $f.write_str(concat!(" = ", $operator))?;
        Tld($lhs).fmt($f)
    }};
}

impl ExpressionDisplay<'_> {
    pub(super) fn fmt(&self, f: &mut Formatter<'_, '_, '_>, ir: &IR<'_>) -> std::fmt::Result {
        match self.0 {
            TypedExpression::Unreachable(_) => f.write_str("unreachable"),
            TypedExpression::DeclareVariable(_, id, ty, name) => {
                // let <name>: <ty> = _<id>
                f.write_str("declare ")?;
                f.write_value(name)?;
                f.write_str(": ")?;
                f.write_value(ty)?;
                f.write_str(" = _")?;
                f.write_value(id)
            }
            TypedExpression::Asm {
                dst,
                inputs,
                registers,
                volatile,
                asm,
                ..
            } => {
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
                        f.write_value(input)?;
                    }
                    f.write_char(')')?;
                }
                f.pop_indent();
                f.write_str("\n)")
            }
            TypedExpression::Return(_, typed_literal) => {
                f.write_str("return ")?;
                Tld(typed_literal).fmt(f)
            }
            TypedExpression::Block(_, block, annotations) => {
                f.write_value(annotations)?;
                write_block(f, *block, ir)
            }
            TypedExpression::If {
                cond,
                if_block,
                else_block,
                ..
            } => {
                f.write_str("if ")?;
                Tld(cond).fmt(f)?;
                f.write_char(' ')?;
                write_implicit_block(f, *if_block, ir)?;
                if let Some(else_block) = else_block {
                    f.write_str(" else ")?;
                    write_implicit_block(f, *else_block, ir)?;
                }
                Ok(())
            }
            TypedExpression::While {
                cond_block,
                cond,
                body,
                ..
            } => {
                f.write_str("while_cond_block ")?;
                write_implicit_block(f, *cond_block, ir)?;
                f.write_str("\nwhile ")?;
                Tld(cond).fmt(f)?;
                f.write_char(' ')?;
                write_implicit_block(f, *body, ir)
            }
            TypedExpression::Range {
                lhs,
                rhs,
                inclusive,
                dst,
                ..
            } => {
                f.write_value(dst)?;
                f.write_str(" = ")?;
                Tld(lhs).fmt(f)?;
                f.write_str("..")?;
                if *inclusive {
                    f.write_char('=')?;
                }
                Tld(rhs).fmt(f)
            }
            TypedExpression::StoreAssignment(_, lhs, rhs) => {
                f.write_char('*')?;
                Tld(lhs).fmt(f)?;
                f.write_str(" = ")?;
                Tld(rhs).fmt(f)
            }
            TypedExpression::Call(_, lhs, rhs, args) => {
                f.write_value(lhs)?;
                f.write_str(" = ")?;
                Tld(rhs).fmt(f)?;
                for (idx, arg) in args.iter().enumerate() {
                    if idx != 0 {
                        f.write_str(", ")?;
                    }
                    Tld(arg).fmt(f)?;
                }
                f.write_char(')')
            }
            TypedExpression::DirectCall(_, dst, func, args, _) => {
                f.write_value(dst)?;
                f.write_str(" = ")?;
                Tld(&TypedLiteral::Function(*func, EMPTY_TYLIST)).fmt(f)?;
                f.write_char('(')?;
                for (idx, arg) in args.iter().enumerate() {
                    if idx != 0 {
                        f.write_str(", ")?;
                    }
                    Tld(arg).fmt(f)?;
                }
                f.write_char(')')
            }
            TypedExpression::DirectExternCall(_, dst, func, args) => {
                f.write_value(dst)?;
                f.write_str(" = ")?;
                Tld(&TypedLiteral::ExternalFunction(*func)).fmt(f)?;
                f.write_char('(')?;
                for (idx, arg) in args.iter().enumerate() {
                    if idx != 0 {
                        f.write_str(", ")?;
                    }
                    Tld(arg).fmt(f)?;
                }
                f.write_char(')')
            }
            TypedExpression::LLVMIntrinsicCall(_, dst, intrinsic, args) => {
                f.write_value(dst)?;
                f.write_str(" = ")?;
                Tld(&TypedLiteral::LLVMIntrinsic(*intrinsic)).fmt(f)?;
                f.write_char('(')?;
                for (idx, arg) in args.iter().enumerate() {
                    if idx != 0 {
                        f.write_str(", ")?;
                    }
                    Tld(arg).fmt(f)?;
                }
                f.write_char(')')
            }
            TypedExpression::IntrinsicCall(_, dst, intrinsic, args, _) => {
                f.write_value(dst)?;
                f.write_str(" = ")?;
                Tld(&TypedLiteral::Intrinsic(*intrinsic, EMPTY_TYLIST)).fmt(f)?;
                f.write_char('(')?;
                for (idx, arg) in args.iter().enumerate() {
                    if idx != 0 {
                        f.write_str(", ")?;
                    }
                    Tld(arg).fmt(f)?;
                }
                f.write_char(')')
            }
            TypedExpression::Pos(_, lhs, rhs) => expr!(unary f, lhs, rhs, "+"),
            TypedExpression::Neg(_, lhs, rhs) => expr!(unary f, lhs, rhs, "-"),
            TypedExpression::LNot(_, lhs, rhs) => expr!(unary f, lhs, rhs, "!"),
            TypedExpression::BNot(_, lhs, rhs) => expr!(unary f, lhs, rhs, "~"),
            TypedExpression::Add(_, dst, lhs, rhs) => expr!(binary f, dst, lhs, rhs, "+"),
            TypedExpression::Sub(_, dst, lhs, rhs) => expr!(binary f, dst, lhs, rhs, "-"),
            TypedExpression::Mul(_, dst, lhs, rhs) => expr!(binary f, dst, lhs, rhs, "*"),
            TypedExpression::Div(_, dst, lhs, rhs) => expr!(binary f, dst, lhs, rhs, "/"),
            TypedExpression::Mod(_, dst, lhs, rhs) => expr!(binary f, dst, lhs, rhs, "%"),
            TypedExpression::BAnd(_, dst, lhs, rhs) => expr!(binary f, dst, lhs, rhs, "&"),
            TypedExpression::BOr(_, dst, lhs, rhs) => expr!(binary f, dst, lhs, rhs, "|"),
            TypedExpression::BXor(_, dst, lhs, rhs) => expr!(binary f, dst, lhs, rhs, "^"),
            TypedExpression::GreaterThan(_, dst, lhs, rhs) => {
                expr!(binary f, dst, lhs, rhs, ">")
            }
            TypedExpression::LessThan(_, dst, lhs, rhs) => {
                expr!(binary f, dst, lhs, rhs, "<")
            }
            TypedExpression::LAnd(_, dst, lhs, rhs, blk) => {
                f.write_value(dst)?;
                f.write_str(" = ")?;
                Tld(lhs).fmt(f)?;
                f.write_str(" && ")?;
                if !ir.get_block_exprs(*blk).is_empty() {
                    write_block(f, *blk, ir)?;
                    f.write_char(' ')?;
                }
                Tld(rhs).fmt(f)
            }
            TypedExpression::LOr(_, dst, lhs, rhs, blk) => {
                f.write_value(dst)?;
                f.write_str(" = ")?;
                Tld(lhs).fmt(f)?;
                f.write_str(" || ")?;
                if !ir.get_block_exprs(*blk).is_empty() {
                    write_block(f, *blk, ir)?;
                    f.write_char(' ')?;
                }
                Tld(rhs).fmt(f)
            }
            TypedExpression::GreaterThanEq(_, dst, lhs, rhs) => {
                expr!(binary f, dst, lhs, rhs, ">=")
            }
            TypedExpression::LessThanEq(_, dst, lhs, rhs) => {
                expr!(binary f, dst, lhs, rhs, "<=")
            }
            TypedExpression::Eq(_, dst, lhs, rhs) => expr!(binary f, dst, lhs, rhs, "=="),
            TypedExpression::Neq(_, dst, lhs, rhs) => expr!(binary f, dst, lhs, rhs, "!="),
            TypedExpression::LShift(_, dst, lhs, rhs) => expr!(binary f, dst, lhs, rhs, "<<"),
            TypedExpression::RShift(_, dst, lhs, rhs) => expr!(binary f, dst, lhs, rhs, ">>"),
            TypedExpression::Reference(_, lhs, rhs) => expr!(unary f, lhs, rhs, "&"),
            TypedExpression::Dereference(_, lhs, rhs) => expr!(unary f, lhs, rhs, "*"),
            TypedExpression::Offset(_, lhs, rhs, offsets) => {
                f.write_value(lhs)?;
                f.write_str(" = offset(")?;
                Tld(rhs).fmt(f)?;
                f.write_str(", ")?;
                f.write_debug(offsets)?;
                f.write_char(')')
            }
            TypedExpression::OffsetNonPointer(_, lhs, rhs, offset_value) => {
                f.write_value(lhs)?;
                f.write_str(" = offset_non_ptr(")?;
                Tld(rhs).fmt(f)?;
                f.write_str(", ")?;
                f.write_value(offset_value)?;
                f.write_char(')')
            }
            TypedExpression::DynCall(_, dst, args, offset) => {
                f.write_value(dst)?;
                f.write_str(" = dyncall(")?;
                f.write_value(offset)?;
                for arg in args {
                    f.write_str(", ")?;
                    Tld(arg).fmt(f)?;
                }
                f.write_char(')')
            }
            TypedExpression::Bitcast(_, lhs, rhs)
            | TypedExpression::PtrToInt(_, lhs, rhs)
            | TypedExpression::IntToPtr(_, lhs, rhs)
            | TypedExpression::Alias(_, lhs, rhs)
            | TypedExpression::StripMetadata(_, lhs, rhs)
            | TypedExpression::IntCast(_, lhs, rhs) => {
                f.write_value(lhs)?;
                f.write_str(" = ")?;
                Tld(rhs).fmt(f)?;
                f.write_str(" as ")?;
                f.write_value(&*ir.get_ty(*lhs))
            }
            TypedExpression::Literal(_, lhs, rhs) => {
                f.write_value(lhs)?;
                f.write_str(" = ")?;
                Tld(rhs).fmt(f)
            }
            TypedExpression::AttachVtable(_, lhs, rhs, (_, traits)) => {
                f.write_value(lhs)?;
                f.write_str(" = attach_vtable(")?;
                Tld(rhs).fmt(f)?;
                f.write_str(", ")?;
                f.write_debug(traits)?;
                f.write_char(')')
            }
            TypedExpression::MakeUnsizedSlice(_, lhs, rhs, size) => {
                f.write_value(lhs)?;
                f.write_str(" = attach_size_metadata(")?;
                Tld(rhs).fmt(f)?;
                f.write_str(", ")?;
                f.write_value(size)?;
                f.write_char(')')
            }
            TypedExpression::Drop(_, value) => {
                f.write_str("drop(_")?;
                f.write_value(value)?;
                f.write_char(')')
            }
            TypedExpression::DropIf(_, value, cond) => {
                f.write_str("if _")?;
                f.write_value(cond)?;
                f.write_str(" drop(_")?;
                f.write_value(value)?;
                f.write_char(')')
            }
            TypedExpression::TraitCall {
                ty,
                trait_id,
                func,
                args,
                dst,
                span: _,
            } => {
                f.write_value(dst)?;
                f.write_str(" = trait_call trait_")?;
                f.write_value(trait_id)?;
                f.write_str("::func_")?;
                f.write_value(func)?;
                f.write_str(" on ")?;
                f.write_value(ty)?;
                f.write_char('(')?;
                for (idx, arg) in args.iter().enumerate() {
                    if idx != 0 {
                        f.write_str(", ")?;
                    }
                    Tld(arg).fmt(f)?;
                }
                f.write_char(')')
            }
            TypedExpression::Empty(_) => f.write_str("<removed>"),
            TypedExpression::None => f.write_str("<none>"),
        }
    }
}

pub(super) fn write_block(
    f: &mut Formatter<'_, '_, '_>,
    block: BlockId,
    ir: &IR<'_>,
) -> std::fmt::Result {
    let exprs = ir.get_block_exprs(block);
    f.write_char('{')?;
    f.push_indent();
    for expr in exprs {
        f.write_char('\n')?;
        ExpressionDisplay(expr).fmt(f, ir)?;
    }
    f.pop_indent();
    f.write_str("\n}")
}

// writes a block to `f` unless the block is a single expression of type
// `TypedExpression::Block`, in which case it will print the block.
pub(super) fn write_implicit_block(
    f: &mut Formatter<'_, '_, '_>,
    block: BlockId,
    ir: &IR<'_>,
) -> std::fmt::Result {
    let exprs = ir.get_block_exprs(block);
    if exprs.len() == 1 && matches!(exprs[0], TypedExpression::Block(..)) {
        ExpressionDisplay(&exprs[0]).fmt(f, ir)
    } else {
        write_block(f, block, ir)
    }
}
