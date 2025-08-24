use std::collections::HashSet;

use crate::{
    TypecheckingContext, TypedFunction, TypedStatic,
    ir::{TypedExpression, TypedLiteral},
};
use mira_common::store::StoreKey;

struct DceContext<'tc, 'arena> {
    used_functions: HashSet<StoreKey<TypedFunction<'arena>>>,
    used_statics: HashSet<StoreKey<TypedStatic<'arena>>>,
    funcs_left: HashSet<StoreKey<TypedFunction<'arena>>>,
    tc_ctx: &'tc TypecheckingContext<'arena>,
}

pub fn run_dce<'arena>(
    tc_ctx: &TypecheckingContext<'arena>,
    used_funcs: &[StoreKey<TypedFunction<'arena>>],
    used_statics: &[StoreKey<TypedStatic<'arena>>],
) {
    let mut ctx = DceContext {
        used_functions: HashSet::new(),
        used_statics: HashSet::new(),
        funcs_left: HashSet::new(),
        tc_ctx,
    };

    for key in used_statics.iter().copied() {
        ctx.used_statics.insert(key);
    }

    if let Some(main_fn) = tc_ctx.main_function.get() {
        ctx.funcs_left.insert(*main_fn);
    }

    for func in used_funcs {
        ctx.funcs_left.insert(*func);
    }
    for (_, ext_fn_body) in ctx.tc_ctx.external_functions.read().iter() {
        let Some(body) = ext_fn_body else { continue };
        run_block(&body.exprs, &mut ctx);
    }
    while let Some(func) = ctx.funcs_left.iter().next().copied() {
        if !ctx.used_functions.contains(&func) {
            run_fn(func, &mut ctx);
        }
    }

    tc_ctx
        .functions
        .write()
        .retain(|k, _| ctx.used_functions.contains(k));
    tc_ctx
        .statics
        .write()
        .retain(|k, _| ctx.used_statics.contains(k));
}

fn run_fn<'arena>(func: StoreKey<TypedFunction<'arena>>, ctx: &mut DceContext<'_, 'arena>) {
    ctx.funcs_left.remove(&func);
    ctx.used_functions.insert(func);
    let func_reader = ctx.tc_ctx.functions.read();
    let Some((_, func_body)) = func_reader.get(&func) else {
        return;
    };
    run_block(&func_body.exprs, ctx);
}

fn run_block<'arena>(block: &[TypedExpression<'arena>], ctx: &mut DceContext<'_, 'arena>) {
    for expr in block {
        match expr {
            TypedExpression::If {
                cond,
                if_block,
                else_block,
                ..
            } => {
                run_literal(cond, ctx);
                run_block(&if_block.0, ctx);
                if let Some((else_block, _)) = else_block {
                    run_block(else_block, ctx);
                }
            }
            TypedExpression::While {
                cond_block,
                cond,
                body,
                ..
            } => {
                run_block(cond_block, ctx);
                run_literal(cond, ctx);
                run_block(&body.0, ctx);
            }
            TypedExpression::Block(_, block, _) => run_block(block, ctx),
            TypedExpression::DirectCall(_, _, func, lits, _) => {
                if !ctx.used_functions.contains(func) {
                    ctx.funcs_left.insert(*func);
                }
                for lit in lits {
                    run_literal(lit, ctx);
                }
            }
            TypedExpression::Call(_, _, lit1, lits) => {
                run_literal(lit1, ctx);
                for lit in lits {
                    run_literal(lit, ctx);
                }
            }
            TypedExpression::Reference(_, _, lit)
            | TypedExpression::Dereference(_, _, lit)
            | TypedExpression::Offset(_, _, lit, _)
            | TypedExpression::OffsetNonPointer(_, _, lit, _)
            | TypedExpression::Literal(_, _, lit)
            | TypedExpression::Alias(_, _, lit)
            | TypedExpression::Bitcast(_, _, lit)
            | TypedExpression::IntCast(_, _, lit)
            | TypedExpression::PtrToInt(_, _, lit)
            | TypedExpression::IntToPtr(_, _, lit)
            | TypedExpression::StripMetadata(_, _, lit)
            | TypedExpression::MakeUnsizedSlice(_, _, lit, _)
            | TypedExpression::AttachVtable(_, _, lit, _)
            | TypedExpression::Return(_, lit)
            | TypedExpression::Pos(_, _, lit)
            | TypedExpression::Neg(_, _, lit)
            | TypedExpression::LNot(_, _, lit)
            | TypedExpression::BNot(_, _, lit) => run_literal(lit, ctx),
            TypedExpression::Range {
                lhs: lit1,
                rhs: lit2,
                ..
            }
            | TypedExpression::StoreAssignment(_, lit1, lit2)
            | TypedExpression::Add(_, _, lit1, lit2)
            | TypedExpression::Sub(_, _, lit1, lit2)
            | TypedExpression::Mul(_, _, lit1, lit2)
            | TypedExpression::Div(_, _, lit1, lit2)
            | TypedExpression::Mod(_, _, lit1, lit2)
            | TypedExpression::BAnd(_, _, lit1, lit2)
            | TypedExpression::BOr(_, _, lit1, lit2)
            | TypedExpression::BXor(_, _, lit1, lit2)
            | TypedExpression::GreaterThan(_, _, lit1, lit2)
            | TypedExpression::LessThan(_, _, lit1, lit2)
            | TypedExpression::GreaterThanEq(_, _, lit1, lit2)
            | TypedExpression::LessThanEq(_, _, lit1, lit2)
            | TypedExpression::Eq(_, _, lit1, lit2)
            | TypedExpression::Neq(_, _, lit1, lit2)
            | TypedExpression::LShift(_, _, lit1, lit2)
            | TypedExpression::RShift(_, _, lit1, lit2) => {
                run_literal(lit1, ctx);
                run_literal(lit2, ctx);
            }

            TypedExpression::LAnd(_, _, lit1, lit2, blk)
            | TypedExpression::LOr(_, _, lit1, lit2, blk) => {
                run_literal(lit1, ctx);
                run_block(blk, ctx);
                run_literal(lit2, ctx);
            }
            TypedExpression::DirectExternCall(_, _, _, lits)
            | TypedExpression::DynCall(_, _, lits, _)
            | TypedExpression::LLVMIntrinsicCall(_, _, _, lits)
            | TypedExpression::IntrinsicCall(_, _, _, lits, _) => {
                for lit in lits {
                    run_literal(lit, ctx);
                }
            }
            TypedExpression::DropIf(..)
            | TypedExpression::Drop(..)
            | TypedExpression::Asm { .. }
            | TypedExpression::DeclareVariable(..)
            | TypedExpression::Empty(..)
            | TypedExpression::Unreachable(..)
            | TypedExpression::None => (),
        }
    }
}

fn run_literal<'arena>(literal: &TypedLiteral<'arena>, ctx: &mut DceContext<'_, 'arena>) {
    match literal {
        TypedLiteral::Function(func, _) => {
            if !ctx.used_functions.contains(func) {
                ctx.funcs_left.insert(*func);
            }
        }
        TypedLiteral::Static(static_key) => _ = ctx.used_statics.insert(*static_key),
        TypedLiteral::Array(_, literals)
        | TypedLiteral::Struct(_, literals)
        | TypedLiteral::Tuple(literals) => {
            for lit in literals {
                run_literal(lit, ctx);
            }
        }
        TypedLiteral::ArrayInit(_, literal, _) => run_literal(literal, ctx),

        _ => (),
    }
}
