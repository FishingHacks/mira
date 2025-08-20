use mira_common::store::VecStore;
use mira_parser::std_annotations::intrinsic::Intrinsic;

use crate::{TypeCtx, default_types, ir::TypedLiteral};

use super::super::{IRVisitor, ScopeEntry, TypedExpression};

pub struct TypenameIntrinsicPass;

impl<'a> IRVisitor<'a> for TypenameIntrinsicPass {
    fn visit_expr(
        &mut self,
        expr: &mut TypedExpression<'a>,
        values: &mut VecStore<ScopeEntry<'a>>,
        tcx: TypeCtx<'a>,
    ) {
        if let TypedExpression::IntrinsicCall(pos, dst, Intrinsic::TypeName, _, generics) = expr {
            if generics.len() != 1 {
                return;
            }
            assert_eq!(
                values[*dst].ty,
                default_types::str_ref,
                "type_name intrinsic doesn't return &str"
            );
            *expr = TypedExpression::Literal(
                *pos,
                *dst,
                TypedLiteral::String(tcx.intern_str(&format!("{}", generics[0]))),
            );
        }
    }
}
