use mira_parser::std_annotations::intrinsic::Intrinsic;

use crate::{
    TypeCtx, default_types,
    ir::{MutVisitor, Scope, TypedExpression, TypedLiteral},
};

pub struct TypenameIntrinsicPass;

impl<'a> MutVisitor<'a> for TypenameIntrinsicPass {
    fn visit_expr(
        &mut self,
        expr: &mut TypedExpression<'a>,
        scope: &mut Scope<'a>,
        tcx: TypeCtx<'a>,
    ) {
        if let TypedExpression::IntrinsicCall(pos, dst, Intrinsic::TypeName, _, generics) = expr {
            if generics.len() != 1 {
                return;
            }
            assert_eq!(
                scope.get_ty(*dst),
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
