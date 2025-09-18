use std::ops::Deref;

use crate::{Ty, TyKind, TyList, TypeckCtx};

pub struct SubstitutionCtx<'ctx, 'a, 'tc> {
    tc_ctx: &'tc TypeckCtx<'ctx>,
    type_args: &'a [Ty<'ctx>],
}

impl<'ctx, 'a, 'tc> SubstitutionCtx<'ctx, 'a, 'tc> {
    pub fn new(tc_ctx: &'tc TypeckCtx<'ctx>, type_args: &'a [Ty<'ctx>]) -> Self {
        Self { tc_ctx, type_args }
    }
}

impl<'ctx> Deref for SubstitutionCtx<'ctx, '_, '_> {
    type Target = TypeckCtx<'ctx>;

    fn deref(&self) -> &Self::Target {
        self.tc_ctx
    }
}

pub trait Substitute<'ctx>: 'ctx {
    fn substitute(self, ctx: &SubstitutionCtx<'ctx, '_, '_>) -> Self;
    fn would_substitute(&self) -> bool;
}

impl<'ctx> Substitute<'ctx> for Ty<'ctx> {
    fn substitute(self, ctx: &SubstitutionCtx<'ctx, '_, '_>) -> Self {
        match **self {
            TyKind::UnsizedArray(ty) => ctx.ctx.intern_ty(TyKind::UnsizedArray(ty.substitute(ctx))),
            TyKind::SizedArray {
                ty,
                number_elements,
            } => {
                let ty = ty.substitute(ctx);
                ctx.ctx.intern_ty(TyKind::SizedArray {
                    ty,
                    number_elements,
                })
            }
            TyKind::Tuple(ty_list) => ctx.ctx.intern_ty(TyKind::Tuple(ty_list.substitute(ctx))),
            TyKind::Ref(ty) => ctx.ctx.intern_ty(TyKind::Ref(ty.substitute(ctx))),
            TyKind::Generic { generic_id, .. } => ctx.type_args[generic_id as usize],
            _ => self,
        }
    }

    fn would_substitute(&self) -> bool {
        match ***self {
            TyKind::Ref(ty) | TyKind::UnsizedArray(ty) | TyKind::SizedArray { ty, .. } => {
                ty.would_substitute()
            }
            TyKind::Tuple(ty_list) => ty_list.would_substitute(),
            TyKind::Generic { .. } => true,
            _ => false,
        }
    }
}

impl<'ctx> Substitute<'ctx> for TyList<'ctx> {
    fn substitute(self, ctx: &SubstitutionCtx<'ctx, '_, '_>) -> Self {
        let mut types = Vec::new();
        let mut modified = false;
        for (i, &ty) in self.iter().enumerate() {
            let substituted = ty.substitute(ctx);
            if modified {
                types.push(substituted);
            } else if substituted != ty {
                types.extend_from_slice(&self[..i]);
                types.push(substituted);
                modified = true;
            }
        }
        if modified {
            ctx.ctx.intern_tylist(&types)
        } else {
            self
        }
    }

    fn would_substitute(&self) -> bool {
        self.iter().any(Substitute::would_substitute)
    }
}

impl<'ctx, T: Substitute<'ctx> + Default> Substitute<'ctx> for Box<[T]> {
    fn substitute(mut self, ctx: &SubstitutionCtx<'ctx, '_, '_>) -> Self {
        let mut tmp = T::default();
        if self.would_substitute() {
            for v in &mut self {
                std::mem::swap(&mut tmp, v);
                tmp = tmp.substitute(ctx);
                std::mem::swap(&mut tmp, v);
            }
        }
        self
    }

    fn would_substitute(&self) -> bool {
        self.iter().any(Substitute::would_substitute)
    }
}

impl<'ctx, T: Substitute<'ctx>> Substitute<'ctx> for Vec<T> {
    fn substitute(mut self, ctx: &SubstitutionCtx<'ctx, '_, '_>) -> Self {
        if self.would_substitute() {
            for i in 0..self.len() {
                let v = self.remove(i).substitute(ctx);
                self.insert(i, v);
            }
        }
        self
    }

    fn would_substitute(&self) -> bool {
        self.iter().any(Substitute::would_substitute)
    }
}
