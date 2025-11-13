use std::ops::Deref;

use crate::{FunctionType, Ty, TyKind, TyList, TypeCtx};

pub struct SubstitutionCtx<'ctx, 'a> {
    ctx: TypeCtx<'ctx>,
    type_args: &'a [Ty<'ctx>],
}

impl<'ctx, 'a> SubstitutionCtx<'ctx, 'a> {
    pub fn new(ctx: TypeCtx<'ctx>, type_args: &'a [Ty<'ctx>]) -> Self {
        Self { ctx, type_args }
    }
}

impl<'ctx> Deref for SubstitutionCtx<'ctx, '_> {
    type Target = TypeCtx<'ctx>;

    fn deref(&self) -> &Self::Target {
        &self.ctx
    }
}

pub trait Substitute<'ctx>: 'ctx {
    fn substitute(self, ctx: &SubstitutionCtx<'ctx, '_>) -> Self;
    fn would_substitute(&self) -> bool;
}

impl<'ctx> Substitute<'ctx> for FunctionType<'ctx> {
    fn substitute(self, ctx: &SubstitutionCtx<'ctx, '_>) -> Self {
        Self {
            arguments: self.arguments.substitute(ctx),
            return_type: self.return_type.substitute(ctx),
        }
    }

    fn would_substitute(&self) -> bool {
        self.arguments.would_substitute() || self.return_type.would_substitute()
    }
}

impl<'ctx> Substitute<'ctx> for Ty<'ctx> {
    fn substitute(self, ctx: &SubstitutionCtx<'ctx, '_>) -> Self {
        match **self {
            TyKind::UnsizedArray(ty) => ctx.intern_ty(TyKind::UnsizedArray(ty.substitute(ctx))),
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
            TyKind::Tuple(ty_list) => ctx.intern_ty(TyKind::Tuple(ty_list.substitute(ctx))),
            TyKind::Ref(ty) => ctx.intern_ty(TyKind::Ref(ty.substitute(ctx))),
            TyKind::Generic { generic_id, .. } => ctx.type_args[generic_id as usize],
            TyKind::Struct {
                struct_id,
                generics,
                name,
            } => ctx.intern_ty(TyKind::Struct {
                struct_id,
                generics: generics.substitute(ctx),
                name,
            }),
            TyKind::Function(fn_ty) => ctx.intern_ty(TyKind::Function(fn_ty.substitute(ctx))),
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
            TyKind::Struct { generics, .. } => generics.would_substitute(),
            _ => false,
        }
    }
}

impl<'ctx> Substitute<'ctx> for TyList<'ctx> {
    fn substitute(self, ctx: &SubstitutionCtx<'ctx, '_>) -> Self {
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
    fn substitute(mut self, ctx: &SubstitutionCtx<'ctx, '_>) -> Self {
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
    fn substitute(mut self, ctx: &SubstitutionCtx<'ctx, '_>) -> Self {
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
