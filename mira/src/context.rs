use std::marker::PhantomData;

pub struct GlobalContext<'arena> {
    _d: PhantomData<&'arena ()>,
}
