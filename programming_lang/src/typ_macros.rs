#[allow(unused_imports)]
use crate::{
    globals::GlobalStr,
    module::TraitId,
    typechecking::{Type, TypecheckingContext},
};

struct BasicFunctionContract {
    return_type: Type,
    args: Vec<Type>,
}

#[macro_export]
macro_rules! pltype {
    ($ctx: ident, $($ty: tt)+) => {{
        $crate::pltype!(!def $ctx, 0, $($ty)+)
    }};

    (!def $ctx:ident, $num_refs:expr, &$($ty:tt)+) => {{
        $crate::pltype!(!def $ctx, $num_refs + 1, $($ty)+)
    }};


    (!def $ctx:ident, $num_refs:expr, &&$($ty:tt)+) => {
        $crate::pltype!(!def $ctx, $num_refs + 2, $($ty)+)
    };

    (!def $ctx:ident, $num_refs:expr, [$amount:literal * $($ty:tt)+]) => {
        Type::SizedArray { typ: Box::new($crate::pltype!($ctx, $($ty)+)), num_references: $num_refs, number_elements: $amount as usize }
    };

    (!def $ctx:ident, $num_refs:expr, [$($ty:tt)+]) => {
        Type::UnsizedArray { typ: Box::new($crate::pltype!($ctx, $($ty)+)), num_references: $num_refs }
    };

    (!def $ctx:ident, $num_refs:expr, generic($name:ident)) => {
        Type::Generic(GlobalStr::new(stringify!($name)), $num_refs)
    };

    (!def $ctx:ident, $num_refs:expr, generic($name:ident: $first_generic:ident $(+ $generic:ident)*)) => {{
        let lang_items = $ctx.lang_items.read();
        let mut bounds = Vec::new();
        if let Some(v) = lang_items.$first_generic { bounds.push(v); };
        $(if let Some(v) = lang_items.$generic { bounds.push(v); };)*
        Type::Trait { trait_refs: bounds, num_references: $num_refs, real_name: GlobalStr::new(stringify!($name)) }
    }};

    (!def $ctx:ident, $num_refs:expr, dyn $first_generic:ident $(+ $generic:ident)*) => {{
        let traits = $ctx.traits.read();
        let lang_items = $ctx.lang_items.read();
        let mut bounds = Vec::new();
        if let Some(v) = lang_items.$first_generic { bounds.push((v, traits[v].name.clone())); };
        $(if let Some(v) = lang_items.$generic { bounds.push((v, traits[v].name.clone())); };)*
        Type::DynType { trait_refs: bounds, num_references: $num_refs }
    }};

    (!def $ctx:ident, $num_refs:expr, void) => {
        Type::PrimitiveVoid($num_refs)
    };

    (!def $ctx:ident, $num_refs:expr, struct_lang_item($lang_item: ident)) => {{
        let lang_items = $ctx.lang_items.read();
        let structs = $ctx.structs.read();
        if let Some(v) = lang_items.$lang_item {
            Type::Struct { struct_id: v, num_references: $num_refs, name: structs[v].name.clone() }
        } else { panic!("struct lang item is not initialized.") };
    }};
}

#[macro_export]
macro_rules! func_sig {
    ($ctx:ident, fn($($($ty:tt)+),* $(,)?)) => { $crate::func_sig!($ctx, fn($($($ty)+),*) -> void) };
    ($ctx:ident, fn($($($ty:tt)+),* $(,)?) -> $($ret:tt)*) => {{
        let return_type = $crate::pltype!($ctx, $($ret)*);
        #[allow(unused_mut)]
        let mut args = Vec::new();
        $(
            args.push($crate::pltype!($ctx, $($ty)+));
        )*
        BasicFunctionContract { args, return_type }
    }};
}
