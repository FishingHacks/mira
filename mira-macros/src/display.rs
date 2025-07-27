use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::{Attribute, DataEnum, DataStruct, DeriveInput, Error, Generics, parse_macro_input};

use crate::utils::{enum_make_match, struct_make_fields};

pub fn derive_display(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match input.data {
        syn::Data::Struct(s) => derive_for_struct(s, &input.attrs, input.ident, input.generics)
            .unwrap_or_else(|err| err.to_compile_error())
            .into(),
        syn::Data::Enum(e) => derive_for_enum(e, input.ident, input.generics)
            .unwrap_or_else(|err| err.to_compile_error())
            .into(),
        syn::Data::Union(_) => Error::new(input.ident.span(), "Display does not support unions")
            .to_compile_error()
            .into(),
    }
}

fn derive_for_enum(s: DataEnum, ident: Ident, generics: Generics) -> Result<TokenStream, Error> {
    let matches = s.variants.iter().map(enum_make_match);
    let mut display = Vec::with_capacity(s.variants.len());
    for variant in s.variants.iter() {
        display.push(parse_attrs(&variant.attrs, variant.ident.span())?);
    }

    let (impl_generics, ty_generics, where_clauses) = generics.split_for_impl();

    Ok(quote! {
        impl #impl_generics std::fmt::Display for #ident #ty_generics #where_clauses {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    #(#matches => f.write_fmt(format_args!(#display)),)*
                }
            }
        }
    })
}

fn derive_for_struct(
    s: DataStruct,
    attrs: &[Attribute],
    ident: Ident,
    generics: Generics,
) -> Result<TokenStream, Error> {
    let display = parse_attrs(attrs, ident.span())?;
    let (impl_generics, ty_generics, where_clauses) = generics.split_for_impl();

    let fields = struct_make_fields(&s.fields);

    Ok(quote! {
        impl #impl_generics std::fmt::Display for #ident #ty_generics #where_clauses {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                #fields
                f.write_fmt(format_args!(#display))
            }
        }
    })
}

fn parse_attrs(attrs: &[Attribute], span: Span) -> Result<&TokenStream, Error> {
    let mut display = None;
    for attr in attrs.iter().filter(|v| v.path().is_ident("display")) {
        match &attr.meta {
            syn::Meta::List(_) if display.is_some() => {
                return Err(Error::new_spanned(attr, "`display` was already defined"));
            }
            syn::Meta::List(meta_list) => display = Some(&meta_list.tokens),
            _ => return Err(Error::new_spanned(attr, "usage: #[display(<fmt>)]")),
        }
    }
    display.ok_or_else(|| Error::new(span, "`display` has not been defined"))
}
