use proc_macro2::TokenStream;
use quote::quote;
use syn::{Fields, Ident, Variant, spanned::Spanned};

pub(crate) fn enum_make_match(variant: &Variant) -> TokenStream {
    let ident = &variant.ident;
    match &variant.fields {
        Fields::Named(fields_named) => {
            let fields = fields_named.named.iter().map(|v| v.ident.as_ref().unwrap());
            quote! { Self::#ident { #(#fields),* } }
        }
        Fields::Unnamed(fields_unnamed) => {
            let field_count = fields_unnamed.unnamed.len();
            let field_names =
                (0..field_count).map(|v| Ident::new(&format!("_{v}"), fields_unnamed.span()));
            quote! { Self::#ident(#(#field_names),*) }
        }
        Fields::Unit => quote! { Self::#ident },
    }
}

pub(crate) fn struct_make_fields(fields: &Fields) -> TokenStream {
    match fields {
        Fields::Named(fields_named) => {
            let fields = fields_named.named.iter().map(|v| v.ident.as_ref().unwrap());
            quote! { #(let #fields = &self.#fields;)* }
        }
        Fields::Unnamed(fields_unnamed) => {
            let field_count = fields_unnamed.unnamed.len();
            let fields = fields.members();
            let field_names =
                (0..field_count).map(|v| Ident::new(&format!("_{v}"), fields_unnamed.span()));
            quote! { #(let #field_names = &self.#fields;)* }
        }
        Fields::Unit => TokenStream::new(),
    }
}
