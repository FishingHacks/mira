use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{
    Attribute, DataEnum, DataStruct, DeriveInput, Error, Expr, Fields, Generics, Ident, Variant,
    parse::ParseStream, parse_macro_input,
};

use crate::utils::{enum_make_match, struct_make_fields};

pub fn derive_error_data(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match input.data {
        syn::Data::Struct(s) => derive_for_struct(s, &input.attrs, input.ident, input.generics)
            .unwrap_or_else(|err| err.to_compile_error())
            .into(),
        syn::Data::Enum(e) => derive_for_enum(e, input.ident, input.generics)
            .unwrap_or_else(|err| err.to_compile_error())
            .into(),
        syn::Data::Union(_) => Error::new(input.ident.span(), "ErrorData does not support unions")
            .to_compile_error()
            .into(),
    }
}

fn enum_make_match_empty(variant: &Variant) -> TokenStream {
    let ident = &variant.ident;
    match &variant.fields {
        Fields::Named(_) => quote! { Self::#ident { .. } },
        Fields::Unnamed(_) => quote! { Self::#ident(..) },
        Fields::Unit => quote! { Self::#ident },
    }
}

fn derive_for_enum(s: DataEnum, ident: Ident, generics: Generics) -> Result<TokenStream, Error> {
    let mut errs = Vec::with_capacity(s.variants.len());
    let mut notes = Vec::with_capacity(s.variants.len());
    let mut error_codes = Vec::with_capacity(s.variants.len());
    let mut matches = Vec::with_capacity(s.variants.len());
    let mut labels = Vec::with_capacity(s.variants.len());

    for variant in s.variants.iter() {
        let attr = parse_attrs(&variant.attrs, variant.ident.span())?;
        errs.push(attr.err);
        let notes_value = attr.notes;
        notes.push(quote! { #(cb(format_args!(#notes_value))?;)* });
        error_codes.push(attr.error_code);
        matches.push(enum_make_match(variant));
        labels.push(generate_labeled_spans_for_fields(&variant.fields)?);
    }
    let empty_matches = s.variants.iter().map(enum_make_match_empty);
    let maybe_copy = generate_maybecopy();

    let (impl_generics, ty_generics, where_clauses) = generics.split_for_impl();

    Ok(quote! {
        #[allow(unused, dead_code)]
        impl #impl_generics mira_errors::ErrorData for #ident #ty_generics #where_clauses {
            fn message<'ctx>(
                &'ctx self,
                ctx: mira_errors::FormattingCtx<'ctx>,
                cb: &mut dyn FnMut(std::fmt::Arguments<'_>) -> std::fmt::Result,
            ) -> std::fmt::Result {
                match self {
                    #(#matches => cb(format_args!(#errs)),)*
                }
            }

            fn notes<'ctx>(
                &'ctx self,
                ctx: mira_errors::FormattingCtx<'ctx>,
                cb: &mut dyn FnMut(std::fmt::Arguments<'_>) -> std::fmt::Result,
            ) -> std::fmt::Result {
                match self {
                    #(#matches => { #notes })*
                }
                Ok(())
            }

            fn labeled_spans(&self, ctx: mira_errors::FormattingCtx<'_>) -> Vec<mira_errors::LabeledSpan> {
                #maybe_copy
                match self {
                    #(#matches => { #labels })*
                }
            }

            fn error_code(&self) -> Option<&'static str> {
                match self {
                    #(#empty_matches => #error_codes,)*
                }
            }
        }
    })
}

enum ParsedLabeledSpanType {
    Primary,
    Secondary,
    SecondaryList,
}

fn parse_field_attrs(
    attrs: &[Attribute],
) -> impl Iterator<Item = Result<(ParsedLabeledSpanType, &TokenStream), Error>> {
    attrs.iter().filter_map(|attr| {
        let ty = match () {
            _ if attr.path().is_ident("primary_label") => ParsedLabeledSpanType::Primary,
            _ if attr.path().is_ident("secondary_label") => ParsedLabeledSpanType::Secondary,
            _ if attr.path().is_ident("secondary_labels") => ParsedLabeledSpanType::SecondaryList,
            _ => return None,
        };
        Some(attr.meta.require_list().map(|v| (ty, &v.tokens)))
    })
}

fn generate_maybecopy() -> TokenStream {
    quote! {
        trait MaybeCopy: Copy {
            fn copy(&self) -> Self;
        }
        impl<T: MaybeCopy> MaybeCopy for &T {
            fn copy(&self) -> Self {
                *self
            }
        }
        impl MaybeCopy for mira_spans::Span<'_> {
            fn copy(&self) -> Self {
                *self
            }
        }
    }
}

fn generate_labeled_spans_for_fields(fields: &Fields) -> Result<TokenStream, Error> {
    let mut stream = quote! { let mut labels = Vec::new(); };

    for (field, member) in fields.iter().zip(fields.members()) {
        for v in parse_field_attrs(&field.attrs) {
            let (typ, format) = v?;
            match typ {
                ParsedLabeledSpanType::Primary => stream.extend(quote! {
                    labels.push(mira_errors::LabeledSpan::primary(format!(#format), MaybeCopy::copy(&self.#member)));
                }),
                ParsedLabeledSpanType::Secondary => stream.extend(quote! {
                    labels.push(mira_errors::LabeledSpan::secondary(format!(#format), MaybeCopy::copy(&self.#member)));
                }),
                ParsedLabeledSpanType::SecondaryList => stream.extend(quote! {
                    for span in self.#member.iter() {
                        labels.push(mira_errors::LabeledSpan::secondary(format!(#format), MaybeCopy::copy(&span)));
                    }
                }),
            }
        }
    }

    stream.extend(quote! { labels });

    Ok(stream)
}

fn derive_for_struct(
    s: DataStruct,
    attrs: &[Attribute],
    ident: Ident,
    generics: Generics,
) -> Result<TokenStream, Error> {
    let attrs = parse_attrs(attrs, ident.span())?;
    let ErrorAttrs {
        error_code,
        err,
        notes,
    } = attrs;
    let fields = struct_make_fields(&s.fields);
    let (impl_generics, ty_generics, where_clauses) = generics.split_for_impl();
    let labeled_spans = generate_labeled_spans_for_fields(&s.fields)?;
    let maybe_copy = generate_maybecopy();

    Ok(quote! {
        #[allow(unused, dead_code)]
        impl #impl_generics mira_errors::ErrorData for #ident #ty_generics #where_clauses {
            fn message<'ctx>(
                &'ctx self,
                ctx: mira_errors::FormattingCtx<'ctx>,
                cb: &mut dyn FnMut(std::fmt::Arguments<'_>) -> std::fmt::Result,
            ) -> std::fmt::Result {
                #fields
                cb(format_args!(#err))
            }

            fn notes<'ctx>(
                &'ctx self,
                ctx: mira_errors::FormattingCtx<'ctx>,
                mut cb: &mut dyn FnMut(std::fmt::Arguments<'_>) -> std::fmt::Result,
            ) -> std::fmt::Result {
                #fields
                #(cb(format_args!(#notes))?;)*
                Ok(())
            }

            fn error_code(&self) -> Option<&'static str> {
                #error_code
            }


            fn labeled_spans(&self, ctx: mira_errors::FormattingCtx<'_>) -> Vec<mira_errors::LabeledSpan> {
                #maybe_copy
                #fields
                #labeled_spans
            }
        }
    })
}

struct ErrorAttrs<'a> {
    error_code: TokenStream,
    err: &'a TokenStream,
    notes: Box<[&'a TokenStream]>,
}

fn parse_attrs<'a>(attrs: &'a [Attribute], err_span: Span) -> Result<ErrorAttrs<'a>, Error> {
    let mut error_code: Option<Expr> = None;
    let mut err: Option<&'a TokenStream> = None;
    let mut notes: Vec<&'a TokenStream> = Vec::new();
    for attr in attrs.iter() {
        if attr.path().is_ident("error_code") && error_code.is_none() {
            if error_code.is_some() {
                return Err(Error::new_spanned(
                    attr,
                    "error_code has already been defined",
                ));
            }
            error_code = Some(parse_error_code(attr)?);
        } else if attr.path().is_ident("error") {
            if err.is_some() {
                return Err(Error::new_spanned(attr, "error has already been defined"));
            }
            err = Some(&attr.meta.require_list()?.tokens);
        } else if attr.path().is_ident("note") {
            notes.push(&attr.meta.require_list()?.tokens);
        }
    }
    Ok(ErrorAttrs {
        err: err.ok_or_else(|| Error::new(err_span, "error has not been defined"))?,
        error_code: error_code
            .map(|v| quote! {Some(#v)})
            .unwrap_or_else(|| quote! {None}),
        notes: notes.into_boxed_slice(),
    })
}

fn parse_error_code(attr: &Attribute) -> Result<Expr, Error> {
    match &attr.meta {
        syn::Meta::List(meta_list) => {
            meta_list.parse_args_with(|stream: ParseStream<'_>| stream.parse::<Expr>())
        }
        syn::Meta::NameValue(meta) => Ok(meta.value.clone()),
        _ => Err(Error::new_spanned(
            attr,
            "usage: #[error_code(0001)] or #[error_code = 0001]",
        )),
    }
}
