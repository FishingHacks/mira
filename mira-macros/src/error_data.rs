use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, quote};
use syn::{
    Attribute, DataEnum, DataStruct, DeriveInput, Error, Expr, Fields, Generics, Ident, Variant,
    parse::ParseStream, parse_macro_input, spanned::Spanned,
};

use crate::utils::{enum_make_match, struct_make_fields};

pub fn derive_error_data(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let with_arena_lifetime = parse_with_arena_lifetime(&input.attrs);
    match input.data {
        syn::Data::Struct(s) => derive_for_struct(
            s,
            &input.attrs,
            input.ident,
            input.generics,
            with_arena_lifetime,
        )
        .unwrap_or_else(|err| err.to_compile_error())
        .into(),
        syn::Data::Enum(e) => derive_for_enum(e, input.ident, input.generics, with_arena_lifetime)
            .unwrap_or_else(|err| err.to_compile_error())
            .into(),
        syn::Data::Union(_) => Error::new(input.ident.span(), "ErrorData does not support unions")
            .to_compile_error()
            .into(),
    }
}

fn parse_with_arena_lifetime(attrs: &[Attribute]) -> bool {
    for attr in attrs {
        if attr.path().is_ident("no_arena_lifetime") {
            return false;
        }
    }
    true
}

fn enum_make_match_empty(variant: &Variant) -> TokenStream {
    let ident = &variant.ident;
    match &variant.fields {
        Fields::Named(_) => quote! { Self::#ident { .. } },
        Fields::Unnamed(_) => quote! { Self::#ident(..) },
        Fields::Unit => quote! { Self::#ident },
    }
}

fn derive_for_enum(
    s: DataEnum,
    ident: Ident,
    generics: Generics,
    with_arena_lifetime: bool,
) -> Result<TokenStream, Error> {
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
    let diagnostic_generics = if with_arena_lifetime {
        ty_generics.to_token_stream()
    } else {
        quote! { <'static> }
    };
    let diagnostics_ext = enum_diagnostics_ext(&ident, s.variants.iter());

    Ok(quote! {
        #[allow(unused, dead_code, deprecated)]
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

        impl #impl_generics #ident #ty_generics #where_clauses {
            pub fn to_error(self) -> mira_errors::Diagnostic #diagnostic_generics { mira_errors::Diagnostic::new(self, mira_errors::Severity::Error) }
            pub fn to_warning(self) -> mira_errors::Diagnostic #diagnostic_generics { mira_errors::Diagnostic::new(self, mira_errors::Severity::Warn) }
            pub fn as_diagnostic(self, severity: mira_errors::Severity) -> mira_errors::Diagnostic #diagnostic_generics { mira_errors::Diagnostic::new(self, severity) }
        }

        #diagnostics_ext
    })
}

fn to_camel_case(s: &str) -> String {
    let mut result = String::with_capacity(s.len());

    for c in s.chars() {
        if c.is_uppercase() && !result.is_empty() {
            result.push('_');
        }
        c.to_lowercase().for_each(|c| result.push(c));
    }

    result
}

fn enum_diagnostics_ext<'a>(
    ident: &Ident,
    variants: impl Iterator<Item = &'a Variant>,
) -> TokenStream {
    let diagnostics_ext_trait = Ident::new(&format!("{ident}DiagnosticsExt"), ident.span());
    let emitter_ext_trait = Ident::new(&format!("{ident}EmitterExt"), ident.span());
    let mut diagnostics_def = TokenStream::new();
    let mut diagnostics_impl = TokenStream::new();

    let mut emitter_def = TokenStream::new();

    for variant in variants {
        let mut add_name = to_camel_case(&format!("{}", variant.ident));
        add_name.insert_str(0, "add_");
        let mut function_args = TokenStream::new();
        let mut constructor = TokenStream::new();
        let variant_name = &variant.ident;
        let add_name = Ident::new(&add_name, variant.ident.span());

        let mut emit_name = to_camel_case(&format!("{}", variant.ident));
        emit_name.insert_str(0, "emit_");
        let emit_name = Ident::new(&emit_name, variant.ident.span());

        let add_func = if variant
            .attrs
            .iter()
            .any(|v| v.path().is_ident("is_warning"))
        {
            Ident::new("add_warn", ident.span())
        } else {
            Ident::new("add_err", ident.span())
        };

        let level = if variant
            .attrs
            .iter()
            .any(|v| v.path().is_ident("is_warning"))
        {
            quote! { mira_errors::Severity::Warn }
        } else {
            quote! { mira_errors::Severity::Error }
        };

        match &variant.fields {
            Fields::Named(fields) => {
                for field in fields.named.iter() {
                    let ty = &field.ty;
                    let name = field.ident.as_ref().unwrap();
                    function_args.extend(quote! { #name : #ty , });
                    constructor.extend(quote! { #name , });
                }
                constructor = quote! { #ident::#variant_name {#constructor} };
            }
            Fields::Unnamed(fields) => {
                for (i, field) in fields.unnamed.iter().enumerate() {
                    let argname = format!("field_{i}");
                    let argname = Ident::new(&argname, field.span());
                    let ty = &field.ty;
                    function_args.extend(quote! { #argname : #ty , });
                    constructor.extend(quote! { #argname , });
                }
                constructor = quote! { #ident::#variant_name (#constructor) };
            }
            Fields::Unit => constructor = quote! { #ident::#variant_name },
        }
        diagnostics_def.extend(quote! { fn #add_name(&mut self, #function_args) -> &mut mira_errors::Diagnostic<'arena>; });
        diagnostics_impl.extend(quote! {
            fn #add_name(&mut self, #function_args) -> &mut mira_errors::Diagnostic<'arena> {
                self.#add_func(#constructor)
            }
        });
        emitter_def.extend(quote! {
            fn #emit_name(&self, #function_args) {
                self.emit_diagnostic(mira_errors::Diagnostic::new(#constructor, #level));
            }
        });
    }

    quote! {
        pub trait #diagnostics_ext_trait<'arena> { #diagnostics_def }
        impl<'arena> #diagnostics_ext_trait<'arena> for mira_errors::Diagnostics<'arena> { #diagnostics_impl }
        pub trait #emitter_ext_trait<'arena>: mira_errors::DiagEmitter<'arena> { #emitter_def }
        impl<'arena> #emitter_ext_trait<'arena> for mira_context::SharedCtx<'arena> {}
    }
}

fn struct_diagnostics_ext(fields: &Fields, attrs: &[Attribute], ident: &Ident) -> TokenStream {
    let ext_trait = Ident::new(&format!("{ident}DiagnosticsExt"), ident.span());
    let mut camel_case_name = to_camel_case(&format!("{ident}"));
    camel_case_name.insert_str(0, "add_");
    let camel_case_name = Ident::new(&camel_case_name, ident.span());
    let mut function_args = TokenStream::new();
    let mut constructor = TokenStream::new();

    let add_func = if attrs.iter().any(|v| v.path().is_ident("is_warning")) {
        Ident::new("add_warn", ident.span())
    } else {
        Ident::new("add_err", ident.span())
    };

    match &fields {
        Fields::Named(fields) => {
            for field in fields.named.iter() {
                let ty = &field.ty;
                let name = field.ident.as_ref().unwrap();
                function_args.extend(quote! { #name : #ty , });
                constructor.extend(quote! { #name , });
            }
            constructor = quote! { #ident {#constructor} };
        }
        Fields::Unnamed(fields) => {
            for (i, field) in fields.unnamed.iter().enumerate() {
                let argname = format!("field_{i}");
                let argname = Ident::new(&argname, field.span());
                let ty = &field.ty;
                function_args.extend(quote! { #argname : #ty , });
                constructor.extend(quote! { #argname , });
            }
            constructor = quote! { #ident (#constructor) };
        }
        Fields::Unit => constructor = quote! { #ident },
    }
    quote! {
        trait #ext_trait<'arena> {
            fn #camel_case_name(&mut self, #function_args) -> &mut mira_errors::Diagnostic<'arena>;
        }
        impl<'arena> #ext_trait<'arena> for mira_errors::Diagnostics<'arena> {
            fn #camel_case_name(&mut self, #function_args) -> &mut mira_errors::Diagnostic<'arena> {
                self.#add_func(#constructor)
            }
        }
    }
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

    for (idx, field) in fields.iter().enumerate() {
        let field_name = field
            .ident
            .clone()
            .unwrap_or_else(|| Ident::new(&format!("_{idx}"), field.span()));

        for v in parse_field_attrs(&field.attrs) {
            let (ty, format) = v?;
            match ty {
                ParsedLabeledSpanType::Primary => stream.extend(quote! {
                    labels.push(mira_errors::LabeledSpan::primary(format!(#format), MaybeCopy::copy(#field_name)));
                }),
                ParsedLabeledSpanType::Secondary => stream.extend(quote! {
                    labels.push(mira_errors::LabeledSpan::secondary(format!(#format), MaybeCopy::copy(#field_name)));
                }),
                ParsedLabeledSpanType::SecondaryList => stream.extend(quote! {
                    for span in #field_name.iter() {
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
    with_arena_lifetime: bool,
) -> Result<TokenStream, Error> {
    let diagnostics_ext = struct_diagnostics_ext(&s.fields, attrs, &ident);
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
    let diagnostic_generics = if with_arena_lifetime {
        ty_generics.to_token_stream()
    } else {
        quote! { <'static> }
    };

    Ok(quote! {
        #[allow(unused, dead_code, deprecated)]
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

        impl #impl_generics #ident #ty_generics #where_clauses {
            pub fn to_error(self) -> mira_errors::Diagnostic #diagnostic_generics { mira_errors::Diagnostic::new(self, mira_errors::Severity::Error) }
            pub fn to_warning(self) -> mira_errors::Diagnostic #diagnostic_generics { mira_errors::Diagnostic::new(self, mira_errors::Severity::Warn) }
            pub fn as_diagnostic(self, severity: mira_errors::Severity) -> mira_errors::Diagnostic #diagnostic_generics { mira_errors::Diagnostic::new(self, severity) }
        }

        #diagnostics_ext
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
