use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    Error, Ident, Token, Type, TypeReference, braced, bracketed, parenthesized,
    parse::Parse,
    spanned::Spanned,
    token::{Brace, Paren},
};

/*
queries         = { query }
query           = { annotation } query_body
query_body      = name "(" ty ")" -> ty
annotation      = "#[" (annotation_body) "]"
annotation_body = "allocated"
                | "no_cache"
                | "manually_allocated"
                | "manually_allocated(" ty ")"
                | "desc(" desc_body ")"
desc_body       = desc_fmt
                | desc_closure desc_fmt
                | "{" desc_closure "}" desc_fmt
desc_closure    = "|" ident "|"
desc_fmt        = string {, desc_arg} [,]
desc_arg        = expr | name "=" expr
*/

// desc_body
struct DescAnnotation {
    ctx_name: Option<Ident>,
    args: TokenStream,
}

impl Parse for DescAnnotation {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let ctx_name;
        let args;
        if input.parse::<Token![|]>().is_ok() {
            ctx_name = input.parse()?;
            input.parse::<Token![|]>()?;
            if input.peek(Brace) {
                let inner;
                braced!(inner in input);
                args = inner.parse()?;
            } else {
                args = input.parse()?;
            }
        } else {
            ctx_name = None;
            args = input.parse()?;
        };
        Ok(Self { ctx_name, args })
    }
}

// { annotation }
#[derive(Default)]
struct Annotations {
    allocated: Option<Ident>,
    no_cache: Option<Ident>,
    manually_allocated: Option<Ident>,
    manually_allocated_ty: Option<Type>,
    desc: Option<(Ident, DescAnnotation)>,
}

impl Parse for Annotations {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let mut me = Self::default();
        // annotation
        while input.parse::<Token![#]>().is_ok() {
            let inner;
            bracketed!(inner in input);
            let ident = inner.parse::<Ident>()?;
            macro_rules! err_if_exist {
                ($thing:ident) => {
                    if me.$thing.is_some() {
                        return Err(Error::new(
                            ident.span(),
                            "Only one annotation of each type is allowed",
                        ));
                    }
                };
            }
            if ident == "allocated" {
                err_if_exist!(allocated);
                me.allocated = Some(ident);
            } else if ident == "no_cache" {
                err_if_exist!(no_cache);
                me.no_cache = Some(ident);
            } else if ident == "manually_allocated" {
                err_if_exist!(manually_allocated);
                me.manually_allocated = Some(ident);
                if inner.peek(Paren) {
                    let ty_inner;
                    parenthesized!(ty_inner in inner);
                    me.manually_allocated_ty = Some(ty_inner.parse()?);
                }
            } else if ident == "desc" {
                err_if_exist!(desc);
                let inner_inner;
                parenthesized!(inner_inner in inner);
                me.desc = Some((ident, inner_inner.parse()?));
            } else {
                return Err(Error::new(
                    ident.span(),
                    format!(
                        "Unknown annotation {ident}, allowed annotations: allocated, no_cache, manually_allocated, desc"
                    ),
                ));
            }
        }
        Ok(me)
    }
}

impl Annotations {
    fn is_allocated(&self) -> bool {
        self.manually_allocated.is_some() || self.allocated.is_some()
    }
}

// query
struct Query {
    annotations: Annotations,
    name: Ident,
    key: Type,
    value: Type,
}

impl Parse for Query {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let annotations = input.parse()?;
        let name = input.parse()?;
        let inner;
        parenthesized!(inner in input);
        let key = inner.parse()?;
        input.parse::<Token![->]>()?;
        let value = input.parse()?;
        input.parse::<Token![;]>()?;
        Ok(Self {
            annotations,
            name,
            key,
            value,
        })
    }
}

struct Queries(Box<[Query]>);

impl Parse for Queries {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let mut vec = Vec::new();
        while !input.is_empty() {
            vec.push(input.parse()?);
        }
        Ok(Self(vec.into_boxed_slice()))
    }
}

pub(crate) fn run(input: proc_macro::TokenStream) -> Result<TokenStream, Error> {
    let queries: Queries = syn::parse(input)?;
    let mut names = TokenStream::new();
    for e in queries.0.iter() {
        let name = &e.name;
        names.extend(quote! { #name });
    }
    let mut macro_invocations = TokenStream::new();
    for q in queries.0.iter() {
        let alloc_ty = match &q.value {
            _ if !q.annotations.is_allocated() => None,
            Type::Reference(TypeReference {
                lifetime: Some(lt),
                mutability: None,
                elem,
                ..
            }) if lt.ident == "arena" => Some(&**elem),
            _ => {
                return Err(Error::new(
                    q.value.span(),
                    "Allocated queries have to return &'arena _",
                ));
            }
        };
        let allocated_ty = q.annotations.manually_allocated_ty.as_ref().or(alloc_ty);
        let uncached = &q.annotations.no_cache;
        let manually_allocated = &q.annotations.manually_allocated;
        let desc_name = q
            .annotations
            .desc
            .as_ref()
            .map(|v| &v.1.ctx_name)
            .and_then(Option::as_ref);
        let args = q
            .annotations
            .desc
            .as_ref()
            .map(|(_, DescAnnotation { args, .. })| quote! { (#args) });
        let desc_key = args.as_ref().map(|_| quote! { : key });
        let key = &q.key;
        let value = &q.value;
        let name = &q.name;
        macro_invocations.extend(quote! {
            $m!(#allocated_ty;#uncached;#desc_name #desc_key #args;#manually_allocated;#name;#key;#value);
        });
    }
    Ok(quote! {
        macro_rules! all_queries {
            ($m:ident) => { #macro_invocations };
            (names $m:ident) => { $m!(#names); };
        }
    })
}
