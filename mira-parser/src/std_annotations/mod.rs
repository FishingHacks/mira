use crate::annotations::{Annotation, AnnotationParser, AnnotationReceiver, Annotations};
use crate::error::ParsingError;
use crate::tokenstream::TokenStream;
use mira_context::SharedCtx;
use mira_lexer::TokenType;
use mira_lexer::token::StrIdentDisplay;
use std::collections::HashMap;
use std::fmt::{Display, Write};

macro_rules! annotation {
    ($tyname:ident($self:ident) = $value:literal {
        is_valid_for: |$thing:ident, $annotations:ident| $is_valid_for_body:expr,
        parse: |$tokens: ident $(, $ctx:ident)?| $parse_body:expr,
        display: |$f:ident| $display_body:expr $(,)?
    }) => {
        impl Annotation for $tyname {
            fn get_name(&$self) -> &'static str {
                $value
            }
            fn is_valid_for(
                &$self,
                $thing: AnnotationReceiver,
                $annotations: &Annotations<'_>,
            ) -> bool {
                $is_valid_for_body
            }
        }

        impl Display for $tyname {
            fn fmt(&$self, $f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                $f.write_str("@")?;
                $f.write_str($value)?;
                $f.write_str("(")?;
                $display_body?;
                $f.write_str(")")
            }
        }

        pub(super) fn parse<'ctx>(
            mut $tokens: TokenStream<'_, 'ctx>,
            annotation!(@parse_args $($ctx)?): SharedCtx<'ctx>,
        ) -> Result<$tyname, ParsingError<'ctx>> {
            $parse_body
        }
    };
    (@parse_args $ctx:ident) => {
        $ctx
    };
    (@parse_args) => {
        _
    };
}

macro_rules! annotations {
    ($($name:ident $(= $annotation_name:literal)?),* $(,)?) => {
        $(pub mod $name;)*
        pub fn add_annotations(hashmap: &mut HashMap<&'static str, AnnotationParser>) {
            $(hashmap.insert(annotations!(@annotation_name $name $(= $annotation_name)?), Box::new(|stream, ctx| Ok(Box::new($name::parse(stream, ctx)?))));)*
        }
    };
    (@annotation_name $name:ident) => { stringify!($name) };
    (@annotation_name $name:ident = $actual_name:literal) => { $actual_name };
}

annotations!(
    alias,
    ext_vararg,
    callconv,
    function_attr,
    noinline,
    section,
    lang_item = "lang",
    intrinsic,
    llvm_intrinsic,
    doc,
);
