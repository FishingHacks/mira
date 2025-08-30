use crate::annotations::{Annotation, AnnotationParser, AnnotationReceiver, Annotations};
use crate::error::ParsingError;
use crate::tokenstream::TokenStream;
use mira_context::SharedCtx;
use mira_lexer::TokenType;
use mira_lexer::token::StrIdentDisplay;
use std::collections::HashMap;
use std::fmt::{Display, Write};

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
