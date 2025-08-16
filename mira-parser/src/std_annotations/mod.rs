use crate::annotations::{Annotation, AnnotationParser, AnnotationReceiver, Annotations};
use crate::error::ParsingError;
use crate::tokenstream::TokenStream;
use mira_lexer::TokenType;
use mira_lexer::token::StrIdentDisplay;
use std::collections::HashMap;
use std::fmt::{Display, Write};

macro_rules! annotations {
    ($($name:ident),* $(,)?) => {
        $(pub mod $name;)*
        pub fn add_annotations(hashmap: &mut HashMap<&'static str, AnnotationParser>) {
            $(hashmap.insert(stringify!($name), Box::new(|stream| Ok(Box::new($name::parse(stream)?))));)*
        }
    };
}

annotations!(
    alias,
    ext_vararg,
    callconv,
    function_attr,
    noinline,
    section,
    lang_item,
    intrinsic,
);
