type AnnotationParser =
    Box<dyn Fn(TokenStream) -> Result<Box<dyn ClonableAnnotation>, ParsingError>>;
use crate::annotations::ClonableAnnotation;
pub use crate::annotations::{Annotation, AnnotationReceiver, Annotations};
pub use crate::error::ParsingError;
pub use crate::globals::GlobalStr;
pub use crate::tokenizer::TokenType;
pub use crate::tokenstream::TokenStream;
use std::collections::HashMap;
pub use std::fmt::{Display, Write};

//pub mod alias;
//pub mod ext_vararg;

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
    section
);
