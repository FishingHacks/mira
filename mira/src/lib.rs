// do not scream at us (for now)
#![allow(unused_variables)]

pub const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const AUTHORS: &str = env!("CARGO_PKG_AUTHORS");
pub mod error;
pub mod globals;
pub mod lang_items;
pub mod module;
pub mod parser;
pub mod tokenizer;
#[macro_use]
mod typ_macros;
pub(crate) mod annotations;
mod builtin_macros;
pub mod codegen;
pub(crate) mod std_annotations;
pub mod typechecking;
