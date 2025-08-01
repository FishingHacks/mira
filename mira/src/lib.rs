// do not scream at us (for now)
//#![allow(unused_variables)]

pub const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const AUTHORS: &str = env!("CARGO_PKG_AUTHORS");
pub mod annotations;
mod builtin_macros;
pub mod codegen;
pub mod error;
pub mod lang_items;
pub mod linking;
pub mod module;
pub mod parser;
pub(crate) mod std_annotations;
pub mod target;
pub mod tokenizer;
pub mod tokenstream;
pub mod typechecking;
pub use parser::module_resolution;
pub mod context;
pub mod optimizations;
pub mod progress_bar;
pub mod slab;
pub mod store;
pub(crate) mod threadpool;
pub use mira_errors::{AsciiPrinter, Output, UnicodePrinter};
pub use mira_spans::interner::symbols;
pub use mira_spans::Arena;
