use inkwell::{builder::BuilderError, support::LLVMString, targets::TargetTriple};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CodegenError {
    #[error("[LLVM Native]: {}", _0.to_string_lossy())]
    LLVMNative(LLVMString),
    #[error("Unknown Triple: {0}")]
    UnknownTriple(TargetTriple),
    #[error("{0}")]
    Builder(#[from] BuilderError),
}

impl From<LLVMString> for CodegenError {
    fn from(value: LLVMString) -> Self {
        Self::LLVMNative(value)
    }
}
