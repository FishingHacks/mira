use inkwell::{support::LLVMString, targets::TargetTriple};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CodegenError {
    #[error("[LLVM Native]: {0}")]
    LLVMNative(LLVMString),
    #[error("Unknown Triple: {0}")]
    UnknownTriple(TargetTriple),
}

impl From<LLVMString> for CodegenError {
    fn from(value: LLVMString) -> Self {
        Self::LLVMNative(value)
    }
}
