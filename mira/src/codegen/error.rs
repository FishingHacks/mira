use inkwell::{builder::BuilderError, support::LLVMString, targets::TargetTriple};
use mira_macros::ErrorData;

#[derive(ErrorData, Debug)]
#[no_arena_lifetime]
pub enum CodegenError {
    #[error("[LLVM Native]: {}", _0)]
    LLVMNative(String),
    #[error("Unknown Triple: {_0}")]
    UnknownTriple(String),
    #[error("{_0}")]
    Builder(BuilderError),
    #[error("Failed to write assembly: {_0}")]
    WriteAssemblyError(std::io::Error),
    #[error("Failed to write binary object: {_0}")]
    WriteObjectError(std::io::Error),
    #[error("Failed to write llvm bitcode: {_0}")]
    WriteBitcodeError(std::io::Error),
    #[error("Failed to write llvm ir: {_0}")]
    WriteLLVMIRError(std::io::Error),
}

impl From<LLVMString> for CodegenError {
    fn from(value: LLVMString) -> Self {
        Self::LLVMNative(value.to_string())
    }
}

impl CodegenError {
    pub fn unknown_triple(triple: TargetTriple) -> Self {
        Self::UnknownTriple(triple.to_string())
    }
}

impl From<BuilderError> for CodegenError {
    fn from(value: BuilderError) -> Self {
        Self::Builder(value)
    }
}
