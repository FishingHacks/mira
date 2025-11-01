use inkwell::{builder::BuilderError, support::LLVMString, targets::TargetTriple};
use mira_macros::ErrorData;
use mira_spans::Span;

#[derive(ErrorData, Debug)]
pub enum CodegenError<'arena> {
    #[error("[LLVM]: {_0}")]
    LLVMNative(String),
    #[error("Unknown Triple: {_0}")]
    UnknownTriple(String),
    #[error("{_0}")]
    Builder(
        BuilderError,
        #[primary_label("while trying to build an operation for")] Span<'arena>,
    ),
    #[error("{_0}")]
    #[note("While trying to build a deferred block")]
    DeferredBlockBuild(BuilderError),
    #[error("Failed to write assembly: {_0}")]
    WriteAssemblyError(std::io::Error),
    #[error("Failed to write binary object: {_0}")]
    WriteObjectError(std::io::Error),
    #[error("Failed to write llvm bitcode: {_0}")]
    WriteBitcodeError(std::io::Error),
    #[error("Failed to write llvm ir: {_0}")]
    WriteLLVMIRError(std::io::Error),
}

impl From<LLVMString> for CodegenError<'_> {
    fn from(value: LLVMString) -> Self {
        Self::LLVMNative(value.to_string())
    }
}

impl CodegenError<'_> {
    pub fn unknown_triple(triple: TargetTriple) -> Self {
        Self::UnknownTriple(triple.to_string())
    }
}
