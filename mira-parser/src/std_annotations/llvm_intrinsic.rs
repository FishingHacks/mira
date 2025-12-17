use super::*;

#[repr(transparent)]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct LLVMIntrinsicAnnotation(Box<str>);

annotation!(LLVMIntrinsicAnnotation(self) = "llvm_intrinsic" {
    is_valid_for: |thing, annotations|
        thing == AnnotationReceiver::Function
            && annotations.get_annotations::<Self>().nth(1).is_none(),
    parse: |tokens| {
        let tok = tokens.expect_one_of(&[TokenType::IdentifierLiteral, TokenType::StringLiteral])?;
        tokens.finish()?;
        let mut s = tok.string_literal().to_string();
        s.insert_str(0, "llvm.");
        Ok(LLVMIntrinsicAnnotation(s.into_boxed_str()))
    },
    display: |f| f.write_str(&self.0[5..]),
});

impl LLVMIntrinsicAnnotation {
    pub fn get(&self) -> &str {
        &self.0
    }
}
