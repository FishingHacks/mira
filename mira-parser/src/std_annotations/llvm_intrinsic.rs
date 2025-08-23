use super::*;

#[repr(transparent)]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct LLVMIntrinsicAnnotation(Box<str>);

impl Display for LLVMIntrinsicAnnotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // remove `llvm.`
        f.write_fmt(format_args!("@llvm_intrinsic({:?})", &self.0[5..]))
    }
}

impl Annotation for LLVMIntrinsicAnnotation {
    fn get_name(&self) -> &'static str {
        "llvm_intrinsic"
    }

    fn is_valid_for(&self, thing: AnnotationReceiver, annotations: &Annotations) -> bool {
        thing == AnnotationReceiver::Function
            && annotations.get_annotations::<Self>().nth(1).is_none()
    }
}

impl LLVMIntrinsicAnnotation {
    pub fn get(&self) -> &str {
        &self.0
    }
}

pub fn parse<'arena>(
    mut tokens: TokenStream<'arena>,
) -> Result<LLVMIntrinsicAnnotation, ParsingError<'arena>> {
    let tok = tokens.expect_one_of(&[TokenType::IdentifierLiteral, TokenType::StringLiteral])?;
    tokens.finish()?;
    let mut s = tok.string_literal().to_string();
    s.insert_str(0, "llvm.");
    Ok(LLVMIntrinsicAnnotation(s.into_boxed_str()))
}
