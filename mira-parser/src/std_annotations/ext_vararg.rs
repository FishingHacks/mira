use super::*;

#[derive(Debug, Clone)]
pub struct ExternVarArg;
impl Annotation for ExternVarArg {
    fn get_name(&self) -> &'static str {
        "ext_vararg"
    }
    fn is_valid_for(
        &self,
        thing: super::AnnotationReceiver,
        annotations: &super::Annotations,
    ) -> bool {
        thing == AnnotationReceiver::ExternalFunction
            && annotations.get_annotations::<Self>().count() < 2
    }
}
impl Display for ExternVarArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("@ext_vararg()")
    }
}
pub fn parse(mut tokens: TokenStream) -> Result<ExternVarArg, ParsingError> {
    tokens.finish().map(|_| ExternVarArg)
}
