use super::*;

#[derive(Debug, Clone)]
pub struct ExternAliasAnnotation(pub String);
impl Annotation for ExternAliasAnnotation {
    fn get_name(&self) -> &'static str {
        "alias"
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
impl Display for ExternAliasAnnotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("@alias({:?})", self.0))
    }
}
pub fn parse<'a>(
    mut tokens: TokenStream<'a>,
    _: SharedCtx<'a>,
) -> Result<ExternAliasAnnotation, ParsingError<'a>> {
    let (name, _) = tokens.expect_string()?;
    tokens.finish()?;
    Ok(ExternAliasAnnotation(name.to_string()))
}
