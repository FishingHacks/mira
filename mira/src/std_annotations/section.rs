use super::*;

#[derive(Debug, Clone)]
pub struct SectionAnnotation(pub String);
impl Annotation for SectionAnnotation {
    fn get_name(&self) -> &'static str {
        "section"
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
impl Display for SectionAnnotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("@section({:?})", self.0))
    }
}
pub fn parse(mut tokens: TokenStream) -> Result<SectionAnnotation, ParsingError> {
    let (name, _) = tokens.expect_string()?;
    tokens.finish()?;
    Ok(SectionAnnotation(name.to_string()))
}
