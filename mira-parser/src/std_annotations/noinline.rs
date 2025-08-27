use super::*;

#[derive(Debug, Clone)]
pub struct Noinline;
impl Annotation for Noinline {
    fn get_name(&self) -> &'static str {
        "noinline"
    }

    fn is_valid_for(&self, thing: super::AnnotationReceiver, _: &super::Annotations) -> bool {
        thing == AnnotationReceiver::Function || thing == AnnotationReceiver::ExternalFunction
    }
}
impl Display for Noinline {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("@{}()", self.get_name()))
    }
}
pub fn parse<'a>(
    mut tokens: TokenStream<'a>,
    _: SharedCtx<'a>,
) -> Result<Noinline, ParsingError<'a>> {
    tokens.finish()?;
    Ok(Noinline)
}
