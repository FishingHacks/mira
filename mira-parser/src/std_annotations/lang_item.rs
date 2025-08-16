use super::*;

#[derive(Clone, Debug)]
pub struct LangItemAnnotation(String);

impl Annotation for LangItemAnnotation {
    fn get_name(&self) -> &'static str {
        "lang"
    }

    fn is_valid_for(&self, thing: AnnotationReceiver, _: &Annotations) -> bool {
        matches!(
            thing,
            AnnotationReceiver::Function
                | AnnotationReceiver::ExternalFunction
                | AnnotationReceiver::Struct
                | AnnotationReceiver::Trait
                | AnnotationReceiver::Static
        )
    }
}

impl LangItemAnnotation {
    pub fn get_langitem(&self) -> &str {
        &self.0
    }
}

impl Display for LangItemAnnotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("@lang({})", StrIdentDisplay(&self.0)))
    }
}

pub fn parse(mut tokens: TokenStream) -> Result<LangItemAnnotation, ParsingError> {
    let (item, _) = tokens.expect_string()?;
    tokens.finish()?;
    Ok(LangItemAnnotation(item.to_string()))
}
