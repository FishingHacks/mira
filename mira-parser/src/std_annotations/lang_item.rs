use super::*;

#[derive(Clone, Debug)]
pub struct LangItemAnnotation(String);

annotation!(LangItemAnnotation(self) = "lang" {
    is_valid_for: |thing, _annotations| matches!(
        thing,
        AnnotationReceiver::Function
            | AnnotationReceiver::ExternalFunction
            | AnnotationReceiver::Struct
            | AnnotationReceiver::Trait
            | AnnotationReceiver::Static
    ),
    parse: |tokens| {
        let (item, _) = tokens.expect_string()?;
        tokens.finish()?;
        Ok(LangItemAnnotation(item.to_string()))
    },
    display: |f| Display::fmt(&StrIdentDisplay(&self.0), f),
});

impl LangItemAnnotation {
    pub fn get_langitem(&self) -> &str {
        &self.0
    }
}
