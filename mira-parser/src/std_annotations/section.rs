use super::*;

#[derive(Debug, Clone)]
pub struct SectionAnnotation(pub String);

annotation!(SectionAnnotation(self) = "section" {
    is_valid_for: |thing, annotations|
        thing == AnnotationReceiver::ExternalFunction
            && annotations.get_annotations::<Self>().count() < 2,
    parse: |tokens| {
        let (name, _) = tokens.expect_string()?;
        tokens.finish()?;
        Ok(SectionAnnotation(name.to_string()))
    },
    display: |f| f.write_str(&self.0),
});
