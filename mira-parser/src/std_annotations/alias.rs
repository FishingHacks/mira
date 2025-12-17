use std::fmt::Debug;

use super::*;

#[derive(Debug, Clone)]
pub struct ExternAliasAnnotation(pub String);

annotation!(ExternAliasAnnotation(self) = "alias" {
    is_valid_for: |thing, annotations|
        thing == AnnotationReceiver::ExternalFunction
            && annotations.get_annotations::<Self>().count() < 2,
    parse: |tokens| {
        let (name, _) = tokens.expect_string()?;
        tokens.finish()?;
        Ok(ExternAliasAnnotation(name.to_string()))
    },
    display: |f| Debug::fmt(&self.0, f),
});
