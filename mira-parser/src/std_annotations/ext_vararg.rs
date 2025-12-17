use super::*;

#[derive(Debug, Clone, Copy)]
pub struct ExternVarArg;

annotation!(ExternVarArg(self) = "ext_vararg" {
    is_valid_for: |thing, annotations|
        thing == AnnotationReceiver::ExternalFunction && annotations.get_annotations::<Self>().count() < 2,
    parse: |tokens| tokens.finish().map(|()| ExternVarArg),
    display: |f| Ok(())
});
