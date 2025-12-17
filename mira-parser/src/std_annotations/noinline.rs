use super::*;

#[derive(Debug, Clone, Copy)]
pub struct Noinline;

annotation!(Noinline(self) = "noinline" {
    is_valid_for: |thing, _annotations|
        thing == AnnotationReceiver::Function || thing == AnnotationReceiver::ExternalFunction,
    parse: |tokens| tokens.finish().map(|()| Noinline),
    display: |f| Ok(()),
});
