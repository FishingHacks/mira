use super::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CallConvAnnotation {
    C,
    Naked,
    Fast,
    Cold,
    Inline,
}

impl Annotation for CallConvAnnotation {
    fn get_name(&self) -> &'static str {
        "callconv"
    }

    fn is_valid_for(&self, thing: AnnotationReceiver, annotations: &Annotations) -> bool {
        if thing == AnnotationReceiver::Function
            || (thing == AnnotationReceiver::ExternalFunction
                && *self != CallConvAnnotation::Inline)
        {
            return annotations.get_annotations::<Self>().nth(1).is_none();
        } else {
            false
        }
    }
}

impl Display for CallConvAnnotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('@')?;
        f.write_str(self.get_name())?;
        f.write_char('(')?;
        match self {
            CallConvAnnotation::C => f.write_char('c')?,
            CallConvAnnotation::Naked => f.write_str("naked")?,
            CallConvAnnotation::Fast => f.write_str("fast")?,
            CallConvAnnotation::Cold => f.write_str("cold")?,
            CallConvAnnotation::Inline => f.write_str("inline")?,
        }
        f.write_char(')')
    }
}

pub fn parse(mut tokens: TokenStream) -> Result<CallConvAnnotation, ParsingError> {
    let (name, loc) = tokens.expect_remove_identifier()?;
    tokens.finish()?;
    name.with(|v| match v {
        "c" | "C" => Some(CallConvAnnotation::C),
        "naked" | "Naked" => Some(CallConvAnnotation::Naked),
        "fast" | "Fast" => Some(CallConvAnnotation::Fast),
        "cold" | "Cold" => Some(CallConvAnnotation::Cold),
        "inline" | "Inline" => Some(CallConvAnnotation::Inline),
        _ => None,
    })
    .ok_or_else(|| ParsingError::InvalidIntrinsic { loc, name })
}
