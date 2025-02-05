use std::fmt::Write;

use super::*;

#[derive(Debug, Clone, Copy, Default)]
pub struct FunctionAttr {
    hotness: Option<bool>,
}

impl Annotation for FunctionAttr {
    fn get_name(&self) -> &'static str {
        "function_attr"
    }

    fn is_valid_for(&self, thing: AnnotationReceiver, _: &Annotations) -> bool {
        thing == AnnotationReceiver::ExternalFunction || thing == AnnotationReceiver::Function
    }
}

impl Display for FunctionAttr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('@')?;
        f.write_str(self.get_name())?;
        f.write_char('(')?;
        let mut put_comma = self.hotness.is_some();
        match self.hotness {
            None => {}
            Some(true) => f.write_str("hot")?,
            Some(false) => f.write_str("cold")?,
        }
        f.write_char(')')
    }
}

pub fn parse(mut tokens: TokenStream) -> Result<FunctionAttr, ParsingError> {
    let mut function_attr = FunctionAttr::default();
    let mut needs_comma = false;
    while !tokens.is_at_end() {
        if needs_comma {
            tokens.expect_remove_token(TokenType::Comma)?;
        } else {
            needs_comma = true;
        }
        let (name, loc) = tokens.expect_remove_identifier()?;
        name.with(|v| match v {
            "hot" => Some(function_attr.hotness = Some(true)),
            "cold" => Some(function_attr.hotness = Some(false)),
            _ => None,
        })
        .ok_or_else(|| ParsingError::InvalidFunctionAttribute { loc, name })?;
    }
    Ok(function_attr)
}
