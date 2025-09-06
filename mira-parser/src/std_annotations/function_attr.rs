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

    fn is_valid_for(&self, thing: AnnotationReceiver, _: &Annotations<'_>) -> bool {
        thing == AnnotationReceiver::ExternalFunction || thing == AnnotationReceiver::Function
    }
}

impl Display for FunctionAttr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('@')?;
        f.write_str(self.get_name())?;
        f.write_char('(')?;
        match self.hotness {
            None => {}
            Some(true) => f.write_str("hot")?,
            Some(false) => f.write_str("cold")?,
        }
        f.write_char(')')
    }
}

pub fn parse<'a>(
    mut tokens: TokenStream<'a>,
    _: SharedCtx<'a>,
) -> Result<FunctionAttr, ParsingError<'a>> {
    let mut function_attr = FunctionAttr::default();
    let mut needs_comma = false;
    while !tokens.is_at_end() {
        if needs_comma {
            tokens.expect(TokenType::Comma)?;
        }
        needs_comma = true;
        let ident = tokens.expect_identifier()?;
        match &*ident {
            "hot" => function_attr.hotness = Some(true),
            "cold" => function_attr.hotness = Some(false),
            _ => {
                return Err(ParsingError::InvalidFunctionAttribute {
                    span: ident.span(),
                    name: ident.symbol(),
                });
            }
        }
    }
    Ok(function_attr)
}
