use super::*;

#[derive(Debug, Clone, Copy, Default)]
pub struct FunctionAttr {
    hotness: Option<bool>,
}

annotation!(FunctionAttr(self) = "function_attr" {
    is_valid_for: |thing, _annotations|
         thing == AnnotationReceiver::ExternalFunction || thing == AnnotationReceiver::Function,
    parse: |tokens| {
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
    },
    display: |f| match self.hotness {
        None => Ok(()),
        Some(true) => f.write_str("hot"),
        Some(false) => f.write_str("cold"),
    }
});
