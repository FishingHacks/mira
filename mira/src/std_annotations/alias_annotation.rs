use super::*;

#[derive(Debug, Clone)]
pub struct ExternAliasAnnotation(pub GlobalStr);
impl Annotation for ExternAliasAnnotation {
    fn get_name(&self) -> &'static str {
        "alias"
    }
    fn is_valid_for(
        &self,
        thing: super::AnnotationReceiver,
        annotations: &super::Annotations,
    ) -> bool {
        thing == AnnotationReceiver::ExternalFunction
            && annotations.get_annotations::<Self>().count() < 2
    }
}
impl Display for ExternAliasAnnotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("@alias({:?})", self.0))
    }
}
pub fn parse(mut tokens: Vec<Token>, loc: Location) -> Result<ExternAliasAnnotation, ParsingError> {
    if tokens.len() < 1 {
        return Err(ParsingError::ExpectedArbitrary {
            loc,
            expected: TokenType::StringLiteral,
            found: TokenType::Eof,
        });
    }
    if tokens.len() > 1 {
        return Err(ParsingError::ExpectedArbitrary {
            loc: tokens.remove(1).location,
            expected: TokenType::Eof,
            found: tokens[1].typ,
        });
    }
    let first_tok = tokens.remove(0);
    if first_tok.typ != TokenType::StringLiteral {
        return Err(ParsingError::ExpectedArbitrary {
            loc: first_tok.location,
            expected: TokenType::StringLiteral,
            found: first_tok.typ,
        });
    }
    let Some(Literal::String(v)) = first_tok.literal else {
        return Err(ParsingError::InvalidTokenization {
            loc: first_tok.location,
        });
    };
    Ok(ExternAliasAnnotation(v))
}
