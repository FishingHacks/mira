use std::{
    any::Any,
    collections::HashMap,
    fmt::{Debug, Display, Write},
    sync::LazyLock,
};

use crate::{
    error::ParsingError, lang_items::LangItemAnnotation, std_annotations, tokenizer::Token,
    tokenstream::TokenStream, typechecking::intrinsics::IntrinsicAnnotation,
};
use mira_spans::Span;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum AnnotationReceiver {
    Struct,
    Function,
    Trait,
    ExternalFunction,
    Variable,
    Static,
    Block,
    If,
    While,
    For,
    Module,
}

impl Display for AnnotationReceiver {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnnotationReceiver::Struct => f.write_str("struct"),
            AnnotationReceiver::Function => f.write_str("function"),
            AnnotationReceiver::Trait => f.write_str("trait"),
            AnnotationReceiver::ExternalFunction => f.write_str("external function"),
            AnnotationReceiver::Variable => f.write_str("variable"),
            AnnotationReceiver::Static => f.write_str("static"),
            AnnotationReceiver::Block => f.write_str("block"),
            AnnotationReceiver::If => f.write_str("if statement"),
            AnnotationReceiver::While => f.write_str("while statement"),
            AnnotationReceiver::For => f.write_str("for statement"),
            AnnotationReceiver::Module => f.write_str("module"),
        }
    }
}

pub trait Annotation: Display + Debug + Any {
    fn get_name(&self) -> &'static str;
    #[allow(unused_variables)]
    fn is_valid_for(&self, thing: AnnotationReceiver, annotations: &Annotations) -> bool {
        true
    }
}

pub trait ClonableAnnotation: Annotation + 'static + Send + Sync {
    fn clone_box(&self) -> Box<dyn ClonableAnnotation>;
    fn as_any(&self) -> &dyn Any;
}

impl<T: Annotation + Clone + 'static + Send + Sync> ClonableAnnotation for T {
    fn clone_box(&self) -> Box<dyn ClonableAnnotation> {
        Box::new(self.clone())
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Clone for Box<dyn ClonableAnnotation> {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}

pub type AnnotationParser =
    Box<dyn Fn(TokenStream) -> Result<Box<dyn ClonableAnnotation>, ParsingError> + Send + Sync>;

static ANNOTATIONS_REGISTRY: LazyLock<HashMap<&'static str, AnnotationParser>> =
    LazyLock::new(|| {
        let mut hashmap: HashMap<&'static str, AnnotationParser> = HashMap::new();

        macro_rules! annotations {
            ($($name:literal => $func:path),* $(,)?) => {
                $(hashmap.insert($name, Box::new(|stream| Ok(Box::new($func(stream)?))));)*
            };
        }

        annotations!(
            "lang" => LangItemAnnotation::parse,
            "intrinsic" => IntrinsicAnnotation::parse,
        );
        std_annotations::add_annotations(&mut hashmap);

        hashmap
    });

pub fn parse_annotation<'arena>(
    name: &str,
    tokens: Vec<Token<'arena>>,
    span: Span<'arena>,
) -> Result<Box<dyn ClonableAnnotation>, ParsingError<'arena>> {
    if let Some(parser) = ANNOTATIONS_REGISTRY.get(name) {
        parser(TokenStream::new(tokens, span))
    } else {
        Err(ParsingError::UnknownAnnotation {
            loc: span,
            name: name.to_string(),
        })
    }
}

#[derive(Debug, Default, Clone)]
pub struct Annotations<'arena>(Vec<(Box<dyn ClonableAnnotation>, Span<'arena>)>);

impl Display for Annotations<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (annotation, _) in self.0.iter() {
            Display::fmt(annotation, f)?;
            f.write_char('\n')?;
        }
        Ok(())
    }
}

impl<'arena> Annotations<'arena> {
    pub const fn new() -> Self {
        Self(Vec::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn push_annotation(
        &mut self,
        name: &str,
        tokens: Vec<Token<'arena>>,
        span: Span<'arena>,
    ) -> Result<(), ParsingError<'arena>> {
        self.0.push((parse_annotation(name, tokens, span)?, span));
        Ok(())
    }

    pub fn are_annotations_valid_for(
        &self,
        typ: AnnotationReceiver,
    ) -> Result<(), ParsingError<'arena>> {
        for (annotation, span) in &self.0 {
            if !annotation.is_valid_for(typ, self) {
                return Err(ParsingError::AnnotationDoesNotGoOn {
                    loc: *span,
                    name: annotation.get_name(),
                    thing: typ,
                });
            }
        }
        Ok(())
    }

    pub fn iter(&self) -> impl Iterator<Item = &dyn ClonableAnnotation> {
        self.0.iter().map(|v| &*v.0)
    }

    pub fn get_annotations<T: ClonableAnnotation + 'static>(&self) -> impl Iterator<Item = &T> {
        self.0
            .iter()
            .filter_map(|v| v.0.as_any().downcast_ref::<T>())
    }

    pub fn has_annotation<T: ClonableAnnotation + 'static>(&self) -> bool {
        self.get_first_annotation::<T>().is_some()
    }

    pub fn get_first_annotation<T: ClonableAnnotation + 'static>(&self) -> Option<&T> {
        self.0
            .iter()
            .filter_map(|v| v.0.as_any().downcast_ref::<T>())
            .next()
    }

    pub fn get_annotations_indexed<T: ClonableAnnotation + 'static>(
        &self,
    ) -> impl Iterator<Item = (&T, usize)> {
        self.0
            .iter()
            .enumerate()
            .filter_map(|(idx, v)| Some((v.0.as_any().downcast_ref::<T>()?, idx)))
    }

    pub fn get_first_annotation_indexed<T: ClonableAnnotation + 'static>(
        &self,
    ) -> Option<(&T, usize)> {
        self.0
            .iter()
            .enumerate()
            .filter_map(|(idx, v)| Some((v.0.as_any().downcast_ref::<T>()?, idx)))
            .next()
    }

    pub fn remove_annotation(&mut self, name: &str) {
        for i in 1..=self.0.len() {
            let i = self.0.len() - i;
            if self.0[i].0.get_name() == name {
                self.0.remove(i);
            }
        }
    }
}
