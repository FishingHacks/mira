use std::{
    any::Any,
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Display, Write},
};

use crate::{
    error::ParsingError,
    lang_items::LangItemAnnotation,
    tokenizer::{Location, Token},
};

pub trait Annotation: Display + Debug + Any {
    fn get_name(&self) -> &'static str;
}

pub trait ClonableAnnotation: Annotation {
    fn clone_box(&self) -> Box<dyn ClonableAnnotation>;
    fn as_any(&self) -> &dyn Any;
}

impl<T: Annotation + Clone + 'static> ClonableAnnotation for T {
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

type AnnotationParser =
    Box<dyn Fn(Vec<Token>, Location) -> Result<Box<dyn ClonableAnnotation>, ParsingError>>;

thread_local! {
    static ANNOTATIONS_REGISTRY: RefCell<HashMap<&'static str, AnnotationParser>> = {
        let mut hashmap: HashMap<&'static str, AnnotationParser> = HashMap::new();

        hashmap.insert("lang", Box::new(|a, b| Ok(Box::new(LangItemAnnotation::parse(a, b)?))));

        RefCell::new(hashmap)
    };
}

pub fn parse_annotation(
    name: &str,
    tokens: Vec<Token>,
    loc: Location,
) -> Result<Box<dyn ClonableAnnotation>, ParsingError> {
    ANNOTATIONS_REGISTRY.with_borrow(|annotations| {
        if let Some(parser) = annotations.get(name) {
            parser(tokens, loc)
        } else {
            Err(ParsingError::UnknownAnnotation {
                loc,
                name: name.to_string(),
            })
        }
    })
}

#[derive(Debug, Default, Clone)]
pub struct Annotations {
    annotations: Vec<Box<dyn ClonableAnnotation>>,
}

impl Display for Annotations {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for annotation in self.annotations.iter() {
            Display::fmt(annotation, f)?;
            f.write_char('\n')?;
        }
        Ok(())
    }
}

impl Annotations {
    pub const fn new() -> Self {
        Self {
            annotations: Vec::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.annotations.len()
    }
    pub fn is_empty(&self) -> bool {
        self.annotations.is_empty()
    }

    pub fn push_annotation(
        &mut self,
        name: &str,
        tokens: Vec<Token>,
        loc: Location,
    ) -> Result<(), ParsingError> {
        self.annotations.push(parse_annotation(name, tokens, loc)?);
        Ok(())
    }

    pub fn get_annotations<'a, T: ClonableAnnotation + 'static>(
        &'a self,
    ) -> impl Iterator<Item = &'a T> {
        self.annotations
            .iter()
            .filter_map(|v| v.as_any().downcast_ref::<T>())
    }

    pub fn get_first_annotation<'a, T: ClonableAnnotation + 'static>(&'a self) -> Option<&'a T> {
        self.annotations
            .iter()
            .filter_map(|v| v.as_any().downcast_ref::<T>())
            .next()
    }

    pub fn get_annotations_indexed<'a, T: ClonableAnnotation + 'static>(
        &'a self,
    ) -> impl Iterator<Item = (&'a T, usize)> {
        self.annotations
            .iter()
            .enumerate()
            .filter_map(|(idx, v)| Some((v.as_any().downcast_ref::<T>()?, idx)))
    }

    pub fn get_first_annotation_indexed<'a, T: ClonableAnnotation + 'static>(
        &'a self,
    ) -> Option<(&'a T, usize)> {
        self.annotations
            .iter()
            .enumerate()
            .filter_map(|(idx, v)| Some((v.as_any().downcast_ref::<T>()?, idx)))
            .next()
    }

    pub fn remove_annotation(&mut self, name: &str) {
        for i in 1..=self.annotations.len() {
            let i = self.annotations.len() - i;
            if self.annotations[i].get_name() == name {
                self.annotations.remove(i);
            }
        }
    }
}
