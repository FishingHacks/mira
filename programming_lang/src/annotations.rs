use std::{
    any::Any,
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Display, Write},
};

use crate::{
    error::ParsingError,
    lang_items::LangItemAnnotation,
    std_annotations,
    tokenizer::{Location, Token},
    typechecking::intrinsics::IntrinsicAnnotation,
};

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
        }
    }
}

pub trait Annotation: Display + Debug + Any {
    fn get_name(&self) -> &'static str;
    fn is_valid_for(&self, thing: AnnotationReceiver, annotations: &Annotations) -> bool {
        true
    }
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
        hashmap.insert("intrinsic", Box::new(|a, b| Ok(Box::new(IntrinsicAnnotation::parse(a, b)?))));
        hashmap.insert("alias", Box::new(|a, b| Ok(Box::new(std_annotations::alias_annotation::parse(a, b)?))));

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
pub struct Annotations(Vec<(Box<dyn ClonableAnnotation>, Location)>);

impl Display for Annotations {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (annotation, _) in self.0.iter() {
            Display::fmt(annotation, f)?;
            f.write_char('\n')?;
        }
        Ok(())
    }
}

impl Annotations {
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
        tokens: Vec<Token>,
        loc: Location,
    ) -> Result<(), ParsingError> {
        self.0
            .push((parse_annotation(name, tokens, loc.clone())?, loc));
        Ok(())
    }

    pub fn are_annotations_valid_for(&self, typ: AnnotationReceiver) -> Result<(), ParsingError> {
        for (annotation, loc) in &self.0 {
            if !annotation.is_valid_for(typ, self) {
                return Err(ParsingError::AnnotationDoesNotGoOn {
                    loc: loc.clone(),
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

    pub fn get_annotations<'a, T: ClonableAnnotation + 'static>(
        &'a self,
    ) -> impl Iterator<Item = &'a T> {
        self.0
            .iter()
            .filter_map(|v| v.0.as_any().downcast_ref::<T>())
    }

    pub fn get_first_annotation<'a, T: ClonableAnnotation + 'static>(&'a self) -> Option<&'a T> {
        self.0
            .iter()
            .filter_map(|v| v.0.as_any().downcast_ref::<T>())
            .next()
    }

    pub fn get_annotations_indexed<'a, T: ClonableAnnotation + 'static>(
        &'a self,
    ) -> impl Iterator<Item = (&'a T, usize)> {
        self.0
            .iter()
            .enumerate()
            .filter_map(|(idx, v)| Some((v.0.as_any().downcast_ref::<T>()?, idx)))
    }

    pub fn get_first_annotation_indexed<'a, T: ClonableAnnotation + 'static>(
        &'a self,
    ) -> Option<(&'a T, usize)> {
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