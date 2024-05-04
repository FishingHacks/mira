use std::fmt::{Debug, Display};

use crate::tokens::Loc;

#[derive(Debug)]
pub enum LangErrType {
    ExpectedStrs(Box<[&'static str]>),
    ExpectedSomething,
}

impl Display for LangErrType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExpectedStrs(strings) => {
                f.write_str("Expected one of the following: ")?;
                for i in 0..strings.len() {
                    f.write_fmt(format_args!("`{}`", strings[i]))?;
                    if i < strings.len() - 2 {
                        f.write_str(", ")?;
                    }
                    if i == strings.len() - 2 {
                        f.write_str(" or ")?;
                    }
                }
                Ok(())
            }

            Self::ExpectedSomething => {
                f.write_str("Expected something, but found nothing instead")
            }
        }
    }
}

pub struct LangErr {
    err: LangErrType,
    loc: Loc
}

impl LangErr {
    pub fn new(err: LangErrType, loc: Loc) -> Self {
        Self { err, loc }
    }
}

impl Debug for LangErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.loc, f)?;
        f.write_fmt(format_args!(": {}", self.err))
    }
}