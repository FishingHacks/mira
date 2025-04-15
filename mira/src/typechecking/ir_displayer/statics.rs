use crate::typechecking::Static;

use super::typed_literal::Tld;
use super::Formatter;

pub struct StaticDisplay<'a>(pub &'a Static);

impl StaticDisplay<'_> {
    pub fn fmt(&self, f: &mut Formatter, id: usize) -> std::fmt::Result {
        f.write_value(&self.0 .4)?;
        f.write_str("static static_")?;
        f.write_value(&id)?;
        f.write_str(": ")?;
        f.write_value(&self.0 .0)?;
        f.write_str(" = ")?;
        Tld(&self.0 .1).fmt(f)?;
        f.write_char(';')
    }
}
