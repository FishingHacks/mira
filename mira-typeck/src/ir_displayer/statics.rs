use crate::TypedStatic;
use mira_common::store::StoreKey;

use super::Formatter;
use super::typed_literal::Tld;

pub struct StaticDisplay<'a>(pub &'a TypedStatic<'a>);

impl StaticDisplay<'_> {
    pub fn fmt(&self, f: &mut Formatter, id: StoreKey<TypedStatic>) -> std::fmt::Result {
        f.write_value(&self.0.annotations)?;
        f.write_str("static static_")?;
        f.write_value(&id)?;
        f.write_str(": ")?;
        f.write_value(&self.0.type_)?;
        f.write_str(" = ")?;
        Tld(&self.0.value).fmt(f)?;
        f.write_char(';')
    }
}
