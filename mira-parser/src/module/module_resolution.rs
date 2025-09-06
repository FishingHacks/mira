use super::{Module, ProgramFormingError};
use std::{path::Path, sync::Arc};

use mira_spans::{Ident, SourceMap, Span, interner::SpanInterner};

// if $current_file is the $package's root file or is called "mod.mr":
//   -> if "${current_file.path.parent}/$name.mr" exists, return that
//   -> if "${current_file.path.parent}/$name/mod.mr" exists, return that
//   -> return an err
// else
//   -> if "${current_file.path.parent}/${current_file.path.stem}/$name.mr" exists, return that
//   -> if "${current_file.path.parent}/${current_file.path.stem}/$name/mod.mr" exists, return that
//   -> return an err
//
// this is exactly like how rust does it.
pub(super) fn resolve_module<'arena>(
    span_interner: &SpanInterner<'arena>,
    name: Ident<'arena>,
    module: &Module<'arena>,
    source_map: &SourceMap,
    mod_span: Span<'arena>,
    semicolon_span: Span<'arena>,
) -> Result<Arc<Path>, ProgramFormingError<'arena>> {
    if let Some(c) = is_valid_filename(&name) {
        let mut data = name.span().get_span_data();
        data.len = 1;
        data.pos += c as u32;
        let pos = Span::new(data, span_interner);
        return Err(ProgramFormingError::InvalidFileNameErr(name.symbol(), pos));
    }
    let current_file = &module.file;
    let mut searchdir = current_file
        .path
        .parent()
        .expect("every file should have a parent directory")
        .to_path_buf();
    // if this is the package root or ends in `mod.mr`, use the current directory.
    // otherwise, use $cwd/$stem.
    if !module.parent.is_undefined()
        && current_file
            .path
            .file_name()
            .map(|v| v != "mod.mr")
            .unwrap_or(true)
    {
        let file_stem = current_file.path.file_stem().unwrap();
        searchdir.push(file_stem);
    };

    // check for $name.mr first
    searchdir.push(*name.symbol());
    searchdir.set_extension("mr");
    if source_map.exists(&searchdir) && !source_map.is_dir(&searchdir) {
        return Ok(searchdir.into());
    }
    // ...then check for $name/mod.mr
    searchdir.pop();
    searchdir.push(*name.symbol());
    searchdir.push("mod.mr");
    if source_map.exists(&searchdir) && !source_map.is_dir(&searchdir) {
        return Ok(searchdir.into());
    }
    searchdir.pop();
    searchdir.pop();
    let span = mod_span.combine_with([semicolon_span, name.span()], span_interner);
    Err(ProgramFormingError::FileNotFoundErr(
        name.symbol(),
        span,
        searchdir,
    ))
}

fn is_valid_filename(name: &str) -> Option<usize> {
    name.chars().position(filename_char_invalid)
}

fn filename_char_invalid(c: char) -> bool {
    matches!(
        c,
        '.' | '\0' | '<' | '>' | ':' | '"' | '/' | '\\' | '|' | '?' | '*'
    )
}
