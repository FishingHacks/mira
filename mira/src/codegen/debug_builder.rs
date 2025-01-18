use std::{path::Path, sync::Arc};

use inkwell::{
    context::Context,
    debug_info::{DICompileUnit, DIFile, DILocation, DINamespace, DIScope, DebugInfoBuilder},
};
use parking_lot::RwLock;

use crate::tokenizer::Location;

pub struct DebugContext<'ctx> {
    pub(super) builder: DebugInfoBuilder<'ctx>,
    pub(super) current_scope: RwLock<DIScope<'ctx>>,
    pub(super) compile_unit: DICompileUnit<'ctx>,
    pub(super) modules: Vec<DINamespace<'ctx>>,
}

impl<'ctx> DebugContext<'ctx> {
    pub fn get_location(&self, ctx: &'ctx Context, loc: &Location) -> DILocation<'ctx> {
        self.builder.create_debug_location(
            ctx,
            loc.line,
            loc.column,
            *self.current_scope.read(),
            None,
        )
    }

    pub fn get_file(&self, file: &Arc<Path>) -> DIFile<'ctx> {
        let file_name = file
            .file_name()
            .map(|v| v.to_string_lossy())
            .unwrap_or_else(|| "".into());
        let directory = file
            .parent()
            .map(|v| v.to_string_lossy())
            .unwrap_or_else(|| "".into());
        self.builder.create_file(&file_name, &directory)
    }

    pub fn set_scope(&self, scope: DIScope<'ctx>) {
        *self.current_scope.write() = scope;
    }
}
