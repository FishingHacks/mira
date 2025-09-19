use inkwell::values::{BasicValue, BasicValueEnum, PointerValue};
use mira_typeck::ir::ValueId;

use crate::builder::FunctionCodegenContext;

impl<'ctx> FunctionCodegenContext<'ctx, '_, '_, '_, '_> {
    /// In case you already made an alloca for the value, as push_value does an alloca and store if
    /// the value is stack allocated.
    pub(crate) fn push_value_raw(&mut self, id: ValueId, value: PointerValue<'ctx>) {
        assert!(self.ir.is_stack_allocated(id));
        self.scope.insert(id, value.as_basic_value_enum());
    }

    pub(crate) fn push_value(&mut self, id: ValueId, value: BasicValueEnum<'ctx>) {
        if !self.ir.is_stack_allocated(id) {
            self.scope.insert(id, value);
            return;
        }
        let ty = self.substitute(self.ir.get_ty(id));
        let allocated_value = self
            .build_alloca(self.basic_type(&ty), "")
            .expect("failed to build alloca for a stack allocated value");
        self.build_ptr_store(allocated_value, value, ty, false)
            .expect("failed to build store to store a basic value into a stack allocated value");
        self.scope.insert(id, allocated_value.into());
    }

    // gets a scoped value, dereferencing it if it is stack allocated.
    pub(crate) fn get_value(&self, id: ValueId) -> BasicValueEnum<'ctx> {
        if self.scope.get(id).is_none() {
            panic!("cannot get not-yet-defined value _{id}");
        }
        if self.ir.is_stack_allocated(id) {
            let ptr = self.scope[id].into_pointer_value();
            let ty = self.substitute(self.ir.get_ty(id));
            self.build_deref(ptr, ty, false)
                .expect("failed to build a dereference for a stack allocated value")
        } else {
            self.scope[id]
        }
    }

    // gets the pointer to a stack allocated value and panics if the value isn't stack allocated
    pub(crate) fn get_value_ptr(&self, id: ValueId) -> PointerValue<'ctx> {
        if self.scope.get(id).is_none() {
            panic!("cannot get not-yet-defined value _{id}");
        }
        if !self.ir.is_stack_allocated(id) {
            panic!("cannot get pointer to non-stackallocated value _{id}");
        }
        self.scope[id].into_pointer_value()
    }
}
