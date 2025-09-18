use inkwell::types::BasicType;
use mira_typeck::ir::ValueId;

use super::{FunctionCodegenContext, Result};

impl FunctionCodegenContext<'_, '_, '_, '_, '_> {
    pub(super) fn build_asm(
        &mut self,
        dst: ValueId,
        inputs: &[ValueId],
        registers: &str,
        volatile: bool,
        asm: &str,
    ) -> Result {
        let input_types = inputs
            .iter()
            .map(|v| {
                let ty = self.substitute(self.ir.get_ty(*v));
                self.basic_type(&ty).into()
            })
            .collect::<Vec<_>>();
        let ret_ty = self.substitute(self.ir.get_ty(dst));
        let fn_ty = if ret_ty.is_voidlike() {
            self.ctx.context.void_type().fn_type(&input_types, false)
        } else {
            self.basic_type(&ret_ty).fn_type(&input_types, false)
        };
        let mut constraints = registers.to_string();

        // For some targets, Clang unconditionally adds some clobbers to all inline assembly.
        // While this is probably not strictly necessary, if we don't follow Clang's lead
        // here then we may risk tripping LLVM bugs since anything not used by Clang tends
        // to be buggy and regress often.
        let cpu = self.ctx.machine.get_cpu();
        match cpu.to_bytes() {
            b"x86" | b"x86_64" | b"x86-64" => {
                if !constraints.is_empty() {
                    constraints.push(',');
                }
                constraints.push_str("~{dirflag},~{fpsr},~{flags}");
            }
            cpu => unreachable!("unhandled target cpu: {}", String::from_utf8_lossy(cpu)),
        }

        let asm_fn_ptr = self.ctx.context.create_inline_asm(
            fn_ty,
            asm.to_string(),
            constraints,
            volatile,
            false,
            None,
            false,
        );
        let args = inputs
            .iter()
            .map(|v| self.get_value(*v).into())
            .collect::<Vec<_>>();
        let val = self.build_indirect_call(fn_ty, asm_fn_ptr, &args, "")?;
        self.push_value(
            dst,
            val.try_as_basic_value()
                .left_or_else(|_| self.ctx.default_types.empty_struct.const_zero().into()),
        );
        Ok(())
    }
}
