use inkwell::builder::{Builder, BuilderError};
use inkwell::intrinsics::Intrinsic as LLVMIntrinsic;
use inkwell::module::Module;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicMetadataValueEnum, CallSiteValue, FunctionValue};

pub(super) struct Intrinsic(LLVMIntrinsic);

impl Intrinsic {
    pub fn build_call<'ctx>(
        &self,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
        values: &[BasicMetadataValueEnum<'ctx>],
        types: &[BasicTypeEnum<'ctx>],
    ) -> Result<CallSiteValue<'ctx>, BuilderError> {
        builder.build_direct_call(self.get_decl(module, types), values, "")
    }

    pub fn get_decl<'ctx>(
        &self,
        module: &Module<'ctx>,
        types: &[BasicTypeEnum<'ctx>],
    ) -> FunctionValue<'ctx> {
        self.0
            .get_declaration(module, types)
            .expect("could not get declaration for known intrinsic")
    }
}

macro_rules! llvm_intrinsics {
    ($($intrinsic: ident $name:literal),* $(,)?) => {
        pub(super) struct LLVMIntrinsics {
            $(pub $intrinsic: Intrinsic),*
        }

        impl LLVMIntrinsics {
            pub fn init() -> Self {
                Self {
                    $($intrinsic: Intrinsic(LLVMIntrinsic::find(concat!("llvm.", $name)).expect(concat!("failed to get intrinsic llvm.", $name)))),*
                }
            }
        }
    };
}

llvm_intrinsics! {
    trap "trap",
    breakpoint "debugtrap",
}
