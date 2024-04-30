use crate::builder::Builder;
use crate::builder_spirv::{SpirvValue, SpirvValueExt, SpirvValueKind};
use crate::spirv_type::SpirvType;
use rspirv::spirv::Word;
use rustc_target::abi::call::PassMode;

impl<'a, 'tcx> Builder<'a, 'tcx> {
    /// Note: DOES NOT do bounds checking! Bounds checking is expected to be done in the caller.
    pub fn codegen_runtime_array_index_intrinsic(
        &mut self,
        result_type: Word,
        args: &[SpirvValue],
        pass_mode: &PassMode,
    ) -> SpirvValue {
        match pass_mode {
            PassMode::Ignore => {
                return SpirvValue {
                    kind: SpirvValueKind::IllegalTypeUsed(result_type),
                    ty: result_type,
                };
            }
            // PassMode::Pair is identical to PassMode::Direct - it's returned as a struct
            PassMode::Direct(_) | PassMode::Pair(_, _) => (),
            PassMode::Cast { .. } => {
                self.fatal("PassMode::Cast not supported in codegen_runtime_array_index_intrinsic")
            }
            PassMode::Indirect { .. } => self
                .fatal("PassMode::Indirect not supported in codegen_runtime_array_index_intrinsic"),
        }

        // Signatures:
        // fn <T: ?Sized>(runtime_array: &RuntimeArray<T>, index: usize) -> &T
        // fn <T: ?Sized>(runtime_array: &mut RuntimeArray<T>, index: usize) -> &mut T
        if args.len() != 2 {
            self.fatal(format!(
                "runtime_array_index_intrinsic should have 3 args, it has {}",
                args.len()
            ));
        }
        let runtime_array = args[0];
        let index = args[1];

        let runtime_array_type = self.lookup_type(runtime_array.ty);
        match runtime_array_type {
            SpirvType::Pointer { pointee } => {
                match self.lookup_type(pointee) {
					SpirvType::RuntimeArray { element } => {
						let indices = match self.lookup_type(element) {
							SpirvType::InterfaceBlock { .. } => {
								vec![index.def(self), self.constant_i32(self.span(), 0).def(self)]
							}
							_ => vec![index.def(self)],
						};

						let mut builder = self.emit();
						let obj = builder
							.access_chain(result_type, None, runtime_array.def(self), indices)
							.unwrap();
						// FIXME OpAccessChain's result_type is a Pointer<float>
						// self.fatal(format!("result_type {:?}", self.lookup_type(result_type)));
						// builder.store(at, obj, None, []).unwrap().with_type(result_type);
						obj.with_type(result_type)
					}
					_ => self.fatal(format!(
						"runtime_array_index_intrinsic args[0] is {:?} and not a Pointer to a RuntimeArray!",
						runtime_array_type
					)),
				}
            }
            _ => self.fatal(format!(
                "runtime_array_index_intrinsic args[0] is {:?} and not a Pointer!",
                runtime_array_type
            )),
        }
    }
}
