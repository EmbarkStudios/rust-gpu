use crate::builder::Builder;
use crate::builder_spirv::{SpirvValue, SpirvValueExt, SpirvValueKind};
use crate::spirv_type::SpirvType;
use rspirv::spirv::Word;
use rustc_codegen_ssa::traits::{BaseTypeMethods, BuilderMethods};
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
        let element_ty = match runtime_array_type {
			SpirvType::Pointer { pointee } => {
				match self.lookup_type(pointee) {
					SpirvType::RuntimeArray { element } => {
						element
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
		};

        let ptr_element = self.type_ptr_to(element_ty);
        let element = self
            .emit()
            .access_chain(
                ptr_element,
                None,
                runtime_array.def(self),
                [index.def(self)],
            )
            .unwrap()
            .with_type(ptr_element);

        match self.lookup_type(element_ty) {
            SpirvType::InterfaceBlock { .. } => {
                // array of buffer descriptors
                let inner = self.struct_gep(element_ty, element, 0);
                match pass_mode {
                    PassMode::Direct(_) => {
                        // element is sized
                        if inner.ty == result_type {
                            inner
                        } else {
                            self.fatal(format!(
								"runtime_array_index_intrinsic expected result_type to equal RuntimeArray's InterfaceBlock's inner_type: {:?} == {:?}",
								self.lookup_type(result_type).debug(result_type, self),
								self.lookup_type(inner.ty).debug(inner.ty, self)
							))
                        }
                    }
                    PassMode::Pair(_, _) => {
                        // element is a slice
                        match self.lookup_type(result_type) {
                            SpirvType::Adt { field_types, .. } if field_types.len() == 2
								&& matches!(self.lookup_type(field_types[0]), SpirvType::Pointer {..})
								&& field_types[1] == self.type_isize() => {
							}
							_ => self.fatal(format!(
								"Expected element of RuntimeArray to be a plain slice, like `&RuntimeArray<[u32]>`, but got {:?}!",
								self.lookup_type(result_type).debug(result_type, self)
							))
                        };
                        let len = self
                            .emit()
                            .array_length(self.type_isize(), None, element.def(self), 0)
                            .unwrap();
                        self.emit()
                            .composite_construct(result_type, None, [inner.def(self), len])
                            .unwrap()
                            .with_type(result_type)
                    }
                    _ => unreachable!(),
                }
            }
            _ => {
                // array of UniformConstant (image, sampler, etc.) descriptors
                if ptr_element == result_type {
                    element
                } else {
                    self.fatal(format!(
						"runtime_array_index_intrinsic expected result_type to equal RuntimeArray's element_ty: {:?} == {:?}",
						self.lookup_type(result_type).debug(result_type, self),
						self.lookup_type(ptr_element).debug(ptr_element, self)
					))
                }
            }
        }
    }
}
