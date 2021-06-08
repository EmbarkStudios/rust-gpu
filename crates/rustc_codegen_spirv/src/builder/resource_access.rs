use super::Builder;
use crate::builder_spirv::{SpirvValue, SpirvValueExt};
use crate::spirv_type::SpirvType;
use rspirv::spirv::Word;

impl<'a, 'tcx> Builder<'a, 'tcx> {
    pub(crate) fn codegen_resource_access(
        &mut self,
        result_type: Word,
        args: &[SpirvValue],
    ) -> SpirvValue {
        if !self.bindless() {
            self.fatal("Need to run the compiler with -Ctarget-feature=+bindless to be able to use the bindless features");
        }

        let bindless_idx = args[0].def(self);

        match self.lookup_type(result_type) {
            SpirvType::Sampler => {
                let result_ptr = SpirvType::Pointer {
                    pointee: result_type,
                }
                .def(rustc_span::DUMMY_SP, self);

                let set = self.cx.sampler_bindless_descriptor_set();
                let access = self
                    .emit()
                    .access_chain(result_ptr, None, set, vec![bindless_idx])
                    .unwrap();
                self.emit()
                    .load(result_type, None, access, None, std::iter::empty())
                    .unwrap()
                    .with_type(result_type)
            }
            SpirvType::Image { .. } => {
                let result_ptr = SpirvType::Pointer {
                    pointee: result_type,
                }
                .def(rustc_span::DUMMY_SP, self);

                let set = self.cx.texture_bindless_descriptor_set(result_type);
                let access = self
                    .emit()
                    .access_chain(result_ptr, None, set, vec![bindless_idx])
                    .unwrap();
                self.emit()
                    .load(result_type, None, access, None, std::iter::empty())
                    .unwrap()
                    .with_type(result_type)
            }
            SpirvType::AccelerationStructureKhr => {
                let result_ptr = SpirvType::Pointer {
                    pointee: result_type,
                }
                .def(rustc_span::DUMMY_SP, self);

                let set = self.cx.acceleration_structure_bindless_descriptor_set();
                let access = self
                    .emit()
                    .access_chain(result_ptr, None, set, vec![bindless_idx])
                    .unwrap();
                self.emit()
                    .load(result_type, None, access, None, std::iter::empty())
                    .unwrap()
                    .with_type(result_type)
            }
            SpirvType::Pointer { pointee } => match self.lookup_type(pointee) {
                SpirvType::RuntimeArray { .. } => {
                    let uint_ty = SpirvType::Integer(32, false).def(rustc_span::DUMMY_SP, self);
                    let zero = self.constant_int(uint_ty, 0).def(self);

                    let set = self.cx.buffer_bindless_descriptor_set(pointee);
                    self.emit()
                        .access_chain(result_type, None, set, vec![bindless_idx, zero])
                        .unwrap()
                        .with_type(result_type)
                }
                ty => self.fatal(&format!(
                    "Pointer({:?}) unsupported for resource access",
                    ty
                )),
            },
            ty => self.fatal(&format!("{:?} unsupported for resource access", ty)),
        }
    }
}
