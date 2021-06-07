use super::Builder;
use crate::spirv_type::SpirvType;
use crate::builder_spirv::{SpirvValue, SpirvValueExt};
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
        let sets = self.bindless_descriptor_sets.borrow().unwrap();

        match self.lookup_type(result_type) {
            SpirvType::Sampler => {
                let result_ptr =
                    SpirvType::Pointer { pointee: result_type }.def(rustc_span::DUMMY_SP, self);

                let access = self.emit()
                    .access_chain(result_ptr, None, sets.samplers, vec![bindless_idx])
                    .unwrap();
                self.emit()
                    .load(result_type, None, access, None, std::iter::empty())
                    .unwrap()
                    .with_type(result_type)
            }
            SpirvType::Image { .. } => {
                let result_ptr =
                    SpirvType::Pointer { pointee: result_type }.def(rustc_span::DUMMY_SP, self);

                // todo: sampled 2d
                let access = self.emit()
                    .access_chain(result_ptr, None, sets.sampled_image_2d, vec![bindless_idx])
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

                    self.emit()
                        .access_chain(result_type, None, sets.buffers, vec![bindless_idx, zero])
                        .unwrap()
                        .with_type(result_type)
                }
                ty => self.fatal(&format!("Pointer({:?}) unsupported for resource access", ty)),
            }
            ty => self.fatal(&format!("{:?} unsupported for resource access", ty)),
        }
    }
}
