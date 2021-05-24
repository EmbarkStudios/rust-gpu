use super::Builder;
use crate::builder_spirv::{SpirvValue, SpirvValueExt};
use rspirv::spirv::{CLOp, GLOp, Word};
use rspirv::{dr::Operand, spirv::Capability};

const GLSL_STD_450: &str = "GLSL.std.450";
const OPENCL_STD: &str = "OpenCL.std";

/// Manager for OpExtInst/OpExtImport instructions
#[derive(Default)]
pub struct ExtInst {
    glsl: Option<Word>,
    opencl: Option<Word>,
    integer_functions_2_intel: bool,
}

impl ExtInst {
    pub fn import_glsl<'a, 'tcx>(&mut self, bx: &Builder<'a, 'tcx>) -> Word {
        assert!(!bx.target.is_kernel());
        match self.glsl {
            Some(id) => id,
            None => {
                let id = bx.emit_global().ext_inst_import(GLSL_STD_450);
                self.glsl = Some(id);
                id
            }
        }
    }

    pub fn import_opencl<'a, 'tcx>(&mut self, bx: &Builder<'a, 'tcx>) -> Word {
        assert!(bx.target.is_kernel());
        match self.opencl {
            Some(id) => id,
            None => {
                let id = bx.emit_global().ext_inst_import(OPENCL_STD);
                self.opencl = Some(id);
                id
            }
        }
    }

    pub fn require_integer_functions_2_intel<'a, 'tcx>(
        &mut self,
        bx: &Builder<'a, 'tcx>,
        to_zombie: Word,
    ) {
        if !self.integer_functions_2_intel {
            assert!(!bx.target.is_kernel());
            self.integer_functions_2_intel = true;
            if !bx
                .builder
                .has_capability(Capability::IntegerFunctions2INTEL)
            {
                bx.zombie(to_zombie, "capability IntegerFunctions2INTEL is required");
            }
            if !bx
                .builder
                .has_extension("SPV_INTEL_shader_integer_functions2")
            {
                bx.zombie(to_zombie, "extension IntegerFunctions2INTEL is required");
            }
        }
    }
}

impl<'a, 'tcx> Builder<'a, 'tcx> {
    pub fn gl_op(
        &mut self,
        op: GLOp,
        result_type: Word,
        args: impl AsRef<[SpirvValue]>,
    ) -> SpirvValue {
        let args = args.as_ref();
        let glsl = self.ext_inst.borrow_mut().import_glsl(self);
        self.emit()
            .ext_inst(
                result_type,
                None,
                glsl,
                op as u32,
                args.iter().map(|a| Operand::IdRef(a.def(self))),
            )
            .unwrap()
            .with_type(result_type)
    }

    pub fn cl_op(
        &mut self,
        op: CLOp,
        result_type: Word,
        args: impl AsRef<[SpirvValue]>,
    ) -> SpirvValue {
        let args = args.as_ref();
        let opencl = self.ext_inst.borrow_mut().import_opencl(self);
        self.emit()
            .ext_inst(
                result_type,
                None,
                opencl,
                op as u32,
                args.iter().map(|a| Operand::IdRef(a.def(self))),
            )
            .unwrap()
            .with_type(result_type)
    }
}
