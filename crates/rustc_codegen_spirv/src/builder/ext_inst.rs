use super::Builder;
use crate::builder_spirv::{SpirvValue, SpirvValueExt};
use crate::codegen_cx::CodegenCx;
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
        assert!(!bx.kernel_mode);
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
        assert!(bx.kernel_mode);
        match self.opencl {
            Some(id) => id,
            None => {
                let id = bx.emit_global().ext_inst_import(OPENCL_STD);
                self.opencl = Some(id);
                id
            }
        }
    }

    pub fn import_integer_functions_2_intel<'tcx>(&mut self, cx: &CodegenCx<'tcx>) {
        if !self.integer_functions_2_intel {
            assert!(!cx.kernel_mode);
            self.integer_functions_2_intel = true;
            cx.emit_global()
                .extension("SPV_INTEL_shader_integer_functions2");
            cx.emit_global()
                .capability(Capability::IntegerFunctions2INTEL);
        }
    }
}

impl<'a, 'tcx> Builder<'a, 'tcx> {
    pub fn gl_op(&mut self, op: GLOp, args: impl AsRef<[SpirvValue]>) -> SpirvValue {
        let args = args.as_ref();
        let glsl = self.ext_inst.borrow_mut().import_glsl(self);
        self.emit()
            .ext_inst(
                args[0].ty,
                None,
                glsl,
                op as u32,
                args.iter().map(|a| Operand::IdRef(a.def)),
            )
            .unwrap()
            .with_type(args[0].ty)
    }

    pub fn cl_op(&mut self, op: CLOp, args: impl AsRef<[SpirvValue]>) -> SpirvValue {
        let args = args.as_ref();
        let opencl = self.ext_inst.borrow_mut().import_opencl(self);
        self.emit()
            .ext_inst(
                args[0].ty,
                None,
                opencl,
                op as u32,
                args.iter().map(|a| Operand::IdRef(a.def)),
            )
            .unwrap()
            .with_type(args[0].ty)
    }
}
