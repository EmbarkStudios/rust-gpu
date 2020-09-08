use super::Builder;
use crate::builder_spirv::{SpirvValue, SpirvValueExt};
use rspirv::dr::Operand;
use rspirv::spirv::{GLOp, Word};

const GLSL_STD_450: &str = "GLSL.std.450";

/// Manager for OpExtInst/OpExtImport instructions
#[derive(Default)]
pub struct ExtInst {
    glsl: Option<Word>,
}

impl ExtInst {
    pub fn import_glsl<'a, 'tcx>(&mut self, bx: &Builder<'a, 'tcx>) -> Word {
        match self.glsl {
            Some(id) => id,
            None => {
                let id = bx.emit_global().ext_inst_import(GLSL_STD_450);
                self.glsl = Some(id);
                id
            }
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
}
