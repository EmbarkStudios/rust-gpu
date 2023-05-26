use super::Builder;
use crate::builder_spirv::{SpirvValue, SpirvValueExt};
use crate::custom_insts;
use rspirv::spirv::{GLOp, Word};
use rspirv::{dr::Operand, spirv::Capability};

const GLSL_STD_450: &str = "GLSL.std.450";

/// Manager for OpExtInst/OpExtImport instructions
#[derive(Default)]
pub struct ExtInst {
    /// See `crate::custom_insts` for more details on what this entails.
    custom: Option<Word>,

    glsl: Option<Word>,
    integer_functions_2_intel: bool,
}

impl ExtInst {
    pub fn import_custom(&mut self, bx: &Builder<'_, '_>) -> Word {
        if let Some(id) = self.custom {
            id
        } else {
            let id = bx
                .emit_global()
                .ext_inst_import(custom_insts::CUSTOM_EXT_INST_SET.clone());
            self.custom = Some(id);
            id
        }
    }

    pub fn import_glsl(&mut self, bx: &Builder<'_, '_>) -> Word {
        if let Some(id) = self.glsl {
            id
        } else {
            let id = bx.emit_global().ext_inst_import(GLSL_STD_450);
            self.glsl = Some(id);
            id
        }
    }

    pub fn require_integer_functions_2_intel(&mut self, bx: &Builder<'_, '_>, to_zombie: Word) {
        if !self.integer_functions_2_intel {
            self.integer_functions_2_intel = true;
            if !bx
                .builder
                .has_capability(Capability::IntegerFunctions2INTEL)
            {
                bx.zombie(to_zombie, "capability IntegerFunctions2INTEL is required");
            }
            if !bx
                .builder
                .has_extension(bx.sym.spv_intel_shader_integer_functions2)
            {
                bx.zombie(
                    to_zombie,
                    "extension SPV_INTEL_shader_integer_functions2 is required",
                );
            }
        }
    }
}

impl<'a, 'tcx> Builder<'a, 'tcx> {
    pub fn custom_inst(
        &mut self,
        result_type: Word,
        inst: custom_insts::CustomInst<Operand>,
    ) -> SpirvValue {
        let custom_ext_inst_set = self.ext_inst.borrow_mut().import_custom(self);
        self.emit()
            .ext_inst(
                result_type,
                None,
                custom_ext_inst_set,
                inst.op() as u32,
                inst.into_operands(),
            )
            .unwrap()
            .with_type(result_type)
    }

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
}
