use rspirv::dr::{Instruction, Module, Operand};
use rspirv::spirv::{Decoration, Op, Word};
use std::collections::HashMap;

pub fn export_zombies(module: &mut Module, zombies: &HashMap<Word, &'static str>) {
    for (&id, &reason) in zombies {
        // TODO: Right now we just piggyback off UserTypeGOOGLE since we never use it elsewhere. We should, uh, fix this
        // to use non_semantic or something.
        // https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_non_semantic_info.html
        let inst = Instruction::new(
            Op::DecorateString,
            None,
            None,
            vec![
                Operand::IdRef(id),
                Operand::Decoration(Decoration::UserTypeGOOGLE),
                Operand::LiteralString(reason.to_string()),
            ],
        );
        module.annotations.push(inst);
    }
}
