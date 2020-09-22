use rspirv::dr::{Block, Function, Instruction, Module, Operand};
use rspirv::spirv::{Decoration, Op, Word};
use std::collections::{HashMap, HashSet};
use std::iter::once;
use std::mem::replace;

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

fn label_of(block: &Block) -> Word {
    block.label.as_ref().unwrap().result_id.unwrap()
}

// TODO: Do we move this to the linker?
pub fn block_ordering_pass(func: &mut Function) {
    if func.blocks.len() < 2 {
        return;
    }
    fn visit_postorder(
        func: &Function,
        visited: &mut HashSet<Word>,
        postorder: &mut Vec<Word>,
        current: Word,
    ) {
        if !visited.insert(current) {
            return;
        }
        let current_block = func.blocks.iter().find(|b| label_of(b) == current).unwrap();
        // Reverse the order, so reverse-postorder keeps things tidy
        for &outgoing in outgoing_edges(current_block).iter().rev() {
            visit_postorder(func, visited, postorder, outgoing);
        }
        postorder.push(current);
    }

    let mut visited = HashSet::new();
    let mut postorder = Vec::new();

    let entry_label = label_of(&func.blocks[0]);
    visit_postorder(func, &mut visited, &mut postorder, entry_label);

    let mut old_blocks = replace(&mut func.blocks, Vec::new());
    // Order blocks according to reverse postorder
    for &block in postorder.iter().rev() {
        let index = old_blocks
            .iter()
            .position(|b| label_of(b) == block)
            .unwrap();
        func.blocks.push(old_blocks.remove(index));
    }
    // Note: if old_blocks isn't empty here, that means there were unreachable blocks that were deleted.
    assert_eq!(label_of(&func.blocks[0]), entry_label);
}

fn outgoing_edges(block: &Block) -> Vec<Word> {
    fn unwrap_id_ref(operand: &Operand) -> Word {
        match *operand {
            Operand::IdRef(word) => word,
            _ => panic!("Expected Operand::IdRef: {}", operand),
        }
    }
    let terminator = block.instructions.last().unwrap();
    // https://www.khronos.org/registry/spir-v/specs/unified1/SPIRV.html#Termination
    match terminator.class.opcode {
        Op::Branch => vec![unwrap_id_ref(&terminator.operands[0])],
        Op::BranchConditional => vec![
            unwrap_id_ref(&terminator.operands[1]),
            unwrap_id_ref(&terminator.operands[2]),
        ],
        Op::Switch => once(unwrap_id_ref(&terminator.operands[1]))
            .chain(
                terminator.operands[3..]
                    .iter()
                    .step_by(2)
                    .map(unwrap_id_ref),
            )
            .collect(),
        Op::Return | Op::ReturnValue | Op::Kill | Op::Unreachable => Vec::new(),
        _ => panic!("Invalid block terminator: {:?}", terminator),
    }
}
