use super::{get_name, get_names, Result};
use rspirv::dr::{Block, Function, Module};
use rspirv::spirv::{ExecutionModel, Op, Word};
use rustc_data_structures::fx::{FxHashMap, FxHashSet};
use rustc_session::Session;
use std::iter::once;
use std::mem::take;

pub fn shift_ids(module: &mut Module, add: u32) {
    module.all_inst_iter_mut().for_each(|inst| {
        if let Some(ref mut result_id) = &mut inst.result_id {
            *result_id += add;
        }

        if let Some(ref mut result_type) = &mut inst.result_type {
            *result_type += add;
        }

        inst.operands.iter_mut().for_each(|op| {
            if let Some(w) = op.id_ref_any_mut() {
                *w += add;
            }
        });
    });
}

/// spir-v requires basic blocks to be ordered so that if A dominates B, A appears before B (except
/// in the case of backedges). Reverse post-order is a good ordering that satisfies this condition
/// (with an "already visited set" that blocks going deeper, which solves both the fact that it's a
/// DAG, not a tree, as well as backedges).
pub fn block_ordering_pass(func: &mut Function) {
    if func.blocks.len() < 2 {
        return;
    }
    fn visit_postorder(
        func: &Function,
        visited: &mut FxHashSet<Word>,
        postorder: &mut Vec<Word>,
        current: Word,
    ) {
        if !visited.insert(current) {
            return;
        }
        let current_block = func
            .blocks
            .iter()
            .find(|b| b.label_id().unwrap() == current)
            .unwrap();
        let mut edges = outgoing_edges(current_block).collect::<Vec<_>>();
        // HACK(eddyb) treat `OpSelectionMerge` as an edge, in case it points
        // to an otherwise-unreachable block.
        if let Some(before_last_idx) = current_block.instructions.len().checked_sub(2) {
            if let Some(before_last) = current_block.instructions.get(before_last_idx) {
                if before_last.class.opcode == Op::SelectionMerge {
                    edges.push(before_last.operands[0].unwrap_id_ref());
                }
            }
        }
        // Reverse the order, so reverse-postorder keeps things tidy
        for &outgoing in edges.iter().rev() {
            visit_postorder(func, visited, postorder, outgoing);
        }
        postorder.push(current);
    }

    let mut visited = FxHashSet::default();
    let mut postorder = Vec::new();

    let entry_label = func.blocks[0].label_id().unwrap();
    visit_postorder(func, &mut visited, &mut postorder, entry_label);

    let mut old_blocks = take(&mut func.blocks);
    // Order blocks according to reverse postorder
    for &block in postorder.iter().rev() {
        let index = old_blocks
            .iter()
            .position(|b| b.label_id().unwrap() == block)
            .unwrap();
        func.blocks.push(old_blocks.remove(index));
    }
    // Note: if old_blocks isn't empty here, that means there were unreachable blocks that were deleted.
    assert_eq!(func.blocks[0].label_id().unwrap(), entry_label);
}

pub fn outgoing_edges(block: &Block) -> impl Iterator<Item = Word> + '_ {
    let terminator = block.instructions.last().unwrap();
    // https://www.khronos.org/registry/spir-v/specs/unified1/SPIRV.html#Termination
    let operand_indices = match terminator.class.opcode {
        Op::Branch => (0..1).step_by(1),
        Op::BranchConditional => (1..3).step_by(1),
        Op::Switch => (1..terminator.operands.len()).step_by(2),
        Op::Return
        | Op::ReturnValue
        | Op::Kill
        | Op::Unreachable
        | Op::IgnoreIntersectionKHR
        | Op::TerminateRayKHR => (0..0).step_by(1),
        _ => panic!("Invalid block terminator: {terminator:?}"),
    };
    operand_indices.map(move |i| terminator.operands[i].unwrap_id_ref())
}

pub fn compact_ids(module: &mut Module) -> u32 {
    let mut remap = FxHashMap::default();

    let mut insert = |current_id: u32| -> u32 {
        let len = remap.len();
        *remap.entry(current_id).or_insert_with(|| len as u32 + 1)
    };

    module.all_inst_iter_mut().for_each(|inst| {
        if let Some(ref mut result_id) = &mut inst.result_id {
            *result_id = insert(*result_id);
        }

        if let Some(ref mut result_type) = &mut inst.result_type {
            *result_type = insert(*result_type);
        }

        inst.operands.iter_mut().for_each(|op| {
            if let Some(w) = op.id_ref_any_mut() {
                *w = insert(*w);
            }
        });
    });

    remap.len() as u32 + 1
}

pub fn sort_globals(module: &mut Module) {
    // Function declarations come before definitions. TODO: Figure out if it's even possible to
    // have a function declaration without a body in a fully linked module?
    module.functions.sort_by_key(|f| !f.blocks.is_empty());
}

pub fn name_variables_pass(module: &mut Module) {
    let variables = module
        .types_global_values
        .iter()
        .filter(|inst| inst.class.opcode == Op::Variable)
        .map(|inst| inst.result_id.unwrap())
        .collect::<FxHashSet<Word>>();
    module
        .debug_names
        .retain(|inst| variables.contains(&inst.operands[0].unwrap_id_ref()));
    // FIXME(eddyb) why does this remove `OpLine` instructions?
    module
        .types_global_values
        .retain(|inst| inst.class.opcode != Op::Line);
    for func in &mut module.functions {
        for block in &mut func.blocks {
            block
                .instructions
                .retain(|inst| inst.class.opcode != Op::Line);
        }
    }
}

// Some instructions are only valid in fragment shaders. Check them.
pub fn check_fragment_insts(sess: &Session, module: &Module) -> Result<()> {
    let mut visited = vec![false; module.functions.len()];
    let mut stack = Vec::new();
    let mut names = None;
    let func_id_to_idx: FxHashMap<Word, usize> = module
        .functions
        .iter()
        .enumerate()
        .map(|(index, func)| (func.def_id().unwrap(), index))
        .collect();
    let entries = module
        .entry_points
        .iter()
        .filter(|i| i.operands[0].unwrap_execution_model() != ExecutionModel::Fragment)
        .map(|i| func_id_to_idx[&i.operands[1].unwrap_id_ref()]);
    let mut any_err = None;
    for entry in entries {
        let entry_had_err = visit(
            sess,
            module,
            &mut visited,
            &mut stack,
            &mut names,
            entry,
            &func_id_to_idx,
        )
        .err();
        any_err = any_err.or(entry_had_err);
    }
    return match any_err {
        Some(err) => Err(err),
        None => Ok(()),
    };

    fn visit<'m>(
        sess: &Session,
        module: &'m Module,
        visited: &mut Vec<bool>,
        stack: &mut Vec<Word>,
        names: &mut Option<FxHashMap<Word, &'m str>>,
        index: usize,
        func_id_to_idx: &FxHashMap<Word, usize>,
    ) -> Result<()> {
        if visited[index] {
            return Ok(());
        }
        visited[index] = true;
        stack.push(module.functions[index].def_id().unwrap());
        let mut any_err = None;
        for inst in module.functions[index].all_inst_iter() {
            if inst.class.opcode == Op::FunctionCall {
                let callee = func_id_to_idx[&inst.operands[0].unwrap_id_ref()];
                let callee_had_err =
                    visit(sess, module, visited, stack, names, callee, func_id_to_idx).err();
                any_err = any_err.or(callee_had_err);
            }
            if matches!(
                inst.class.opcode,
                Op::ImageSampleImplicitLod
                    | Op::ImageSampleDrefImplicitLod
                    | Op::ImageSampleProjImplicitLod
                    | Op::ImageSampleProjDrefImplicitLod
                    | Op::ImageQueryLod
                    | Op::ImageSparseSampleImplicitLod
                    | Op::ImageSparseSampleDrefImplicitLod
                    | Op::DPdx
                    | Op::DPdy
                    | Op::Fwidth
                    | Op::DPdxFine
                    | Op::DPdyFine
                    | Op::FwidthFine
                    | Op::DPdxCoarse
                    | Op::DPdyCoarse
                    | Op::FwidthCoarse
                    | Op::Kill
            ) {
                // These instructions are (usually) in system functions - if we get an error, allow
                // the system function to be visited again from elsewhere to emit another error
                // from another callsite.
                visited[index] = false;

                let names = names.get_or_insert_with(|| get_names(module));
                let stack = stack.iter().rev().map(|&s| get_name(names, s).into_owned());
                let note = once("Stack:".to_string())
                    .chain(stack)
                    .collect::<Vec<_>>()
                    .join("\n");
                any_err = Some(
                    sess.struct_err(format!(
                        "{} cannot be used outside a fragment shader",
                        inst.class.opname
                    ))
                    .note(note)
                    .emit(),
                );
            }
        }
        stack.pop();

        match any_err {
            Some(err) => Err(err),
            None => Ok(()),
        }
    }
}
