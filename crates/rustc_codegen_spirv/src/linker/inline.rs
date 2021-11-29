//! This algorithm is not intended to be an optimization, it is rather for legalization.
//! Specifically, spir-v disallows things like a `StorageClass::Function` pointer to a
//! `StorageClass::Input` pointer. Our frontend definitely allows it, though, this is like taking a
//! `&Input<T>` in a function! So, we inline all functions that take these "illegal" pointers, then
//! run mem2reg (see mem2reg.rs) on the result to "unwrap" the Function pointer.

use super::apply_rewrite_rules;
use super::simple_passes::outgoing_edges;
use super::{get_name, get_names};
use rspirv::dr::{Block, Function, Instruction, Module, ModuleHeader, Operand};
use rspirv::spirv::{FunctionControl, Op, StorageClass, Word};
use rustc_data_structures::fx::{FxHashMap, FxHashSet};
use rustc_errors::ErrorGuaranteed;
use rustc_session::Session;
use std::mem::take;

type FunctionMap = FxHashMap<Word, usize>;

pub fn inline(sess: &Session, module: &mut Module) -> super::Result<()> {
    let (disallowed_argument_types, disallowed_return_types) =
        compute_disallowed_argument_and_return_types(module);
    let mut to_delete: Vec<_> = module
        .functions
        .iter()
        .map(|f| should_inline(&disallowed_argument_types, &disallowed_return_types, f))
        .collect();
    // This algorithm gets real sad if there's recursion - but, good news, SPIR-V bans recursion
    let postorder = compute_function_postorder(sess, module, &mut to_delete)?;
    let functions = module
        .functions
        .iter()
        .enumerate()
        .map(|(idx, f)| (f.def_id().unwrap(), idx))
        .collect();
    let void = module
        .types_global_values
        .iter()
        .find(|inst| inst.class.opcode == Op::TypeVoid)
        .map_or(0, |inst| inst.result_id.unwrap());
    let ptr_map: FxHashMap<_, _> = module
        .types_global_values
        .iter()
        .filter_map(|inst| {
            if inst.class.opcode == Op::TypePointer
                && inst.operands[0].unwrap_storage_class() == StorageClass::Function
            {
                Some((inst.operands[1].unwrap_id_ref(), inst.result_id.unwrap()))
            } else {
                None
            }
        })
        .collect();
    // Drop all the functions we'll be inlining. (This also means we won't waste time processing
    // inlines in functions that will get inlined)
    let mut inliner = Inliner {
        header: module.header.as_mut().unwrap(),
        types_global_values: &mut module.types_global_values,
        void,
        ptr_map,
        functions: &functions,
        needs_inline: &to_delete,
    };
    for index in postorder {
        inliner.inline_fn(&mut module.functions, index);
        fuse_trivial_branches(&mut module.functions[index]);
    }
    let mut dropped_ids = FxHashSet::default();
    for i in (0..module.functions.len()).rev() {
        if to_delete[i] {
            dropped_ids.insert(module.functions.remove(i).def_id().unwrap());
        }
    }
    // Drop OpName etc. for inlined functions
    module.debug_names.retain(|inst| {
        !inst.operands.iter().any(|op| {
            op.id_ref_any()
                .map_or(false, |id| dropped_ids.contains(&id))
        })
    });
    Ok(())
}

/// Topological sorting algorithm due to T. Cormen
/// Starts from module's entry points, so only reachable functions will be returned
/// in post-traversal order of DFS. For all unvisited functions `module.functions[i]`,
/// `to_delete[i]` is set to true.
fn compute_function_postorder(
    sess: &Session,
    module: &Module,
    to_delete: &mut [bool],
) -> super::Result<Vec<usize>> {
    let func_to_index: FxHashMap<Word, usize> = module
        .functions
        .iter()
        .enumerate()
        .map(|(index, func)| (func.def_id().unwrap(), index))
        .collect();
    /// Possible node states for cycle-discovering DFS.
    #[derive(Clone, PartialEq)]
    enum NodeState {
        /// Normal, not visited.
        NotVisited,
        /// Currently being visited.
        Discovered,
        /// DFS returned.
        Finished,
        /// Not visited, entry point.
        Entry,
    }
    let mut states = vec![NodeState::NotVisited; module.functions.len()];
    for opep in module.entry_points.iter() {
        let func_id = opep.operands[1].unwrap_id_ref();
        states[func_to_index[&func_id]] = NodeState::Entry;
    }
    let mut has_recursion = None;
    let mut postorder = vec![];
    for index in 0..module.functions.len() {
        if NodeState::Entry == states[index] {
            visit(
                sess,
                module,
                index,
                &mut states[..],
                &mut has_recursion,
                &mut postorder,
                &func_to_index,
            );
        }
    }

    for index in 0..module.functions.len() {
        if NodeState::NotVisited == states[index] {
            to_delete[index] = true;
        }
    }

    fn visit(
        sess: &Session,
        module: &Module,
        current: usize,
        states: &mut [NodeState],
        has_recursion: &mut Option<ErrorGuaranteed>,
        postorder: &mut Vec<usize>,
        func_to_index: &FxHashMap<Word, usize>,
    ) {
        states[current] = NodeState::Discovered;

        for next in calls(&module.functions[current], func_to_index) {
            match states[next] {
                NodeState::Discovered => {
                    let names = get_names(module);
                    let current_name =
                        get_name(&names, module.functions[current].def_id().unwrap());
                    let next_name = get_name(&names, module.functions[next].def_id().unwrap());
                    *has_recursion = Some(sess.err(&format!(
                        "module has recursion, which is not allowed: `{}` calls `{}`",
                        current_name, next_name
                    )));
                    break;
                }
                NodeState::NotVisited | NodeState::Entry => {
                    visit(
                        sess,
                        module,
                        next,
                        states,
                        has_recursion,
                        postorder,
                        func_to_index,
                    );
                }
                NodeState::Finished => {}
            }
        }

        states[current] = NodeState::Finished;
        postorder.push(current)
    }

    fn calls<'a>(
        func: &'a Function,
        func_to_index: &'a FxHashMap<Word, usize>,
    ) -> impl Iterator<Item = usize> + 'a {
        func.all_inst_iter()
            .filter(|inst| inst.class.opcode == Op::FunctionCall)
            .map(move |inst| {
                *func_to_index
                    .get(&inst.operands[0].id_ref_any().unwrap())
                    .unwrap()
            })
    }

    match has_recursion {
        Some(err) => Err(err),
        None => Ok(postorder),
    }
}

fn compute_disallowed_argument_and_return_types(
    module: &Module,
) -> (FxHashSet<Word>, FxHashSet<Word>) {
    let allowed_argument_storage_classes = &[
        StorageClass::UniformConstant,
        StorageClass::Function,
        StorageClass::Private,
        StorageClass::Workgroup,
        StorageClass::AtomicCounter,
    ];
    let mut disallowed_argument_types = FxHashSet::default();
    let mut disallowed_pointees = FxHashSet::default();
    let mut disallowed_return_types = FxHashSet::default();
    for inst in &module.types_global_values {
        match inst.class.opcode {
            Op::TypePointer => {
                let storage_class = inst.operands[0].unwrap_storage_class();
                let pointee = inst.operands[1].unwrap_id_ref();
                if !allowed_argument_storage_classes.contains(&storage_class)
                    || disallowed_pointees.contains(&pointee)
                    || disallowed_argument_types.contains(&pointee)
                {
                    disallowed_argument_types.insert(inst.result_id.unwrap());
                }
                disallowed_pointees.insert(inst.result_id.unwrap());
                disallowed_return_types.insert(inst.result_id.unwrap());
            }
            Op::TypeStruct => {
                let fields = || inst.operands.iter().map(|op| op.id_ref_any().unwrap());
                if fields().any(|id| disallowed_argument_types.contains(&id)) {
                    disallowed_argument_types.insert(inst.result_id.unwrap());
                }
                if fields().any(|id| disallowed_pointees.contains(&id)) {
                    disallowed_pointees.insert(inst.result_id.unwrap());
                }
                if fields().any(|id| disallowed_return_types.contains(&id)) {
                    disallowed_return_types.insert(inst.result_id.unwrap());
                }
            }
            Op::TypeArray | Op::TypeRuntimeArray | Op::TypeVector => {
                let id = inst.operands[0].id_ref_any().unwrap();
                if disallowed_argument_types.contains(&id) {
                    disallowed_argument_types.insert(inst.result_id.unwrap());
                }
                if disallowed_pointees.contains(&id) {
                    disallowed_pointees.insert(inst.result_id.unwrap());
                }
            }
            _ => {}
        }
    }
    (disallowed_argument_types, disallowed_return_types)
}

fn should_inline(
    disallowed_argument_types: &FxHashSet<Word>,
    disallowed_return_types: &FxHashSet<Word>,
    function: &Function,
) -> bool {
    let def = function.def.as_ref().unwrap();
    let control = def.operands[0].unwrap_function_control();
    control.contains(FunctionControl::INLINE)
        || function
            .parameters
            .iter()
            .any(|inst| disallowed_argument_types.contains(inst.result_type.as_ref().unwrap()))
        || disallowed_return_types.contains(&function.def.as_ref().unwrap().result_type.unwrap())
}

// This should be more general, but a very common problem is passing an OpAccessChain to an
// OpFunctionCall (i.e. `f(&s.x)`, or more commonly, `s.x.f()` where `f` takes `&self`), so detect
// that case and inline the call.
fn args_invalid(function: &Function, call: &Instruction) -> bool {
    for inst in function.all_inst_iter() {
        if inst.class.opcode == Op::AccessChain {
            let inst_result = inst.result_id.unwrap();
            if call
                .operands
                .iter()
                .any(|op| *op == Operand::IdRef(inst_result))
            {
                return true;
            }
        }
    }
    false
}

// Steps:
// Move OpVariable decls
// Rewrite return
// Renumber IDs
// Insert blocks

struct Inliner<'m, 'map> {
    header: &'m mut ModuleHeader,
    types_global_values: &'m mut Vec<Instruction>,
    void: Word,
    ptr_map: FxHashMap<Word, Word>,
    functions: &'map FunctionMap,
    needs_inline: &'map [bool],
}

impl Inliner<'_, '_> {
    fn id(&mut self) -> Word {
        let result = self.header.bound;
        self.header.bound += 1;
        result
    }

    fn ptr_ty(&mut self, pointee: Word) -> Word {
        let existing = self.ptr_map.get(&pointee);
        if let Some(existing) = existing {
            return *existing;
        }
        let inst_id = self.id();
        self.types_global_values.push(Instruction::new(
            Op::TypePointer,
            None,
            Some(inst_id),
            vec![
                Operand::StorageClass(StorageClass::Function),
                Operand::IdRef(pointee),
            ],
        ));
        self.ptr_map.insert(pointee, inst_id);
        inst_id
    }

    fn inline_fn(&mut self, functions: &mut [Function], index: usize) {
        let mut function = take(&mut functions[index]);
        let mut block_idx = 0;
        while block_idx < function.blocks.len() {
            // If we successfully inlined a block, then continue processing on the next block or its tail.
            // TODO: this is quadratic in cases where [`Op::AccessChain`]s cascade into inner arguments.
            // For the common case of "we knew which functions to inline", it is linear.
            self.inline_block(&mut function, &functions, block_idx);
            block_idx += 1;
        }
        functions[index] = function;
    }

    /// Inlines one block and returns whether inlining actually occurred.
    /// After calling this, blocks[block_idx] is finished processing.
    fn inline_block(
        &mut self,
        caller: &mut Function,
        functions: &[Function],
        block_idx: usize,
    ) -> bool {
        // Find the first inlined OpFunctionCall
        let call = caller.blocks[block_idx]
            .instructions
            .iter()
            .enumerate()
            .filter(|(_, inst)| inst.class.opcode == Op::FunctionCall)
            .map(|(index, inst)| {
                (
                    index,
                    inst,
                    self.functions[&inst.operands[0].id_ref_any().unwrap()],
                )
            })
            .find(|(index, inst, func_idx)| {
                self.needs_inline[*func_idx] || args_invalid(caller, inst)
            });
        let (call_index, call_inst, callee_idx) = match call {
            None => return false,
            Some(call) => call,
        };
        let callee = &functions[callee_idx];
        let call_result_type = {
            let ty = call_inst.result_type.unwrap();
            if ty == self.void {
                None
            } else {
                Some(ty)
            }
        };
        let call_result_id = call_inst.result_id.unwrap();
        // Rewrite parameters to arguments
        let call_arguments = call_inst
            .operands
            .iter()
            .skip(1)
            .map(|op| op.id_ref_any().unwrap());
        let callee_parameters = callee.parameters.iter().map(|inst| {
            assert!(inst.class.opcode == Op::FunctionParameter);
            inst.result_id.unwrap()
        });
        let mut rewrite_rules = callee_parameters.zip(call_arguments).collect();

        let return_variable = if call_result_type.is_some() {
            Some(self.id())
        } else {
            None
        };
        let return_jump = self.id();
        // Rewrite OpReturns of the callee.
        let mut inlined_blocks = get_inlined_blocks(callee, return_variable, return_jump);
        // Clone the IDs of the callee, because otherwise they'd be defined multiple times if the
        // fn is inlined multiple times.
        self.add_clone_id_rules(&mut rewrite_rules, &inlined_blocks);
        apply_rewrite_rules(&rewrite_rules, &mut inlined_blocks);

        // Split the block containing the OpFunctionCall into two, around the call.
        let mut post_call_block_insts = caller.blocks[block_idx]
            .instructions
            .split_off(call_index + 1);
        // pop off OpFunctionCall
        let call = caller.blocks[block_idx].instructions.pop().unwrap();
        assert!(call.class.opcode == Op::FunctionCall);

        if let Some(call_result_type) = call_result_type {
            // Generate the storage space for the return value: Do this *after* the split above,
            // because if block_idx=0, inserting a variable here shifts call_index.
            insert_opvariable(
                &mut caller.blocks[0],
                self.ptr_ty(call_result_type),
                return_variable.unwrap(),
            );
        }

        // Move the variables over from the inlined function to here.
        let mut callee_header = take(&mut inlined_blocks[0]).instructions;
        // TODO: OpLine handling
        let num_variables = callee_header.partition_point(|inst| inst.class.opcode == Op::Variable);
        // Rather than fuse blocks, generate a new jump here. Branch fusing will take care of
        // it, and we maintain the invariant that current block has finished processing.
        let jump_to = self.id();
        inlined_blocks[0] = Block {
            label: Some(Instruction::new(Op::Label, None, Some(jump_to), vec![])),
            instructions: callee_header.split_off(num_variables),
        };
        caller.blocks[block_idx].instructions.push(Instruction::new(
            Op::Branch,
            None,
            None,
            vec![Operand::IdRef(jump_to)],
        ));
        // Move the OpVariables of the callee to the caller.
        insert_opvariables(&mut caller.blocks[0], callee_header);

        if let Some(call_result_type) = call_result_type {
            // Add the load of the result value after the inlined function. Note there's guaranteed no
            // OpPhi instructions since we just split this block.
            post_call_block_insts.insert(
                0,
                Instruction::new(
                    Op::Load,
                    Some(call_result_type),
                    Some(call_result_id),
                    vec![Operand::IdRef(return_variable.unwrap())],
                ),
            );
        }
        // Insert the second half of the split block.
        let continue_block = Block {
            label: Some(Instruction::new(Op::Label, None, Some(return_jump), vec![])),
            instructions: post_call_block_insts,
        };
        caller.blocks.insert(block_idx + 1, continue_block);

        // Insert the rest of the blocks (i.e. not the first) between the original block that was
        // split.
        caller
            .blocks
            .splice((block_idx + 1)..(block_idx + 1), inlined_blocks);

        true
    }

    fn add_clone_id_rules(&mut self, rewrite_rules: &mut FxHashMap<Word, Word>, blocks: &[Block]) {
        for block in blocks {
            for inst in block.label.iter().chain(&block.instructions) {
                if let Some(result_id) = inst.result_id {
                    let new_id = self.id();
                    let old = rewrite_rules.insert(result_id, new_id);
                    assert!(old.is_none());
                }
            }
        }
    }
}

fn get_inlined_blocks(
    function: &Function,
    return_variable: Option<Word>,
    return_jump: Word,
) -> Vec<Block> {
    let mut blocks = function.blocks.clone();
    for block in &mut blocks {
        let last = block.instructions.last().unwrap();
        if let Op::Return | Op::ReturnValue = last.class.opcode {
            if Op::ReturnValue == last.class.opcode {
                let return_value = last.operands[0].id_ref_any().unwrap();
                block.instructions.insert(
                    block.instructions.len() - 1,
                    Instruction::new(
                        Op::Store,
                        None,
                        None,
                        vec![
                            Operand::IdRef(return_variable.unwrap()),
                            Operand::IdRef(return_value),
                        ],
                    ),
                );
            } else {
                assert!(return_variable.is_none());
            }
            *block.instructions.last_mut().unwrap() =
                Instruction::new(Op::Branch, None, None, vec![Operand::IdRef(return_jump)]);
        }
    }
    blocks
}

fn insert_opvariable(block: &mut Block, ptr_ty: Word, result_id: Word) {
    let index = block
        .instructions
        .partition_point(|inst| inst.class.opcode == Op::Variable);

    let inst = Instruction::new(
        Op::Variable,
        Some(ptr_ty),
        Some(result_id),
        vec![Operand::StorageClass(StorageClass::Function)],
    );
    block.instructions.insert(index, inst)
}

fn insert_opvariables(block: &mut Block, insts: Vec<Instruction>) {
    let index = block
        .instructions
        .partition_point(|inst| inst.class.opcode == Op::Variable);
    block.instructions.splice(index..index, insts);
}

fn fuse_trivial_branches(function: &mut Function) {
    let all_preds = compute_preds(&function.blocks);
    'outer: for (dest_block, mut preds) in all_preds.iter().enumerate() {
        // if there's two trivial branches in a row, the middle one might get inlined before the
        // last one, so when processing the last one, skip through to the first one.
        let pred = loop {
            if preds.len() != 1 || preds[0] == dest_block {
                continue 'outer;
            }
            let pred = preds[0];
            if !function.blocks[pred].instructions.is_empty() {
                break pred;
            }
            preds = &all_preds[pred];
        };
        let pred_insts = &function.blocks[pred].instructions;
        if pred_insts.last().unwrap().class.opcode == Op::Branch {
            let mut dest_insts = take(&mut function.blocks[dest_block].instructions);
            let pred_insts = &mut function.blocks[pred].instructions;
            pred_insts.pop(); // pop the branch
            pred_insts.append(&mut dest_insts);
        }
    }
    function.blocks.retain(|b| !b.instructions.is_empty());
}

fn compute_preds(blocks: &[Block]) -> Vec<Vec<usize>> {
    let mut result = vec![vec![]; blocks.len()];
    for (source_idx, source) in blocks.iter().enumerate() {
        for dest_id in outgoing_edges(source) {
            let dest_idx = blocks
                .iter()
                .position(|b| b.label_id().unwrap() == dest_id)
                .unwrap();
            result[dest_idx].push(source_idx);
        }
    }
    result
}
