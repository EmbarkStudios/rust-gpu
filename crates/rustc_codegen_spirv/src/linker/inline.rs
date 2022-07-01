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
use smallvec::SmallVec;
use std::convert::TryInto;
use std::mem::take;

type FunctionMap = FxHashMap<Word, Function>;

pub fn inline(sess: &Session, module: &mut Module) -> super::Result<()> {
    // This algorithm gets real sad if there's recursion - but, good news, SPIR-V bans recursion
    deny_recursion_in_module(sess, module)?;

    let functions = module
        .functions
        .iter()
        .map(|f| (f.def_id().unwrap(), f.clone()))
        .collect();
    let (disallowed_argument_types, disallowed_return_types) =
        compute_disallowed_argument_and_return_types(module);
    let void = module
        .types_global_values
        .iter()
        .find(|inst| inst.class.opcode == Op::TypeVoid)
        .map_or(0, |inst| inst.result_id.unwrap());
    // Drop all the functions we'll be inlining. (This also means we won't waste time processing
    // inlines in functions that will get inlined)
    let mut dropped_ids = FxHashSet::default();
    let mut inlined_dont_inlines = Vec::new();
    module.functions.retain(|f| {
        if should_inline(&disallowed_argument_types, &disallowed_return_types, f) {
            if has_dont_inline(f) {
                inlined_dont_inlines.push(f.def_id().unwrap());
            }
            // TODO: We should insert all defined IDs in this function.
            dropped_ids.insert(f.def_id().unwrap());
            false
        } else {
            true
        }
    });
    if !inlined_dont_inlines.is_empty() {
        let names = get_names(module);
        for f in inlined_dont_inlines {
            sess.warn(format!(
                "`#[inline(never)]` function `{}` needs to be inlined \
                 because it has illegal argument or return types",
                get_name(&names, f)
            ));
        }
    }

    // Drop OpName etc. for inlined functions
    module.debug_names.retain(|inst| {
        !inst.operands.iter().any(|op| {
            op.id_ref_any()
                .map_or(false, |id| dropped_ids.contains(&id))
        })
    });
    let mut inliner = Inliner {
        header: module.header.as_mut().unwrap(),
        types_global_values: &mut module.types_global_values,
        void,
        functions: &functions,
        disallowed_argument_types: &disallowed_argument_types,
        disallowed_return_types: &disallowed_return_types,
    };
    for function in &mut module.functions {
        inliner.inline_fn(function);
        fuse_trivial_branches(function);
    }
    Ok(())
}

// https://stackoverflow.com/a/53995651
fn deny_recursion_in_module(sess: &Session, module: &Module) -> super::Result<()> {
    let func_to_index: FxHashMap<Word, usize> = module
        .functions
        .iter()
        .enumerate()
        .map(|(index, func)| (func.def_id().unwrap(), index))
        .collect();
    let mut discovered = vec![false; module.functions.len()];
    let mut finished = vec![false; module.functions.len()];
    let mut has_recursion = None;
    for index in 0..module.functions.len() {
        if !discovered[index] && !finished[index] {
            visit(
                sess,
                module,
                index,
                &mut discovered,
                &mut finished,
                &mut has_recursion,
                &func_to_index,
            );
        }
    }

    fn visit(
        sess: &Session,
        module: &Module,
        current: usize,
        discovered: &mut Vec<bool>,
        finished: &mut Vec<bool>,
        has_recursion: &mut Option<ErrorGuaranteed>,
        func_to_index: &FxHashMap<Word, usize>,
    ) {
        discovered[current] = true;

        for next in calls(&module.functions[current], func_to_index) {
            if discovered[next] {
                let names = get_names(module);
                let current_name = get_name(&names, module.functions[current].def_id().unwrap());
                let next_name = get_name(&names, module.functions[next].def_id().unwrap());
                *has_recursion = Some(sess.err(format!(
                    "module has recursion, which is not allowed: `{}` calls `{}`",
                    current_name, next_name
                )));
                break;
            }

            if !finished[next] {
                visit(
                    sess,
                    module,
                    next,
                    discovered,
                    finished,
                    has_recursion,
                    func_to_index,
                );
            }
        }

        discovered[current] = false;
        finished[current] = true;
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
        None => Ok(()),
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

fn has_dont_inline(function: &Function) -> bool {
    let def = function.def.as_ref().unwrap();
    let control = def.operands[0].unwrap_function_control();
    control.contains(FunctionControl::DONT_INLINE)
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
    functions: &'map FunctionMap,
    disallowed_argument_types: &'map FxHashSet<Word>,
    disallowed_return_types: &'map FxHashSet<Word>,
    // rewrite_rules: FxHashMap<Word, Word>,
}

impl Inliner<'_, '_> {
    fn id(&mut self) -> Word {
        let result = self.header.bound;
        self.header.bound += 1;
        result
    }

    fn ptr_ty(&mut self, pointee: Word) -> Word {
        // TODO: This is horribly slow, fix this
        let existing = self.types_global_values.iter().find(|inst| {
            inst.class.opcode == Op::TypePointer
                && inst.operands[0].unwrap_storage_class() == StorageClass::Function
                && inst.operands[1].unwrap_id_ref() == pointee
        });
        if let Some(existing) = existing {
            return existing.result_id.unwrap();
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
        inst_id
    }

    fn inline_fn(&mut self, function: &mut Function) {
        let mut block_idx = 0;
        while block_idx < function.blocks.len() {
            // If we successfully inlined a block, then repeat processing on the same block, in
            // case the newly inlined block has more inlined calls.
            // TODO: This is quadratic
            if !self.inline_block(function, block_idx) {
                block_idx += 1;
            }
        }
    }

    fn inline_block(&mut self, caller: &mut Function, block_idx: usize) -> bool {
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
                    self.functions
                        .get(&inst.operands[0].id_ref_any().unwrap())
                        .unwrap(),
                )
            })
            .find(|(_, inst, f)| {
                should_inline(
                    self.disallowed_argument_types,
                    self.disallowed_return_types,
                    f,
                ) || args_invalid(caller, inst)
            });
        let (call_index, call_inst, callee) = match call {
            None => return false,
            Some(call) => call,
        };
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
        let mut inlined_callee_blocks = get_inlined_blocks(callee, return_variable, return_jump);
        // Clone the IDs of the callee, because otherwise they'd be defined multiple times if the
        // fn is inlined multiple times.
        self.add_clone_id_rules(&mut rewrite_rules, &inlined_callee_blocks);
        apply_rewrite_rules(&rewrite_rules, &mut inlined_callee_blocks);

        // Split the block containing the `OpFunctionCall` into pre-call vs post-call.
        let pre_call_block_idx = block_idx;
        #[expect(unused)]
        let block_idx: usize; // HACK(eddyb) disallowing using the unrenamed variable.
        let mut post_call_block_insts = caller.blocks[pre_call_block_idx]
            .instructions
            .split_off(call_index + 1);
        // pop off OpFunctionCall
        let call = caller.blocks[pre_call_block_idx]
            .instructions
            .pop()
            .unwrap();
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

        // Insert non-entry inlined callee blocks just after the pre-call block.
        let non_entry_inlined_callee_blocks = inlined_callee_blocks.drain(1..);
        let num_non_entry_inlined_callee_blocks = non_entry_inlined_callee_blocks.len();
        caller.blocks.splice(
            (pre_call_block_idx + 1)..(pre_call_block_idx + 1),
            non_entry_inlined_callee_blocks,
        );

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

        // Insert the post-call block, after all the inlined callee blocks.
        {
            let post_call_block_idx = pre_call_block_idx + num_non_entry_inlined_callee_blocks + 1;
            let post_call_block = Block {
                label: Some(Instruction::new(Op::Label, None, Some(return_jump), vec![])),
                instructions: post_call_block_insts,
            };
            caller.blocks.insert(post_call_block_idx, post_call_block);

            // Adjust any `OpPhi`s in the (caller) targets of the original call block,
            // to refer to post-call block (the new source of those CFG edges).
            rewrite_phi_sources(
                caller.blocks[pre_call_block_idx].label_id().unwrap(),
                &mut caller.blocks,
                post_call_block_idx,
            );
        }

        // Fuse the inlined callee entry block into the pre-call block.
        // This is okay because it's illegal to branch to the first BB in a function.
        {
            // Take a prefix sequence of `OpVariable`s from the start of `insts`,
            // and return it as a new `Vec` (while keeping the rest in `insts`).
            let steal_vars = |insts: &mut Vec<Instruction>| {
                let mut vars_and_non_vars = take(insts);

                // TODO: OpLine handling
                let num_vars = vars_and_non_vars
                    .iter()
                    .position(|inst| inst.class.opcode != Op::Variable)
                    .unwrap_or(vars_and_non_vars.len());
                let (non_vars, vars) = (vars_and_non_vars.split_off(num_vars), vars_and_non_vars);

                // Keep non-`OpVariable`s in `insts` while returning `OpVariable`s.
                *insts = non_vars;
                vars
            };

            let [mut inlined_callee_entry_block]: [_; 1] =
                inlined_callee_blocks.try_into().unwrap();

            // Move the `OpVariable`s of the callee to the caller.
            insert_opvariables(
                &mut caller.blocks[0],
                steal_vars(&mut inlined_callee_entry_block.instructions),
            );

            caller.blocks[pre_call_block_idx]
                .instructions
                .append(&mut inlined_callee_entry_block.instructions);

            // Adjust any `OpPhi`s in the (inlined callee) targets of the
            // inlined callee entry block, to refer to the pre-call block
            // (the new source of those CFG edges).
            rewrite_phi_sources(
                inlined_callee_entry_block.label_id().unwrap(),
                &mut caller.blocks,
                pre_call_block_idx,
            );
        }

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
        .iter()
        .enumerate()
        .find_map(|(index, inst)| {
            if inst.class.opcode != Op::Variable {
                Some(index)
            } else {
                None
            }
        });
    let inst = Instruction::new(
        Op::Variable,
        Some(ptr_ty),
        Some(result_id),
        vec![Operand::StorageClass(StorageClass::Function)],
    );
    match index {
        Some(index) => block.instructions.insert(index, inst),
        None => block.instructions.push(inst),
    }
}

fn insert_opvariables(block: &mut Block, mut insts: Vec<Instruction>) {
    let index = block
        .instructions
        .iter()
        .enumerate()
        .find_map(|(index, inst)| {
            if inst.class.opcode != Op::Variable {
                Some(index)
            } else {
                None
            }
        });
    match index {
        Some(index) => {
            block.instructions.splice(index..index, insts);
        }
        None => block.instructions.append(&mut insts),
    }
}

fn fuse_trivial_branches(function: &mut Function) {
    let all_preds = compute_preds(&function.blocks);
    'outer: for (dest_block, mut preds) in all_preds.iter().enumerate() {
        // Don't fuse branches into blocks with `OpPhi`s.
        let any_phis = function.blocks[dest_block]
            .instructions
            .iter()
            .filter(|inst| {
                // These are the only instructions that are allowed before `OpPhi`.
                !matches!(inst.class.opcode, Op::Line | Op::NoLine)
            })
            .take_while(|inst| inst.class.opcode == Op::Phi)
            .next()
            .is_some();
        if any_phis {
            continue;
        }

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

            // Adjust any `OpPhi`s in the targets of the original block, to refer
            // to the sole predecessor (the new source of those CFG edges).
            rewrite_phi_sources(
                function.blocks[dest_block].label_id().unwrap(),
                &mut function.blocks,
                pred,
            );
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

/// Helper for adjusting `OpPhi` source label IDs, when the terminator of the
/// `original_label_id`-labeled block got moved to `blocks[original_block_idx]`.
fn rewrite_phi_sources(original_label_id: Word, blocks: &mut [Block], new_block_idx: usize) {
    let new_label_id = blocks[new_block_idx].label_id().unwrap();

    // HACK(eddyb) can't keep `blocks` borrowed, the loop needs mutable access.
    let target_ids: SmallVec<[_; 4]> = outgoing_edges(&blocks[new_block_idx]).collect();

    for target_id in target_ids {
        let target_block = blocks
            .iter_mut()
            .find(|b| b.label_id().unwrap() == target_id)
            .unwrap();
        let phis = target_block
            .instructions
            .iter_mut()
            .filter(|inst| {
                // These are the only instructions that are allowed before `OpPhi`.
                !matches!(inst.class.opcode, Op::Line | Op::NoLine)
            })
            .take_while(|inst| inst.class.opcode == Op::Phi);
        for phi in phis {
            for value_and_source_id in phi.operands.chunks_mut(2) {
                let source_id = value_and_source_id[1].id_ref_any_mut().unwrap();
                if *source_id == original_label_id {
                    *source_id = new_label_id;
                    break;
                }
            }
        }
    }
}
