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
    let to_delete: Vec<_> = module
        .functions
        .iter()
        .map(|f| should_inline(&disallowed_argument_types, &disallowed_return_types, f))
        .collect();
    // This algorithm gets real sad if there's recursion - but, good news, SPIR-V bans recursion
    let postorder = compute_function_postorder(sess, module, &to_delete)?;
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

    let invalid_args = module.functions.iter().flat_map(get_invalid_args).collect();

    // Drop all the functions we'll be inlining. (This also means we won't waste time processing
    // inlines in functions that will get inlined)
    let mut inliner = Inliner {
        header: module.header.as_mut().unwrap(),
        void,
        functions: &functions,
        needs_inline: &to_delete,
        invalid_args,
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
    to_delete: &[bool],
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
    }
    let mut states = vec![NodeState::NotVisited; module.functions.len()];
    let mut has_recursion = None;
    let mut postorder = vec![];
    for index in 0..module.functions.len() {
        if NodeState::NotVisited == states[index] && !to_delete[index] {
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
                NodeState::NotVisited => {
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
        postorder.push(current);
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
fn get_invalid_args(function: &Function) -> impl Iterator<Item = Word> + '_ {
    function.all_inst_iter().filter_map(|inst| {
        if inst.class.opcode == Op::AccessChain {
            inst.result_id
        } else {
            None
        }
    })
}

fn args_invalid(invalid_args: &FxHashSet<Word>, call: &Instruction) -> bool {
    call.operands.iter().skip(1).any(|op| {
        op.id_ref_any()
            .map_or(false, |arg| invalid_args.contains(&arg))
    })
}

// Steps:
// Move OpVariable decls
// Rewrite return
// Renumber IDs
// Insert blocks

struct Inliner<'m, 'map> {
    header: &'m mut ModuleHeader,
    void: Word,
    functions: &'map FunctionMap,
    needs_inline: &'map [bool],
    invalid_args: FxHashSet<Word>,
}

impl Inliner<'_, '_> {
    fn id(&mut self) -> Word {
        let result = self.header.bound;
        self.header.bound += 1;
        result
    }

    fn inline_fn(&mut self, functions: &mut [Function], index: usize) {
        let mut function = take(&mut functions[index]);
        let mut block_idx = 0;
        while block_idx < function.blocks.len() {
            // If we successfully inlined a block, then continue processing on the next block or its tail.
            // TODO: this is quadratic in cases where [`Op::AccessChain`]s cascade into inner arguments.
            // For the common case of "we knew which functions to inline", it is linear.
            self.inline_block(&mut function, functions, block_idx);
            block_idx += 1;
        }
        functions[index] = function;
    }

    /// Inlines one block and returns whether inlining actually occurred.
    /// After calling this, `blocks[block_idx]` is finished processing.
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
            .find(|(_, inst, func_idx)| {
                self.needs_inline[*func_idx] || args_invalid(&self.invalid_args, inst)
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

        let return_jump = self.id();
        // Rewrite OpReturns of the callee.
        let (mut inlined_blocks, phi_pairs) = get_inlined_blocks(callee, return_jump);
        // Clone the IDs of the callee, because otherwise they'd be defined multiple times if the
        // fn is inlined multiple times.
        self.add_clone_id_rules(&mut rewrite_rules, &inlined_blocks);
        // If any of the OpReturns were invalid, return will also be invalid.
        for (value, _) in &phi_pairs {
            if self.invalid_args.contains(value) {
                self.invalid_args.insert(call_result_id);
                self.invalid_args
                    .insert(*rewrite_rules.get(value).unwrap_or(value));
            }
        }
        apply_rewrite_rules(&rewrite_rules, &mut inlined_blocks);
        // unnecessary: invalidate_more_args(&rewrite_rules, &mut self.invalid_args);
        // as no values from inside the inlined function ever make it directly out.

        // Split the block containing the OpFunctionCall into two, around the call.
        let mut post_call_block_insts = caller.blocks[block_idx]
            .instructions
            .split_off(call_index + 1);
        // pop off OpFunctionCall
        let call = caller.blocks[block_idx].instructions.pop().unwrap();
        assert!(call.class.opcode == Op::FunctionCall);

        // Move the variables over from the inlined function to here.
        let mut callee_header = take(&mut inlined_blocks[0]).instructions;
        // TODO: OpLine handling
        let num_variables = callee_header.partition_point(|inst| inst.class.opcode == Op::Variable);
        // Rather than fuse blocks, generate a new jump here. Branch fusing will take care of
        // it, and we maintain the invariant that current block has finished processing.
        let first_block_id = self.id();
        inlined_blocks[0] = Block {
            label: Some(Instruction::new(
                Op::Label,
                None,
                Some(first_block_id),
                vec![],
            )),
            instructions: callee_header.split_off(num_variables),
        };
        caller.blocks[block_idx].instructions.push(Instruction::new(
            Op::Branch,
            None,
            None,
            vec![Operand::IdRef(first_block_id)],
        ));
        // Move the OpVariables of the callee to the caller.
        insert_opvariables(&mut caller.blocks[0], callee_header);

        if let Some(call_result_type) = call_result_type {
            // Add the load of the result value after the inlined function. Note there's guaranteed no
            // OpPhi instructions since we just split this block.
            post_call_block_insts.insert(
                0,
                Instruction::new(
                    Op::Phi,
                    Some(call_result_type),
                    Some(call_result_id),
                    phi_pairs
                        .into_iter()
                        .flat_map(|(value, parent)| {
                            use std::iter;
                            iter::once(Operand::IdRef(*rewrite_rules.get(&value).unwrap_or(&value)))
                                .chain(iter::once(Operand::IdRef(rewrite_rules[&parent])))
                        })
                        .collect(),
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

fn get_inlined_blocks(function: &Function, return_jump: Word) -> (Vec<Block>, Vec<(Word, Word)>) {
    let mut blocks = function.blocks.clone();
    let mut phipairs = Vec::new();
    for block in &mut blocks {
        let last = block.instructions.last().unwrap();
        if let Op::Return | Op::ReturnValue = last.class.opcode {
            if Op::ReturnValue == last.class.opcode {
                let return_value = last.operands[0].id_ref_any().unwrap();
                phipairs.push((return_value, block.label_id().unwrap()));
            }
            *block.instructions.last_mut().unwrap() =
                Instruction::new(Op::Branch, None, None, vec![Operand::IdRef(return_jump)]);
        }
    }
    (blocks, phipairs)
}

fn insert_opvariables(block: &mut Block, insts: Vec<Instruction>) {
    let index = block
        .instructions
        .partition_point(|inst| inst.class.opcode == Op::Variable);
    block.instructions.splice(index..index, insts);
}

fn fuse_trivial_branches(function: &mut Function) {
    let mut chain_list = compute_outgoing_1to1_branches(&function.blocks);
    let mut rewrite_rules = FxHashMap::default();

    for block_idx in 0..chain_list.len() {
        let mut next = chain_list[block_idx].take();
        loop {
            match next {
                None => {
                    // end of the chain list
                    break;
                }
                Some(x) if x == block_idx => {
                    // loop detected
                    break;
                }
                Some(next_idx) => {
                    let mut dest_insts = take(&mut function.blocks[next_idx].instructions);
                    dest_insts.retain(|inst| {
                        if inst.class.opcode == Op::Phi {
                            assert_eq!(inst.operands.len(), 2);
                            rewrite_rules
                                .insert(inst.result_id.unwrap(), inst.operands[0].unwrap_id_ref());
                            false
                        } else {
                            true
                        }
                    });
                    let self_insts = &mut function.blocks[block_idx].instructions;
                    self_insts.pop(); // pop the branch
                    self_insts.append(&mut dest_insts);
                    next = chain_list[next_idx].take();
                }
            }
        }
    }
    function.blocks.retain(|b| !b.instructions.is_empty());
    // Calculate a closure, as these rules can be transitive
    let mut rewrite_rules_new = rewrite_rules.clone();
    for value in rewrite_rules_new.values_mut() {
        while let Some(next) = rewrite_rules.get(value) {
            *value = *next;
        }
    }
    apply_rewrite_rules(&rewrite_rules_new, &mut function.blocks);
}

fn compute_outgoing_1to1_branches(blocks: &[Block]) -> Vec<Option<usize>> {
    let block_id_to_idx: FxHashMap<_, _> = blocks
        .iter()
        .enumerate()
        .map(|(idx, block)| (block.label_id().unwrap(), idx))
        .collect();
    #[derive(Clone)]
    enum NumIncoming {
        Zero,
        One(usize),
        TooMany,
    }
    let mut incoming = vec![NumIncoming::Zero; blocks.len()];
    for (source_idx, source) in blocks.iter().enumerate() {
        for dest_id in outgoing_edges(source) {
            let dest_idx = block_id_to_idx[&dest_id];
            incoming[dest_idx] = match incoming[dest_idx] {
                NumIncoming::Zero => NumIncoming::One(source_idx),
                _ => NumIncoming::TooMany,
            }
        }
    }

    let mut result = vec![None; blocks.len()];

    for (dest_idx, inc) in incoming.iter().enumerate() {
        if let &NumIncoming::One(source_idx) = inc {
            if blocks[source_idx].instructions.last().unwrap().class.opcode == Op::Branch {
                result[source_idx] = Some(dest_idx);
            }
        }
    }

    result
}
