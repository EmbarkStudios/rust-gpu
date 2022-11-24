use indexmap::{indexmap, IndexMap};
use rspirv::dr::{Block, Builder, Function, InsertPoint, Module, Operand};
use rspirv::spirv::{LoopControl, Op, SelectionControl, Word};
use rustc_data_structures::fx::FxHashMap;
use std::{iter, mem};

/// Cached IDs of `OpTypeBool`, `OpConstantFalse`, and `OpConstantTrue`.
struct Globals {
    type_bool: Word,
    const_false: Word,
    const_true: Word,
}

// FIXME(eddyb) move this into some common module. Also consider whether we
// actually need a "builder" or could just operate on a `&mut Function`.
struct FuncBuilder<'a> {
    builder: &'a mut Builder,
}

impl FuncBuilder<'_> {
    fn function(&self) -> &Function {
        let func_idx = self.builder.selected_function().unwrap();
        &self.builder.module_ref().functions[func_idx]
    }

    fn function_mut(&mut self) -> &mut Function {
        let func_idx = self.builder.selected_function().unwrap();
        &mut self.builder.module_mut().functions[func_idx]
    }

    fn blocks(&self) -> &[Block] {
        &self.function().blocks
    }

    fn blocks_mut(&mut self) -> &mut [Block] {
        &mut self.function_mut().blocks
    }
}

pub fn structurize(module: Module) -> Module {
    let mut builder = Builder::new_from_module(module);

    // Get the `OpTypeBool` type (it will only be created if it's missing).
    let type_bool = builder.type_bool();

    // Find already present `OpConstant{False,True}` (if they're in the module).
    let mut existing_const_false = None;
    let mut existing_const_true = None;
    for inst in &builder.module_ref().types_global_values {
        let existing = match inst.class.opcode {
            Op::ConstantFalse => &mut existing_const_false,
            Op::ConstantTrue => &mut existing_const_true,
            _ => continue,
        };

        if existing.is_none() {
            *existing = Some(inst.result_id.unwrap());
        }

        if existing_const_false.is_some() && existing_const_true.is_some() {
            break;
        }
    }

    // Create new `OpConstant{False,True}` if they're missing.
    let const_false = existing_const_false.unwrap_or_else(|| builder.constant_false(type_bool));
    let const_true = existing_const_true.unwrap_or_else(|| builder.constant_true(type_bool));

    for func_idx in 0..builder.module_ref().functions.len() {
        builder.select_function(Some(func_idx)).unwrap();
        let func = FuncBuilder {
            builder: &mut builder,
        };

        let block_id_to_idx = func
            .blocks()
            .iter()
            .enumerate()
            .map(|(i, block)| (block.label_id().unwrap(), i))
            .collect();

        Structurizer {
            globals: Globals {
                type_bool,
                const_false,
                const_true,
            },
            func,
            block_id_to_idx,
            incoming_edge_count: vec![],
            regions: FxHashMap::default(),
        }
        .structurize_func();
    }

    builder.module()
}

// FIXME(eddyb) use newtyped indices and `IndexVec`.
type BlockIdx = usize;
type BlockId = Word;

/// Regions are made up of their entry block and all other blocks dominated
/// by that block. All edges leaving a region are considered "exits".
struct Region {
    /// After structurizing a region, all paths through it must lead to a single
    /// "merge" block (i.e. `merge` post-dominates the entire region).
    /// The `merge` block must be terminated by one of `OpReturn`, `OpReturnValue`,
    /// `OpKill`, `OpIgnoreIntersectionKHR`, `OpTerminateRayKHR` or
    /// `OpUnreachable`. If `exits` isn't empty, `merge` will receive an
    /// `OpBranch` from its parent region (to an outer merge block).
    merge: BlockIdx,
    merge_id: BlockId,

    exits: IndexMap<BlockIdx, Exit>,
}

#[derive(Default)]
struct Exit {
    /// Number of total edges to this target (a subset of the target's predecessors).
    edge_count: usize,

    /// If this is a deferred exit, `condition` is a boolean value which must
    /// be `true` in order to execute this exit.
    condition: Option<Word>,
}

struct Structurizer<'a> {
    globals: Globals,

    func: FuncBuilder<'a>,
    block_id_to_idx: FxHashMap<BlockId, BlockIdx>,

    /// Number of edges pointing to each block.
    /// Computed by `post_order` and updated when structuring loops
    /// (backedge count is subtracted to hide them from outer regions).
    incoming_edge_count: Vec<usize>,

    regions: FxHashMap<BlockIdx, Region>,
}

impl Structurizer<'_> {
    fn structurize_func(&mut self) {
        let Globals {
            const_false,
            const_true,
            type_bool,
        } = self.globals;

        // By iterating in post-order, we are guaranteed to visit "inner" regions
        // before "outer" ones.
        for block in self.post_order() {
            let block_id = self.func.blocks()[block].label_id().unwrap();
            let terminator = self.func.blocks()[block].instructions.last().unwrap();
            let mut region = match terminator.class.opcode {
                Op::Return
                | Op::ReturnValue
                | Op::Kill
                | Op::IgnoreIntersectionKHR
                | Op::TerminateRayKHR
                | Op::Unreachable => Region {
                    merge: block,
                    merge_id: block_id,
                    exits: indexmap! {},
                },

                Op::Branch => {
                    let target = self.block_id_to_idx[&terminator.operands[0].unwrap_id_ref()];
                    self.child_region(target).unwrap_or_else(|| {
                        self.func.builder.select_block(Some(block)).unwrap();
                        self.func.builder.pop_instruction().unwrap();
                        // Default all merges to `OpUnreachable`, in case they're unused.
                        self.func.builder.unreachable().unwrap();
                        Region {
                            merge: block,
                            merge_id: block_id,
                            exits: indexmap! {
                                target => Exit { edge_count: 1, condition: None }
                            },
                        }
                    })
                }

                Op::BranchConditional | Op::Switch => {
                    let target_operand_indices = match terminator.class.opcode {
                        Op::BranchConditional => (1..3).step_by(1),
                        Op::Switch => (1..terminator.operands.len()).step_by(2),
                        _ => unreachable!(),
                    };

                    // FIXME(eddyb) avoid wasteful allocation.
                    let child_regions: Vec<_> = target_operand_indices
                        .map(|i| {
                            let target_id = self.func.blocks()[block]
                                .instructions
                                .last()
                                .unwrap()
                                .operands[i]
                                .unwrap_id_ref();
                            let target = self.block_id_to_idx[&target_id];
                            self.child_region(target).unwrap_or_else(|| {
                                // Synthesize a single-block region for every edge that
                                // doesn't already enter a child region, so that the
                                // merge block we later generate has an unique source for
                                // every single arm of this conditional branch or switch,
                                // to attach per-exit condition phis to.
                                let new_block_id = self.func.builder.begin_block(None).unwrap();
                                let new_block = self.func.builder.selected_block().unwrap();
                                // Default all merges to `OpUnreachable`, in case they're unused.
                                self.func.builder.unreachable().unwrap();
                                self.func.blocks_mut()[block]
                                    .instructions
                                    .last_mut()
                                    .unwrap()
                                    .operands[i] = Operand::IdRef(new_block_id);
                                Region {
                                    merge: new_block,
                                    merge_id: new_block_id,
                                    exits: indexmap! {
                                        target => Exit { edge_count: 1, condition: None }
                                    },
                                }
                            })
                        })
                        .collect();

                    self.selection_merge_regions(block, &child_regions)
                }
                _ => panic!("Invalid block terminator: {:?}", terminator),
            };

            // Peel off deferred exits which have all their edges accounted for
            // already, within this region. Repeat until no such exits are left.
            while let Some((&target, _)) = region
                .exits
                .iter()
                .find(|&(&target, exit)| exit.edge_count == self.incoming_edge_count[target])
            {
                let taken_block_id = self.func.blocks()[target].label_id().unwrap();
                let exit = region.exits.remove(&target).unwrap();

                // Special-case the last exit as unconditional - regardless of
                // what might end up in `exit.condition`, what we'd generate is
                // `if exit.condition { branch target; } else { unreachable; }`
                // which is just `branch target;` with an extra assumption that
                // `exit.condition` is `true` (which we can just ignore).
                if region.exits.is_empty() {
                    self.func.builder.select_block(Some(region.merge)).unwrap();
                    assert_eq!(
                        self.func.builder.pop_instruction().unwrap().class.opcode,
                        Op::Unreachable
                    );
                    self.func.builder.branch(taken_block_id).unwrap();
                    region = self.regions.remove(&target).unwrap();
                    continue;
                }

                // Create a new block for the "`exit` not taken" path.
                let not_taken_block_id = self.func.builder.begin_block(None).unwrap();
                let not_taken_block = self.func.builder.selected_block().unwrap();
                // Default all merges to `OpUnreachable`, in case they're unused.
                self.func.builder.unreachable().unwrap();

                // Choose whether to take this `exit`, in the previous merge block.
                let branch_block = region.merge;
                self.func.builder.select_block(Some(branch_block)).unwrap();
                assert_eq!(
                    self.func.builder.pop_instruction().unwrap().class.opcode,
                    Op::Unreachable
                );
                self.func
                    .builder
                    .branch_conditional(
                        exit.condition.unwrap(),
                        taken_block_id,
                        not_taken_block_id,
                        iter::empty(),
                    )
                    .unwrap();

                // Merge the "taken" and "not taken" paths.
                let taken_region = self.regions.remove(&target).unwrap();
                let not_taken_region = Region {
                    merge: not_taken_block,
                    merge_id: not_taken_block_id,
                    exits: region.exits,
                };
                region =
                    self.selection_merge_regions(branch_block, &[taken_region, not_taken_region]);
            }

            // Peel off a backedge exit, which indicates this region is a loop.
            if let Some(mut backedge_exit) = region.exits.remove(&block) {
                // Inject a `while`-like loop header just before the start of the
                // loop body. This is needed because our "`break` vs `continue`"
                // choice is *after* the loop body, like in a `do`-`while` loop,
                // but SPIR-V requires it at the start, like in a `while` loop.
                let while_header_block_id = self.func.builder.begin_block(None).unwrap();
                let while_header_block = self.func.builder.selected_block().unwrap();
                self.func.builder.select_block(None).unwrap();
                let while_exit_block_id = self.func.builder.begin_block(None).unwrap();
                let while_exit_block = self.func.builder.selected_block().unwrap();
                // Default all merges to `OpUnreachable`, in case they're unused.
                self.func.builder.unreachable().unwrap();
                let while_body_block_id = self.func.builder.begin_block(None).unwrap();
                let while_body_block = self.func.builder.selected_block().unwrap();
                self.func.builder.select_block(None).unwrap();

                // Move all of the contents of the original `block` into the
                // new loop body, but keep labels and indices intact.
                // Also update the existing merge if it happens to be the `block`
                // we just moved (this should only be relevant to infinite loops).
                self.func.blocks_mut()[while_body_block].instructions =
                    mem::take(&mut self.func.blocks_mut()[block].instructions);
                if region.merge == block {
                    region.merge = while_body_block;
                    region.merge_id = while_body_block_id;
                }

                // Create a separate merge block for the loop body, as the original
                // one might be used by an `OpSelectionMerge` and cannot be reused.
                let while_body_merge_id = self.func.builder.begin_block(None).unwrap();
                let while_body_merge = self.func.builder.selected_block().unwrap();
                self.func.builder.select_block(None).unwrap();
                self.func.builder.select_block(Some(region.merge)).unwrap();
                assert_eq!(
                    self.func.builder.pop_instruction().unwrap().class.opcode,
                    Op::Unreachable
                );
                self.func.builder.branch(while_body_merge_id).unwrap();

                // Point both the original block and the merge of the loop body,
                // at the new loop header, and compute phis for all the exit
                // conditions (including the backedge, which indicates "continue").
                self.func.builder.select_block(Some(block)).unwrap();
                self.func.builder.branch(while_header_block_id).unwrap();
                self.func
                    .builder
                    .select_block(Some(while_body_merge))
                    .unwrap();
                self.func.builder.branch(while_header_block_id).unwrap();
                self.func
                    .builder
                    .select_block(Some(while_header_block))
                    .unwrap();

                for (&target, exit) in region
                    .exits
                    .iter_mut()
                    .chain(iter::once((&while_body_block, &mut backedge_exit)))
                {
                    let first_entry_case = (
                        if target == while_body_block {
                            const_true
                        } else {
                            const_false
                        },
                        block_id,
                    );
                    let repeat_case = (exit.condition.unwrap_or(const_true), while_body_merge_id);
                    let phi_cases = [first_entry_case, repeat_case];
                    exit.condition = Some(
                        self.func
                            .builder
                            .phi(type_bool, None, phi_cases.iter().copied())
                            .unwrap(),
                    );
                }

                // Choose whether to keep looping, in the `while`-like loop header.
                self.func
                    .builder
                    .select_block(Some(while_header_block))
                    .unwrap();
                self.func
                    .builder
                    .loop_merge(
                        while_exit_block_id,
                        while_body_merge_id,
                        LoopControl::NONE,
                        iter::empty(),
                    )
                    .unwrap();
                self.func
                    .builder
                    .select_block(Some(while_header_block))
                    .unwrap();
                self.func
                    .builder
                    .branch_conditional(
                        backedge_exit.condition.unwrap(),
                        while_body_block_id,
                        while_exit_block_id,
                        iter::empty(),
                    )
                    .unwrap();
                region.merge = while_exit_block;
                region.merge_id = while_exit_block_id;

                // Remove the backedge count from the total incoming count of `block`.
                // This will allow outer regions to treat the loop opaquely.
                self.incoming_edge_count[block] -= backedge_exit.edge_count;
            }

            self.regions.insert(block, region);
        }

        assert_eq!(self.regions.len(), 1);
        assert_eq!(self.regions.values().next().unwrap().exits.len(), 0);
    }

    fn child_region(&mut self, target: BlockIdx) -> Option<Region> {
        // An "entry" edge is the unique edge into a region.
        if self.incoming_edge_count[target] == 1 {
            Some(self.regions.remove(&target).unwrap())
        } else {
            None
        }
    }

    fn selection_merge_regions(&mut self, block: BlockIdx, child_regions: &[Region]) -> Region {
        let Globals {
            const_false,
            const_true,
            type_bool,
        } = self.globals;

        // HACK(eddyb) this special-cases the easy case where we can
        // just reuse a merge block, and don't have to create our own.
        let unconditional_single_exit = |region: &Region| {
            region.exits.len() == 1 && region.exits.get_index(0).unwrap().1.condition.is_none()
        };
        let structural_merge = if child_regions.iter().all(unconditional_single_exit) {
            let merge = *child_regions[0].exits.get_index(0).unwrap().0;
            if child_regions
                .iter()
                .all(|region| *region.exits.get_index(0).unwrap().0 == merge)
                && child_regions
                    .iter()
                    .map(|region| region.exits.get_index(0).unwrap().1.edge_count)
                    .sum::<usize>()
                    == self.incoming_edge_count[merge]
            {
                Some(merge)
            } else {
                None
            }
        } else {
            None
        };

        // Reuse or create a merge block, and use it as the selection merge.
        let merge = structural_merge.unwrap_or_else(|| {
            self.func.builder.begin_block(None).unwrap();
            self.func.builder.selected_block().unwrap()
        });
        let merge_id = self.func.blocks()[merge].label_id().unwrap();
        self.func.builder.select_block(Some(block)).unwrap();
        self.func
            .builder
            .insert_selection_merge(InsertPoint::FromEnd(1), merge_id, SelectionControl::NONE)
            .unwrap();

        // Branch all the child regions into our merge block.
        for region in child_regions {
            // HACK(eddyb) empty `region.exits` indicate diverging control-flow,
            // and that we should ignore `region.merge`.
            if !region.exits.is_empty() {
                self.func.builder.select_block(Some(region.merge)).unwrap();
                assert_eq!(
                    self.func.builder.pop_instruction().unwrap().class.opcode,
                    Op::Unreachable
                );
                self.func.builder.branch(merge_id).unwrap();
            }
        }

        if let Some(merge) = structural_merge {
            self.regions.remove(&merge).unwrap()
        } else {
            self.func.builder.select_block(Some(merge)).unwrap();

            // Gather all the potential exits.
            let mut exits: IndexMap<BlockIdx, Exit> = indexmap! {};
            for region in child_regions {
                for (&target, exit) in &region.exits {
                    exits.entry(target).or_default().edge_count += exit.edge_count;
                }
            }

            // Update conditions using phis.
            for (&target, exit) in &mut exits {
                let phi_cases = child_regions
                    .iter()
                    .filter(|region| {
                        // HACK(eddyb) empty `region.exits` indicate diverging control-flow,
                        // and that we should ignore `region.merge`.
                        !region.exits.is_empty()
                    })
                    .map(|region| {
                        (
                            match region.exits.get(&target) {
                                Some(exit) => exit.condition.unwrap_or(const_true),
                                None => const_false,
                            },
                            region.merge_id,
                        )
                    });
                exit.condition = Some(self.func.builder.phi(type_bool, None, phi_cases).unwrap());
            }

            // Default all merges to `OpUnreachable`, in case they're unused.
            self.func.builder.unreachable().unwrap();

            Region {
                merge,
                merge_id,
                exits,
            }
        }
    }

    // FIXME(eddyb) replace this with `rustc_data_structures::graph::iterate`
    // (or similar).
    fn post_order(&mut self) -> Vec<BlockIdx> {
        let blocks = self.func.blocks();

        // HACK(eddyb) compute edge counts through the post-order traversal.
        assert!(self.incoming_edge_count.is_empty());
        self.incoming_edge_count = vec![0; blocks.len()];

        // FIXME(eddyb) use a proper bitset.
        let mut visited = vec![false; blocks.len()];
        let mut post_order = Vec::with_capacity(blocks.len());

        self.post_order_step(0, &mut visited, &mut post_order);

        post_order
    }

    fn post_order_step(
        &mut self,
        block: BlockIdx,
        visited: &mut [bool],
        post_order: &mut Vec<BlockIdx>,
    ) {
        self.incoming_edge_count[block] += 1;

        if visited[block] {
            return;
        }
        visited[block] = true;

        for target in
            super::simple_passes::outgoing_edges(&self.func.blocks()[block]).collect::<Vec<_>>()
        {
            self.post_order_step(self.block_id_to_idx[&target], visited, post_order);
        }

        post_order.push(block);
    }
}
