use crate::decorations::UnrollLoopsDecoration;
use indexmap::{indexmap, IndexMap};
use rspirv::dr::{Block, Builder, Function, InsertPoint, Module, Operand};
use rspirv::spirv::{LoopControl, Op, SelectionControl, Word};
use std::collections::HashMap;
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

pub fn structurize(
    module: Module,
    unroll_loops_decorations: HashMap<Word, UnrollLoopsDecoration>,
) -> Module {
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

        let func_id = func.function().def_id().unwrap();

        let loop_control = match unroll_loops_decorations.get(&func_id) {
            Some(UnrollLoopsDecoration {}) => LoopControl::UNROLL,
            None => LoopControl::NONE,
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
            loop_control,
            incoming_edge_count: vec![],
            regions: HashMap::new(),
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
#[derive(Debug)]
enum Region {
    /// All paths through the region "diverge", i.e. they never leave the region
    /// by branching to another block in the same function, so they either:
    /// * get stuck in an (infinite) loop
    /// * reach `OpReturn`, `OpReturnValue`, `OpKill`, or `OpUnreachable`
    ///
    /// Such a region is fully structurized and requires no propagation to its
    /// parent region (only that a branch into its entry block exists).
    Divergent,

    /// Some paths through the region don't "diverge" (see `ConvergentRegion`).
    Convergent(ConvergentRegion),
}

/// `Region` where at least some paths through the region leave it by branching
/// to other blocks in the same function.
///
/// After structurizing, all paths through it must lead to a single "merge" block
/// (i.e. `merge` post-dominates the entire region), with all of the branches to
/// outside the region having been moved from the CFG itself, into `exits`.
///
/// The `merge` block is terminated by `OpUnreachable`, to be replaced with an
/// `OpBranch` (to an outer merge block) by its parent region, which will also
/// inherit its `exits` (potentially attaching some/all of them to itself).
#[derive(Debug)]
struct ConvergentRegion {
    merge: BlockIdx,
    merge_id: BlockId,

    // FIXME(eddyb) use something more compact when `exits.len()` is small
    // (e.g. `ArrayVec<[(BlockIdx, Exit); 4]>` with linear search).
    exits: IndexMap<BlockIdx, Exit>,
}

#[derive(Default, Debug)]
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
    block_id_to_idx: HashMap<BlockId, BlockIdx>,

    /// `LoopControl` to use in all loops' `OpLoopMerge` instruction.
    /// Currently only affected by function-scoped `#[spirv(unroll_loops)]`.
    loop_control: LoopControl,

    /// Number of edges pointing to each block.
    /// Computed by `post_order` and updated when structuring loops
    /// (backedge count is subtracted to hide them from outer regions).
    incoming_edge_count: Vec<usize>,

    regions: HashMap<BlockIdx, Region>,
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
                Op::Return | Op::ReturnValue | Op::Kill | Op::Unreachable => Region::Divergent,

                Op::Branch => {
                    let target = self.block_id_to_idx[&terminator.operands[0].unwrap_id_ref()];
                    self.child_region(target).unwrap_or_else(|| {
                        self.func.builder.select_block(Some(block)).unwrap();
                        self.func.builder.pop_instruction().unwrap();
                        // Default all merges to `OpUnreachable`, in case they're unused.
                        self.func.builder.unreachable().unwrap();
                        Region::Convergent(ConvergentRegion {
                            merge: block,
                            merge_id: block_id,
                            exits: indexmap! {
                                target => Exit { edge_count: 1, condition: None }
                            },
                        })
                    })
                }

                Op::BranchConditional | Op::Switch => {
                    let target_operand_indices = match terminator.class.opcode {
                        Op::BranchConditional => (1..3).step_by(1),
                        Op::Switch => (1..terminator.operands.len()).step_by(2),
                        _ => unreachable!(),
                    };

                    // FIXME(eddyb) avoid wasteful allocation.
                    let convergent_child_regions: Vec<_> = target_operand_indices
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
                                Region::Convergent(ConvergentRegion {
                                    merge: new_block,
                                    merge_id: new_block_id,
                                    exits: indexmap! {
                                        target => Exit { edge_count: 1, condition: None }
                                    },
                                })
                            })
                        })
                        .filter_map(|region| match region {
                            Region::Divergent => None,
                            Region::Convergent(region) => Some(region),
                        })
                        .collect();

                    self.selection_merge_convergent_regions(block, &convergent_child_regions)
                }
                _ => panic!("Invalid block terminator: {:?}", terminator),
            };

            // Peel off deferred exits which have all their edges accounted for
            // already, within this region. Repeat until no such exits are left.
            while let Region::Convergent(convergent_region) = &mut region {
                let (region_merge, target, exit) =
                    match convergent_region.exits.iter().find(|&(&target, exit)| {
                        exit.edge_count == self.incoming_edge_count[target]
                    }) {
                        Some((&target, _)) => {
                            let exit = convergent_region.exits.remove(&target).unwrap();
                            let region_merge = convergent_region.merge;
                            if convergent_region.exits.is_empty() {
                                region = Region::Divergent;
                            }
                            (region_merge, target, exit)
                        }
                        None => {
                            break;
                        }
                    };

                let taken_block_id = self.func.blocks()[target].label_id().unwrap();
                let taken_region = self.regions.remove(&target).unwrap();

                // Choose whether to take this `exit`, in the previous merge block.
                region = match region {
                    Region::Divergent => {
                        // Special-case the last exit as unconditional - regardless of
                        // what might end up in `exit.condition`, what we'd generate is
                        // `if exit.condition { branch target; } else { unreachable; }`
                        // which is just `branch target;` with an extra assumption that
                        // `exit.condition` is `true` (which we can just ignore).
                        self.func.builder.select_block(Some(region_merge)).unwrap();
                        assert_eq!(
                            self.func.builder.pop_instruction().unwrap().class.opcode,
                            Op::Unreachable
                        );
                        self.func.builder.branch(taken_block_id).unwrap();
                        taken_region
                    }
                    Region::Convergent(ConvergentRegion { exits, .. }) => {
                        // Create a new block for the "`exit` not taken" path.
                        let not_taken_block_id = self.func.builder.begin_block(None).unwrap();
                        let not_taken_block = self.func.builder.selected_block().unwrap();
                        // Default all merges to `OpUnreachable`, in case they're unused.
                        self.func.builder.unreachable().unwrap();

                        let not_taken_region = ConvergentRegion {
                            merge: not_taken_block,
                            merge_id: not_taken_block_id,
                            exits,
                        };

                        self.func.builder.select_block(Some(region_merge)).unwrap();
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
                        match taken_region {
                            Region::Divergent => {
                                self.func.builder.select_block(Some(region_merge)).unwrap();
                                self.func
                                    .builder
                                    .insert_selection_merge(
                                        InsertPoint::FromEnd(1),
                                        not_taken_block_id,
                                        SelectionControl::NONE,
                                    )
                                    .unwrap();
                                Region::Convergent(not_taken_region)
                            }
                            Region::Convergent(taken_region) => self
                                .selection_merge_convergent_regions(
                                    region_merge,
                                    &[taken_region, not_taken_region],
                                ),
                        }
                    }
                };
            }

            // Peel off a backedge exit, which indicates this region is a loop.
            let region_merge_and_backedge_exit = match &mut region {
                Region::Divergent => None,
                Region::Convergent(convergent_region) => {
                    match convergent_region.exits.remove(&block) {
                        Some(backedge_exit) => {
                            let region_merge = convergent_region.merge;
                            if convergent_region.exits.is_empty() {
                                region = Region::Divergent;
                            }
                            Some((region_merge, backedge_exit))
                        }
                        None => None,
                    }
                }
            };
            if let Some((mut region_merge, mut backedge_exit)) = region_merge_and_backedge_exit {
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
                    mem::replace(&mut self.func.blocks_mut()[block].instructions, vec![]);
                if region_merge == block {
                    region_merge = while_body_block;
                }

                // Create a separate merge block for the loop body, as the original
                // one might be used by an `OpSelectionMerge` and cannot be reused.
                let while_body_merge_id = self.func.builder.begin_block(None).unwrap();
                let while_body_merge = self.func.builder.selected_block().unwrap();
                self.func.builder.select_block(None).unwrap();
                self.func.builder.select_block(Some(region_merge)).unwrap();
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

                let region_exits = match &mut region {
                    Region::Divergent => None,
                    Region::Convergent(ConvergentRegion { exits, .. }) => Some(exits.iter_mut()),
                };
                for (&target, exit) in region_exits
                    .into_iter()
                    .flatten()
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
                        self.loop_control,
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
                region = match region {
                    Region::Divergent => Region::Divergent,
                    Region::Convergent(ConvergentRegion { exits, .. }) => {
                        Region::Convergent(ConvergentRegion {
                            merge: while_exit_block,
                            merge_id: while_exit_block_id,
                            exits,
                        })
                    }
                };

                // Remove the backedge count from the total incoming count of `block`.
                // This will allow outer regions to treat the loop opaquely.
                self.incoming_edge_count[block] -= backedge_exit.edge_count;
            }

            self.regions.insert(block, region);
        }

        assert_eq!(self.regions.len(), 1);
        assert_matches!(self.regions.values().next().unwrap(), Region::Divergent);
    }

    fn child_region(&mut self, target: BlockIdx) -> Option<Region> {
        // An "entry" edge is the unique edge into a region.
        if self.incoming_edge_count[target] == 1 {
            Some(self.regions.remove(&target).unwrap())
        } else {
            None
        }
    }

    fn selection_merge_convergent_regions(
        &mut self,
        block: BlockIdx,
        child_regions: &[ConvergentRegion],
    ) -> Region {
        let Globals {
            const_false,
            const_true,
            type_bool,
        } = self.globals;

        // HACK(eddyb) this special-cases the easy case where we can
        // just reuse a merge block, and don't have to create our own.
        let unconditional_single_exit = |region: &ConvergentRegion| {
            region.exits.len() == 1 && region.exits.get_index(0).unwrap().1.condition.is_none()
        };
        let structural_merge =
            if !child_regions.is_empty() && child_regions.iter().all(unconditional_single_exit) {
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
            self.func.builder.select_block(Some(region.merge)).unwrap();
            assert_eq!(
                self.func.builder.pop_instruction().unwrap().class.opcode,
                Op::Unreachable
            );
            self.func.builder.branch(merge_id).unwrap();
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
                let phi_cases = child_regions.iter().map(|region| {
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

            if exits.is_empty() {
                Region::Divergent
            } else {
                Region::Convergent(ConvergentRegion {
                    merge,
                    merge_id,
                    exits,
                })
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

        for target in super::simple_passes::outgoing_edges(&self.func.blocks()[block]) {
            self.post_order_step(self.block_id_to_idx[&target], visited, post_order)
        }

        post_order.push(block);
    }
}
