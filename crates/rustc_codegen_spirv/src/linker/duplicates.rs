use crate::custom_insts::{self, CustomOp};
use rspirv::binary::Assemble;
use rspirv::dr::{Instruction, Module, Operand};
use rspirv::spirv::{Op, Word};
use rustc_data_structures::fx::{FxHashMap, FxHashSet};
use rustc_middle::bug;
use smallvec::SmallVec;
use std::collections::hash_map;
use std::mem;

// FIXME(eddyb) consider deduplicating the `OpString` and `OpSource` created for
// file-level debuginfo (but using SPIR-T for linking might be better?).

pub fn remove_duplicate_extensions(module: &mut Module) {
    let mut set = FxHashSet::default();

    module.extensions.retain(|inst| {
        inst.class.opcode != Op::Extension
            || set.insert(inst.operands[0].unwrap_literal_string().to_string())
    });
}

pub fn remove_duplicate_capablities(module: &mut Module) {
    let mut set = FxHashSet::default();
    module.capabilities.retain(|inst| {
        inst.class.opcode != Op::Capability || set.insert(inst.operands[0].unwrap_capability())
    });
}

pub fn remove_duplicate_ext_inst_imports(module: &mut Module) {
    // This is a simpler version of remove_duplicate_types, see that for comments
    let mut ext_to_id = FxHashMap::default();
    let mut rewrite_rules = FxHashMap::default();

    // First deduplicate the imports
    for inst in &mut module.ext_inst_imports {
        if let Operand::LiteralString(ext_inst_import) = &inst.operands[0] {
            match ext_to_id.entry(ext_inst_import.clone()) {
                hash_map::Entry::Vacant(entry) => {
                    entry.insert(inst.result_id.unwrap());
                }
                hash_map::Entry::Occupied(entry) => {
                    let old_value = rewrite_rules.insert(inst.result_id.unwrap(), *entry.get());
                    assert!(old_value.is_none());
                    // We're iterating through the vec, so removing items is hard - nop it out.
                    *inst = Instruction::new(Op::Nop, None, None, vec![]);
                }
            }
        }
    }

    // Delete the nops we inserted
    module
        .ext_inst_imports
        .retain(|op| op.class.opcode != Op::Nop);

    // Then rewrite all OpExtInst referencing the rewritten IDs
    for inst in module.all_inst_iter_mut() {
        if inst.class.opcode == Op::ExtInst {
            if let Operand::IdRef(ref mut id) = inst.operands[0] {
                *id = rewrite_rules.get(id).copied().unwrap_or(*id);
            }
        }
    }
}

fn make_annotation_key(inst: &Instruction) -> Vec<u32> {
    let mut data = vec![inst.class.opcode as u32];

    // skip over the target ID
    for op in inst.operands.iter().skip(1) {
        op.assemble_into(&mut data);
    }

    data
}

fn gather_annotations(annotations: &[Instruction]) -> FxHashMap<Word, Vec<u32>> {
    let mut map = FxHashMap::default();
    for inst in annotations {
        match inst.class.opcode {
            Op::Decorate
            | Op::DecorateId
            | Op::DecorateString
            | Op::MemberDecorate
            | Op::MemberDecorateString => match map.entry(inst.operands[0].id_ref_any().unwrap()) {
                hash_map::Entry::Vacant(entry) => {
                    entry.insert(vec![make_annotation_key(inst)]);
                }
                hash_map::Entry::Occupied(mut entry) => {
                    entry.get_mut().push(make_annotation_key(inst));
                }
            },
            _ => {}
        }
    }
    map.into_iter()
        .map(|(key, mut value)| {
            (key, {
                value.sort();
                value.concat()
            })
        })
        .collect()
}

fn gather_names(debug_names: &[Instruction]) -> FxHashMap<Word, String> {
    debug_names
        .iter()
        .filter(|inst| inst.class.opcode == Op::Name)
        .map(|inst| {
            (
                inst.operands[0].unwrap_id_ref(),
                inst.operands[1].unwrap_literal_string().to_owned(),
            )
        })
        .collect()
}

fn make_dedupe_key(
    inst: &Instruction,
    unresolved_forward_pointers: &FxHashSet<Word>,
    annotations: &FxHashMap<Word, Vec<u32>>,
    names: &FxHashMap<Word, String>,
) -> Vec<u32> {
    let mut data = vec![inst.class.opcode as u32];

    if let Some(id) = inst.result_type {
        // We're not only deduplicating types here, but constants as well. Those contain result_types, and so we
        // need to include those here. For example, OpConstant can have the same arg, but different result_type,
        // and it should not be deduplicated (e.g. the constants 1u8 and 1u16).
        data.push(id);
    }
    for op in &inst.operands {
        if let Operand::IdRef(id) = op {
            if unresolved_forward_pointers.contains(id) {
                // TODO: This is implementing forward pointers incorrectly. All unresolved forward pointers will
                // compare equal.
                Operand::IdRef(0).assemble_into(&mut data);
            } else {
                op.assemble_into(&mut data);
            }
        } else {
            op.assemble_into(&mut data);
        }
    }
    if let Some(id) = inst.result_id {
        if let Some(annos) = annotations.get(&id) {
            data.extend_from_slice(annos);
        }
        if inst.class.opcode == Op::Variable {
            // Names only matter for OpVariable.
            if let Some(name) = names.get(&id) {
                // Jump through some hoops to shove a String into a Vec<u32>.
                //
                // FIXME(eddyb) this should `.assemble_into(&mut data)` the
                // `Operand::LiteralString(...)` from the original `Op::Name`.
                for chunk in name.as_bytes().chunks(4) {
                    let slice = match *chunk {
                        [a] => [a, 0, 0, 0],
                        [a, b] => [a, b, 0, 0],
                        [a, b, c] => [a, b, c, 0],
                        [a, b, c, d] => [a, b, c, d],
                        _ => bug!(),
                    };
                    data.push(u32::from_le_bytes(slice));
                }
            }
        }
    }

    data
}

fn rewrite_inst_with_rules(inst: &mut Instruction, rules: &FxHashMap<u32, u32>) {
    if let Some(ref mut id) = inst.result_type {
        // If the rewrite rules contain this ID, replace with the mapped value, otherwise don't touch it.
        *id = rules.get(id).copied().unwrap_or(*id);
    }
    for op in &mut inst.operands {
        if let Some(id) = op.id_ref_any_mut() {
            *id = rules.get(id).copied().unwrap_or(*id);
        }
    }
}

pub fn remove_duplicate_types(module: &mut Module) {
    // Keep in mind, this algorithm requires forward type references to not exist - i.e. it's a valid spir-v module.

    // When a duplicate type is encountered, then this is a map from the deleted ID, to the new, deduplicated ID.
    let mut rewrite_rules = FxHashMap::default();
    // Instructions are encoded into "keys": their opcode, followed by arguments, then annotations.
    // Importantly, result_id is left out. This means that any instruction that declares the same
    // type, but with different result_id, will result in the same key.
    let mut key_to_result_id = FxHashMap::default();
    // TODO: This is implementing forward pointers incorrectly.
    let mut unresolved_forward_pointers = FxHashSet::default();

    // Collect a map from type ID to an annotation "key blob" (to append to the type key)
    let annotations = gather_annotations(&module.annotations);
    let names = gather_names(&module.debug_names);

    for inst in &mut module.types_global_values {
        if inst.class.opcode == Op::TypeForwardPointer {
            if let Operand::IdRef(id) = inst.operands[0] {
                unresolved_forward_pointers.insert(id);
                continue;
            }
        }
        if inst.class.opcode == Op::TypePointer
            && unresolved_forward_pointers.contains(&inst.result_id.unwrap())
        {
            unresolved_forward_pointers.remove(&inst.result_id.unwrap());
        }
        // This is an important spot: Say that we come upon a duplicated aggregate type (one that references
        // other types). Its arguments may be duplicated themselves, and so building the key directly will fail
        // to match up with the first type. However, **because forward references are not allowed**, we're
        // guaranteed to have already found and deduplicated the argument types! So that means the deduplication
        // translation is already in rewrite_rules, and we merely need to apply the mapping before generating
        // the key.
        // Nit: Overwriting the instruction isn't technically necessary, as it will get handled by the final
        // all_inst_iter_mut pass below. However, the code is a lil bit cleaner this way I guess.
        rewrite_inst_with_rules(inst, &rewrite_rules);

        let key = make_dedupe_key(inst, &unresolved_forward_pointers, &annotations, &names);

        match key_to_result_id.entry(key) {
            hash_map::Entry::Vacant(entry) => {
                // This is the first time we've seen this key. Insert the key into the map, registering this type as
                // something other types can deduplicate to.
                entry.insert(inst.result_id.unwrap());
            }
            hash_map::Entry::Occupied(entry) => {
                // We've already seen this key. We need to do two things:
                // 1) Add a rewrite rule from this type to the type that we saw before.
                let old_value = rewrite_rules.insert(inst.result_id.unwrap(), *entry.get());
                // 2) Erase this instruction. Because we're iterating over this vec, removing an element is hard, so
                // clear it with OpNop, and then remove it in the retain() call below.
                assert!(old_value.is_none());
                *inst = Instruction::new(Op::Nop, None, None, vec![]);
            }
        }
    }

    // We rewrote instructions we wanted to remove with OpNop. Remove them properly.
    module
        .types_global_values
        .retain(|op| op.class.opcode != Op::Nop);

    // Apply the rewrite rules to the whole module
    for inst in module.all_inst_iter_mut() {
        rewrite_inst_with_rules(inst, &rewrite_rules);
    }

    // The same decorations for duplicated types will cause those different types to merge
    // together. So, we need to deduplicate the annotations as well. (Note we *do* care about the
    // ID of the type being applied to here, unlike `gather_annotations`)
    let mut anno_set = FxHashSet::default();
    module
        .annotations
        .retain(|inst| anno_set.insert(inst.assemble()));
    // Same thing with OpName
    let mut name_ids = FxHashSet::default();
    let mut member_name_ids = FxHashSet::default();
    module.debug_names.retain(|inst| {
        (inst.class.opcode != Op::Name || name_ids.insert(inst.operands[0].unwrap_id_ref()))
            && (inst.class.opcode != Op::MemberName
                || member_name_ids.insert((
                    inst.operands[0].unwrap_id_ref(),
                    inst.operands[1].unwrap_literal_int32(),
                )))
    });
}

pub fn remove_duplicate_debuginfo(module: &mut Module) {
    // FIXME(eddyb) avoid repeating this across different passes/helpers.
    let custom_ext_inst_set_import = module
        .ext_inst_imports
        .iter()
        .find(|inst| {
            assert_eq!(inst.class.opcode, Op::ExtInstImport);
            inst.operands[0].unwrap_literal_string() == &custom_insts::CUSTOM_EXT_INST_SET[..]
        })
        .map(|inst| inst.result_id.unwrap());

    for func in &mut module.functions {
        for block in &mut func.blocks {
            // Ignore the terminator, it's effectively "outside" debuginfo.
            let (_, insts) = block.instructions.split_last_mut().unwrap();

            // HACK(eddyb) to make random access easier, we first replace unused
            // instructions with `OpNop`, and then remove all the `OpNop`s.

            #[derive(Clone)]
            struct DbgLocInst {
                inst_idx: usize,
                used: bool,
            }

            fn nop() -> Instruction {
                Instruction::new(Op::Nop, None, None, vec![])
            }
            impl DbgLocInst {
                fn nop_if_unused(&self, insts: &mut [Instruction]) {
                    if !self.used {
                        insts[self.inst_idx] = nop();
                    }
                }
            }

            #[derive(Clone, Default)]
            struct DbgState {
                loc: Option<DbgLocInst>,
                has_semantic_insts: bool,
            }
            let mut dbg = DbgState::default();

            struct Frame {
                call_dbg: DbgState,
                push_inst_idx: usize,
            }
            let mut inlined_frames = SmallVec::<[Frame; 8]>::new();

            // HACK(eddyb) `PopInlinedCallFrame` moves `inlined_frames.last()`
            // `fusable_freshly_popped_inlined_frames.last()`, so a sequence of
            // N pops will reverse the N last entries of `inlined_frames` into
            // this vector (and go from outside-in, to inside-out), which allows
            // *fusing* a pop with a push (of an identical inlined frame), when
            // no interverning instructions prevent it (such instructions will
            // clear this vector to indicate the pops are "fully committed").
            struct PoppedFrame {
                frame: Frame,
                callee_has_semantic_insts: bool,
                pop_inst_idx: usize,
            }
            let mut fusable_freshly_popped_inlined_frames = SmallVec::<[PoppedFrame; 8]>::new();

            for inst_idx in 0..insts.len() {
                let inst = &insts[inst_idx];
                let custom_op = match inst.class.opcode {
                    Op::ExtInst
                        if Some(inst.operands[0].unwrap_id_ref()) == custom_ext_inst_set_import =>
                    {
                        Some(CustomOp::decode_from_ext_inst(inst))
                    }
                    _ => None,
                };

                fn inst_eq_key(inst: &Instruction) -> impl PartialEq + '_ {
                    (inst.class.opcode, &inst.operands)
                }

                // NOTE(eddyb) `fusable_freshly_popped_inlined_frames`-preserving
                // cases must all use `if can_continue { continue; }` to skip the
                // draining logic (`can_continue` is only `false` at the very end).
                let can_continue = inst_idx < insts.len() - 1;
                let prev_dbg_loc_snapshot = dbg.loc.clone();
                match (inst.class.opcode, custom_op) {
                    (Op::Line | Op::NoLine, _)
                    | (_, Some(CustomOp::SetDebugSrcLoc | CustomOp::ClearDebugSrcLoc)) => {
                        // HACK(eddyb) prefer keeping older active `DbgLocInst`s,
                        // if all the details are the same (it helps with fusion).
                        if dbg.loc.as_ref().is_some_and(|old_dbg_loc| {
                            inst_eq_key(inst) == inst_eq_key(&insts[old_dbg_loc.inst_idx])
                        }) {
                            insts[inst_idx] = nop();
                            if can_continue {
                                continue;
                            }
                        } else {
                            dbg.loc = Some(DbgLocInst {
                                inst_idx,
                                used: false,
                            });
                        }
                    }
                    (_, Some(CustomOp::PushInlinedCallFrame)) => {
                        // HACK(eddyb) attempt fusing this push with the last pop.
                        let fuse_with_last_pop = fusable_freshly_popped_inlined_frames
                            .last()
                            .is_some_and(|last_popped| {
                                // HACK(eddyb) updating `dbg.loc` deduplicates eagerly,
                                // so here it suffices to check the (deduped) indices.
                                let dbg_loc_inst_idx =
                                    |dbg: &DbgState| dbg.loc.as_ref().map(|d| d.inst_idx);
                                dbg_loc_inst_idx(&last_popped.frame.call_dbg)
                                    == dbg_loc_inst_idx(&dbg)
                                    && inst_eq_key(inst)
                                        == inst_eq_key(&insts[last_popped.frame.push_inst_idx])
                            });
                        if fuse_with_last_pop {
                            let PoppedFrame {
                                frame,
                                callee_has_semantic_insts,
                                pop_inst_idx,
                            } = fusable_freshly_popped_inlined_frames.pop().unwrap();

                            insts[pop_inst_idx] = nop();

                            // Can't make entering an inlined function a nop,
                            // as it needs to reset callee-side `DbgLocInst`,
                            // but we can replace it in-place and hope later
                            // it get nop'd out by some real `DbgLocInst`.
                            insts[inst_idx].operands.splice(
                                1..,
                                [Operand::LiteralExtInstInteger(
                                    CustomOp::ClearDebugSrcLoc as u32,
                                )],
                            );
                            dbg = DbgState {
                                loc: Some(DbgLocInst {
                                    inst_idx,
                                    used: false,
                                }),
                                has_semantic_insts: callee_has_semantic_insts,
                            };

                            inlined_frames.push(frame);

                            // Allow further fusing to occur.
                            if can_continue {
                                continue;
                            }
                        } else {
                            // HACK(eddyb) the actual push to `inlined_frames` is
                            // done at the very end of the loop body, to be able
                            // to process any pending updates on the previous state.
                        }
                    }
                    (_, Some(CustomOp::PopInlinedCallFrame)) => {
                        // Leaving an inlined function doesn't use `DbgLocInst`.
                        if let Some(dbg_loc) = dbg.loc.take() {
                            // HACK(eddyb) only treat as "definitely unused"
                            // instructions that are too "recent" to have been
                            // used by a `PushInlinedCallFrame` with a still
                            // uncommitted `PopInlinedCallFrame`.
                            let min_safe_inst_idx_to_nop = fusable_freshly_popped_inlined_frames
                                .last()
                                .map_or(0, |last_popped| last_popped.pop_inst_idx);
                            if dbg_loc.inst_idx > min_safe_inst_idx_to_nop {
                                dbg_loc.nop_if_unused(insts);
                            }
                        }
                        if let Some(frame) = inlined_frames.pop() {
                            let callee_has_semantic_insts = dbg.has_semantic_insts;
                            dbg = frame.call_dbg.clone();
                            dbg.has_semantic_insts |= callee_has_semantic_insts;

                            // HACK(eddyb) inform future `PushInlinedCallFrame`s
                            // of potential fusion, by saving a copy of the frame.
                            fusable_freshly_popped_inlined_frames.push(PoppedFrame {
                                frame,
                                callee_has_semantic_insts,
                                pop_inst_idx: inst_idx,
                            });
                        } else {
                            // FIXME(eddyb) this may indicate a bug elsewhere.
                            insts[inst_idx] = nop();
                        }
                        if can_continue {
                            continue;
                        }
                    }
                    _ => {
                        if let Some(dbg_loc) = &mut dbg.loc {
                            dbg_loc.used = true;
                        }
                        dbg.has_semantic_insts = true;
                    }
                }

                // NOTE(eddyb) mutable so that it may be marked as used below.
                let mut freshly_replaced_dbg_loc = prev_dbg_loc_snapshot.filter(|prev_dbg_loc| {
                    dbg.loc.as_ref().map(|d| d.inst_idx) != Some(prev_dbg_loc.inst_idx)
                });

                // NOTE(eddyb) the iteration order doesn't matter, as this is
                // effectively a set of `PopInlinedCallFrame`s which have had
                // all their other side-effects processed, and didn't get a
                // chance to be fused away, so they're getting committed.
                for popped in fusable_freshly_popped_inlined_frames.drain(..) {
                    let PoppedFrame {
                        mut frame,
                        callee_has_semantic_insts,
                        pop_inst_idx,
                    } = popped;

                    // HACK(eddyb) this popped frame's `call_dbg.loc` may still
                    // be used elsewhere, in which case that use takes precedence,
                    // and is effectively the new "owner" of the `DbgLocInst`.
                    let call_dbg_loc_used_elsewhere =
                        frame.call_dbg.loc.as_ref().and_then(|call_dbg_loc| {
                            [dbg.loc.as_mut(), freshly_replaced_dbg_loc.as_mut()]
                                .into_iter()
                                .flatten()
                                .find(|dbg_loc| dbg_loc.inst_idx == call_dbg_loc.inst_idx)
                        });
                    if call_dbg_loc_used_elsewhere.is_some() {
                        frame.call_dbg.loc = None;
                    }

                    if callee_has_semantic_insts {
                        // The `PushInlinedCallFrame` being kept requires its
                        // original `DbgLocInst` to also be kept around.
                        if let Some(call_dbg_loc) = call_dbg_loc_used_elsewhere {
                            call_dbg_loc.used = true;
                        }
                    } else {
                        // If the entire inlined call is all `OpNop`s now,
                        // entering/leaving it can also become `OpNop`s.
                        if let Some(call_dbg_loc) = &mut frame.call_dbg.loc {
                            call_dbg_loc.nop_if_unused(insts);
                        }
                        insts[frame.push_inst_idx] = nop();
                        insts[pop_inst_idx] = nop();
                    }
                }

                // Only remove a replaced `DbgLocInst` after it had a chance to
                // be marked as used above (for e.g. a `PushInlinedCallFrame`).
                if let Some(old_dbg_loc) = freshly_replaced_dbg_loc {
                    old_dbg_loc.nop_if_unused(insts);
                }

                // HACK(eddyb) the actual push to `inlined_frames` is
                // done at the very end of the loop body, to be able
                // to process any pending updates on the previous state.
                if custom_op == Some(CustomOp::PushInlinedCallFrame) {
                    inlined_frames.push(Frame {
                        call_dbg: mem::take(&mut dbg),
                        push_inst_idx: inst_idx,
                    });
                }
            }

            assert!(fusable_freshly_popped_inlined_frames.is_empty());

            block
                .instructions
                .retain(|inst| inst.class.opcode != Op::Nop);
        }
    }
}
