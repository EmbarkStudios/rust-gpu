use rspirv::binary::Assemble;
use rspirv::dr::{Instruction, Module, Operand};
use rspirv::spirv::{Op, Word};
use rustc_data_structures::fx::{FxHashMap, FxHashSet};
use rustc_middle::bug;
use std::collections::hash_map;

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

pub fn remove_duplicate_lines(module: &mut Module) {
    for func in &mut module.functions {
        for block in &mut func.blocks {
            block.instructions.dedup_by(|a, b| {
                if a.class.opcode == Op::Line && b.class.opcode == Op::Line {
                    // dedup_by removes the *second* element in a pair of duplicates. We want to
                    // remove the *first* (so the last OpLine is kept). So, swap them! :D
                    std::mem::swap(a, b);
                    true
                } else {
                    false
                }
            });
        }
    }
}
