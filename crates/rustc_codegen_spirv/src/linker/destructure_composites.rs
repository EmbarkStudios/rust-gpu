//! Simplify OpCompositeExtract pointing to OpCompositeConstructs / OpCompositeInserts.
//! Such constructions arise after inlining, when using multi-argument closures
//! (and other Fn* trait implementations). These composites can frequently be invalid,
//! containing pointers, OpFunctionArguments, etc. After simplification, components
//! will become valid targets for OpLoad/OpStore.
use super::apply_rewrite_rules;
use rspirv::dr::{Module, Instruction, Operand};
use rspirv::spirv::Op;
use rustc_data_structures::fx::{FxHashMap, FxHashSet};

pub fn destructure_composites(module: &mut Module) {
    for function in module.functions.iter_mut() {
        let mut rewrite_rules = FxHashMap::default();
        let reference: FxHashMap<_, _> = function.all_inst_iter()
            .filter_map(|inst| {
                match inst.class.opcode {
                    Op::CompositeConstruct => Some((inst.result_id.unwrap(), inst.clone())),
                    Op::CompositeInsert if inst.operands.len() == 3 => Some((inst.result_id.unwrap(), inst.clone())),
                    _ => None
                }
            }).collect();
        let mut unused: FxHashSet<_> = reference.keys().map(|x| *x).collect();
        for inst in function.all_inst_iter_mut() {
            if inst.class.opcode == Op::CompositeExtract && inst.operands.len() == 2 {
                let mut composite = inst.operands[0].unwrap_id_ref();
                let index = inst.operands[1].unwrap_literal_int32();

                let origin = loop {
                    if let Some(inst) = reference.get(&composite) {
                        match inst.class.opcode {
                            Op::CompositeInsert => {
                                let insert_index = inst.operands[2].unwrap_literal_int32();
                                if insert_index == index {
                                    break Some(inst.operands[0].unwrap_id_ref());
                                }
                                composite = inst.operands[1].unwrap_id_ref();
                            }
                            Op::CompositeConstruct => {
                                break inst.operands.get(index as usize).map(|o| o.unwrap_id_ref());
                            }
                            _ => unreachable!()
                        }
                    }
                    else {
                        break None;
                    }
                };

                if let Some(origin_id) = origin {
                    rewrite_rules.insert(inst.result_id.unwrap(),
                                         rewrite_rules.get(&origin_id).map_or(origin_id, |id| *id));
                    *inst = Instruction::new(Op::Nop, None, None, vec![]);
                    continue;
                }
            }

            // If the instruction wasn't replaced, modify the unused set
            if inst.class.opcode != Op::CompositeInsert {
                for op in inst.operands.iter() {
                    if let Operand::IdRef(id_ref) = op {
                        unused.remove(&id_ref);
                    }
                }
            }
        }

        // Apply transitive used to OpComposite* referenced by OpCompositeInserts
        let mut changed = true;
        while changed {
            changed = false;
            for (id, inst) in reference.iter() {
                if inst.class.opcode == Op::CompositeInsert && !unused.contains(id) {
                    changed |= unused.remove(&inst.operands[1].unwrap_id_ref());
                }
            }
        }

        // Remove instructions replaced by NOPs, as well as unused composite values.
        for block in function.blocks.iter_mut() {
            block.instructions.retain(|inst|
                inst.class.opcode != Op::Nop && inst.result_id.map_or(true,
                                                                      |res_id| !unused.contains(&res_id)));
        }
        apply_rewrite_rules(&rewrite_rules, &mut function.blocks);
    }
}