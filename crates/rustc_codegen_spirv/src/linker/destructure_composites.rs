//! Simplify `OpCompositeExtract` pointing to `OpCompositeConstruct`s / `OpCompositeInsert`s.
//! Such constructions arise after inlining, when using multi-argument closures
//! (and other `Fn*` trait implementations). These composites can frequently be invalid,
//! containing pointers, `OpFunctionArgument`s, etc. After simplification, components
//! will become valid targets for `OpLoad`/`OpStore`.
use super::apply_rewrite_rules;
use rspirv::dr::{Function, Instruction};
use rspirv::spirv::Op;
use rustc_data_structures::fx::FxHashMap;

pub fn destructure_composites(function: &mut Function) {
    let mut rewrite_rules = FxHashMap::default();
    let reference: FxHashMap<_, _> = function
        .all_inst_iter()
        .filter_map(|inst| match inst.class.opcode {
            Op::CompositeConstruct => Some((inst.result_id.unwrap(), inst.clone())),
            Op::CompositeInsert => {
                Some((inst.result_id.unwrap(), inst.clone()))
            }
            _ => None,
        })
        .collect();
    for inst in function.all_inst_iter_mut() {
        if inst.class.opcode == Op::CompositeExtract {
            let mut composite = inst.operands[0].unwrap_id_ref();
            let index: Vec<u32> = inst.operands[1..].iter().map(|i| i.unwrap_literal_int32()).collect();

            let origin = loop {
                if let Some(inst) = reference.get(&composite) {
                    match inst.class.opcode {
                        Op::CompositeInsert => {
                            let insert_index: Vec<u32> = inst.operands[2..].iter().map(|i| i.unwrap_literal_int32()).collect();
                            if insert_index == index {
                                break Some(inst.operands[0].unwrap_id_ref());
                            }
                            composite = inst.operands[1].unwrap_id_ref();
                        }
                        Op::CompositeConstruct => {
                            if index.len() ==1 {
                                break inst.operands.get(index[0] as usize).map(|o| o.unwrap_id_ref());
                            }
                        }
                        _ => unreachable!(),
                    }
                } else {
                    break None;
                }
            };

            if let Some(origin_id) = origin {
                rewrite_rules.insert(
                    inst.result_id.unwrap(),
                    rewrite_rules.get(&origin_id).map_or(origin_id, |id| *id),
                );
                *inst = Instruction::new(Op::Nop, None, None, vec![]);
                continue;
            }
        }
    }

    // Transitive closure computation
    let mut closed_rewrite_rules = rewrite_rules.clone();
    for (_, value) in closed_rewrite_rules.iter_mut() {
        while let Some(next) = rewrite_rules.get(value) {
            *value = *next;
        }
    }

    // Remove instructions replaced by NOPs, as well as unused composite values.
    for block in function.blocks.iter_mut() {
        block
            .instructions
            .retain(|inst| inst.class.opcode != Op::Nop);
    }
    apply_rewrite_rules(&closed_rewrite_rules, &mut function.blocks);
}
