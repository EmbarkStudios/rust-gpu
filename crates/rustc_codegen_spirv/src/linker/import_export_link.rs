use super::Result;
use rspirv::dr::{Instruction, Module};
use rspirv::spirv::{Capability, Decoration, LinkageType, Op, Word};
use rustc_data_structures::fx::{FxHashMap, FxHashSet};
use rustc_errors::ErrorReported;
use rustc_session::Session;

pub fn run(sess: &Session, module: &mut Module) -> Result<()> {
    let (rewrite_rules, killed_parameters) =
        find_import_export_pairs_and_killed_params(sess, module)?;
    kill_linkage_instructions(module, &rewrite_rules);
    import_kill_annotations_and_debug(module, &rewrite_rules, &killed_parameters);
    replace_all_uses_with(module, &rewrite_rules);
    Ok(())
}

fn find_import_export_pairs_and_killed_params(
    sess: &Session,
    module: &Module,
) -> Result<(FxHashMap<u32, u32>, FxHashSet<u32>)> {
    let type_map = get_type_map(module);
    let fn_parameters = fn_parameters(module);

    // Map from name -> (definition, type)
    let mut exports = FxHashMap::default();
    // Rules to rewrite the module with
    let mut rewrite_rules = FxHashMap::default();
    let mut killed_parameters = FxHashSet::default();

    // First, collect all the exports.
    for annotation in &module.annotations {
        let (id, name) = match get_linkage_inst(annotation) {
            Some((id, name, LinkageType::Export)) => (id, name),
            _ => continue,
        };
        let type_id = *type_map.get(&id).expect("Unexpected op");
        if exports.insert(name, (id, type_id)).is_some() {
            sess.err(&format!("Multiple exports found for {:?}", name));
            return Err(ErrorReported);
        }
    }
    let mut has_err = false;
    // Then, collect all the imports, and create the rewrite rules.
    for annotation in &module.annotations {
        let (import_id, name) = match get_linkage_inst(annotation) {
            Some((id, name, LinkageType::Import)) => (id, name),
            _ => continue,
        };
        let (export_id, export_type) = match exports.get(name) {
            None => {
                sess.err(&format!("Unresolved symbol {:?}", name));
                has_err = true;
                continue;
            }
            Some(&x) => x,
        };
        let import_type = *type_map.get(&import_id).expect("Unexpected op");
        // Make sure the import/export pair has the same type.
        check_tys_equal(sess, name, import_type, export_type)?;
        rewrite_rules.insert(import_id, export_id);
        if let Some(params) = fn_parameters.get(&import_id) {
            for &param in params {
                killed_parameters.insert(param);
            }
        }
    }
    if has_err {
        return Err(ErrorReported);
    }

    Ok((rewrite_rules, killed_parameters))
}

fn get_linkage_inst(inst: &Instruction) -> Option<(Word, &str, LinkageType)> {
    if inst.class.opcode == Op::Decorate
        && inst.operands[1].unwrap_decoration() == Decoration::LinkageAttributes
    {
        let id = inst.operands[0].unwrap_id_ref();
        let name = inst.operands[2].unwrap_literal_string();
        let linkage_ty = inst.operands[3].unwrap_linkage_type();
        Some((id, name, linkage_ty))
    } else {
        None
    }
}

fn get_type_map(module: &Module) -> FxHashMap<Word, Word> {
    let vars = module
        .types_global_values
        .iter()
        .filter(|i| i.class.opcode == Op::Variable)
        .map(|i| (i.result_id.unwrap(), i.result_type.unwrap()));
    let fns = module.functions.iter().map(|i| {
        let d = i.def.as_ref().unwrap();
        (d.result_id.unwrap(), d.operands[1].unwrap_id_ref())
    });
    vars.chain(fns).collect()
}

fn fn_parameters(module: &Module) -> FxHashMap<Word, Vec<Word>> {
    module
        .functions
        .iter()
        .map(|f| {
            let params = f.parameters.iter().map(|i| i.result_id.unwrap()).collect();
            (f.def_id().unwrap(), params)
        })
        .collect()
}

fn check_tys_equal(sess: &Session, name: &str, import_type: Word, export_type: Word) -> Result<()> {
    if import_type == export_type {
        Ok(())
    } else {
        sess.err(&format!("Types mismatch for {:?}", name));
        Err(ErrorReported)
    }
}

fn replace_all_uses_with(module: &mut Module, rules: &FxHashMap<u32, u32>) {
    module.all_inst_iter_mut().for_each(|inst| {
        if let Some(ref mut result_type) = &mut inst.result_type {
            if let Some(&rewrite) = rules.get(result_type) {
                *result_type = rewrite;
            }
        }

        inst.operands.iter_mut().for_each(|op| {
            if let Some(w) = op.id_ref_any_mut() {
                if let Some(&rewrite) = rules.get(w) {
                    *w = rewrite;
                }
            }
        })
    });
}

fn kill_linkage_instructions(module: &mut Module, rewrite_rules: &FxHashMap<u32, u32>) {
    // drop imported functions
    module
        .functions
        .retain(|f| !rewrite_rules.contains_key(&f.def_id().unwrap()));

    // drop imported variables
    module.types_global_values.retain(|v| {
        v.result_id
            .map_or(true, |v| !rewrite_rules.contains_key(&v))
    });

    module.annotations.retain(|inst| {
        inst.class.opcode != Op::Decorate
            || inst.operands[1].unwrap_decoration() != Decoration::LinkageAttributes
    });

    // drop OpCapability Linkage
    module.capabilities.retain(|inst| {
        inst.class.opcode != Op::Capability
            || inst.operands[0].unwrap_capability() != Capability::Linkage
    })
}

fn import_kill_annotations_and_debug(
    module: &mut Module,
    rewrite_rules: &FxHashMap<u32, u32>,
    killed_parameters: &FxHashSet<u32>,
) {
    module.annotations.retain(|inst| {
        inst.operands.is_empty()
            || inst.operands[0].id_ref_any().map_or(true, |id| {
                !rewrite_rules.contains_key(&id) && !killed_parameters.contains(&id)
            })
    });
    module.debugs.retain(|inst| {
        inst.operands.is_empty()
            || inst.operands[0].id_ref_any().map_or(true, |id| {
                !rewrite_rules.contains_key(&id) && !killed_parameters.contains(&id)
            })
    });
    // need to remove OpGroupDecorate members that mention this id
    for inst in &mut module.annotations {
        if inst.class.opcode == Op::GroupDecorate {
            inst.operands.retain(|op| {
                op.id_ref_any().map_or(true, |id| {
                    !rewrite_rules.contains_key(&id) && !killed_parameters.contains(&id)
                })
            })
        }
    }
}
