use super::ty::trans_aggregate_type;
use super::{print_type, DefAnalyzer, LinkerError, Result};
use rspirv::dr::{Instruction, Module};
use rspirv::spirv::{Capability, Decoration, LinkageType, Op, Word};
use std::collections::{HashMap, HashSet};

pub fn run(module: &mut Module) -> Result<()> {
    let (rewrite_rules, killed_parameters) = find_import_export_pairs_and_killed_params(module)?;
    kill_linkage_instructions(module, &rewrite_rules);
    import_kill_annotations_and_debug(module, &rewrite_rules, &killed_parameters);
    replace_all_uses_with(module, &rewrite_rules);
    Ok(())
}

fn find_import_export_pairs_and_killed_params(
    module: &Module,
) -> Result<(HashMap<u32, u32>, HashSet<u32>)> {
    let defs = DefAnalyzer::new(module);

    // Map from name -> (definition, type)
    let mut exports = HashMap::new();
    // Rules to rewrite the module with
    let mut rewrite_rules = HashMap::new();
    let mut killed_parameters = HashSet::new();

    // First, collect all the exports.
    for annotation in &module.annotations {
        let (id, name) = match get_linkage_inst(annotation) {
            Some((id, name, LinkageType::Export)) => (id, name),
            _ => continue,
        };
        let type_id = get_type_for_link(&defs, id);
        if exports.insert(name, (id, type_id)).is_some() {
            return Err(LinkerError::MultipleExports(name.to_string()));
        }
    }
    // Then, collect all the imports, and create the rewrite rules.
    for annotation in &module.annotations {
        let (import_id, name) = match get_linkage_inst(annotation) {
            Some((id, name, LinkageType::Import)) => (id, name),
            _ => continue,
        };
        let (export_id, export_type) = match exports.get(name) {
            None => {
                return Err(LinkerError::UnresolvedSymbol(name.to_string()));
            }
            Some(&x) => x,
        };
        let import_type = get_type_for_link(&defs, import_id);
        // Make sure the import/export pair has the same type.
        check_tys_equal(&defs, name, import_type, export_type)?;
        rewrite_rules.insert(import_id, export_id);
        for param in fn_parameters(module, &defs, import_id) {
            killed_parameters.insert(param);
        }
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

fn get_type_for_link(defs: &DefAnalyzer<'_>, id: Word) -> Word {
    let def_inst = defs
        .def(id)
        .unwrap_or_else(|| panic!("Need a matching op for ID {}", id));

    match def_inst.class.opcode {
        Op::Variable => def_inst.result_type.unwrap(),
        // Note: the result_type of OpFunction is the return type, not the function type. The
        // function type is in operands[1].
        Op::Function => def_inst.operands[1].unwrap_id_ref(),
        _ => panic!("Unexpected op"),
    }
}

fn fn_parameters<'a>(
    module: &'a Module,
    defs: &DefAnalyzer<'_>,
    id: Word,
) -> impl IntoIterator<Item = Word> + 'a {
    let def_inst = defs
        .def(id)
        .unwrap_or_else(|| panic!("Need a matching op for ID {}", id));

    match def_inst.class.opcode {
        Op::Variable => &[],
        Op::Function => {
            &module
                .functions
                .iter()
                .find(|f| f.def.as_ref().unwrap().result_id == def_inst.result_id)
                .unwrap()
                .parameters as &[Instruction]
        }
        _ => panic!("Unexpected op"),
    }
    .iter()
    .map(|p| p.result_id.unwrap())
}

fn check_tys_equal(
    defs: &DefAnalyzer<'_>,
    name: &str,
    import_type_id: Word,
    export_type_id: Word,
) -> Result<()> {
    let import_type = defs.def(import_type_id).unwrap();
    let export_type = defs.def(export_type_id).unwrap();

    let imp = trans_aggregate_type(defs, import_type);
    let exp = trans_aggregate_type(defs, export_type);

    if imp == exp {
        Ok(())
    } else {
        Err(LinkerError::TypeMismatch {
            name: name.to_string(),
            import_type: print_type(defs, import_type),
            export_type: print_type(defs, export_type),
        })
    }
}

fn replace_all_uses_with(module: &mut Module, rules: &HashMap<u32, u32>) {
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

fn kill_linkage_instructions(module: &mut Module, rewrite_rules: &HashMap<u32, u32>) {
    // drop imported functions
    module
        .functions
        .retain(|f| !rewrite_rules.contains_key(&f.def.as_ref().unwrap().result_id.unwrap()));

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
    rewrite_rules: &HashMap<u32, u32>,
    killed_parameters: &HashSet<u32>,
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
