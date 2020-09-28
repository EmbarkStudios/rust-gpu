use crate::ty::trans_aggregate_type;
use crate::{operand_idref, operand_idref_mut, print_type, DefAnalyzer, LinkerError, Result};
use rspirv::spirv;
use std::collections::{HashMap, HashSet};

pub fn run(module: &mut rspirv::dr::Module) -> Result<()> {
    let (rewrite_rules, killed_parameters) = find_import_export_pairs_and_killed_params(module)?;
    kill_linkage_instructions(module, &rewrite_rules);
    import_kill_annotations_and_debug(module, &rewrite_rules, &killed_parameters);
    replace_all_uses_with(module, &rewrite_rules);
    Ok(())
}

fn find_import_export_pairs_and_killed_params(
    module: &rspirv::dr::Module,
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
            Some((id, name, spirv::LinkageType::Export)) => (id, name),
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
            Some((id, name, spirv::LinkageType::Import)) => (id, name),
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

fn get_linkage_inst(
    inst: &rspirv::dr::Instruction,
) -> Option<(spirv::Word, &str, spirv::LinkageType)> {
    if inst.class.opcode == spirv::Op::Decorate
        && inst.operands[1] == rspirv::dr::Operand::Decoration(spirv::Decoration::LinkageAttributes)
    {
        let id = match inst.operands[0] {
            rspirv::dr::Operand::IdRef(i) => i,
            _ => panic!("Expected IdRef"),
        };

        let name = match &inst.operands[2] {
            rspirv::dr::Operand::LiteralString(s) => s,
            _ => panic!("Expected LiteralString"),
        };

        let linkage_ty = match inst.operands[3] {
            rspirv::dr::Operand::LinkageType(t) => t,
            _ => panic!("Expected LinkageType"),
        };
        Some((id, name, linkage_ty))
    } else {
        None
    }
}

fn get_type_for_link(defs: &DefAnalyzer, id: spirv::Word) -> spirv::Word {
    let def_inst = defs
        .def(id)
        .unwrap_or_else(|| panic!("Need a matching op for ID {}", id));

    match def_inst.class.opcode {
        spirv::Op::Variable => def_inst.result_type.unwrap(),
        // Note: the result_type of OpFunction is the return type, not the function type. The
        // function type is in operands[1].
        spirv::Op::Function => {
            if let rspirv::dr::Operand::IdRef(id) = def_inst.operands[1] {
                id
            } else {
                panic!("Expected IdRef");
            }
        }
        _ => panic!("Unexpected op"),
    }
}

fn fn_parameters<'a>(
    module: &'a rspirv::dr::Module,
    defs: &DefAnalyzer,
    id: spirv::Word,
) -> impl IntoIterator<Item = spirv::Word> + 'a {
    let def_inst = defs
        .def(id)
        .unwrap_or_else(|| panic!("Need a matching op for ID {}", id));

    match def_inst.class.opcode {
        spirv::Op::Variable => &[],
        spirv::Op::Function => {
            &module
                .functions
                .iter()
                .find(|f| f.def.as_ref().unwrap().result_id == def_inst.result_id)
                .unwrap()
                .parameters as &[rspirv::dr::Instruction]
        }
        _ => panic!("Unexpected op"),
    }
    .iter()
    .map(|p| p.result_id.unwrap())
}

fn check_tys_equal(
    defs: &DefAnalyzer,
    name: &str,
    import_type_id: spirv::Word,
    export_type_id: spirv::Word,
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

fn replace_all_uses_with(module: &mut rspirv::dr::Module, rules: &HashMap<u32, u32>) {
    module.all_inst_iter_mut().for_each(|inst| {
        if let Some(ref mut result_type) = &mut inst.result_type {
            if let Some(&rewrite) = rules.get(result_type) {
                *result_type = rewrite;
            }
        }

        inst.operands.iter_mut().for_each(|op| {
            if let Some(w) = operand_idref_mut(op) {
                if let Some(&rewrite) = rules.get(w) {
                    *w = rewrite;
                }
            }
        })
    });
}

fn kill_linkage_instructions(module: &mut rspirv::dr::Module, rewrite_rules: &HashMap<u32, u32>) {
    // drop imported functions
    for &id in rewrite_rules.keys() {
        module
            .functions
            .retain(|f| id != f.def.as_ref().unwrap().result_id.unwrap());
    }

    // drop imported variables
    for &id in rewrite_rules.keys() {
        module
            .types_global_values
            .retain(|v| v.result_id.map_or(true, |v| v != id));
    }

    module.annotations.retain(|inst| {
        inst.class.opcode != spirv::Op::Decorate
            || inst.operands[1]
                != rspirv::dr::Operand::Decoration(spirv::Decoration::LinkageAttributes)
    });

    // drop OpCapability Linkage
    module.capabilities.retain(|inst| {
        inst.class.opcode != spirv::Op::Capability
            || inst.operands[0] != rspirv::dr::Operand::Capability(spirv::Capability::Linkage)
    })
}

fn import_kill_annotations_and_debug(
    module: &mut rspirv::dr::Module,
    rewrite_rules: &HashMap<u32, u32>,
    killed_parameters: &HashSet<u32>,
) {
    module.annotations.retain(|inst| {
        inst.operands.is_empty()
            || operand_idref(&inst.operands[0]).map_or(true, |id| {
                !rewrite_rules.contains_key(&id) && !killed_parameters.contains(&id)
            })
    });
    module.debugs.retain(|inst| {
        inst.operands.is_empty()
            || operand_idref(&inst.operands[0]).map_or(true, |id| {
                !rewrite_rules.contains_key(&id) && !killed_parameters.contains(&id)
            })
    });
    // need to remove OpGroupDecorate members that mention this id
    for inst in &mut module.annotations {
        if inst.class.opcode == spirv::Op::GroupDecorate {
            inst.operands.retain(|op| {
                operand_idref(op).map_or(true, |id| {
                    !rewrite_rules.contains_key(&id) && !killed_parameters.contains(&id)
                })
            })
        }
    }
}
