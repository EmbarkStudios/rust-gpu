use super::Result;
use crate::decorations::{CustomDecoration, ZombieDecoration};
use rspirv::dr::{Instruction, Module};
use rspirv::spirv::{Capability, Decoration, LinkageType, Op, Word};
use rustc_data_structures::fx::{FxHashMap, FxHashSet};
use rustc_session::Session;

pub fn run(opts: &super::Options, sess: &Session, module: &mut Module) -> Result<()> {
    let (rewrite_rules, killed_parameters) =
        find_import_export_pairs_and_killed_params(sess, module)?;
    kill_linkage_instructions(opts, module, &rewrite_rules);
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

    // HACK(eddyb) collect all zombies, and anything that transitively mentions
    // them (or is "infected"), to ignore them when doing type checks, as they
    // will either be removed later, or become an error of their own.
    let mut zombie_infected: FxHashSet<Word> = ZombieDecoration::decode_all(module)
        .map(|(z, _)| z)
        .collect();
    for inst in module.global_inst_iter() {
        if let Some(result_id) = inst.result_id {
            if !zombie_infected.contains(&result_id) {
                let mut id_operands = inst.operands.iter().filter_map(|o| o.id_ref_any());
                // NOTE(eddyb) this takes advantage of the fact that the module
                // is ordered def-before-use (with the minor exception of forward
                // references for recursive data, which are not fully supported),
                // to be able to propagate "zombie infection" in one pass.
                if id_operands.any(|id| zombie_infected.contains(&id)) {
                    zombie_infected.insert(result_id);
                }
            }
        }
    }

    // First, collect all the exports.
    for annotation in &module.annotations {
        let (id, name) = match get_linkage_inst(annotation) {
            Some((id, name, LinkageType::Export)) => (id, name),
            _ => continue,
        };
        let type_id = *type_map.get(&id).expect("Unexpected op");
        if exports.insert(name, (id, type_id)).is_some() {
            return Err(sess.err(format!("Multiple exports found for {:?}", name)));
        }
    }
    let mut any_err = None;
    // Then, collect all the imports, and create the rewrite rules.
    for annotation in &module.annotations {
        let (import_id, name) = match get_linkage_inst(annotation) {
            Some((id, name, LinkageType::Import)) => (id, name),
            _ => continue,
        };
        let (export_id, export_type) = match exports.get(name) {
            None => {
                any_err = Some(sess.err(format!("Unresolved symbol {:?}", name)));
                continue;
            }
            Some(&x) => x,
        };
        let import_type = *type_map.get(&import_id).expect("Unexpected op");
        // Make sure the import/export pair has the same type.
        check_tys_equal(
            sess,
            module,
            name,
            import_type,
            export_type,
            &zombie_infected,
        )?;
        rewrite_rules.insert(import_id, export_id);
        if let Some(params) = fn_parameters.get(&import_id) {
            for &param in params {
                killed_parameters.insert(param);
            }
        }
    }

    match any_err {
        Some(err) => Err(err),
        None => Ok((rewrite_rules, killed_parameters)),
    }
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

fn check_tys_equal(
    sess: &Session,
    module: &Module,
    name: &str,
    import_type: Word,
    export_type: Word,
    zombie_infected: &FxHashSet<Word>,
) -> Result<()> {
    let allowed = import_type == export_type || {
        // HACK(eddyb) zombies can cause types to differ in definition, due to
        // requiring multiple different instances (usually different `Span`s),
        // so we ignore them, as `find_import_export_pairs_and_killed_params`'s
        // own comment explains (zombies will cause errors or be removed, *later*).
        zombie_infected.contains(&import_type) && zombie_infected.contains(&export_type)
    };

    if allowed {
        Ok(())
    } else {
        // We have an error. It's okay to do something really slow now to report the error.
        use std::fmt::Write;
        let ty_defs = module
            .types_global_values
            .iter()
            .filter_map(|inst| Some((inst.result_id?, inst)))
            .collect();
        fn format_ty(ty_defs: &FxHashMap<Word, &Instruction>, ty: Word, buf: &mut String) {
            match ty_defs.get(&ty) {
                Some(def) => {
                    write!(buf, "({}", def.class.opname).unwrap();
                    if let Some(result_type) = def.result_type {
                        write!(buf, " {}", result_type).unwrap();
                    }
                    for op in &def.operands {
                        if let Some(id) = op.id_ref_any() {
                            write!(buf, " ").unwrap();
                            format_ty(ty_defs, id, buf);
                        }
                    }
                    write!(buf, ")").unwrap();
                }
                None => write!(buf, "{}", ty).unwrap(),
            }
        }
        fn format_ty_(ty_defs: &FxHashMap<Word, &Instruction>, ty: Word) -> String {
            let mut result = String::new();
            format_ty(ty_defs, ty, &mut result);
            result
        }
        Err(sess
            .struct_err(&format!("Types mismatch for {:?}", name))
            .note(&format!(
                "import type: {}",
                format_ty_(&ty_defs, import_type)
            ))
            .note(&format!(
                "export type: {}",
                format_ty_(&ty_defs, export_type)
            ))
            .emit())
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
        });
    });
}

fn kill_linkage_instructions(
    opts: &super::Options,
    module: &mut Module,
    rewrite_rules: &FxHashMap<u32, u32>,
) {
    // drop imported functions
    module
        .functions
        .retain(|f| !rewrite_rules.contains_key(&f.def_id().unwrap()));

    // drop imported variables
    module.types_global_values.retain(|v| {
        v.result_id
            .map_or(true, |v| !rewrite_rules.contains_key(&v))
    });

    // NOTE(eddyb) `Options`'s `keep_link_export`s field requests that `Export`s
    // are left in (primarily for unit testing - see also its doc comment).
    let mut kept_any_linkage_decorations = false;
    module.annotations.retain(|inst| {
        !(inst.class.opcode == Op::Decorate
            && inst.operands[1].unwrap_decoration() == Decoration::LinkageAttributes
            && match inst.operands[3].unwrap_linkage_type() {
                LinkageType::Export if opts.keep_link_exports => {
                    kept_any_linkage_decorations = true;
                    false
                }
                _ => true,
            })
    });
    if !kept_any_linkage_decorations {
        // drop OpCapability Linkage
        module.capabilities.retain(|inst| {
            inst.class.opcode != Op::Capability
                || inst.operands[0].unwrap_capability() != Capability::Linkage
        });
    }
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
    module.debug_names.retain(|inst| {
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
            });
        }
    }
}
