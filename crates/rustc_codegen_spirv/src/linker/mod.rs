#[cfg(test)]
mod test;

mod dce;
mod destructure_composites;
mod duplicates;
mod import_export_link;
mod inline;
mod ipo;
mod mem2reg;
mod param_weakening;
mod peephole_opts;
mod simple_passes;
mod specializer;
mod structurizer;
mod zombies;

use crate::codegen_cx::SpirvMetadata;
use crate::decorations::{CustomDecoration, UnrollLoopsDecoration};
use rspirv::binary::{Assemble, Consumer};
use rspirv::dr::{Block, Instruction, Loader, Module, ModuleHeader, Operand};
use rspirv::spirv::{Op, StorageClass, Word};
use rustc_data_structures::fx::{FxHashMap, FxHashSet};
use rustc_errors::ErrorReported;
use rustc_session::Session;

pub type Result<T> = std::result::Result<T, ErrorReported>;

pub struct Options {
    pub compact_ids: bool,
    pub dce: bool,
    pub structurize: bool,
    pub emit_multiple_modules: bool,
    pub spirv_metadata: SpirvMetadata,
}

pub enum LinkResult {
    SingleModule(Module),
    MultipleModules(FxHashMap<String, Module>),
}

fn id(header: &mut ModuleHeader) -> Word {
    let result = header.bound;
    header.bound += 1;
    result
}

fn apply_rewrite_rules(rewrite_rules: &FxHashMap<Word, Word>, blocks: &mut [Block]) {
    let apply = |inst: &mut Instruction| {
        if let Some(ref mut id) = &mut inst.result_id {
            if let Some(&rewrite) = rewrite_rules.get(id) {
                *id = rewrite;
            }
        }

        if let Some(ref mut id) = &mut inst.result_type {
            if let Some(&rewrite) = rewrite_rules.get(id) {
                *id = rewrite;
            }
        }

        inst.operands.iter_mut().for_each(|op| {
            if let Some(id) = op.id_ref_any_mut() {
                if let Some(&rewrite) = rewrite_rules.get(id) {
                    *id = rewrite;
                }
            }
        });
    };
    for block in blocks {
        for inst in &mut block.label {
            apply(inst);
        }
        for inst in &mut block.instructions {
            apply(inst);
        }
    }
}

pub fn link(sess: &Session, mut inputs: Vec<Module>, opts: &Options) -> Result<LinkResult> {
    let mut output = {
        let _timer = sess.timer("link_merge");
        // shift all the ids
        let mut bound = inputs[0].header.as_ref().unwrap().bound - 1;
        let version = inputs[0].header.as_ref().unwrap().version();

        for mut module in inputs.iter_mut().skip(1) {
            simple_passes::shift_ids(&mut module, bound);
            bound += module.header.as_ref().unwrap().bound - 1;
            let this_version = module.header.as_ref().unwrap().version();
            if version != this_version {
                sess.fatal(&format!(
                    "cannot link two modules with different SPIR-V versions: v{}.{} and v{}.{}",
                    version.0, version.1, this_version.0, this_version.1
                ))
            }
        }

        // merge the binaries
        let mut loader = Loader::new();

        for module in inputs {
            module.all_inst_iter().for_each(|inst| {
                loader.consume_instruction(inst.clone());
            });
        }

        let mut output = loader.module();
        let mut header = ModuleHeader::new(bound + 1);
        header.set_version(version.0, version.1);
        header.generator = 0x001B_0000;
        output.header = Some(header);
        output
    };

    if let Ok(ref path) = std::env::var("DUMP_POST_MERGE") {
        std::fs::write(path, spirv_tools::binary::from_binary(&output.assemble())).unwrap();
    }

    // remove duplicates (https://github.com/KhronosGroup/SPIRV-Tools/blob/e7866de4b1dc2a7e8672867caeb0bdca49f458d3/source/opt/remove_duplicates_pass.cpp)
    {
        let _timer = sess.timer("link_remove_duplicates");
        duplicates::remove_duplicate_extensions(&mut output);
        duplicates::remove_duplicate_capablities(&mut output);
        duplicates::remove_duplicate_ext_inst_imports(&mut output);
        duplicates::remove_duplicate_types(&mut output);
        // jb-todo: strip identical OpDecoration / OpDecorationGroups
    }

    // find import / export pairs
    {
        let _timer = sess.timer("link_find_pairs");
        import_export_link::run(sess, &mut output)?;
    }

    // HACK(eddyb) this has to run before the `remove_zombies` pass, so that any
    // zombies that are passed as call arguments, but eventually unused, won't
    // be (incorrectly) considered used.
    {
        let _timer = sess.timer("link_remove_unused_params");
        output = param_weakening::remove_unused_params(output);
    }

    {
        let _timer = sess.timer("link_remove_zombies");
        zombies::remove_zombies(sess, &mut output);
    }

    {
        let _timer = sess.timer("specialize_generic_storage_class");
        // HACK(eddyb) `specializer` requires functions' blocks to be in RPO order
        // (i.e. `block_ordering_pass`) - this could be relaxed by using RPO visit
        // inside `specializer`, but this is easier.
        for func in &mut output.functions {
            simple_passes::block_ordering_pass(func);
        }
        output = specializer::specialize(
            output,
            specializer::SimpleSpecialization {
                specialize_operand: |operand| {
                    matches!(operand, Operand::StorageClass(StorageClass::Generic))
                },

                // NOTE(eddyb) this can be anything that is guaranteed to pass
                // validation - there are no constraints so this is either some
                // unused pointer, or perhaps one created using `OpConstantNull`
                // and simply never mixed with pointers that have a storage class.
                // It would be nice to use `Generic` itself here so that we leave
                // some kind of indication of it being unconstrained, but `Generic`
                // requires additional capabilities, so we use `Function` instead.
                // TODO(eddyb) investigate whether this can end up in a pointer
                // type that's the value of a module-scoped variable, and whether
                // `Function` is actually invalid! (may need `Private`)
                concrete_fallback: Operand::StorageClass(StorageClass::Function),
            },
        );
    }

    {
        let _timer = sess.timer("link_inline");
        inline::inline(&mut output);
    }

    if opts.dce {
        let _timer = sess.timer("link_dce");
        dce::dce(&mut output);
    }

    let unroll_loops_decorations = UnrollLoopsDecoration::decode_all(&output)
        .map(|(id, _)| id)
        .collect::<FxHashSet<_>>();
    UnrollLoopsDecoration::remove_all(&mut output);

    let mut output = if opts.structurize {
        let _timer = sess.timer("link_structurize");
        structurizer::structurize(output, unroll_loops_decorations)
    } else {
        output
    };

    {
        let _timer = sess.timer("link_block_ordering_pass_and_mem2reg");
        let mut pointer_to_pointee = FxHashMap::default();
        let mut constants = FxHashMap::default();
        let mut u32 = None;
        for inst in &output.types_global_values {
            match inst.class.opcode {
                Op::TypePointer => {
                    pointer_to_pointee
                        .insert(inst.result_id.unwrap(), inst.operands[1].unwrap_id_ref());
                }
                Op::TypeInt
                    if inst.operands[0].unwrap_literal_int32() == 32
                        && inst.operands[1].unwrap_literal_int32() == 0 =>
                {
                    assert!(u32.is_none());
                    u32 = Some(inst.result_id.unwrap());
                }
                Op::Constant if u32.is_some() && inst.result_type == u32 => {
                    let value = inst.operands[0].unwrap_literal_int32();
                    constants.insert(inst.result_id.unwrap(), value);
                }
                _ => {}
            }
        }
        for func in &mut output.functions {
            simple_passes::block_ordering_pass(func);
            // Note: mem2reg requires functions to be in RPO order (i.e. block_ordering_pass)
            mem2reg::mem2reg(
                output.header.as_mut().unwrap(),
                &mut output.types_global_values,
                &pointer_to_pointee,
                &constants,
                func,
            );
            destructure_composites::destructure_composites(func);
        }
    }

    {
        let _timer = sess.timer("peephole_opts");
        let types = peephole_opts::collect_types(&output);
        for func in &mut output.functions {
            peephole_opts::composite_construct(&types, func);
            peephole_opts::vector_ops(output.header.as_mut().unwrap(), &types, func);
        }
    }

    if opts.spirv_metadata == SpirvMetadata::NameVariables {
        let _timer = sess.timer("link_name_variables");
        simple_passes::name_variables_pass(&mut output);
    }

    {
        let _timer = sess.timer("link_sort_globals");
        simple_passes::sort_globals(&mut output);
    }

    let mut output = if opts.emit_multiple_modules {
        let modules = output
            .entry_points
            .iter()
            .map(|entry| {
                let mut module = output.clone();
                module.entry_points.clear();
                module.entry_points.push(entry.clone());
                let name = entry.operands[2].unwrap_literal_string();
                (name.to_string(), module)
            })
            .collect();
        LinkResult::MultipleModules(modules)
    } else {
        LinkResult::SingleModule(output)
    };

    let output_module_iter: Box<dyn Iterator<Item = &mut Module>> = match output {
        LinkResult::SingleModule(ref mut m) => Box::new(std::iter::once(m)),
        LinkResult::MultipleModules(ref mut m) => Box::new(m.values_mut()),
    };
    for (i, output) in output_module_iter.enumerate() {
        if let Some(mut path) = crate::get_env_dump_dir("DUMP_POST_SPLIT") {
            path.push(format!("mod_{}.spv", i));
            std::fs::write(path, spirv_tools::binary::from_binary(&output.assemble())).unwrap();
        }
        // Run DCE again, even if emit_multiple_modules==false - the first DCE ran before
        // structurization and mem2reg (for perf reasons), and mem2reg may remove references to
        // invalid types, so we need to DCE again.
        if opts.dce {
            let _timer = sess.timer("link_dce_2");
            dce::dce(output);
        }

        {
            let _timer = sess.timer("link_remove_duplicate_lines");
            duplicates::remove_duplicate_lines(output);
        }

        if opts.compact_ids {
            let _timer = sess.timer("link_compact_ids");
            // compact the ids https://github.com/KhronosGroup/SPIRV-Tools/blob/e02f178a716b0c3c803ce31b9df4088596537872/source/opt/compact_ids_pass.cpp#L43
            output.header.as_mut().unwrap().bound = simple_passes::compact_ids(output);
        };
    }

    Ok(output)
}
