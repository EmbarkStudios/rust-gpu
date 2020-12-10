#[cfg(test)]
mod test;

mod capability_computation;
mod dce;
mod duplicates;
mod import_export_link;
mod inline;
mod mem2reg;
mod new_structurizer;
mod simple_passes;
mod structurizer;
mod zombies;

use crate::decorations::{CustomDecoration, UnrollLoopsDecoration};
use rspirv::binary::Consumer;
use rspirv::dr::{Block, Instruction, Loader, Module, ModuleHeader};
use rspirv::spirv::{Op, Word};
use rustc_errors::ErrorReported;
use rustc_session::Session;
use std::collections::HashMap;

pub type Result<T> = std::result::Result<T, ErrorReported>;

pub struct Options {
    pub compact_ids: bool,
    pub dce: bool,
    pub inline: bool,
    pub mem2reg: bool,
    pub structurize: bool,
    pub use_new_structurizer: bool,
}

fn id(header: &mut ModuleHeader) -> Word {
    let result = header.bound;
    header.bound += 1;
    result
}

fn apply_rewrite_rules(rewrite_rules: &HashMap<Word, Word>, blocks: &mut [Block]) {
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
        })
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

// Sess needs to be Option because linker tests call this method, and linker tests can't synthesize
// a test Session (not sure how to do that).
pub fn link(sess: &Session, mut inputs: Vec<Module>, opts: &Options) -> Result<Module> {
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
        output.header = Some(header);
        output
    };

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

    {
        let _timer = sess.timer("link_remove_zombies");
        zombies::remove_zombies(sess, &mut output);
    }

    if opts.inline {
        let _timer = sess.timer("link_inline");
        inline::inline(&mut output);
    }

    if opts.dce {
        let _timer = sess.timer("link_dce");
        dce::dce(&mut output);
    }

    let unroll_loops_decorations = UnrollLoopsDecoration::decode_all(&output)
        .map(|(id, unroll_loops)| (id, unroll_loops.deserialize()))
        .collect::<HashMap<_, _>>();
    UnrollLoopsDecoration::remove_all(&mut output);

    let mut output = if opts.structurize {
        let _timer = sess.timer("link_structurize");
        if opts.use_new_structurizer {
            new_structurizer::structurize(output, unroll_loops_decorations)
        } else {
            structurizer::structurize(sess, output, unroll_loops_decorations)
        }
    } else {
        output
    };

    {
        let _timer = sess.timer("link_block_ordering_pass_and_mem2reg");
        let mut pointer_to_pointee = HashMap::new();
        let mut constants = HashMap::new();
        if opts.mem2reg {
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
        }
        for func in &mut output.functions {
            simple_passes::block_ordering_pass(func);
            if opts.mem2reg {
                // Note: mem2reg requires functions to be in RPO order (i.e. block_ordering_pass)
                mem2reg::mem2reg(
                    output.header.as_mut().unwrap(),
                    &mut output.types_global_values,
                    &pointer_to_pointee,
                    &constants,
                    func,
                );
            }
        }
    }
    {
        let _timer = sess.timer("link_sort_globals");
        simple_passes::sort_globals(&mut output);
    }

    {
        let _timer = sess.timer("link_remove_extra_capabilities");
        capability_computation::remove_extra_capabilities(&mut output);
        capability_computation::remove_extra_extensions(&mut output);
    }

    if opts.compact_ids {
        let _timer = sess.timer("link_compact_ids");
        // compact the ids https://github.com/KhronosGroup/SPIRV-Tools/blob/e02f178a716b0c3c803ce31b9df4088596537872/source/opt/compact_ids_pass.cpp#L43
        output.header.as_mut().unwrap().bound = simple_passes::compact_ids(&mut output);
    };

    // output the module
    Ok(output)
}
