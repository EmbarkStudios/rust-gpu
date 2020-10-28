#[cfg(test)]
mod test;

mod capability_computation;
mod dce;
mod def_analyzer;
mod duplicates;
mod import_export_link;
mod inline;
mod mem2reg;
mod simple_passes;
mod structurizer;
mod ty;
mod zombies;

use def_analyzer::DefAnalyzer;
use rspirv::binary::Consumer;
use rspirv::dr::{Block, Instruction, Loader, Module, ModuleHeader, Operand};
use rspirv::spirv::{Op, Word};
use rustc_session::Session;
use std::collections::HashMap;
use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub enum LinkerError {
    #[error("Unresolved symbol {:?}", .0)]
    UnresolvedSymbol(String),
    #[error("Multiple exports found for {:?}", .0)]
    MultipleExports(String),
    #[error("Types mismatch for {:?}, imported with type {:?}, exported with type {:?}", .name, .import_type, .export_type)]
    TypeMismatch {
        name: String,
        import_type: String,
        export_type: String,
    },
}

pub type Result<T> = std::result::Result<T, LinkerError>;

pub struct Options {
    pub compact_ids: bool,
    pub dce: bool,
    pub inline: bool,
    pub mem2reg: bool,
    pub structurize: bool,
}

fn id(header: &mut ModuleHeader) -> Word {
    let result = header.bound;
    header.bound += 1;
    result
}

fn print_type(defs: &DefAnalyzer<'_>, ty: &Instruction) -> String {
    format!("{}", ty::trans_aggregate_type(defs, ty).unwrap())
}

fn extract_literal_int_as_u64(op: &Operand) -> u64 {
    match op {
        Operand::LiteralInt32(v) => (*v).into(),
        Operand::LiteralInt64(v) => *v,
        _ => panic!("Unexpected literal int"),
    }
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
pub fn link(sess: Option<&Session>, inputs: &mut [&mut Module], opts: &Options) -> Result<Module> {
    let timer = |n| sess.map(|s| s.timer(n));
    let mut output = {
        let _timer = timer("link_merge");
        // shift all the ids
        let mut bound = inputs[0].header.as_ref().unwrap().bound - 1;
        let version = inputs[0].header.as_ref().unwrap().version();

        for mut module in inputs.iter_mut().skip(1) {
            simple_passes::shift_ids(&mut module, bound);
            bound += module.header.as_ref().unwrap().bound - 1;
            assert_eq!(version, module.header.as_ref().unwrap().version());
        }

        // merge the binaries
        let mut loader = Loader::new();

        for module in inputs.iter() {
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

    // find import / export pairs
    {
        let _timer = timer("link_find_pairs");
        import_export_link::run(&mut output)?;
    }

    // remove duplicates (https://github.com/KhronosGroup/SPIRV-Tools/blob/e7866de4b1dc2a7e8672867caeb0bdca49f458d3/source/opt/remove_duplicates_pass.cpp)
    {
        let _timer = timer("link_remove_duplicates");
        duplicates::remove_duplicate_extensions(&mut output);
        duplicates::remove_duplicate_capablities(&mut output);
        duplicates::remove_duplicate_ext_inst_imports(&mut output);
        duplicates::remove_duplicate_types(&mut output);
        // jb-todo: strip identical OpDecoration / OpDecorationGroups
    }

    {
        let _timer = timer("link_remove_zombies");
        zombies::remove_zombies(sess, &mut output);
    }

    if opts.inline {
        let _timer = timer("link_inline");
        inline::inline(&mut output);
    }

    {
        let _timer = timer("link_block_ordering_pass_and_mem2reg");
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
        let _timer = timer("link_sort_globals");
        simple_passes::sort_globals(&mut output);
    }

    if opts.dce {
        let _timer = timer("link_dce");
        dce::dce(&mut output);
    }

    if opts.structurize {
        let _timer = timer("link_structurize");
        for func in &mut output.functions {
            structurizer::structurize(sess, output.header.as_mut().unwrap(), func);
        }
    }

    {
        let _timer = timer("link_remove_extra_capabilities");
        capability_computation::remove_extra_capabilities(&mut output);
        capability_computation::remove_extra_extensions(&mut output);
    }

    if opts.compact_ids {
        let _timer = timer("link_compact_ids");
        // compact the ids https://github.com/KhronosGroup/SPIRV-Tools/blob/e02f178a716b0c3c803ce31b9df4088596537872/source/opt/compact_ids_pass.cpp#L43
        output.header.as_mut().unwrap().bound = simple_passes::compact_ids(&mut output);
    };

    // output the module
    Ok(output)
}
