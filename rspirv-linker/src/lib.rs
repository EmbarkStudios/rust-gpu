#[cfg(test)]
mod test;

mod capability_computation;
mod dce;
mod def_analyzer;
mod duplicates;
mod import_export_link;
mod inline;
mod simple_passes;
mod ty;
mod zombies;

use def_analyzer::DefAnalyzer;
use rspirv::binary::Consumer;
use rspirv::dr::{Instruction, Loader, Module, ModuleHeader, Operand};
use rspirv::spirv::{Op, Word};
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
    #[error("unknown data store error")]
    Unknown,
}

pub type Result<T> = std::result::Result<T, LinkerError>;

pub struct Options {
    pub compact_ids: bool,
    pub dce: bool,
    pub inline: bool,
}

pub fn load(bytes: &[u8]) -> Module {
    let mut loader = Loader::new();
    rspirv::binary::parse_bytes(&bytes, &mut loader).unwrap();
    loader.module()
}

fn operand_idref(op: &Operand) -> Option<Word> {
    match *op {
        Operand::IdMemorySemantics(w) | Operand::IdScope(w) | Operand::IdRef(w) => Some(w),
        _ => None,
    }
}
fn operand_idref_mut(op: &mut Operand) -> Option<&mut Word> {
    match op {
        Operand::IdMemorySemantics(w) | Operand::IdScope(w) | Operand::IdRef(w) => Some(w),
        _ => None,
    }
}

fn print_type(defs: &DefAnalyzer, ty: &Instruction) -> String {
    format!("{}", ty::trans_aggregate_type(defs, ty).unwrap())
}

fn extract_literal_int_as_u64(op: &Operand) -> u64 {
    match op {
        Operand::LiteralInt32(v) => (*v).into(),
        Operand::LiteralInt64(v) => *v,
        _ => panic!("Unexpected literal int"),
    }
}

fn extract_literal_u32(op: &Operand) -> u32 {
    match op {
        Operand::LiteralInt32(v) => *v,
        _ => panic!("Unexpected literal u32"),
    }
}

pub fn link<T>(
    inputs: &mut [&mut Module],
    opts: &Options,
    timer: impl Fn(&'static str) -> T,
) -> Result<Module> {
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
        zombies::remove_zombies(&mut output);
    }

    if opts.inline {
        let _timer = timer("link_inline");
        inline::inline(&mut output);
    }

    {
        let _timer = timer("link_block_ordering_pass");
        for func in &mut output.functions {
            simple_passes::block_ordering_pass(func);
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

    output.debugs.push(Instruction::new(
        Op::ModuleProcessed,
        None,
        None,
        vec![Operand::LiteralString(
            "Linked by rspirv-linker".to_string(),
        )],
    ));

    // output the module
    Ok(output)
}
