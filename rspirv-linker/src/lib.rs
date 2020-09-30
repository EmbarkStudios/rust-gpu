#[cfg(test)]
mod test;

mod def_analyzer;
mod duplicates;
mod import_export_link;
mod simple_passes;
mod ty;
mod zombies;

use def_analyzer::DefAnalyzer;
use rspirv::binary::Consumer;
use rspirv::spirv;
use std::env;
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

pub fn load(bytes: &[u8]) -> rspirv::dr::Module {
    let mut loader = rspirv::dr::Loader::new();
    rspirv::binary::parse_bytes(&bytes, &mut loader).unwrap();
    loader.module()
}

fn operand_idref(op: &rspirv::dr::Operand) -> Option<spirv::Word> {
    match *op {
        rspirv::dr::Operand::IdMemorySemantics(w)
        | rspirv::dr::Operand::IdScope(w)
        | rspirv::dr::Operand::IdRef(w) => Some(w),
        _ => None,
    }
}
fn operand_idref_mut(op: &mut rspirv::dr::Operand) -> Option<&mut spirv::Word> {
    match op {
        rspirv::dr::Operand::IdMemorySemantics(w)
        | rspirv::dr::Operand::IdScope(w)
        | rspirv::dr::Operand::IdRef(w) => Some(w),
        _ => None,
    }
}

fn print_type(defs: &DefAnalyzer, ty: &rspirv::dr::Instruction) -> String {
    format!("{}", ty::trans_aggregate_type(defs, ty).unwrap())
}

fn extract_literal_int_as_u64(op: &rspirv::dr::Operand) -> u64 {
    match op {
        rspirv::dr::Operand::LiteralInt32(v) => (*v).into(),
        rspirv::dr::Operand::LiteralInt64(v) => *v,
        _ => panic!("Unexpected literal int"),
    }
}

fn extract_literal_u32(op: &rspirv::dr::Operand) -> u32 {
    match op {
        rspirv::dr::Operand::LiteralInt32(v) => *v,
        _ => panic!("Unexpected literal u32"),
    }
}

pub fn link<T>(
    inputs: &mut [&mut rspirv::dr::Module],
    timer: impl Fn(&'static str) -> T,
) -> Result<rspirv::dr::Module> {
    let merge_timer = timer("link_merge");
    // shift all the ids
    let mut bound = inputs[0].header.as_ref().unwrap().bound - 1;
    let version = inputs[0].header.as_ref().unwrap().version();

    for mut module in inputs.iter_mut().skip(1) {
        simple_passes::shift_ids(&mut module, bound);
        bound += module.header.as_ref().unwrap().bound - 1;
        assert_eq!(version, module.header.as_ref().unwrap().version());
    }

    // merge the binaries
    let mut loader = rspirv::dr::Loader::new();

    for module in inputs.iter() {
        module.all_inst_iter().for_each(|inst| {
            loader.consume_instruction(inst.clone());
        });
    }

    let mut output = loader.module();
    let mut header = rspirv::dr::ModuleHeader::new(bound);
    header.set_version(version.0, version.1);
    output.header = Some(header);

    drop(merge_timer);

    let find_pairs_timer = timer("link_find_pairs");
    // find import / export pairs
    import_export_link::run(&mut output)?;
    drop(find_pairs_timer);

    let remove_duplicates_timer = timer("link_remove_duplicates");
    // remove duplicates (https://github.com/KhronosGroup/SPIRV-Tools/blob/e7866de4b1dc2a7e8672867caeb0bdca49f458d3/source/opt/remove_duplicates_pass.cpp)
    duplicates::remove_duplicate_capablities(&mut output);
    duplicates::remove_duplicate_ext_inst_imports(&mut output);
    duplicates::remove_duplicate_types(&mut output);
    // jb-todo: strip identical OpDecoration / OpDecorationGroups
    drop(remove_duplicates_timer);

    let remove_zombies_timer = timer("link_remove_zombies");
    zombies::remove_zombies(&mut output);
    drop(remove_zombies_timer);

    let block_ordering_pass_timer = timer("link_block_ordering_pass");
    for func in &mut output.functions {
        simple_passes::block_ordering_pass(func);
    }
    drop(block_ordering_pass_timer);
    let sort_globals_timer = timer("link_sort_globals");
    simple_passes::sort_globals(&mut output);
    drop(sort_globals_timer);

    if env::var("NO_COMPACT_IDS").is_err() {
        let compact_ids_timer = timer("link_compact_ids");
        // compact the ids https://github.com/KhronosGroup/SPIRV-Tools/blob/e02f178a716b0c3c803ce31b9df4088596537872/source/opt/compact_ids_pass.cpp#L43
        output.header.as_mut().unwrap().bound = simple_passes::compact_ids(&mut output);
        drop(compact_ids_timer);
    };

    output.debugs.push(rspirv::dr::Instruction::new(
        spirv::Op::ModuleProcessed,
        None,
        None,
        vec![rspirv::dr::Operand::LiteralString(
            "Linked by rspirv-linker".to_string(),
        )],
    ));

    // output the module
    Ok(output)
}
