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

pub struct Options {
    /// `true` if we're creating a library
    pub lib: bool,

    /// `true` if partial linking is allowed
    pub partial: bool,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            lib: false,
            partial: false,
        }
    }
}

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

fn replace_all_uses_with(module: &mut rspirv::dr::Module, before: u32, after: u32) {
    module.all_inst_iter_mut().for_each(|inst| {
        if let Some(ref mut result_type) = &mut inst.result_type {
            if *result_type == before {
                *result_type = after;
            }
        }

        inst.operands.iter_mut().for_each(|op| {
            if let Some(w) = operand_idref_mut(op) {
                if *w == before {
                    *w = after
                }
            }
        })
    });
}

fn inst_fully_eq(a: &rspirv::dr::Instruction, b: &rspirv::dr::Instruction) -> bool {
    // both function instructions need to be 100% identical so check all members
    // jb-todo: derive(PartialEq) on Instruction?
    a.result_id == b.result_id
        && a.class == b.class
        && a.result_type == b.result_type
        && a.operands == b.operands
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

pub fn link(inputs: &mut [&mut rspirv::dr::Module], opts: &Options) -> Result<rspirv::dr::Module> {
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

    // find import / export pairs
    let defs = DefAnalyzer::new(&output);
    let info = import_export_link::find_import_export_pairs(&output, &defs)?;

    // ensure import / export pairs have matching types and defintions
    let matching_pairs = info.ensure_matching_import_export_pairs(&defs)?;

    // remove duplicates (https://github.com/KhronosGroup/SPIRV-Tools/blob/e7866de4b1dc2a7e8672867caeb0bdca49f458d3/source/opt/remove_duplicates_pass.cpp)
    duplicates::remove_duplicate_capablities(&mut output);
    duplicates::remove_duplicate_ext_inst_imports(&mut output);
    duplicates::remove_duplicate_types(&mut output);
    // jb-todo: strip identical OpDecoration / OpDecorationGroups

    // remove names and decorations of import variables / functions https://github.com/KhronosGroup/SPIRV-Tools/blob/8a0ebd40f86d1f18ad42ea96c6ac53915076c3c7/source/opt/ir_context.cpp#L404
    import_export_link::import_kill_annotations_and_debug(&mut output, &info);

    // rematch import variables and functions to export variables / functions https://github.com/KhronosGroup/SPIRV-Tools/blob/8a0ebd40f86d1f18ad42ea96c6ac53915076c3c7/source/opt/ir_context.cpp#L255
    for pair in matching_pairs {
        replace_all_uses_with(&mut output, pair.import.id, pair.export.id);
    }

    // remove linkage specific instructions
    import_export_link::kill_linkage_instructions(&matching_pairs, &mut output, &opts);

    zombies::remove_zombies(&mut output);

    simple_passes::sort_globals(&mut output);

    let bound = if env::var("NO_COMPACT_IDS").is_ok() {
        // It is sometimes useful to not rewrite IDs. For example, if using PRINT_ALL_ZOMBIE, it's useful to be able to
        // inspect the module and have the same IDs as the ones that were printed in the zombie pass.
        simple_passes::max_bound(&output)
    } else {
        // compact the ids https://github.com/KhronosGroup/SPIRV-Tools/blob/e02f178a716b0c3c803ce31b9df4088596537872/source/opt/compact_ids_pass.cpp#L43
        simple_passes::compact_ids(&mut output)
    };
    let mut header = rspirv::dr::ModuleHeader::new(bound);
    header.set_version(version.0, version.1);
    output.header = Some(header);

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
