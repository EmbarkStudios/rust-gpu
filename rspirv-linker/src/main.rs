mod test;

use rspirv::binary::Consumer;
use rspirv::binary::Disassemble;
use rspirv::spirv;
use std::collections::{HashMap, HashSet};
use topological_sort::TopologicalSort;

fn load(bytes: &[u8]) -> rspirv::dr::Module {
    let mut loader = rspirv::dr::Loader::new();
    rspirv::binary::parse_bytes(&bytes, &mut loader).unwrap();
    let module = loader.module();
    module
}

fn shift_ids(module: &mut rspirv::dr::Module, add: u32) {
    module.all_inst_iter_mut().for_each(|inst| {
        if let Some(ref mut result_id) = &mut inst.result_id {
            *result_id += add;
        }

        if let Some(ref mut result_type) = &mut inst.result_type {
            *result_type += add;
        }

        inst.operands.iter_mut().for_each(|op| match op {
            rspirv::dr::Operand::IdMemorySemantics(w)
            | rspirv::dr::Operand::IdScope(w)
            | rspirv::dr::Operand::IdRef(w) => *w += add,
            _ => {}
        })
    });
}

fn replace_all_uses_with(module: &mut rspirv::dr::Module, before: u32, after: u32) {
    module.all_inst_iter_mut().for_each(|inst| {
        if let Some(ref mut result_type) = &mut inst.result_type {
            if *result_type == before {
                *result_type = after;
            }
        }

        inst.operands.iter_mut().for_each(|op| match op {
            rspirv::dr::Operand::IdMemorySemantics(w)
            | rspirv::dr::Operand::IdScope(w)
            | rspirv::dr::Operand::IdRef(w) => {
                if *w == before {
                    *w = after
                }
            }
            _ => {}
        })
    });
}

fn remove_duplicate_capablities(module: &mut rspirv::dr::Module) {
    let mut set = HashSet::new();
    let mut caps = vec![];

    for c in &module.capabilities {
        let keep = match c.operands[0] {
            rspirv::dr::Operand::Capability(cap) => set.insert(cap),
            _ => true,
        };

        if keep {
            caps.push(c.clone());
        }
    }

    module.capabilities = caps;
}

fn remove_duplicate_ext_inst_imports(module: &mut rspirv::dr::Module) {
    let mut set = HashSet::new();
    let mut caps = vec![];

    for c in &module.ext_inst_imports {
        let keep = match &c.operands[0] {
            rspirv::dr::Operand::LiteralString(ext_inst_import) => set.insert(ext_inst_import),
            _ => true,
        };

        if keep {
            caps.push(c.clone());
        }
    }

    module.ext_inst_imports = caps;
}

fn kill_with_id(insts: &mut Vec<rspirv::dr::Instruction>, id: u32) {
    kill_with(insts, |inst| match inst.operands[0] {
        rspirv::dr::Operand::IdMemorySemantics(w)
        | rspirv::dr::Operand::IdScope(w)
        | rspirv::dr::Operand::IdRef(w)
            if w == id =>
        {
            true
        }
        _ => false,
    })
}

fn kill_with<F>(insts: &mut Vec<rspirv::dr::Instruction>, f: F)
where
    F: Fn(&rspirv::dr::Instruction) -> bool,
{
    if insts.is_empty() {
        return;
    }

    let mut idx = insts.len() - 1;
    // odd backwards loop so we can swap_remove
    loop {
        if f(&insts[idx]) {
            insts.swap_remove(idx);
        }

        if idx == 0 {
            break;
        }

        idx -= 1;
    }
}

fn kill_annotations_and_debug(module: &mut rspirv::dr::Module, id: u32) {
    kill_with_id(&mut module.annotations, id);
    kill_with_id(&mut module.debugs, id);
}

fn remove_duplicate_types(module: &mut rspirv::dr::Module) {
    // jb-todo: spirv-tools's linker has special case handling for SpvOpTypeForwardPointer,
    // not sure if we need that; see https://github.com/KhronosGroup/SPIRV-Tools/blob/e7866de4b1dc2a7e8672867caeb0bdca49f458d3/source/opt/remove_duplicates_pass.cpp for reference
    let mut start = 0;

    // need to do this process iteratively because types can reference each other
    loop {
        let mut replace = None;

        // start with `nth` so we can restart this loop quickly after killing the op
        for (i_idx, i) in module.types_global_values.iter().enumerate().nth(start) {
            let mut identical = None;
            for j in module.types_global_values.iter().skip(i_idx + 1) {
                if i.is_type_identical(j) {
                    identical = j.result_id;
                    break;
                }
            }

            if let Some(identical) = identical {
                replace = Some((i.result_id.unwrap(), identical, i_idx));
                break;
            }
        }

        // can't do this directly in the previous loop because of the
        // mut borrow needed on `module`
        if let Some((remove, keep, kill_idx)) = replace {
            kill_annotations_and_debug(module, remove);
            replace_all_uses_with(module, remove, keep);
            module.types_global_values.swap_remove(kill_idx);
            start = kill_idx; // jb-todo: is it correct to restart this loop here?
        } else {
            break;
        }
    }
}

fn remove_duplicates(module: &mut rspirv::dr::Module) {
    remove_duplicate_capablities(module);
    remove_duplicate_ext_inst_imports(module);
    remove_duplicate_types(module);
    // jb-todo: strip identical OpDecoration / OpDecorationGroups
}

#[derive(Clone, Debug)]
struct LinkSymbol {
    name: String,
    id: u32,
    type_id: u32,
    parameters: Vec<rspirv::dr::Instruction>,
}

#[derive(Debug)]
struct ImportExportPair {
    import: LinkSymbol,
    export: LinkSymbol,
}

#[derive(Debug)]
struct LinkInfo {
    imports: Vec<LinkSymbol>,
    exports: HashMap<String, Vec<LinkSymbol>>,
    potential_pairs: Vec<ImportExportPair>,
}

fn inst_fully_eq(a: &rspirv::dr::Instruction, b: &rspirv::dr::Instruction) -> bool {
    // both function instructions need to be 100% identical so check all members
    // jb-todo: derive(PartialEq) on Instruction?
    a.result_id == b.result_id
        && a.class == b.class
        && a.result_type == b.result_type
        && a.operands == b.operands
}

fn find_import_export_pairs(module: &rspirv::dr::Module, defs: &DefAnalyzer) -> LinkInfo {
    let mut imports = vec![];
    let mut exports: HashMap<String, Vec<LinkSymbol>> = HashMap::new();

    for annotation in &module.annotations {
        if annotation.class.opcode == spirv::Op::Decorate
            && annotation.operands[1]
                == rspirv::dr::Operand::Decoration(spirv::Decoration::LinkageAttributes)
        {
            let id = match annotation.operands[0] {
                rspirv::dr::Operand::IdRef(i) => i,
                _ => panic!("Expected IdRef"),
            };
            let name = &annotation.operands[2];
            let ty = &annotation.operands[3];

            let def_inst = defs
                .def(id)
                .expect(&format!("Need a matching op for ID {}", id));

            let (type_id, parameters) = match def_inst.class.opcode {
                spirv::Op::Variable => (def_inst.result_type.unwrap(), vec![]),
                spirv::Op::Function => {
                    let type_id = if let rspirv::dr::Operand::IdRef(id) = &def_inst.operands[1] {
                        *id
                    } else {
                        panic!("Expected IdRef");
                    };

                    let def_fn = module
                        .functions
                        .iter()
                        .find(|f| inst_fully_eq(f.def.as_ref().unwrap(), def_inst))
                        .unwrap();

                    (type_id, def_fn.parameters.clone())
                }
                _ => panic!("Unexpected op"),
            };

            let symbol = LinkSymbol {
                name: name.to_string(),
                id,
                type_id,
                parameters,
            };

            if ty == &rspirv::dr::Operand::LinkageType(spirv::LinkageType::Import) {
                imports.push(symbol);
            } else {
                exports
                    .entry(symbol.name.clone())
                    .and_modify(|v| v.push(symbol.clone()))
                    .or_insert_with(|| vec![symbol.clone()]);
            }
        }
    }

    LinkInfo {
        imports,
        exports,
        potential_pairs: vec![],
    }
    .find_potential_pairs()
}

impl LinkInfo {
    fn find_potential_pairs(mut self) -> Self {
        for import in &self.imports {
            let potential_matching_exports = self.exports.get(&import.name);
            if let Some(potential_matching_exports) = potential_matching_exports {
                self.potential_pairs.push(ImportExportPair {
                    import: import.clone(),
                    export: potential_matching_exports.first().unwrap().clone(),
                });
            } else {
                panic!("Can't find matching export for {}", import.name);
            }
        }

        self
    }

    /// returns the list of matching import / export pairs after validation the list of potential pairs
    fn ensure_matching_import_export_pairs(&self) -> &Vec<ImportExportPair> {
        for pair in &self.potential_pairs {
            for (import_param, export_param) in pair
                .import
                .parameters
                .iter()
                .zip(pair.export.parameters.iter())
            {
                if !import_param.is_type_identical(export_param) {
                    panic!("Type error in signatures")
                }

                // jb-todo: validate that OpDecoration is identical too
            }
        }

        &self.potential_pairs
    }
}

struct DefAnalyzer {
    def_ids: HashMap<u32, rspirv::dr::Instruction>,
}

impl DefAnalyzer {
    fn new(module: &rspirv::dr::Module) -> Self {
        let mut def_ids = HashMap::new();

        module.all_inst_iter().for_each(|inst| {
            if let Some(def_id) = inst.result_id {
                def_ids
                    .entry(def_id)
                    .and_modify(|stored_inst| {
                        *stored_inst = inst.clone();
                    })
                    .or_insert(inst.clone());
            }
        });

        Self { def_ids }
    }

    fn def(&self, id: u32) -> Option<&rspirv::dr::Instruction> {
        self.def_ids.get(&id)
    }
}

fn import_kill_annotations_and_debug(module: &mut rspirv::dr::Module, info: &LinkInfo) {
    for import in &info.imports {
        kill_annotations_and_debug(module, import.id);
        for param in &import.parameters {
            kill_annotations_and_debug(module, param.result_id.unwrap())
        }
    }
}

struct Options {
    /// `true` if we're creating a library
    lib: bool,

    /// `true` if partial linking is allowed
    partial: bool,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            lib: false,
            partial: false,
        }
    }
}

fn kill_linkage_instructions(
    pairs: &Vec<ImportExportPair>,
    module: &mut rspirv::dr::Module,
    opts: &Options,
) {
    // drop imported functions
    for pair in pairs.iter() {
        module
            .functions
            .retain(|f| pair.import.id != f.def.as_ref().unwrap().result_id.unwrap());
    }

    // drop imported variables
    for pair in pairs.iter() {
        module
            .types_global_values
            .retain(|v| pair.import.id != v.result_id.unwrap());
    }

    // drop linkage attributes (both import and export)
    kill_with(&mut module.annotations, |inst| {
        let eq = pairs
            .iter()
            .find(|p| {
                if let rspirv::dr::Operand::IdRef(id) = inst.operands[0] {
                    id == p.import.id || id == p.export.id
                } else {
                    false
                }
            })
            .is_some();

        eq && inst.class.opcode == spirv::Op::Decorate
            && inst.operands[1]
                == rspirv::dr::Operand::Decoration(spirv::Decoration::LinkageAttributes)
    });

    if !opts.lib {
        kill_with(&mut module.annotations, |inst| {
            inst.class.opcode == spirv::Op::Decorate
                && inst.operands[1]
                    == rspirv::dr::Operand::Decoration(spirv::Decoration::LinkageAttributes)
                && inst.operands[3] == rspirv::dr::Operand::LinkageType(spirv::LinkageType::Export)
        });
    }

    // drop OpCapability Linkage
    kill_with(&mut module.capabilities, |inst| {
        inst.class.opcode == spirv::Op::Capability
            && inst.operands[0] == rspirv::dr::Operand::Capability(spirv::Capability::Linkage)
    })
}

fn compact_ids(module: &mut rspirv::dr::Module) -> u32 {
    let mut remap = HashMap::new();

    let mut insert = |current_id: u32| -> u32 {
        if remap.contains_key(&current_id) {
            remap[&current_id]
        } else {
            let new_id = remap.len() as u32 + 1;
            remap.insert(current_id, new_id);
            new_id
        }
    };

    module.all_inst_iter_mut().for_each(|inst| {
        if let Some(ref mut result_id) = &mut inst.result_id {
            *result_id = insert(*result_id);
        }

        if let Some(ref mut result_type) = &mut inst.result_type {
            *result_type = insert(*result_type);
        }

        inst.operands.iter_mut().for_each(|op| match op {
            rspirv::dr::Operand::IdMemorySemantics(w)
            | rspirv::dr::Operand::IdScope(w)
            | rspirv::dr::Operand::IdRef(w) => {
                *w = insert(*w);
            }
            _ => {}
        })
    });

    remap.len() as u32 + 1
}

fn sort_globals(module: &mut rspirv::dr::Module) {
    let mut ts = TopologicalSort::<u32>::new();

    for t in module.types_global_values.iter() {
        if let Some(result_type) = t.result_type {
            if let Some(result_id) = t.result_id {
                ts.add_dependency(result_type, result_id);

                for op in &t.operands {
                    match op {
                        rspirv::dr::Operand::IdMemorySemantics(w)
                        | rspirv::dr::Operand::IdScope(w)
                        | rspirv::dr::Operand::IdRef(w) => {
                            ts.add_dependency(*w, result_id); // the op defining the IdRef should come before our op / result_id
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    let defs = DefAnalyzer::new(&module);

    let mut new_types_global_values = vec![];

    loop {
        let mut v = ts.pop_all();
        v.sort();

        for result_id in v {
            new_types_global_values.push(defs.def(result_id).unwrap().clone());
        }

        if ts.is_empty() {
            break;
        }
    }

    assert!(module.types_global_values.len() == new_types_global_values.len());

    module.types_global_values = new_types_global_values;
}

fn link(inputs: &mut [&mut rspirv::dr::Module], opts: &Options) -> rspirv::dr::Module {
    // shift all the ids
    let mut bound = inputs[0].header.as_ref().unwrap().bound - 1;

    for mut module in inputs.iter_mut().skip(1) {
        shift_ids(&mut module, bound);
        bound += module.header.as_ref().unwrap().bound - 1;
    }

    for i in inputs.iter() {
        println!("{}\n\n", i.disassemble());
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
    let info = find_import_export_pairs(&output, &defs);

    // ensure import / export pairs have matching types and defintions
    let matching_pairs = info.ensure_matching_import_export_pairs();

    // remove duplicates (https://github.com/KhronosGroup/SPIRV-Tools/blob/e7866de4b1dc2a7e8672867caeb0bdca49f458d3/source/opt/remove_duplicates_pass.cpp)
    remove_duplicates(&mut output);

    // remove names and decorations of import variables / functions https://github.com/KhronosGroup/SPIRV-Tools/blob/8a0ebd40f86d1f18ad42ea96c6ac53915076c3c7/source/opt/ir_context.cpp#L404
    import_kill_annotations_and_debug(&mut output, &info);

    // rematch import variables and functions to export variables / functions https://github.com/KhronosGroup/SPIRV-Tools/blob/8a0ebd40f86d1f18ad42ea96c6ac53915076c3c7/source/opt/ir_context.cpp#L255
    for pair in matching_pairs {
        replace_all_uses_with(&mut output, pair.import.id, pair.export.id);
    }

    // remove linkage specific instructions
    kill_linkage_instructions(&matching_pairs, &mut output, &opts);

    sort_globals(&mut output);

    // compact the ids https://github.com/KhronosGroup/SPIRV-Tools/blob/e02f178a716b0c3c803ce31b9df4088596537872/source/opt/compact_ids_pass.cpp#L43
    let bound = compact_ids(&mut output);
    output.header = Some(rspirv::dr::ModuleHeader::new(bound));

    output.debugs.push(rspirv::dr::Instruction::new(
        spirv::Op::ModuleProcessed,
        None,
        None,
        vec![rspirv::dr::Operand::LiteralString(
            "Linked by rspirv-linker".to_string(),
        )],
    ));

    println!("{}\n\n", output.disassemble());

    // output the module
    output
}

fn main() {
    let body1 = include_bytes!("../test/1/body_1.spv");
    let body2 = include_bytes!("../test/1/body_2.spv");

    let mut body1 = load(&body1[..]);
    let mut body2 = load(&body2[..]);

    let opts = Options {
        lib: false,
        partial: false,
    };

    let output = link(&mut [&mut body1, &mut body2], &opts);
    println!("{}\n\n", output.disassemble());
}
