#[cfg(test)]
mod test;

use rspirv::binary::Consumer;
use rspirv::binary::Disassemble;
use rspirv::spirv;
use std::collections::{HashMap, HashSet};
use thiserror::Error;
use topological_sort::TopologicalSort;

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
    kill_with(insts, |inst| {
        if inst.operands.is_empty() {
            return false;
        }

        match inst.operands[0] {
            rspirv::dr::Operand::IdMemorySemantics(w)
            | rspirv::dr::Operand::IdScope(w)
            | rspirv::dr::Operand::IdRef(w)
                if w == id =>
            {
                true
            }
            _ => false,
        }
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

        if idx == 0 || insts.is_empty() {
            break;
        }

        idx -= 1;
    }
}

fn kill_annotations_and_debug(module: &mut rspirv::dr::Module, id: u32) {
    kill_with_id(&mut module.annotations, id);

    // need to remove OpGroupDecorate members that mention this id
    module.annotations.iter_mut().for_each(|inst| {
        if inst.class.opcode == spirv::Op::GroupDecorate {
            inst.operands.retain(|op| match op {
                rspirv::dr::Operand::IdRef(w) if *w != id => return true,
                _ => return false,
            });
        }
    });

    kill_with_id(&mut module.debugs, id);
}

fn remove_duplicate_types(module: rspirv::dr::Module) -> rspirv::dr::Module {
    use rspirv::binary::Assemble;

    // jb-todo: spirv-tools's linker has special case handling for SpvOpTypeForwardPointer,
    // not sure if we need that; see https://github.com/KhronosGroup/SPIRV-Tools/blob/e7866de4b1dc2a7e8672867caeb0bdca49f458d3/source/opt/remove_duplicates_pass.cpp for reference

    // need to do this process iteratively because types can reference each other


    let instructions = module
        .all_inst_iter()
        .cloned()
        .map(|v| std::rc::Rc::new(std::cell::RefCell::new(v)))
        .collect::<Vec<_>>()
        .into_boxed_slice();

    let mut def_ids = HashMap::new();
    let mut use_ids: HashMap<u32, Vec<std::rc::Rc<std::cell::RefCell<rspirv::dr::Instruction>>>> = HashMap::new();
    let mut use_result_type_ids: HashMap<u32, Vec<std::rc::Rc<std::cell::RefCell<rspirv::dr::Instruction>>>> = HashMap::new();

    instructions.iter().for_each(|inst| {
        if let Some(def_id) = inst.borrow().result_id {
            def_ids
                .entry(def_id)
                .and_modify(|stored_inst| {
                    *stored_inst = inst;
                })
                .or_insert(inst);
        }

        if let Some(result_type) = inst.borrow().result_type {
            use_result_type_ids.entry(result_type)
                .and_modify(|v| v.push(inst.clone()))
                .or_insert(vec![inst.clone()]);
        }

        for op in inst.borrow().operands.iter() {
            match op {
                rspirv::dr::Operand::IdMemorySemantics(w)
                | rspirv::dr::Operand::IdScope(w)
                | rspirv::dr::Operand::IdRef(w) => {
                    use_ids.entry(*w)
                        .and_modify(|v| v.push(inst.clone()))
                        .or_insert(vec![inst.clone()]);
                }
                _ => {}
            }
        }
    });

    let mut kill_annotations = vec![];
    let mut continue_from_idx = 0;
    loop {
        let mut dedup = std::collections::HashMap::new();
        let mut duplicate = None;

        let iter = module.types_global_values.iter().enumerate().skip(continue_from_idx);

        for (i_idx, i) in iter {
            let i = def_ids[&i.result_id.unwrap()];

            if i.borrow().class.opcode == spirv::Op::Nop {
                continue;
            }

            // partially assemble only the opcode and operands to be used as a key
            // maybe this should also include the result_type
            let data =  {
                let mut data = vec![];

                let inst = i.borrow();
                data.push(inst.class.opcode as u32);
                for op in &inst.operands {
                    op.assemble_into(&mut data);
                }

                data
            };

            dedup
                .entry(data)
                .and_modify(|(identical, first_found_idx)| {
                    duplicate = Some((i, *identical, *first_found_idx));
                })
                .or_insert((i, i_idx));

            if let Some((_, _, first_found_idx)) = duplicate {
                continue_from_idx = first_found_idx;
                break;
            }
        }

        if let Some((before, after, _)) = duplicate {
            let before_id = before.borrow().result_id.unwrap();
            let after_id = after.borrow().result_id.unwrap();

            kill_annotations.push(before_id);

            if let Some(use_result_type_id) = use_result_type_ids.get(&before_id) {
                for before_inst in use_result_type_id {
                    before_inst.borrow_mut().result_type = Some(after_id);
                }
            }

            if let Some(use_id) = use_ids.get(&before_id) {
                for before_inst in use_id {
                    for op in before_inst.borrow_mut().operands.iter_mut() {
                        match op {
                            rspirv::dr::Operand::IdMemorySemantics(w)
                            | rspirv::dr::Operand::IdScope(w)
                            | rspirv::dr::Operand::IdRef(w) => {
                                if *w == before_id {
                                    *w = after_id
                                }
                            },
                            _ => {}
                        }
                    }
                }
            }

            before.replace(rspirv::dr::Instruction::new(spirv::Op::Nop, None, None, vec![]));
        } else {
            break;
        }
    }

    let mut loader = rspirv::dr::Loader::new();

    for inst in instructions.iter() {
        loader.consume_instruction(inst.borrow().clone());
    }

    let mut module = loader.module();

    for remove in kill_annotations {
        kill_annotations_and_debug(&mut module, remove);
    }

    module
}

fn remove_duplicates(module: &mut rspirv::dr::Module) {
    
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

fn find_import_export_pairs(module: &rspirv::dr::Module, defs: &DefAnalyzer) -> Result<LinkInfo> {
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

            let name = match &annotation.operands[2] {
                rspirv::dr::Operand::LiteralString(s) => s,
                _ => panic!("Expected LiteralString"),
            };

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

fn cleanup_type(mut ty: rspirv::dr::Instruction) -> String {
    ty.result_id = None;
    ty.disassemble()
}

impl LinkInfo {
    fn find_potential_pairs(mut self) -> Result<Self> {
        for import in &self.imports {
            let potential_matching_exports = self.exports.get(&import.name);
            if let Some(potential_matching_exports) = potential_matching_exports {
                if potential_matching_exports.len() > 1 {
                    return Err(LinkerError::MultipleExports(import.name.clone()));
                }

                self.potential_pairs.push(ImportExportPair {
                    import: import.clone(),
                    export: potential_matching_exports.first().unwrap().clone(),
                });
            } else {
                return Err(LinkerError::UnresolvedSymbol(import.name.clone()));
            }
        }

        Ok(self)
    }

    /// returns the list of matching import / export pairs after validation the list of potential pairs
    fn ensure_matching_import_export_pairs(
        &self,
        defs: &DefAnalyzer,
    ) -> Result<&Vec<ImportExportPair>> {
        for pair in &self.potential_pairs {
            let import_result_type = defs.def(pair.import.type_id).unwrap();
            let export_result_type = defs.def(pair.export.type_id).unwrap();

            let imp = trans_aggregate_type(defs, import_result_type);
            let exp = trans_aggregate_type(defs, export_result_type);

            if imp != exp {
                return Err(LinkerError::TypeMismatch {
                    name: pair.import.name.clone(),
                    import_type: cleanup_type(import_result_type.clone()),
                    export_type: cleanup_type(export_result_type.clone()),
                });
            }

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

        Ok(&self.potential_pairs)
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
                if inst.operands.is_empty() {
                    return false;
                }

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
        if let Some(result_id) = t.result_id {
            if let Some(result_type) = t.result_type {
                ts.add_dependency(result_type, result_id);
            }

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

    let defs = DefAnalyzer::new(&module);

    let mut new_types_global_values = vec![];

    loop {
        if ts.is_empty() {
            break;
        }

        let mut v = ts.pop_all();
        v.sort();

        for result_id in v {
            new_types_global_values.push(defs.def(result_id).unwrap().clone());
        }
    }

    assert!(module.types_global_values.len() == new_types_global_values.len());

    module.types_global_values = new_types_global_values;
}

#[derive(PartialEq, Debug)]
enum ScalarType {
    Void,
    Bool,
    Int { width: u32, signed: bool },
    Float { width: u32 },
    Opaque { name: String },
    Event,
    DeviceEvent,
    ReserveId,
    Queue,
    Pipe,
    ForwardPointer { storage_class: spirv::StorageClass },
    PipeStorage,
    NamedBarrier,
    Sampler,
}

fn trans_scalar_type(inst: &rspirv::dr::Instruction) -> Option<ScalarType> {
    Some(match inst.class.opcode {
        spirv::Op::TypeVoid => ScalarType::Void,
        spirv::Op::TypeBool => ScalarType::Bool,
        spirv::Op::TypeEvent => ScalarType::Event,
        spirv::Op::TypeDeviceEvent => ScalarType::DeviceEvent,
        spirv::Op::TypeReserveId => ScalarType::ReserveId,
        spirv::Op::TypeQueue => ScalarType::Queue,
        spirv::Op::TypePipe => ScalarType::Pipe,
        spirv::Op::TypePipeStorage => ScalarType::PipeStorage,
        spirv::Op::TypeNamedBarrier => ScalarType::NamedBarrier,
        spirv::Op::TypeSampler => ScalarType::Sampler,
        spirv::Op::TypeForwardPointer => ScalarType::ForwardPointer {
            storage_class: match inst.operands[0] {
                rspirv::dr::Operand::StorageClass(s) => s,
                _ => panic!("Unexpected operand while parsing type"),
            },
        },
        spirv::Op::TypeInt => ScalarType::Int {
            width: match inst.operands[0] {
                rspirv::dr::Operand::LiteralInt32(w) => w,
                _ => panic!("Unexpected operand while parsing type"),
            },
            signed: match inst.operands[1] {
                rspirv::dr::Operand::LiteralInt32(s) => {
                    if s == 0 {
                        false
                    } else {
                        true
                    }
                }
                _ => panic!("Unexpected operand while parsing type"),
            },
        },
        spirv::Op::TypeFloat => ScalarType::Float {
            width: match inst.operands[0] {
                rspirv::dr::Operand::LiteralInt32(w) => w,
                _ => panic!("Unexpected operand while parsing type"),
            },
        },
        spirv::Op::TypeOpaque => ScalarType::Opaque {
            name: match &inst.operands[0] {
                rspirv::dr::Operand::LiteralString(s) => s.clone(),
                _ => panic!("Unexpected operand while parsing type"),
            },
        },
        _ => return None,
    })
}

#[derive(PartialEq, Debug)]
enum AggregateType {
    Scalar(ScalarType),
    Array {
        ty: Box<AggregateType>,
        len: u64,
    },
    Pointer {
        ty: Box<AggregateType>,
        storage_class: spirv::StorageClass,
    },
    Image {
        ty: Box<AggregateType>,
        dim: spirv::Dim,
        depth: u32,
        arrayed: u32,
        multi_sampled: u32,
        sampled: u32,
        format: spirv::ImageFormat,
        access: Option<spirv::AccessQualifier>,
    },
    SampledImage {
        ty: Box<AggregateType>,
    },
    Aggregate(Vec<AggregateType>),
}

fn op_def(def: &DefAnalyzer, operand: &rspirv::dr::Operand) -> rspirv::dr::Instruction {
    def.def(match operand {
        rspirv::dr::Operand::IdMemorySemantics(w)
        | rspirv::dr::Operand::IdScope(w)
        | rspirv::dr::Operand::IdRef(w) => *w,
        _ => panic!("Expected ID"),
    })
    .unwrap()
    .clone()
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

fn trans_aggregate_type(
    def: &DefAnalyzer,
    inst: &rspirv::dr::Instruction,
) -> Option<AggregateType> {
    Some(match inst.class.opcode {
        spirv::Op::TypeArray => {
            let len_def = op_def(def, &inst.operands[1]);
            assert!(len_def.class.opcode == spirv::Op::Constant); // don't support spec constants yet

            let len_value = extract_literal_int_as_u64(&len_def.operands[1]);

            AggregateType::Array {
                ty: Box::new(
                    trans_aggregate_type(def, &op_def(def, &inst.operands[0]))
                        .expect("Expect base type for OpTypeArray"),
                ),
                len: len_value,
            }
        }
        spirv::Op::TypePointer => AggregateType::Pointer {
            storage_class: match inst.operands[0] {
                rspirv::dr::Operand::StorageClass(s) => s,
                _ => panic!("Unexpected operand while parsing type"),
            },
            ty: Box::new(
                trans_aggregate_type(def, &op_def(def, &inst.operands[1]))
                    .expect("Expect base type for OpTypePointer"),
            ),
        },
        spirv::Op::TypeRuntimeArray
        | spirv::Op::TypeVector
        | spirv::Op::TypeMatrix
        | spirv::Op::TypeSampledImage => AggregateType::Aggregate(
            trans_aggregate_type(def, &op_def(def, &inst.operands[0]))
                .map_or_else(|| vec![], |v| vec![v]),
        ),
        spirv::Op::TypeStruct | spirv::Op::TypeFunction => {
            let mut types = vec![];
            for operand in inst.operands.iter() {
                let op_def = op_def(def, operand);

                match trans_aggregate_type(def, &op_def) {
                    Some(ty) => types.push(ty),
                    None => panic!("Expected type"),
                }
            }

            AggregateType::Aggregate(types)
        }
        spirv::Op::TypeImage => AggregateType::Image {
            ty: Box::new(
                trans_aggregate_type(def, &op_def(def, &inst.operands[0]))
                    .expect("Expect base type for OpTypeImage"),
            ),
            dim: match inst.operands[1] {
                rspirv::dr::Operand::Dim(d) => d,
                _ => panic!("Invalid dim"),
            },
            depth: extract_literal_u32(&inst.operands[2]),
            arrayed: extract_literal_u32(&inst.operands[3]),
            multi_sampled: extract_literal_u32(&inst.operands[4]),
            sampled: extract_literal_u32(&inst.operands[5]),
            format: match inst.operands[6] {
                rspirv::dr::Operand::ImageFormat(f) => f,
                _ => panic!("Invalid image format"),
            },
            access: inst
                .operands
                .get(7)
                .map(|op| match op {
                    rspirv::dr::Operand::AccessQualifier(a) => Some(a.clone()),
                    _ => None,
                })
                .flatten(),
        },
        _ => {
            if let Some(ty) = trans_scalar_type(inst) {
                AggregateType::Scalar(ty)
            } else {
                return None;
            }
        }
    })
}

pub fn link(inputs: &mut [&mut rspirv::dr::Module], opts: &Options) -> Result<rspirv::dr::Module> {
    // shift all the ids
    let mut bound = inputs[0].header.as_ref().unwrap().bound - 1;

    for mut module in inputs.iter_mut().skip(1) {
        shift_ids(&mut module, bound);
        bound += module.header.as_ref().unwrap().bound - 1;
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
    let info = find_import_export_pairs(&output, &defs)?;

    // ensure import / export pairs have matching types and defintions
    let matching_pairs = info.ensure_matching_import_export_pairs(&defs)?;

    // remove duplicates (https://github.com/KhronosGroup/SPIRV-Tools/blob/e7866de4b1dc2a7e8672867caeb0bdca49f458d3/source/opt/remove_duplicates_pass.cpp)
    remove_duplicate_capablities(&mut output);
    remove_duplicate_ext_inst_imports(&mut output);
    let mut output = remove_duplicate_types(output);
    // jb-todo: strip identical OpDecoration / OpDecorationGroups

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

    // output the module
    Ok(output)
}
