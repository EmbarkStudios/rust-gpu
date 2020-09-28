use crate::ty::trans_aggregate_type;
use crate::{
    inst_fully_eq, kill_annotations_and_debug, kill_with, print_type, DefAnalyzer, LinkerError,
    Options, Result,
};
use rspirv::spirv;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct LinkSymbol {
    name: String,
    pub id: u32,
    type_id: u32,
    parameters: Vec<rspirv::dr::Instruction>,
}

#[derive(Debug)]
pub struct ImportExportPair {
    pub import: LinkSymbol,
    pub export: LinkSymbol,
}

#[derive(Debug)]
pub struct LinkInfo {
    imports: Vec<LinkSymbol>,
    exports: HashMap<String, Vec<LinkSymbol>>,
    potential_pairs: Vec<ImportExportPair>,
}

pub(crate) fn find_import_export_pairs(
    module: &rspirv::dr::Module,
    defs: &DefAnalyzer,
) -> Result<LinkInfo> {
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
                .unwrap_or_else(|| panic!("Need a matching op for ID {}", id));

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
    pub(crate) fn ensure_matching_import_export_pairs(
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
                    import_type: print_type(defs, import_result_type),
                    export_type: print_type(defs, export_result_type),
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

pub fn kill_linkage_instructions(
    pairs: &[ImportExportPair],
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
            .retain(|v| v.result_id.map_or(true, |v| v != pair.import.id));
    }

    // drop linkage attributes (both import and export)
    kill_with(&mut module.annotations, |inst| {
        let eq = pairs.iter().any(|p| {
            if inst.operands.is_empty() {
                return false;
            }

            if let rspirv::dr::Operand::IdRef(id) = inst.operands[0] {
                id == p.import.id || id == p.export.id
            } else {
                false
            }
        });

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

pub fn import_kill_annotations_and_debug(module: &mut rspirv::dr::Module, info: &LinkInfo) {
    for import in &info.imports {
        kill_annotations_and_debug(module, import.id);
        for param in &import.parameters {
            kill_annotations_and_debug(module, param.result_id.unwrap())
        }
    }
}
