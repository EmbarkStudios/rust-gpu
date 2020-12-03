use rspirv::dr::Module;
use rspirv::spirv::{Capability, Op};
use std::collections::HashSet;

pub fn remove_extra_capabilities(module: &mut Module) {
    let used_capabilities = used_capabilities(module);
    let removable_capabilities: HashSet<Capability> = [
        Capability::Int8,
        Capability::Int16,
        Capability::Int64,
        Capability::Float16,
        Capability::Float64,
        Capability::IntegerFunctions2INTEL,
    ]
    .iter()
    .copied()
    .collect();
    let to_remove = removable_capabilities
        .difference(&used_capabilities)
        .copied()
        .collect();
    remove_capabilities(module, &to_remove);
}

fn used_capabilities(module: &Module) -> HashSet<Capability> {
    let mut set = HashSet::new();
    for inst in module.all_inst_iter() {
        set.extend(inst.class.capabilities);
        match inst.class.opcode {
            Op::TypeInt => match inst.operands[0].unwrap_literal_int32() {
                8 => {
                    set.insert(Capability::Int8);
                }
                16 => {
                    set.insert(Capability::Int16);
                }
                64 => {
                    set.insert(Capability::Int64);
                }
                _ => {}
            },
            Op::TypeFloat => match inst.operands[0].unwrap_literal_int32() {
                16 => {
                    set.insert(Capability::Float16);
                }
                64 => {
                    set.insert(Capability::Float64);
                }
                _ => {}
            },
            _ => {}
        }
    }
    set
}

fn remove_capabilities(module: &mut Module, set: &HashSet<Capability>) {
    module.capabilities.retain(|inst| {
        inst.class.opcode != Op::Capability || !set.contains(&inst.operands[0].unwrap_capability())
    });
}

pub fn remove_extra_extensions(module: &mut Module) {
    // TODO: Make this more generalized once this gets more advanced.
    let has_intel_integer_cap = module.capabilities.iter().any(|inst| {
        inst.class.opcode == Op::Capability
            && inst.operands[0].unwrap_capability() == Capability::IntegerFunctions2INTEL
    });
    if !has_intel_integer_cap {
        module.extensions.retain(|inst| {
            inst.class.opcode != Op::Extension
                || inst.operands[0].unwrap_literal_string() != "SPV_INTEL_shader_integer_functions2"
        })
    }
}
