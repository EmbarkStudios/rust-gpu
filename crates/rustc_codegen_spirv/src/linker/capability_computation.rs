use rspirv::dr::Module;
use rspirv::spirv::{Capability, Op};
use std::collections::HashSet;

pub fn remove_extra_capabilities(module: &mut Module) {
    remove_capabilities(module, &compute_capabilities(module));
}

// TODO: This is enormously unimplemented
fn compute_capabilities(module: &Module) -> HashSet<Capability> {
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
    // always keep these capabilities, for now
    set.insert(Capability::Addresses);
    set.insert(Capability::Kernel);
    set.insert(Capability::Shader);
    set.insert(Capability::VariablePointers);
    set.insert(Capability::VulkanMemoryModel);
    set
}

fn remove_capabilities(module: &mut Module, set: &HashSet<Capability>) {
    module.capabilities.retain(|inst| {
        inst.class.opcode != Op::Capability || set.contains(&inst.operands[0].unwrap_capability())
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
