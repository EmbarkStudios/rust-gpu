use rspirv::dr::{Instruction, Module};
use rspirv::spirv::{Capability, Op};
use rustc_data_structures::fx::FxHashSet;

pub fn remove_extra_capabilities(module: &mut Module) {
    let used_capabilities = used_capabilities(module);
    let removable_capabilities: FxHashSet<Capability> = [
        Capability::Int8,
        Capability::Int16,
        Capability::Int64,
        Capability::Float16,
        Capability::Float64,
        Capability::IntegerFunctions2INTEL,
        Capability::DemoteToHelperInvocationEXT,
        Capability::DerivativeControl,
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

fn used_capabilities(module: &Module) -> FxHashSet<Capability> {
    let mut set = FxHashSet::default();
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

fn remove_capabilities(module: &mut Module, set: &FxHashSet<Capability>) {
    module.capabilities.retain(|inst| {
        inst.class.opcode != Op::Capability || !set.contains(&inst.operands[0].unwrap_capability())
    });
}

// rspirv pulls its spec information from the latest version. However, we might not be compiling for
// the latest version.
// For example, we might run into this situation:
// OpCapability VulkanMemoryModel in SPIR-V v1.5 requires no extensions
// OpCapability VulkanMemoryModel in SPIR-V <= v1.4 requires OpExtension SPV_KHR_vulkan_memory_model
// rspirv uses SPIR-V v1.5 (as of now), and so it states that VulkanMemoryModel needs no extensions
// We're compiling for, say, SPIR-V 1.3, and ask rspirv if VulkanMemoryModel requires an extension
// It says no. We strip it. Things explode.
// So, this function is to encode any special version-specific rules that aren't in rspirv.
fn additional_extensions(module: &Module, inst: &Instruction) -> &'static [&'static str] {
    if inst.class.opcode == Op::Capability {
        let version = module.header.as_ref().unwrap().version();
        match inst.operands[0].unwrap_capability() {
            Capability::VulkanMemoryModel if version < (1, 5) => &["SPV_KHR_vulkan_memory_model"],
            Capability::RuntimeDescriptorArray if version < (1, 5) => {
                &["SPV_EXT_descriptor_indexing"]
            }
            _ => &[],
        }
    } else {
        &[]
    }
}

pub fn remove_extra_extensions(module: &mut Module) {
    let set: FxHashSet<&str> = module
        .all_inst_iter()
        .flat_map(|inst| {
            let extensions = inst.class.extensions.iter().copied();
            let operand_extensions = inst.operands.iter().flat_map(|op| op.required_extensions());
            let additional_extensions = additional_extensions(module, inst).iter().copied();
            extensions
                .chain(operand_extensions)
                .chain(additional_extensions)
        })
        .collect();

    module.extensions.retain(|inst| {
        inst.class.opcode != Op::Extension || set.contains(inst.operands[0].unwrap_literal_string())
    })
}
