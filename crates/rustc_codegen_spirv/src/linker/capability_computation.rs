use rspirv::dr::{Instruction, Module, Operand};
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

// TODO: Move this to rspirv
fn operand_required_extensions(op: &Operand) -> &'static [&'static str] {
    match op {
        Operand::SourceLanguage(x) => x.required_extensions(),
        Operand::ExecutionModel(x) => x.required_extensions(),
        Operand::AddressingModel(x) => x.required_extensions(),
        Operand::MemoryModel(x) => x.required_extensions(),
        Operand::ExecutionMode(x) => x.required_extensions(),
        Operand::StorageClass(x) => x.required_extensions(),
        Operand::Dim(x) => x.required_extensions(),
        Operand::SamplerAddressingMode(x) => x.required_extensions(),
        Operand::SamplerFilterMode(x) => x.required_extensions(),
        Operand::ImageFormat(x) => x.required_extensions(),
        Operand::ImageChannelOrder(x) => x.required_extensions(),
        Operand::ImageChannelDataType(x) => x.required_extensions(),
        Operand::FPRoundingMode(x) => x.required_extensions(),
        Operand::LinkageType(x) => x.required_extensions(),
        Operand::AccessQualifier(x) => x.required_extensions(),
        Operand::FunctionParameterAttribute(x) => x.required_extensions(),
        Operand::Decoration(x) => x.required_extensions(),
        Operand::BuiltIn(x) => x.required_extensions(),
        Operand::Scope(x) => x.required_extensions(),
        Operand::GroupOperation(x) => x.required_extensions(),
        Operand::KernelEnqueueFlags(x) => x.required_extensions(),
        Operand::Capability(x) => x.required_extensions(),
        Operand::RayQueryIntersection(x) => x.required_extensions(),
        Operand::RayQueryCommittedIntersectionType(x) => x.required_extensions(),
        Operand::RayQueryCandidateIntersectionType(x) => x.required_extensions(),
        _ => &[],
    }
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
    if module.header.as_ref().unwrap().version() < (1, 5)
        && inst.class.opcode == Op::Capability
        && inst.operands[0].unwrap_capability() == Capability::VulkanMemoryModel
    {
        &["SPV_KHR_vulkan_memory_model"]
    } else {
        &[]
    }
}

pub fn remove_extra_extensions(module: &mut Module) {
    let set: HashSet<&str> = module
        .all_inst_iter()
        .flat_map(|inst| {
            inst.class
                .extensions
                .iter()
                .chain(inst.operands.iter().flat_map(operand_required_extensions))
                .chain(additional_extensions(module, inst))
        })
        .copied()
        .collect();
    module.extensions.retain(|inst| {
        inst.class.opcode != Op::Extension || set.contains(inst.operands[0].unwrap_literal_string())
    })
}
