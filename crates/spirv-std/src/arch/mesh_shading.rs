#[cfg(target_arch = "spirv")]
use core::arch::asm;

/// Sets the actual output size of the primitives and vertices that the mesh shader
/// workgroup will emit upon completion.
///
/// 'Vertex Count' must be a 32-bit unsigned integer value.
/// It defines the array size of per-vertex outputs.
///
/// 'Primitive Count' must a 32-bit unsigned integer value.
/// It defines the array size of per-primitive outputs.
///
/// The arguments are taken from the first invocation in each workgroup.
/// Any invocation must execute this instruction no more than once and under
/// uniform control flow.
/// There must not be any control flow path to an output write that is not preceded
/// by this instruction.
///
/// This instruction is only valid in the *MeshEXT* Execution Model.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpSetMeshOutputsEXT")]
#[inline]
pub unsafe fn set_mesh_outputs_ext(vertex_count: u32, primitive_count: u32) {
    asm! {
        "OpSetMeshOutputsEXT {vertex_count} {primitive_count}",
        vertex_count = in(reg) vertex_count,
        primitive_count = in(reg) primitive_count,
    }
}

/// Defines the grid size of subsequent mesh shader workgroups to generate
/// upon completion of the task shader workgroup.
///
/// 'Group Count X Y Z' must each be a 32-bit unsigned integer value.
/// They configure the number of local workgroups in each respective dimensions
/// for the launch of child mesh tasks. See Vulkan API specification for more detail.
///
/// 'Payload' is an optional pointer to the payload structure to pass to the generated mesh shader invocations.
/// 'Payload' must be the result of an *OpVariable* with a storage class of *TaskPayloadWorkgroupEXT*.
///
/// The arguments are taken from the first invocation in each workgroup.
/// Any invocation must execute this instruction exactly once and under uniform
/// control flow.
/// This instruction also serves as an *OpControlBarrier* instruction, and also
/// performs and adheres to the description and semantics of an *OpControlBarrier*
/// instruction with the 'Execution' and 'Memory' operands set to *Workgroup* and
/// the 'Semantics' operand set to a combination of *WorkgroupMemory* and
/// *AcquireRelease*.
/// Ceases all further processing: Only instructions executed before
/// *OpEmitMeshTasksEXT* have observable side effects.
///
/// This instruction must be the last instruction in a block.
///
/// This instruction is only valid in the *TaskEXT* Execution Model.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpEmitMeshTasksEXT")]
#[inline]
pub unsafe fn emit_mesh_tasks_ext(group_count_x: u32, group_count_y: u32, group_count_z: u32) -> ! {
    asm! {
        "OpEmitMeshTasksEXT {group_count_x} {group_count_y} {group_count_z}",
        group_count_x = in(reg) group_count_x,
        group_count_y = in(reg) group_count_y,
        group_count_z = in(reg) group_count_z,
        options(noreturn),
    }
}

/// Defines the grid size of subsequent mesh shader workgroups to generate
/// upon completion of the task shader workgroup.
///
/// 'Group Count X Y Z' must each be a 32-bit unsigned integer value.
/// They configure the number of local workgroups in each respective dimensions
/// for the launch of child mesh tasks. See Vulkan API specification for more detail.
///
/// 'Payload' is an optional pointer to the payload structure to pass to the generated mesh shader invocations.
/// 'Payload' must be the result of an *OpVariable* with a storage class of *TaskPayloadWorkgroupEXT*.
///
/// The arguments are taken from the first invocation in each workgroup.
/// Any invocation must execute this instruction exactly once and under uniform
/// control flow.
/// This instruction also serves as an *OpControlBarrier* instruction, and also
/// performs and adheres to the description and semantics of an *OpControlBarrier*
/// instruction with the 'Execution' and 'Memory' operands set to *Workgroup* and
/// the 'Semantics' operand set to a combination of *WorkgroupMemory* and
/// *AcquireRelease*.
/// Ceases all further processing: Only instructions executed before
/// *OpEmitMeshTasksEXT* have observable side effects.
///
/// This instruction must be the last instruction in a block.
///
/// This instruction is only valid in the *TaskEXT* Execution Model.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpEmitMeshTasksEXT")]
#[inline]
pub unsafe fn emit_mesh_tasks_ext_payload<T>(
    group_count_x: u32,
    group_count_y: u32,
    group_count_z: u32,
    payload: &mut T,
) -> ! {
    asm! {
        "OpEmitMeshTasksEXT {group_count_x} {group_count_y} {group_count_z} {payload}",
        group_count_x = in(reg) group_count_x,
        group_count_y = in(reg) group_count_y,
        group_count_z = in(reg) group_count_z,
        payload = in(reg) payload,
        options(noreturn),
    }
}
