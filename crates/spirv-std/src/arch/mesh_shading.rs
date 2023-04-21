#[cfg(target_arch = "spirv")]
use core::arch::asm;

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
