// build-pass
// only-vulkan1.2
// compile-flags: -Ctarget-feature=+MeshShadingEXT,+ext:SPV_EXT_mesh_shader

use spirv_std::arch::emit_mesh_tasks_ext;
use spirv_std::spirv;

#[spirv(task_ext(threads(1)))]
pub fn main() {
    unsafe {
        emit_mesh_tasks_ext(1, 2, 3);
    }
}
