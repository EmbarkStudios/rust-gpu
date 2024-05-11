// build-pass
// only-vulkan1.2
// compile-flags: -Ctarget-feature=+MeshShadingEXT,+ext:SPV_EXT_mesh_shader

use spirv_std::arch::emit_mesh_tasks_ext_payload;
use spirv_std::spirv;

pub struct Payload {
    pub first: u32,
    pub second: i32,
}

#[spirv(task_ext(threads(1)))]
pub fn main(#[spirv(task_payload_workgroup_ext)] payload: &mut Payload) {
    payload.first = 1;
    payload.second = 2;

    unsafe {
        emit_mesh_tasks_ext_payload(3, 4, 5, payload);
    }
}
