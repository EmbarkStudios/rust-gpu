// build-pass
// only-vulkan1.2
// compile-flags: -Ctarget-feature=+MeshShadingEXT,+ext:SPV_EXT_mesh_shader

use spirv_std::arch::set_mesh_outputs_ext;
use spirv_std::glam::{UVec2, Vec4};
use spirv_std::spirv;

#[spirv(mesh_ext(
    threads(1),
    output_vertices = 2,
    output_primitives_ext = 1,
    output_lines_ext
))]
pub fn main(
    #[spirv(position)] positions: &mut [Vec4; 2],
    #[spirv(primitive_line_indices_ext)] indices: &mut [UVec2; 1],
) {
    unsafe {
        set_mesh_outputs_ext(2, 1);
    }

    positions[0] = Vec4::new(-0.5, 0.5, 0.0, 1.0);
    positions[1] = Vec4::new(0.5, 0.5, 0.0, 1.0);

    indices[0] = UVec2::new(0, 1);
}
