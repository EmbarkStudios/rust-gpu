// build-pass
// compile-flags: -Ctarget-feature=+Geometry

use spirv_std::spirv;

#[spirv(geometry(input_lines = 2, output_points = 2))]
pub fn main() {
    unsafe {
        spirv_std::arch::emit_vertex();
    };
}
