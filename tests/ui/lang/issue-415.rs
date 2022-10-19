// Test that zero sized unions don't ICE (even if unions are generally not supported yet)
// build-pass

use spirv_std as _;

union U {
    a: (),
}

#[rust_gpu::spirv(fragment)]
pub fn main() {
    let _u = U { a: () };
}
