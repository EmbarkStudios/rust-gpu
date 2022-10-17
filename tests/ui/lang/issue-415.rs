// Test that zero sized unions don't ICE (even if unions are generally not supported yet)
// build-pass

use spirv_std::spirv;

union U {
    a: (),
}

#[spirv(fragment)]
pub fn main() {
    let _u = U { a: () };
}
