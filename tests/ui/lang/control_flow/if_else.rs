// build-pass

use spirv_std as _;

#[rust_gpu::spirv(fragment)]
pub fn main(#[rust_gpu::spirv(flat)] i: i32) {
    if i > 0 {
    } else {
    }
}
