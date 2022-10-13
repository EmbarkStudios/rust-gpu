// build-pass

use spirv_std as _;

#[rust_gpu::spirv(fragment)]
pub fn main(#[rust_gpu::spirv(flat)] i: i32) {
    while i < 20 {
        while i < 10 {
            if i > 10 {
                break;
            }
        }
    }
}
