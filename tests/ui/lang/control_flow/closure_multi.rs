// build-pass

use spirv_std;

fn closure_user<F: FnMut(&u32, u32)>(ptr: &u32, xmax: u32, mut callback: F) {
    for i in 0..xmax {
        callback(ptr, i);
    }
}

#[rust_gpu::spirv(fragment)]
pub fn main(#[rust_gpu::spirv(flat)] ptr: &mut u32) {
    closure_user(ptr, 10, |ptr, i| {
        if *ptr == i {
            spirv_std::arch::kill();
        }
    });
}
