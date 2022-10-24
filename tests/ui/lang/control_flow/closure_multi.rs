// build-pass

use spirv_std;
use spirv_std::spirv;

fn closure_user<F: FnMut(&u32, u32)>(ptr: &u32, xmax: u32, mut callback: F) {
    for i in 0..xmax {
        callback(ptr, i);
    }
}

#[spirv(fragment)]
pub fn main(ptr: &mut u32) {
    closure_user(ptr, 10, |ptr, i| {
        if *ptr == i {
            spirv_std::arch::kill();
        }
    });
}
