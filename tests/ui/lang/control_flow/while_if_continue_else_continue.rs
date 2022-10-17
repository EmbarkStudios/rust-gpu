// build-pass

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(#[spirv(flat)] i: i32) {
    while i < 10 {
        if i == 0 {
            continue;
        } else {
            continue;
        }
    }
}
