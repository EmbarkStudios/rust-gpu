// build-pass

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(#[spirv(flat)] i: u32) {
    for _ in 0..i {}
}
