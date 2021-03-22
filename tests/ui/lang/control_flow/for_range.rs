// build-pass

use spirv_std as _;

#[spirv(fragment)]
pub fn main(i: i32) {
    for _ in 0..i {
    }
}
