// build-pass

use spirv_std as _;

#[spirv(fragment)]
pub fn main(#[spirv(flat)] i: i32) {
    if i > 0 {
    } else if i < 0 {
    } else {
    }
}
