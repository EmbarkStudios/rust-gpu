// build-pass

use spirv_std as _;

#[spirv(fragment)]
pub fn main(#[spirv(flat)] i: i32) {
    while i < 10 {
        break;
    }
}
