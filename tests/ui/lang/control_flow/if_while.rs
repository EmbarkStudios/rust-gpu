// build-pass

use spirv_std as _;

#[spirv(fragment)]
pub fn main(i: i32) {
    if i == 0 {
        while i < 10 {
        }
    }
}
