// build-pass

use spirv_std as _;

#[spirv(fragment)]
pub fn main(i: &i32) {
    while *i < 20 {
        while *i < 10 {
        }
    }
}
