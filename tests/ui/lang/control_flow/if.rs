// build-pass

use spirv_std as _;

#[spirv(fragment)]
pub fn main(i: &i32) {
    if *i > 0 {

    }
}
