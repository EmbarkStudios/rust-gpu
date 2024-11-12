// Test creating an array.
// build-pass

use spirv_std::macros::spirv;

#[spirv(fragment)]
pub fn main(o: &mut i16) {
    let array = [0i16; 4];
    *o = array[1];
}
