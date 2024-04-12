// Test creating an array.
// build-pass

use spirv_std::macros::spirv;

#[spirv(fragment)]
pub fn main(o: &mut i8) {
    let array = [0i8; 4];
    *o = array[1];
}
