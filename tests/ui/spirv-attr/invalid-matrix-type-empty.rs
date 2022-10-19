// Tests that matrix type inference fails correctly, for empty struct
// build-fail

use spirv_std::spirv;

#[spirv(matrix)]
pub struct _EmptyStruct {}

#[spirv(fragment)]
pub fn _entry() {
    let _empty_struct = _EmptyStruct {};
}
