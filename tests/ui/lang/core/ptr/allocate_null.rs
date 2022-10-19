// Tests allocating a null pointer at `const` time.
// build-pass

use spirv_std::spirv;

use core::ptr::null;
const NULL_PTR: *const i32 = null();

#[spirv(fragment)]
pub fn main() {
    let _null_ptr = NULL_PTR;
}
