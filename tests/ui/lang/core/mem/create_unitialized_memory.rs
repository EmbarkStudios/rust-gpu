// Test creating unitialized memory.
// build-pass

use spirv_std::spirv;

use core::mem::MaybeUninit;
const MAYBEI32: MaybeUninit<&i32> = MaybeUninit::<&i32>::uninit();

pub fn create_uninit_and_write() {
    let mut maybei32 = MAYBEI32;
    unsafe {
        maybei32.as_mut_ptr().write(&0);
    }
    let _maybei32 = unsafe { maybei32.assume_init() };
}

#[spirv(fragment)]
pub fn main() {}
