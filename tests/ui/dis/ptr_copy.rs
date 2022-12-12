// normalize-stderr-not_spirt "OpLine %8 32 1" -> "OpNoLine"

// revisions: normal via_intrinsic
//[normal] build-fail
// normalize-stderr-test "\S*/library/core/src/" -> "$$CORE_SRC/"
//[via_intrinsic] build-pass
// compile-flags: -C llvm-args=--disassemble-fn=ptr_copy::copy_via_raw_ptr

#![cfg_attr(via_intrinsic, feature(intrinsics))]

use spirv_std::spirv;

fn copy_via_raw_ptr(src: &f32, dst: &mut f32) {
    #[cfg(via_intrinsic)]
    {
        extern "rust-intrinsic" {
            fn copy<T>(src: *const T, dst: *mut T, count: usize);
        }
        unsafe { copy(src, dst, 1) }
    }

    #[cfg(normal)]
    {
        // FIXME(eddyb) `ptr::copy` doesn't currently work, and so the test uses
        // the intrinsic for success and `ptr::copy` for failure, so that it can
        // be switched back to `ptr::copy`-only when that starts succeeding,
        // likely once https://github.com/rust-lang/rust/pull/86003 is reverted
        // (and/or https://github.com/rust-lang/rust/pull/81238 is reattempted),
        // which is blocked on https://github.com/rust-lang/rust/pull/86699.
        unsafe { core::ptr::copy(src, dst, 1) }
    }
}
#[spirv(fragment)]
pub fn main(i: f32, o: &mut f32) {
    copy_via_raw_ptr(&i, o);
    // FIXME(eddyb) above call results in inlining `copy_via_raw_ptr`,
    // due to the to `Output` storage classe, so to get the disassembled
    // function we also need `Function`-local pointers:
    let (src, mut dst) = (0.0, 0.0);
    copy_via_raw_ptr(&src, &mut dst);
}
