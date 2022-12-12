// normalize-stderr-not_spirt "OpLine %11 10 1" -> "OpNoLine"

// build-pass
// compile-flags: -C llvm-args=--disassemble-fn=ptr_read::copy_via_raw_ptr

use spirv_std::spirv;

fn copy_via_raw_ptr(src: &f32, dst: &mut f32) {
    unsafe { *dst = core::ptr::read(src) }
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
