// build-pass
// compile-flags: -C llvm-args=--disassemble-fn=ptr_write_method::copy_via_raw_ptr

use spirv_std as _;

fn copy_via_raw_ptr(src: &f32, dst: &mut f32) {
    unsafe { (dst as *mut f32).write(*src) }
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
