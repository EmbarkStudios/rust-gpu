// build-pass
// compile-flags: -C llvm-args=--disassemble-fn=unroll_loops::java_hash_ten_times

use spirv_std as _;

#[spirv(unroll_loops)]
fn java_hash_ten_times(mut x: u32, y: u32) -> u32 {
    let mut i = 0;
    while i < 10 {
        x = 31 * x + y;
        i += 1;
    }
    x
}
#[spirv(fragment)]
pub fn main() {
    java_hash_ten_times(7, 42);
}
