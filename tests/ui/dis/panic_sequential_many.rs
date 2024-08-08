#![crate_name = "panic_sequential_many"]

// Test a long sequence of conditional panics, which has historically generated
// very nested structured control-flow (instead of a single merged chain).

// build-pass
// compile-flags: -C target-feature=+ext:SPV_KHR_non_semantic_info
// compile-flags: -C llvm-args=--abort-strategy=debug-printf
// compile-flags: -C llvm-args=--disassemble

// FIXME(eddyb) consider using such replacements also for dealing
// with `OpLine` changing all the time (esp. in libcore functions).
//
// normalize-stderr-test "; (SPIR-V|Generator: rspirv|Version: 1\.\d+|Bound: \d+)\n" -> ""
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "OpExtension .SPV_KHR_vulkan_memory_model.\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"

// FIXME(eddyb) handle this one in the test runner.
// normalize-stderr-test "\S*/lib/rustlib/" -> "$$SYSROOT/lib/rustlib/"

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(#[spirv(flat)] x: u32, #[spirv(flat)] y: u32, o: &mut u32) {
    // HACK(eddyb) this might stop working if the checks get optimized out,
    // after the first `y != 0` (which dominates the rest of the function).
    *o = x / y / y / y / y / y / y / y / y / y / y / y / y / y / y / y / y / y;
}
