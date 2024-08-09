#![crate_name = "panic_builtin_bounds_check"]

// Test that bounds checking panics get converted to `debugPrintf` correctly.

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

fn array_bounds_check(x: [u32; 4], i: usize) -> u32 {
    x[i]
}

#[spirv(fragment)]
pub fn main() {
    array_bounds_check([0, 1, 2, 3], 5);
}
