// Test that interface (global) `OpVariable`s mentioned by `OpEntryPoint` don't
// have to be used by the shader, for storage class inference to succeed.

// NOTE(eddyb) this test will likely become useless (won't fail without the fix)
// once we start doing the copy out of the `Input` and into a `Function`-scoped
// `OpVariable` (see #731), that's why there is another `issue-723-*` test.

// build-pass
// compile-flags: -C llvm-args=--disassemble-globals
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpExtension .SPV_KHR_vulkan_memory_model.\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"

use spirv_std as _;

#[spirv(fragment)]
pub fn main(/* unused Input */ _: [f32; 3]) {}
