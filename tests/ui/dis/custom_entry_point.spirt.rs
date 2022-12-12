// HACK(eddyb) duplicate of custom_entry_point.not_spirt.rs because only-/ignore- do not work with revisions.
// only-spirt

// build-pass
// compile-flags: -C llvm-args=--disassemble-globals
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpExtension .SPV_KHR_vulkan_memory_model.\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"

use spirv_std::spirv;

#[spirv(fragment(entry_point_name = "hello_world"))]
pub fn main() {}
