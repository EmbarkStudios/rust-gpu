// HACK(eddyb) duplicate of generic-fn-op-name.spirt.rs because only-/ignore- do not work with revisions.
// only-not_spirt

// Test that generic functions' `OpName` correctly include generic arguments.

// build-pass
// compile-flags: -C llvm-args=--disassemble-globals
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpExtension .SPV_KHR_vulkan_memory_model.\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"

#![feature(adt_const_params)]
#![allow(incomplete_features)]

use spirv_std::image::Dimensionality;
use spirv_std::spirv;

fn generic<T, const DIM: Dimensionality>() {}

#[spirv(fragment)]
pub fn main() {
    generic::<f32, { Dimensionality::TwoD }>();
}
