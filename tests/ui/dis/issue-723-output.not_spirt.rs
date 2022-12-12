// HACK(eddyb) duplicate of issue-723-output.spirt.rs because only-/ignore- do not work with revisions.
// only-not_spirt

// Test that interface (global) `OpVariable`s mentioned by `OpEntryPoint` don't
// have to be used by the shader, for storage class inference to succeed.

// NOTE(eddyb) this relies on two subtleties (in order to fail without the fix):
// * disabling debuginfo (to prevent `%x.dbg.spill` stack slot generation)
//   * this could be alleviated in the future if we clean up how debuginfo
//     is handled in `rustc_codegen_ssa`, to not assume LLVM limitations
//   * it probably needs to stay like this, to ensure the parameter is unused
// * `Output`s being handled with `&mut`, instead of by-value returns
//   * if this changes, this test will likely need >=1.4 SPIR-V, which supports
//     all interface `OpVariables` in `OpEntryPoint`, not just `Input`/`Output`

// build-pass
// compile-flags: -C debuginfo=0 -C llvm-args=--disassemble-globals
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpExtension .SPV_KHR_vulkan_memory_model.\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(/* unused Output */ _: &mut glam::Vec4) {}
