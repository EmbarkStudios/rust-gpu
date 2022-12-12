// HACK(eddyb) duplicate of asm_op_decorate.not_spirt.rs because only-/ignore- do not work with revisions.
// only-spirt

// build-pass
// compile-flags: -C target-feature=+RuntimeDescriptorArray,+ext:SPV_EXT_descriptor_indexing
// compile-flags: -C llvm-args=--disassemble-globals
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpExtension .SPV_KHR_vulkan_memory_model.\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"

// FIXME(eddyb) this should use revisions to track both the `vulkan1.2` output
// and the pre-`vulkan1.2` output, but per-revisions `{only,ignore}-*` directives
// are not supported in `compiletest-rs`.
// ignore-vulkan1.2

use core::arch::asm;
use spirv_std::spirv;

fn add_decorate() {
    unsafe {
        let offset = 1u32;
        asm!(
            "OpDecorate %image_2d_var DescriptorSet 0",
            "OpDecorate %image_2d_var Binding 0",
            "%uint                  = OpTypeInt 32 0",
            "%float                 = OpTypeFloat 32",
            "%uint_0                = OpConstant %uint 0",
            "%image_2d              = OpTypeImage %float Dim2D 0 0 0 1 Unknown",
            "%sampled_image_2d      = OpTypeSampledImage %image_2d",
            "%image_array           = OpTypeRuntimeArray %sampled_image_2d",
            // NOTE(eddyb) `Generic` is used here because it's the placeholder
            // for storage class inference - both of the two `OpTypePointer`
            // types below should end up inferring to `UniformConstant`.
            "%ptr_image_array       = OpTypePointer Generic %image_array",
            "%image_2d_var          = OpVariable %ptr_image_array UniformConstant",
            "%ptr_sampled_image_2d  = OpTypePointer Generic %sampled_image_2d",
            "", // ^^ type preamble
            "%offset                = OpLoad _ {0}",
            "%24                    = OpAccessChain %ptr_sampled_image_2d %image_2d_var %offset",
            "%25                    = OpLoad %sampled_image_2d %24",
            in(reg) &offset,
        );
    }
}
#[spirv(fragment)]
pub fn main() {
    add_decorate();
}
