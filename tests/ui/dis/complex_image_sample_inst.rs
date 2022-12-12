// normalize-stderr-not_spirt "OpLine %12 60 1" -> "OpNoLine"

// build-pass
// compile-flags: -Ctarget-feature=+RuntimeDescriptorArray,+ext:SPV_EXT_descriptor_indexing
// compile-flags: -C llvm-args=--disassemble-fn=complex_image_sample_inst::sample_proj_lod

use core::arch::asm;
use spirv_std::spirv;

fn sample_proj_lod(
    coord: glam::Vec4,
    ddx: glam::Vec2,
    ddy: glam::Vec2,
    offset_x: i32,
    offset_y: i32,
) -> glam::Vec4 {
    unsafe {
        let mut result = glam::Vec4::default();
        let index = 0u32;
        asm!(
            "OpDecorate %image_2d_var DescriptorSet 0",
            "OpDecorate %image_2d_var Binding 0",
            "%uint                  = OpTypeInt 32 0",
            "%int                   = OpTypeInt 32 1",
            "%float                 = OpTypeFloat 32",
            "%v2int                 = OpTypeVector %int 2",
            "%uint_0                = OpConstant %uint 0",
            "%int_0                 = OpConstant %int 0",
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
            "%offset                = OpLoad _ {1}",
            "%24                    = OpAccessChain %ptr_sampled_image_2d %image_2d_var %offset",
            "%25                    = OpLoad %sampled_image_2d %24",
            "%coord                 = OpLoad _ {0}",
            "%ddx                   = OpLoad _ {3}",
            "%ddy                   = OpLoad _ {4}",
            "%offset_x              = OpLoad _ {5}",
            "%offset_y              = OpLoad _ {6}",
            "%const_offset          = OpConstantComposite %v2int %int_0 %int_0",
            "%result                = OpImageSampleProjExplicitLod _ %25 %coord Grad|ConstOffset %ddx %ddy %const_offset",
            "OpStore {2} %result",
            in(reg) &coord,
            in(reg) &index,
            in(reg) &mut result,
            in(reg) &ddx,
            in(reg) &ddy,
            in(reg) &offset_x,
            in(reg) &offset_y,
        );
        result
    }
}
#[spirv(fragment)]
pub fn main() {
    sample_proj_lod(glam::Vec4::ZERO, glam::Vec2::ZERO, glam::Vec2::ZERO, 0, 0);
}
