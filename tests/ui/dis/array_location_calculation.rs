// build-pass
// compile-flags: -C llvm-args=--disassemble-globals
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpExtension .SPV_KHR_vulkan_memory_model.\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"
// normalize-stderr-test "OpMemberName %12 0 .0.\n" -> ""

use spirv_std::{
    self as _,
    glam::{DVec3, IVec4, Mat3, Vec4},
};

#[spirv(matrix)]
pub struct Mat4x3 {
    pub col_0: Vec4,
    pub col_1: Vec4,
    pub col_2: Vec4,
}

#[spirv(fragment)]
pub fn main(
    one: [f32; 7],
    two: [f32; 3],
    three: Mat3,
    four: DVec3,
    five: IVec4,
    six: f32,
    seven: Mat4x3,
    eight: u32,
) {
}
