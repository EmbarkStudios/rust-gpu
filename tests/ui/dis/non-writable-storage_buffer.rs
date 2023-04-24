#![crate_name = "non_writable_storage_buffer"]

// Tests that only `&T` (where `T: Freeze`) storage buffers get `NonWritable`.

// build-pass
// compile-flags: -C llvm-args=--disassemble-globals
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "OpExtension .SPV_KHR_vulkan_memory_model.\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"

// FIXME(eddyb) this should use revisions to track both the `vulkan1.2` output
// and the pre-`vulkan1.2` output, but per-revisions `{only,ignore}-*` directives
// are not supported in `compiletest-rs`.
// ignore-vulkan1.2

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] buf_imm: &u32,
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] buf_mut: &mut u32,
    // FIXME(eddyb) use `AtomicU32` when methods on that work.
    #[spirv(storage_buffer, descriptor_set = 0, binding = 2)]
    buf_interior_mut: &core::cell::UnsafeCell<u32>,
) {
    let x = *buf_imm;
    *buf_mut = x;
    unsafe {
        *buf_interior_mut.get() = x;
    }
}
