// build-pass
// compile-flags: -C target-feature=+RuntimeDescriptorArray,+ext:SPV_EXT_descriptor_indexing

use spirv_std::spirv;
use spirv_std::{ByteAddressableBuffer, Image, RuntimeArray, Sampler};

#[repr(C)]
#[derive(Copy, Clone)]
pub struct TestStruct {
    vec: glam::Vec4,
    nest1: Nested,
    array: [f32; 3],
    nest2: Nested,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct Nested([u32; 3]);

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] float_in: &mut RuntimeArray<f32>,
    #[spirv(descriptor_set = 0, binding = 1, storage_buffer)] float_out: &mut f32,
    #[spirv(descriptor_set = 0, binding = 2, storage_buffer)] nested_in: &mut RuntimeArray<Nested>,
    #[spirv(descriptor_set = 0, binding = 3, storage_buffer)] nested_out: &mut Nested,
    #[spirv(descriptor_set = 2, binding = 0, storage_buffer)] buffers: &mut RuntimeArray<[u32]>,
    #[spirv(descriptor_set = 3, binding = 0, storage_buffer)] output0: &mut [u32; 4],
    #[spirv(descriptor_set = 3, binding = 1, storage_buffer)] output1: &mut glam::Vec4,
    #[spirv(descriptor_set = 3, binding = 2, storage_buffer)] output2: &mut TestStruct,
) {
    *float_out = unsafe { *float_in.index(42) };
    *nested_out = unsafe { *nested_in.index(69) };

    // *output0 = unsafe { ByteAddressableBuffer::new(buffers.index_mut(0)).load(0) };
    // *output1 = unsafe { ByteAddressableBuffer::new(buffers.index_mut(1)).load(0) };
    // *output2 = unsafe { ByteAddressableBuffer::new(buffers.index_mut(2)).load(0) };
}
