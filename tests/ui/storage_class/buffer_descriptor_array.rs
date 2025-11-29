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
    #[spirv(descriptor_set = 0, binding = 2, storage_buffer)] test_struct_in: &mut RuntimeArray<
        TestStruct,
    >,
    #[spirv(descriptor_set = 0, binding = 3, storage_buffer)] test_struct_out: &mut TestStruct,

    #[spirv(descriptor_set = 2, binding = 0, storage_buffer)] buffers: &mut RuntimeArray<[u32]>,
    #[spirv(descriptor_set = 3, binding = 0, storage_buffer)] output0: &mut u32,
    #[spirv(descriptor_set = 3, binding = 1, storage_buffer)] output1: &mut u32,
    #[spirv(descriptor_set = 3, binding = 2, storage_buffer)] output2: &mut [u32; 4],
    #[spirv(descriptor_set = 3, binding = 3, storage_buffer)] output3: &mut glam::Vec4,
    #[spirv(descriptor_set = 3, binding = 4, storage_buffer)] output4: &mut TestStruct,
) {
    *float_out = unsafe { *float_in.index(42) };
    *test_struct_out = unsafe { *test_struct_in.index(69) };

    *output0 = unsafe { buffers.index_mut(42)[69] };
    *output1 = unsafe { ByteAddressableBuffer::new(buffers.index_mut(0)).load(64) };
    *output2 = unsafe { ByteAddressableBuffer::new(buffers.index_mut(1)).load(128) };
    *output3 = unsafe { ByteAddressableBuffer::new(buffers.index_mut(2)).load(256) };
    *output4 = unsafe { ByteAddressableBuffer::new(buffers.index_mut(3)).load(512) };
}
