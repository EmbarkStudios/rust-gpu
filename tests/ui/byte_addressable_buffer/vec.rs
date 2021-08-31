// build-pass

use spirv_std::{glam::Vec4, ByteAddressableBuffer};

#[spirv(fragment)]
pub fn load(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &mut [u32],
    out: &mut Vec4,
) {
    unsafe {
        let buf = ByteAddressableBuffer::new(buf);
        *out = buf.load(5);
    }
}

#[spirv(fragment)]
pub fn store(#[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &mut [u32], val: Vec4) {
    unsafe {
        let buf = ByteAddressableBuffer::new(buf);
        buf.store(5, val);
    }
}
