// build-pass

use spirv_std::spirv;
use spirv_std::{glam::Vec4, ByteAddressableBuffer};

#[spirv(fragment)]
pub fn load(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &mut [u32],
    out: &mut [i32; 4],
) {
    unsafe {
        let buf = ByteAddressableBuffer::new(buf);
        *out = buf.load(5);
    }
}

#[spirv(fragment)]
pub fn store(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &mut [u32],
    #[spirv(flat)] val: [i32; 4],
) {
    unsafe {
        let mut buf = ByteAddressableBuffer::new(buf);
        buf.store(5, val);
    }
}
