// build-pass

use spirv_std::spirv;
use spirv_std::{glam::Vec4, ByteAddressableBuffer};

#[spirv(matrix)]
pub struct Mat4 {
    x: Vec4,
    y: Vec4,
    z: Vec4,
    w: Vec4,
}

#[spirv(fragment)]
pub fn load(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &mut [u32],
    out: &mut Vec4,
    outmat: &mut Mat4,
) {
    unsafe {
        let buf = ByteAddressableBuffer::new(buf);
        *out = buf.load(5);
        *outmat = buf.load(5);
    }
}

#[spirv(fragment)]
pub fn store(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &mut [u32],
    val: Vec4,
    valmat: Mat4,
) {
    unsafe {
        let mut buf = ByteAddressableBuffer::new(buf);
        buf.store(5, val);
        buf.store(5, valmat);
    }
}
