// build-pass

use spirv_std::{glam::Vec4, ByteAddressableBuffer};

#[spirv(fragment)]
pub fn load(#[spirv(storage_buffer)] buf: &mut [u32], out: &mut Vec4) {
    let buf = ByteAddressableBuffer::new(buf);
    *out = buf.load(5);
}

#[spirv(fragment)]
pub fn store(#[spirv(storage_buffer)] buf: &mut [u32], val: Vec4) {
    let buf = ByteAddressableBuffer::new(buf);
    buf.store(5, val);
}
