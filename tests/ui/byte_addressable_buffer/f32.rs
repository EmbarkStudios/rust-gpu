// build-pass

use spirv_std::ByteAddressableBuffer;

#[spirv(fragment)]
pub fn load(#[spirv(storage_buffer)] buf: &mut [u32], out: &mut f32) {
    let buf = ByteAddressableBuffer::new(buf);
    *out = buf.load(5);
}

#[spirv(fragment)]
pub fn store(#[spirv(storage_buffer)] buf: &mut [u32], val: f32) {
    let buf = ByteAddressableBuffer::new(buf);
    buf.store(5, val);
}
