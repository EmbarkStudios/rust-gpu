// build-pass

use spirv_std::ByteAddressableBuffer;

pub struct EmptyStruct {}

#[rust_gpu::spirv(fragment)]
pub fn load(
    #[rust_gpu::spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &mut [u32],
    #[rust_gpu::spirv(flat)] out: &mut EmptyStruct,
) {
    unsafe {
        let buf = ByteAddressableBuffer::new(buf);
        *out = buf.load(5);
    }
}

#[rust_gpu::spirv(fragment)]
pub fn store(#[rust_gpu::spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &mut [u32]) {
    let val = EmptyStruct {};
    unsafe {
        let mut buf = ByteAddressableBuffer::new(buf);
        buf.store(5, val);
    }
}
