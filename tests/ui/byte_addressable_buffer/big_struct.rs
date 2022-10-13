// build-pass

use spirv_std::ByteAddressableBuffer;

pub struct BigStruct {
    a: u32,
    b: u32,
    c: u32,
    d: u32,
    e: u32,
    f: u32,
}

#[rust_gpu::spirv(fragment)]
pub fn load(
    #[rust_gpu::spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &mut [u32],
    #[rust_gpu::spirv(flat)] out: &mut BigStruct,
) {
    unsafe {
        let buf = ByteAddressableBuffer::new(buf);
        *out = buf.load(5);
    }
}

#[rust_gpu::spirv(fragment)]
pub fn store(
    #[rust_gpu::spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &mut [u32],
    #[rust_gpu::spirv(flat)] val: BigStruct,
) {
    unsafe {
        let mut buf = ByteAddressableBuffer::new(buf);
        buf.store(5, val);
    }
}
