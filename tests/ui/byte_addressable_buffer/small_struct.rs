// build-pass

use spirv_std::spirv;
use spirv_std::ByteAddressableBuffer;

pub struct SmallStruct {
    a: u32,
    b: u32,
}

#[spirv(fragment)]
pub fn load(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &mut [u32],
    out: &mut SmallStruct,
) {
    unsafe {
        let buf = ByteAddressableBuffer::new(buf);
        *out = buf.load(5);
    }
}

#[spirv(fragment)]
pub fn store(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &mut [u32],
    #[spirv(flat)] a: u32,
    #[spirv(flat)] b: u32,
) {
    let val = SmallStruct { a, b };
    unsafe {
        let mut buf = ByteAddressableBuffer::new(buf);
        buf.store(5, val);
    }
}
