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

#[spirv(fragment)]
pub fn load(#[spirv(storage_buffer)] buf: &mut [u32], out: &mut BigStruct) {
    let buf = ByteAddressableBuffer::new(buf);
    *out = buf.load(5);
}

#[spirv(fragment)]
pub fn store(#[spirv(storage_buffer)] buf: &mut [u32], val: BigStruct) {
    let buf = ByteAddressableBuffer::new(buf);
    buf.store(5, val);
}
