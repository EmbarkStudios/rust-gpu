// build-pass

use spirv_std::spirv;
use spirv_std::{glam::Vec2, ByteAddressableBuffer};

pub struct Complex {
    x: u32,
    y: f32,
    n: Nesty,
    v: Vec2,
    a: [f32; 7],
    m: [Nesty; 2],
}

pub struct Nesty {
    x: f32,
    y: f32,
    z: f32,
}

#[spirv(fragment)]
pub fn load(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &mut [u32],
    out: &mut Nesty,
) {
    unsafe {
        let buf = ByteAddressableBuffer::new(buf);
        *out = buf.load(5);
    }
}

#[spirv(fragment)]
pub fn store(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &mut [u32],
    val: Nesty,
) {
    unsafe {
        let mut buf = ByteAddressableBuffer::new(buf);
        buf.store(5, val);
    }
}
