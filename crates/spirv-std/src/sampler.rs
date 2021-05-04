/// An opaque reference to settings that describe how to access, filter, or
/// sample an image.
#[spirv(sampler)]
#[derive(Copy, Clone)]
pub struct Sampler {
    _x: u32,
}
