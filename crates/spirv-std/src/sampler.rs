/// An opaque reference to settings that describe how to access, filter, or
/// sample an image.
#[spirv(sampler)]
#[derive(Copy, Clone)]
// HACK(eddyb) avoids "transparent newtype of `_anti_zst_padding`" misinterpretation.
#[repr(C)]
pub struct Sampler {
    // HACK(eddyb) avoids the layout becoming ZST (and being elided in one way
    // or another, before `#[spirv(sampler)]` can special-case it).
    _anti_zst_padding: core::mem::MaybeUninit<u32>,
}
