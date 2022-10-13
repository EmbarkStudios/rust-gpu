// Test that newtypes of `ScalarPair` can have references taken to their field.

// build-pass

use spirv_std as _;

struct Newtype<T>(T);

impl<T> Newtype<T> {
    fn get(&self) -> &T {
        &self.0
    }
}

impl Newtype<&[u32]> {
    fn slice_get(&self) -> &&[u32] {
        &self.0
    }
}

impl<T: core::ops::Deref<Target = [u32]>> Newtype<T> {
    fn deref_index(&self, i: usize) -> &u32 {
        &self.0[i]
    }
}

struct CustomPair(u32, u32);

#[rust_gpu::spirv(fragment)]
pub fn main(
    #[rust_gpu::spirv(descriptor_set = 0, binding = 0, storage_buffer)] slice: &[u32],
    #[rust_gpu::spirv(flat)] out: &mut u32,
) {
    let newtype_slice = Newtype(slice);
    *out = newtype_slice.get()[0];
    *out += newtype_slice.slice_get()[1];
    *out += newtype_slice.deref_index(2);

    let newtype_custom_pair = Newtype(CustomPair(*out, *out + 1));
    *out += newtype_custom_pair.get().0;
    *out += newtype_custom_pair.get().1;
}
