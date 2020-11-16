use super::val;

#[test]
fn deref_access() {
    val(r#"
use spirv_std::glam::Vec4;
use core::ops::{Deref, DerefMut};

#[allow(unused_attributes)]
#[spirv(input)]
pub struct Input<'a, T> {
    x: &'a T
}

impl<T> Deref for Input<'_, T> {
    type Target = T;
    #[inline]
    #[allow(unused_attributes)]
    #[spirv(really_unsafe_ignore_bitcasts)]
    fn deref(&self) -> &T {
        &*self.x
    }
}

#[allow(unused_attributes)]
#[spirv(output)]
pub struct Output<'a, T> {
    x: &'a mut T
}

impl<T> Deref for Output<'_, T> {
    type Target = T;
    #[inline]
    #[allow(unused_attributes)]
    #[spirv(really_unsafe_ignore_bitcasts)]
    fn deref(&self) -> &T {
        &*self.x
    }
}

impl<T> DerefMut for Output<'_, T> {
    #[inline]
    #[allow(unused_attributes)]
    #[spirv(really_unsafe_ignore_bitcasts)]
    fn deref_mut(&mut self) -> &mut T {
        &mut *self.x
    }
}

#[repr(simd)]
pub struct Dim3 {
    x: u32,
    y: u32,
    z: u32
}

#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main_fs(input: Input<[u32; 3]>, mut output: Output<Vec4>) {
    if input[0] > input[1] {
        *output.y_mut() = 1.;
    }
}

#[allow(unused_attributes)]
#[spirv(storage_buffer)]
pub struct StorageBuffer<'a, T: ?Sized> {
    x: &'a mut T
}

impl<T: ?Sized> Deref for StorageBuffer<'_, T> {
    type Target = T;
    #[inline]
    #[allow(unused_attributes)]
    #[spirv(really_unsafe_ignore_bitcasts)]
    fn deref(&self) -> &T {
        &*self.x
    }
}

impl<T: ?Sized> DerefMut for StorageBuffer<'_, T> {
    #[inline]
    #[allow(unused_attributes)]
    #[spirv(really_unsafe_ignore_bitcasts)]
    fn deref_mut(&mut self) -> &mut T {
        &mut *self.x
    }
}

#[allow(unused_attributes)]
#[spirv(gl_compute)]
pub fn main_cs(
    #[spirv(global_invocation_id)] global_id: Input<Dim3>,
    #[spirv(descriptor_set=1, binding=0)] mut y: StorageBuffer<[f32; 100]>
) {
    if (*global_id).x == 0 {
        y[0] = 1.;
    }
}
"#);
}
