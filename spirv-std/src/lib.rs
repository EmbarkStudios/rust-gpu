#![no_std]
#![feature(register_attr)]
#![register_attr(spirv)]

#[allow(unused_attributes)]
#[spirv(storage_class = "private")]
pub struct Private<'a, T> {
    x: &'a mut T,
}

impl<'a, T: Copy> Private<'a, T> {
    #[inline]
    pub fn load(&self) -> T {
        *self.x
    }

    #[inline]
    pub fn store(&mut self, v: T) {
        *self.x = v
    }
}

#[allow(unused_attributes)]
#[spirv(storage_class = "workgroup")]
pub struct Workgroup<'a, T> {
    x: &'a mut T,
}

impl<'a, T: Copy> Workgroup<'a, T> {
    #[inline]
    pub fn load(&self) -> T {
        *self.x
    }

    #[inline]
    pub fn store(&mut self, v: T) {
        *self.x = v
    }
}
