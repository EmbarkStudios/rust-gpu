use spirv_tools_sys::shared;

pub struct Binary {
    inner: *mut shared::Binary,
}

impl Binary {
    pub(crate) fn new(bin: *mut shared::Binary) -> Self {
        Self { inner: bin }
    }
}

impl AsRef<[u32]> for Binary {
    fn as_ref(&self) -> &[u32] {
        unsafe { std::slice::from_raw_parts((*self.inner).code, (*self.inner).size) }
    }
}

impl AsRef<[u8]> for Binary {
    fn as_ref(&self) -> &[u8] {
        unsafe {
            std::slice::from_raw_parts(
                (*self.inner).code as *const u8,
                (*self.inner).size * std::mem::size_of::<u32>(),
            )
        }
    }
}

impl Drop for Binary {
    fn drop(&mut self) {
        unsafe {
            shared::binary_destroy(self.inner);
        }
    }
}
