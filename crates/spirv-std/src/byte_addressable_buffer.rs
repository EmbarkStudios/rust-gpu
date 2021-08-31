use core::mem;

#[spirv(buffer_load_intrinsic)]
#[spirv_std_macros::gpu_only]
#[allow(improper_ctypes_definitions)]
unsafe extern "unadjusted" fn buffer_load_intrinsic<T>(_buffer: &[u32], _offset: u32) -> T {
    unimplemented!()
} // actually implemented in the compiler

#[spirv(buffer_store_intrinsic)]
#[spirv_std_macros::gpu_only]
#[allow(improper_ctypes_definitions)]
unsafe extern "unadjusted" fn buffer_store_intrinsic<T>(
    _buffer: &mut [u32],
    _offset: u32,
    _value: T,
) {
    unimplemented!()
} // actually implemented in the compiler

#[repr(transparent)]
pub struct ByteAddressableBuffer<'a> {
    pub data: &'a mut [u32],
}

/// `ByteAddressableBuffer` is an untyped blob of data, allowing loads and stores of arbitrary
/// basic data types at arbitrary indicies. However, all data must be aligned to size 4, each
/// element within the data (e.g. struct fields) must have a size and alignment of a multiple of 4,
/// and the `byte_index` passed to load and store must be a multiple of 4 (`byte_index` will be
/// rounded down to the nearest multiple of 4). So, it's not technically a *byte* addressable
/// buffer, but rather a *word* buffer, but this naming and behavior was inhereted from HLSL (where
/// it's UB to pass in an index not a multiple of 4).
impl<'a> ByteAddressableBuffer<'a> {
    #[inline]
    pub fn new(data: &'a mut [u32]) -> Self {
        Self { data }
    }

    /// Loads an arbitrary type from the buffer. `byte_index` must be a multiple of 4, otherwise,
    /// it will get silently rounded down to the nearest multiple of 4.
    ///
    /// # Safety
    /// This function allows writing a type to an untyped buffer, then reading a different type
    /// from the same buffer, allowing all sorts of safety guarantees to be bypassed (effectively a
    /// transmute)
    pub unsafe fn load<T>(self, byte_index: u32) -> T {
        if byte_index + mem::size_of::<T>() as u32 > self.data.len() as u32 {
            panic!("Index out of range")
        }
        buffer_load_intrinsic(self.data, byte_index)
    }

    /// Stores an arbitrary type int the buffer. `byte_index` must be a multiple of 4, otherwise,
    /// it will get silently rounded down to the nearest multiple of 4.
    ///
    /// # Safety
    /// This function allows writing a type to an untyped buffer, then reading a different type
    /// from the same buffer, allowing all sorts of safety guarantees to be bypassed (effectively a
    /// transmute)
    pub unsafe fn store<T>(self, byte_index: u32, value: T) {
        if byte_index + mem::size_of::<T>() as u32 > self.data.len() as u32 {
            panic!("Index out of range")
        }
        buffer_store_intrinsic(self.data, byte_index, value);
    }
}
