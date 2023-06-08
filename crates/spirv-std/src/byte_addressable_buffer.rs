//! Container for an untyped blob of data.

use core::mem;

#[spirv(buffer_load_intrinsic)]
// HACK(eddyb) try to prevent MIR inlining from breaking our intrinsics.
#[inline(never)]
#[spirv_std_macros::gpu_only]
unsafe fn buffer_load_intrinsic<T>(
    buffer: &[u32],
    // FIXME(eddyb) should be `usize`.
    offset: u32,
) -> T {
    // NOTE(eddyb) this doesn't work with `rustc_codegen_spirv` and is only here
    // for explanatory purposes, and to cause some kind of verbose error if
    // `#[spirv(buffer_load_intrinsic)]` fails to replace calls to this function.
    buffer
        .as_ptr()
        .cast::<u8>()
        .add(offset as usize)
        .cast::<T>()
        .read()
}

#[spirv(buffer_store_intrinsic)]
// HACK(eddyb) try to prevent MIR inlining from breaking our intrinsics.
#[inline(never)]
#[spirv_std_macros::gpu_only]
unsafe fn buffer_store_intrinsic<T>(
    buffer: &mut [u32],
    // FIXME(eddyb) should be `usize`.
    offset: u32,
    value: T,
) {
    // NOTE(eddyb) this doesn't work with `rustc_codegen_spirv` and is only here
    // for explanatory purposes, and to cause some kind of verbose error if
    // `#[spirv(buffer_store_intrinsic)]` fails to replace calls to this function.
    buffer
        .as_mut_ptr()
        .cast::<u8>()
        .add(offset as usize)
        .cast::<T>()
        .write(value);
}

/// `ByteAddressableBuffer` is an untyped blob of data, allowing loads and stores of arbitrary
/// basic data types at arbitrary indicies. However, all data must be aligned to size 4, each
/// element within the data (e.g. struct fields) must have a size and alignment of a multiple of 4,
/// and the `byte_index` passed to load and store must be a multiple of 4 (`byte_index` will be
/// rounded down to the nearest multiple of 4). So, it's not technically a *byte* addressable
/// buffer, but rather a *word* buffer, but this naming and behavior was inhereted from HLSL (where
/// it's UB to pass in an index not a multiple of 4).
#[repr(transparent)]
pub struct ByteAddressableBuffer<'a> {
    /// The underlying array of bytes, able to be directly accessed.
    pub data: &'a mut [u32],
}

impl<'a> ByteAddressableBuffer<'a> {
    /// Creates a `ByteAddressableBuffer` from the untyped blob of data.
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
    pub unsafe fn load<T>(&self, byte_index: u32) -> T {
        if byte_index + mem::size_of::<T>() as u32 > self.data.len() as u32 {
            panic!("Index out of range");
        }
        buffer_load_intrinsic(self.data, byte_index)
    }

    /// Loads an arbitrary type from the buffer. `byte_index` must be a multiple of 4, otherwise,
    /// it will get silently rounded down to the nearest multiple of 4. Bounds checking is not
    /// performed.
    ///
    /// # Safety
    /// This function allows writing a type to an untyped buffer, then reading a different type
    /// from the same buffer, allowing all sorts of safety guarantees to be bypassed (effectively a
    /// transmute). Additionally, bounds checking is not performed.
    pub unsafe fn load_unchecked<T>(&self, byte_index: u32) -> T {
        buffer_load_intrinsic(self.data, byte_index)
    }

    /// Stores an arbitrary type int the buffer. `byte_index` must be a multiple of 4, otherwise,
    /// it will get silently rounded down to the nearest multiple of 4.
    ///
    /// # Safety
    /// This function allows writing a type to an untyped buffer, then reading a different type
    /// from the same buffer, allowing all sorts of safety guarantees to be bypassed (effectively a
    /// transmute)
    pub unsafe fn store<T>(&mut self, byte_index: u32, value: T) {
        if byte_index + mem::size_of::<T>() as u32 > self.data.len() as u32 {
            panic!("Index out of range");
        }
        buffer_store_intrinsic(self.data, byte_index, value);
    }

    /// Stores an arbitrary type int the buffer. `byte_index` must be a multiple of 4, otherwise,
    /// it will get silently rounded down to the nearest multiple of 4. Bounds checking is not
    /// performed.
    ///
    /// # Safety
    /// This function allows writing a type to an untyped buffer, then reading a different type
    /// from the same buffer, allowing all sorts of safety guarantees to be bypassed (effectively a
    /// transmute). Additionally, bounds checking is not performed.
    pub unsafe fn store_unchecked<T>(&mut self, byte_index: u32, value: T) {
        buffer_store_intrinsic(self.data, byte_index, value);
    }
}
