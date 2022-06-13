#[cfg(target_arch = "spirv")]
use core::arch::asm;
use core::marker::PhantomData;

/// Dynamically-sized arrays in Rust carry around their length as the second half of a tuple.
/// Unfortunately, sometimes SPIR-V provides an unsized array with no way of obtaining its length.
/// Hence, this type represents something very similar to a slice, but with no way of knowing its
/// length.
#[spirv(runtime_array)]
pub struct RuntimeArray<T> {
    // spooky! this field does not exist, so if it's referenced in rust code, things will explode
    _do_not_touch: u32,
    _phantom: PhantomData<T>,
}

// It would be nice to use the Index/IndexMut traits here, but because we do not have the length of
// the array, it's impossible to make them be safe operations (indexing out of bounds), and
// Index/IndexMut are marked as safe functions.
impl<T> RuntimeArray<T> {
    /// Index the array. Unfortunately, because the length of the runtime array cannot be known,
    /// this function will happily index outside of the bounds of the array, and so is unsafe.
    ///
    /// # Safety
    /// Bounds checking is not performed, and indexing outside the bounds of the array can happen,
    /// and lead to UB.
    #[spirv_std_macros::gpu_only]
    pub unsafe fn index(&self, index: usize) -> &T {
        asm! {
            "%result = OpAccessChain _ {arr} {index}",
            "OpReturnValue %result",
            arr = in(reg) self,
            index = in(reg) index,
            options(noreturn),
        }
    }

    /// Index the array, returning a mutable reference to an element. Unfortunately, because the
    /// length of the runtime array cannot be known, this function will happily index outside of
    /// the bounds of the array, and so is unsafe.
    ///
    /// # Safety
    /// Bounds checking is not performed, and indexing outside the bounds of the array can happen,
    /// and lead to UB.
    #[spirv_std_macros::gpu_only]
    pub unsafe fn index_mut(&mut self, index: usize) -> &mut T {
        asm! {
            "%result = OpAccessChain _ {arr} {index}",
            "OpReturnValue %result",
            arr = in(reg) self,
            index = in(reg) index,
            options(noreturn),
        }
    }
}
