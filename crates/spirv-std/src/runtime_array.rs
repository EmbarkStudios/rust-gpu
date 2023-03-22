#[cfg(target_arch = "spirv")]
use core::arch::asm;
use core::marker::PhantomData;

/// Dynamically-sized arrays in Rust carry around their length as the second half of a tuple.
/// Unfortunately, sometimes SPIR-V provides an unsized array with no way of obtaining its length.
/// Hence, this type represents something very similar to a slice, but with no way of knowing its
/// length.
#[spirv(runtime_array)]
// HACK(eddyb) avoids "transparent newtype of `_anti_zst_padding`" misinterpretation.
#[repr(C)]
pub struct RuntimeArray<T> {
    // HACK(eddyb) avoids the layout becoming ZST (and being elided in one way
    // or another, before `#[spirv(runtime_array)]` can special-case it).
    _anti_zst_padding: core::mem::MaybeUninit<u32>,
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
        // FIXME(eddyb) `let mut result = T::default()` uses (for `asm!`), with this.
        let mut result_slot = core::mem::MaybeUninit::uninit();
        asm! {
            "%result = OpAccessChain _ {arr} {index}",
            "OpStore {result_slot} %result",
            arr = in(reg) self,
            index = in(reg) index,
            result_slot = in(reg) result_slot.as_mut_ptr(),
        }
        result_slot.assume_init()
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
        // FIXME(eddyb) `let mut result = T::default()` uses (for `asm!`), with this.
        let mut result_slot = core::mem::MaybeUninit::uninit();
        asm! {
            "%result = OpAccessChain _ {arr} {index}",
            "OpStore {result_slot} %result",
            arr = in(reg) self,
            index = in(reg) index,
            result_slot = in(reg) result_slot.as_mut_ptr(),
        }
        result_slot.assume_init()
    }
}
