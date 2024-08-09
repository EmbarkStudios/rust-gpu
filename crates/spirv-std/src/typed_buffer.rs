#[cfg(target_arch = "spirv")]
use core::arch::asm;
use core::marker::PhantomData;
use core::ops::{Deref, DerefMut};

/// Explicit (uniform/storage) buffer handle for descriptor indexing.
///
/// Examples (for an `#[spirv(storage_buffer)]`-annotated entry-point parameter):
/// - `buffer: &[u32]` (implicit, 1 buffer)
/// - `buffer: &TypedBuffer<[u32]>` (explicit, one buffer)
/// - `buffers: &RuntimeArray<TypedBuffer<[u32]>>` (explicit, many buffers)
//
// TODO(eddyb) fully document!
#[spirv(typed_buffer)]
// HACK(eddyb) avoids "transparent newtype of `_anti_zst_padding`" misinterpretation.
#[repr(C)]
pub struct TypedBuffer<T: ?Sized> {
    // HACK(eddyb) avoids the layout becoming ZST (and being elided in one way
    // or another, before `#[spirv(runtime_array)]` can special-case it).
    _anti_zst_padding: core::mem::MaybeUninit<u32>,
    _phantom: PhantomData<T>,
}

impl<T> Deref for TypedBuffer<T> {
    type Target = T;
    #[spirv_std_macros::gpu_only]
    fn deref(&self) -> &T {
        unsafe {
            let mut result_slot = core::mem::MaybeUninit::uninit();
            asm! {
                "%uint = OpTypeInt 32 0",
                "%uint_0 = OpConstant %uint 0",
                "%result = OpAccessChain _ {buffer} %uint_0",
                "OpStore {result_slot} %result",
                buffer = in(reg) self,
                result_slot = in(reg) result_slot.as_mut_ptr(),
            }
            result_slot.assume_init()
        }
    }
}

impl<T> DerefMut for TypedBuffer<T> {
    #[spirv_std_macros::gpu_only]
    fn deref_mut(&mut self) -> &mut T {
        unsafe {
            let mut result_slot = core::mem::MaybeUninit::uninit();
            asm! {
                "%uint = OpTypeInt 32 0",
                "%uint_0 = OpConstant %uint 0",
                "%result = OpAccessChain _ {buffer} %uint_0",
                "OpStore {result_slot} %result",
                buffer = in(reg) self,
                result_slot = in(reg) result_slot.as_mut_ptr(),
            }
            result_slot.assume_init()
        }
    }
}

impl<T> Deref for TypedBuffer<[T]> {
    type Target = [T];
    #[spirv_std_macros::gpu_only]
    fn deref(&self) -> &[T] {
        unsafe {
            let mut result_slot = core::mem::MaybeUninit::uninit();
            asm! {
                "%uint = OpTypeInt 32 0",
                "%uint_0 = OpConstant %uint 0",
                "%inner_ptr = OpAccessChain _ {buffer} %uint_0",
                "%inner_len = OpArrayLength %uint {buffer} 0",
                "%result = OpCompositeConstruct typeof*{result_slot} %inner_ptr %inner_len",
                "OpStore {result_slot} %result",
                buffer = in(reg) self,
                result_slot = in(reg) result_slot.as_mut_ptr(),
            }
            result_slot.assume_init()
        }
    }
}

impl<T> DerefMut for TypedBuffer<[T]> {
    #[spirv_std_macros::gpu_only]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {
            let mut result_slot = core::mem::MaybeUninit::uninit();
            asm! {
                "%uint = OpTypeInt 32 0",
                "%uint_0 = OpConstant %uint 0",
                "%inner_ptr = OpAccessChain _ {buffer} %uint_0",
                "%inner_len = OpArrayLength %uint {buffer} 0",
                "%result = OpCompositeConstruct typeof*{result_slot} %inner_ptr %inner_len",
                "OpStore {result_slot} %result",
                buffer = in(reg) self,
                result_slot = in(reg) result_slot.as_mut_ptr(),
            }
            result_slot.assume_init()
        }
    }
}
