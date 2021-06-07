use core::marker::PhantomData;

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
    #[spirv_std_macros::gpu_only]
    #[allow(clippy::empty_loop)]
    pub unsafe fn index(&self, index: usize) -> &T {
        asm! {
            "%result = OpAccessChain _ {arr} {index}",
            "OpReturnValue %result",
            "%unused = OpLabel",
            arr = in(reg) self,
            index = in(reg) index,
        }
        loop {}
    }

    #[spirv_std_macros::gpu_only]
    #[allow(clippy::empty_loop)]
    pub unsafe fn index_mut(&mut self, index: usize) -> &mut T {
        asm! {
            "%result = OpAccessChain _ {arr} {index}",
            "OpReturnValue %result",
            "%unused = OpLabel",
            arr = in(reg) self,
            index = in(reg) index,
        }
        loop {}
    }
}

impl RuntimeArray<u32> {
    #[spirv(internal_buffer_load)]
    #[spirv_std_macros::gpu_only]
    pub extern "unadjusted" fn load<T>(&self, _offset: u32) -> T {
        unimplemented!()
    } // actually implemented in the compiler

    #[spirv(internal_buffer_store)]
    #[spirv_std_macros::gpu_only]
    pub unsafe extern "unadjusted" fn store<T>(
        &mut self,
        _offset: u32,
        _value: T,
    ) {
        unimplemented!()
    } // actually implemented in the compiler
}
