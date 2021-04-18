#[spirv(descriptor_array)]
pub struct DescriptorArray<T: ?Sized, const LEN: usize>(core::marker::PhantomData<T>);

impl<T: ?Sized, const LEN: usize> DescriptorArray<T, LEN> {
    #[spirv_std_macros::gpu_only]
    #[spirv(descriptor_index)]
    pub unsafe fn index_unchecked(&self, _index: usize) -> &T {
        // compiler implemented
        unimplemented!()
    }
}

#[spirv(runtime_descriptor_array)]
pub struct RuntimeDescriptorArray<T: ?Sized>(core::marker::PhantomData<T>);

impl<T: ?Sized> RuntimeDescriptorArray<T> {
    #[spirv_std_macros::gpu_only]
    #[spirv(descriptor_index)]
    pub unsafe fn index_unchecked(&self, _index: usize) -> &T {
        // compiler implemented
        unimplemented!()
    }
}
