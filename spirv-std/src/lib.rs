#![no_std]
#![feature(register_attr)]
#![register_attr(spirv)]

macro_rules! pointer_addrspace {
    ($storage_class:literal, $type_name:ident) => {
        #[allow(unused_attributes)]
        #[spirv(storage_class = $storage_class)]
        pub struct $type_name<'a, T> {
            x: &'a mut T,
        }

        impl<'a, T: Copy> $type_name<'a, T> {
            #[inline]
            pub fn load(&self) -> T {
                *self.x
            }

            #[inline]
            pub fn store(&mut self, v: T) {
                *self.x = v
            }
        }
    };
}

// Make sure these strings stay synced with symbols.rs
// Note the type names don't have to match anything, they can be renamed (only the string must match)
pointer_addrspace!("uniform_constant", UniformConstant);
pointer_addrspace!("input", Input);
pointer_addrspace!("uniform", Uniform);
pointer_addrspace!("output", Output);
pointer_addrspace!("workgroup", Workgroup);
pointer_addrspace!("cross_workgroup", CrossWorkgroup);
pointer_addrspace!("private", Private);
pointer_addrspace!("function", Function);
pointer_addrspace!("generic", Generic);
pointer_addrspace!("push_constant", PushConstant);
pointer_addrspace!("atomic_counter", AtomicCounter);
pointer_addrspace!("image", Image);
pointer_addrspace!("storage_buffer", StorageBuffer);
pointer_addrspace!("callable_data_nv", CallableDataNV);
pointer_addrspace!("incoming_callable_data_nv", IncomingCallableDataNV);
pointer_addrspace!("ray_payload_nv", RayPayloadNV);
pointer_addrspace!("hit_attribute_nv", HitAttributeNV);
pointer_addrspace!("incoming_ray_payload_nv", IncomingRayPayloadNV);
pointer_addrspace!("shader_record_buffer_nv", ShaderRecordBufferNV);
pointer_addrspace!("physical_storage_buffer", PhysicalStorageBuffer);
