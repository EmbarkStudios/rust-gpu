use rspirv::spirv::StorageClass;
use rustc_span::symbol::Symbol;

pub struct Symbols {
    pub spirv: Symbol,
    pub storage_class: Symbol,

    // storage classes
    pub uniform_constant: Symbol,
    pub input: Symbol,
    pub uniform: Symbol,
    pub output: Symbol,
    pub workgroup: Symbol,
    pub cross_workgroup: Symbol,
    pub private: Symbol,
    pub function: Symbol,
    pub generic: Symbol,
    pub push_constant: Symbol,
    pub atomic_counter: Symbol,
    pub image: Symbol,
    pub storage_buffer: Symbol,
    pub callable_data_nv: Symbol,
    pub incoming_callable_data_nv: Symbol,
    pub ray_payload_nv: Symbol,
    pub hit_attribute_nv: Symbol,
    pub incoming_ray_payload_nv: Symbol,
    pub shader_record_buffer_nv: Symbol,
    pub physical_storage_buffer: Symbol,
}

impl Symbols {
    pub fn new() -> Self {
        Symbols {
            spirv: Symbol::intern("spirv"),
            storage_class: Symbol::intern("storage_class"),

            uniform_constant: Symbol::intern("uniform_constant"),
            input: Symbol::intern("input"),
            uniform: Symbol::intern("uniform"),
            output: Symbol::intern("output"),
            workgroup: Symbol::intern("workgroup"),
            cross_workgroup: Symbol::intern("cross_workgroup"),
            private: Symbol::intern("private"),
            function: Symbol::intern("function"),
            generic: Symbol::intern("generic"),
            push_constant: Symbol::intern("push_constant"),
            atomic_counter: Symbol::intern("atomic_counter"),
            image: Symbol::intern("image"),
            storage_buffer: Symbol::intern("storage_buffer"),
            callable_data_nv: Symbol::intern("callable_data_nv"),
            incoming_callable_data_nv: Symbol::intern("incoming_callable_data_nv"),
            ray_payload_nv: Symbol::intern("ray_payload_nv"),
            hit_attribute_nv: Symbol::intern("hit_attribute_nv"),
            incoming_ray_payload_nv: Symbol::intern("incoming_ray_payload_nv"),
            shader_record_buffer_nv: Symbol::intern("shader_record_buffer_nv"),
            physical_storage_buffer: Symbol::intern("physical_storage_buffer"),
        }
    }

    pub fn symbol_to_storageclass(&self, sym: Symbol) -> Option<StorageClass> {
        let result = if sym == self.uniform_constant {
            StorageClass::UniformConstant
        } else if sym == self.input {
            StorageClass::Input
        } else if sym == self.uniform {
            StorageClass::Uniform
        } else if sym == self.output {
            StorageClass::Output
        } else if sym == self.workgroup {
            StorageClass::Workgroup
        } else if sym == self.cross_workgroup {
            StorageClass::CrossWorkgroup
        } else if sym == self.private {
            StorageClass::Private
        } else if sym == self.function {
            StorageClass::Function
        } else if sym == self.generic {
            StorageClass::Generic
        } else if sym == self.push_constant {
            StorageClass::PushConstant
        } else if sym == self.atomic_counter {
            StorageClass::AtomicCounter
        } else if sym == self.image {
            StorageClass::Image
        } else if sym == self.storage_buffer {
            StorageClass::StorageBuffer
        } else if sym == self.callable_data_nv {
            StorageClass::CallableDataNV
        } else if sym == self.incoming_callable_data_nv {
            StorageClass::IncomingCallableDataNV
        } else if sym == self.ray_payload_nv {
            StorageClass::RayPayloadNV
        } else if sym == self.hit_attribute_nv {
            StorageClass::HitAttributeNV
        } else if sym == self.incoming_ray_payload_nv {
            StorageClass::IncomingRayPayloadNV
        } else if sym == self.shader_record_buffer_nv {
            StorageClass::ShaderRecordBufferNV
        } else if sym == self.physical_storage_buffer {
            StorageClass::PhysicalStorageBuffer
        } else {
            return None;
        };
        Some(result)
    }
}
