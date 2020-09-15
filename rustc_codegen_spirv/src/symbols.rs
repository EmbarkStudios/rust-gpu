use crate::codegen_cx::CodegenCx;
use rspirv::spirv::StorageClass;
use rustc_ast::ast::{AttrKind, Attribute};
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

pub enum SpirvAttribute {
    StorageClass(StorageClass),
}

// Note that we could mark thie attr as used via cx.tcx.sess.mark_attr_used(attr), but unused reporting already happens
// even before we get here :(
/// Returns None if this attribute is not a spirv attribute, or if it's a malformed (and an error is reported).
pub fn parse_attr<'tcx>(cx: &CodegenCx<'tcx>, attr: &Attribute) -> Option<SpirvAttribute> {
    let is_spirv = match attr.kind {
        AttrKind::Normal(ref item) => {
            // TODO: We ignore the rest of the path. Is this right?
            let last = item.path.segments.last();
            last.map_or(false, |seg| seg.ident.name == cx.sym.spirv)
        }
        AttrKind::DocComment(..) => false,
    };
    if !is_spirv {
        return None;
    }
    let args = if let Some(args) = attr.meta_item_list() {
        args
    } else {
        cx.tcx
            .sess
            .span_err(attr.span, "#[spirv(..)] attribute must have one argument");
        return None;
    };
    if args.len() != 1 {
        cx.tcx
            .sess
            .span_err(attr.span, "#[spirv(..)] attribute must have one argument");
        return None;
    }
    let arg = &args[0];
    if arg.has_name(cx.sym.storage_class) {
        if let Some(storage_arg) = arg.value_str() {
            match cx.sym.symbol_to_storageclass(storage_arg) {
                Some(storage_class) => Some(SpirvAttribute::StorageClass(storage_class)),
                None => {
                    cx.tcx
                        .sess
                        .span_err(attr.span, "unknown spir-v storage class");
                    None
                }
            }
        } else {
            cx.tcx.sess.span_err(
                attr.span,
                "storage_class must have value: #[spirv(storage_class = \"..\")]",
            );
            None
        }
    } else {
        cx.tcx
            .sess
            .span_err(attr.span, "unknown argument to spirv attribute");
        None
    }
}
