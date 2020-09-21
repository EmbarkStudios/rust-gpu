use crate::codegen_cx::CodegenCx;
use rspirv::spirv::{ExecutionModel, StorageClass};
use rustc_ast::ast::{AttrKind, Attribute};
use rustc_span::symbol::Symbol;
use std::collections::HashMap;

pub struct Symbols {
    pub spirv: Symbol,
    pub storage_class: Symbol,
    pub entry: Symbol,

    storage_classes: HashMap<Symbol, StorageClass>,
    execution_models: HashMap<Symbol, ExecutionModel>,
}

fn make_storage_classes() -> HashMap<Symbol, StorageClass> {
    use StorageClass::*;
    // make sure these strings stay synced with spirv-std's pointer types
    [
        ("uniform_constant", UniformConstant),
        ("input", Input),
        ("uniform", Uniform),
        ("output", Output),
        ("workgroup", Workgroup),
        ("cross_workgroup", CrossWorkgroup),
        ("private", Private),
        ("function", Function),
        ("generic", Generic),
        ("push_constant", PushConstant),
        ("atomic_counter", AtomicCounter),
        ("image", Image),
        ("storage_buffer", StorageBuffer),
        ("callable_data_nv", CallableDataNV),
        ("incoming_callable_data_nv", IncomingCallableDataNV),
        ("ray_payload_nv", RayPayloadNV),
        ("hit_attribute_nv", HitAttributeNV),
        ("incoming_ray_payload_nv", IncomingRayPayloadNV),
        ("shader_record_buffer_nv", ShaderRecordBufferNV),
        ("physical_storage_buffer", PhysicalStorageBuffer),
    ]
    .iter()
    .map(|&(a, b)| (Symbol::intern(a), b))
    .collect()
}

fn make_execution_models() -> HashMap<Symbol, ExecutionModel> {
    use ExecutionModel::*;
    [
        ("vertex", Vertex),
        ("tessellation_control", TessellationControl),
        ("tessellation_evaluation", TessellationEvaluation),
        ("geometry", Geometry),
        ("fragment", Fragment),
        ("gl_compute", GLCompute),
        ("kernel", Kernel),
        ("task_nv", TaskNV),
        ("mesh_nv", MeshNV),
        ("ray_generation_nv", RayGenerationNV),
        ("intersection_nv", IntersectionNV),
        ("any_hit_nv", AnyHitNV),
        ("closest_hit_nv", ClosestHitNV),
        ("miss_nv", MissNV),
        ("callable_nv", CallableNV),
    ]
    .iter()
    .map(|&(a, b)| (Symbol::intern(a), b))
    .collect()
}

impl Symbols {
    pub fn new() -> Self {
        Symbols {
            spirv: Symbol::intern("spirv"),
            storage_class: Symbol::intern("storage_class"),
            entry: Symbol::intern("entry"),
            storage_classes: make_storage_classes(),
            execution_models: make_execution_models(),
        }
    }

    pub fn symbol_to_storageclass(&self, sym: Symbol) -> Option<StorageClass> {
        self.storage_classes.get(&sym).copied()
    }

    pub fn symbol_to_execution_model(&self, sym: Symbol) -> Option<ExecutionModel> {
        self.execution_models.get(&sym).copied()
    }
}

pub enum SpirvAttribute {
    StorageClass(StorageClass),
    Entry(ExecutionModel),
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
    } else if arg.has_name(cx.sym.entry) {
        if let Some(storage_arg) = arg.value_str() {
            match cx.sym.symbol_to_execution_model(storage_arg) {
                Some(execution_model) => Some(SpirvAttribute::Entry(execution_model)),
                None => {
                    cx.tcx
                        .sess
                        .span_err(attr.span, "unknown spir-v execution model");
                    None
                }
            }
        } else {
            cx.tcx
                .sess
                .span_err(attr.span, "entry must have value: #[spirv(entry = \"..\")]");
            None
        }
    } else {
        cx.tcx
            .sess
            .span_err(attr.span, "unknown argument to spirv attribute");
        None
    }
}
