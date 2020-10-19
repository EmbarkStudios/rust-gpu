use crate::codegen_cx::CodegenCx;
use rspirv::spirv::{BuiltIn, ExecutionModel, StorageClass};
use rustc_ast::ast::{AttrKind, Attribute};
use rustc_span::symbol::Symbol;
use std::collections::HashMap;

/// Various places in the codebase (mostly attribute parsing) need to compare rustc Symbols to particular keywords.
/// Symbols are interned, as in, they don't actually store the string itself inside them, but rather an index into a
/// global table of strings. Then, whenever a new Symbol is created, the global table is checked to see if the string
/// already exists, deduplicating it if so. This makes things like comparison and cloning really cheap. So, this struct
/// is to allocate all our keywords up front and intern them all, so we can do comparisons really easily and fast.
pub struct Symbols {
    pub spirv: Symbol,
    pub spirv_std: Symbol,
    pub kernel: Symbol,
    pub builtin: Symbol,
    pub storage_class: Symbol,
    pub entry: Symbol,
    pub really_unsafe_ignore_bitcasts: Symbol,

    builtins: HashMap<Symbol, BuiltIn>,
    storage_classes: HashMap<Symbol, StorageClass>,
    execution_models: HashMap<Symbol, ExecutionModel>,
}

fn make_builtins() -> HashMap<Symbol, BuiltIn> {
    use BuiltIn::*;
    [
        ("position", Position),
        ("point_size", PointSize),
        ("clip_distance", ClipDistance),
        ("cull_distance", CullDistance),
        ("vertex_id", VertexId),
        ("instance_id", InstanceId),
        ("primitive_id", PrimitiveId),
        ("invocation_id", InvocationId),
        ("layer", Layer),
        ("viewport_index", ViewportIndex),
        ("tess_level_outer", TessLevelOuter),
        ("tess_level_inner", TessLevelInner),
        ("tess_coord", TessCoord),
        ("patch_vertices", PatchVertices),
        ("frag_coord", FragCoord),
        ("point_coord", PointCoord),
        ("front_facing", FrontFacing),
        ("sample_id", SampleId),
        ("sample_position", SamplePosition),
        ("sample_mask", SampleMask),
        ("frag_depth", FragDepth),
        ("helper_invocation", HelperInvocation),
        ("num_workgroups", NumWorkgroups),
        ("workgroup_size", WorkgroupSize),
        ("workgroup_id", WorkgroupId),
        ("local_invocation_id", LocalInvocationId),
        ("global_invocation_id", GlobalInvocationId),
        ("local_invocation_index", LocalInvocationIndex),
        ("work_dim", WorkDim),
        ("global_size", GlobalSize),
        ("enqueued_workgroup_size", EnqueuedWorkgroupSize),
        ("global_offset", GlobalOffset),
        ("global_linear_id", GlobalLinearId),
        ("subgroup_size", SubgroupSize),
        ("subgroup_max_size", SubgroupMaxSize),
        ("num_subgroups", NumSubgroups),
        ("num_enqueued_subgroups", NumEnqueuedSubgroups),
        ("subgroup_id", SubgroupId),
        ("subgroup_local_invocation_id", SubgroupLocalInvocationId),
        ("vertex_index", VertexIndex),
        ("instance_index", InstanceIndex),
        ("subgroup_eq_mask", SubgroupEqMask),
        ("subgroup_ge_mask", SubgroupGeMask),
        ("subgroup_gt_mask", SubgroupGtMask),
        ("subgroup_le_mask", SubgroupLeMask),
        ("subgroup_lt_mask", SubgroupLtMask),
        ("base_vertex", BaseVertex),
        ("base_instance", BaseInstance),
        ("draw_index", DrawIndex),
        ("device_index", DeviceIndex),
        ("view_index", ViewIndex),
        ("bary_coord_no_persp_amd", BaryCoordNoPerspAMD),
        (
            "bary_coord_no_persp_centroid_amd",
            BaryCoordNoPerspCentroidAMD,
        ),
        ("bary_coord_no_persp_sample_amd", BaryCoordNoPerspSampleAMD),
        ("bary_coord_smooth_amd", BaryCoordSmoothAMD),
        ("bary_coord_smooth_centroid_amd", BaryCoordSmoothCentroidAMD),
        ("bary_coord_smooth_sample_amd", BaryCoordSmoothSampleAMD),
        ("bary_coord_pull_model_amd", BaryCoordPullModelAMD),
        ("frag_stencil_ref_ext", FragStencilRefEXT),
        ("viewport_mask_nv", ViewportMaskNV),
        ("secondary_position_nv", SecondaryPositionNV),
        ("secondary_viewport_mask_nv", SecondaryViewportMaskNV),
        ("position_per_view_nv", PositionPerViewNV),
        ("viewport_mask_per_view_nv", ViewportMaskPerViewNV),
        ("fully_covered_ext", FullyCoveredEXT),
        ("task_count_nv", TaskCountNV),
        ("primitive_count_nv", PrimitiveCountNV),
        ("primitive_indices_nv", PrimitiveIndicesNV),
        ("clip_distance_per_view_nv", ClipDistancePerViewNV),
        ("cull_distance_per_view_nv", CullDistancePerViewNV),
        ("layer_per_viewNV", LayerPerViewNV),
        ("mesh_view_count_nv", MeshViewCountNV),
        ("mesh_view_indices_nv", MeshViewIndicesNV),
        ("bary_coord_nv", BaryCoordNV),
        ("bary_coord_no_persp_nv", BaryCoordNoPerspNV),
        ("frag_size_ext", FragSizeEXT),
        ("frag_invocation_count_ext", FragInvocationCountEXT),
        ("launch_id_nv", LaunchIdNV),
        ("launch_size_nv", LaunchSizeNV),
        ("world_ray_origin_nv", WorldRayOriginNV),
        ("world_ray_direction_nv", WorldRayDirectionNV),
        ("object_ray_origin_nv", ObjectRayOriginNV),
        ("object_ray_direction_nv", ObjectRayDirectionNV),
        ("ray_tmin_nv", RayTminNV),
        ("ray_tmax_nv", RayTmaxNV),
        ("instance_custom_index_nv", InstanceCustomIndexNV),
        ("object_to_world_nv", ObjectToWorldNV),
        ("world_to_object_nv", WorldToObjectNV),
        ("hit_t_nv", HitTNV),
        ("hit_kind_nv", HitKindNV),
        ("incoming_ray_flags_nv", IncomingRayFlagsNV),
        ("ray_geometry_index_khr", RayGeometryIndexKHR),
        ("warps_per_sm_nv", WarpsPerSMNV),
        ("sm_count_nv", SMCountNV),
        ("warp_id_nv", WarpIDNV),
        ("SMIDNV", SMIDNV),
    ]
    .iter()
    .map(|&(a, b)| (Symbol::intern(a), b))
    .collect()
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
        ("callable_data_khr", StorageClass::CallableDataKHR),
        (
            "incoming_callable_data_khr",
            StorageClass::IncomingCallableDataKHR,
        ),
        ("ray_payload_khr", StorageClass::RayPayloadKHR),
        ("hit_attribute_khr", StorageClass::HitAttributeKHR),
        (
            "incoming_ray_payload_khr",
            StorageClass::IncomingRayPayloadKHR,
        ),
        (
            "shader_record_buffer_khr",
            StorageClass::ShaderRecordBufferKHR,
        ),
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
        Self {
            spirv: Symbol::intern("spirv"),
            spirv_std: Symbol::intern("spirv_std"),
            kernel: Symbol::intern("kernel"),
            builtin: Symbol::intern("builtin"),
            storage_class: Symbol::intern("storage_class"),
            entry: Symbol::intern("entry"),
            really_unsafe_ignore_bitcasts: Symbol::intern("really_unsafe_ignore_bitcasts"),
            builtins: make_builtins(),
            storage_classes: make_storage_classes(),
            execution_models: make_execution_models(),
        }
    }

    pub fn symbol_to_builtin(&self, sym: Symbol) -> Option<BuiltIn> {
        self.builtins.get(&sym).copied()
    }

    pub fn symbol_to_storageclass(&self, sym: Symbol) -> Option<StorageClass> {
        self.storage_classes.get(&sym).copied()
    }

    pub fn symbol_to_execution_model(&self, sym: Symbol) -> Option<ExecutionModel> {
        self.execution_models.get(&sym).copied()
    }
}

pub enum SpirvAttribute {
    Builtin(BuiltIn),
    StorageClass(StorageClass),
    Entry(ExecutionModel),
    ReallyUnsafeIgnoreBitcasts,
}

// Note that we could mark the attr as used via cx.tcx.sess.mark_attr_used(attr), but unused reporting already happens
// even before we get here :(
/// Returns None if this attribute is not a spirv attribute, or if it's malformed (and an error is reported).
pub fn parse_attr<'tcx>(cx: &CodegenCx<'tcx>, attr: &Attribute) -> Option<SpirvAttribute> {
    // Example attributes that we parse here:
    // #[spirv(storage_class = "uniform")]
    // #[spirv(entry = "kernel")]
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
    if arg.has_name(cx.sym.builtin) {
        if let Some(builtin_arg) = arg.value_str() {
            match cx.sym.symbol_to_builtin(builtin_arg) {
                Some(builtin) => Some(SpirvAttribute::Builtin(builtin)),
                None => {
                    cx.tcx.sess.span_err(attr.span, "unknown spir-v builtin");
                    None
                }
            }
        } else {
            cx.tcx.sess.span_err(
                attr.span,
                "builtin must have value: #[spirv(builtin = \"..\")]",
            );
            None
        }
    } else if arg.has_name(cx.sym.storage_class) {
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
    } else if arg.has_name(cx.sym.really_unsafe_ignore_bitcasts) {
        Some(SpirvAttribute::ReallyUnsafeIgnoreBitcasts)
    } else {
        cx.tcx
            .sess
            .span_err(attr.span, "unknown argument to spirv attribute");
        None
    }
}
