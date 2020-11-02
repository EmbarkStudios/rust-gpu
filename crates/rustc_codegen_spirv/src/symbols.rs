use crate::codegen_cx::CodegenCx;
use rspirv::spirv::{BuiltIn, ExecutionModel, StorageClass};
use rustc_ast::ast::{AttrKind, Attribute, Lit, LitIntType, LitKind, NestedMetaItem};
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
    pub simple: Symbol,
    pub vulkan: Symbol,
    pub glsl450: Symbol,
    pub spirv10: Symbol,
    pub spirv11: Symbol,
    pub spirv12: Symbol,
    pub spirv13: Symbol,
    pub spirv14: Symbol,
    pub spirv15: Symbol,
    descriptor_set: Symbol,
    binding: Symbol,
    attributes: HashMap<Symbol, SpirvAttribute>,
}

const BUILTINS: &[(&str, BuiltIn)] = {
    use BuiltIn::*;
    &[
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
};

const STORAGE_CLASSES: &[(&str, StorageClass)] = {
    use StorageClass::*;
    // make sure these strings stay synced with spirv-std's pointer types
    &[
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
};

const EXECUTION_MODELS: &[(&str, ExecutionModel)] = {
    use ExecutionModel::*;
    &[
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
};

impl Symbols {
    pub fn new() -> Self {
        let builtins = BUILTINS
            .iter()
            .map(|&(a, b)| (a, SpirvAttribute::Builtin(b)));
        let storage_classes = STORAGE_CLASSES
            .iter()
            .map(|&(a, b)| (a, SpirvAttribute::StorageClass(b)));
        let execution_models = EXECUTION_MODELS
            .iter()
            .map(|&(a, b)| (a, SpirvAttribute::Entry(b)));
        let custom = std::iter::once((
            "really_unsafe_ignore_bitcasts",
            SpirvAttribute::ReallyUnsafeIgnoreBitcasts,
        ));
        let attributes_iter = builtins
            .chain(storage_classes)
            .chain(execution_models)
            .chain(custom)
            .map(|(a, b)| (Symbol::intern(a), b));
        let mut attributes = HashMap::new();
        for attr in attributes_iter {
            let old = attributes.insert(attr.0, attr.1);
            // `.collect()` into a HashMap does not error on duplicates, so manually write out the
            // loop here to error on duplicates.
            assert!(old.is_none());
        }
        Self {
            spirv: Symbol::intern("spirv"),
            spirv_std: Symbol::intern("spirv_std"),
            kernel: Symbol::intern("kernel"),
            simple: Symbol::intern("simple"),
            vulkan: Symbol::intern("vulkan"),
            glsl450: Symbol::intern("glsl450"),
            spirv10: Symbol::intern("spirv1.0"),
            spirv11: Symbol::intern("spirv1.1"),
            spirv12: Symbol::intern("spirv1.2"),
            spirv13: Symbol::intern("spirv1.3"),
            spirv14: Symbol::intern("spirv1.4"),
            spirv15: Symbol::intern("spirv1.5"),
            descriptor_set: Symbol::intern("descriptor_set"),
            binding: Symbol::intern("binding"),
            attributes,
        }
    }
}

#[derive(Debug, Clone)]
pub enum SpirvAttribute {
    Builtin(BuiltIn),
    StorageClass(StorageClass),
    Entry(ExecutionModel),
    DescriptorSet(u32),
    Binding(u32),
    ReallyUnsafeIgnoreBitcasts,
}

// Note that we could mark the attr as used via cx.tcx.sess.mark_attr_used(attr), but unused
// reporting already happens even before we get here :(
/// Returns empty if this attribute is not a spirv attribute, or if it's malformed (and an error is
/// reported).
pub fn parse_attrs(
    cx: &CodegenCx<'_>,
    attrs: &[Attribute],
) -> impl Iterator<Item = SpirvAttribute> {
    let result = attrs.iter().flat_map(|attr| {
        let is_spirv = match attr.kind {
            AttrKind::Normal(ref item) => {
                // TODO: We ignore the rest of the path. Is this right?
                let last = item.path.segments.last();
                last.map_or(false, |seg| seg.ident.name == cx.sym.spirv)
            }
            AttrKind::DocComment(..) => false,
        };
        let args = if !is_spirv {
            // Use an empty vec here to return empty
            Vec::new()
        } else if let Some(args) = attr.meta_item_list() {
            args
        } else {
            cx.tcx.sess.span_err(
                attr.span,
                "#[spirv(..)] attribute must have at least one argument",
            );
            Vec::new()
        };
        args.into_iter().filter_map(move |ref arg| {
            if arg.has_name(cx.sym.descriptor_set) {
                match parse_attr_int_value(cx, arg) {
                    Some(x) => Some(SpirvAttribute::DescriptorSet(x)),
                    None => None,
                }
            } else if arg.has_name(cx.sym.binding) {
                match parse_attr_int_value(cx, arg) {
                    Some(x) => Some(SpirvAttribute::Binding(x)),
                    None => None,
                }
            } else {
                let name = match arg.ident() {
                    Some(i) => i,
                    None => {
                        cx.tcx.sess.span_err(
                            arg.span(),
                            "#[spirv(..)] attribute argument must be single identifier",
                        );
                        return None;
                    }
                };
                match cx.sym.attributes.get(&name.name) {
                    Some(a) => Some(a.clone()),
                    None => {
                        cx.tcx
                            .sess
                            .span_err(name.span, "unknown argument to spirv attribute");
                        None
                    }
                }
            }
        })
    });
    // lifetimes are hard :(
    result.collect::<Vec<_>>().into_iter()
}

fn parse_attr_int_value(cx: &CodegenCx<'_>, arg: &NestedMetaItem) -> Option<u32> {
    let arg = match arg.meta_item() {
        Some(arg) => arg,
        None => {
            cx.tcx
                .sess
                .span_err(arg.span(), "attribute must have value");
            return None;
        }
    };
    match arg.name_value_literal() {
        Some(&Lit {
            kind: LitKind::Int(x, LitIntType::Unsuffixed),
            ..
        }) if x <= u32::MAX as u128 => Some(x as u32),
        _ => {
            cx.tcx
                .sess
                .span_err(arg.span, "attribute value must be integer");
            None
        }
    }
}
