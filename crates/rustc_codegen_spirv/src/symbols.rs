use crate::builder::libm_intrinsics;
use crate::codegen_cx::CodegenCx;
use rspirv::spirv::{
    AccessQualifier, BuiltIn, Dim, ExecutionMode, ExecutionModel, ImageFormat, StorageClass,
};
use rustc_ast::ast::{AttrKind, Attribute, Lit, LitIntType, LitKind, NestedMetaItem};
use rustc_data_structures::captures::Captures;
use rustc_span::symbol::{Ident, Symbol};
use rustc_span::Span;
use std::collections::HashMap;
use std::rc::Rc;

/// Various places in the codebase (mostly attribute parsing) need to compare rustc Symbols to particular keywords.
/// Symbols are interned, as in, they don't actually store the string itself inside them, but rather an index into a
/// global table of strings. Then, whenever a new Symbol is created, the global table is checked to see if the string
/// already exists, deduplicating it if so. This makes things like comparison and cloning really cheap. So, this struct
/// is to allocate all our keywords up front and intern them all, so we can do comparisons really easily and fast.
pub struct Symbols {
    // Used by `is_blocklisted_fn`.
    pub fmt_decimal: Symbol,

    pub spirv: Symbol,
    pub spirv_std: Symbol,
    pub libm: Symbol,
    pub num_traits: Symbol,
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
    pub entry_point_name: Symbol,
    descriptor_set: Symbol,
    binding: Symbol,
    image_type: Symbol,
    dim: Symbol,
    depth: Symbol,
    arrayed: Symbol,
    multisampled: Symbol,
    sampled: Symbol,
    image_format: Symbol,
    access_qualifier: Symbol,
    attributes: HashMap<Symbol, SpirvAttribute>,
    execution_modes: HashMap<Symbol, (ExecutionMode, ExecutionModeExtraDim)>,
    pub libm_intrinsics: HashMap<Symbol, libm_intrinsics::LibmIntrinsic>,
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

#[derive(Copy, Clone, Debug)]
enum ExecutionModeExtraDim {
    None,
    Value,
    X,
    Y,
    Z,
}

const EXECUTION_MODES: &[(&str, ExecutionMode, ExecutionModeExtraDim)] = {
    use ExecutionMode::*;
    use ExecutionModeExtraDim::*;
    &[
        ("invocations", Invocations, Value),
        ("spacing_equal", SpacingEqual, None),
        ("spacing_fraction_even", SpacingFractionalEven, None),
        ("spacing_fraction_odd", SpacingFractionalOdd, None),
        ("vertex_order_cw", VertexOrderCw, None),
        ("vertex_order_ccw", VertexOrderCcw, None),
        ("pixel_center_integer", PixelCenterInteger, None),
        ("orgin_upper_left", OriginUpperLeft, None),
        ("origin_lower_left", OriginLowerLeft, None),
        ("early_fragment_tests", EarlyFragmentTests, None),
        ("point_mode", PointMode, None),
        ("xfb", Xfb, None),
        ("depth_replacing", DepthReplacing, None),
        ("depth_greater", DepthGreater, None),
        ("depth_less", DepthLess, None),
        ("depth_unchanged", DepthUnchanged, None),
        ("local_size_x", LocalSize, X),
        ("local_size_y", LocalSize, Y),
        ("local_size_z", LocalSize, Z),
        ("local_size_hint_x", LocalSizeHint, X),
        ("local_size_hint_y", LocalSizeHint, Y),
        ("local_size_hint_z", LocalSizeHint, Z),
        ("input_points", InputPoints, None),
        ("input_lines", InputLines, None),
        ("input_lines_adjacency", InputLinesAdjacency, None),
        ("triangles", Triangles, None),
        ("input_triangles_adjacency", InputTrianglesAdjacency, None),
        ("quads", Quads, None),
        ("isolines", Isolines, None),
        ("output_vertices", OutputVertices, Value),
        ("output_points", OutputPoints, None),
        ("output_line_strip", OutputLineStrip, None),
        ("output_triangle_strip", OutputTriangleStrip, None),
        ("vec_type_hint", VecTypeHint, Value),
        ("contraction_off", ContractionOff, None),
        ("initializer", Initializer, None),
        ("finalizer", Finalizer, None),
        ("subgroup_size", SubgroupSize, Value),
        ("subgroups_per_workgroup", SubgroupsPerWorkgroup, Value),
        ("subgroups_per_workgroup_id", SubgroupsPerWorkgroupId, Value),
        ("local_size_id_x", LocalSizeId, X),
        ("local_size_id_y", LocalSizeId, Y),
        ("local_size_id_z", LocalSizeId, Z),
        ("local_size_hint_id", LocalSizeHintId, Value),
        ("post_depth_coverage", PostDepthCoverage, None),
        ("denorm_preserve", DenormPreserve, None),
        ("denorm_flush_to_zero", DenormFlushToZero, Value),
        (
            "signed_zero_inf_nan_preserve",
            SignedZeroInfNanPreserve,
            Value,
        ),
        ("rounding_mode_rte", RoundingModeRTE, Value),
        ("rounding_mode_rtz", RoundingModeRTZ, Value),
        ("stencil_ref_replacing_ext", StencilRefReplacingEXT, None),
        ("output_lines_nv", OutputLinesNV, None),
        ("output_primitives_nv", OutputPrimitivesNV, Value),
        ("derivative_group_quads_nv", DerivativeGroupQuadsNV, None),
        ("output_triangles_nv", OutputTrianglesNV, None),
        (
            "pixel_interlock_ordered_ext",
            PixelInterlockOrderedEXT,
            None,
        ),
        (
            "pixel_interlock_unordered_ext",
            PixelInterlockUnorderedEXT,
            None,
        ),
        (
            "sample_interlock_ordered_ext",
            SampleInterlockOrderedEXT,
            None,
        ),
        (
            "sample_interlock_unordered_ext",
            SampleInterlockUnorderedEXT,
            None,
        ),
        (
            "shading_rate_interlock_ordered_ext",
            ShadingRateInterlockOrderedEXT,
            None,
        ),
        (
            "shading_rate_interlock_unordered_ext",
            ShadingRateInterlockUnorderedEXT,
            None,
        ),
        // Reserved
        /*("max_workgroup_size_intel_x", MaxWorkgroupSizeINTEL, X),
        ("max_workgroup_size_intel_y", MaxWorkgroupSizeINTEL, Y),
        ("max_workgroup_size_intel_z", MaxWorkgroupSizeINTEL, Z),
        ("max_work_dim_intel", MaxWorkDimINTEL, Value),
        ("no_global_offset_intel", NoGlobalOffsetINTEL, None),
        ("num_simd_workitems_intel", NumSIMDWorkitemsINTEL, Value),*/
    ]
};

// FIXME(eddyb) clippy bug suggests `Self` even when it couldn't possibly work.
#[allow(clippy::use_self)]
impl Symbols {
    fn new() -> Self {
        let builtins = BUILTINS
            .iter()
            .map(|&(a, b)| (a, SpirvAttribute::Builtin(b)));
        let storage_classes = STORAGE_CLASSES
            .iter()
            .map(|&(a, b)| (a, SpirvAttribute::StorageClass(b)));
        let execution_models = EXECUTION_MODELS
            .iter()
            .map(|&(a, b)| (a, SpirvAttribute::Entry(b.into())));
        let custom_attributes = [
            ("sampler", SpirvAttribute::Sampler),
            ("block", SpirvAttribute::Block),
            ("flat", SpirvAttribute::Flat),
            ("sampled_image", SpirvAttribute::SampledImage),
            ("unroll_loops", SpirvAttribute::UnrollLoops),
        ]
        .iter()
        .cloned();
        let attributes_iter = builtins
            .chain(storage_classes)
            .chain(execution_models)
            .chain(custom_attributes)
            .map(|(a, b)| (Symbol::intern(a), b));
        let mut attributes = HashMap::new();
        for (a, b) in attributes_iter {
            let old = attributes.insert(a, b);
            // `.collect()` into a HashMap does not error on duplicates, so manually write out the
            // loop here to error on duplicates.
            assert!(old.is_none());
        }
        let mut execution_modes = HashMap::new();
        for &(key, mode, dim) in EXECUTION_MODES {
            let old = execution_modes.insert(Symbol::intern(key), (mode, dim));
            assert!(old.is_none());
        }

        let mut libm_intrinsics = HashMap::new();
        for &(a, b) in libm_intrinsics::TABLE {
            let old = libm_intrinsics.insert(Symbol::intern(a), b);
            assert!(old.is_none());
        }
        Self {
            fmt_decimal: Symbol::intern("fmt_decimal"),

            entry_point_name: Symbol::intern("entry_point_name"),
            spirv: Symbol::intern("spirv"),
            spirv_std: Symbol::intern("spirv_std"),
            libm: Symbol::intern("libm"),
            num_traits: Symbol::intern("num_traits"),
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
            image_type: Symbol::intern("image_type"),
            dim: Symbol::intern("dim"),
            depth: Symbol::intern("depth"),
            arrayed: Symbol::intern("arrayed"),
            multisampled: Symbol::intern("multisampled"),
            sampled: Symbol::intern("sampled"),
            image_format: Symbol::intern("image_format"),
            access_qualifier: Symbol::intern("access_qualifier"),
            attributes,
            execution_modes,
            libm_intrinsics,
        }
    }

    /// Obtain an `Rc` handle to the current thread's `Symbols` instance, which
    /// will be shared between all `Symbols::get()` calls on the same thread.
    ///
    /// While this is relatively cheap, prefer caching it in e.g. `CodegenCx`,
    /// rather than calling `get()` every time a field of `Symbols` is needed.
    pub fn get() -> Rc<Self> {
        thread_local!(static SYMBOLS: Rc<Symbols> = Rc::new(Symbols::new()));
        SYMBOLS.with(Rc::clone)
    }
}

#[derive(Copy, Clone, Debug)]
pub struct ExecutionModeExtra {
    args: [u32; 3],
    len: u8,
}

impl ExecutionModeExtra {
    fn new(args: impl AsRef<[u32]>) -> Self {
        let _args = args.as_ref();
        let mut args = [0; 3];
        args[.._args.len()].copy_from_slice(_args);
        let len = _args.len() as u8;
        Self { args, len }
    }
}

impl AsRef<[u32]> for ExecutionModeExtra {
    fn as_ref(&self) -> &[u32] {
        &self.args[..self.len as _]
    }
}

#[derive(Clone, Debug)]
pub struct Entry {
    pub execution_model: ExecutionModel,
    pub execution_modes: Vec<(ExecutionMode, ExecutionModeExtra)>,
    pub name: Option<Symbol>,
}

impl From<ExecutionModel> for Entry {
    fn from(execution_model: ExecutionModel) -> Self {
        Self {
            execution_model,
            execution_modes: Vec::new(),
            name: None,
        }
    }
}

// FIXME(eddyb) maybe move this to `attr`?
#[derive(Debug, Clone)]
pub enum SpirvAttribute {
    Builtin(BuiltIn),
    StorageClass(StorageClass),
    Entry(Entry),
    DescriptorSet(u32),
    Binding(u32),
    ImageType {
        dim: Dim,
        depth: u32,
        arrayed: u32,
        multisampled: u32,
        sampled: u32,
        image_format: ImageFormat,
        access_qualifier: Option<AccessQualifier>,
    },
    Sampler,
    SampledImage,
    Block,
    Flat,
    UnrollLoops,
}

// FIXME(eddyb) maybe move this to `attr`?
/// Returns only the spirv attributes that could successfully parsed.
/// For any malformed ones, an error is reported prior to codegen, by a check pass.
pub fn parse_attrs<'a, 'tcx>(
    cx: &'a CodegenCx<'tcx>,
    attrs: &'tcx [Attribute],
) -> impl Iterator<Item = SpirvAttribute> + Captures<'tcx> + 'a {
    parse_attrs_for_checking(&cx.sym, attrs)
        .filter_map(move |(_, parse_attr_result)| {
            // NOTE(eddyb) `delay_span_bug` ensures that if attribute checking fails
            // to see an attribute error, it will cause an ICE instead.
            parse_attr_result
                .map_err(|(span, msg)| cx.tcx.sess.delay_span_bug(span, &msg))
                .ok()
        })
        .map(|(_span, parsed_attr)| parsed_attr)
}

// FIXME(eddyb) find something nicer for the error type.
type ParseAttrError = (Span, String);

// FIXME(eddyb) maybe move this to `attr`?
pub(crate) fn parse_attrs_for_checking<'a>(
    sym: &'a Symbols,
    attrs: &'a [Attribute],
) -> impl Iterator<
    Item = (
        &'a Attribute,
        Result<(Span, SpirvAttribute), ParseAttrError>,
    ),
> + 'a {
    attrs.iter().flat_map(move |attr| {
        let is_spirv = match attr.kind {
            AttrKind::Normal(ref item, _) => {
                // TODO: We ignore the rest of the path. Is this right?
                let last = item.path.segments.last();
                last.map_or(false, |seg| seg.ident.name == sym.spirv)
            }
            AttrKind::DocComment(..) => false,
        };
        let (whole_attr_error, args) = if !is_spirv {
            // Use an empty vec here to return empty
            (None, Vec::new())
        } else if let Some(args) = attr.meta_item_list() {
            (None, args)
        } else {
            (
                Some(Err((
                    attr.span,
                    "#[spirv(..)] attribute must have at least one argument".to_string(),
                ))),
                Vec::new(),
            )
        };
        whole_attr_error
            .into_iter()
            .chain(args.into_iter().map(move |ref arg| {
                let span = arg.span();
                let parsed_attr = if arg.has_name(sym.image_type) {
                    parse_image_type(sym, arg)?
                } else if arg.has_name(sym.descriptor_set) {
                    SpirvAttribute::DescriptorSet(parse_attr_int_value(arg)?)
                } else if arg.has_name(sym.binding) {
                    SpirvAttribute::Binding(parse_attr_int_value(arg)?)
                } else {
                    let name = match arg.ident() {
                        Some(i) => i,
                        None => {
                            return Err((
                                span,
                                "#[spirv(..)] attribute argument must be single identifier"
                                    .to_string(),
                            ));
                        }
                    };
                    sym.attributes
                        .get(&name.name)
                        .map(|a| {
                            Ok(match a {
                                SpirvAttribute::Entry(entry) => SpirvAttribute::Entry(
                                    parse_entry_attrs(sym, arg, &name, entry.execution_model)?,
                                ),
                                _ => a.clone(),
                            })
                        })
                        .unwrap_or_else(|| {
                            Err((name.span, "unknown argument to spirv attribute".to_string()))
                        })?
                };
                Ok((span, parsed_attr))
            }))
            .map(move |parse_attr_result| (attr, parse_attr_result))
    })
}

fn parse_image_type(
    sym: &Symbols,
    attr: &NestedMetaItem,
) -> Result<SpirvAttribute, ParseAttrError> {
    let args = match attr.meta_item_list() {
        Some(args) => args,
        None => {
            return Err((
                attr.span(),
                "image_type attribute must have arguments".to_string(),
            ))
        }
    };
    if args.len() != 6 && args.len() != 7 {
        return Err((
            attr.span(),
            "image_type attribute must have 6 or 7 arguments".to_string(),
        ));
    }
    let check = |idx: usize, sym: Symbol| -> Result<(), ParseAttrError> {
        if args[idx].has_name(sym) {
            Ok(())
        } else {
            Err((
                args[idx].span(),
                format!(
                    "image_type attribute argument {} must be {}=...",
                    idx + 1,
                    sym
                ),
            ))
        }
    };
    check(0, sym.dim)?;
    check(1, sym.depth)?;
    check(2, sym.arrayed)?;
    check(3, sym.multisampled)?;
    check(4, sym.sampled)?;
    check(5, sym.image_format)?;
    if args.len() == 7 {
        check(6, sym.access_qualifier)?;
    }
    let arg_values = args
        .iter()
        .map(
            |arg| match arg.meta_item().and_then(|arg| arg.name_value_literal()) {
                Some(arg) => Ok(arg),
                None => Err((
                    arg.span(),
                    "image_type attribute must be name=value".to_string(),
                )),
            },
        )
        .collect::<Result<Vec<_>, _>>()?;
    let dim = match arg_values[0].kind {
        LitKind::Str(dim, _) => match dim.as_str().parse() {
            Ok(dim) => dim,
            Err(()) => return Err((args[0].span(), "invalid dim value".to_string())),
        },
        _ => return Err((args[0].span(), "dim value must be str".to_string())),
    };
    let parse_lit = |idx: usize, name: &str| -> Result<u32, ParseAttrError> {
        match arg_values[idx].kind {
            LitKind::Int(v, _) => Ok(v as u32),
            _ => Err((args[idx].span(), format!("{} value must be int", name))),
        }
    };
    let depth = parse_lit(1, "depth")?;
    let arrayed = parse_lit(2, "arrayed")?;
    let multisampled = parse_lit(3, "multisampled")?;
    let sampled = parse_lit(4, "sampled")?;
    let image_format = match arg_values[5].kind {
        LitKind::Str(image_format, _) => match image_format.as_str().parse() {
            Ok(image_format) => image_format,
            Err(()) => return Err((args[5].span(), "invalid image_format value".to_string())),
        },
        _ => return Err((args[5].span(), "image_format value must be str".to_string())),
    };
    let access_qualifier = if args.len() == 7 {
        Some(match arg_values[6].kind {
            LitKind::Str(access_qualifier, _) => match access_qualifier.as_str().parse() {
                Ok(access_qualifier) => access_qualifier,
                Err(()) => {
                    return Err((args[6].span(), "invalid access_qualifier value".to_string()));
                }
            },
            _ => {
                return Err((
                    args[6].span(),
                    "access_qualifier value must be str".to_string(),
                ));
            }
        })
    } else {
        None
    };
    Ok(SpirvAttribute::ImageType {
        dim,
        depth,
        arrayed,
        multisampled,
        sampled,
        image_format,
        access_qualifier,
    })
}

fn parse_attr_int_value(arg: &NestedMetaItem) -> Result<u32, ParseAttrError> {
    let arg = match arg.meta_item() {
        Some(arg) => arg,
        None => return Err((arg.span(), "attribute must have value".to_string())),
    };
    match arg.name_value_literal() {
        Some(&Lit {
            kind: LitKind::Int(x, LitIntType::Unsuffixed),
            ..
        }) if x <= u32::MAX as u128 => Ok(x as u32),
        _ => Err((arg.span, "attribute value must be integer".to_string())),
    }
}

// for a given entry, gather up the additional attributes
// in this case ExecutionMode's, some have extra arguments
// others are specified with x, y, or z components
// ie #[spirv(fragment(origin_lower_left))] or #[spirv(gl_compute(local_size_x=64, local_size_y=8))]
fn parse_entry_attrs(
    sym: &Symbols,
    arg: &NestedMetaItem,
    name: &Ident,
    execution_model: ExecutionModel,
) -> Result<Entry, ParseAttrError> {
    use ExecutionMode::*;
    use ExecutionModel::*;
    let mut entry = Entry::from(execution_model);
    let mut origin_mode: Option<ExecutionMode> = None;
    let mut local_size: Option<[u32; 3]> = None;
    let mut local_size_hint: Option<[u32; 3]> = None;
    // Reserved
    //let mut max_workgroup_size_intel: Option<[u32; 3]> = None;
    if let Some(attrs) = arg.meta_item_list() {
        for attr in attrs {
            if let Some(attr_name) = attr.ident() {
                if let Some((execution_mode, extra_dim)) = sym.execution_modes.get(&attr_name.name)
                {
                    use ExecutionModeExtraDim::*;
                    let val = match extra_dim {
                        None => Option::None,
                        _ => Some(parse_attr_int_value(attr)?),
                    };
                    match execution_mode {
                        OriginUpperLeft | OriginLowerLeft => {
                            origin_mode.replace(*execution_mode);
                        }
                        LocalSize => {
                            let val = val.unwrap();
                            if local_size.is_none() {
                                local_size.replace([1, 1, 1]);
                            }
                            let local_size = local_size.as_mut().unwrap();
                            match extra_dim {
                                X => {
                                    local_size[0] = val;
                                }
                                Y => {
                                    local_size[1] = val;
                                }
                                Z => {
                                    local_size[2] = val;
                                }
                                _ => unreachable!(),
                            }
                        }
                        LocalSizeHint => {
                            let val = val.unwrap();
                            if local_size_hint.is_none() {
                                local_size_hint.replace([1, 1, 1]);
                            }
                            let local_size_hint = local_size_hint.as_mut().unwrap();
                            match extra_dim {
                                X => {
                                    local_size_hint[0] = val;
                                }
                                Y => {
                                    local_size_hint[1] = val;
                                }
                                Z => {
                                    local_size_hint[2] = val;
                                }
                                _ => unreachable!(),
                            }
                        }
                        // Reserved
                        /*MaxWorkgroupSizeINTEL => {
                            let val = val.unwrap();
                            if max_workgroup_size_intel.is_none() {
                                max_workgroup_size_intel.replace([1, 1, 1]);
                            }
                            let max_workgroup_size_intel = max_workgroup_size_intel.as_mut()
                                .unwrap();
                            match extra_dim {
                                X => {
                                    max_workgroup_size_intel[0] = val;
                                },
                                Y => {
                                    max_workgroup_size_intel[1] = val;
                                },
                                Z => {
                                    max_workgroup_size_intel[2] = val;
                                },
                                _ => unreachable!(),
                            }
                        },*/
                        _ => {
                            if let Some(val) = val {
                                entry
                                    .execution_modes
                                    .push((*execution_mode, ExecutionModeExtra::new([val])));
                            } else {
                                entry
                                    .execution_modes
                                    .push((*execution_mode, ExecutionModeExtra::new([])));
                            }
                        }
                    }
                } else if attr_name.name == sym.entry_point_name {
                    match attr.value_str() {
                        Some(sym) => {
                            entry.name = Some(sym);
                        }
                        None => {
                            return Err((
                                attr_name.span,
                                format!(
                                    "#[spirv({}(..))] unknown attribute argument {}",
                                    name.name.to_ident_string(),
                                    attr_name.name.to_ident_string()
                                ),
                            ))
                        }
                    }
                } else {
                    return Err((
                        attr_name.span,
                        format!(
                            "#[spirv({}(..))] unknown attribute argument {}",
                            name.name.to_ident_string(),
                            attr_name.name.to_ident_string()
                        ),
                    ));
                }
            } else {
                return Err((
                    arg.span(),
                    format!(
                        "#[spirv({}(..))] attribute argument must be single identifier",
                        name.name.to_ident_string()
                    ),
                ));
            }
        }
    }
    match entry.execution_model {
        Fragment => {
            let origin_mode = origin_mode.unwrap_or(OriginUpperLeft);
            entry
                .execution_modes
                .push((origin_mode, ExecutionModeExtra::new([])));
        }
        GLCompute => {
            let local_size = local_size.unwrap_or([1, 1, 1]);
            entry
                .execution_modes
                .push((LocalSize, ExecutionModeExtra::new(local_size)));
        }
        Kernel => {
            if let Some(local_size) = local_size {
                entry
                    .execution_modes
                    .push((LocalSize, ExecutionModeExtra::new(local_size)));
            }
            if let Some(local_size_hint) = local_size_hint {
                entry
                    .execution_modes
                    .push((LocalSizeHint, ExecutionModeExtra::new(local_size_hint)));
            }
            // Reserved
            /*if let Some(max_workgroup_size_intel) = max_workgroup_size_intel {
                entry.execution_modes.push((MaxWorkgroupSizeINTEL, ExecutionModeExtra::new(max_workgroup_size_intel)));
            }*/
        }
        //TODO: Cover more defaults
        _ => {}
    }
    Ok(entry)
}
