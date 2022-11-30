use crate::attr::{Entry, ExecutionModeExtra, IntrinsicType, SpirvAttribute};
use crate::builder::libm_intrinsics;
use rspirv::spirv::{BuiltIn, ExecutionMode, ExecutionModel, StorageClass};
use rustc_ast::ast::{AttrKind, Attribute, Lit, LitIntType, LitKind, NestedMetaItem};
use rustc_data_structures::fx::FxHashMap;
use rustc_span::symbol::{Ident, Symbol};
use rustc_span::Span;
use std::rc::Rc;

/// Various places in the codebase (mostly attribute parsing) need to compare rustc Symbols to particular keywords.
/// Symbols are interned, as in, they don't actually store the string itself inside them, but rather an index into a
/// global table of strings. Then, whenever a new Symbol is created, the global table is checked to see if the string
/// already exists, deduplicating it if so. This makes things like comparison and cloning really cheap. So, this struct
/// is to allocate all our keywords up front and intern them all, so we can do comparisons really easily and fast.
pub struct Symbols {
    // Used by `is_blocklisted_fn`.
    pub fmt_decimal: Symbol,

    pub discriminant: Symbol,
    pub rust_gpu: Symbol,
    pub spirv: Symbol,
    pub spirv_std: Symbol,
    pub libm: Symbol,
    pub num_traits: Symbol,
    pub entry_point_name: Symbol,
    pub spv_intel_shader_integer_functions2: Symbol,
    pub spv_khr_vulkan_memory_model: Symbol,

    descriptor_set: Symbol,
    binding: Symbol,
    input_attachment_index: Symbol,

    attributes: FxHashMap<Symbol, SpirvAttribute>,
    execution_modes: FxHashMap<Symbol, (ExecutionMode, ExecutionModeExtraDim)>,
    pub libm_intrinsics: FxHashMap<Symbol, libm_intrinsics::LibmIntrinsic>,
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
        // ("workgroup_size", WorkgroupSize), -- constant
        ("workgroup_id", WorkgroupId),
        ("local_invocation_id", LocalInvocationId),
        ("global_invocation_id", GlobalInvocationId),
        ("local_invocation_index", LocalInvocationIndex),
        // ("work_dim", WorkDim), -- Kernel-only
        // ("global_size", GlobalSize), -- Kernel-only
        // ("enqueued_workgroup_size", EnqueuedWorkgroupSize), -- Kernel-only
        // ("global_offset", GlobalOffset), -- Kernel-only
        // ("global_linear_id", GlobalLinearId), -- Kernel-only
        ("subgroup_size", SubgroupSize),
        // ("subgroup_max_size", SubgroupMaxSize), -- Kernel-only
        ("num_subgroups", NumSubgroups),
        // ("num_enqueued_subgroups", NumEnqueuedSubgroups), -- Kernel-only
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
        ("layer_per_view_nv", LayerPerViewNV),
        ("mesh_view_count_nv", MeshViewCountNV),
        ("mesh_view_indices_nv", MeshViewIndicesNV),
        ("bary_coord_nv", BaryCoordNV),
        ("bary_coord_no_persp_nv", BaryCoordNoPerspNV),
        ("frag_size_ext", FragSizeEXT),
        ("frag_invocation_count_ext", FragInvocationCountEXT),
        ("launch_id", BuiltIn::LaunchIdKHR),
        ("launch_size", BuiltIn::LaunchSizeKHR),
        ("instance_custom_index", BuiltIn::InstanceCustomIndexKHR),
        ("ray_geometry_index", BuiltIn::RayGeometryIndexKHR),
        ("world_ray_origin", BuiltIn::WorldRayOriginKHR),
        ("world_ray_direction", BuiltIn::WorldRayDirectionKHR),
        ("object_ray_origin", BuiltIn::ObjectRayOriginKHR),
        ("object_ray_direction", BuiltIn::ObjectRayDirectionKHR),
        ("ray_tmin", BuiltIn::RayTminKHR),
        ("ray_tmax", BuiltIn::RayTmaxKHR),
        ("object_to_world", BuiltIn::ObjectToWorldKHR),
        ("world_to_object", BuiltIn::WorldToObjectKHR),
        ("hit_kind", BuiltIn::HitKindKHR),
        ("incoming_ray_flags", BuiltIn::IncomingRayFlagsKHR),
        ("warps_per_sm_nv", WarpsPerSMNV),
        ("sm_count_nv", SMCountNV),
        ("warp_id_nv", WarpIDNV),
        ("SMIDNV", SMIDNV),
    ]
};

const STORAGE_CLASSES: &[(&str, StorageClass)] = {
    use StorageClass::*;
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
        ("callable_data", StorageClass::CallableDataKHR),
        (
            "incoming_callable_data",
            StorageClass::IncomingCallableDataKHR,
        ),
        ("ray_payload", StorageClass::RayPayloadKHR),
        ("hit_attribute", StorageClass::HitAttributeKHR),
        ("incoming_ray_payload", StorageClass::IncomingRayPayloadKHR),
        ("shader_record_buffer", StorageClass::ShaderRecordBufferKHR),
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
        ("compute", GLCompute),
        ("task_nv", TaskNV),
        ("mesh_nv", MeshNV),
        ("ray_generation", ExecutionModel::RayGenerationKHR),
        ("intersection", ExecutionModel::IntersectionKHR),
        ("any_hit", ExecutionModel::AnyHitKHR),
        ("closest_hit", ExecutionModel::ClosestHitKHR),
        ("miss", ExecutionModel::MissKHR),
        ("callable", ExecutionModel::CallableKHR),
    ]
};

#[derive(Copy, Clone, Debug)]
enum ExecutionModeExtraDim {
    None,
    Value,
    X,
    Y,
    Z,
    Tuple,
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
        ("threads", LocalSize, Tuple),
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
            (
                "sampler",
                SpirvAttribute::IntrinsicType(IntrinsicType::Sampler),
            ),
            (
                "generic_image_type",
                SpirvAttribute::IntrinsicType(IntrinsicType::GenericImageType),
            ),
            (
                "acceleration_structure",
                SpirvAttribute::IntrinsicType(IntrinsicType::AccelerationStructureKhr),
            ),
            (
                "ray_query",
                SpirvAttribute::IntrinsicType(IntrinsicType::RayQueryKhr),
            ),
            ("block", SpirvAttribute::Block),
            ("flat", SpirvAttribute::Flat),
            ("invariant", SpirvAttribute::Invariant),
            (
                "sampled_image",
                SpirvAttribute::IntrinsicType(IntrinsicType::SampledImage),
            ),
            (
                "runtime_array",
                SpirvAttribute::IntrinsicType(IntrinsicType::RuntimeArray),
            ),
            (
                "matrix",
                SpirvAttribute::IntrinsicType(IntrinsicType::Matrix),
            ),
            ("buffer_load_intrinsic", SpirvAttribute::BufferLoadIntrinsic),
            (
                "buffer_store_intrinsic",
                SpirvAttribute::BufferStoreIntrinsic,
            ),
        ]
        .iter()
        .cloned();
        let attributes_iter = builtins
            .chain(storage_classes)
            .chain(execution_models)
            .chain(custom_attributes)
            .map(|(a, b)| (Symbol::intern(a), b));
        let mut attributes = FxHashMap::default();
        for (a, b) in attributes_iter {
            let old = attributes.insert(a, b);
            // `.collect()` into a FxHashMap does not error on duplicates, so manually write out the
            // loop here to error on duplicates.
            assert!(old.is_none());
        }
        let mut execution_modes = FxHashMap::default();
        for &(key, mode, dim) in EXECUTION_MODES {
            let old = execution_modes.insert(Symbol::intern(key), (mode, dim));
            assert!(old.is_none());
        }

        let mut libm_intrinsics = FxHashMap::default();
        for &(a, b) in libm_intrinsics::TABLE {
            let old = libm_intrinsics.insert(Symbol::intern(a), b);
            assert!(old.is_none());
        }
        Self {
            fmt_decimal: Symbol::intern("fmt_decimal"),

            discriminant: Symbol::intern("discriminant"),
            rust_gpu: Symbol::intern("rust_gpu"),
            spirv: Symbol::intern("spirv"),
            spirv_std: Symbol::intern("spirv_std"),
            libm: Symbol::intern("libm"),
            num_traits: Symbol::intern("num_traits"),
            entry_point_name: Symbol::intern("entry_point_name"),
            spv_intel_shader_integer_functions2: Symbol::intern(
                "SPV_INTEL_shader_integer_functions2",
            ),
            spv_khr_vulkan_memory_model: Symbol::intern("SPV_KHR_vulkan_memory_model"),

            descriptor_set: Symbol::intern("descriptor_set"),
            binding: Symbol::intern("binding"),
            input_attachment_index: Symbol::intern("input_attachment_index"),

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

// FIXME(eddyb) find something nicer for the error type.
type ParseAttrError = (Span, String);

// FIXME(eddyb) maybe move this to `attr`?
pub(crate) fn parse_attrs_for_checking<'a>(
    sym: &'a Symbols,
    attrs: &'a [Attribute],
) -> impl Iterator<Item = Result<(Span, SpirvAttribute), ParseAttrError>> + 'a {
    attrs.iter().flat_map(move |attr| {
        let (whole_attr_error, args) = match attr.kind {
            AttrKind::Normal(ref normal) => {
                // #[...]
                let s = &normal.item.path.segments;
                if s.len() > 1 && s[0].ident.name == sym.rust_gpu {
                    // #[rust_gpu ...]
                    if s.len() != 2 || s[1].ident.name != sym.spirv {
                        // #[rust_gpu::...] but not #[rust_gpu::spirv]
                        (
                            Some(Err((
                                attr.span,
                                "unknown `rust_gpu` attribute, expected `rust_gpu::spirv`"
                                    .to_string(),
                            ))),
                            Vec::new(),
                        )
                    } else if let Some(args) = attr.meta_item_list() {
                        // #[rust_gpu::spirv(...)]
                        (None, args)
                    } else {
                        // #[rust_gpu::spirv]
                        (
                            Some(Err((
                                attr.span,
                                "#[rust_gpu::spirv(..)] attribute must have at least one argument"
                                    .to_string(),
                            ))),
                            Vec::new(),
                        )
                    }
                } else {
                    // #[...] but not #[rust_gpu ...]
                    (None, Vec::new())
                }
            }
            AttrKind::DocComment(..) => (None, Vec::new()), // doccomment
        };

        whole_attr_error
            .into_iter()
            .chain(args.into_iter().map(move |ref arg| {
                let span = arg.span();
                let parsed_attr = if arg.has_name(sym.descriptor_set) {
                    SpirvAttribute::DescriptorSet(parse_attr_int_value(arg)?)
                } else if arg.has_name(sym.binding) {
                    SpirvAttribute::Binding(parse_attr_int_value(arg)?)
                } else if arg.has_name(sym.input_attachment_index) {
                    SpirvAttribute::InputAttachmentIndex(parse_attr_int_value(arg)?)
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
                    sym.attributes.get(&name.name).map_or_else(
                        || Err((name.span, "unknown argument to spirv attribute".to_string())),
                        |a| {
                            Ok(match a {
                                SpirvAttribute::Entry(entry) => SpirvAttribute::Entry(
                                    parse_entry_attrs(sym, arg, &name, entry.execution_model)?,
                                ),
                                _ => a.clone(),
                            })
                        },
                    )?
                };
                Ok((span, parsed_attr))
            }))
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

fn parse_local_size_attr(arg: &NestedMetaItem) -> Result<[u32; 3], ParseAttrError> {
    let arg = match arg.meta_item() {
        Some(arg) => arg,
        None => return Err((arg.span(), "attribute must have value".to_string())),
    };
    match arg.meta_item_list() {
        Some(tuple) if !tuple.is_empty() && tuple.len() < 4 => {
            let mut local_size = [1; 3];
            for (idx, lit) in tuple.iter().enumerate() {
                match lit.literal() {
                    Some(&Lit {
                        kind: LitKind::Int(x, LitIntType::Unsuffixed),
                        ..
                    }) if x <= u32::MAX as u128 => local_size[idx] = x as u32,
                    _ => return Err((lit.span(), "must be a u32 literal".to_string())),
                }
            }
            Ok(local_size)
        }
        Some(tuple) if tuple.is_empty() => Err((
            arg.span,
            "#[spirv(compute(threads(x, y, z)))] must have the x dimension specified, trailing ones may be elided".to_string(),
        )),
        Some(tuple) if tuple.len() > 3 => Err((
            arg.span,
            "#[spirv(compute(threads(x, y, z)))] is three dimensional".to_string(),
        )),
        _ => Err((
            arg.span,
            "#[spirv(compute(threads(x, y, z)))] must have 1 to 3 parameters, trailing ones may be elided".to_string(),
        )),
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
                        None | Tuple => Option::None,
                        _ => Some(parse_attr_int_value(attr)?),
                    };
                    match execution_mode {
                        OriginUpperLeft | OriginLowerLeft => {
                            origin_mode.replace(*execution_mode);
                        }
                        LocalSize => {
                            if local_size.is_none() {
                                local_size.replace(parse_local_size_attr(attr)?);
                            } else {
                                return Err((
                                    attr_name.span,
                                    String::from(
                                        "`#[spirv(compute(threads))]` may only be specified once",
                                    ),
                                ));
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
                                    "#[spirv({name}(..))] unknown attribute argument {attr_name}"
                                ),
                            ))
                        }
                    }
                } else {
                    return Err((
                        attr_name.span,
                        format!("#[spirv({name}(..))] unknown attribute argument {attr_name}",),
                    ));
                }
            } else {
                return Err((
                    arg.span(),
                    format!("#[spirv({name}(..))] attribute argument must be single identifier"),
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
        GLCompute | MeshNV | TaskNV => {
            if let Some(local_size) = local_size {
                entry
                    .execution_modes
                    .push((LocalSize, ExecutionModeExtra::new(local_size)));
            } else {
                return Err((
                    arg.span(),
                    String::from(
                        "The `threads` argument must be specified when using `#[spirv(compute)]`, `#[spirv(mesh_nv)]` or `#[spirv(task_nv)]`",
                    ),
                ));
            }
        }
        //TODO: Cover more defaults
        _ => {}
    }
    Ok(entry)
}
