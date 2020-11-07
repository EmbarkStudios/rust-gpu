use crate::codegen_cx::CodegenCx;
use rspirv::spirv::{BuiltIn, StorageClass, ExecutionModel, ExecutionMode};
use rustc_ast::ast::{AttrKind, Attribute, Lit, LitIntType, LitKind, NestedMetaItem};
use rustc_span::symbol::{Symbol, Ident};
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
    really_unsafe_ignore_bitcasts: Symbol,
    attributes: HashMap<Symbol, SpirvAttribute>,
    execution_modes: HashMap<Symbol, (ExecutionMode, ExecutionModeExtraDim)>,
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
    Z    
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
        ("signed_zero_inf_nan_preserve", SignedZeroInfNanPreserve, Value),
        ("rounding_mode_rte", RoundingModeRTE, Value),
        ("rounding_mode_rtz", RoundingModeRTZ, Value),
        ("stencil_ref_replacing_ext", StencilRefReplacingEXT, None),
        ("output_lines_nv", OutputLinesNV, None),
        ("output_primitives_nv", OutputPrimitivesNV, Value),
        ("derivative_group_quads_nv", DerivativeGroupQuadsNV, None),
        ("output_triangles_nv", OutputTrianglesNV, None),
        ("pixel_interlock_ordered_ext", PixelInterlockOrderedEXT, None),
        ("pixel_interlock_unordered_ext", PixelInterlockUnorderedEXT, None),
        ("sample_interlock_ordered_ext", SampleInterlockOrderedEXT, None),
        ("sample_interlock_unordered_ext", SampleInterlockUnorderedEXT, None),
        ("shading_rate_interlock_ordered_ext", ShadingRateInterlockOrderedEXT, None),
        ("shading_rate_interlock_unordered_ext", ShadingRateInterlockUnorderedEXT, None),
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
    pub fn new() -> Self {
        let builtins = BUILTINS
            .iter()
            .map(|&(a, b)| (a, SpirvAttribute::Builtin(b)));
        let storage_classes = STORAGE_CLASSES
            .iter()
            .map(|&(a, b)| (a, SpirvAttribute::StorageClass(b)));
        let execution_models = EXECUTION_MODELS
            .iter()
            .map(|&(a, b)| (a, SpirvAttribute::Entry(b.into())));
        let attributes_iter = builtins
            .chain(storage_classes)
            .chain(execution_models)
            .map(|(a, b)| (Symbol::intern(a), b));
        let mut attributes = HashMap::new();
        attributes_iter.for_each(|(a, b)| {
            let old = attributes.insert(a, b);
            // `.collect()` into a HashMap does not error on duplicates, so manually write out the
            // loop here to error on duplicates.
            assert!(old.is_none());
        });
        let mut execution_modes = HashMap::new();
        EXECUTION_MODES.iter()
            .for_each(|(key, mode, dim)| {
                let old = execution_modes.insert(Symbol::intern(key), (*mode, *dim));
                assert!(old.is_none());
            }); 
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
            really_unsafe_ignore_bitcasts: Symbol::intern("really_unsafe_ignore_bitcasts"),
            attributes,
            execution_modes,
        }
    }
}


#[derive(Copy, Clone, Debug)]
pub struct ExecutionModeExtra {
    args: [u32; 3],
    len: u8 
}

impl ExecutionModeExtra {
    fn new(args: impl AsRef<[u32]>) -> Self {
        let _args = args.as_ref();
        let mut args = [0; 3];
        args[.._args.len()].copy_from_slice(_args);
        let len = _args.len() as u8;
        Self {
            args,
            len
        }
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
}

impl From<ExecutionModel> for Entry {
    fn from(execution_model: ExecutionModel) -> Self {
        Self {
            execution_model,
            execution_modes: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum SpirvAttribute {
    Builtin(BuiltIn),
    StorageClass(StorageClass),
    Entry(Entry),
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
            if arg.has_name(cx.sym.really_unsafe_ignore_bitcasts) {
                Some(SpirvAttribute::ReallyUnsafeIgnoreBitcasts)
            } else if arg.has_name(cx.sym.descriptor_set) {
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
                cx.sym.attributes.get(&name.name)
                    .map(|a| {
                        match a {
                            SpirvAttribute::Entry(entry) => SpirvAttribute::Entry(parse_entry_attrs(cx, arg, &name, entry.execution_model)),
                            _ => a.clone(),
                        }
                    })
                    .or_else(|| {
                        cx.tcx
                            .sess
                            .span_err(name.span, "unknown argument to spirv attribute");
                        None
                    })
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

// for a given entry, gather up the additional attributes 
// in this case ExecutionMode's, some have extra arguments
// others are specified with x, y, or z components
// ie #[spirv(fragment(origin_lower_left))] or #[spirv(gl_compute(local_size_x=64, local_size_y=8))] 
fn parse_entry_attrs(cx: &CodegenCx<'_>, arg: &NestedMetaItem, name: &Ident, execution_model: ExecutionModel) -> Entry {
    use ExecutionModel::*;
    use ExecutionMode::*;
    let mut entry = Entry::from(execution_model);
    let mut origin_mode: Option<ExecutionMode> = None;
    let mut local_size: Option<[u32; 3]> = None;
    let mut local_size_hint: Option<[u32; 3]> = None;
    // Reserved
    //let mut max_workgroup_size_intel: Option<[u32; 3]> = None;
    if let Some(attrs) = arg.meta_item_list() {
        attrs.iter()
            .for_each(|attr| {
                if let Some(attr_name) = attr.ident() {
                    if let Some((execution_mode, extra_dim)) = cx.sym.execution_modes.get(&attr_name.name) {
                        use ExecutionModeExtraDim::*;
                        let val = match extra_dim {
                            None => Option::None,
                            _ => parse_attr_int_value(cx, attr)
                        };
                        match execution_mode {
                            OriginUpperLeft | OriginLowerLeft => {
                                origin_mode.replace(*execution_mode);        
                            },  
                            LocalSize => {
                                let val = val.unwrap();
                                if local_size.is_none() {
                                    local_size.replace([1, 1, 1]);
                                }
                                let local_size = local_size.as_mut()
                                    .unwrap();
                                match extra_dim {
                                    X => {
                                        local_size[0] = val;
                                    },
                                    Y => {
                                        local_size[1] = val;
                                    },
                                    Z => {
                                        local_size[2] = val;
                                    },
                                    _ => unreachable!(),
                                }
                            },
                            LocalSizeHint => {
                                let val = val.unwrap();
                                if local_size_hint.is_none() {
                                    local_size_hint.replace([1, 1, 1]);
                                }
                                let local_size_hint = local_size_hint.as_mut()
                                    .unwrap();
                                match extra_dim {
                                    X => {
                                        local_size_hint[0] = val;
                                    },
                                    Y => {
                                        local_size_hint[1] = val;
                                    },
                                    Z => {
                                        local_size_hint[2] = val;
                                    },
                                    _ => unreachable!(),
                                }
                            },
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
                                    entry.execution_modes.push((*execution_mode, ExecutionModeExtra::new([val])));
                                }
                                else {
                                    entry.execution_modes.push((*execution_mode, ExecutionModeExtra::new([])));
                                }
                            }
                        }
                    }
                    else {
                        cx.tcx.sess.span_err(
                            attr_name.span,
                            &format!(
                                "#[spirv({}(..))] unknown attribute argument {}", 
                                name.name.to_ident_string(),
                                attr_name.name.to_ident_string()
                            ),
                        );
                    }
                }
                else {
                    cx.tcx.sess.span_err(
                        arg.span(),
                        &format!(
                            "#[spirv({}(..))] attribute argument must be single identifier", 
                            name.name.to_ident_string()
                        ),
                    );
                }
                    
            });
    }
    match entry.execution_model {
        Fragment => {
            let origin_mode = origin_mode.unwrap_or(OriginUpperLeft);
            entry.execution_modes.push((origin_mode, ExecutionModeExtra::new([])));
        },
        GLCompute => {
            let local_size = local_size.unwrap_or([1, 1, 1]);
            entry.execution_modes.push((LocalSize, ExecutionModeExtra::new(local_size)));
        },
        Kernel => {
            if let Some(local_size) = local_size {
                entry.execution_modes.push((LocalSize, ExecutionModeExtra::new(local_size)));
            }
            if let Some(local_size_hint) = local_size_hint {
                entry.execution_modes.push((LocalSizeHint, ExecutionModeExtra::new(local_size_hint)));
            }
            // Reserved
            /*if let Some(max_workgroup_size_intel) = max_workgroup_size_intel {
                entry.execution_modes.push((MaxWorkgroupSizeINTEL, ExecutionModeExtra::new(max_workgroup_size_intel)));
            }*/
        },
        //TODO: Cover more defaults
        _ => {},
    }
    entry
}
