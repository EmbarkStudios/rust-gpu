// build-pass
// only-vulkan1.1
// compile-flags: -Ctarget-feature=+DeviceGroup,+DrawParameters,+FragmentBarycentricNV,+FragmentDensityEXT,+FragmentFullyCoveredEXT,+Geometry,+GroupNonUniform,+GroupNonUniformBallot,+MeshShadingNV,+MultiView,+MultiViewport,+RayTracingKHR,+SampleRateShading,+ShaderSMBuiltinsNV,+ShaderStereoViewNV,+StencilExportEXT,+Tessellation,+ext:SPV_AMD_shader_explicit_vertex_parameter,+ext:SPV_EXT_fragment_fully_covered,+ext:SPV_EXT_fragment_invocation_density,+ext:SPV_EXT_shader_stencil_export,+ext:SPV_KHR_ray_tracing,+ext:SPV_NV_fragment_shader_barycentric,+ext:SPV_NV_mesh_shader,+ext:SPV_NV_shader_sm_builtins,+ext:SPV_NV_stereo_view_rendering

use spirv_std::glam::*;
use spirv_std::spirv;

#[derive(Clone, Copy)]
#[spirv(matrix)]
pub struct Matrix4x3 {
    pub x: glam::Vec3,
    pub y: glam::Vec3,
    pub z: glam::Vec3,
    pub w: glam::Vec3,
}

#[spirv(tessellation_control)]
pub fn tessellation_control(
    #[spirv(invocation_id)] invocation_id: u32,
    #[spirv(patch_vertices)] patch_vertices: u32,
    #[spirv(tess_level_inner)] tess_level_inner: &mut [f32; 2],
    #[spirv(tess_level_outer)] tess_level_outer: &mut [f32; 4],
) {
}

#[spirv(tessellation_evaluation)]
pub fn tessellation_evaluation(#[spirv(tess_coord)] tess_coord: Vec3) {}

#[spirv(compute(threads(1)))]
pub fn compute(
    #[spirv(global_invocation_id)] global_invocation_id: UVec3,
    #[spirv(local_invocation_id)] local_invocation_id: UVec3,
    #[spirv(subgroup_local_invocation_id)] subgroup_local_invocation_id: u32,
    #[spirv(num_subgroups)] num_subgroups: u32,
    #[spirv(num_workgroups)] num_workgroups: UVec3,
    #[spirv(subgroup_id)] subgroup_id: u32,
    #[spirv(workgroup_id)] workgroup_id: UVec3,
    #[spirv(workgroup)] workgroup_local_memory: &mut [u32; 256],
) {
}

#[spirv(vertex)]
pub fn vertex(
    #[spirv(SMIDNV)] smidnv: u32,
    #[spirv(base_instance)] base_instance: u32,
    #[spirv(base_vertex)] base_vertex: u32,
    #[spirv(clip_distance_per_view_nv)] clip_distance_per_view_nv: u32,
    #[spirv(cull_distance_per_view_nv)] cull_distance_per_view_nv: u32,
    #[spirv(device_index)] device_index: u32,
    #[spirv(draw_index)] draw_index: u32,
    #[spirv(frag_depth)] frag_depth: &mut f32,
    #[spirv(frag_stencil_ref_ext)] frag_stencil_ref_ext: &mut u32,
    #[spirv(instance_index)] instance_index: u32,
    #[spirv(layer_per_view_nv)] layer_per_view_nv: u32,
    #[spirv(local_invocation_index)] local_invocation_index: UVec3,
    #[spirv(mesh_view_count_nv)] mesh_view_count_nv: u32,
    #[spirv(mesh_view_indices_nv)] mesh_view_indices_nv: u32,
    #[spirv(point_size)] point_size: &mut u32,
    #[spirv(position)] position: &mut u32,
    #[spirv(position_per_view_nv)] position_per_view_nv: u32,
    #[spirv(primitive_count_nv)] primitive_count_nv: u32,
    #[spirv(primitive_indices_nv)] primitive_indices_nv: u32,
    #[spirv(secondary_position_nv)] secondary_position_nv: u32,
    #[spirv(secondary_viewport_mask_nv)] secondary_viewport_mask_nv: u32,
    #[spirv(sm_count_nv)] sm_count_nv: u32,
    #[spirv(subgroup_eq_mask)] subgroup_eq_mask: UVec4,
    #[spirv(subgroup_ge_mask)] subgroup_ge_mask: UVec4,
    #[spirv(subgroup_gt_mask)] subgroup_gt_mask: UVec4,
    #[spirv(subgroup_le_mask)] subgroup_le_mask: UVec4,
    #[spirv(subgroup_lt_mask)] subgroup_lt_mask: UVec4,
    #[spirv(subgroup_size)] subgroup_size: u32,
    #[spirv(task_count_nv)] task_count_nv: u32,
    #[spirv(vertex_index)] vertex_index: u32,
    #[spirv(view_index)] view_index: u32,
    #[spirv(viewport_mask_nv)] viewport_mask_nv: u32,
    #[spirv(viewport_mask_per_view_nv)] viewport_mask_per_view_nv: u32,
    #[spirv(warp_id_nv)] warp_id_nv: u32,
    #[spirv(warps_per_sm_nv)] warps_per_sm_nv: u32,
    // #[spirv(vertex_id)] vertex_id: u32, -- not allowed with vulkan
) {
}

#[spirv(fragment)]
pub fn fragment(
    #[spirv(bary_coord_no_persp_amd)] bary_coord_no_persp_amd: Vec3,
    #[spirv(bary_coord_no_persp_centroid_amd)] bary_coord_no_persp_centroid_amd: Vec3,
    #[spirv(bary_coord_no_persp_nv)] bary_coord_no_persp_nv: Vec3,
    #[spirv(bary_coord_no_persp_sample_amd)] bary_coord_no_persp_sample_amd: Vec3,
    #[spirv(bary_coord_nv)] bary_coord_nv: Vec3,
    #[spirv(bary_coord_pull_model_amd)] bary_coord_pull_model_amd: Vec3,
    #[spirv(bary_coord_smooth_amd)] bary_coord_smooth_amd: Vec3,
    #[spirv(bary_coord_smooth_centroid_amd)] bary_coord_smooth_centroid_amd: Vec3,
    #[spirv(bary_coord_smooth_sample_amd)] bary_coord_smooth_sample_amd: Vec3,
    #[spirv(clip_distance)] clip_distance: [f32; 1],
    #[spirv(cull_distance)] cull_distance: [f32; 1],
    #[spirv(frag_coord)] frag_coord: Vec4,
    #[spirv(frag_invocation_count_ext, flat)] frag_invocation_count_ext: u32,
    #[spirv(frag_size_ext, flat)] frag_size_ext: UVec2,
    #[spirv(front_facing)] front_facing: bool,
    #[spirv(fully_covered_ext)] fully_covered_ext: bool,
    #[spirv(helper_invocation)] helper_invocation: bool,
    #[spirv(layer, flat)] layer: u32,
    #[spirv(point_coord)] point_coord: Vec2,
    #[spirv(primitive_id, flat)] primitive_id: u32,
    #[spirv(sample_id, flat)] sample_id: u32,
    #[spirv(sample_mask, flat)] sample_mask: [u32; 1],
    #[spirv(sample_position)] sample_position: Vec2,
    #[spirv(viewport_index, flat)] viewport_index: u32,
) {
}
#[spirv(closest_hit)]
pub fn closest_hit(
    #[spirv(hit_kind)] hit_kind: u32,
    #[spirv(incoming_ray_flags)] incoming_ray_flags: u32,
    #[spirv(instance_custom_index)] instance_custom_index: u32,
    #[spirv(instance_id)] instance_id: u32,
    #[spirv(launch_id)] launch_id: UVec3,
    #[spirv(launch_size)] launch_size: UVec3,
    #[spirv(object_ray_direction)] object_ray_direction: Vec3,
    #[spirv(object_ray_origin)] object_ray_origin: Vec3,
    #[spirv(object_to_world)] object_to_world: Matrix4x3,
    #[spirv(ray_geometry_index)] ray_geometry_index: u32,
    #[spirv(ray_tmax)] ray_tmax: f32,
    #[spirv(ray_tmin)] ray_tmin: f32,
    #[spirv(world_ray_direction)] world_ray_direction: Vec3,
    #[spirv(world_ray_origin)] world_ray_origin: Vec3,
    #[spirv(world_to_object)] world_to_object: Matrix4x3,
) {
}
