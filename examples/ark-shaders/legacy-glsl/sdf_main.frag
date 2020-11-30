#version 450
#extension GL_ARB_shading_language_420pack : enable
#extension GL_EXT_multiview : enable

layout(set = 0, binding = 0) uniform SdfUniforms {
    mat4x4 clip_to_world;
    mat4x4 clip_to_world_stereo;

    mat4x4 world_to_clip;
    mat4x4 world_to_clip_stereo;

    vec4 camera_world_pos;

    vec4 sun_dir;
    vec4 sun_color;
    vec4 ground_color;
    vec4 sky_color;
    vec4 horizon_color;
}
sdf_uniforms;

layout(push_constant) uniform PushConstants {
    vec4 sdf_bbox_half_size;
    vec4 sdf_bbox_center;
    mat4x4 sdf_to_world;
    uvec4 program_and_data_offset;
    float opacity;
    float lighting; // 0 for disabled, 1 for enabled
}
push_constants;

layout(location = 0) in vec3 in_world_ray_origin;
layout(location = 1) in vec3 in_world_ray_direction;
layout(location = 2) in vec3 in_sdf_ray_origin;
layout(location = 3) in vec3 in_sdf_ray_direction;

layout(location = 0) out vec4 out_color;
layout(depth_greater) out float gl_FragDepth; // Reverse Z

layout(set = 0, binding = 1) readonly buffer SdfProgram { uint SDF_PROGRAM[]; };
layout(set = 0, binding = 2) readonly buffer SdfConstants { vec4 SDF_CONSTANTS[]; };

#define SDF_PROGRAM_OFFSET   push_constants.program_and_data_offset.x
#define SDF_CONSTANTS_OFFSET push_constants.program_and_data_offset.y

#include <lighting.glsl>
#include <sdf_generated.glsl>
#include <sdf_marcher.glsl>

bool ray_aabb_intersect(const vec3 box_min,
                        const vec3 box_max,
                        vec3 ray_origin,
                        vec3 inv_ray_dir,
                        out float out_tmin,
                        out float out_tmax) {

    vec3 tbot = inv_ray_dir * (box_min - ray_origin);
    vec3 ttop = inv_ray_dir * (box_max - ray_origin);
    vec3 tmin = min(ttop, tbot);
    vec3 tmax = max(ttop, tbot);
    vec2 t = max(tmin.xx, tmin.yz);
    float t0 = max(t.x, t.y);
    t = min(tmax.xx, tmax.yz);
    float t1 = min(t.x, t.y);
    out_tmin = t0;
    out_tmax = t1;
    return t1 > max(t0, 0.0);
}

// Assumes isometric scaling (the only type allowed for SDF volumes).
float matrix_iso_scale(mat4 m) {
    return length(m[0].xyz);
}

Ray ray_in_sdf() {
    vec3 dir_norm = normalize(in_sdf_ray_direction.xyz);
    return Ray(in_sdf_ray_origin.xyz, dir_norm);
}

Ray ray_in_world() {
    vec3 dir_norm = normalize(in_world_ray_direction.xyz);
    return Ray(in_world_ray_origin.xyz, dir_norm);
}

vec3 sdf_to_world_vector(vec3 v) { return (push_constants.sdf_to_world * vec4(v, 0.0)).xyz; }

vec3 sdf_to_world_point(vec3 v) { return (push_constants.sdf_to_world * vec4(v, 1.0)).xyz; }

float sdf_to_world_ray_scale() {
    return matrix_iso_scale(push_constants.sdf_to_world);
}

vec3 cube_to_sdf_bbox(vec3 cube) {
    return (push_constants.sdf_bbox_half_size.xyz * cube) + push_constants.sdf_bbox_center.xyz;
}

float depth_from_world_pos(vec3 world_position, uint viewIndex) {
    if (viewIndex == 0) {
        vec4 clip = sdf_uniforms.world_to_clip * vec4(world_position, 1.0);
        return clip.z / clip.w;
    } else {
        vec4 clip = sdf_uniforms.world_to_clip_stereo * vec4(world_position, 1.0);
        return clip.z / clip.w;
    }
}

void on_ray_sdf_hit(
    out vec4 color, out float depth, in vec3 world_position, vec3 final_color, in int viewIndex) {
    depth = depth_from_world_pos(world_position, viewIndex);
    color = vec4(final_color, 1.0) * push_constants.opacity;
}

void on_ray_sdf_miss(out vec4 color, out float depth) {
    // We cannot set a more distant depth as we're running with conservative depth and front
    // face culling
    // TODO: Ideally we should have two pipelines.
    // One for when we're inside the bounding box and one for when we're outside.

    color = vec4(0.0, 0.0, 0.0, 0.0);
    depth = gl_FragCoord.z;
    discard;
}

void main() {
    Ray sdf_ray = ray_in_sdf();
    vec3 sdf_bbox_min = cube_to_sdf_bbox(vec3(-1.0, -1.0, -1.0));
    vec3 sdf_bbox_max = cube_to_sdf_bbox(vec3(1.0, 1.0, 1.0));

    float sdf_ray_bbox_tmin, sdf_ray_bbox_tmax;
    if (!ray_aabb_intersect(sdf_bbox_min,
                            sdf_bbox_max,
                            sdf_ray.origin,
                            1.0 / sdf_ray.direction,
                            sdf_ray_bbox_tmin,
                            sdf_ray_bbox_tmax)) {
        on_ray_sdf_miss(out_color, gl_FragDepth);
        return;
    }

    // Never allow a negative tmin (behind the camera)
    sdf_ray_bbox_tmin = max(sdf_ray_bbox_tmin, 0.0);

    // Cast ray in the sdf
    RaySdfIntersectResult result;
    if (!ray_sdf_intersect(sdf_ray, sdf_ray_bbox_tmin, sdf_ray_bbox_tmax, result)) {
        on_ray_sdf_miss(out_color, gl_FragDepth);
        return;
    }

    Ray world_ray = ray_in_world();

    // The distance for the camera ray in sdf space
    float sdf_ray_distance = result.t;

    bool camera_inside_sdf = sdf_ray_distance <= 0.0;
    if (camera_inside_sdf) {
        // We sample the diffuse color of the sdf at the camera origin.
        vec3 world_position = world_ray.origin;
        vec3 diffuse_color = sdf_diffuse_color_at(result.position);
        on_ray_sdf_hit(out_color, gl_FragDepth, world_position, diffuse_color, gl_ViewIndex);
    } else {
        float world_ray_distance = sdf_ray_distance * sdf_to_world_ray_scale();
        vec3 world_position = world_ray.origin + world_ray.direction * world_ray_distance;
        vec3 world_normal = normalize(sdf_to_world_vector(sdf_gradient_at(result.position)));
        vec3 diffuse_color = sdf_diffuse_color_at(result.position);
        vec3 lit_color = ark_light(surface_params(diffuse_color, world_position, world_normal),
                                     view_params(world_ray.origin),
                                     env_params(sdf_uniforms.sun_dir.xyz,
                                                sdf_uniforms.sun_color.rgb,
                                                sdf_uniforms.ground_color.rgb,
                                                sdf_uniforms.sky_color.rgb,
                                                sdf_uniforms.horizon_color.rgb));
        vec3 final_color = mix(diffuse_color, lit_color, push_constants.lighting);

        on_ray_sdf_hit(out_color, gl_FragDepth, world_position, final_color, gl_ViewIndex);
    }
}
