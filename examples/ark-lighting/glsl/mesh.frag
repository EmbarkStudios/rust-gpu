#version 450
#extension GL_ARB_shading_language_420pack : enable

layout(location = 0) in vec4 diffuse_color;
layout(location = 1) in vec3 world_normal;
layout(location = 2) in vec3 world_pos;
layout(location = 3) in float opacity;
layout(location = 4) flat in uint flags;

layout(location = 0) out vec4 out_color;

// The first 6 flags here correspond to MeshStyleFlags
#define LIGHTING_ENABLE            (1 << 0)
#define ALPHA_BLENDING_ENABLE      (1 << 1)
#define FLAT_SHADING_ENABLE        (1 << 2)
#define HSV_TRANSFORM_ENABLE       (1 << 3)
#define PREMULTIPLIED_ALPHA_ENABLE (1 << 4)
#define BILLBOARD_ENABLE           (1 << 5)

// These are internal, set by the render backend, driven by what is available in
// the data
#define VERTEX_COLOR_ENABLE (1 << 16)
#define NORMALS_ENABLE      (1 << 17)

layout(set = 0, binding = 0) uniform GlobalUniforms {
    mat4x4 world_to_clip;
    mat4x4 world_to_clip_stereo;
    mat4x4 view_to_world;
    mat4x4 view_to_world_stereo;
    vec4 sun_dir;
    vec4 camera_world_pos;

    vec4 sun_color;
    vec4 ground_color;
    vec4 sky_color;
    vec4 horizon_color;
}
global_uniforms;

#include <lighting.glsl>

void main() {
    if ((flags & LIGHTING_ENABLE) != 0) {
        vec3 normal = world_normal;
        if ((flags & FLAT_SHADING_ENABLE) != 0) {
            normal = normalize(-cross(dFdx(world_pos), dFdy(world_pos)));
        }

        out_color.rgb = ark_light(surface_params(diffuse_color.rgb, world_pos, normal),
                                  view_params(global_uniforms.camera_world_pos.xyz),
                                  env_params(global_uniforms.sun_dir.xyz,
                                             global_uniforms.sun_color.rgb,
                                             global_uniforms.ground_color.rgb,
                                             global_uniforms.sky_color.rgb,
                                             global_uniforms.horizon_color.rgb)) *
                        vec3(diffuse_color.a);

        out_color.a = opacity;
    } else {
        out_color.rgb = diffuse_color.rgb * diffuse_color.a;
        out_color.a = opacity;
    }
}
