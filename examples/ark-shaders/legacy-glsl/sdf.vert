#version 450

#extension GL_EXT_multiview : enable

layout(location = 0) out vec3 out_world_ray_origin;
layout(location = 1) out vec3 out_world_ray_direction;
layout(location = 2) out vec3 out_sdf_ray_origin;
layout(location = 3) out vec3 out_sdf_ray_direction;

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
}
push_constants;

out gl_PerVertex { vec4 gl_Position; };

// https://gist.github.com/rikusalminen/9393151
// Returns a normalized cube (in the -1..1 range) position from a
// vertex index (range 0-35, 36 vertices, 12 triangles)
vec3 get_cube_pos(int vertexIndex) {
    int tri = vertexIndex / 3;
    int idx = vertexIndex % 3;
    int face = tri / 2;
    int top = tri % 2;

    int dir = face % 3;
    int pos = face / 3;

    int nz = dir >> 1;
    int ny = dir & 1;
    int nx = 1 ^ (ny | nz);

    vec3 d = vec3(nx, ny, nz);
    float flip = 1 - 2 * pos;

    vec3 n = flip * d;
    vec3 u = -d.yzx;
    vec3 v = flip * d.zxy;

    float mirror = -1 + 2 * top;
    vec3 xyz = n + mirror * (1 - 2 * (idx & 1)) * u + mirror * (1 - 2 * (idx >> 1)) * v;
    return xyz;
}

vec3 cube_to_sdf_bbox(vec3 cube) {
    return (push_constants.sdf_bbox_half_size.xyz * cube) + push_constants.sdf_bbox_center.xyz;
}

void main() {

    mat4x4 world_to_sdf = inverse(push_constants.sdf_to_world);
    vec3 world_camera_position = sdf_uniforms.camera_world_pos.xyz;
    vec3 sdf_camera_position = (world_to_sdf * vec4(world_camera_position, 1.0)).xyz;

    int vertex_index = gl_VertexIndex;

    vec3 sdf_position = cube_to_sdf_bbox(get_cube_pos(vertex_index));
    vec3 world_position = (push_constants.sdf_to_world * vec4(sdf_position, 1.0)).xyz;

    // Ray direction is determined by the camera to bounding box direction in
    // world space
    out_world_ray_origin = world_camera_position;
    out_world_ray_direction = world_position - world_camera_position;
    out_sdf_ray_origin = sdf_camera_position;
    out_sdf_ray_direction = (world_to_sdf * vec4(out_world_ray_direction, 0.0)).xyz;

    if (gl_ViewIndex == 0) {
        gl_Position = sdf_uniforms.world_to_clip * vec4(world_position.xyz, 1.0);
    } else {
        gl_Position = sdf_uniforms.world_to_clip_stereo * vec4(world_position.xyz, 1.0);
    }

    gl_Position.y = -gl_Position.y; // TODO: Change to use negative viewport height instead
}
