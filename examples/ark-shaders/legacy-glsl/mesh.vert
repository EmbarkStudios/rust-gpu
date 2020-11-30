#version 450

#extension GL_EXT_multiview : enable

layout(location = 0) in vec3 in_pos;
layout(location = 1) in vec3 in_normal;
layout(location = 2) in vec4 in_color;

// The first 6 flags here correspond to MeshStyleFlags
#define LIGHTING_ENABLE            (1 << 0)
#define ALPHA_BLENDING_ENABLE      (1 << 1)
#define FLAT_SHADING_ENABLE        (1 << 2)
#define HSV_TRANSFORM_ENABLE       (1 << 3)
#define PREMULTIPLIED_ALPHA_ENABLE (1 << 4)
#define BILLBOARD_ENABLE           (1 << 5)

// These are internal, set by the render backend, driven by what is available in the data
#define VERTEX_COLOR_ENABLE (1 << 16)
#define NORMALS_ENABLE      (1 << 17)

struct InstanceDataEntry {
    mat4x4 object_to_world;
    vec4 diffuse_tint;
    vec4 hsv_transform_data;
    uint flags;
};

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

layout(set = 0, binding = 1) readonly buffer InstanceDataBuffer {
    InstanceDataEntry instance_data[];
};

layout(location = 0) out vec4 diffuse_color;
layout(location = 1) out vec3 world_normal;
layout(location = 2) out vec3 world_pos;
layout(location = 3) out float opacity;
layout(location = 4) out uint flags;

out gl_PerVertex { vec4 gl_Position; };

vec3 encodeSRGB(vec3 linearRGB) {
    vec3 a = 12.92 * linearRGB;
    vec3 b = 1.055 * pow(linearRGB, vec3(1.0 / 2.4)) - 0.055;
    vec3 c = step(vec3(0.0031308), linearRGB);
    return mix(a, b, c);
}

vec3 decodeSRGB(vec3 screenRGB) {
    vec3 a = screenRGB / 12.92;
    vec3 b = pow((screenRGB + 0.055) / 1.055, vec3(2.4));
    vec3 c = step(vec3(0.04045), screenRGB);
    return mix(a, b, c);
}

vec4 degamma(vec4 v) { return vec4(decodeSRGB(v.xyz), v.w); }

// All components are in the range [0…1], including hue.
// http://lolengine.net/blog/2013/07/27/rgb-to-hsv-in-glsl
vec3 rgb2hsv(vec3 c) {
    vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
    vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));
    vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));

    float d = q.x - min(q.w, q.y);
    float e = 1.0e-10;
    return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
}

// All components are in the range [0…1], including hue.
// http://lolengine.net/blog/2013/07/27/rgb-to-hsv-in-glsl
vec3 hsv2rgb(vec3 c) {
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

float extract_conservative_scale_from_transform(mat4 t) {
    vec3 v1 = t[0].xyz;
    vec3 v2 = t[1].xyz;
    vec3 v3 = t[2].xyz;

    float l1 = dot(v1, v1);
    float l2 = dot(v2, v2);
    float l3 = dot(v3, v3);

    return sqrt(max(l1, max(l2, l3)));
}

void main() {
    InstanceDataEntry instance = instance_data[gl_InstanceIndex];

    flags = instance.flags;

    // De-gamma diffuse color - this is the right place.
    if ((instance.flags & VERTEX_COLOR_ENABLE) != 0) {
        diffuse_color = degamma(in_color);
    } else {
        diffuse_color = vec4(1.0, 1.0, 1.0, 1.0);
    }

    if ((instance.flags & HSV_TRANSFORM_ENABLE) != 0) {
        vec3 rgb = diffuse_color.rgb;
        vec3 hsv = rgb2hsv(rgb);

        // rotate hue
        hsv.x = fract(hsv.x + instance.hsv_transform_data.x);

        // clamp saturation / value
        hsv.y = clamp(hsv.y + instance.hsv_transform_data.y, 0.0, 1.0);
        hsv.z = clamp(hsv.z + instance.hsv_transform_data.z, 0.0, 1.0);

        rgb = hsv2rgb(hsv);
        diffuse_color.rgb = rgb;
    }

    diffuse_color = diffuse_color * instance.diffuse_tint;

    mat4 object_to_world = instance.object_to_world;

    if ((instance.flags & BILLBOARD_ENABLE) != 0) {
        float scale = extract_conservative_scale_from_transform(object_to_world);
        if (gl_ViewIndex == 0) {
            object_to_world = mat4(mat3(global_uniforms.view_to_world) * mat3(scale));
        } else {
            object_to_world = mat4(mat3(global_uniforms.view_to_world_stereo) * mat3(scale));
        }
        object_to_world[3] = instance.object_to_world[3];
    }

    world_pos = (object_to_world * vec4(in_pos, 1.0)).xyz;
    if ((instance.flags & NORMALS_ENABLE) != 0) {
        world_normal = mat3(object_to_world) * in_normal;
    } else {
        // Will be ignored in the fragment shader.
        world_normal = vec3(0.0, 1.0, 0.0);
    }

    // The pipeline will always do premultiplied alpha, we have logic here to select whether we want
    // traditional alpha blending, or premultiplied
    switch (instance.flags & (ALPHA_BLENDING_ENABLE | PREMULTIPLIED_ALPHA_ENABLE)) {
        case (ALPHA_BLENDING_ENABLE & (~PREMULTIPLIED_ALPHA_ENABLE)):
            opacity = diffuse_color.a;
            break;
        case (ALPHA_BLENDING_ENABLE | PREMULTIPLIED_ALPHA_ENABLE):
            opacity = diffuse_color.a;
            diffuse_color.a = 1.0;
            break;
        default:
            opacity = 1.0;
            diffuse_color.a = 1.0;
            break;
    }

    if (gl_ViewIndex == 0) {
        gl_Position = global_uniforms.world_to_clip * vec4(world_pos.xyz, 1.0);
    } else {
        gl_Position = global_uniforms.world_to_clip_stereo * vec4(world_pos.xyz, 1.0);
    }

    gl_Position.y = -gl_Position.y; // TODO: Change to use negative viewport height instead
}
