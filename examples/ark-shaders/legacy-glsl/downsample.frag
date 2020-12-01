#version 450
#extension GL_ARB_shading_language_420pack : enable
#extension GL_EXT_samplerless_texture_functions : enable

layout (set = 0, binding = 0) uniform texture2D in_image;
layout (set = 0, binding = 1) uniform sampler bilinearSampler;

layout (location = 0) in vec3 uv_layer;
layout (location = 0) out vec4 outColor;

void main() {
    vec2 uv = uv_layer.xy;
    outColor = texture(sampler2D(in_image, bilinearSampler), uv);
}
