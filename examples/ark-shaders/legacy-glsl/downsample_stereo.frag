#version 450
#extension GL_ARB_shading_language_420pack : enable
#extension GL_EXT_samplerless_texture_functions : enable

layout (set = 0, binding = 0) uniform texture2DArray in_image;
layout (set = 0, binding = 1) uniform sampler bilinearSampler;

layout (location = 0) in vec3 uv_layer;
layout (location = 0) out vec4 outColor;

void main() {
    vec3 uv = uv_layer.xyz;
    outColor = texture(sampler2DArray(in_image, bilinearSampler), uv);
}
