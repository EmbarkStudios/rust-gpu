#version 450
#extension GL_ARB_shading_language_420pack : enable

layout (set = 0, binding = 0) uniform texture2DArray img;
layout (set = 0, binding = 1) uniform sampler bilinearSampler;

layout (location = 0) in vec2 uv;
layout (location = 0) out vec4 outColor;

void main() {
    // Just pick the color from the first image.
    outColor = vec4(texture(sampler2DArray(img, bilinearSampler), vec3(uv, 0)).xyz, 1.0);
}
