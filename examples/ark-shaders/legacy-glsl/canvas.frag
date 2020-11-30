#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout (set = 0, binding = 0) uniform texture2D fontTex;
layout (set = 0, binding = 1) uniform sampler fontSampler;

layout (location = 0) in vec2 inUV;
layout (location = 1) in vec4 inColor;

layout (location = 0) out vec4 outColor;

void main() {
    outColor = inColor * texture(sampler2D(fontTex, fontSampler), inUV);
}
