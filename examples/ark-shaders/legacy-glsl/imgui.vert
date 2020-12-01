#version 450

layout (location = 0) in vec2 inPos;
layout (location = 1) in vec2 inUV;
layout (location = 2) in vec4 inColor;

layout (push_constant) uniform PushConstants {
    mat4x4 projectionMatrix;
} pushConstants;

layout (location = 0) out vec2 outUV;
layout (location = 1) out vec4 outColor;

out gl_PerVertex
{
    vec4 gl_Position;
};

vec3 linear_from_srgb(vec3 srgb) {
    vec3 a = srgb / 12.92;
    vec3 b = pow((srgb + 0.055) / 1.055, vec3(2.4));
    vec3 c = step(vec3(0.04045), srgb);
    return mix(a, b, c);
}

vec4 linear_from_srgba(vec4 srgba) {
    return vec4(linear_from_srgb(srgba.rgb), srgba.a);
}

void main()
{
    outUV = inUV;
    outColor = linear_from_srgba(inColor);

    gl_Position = pushConstants.projectionMatrix * vec4(inPos, 0.0, 1.0);
    gl_Position.y = -gl_Position.y; // TODO: Change to use negative viewport height instead
}