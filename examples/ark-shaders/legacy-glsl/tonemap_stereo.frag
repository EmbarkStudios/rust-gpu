#version 450
#extension GL_ARB_shading_language_420pack : enable

layout (location = 0) in vec3 uv;
layout (location = 0) out vec4 outColor_l;
layout (location = 1) out vec4 outColor_r;

layout (set = 0, binding = 0) uniform texture2DArray img;
layout (set = 0, binding = 1) uniform texture2DArray bloom;
layout (set = 0, binding = 2) uniform sampler bilinearSampler;

layout (push_constant) uniform PushConstants {
	float exposure;
} pushConstants;

float calculate_luma(vec3 col) {
    return dot(vec3(0.2126, 0.7152, 0.0722), col);
}

vec3 tonemap_neutral(vec3 col) {
   float tm_luma = 1.0 - exp(-calculate_luma(col.rgb));
   vec3 tm0 = col.rgb * max(0.0, tm_luma / max(1e-5, calculate_luma(col.rgb)));
   vec3 tm1 = col.rgb = 1.0 - exp(-col.rgb);
   return mix(tm0, tm1, tm_luma);
}

void main() {
    vec2 screenPos = uv.xy;
    float bloomStrength = 0.06;
    vec3 color_l = mix(
        texture(sampler2DArray(img, bilinearSampler), vec3(screenPos.xy, 0)).rgb,
        texture(sampler2DArray(bloom, bilinearSampler), vec3(screenPos.xy, 0)).rgb,
        bloomStrength);
    vec3 color_r = mix(
        texture(sampler2DArray(img, bilinearSampler), vec3(screenPos.xy, 1)).rgb,
        texture(sampler2DArray(bloom, bilinearSampler), vec3(screenPos.xy, 1)).rgb,
        bloomStrength);

    outColor_l = vec4(tonemap_neutral(color_l * pushConstants.exposure), 1.0);
    outColor_r = vec4(tonemap_neutral(color_r * pushConstants.exposure), 1.0);
}
