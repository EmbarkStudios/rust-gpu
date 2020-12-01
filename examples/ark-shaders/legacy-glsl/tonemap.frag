#version 450
#extension GL_ARB_shading_language_420pack : enable

layout (location = 0) in vec3 uv;
layout (location = 0) out vec4 outColor;

layout (set = 0, binding = 0) uniform texture2D img;
layout (set = 0, binding = 1) uniform texture2DArray bloom;
layout (set = 0, binding = 2) uniform sampler bilinearSampler;

layout (push_constant) uniform PushConstants {
    float exposure;
    float aspect;
} push_constants;

float calculate_luma(vec3 col) {
    return dot(vec3(0.2126, 0.7152, 0.0722), col);
}

vec3 tonemap_neutral(vec3 col) {
   float tm_luma = 1.0 - exp(-calculate_luma(col.rgb));
   vec3 tm0 = col.rgb * max(0.0, tm_luma / max(1e-5, calculate_luma(col.rgb)));
   vec3 tm1 = col.rgb = 1.0 - exp(-col.rgb);
   return mix(tm0, tm1, tm_luma);
}

float dither_pattern_at_coordinate(uvec2 coord) {
    float dither[16] = { 0, 8, 2, 10, 12, 4, 14, 6, 3, 11, 1, 9, 15, 7, 13, 5};
    uint ditherIndex = (coord.x & 3) + ((coord.y << 2) & 0xC);
    return (dither[ditherIndex] - 7.5) / (255.0 * 15.0);
}

float vignette(vec2 pos, float aspect, float strength) {
    vec2 centered = (vec2(pos.x, pos.y) * 2.0) - 1.0;
    centered.x *= aspect;
    return max(0.0, 1.0 - dot(centered.xy, centered.xy) * strength);
}

void main() {
    float bloom_strength = 0.06;
    float dither_strength = 1.0;
    float vignette_strength = 0.10;

    vec2 screen_pos = uv.xy;
    vec3 color = mix(
        texture(sampler2D(img, bilinearSampler), screen_pos.xy).rgb,
        texture(sampler2DArray(bloom, bilinearSampler), vec3(screen_pos.xy, 0.0)).rgb,
        bloom_strength);
    float vignette_amount = vignette(screen_pos, push_constants.aspect, vignette_strength);
    vec3 tonemapped = tonemap_neutral(color * (push_constants.exposure * vignette_amount));
    float dither = dither_pattern_at_coordinate(uvec2(gl_FragCoord.xy));
    vec3 final_color = tonemapped + dither * dither_strength;
    outColor = vec4(final_color, 1.0);
}
